/*
This file is part of OpenLogos/LogOSMaTrans.  Copyright (C) 2005 Globalware AG

OpenLogos/LogOSMaTrans has two licensing options:

The Commercial License, which allows you to provide commercial software
licenses to your customers or distribute Logos MT based applications or to use
LogOSMaTran for commercial purposes. This is for organizations who do not want
to comply with the GNU General Public License (GPL) in releasing the source
code for their applications as open source / free software.

The Open Source License allows you to offer your software under an open source
/ free software license to all who wish to use, modify, and distribute it
freely. The Open Source License allows you to use the software at no charge
under the condition that if you use OpenLogos/LogOSMaTran in an application you
redistribute, the complete source code for your application must be available
and freely redistributable under reasonable conditions. GlobalWare AG bases its
interpretation of the GPL on the Free Software Foundation's Frequently Asked
Questions.

OpenLogos is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the License conditions along with this
program. If not, write to Globalware AG, Hospitalstraﬂe 6, D-99817 Eisenach.

Linux port modifications and additions by Bernd Kiefer, Walter Kasper,
Deutsches Forschungszentrum fuer kuenstliche Intelligenz (DFKI)
Stuhlsatzenhausweg 3, D-66123 Saarbruecken
*/
/*******************************************************************
 *
 *    DESCRIPTION:
 *
 *    AUTHOR:
 *
 *    HISTORY:    
 *
 *******************************************************************/

#include <assert.h>
 
#include <logos_include/logoscommon.h>
#include <logos_libs/utility/argumentexception.h>
#include <logos_libs/linguistic/llanguage.h>

#include <logos_libs/sql/sqlconnection.h>
#include <logos_libs/linguistic/targetsentenceunit.h>
#include <logos_libs/linguistic/targetdictionaryunit.h>

#include <lgs_db_io/stemgenrulequery.h>
#include <lgs_stemgen/stemgeneratorrules.h>
#include <lgs_stemgen/stemgenerator.h>
#include <logos_libs/gerdem/pattablequeryman.h>
#include <logos_libs/gerdem/stembuilder.h>
#include <logos_libs/gerdem/wordphrasequery.h>


StemBuilder::StemBuilder()
            :_pPatTableQueryManager(0),
             _pStemGenRuleQuery(0),
             _pWordPhraseQuery(0),
             _pStemGenerator(0),
             _isOpen(false) 
{
}

StemBuilder::~StemBuilder()
{

//XXX API, some problems w/ this destructor
   delete _pPatTableQueryManager;
   _pPatTableQueryManager = 0;
   
   if (_pStemGenRuleQuery)
   {
      _pStemGenRuleQuery->Close();
      delete _pStemGenRuleQuery;
      _pStemGenRuleQuery = 0;
   }

   if (_pWordPhraseQuery)
   {
      _pWordPhraseQuery->Close();
//XXX API, some problems w/ this destructor
      delete _pWordPhraseQuery;
      _pWordPhraseQuery = 0;
   }
   
   delete _pStemGenerator;
   _pStemGenerator = 0;

}

void StemBuilder::open(SqlConnection* pConnection_, const LLanguage& language_)
{
   assert(!_pPatTableQueryManager && !_pStemGenRuleQuery && !_pStemGenerator);
   
   try
   {
      _pPatTableQueryManager = new PatTableQueryManager(pConnection_, language_);
      _pStemGenRuleQuery = new StemGenRuleQuery;
      _pStemGenRuleQuery->Open(pConnection_, language_);
      _pWordPhraseQuery = new WordPhraseQuery;
      _pWordPhraseQuery->Open(pConnection_);
      _pStemGenerator = new StemGenerator(language_);
      _isOpen = true;
      _langCode = language_.id();
   }
   catch (bad_alloc)
   {
      throw;
   }
}

void StemBuilder::close()
{
   _isOpen = false;
}


void StemBuilder::getStem(LgsString* text_, const TargetSentenceUnit& unit_, bool isHeadWordInPhrase)
{
   if (_isOpen && !_pPatTableQueryManager->Execute(unit_))
   {
      _getStem(text_, unit_.patNumber(), unit_.wordClassCode(), unit_, isHeadWordInPhrase);
   }
}


void StemBuilder::getStem(LgsString* text_, int patNumber_, int wordClassCode_,
                          const TargetSentenceUnit& unit_)
{
   if (_isOpen && !_pPatTableQueryManager->Execute(patNumber_, wordClassCode_, unit_, true))
   {
      _getStem(text_, patNumber_, wordClassCode_, unit_, false);
   }
}

void StemBuilder::_getStem(LgsString* text_, int patNumber_, int wordClassCode_,
                           const TargetSentenceUnit& unit_, bool isHeadWordInPhrase)
{
   int stemNumber;
   LgsString ending;

   if (_pPatTableQueryManager->Fetch(stemNumber, ending))
   {
      StemGeneratorRules rules;

      _pStemGenRuleQuery->Execute(patNumber_, stemNumber);
      _pStemGenRuleQuery->Fetch(rules.addEnding, rules.dropEnding, rules.replaceRule, 
                                rules.addPrefix);
   
      LgsString stem;
      stem.reserve(255);
   
      if (!unit_.isFunctionalConstant())
      {
         const TargetDictionaryUnit& unit = dynamic_cast<const TargetDictionaryUnit&>(unit_);
         rules.sepPrefixPos = unit.verbPrefixSeparablePosition();
         rules.sepPrefixLen = unit.verbPrefixSeparableLength();
         rules.insepPrefixPos = unit.verbPrefixInseparablePosition();
         rules.insepPrefixLen = unit.verbPrefixInseparableLength()  ;
      }
   
      if ((_langCode == LLanguage::GermanID) && (wordClassCode_ == LLanguage::ADJECTIVE) &&
          ((unit_.wordClassCode() == LLanguage::NOUN) ||
           (!isHeadWordInPhrase && (unit_.wordClassCode() == LLanguage::ADJECTIVE) &&
            ((unit_.patNumber() == 536) || (unit_.patNumber() == 546)))))
      {
         int endingLength;
         const TargetDictionaryUnit& targetUnit = dynamic_cast<const TargetDictionaryUnit&>(unit_);
         _pWordPhraseQuery->Execute(targetUnit.wordID());
         _pWordPhraseQuery->Fetch(&endingLength);
         text_->erase((text_->length() - endingLength), endingLength);
      }
      _pStemGenerator->applyRules(text_, rules, wordClassCode_);

      text_->append(ending);
   }
   else
   {
      if ((_langCode == LLanguage::GermanID) && (wordClassCode_ == LLanguage::VERB) &&
          (unit_.tense() == 9))
      {
         *text_ = "";
      }
   }
}

//void 
//StemBuilder
//::_decomposeWord(const LgsString& text_, 
//                 const TargetSentenceUnit& unit_,
//                 LgsString* stem_, 
//                 LgsString* separablePrefix_,
//                 LgsString* inseparablePrefix_)
//{
//   const TargetDictionaryUnit& u = 
//      dynamic_cast<const TargetDictionaryUnit&>(unit_);
//   *separablePrefix_ = 
//      text_.substr(u.verbPrefixSeparablePosition(), 
//                   u.verbPrefixSeparableLength());
//   *inseparablePrefix_ = 
//      text_.substr(u.verbPrefixInseparablePosition(),
//                   u.verbPrefixInseparableLength()  );
//   int prefixLength = 
//      separablePrefix_->length() + inseparablePrefix_->length();
//   
//   *stem_ = text_.substr(prefixLength);
//}
