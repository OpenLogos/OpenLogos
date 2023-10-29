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

#include <stdio.h>
#include <logos_include/lgsstring.h>
#include <ostream>
#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/llanguage.h>

#include <lgs_stemgen/stemreplacerule.h>
#include <lgs_stemgen/prefixrule.h>
#include <lgs_stemgen/stemgeneratorrules.h>
#include <lgs_stemgen/stemgenerator.h>


StemGenerator::StemGenerator(const LLanguage& language_)
              : _language(language_)
{
}

StemGenerator::~StemGenerator()
{
}


void StemGenerator::applyRules(LgsString* text_, const StemGeneratorRules& rules_,
                               int wordClassCd)
{
   if ((_language.id() == LLanguage::GermanID) && (wordClassCd == 2))
   {
      _dropPrefixes(text_, rules_);
   }
   LgsString stem = *text_;
   text_->erase();
   
   _dropEnding(&stem, rules_.dropEnding);
   _replaceRule(rules_.replaceRule, &stem);
   _addPrefix(text_, rules_.addPrefix, stem);
   _addEnding(text_, rules_.addEnding, rules_.auxiliaryCode);
   _makeSpaces(text_);
}


void StemGenerator::_dropPrefixes(LgsString* text_, const StemGeneratorRules& rules_)
{
   _separablePrefix.erase();
   _inseparablePrefix.erase();

   if (!rules_.sepPrefixLen && !rules_.insepPrefixLen) return;
   
   LgsString::size_type size = text_->size();
   
   if (rules_.sepPrefixPos + rules_.sepPrefixLen > size ||
       rules_.insepPrefixPos + rules_.insepPrefixLen > size)
   {
      return;
   }
   
   LgsString str;
   
   // prefix position is 0-based
   if (!rules_.insepPrefixLen)
   {
      str = text_->substr(0, rules_.sepPrefixPos);
      _separablePrefix = text_->substr(rules_.sepPrefixPos, rules_.sepPrefixLen);
      *text_ = str + text_->substr(rules_.sepPrefixPos + rules_.sepPrefixLen);
   }
   else if (!rules_.sepPrefixLen)
   {
      str = text_->substr(0, rules_.insepPrefixPos);
      _inseparablePrefix = text_->substr(rules_.insepPrefixPos, rules_.insepPrefixLen);
      *text_ = str + text_->substr(rules_.insepPrefixPos + rules_.insepPrefixLen);
   }
   else if (rules_.sepPrefixPos < rules_.insepPrefixPos)
   {
      str = text_->substr(0, rules_.sepPrefixPos);
      _separablePrefix = text_->substr(rules_.sepPrefixPos, rules_.sepPrefixLen);
      str += text_->substr(rules_.sepPrefixPos + rules_.sepPrefixLen,
                           rules_.insepPrefixPos - rules_.sepPrefixPos - rules_.sepPrefixLen);
      _inseparablePrefix = text_->substr(rules_.insepPrefixPos, rules_.insepPrefixLen);
      *text_ = str + text_->substr(rules_.insepPrefixPos + rules_.insepPrefixLen);
   } 
   else
   {
      str = text_->substr(0, rules_.insepPrefixPos);
      _inseparablePrefix = text_->substr(rules_.insepPrefixPos, rules_.insepPrefixLen);
      str += text_->substr(rules_.insepPrefixPos + rules_.insepPrefixLen,
                           rules_.sepPrefixPos - rules_.insepPrefixPos - rules_.insepPrefixLen);
      _separablePrefix = text_->substr(rules_.sepPrefixPos, rules_.sepPrefixLen);
      *text_ = str + text_->substr(rules_.sepPrefixPos + rules_.sepPrefixLen);
   }
}   


void StemGenerator::_dropEnding(LgsString* stem_, const LgsString& rule_)
{
   if (!stem_->empty() && !rule_.empty())
   {
      int n;

      if (n = atoi(rule_.c_str()))
      {
         stem_->resize(stem_->size() > n ? stem_->size() - n : 0);
      }
      else if (rule_[0] == '@')
      {
         stem_->erase();
      }
      else 
      {
         int pos = stem_->size() - rule_.size();
         
         if (pos >= 0 &&
             // _fix_me_
             // I hope the destructive change in the string is not intended.
             // This would be highly illegal.
             !strcmp(_strlwr(const_cast<char*>(stem_->substr(pos).c_str())),
                     _strlwr(const_cast<char*>(rule_.c_str())))
             )
         {
            stem_->resize(pos);
         }
      }
   }
}


void StemGenerator::_replaceRule(const LgsString& replaceRuleString_, LgsString* pStem_)
{
   StemReplaceRule(replaceRuleString_, _language).fire(pStem_);
}   


void StemGenerator::_addPrefix(LgsString* text_, const LgsString& prefixRuleString_, 
                               const LgsString& stem_)
{
   PrefixRule(prefixRuleString_, _separablePrefix, _inseparablePrefix, _language).fire(text_, stem_);
}   


void StemGenerator::_addEnding(LgsString* text_, const LgsString& rule_, char auxCd_)
{
   LgsString::size_type pos = rule_.find_first_of('&');
   
   if (pos == LgsString::npos)
   {
      *text_ += rule_;
   }
   else
   {
      *text_ += rule_.substr(0, pos);
      
      if (_language.id() == LLanguage::GermanID)
      {
         if (auxCd_ == '1')
            *text_ += "haben";
         else /*auxCd_ == '2'*/   
            *text_ += "sein";
      }
         
      pos = rule_.find_first_of(' ', pos);   
      
      if (pos != LgsString::npos)
      {
         pos = rule_.find_first_not_of(' ', pos);   
      
         if (pos != LgsString::npos)
         {
            *text_ += " ";
            *text_ += rule_.substr(pos);
         }
      }
   }
}


void StemGenerator::_makeSpaces(LgsString* text_)
{
   for (int i = 0; i < text_->length(); i++)
   {
      if ((*text_)[i] == '_')
      {
         (*text_)[i] = ' ';
      }
   }
}
