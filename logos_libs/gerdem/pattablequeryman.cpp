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

#include <logos_include/logoscommon.h>
#include <logos_libs/sql/sqlstatement.h>
#include <logos_libs/sql/sqlcolumn.h>
#include <logos_libs/sql/sqlexception.h>
#include <logos_libs/linguistic/llanguage.h>
#include <logos_libs/sql/sqlquery.h>
#include <logos_libs/linguistic/targetsentenceunit.h>
#include <lgs_db_io/pattablequery.h>

#include <logos_libs/gerdem/pattablequeryman.h>

const int WORDCLASS2_WITH_GENDER = 100;  // exception for wordclass 2
const int GERMANNOUN_WITH_GENDER = 200;  // exception for wordclass 1, target German
const int ITALIANNOUN_WITH_GENDER = 300;  // exception for wordclass 1, target Italian


PatTableQueryManager
::PatTableQueryManager(SqlConnection* pConnection_, 
                       const LLanguage& language_)
{
   _open(pConnection_, language_.id());
}

PatTableQueryManager::~PatTableQueryManager()
{
   for (PatTableQueryMap::iterator i = _ptqMap.begin();
      i != _ptqMap.end(); i++)
   {
      i->second->Close();
      delete i->second;
      i->second = 0;
   }
}

void PatTableQueryManager::_open(SqlConnection* pConnection_, int langCode_)
{
   switch (langCode_)
   {
      case 1 : _languageCode = "01"; break;
      case 2 : _languageCode = "02"; break;
      case 3 : _languageCode = "03"; break;
      case 4 : _languageCode = "04"; break;
      case 5 : _languageCode = "05"; break;
      case 6 : _languageCode = "06"; break;
      default: throw (LgsString ("illegal language Code"));
   }

    // this is an ugly kludge
    struct PatStruct
    {
       int language;
       int wordClass;
       int gender;
       int number;
       int caseCode;
       int tense;
       int person;
       int degree;
       int declension;
    } 
    patMap[] = { 
      { 1,  1, 0, 5, 7, 0, 0, 0, 0 },
      { 2,  1, 0, 5, 7, 0, 0, 0, 0 },
      { 3,  1, 0, 5, 7, 0, 0, 0, 0 },
      { 4,  1, 0, 5, 7, 0, 0, 0, 0 },
      { 5,  1, 0, 5, 7, 0, 0, 0, 0 },
      { 6,  1, 0, 5, 7, 0, 0, 0, 0 },
 
      { 1,  2, 4, 5, 0, 7, 6, 0, 0 },
      { 2,  2, 0, 5, 0, 7, 6, 0, 0 },
      { 3,  2, 4, 5, 0, 7, 6, 0, 0 },
      { 4,  2, 4, 5, 0, 7, 6, 0, 0 },
      { 5,  2, 4, 5, 0, 7, 6, 0, 0 },
      { 6,  2, 4, 5, 0, 7, 6, 0, 0 },

      { 1,  3, 0, 0, 0, 0, 0,12, 0 },
      { 2,  3, 0, 0, 0, 0, 0, 7, 0 },
      { 4,  3, 0, 0, 0, 0, 0, 7, 0 },
      { 5,  3, 0, 0, 0, 0, 0, 7, 0 },
      { 6,  3, 0, 0, 0, 0, 0, 7, 0 },

      { 1,  4, 4, 5, 7, 0, 0,12, 3 },
      { 2,  4, 0, 5, 0, 0, 0, 7, 0 },
      { 3,  4, 4, 5, 0, 0, 0, 7, 0 },
      { 4,  4, 4, 5, 0, 0, 0, 7, 0 },
      { 5,  4, 4, 5, 0, 0, 0, 7, 0 },
      { 6,  4, 4, 5, 0, 0, 0, 7, 0 },

      { 1,  5, 4, 5, 7, 0, 6, 0, 0 },
      { 2,  5, 4, 5, 7, 0, 6, 0, 0 },
      { 3,  5, 4, 5, 7, 0, 6, 0, 0 },
      { 4,  5, 4, 5, 7, 0, 6, 0, 0 },
      { 5,  5, 4, 5, 7, 0, 6, 0, 0 },
      { 6,  5, 4, 5, 7, 0, 6, 0, 0 },

      { 1,  6, 0, 0, 0, 0, 0,12, 0 },
      { 2,  6, 0, 0, 0, 0, 0, 7, 0 },
      { 4,  6, 0, 0, 0, 0, 0, 7, 0 },
      { 5,  6, 0, 0, 0, 0, 0, 7, 0 },
      { 6,  6, 0, 0, 0, 0, 0, 7, 0 },

      { 1, 14, 4, 5, 7, 0, 0, 0, 0 },
      { 2, 14, 0, 5, 0, 0, 0, 0, 0 },
      { 3, 14, 4, 5, 0, 0, 0, 0, 0 },
      { 4, 14, 4, 5, 0, 0, 0, 0, 0 },
      { 5, 14, 4, 5, 0, 0, 0, 0, 0 },
      { 6, 14, 4, 5, 0, 0, 0, 0, 0 },

      { 1, 15, 4, 5, 7, 0, 0, 0, 0 },
      { 2, 15, 0, 5, 0, 0, 0, 0, 0 },
      { 3, 15, 4, 5, 0, 0, 0, 0, 0 },
      { 4, 15, 4, 5, 0, 0, 0, 0, 0 },
      { 5, 15, 4, 5, 0, 0, 0, 0, 0 },
      { 6, 15, 4, 5, 0, 0, 0, 0, 0 },

      { 1, 16, 4, 5, 7, 0, 0, 0, 0 },
      { 2, 16, 0, 5, 0, 0, 0, 0, 0 },
      { 3, 16, 4, 5, 0, 0, 0, 0, 0 },
      { 4, 16, 4, 5, 0, 0, 0, 0, 0 },
      { 5, 16, 4, 5, 0, 0, 0, 0, 0 },
      { 6, 16, 4, 5, 0, 0, 0, 0, 0 },

      { 1, 18, 4, 5, 7, 0, 0, 0, 0 },
      { 2, 18, 0, 5, 7, 0, 0, 0, 0 },
      { 3, 18, 4, 5, 7, 0, 0, 0, 0 },
      { 4, 18, 4, 5, 7, 0, 0, 0, 0 },
      { 5, 18, 4, 5, 7, 0, 0, 0, 0 },
      { 6, 18, 4, 5, 0, 0, 0, 0, 0 }

   };

   for (int i = 0; i < sizeof(patMap)/sizeof(PatStruct); i++)
   {
      if (patMap[i].language != langCode_)
         continue;
      
      PatTableQuery* ptq = new PatTableQuery
      (
         _languageCode,
         (patMap[i].declension ?  PatTableQuery::DECLENSION_BIT : 0) |
         (patMap[i].gender     ?  PatTableQuery::GENDER_BIT     : 0) |
         (patMap[i].number     ?  PatTableQuery::NUMBER_BIT     : 0) |
         (patMap[i].person     ?  PatTableQuery::PERSON_BIT     : 0) |
         (patMap[i].caseCode   ?  PatTableQuery::CASE_BIT       : 0) |
         (patMap[i].tense      ?  PatTableQuery::TENSE_BIT      : 0) |
         (patMap[i].degree==7  ?  PatTableQuery::DEGREE7_BIT    : 0) |
         (patMap[i].degree==12 ?  PatTableQuery::DEGREE12_BIT   : 0)
      );

      ptq->Open(pConnection_);

      int wordClass = patMap[i].wordClass;

      if (wordClass == LLanguage::VERB)  // WordClass 2 exception with gender
      {
         wordClass = WORDCLASS2_WITH_GENDER;
      }
      _insertQuery(wordClass, ptq);

      if (patMap[i].wordClass == LLanguage::VERB)  // WordClass 2 exception without gender
      {
         ptq = new PatTableQuery
         (
            _languageCode,
            ((patMap[i].declension) ?  PatTableQuery::DECLENSION_BIT : 0) |
            ((patMap[i].number    ) ?  PatTableQuery::NUMBER_BIT : 0) |
            ((patMap[i].person    ) ?  PatTableQuery::PERSON_BIT : 0) |
            ((patMap[i].caseCode  ) ?  PatTableQuery::CASE_BIT : 0) |
            ((patMap[i].tense     ) ?  PatTableQuery::TENSE_BIT : 0) |
            ((patMap[i].degree==7 ) ?  PatTableQuery::DEGREE7_BIT : 0) |
            ((patMap[i].degree==12) ?  PatTableQuery::DEGREE12_BIT : 0)
         );

         if (!ptq)
            continue;

         ptq->Open(pConnection_);
         _insertQuery(LLanguage::VERB, ptq);
      }

      // Special case for German with noun wordclass
      if ((langCode_ == LLanguage::GermanID) && (wordClass == LLanguage::NOUN))
      {
         ptq = new PatTableQuery
         (
            _languageCode,
            (patMap[i].declension ?  PatTableQuery::DECLENSION_BIT : 0) |
            PatTableQuery::GENDER_BIT |
            (patMap[i].number     ?  PatTableQuery::NUMBER_BIT     : 0) |
            (patMap[i].person     ?  PatTableQuery::PERSON_BIT     : 0) |
            (patMap[i].caseCode   ?  PatTableQuery::CASE_BIT       : 0) |
            (patMap[i].tense      ?  PatTableQuery::TENSE_BIT      : 0) |
            (patMap[i].degree==7  ?  PatTableQuery::DEGREE7_BIT    : 0) |
            (patMap[i].degree==12 ?  PatTableQuery::DEGREE12_BIT   : 0)
         );

         if (!ptq)
            continue;

         ptq->Open(pConnection_);
         _insertQuery(GERMANNOUN_WITH_GENDER, ptq);
      }

      // Special case for Italian with noun wordclass
      if ((langCode_ == LLanguage::ItalianID) && (wordClass == LLanguage::NOUN))
      {
         ptq = new PatTableQuery
         (
            _languageCode,
            (patMap[i].declension ?  PatTableQuery::DECLENSION_BIT : 0) |
            PatTableQuery::GENDER_BIT |
            (patMap[i].number     ?  PatTableQuery::NUMBER_BIT     : 0) |
            (patMap[i].person     ?  PatTableQuery::PERSON_BIT     : 0) |
            (patMap[i].caseCode   ?  PatTableQuery::CASE_BIT       : 0) |
            (patMap[i].tense      ?  PatTableQuery::TENSE_BIT      : 0) |
            (patMap[i].degree==7  ?  PatTableQuery::DEGREE7_BIT    : 0) |
            (patMap[i].degree==12 ?  PatTableQuery::DEGREE12_BIT   : 0)
         );

         if (!ptq)
            continue;

         ptq->Open(pConnection_);
         _insertQuery(ITALIANNOUN_WITH_GENDER, ptq);
      }
   }
}


int PatTableQueryManager::Execute(const TargetSentenceUnit& unit_)
{
   return Execute(unit_.patNumber(), unit_.wordClassCode(), unit_, false);
}


int PatTableQueryManager::Execute(int patNumber_, int wordClassCode_,
                                  const TargetSentenceUnit& unit_, bool fromPhrase)
{
   int degree_ = unit_.degree();
   int declension_ = unit_.declension();

   // Gender is included in stem generation only for German target with these pat numbers
   if (wordClassCode_ == LLanguage::NOUN)
   {
      if ((_languageCode == "01") && ((patNumber_ == 52) || (patNumber_ == 55) ||
                                      (patNumber_ == 56) || (patNumber_ == 63) ||
                                      (patNumber_ == 75)))
      {
         wordClassCode_ = GERMANNOUN_WITH_GENDER;
      }
      else if ((_languageCode == "05") && ((patNumber_ == 96) || (patNumber_ == 99) ||
                                           (patNumber_ == 164) || (patNumber_ == 165) ||
                                           (patNumber_ == 194)))
      {
         wordClassCode_ = ITALIANNOUN_WITH_GENDER;
      }
   }
   else
   {
      if (wordClassCode_ == LLanguage::VERB)
      {
         if (_languageCode == "01") // german
         {
            switch (unit_.tense())
            {
               case 14:
               case 15:
               case 16:
               case 17:
                  wordClassCode_ = WORDCLASS2_WITH_GENDER;
                  break;
            }
         }
         else if(_languageCode == "03" ||  // french
                 _languageCode == "04" ||  // spanish
                 _languageCode == "05")    // italian
         {
            switch (unit_.tense())
            {
               case 10:
               case 11:
                  wordClassCode_ = WORDCLASS2_WITH_GENDER;
                  break;
            }
         }
         else if (_languageCode == "06")  // portuguese
         {
            switch (unit_.tense())
            {
            case 12:
            case 13:
               wordClassCode_ = WORDCLASS2_WITH_GENDER;
               break;
            }
         }
      }
   }
   
   // If the unit is an adjective in an adjective noun phrase with a target
   // language of German, then the degree and declension must be set according
   // to the Pat Table Criteria
   if ((_languageCode == "01") && (wordClassCode_ == LLanguage::ADJECTIVE) &&
       (unit_.wordClassCode() == LLanguage::NOUN))
   {
      degree_ = 1;

      if ((declension_ != 3) && (declension_ != 6) && (declension_ != 9))
         declension_ = 3;
   }

   // For all adverbs and adjectives in a phrase for all target languages,
   // the degree is set to one.
   if (fromPhrase && ((wordClassCode_ == LLanguage::ADVERB_LOCATIVE) || (wordClassCode_ == LLanguage::ADJECTIVE) ||
       (wordClassCode_ == LLanguage::ADVERB)))
   {
      degree_ = 1;
   }

   _pQuery = _findQuery(wordClassCode_);

   return !_pQuery ? -1 : _pQuery->Execute(patNumber_, degree_, declension_, unit_);
}


bool PatTableQueryManager::Fetch(int& stemNumber_, LgsString& ending_)
{
   bool result = false;

   if(_pQuery)
   {
      try
      {
         result = _pQuery->Fetch(stemNumber_, ending_);
      }
      catch(SqlException& x)
      {
         cout << x.Message() << endl;
         throw;
      }
   }
   
   return result;
}

int PatTableQueryManager::_count()
{
   return _ptqMap.size();
}

void PatTableQueryManager::_insertQuery(int wordClass_, PatTableQuery *ptq_)
{
   _ptqMap[wordClass_] = ptq_;
}

PatTableQuery* PatTableQueryManager::_findQuery(int wordClass_) const
{
   PatTableQueryMap::const_iterator i = _ptqMap.find(wordClass_);

   return i == _ptqMap.end() ? NULL : (*i).second;
}

