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
 *    DESCRIPTION:   Contains all information specific to Stem 
 *                   generation process.
 *
 *    AUTHOR:        Vlad Yakimetz
 *
 *    HISTORY:       06/02/98
 *
 *******************************************************************/

#include <logos_include/logoscommon.h>
#include <logos_libs/sql/sqlstatement.h>
#include <logos_libs/sql/sqlcolumn.h>
#include <lgs_stemgen/stemgeneratorrules.h>
#include "StemgenQuery.h"


StemgenQuery::StemgenQuery(const LgsString& word_,
                           const LgsString& langCd_,
                           int wordClassCd_,
                           int patNo_,
                           char auxiliaryCode_,
                           int sepPrefixPos_,
                           int sepPrefixLen_,
                           int insepPrefixPos_,
                           int insepPrefixLen_)
   : _word(word_),
     _langCd(langCd_),
     _wordClassCd(wordClassCd_),
     _patNo(patNo_),
     _auxiliaryCode(auxiliaryCode_),
     _sepPrefixPos(sepPrefixPos_),
     _sepPrefixLen(sepPrefixLen_),
     _insepPrefixPos(insepPrefixPos_),
     _insepPrefixLen(insepPrefixLen_),
     _rules(0),
     _dropEnding(0),
     _replaceRule(0),
     _addPrefix(0),
     _addEnding(0) 
{
   _makeQryString();
   
   try 
   {
      _rules = new StemGeneratorRules();
   }
   catch (bad_alloc)
   {
      _rules = 0;
   }
}


StemgenQuery::StemgenQuery(const StemgenQuery& sq_)
{
   _word = sq_._word;
   _langCd = sq_._langCd;
   _wordClassCd = sq_._wordClassCd;
   _patNo = sq_._patNo;
   _auxiliaryCode = sq_._auxiliaryCode;
   _sepPrefixPos = sq_._sepPrefixPos;
   _sepPrefixLen = sq_._sepPrefixLen;
   _insepPrefixPos = sq_._insepPrefixPos;
   _insepPrefixLen = sq_._insepPrefixLen;
   
   _qryString = sq_._qryString;
   
   delete _rules; _rules = 0;
   
   try 
   {
      _rules = new StemGeneratorRules();
   }
   catch (bad_alloc)
   {
      _rules = 0;
   }
   
   _dropEnding = 0;
   _replaceRule = 0;
   _addPrefix = 0;
   _addEnding = 0;
}  


StemgenQuery::~StemgenQuery() 
{
   delete _rules;
   _rules = 0;
   
   _dropEnding = 0;
   _replaceRule = 0;
   _addPrefix = 0;
   _addEnding = 0;
}


StemgenQuery& StemgenQuery::operator=(const StemgenQuery& sq_)
{
   if (&sq_ != this)
   {
      _qryString = sq_._qryString;
      
      _word = sq_._word;
      _langCd = sq_._langCd;
      _wordClassCd = sq_._wordClassCd;
      _patNo = sq_._patNo;
      _auxiliaryCode = sq_._auxiliaryCode;
      _sepPrefixPos = sq_._sepPrefixPos;
      _sepPrefixLen = sq_._sepPrefixLen;
      _insepPrefixPos = sq_._insepPrefixPos;
      _insepPrefixLen = sq_._insepPrefixLen;
      
      delete _rules;
      _rules = 0;
      
      try 
      {
         _rules = new StemGeneratorRules();
      }
      catch (bad_alloc)
      {
         _rules = 0;
      }
   
      _dropEnding = 0;
      _replaceRule = 0;
      _addPrefix = 0;
      _addEnding = 0;
  }
  
  return *this;
}  


void StemgenQuery::bind(Statement& st_)
{
   _dropEnding = st_.stmt().BindOutputColumn(1, SqlColumn::StringType);
   _replaceRule = st_.stmt().BindOutputColumn(2, SqlColumn::StringType);
   _addPrefix = st_.stmt().BindOutputColumn(3, SqlColumn::StringType);
   _addEnding = st_.stmt().BindOutputColumn(4, SqlColumn::StringType);
}


void StemgenQuery::_makeQryString()
{
   char buf[10];

   _qryString =
      "select "
         "drop_ending, replace_rule, add_prefix, add_ending "
      "from "
         "stem_generation_rule "
      "where "
         "language_code = '" + _langCd + "' "
         "and pat_number = " + itoa(_patNo, buf, 10) + 
      " "
      "order by "
         "stem_number"
   ;
}


bool StemgenQuery::updateRules()
{
   if (!_rules) return false;
   
   _rules->auxiliaryCode = _auxiliaryCode;
   _rules->sepPrefixPos = _sepPrefixPos > 0 ? _sepPrefixPos - 1 : 0;
   _rules->sepPrefixLen = _sepPrefixLen;
   _rules->insepPrefixPos = _insepPrefixPos > 0 ? _insepPrefixPos - 1 : 0;
   _rules->insepPrefixLen = _insepPrefixLen;
   
   _rules->dropEnding = _dropEnding->AsString();
   _rules->replaceRule = _replaceRule->AsString();
   _rules->addPrefix = _addPrefix->AsString();
   _rules->addEnding = _addEnding->AsString();
   
   return true;
}
