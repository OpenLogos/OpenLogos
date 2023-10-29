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
 *    DESCRIPTION:   Contains all information specific to PAT 
 *                   generation process.
 *
 *    AUTHOR:        Vlad Yakimetz
 *
 *    HISTORY:       06/02/98
 *
 *******************************************************************/

#include <logos_include/logoscommon.h>
#include <logos_libs/sql/sqlconnection.h>
#include <logos_libs/sql/sqlcolumn.h>
#include "PatgenQuery.h"


PatgenQuery
::PatgenQuery(const LgsString&  langCd_, 
              const LgsString&  wordClassCd_,  
              char           genderCd_,  
              char           numberCd_,  
              int            sepPrefixLen_,  
              int            insepPrefixLen_)
   : _langCd        (langCd_        ),          
     _wordClassCd   (wordClassCd_   ),     
     _genderCd      (genderCd_      ),     
     _numberCd      (numberCd_      ),     
     _sepPrefixLen  (sepPrefixLen_  ),     
     _insepPrefixLen(insepPrefixLen_),   
     _patNo(0), _ending(0) 
{
   _makeQryString();
}


PatgenQuery
::PatgenQuery(const PatgenQuery& pq_)
{
   _langCd         = pq_._langCd        ;          
   _wordClassCd    = pq_._wordClassCd   ;     
   _genderCd       = pq_._genderCd      ;     
   _numberCd       = pq_._numberCd      ;     
   _sepPrefixLen   = pq_._sepPrefixLen  ;     
   _insepPrefixLen = pq_._insepPrefixLen;   
   
   _qryString      = pq_._qryString     ;   
   
   _patNo          = 0;
   _ending         = 0;
}  


PatgenQuery
::~PatgenQuery() 
{
	_patNo  = 0;
	_ending = 0;
}


PatgenQuery& 
PatgenQuery
::operator=(const PatgenQuery& pq_)
{
   if (&pq_ != this)       
   {
      _langCd         = pq_._langCd        ;          
      _wordClassCd    = pq_._wordClassCd   ;     
      _genderCd       = pq_._genderCd      ;     
      _numberCd       = pq_._numberCd      ;     
      _sepPrefixLen   = pq_._sepPrefixLen  ;     
      _insepPrefixLen = pq_._insepPrefixLen;   
      
      _qryString      = pq_._qryString     ;   
      
      _patNo          = 0;
      _ending         = 0;
   }
  
   return *this;
}  


void 
PatgenQuery
::bind(Statement& st_)
{
   _patNo  = st_.stmt().BindOutputColumn(1, SqlColumn::Integer)   ;  
   _ending = st_.stmt().BindOutputColumn(2, SqlColumn::StringType);  
}


void
PatgenQuery
::_makeQryString()
{
   _qryString =
      "select "
         "pat_number, ending "
      "from "
         "stem_generation_pat "
      "where "
         "language_code = '" + _langCd + "' "
         "and " +
         "word_class_code = '" + _wordClassCd + "' "
   ;
   
   if (!_sepPrefixLen && !_insepPrefixLen)
   {
      _qryString += 
         "and "
         "prefix_pattern is null "
      ;
   }
   else if (!_insepPrefixLen)
   {
      _qryString += 
         "and "
         "prefix_pattern = '&-' "
      ;
   }
   else if (!_sepPrefixLen)
   {
      _qryString += 
         "and "
         "prefix_pattern = '&+' "
      ;
   }
   else
   {
      _qryString += 
         "and "
         "(prefix_pattern = '&-&+' or prefix_pattern = '&+&-') "
      ;
   }
   
   // Gender and Number make sense only for nouns (wcc of '01') where
   // Gender is null only for English and Number is never null.
   // Currently the Number field in DB is not populated correctly.
   
   if (_wordClassCd != "01")
   {
      _qryString += 
         "and "
         "gender_code is null "
         //"and "
         //"number_code is null "
      ;
   }
   else
   {
      if (!_genderCd)
      {
         _qryString += 
         "and "
         "gender_code is null "
         ;
      }
      else
      {
         _qryString +=
         "and "
         "gender_code = '"
         ;
         _qryString += _genderCd;
         _qryString += "' ";
      }
      
      //_qryString += 
      //   "and "
      //   "number_code = '"
      //;
      //_qryString += _numberCd;
      //_qryString += "' ";
   }
      
   _qryString +=
      "order by "
         "pat_number, ending"
   ;
}


int 
PatgenQuery
::patNumber() 
{
   return _patNo->AsInteger();
}


LgsString 
PatgenQuery
::ending() 
{
   return _ending->AsString();
}
