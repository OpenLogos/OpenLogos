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
 *    DESCRIPTION:   Pat generation interface class
 *
 *    AUTHOR:        Vlad Yakimetz
 *
 *    HISTORY:       06/03/98
 *
 *******************************************************************/

#include "PatFinder.h"
#include "ConnectionManager.h"
#include "PatgenQuery.h"
#include "PatGeneratorImpl.h"


PatGeneratorImpl
::~PatGeneratorImpl()
{

}   


int 
PatGeneratorImpl
::openSession(const LgsString& word_, 
              const LgsString& langCd_, 
              const LgsString& wordClassCd_, 
              char          genderCd_, 
              char          numberCd_, 
              int           sepPrefixLen_, 
              int           insepPrefixLen_)
{
   ConnectionManager& cm = ConnectionManager::theCM();
   int hstat = _hstat;
   
   if (!_hstat && !(hstat = cm.openSession(_hconn, PatgenQuery())))
   {
      return 0;
   }

   try
   {
      Statement& st = cm.statement(hstat);
      
      st.data(PatgenQuery(langCd_, wordClassCd_, genderCd_, 
                          numberCd_, sepPrefixLen_, insepPrefixLen_));
      st.execDirect();
   }
   catch(...)
   {
      cm.closeSession(hstat);
      return 0;
   }
   
   _word   = word_.substr(sepPrefixLen_+insepPrefixLen_);
   _langCd = langCd_;

   return _hstat = hstat;
}


int 
PatGeneratorImpl
::generatePat(const LgsString& word_, 
              const LgsString& langCd_, 
              const LgsString& wordClassCd_, 
              char          genderCd_, 
              char          numberCd_, 
              int           sepPrefixLen_, 
              int           insepPrefixLen_)
{
   if (!_hstat && !openSession(word_, 
                               langCd_, 
                               wordClassCd_, 
                               genderCd_, 
                               numberCd_, 
                               sepPrefixLen_, 
                               insepPrefixLen_))
   {
      return 0;
   }

   int patNo = generatePat();
   closeSession(_hstat);
   
   return patNo;
}


int 
PatGeneratorImpl
::generatePat(int hstat_)
{
   if (_hstat) 
      hstat_ = _hstat;
   else if (!hstat_) 
      return 0;

   ConnectionManager& cm = ConnectionManager::theCM();

   try
   {
      Statement& st = cm.statement(hstat_);
      PatgenQuery& patgenQuery = st.result(PatgenQuery());
      PatFinder patFind(st.language(_langCd), _word);

      while (st.fetch()
         && !patFind.isBestMatch(patgenQuery.patNumber(), patgenQuery.ending()))
      {
      }

      return patFind.bestMatch();
   }
   catch(...)
   {
      return 0;
   }
}


void 
PatGeneratorImpl
::closeSession(int hstat_)
{
   if (_hstat) hstat_ = _hstat;
   ConnectionManager::theCM().closeSession(hstat_);
}   
