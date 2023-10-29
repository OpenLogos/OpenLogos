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
// NonInflectingWordCache.cpp: implementation of the NonInflectingWordCache class.
//
//////////////////////////////////////////////////////////////////////

#include <logos_include/logoscommon.h>
#include <logos_libs/translutility/translcommonobjects.h>
#include <logos_libs/linguistic/noninflectingwordcache.h>
#include <configdatafileinterface/configdatainterfacemain.h>

// Static member
NonInflectingWordCache *NonInflectingWordCache::m_pNonInflectingWordCache = 0;

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

NonInflectingWordCache::NonInflectingWordCache()
{
   Load();
}
//---------------------------------------------------------------------
NonInflectingWordCache::~NonInflectingWordCache()
{
   if (!m_List.empty())
   {
      m_List.clear();
   }
}
//---------------------------------------------------------------------
void NonInflectingWordCache::DestroyObject()
{
   if (m_pNonInflectingWordCache)
   {
      delete m_pNonInflectingWordCache;
      m_pNonInflectingWordCache = 0;
   }
}
//---------------------------------------------------------------------
NonInflectingWordCache *NonInflectingWordCache::GetObject()
{
   if (!m_pNonInflectingWordCache)
   {
      m_pNonInflectingWordCache = new NonInflectingWordCache;
   }

   return m_pNonInflectingWordCache;
}
//---------------------------------------------------------------------
void NonInflectingWordCache::Load()
{
   char wordlistFile[MAX_FILEPATH_LEN];

   GetConfigData("sourcedata", "noninflecting_wordlist", wordlistFile, MAX_FILEPATH_LEN);

   // Open and read file
   ifstream stream(wordlistFile);
   if (stream.good())
   {   
      // Clear list
      m_List.clear();

      LgsString str;
      while (!stream.eof())
      {
         stream >> str;
         if (!str.empty())
         {
            m_List.insert(str);
         }
      }
   }
}
//---------------------------------------------------------------------
bool NonInflectingWordCache::inCache(const LWord &aWord) const
{
   if (aWord.language()->id() == TranslCommonObjects::GetSourceLanguage()->id() &&
       !m_List.empty())
   {
      // Do search
      if (m_List.find(aWord) != m_List.end())
      {
         return true; // found => it CAN NOT be rooted
      }
   }

   return false; // not found => it can be rooted
}
