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
#ifndef __GMatchCache_h__
#define __GMatchCache_h__

//-------------------------------------------------------------------
// File - GMatchCache.h
//
// Class - GMatchCache (interface)
//
// Patterns
//
//-------------------------------------------------------------------

#include <logos_libs/entity/dwordphrase.h>

//typedef map< LgsString, DWordPhrase, less<LgsString> > MatchMap;
typedef LgsMap(LgsString, DWordPhraseVector*) MatchMap;

class GMatchCache
{
public:
   //---------------------------------------------------------------
   // Destructor.
   //---------------------------------------------------------------

   GMatchCache();
   virtual ~GMatchCache();

   DWordPhraseVector* FindMatch(const LgsString&, bool BOS) const;
   void InsertMatch(const LgsString&, DWordPhraseVector*, bool BOS);
   void CleanupMemory();
   int Count();

private:
   MatchMap v_matches;
   MatchMap v_matchesBOS;
   DWordPhraseVector* p_emptyVector;
};
//---------------------------------------------------------------------
inline int GMatchCache::Count()
{
   return v_matches.size() + v_matchesBOS.size();
}
//---------------------------------------------------------------------
inline void GMatchCache::InsertMatch(const LgsString& s, DWordPhraseVector* m, bool BOS)
{
   if (BOS)
   {
      v_matchesBOS[s] = m;
   }
   else
   {
      v_matches[s] = m;
   }
}
//---------------------------------------------------------------------
inline DWordPhraseVector* GMatchCache::FindMatch(const LgsString& s, bool BOS) const
{
   DWordPhraseVector* p = p_emptyVector;

   if (BOS)
   {
      MatchMap::const_iterator m = v_matchesBOS.find(s);
      if (m != v_matchesBOS.end())
      {
         p = (*m).second;
      }
   }
   else
   {
      MatchMap::const_iterator m = v_matches.find(s);
      if (m != v_matches.end())
      {
         p = (*m).second;
      }
   }

   return p;
}


#endif // __GMatchCache_h__

