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
//---------------------------------------------------------------------
// File - GEntryCache.cpp
//
// Class - GEntryCache (implementation)
//
//---------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/gerdem/gentrycache.h>

//---------------------------------------------------------------------
GEntryCache::GEntryCache(int capacity)
            :v_capacity(capacity)
{
    p_entries = new EntryMap;
}
//---------------------------------------------------------------------
GEntryCache::~GEntryCache()
{
   delete p_entries;
}
//---------------------------------------------------------------------
void GEntryCache::CleanupMemory()
{
   for (EntryMap::iterator i = p_entries->begin(); i != p_entries->end(); i++)
   {
      LDictionaryEntry* pEntry = (*i).second;
      pEntry->deleteSsuComponents();
      delete pEntry;
   }
   p_entries->clear();
}
//---------------------------------------------------------------------
int GEntryCache::Count()
{
   // simply passes requiest on to internal object from STL.
   return p_entries->size();
}
//---------------------------------------------------------------------
LDictionaryEntry* GEntryCache::FindEntry(const LgsString& s) const
{
   // returns the first DictionaryEntry that is mapped to a LgsString
   // that is equal to the input arguement.
   EntryMap::const_iterator i = p_entries->find( s);

   if (i != p_entries->end())
   {
      return const_cast<LDictionaryEntry*>((*i).second);
   }
   return 0;
}
//---------------------------------------------------------------------
LDictionaryEntry* GEntryCache::FindEntry(LWordIterator begin, LWordIterator end, int wordCount) const
{
   // returns the first DictionaryEntry that is mapped to a LgsString
   // that is equal to the input arguement.
   char buffer[256] = "";

   int wordsIncluded = 0;
   for (LWordIterator i = begin; i != end; i++)
   {
      wordsIncluded++;
      if (wordsIncluded > wordCount)
      {
         break;
      }
      strcat(buffer, i->c_str());
      if ((i + 1) != end)
      {
         strcat(buffer, " ");
      }
   }
   LgsString s = buffer;
   EntryMap::const_iterator e = p_entries->find(s);

   if (e != p_entries->end())
   {
      return const_cast<LDictionaryEntry*>((*e).second);
   }
   return 0;
}
//---------------------------------------------------------------------
bool GEntryCache::InsertEntry(const LgsString& s, LDictionaryEntry* pEntry)
{
   // creates a mapping of a new Dictionary Entry to its key LgsString
   // and stores it in the cache.
   if (Count() < Capacity())
   {
      (*p_entries)[s] = pEntry;
      if (pEntry)
         pEntry->setCached(true);
      return true;
   }
   return false;
}
//---------------------------------------------------------------------
bool GEntryCache::InsertEntry(LWordIterator begin, LWordIterator end, LDictionaryEntry* pEntry)
{
   // creates a mapping of a new Dictionary Entry to its key LgsString
   // and stores it in the cache.
   char buffer[256] = "";

   for (LWordIterator i = begin; i != end; i++)
   {
      strcat( buffer, i->c_str());
      if ((i + 1) != end )
      {
         strcat(buffer, " ");
      }
   }
   if (Count() < Capacity())
   {
   LgsString s = buffer;
   (*p_entries)[s] = pEntry;
   pEntry->setCached(true);
   return true;
   }
   return false;
}
//---------------------------------------------------------------------
LDictionaryEntry* GEntryCache::FindEntry(const DWordPhraseVector* v) const
{
   // Uses another method within this object. Simply allows the
   // search to work with a whole vector of Word Matches instead of
   // a single one. This method merely wraps an iterator around
   // another method of the same name.
   LDictionaryEntry* p = 0;

   for (DWordPhraseVector::const_iterator i = v->begin(); i != v->end(); i++)
   {
      p = FindEntry(*i);
      if (p)
      {
         break;
      }
   }
   return p;
}
