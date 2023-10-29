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
#ifndef __GEntryCache_h__
#define __GEntryCache_h__

//---------------------------------------------------------------------
// File - GEntryCache.h
//
// Class - EntryMap
// Class - GEntryCache (interface)
//
// Description - An object of this class contains fully constructed
//      Dictionary Entries. They are cached to that they do not have
//      to be rebuilt from the database.
//
//      The methods of this object allow for Dictionary Entries to be
//      entered and to be found.
//
//---------------------------------------------------------------------

#include <logos_libs/linguistic/ldictionaryentry.h>
#include <logos_libs/entity/dwordphrase.h>
#include <logos_libs/entity/dwordphrase.h>

//---------------------------------------------------------------------
class EntryMap: public LgsMap(LgsString, LDictionaryEntry*)
{
};

//---------------------------------------------------------------------
class GEntryCache
{
public:
   //-----------------------------------------------------------------
   // Constructor and Destructor.
   //-----------------------------------------------------------------

   GEntryCache(int capacity);
   virtual ~GEntryCache();

   //-----------------------------------------------------------------
   // FindEntry - these methods find a cached Dictionary Entry. They
   // return a null pointer in the event that none is found. Three
   // versions of FindEntry allow the search do be done on either a
   // LgsString, a WordMatch or an member of a vector of WordMatches.
   //-----------------------------------------------------------------

   LDictionaryEntry* FindEntry(const DWordPhraseVector*) const;
   LDictionaryEntry* FindEntry(const DWordPhrase&) const;
   LDictionaryEntry* FindEntry(const LgsString&) const;
   LDictionaryEntry* FindEntry(LWordIterator begin, LWordIterator end, int wordCount) const;

   //-----------------------------------------------------------------
   // InsertEntry - these methods enter a new Dictionary Entry into
   // the cache. The Dictionary Entry is mapped to a LgsString key that
   // represents the lexical value of the word or phrase.
   //-----------------------------------------------------------------
   bool InsertEntry(const LgsString&, LDictionaryEntry*);
   bool InsertEntry(LDictionaryEntry*);
   bool InsertEntry(LWordIterator begin, LWordIterator end, LDictionaryEntry* pEntry);

   //-----------------------------------------------------------------
   // returns the number of entries in the cache.
   //-----------------------------------------------------------------
   int Count();
   int Capacity() const;
   void SetCapacity(int);
   void CleanupMemory();

private:
   EntryMap* p_entries;
   int v_capacity;
};

//---------------------------------------------------------------------
inline int GEntryCache::Capacity() const
{
   // the maximum number of entries that can be cached.
   return v_capacity;
}
//---------------------------------------------------------------------
inline void GEntryCache::SetCapacity(int capacity)
{
   v_capacity = capacity;
}
//---------------------------------------------------------------------
inline LDictionaryEntry* GEntryCache::FindEntry(const DWordPhrase& wm) const
{
   // Making the best use of OOP, this method is based upon another
   // method in the same object. Just allows the user to work with a
   // different arguement.
   return FindEntry(wm.Word());
}
//---------------------------------------------------------------------
inline bool GEntryCache::InsertEntry(LDictionaryEntry* pEntry)
{
   // Uses another method to perform the insertion of a Dictionary
   // Entry and its key LgsString. However, first it obtains the key
   // LgsString from the Dictionary Entry itself.
   const LgsString& token = pEntry->dictionaryToken();

   return InsertEntry(token, pEntry);
}

#endif // __GEntryCache_h__

