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
#ifndef __entrybuilder_h__
#define __entrybuilder_h__

//-----------------------------------------------------------------------------------
// File - entrybuilder.h
//
// Class - EntryBuilder (Abstract)
//
// Description - This is a part of an application of the "Builder" pattern. This is an
// abstract class. It primarily serves to make the Linguistic library independent of the
// Gerdem library or some other library that contains the strategy for matching words to
// the persistent dictionary. An object of this type is used by the Dictionary object to
// build a DictionaryEntry object given a specific word or an ordered group of words.
//-----------------------------------------------------------------------------------

#include <logos_libs/linguistic/lword.h>

class LDictionaryEntry;
class LDictionaryToken;
class LinguisticFactory;

class EntryBuilder
{
public:
   enum UnfoundTYPE
   {
      Unfound, AllCaps, AllCapsS, AllCapsApostS, BeginCap, BeginCapBOS, BeginCapS, BeginCapSBOS,
      BeginCapApostS, EndingS, EndingApostS, UnfoundAgent
   };

   //-----------------------------------------------------------------------------------
   // The destructor will perhaps do nothing for this class but it needs to be declared
   // virtual so that the delete of a pointer to this abstract class will cause the subclass's
   // destructor to be executed.
   //-----------------------------------------------------------------------------------
   virtual ~EntryBuilder();

   //-----------------------------------------------------------------------------------
   // builderEntry - This is the primary member of class. It returns a new DictionaryEntry
   //    that is based upon as many source words as possible. The beginSource iterator indicates
   //    the first source word that can be matched. The endSource iterator indicates the non-inclusive
   //    end of the source words. The matchEnd iterator is updated by the function and represents the
   //    non-inclusive end of the source words used in building the LDictionaryEntry object.
   //    Non-inclusive vector endings correspond to STL style.
   //
   // Unfortunately, the entry building process became more involved to handle special situation and
   // hence we have the following methods.
   //
   // buildProtectedEntry() - returns a protected entry. The "build" prefix suggests to the user that
   //    it is the new owner of an allocated object and is responsible for deleting the same.
   // buildPunctuationEosEntry() - returns a normal EOS entry for punctuation that terminates a sentence.
   //    The "build" prefix suggests to the user that it is the new owner of an allocated object and is
   //    responsible for deleting the same.
   // buildSpecialEntry() - returns a "special" entry such as dates, numbers, etc. The "build" prefix
   //    suggests to the user that it is the new owner of an allocated object and is responsible for
   //    deleting the same.
   // bosEntry() - returns a BOS entry.
   // eosEntry() - returms an EOS entry.
   // unfoundEntry() - returns an entry for an unfound word.
   // unfoundCapEntry() - returns an entry for an unfound word that begins with a capital.
   // ordinalEntry() - returns an entry for an ordinal number.
   // alphanumericEntry() - returns an entry for an alphanumeric token.
   // createTempBosEntry() - produces a BOS for a sentence that is being split.
   // createTempEosEntry() - produces an EOS for a sentence that is being split.
   //-----------------------------------------------------------------------------------

   virtual LDictionaryEntry* buildEntry(const LWordIterator& begin, const LWordIterator& end,
                                        bool BOS, LWordVector * srcWrdVector, bool bAllCapitalWords) = 0;
   virtual LDictionaryEntry* buildProtectedEntry(const LWord&) = 0;
   virtual LDictionaryEntry* buildProtectedWordEntry(const LWordIterator& word, LgsString& protectedString) = 0;
   virtual LDictionaryEntry* buildPunctuationEosEntry(const LWord&) = 0;
   virtual LDictionaryEntry* buildSpecialEntry(const LWordIterator& begin, const LWordIterator& end) = 0;

   virtual LDictionaryEntry* bosEntry() = 0;
   virtual LDictionaryEntry* eosEntry() = 0;
   virtual LDictionaryEntry* unfoundEntry(UnfoundTYPE typeOfUnfound, const LWord & word) = 0;

   virtual LDictionaryEntry* createTempBosEntry(const LgsString&) = 0;
   virtual LDictionaryEntry* createTempEosEntry(const LgsString&) = 0;

   virtual LinguisticFactory& factory() = 0;
   virtual void setFactory(LinguisticFactory*) = 0;
   virtual void initializeWordGroupManager(LWordVector * srcWords) = 0;
   virtual void updateWordPosition(int wordCount, LWordIterator startSeq) = 0;

protected:
    //---------------------------------------------------------------
    // Default constructor is protected. This is not necessary, but
    // it is more information to the user's of this class. The class
    // is abstract and needs no public constructors.
    //---------------------------------------------------------------
    EntryBuilder();
};

#endif // __entrybuilder_h__

