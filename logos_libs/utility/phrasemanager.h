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
// File - PhraseManager.h

#ifndef __PhraseManager_h__
#define __PhraseManager_h__

#include <logos_libs/utility/charactervector.h>
#include <logos_libs/utility/phrase.h>

const char defaultSeparator = ' ';

class PhraseManagerStrategy;
class PhraseManagerInterface;

// final class
// manages a collection of phrases
// template <class Key>

typedef LgsVector(Phrase*) PhraseVector;
typedef PhraseVector::iterator PhraseIterator;
typedef PhraseVector::const_iterator PhraseConstIterator;

class PhraseManager
{
   friend class PhraseManagerInterface;
   
public:
   typedef int Key;

    // Constructor.
    // The separator is used as a space character,
    // and separates words within a phrase and phrases within a sentance.
    // The stragegy must be allocated on the heap, and after construction
    // is owned by this class, ie it will be deleted on destruction.
    PhraseManager(PhraseManagerStrategy* strategy = 0, char separator = defaultSeparator);

    ~PhraseManager();

    // Change the strategy in mid-flight, deleting the previous strategy.
    void changeStrategy(PhraseManagerStrategy* strategy);

    // Returns an interface to this class that can only invoke the distribute method of this class
    // The interface is owned by this class - i.e. it will be deleted on destruction.
    PhraseManagerInterface* GetPhraseManagerInterface() const;

    // Append phrase to list. The phrase must not be an empty LgsString.
    // The key is a unique key per phrase, used to later retrieve the phrase.
    // A given number of separators is appended.
    void appendPhrase(const LgsString& phrase, Key key, int trailingSpaces);

    // Returns true iff the phrase with the given key is still in the collection.
    // This method must be called before phraseChanged, getPhrase, or getPhrasePos is called
    // since the given phrase may have been deleted during a replace operation.
    bool phraseExists(Key key) const;

    // Whether the phrase with the given key has changed, since the construction or the last read.
    // It is not const since it changes the modification flag.
    // phraseExists must be called first, since the phrase may have been deleted during a replace operation.
    bool phraseChanged(Key key) const;

    // Retrieves the phrase with trailing spaces removed, given the given key.
    // If trailingSpaces is not null, the number of trailing spaces is returned in this parameter.
    // It is not const since the modification flag is cleared.
    // phraseExists must be called first, since the phrase may have been deleted during a replace operation.
    LgsString getPhrase(Key key, int* trailingSpaces = 0) const;

    // Retrieves the phrase including trailing spaces, given the given key.
    LgsString getFullPhrase(Key key) const;

    // Get the starting location of the phrase with the given key.
    // phraseExists must be called first, since the phrase may have been deleted during a replace operation.
    int getPhrasePos(Key key) const;

    // Return the key for the phrase at a given position.
    Key getPhraseAtPos(int pos);

    // Retrieves a concatenation of all phrases including trailing spaces for each phrase.
    LgsString getText() const;

    // returns the length of the LgsString getText()
    int getTextLength() const;

    // Replace the text (from getText()), with the given LgsString. As a side effect any phrases
    // affected are updated.
    // If a separator is removed phrases will be merged.
    void replace(int position, int length, const LgsString& replacement);

    void adjForMinReplace(int& pos, int& len, LgsString &replacement);

    // Update key with the key to the next phrase. Return false if the key is at the end of the collection.
    bool getNextKey(Key& key) const;

    // Update key with the key to the previous phrase. Return false if the key is at the start of the collection.
    bool getPrevKey(Key& key) const;

    // Merge the phrase at the given key together with the following (noOfPhrases - 1) phrases
    // int the leftmost phrase. The following phrases are then deleted
    void merge(Key key, int noOfPhrases);

    // go through the list of phrases - from the second phrase onwards and remove any leading spaces
    // for each space removed, increment the trailing space count of the preceding phrase
    // Note: this does not change the text of the buffer
    void moveLeadingSpaces();
    
    // erase all the phrases
    void erase();

private:
    
    struct LocationData
    {
        PhraseIterator startPhrase;  // starting phrase to modify
        PhraseIterator endPhrase;    // ending (one beyond) phrase to modify
        int startOffsetInPhrase;                // start offset of text within first matched phrase
        int endOffsetInPhrase;                  // (past) end offset of text in last (not past) matched phrase
    };

    char separator_;                            // separator between words and phrases
    PhraseManagerStrategy* strategy_;           // word redistribution strategy - owned by this class
    PhraseManagerInterface* interface_;         // interface to distribute words - owned by this class

    // collections
    PhraseVector phraseList_;                // list of phrases
    LgsMap(Key, Phrase*) lookup_;      // mapping between keys and phrases
    CharacterVector buffer_;                    // buffer for concatenated phrases

    // members updated during matching
    LocationData matchBounds_;                  // bounds of matching LgsString
    LgsStringVector replacementWords_;           // words within the replacement LgsString
    LgsIntVector wordCount_;                     // count of words in matched phrases
    int totalWordCount_;                        // sum of wordCount_

    // appends a LgsString to a character vector buffer
    static void appendString(const LgsString& s, CharacterVector& buffer);

    // looks up a phrase in the map given a key - throws an exception if not found
    Phrase* lookup(Key key) const;

    // appends a phrase to the buffer_ including trailing spaces
    void appendPhraseToBuffer(const LgsString& phrase, int trailingSpaces);

    // calculate the phrases that are affected in a replace operation
    void getBounds(int pos, int length);

    // replace the text in the buffer - without side effects
    void replaceText(int pos, int length, const LgsString& replacement);

    // return the starting and ending (one past) replacement offsets for a phrase
    int startPos(const PhraseConstIterator& current) const;
    int endPos(const PhraseConstIterator& current) const;

    // count the words in each affected sub-phrase, and remove affected sub-phrases
    void countAndDeleteWords();

    // remove the current phrase from the collection and delete the phrase
    void removePhrase(PhraseIterator& current);

    // remove any blank phrases from phraseList_ and lookup_
    void removeBlankPhrases();

    // Method used only by the phraseManagerStrategy via the interface_ member.
    // Distributes the given number of words from replacementWords_ starting at index startWord
    // in replacementWords_ to the phrase at index (startPhrase_.phrase + phrase).
    void distribute(int startWord, int words, int phrase);

    // Get the starting location of the given phrase.
    int getPhrasePos(Phrase* phrase) const;

    // return an iterator in the collection to the given phrase
    PhraseIterator getIterator(Phrase* phrase);
    PhraseConstIterator getIterator(Phrase* phrase) const;
};

// template <class Key>
inline PhraseManagerInterface* PhraseManager::GetPhraseManagerInterface() const
{
    return interface_;
}

#endif



