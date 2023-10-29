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
#ifndef __GermanSearch_h__
#define __GermanSearch_h__

//---------------------------------------------------------------------
// File - GermanSearch.h
//
// class - GermanSearch
//
// Description - class holding a backtrack state, used to decompose a compound
//               German word into parts - with backtracking
//
//---------------------------------------------------------------------

#include <logos_libs/halfnoun/germanbufferlength.h>
#include <logos_libs/halfnoun/germantoken.h>
#include <logos_libs/halfnoun/germandictionary.h>
#include <logos_libs/halfnoun/germanutil.h>
#include <logos_libs/halfnoun/germanheadwordinfo.h>

//---------------------------------------------------------------------
class GermanBuffer
{
public:
   void setWord(LgsString& word);
   int length() const;
   void advance(int length);
   void retreat(int length);
   const char* c_str() const;
   LgsString word(int wordLength) const;
   char nextLetter() const;

private:
   LgsString word_;               // normalized version of compound word
   int wordOffset_;            // boundary between decomposed part and un-decomposed part
};

inline void GermanBuffer::setWord(LgsString& word)
{
   wordOffset_ = 0;
   char buffer[BUFFER_LENGTH];
   GermanUtil::normalize(word.c_str(), buffer);
   word = word_ = buffer;
}

inline int GermanBuffer::length() const
{
   return word_.length() - wordOffset_;
}

inline void GermanBuffer::advance(int length)
{
   wordOffset_ += length;
   assert(wordOffset_ <= word_.length());
}

inline void GermanBuffer::retreat(int length)
{
   wordOffset_ -= length;
   assert(wordOffset_ >= 0);
}

inline const char* GermanBuffer::c_str() const
{
   return word_.c_str() + wordOffset_;
}

inline LgsString GermanBuffer::word(int wordLength) const
{
   assert(wordOffset_ + wordLength <= word_.length());
   return LgsString(word_.begin() + wordOffset_, word_.begin() + wordOffset_ + wordLength);
}

inline char GermanBuffer::nextLetter() const
{
   return word_.c_str()[wordOffset_];
}

//---------------------------------------------------------------------
// search item - used internally by GermanSearch only
// GermanSearch has one of these objects
class GermanSearchItem
{
public:
   // constructor
   GermanSearchItem(const LgsVector(LgsString)& companies);
   ~GermanSearchItem(void);

   // store the compound word to be decomposed
   void setDetails(LgsString& compoundWord, GermanHeadWordInfo headWordInfo);

   // append a word token into the decomposition_, using tokenLength()
   void appendWord(int wordLength, LgsList(const GermanDictionaryEntry*)& entries);

   // append a connector token into the decomposition_, and advance past it
   void appendConnector();

   // append a suffix token into the decomposition_, and advance to end of compoundword
   void appendSuffix();

   // is the buffer_ an exact suffix
   bool isSuffix() const;

   // get length from last token in decomposition_ - of type GermanTokenTrialWord
   int getTrialTokenWordLength() const;

   // get length from last token in decomposition_ - of type GermanTokenTrialConnector
   int getTrialTokenConnectorLength() const;

   // append a GermanTokenTrialWord using length of buffer_
   void appendTrialWord();

   // append a GermanTokenTrialConnector using length of buffer_
   void appendTrialConnector();

   // append a GermanTokenTrialSuffix
   void appendTrialSuffix();

   // precondition: the last token in decomposition_ is of type GermanTokenTrial
   // returns true if the last token in decomposition_
   // is not null and of type GermanTokenTrialWord; returns false otherwise
   bool isTrialWordToken() const;

   // precondition: the last token in decomposition_ is of type GermanTokenTrial
   // returns true if the last token in decomposition_
   // is not null and of type GermanTokenTrialConnector; returns false otherwise
   bool isTrialConnectorToken() const;

   // precondition: the last token in decomposition_ is of type GermanTokenTrial
   // returns true if the last token in decomposition_
   // is not null and of type GermanTokenTrialSuffix; returns false otherwise
   bool isTrialSuffixToken() const;

   // return true if the buffer is empty
   bool isEmpty() const;

   // append decomposition_ into decomposition
   void appendDecomposition(GermanTokenList& decomposition) const;

   // precondition: the last token in decomposition_ is of type GermanTokenTrialWord
   // return the longest prefix of the buffer from the dictionary
   // updates the last token in decomposition_ with the length
   int longestPrefix(GermanDictionary& dictionary, GermanDictionary::Range& range);

   // precondition: the last token in decomposition_ is of type GermanTokenTrialConnector
   // returns the longest connector of the buffer
   // updates the last token in decomposition_ with the length
   int connectorLength();

   // if the last token in decomposition_ is a trial word of length less than the minimum length
   // return true. else return false.
   bool mustBacktrack() const;

   // backtrack algorithm - small step
   // trial connector: if length is non-zero decrement it
   //                  if length is zero (including after decrement) decay to trial word
   // trial word:      pre-condition: length non-zero
   //                  decrement length
   void backtrack();

   // backtrack algorithm - large step
   // suffix: pop 3 items
   // word: pop 1 item
   // trial word pop 2 items
   // In all cases replace text at left of buffer for Tokens removed
   void backtrackBigStep();

   // pop given no of tokens off back of the list
   void popBack(int tokens);

   // return true if item has no tokens
   bool hasNoTokens() const;

   // start with range and filter out invalid entries using validEntry()
   LgsList(const GermanDictionaryEntry*) filter(const GermanDictionary::Range& range) const;

   // does the last word token allow a suffix
   // this will occur unless the only items for the word are all special doubled consonant entries
   bool allowSuffix() const;

   // does the decomposition have valid suffix data
   bool suffixOk() const;

   // is the head word class consistant with headWordInfo_
   // note we do not prune any head words here, so the client may need to do this
   // for example if the headWordInfo_ accepts nouns and adjectives, but not verbs
   //     we still cannot remove the head verbs since in a later backtrack the verb
   //     may be required in a non-head position. this is due to the fact that suffixes
   //     can be as long as 4 characters, so there is ambiguity as to when a word is a
   //     head or not, in some cases
   bool headWordclassOk() const;
   void resetWordCount(void);
   void unlinkInvalidEntries(void);

private:
   // return the minimum length of a word-part (half-noun)
   static int minWordLength();

   // can the suffix also be a word
   static bool canSuffixBeWord(const char* suffix);

   GermanHeadWordInfo headWordInfo_;      // details about the head word
   LgsVector(LgsString) companies_;      // list of allowed company codes
   GermanBuffer buffer_;                  // holds the normalized word and offset
   GermanTokenList* decomposition_;       // holds a decomposition in progress
   short wordCount_;

   // advance buffer by length, and append a token to decomposition_
   void appendToken(int length, GermanTokenAbstract* token);

   // return true if the decomposition_ does not contain any words yet
   bool isFirstWord() const;

   // get the last word in decomposition_, 0 if none
   GermanTokenWord* getHeadWord() const;

   // adjust length in last token in decomposition_ - of type GermanTokenTrialWord
   void updateTrialTokenWordLength(int length);

   // adjust length in last token in decomposition_ - of type GermanTokenTrialConnector
   void updateTrialTokenConnectorLength(int length);

   // is the company of the entry in the company list
   bool validCompanyForEntry(const GermanDictionaryEntry& entry) const;

   // if the entry is a special doubled-consonant entry eg 'Ballet' (from Ballett)
   // return false if the following letter does not start with the consonant
   bool validDoubleLetterForEntry(const GermanDictionaryEntry& entry) const;

   // does the entry have a non-head word-class in its flags
   bool validAsNonHead(const GermanDictionaryEntry& entry) const;

   // does the entry have a head word-class in its flags
   bool validAsHead(const GermanDictionaryEntry& entry) const;

   // is the entry valid - check company codes, head/non-head data, doubled consonant
   bool validEntry(const GermanDictionaryEntry& entry) const;

   // check the entry and suffix for consistency
   bool validForHead(const GermanDictionaryEntry& entry) const;
};

inline GermanSearchItem::GermanSearchItem(const LgsVector(LgsString)& companies)
                        :decomposition_(0),
                         wordCount_(0)
{
	for(LgsVector(LgsString)::const_iterator iter = companies.begin();
		iter != companies.end();
		iter++)
	{
		companies_.push_back((*iter).c_str());
	}
}

inline void GermanSearchItem::resetWordCount(void)
{
   wordCount_ = 0;
}

inline GermanSearchItem::~GermanSearchItem(void)
{
   if (decomposition_)
   {
      delete decomposition_;
      decomposition_ = 0;
   }
}

inline void GermanSearchItem::setDetails(LgsString& compoundWord, GermanHeadWordInfo headWordInfo)
{
   assert(compoundWord.find('-') == LgsString::npos);

   buffer_.setWord(compoundWord);
   headWordInfo_ = headWordInfo;
   delete decomposition_;
   decomposition_ = new GermanTokenList;
   appendTrialWord();
}

inline bool GermanSearchItem::isFirstWord() const
{
   return decomposition_->size() == 1;
}

inline GermanTokenWord* GermanSearchItem::getHeadWord() const
{
   return decomposition_->getHeadWord();
}

inline void GermanSearchItem::appendTrialWord()
{
   decomposition_->push_back(new GermanTokenTrialWord(buffer_.length()));
}

inline bool GermanSearchItem::isEmpty() const
{
   return buffer_.length() == 0;
}

inline void GermanSearchItem::appendDecomposition(GermanTokenList& decomposition) const
{
   decomposition.append(*decomposition_);
}

inline bool GermanSearchItem::hasNoTokens() const
{
   return decomposition_->empty();
}

inline int GermanSearchItem::minWordLength()
{
   return 2;
}

class StringCompare
{
public:
   bool operator()(const LgsString & lhs, const LgsString & rhs) const;
};

inline bool StringCompare::operator ()(const LgsString & lhs, const LgsString & rhs) const
{
   if (lhs.compare(rhs) < 0)
   {
      return true;
   }
   return false;
}

//---------------------------------------------------------------------
class GermanSearch
{
public:
   // initializes the search with a compound word to decompose,
   // a dictionary already filled with words and a buffer already initialized
   // iniFileName contains a section [no_decomposition] containing words that do not decompose
   GermanSearch(const LgsString& placeFile, const LgsString& properNameFile,
   const LgsVector(LgsString)& companies,
   GermanDictionary& dictionary, bool backtrack = true);
   ~GermanSearch(void);

   // returns true iff the current state is a solution - ie there is a decomposition
   // decomposition is input as empty vector
   // if true fills decomposition with the current decomposition
   bool decompose(const LgsString& compoundWord, GermanTokenList& decomposition,
                  GermanHeadWordInfo headWordInfo, bool allUpperCase);

private:
   LgsSetP(LgsString, StringCompare) noDecomposeList_;          // list of words that should not be decomposed
   LgsString compoundWord_;                  // un-normalized form of compound word
   bool allUpperCase_;                    // is the search word all in upper case
   GermanDictionary& dictionary_;   // cached dictionary of German words
   GermanSearchItem item_;                // current information of the decomposition
   bool backtrack_;                       // decompose with backtracking on or off

   // goes to the next state
   // backtracks if current state is a solution - or if a deadend
   // returns true iff there is a next state
   bool nextState();

   // returns true iff the current state is a solution - ie there is a decomposition
   bool isSolution();

   // pre-condition: isSolution() is true
   // returns true iff the solution matches the un-normalized form of the word
   bool matchesUnnormalized();

   // backtrack algorithm:
   bool backtrack();

   // add words in file to noDecomposition list
   void appendFile(const LgsString& fileName);
};

inline bool GermanSearch::isSolution()
{
   return item_.isEmpty();
}

inline bool GermanSearch::matchesUnnormalized()
{
   return true;
}

#endif



