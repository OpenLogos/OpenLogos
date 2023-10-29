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
// WordGroupLookup.cpp: implementation of the WordGroupLookup class.
// 
// Description:
// Definition of member functions which help in iterating through the
// vector of LDictionaryEntry.
// Definition of a member function which will identify if the word
// iterator passed as a parameter (particular word in a sentence) is the 
// starting word in the pattern (group of words).
//////////////////////////////////////////////////////////////////////

#include <logos_include/logoscommon.h>
#include <logos_libs/gerdem/wordgrouplookup.h>
#include <logos_libs/startrules/variable.h>

ST_Variable *WordGroupLookup::_sentenceMatchInfo = 0;
LWordVector *WordGroupLookup::_srcWords = 0;

// Constructor
//

WordGroupLookup::WordGroupLookup(void):
                  _regEx(0), _nextMatchPos(-1, 0), _currentWordPos(0),  
                  _currentItem(0), _dictIndex(0), _lookupSuccess(false)
{
   _matchSearchInfo.matchInfo_ = 0;
   _matchSearchInfo.searchPosition_ = 0;
}

// Creates an instance of ST_Variable initialized with the sentence 
// source words                  
void WordGroupLookup::setST_Variable(LWordVector * srcWords)
{
   if (srcWords)
   {
      if (_sentenceMatchInfo)
      {
         delete _sentenceMatchInfo;
         _sentenceMatchInfo = 0;
         _srcWords = 0;
      }
      _sentenceMatchInfo = new ST_Variable(*srcWords);
      _srcWords = srcWords;
   }
}

// deletes the ST_Variable created by ST_VariableInstance
void WordGroupLookup::destroyST_Variable(void)
{
   if (_sentenceMatchInfo)
   {
      delete _sentenceMatchInfo;
      _sentenceMatchInfo = 0;
   }
}

LWordVector * WordGroupLookup::sourceWords(void)
{
   return _srcWords;
}

const LgsString & WordGroupLookup::getSubString(short groupNo) const
{
   return _matchSearchInfo.matchInfo_->getSubString(groupNo);
}

WordGroupLookup::~WordGroupLookup()
{
	_dictEntries.clear();
   if (!_regEx)
   {
      delete _regEx;
      _regEx = 0;
   }
   // Destroy the match and search positions if they exist
   if (_matchSearchInfo.matchInfo_)
   {
      delete _matchSearchInfo.matchInfo_;
      _matchSearchInfo.matchInfo_ = 0;
   }
   if (_matchSearchInfo.searchPosition_)
   {
      delete _matchSearchInfo.searchPosition_;
      _matchSearchInfo.searchPosition_ = 0;
   }
}

void WordGroupLookup::addEntry(LDictionaryEntry * pEntry)
{
   _dictEntries.push_back(pEntry);
}

// Initializes the RegularExpression of the WordGroupLookup class by creating 
// a RegularExpression class for the pattern specified. This will probably be
// called by the Derived Class Constructor soon after initializing the Regular
// Expression pattern variable
void WordGroupLookup::initializeRE(void)
{
	_regEx = new RegularExpression(_pattern, RegularExpression::None);
}

// RegularExpression is passed to the ST_Variable member function "matches()" 
// which tries to match for the pattern in the vector of source words contained 
// in the ST_Variable class. 
// The member variable _nextMatchPos holds the match information of position
// of the character in the source word vector where the match occured.
// This function has to be called after construction of the class to do the
// first match.
void WordGroupLookup::initialize(void)
{
   _currentWordPos = 0; // Set current word position to start of the sentence
   // Destroy the previous match and search positions if they are set
   if (_matchSearchInfo.matchInfo_)
   {
      delete _matchSearchInfo.matchInfo_;
      _matchSearchInfo.matchInfo_ = 0;
   }
   if (_matchSearchInfo.searchPosition_)
   {
      delete _matchSearchInfo.searchPosition_;
      _matchSearchInfo.searchPosition_ = 0;
   }
   // Since the ST_Variable class is static and is used by all 
   // WordGroupLookup class instances, make sure to save the current search 
   // position and matched position stored in the ST_Variable class
   // 
   _matchSearchInfo = _sentenceMatchInfo->matchAndSearchInfo(_matchSearchInfo);

   // Look for the Regular Expression pattern in the source word vector.
   // Matching is done and the match information will be saved. The saved
   // value will be used while processing each word to check if the word
   // being processed is the starting word of the pattern.
   _sentenceMatchInfo->resetStartPos();
	if (_sentenceMatchInfo->matches(*_regEx))
	{
      // Successful. Store the position of match in the _nextMatchPos
		_nextMatchPos = _sentenceMatchInfo->getMatch();
      _firstWordInGroup = 	_sentenceMatchInfo->getStartWord();
	}
   else 
	{
		_nextMatchPos.pos = -1;
	}

   // Restore the Values in the ST_Variable class while saving the 
   // matched positions in the member variable matchSearchInfo_
   _matchSearchInfo = _sentenceMatchInfo->matchAndSearchInfo(_matchSearchInfo); // Get current value and set old value
}

// Updates the position (in characters) of the current word that was processed 
// (looked up) Called soon after a word is looked up (processed). 
// LSentence::makeSourceSentenceUnits() soon after receiving the
// SourceSentenceUnit from the LDictionary class, it calls 
// LDictionary::updateWordPosition() (currently it is called for
// German only). LDictionary::updateWordPosition() calls DictionaryEntryBuilder::
// updateWordPosition() which in turn will call this member function
// to update the word position.
// The word position set here will be used later on, when matchPattern()
// is called to verify if the current word position is the start of the
// regular expression pattern
//
// Parameters:
//    iNoWords: The number of words that were looked up
//    iWord: word iterator which points to a particular word within the
//           vector of source words that were processed
void WordGroupLookup::updateWordPos(int iNoWords, const LWordIterator & iWord)
{
	LWordIterator  tempWord = iWord;

   // _currentWordPos is the position pointing to the character just after
   // the end of the last word that was processed (starting from the first 
   // character of the beginning of the first word in the vector of source words)

   // Example:
   // Source Words:     "ThisSisSaStestSSforSwordSSSgroupSlookup" (S is SPACE)
   // Let us say we just processed the word  "group" in the above test.
   // The _currentWordPos will be value "24" before updating and the position
   // is pointing to the first SPACE after the word "word".
   // To update the word position, we add the number of precedingSpaces preceding
   // the word "group" which is 3 and the length of the word "group" which is 5.
   // Hence the new _currentWordPos will become "24+3+5" which is 32 and points to
   // the first SPACE after the word "group"

	for (int i = 0; i < iNoWords; i++, tempWord++)
	{
		_currentWordPos += tempWord->precedingSpaces() + tempWord->length();
	}
}

// Checks if the word iterator passed as a parameter is the start of
// the regular expression pattern.
//
// Parameters:
//    iWord: current word TO BE processed
bool WordGroupLookup::matchPattern(const LWordIterator  & iWord)
{
	_lookupSuccess = false;
	bool retVal = false;

   // Add the preceding spaces for the word BEING processed with
   // the _currentWordPos to get the position of the first character
   // of the word that is BEING processed.
   // Check if this value matches with the saved position value in
   // _nextMatchPos (populated by initialize() and lookup() member
   // functions after doing a Regular Expression match on the whole
   // source word vector)
	if ((_currentWordPos + iWord->precedingSpaces()) == _nextMatchPos.pos)
	{
		retVal = true;
	}
	return retVal;
}

// This function has to be called if matchPattern() member function 
// returns true.
// This function will lookup the group of words in the pattern and
// populates the LDictionaryEntry vector elements.
// THIS CALLS A PURE VIRTUAL FUNCTION _lookup() which does the actual
// job of using the SubType specific rules to do the lookup on the 
// words present in the group
// 
void WordGroupLookup::lookup(bool BOS, bool bAllCapitalWords)
{
   // Clear the previous LDictionaryEntry contents
   _dictEntries.clear();

   // Call pure virtual function for the sub-type to do the job
   _lookup(BOS, bAllCapitalWords);

   // Since the ST_Variable class is static and is used by all 
   // WordGroupLookup class instances, make sure to set the current search 
   // position and matched position from the saved values before doing a 
   // match (_matchSearchInfo contains saved values)
   // 
   _matchSearchInfo = _sentenceMatchInfo->matchAndSearchInfo(_matchSearchInfo);
   // Increment the position from where the RegularExpression matching
   // should start now. This is to check if there are more similar 
   // patterns within the same sentence.
   // 
	_sentenceMatchInfo->incrementStartPos();
   if (_sentenceMatchInfo->matches(*_regEx))
	{
      // There are more patterns in the same sentence
      // Save the next match position in the sentence where the 
      // pattern matching occured.
		_nextMatchPos = _sentenceMatchInfo->getMatch();
      _firstWordInGroup = 	_sentenceMatchInfo->getStartWord();
	} 
   else 
	{
		_nextMatchPos.pos = -1;
	}
   // Restore the Values in the ST_Variable class while saving the 
   // matched positions in the member variable _matchSearchInfo
   _matchSearchInfo = _sentenceMatchInfo->matchAndSearchInfo(_matchSearchInfo);
   _lookupSuccess = true;
}


// returns true if there are elements in the object remaining to be
// read (LDictionaryEntry values)
WordGroupLookup::operator bool(void) const
{
	return (_lookupSuccess && (_currentItem != _dictEntries.end()));
}

// Point to the first LDictionaryEntry in the vector of entries
void WordGroupLookup::restart(void)
{
	_currentItem = _dictEntries.begin();
   _dictIndex = 0;
}

// Utility function to get the current LDictionaryEntry pointer and
// increment the iterator
LDictionaryEntry * WordGroupLookup::operator++(int)
{
	LDictionaryEntry * retVal = 0;
	if (_currentItem != _dictEntries.end())
	{
		retVal = (*_currentItem);
      SetWordCount(retVal, _dictIndex);
      _currentItem++;
      _dictIndex++;
	}
	return retVal;
}

// This will be used by the sub-type class to get the word boundaries for a 
// particular group in the regular expression
void WordGroupLookup::GetWordBoundaries(LWordIterator & firstWord, LWordIterator & lastWord, short groupNo)
{
   int fWordOffset, lWordOffset;
   // Since the ST_Variable class is static and is used by all 
   // WordGroupLookup class instances, make sure to set the current search 
   // position and matched position from the saved value before doing a 
   // match (_matchSearchInfo contains saved values)
   // 
   _matchSearchInfo = _sentenceMatchInfo->matchAndSearchInfo(_matchSearchInfo);
	_sentenceMatchInfo->getKeys(fWordOffset, lWordOffset, groupNo);
   firstWord = _firstWordInGroup + fWordOffset;
   lastWord = _firstWordInGroup + lWordOffset - 1;
   // Restore the Values in the ST_Variable class while saving the 
   // matched positions in the member variable _matchSearchInfo
   _matchSearchInfo = _sentenceMatchInfo->matchAndSearchInfo(_matchSearchInfo);
}

