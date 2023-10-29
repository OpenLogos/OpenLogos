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
program. If not, write to Globalware AG, Hospitalstraße 6, D-99817 Eisenach.

Linux port modifications and additions by Bernd Kiefer, Walter Kasper,
Deutsches Forschungszentrum fuer kuenstliche Intelligenz (DFKI)
Stuhlsatzenhausweg 3, D-66123 Saarbruecken
*/
// WordGroupLookup.h: interface for the WordGroupLookup class.
// Description:
// This contains the class definition for WordGroupLookup class.
// This class encapsulates the concept of looking up a group of
// words which are closely related. Lookup of a word in this group
// of words may be dependent upon the lookup of other words in the
// group. This class will have a Regular Expression pattern. The group
// of words which match the Regular Expression pattern will be looked
// up in the scope of this class. This class will have a pure virtual
// function "lookup()" which will have all the rules of looking up 
// the group of words.
// This class will have a vector of LDictionaryEntry (each entry holds the
// SWORK information of a word in the group).
// This class has iterator utilities to iterate through the list of 
// LDictionaryEntry.
//
//////////////////////////////////////////////////////////////////////

#ifndef _WordGroupLookup_h
#define _WordGroupLookup_h

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#include <logos_libs/gerdem/dictionaryentrybuilder.h>
#include <logos_libs/startrules/variable.h>
#include <logos_libs/regex/regularexpression.h>

class WordGroupLookup  
{
public:
   // Statics
   static void setST_Variable(LWordVector * srcWords = 0);
   static void destroyST_Variable(void);
   // Constructor
   WordGroupLookup(void);
   // Operations
   void initialize(void);
	void lookup(bool BOS, bool bAllCapitalWords);
   // Virtual Functions (Overrides)
   virtual ~WordGroupLookup();
	virtual void restart(void);
	virtual operator bool(void) const;
	virtual LDictionaryEntry * operator++(int);
	virtual bool matchPattern(const LWordIterator & iWord);
	virtual void updateWordPos(int iNoWords, const LWordIterator & iWord);

protected:
   LgsString _pattern;
   // Operations
   void initializeRE(void);
   void lookupSuccess(bool BOS);
   void addEntry(LDictionaryEntry * pEntry);
   void GetWordBoundaries(LWordIterator & startWord, LWordIterator & endWord, short groupNo);
   static LWordVector * sourceWords(void);
   const LgsString & getSubString(short groupNo) const;
   // Virtual Functions (Overrides)
   virtual void _lookup(bool BOS, bool bAllCapitalWords) = 0;
   virtual void SetWordCount(LDictionaryEntry * dictEntry, short dictIndex) = 0;

private:
   static ST_Variable * _sentenceMatchInfo;
   static LWordVector * _srcWords;
   RegularExpression *_regEx;
   RegularExpression::MatchPos _nextMatchPos;
   LDictionaryEntryPVector _dictEntries;
   short _currentWordPos;
   LDictionaryEntryPIterator _currentItem;
   short _dictIndex;
   bool _lookupSuccess;
   LWordIterator _firstWordInGroup;
   MatchSearchInfo _matchSearchInfo;
};

#endif
