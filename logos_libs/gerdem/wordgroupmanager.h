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
// WordGroupManager.h: interface for the WordGroupManager class.
//
//////////////////////////////////////////////////////////////////////

//#if !defined(LGS_WORDGROUPMANAGER_H__7AE5DD24_AD4A_11D2_948C_0060972D3059__INCLUDED_)
//#define LGS_WORDGROUPMANAGER_H__7AE5DD24_AD4A_11D2_948C_0060972D3059__INCLUDED_
#ifndef _WordGroupManager_h
#define _WordGroupManager_h

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

class WordGroupLookup;
class ST_Variable;
class DictionaryEntryBuilder;

// This class manages the instances of different WordGroupLookup classes.
// Lookup  using WordGroupLookup class is done as follows:
//    - The ST_Variable* has to set using getST_Variable() member function
//    - initialize() has to be called.
//    - For each word call matchPattern() to see if the word being looked up
//      is the start of a regular expression pattern.   
//    - if above is true, then call lookup() function to actually do a lookup
//      using the pattern specific rules (which are contained in the lookup()). 
//      the lookup() will load the LDictionaryEntry (contains SWORK info) values
//      internally.
//    - Use member functions operator bool(), operator++() to iterate through
//      the various LDictionaryEntry elements stored.
//
// This class provides member functions similar to the  public member
// functions by the WordGroupLookup class. Some member functions defined in this
// class goes through the vector of WordGroupLookup instances and calls its
// corresponding function in the WordGroupLookup class.
// A lookup using this class is done as follows:
// Call member functions of this class in the following order (resulting function
// calls are also mentioned):
//    - initialize() of this class will call initialize() of each WordGroupLookup
//      class instance
//    - matchPattern() will call matchPattern() of each WordGroupLookup class 
//      instance until one of them returns true. It saves the instance of the
//      WordGroupClass that returned in an internal member variable _matchedGroup
//    - lookup() will call _matchedGroup->lookup()
//    - operator bool() will call _matchedGroup->operator bool() and operator++()
//      will call _matchedGroup->operator++()
//
class WordGroupManager  
{
public:
   // Constructor
	WordGroupManager(DictionaryEntryBuilder & entryBuilder);
   // Operations
   void initialize(LWordVector * srcWords);
	void lookup(bool BOS, bool bAllCapitalWords);
	void restart(void);
	operator bool(void);
	LDictionaryEntry * operator++(int);
	bool matchPattern(const LWordIterator & iWord);
	void updateWordPos(int iNoWords, const LWordIterator & iWord);
   // Virtual Functions (Overrides)
	virtual ~WordGroupManager();
   void CleanupMemory();

private:
   typedef LgsVector(WordGroupLookup*) WordGroupLookupVector;
   typedef WordGroupLookupVector::iterator WordGroupLookupIterator;

   WordGroupLookupVector _wordGroups;
   WordGroupLookup *_matchedGroup;
   DictionaryEntryBuilder & _entryBuilder;
   bool _initialized;
};

#endif
