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
#ifndef __GermanDictionary_h__
#define __GermanDictionary_h__

//---------------------------------------------------------------------
// File - GermanDictionary.h
//
// class - GermanDictionaryField
// class - GermanDictionary
//
// Description - German class used to decompose compound nouns to half-noun
//               parts. only lexical information is used
//
//---------------------------------------------------------------------

#include <logos_libs/sql/sqlconnection.h>
#include <logos_libs/halfnoun/germandictionaryentry.h>
#include <logos_libs/halfnoun/timer.h>

//---------------------------------------------------------------------
// keeps track of the key field and when a field break occurs
class GermanDictionaryField
{
public:
    DefaultConstructor(GermanDictionaryField)

    // check if there is a field break
    // ie the input data is different from the stored data
    // if there is a field break store the new data
    bool isBreak(const LgsString& company, const LgsString& word, const LgsString& wordClass, int pat, int stem);

private:
    LgsString company_;            // company code
    LgsString word_;               // normalized word
    LgsString wordClass_;          // word class on data base
    int pat_;
    int stem_;
};

//---------------------------------------------------------------------
class GermanDictionary
{
public:
    friend class GermanDictionaryEntry;

    typedef GermanDictionaryEntryVector::const_iterator Iterator;
    typedef pair<Iterator, Iterator> Range;

    GermanDictionary(const LgsString& iniFileName, const LgsString& binaryFileName,
        const LgsString& logFileName, const LgsString& textFileName = LgsString());
	~GermanDictionary(void);

    // read from the database - store both normalized and un-normalized, sorted in cache
    // then stream out to disk
    void readDataBase(SqlConnection* connect);

    // stream in from disk
    void streamIn();

    // stream out to disk as text
    void streamOutAsText();

    // Return the longest prefix length less than or equal to maxLength that is in the dictionary
    // If maxLength is 0 there is no restriction on the longest prefix returned
    // Words of length minLength or less are not accepted
    // If return is non-zero range holds the range of prefixes that hold
    int longestPrefix(const char* word, int minLength, int maxLength, Range& range) const;

private:
    enum { INITIAL_SIZE = 100000 };

    LgsString binaryFileName_;                // binary file to stream in and out
    LgsString textFileName_;                  // text file to stream out
    GermanDictionaryEntryVector wordList_;    // sorted list of words
    LgsString iniFileName_;                   // ini file name

    // helper for longestPrefix
    // called recursively
    // search only from start of wordList_ to end iterator
    // if return is non-zero range holds the range of prefixes that hold
    // word must already be normalized
    int longestPrefix_(char* word, int minLength, Iterator end, Range& range) const;

    // entry point for all database reads - also includes .ini file data
    void readDataBase_(SqlConnection* connect);

    // read from the database - store both normalized and un-normalized
    void readWordList(SqlConnection* connect);

    // check the dictionary entry - store in the cache if
    //     not null and if previously filled with at least one word-class
    void storeEntry(GermanDictionaryEntry* dictEntry, int& total, int& valid, int& doubled);

    // store the dictionary entry in the vector
    // check for doubled last letter and if so add a duplicate
    //     entry with last letter removed
    // return true iff a duplicate entry was added
    bool storeEntry(GermanDictionaryEntry* dictEntry);

    // sort the cache
    void sortCache();

    // stream out to disk
    void streamOut();

    // convert word class to GermanWordClass
    // return true if entry is acceptable - false if not (eg on a Proper noun)
    bool convertWordClass(const LgsString& word, const LgsString& clazz,
                          int pat, int stem,
                          int s0, int s1, int s2, GermanWordClass& wordClass);
};

#endif



