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
#ifndef __GermanUtil_h__
#define __GermanUtil_h__

//---------------------------------------------------------------------
// File - GermanUtil.h
//
// class - GermanUtil
//
// Description - utility module
//
//---------------------------------------------------------------------


struct GermanUtil
{
    // similar to strdup - but using function "new"
    // if length is 0 the full buffer is duplicated
    // if length is not 0 only length characters are duplicated (up to strlen(s))
    static char* duplicate(const char* s, int length);

    // converts to lower case - replaces ss with eszett
    // if the un-normalized form begins with upper-case sets the flag
    // if any other change is made set the flag
    static void normalize(const char* s, char* buffer, bool& initialCapital, bool& otherChange);

    // same as above, without returning flags
    static void normalize(const char* s, char* buffer);

    // calculate the length of the common prefix of entry and word
    static int calcCommonPrefixLength(const char* entry, const char* word);

    // removes the last letter of a word
    static void removeLastLetter(char* buffer);

    // checks if the given normalized word ends in a double consonant - but not an s
    static bool endsInDoubleConsonant(const char* word);

    // checks if the lower case letter is a german consonant - not eszett
    static bool isConsonant(char c);

    // Return true iff the word has length 2 or more
    static bool validCompoundPartLength(const char* word);

    // Return true iff validCompoundPartLength and consists of alpha chars or hyphens
    static bool validCompoundPart(const char* word);

    // binary stream functions
    static void printCharOn(char c, ofstream& output);
    static void printIntOn(int i, ofstream& output);
    static void printShortOn(short i, ofstream& output);
    static void printStringOn(const char* s, int length, ofstream& output);
    static void printCharArrayOn(const char* s, int length, ofstream& output);

    static void printByteArrayOn(const char* s, int length, ofstream& output);
    static bool readStringFrom(char*& s, istream& input);  // allocates buffer
    static bool readCharArrayFrom(char* s, int length, istream& input);
    static bool readCharFrom(char& c, istream& input);
    static bool readIntFrom(int& i, istream& input);
    static bool readShortFrom(short& i, istream& input);
    static bool readByteArrayFrom(char* s, int length, istream& input);

    // does the word have the given ending
    static bool isEnding(const char* ending, const LgsString& word);

    // return the last character in the LgsString
    static char lastChar(const LgsString& word);

    // compound is un-normalized
    // wordPart is normalized - hence the length difference (eszett becomes 'ss')
    // returns the length of the matching wordpart in the compound, starting at position pos    
    // returns -1 if there is a mismatch. This can happen if there is an eszett in the
    // compound word which is split between 2 word parts - a low probability
    //
    // precondition: compound has a wordpart at position pos - that is the un-normalized
    //               form of wordPart (normalized)
    static int wordPartLength(const LgsString& compound, int pos, const LgsString& wordPart);

};

inline char GermanUtil::lastChar(const LgsString& word)
{
    return char(word[word.length() - 1]);
}

inline void GermanUtil::removeLastLetter(char* buffer)
{
    buffer[strlen(buffer) - 1] = 0;
}

inline bool GermanUtil::isConsonant(char c)
{
    // note: y is not treated as a consonant
    return strchr("bcdfghjklmnpqrstvwxz", c) != 0;
}

inline bool GermanUtil::validCompoundPartLength(const char* word)
{
    return strlen(word) >= 2;
}

inline void GermanUtil::printCharOn(char c, ofstream& output)
{
    output.write(&c, sizeof(c));
}

inline void GermanUtil::printIntOn(int i, ofstream& output)
{
    output.write((char*)&i, sizeof(i));
}

inline void GermanUtil::printShortOn(short i, ofstream& output)
{
    output.write((char*)&i, sizeof(i));
}

inline void GermanUtil::printByteArrayOn(const char* s, int length, ofstream& output)
{
    output.write(s, length);
}

#endif



