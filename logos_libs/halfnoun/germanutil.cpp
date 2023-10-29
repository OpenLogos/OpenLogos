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
// File - GermanUtil.cpp
//
// class - GermanUtil
//
//---------------------------------------------------------------------

#include <logos_include/logoscommon.h>

#include <logos_libs/halfnoun/germanutil.h>
#include <logos_libs/halfnoun/timer.h>
#include <logos_libs/regex/charutil.h>
#include <logos_libs/utility/stringutil.h>




char* GermanUtil::duplicate(const char* s, int length)
{
    if (s == 0)
        return 0;

    assert(0 < length && length <= strlen(s));

    char* ret = new char[length + 1];
    strncpy(ret, s, length);
    ret[length] = 0;
    return ret;
}

void GermanUtil::normalize(const char* s, char* buffer, bool& initialCapital, bool& otherChange)
{
    otherChange = false;
    initialCapital = CharUtil::isUpper(*s);

    char* j = buffer;
    for (const char* i = s; *i != 0; i++)
    {
        if (*i == 'ﬂ')
        {
            *j++ = 's';
            *j++ = 's';
            otherChange = true;
        }
        else
        {
            if (i != s && CharUtil::isUpper(*i))
                otherChange = true;
            *j++ = CharUtil::lower(*i);
        }
    }
    *j = 0;
}

void GermanUtil::normalize(const char* s, char* buffer)
{
    bool initialCapital, otherChange;
    normalize(s, buffer, initialCapital, otherChange);
}

int GermanUtil::calcCommonPrefixLength(const char* entry, const char* word)
{
    const char* entryPtr;
    const char* wordPtr;
    for (entryPtr = entry, wordPtr = word; *entryPtr != 0 && *wordPtr != 0; entryPtr++, wordPtr++)
    if (*entryPtr != *wordPtr)
        break;
    return entryPtr - entry;
}

bool GermanUtil::validCompoundPart(const char* word)
{
    if (!validCompoundPartLength(word))
        return false;

    for (const char* s = word; *s != 0; s++)
    {
        char c(*s);

        if (!CharUtil::isAlphabetic(c))
            return false;
    }

    return true;
}

bool GermanUtil::endsInDoubleConsonant(const char* word)
{
    int last = strlen(word) - 1;
        if (word[last] == 's')
                return false;
    return word[last - 1] == word[last] && isConsonant(word[last]);
}

void GermanUtil::printStringOn(const char* s, int length, ofstream& output)
{
    printIntOn(length, output);
    output.write(s, length);
}

void GermanUtil::printCharArrayOn(const char* s, int length, ofstream& output)
{
    output.write(s, length);
}

bool GermanUtil::readStringFrom(char*& s, istream& input)
{
    int length;
    if (!readIntFrom(length, input))
        return false;

    s = new char[length + 1];
    s[length] = 0;

    input.read(s, length);
    if (input.bad() || input.eof())
    {
        delete[] s;
        return false;
    }
    return true;
}

bool GermanUtil::readCharArrayFrom(char* s, int length, istream& input)
{
    input.read(s, length);
    return !(input.bad() || input.eof());
}

bool GermanUtil::readCharFrom(char& c, istream& input)
{
    input.read(&c, sizeof(c));
    return !(input.bad() || input.eof());
}

bool GermanUtil::readIntFrom(int& i, istream& input)
{
    input.read((char*)&i, sizeof(i));
    return !(input.bad() || input.eof());
}

bool GermanUtil::readShortFrom(short& i, istream& input)
{
    input.read((char*)&i, sizeof(i));
    return !(input.bad() || input.eof());
}

bool GermanUtil::readByteArrayFrom(char* s, int length, istream& input)
{
    input.read(s, length);
    return !(input.bad() || input.eof());
}

bool GermanUtil::isEnding(const char* ending, const LgsString& word)
{
    int endingLength = strlen(ending);
    int wordLength = word.length();
    if (wordLength < endingLength)
        return false;

    return LgsString(word.begin() + wordLength - endingLength, word.end()) == LgsString(ending);
}

int GermanUtil::wordPartLength(const LgsString& compound, int pos, const LgsString& wordPart)
{
    int length = 0;
    for (LgsString::const_iterator iter = wordPart.begin(); iter != wordPart.end(); iter++)
    {
        if (pos + length >= compound.length())
            return -1;
        bool esszett = compound[pos + length++] == 'ﬂ';
        if (esszett)
        {
            iter++; // since normalized word has 2 chars instead of 1
            if (iter == wordPart.end())
            {
                // have a low probability situation where the matching
                // algorithm was incorrect since the normalized 'ﬂ' was
                // replaced by 'ss' and the decomposition put each 's' in
                // a different half-noun
                // -- since this is such an infrequent event we return an unfound
                // word here. A more correct approach would be to back-track
                // and retrieve the next decomposition
                
                return -1;
            }
        }
    }
    return length;
}


