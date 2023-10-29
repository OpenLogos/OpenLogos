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
//-----------------------------------------------------------------
// File - CharUtil.h
//
// Class - CharUtil
//
// Character utility module.
//
//-------------------------------------------------------------------

#include <ctype.h>
#include <logos_include/lgsstring.h> //strchr prototype
#include <cstdio>
#include <algorithm>

#ifndef CharUtil_h
#define CharUtil_h

//--------------------------------------------------------------------------------
// A utility class for character functionality
class CharUtil
{
public:
    // special values
    static char space();
    static unsigned char unsigned_space();
    static char null();

    // vowel information for English
    static const char* vowels();
    static bool isVowel(char c);

    // character classification - differs from <ctype.h>
    // uses latin1 character set except for the ligature 'oe' and 'OE' taken from the
    // Microsoft windows character set

    // from <ctype.h>
    static bool isAscii(char c);

    // from <ctype.h> - restricted to ascii
    static bool isControl(char c);
    static bool isHexDigit(char c);

    // from <ctype.h> for ascii, plus characters above and including '\xa0'
    static bool isPrint(char c);

    // isAlphaNumeric(c) || isPunctuation(c);
    static bool isGraph(char c);

    // from <ctype.h> for ascii, plus '\xa0'
    static bool isSpace(char c);

    // isBlank: include all chars below and including '\x32', as well as '\xa0'
    static bool isBlank(char c);

    // isPunctuation accepts only the following characters: / - ' " () [] {} : ; , . ! ? ø °
    static bool isPunctuation(char c);

    // note that isdigit gets incorrect values when the high bit of c is set
    static bool isNumeric(char c);

    // isLower, isUpper, isAlphabetic, isAlphaNumeric: include accented chars
    // alphabetic = lower + upper; alphanumeric = alphabetic + digits
    static bool isLower(char c);
    static bool isUpper(char c);
    static bool isAlphabetic(char c);
    static bool isAlphaNumeric(char c);
    static bool isJoiningCharacter(char c);
    static bool isWordSeparator(char c);

    // symbol characters are non-Blank, non-AlphaNumeric, non-Punctuation chars
    static bool isSymbol(char c);

    // conversions
    static char lower(char c);
    static char upper(char c);

    // utility functions
    static char henumChar(char c);

private:
    typedef unsigned char uchar;
    // string of Joining Characters
    static const LgsString m_joiningCharacters;
    static const LgsString m_wordSeparators;
    static bool charRange(char c, unsigned char c1, unsigned char c2);
};

inline char CharUtil::space()
{
    return ' ';
}

inline unsigned char CharUtil::unsigned_space()
{
    return ' ';
}

inline char CharUtil::null()
{
    return 0;
}

inline const char* CharUtil::vowels()
{
    return "aeiouAEIOU‰ˆ¸ƒ÷‹";
}

inline bool CharUtil::isVowel(char c)
{
    const char* vowel_list = vowels();
    return strchr(vowel_list, c) != 0;
}

inline bool CharUtil::isLower(char c)
{
    return uchar(c) == uchar('\x9c') || charRange(c, uchar('\x61'), uchar('\x7a')) ||
           (charRange(c, uchar('\xdf'), uchar('\xff')) && uchar(c) != uchar('\xf7'));
}

inline bool CharUtil::isUpper(char c)
{
    return uchar(c) == uchar('\x8c') || charRange(c, uchar('\x41'), uchar('\x5a')) ||
           (charRange(c, uchar('\xc0'), uchar('\xde')) && uchar(c) != uchar('\xd7'));
}

inline bool CharUtil::isAscii(char c)
{
#if _MSC_VER >= 1100
    return __isascii(c);
#else
   return isascii(c) ;
#endif
}

inline bool CharUtil::isPrint(char c)
{
    if (isAscii(c))
        return isprint(c) != 0;
    else
        return uchar(c) >= uchar('\xa0');
}

inline bool CharUtil::isGraph(char c)
{
    if (isAscii(c))
        return isgraph(c) != 0;
    else
        return uchar(c) > uchar('\xa0');
}

inline bool CharUtil::isControl(char c)
{
    return isAscii(c) && iscntrl(c);
}

inline bool CharUtil::isSpace(char c)
{
    return uchar(c) == uchar('\xa0') || (isAscii(c) && isspace(c));
}

inline bool CharUtil::isHexDigit(char c)
{
    return isAscii(c) && isxdigit(c);
}

inline bool CharUtil::isBlank(char c)
{
    return uchar(c) == uchar('\xa0') || uchar(c) <= unsigned_space();
}

inline bool CharUtil::isAlphabetic(char c)
{
    return isUpper(c) || isLower(c);
}

inline bool CharUtil::isNumeric(char c)
{
    return isAscii(c) && isdigit(c);
}

inline bool CharUtil::isAlphaNumeric(char c)
{
    return isAlphabetic(c) || isNumeric(c);
}

inline bool CharUtil::isPunctuation(char c)
{
    const char* punctuation_list = "/-'\"()[]{}:;,.!?ø°";
    return strchr(punctuation_list, c) != 0;
}

inline bool CharUtil::isSymbol(char c)
{
    return !CharUtil::isBlank(c) && !CharUtil::isPunctuation(c) && !CharUtil::isAlphaNumeric(c);
}

inline char CharUtil::lower(char c)
{
    if (c == '\x8c')
        return '\x9c';
    return isUpper(c) ? c + ('a' - 'A') : c;
}

inline char CharUtil::upper(char c)
{
    if (c == '\x9c')
        return '\x8c';
    return (isLower(c) && c != 'ﬂ' && c != 'ˇ') ? c + ('A' - 'a') : c;
}

inline bool CharUtil::charRange(char c, unsigned char c1, unsigned char c2)
{
    return (c1 <= uchar(c) && uchar(c) <= c2);
}

inline bool CharUtil::isJoiningCharacter(char c)
{
    LgsString::const_iterator endIter = m_joiningCharacters.end();
    LgsString::const_iterator charIt = m_joiningCharacters.begin(); 
    for (;charIt != endIter && c != (*charIt); charIt++);
    return charIt != endIter;
}

inline bool CharUtil::isWordSeparator(char c)
{
    LgsString::const_iterator endIter = m_wordSeparators.end();
    LgsString::const_iterator charIt = m_wordSeparators.begin(); 
    for (;charIt != endIter && c != (*charIt); charIt++);
    return charIt != endIter;
}


#endif


