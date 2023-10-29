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
//-------------------------------------------------------------------
// File - StringUtil.h
//
// Class - StringUtil
//
// LgsString utility functions
//
//-------------------------------------------------------------------

#ifndef __StringUtil_h__
#define __StringUtil_h__

#include <logos_libs/regex/charutil.h>

//-------------------------------------------------------------------
// A utility class for LgsString functionality
struct StringUtil
{
    // return lower/upper case version of input LgsString
    static LgsString lower(const LgsString& s);
    static LgsString upper(const LgsString& s);

    // convert LgsString in place to upper/lower case
    static void toUpper(LgsString& s);
    static void toUpperGerman(LgsString& s);
    static void toLower(LgsString& s);
    static void toLowerFirstChar(LgsString& s);

    // convert the first character of a non-empty LgsString to uppercase
    static void capitalize(LgsString& s);

    // return true if not empty, contains alphabetic chars, but contains no lowercase chars
    static bool isAllUpperCase      (const LgsString& s);
    static bool isAllUpperCaseOrNonAlpha(const LgsString& s);

    // return true if not empty, contains only digits
    static bool isAllNumeric        (const LgsString& s);
    static bool isAllAlphaNumeric   (const LgsString& s);
	static bool isAllAlpha			(const LgsString& s);
	static bool isAllAlphaNumericAndPeriods(const LgsString& s);
	static bool isAllAlphaNumericAndJoiningCharacters(const LgsString& s);
    static bool isEnglishSuffix(const LgsString& s);

    // see char.h for character classification
    static bool beginsUpperCase     (const LgsString& s);
    static bool containsAlphabetic  (const LgsString& s);
    static bool containsAlphanumeric(const LgsString& s);
    static bool containsDigit       (const LgsString& s);
    static bool containsLowerCase   (const LgsString& s);
    static bool containsPunctuation (const LgsString& s);
    static bool containsSpace       (const LgsString& s);
    static bool containsUpperCase   (const LgsString& s);
    static bool containsJoiningCharacter (const LgsString& s);

    static bool equalsIgnoreCase(const LgsString& s1, const LgsString& s2);

    // find a substring ignoring case
    static LgsString::size_type findIgnoreCase(
        const LgsString& data, const LgsString& pattern, LgsString::size_type pos = 0);

    static void leftTrim(LgsString& s, char c = ' ');
    static void rightTrim(LgsString& s, const char* t = " \r");
    static void rightTrim(LgsString& s, char c);

    static void parseInto(const LgsString& s, LgsStringVector& strings,
        char delimiter = ' ', bool delimitersAreTokens = false);

    // split the input LgsString s into tokens, counting and removing spaces between tokens
    // spaces are determined by CharUtil::isBlank()
    // keep groups of alphas or numerics together as one token
    // other characters will form one token each (separately)
    // spaces.length will be one more than tokens.length
    // spaces[0] will contain the leading spaces of the LgsString s
    // spaces[last] will contain the trailing spaces of the LgsString s
    static void parseInto(const LgsString& s, LgsStringVector& tokens, LgsIntVector& spaces);

    static int asInteger(const LgsString& s);
    static LgsString asStringFromInt(int i);
};

inline bool StringUtil::isAllUpperCase(const LgsString& s)
{
    if (s.empty())
        return false;
    return containsAlphabetic(s) && !containsLowerCase(s);
}

inline bool StringUtil::isAllUpperCaseOrNonAlpha(const LgsString& s)
{
    if (s.empty())
        return false;
    return !containsLowerCase(s);
}

inline bool StringUtil::isAllNumeric(const LgsString& s)
{
    if (s.empty())
        return false;

    for (LgsString::const_iterator iter = s.begin(); iter != s.end(); iter++)
        if (!CharUtil::isNumeric(*iter))
            return false;
    return true;
}

inline bool StringUtil::isAllAlpha(const LgsString& s)
{
    if (s.empty())
        return false;

    for (LgsString::const_iterator iter = s.begin(); iter != s.end(); iter++)
        if (!CharUtil::isAlphabetic(*iter))
            return false;
    return true;
}

inline bool StringUtil::isAllAlphaNumeric(const LgsString& s)
{
    if (s.empty())
        return false;

    for (LgsString::const_iterator iter = s.begin(); iter != s.end(); iter++)
        if (!CharUtil::isAlphaNumeric(*iter))
            return false;
    return true;
}

inline bool StringUtil::isAllAlphaNumericAndPeriods(const LgsString& s)
{
    if (s.empty())
        return false;

	bool currAlpNum = false;

    for (LgsString::const_iterator iter = s.begin(); iter != s.end(); iter++)
	{
        if (CharUtil::isAlphaNumeric(*iter))
		{
			currAlpNum = true;
		}
		else if (*iter == '.' && currAlpNum)
		{
			currAlpNum = false;
		}
		else
		{
            return false;
		}
	}
    return true;
}


inline bool StringUtil::isAllAlphaNumericAndJoiningCharacters(const LgsString& s)
{
    if (s.empty())
        return false;

    bool bAlphaNumeric = false;
    for (LgsString::const_iterator iter = s.begin(); iter != s.end(); iter++)
	{
        if (CharUtil::isAlphaNumeric(*iter))
		{
			bAlphaNumeric = true;
		}
        else if (!CharUtil::isJoiningCharacter(*iter))
        {
            return false;
        }
	}
    return bAlphaNumeric;
}

inline bool StringUtil::containsJoiningCharacter(const LgsString& s)
{
    for (LgsString::const_iterator iter = s.begin(); iter != s.end(); iter++)
        if (CharUtil::isJoiningCharacter(*iter))
            return true;
    return false;

}

#endif



