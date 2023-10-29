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
// File - StringUtil
//
// Class - StringUtil
//
//---------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/utility/stringutil.h>
#include <logos_libs/utility/parseinto.h>

LgsString StringUtil::lower(const LgsString& s)
{
    LgsString ret = s;
    for (LgsString::iterator iter =  ret.begin(); iter != ret.end(); iter++ )
        *iter = CharUtil::lower(*iter);
    return ret;
}

LgsString StringUtil::upper(const LgsString& s)
{
    LgsString ret = s;
    for (LgsString::iterator iter =  ret.begin(); iter != ret.end(); iter++ )
        *iter = CharUtil::upper(*iter);
    return ret;
}

void StringUtil::toUpper(LgsString& s)
{
    for (LgsString::iterator iter = s.begin(); iter != s.end(); iter++)
        *iter = CharUtil::upper(*iter);
}

bool StringUtil::isEnglishSuffix(const LgsString& s)
{    
   LgsString m_englishSuffixes[] = {"st", "nd", "rd", "th"};

    for (short sListSize = sizeof(m_englishSuffixes)/sizeof(m_englishSuffixes[0]);
         sListSize; sListSize--)
    {
        if (!m_englishSuffixes[sListSize-1].compare(s))
        {
            return true;
        }
    }
    return false;
}


void StringUtil::toUpperGerman(LgsString& s)
{
   StringUtil::toUpper(s);
   LgsString newStr = "";
   for (LgsString::iterator iter = s.begin(); iter != s.end(); iter++)
   {
      if (*iter == 'ﬂ')
         newStr += "SS";
      else
         newStr += *iter;
   }
   s.erase();
   s = newStr;
}

void StringUtil::toLower(LgsString& s)
{
    for (LgsString::iterator iter = s.begin(); iter != s.end(); iter++)
        *iter = CharUtil::lower(*iter);
}

void StringUtil::toLowerFirstChar(LgsString& s)
{
   LgsString::iterator iter = s.begin();
   if (iter != s.end())
      *iter = CharUtil::lower(*iter);
}

void StringUtil::capitalize(LgsString& s)
{
    LgsString::iterator iter = s.begin();
    if (iter != s.end())
        *iter = CharUtil::upper(*iter);
}

bool StringUtil::containsAlphabetic(const LgsString& s)
{
    for (LgsString::const_iterator iter = s.begin(); iter != s.end(); iter++)
        if (CharUtil::isAlphabetic(*iter))
            return true;
    return false;
}

bool StringUtil::containsAlphanumeric(const LgsString& s)
{
    for (LgsString::const_iterator iter = s.begin(); iter != s.end(); iter++)
        if (CharUtil::isAlphaNumeric(*iter))
            return true;
    return false;
}

bool StringUtil::containsDigit(const LgsString& s)
{
    for (LgsString::const_iterator iter = s.begin(); iter != s.end(); iter++)
        if (CharUtil::isNumeric(*iter))
            return true;
    return false;
}

bool StringUtil::containsLowerCase(const LgsString& s)
{
    for (LgsString::const_iterator iter = s.begin(); iter != s.end(); iter++)
        if (CharUtil::isLower(*iter))
            return true;
    return false;
}

bool StringUtil::containsPunctuation(const LgsString& s)
{
    for (LgsString::const_iterator iter = s.begin(); iter != s.end(); iter++)
        if (CharUtil::isPunctuation(*iter))
            return true;
    return false;
}

bool StringUtil::containsSpace(const LgsString& s)
{
    for (LgsString::const_iterator iter = s.begin(); iter != s.end(); iter++)
        if (CharUtil::isBlank(*iter))
            return true;
    return false;
}

bool StringUtil::containsUpperCase(const LgsString& s)
{
    for (LgsString::const_iterator iter = s.begin(); iter != s.end(); iter++)
        if (CharUtil::isUpper(*iter))
            return true;
    return false;
}

bool StringUtil::equalsIgnoreCase(const LgsString& s1, const LgsString& s2)
{
    // if the strings are not the same size they are not equal

    if (s1.size() != s2.size())
        return false;

    for (int i = 0; i < s1.size(); i++)
        if (CharUtil::lower(s1[i]) != CharUtil::lower(s2[i]))
            return false;

    return true;
}

LgsString::size_type StringUtil::findIgnoreCase(
    const LgsString& data, const LgsString& pattern, LgsString::size_type pos)
{
    // taken from LgsString.cc - modified to ignore case
    for (LgsString::size_type xpos = pos; (xpos + pattern.length()) <= data.length(); xpos++)
    {
        bool found = true;
        for (LgsString::size_type i = 0; i < pattern.length() ; i++)
        {
            if (CharUtil::lower(data[xpos + i]) != CharUtil::lower(pattern[i]))
            {
                found = false;
                break;
            }
        }
        if (found)
            return xpos;
    }

	return NPOS;
}

void StringUtil::leftTrim(LgsString& s, char c)
{
    LgsString::iterator trim_to;
    LgsString::size_type non_space = s.find_first_not_of(c);
	if (non_space == NPOS)
        trim_to = s.end();
    else
        trim_to = s.begin() + non_space;
    s.erase(s.begin(), trim_to);
}

void StringUtil::rightTrim(LgsString& s, char c)
{
    LgsString::iterator trim_from;
    LgsString::size_type non_space = s.find_last_not_of(c);
	if (non_space == NPOS)
		trim_from = s.begin();
    else
        trim_from = s.begin() + non_space + 1;
    s.erase(trim_from, s.end());
}

void StringUtil::rightTrim(LgsString& s, const char* t)
{
    LgsString::iterator trim_from;
    LgsString::size_type non_space = s.find_last_not_of(t);
	if (non_space == NPOS)
		trim_from = s.begin();
    else
        trim_from = s.begin() + non_space + 1;
    s.erase(trim_from, s.end());
}

int StringUtil::asInteger(const LgsString& s)
{
    return (atoi(s.data()));
}

LgsString StringUtil::asStringFromInt(int i)
{
  // changed (bk Jul 19 2005)
   char buffer[11];
   int pos = 0;
   /*
   if (i < 0) { buffer[0] = '-'; i = -i ; pos = 1; }
   if (i = 0) { buffer[0] = '0' ; pos = 1; }
   else 
     while (i > 0) { 
       buffer[pos] = '0' + i % 10; 
       i = i / 10 ; 
       pos ++ ;
     }
   buffer[pos] = '\0';
   */
   itoa(i, buffer, 10);
   return LgsString(buffer);
}

bool StringUtil::beginsUpperCase(const LgsString& s)
{
    if (s.empty())
        return false;
    return CharUtil::isUpper(s[0]);
}

void StringUtil::parseInto(const LgsString& s, LgsStringVector& strings,
    char delimiter, bool delimitersAreTokens)
{
    gParseInto(s.begin(), s.end(), strings, delimiter, delimitersAreTokens);
}

inline void pushSpaces(int& spaceCount, LgsIntVector& spaces)
{
    spaces.push_back(spaceCount);
    spaceCount = 0;
}

inline void pushWord(LgsString& word, LgsStringVector& tokens)
{
    tokens.push_back(word);
    word.erase();
}

void StringUtil::parseInto(const LgsString& s, LgsStringVector& tokens, LgsIntVector& spaces)
{
    assert(tokens.size() == 0);
    assert(spaces.size() == 0);

    // finite state machine
    enum State {sSpace, sDigit, sAlpha, sOther };
    State state = sSpace;
    int spaceCount = 0;
    LgsString word;

    for (LgsString::const_iterator iter = s.begin(); ; iter++)
    {
        char c = iter == s.end() ? 0 : *iter;
        switch (state)
        {
        case sSpace:
            if (!c)
            {
                // push trailing spaces
                pushSpaces(spaceCount, spaces);
                return;
            }
            else if (CharUtil::isBlank(c))
            {
                spaceCount++;
            }
            else
            {
                pushSpaces(spaceCount, spaces);
                word += c;

                if (CharUtil::isNumeric(c))
                    state = sDigit;
                else if (CharUtil::isAlphabetic(c))
                    state = sAlpha;
                else
                    state = sOther;
            }
            break;

        case sDigit:
            if (!c)
            {
                pushWord(word, tokens);

                // push trailing spaces
                pushSpaces(spaceCount, spaces);

                return;
            }
            else if (CharUtil::isNumeric(c))
            {
                word += c;
            }
            else
            {
                pushWord(word, tokens);

                if (CharUtil::isBlank(c))
                {
                    spaceCount++;
                    state = sSpace;
                }
                else
                {
                    pushSpaces(spaceCount, spaces);
                    word += c;

                    if (CharUtil::isAlphabetic(c))
                    {
                        state = sAlpha;
                    }
                    else
                    {
                        state = sOther;
                    }
                }
            }
            break;

        case sAlpha:
            if (!c)
            {
                pushWord(word, tokens);

                // push trailing spaces
                pushSpaces(spaceCount, spaces);

                return;
            }
            else if (CharUtil::isAlphabetic(c))
            {
                word += c;
            }
            else
            {
                pushWord(word, tokens);

                if (CharUtil::isBlank(c))
                {
                    spaceCount++;
                    state = sSpace;
                }
                else
                {
                    pushSpaces(spaceCount, spaces);
                    word += c;

                    if (CharUtil::isNumeric(c))
                    {
                        state = sDigit;
                    }
                    else
                    {
                        state = sOther;
                    }
                }
            }

            break;

        case sOther:
            pushWord(word, tokens);

            if (!c)
            {
                // push trailing spaces
                pushSpaces(spaceCount, spaces);
                return;
            }
            else if (CharUtil::isBlank(c))
            {
                spaceCount++;
                state = sSpace;
            }
            else
            {
                pushSpaces(spaceCount, spaces);
                word += c;

                if (CharUtil::isNumeric(c))
                {
                    state = sDigit;
                }
                else if (CharUtil::isAlphabetic(c))
                {
                    state = sAlpha;
                }
            }
            break;
        }
    }
}

