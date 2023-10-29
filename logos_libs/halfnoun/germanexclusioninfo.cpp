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
// File - GermanExclusionInfo.cpp
//
// class - GermanExclusionInfo
//
//---------------------------------------------------------------------

#include <logos_include/logoscommon.h>

#include <logos_libs/utility/iniparser.h>
#include <logos_libs/halfnoun/germanexclusioninfo.h>
#include <logos_libs/halfnoun/germanutil.h>

//---------------------------------------------------------------------
const short SalValue::WORDCLASS_INDEX = 0;
const short SalValue::SUPERSET_INDEX = 1;
const short SalValue::SET_INDEX = 2;
const short SalValue::SUBSET_INDEX = 3;
const short SalValue::RESTRICT_INDEX = 4;

SalValue::SalValue(const LgsString& value)
{
    // find the comma
    LgsVector(LgsString) salExclusionParameters;
    StringUtil::parseInto(value, salExclusionParameters, ','); 
    superset_ = atoi(salExclusionParameters[SUPERSET_INDEX].c_str());
    set_ = atoi(salExclusionParameters[SET_INDEX].c_str());
    subset_ = atoi(salExclusionParameters[SUBSET_INDEX].c_str());

    strncpy(word_class_, salExclusionParameters[WORDCLASS_INDEX].c_str(), 2);
    const char * cpRestriction = salExclusionParameters[RESTRICT_INDEX].c_str();
    setRestriction(*cpRestriction);
}

bool SalValue::match(const char* word_class, int superset, int set0, int subset0) const
{
    if (*word_class_ != '*' && strncmp(word_class_, word_class, 2) != 0)
        return false;
    return (((superset_ == 0) || (superset_ == superset)) &&
            ((set_ == 0) || (set_ == set0)) &&
            ((subset_ == 0) || (subset_ == subset0)));
}

//---------------------------------------------------------------------

const short WordValue::WORD_INDEX = 0;
const short WordValue::PAT_INDEX = 1;
const short WordValue::STEM_INDEX = 2;
const short WordValue::RESTRICT_INDEX = 3;

WordValue::WordValue(const LgsString& value)
{
    // find the comma
    LgsVector(LgsString) wordExclusionParameters;
    StringUtil::parseInto(value, wordExclusionParameters, ','); 
    m_patValue = atoi(wordExclusionParameters[PAT_INDEX].c_str());
    m_stemValue = atoi(wordExclusionParameters[STEM_INDEX].c_str());
    const char * cpRestriction = wordExclusionParameters[RESTRICT_INDEX].c_str();

    // store word before comma
    word_ = wordExclusionParameters[WORD_INDEX];

    // store restriction after the comma
    setRestriction(*cpRestriction);
}

bool WordValue::match(const LgsString& word, int patValue, int stemValue) const
{
    return word == word_ && m_patValue == patValue && m_stemValue == stemValue;
}

//---------------------------------------------------------------------
EndingValue::EndingValue(const LgsString& value)
    : WordValue(value)
{
}

bool EndingValue::match(const LgsString& word, int patValue, int stemValue) const
{
    return GermanUtil::isEnding(word_.c_str(), word) && m_patValue == patValue &&
           m_stemValue == stemValue;
}

//---------------------------------------------------------------------
PatStemValue::PatStemValue(const LgsString& value)
{
    if (value[0] == '*')
    {
        pat_ = 0;
    }
    else
    {
        assert(CharUtil::isNumeric(value[0]));
        assert(CharUtil::isNumeric(value[1]));
        assert(CharUtil::isNumeric(value[2]));
        pat_ = (value[0] - '0') * 100 + (value[1] - '0') * 10 + (value[2] - '0');
    }
    
    if (value[3] == '*')
    {
        stem_ = 0;
    }
    else
    {
        assert(CharUtil::isNumeric(value[3]));
        assert(CharUtil::isNumeric(value[4]));
        stem_ = (value[3] - '0') * 10 + (value[4] - '0');
    }
    
    setRestriction(value[5]);
}

bool PatStemValue::match(int pat, int stem) const
{
    return (pat_ == 0 || pat_ == pat) && (stem_ == 0 || stem_ == stem);
}

//---------------------------------------------------------------------
GermanExclusionInfo* GermanExclusionInfo::theExclusionInfo_s;
LgsList(SalValue) GermanExclusionInfo::salValue_s;
LgsList(WordValue) GermanExclusionInfo::wordValue_s;
LgsList(EndingValue) GermanExclusionInfo::endingValue_s;
LgsList(PatStemValue) GermanExclusionInfo::patStemValue_s;

void GermanExclusionInfo::initialize(const LgsString& iniFileName)
{
    readIniSal(iniFileName.c_str(), "sal_exclusion");
    readIniWord(iniFileName.c_str(), "word_exclusion");
    readIniEnding(iniFileName.c_str(), "ending_exclusion");
    theExclusionInfo_s = new GermanExclusionInfo();
    readIniPatStem(iniFileName.c_str(), "pat_stem_exclusion");
}

GermanExclusionInfo& GermanExclusionInfo::singleton()
{
    assert(theExclusionInfo_s != 0);

    return *theExclusionInfo_s;
}

void GermanExclusionInfo::readIniSal(const char* iniFileName, const char* section)
{
    IniParser parser;
    parser.open(iniFileName, section);
    int count = parser.AsInteger("count");
    assert(count >= 0);

    for (int i = 0; i < count; i++)
    {
        char key[4];
        sprintf(key, "%02d", i + 1);
        LgsString value = parser.Value(key);
        assert(value.length() >= 10);
        salValue_s.push_back(SalValue(value));
    }
}

void GermanExclusionInfo::readIniWord(const char* iniFileName, const char* section)
{
    IniParser parser;
    parser.open(iniFileName, section);
    int count = parser.AsInteger("count");
    assert(count >= 0);

    for (int i = 0; i < count; i++)
    {
        char key[4];
        sprintf(key, "%02d", i + 1);
        LgsString value = parser.Value(key);
        wordValue_s.push_back(WordValue(value));
    }
}

void GermanExclusionInfo::readIniEnding(const char* iniFileName, const char* section)
{
    IniParser parser;
    parser.open(iniFileName, section);
    int count = parser.AsInteger("count");
    assert(count >= 0);

    for (int i = 0; i < count; i++)
    {
        char key[4];
        sprintf(key, "%02d", i + 1);
        LgsString value = parser.Value(key);
        endingValue_s.push_back(EndingValue(value));
    }
}

void GermanExclusionInfo::readIniPatStem(const char* iniFileName, const char* section)
{
    IniParser parser;
    parser.open(iniFileName, section);
    int count = parser.AsInteger("count");
    assert(count >= 0);

    for (int i = 0; i < count; i++)
    {
        char key[4];
        sprintf(key, "%02d", i + 1);
        LgsString value = parser.Value(key);
        assert(value.length() >= 6);
        patStemValue_s.push_back(PatStemValue(value));
    }
}

Restriction GermanExclusionInfo::getSalRestriction(const LgsString& wordClass, int superset,
                                                   int set0, int subset0) const
{
    assert(wordClass.length() == 2);

    for (LgsList(SalValue)::const_iterator iter = salValue_s.begin(); iter != salValue_s.end(); iter++)
    {
        if (iter->match(wordClass.c_str(), superset, set0, subset0))
        {
            return iter->getRestriction();
        }
    }

    return restrictNoRestriction;
}

Restriction GermanExclusionInfo::getWordRestriction(const LgsString& word, int patValue, int stemValue) const
{
    for (LgsList(WordValue)::const_iterator iter = wordValue_s.begin(); iter != wordValue_s.end(); iter++)
    {
        if (iter->match(word, patValue, stemValue))
        {
            return iter->getRestriction();
        }
    }

    return restrictNoRestriction;
}

Restriction GermanExclusionInfo::getEndingRestriction(const LgsString& word, int patValue, int stemValue) const
{
    for (LgsList(EndingValue)::const_iterator iter = endingValue_s.begin(); iter != endingValue_s.end(); iter++)
    {
        if (iter->match(word, patValue, stemValue))
        {
            return iter->getRestriction();
        }
    }

    return restrictNoRestriction;
}

Restriction GermanExclusionInfo::getPatStemRestriction(int pat, int stem) const
{
    assert(0 <= pat && pat <= 999);
    assert(0 <= stem && stem <= 99);

    for (LgsList(PatStemValue)::const_iterator iter = patStemValue_s.begin();
         iter != patStemValue_s.end();
         iter++)
    {
        if (iter->match(pat, stem))
        {
            return iter->getRestriction();
        }
    }

    return restrictNoRestriction;
}

Restriction GermanExclusionInfo::getRestriction(const LgsString& word, const LgsString& wordClass,
                                                int superset, int set0, int subset0,
                                                int pat, int stem) const
{
    Restriction restr = getWordRestriction(word, pat, stem);
    if (restr != restrictNoRestriction)
        return restr;

    restr = getSalRestriction(wordClass, superset, set0, subset0);
    if (restr != restrictNoRestriction)
        return restr;

    restr = getEndingRestriction(word, pat, stem);
    if (restr != restrictNoRestriction)
        return restr;

    return getPatStemRestriction(pat, stem);
}

