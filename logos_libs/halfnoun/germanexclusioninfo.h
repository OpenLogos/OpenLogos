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
#ifndef __GermanExclusionInfo_h__
#define __GermanExclusionInfo_h__

//---------------------------------------------------------------------
// File - GermanExclusionInfo.h
//
// class - GermanExclusionInfo
//
// Description - class used to hold German Exclusion information
//
//---------------------------------------------------------------------

#include <logos_libs/halfnoun/germanpatstem.h>
#include <logos_libs/halfnoun/germanflag.h>

//---------------------------------------------------------------------
enum Restriction
{
    restrictHeadOnly,
    restrictNonHeadOnly,
    restrictExcluded,
    restrictIncluded,
    restrictNoRestriction
};

//---------------------------------------------------------------------
class RestrictValue
{
public:
    DefaultConstructor(RestrictValue)

    Restriction getRestriction() const;

protected:
    void setRestriction(char c);

private:
    Restriction restrict_;
};

inline void RestrictValue::setRestriction(char c)
{
    switch (c)
    {
    case 'I':
        restrict_ = restrictIncluded;
        break;
    case 'H':
        restrict_ = restrictHeadOnly;
        break;
    case 'N':
        restrict_ = restrictNonHeadOnly;
        break;
    case 'X':
        restrict_ = restrictExcluded;
        break;
    default:
        assert(("invalid value", 0));
        break;
    }
}

inline Restriction RestrictValue::getRestriction() const
{
    return restrict_;
}

//---------------------------------------------------------------------
class SalValue: public RestrictValue
{
public:
    static const short WORDCLASS_INDEX;
    static const short SUPERSET_INDEX;
    static const short SET_INDEX;
    static const short SUBSET_INDEX;
    static const short RESTRICT_INDEX;

    DummyLess(SalValue);
    DummyGreater(SalValue);
    DummyEqual(SalValue);
    DummyNotEqual(SalValue);

    DefaultConstructor(SalValue)
    SalValue(const LgsString& value);

    bool match(const char* word_class, int superset, int set0, int subset0) const;

private:
    char word_class_[2];   // not null-terminated
    int superset_;
    int set_;
    int subset_;
};

//---------------------------------------------------------------------
class WordValue: public RestrictValue
{
public:
    static const short WORD_INDEX;
    static const short PAT_INDEX;
    static const short STEM_INDEX;
    static const short RESTRICT_INDEX;

    DummyLess(WordValue);
    DummyGreater(WordValue);
    DummyEqual(WordValue);
    DummyNotEqual(WordValue);

    DefaultConstructor(WordValue)
    WordValue(const LgsString& value);

    virtual bool match(const LgsString& word, int patValue, int stemValue) const;

protected:
    LgsString word_;
    int m_patValue;
    int m_stemValue;
};

//---------------------------------------------------------------------
class EndingValue: public WordValue
{
public:
    DummyLess(EndingValue);
    DummyGreater(EndingValue);
    DummyEqual(EndingValue);
    DummyNotEqual(EndingValue);

    DefaultConstructor(EndingValue)
    EndingValue(const LgsString& value);

    virtual bool match(const LgsString& word, int patValue, int stemValue) const;
};

//---------------------------------------------------------------------
class PatStemValue: public RestrictValue
{
public:
    DummyLess(PatStemValue);
    DummyGreater(PatStemValue);
    DummyEqual(PatStemValue);
    DummyNotEqual(PatStemValue);

    DefaultConstructor(PatStemValue)
    PatStemValue(const LgsString& value);

    bool match(int pat, int stem) const;

private:
    int pat_;      // 0 matches any value
    int stem_;     // 0 matches any value
};

//---------------------------------------------------------------------
class GermanExclusionInfo
{
public:
    // read details from ini file.
    // also construct the unique instance of this class
    static void initialize(const LgsString& iniFileName);

    // singleton - get unique instance of this class
    static GermanExclusionInfo& singleton();

    // check if the word is restricted
    Restriction getRestriction(const LgsString& word, const LgsString& wordClass,
                               int superset, int set0, int subset0, int pat, int stem) const;

private:
    static GermanExclusionInfo* theExclusionInfo_s;
    static LgsList(SalValue) salValue_s;
    static LgsList(WordValue) wordValue_s;
    static LgsList(EndingValue) endingValue_s;
    static LgsList(PatStemValue) patStemValue_s;

    // read lists for a section from iniFile into array
    static void readIniSal(const char* iniFileName, const char* section);
    static void readIniWord(const char* iniFileName, const char* section);
    static void readIniEnding(const char* iniFileName, const char* section);
    static void readIniPatStem(const char* iniFileName, const char* section);

    DefaultConstructor(GermanExclusionInfo)

    Restriction getSalRestriction(const LgsString& wordClass, int superset, int set0, int subset0) const;
    Restriction getWordRestriction(const LgsString& word, int patValue, int stemValue) const;
    Restriction getEndingRestriction(const LgsString& word, int patValue, int stemValue) const;
    Restriction getPatStemRestriction(int pat, int stem) const;
};

#endif



