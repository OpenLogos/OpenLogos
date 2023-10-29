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
#ifndef __GermanDictionaryEntry_h__
#define __GermanDictionaryEntry_h__

//---------------------------------------------------------------------
// File - GermanDictionaryEntry.h
//
// class - GermanWC_and_ConnRes
// class - GermanDictionaryEntry
//
// Description - German class used to hold cached dictionary entries
//
//---------------------------------------------------------------------

#include <logos_libs/halfnoun/germanflag.h>
#include <logos_libs/halfnoun/germanheadwordinfo.h>
#include <logos_libs/halfnoun/germanwordclass.h>
#include <logos_libs/halfnoun/germanconnectorresolution.h>

class GermanTokenTrialConnector;

const int COMPANY_LENGTH = 3;

//---------------------------------------------------------------------
// stores the GermanWordClass and how the connector information was resolved
// NB: no virtual functions - so we can cast this to a unsigned char*.
class GermanWC_and_ConnRes
{
public:
    DefaultConstructor(GermanWC_and_ConnRes)
    GermanWC_and_ConnRes(GermanWordClass wordClass);
    GermanWC_and_ConnRes(unsigned char c);

    void setConnectorResolution(ConnectorResolution res);

    GermanWordClass getWordClass() const;
    ConnectorResolution getConnectorResolution() const;
    unsigned char toChar() const;

private:
    struct
    {
        unsigned char wc_: 5;
        unsigned char res_: 3;
    } data_;
};

inline GermanWC_and_ConnRes::GermanWC_and_ConnRes(GermanWordClass wordClass)
{
    data_.wc_ = wordClass.encode();
    data_.res_ = (unsigned char)(connNone);
}

inline GermanWC_and_ConnRes::GermanWC_and_ConnRes(unsigned char c)
{
    *(unsigned char*)this = c;
}

inline GermanWordClass GermanWC_and_ConnRes::getWordClass() const
{
    return GermanWordClass(data_.wc_);
}

inline ConnectorResolution GermanWC_and_ConnRes::getConnectorResolution() const
{
    return ConnectorResolution(data_.res_);
}

inline void GermanWC_and_ConnRes::setConnectorResolution(ConnectorResolution res)
{
    data_.res_ = (unsigned char)(res);
}

inline unsigned char GermanWC_and_ConnRes::toChar() const
{
    return *(unsigned char*)this;
}

//---------------------------------------------------------------------
// Memory allocated need never be freed - since these records are used
//     throughout the program's duration.
// allocation are not logged.
// copy and assignment - defaults - are shallow
class GermanDictionaryEntry
{
public:
    class Dummy {};

    GermanDictionaryEntry(void);

    GermanDictionaryEntry(const char* company, const char* text, GermanWordClass wordClass, short patNumber);

    // special constructor - no allocation - just copies normalized form of word
    // flags will not be set. Used only as a parameter to binary search
    GermanDictionaryEntry(const char* text, Dummy);

    // create a duplicate entry from a given entry
    // used for duplicates of nouns like Balett - ending in double consonant
    //     where we add a special entry Balet to our cache
    // The duplicate is a non-head word, and must be followed
    // by a word starting with the the consonant that was removed
    // Word class: halfnounForm
    // Flag settings: de_initialCapital - copied from original
    //                de_doubleEndingRemoved - true
    //                de_nonNormalizedStored - true
    //                conn_none - true
    //                all others ------------ false
    GermanDictionaryEntry(GermanDictionaryEntry* original, const char* normalizedText);

    // get text in the non-normalized and normalized forms
    LgsString getText() const;
    const char* getNormalizedText() const;

    // read/write data on an output stream in binary format
    void printOn(ofstream& output) const;
    bool readFrom(istream& input);

    // write data on an output stream in text format
    void printOnAsText(ofstream& output) const;

    // return the flags for the entry
    const GermanDictionaryEntryFlag& getFlags() const;

    // update the connector and suffix information
    void update(GermanWordClass wordClass, int pat, int stem, const LgsString& company,
                int s0, int s1, int s2);

    // return true if the word has a noun form that may appear in the non-head position
    // used to decide whether to add a pseudo entry for duplicate letters
    // only add a pseudo entry for doubled consonants at end of word for nouns
    bool hasNonHead() const;

    // return company code for the entry
    LgsString getCompany() const;

    // return the word class for the entry
    GermanWordClass getWordClass() const;

    // return the pat number for the entry
    short getPatNumber() const;

    // does the entry have the suffix for the given headWordInfo
    bool hasSuffix(const char* suffix, GermanHeadWordInfo headWordInfo) const;

    // return the maximum connector length less than maxLength of the input text
    // if only the <no-connector> option is possible return 0
    // if no applicable connector return -1
    int maxConnectorLength(const char* text, GermanTokenTrialConnector* trialConnector) const;
	bool isDoubleEnded(void) const;

private:
    // bit-array class storing information about normalization, suffixes, connectors
    GermanDictionaryEntryFlag flags_;

    // Null-terminated LgsString of normalized form of the word in the dictionary
    // The word is normalized by converting to lower-case and replacing
    // any eszett with 'ss'.
    // If the nonNormalizedStored flag is set the non-normalized form of the
    // word is also stored, as a null terminated LgsString following normalized form.
    char* buffer_;
    char company_[COMPANY_LENGTH];
    GermanWC_and_ConnRes wordClass_and_connRes_;
    short patNumber_;

    // store the company code
    void storeCompany(const char* company);

    void createBuffer(const char* text, const char* normalizedText,
        bool initialCapital, bool doubleEndingRemoved, bool otherChange);

    // if the flagValue is set return true iff the text matches the suffix
    // if the flagValue is not set return false
    bool checkSuffix(const char* text, const char* suffix, SuffixFlagValue flagValue) const;

    // does the connecto match the text and the flag
    short checkConnector(const char* text, const char* connector, int length,
        ConnectorFlagValue flagValue) const;

    ConnectorResolution getConnectorResolution() const;
    void setConnectorResolution(ConnectorResolution res);
};

typedef LgsVector(GermanDictionaryEntry) GermanDictionaryEntryVector;

inline ostream& operator<<(ostream& os, const GermanDictionaryEntry& entry)
{
    return os << entry.getText() << "(" << entry.getWordClass().toChar() << ")";
}

inline const char* GermanDictionaryEntry::getNormalizedText() const
{
    return buffer_;
}

inline bool operator<(const GermanDictionaryEntry& lhs, const GermanDictionaryEntry& rhs)
{
    return strcmp(lhs.getNormalizedText(), rhs.getNormalizedText()) < 0;
}

inline bool operator==(const GermanDictionaryEntry& lhs, const GermanDictionaryEntry& rhs)
{
    return strcmp(lhs.getNormalizedText(), rhs.getNormalizedText()) == 0;
}

inline const GermanDictionaryEntryFlag& GermanDictionaryEntry::getFlags() const
{
    return flags_;
}

inline LgsString GermanDictionaryEntry::getCompany() const
{
    return LgsString((char*)company_, (char*)company_ + COMPANY_LENGTH);
}

inline GermanWordClass GermanDictionaryEntry::getWordClass() const
{
    return wordClass_and_connRes_.getWordClass();
}

inline short GermanDictionaryEntry::getPatNumber() const
{
    return patNumber_;
}

inline ConnectorResolution GermanDictionaryEntry::getConnectorResolution() const
{
    return wordClass_and_connRes_.getConnectorResolution();
}

inline void GermanDictionaryEntry::setConnectorResolution(ConnectorResolution res)
{
    wordClass_and_connRes_.setConnectorResolution(res);
}

inline bool GermanDictionaryEntry::isDoubleEnded(void) const
{
	return flags_.get(de_doubleEndingRemoved);
}

#endif



