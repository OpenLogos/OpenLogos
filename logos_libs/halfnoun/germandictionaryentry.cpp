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
// File - GermanDictionaryEntry.cpp
//
// class - GermanDictionaryEntry
//
//---------------------------------------------------------------------

#include <logos_include/logoscommon.h>

#include <logos_libs/regex/charutil.h>
#include <logos_libs/utility/stringutil.h>
#include <logos_libs/halfnoun/germandictionaryentry.h>
#include <logos_libs/halfnoun/germanbufferlength.h>
#include <logos_libs/halfnoun/germanutil.h>
#include <logos_libs/halfnoun/germanpatsteminfo.h>
#include <logos_libs/halfnoun/germansuffixinfo.h>
#include <logos_libs/halfnoun/germanconnectorinfo.h>
#include <logos_libs/halfnoun/germantoken.h>
#include <logos_libs/halfnoun/logfile.h>

//---------------------------------------------------------------------
GermanDictionaryEntry::GermanDictionaryEntry(const char* company, const char* text,
    GermanWordClass wordClass, short patNumber)
    : wordClass_and_connRes_(wordClass)
    , patNumber_(patNumber)
{
    storeCompany(company);
    char normalizedText[BUFFER_LENGTH];
    bool initialCapital, otherChange;
    GermanUtil::normalize(text, normalizedText, initialCapital, otherChange);
    createBuffer(text, normalizedText, initialCapital, false, otherChange);
}

GermanDictionaryEntry::GermanDictionaryEntry(
    GermanDictionaryEntry* original, const char* normalizedText)
    : wordClass_and_connRes_(GermanWordClass(GermanWordClass::noun, false, true))
{
    storeCompany(original->getCompany().c_str());
    createBuffer(original->getText().c_str(), normalizedText,
        original->getFlags().get(de_initialCapital), true, true);
    if (original->getFlags().get(de_initialCapital))
        flags_.set(de_initialCapital);
    flags_.set(de_doubleEndingRemoved);
    flags_.set(de_nonNormalizedStored);
    flags_.set(conn_none, 1);
    patNumber_ = original->getPatNumber();
}

GermanDictionaryEntry::GermanDictionaryEntry(const char* text, Dummy)
    : buffer_((char*)text)
{
}

GermanDictionaryEntry::GermanDictionaryEntry(void) : buffer_(0)
{
	
}

void GermanDictionaryEntry::storeCompany(const char* company)
{
    strncpy(company_, company, COMPANY_LENGTH);
}

LgsString GermanDictionaryEntry::getText() const
{
    if (flags_.get(de_nonNormalizedStored))
    {
        // return stored value
        return LgsString(buffer_ + strlen(buffer_) + 1);
    }

    // calculate non-normalized form

    LgsString ret;

    // reserve space for extra character at the end
    ret.reserve(strlen(buffer_) + 1);

    // start with normalized form
    ret = buffer_;

    // capitalize first character if flag set
    if (flags_.get(de_initialCapital))
        StringUtil::capitalize(ret);

    // double up last character if flag set
    if (flags_.get(de_doubleEndingRemoved))
        ret.insert(ret.end(), 1, ret[ret.length() - 1]);

    return ret;
}

void GermanDictionaryEntry::createBuffer(const char* text, const char* normalizedText,
    bool initialCapital, bool doubleEndingRemoved, bool otherChange)
{
    if (initialCapital)
        flags_.set(de_initialCapital);

    if (doubleEndingRemoved)
        flags_.set(de_doubleEndingRemoved);

    if (otherChange)
        flags_.set(de_nonNormalizedStored);

    // buffer_ will hold one or 2 c-strings: normalized text followed by text
    // write normalized LgsString first, since it used most heavily

    // allocate buffer_ - no logging, never freed
    int length = strlen(text) + 1;
    if (otherChange)
        length += strlen(normalizedText) + 1;
    buffer_ = new char[length];

    // copy normalized text
    char* to = buffer_;
    const char* from = normalizedText;
    while (*from)
    *to++ = *from++;
    *to++ = 0;

    // copy text
    if (otherChange)
    {
        from = text;
        while (*from)
            *to++ = *from++;
        *to++ = 0;
    }
}

void GermanDictionaryEntry::printOn(ofstream& output) const
{
    int textLength = strlen(buffer_);
    if (flags_.get(de_nonNormalizedStored))
    {
        // allow extra space for terminating null of normalized part
        // as well as the un-normalized part, without a null
        textLength += 1 + getText().length();
    }

    flags_.printOn(output);
    GermanUtil::printCharArrayOn(company_, COMPANY_LENGTH, output);
    GermanUtil::printStringOn(buffer_, textLength, output);
    GermanUtil::printCharOn(wordClass_and_connRes_.toChar(), output);
    GermanUtil::printShortOn(patNumber_, output);
}

void GermanDictionaryEntry::printOnAsText(ofstream& output) const
{
    output << getNormalizedText()
           << '(' << getCompany() << ":" << getText() << ") "
           << getWordClass() << " "
           << getConnectorResolution() << " ";
    flags_.printOnAsText(output);
}

bool GermanDictionaryEntry::readFrom(istream& input)
{
    if (!flags_.readFrom(input))
        return false;

    if (!GermanUtil::readCharArrayFrom(company_, COMPANY_LENGTH, input))
        return false;

    if (!GermanUtil::readStringFrom(buffer_, input))
        return false;

    char temp;
    if (!GermanUtil::readCharFrom(temp, input))
        return false;
    GermanWC_and_ConnRes wc(temp);
    wordClass_and_connRes_ = wc;

    if (!GermanUtil::readShortFrom(patNumber_, input))
        return false;

    return true;
}

void GermanDictionaryEntry::update(GermanWordClass wordClass, int pat, int stem,
                                   const LgsString& company, int s0, int s1, int s2)
{
    bool addSuffixes = false;
    bool addConnectors = false;
    if (wordClass.getPartOfSpeech() == GermanWordClass::arith14 ||
        wordClass.getPartOfSpeech() == GermanWordClass::arith16)
    {
        flags_.set(suf_none);
        flags_.set(conn_none, 1);
    }
    else if (wordClass.getPartOfSpeech() == GermanWordClass::halfnoun)
    {
        flags_.set(conn_none, 1);
    }
    else
    {
        if (wordClass.inHead())
            addSuffixes = true;
        if (wordClass.inNonHead())
            addConnectors = true;
    }

#ifdef DEBUG_LOG_ON
LogFile::singleton().printDateTime()
<< "\t\t Add " << (addSuffixes ? "suffixes " : "no suffixes ")
<< (addConnectors ? "connectors" : "no connectors") << endl;
#endif

    if (addSuffixes)
    {
        flags_ |= GermanSuffixInfo::singleton().getSuffixesByPatStem(pat, stem);
    }

    if (addConnectors)
    {
        const LgsString& unNormalized = getText();
        char lastLetter = unNormalized[unNormalized.length() - 1];
        lastLetter = CharUtil::lower(lastLetter);

        ConnectorResolution res;
        flags_ |= GermanConnectorInfo::singleton().getConnectors(
            getNormalizedText(), wordClass, company, pat, stem, lastLetter, res);
        setConnectorResolution(res);
    }
}

bool GermanDictionaryEntry::hasNonHead() const
{
    return getWordClass().inNonHead();
}

bool GermanDictionaryEntry::hasSuffix(const char* suffix, GermanHeadWordInfo headWordInfo) const
{
    if (headWordInfo == noHeadWord)
        return false;

    return
      checkSuffix(suffix, ""    , suf_none) ||
      checkSuffix(suffix, "e"   , suf_e   ) ||
      checkSuffix(suffix, "em"  , suf_em  ) ||
      checkSuffix(suffix, "en"  , suf_en  ) ||
      checkSuffix(suffix, "end" , suf_end ) ||
      checkSuffix(suffix, "ens" , suf_ens ) ||
      checkSuffix(suffix, "er"  , suf_er  ) ||
      checkSuffix(suffix, "ere" , suf_ere ) ||
      checkSuffix(suffix, "erem", suf_erem) ||
      checkSuffix(suffix, "eren", suf_eren) ||
      checkSuffix(suffix, "erer", suf_erer) ||
      checkSuffix(suffix, "eres", suf_eres) ||
      checkSuffix(suffix, "ern" , suf_ern ) ||
      checkSuffix(suffix, "es"  , suf_es  ) ||
      checkSuffix(suffix, "et"  , suf_et  ) ||
      checkSuffix(suffix, "ete" , suf_ete ) ||
      checkSuffix(suffix, "eten", suf_eten) ||
      checkSuffix(suffix, "ien" , suf_ien ) ||
      checkSuffix(suffix, "le"  , suf_le  ) ||
      checkSuffix(suffix, "n"   , suf_n   ) ||
      checkSuffix(suffix, "nen" , suf_nen ) ||
      checkSuffix(suffix, "ns"  , suf_ns  ) ||
      checkSuffix(suffix, "s"   , suf_s   ) ||
      checkSuffix(suffix, "se"  , suf_se  ) ||
      checkSuffix(suffix, "sen" , suf_sen ) ||
      checkSuffix(suffix, "ses" , suf_ses ) ||
      checkSuffix(suffix, "ste" , suf_ste ) ||
      checkSuffix(suffix, "stem", suf_stem) ||
      checkSuffix(suffix, "sten", suf_sten) ||
      checkSuffix(suffix, "ster", suf_ster) ||
      checkSuffix(suffix, "stes", suf_stes) ||
      checkSuffix(suffix, "t"   , suf_t   ) ||
      checkSuffix(suffix, "te"  , suf_te  ) ||
      checkSuffix(suffix, "esten" , suf_esten ) ||
		checkSuffix(suffix, "ester" , suf_ester ) ||
		checkSuffix(suffix, "estem" , suf_estem ) ||
		checkSuffix(suffix, "estes" , suf_estes ) ||
		checkSuffix(suffix, "in" , suf_in ) ||
		checkSuffix(suffix, "innen" , suf_innen ) ||
		checkSuffix(suffix, "Innen", suf_Innen) ||
		checkSuffix(suffix, "este" , suf_este );
}

bool GermanDictionaryEntry::checkSuffix(const char* text, const char* suffix,
                                        SuffixFlagValue flagValue) const
{
    return flags_.get(flagValue) && strcmp(text, suffix) == 0;
}

short GermanDictionaryEntry::checkConnector(const char* text, const char* connector, int length,
                                            ConnectorFlagValue flagValue) const
{
    short connectorWeight = 0;
    if (strncmp(text, connector, length) == 0)
    {
      connectorWeight = flags_.get(flagValue);
    }
    return  connectorWeight ;
}

int GermanDictionaryEntry::maxConnectorLength(const char* text, GermanTokenTrialConnector* trialConnector) const
{
    assert(trialConnector->maxLength() >= 0);

    short connWeight = 0;
    if (trialConnector->maxLength() >= 3)
    {
        if (connWeight = checkConnector(text, "nen", 3, conn_nen))
            trialConnector->insert(connWeight, 3, this);
    }
    if (trialConnector->maxLength() >= 2)
    {
        if ((connWeight = checkConnector(text, "es", 2, conn_es)) ||
            (connWeight = checkConnector(text, "er", 2, conn_er)) ||
            (connWeight = checkConnector(text, "en", 2, conn_en)))
               trialConnector->insert(connWeight, 2, this);
    }
    if (trialConnector->maxLength() >= 1)
    {
        if ((connWeight = checkConnector(text, "e", 1, conn_e)) ||
            (connWeight = checkConnector(text, "n", 1, conn_n)) ||
            (connWeight = checkConnector(text, "s", 1, conn_s)))
               trialConnector->insert(connWeight, 1, this);;
    }
    if (connWeight = flags_.get(conn_none))
    {
       trialConnector->insert(connWeight, 0, this);
    }
    trialConnector->reset();
    return  trialConnector->length();
}

