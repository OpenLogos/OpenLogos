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
// File - GermanDictionary1.cpp
//
// class - GermanDictionaryField
// class - GermanDictionary - database dependent code
//
//---------------------------------------------------------------------

#include <logos_include/logoscommon.h>

#include <logos_libs/utility/stringutil.h>
//#include <logos_libs/odbcsql/odbcconnection.h>
#include <logos_libs/sql/sqlstatement.h>
#include <logos_libs/halfnoun/germandictionary.h>
#include <logos_libs/halfnoun/germandictionaryentry.h>
#include <logos_libs/halfnoun/germanutil.h>
#include <logos_libs/halfnoun/germanbufferlength.h>
#include <logos_libs/halfnoun/germanconnectorinfo.h>
#include <logos_libs/halfnoun/germansuffixinfo.h>
#include <logos_libs/halfnoun/germanpatsteminfo.h>
#include <logos_libs/halfnoun/germanexclusioninfo.h>
#include <logos_libs/halfnoun/logfile.h>

//---------------------------------------------------------------------

bool GermanDictionaryField::isBreak(
    const LgsString& company, const LgsString& word, const LgsString& wordClass, int pat, int stem)
{
    if (word != word_ || company != company_ || wordClass != wordClass_ || pat != pat_ || stem != stem_)
    {
        word_ = word;
        company_ = company;
        wordClass_ = wordClass;
        pat_ = pat;
        stem_ = stem;
        return true;
    }

    return false;
}

//---------------------------------------------------------------------

GermanDictionary::~GermanDictionary(void)
{
	LgsVector(GermanDictionaryEntry)::iterator endIter = wordList_.end();

	for (LgsVector(GermanDictionaryEntry)::iterator currIter = wordList_.begin();
		 currIter != endIter;
		 currIter++ )
	{
		char * textBuffer = const_cast<char *>((*currIter).getNormalizedText());
		if (textBuffer)
		{
			delete [] textBuffer;
		}
	}
   
   LogFile::cleanup();
}

void GermanDictionary::readDataBase(SqlConnection* connect)
{
    assert(wordList_.size() == 0);

    readDataBase_(connect);
    sortCache();
    streamOut();
}

void GermanDictionary::readDataBase_(SqlConnection* connect)
{
    Timer timer(LgsString("readDataBase"));

    // intialize connector information
    GermanConnectorInfo::initialize(iniFileName_, connect);

    // initialize suffix information
    GermanSuffixInfo::initialize(connect);

    // initialize proper noun information
    GermanExclusionInfo::initialize(iniFileName_);

    // initialize pat/stem information
    GermanPatStemInfo::initialize(connect);

    readWordList(connect);
}

bool GermanDictionary::convertWordClass(const LgsString& word, const LgsString& clazz, int pat, int stem,
                                        int s0, int s1, int s2, GermanWordClass& wordClass)
{
    assert(clazz.length() == 2);

#ifdef DEBUG_LOG_ON
LogFile::singleton().printDateTime() << "\t\t Word class is " << clazz << endl;
#endif

    // deal with exclusions
    Restriction restriction = GermanExclusionInfo::singleton().getRestriction(word, clazz, s0, s1, s2, pat, stem);

#ifdef DEBUG_LOG_ON
if (restriction == restrictHeadOnly)    LogFile::singleton().printDateTime() << "\t\t non-head position not allowed" << endl;
if (restriction == restrictNonHeadOnly) LogFile::singleton().printDateTime() << "\t\t head position not allowed" << endl;
if (restriction == restrictExcluded)    LogFile::singleton().printDateTime() << "\t\t word is excluded from cache" << endl;
if (restriction == restrictIncluded)    LogFile::singleton().printDateTime() << "\t\t word can be in any position" << endl;
#endif

    if (restriction == restrictExcluded)
        return false;

    // calculate word-class
    GermanWordClass::PartOfSpeech wc;
    switch (clazz[0])
    {
    case '0':
        switch (clazz[1])
        {
        case '1':
            wc = GermanWordClass::noun;
            break;
        case '2':
            wc = GermanWordClass::verb;
            break;
        case '4':
            // Stored half-nouns have a pat of 485, adjectives do not.
            if (pat == 485)
                wc = GermanWordClass::halfnoun;
            else
                wc = GermanWordClass::adj;
            break;

        default:
            assert(("invalid value", 0));
            break;
        }
        break;

    case '1':
        switch (clazz[1])
        {
        case '4':
            wc = GermanWordClass::arith14;
            break;
        case '6':
            wc = GermanWordClass::arith16;
            break;
        default:
            assert(("invalid value", 0));
            break;
        }
        break;

    default:
        assert(("invalid value", 0));
        break;
    }

    // set allowed positions based on restriction
    bool inHead, inNonHead;

    // stored half-nouns cannot be overridden - only excluded above
    if (wc == GermanWordClass::halfnoun)
    {
        inHead = false;
        inNonHead = true;
    }
    else
    {
        switch (restriction)
        {
        case restrictHeadOnly:
            inHead = true;
            inNonHead = false;
            break;
        case restrictNonHeadOnly:
            inHead = false;
            inNonHead = true;
            break;
        case restrictIncluded:
            inHead = true;
            inNonHead = true;
            break;
        case restrictNoRestriction:
            break;
        default:
            assert(("invalid value", 0));
            break;
        }
    }

    if (restriction != restrictNoRestriction)
        wordClass.reset(wc, inHead, inNonHead);
    else if (GermanPatStemInfo::singleton().isNonHeadPatStem(wc, pat, stem))
        wordClass.reset(wc, true, true);
    else
        wordClass.reset(wc, true, false);

    return true;
}

void GermanDictionary::readWordList(SqlConnection* connect)
{
    Timer timer(LgsString("readWordlist"));


    LgsString statementText =
        "select "
        "  company_code, word, word_class_code, "
        "  pat, stem, superset_id, set_id, subset_id "
        "from halfnoun_dictionary "
        "order by company_code, word, word_class_code, pat, stem";

    // turn off allocation logging - don't use auto_ptr_debug - since
    //      this is defined as auto_ptr which calls delete of the ptr
    //      but the ptr was allocated in the library with new
    //      so there is a mis-match.
    // rather live with a memory leak here
    SqlStatement* statement = connect->CreateStatement();
    assert(statement != 0);

    statement->AddToCommandString(statementText);
    statement->Parse();

    SqlColumn* colCompany = statement->BindOutputColumn(1, SqlColumn::StringType);
    SqlColumn* colWord    = statement->BindOutputColumn(2, SqlColumn::StringType);
    SqlColumn* colClass   = statement->BindOutputColumn(3, SqlColumn::StringType);
    SqlColumn* colPat     = statement->BindOutputColumn(4, SqlColumn::Integer);
    SqlColumn* colStem    = statement->BindOutputColumn(5, SqlColumn::Integer);
    SqlColumn* colS0      = statement->BindOutputColumn(6, SqlColumn::Integer);
    SqlColumn* colS1      = statement->BindOutputColumn(7, SqlColumn::Integer);
    SqlColumn* colS2      = statement->BindOutputColumn(8, SqlColumn::Integer);

    statement->Execute();

    //ifstream temp("c:\\logos_dev\\logos_libs\\halfnoun\\logs\\sql.log");
    //char tempBuffer[512];

    int totalWords = 0;
    int validWords = 0;
    int endsInDoubleConsonantWords = 0;
    GermanDictionaryField field;
    GermanDictionaryEntry* dictEntry = 0;

    for (;;)
    {

        if (!statement->Fetch())
            break;

        LgsString company = colCompany->AsString();
        LgsString word = colWord->AsString();
        LgsString clazz = colClass->AsString();
        int pat = colPat->AsInteger();
        int stem = colStem->AsInteger();
        int s0 = colS0->AsInteger();
        int s1 = colS1->AsInteger();
        int s2 = colS2->AsInteger();

#ifdef DEBUG_LOG_ON
LogFile::singleton().printDateTime() << "\tWord = " << word << endl;
#endif

        if (company.length() == 0)
        {
            LogFile::singleton().printDateTime()
                << "\t\t empty company for word <" << word << ">"
                << endl;
            continue;
        }

        if (word.length() == 0)
        {
            LogFile::singleton().printDateTime()
                << "\t\t empty word for company " << company
                << endl;
            continue;
        }

        GermanWordClass wordClass;
        if (!convertWordClass(word, clazz, pat, stem, s0, s1, s2, wordClass))
            continue;

        bool mustUpdate = false;
        if (field.isBreak(company, word, clazz, pat, stem))
        {
#ifdef DEBUG_LOG_ON
LogFile::singleton().printDateTime() << "\t\t Field break" << endl;
if (dictEntry == 0) LogFile::singleton().printDateTime() << "Previous entry is null" << endl;
#endif
            totalWords++;

            // store the previous dictionary entry
            storeEntry(dictEntry, totalWords, validWords, endsInDoubleConsonantWords);

            // calculate wordClass and
            // create a new dictionary entry
            if (GermanUtil::validCompoundPart(word.c_str()))
            {
                validWords++;
                dictEntry = new GermanDictionaryEntry(company.c_str(), word.c_str(), wordClass, pat);
                mustUpdate = true;
            }
            else
            {
#ifdef DEBUG_LOG_ON
LogFile::singleton().printDateTime() << "\t\t Invalid entry" << endl;
#endif
                dictEntry = 0;
            }
        }
        else
        {
            mustUpdate = dictEntry != 0;

#ifdef DEBUG_LOG_ON
LogFile::singleton().printDateTime() << "\t\t Field continuation row." << endl;
#endif
        }

        if (mustUpdate)
        {
            dictEntry->update(wordClass, pat, stem, company, s0, s1, s2);
        }
        else
        {
#ifdef DEBUG_LOG_ON
LogFile::singleton().printDateTime() << "\t\t No update." << endl;
#endif
        }
    }

    // store the final dictionary entry
    storeEntry(dictEntry, totalWords, validWords, endsInDoubleConsonantWords);

    cout << "totalWords = " << totalWords << endl;
    cout << "validWords = " << validWords << endl;
    cout << "endsInDoubleConsonantWords = " << endsInDoubleConsonantWords << endl;
}

void GermanDictionary::storeEntry(GermanDictionaryEntry* dictEntry, int& total, int& valid, int& doubled)
{
    if (dictEntry == 0)
        return;

    if (storeEntry(dictEntry))
    {

#ifdef DEBUG_LOG_ON
LogFile::singleton().printDateTime() << "Previous entry ended in double-consonant" << endl;
#endif
        total++;
        valid++;
        doubled++;
    }
    delete dictEntry;
}

bool GermanDictionary::storeEntry(GermanDictionaryEntry* dictEntry)
{
    bool ret = false;
    wordList_.push_back(*dictEntry);

    const char* word = dictEntry->getNormalizedText();
    if (dictEntry->hasNonHead() && GermanUtil::endsInDoubleConsonant(word))
    {
        // Add in the word with the double consonant replaced by a single consonant
        // eg for Balett - add in Balet
        // This is used to decompose compound words using the old German spelling rules
        // eg 'Balettheater' decomposes as 'Balett Theater' (new spelling is 'Baletttheater')
        char buffer[BUFFER_LENGTH];
        strcpy(buffer, word);
        GermanUtil::removeLastLetter(buffer);
        if (GermanUtil::validCompoundPartLength(buffer))
        {
            wordList_.push_back(GermanDictionaryEntry(
                dictEntry, buffer));
            ret = true;
        }
    }

    return ret;
}

