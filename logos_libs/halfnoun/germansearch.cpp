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
// File - GermanSearch.h
//
// class - GermanSearch
//
//---------------------------------------------------------------------

#include <logos_include/logoscommon.h>

#include <logos_libs/halfnoun/germansearch.h>
#include <logos_libs/halfnoun/germandictionaryentry.h>
#include <logos_libs/utility/iniparser.h>

//---------------------------------------------------------------------
bool GermanSearchItem::isTrialWordToken() const
{
    return dynamic_cast<const GermanTokenTrialWord*>(decomposition_->back()) != 0;
}

bool GermanSearchItem::isTrialConnectorToken() const
{
    return dynamic_cast<const GermanTokenTrialConnector*>(decomposition_->back()) != 0;
}

bool GermanSearchItem::isTrialSuffixToken() const
{
    return dynamic_cast<const GermanTokenTrialSuffix*>(decomposition_->back()) != 0;
}

int GermanSearchItem::longestPrefix(
    GermanDictionary& dictionary, GermanDictionary::Range& range)
{
    assert(isTrialWordToken());

    int length = dictionary.longestPrefix(buffer_.c_str(),
        minWordLength(), getTrialTokenWordLength(), range);
    updateTrialTokenWordLength(length);
    return length;
}

int GermanSearchItem::connectorLength()
{
    GermanTokenAbstract* back = decomposition_->back();
    GermanTokenTrialConnector* trialConnector = dynamic_cast<GermanTokenTrialConnector*>(back);
    assert(trialConnector != 0);

    // get maximum connector length for all entries for the word -
    // of length no more than trialConnector->length_
    if (!trialConnector->initialized())
    {
		for (GermanTokenWord::EntryLinkConstIterator iter = trialConnector->word_->entries_.begin();
            iter != trialConnector->word_->entries_.end(); iter++)
      {
         (*iter).first->maxConnectorLength(buffer_.c_str(), trialConnector);
      }
	  trialConnector->word_->removeNoConnMatchEntries();
      trialConnector->setAsInitialized();
    }

    return trialConnector->length();
}

bool GermanSearchItem::mustBacktrack() const
{
    if (!isTrialWordToken())
        return false;
    return getTrialTokenWordLength() < minWordLength();
}

void GermanSearchItem::popBack(int tokens)
{
    for (int i = 0; i < tokens; i++)
    {
        if (decomposition_->empty())
            return;

        GermanToken* token = dynamic_cast<GermanToken*>(decomposition_->back());
        if (token != 0)
            buffer_.retreat(token->text_.length());

        delete decomposition_->back();
        decomposition_->pop_back();
    }
}

void GermanSearchItem::backtrackBigStep()
{
    GermanTokenSuffix* suffix = dynamic_cast<GermanTokenSuffix*>(decomposition_->back());
    if (suffix)
        popBack(1);           // remove suffix
    else
    {
        GermanTokenWord* word = dynamic_cast<GermanTokenWord*>(decomposition_->back());
        if (word)
		{
            popBack(1);       // remove word
			wordCount_--;
		}
        else
        {
            assert(dynamic_cast<GermanTokenTrialWord*>(decomposition_->back()) != 0);
            popBack(2);       // remove trial word and previous token ??
        }
    }
}

void GermanSearchItem::backtrack()
{
    GermanTokenTrialSuffix* trialSuffix = dynamic_cast<GermanTokenTrialSuffix*>(decomposition_->back());

    if (trialSuffix != 0)
    {
        delete decomposition_->back();
        decomposition_->pop_back();

        // since the connector needs a word of length at least minWordLength() to follow there is no
        // need for a connector unless length of buffer is minWordLength() + 1 or more
        if (buffer_.length() <= minWordLength())
            appendTrialWord();
        else
            appendTrialConnector();
    }
    else
    {
        GermanTokenTrialConnector* trialConnector = dynamic_cast<GermanTokenTrialConnector*>(
            decomposition_->back());
        if (trialConnector != 0)
        {
            if (trialConnector->length() >= 0)
                trialConnector->backtrack();
            if (trialConnector->length() < 0)
            {
                delete decomposition_->back();
                decomposition_->pop_back();

                // no connectors are allowed - hence we need to backtrack again
                popBack(1);     // remove previous word exposing trial word
                backtrack();
            }
        }
        else
        {
            GermanTokenTrialWord* trialWord = dynamic_cast<GermanTokenTrialWord*>(decomposition_->back());
            assert(trialWord != 0);
            assert(trialWord->length_ > 0);
            trialWord->length_--;
        }
    }
}

void GermanSearchItem::appendToken(int length, GermanTokenAbstract* token)
{
    decomposition_->push_back(token);
    buffer_.advance(length);
}

void GermanSearchItem::appendWord(int wordLength, LgsList(const GermanDictionaryEntry*)& entries)
{
    appendToken(getTrialTokenWordLength(),
        new GermanTokenWord(buffer_.word(wordLength), entries));
	wordCount_++;
}

void GermanSearchItem::appendConnector()
{
    int length = getTrialTokenConnectorLength();
    appendToken(length, new GermanTokenConnector(buffer_.c_str(), length));
}

void GermanSearchItem::appendSuffix()
{
    decomposition_->push_back(new GermanTokenSuffix(buffer_.word(buffer_.length())));
    buffer_.advance(buffer_.length());
}

int GermanSearchItem::getTrialTokenWordLength() const
{
    const GermanTokenTrialWord* back = dynamic_cast<const GermanTokenTrialWord*>(decomposition_->back());
    assert(back != 0);
    return back->length_;
}

int GermanSearchItem::getTrialTokenConnectorLength() const
{
    const GermanTokenTrialConnector* back = dynamic_cast<const GermanTokenTrialConnector*>(decomposition_->back());
    assert(back != 0);
    return back->length();
}

void GermanSearchItem::updateTrialTokenWordLength(int length)
{
    GermanTokenTrialWord* back = dynamic_cast<GermanTokenTrialWord*>(decomposition_->back());
    assert(back != 0);
    back->length_ = length;
}

void GermanSearchItem::appendTrialConnector()
{
    int length = min(longestConnector(), int(buffer_.length()));
    GermanTokenWord* word = dynamic_cast<GermanTokenWord*>(decomposition_->back());
    assert(word != 0);
    decomposition_->push_back(new GermanTokenTrialConnector(length, word));
}

void GermanSearchItem::appendTrialSuffix()
{
    GermanTokenWord* word = dynamic_cast<GermanTokenWord*>(decomposition_->back());
    assert(word != 0);
    decomposition_->push_back(new GermanTokenTrialSuffix(word));
}

bool GermanSearchItem::validCompanyForEntry(const GermanDictionaryEntry& entry) const
{
    for (LgsVector(LgsString)::const_iterator iter = companies_.begin(); iter != companies_.end(); iter++)
    {
        if (*iter == entry.getCompany())
            return true;
    }

    return false;
}

bool GermanSearchItem::validDoubleLetterForEntry(const GermanDictionaryEntry& entry) const
{
    if (!entry.getFlags().get(de_doubleEndingRemoved))
        return true;

    const char* text = entry.getNormalizedText();

    int length = strlen(text);
    return text[length - 1] == buffer_.c_str()[length];
}

bool GermanSearchItem::validAsNonHead(const GermanDictionaryEntry& entry) const
{
    return entry.getWordClass().inNonHead();
}

bool GermanSearchItem::validAsHead(const GermanDictionaryEntry& entry) const
{
    return entry.getWordClass().inHead();
}

bool GermanSearchItem::canSuffixBeWord(const char* suffix)
{
    return strcmp(suffix, "ster") == 0 || strcmp(suffix, "stem") == 0;
}

bool GermanSearchItem::validForHead(const GermanDictionaryEntry& entry) const
{
	LgsString entryText(entry.getText());
    StringUtil::toLower(entryText);
	int iSuffixLocation = 0;
	if (!strncmp(entryText.c_str(), buffer_.c_str(), entryText.length()))
	{
		iSuffixLocation = entryText.length();	
	}
    if (!entry.hasSuffix(buffer_.c_str()+iSuffixLocation, headWordInfo_))
    {
        // entry must be a non-head word
        return validAsNonHead(entry);
    }

    if (canSuffixBeWord(buffer_.c_str()))
    {
        // we have an ambiguity here - the suffix can be treated as a word or a suffix
        // so the current entry can be either a headword or a non-head word
        // there is a small possibility we will accept a decomposition that is not valid
        return validAsHead(entry) || validAsNonHead(entry);
    }
    else
    {
        return validAsHead(entry);
    }
}

bool GermanSearchItem::validEntry(const GermanDictionaryEntry& entry) const
{
    // check company code information - one of the company codes in the list must match
    if (!validCompanyForEntry(entry))
        return false;

    // check for special entry - created from doubled consonant at end of word
    // the next letter needs to be the same as the last letter of the entry
    if (!validDoubleLetterForEntry(entry))
        return false;

    // if the entry must be a non-head word, check this is the case
    if (headWordInfo_ == noHeadWord)
    {
        if (!validAsNonHead(entry))
            return false;
    }
    else
    {
        int length = buffer_.length() - strlen(entry.getNormalizedText());
        assert(length >= 0);
        if (length > longestSuffix())
        {
            if (!validAsNonHead(entry))
                return false;
        }
        else if (length < minWordLength())
        {
            if (!validAsHead(entry))
                return false;
        }
        else
        {
            // word can be either a head or not
            if (!validForHead(entry))
                return false;
        }
    }

    return true;
}

LgsList(const GermanDictionaryEntry*) GermanSearchItem::filter(const GermanDictionary::Range& range) const
{
    LgsList(const GermanDictionaryEntry*) ret;

    for (GermanDictionary::Iterator iter = range.first; iter != range.second; iter++)
    {
        if (validEntry(*iter))
        {
            ret.push_back(&(*iter));
        }
    }

    return ret;
}

bool GermanSearchItem::allowSuffix() const
{
    if (headWordInfo_ == noHeadWord || wordCount_ < 2)
        return false;

    GermanTokenWord* word = dynamic_cast<GermanTokenWord*>(decomposition_->back());
    for (GermanTokenWord::EntryLinkConstIterator iter = word->entries_.begin();
         iter != word->entries_.end(); iter++)
    {
        if (!(*iter).first->getFlags().get(de_doubleEndingRemoved))
            return true;
    }

    return false;
}

bool GermanSearchItem::isSuffix() const
{
    GermanTokenTrialSuffix* trialSuffix = dynamic_cast<GermanTokenTrialSuffix*>(decomposition_->back());
    assert(trialSuffix != 0);

    for (GermanTokenWord::EntryLinkConstIterator iter = trialSuffix->word_->entries_.begin();
         iter != trialSuffix->word_->entries_.end(); iter++)
    {
        if ((*iter).first->hasSuffix(buffer_.c_str(), headWordInfo_))
            return true;
    }

    return false;
}

bool GermanSearchItem::suffixOk() const
{
    if (headWordInfo_ == noHeadWord || wordCount_ < 2)
        return true;                     // since there are no allowed suffixes

    GermanTokenWord* word = dynamic_cast<GermanTokenWord*>(decomposition_->back());
    if (word == 0)
        return true;                     // since there is already a valid suffix

    // check the word allows no suffix
    for (GermanTokenWord::EntryLinkConstIterator iter = word->entries_.begin();
         iter != word->entries_.end(); iter++)
    {
        if ((*iter).first->hasSuffix("", headWordInfo_))
            return true;                 // there is an entry taking no suffix
    }

    return false;
}
void GermanSearchItem::unlinkInvalidEntries(void)
{
	GermanTokenList::const_iterator iterToken = decomposition_->begin();
	for (GermanTokenTrialConnector* trialConnector = decomposition_->getNextTrialConnector(iterToken);
		 0 != trialConnector; iterToken++, trialConnector = decomposition_->getNextTrialConnector(iterToken)
		 )
	{
		trialConnector->unlinkInvalidEntries();
	}
}

bool GermanSearchItem::headWordclassOk() const
{
    if (headWordInfo_ == noHeadWord)
        return true;                     // since there are no allowed headwords
                                         // and head-only words have already been eliminated

    const GermanTokenWord* word = getHeadWord();
    assert(word != 0);

    // check the word allows no suffix
    for (GermanTokenWord::EntryLinkConstIterator iter = word->entries_.begin();
         iter != word->entries_.end(); iter++)
    {
        GermanWordClass::PartOfSpeech partOfSpeech = (*iter).first->getWordClass().getPartOfSpeech();

        switch (headWordInfo_)
        {
        case nounHeadWord:
            if (partOfSpeech == GermanWordClass::noun)
                return true;
            break;
        case adjHeadWord:
            if (partOfSpeech == GermanWordClass::adj)
                return true;
            break;
        case verbHeadWord:
            if (partOfSpeech == GermanWordClass::verb)
                return true;
            break;
        case nounAdjHeadWord:
            if (partOfSpeech == GermanWordClass::noun || partOfSpeech == GermanWordClass::adj)
                return true;
            break;
        }
    }

    return false;
}

//---------------------------------------------------------------------
GermanSearch::GermanSearch(
    const LgsString& placeFile, const LgsString& properNameFile,
    const LgsVector(LgsString)& companies,
    GermanDictionary& dictionary,
    bool backtrack)
    : dictionary_(dictionary)
    , backtrack_(backtrack)
    , item_(companies)
{
    appendFile(placeFile);
    appendFile(properNameFile);
}

GermanSearch::~GermanSearch(void)
{

}

bool GermanSearch::decompose(const LgsString& compoundWord, GermanTokenList& decomposition,
    GermanHeadWordInfo headWordInfo, bool allUpperCase)
{
    //assert(decomposition.size() == 0);

    // check to see that word is longer than 1 character long and is not on the no-decompose list
    LgsString lowerWord = StringUtil::lower(compoundWord);
    if ((lowerWord.length() < 2) || (noDecomposeList_.find(lowerWord) != noDecomposeList_.end()))
        return false;

    allUpperCase_ = allUpperCase;

	//Kludge: Change Later to assign directly - Microsoft Bug
    compoundWord_ = compoundWord.c_str();

    item_.setDetails(compoundWord_, headWordInfo);
	item_.resetWordCount();

    for (;;)
    {
        if (!nextState())
        {
            return false;
        }
        else if (isSolution() && item_.suffixOk() && item_.headWordclassOk() && matchesUnnormalized())
        {
			item_.unlinkInvalidEntries();

            item_.appendDecomposition(decomposition);
            return true;
        }
    }
}

bool GermanSearch::nextState()
{
    // empty token list - we are done searching
    if (item_.hasNoTokens())
        return false;

    // backtrack if we already found a solution
    if (isSolution())
        return backtrack();

    if (item_.isTrialWordToken())
    {
        // check length of trial word
        if (item_.mustBacktrack()) // length of trialWord token is too small to be a word prefix
            return backtrack();

        // try to find a new prefix in the dictionary - longest match
        GermanDictionary::Range range;
        int wordLength = item_.longestPrefix(dictionary_, range);

        // no prefix in the dictionary - so backtrack
        if (wordLength == 0)
            return backtrack();

        // found a prefix in the dictionary - filter out invalid entries
        LgsList(const GermanDictionaryEntry*) validWords = item_.filter(range);

        // backtrack if none left
        if (validWords.empty())
        {
            item_.backtrack();
        }
        else
        {
            // words found - append new trial token
            item_.appendWord(wordLength, validWords);
			if (wordLength == compoundWord_.length())
			{
				return backtrack();
			}
            if (!item_.isEmpty())
            {
                if (item_.allowSuffix())
                    item_.appendTrialSuffix();
                else
                    item_.appendTrialConnector();
            }
        }
    }
    else if (item_.isTrialConnectorToken())
    {
        if (item_.connectorLength() == -1)
        {
            // no connector
            item_.backtrack();
        }
        else
        {
            // found a connector - append a connector and trial word token
            assert(!isSolution());
            item_.appendConnector();
            item_.appendTrialWord();
        }
    }
    else if (item_.isTrialSuffixToken())
    {
        if (item_.isSuffix())
            item_.appendSuffix();
        else
            // no suffix - backtrack item_ (decays to word or connector trial token)
            item_.backtrack();
    }
    else
    {
        assert(("Invalid token", 0));
    }

    return true;
}

bool GermanSearch::backtrack()
{
    if (!backtrack_)
        return false;

    item_.backtrackBigStep();
    if (item_.hasNoTokens())
        return false;
    item_.backtrack();

    return true;
}

void GermanSearch::appendFile(const LgsString& fileName)
{
    ifstream input(fileName.c_str());
    if (!input.good())
        throw LgsString("cannot open (no-decomposition) file ") + LgsString(fileName);

    char buffer[1024];
    for (;;)
    {
        *buffer = 0;
        input.getline(buffer, 1024);
        if (*buffer == 0)
            break;
        LgsString word(buffer);
        StringUtil::toLower(word);
        noDecomposeList_.insert(word);
    }
}

