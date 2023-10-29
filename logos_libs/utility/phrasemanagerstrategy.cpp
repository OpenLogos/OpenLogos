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
// PhraseManagerStrategy.cpp

#include <logos_include/logoscommon.h>
#include <logos_libs/utility/phrase.h>
#include <logos_libs/utility/phrasemanagerstrategy.h>
#include <logos_libs/utility/phrasemanagerinterface.h>

//for debug code
//#include <logos_libs/utility/PhraseDebug.h>

// ------------------------------------------------------------------------------
void PhraseManagerStrategy::distribute(
    int replacementWordsLength, const LgsIntVector & wordCount, int totalWordCount)
{
    //debug code
    //dumpInt(cout, "replacementWordsLength", replacementWordsLength);
    //dumpVector(cout, "wordCount", wordCount);
    //dumpInt(cout, "totalWordCount", totalWordCount);

    int delta = replacementWordsLength - totalWordCount;
    if (delta > 0)
        doDistributeExcess(replacementWordsLength, wordCount, totalWordCount);
    else if (delta == 0)
        distributeEqual(replacementWordsLength, wordCount, totalWordCount);
    else
        doDistributeDeficit(replacementWordsLength, wordCount, totalWordCount);
}

void PhraseManagerStrategy::distributeEqual(
    int replacementWordsLength, const LgsIntVector & wordCount, int totalWordCount)
{
    int startWord = 0;
    for (LgsIntConstIterator current = wordCount.begin();
        current != wordCount.end();
        current++)
    {
        manager_->distribute(startWord, *current, current - wordCount.begin());
        startWord += *current;
    }
}

// ------------------------------------------------------------------------------
void DefaultPhraseManagerStrategy::doDistributeExcess(
    int replacementWordsLength, const LgsIntVector &wordCount, int totalWordCount)
{
    // feed extra words from left to right into phrases             eg. 8
    int delta = replacementWordsLength - totalWordCount;
    int deltaPerPhrase = delta / wordCount.size();               // eg. 8 / 3 = 2
    int extra = delta - (deltaPerPhrase * wordCount.size());     // eg. 8 - 2 * 3 = 2

    int startWord = 0;
    for (LgsIntConstIterator current = wordCount.begin();
        current != wordCount.end();
        current++)
    {
        int extraWord = 0;
        if (extra > 0)
        {
            extra--;
            extraWord++;
        }
        int words = *current + deltaPerPhrase + extraWord;
        manager_->distribute(startWord, words, current - wordCount.begin());
        startWord += words;
    }
}

// ------------------------------------------------------------------------------
void DefaultPhraseManagerStrategy::doDistributeDeficit(
    int replacementWordsLength, const LgsIntVector &wordCount, int totalWordCount)
{
    // array of bins - at end each will contain the no of words to distribute for each phrase
    int *words = new int[wordCount.size()];
    memset(words, 0, sizeof(int) * wordCount.size());

    // distribute tokens to the bins - starting at the first bin left to right
    // until all tokens are used up. we start with totalWordCount tokens
    // do not place more tokens in the bin than the original wordcount of the phrase
    int bin = -1;
    while (replacementWordsLength-- > 0)
    {
        for (;;)
        {
            // increment bin, wrap round when we reach last bin
            if (++bin == wordCount.size())
                bin = 0;

            // only place token in a bin if the bin is not full
            if (words[bin] < wordCount[bin])
            {
                words[bin]++;
                break;
            }
        }
    }

    // now distribute the words
    int startWord = 0;
    for (LgsIntConstIterator current = wordCount.begin();
        current != wordCount.end();
        current++)
    {
        int phrase = current - wordCount.begin();
        manager_->distribute(startWord, words[phrase], phrase);
        startWord += words[phrase];
    }

    delete[] words;
}

