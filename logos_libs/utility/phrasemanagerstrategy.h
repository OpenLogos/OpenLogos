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
// File - PhraseManager.h

#ifndef __PhraseManagerStrategy_h__
#define __PhraseManagerStrategy_h__

class PhraseManagerInterface;

// ------------------------------------------------------------------------------
// defines a strategy for distribution of replacement words to a set of phrases
// abstract class
class PhraseManagerStrategy: public Object
{
   friend class PhraseManager;

public:
    // distribute replacement words into phrases
    // this will only be called for more than one phrase
    // handles the case                          when replacementWordsLength == 0
    // handles the case                          when totalWordCount == replacementWordsLength
    // calls doDistributeExcess() hook function  when totalWordCount <  replacementWordsLength
    // calls doDistributeDeficit() hook function when totalWordCount >  replacementWordsLength
    void distribute(
        int replacementWordsLength,              // no of replacement words
        const LgsIntVector &wordCount,            // no of words in each phrase
        int totalWordCount);                     // total of wordCount

    void distributeEqual(
        int replacementWordsLength, const LgsIntVector &wordCount, int totalWordCount);

    virtual void doDistributeExcess(
        int replacementWordsLength, const LgsIntVector &wordCount, int totalWordCount) = 0;

    virtual void doDistributeDeficit(
        int replacementWordsLength, const LgsIntVector &wordCount, int totalWordCount) = 0;

protected:
    PhraseManagerInterface *manager_;

private:
    void SetManager(PhraseManagerInterface *manager);
};

inline void PhraseManagerStrategy::SetManager(PhraseManagerInterface *manager)
{
    manager_ = manager;
}

// ------------------------------------------------------------------------------
class DefaultPhraseManagerStrategy: public PhraseManagerStrategy
{
public:
    virtual void doDistributeExcess(
        int replacementWordsLength, const LgsIntVector &wordCount, int totalWordCount);

    virtual void doDistributeDeficit(
        int replacementWordsLength, const LgsIntVector &wordCount, int totalWordCount);
};

#endif



