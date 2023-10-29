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
#ifndef __RuleEnginePhraseMgrPosition_h__
#define __RuleEnginePhraseMgrPosition_h__

//----------------------------------------------------------------------------
// File - PhraseMgrPosition.h
//
// Class - RE_PhraseMgrPosition
//
// Description - Holds a current position with a phrase manager buffer for matching purposes
//
//----------------------------------------------------------------------------

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
//namespace RuleEngine {

#include <logos_libs/regex/regularexpression.h>
#include <logos_libs/regex/matchinfo.h>
#include <logos_libs/utility/phrasemanager.h>

class RE_PhraseMgrPosition
{
public:
    RE_PhraseMgrPosition(const PhraseManager* phraseManager, const MatchInfo& matchInfo);

    // get current position
    int get() const;

    // check if the current position is past the end of the buffer
    bool pastEnd() const;

    // reset the current position to the start of the buffer
    void reset();

    // increment the current position beyond the matched expression
    void increment();

    // increment the length of the match, by the given value (positive or negative)
    void increment(int value);

private:
    int pos_;
    const PhraseManager* phraseManager_;
    const MatchInfo& matchInfo_;
};

inline RE_PhraseMgrPosition::RE_PhraseMgrPosition(
    const PhraseManager* phraseManager, const MatchInfo& matchInfo)
    : phraseManager_(phraseManager)
    , matchInfo_(matchInfo)
    , pos_(0)
{
}

inline int RE_PhraseMgrPosition::get() const
{
    return pos_;
}

inline bool RE_PhraseMgrPosition::pastEnd() const
{
    return pos_ >= phraseManager_->getTextLength();
}

inline void RE_PhraseMgrPosition::reset()
{
    pos_ = 0;
}

inline void RE_PhraseMgrPosition::increment()
{
    RegularExpression::MatchPos match = matchInfo_.getMatch();
    if (match.length == 0)
        pos_ = max(pos_ + 1, match.pos + 1);
    else
        pos_ = match.pos + match.length;
}

inline void RE_PhraseMgrPosition::increment(int value)
{
    pos_ += value;
}

#endif


