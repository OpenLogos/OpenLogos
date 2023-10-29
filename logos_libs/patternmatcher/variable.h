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
#ifndef __PatternVariable_h__
#define __PatternVariable_h__

//-------------------------------------------------------------------
// File - Variable.h
//
// Class - PM_Variable
//
// Description - a variable class used as a template argument for
//               the rule-engine templates
//
//-------------------------------------------------------------------

#include <logos_libs/regex/regularexpression.h>
#include <logos_libs/regex/matchinfo.h>
#include <logos_libs/utility/phrasemanager.h>
#include <logos_libs/linguistic/lword.h>
#include <logos_libs/linguistic/targetsentenceunit.h>
#include <logos_libs/ruleengine/phrasemgrposition.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace PatternMatcher {

// holds either source or target side words - stored in an LWwordVector or TargetUnitVector resp
// with a phrase manager as a mirror of the words
// holds a current position to match from
class PM_Variable: public Object
{
public:
    // construct a variable given a list of source words
    PM_Variable(LWordVector& words);

    // construct a variable given a list of target words
    PM_Variable(TargetUnitVector& words);

    virtual ~PM_Variable();

    // search the current sentence from the start position
    // updates the matched region
    bool matches(RegularExpression& pattern);

    // replace the matched expression with the replacement
    void replace(LgsString replacement_);

    // reset the start position - so searches start at the beginning
    void resetStartPos();

    // increment the starting position
    // sets the new starting position beyond the matched region
    void incrementStartPos();

    // are we past the end of the buffer
    bool pastEndPos() const;

    // does the matched region include any protected words
    bool matchProtected() const;

    // protect all words in the matched region
    void protectMatch();

    // synchronizes the source words vector with the current state
    void synchronize();

    // synchronizes the target words with the current state
    void synchronizeTargetUnits();

    // returns a LgsString representation of the current sentence
    LgsString toString() const;

    // get matched substring
    const LgsString& getSubString(int group = 0) const;

    // no of groups
    int getGroups() const;

private:
    bool source_;                          // source or target
    LWordVector* sourceWords_;
    TargetUnitVector* targetWords_;
    PhraseManager* phraseManager_;
    MatchInfo matchInfo_;
    RE_PhraseMgrPosition searchPosition_;

    // get keys for current match
    // the match will span from startKey to endKey inclusively
    // precondition - regular rexpression matched with nonzero length
    void getKeys(PhraseManager::Key& startKey, 
                 PhraseManager::Key& endKey) const;

    // fill in match details from pattern
    void getMatchDetails(const RegularExpression& pattern);
};

inline void PM_Variable::resetStartPos()
{
    searchPosition_.reset();
}

inline void PM_Variable::incrementStartPos()
{
    searchPosition_.increment();
}

inline bool PM_Variable::pastEndPos() const
{
    return searchPosition_.pastEnd();
}

inline LgsString PM_Variable::toString() const
{
    return phraseManager_->getText();
}

inline const LgsString& PM_Variable::getSubString(int group) const
{
    return matchInfo_.getSubString(group);
}

inline int PM_Variable::getGroups() const
{
    return matchInfo_.getGroups();
}

//}

#endif



