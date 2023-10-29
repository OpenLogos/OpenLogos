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
#ifndef __ElisionVariable_h__
#define __ElisionVariable_h__

//-------------------------------------------------------------------
// File - Variable.h
//
// Class - EL_Variable
//
// Description - a variable class used as a base class for all
//               language specific variable classes
//               The child class classes will then be used as
//               template arguments for the rule-engine templates
//
//-------------------------------------------------------------------

#include <logos_libs/regex/regularexpression.h>
#include <logos_libs/regex/matchinfo.h>
#include <logos_libs/utility/phrasemanager.h>
#include <logos_libs/ruleengine/phrasemgrposition.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace Elision {

// holds target side words - stored in a phrase manager
// holds a current position to match from
class EL_Variable: public Object
{
public:
    // construct from a phrase manager
    EL_Variable(PhraseManager& phraseManager);

    // search the current sentence from the start position
    bool matches(RegularExpression& pattern);

    int groups() const;
    LgsString& getSubString(int group = 0);

    void replace(int group, const LgsString& replacement);

    LgsString toString() const;

    void resetStartPos();
    void incrementStartPos();
    bool pastEndPos() const;
   PhraseManager* getPhraseManager();

private:
    PhraseManager* phraseManager_;
    MatchInfo matchInfo_;
    RE_PhraseMgrPosition searchPosition_;

    void getMatchDetails(const RegularExpression& pattern);
};

//-------------------------------------------------------------------
inline EL_Variable::EL_Variable(PhraseManager& phraseManager)
    : phraseManager_(&phraseManager)
    , searchPosition_(phraseManager_, matchInfo_)
{
}

inline LgsString EL_Variable::toString() const
{
    return phraseManager_->getText();
}

inline void EL_Variable::replace(int group, const LgsString& replacement)
{
    RegularExpression::MatchPos match = matchInfo_.getMatch(group);
    phraseManager_->replace(match.pos, match.length, replacement);
}

inline int EL_Variable::groups() const
{
    return matchInfo_.getGroups();
}

inline LgsString& EL_Variable::getSubString(int group)
{
    return matchInfo_.getSubString(group);
}

inline void EL_Variable::resetStartPos()
{
    searchPosition_.reset();
}

inline void EL_Variable::incrementStartPos()
{
    searchPosition_.increment();
}

inline bool EL_Variable::pastEndPos() const
{
    return searchPosition_.pastEnd();
}

inline PhraseManager* EL_Variable::getPhraseManager()
{
   return phraseManager_;
}
//}

#endif



