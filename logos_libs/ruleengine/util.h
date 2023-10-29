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
#ifndef __RuleEngineUtil_h__
#define __RuleEngineUtil_h__

//----------------------------------------------------------------------------
// File - Util.h
//
// Class - RE_Util
//
// Description - Utility class used by various variable classes
//
//----------------------------------------------------------------------------

#include <logos_libs/regex/matchinfo.h>
#include <logos_libs/utility/phrasemanager.h>
#include <logos_libs/linguistic/lword.h>
#include <logos_libs/linguistic/targetsentenceunit.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace RuleEngine {

class RE_Util
{
public:
    // load phrase manager data from source or target word list
    static void loadPhraseManager(LWordVector* sourceWords, PhraseManager* phraseManager);
    static void loadPhraseManager(TargetUnitVector* targetWords, PhraseManager* phraseManager);

    // update source words based on phrase-manager data
    // if any changes are made and updatePhraseManager is true - erase the phrasemanager data
    // and reload the phrasemanager via loadPhraseManager()
    static void updateSourceWords(LWordVector* sourceWords, PhraseManager* phraseManager,
        bool updatePhraseManager = true);

    // get the starting and ending key for a given match from the phrase manager
    static void getKeys(MatchInfo matchInfo, 
                        PhraseManager* phraseManager,
                        PhraseManager::Key& startKey, 
                        PhraseManager::Key& endKey, 
                        int group);
};

#endif



