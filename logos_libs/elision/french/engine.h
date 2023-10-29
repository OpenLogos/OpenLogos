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
#ifndef __ElisionFrenchEngine_h__
#define __ElisionFrenchEngine_h__

//----------------------------------------------------------------------------
// File - Engine.h
//
// Class - EL_FR_Engine
//
// Description - a pattern matcher rule-engine for French elision.
//
//----------------------------------------------------------------------------

#include <logos_libs/elision/engine.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace Elision { namespace French {

class EL_FR_Engine: public EL_Engine
{
    DisableCopyAssign(EL_FR_Engine);

public:
    // describes lists of special words. count__ gives number of lists
    enum ListType { NonCompare, NoElision, CeList1, CeList2, CeList3, CeList4, count__};

    EL_FR_Engine(RE_RuleBase<EL_Variable>* ruleBase);

    LgsSet(LgsString)& getWordList(ListType listType);

private:
    // sets used to store various lists of special words
    LgsSet(LgsString) lookup_[count__];
};

//----------------------------------------------------------------------------
inline EL_FR_Engine::EL_FR_Engine(RE_RuleBase<EL_Variable>* ruleBase)
    : EL_Engine(ruleBase)
{
}

inline LgsSet(LgsString)& EL_FR_Engine::getWordList(ListType listType)
{
    assert(listType != count__);

    return lookup_[listType];
}

//}}

#endif




