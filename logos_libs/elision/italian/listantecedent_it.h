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
program. If not, write to Globalware AG, Hospitalstraße 6, D-99817 Eisenach.

Linux port modifications and additions by Bernd Kiefer, Walter Kasper,
Deutsches Forschungszentrum fuer kuenstliche Intelligenz (DFKI)
Stuhlsatzenhausweg 3, D-66123 Saarbruecken
*/
#ifndef __ElisionItalianListAntecedent_h__
#define __ElisionItalianListAntecedent_h__

//-------------------------------------------------------------------
// File - ListAntecedent.h
//
// Class - EL_IT_ListAntecedent
//
// Description - Antecedent used to check if a word is in or out of
//               one of the special lists of Italian words
//
//-------------------------------------------------------------------

#include <logos_libs/ruleengine/antecedent.h>
#include <logos_libs/elision/variable.h>
#include <logos_libs/elision/italian/engine.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace Elision { namespace Italian

class EL_IT_ListAntecedent: public RE_Antecedent<EL_Variable>
{
    DisableCopyAssign(EL_IT_ListAntecedent);

public:
    EL_IT_ListAntecedent(int group, EL_IT_Engine* engine);

    // return true iff word defined by group_ is in list of special words for engine
    virtual bool evaluate(EL_Variable& variable);

private:
    int group_;                         // matched group to check
    EL_IT_Engine* engine_;              // italian engine containing list of special words
};

//-------------------------------------------------------------------
inline EL_IT_ListAntecedent::EL_IT_ListAntecedent(int group, EL_IT_Engine* engine)
    : group_(group)
    , engine_(engine)
{
}

//}}

#endif



