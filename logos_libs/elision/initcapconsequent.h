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
#ifndef __ElisionInitCapConsequent_h__
#define __ElisionInitCapConsequent_h__

//-------------------------------------------------------------------
// File - InitCapConsequent.h
//
// Class - EL_InitCapConsequent
//
// Description - consequent that capitalizes the first letter of a word
//
//-------------------------------------------------------------------

#include <logos_libs/ruleengine/consequent.h>
#include <logos_libs/elision/variable.h>
#include <logos_libs/elision/primitive.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace Elision {

class EL_InitCapConsequent: public RE_Consequent<EL_Variable>
{
    DisableCopyAssign(EL_InitCapConsequent);

public:
    EL_InitCapConsequent(int group);

    // capitalize first letter in word
    virtual void fire(EL_Variable& variable) const;

private:
    int group_;               // group to InitCap
};

inline EL_InitCapConsequent::EL_InitCapConsequent(int group)
    : group_(group)
{
}

inline void EL_InitCapConsequent::fire(EL_Variable& variable) const
{
    assert(inrange(group_, 0, variable.groups()));

    LgsString& word = variable.getSubString(group_);
    EL_Primitive primitive(word);
    primitive.capitalize();
}

//}

#endif



