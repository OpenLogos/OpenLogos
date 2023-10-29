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
#ifndef __ElisionSpanishAccentConsequent_h__
#define __ElisionSpanishAccentConsequent_h__

//-------------------------------------------------------------------
// File - AccentConsequent.h
//
// Class - EL_SP_AccentConsequent
//
// Description - consequent that places an accent on a vowel in a
//               Spanish word
//
//-------------------------------------------------------------------

#include <logos_libs/ruleengine/consequent.h>
#include <logos_libs/elision/variable.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace Elision { namespace Spanish

class EL_SP_AccentConsequent: public RE_Consequent<EL_Variable>
{
    DisableCopyAssign(EL_SP_AccentConsequent);

public:
    EL_SP_AccentConsequent(int group, int vowel, bool oneOnly);

    // if vowel is 1 accent the last vowel, if vowel is 2 accent the 2nd last vowel
    // but if oneOnly is true do not accent the vowel if one or more is already accented
    virtual void fire(EL_Variable& variable) const;

private:
    int group_;               // group to Accent
    int vowel_;               // which vowel to accent: 1 for last, 2 for 2nd last.
    bool oneOnly_;            // true means - don't accent if word is already accented
};

inline EL_SP_AccentConsequent::EL_SP_AccentConsequent(int group, int vowel, bool oneOnly)
    : group_(group)
    , vowel_(vowel)
    , oneOnly_(oneOnly)
{
}

//}}

#endif



