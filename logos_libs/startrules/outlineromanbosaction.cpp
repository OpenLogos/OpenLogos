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
//-------------------------------------------------------------------
// File - OutlineRomanBOSAction.cpp
//
// Class - ST_OutlineRomanBOSAction
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/startrules/outlineromanbosaction.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace Start {

void ST_OutlineRomanBOSAction::fire(ST_Variable& variable) const
{
    bool lp = variable.getSubString(1).length() != 0;
    dump(lp);
    LgsString text = variable.getSubString(2);
    dump(text);
    bool period = variable.getSubString(3).length() != 0;
    dump(period);
    bool rp = variable.getSubString(4).length() != 0;
    dump(rp);

    if ((!lp && !period && !rp) || (lp && !rp))
        return;

    variable.setTokenType(LookupTokenType::tok_Outline_Notation, true);
}

//}

