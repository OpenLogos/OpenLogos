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
// File - MathTimesMinusAction.cpp
//
// Class - ST_MathTimesMinusAction
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/startrules/mathtimesminusaction.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace Start {

void ST_MathTimesMinusAction::fire(ST_Variable& variable) const
{
    if (variable.getMatch(0).length > 1)
        variable.setTokenType(LookupTokenType::tok_Math_Express, true);
	if (0 != variable.getSubString(5).length())
	{
		variable.convertGroup(5, sourceLocale_.dec_sep_, targetLocale_.dec_sep_, false);
	}
	if (0 != variable.getSubString(11).length())
	{
		variable.convertGroup(11, sourceLocale_.dec_sep_, targetLocale_.dec_sep_, false);
	}
	variable.convertGroup(2, sourceLocale_.thou_sep_, targetLocale_.thou_sep_, false);
	variable.convertGroup(8, sourceLocale_.thou_sep_, targetLocale_.thou_sep_, true);

}

//}

