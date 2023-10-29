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
// File - TimeAction.cpp
//
// Class - ST_TimeAction
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/startrules/timeaction.h>
#include <logos_libs/utility/stringutil.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace Start {

void ST_TimeAction::fire(ST_Variable& variable) const
{
    int hour = StringUtil::asInteger(variable.getSubString(1));
    dump(hour);
    int min = StringUtil::asInteger(variable.getSubString(3));
    dump(min);
    LgsString sec = variable.getSubString(6);
    dump(sec);
    LgsString decimals = variable.getSubString(8);
    dump(decimals);
    LgsString suffix = variable.getSubString(9);
    dump(suffix);

    // validation
    if (suffix.length() != 0 && decimals.length() != 0)
        return;
    if (hour > 24 || min > 60)
        return;
    if (sec.length() != 0)
    {
        int val = StringUtil::asInteger(sec);
        if (val > 60)
            return;
    }
    if (hour > 12 && suffix.length() != 0)
        return;

	if ( 0 != sec.length())
	{
		variable.convertGroup(5, sourceLocale_.time_sep_, targetLocale_.time_sep_, false);   // TIME SEPERATOR
	}
	if (0 != decimals.length())
	{
		variable.convertGroup(7, sourceLocale_.time_dec_sep_, targetLocale_.time_dec_sep_, false);   // TIME DECIMAL SEPERATOR
	}


	variable.convertGroup(2, sourceLocale_.time_sep_, targetLocale_.time_sep_, true);   // TIME SEPERATOR

    variable.setTokenType(LookupTokenType::tok_Time, true);
    if (suffix.length() != 0)
        variable.setTokenType(LookupTokenType::tok_Time_Noun, 9, true);
}

//}

