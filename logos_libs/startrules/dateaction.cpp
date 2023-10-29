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
// File - DateAction.cpp
//
// Class - ST_DateAction
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/startrules/dateaction.h>
#include <logos_libs/utility/stringutil.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace Start {

void ST_DateAction::fire(ST_Variable& variable) const
{

	// replace date seperator to target value
    variable.convertGroup(0, sourceLocale_.date_sep_, targetLocale_.date_sep_, false);

    int month, day;
    switch (sourceLocale_.dateFormat_)
    {
    case ST_Locale::mdy:
		month = StringUtil::asInteger(variable.getSubString(1));
		dump(month);
		day = StringUtil::asInteger(variable.getSubString(2));
		dump(day);
		if (targetLocale_.dateFormat_ == ST_Locale::dmy)
		{
			RegularExpression::MatchPos matchPos = variable.getMatch(2);
			variable.replace(matchPos.pos, variable.getSubString(2).length(), variable.getSubString(1), false);
			matchPos = variable.getMatch(1);		
			variable.replace(matchPos.pos, variable.getSubString(1).length(), variable.getSubString(2), true);
		}
        break;
    case ST_Locale::dmy:
		day = StringUtil::asInteger(variable.getSubString(1));
		dump(day);
		month = StringUtil::asInteger(variable.getSubString(2));
		dump(month);
 		if (targetLocale_.dateFormat_ == ST_Locale::mdy)
		{
			RegularExpression::MatchPos matchPos = variable.getMatch(2);
			variable.replace(matchPos.pos, variable.getSubString(2).length(), variable.getSubString(1), false);
			matchPos = variable.getMatch(1);		
			variable.replace(matchPos.pos, variable.getSubString(1).length(), variable.getSubString(2), true);
		}
       break;
    default:
        assert(("invalid value", 0));
    }

    // simplified date validation
    if (0 < day && day <= 31 && 0 < month && month <= 12)
        variable.setTokenType(LookupTokenType::tok_Date, true);
    else
        variable.setTokenType(LookupTokenType::tok_Unfound_Alpha_Num, true);


}

//}

