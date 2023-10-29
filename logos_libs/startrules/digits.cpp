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
// File - digits.cpp
//
// Class - ST_DigitAction
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/startrules/digits.h>
#include <logos_libs/utility/stringutil.h>
#include <logos_libs/translutility/translcommonobjects.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace Start {

void ST_DigitAction::fire(ST_Variable& variable) const
{
    int digitValue = StringUtil::asInteger(variable.getSubString(1));

    if ( (digitValue % 10) != 0  || digitValue < 1400 || digitValue > 2100)
    {
       if (variable.getSubString(2).empty())
       {
         variable.setTokenType(LookupTokenType::tok_no_pl, true);
       }
       else if (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::GermanID)
       {
         variable.setTokenType(LookupTokenType::tok_Unfound_Agent, true);
       }
       else
       {
         variable.setTokenType(LookupTokenType::tok_Unfound_Agent_Poss, true);
       }
    }
}

//}

