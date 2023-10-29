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
// File - Antecedent.cpp
//
// Class - PM_Antecedent
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/patternmatcher/antecedent.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace PatternMatcher {

PM_Antecedent::PM_Antecedent(const LgsString& pattern)
: pattern_(pattern, RegularExpression::Both)
{
}

bool PM_Antecedent::evaluate(PM_Variable& variable)
{
//    variable.resetStartPos();
// line above commented out because of the rules like that:
//	offer -> offer document
// which loops the pattern matching mechanizm indefinitely.

    for (;;)
    {
        if (variable.pastEndPos())
            return false;

        if (!variable.matches(pattern_))
            return false;

        // check that the text is not protected
        if (variable.matchProtected())
        {
            // increment start position so we don't find the same match
            variable.incrementStartPos();
            continue;
        }
        else
            return true;

        // backtrack, searching from beyond the current pattern
    }
}

//}

