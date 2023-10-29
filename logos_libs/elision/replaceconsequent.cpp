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
// File - ReplaceConsequent.cpp
//
// Class - EL_ReplaceConsequent
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/elision/replaceconsequent.h>
#include <logos_libs/utility/charactervector.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace Elision {

void EL_ReplaceConsequent::fire(EL_Variable& variable) const
{
    char backslash = char('\\');

    // LgsString temp = variable.toString();
    // cout << "Pattern Replace Fired on \"" << variable.toString() << '"' << endl;

    // build up replacement text
    CharacterVector buffer;
    bool inEscape = false;
    for (LgsString::const_iterator iter = replacement_.begin(); iter != replacement_.end(); iter++)
    {
        if (*iter == backslash)
        {
            iter++;
            if (iter == replacement_.end() || !inrange(*iter - '0', 1, 9))
                throw "\\1 to \\9 expected in replacement expression";
            int index = *iter - '0';
            buffer += variable.getSubString(index);
        }
        else
            buffer.push_back(*iter);
    }

    // replace text
    variable.replace(group_, LgsString(buffer.begin(), buffer.end()));
}

//}

