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
//---------------------------------------------------------------------
// File - CharUtil.cpp
//
// Class - CharUtil
//
//---------------------------------------------------------------------

#include "charutil.h"

const LgsString CharUtil::m_joiningCharacters("+~`!@#^&*=|;:,.?¼½¾¿$%£º'_\\");
const LgsString CharUtil::m_wordSeparators("/-[]{}()");

char CharUtil::henumChar(char c)
{
    if (isAlphabetic(c))
    {
        switch (lower(c))
        {
			case 'b': case 'c': case 'a': case 'ä': return '1';
            case 'd': case 'f': case 'e': return '2';
            case 'g': case 'h': case 'i': return '3';
            case 'j': case 'k': case 'o': case 'ö': return '4';
            case 'l': case 'm': case 'u': case 'ü': return '5';
            case 'n': case 'p':           return '6';
            case 'q': case 'r':           return '7';
            case 's': case 't':           return '8';
        }
    }
    return '9';
}

