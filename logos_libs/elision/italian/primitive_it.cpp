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
//----------------------------------------------------------------------------
// File - Primitive.cpp
//
// Class - EL_IT_Primitive
//
//----------------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/elision/italian/primitive_it.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace Elision { namespace Italian {

EL_IT_Primitive::PrefixType EL_IT_Primitive::prefixType()
{
    static char vowels[] = "¿»Ã“Ÿ‡ËÏÚ˘AEIOUaeiouHh8";

    if (strchr(vowels, word_[0]))
        return EL_IT_Primitive::Vowel;

    switch (word_[0])
    {
    case 'z':
    case 'Z':
    case 'Y':
    case 'y':
        return EL_IT_Primitive::ConsSpecial;

    case 'p':
    case 'P':
        if (word_.length() == 1)
            return EL_IT_Primitive::ConsNormal;
        else if (word_[1] == 's' || word_[1] == 'S')
            return EL_IT_Primitive::ConsSpecial;
        else
            return EL_IT_Primitive::ConsNormal;

    case 'g':
    case 'G':
        if (word_.length() == 1)
            return EL_IT_Primitive::ConsNormal;
        else if (word_[1] == 'n' || word_[1] == 'N')
            return EL_IT_Primitive::ConsSpecial;
        else
            return EL_IT_Primitive::ConsNormal;

    case 's':
    case 'S':
        if (word_.length() == 1)
            return EL_IT_Primitive::ConsNormal;
        else if (strchr(vowels, word_[1]))
            return EL_IT_Primitive::ConsNormal;
        else
            return EL_IT_Primitive::ConsSpecial;

    default:
        if (inrange(char(word_[0]), char('A'), char('Z')) ||
            inrange(char(word_[0]), char('a'), char('z')) ||
            inrange(char(word_[0]), char('0'), char('7')) ||
            (char(word_[0]) == char('9')))
            return EL_IT_Primitive::ConsNormal;
        else
            return EL_IT_Primitive::Other;
    }
}

//}}

