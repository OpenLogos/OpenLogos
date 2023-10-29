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
//----------------------------------------------------------------------------
// File - Primitive.cpp
//
// Class - EL_PT_Primitive
//
//----------------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/elision/portuguese/primitive_pt.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace Elision { namespace portuguese {

int EL_PT_Primitive::vowels()
{
    // written as a finite-state-machine but have extra state variables
    // char_is_g_or_q and char_is_u to handle the u in gu or qu
    // which is not regarded as vowel

    enum State { ConsonantState, StrongVowelState, WeakVowelState };

    int count = 0;
    State state = ConsonantState;
    for (LgsString::iterator iter = word_.begin(); iter != word_.end(); iter++)
    {
        char c = *iter;
        CharType type = GetCharType(c);

        switch (state)
        {
        case ConsonantState:
            switch (type)
            {
            case Consonant:
                break;
            case StrongVowel:
                count++;
                vowels_.push_back(iter);
                state = StrongVowelState;
                break;
            case WeakVowel:
                count++;
                vowels_.push_back(iter);
                state = WeakVowelState;
                break;
            }
            break;

        case StrongVowelState:
            switch (type)
            {
            case StrongVowel:
                count++;
                vowels_.push_back(iter);
                break;
            default:
                state = ConsonantState;
                break;
            }
            break;

        case WeakVowelState:
            state = ConsonantState;
            break;
        }
    }

    return count;
}

bool EL_PT_Primitive::hasAccent() const
{
    for (LgsString::iterator iter = word_.begin(); iter != word_.end(); iter++)
        if (isAccent(*iter))
            return true;
    return false;
}

void EL_PT_Primitive::placeAccent(int position)
{
    // could have a slight ineffeciency since the same word may be encountered in
    // an antecedent that counted vowels and in a consequent that adds the accent.
    // since the objects created in these cases are different - 2 scans will occur
    // through the same word. however this should not be very significant

    assert(position == 1 || position == 2);

    if (vowels_.size() == 0)
        vowels();         // to force rescan of vowel list - ignore return value

    // there must be enough vowels in the word to place the accent
    assert(vowels_.size() >= position);

    LgsString::iterator iter = getDominantVowel(vowels_[vowels_.size() - position]);
    if (iter != word_.end())
        placeAccent(iter);
}

EL_PT_Primitive::CharType EL_PT_Primitive::GetCharType(char c)
{
    switch (c)
    {
    case 'A': case 'a': case 'Á': case 'á':
    case 'E': case 'e': case 'É': case 'é':
    case 'Í': case 'í':
    case 'O': case 'o': case 'Ó': case 'ó':
    case 'Ú': case 'ú':
        return StrongVowel;

    case 'I': case 'i':
    case 'U': case 'u': case 'Ü': case 'ü':
        return WeakVowel;

    default:
        return Consonant;
    }
}

bool EL_PT_Primitive::isAccent(char c)
{
    switch (c)
    {
    case 'Á': case 'á':
    case 'É': case 'é':
    case 'Í': case 'í':
    case 'Ó': case 'ó':
    case 'Ú': case 'ú':
        return true;

    default:
        return false;
    }
}

LgsString::iterator EL_PT_Primitive::getDominantVowel(LgsString::iterator iter)
{
    CharType type = GetCharType(*iter);
    assert(type != Consonant);

    if (type == StrongVowel)
        return iter;
    else
    {
        LgsString::iterator next = iter + 1;
        if (next == word_.end())
            return iter;            // weak-vowel at end of a word
        type = GetCharType(*next);
        switch (type)
        {
        case Consonant:
            return iter;
        case StrongVowel:
            return next;
        default:                     // weak-vowel followed by a weak-vowel
            return word_.end();
        }
    }
}

void EL_PT_Primitive::placeAccent(LgsString::iterator iter)
{
    assert(GetCharType(*iter) != Consonant);

    switch (*iter)
    {
    case 'A':           *iter = 'Á'; break;
    case 'a':           *iter = 'á'; break;
    case 'E':           *iter = 'É'; break;
    case 'e':           *iter = 'é'; break;
    case 'I':           *iter = 'Í'; break;
    case 'i':           *iter = 'í'; break;
    case 'O':           *iter = 'Ó'; break;
    case 'o':           *iter = 'ó'; break;
    case 'Ú': case 'Ü': *iter = 'Ú'; break;
    case 'u': case 'ü': *iter = 'ú'; break;
    }
}

//}}

