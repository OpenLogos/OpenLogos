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
#ifndef __ElisionPortuguesePrimitive_h__
#define __ElisionPortuguesePrimitive_h__

//----------------------------------------------------------------------------
// File - Primitive.h
//
// Class - EL_PT_Primitive
//
// Description - a module for primitives used in portuguese elision
//
//----------------------------------------------------------------------------

/* portuguese documentation
------------------------------------------------------------------------------
  definition of a vowel in portuguese

      a,  e,  i,  o,  u   are vowels            (áéíóúü as well)
      a,  e,      o       are strong vowels     (áéíóú  as well)
              i,      u   are weak vowels       (     ü as well)

      2 strong vowels together are regarded as 2 vowels - no combination
      a combination of a strong vowel and a weak vowel is regarded as one vowel
      a combination of 2 weak vowels is regarded as one vowel
      only the strong vowel in a combination gets an accent (a single vowel can too)
      in the case of iu and ui there will be no accent
------------------------------------------------------------------------------
*/

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace Elision { namespace Portuguese {

struct EL_PT_Primitive
{
    EL_PT_Primitive(LgsString& word);

    // return the number of vowels in the word
    // as a side effect store the start of each vowel group
    // logically const - but not physically due to side effect - so we leave off the const
    int vowels();

    // does the word contain an accented vowel
    bool hasAccent() const;

    // if position is 1 place an accent on the last vowel in the word
    // if position is 2 place an accent on the 2nd last vowel in the word - must be at least 2
    void placeAccent(int position);

private:
    // character type - case insensitive
    enum CharType
    {
        Consonant,           // other consonants - any non-alpha char is treated as a consonant too
        StrongVowel,         // strong vowel as defined above
        WeakVowel            // other weak vowels as defined above
    };

    LgsString& word_;                       // word to work on
    LgsVector(LgsString::iterator) vowels_;    // starting position of each vowel group in the word

    // return the character type - input in lower case
    static CharType GetCharType(char c);

    // true if character is an accented vowel
    static bool isAccent(char c);

    // locate the dominant (non-linguist terminology) vowel on the vowel group
    //     starting at the given iterator
    // if the vowel group has one vowel only the dominant vowel is that one.
    // if the vowel at the iterator is a strong vowel the dominant vowel is that one.
    // otherwise the dominant vowel is the next vowel in the group - but if the next
    //     one in the group is a weak vowel there is no dominant vowel eg in iu - in that
    //     case the end() iterator is returned
    LgsString::iterator getDominantVowel(LgsString::iterator iter);

    // place the accent on the vowel at the given iterator
    void placeAccent(LgsString::iterator iter);
};

inline EL_PT_Primitive::EL_PT_Primitive(LgsString& word)
    : word_(word)
{
}

//}}

#endif



