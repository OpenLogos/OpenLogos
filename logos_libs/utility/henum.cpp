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
//---------------------------------------------------------------------
// File - henum.cpp
//
// Class - Henum (implementation)
//
//---------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/utility/henum.h>
#include <logos_libs/utility/charactervector.h>
#include <logos_libs/regex/charutil.h>

//---------------------------------------------------------------------
Henum::Henum( const LgsString& s )
{
    generate( s );
}
//---------------------------------------------------------------------
Henum::~Henum()
{
}
//---------------------------------------------------------------------
void Henum::generate( const LgsString& s,
                      int* firstHenum,
                      int* secondHenum )
{
    Henum theHenum( s );
    *firstHenum  = theHenum.first();
    *secondHenum = theHenum.second();
}
//---------------------------------------------------------------------
//  Henum::generate() - (pseudo-code)
//
// limit the LgsString to 160 characters
// for each character in input LgsString
//     if the character is blank
//         break out of this loop
//     if the character is not a '*' and not a '4' preceded by a vowel
//         if the character is a vowel
//             add corresponding vowel number to Vowel Array
//         else
//             add corresponding consonant number to Consonant Array
// end for loop
// set the Digit Count to the number of consonants found (max 7)
// assign the Consonant Array elements to Hash Array
// if there is less than 7 elements in Hash Array
//     concatenate a 0
// if there is still less than 7 elements in Hash Array
//     concatenate elements of Vowel Array onto the Hash Array
// if there is still less than 7 elements in Hash Array
//     zero fill Hash Array up to 7 elements
// if the count of vowels is greater than 5 or equal to 0
//     set the count of vowels to 5
// decrement the vowel count by one
// if the first Consonant is in a special set
//     add 5 to the vowel count
// if there are exactly 7 consonants and
//                 the LgsString ended in a consonants
//     set the vowel count to 0
// depending upon the vowel count add a small integer to Hash Array
//
//---------------------------------------------------------------------
void Henum::generate( const LgsString& source )
{

    CharacterVector vowels;
    CharacterVector nonVowels;
    CharacterVector hashVector;

    if( !source.empty() )
    {
        partitionVowels( source, vowels, nonVowels );
        hashVector.concatenateToSize( nonVowels.begin(), nonVowels.end(), 7 );
        hashVector.concatenateToSize( char('0'), 7 );
        hashVector.concatenateToSize( vowels.begin(), vowels.end(), 7 );
        hashVector.concatenateToSize( "0000000", 7 );

        hashVector.push_back( determineLastDigit( nonVowels, vowels, source ) );
        loadHenums( hashVector );
    }
    else
    {
        setFirst ( -1 );
        setSecond( -1 );
    }
}
//---------------------------------------------------------------------
void Henum::partitionVowels( const LgsString&    source,
                             CharacterVector& vowels,
                             CharacterVector& nonVowels )
{
    bool isLastCharacterVowel = false;
    for( LgsString::size_type i = 0; i < source.size(); i++ )
    {
        char character = source[i];
        if( (character == CharUtil::space()) || (character == '*') || (character == '-') )
        {
            continue;
        }
        if( CharUtil::isVowel(character))
        {
            isLastCharacterVowel = true;
            vowels.push_back(CharUtil::henumChar(character));
        }
        else
        {
			if (character == 'ﬂ')
			{
				character = 's';
				nonVowels.push_back(CharUtil::henumChar(character));
			}
            nonVowels.push_back(CharUtil::henumChar(character));
        }
    }
}
//---------------------------------------------------------------------
char Henum::determineLastDigit( const CharacterVector& nonVowels,
                                const CharacterVector& vowels,
                                const LgsString&          source )
{
    int vowelCount = setVowelCount( vowels );

    if( !nonVowels.empty() )
    {
        LgsString specials = "cfhkmprtwyCFHKMPRTWY";
        LgsString::size_type firstPos = source.find_first_not_of(CharUtil::vowels());
		if( firstPos != NPOS )
        {
			if( NPOS !=  specials.find( source[firstPos] ) )
            {
                vowelCount += 5;
            }
        }
        if( nonVowels.size() == 7 )
        {
            char lastChar( source[source.size() - 1] );
            if( !CharUtil::isVowel(lastChar))
            {
                vowelCount = 0;
            }
        }
    }
    return '0' + vowelCount;
}
//---------------------------------------------------------------------
void Henum::loadHenums( CharacterVector& hashVector )
{
    assert( hashVector.size() == 8 );

    LgsString buffer = hashVector.asString().c_str();

    setFirst ( atoi( (const char *)buffer.substr( 0, 4 ).c_str() ) );
    setSecond( atoi( (const char *)buffer.substr( 4, 8 ).c_str() ) );
}
//---------------------------------------------------------------------
int Henum::setVowelCount( const CharacterVector& vowels )
{
    if( (vowels.size() > 5) || vowels.empty() )
    {
       return 4;
    }
    return vowels.size() - 1;
}
