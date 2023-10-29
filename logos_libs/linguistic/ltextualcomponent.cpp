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
// File - LTextualComponent.cpp
//
// Class - LTextualComponent (implementation)
//
//---------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/ltextualcomponent.h>
#include <logos_libs/linguistic/linflection.h>
#include <logos_libs/linguistic/llanguage.h>

//---------------------------------------------------------------------
LTextualComponent::LTextualComponent()
    : p_language( 0 )
{
}
//---------------------------------------------------------------------
LTextualComponent::LTextualComponent( const LgsString&      aString,
                                                  const LLanguage*   aLanguage )
    : LinguisticToken( aString, LLanguage::nullInflection() )
    , p_language     ( aLanguage )
{
}
//---------------------------------------------------------------------
LTextualComponent::LTextualComponent( const LgsString&      aString,
                                                  const LLanguage*   aLanguage,
                                      const LInflection* anInflection )
    : LinguisticToken( aString, anInflection )
    , p_language     ( aLanguage )
{
}
//---------------------------------------------------------------------
LTextualComponent::LTextualComponent( const LTextualComponent& rhs )
    : LinguisticToken( rhs )
    , p_language     ( rhs.language() )
{
    // This is the copy constructor.
}
//---------------------------------------------------------------------
LTextualComponent::~LTextualComponent()
{
}
//---------------------------------------------------------------------
bool LTextualComponent::isOrdinal() const
{
    bool result = false;

    for( LgsString::size_type i = 0; i < size(); i++ )
    {
        if (i && (!CharUtil::isNumeric(at(i))))
        {
            result = language()->isOrdinalSuffix( substr( i ) );
            break;
        }
    }
    return result;
}
//---------------------------------------------------------------------
bool
LTextualComponent::isDate() const
{
    bool result = false;

    if( (numericPattern() == "#/#/#") || (numericPattern() == "#.#.#") )
    {
        result = true;
    }
    return result;
}
//---------------------------------------------------------------------
inline bool isDigit( char c )
{
    return CharUtil::isNumeric(c);
}
//---------------------------------------------------------------------
LgsString
LTextualComponent::numericPattern() const
{
    LgsString s( c_str() );
    replace_if( s.begin(), s.end(), isDigit, char('#') );
    LgsString::iterator newEnd = unique( s.begin(), s.end() );
    s.erase( newEnd, s.end() );
    return s;
}
//---------------------------------------------------------------------
const LTextualComponent&
LTextualComponent::operator=( const LTextualComponent& rhs )
{
    if( this != &rhs )
    {
        LinguisticToken::operator=( rhs );
        p_language = rhs.language();
    }
    return *this;
}
//---------------------------------------------------------------------
void
LTextualComponent::persistOut(char * textPtr)
{
    memcpy(textPtr, c_str(), length());
}
//-------------------------------------------------------------------
void
LTextualComponent::persistIn(char * textPtr)
{
    LgsString::operator=(textPtr);
}
