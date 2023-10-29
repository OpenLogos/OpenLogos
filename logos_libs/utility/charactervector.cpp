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
// File - CharacterVector.cpp
//
// Class - CharacterVector(implementation)
//
//---------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/utility/charactervector.h>
#include <logos_libs/utility/parseinto.h>

//-------------------------------------------------------------------
void
CharacterVector::concatenate( CharacterVector::const_iterator from,
                              CharacterVector::const_iterator to )
{
    for( CharacterVector::const_iterator i = from; i != to; i++ )
    {
        push_back( *i );
    }
}

//-------------------------------------------------------------------
void
CharacterVector::concatenateToSize( CharacterVector::const_iterator from,
                                    CharacterVector::const_iterator to,
                                    int                             maxSize )
{
    for( CharacterVector::const_iterator i = from; i != to; i++ )
    {
        if( size() >= maxSize )
        {
            break;
        }
        push_back( *i );
    }
}

//-------------------------------------------------------------------
void
CharacterVector::concatenateToSize( const LgsString& s, int maxSize )
{
    for( LgsString::size_type i = 0; i != s.size(); i++ )
    {
        if( size() >= maxSize )
        {
            break;
        }
        push_back( s[i] );
    }
}

//-------------------------------------------------------------------
void
CharacterVector::concatenateToSize( char c, int maxSize )
{
    if( size() < maxSize )
    {
        push_back( c );
    }
}

//-------------------------------------------------------------------
LgsString
CharacterVector::asString() const
{
    LgsString s;

    for( int i = 0; i < size(); i++ )
    {
        s.append(1, at( i ));
    }
    return s;
}

//-------------------------------------------------------------------
CharacterVector&
CharacterVector::operator +=(const LgsString &s)
{
    // for (LgsString::const_iterator iter = s.begin(); iter != s.end(); iter++)
    //    push_back(*iter);
    copy(s.begin(), s.end(),
#ifdef OS_NO_TYPEDEF_2
		back_inserter( *this, (char*)0)
#else
		back_inserter(*this)
#endif
		);
    return *this;
}

//-------------------------------------------------------------------
CharacterVector&
CharacterVector::operator +=(const CharacterVector &s)
{
    copy(s.begin(), s.end(), end());
    return *this;
}

void CharacterVector::parseInto(LgsStringVector &Strings, char delimiter,
    bool delimitersAreTokens) const
{
    gParseInto(begin(), end(), Strings, delimiter, delimitersAreTokens);
}


