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
// File - compositestring.cpp
//
// Class - CompositeString (implementation)
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/utility/compositestring.h>
#include <logos_libs/utility/charactervector.h>
#include <logos_libs/utility/stringutil.h>

//-------------------------------------------------------------------
CompositeString::CompositeString()
    : v_state( Readable )
{
}
//-------------------------------------------------------------------
CompositeString::CompositeString( const CompositeString& rhs )
    : v_buffer    ( rhs.v_buffer )
    , v_components( rhs.v_components )
    , v_state     ( rhs.v_state  )
{
}
//-------------------------------------------------------------------
CompositeString::~CompositeString()
{
}
//-------------------------------------------------------------------
CompositeString& CompositeString::operator=( const CompositeString& rhs )
{
    if( &rhs == this )
    {
        v_buffer     = rhs.v_buffer;
        v_components = rhs.v_components;
        v_state      = rhs.v_state;
    }
    return *this;
}
//-------------------------------------------------------------------
const LgsStringVector&
CompositeString::components() 
{
    if( NewComposite == v_state )
    {
        /*(const_cast<CompositeString&>(*this)).*/synchronizeComponents();
    }
    return v_components;
}
//-------------------------------------------------------------------
const LgsString&
CompositeString::asString() 
{
    if( NewComponent == v_state )
    {
        /*const_cast<CompositeString&>(*this)).*/synchronizeComposite();
    }
    return v_buffer;
}
//-------------------------------------------------------------------
void
CompositeString::parseFrom( const LgsString& s )
{
    v_buffer = s;
    v_state = NewComposite;
}
//-------------------------------------------------------------------
void
CompositeString::setToComponent( const LgsString& t )
{
    v_components.erase( v_components.begin(), v_components.end() );
    addComponent( t );
}
//-------------------------------------------------------------------
void
CompositeString::addComponent( const LgsString& t )
{
    v_components.push_back( t );
    v_state = NewComponent;
}
//-------------------------------------------------------------------
void
CompositeString::synchronizeComposite() 
{
    CharacterVector s;

    for( LgsStringConstIterator i =  v_components.begin(); i != v_components.end(); i++ )
    {
        s += *i;
        if (i != (v_components.end() - 1))
        {
            s.push_back(' ');
        }
    }
	for(LgsVector(char)::iterator beginIt = s.begin(); beginIt != s.end(); beginIt++)
	v_buffer += *beginIt ;
    //v_buffer = LgsString(s.begin(), s.end());
    v_state = Readable;
}
//-------------------------------------------------------------------
void
CompositeString::synchronizeComponents() 
{
    StringUtil::parseInto(v_buffer, v_components);
    v_state = Readable;
}
