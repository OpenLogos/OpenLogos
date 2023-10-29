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
// File - LInflection.cpp
//
// Class - LInflection (implementation)
//
//---------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/linflection.h>
#include <logos_libs/linguistic/lroot.h>
#include <logos_libs/linguistic/lword.h>
#include <logos_libs/utility/stringutil.h>

//---------------------------------------------------------------------
LInflection::LInflection()
    : v_priority( 100 )
{
}
//---------------------------------------------------------------------
LInflection::LInflection( const LgsString& anEnding, int aPriority )
    : LgsString( anEnding  )
    , v_priority  ( aPriority )
{
        // This constructor allows the object to be initialized from some
        // text and a specified language.
}
//---------------------------------------------------------------------
LInflection::LInflection( const LInflection& rhs )
    : LgsString( rhs )
    , v_priority  ( rhs.priority() )
{
        // This is the copy constructor.
}
//---------------------------------------------------------------------
LInflection::~LInflection()
{
}
//---------------------------------------------------------------------
LRoot* LInflection::createRoot( const LWord& aWord ) const
{
    //

    LRoot* pRoot = 0;
/* 
this is too slow, too many extra allocations while
creating new strings.

    if( rootLength > 1 )
    {
        LgsString lcWord = aWord;
        if (StringUtil::beginsUpperCase(lcWord))
            StringUtil::toLower(lcWord);
        LgsString lcRoot = lcWord.substr( 0, rootLength );
        if( lcWord == (lcRoot + *this) )
        {
            pRoot = new LRoot( aWord.substr( 0, rootLength ),
                                aWord.language(),
                                this );
printf("\"%s\" \"%s\"\n", lcWord.c_str(), (lcRoot+*this).c_str());
fflush(stdout);
        }
    }
*/

	int sz = size();
    int rootLength = aWord.size() - sz;
    if(rootLength>1) {
		int i, j = rootLength;
		for(i=0;i<sz;i++,j++) {
			if(this->at(i)!=tolower(aWord.at(j)))
				return pRoot;
		}
		pRoot = new LRoot(aWord.substr(0, rootLength), aWord.language(), this);
	}

	return pRoot;
}
//---------------------------------------------------------------------
LWord* LInflection::createWord( const LRoot& aRoot ) const
{
    //

    return new LWord( aRoot + *this, aRoot.language() );
}
//---------------------------------------------------------------------
const LInflection& LInflection::operator=( const LInflection& rhs )
{
    if( this != &rhs )
    {
        LgsString::operator=( rhs );
        v_priority = rhs.priority();
    }
    return *this;
}

