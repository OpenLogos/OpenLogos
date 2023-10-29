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
#ifndef __GWordCache_h__
#define __GWordCache_h__

//-------------------------------------------------------------------
// File - GWordCache.h
//
// Class - GWordCache (interface)
//
// Description - This caches a simple set of strings (each one
//     representing a source word. It can be used for caching various
//     unfound words, found words, etc.,. Only stores the text of the
//     word itself for matching purposes.
//
//-------------------------------------------------------------------

class GWordCache
{
public:
        //---------------------------------------------------------------
    // Destructor.
        //---------------------------------------------------------------

                 GWordCache();
        virtual ~GWordCache();

    bool Find  ( const LgsString& ) const;
    void Insert( const LgsString& );

    int Count();

private:

    LgsSet(LgsString) v_set;
};


//-------------------------------------------------------------------
inline int
GWordCache::Count()
{
    return v_set.size();
}
//-------------------------------------------------------------------
inline bool
GWordCache::Find( const LgsString& s ) const
{
    // Per STL, if the result of the find() method is the same as
    // end() the LgsString was not found.

    if( v_set.find( s ) == v_set.end() )
    {
       return false;
    }
    return true;
}
//-------------------------------------------------------------------
inline void
GWordCache::Insert( const LgsString& s )
{
    v_set.insert( s );
}

#endif // __GWordCache_h__

