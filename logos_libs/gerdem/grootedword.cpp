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
// File - GRootedWord.cpp
//
// Class - GRootedWord (implementation)
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/gerdem/grootedword.h>
#include <logos_libs/linguistic/lword.h>
#include <logos_libs/linguistic/llanguage.h>

//-------------------------------------------------------------------
GRootedWord::GRootedWord( const LWord* aWord )
    : p_word ( aWord )
{
    p_roots = p_word->language()->createRootsFromWord( *p_word );
    rootCursor = p_roots->begin();
}
//-------------------------------------------------------------------
GRootedWord::GRootedWord( const LWord* aWord,
                          LRootVector* theRoots )
    : p_word ( aWord )
    , p_roots( theRoots )
    , rootCursor( theRoots->begin() )
{
}
//-------------------------------------------------------------------
GRootedWord::GRootedWord( const GRootedWord& aRootedWord )
    : p_word ( aRootedWord.word() )
    , p_roots( const_cast <LRootVector*> (aRootedWord.roots() ) )
    , rootCursor( aRootedWord.rootCursor )
{
    //---------------------------------------------------------------
    // This is the copy constructor. Required by STL.
    //---------------------------------------------------------------

    p_roots = new LRootVector();

    *p_roots = *(aRootedWord.roots());

    rootCursor = p_roots->begin();
}
//-------------------------------------------------------------------
GRootedWord::~GRootedWord()
{
    //---------------------------------------------------------------
    // It is assumed that this object owns its collection of LRoot
    // objects. It is responsible for deleting the collection. The
    // STL collection cleans up the individual elements.
    //---------------------------------------------------------------

    delete p_roots;
}
//-------------------------------------------------------------------
bool GRootedWord::isRootMatchedBy( const LgsString& s ) const
{
    // Attempts to match every version of the object's text (its
    // word or one of its roots). It returns the first match found.
    // Roots are ordered by priority so the high-priority roots are
    // attempted first.

    if( s == *p_word )
    {
        return true;
    }
    for( LRootVector::const_iterator i = roots()->begin();
         i != roots()->end(); i++ )
    {
        if( s == *i )
        {
            return true;
        }
    }
    return false;
}
//-------------------------------------------------------------------
const LRoot*
GRootedWord::rootMatchedBy( const LgsString& s ) const
{
    // Attempts to match every version of the object's text (its
    // word or one of its roots). It returns the first match found.
    // Roots are ordered by priority so the high-priority roots are
    // attempted first.

    for( LRootVector::const_iterator i = roots()->begin();
         i != roots()->end(); i++ )
    {
        // 11/25/97 AHB: Changed comparison to ignore case. 
        if (StringUtil::equalsIgnoreCase(s, *i))
        {
            return &(*i);
        }
    }
    return 0;
}
//-------------------------------------------------------------------
const LgsString& GRootedWord::primarySearchKey()
{
    // Probably the most important method in this class. It returns
    // the primary database search key for the corresponding word.
    // This is either the word itself or its first root (the word
    // adjusted for inflections) the roots are ordered by inflection
    // priority. Note that the roots are preferred over the original
    // word. The

    if( !p_roots->empty() )
    {
        for( LRootVector::iterator i = p_roots->begin();
             i < p_roots->end(); i++ )
        {
            if( i->size() > 2 )
            {
                rootCursor = i;
                return *rootCursor;
            }
        }
    }
        rootCursor = p_roots->end();
    return *p_word;
}
//-------------------------------------------------------------------
const LRoot* GRootedWord::smallestRoot()
{

    LRoot* smallestRoot = 0;

    for( LRootVector::iterator i = p_roots->begin();
                               i < p_roots->end(); i++ )
    {
        if( (0 == smallestRoot) ||
            (i->size() < smallestRoot->size()) )
        {
            smallestRoot = &(*i);
        }
    }
    return smallestRoot;
}
//-------------------------------------------------------------------
const LgsString& GRootedWord::nextSearchKey()
{
    // The method is useful after the PrimarySearchKey method has
    // used. Obtains the root of the next highest priority. If
    // there is no more roots, it returns the word itself.

        if( rootCursor != p_roots->end() )
        {
        ++rootCursor;
            if( rootCursor != p_roots->end() )
            {
            return *rootCursor;
        }
    }
    return *p_word;
}
