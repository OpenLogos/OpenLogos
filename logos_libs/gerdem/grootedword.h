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
#ifndef __GRootedWord_h__
#define __GRootedWord_h__

//-------------------------------------------------------------------
// File - GRootedWord.h
//
// Class - GRootedWord (interface)
//
// Description - the GRootedWord class defines a set of objects that
//      contain a source word with all of its forms after adjusting
//      for all the inflections that are possible within a given
//      language. The inflections are ordered by the priority of the
//      inflections.
//
//-------------------------------------------------------------------

#include <logos_libs/linguistic/lroot.h>
#include <logos_libs/linguistic/lword.h>

class GRootedWord
{
public:
        //-----------------------------------------------------------------
    // Constructors - typically the objects of this class are created
        //-----------------------------------------------------------------

             GRootedWord() {};       // DO NOT USE! only for STL
                         GRootedWord( const LWord* );
                         GRootedWord( const LWord*,
                                          LRootVector* );
                         GRootedWord( const GRootedWord& );
        virtual ~GRootedWord();

        //---------------------------------------------------------------
    // Word() - returns the word that created the object.
    // Roots() - returns the set of roots that are created from a
    //     rooted word.
    // PrimaryKey() - probably the most important method in the
    //     class. This method knows that the primary key to use in
    //     the root of highest priority or, if there are no roots,
    //     the word itself.
    // IsRootMatchedBy() - this function compares a LgsString to, not
    //     only the LWord but all of its roots in attempting to
    //     obtain a match.
        //---------------------------------------------------------------

        const LWord*       word () const;
        const LRootVector* roots() const;

        const LgsString& primarySearchKey();
        const LgsString& nextSearchKey   ();

    const LRoot*  smallestRoot();

    bool          isRootMatchedBy ( const LgsString& ) const;
    const LRoot*  rootMatchedBy   ( const LgsString& ) const;

private:
        //---------------------------------------------------------------
        //---------------------------------------------------------------

        const LWord* p_word ;
        LRootVector* p_roots;
        LRootVector::iterator rootCursor;
};

//-------------------------------------------------------------------
inline const LWord* GRootedWord::word() const
{
        return p_word;
}
//-------------------------------------------------------------------
inline const LRootVector* GRootedWord::roots() const
{
        return p_roots;
}

#endif // __GRootedWord_h__

