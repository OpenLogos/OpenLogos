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
#ifndef __GAbstractMatcher_h__
#define __GAbstractMatcher_h__

//-------------------------------------------------------------------
// File - GAbstractMatcher.h
//
// Class - GAbstractMatcher (interface)
//
// Description - this is an abstract class that defines the interface
//      for those classes that match words in the sentence to the
//      entries in the dictionary.
//
//-------------------------------------------------------------------

#include <logos_libs/linguistic/lword.h>
#include <logos_libs/entity/dwordphrase.h>
#include <logos_libs/linguistic/ldictionarytoken.h>
#include <logos_libs/linguistic/linguisticfactory.h>
#include <logos_libs/gerdem/phrasematchsequencer.h>

class GAbstractMatcher
{
public:
    //---------------------------------------------------------------
    // The desctructor is virtual to support polymorphism.
    //---------------------------------------------------------------

    virtual ~GAbstractMatcher();

    //---------------------------------------------------------------
    // The FindWordMatch() member function is declared pure virtual
    // in this class. It defines the critical protocol for the
    // subclasses. In the subclasses this member function returns a
    // pointer to a DWordMatch object is considered the best match
    // for the vector of GWord objects passed in as an arguement.
    //---------------------------------------------------------------

    // not const due to phrase cache
    virtual
    LDictionaryTokenVector* FindPhraseMatch( const LWordIterator& begin,
                                             const LWordIterator& end,
											 const LgsString& firstWord,
											 const LgsString& secondWord,
											 short noSpaces,
											 PhraseMatchSequencer::RootAnalysisCode analysisCode) = 0;
    // not const due to phrase cache
    virtual
    DWordPhraseVector* findPhraseCandidates( const LgsString& first, short firstSpaceCount,
                                             const LgsString& second ) = 0;
    virtual
    LDictionaryTokenVector* FindWordMatch  ( const LTextualComponent&, bool BOS, bool bAllCapitalWords ) = 0;

    virtual DWordPhraseVector* FindCachedMatch( const LgsString&, bool BOS ) const = 0;

    virtual void InsertMatchIntoCache( const LgsString&, DWordPhraseVector*, bool BOS ) = 0;

    virtual void               Factory( LinguisticFactory* ) = 0;
    virtual LinguisticFactory& Factory()            = 0;

protected:
    //---------------------------------------------------------------
    // The default constructor is protected because this is an
    // abstract class. The only way that this constructor is called
    // is through the instantiation of a subclass object. A subclass
    // object has access to a protected function.
    //---------------------------------------------------------------

    GAbstractMatcher();

private:
    //---------------------------------------------------------------
    //---------------------------------------------------------------
};

#endif // __GAbstractMatcher_h__

