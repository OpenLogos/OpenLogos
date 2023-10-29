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
#ifndef __ldictionarytoken_h__
#define __ldictionarytoken_h__

//-------------------------------------------------------------------
// File - ldictionarytoken.h
//
// Class - LDictionaryToken (interface)
//
//-------------------------------------------------------------------

#include <logos_libs/linguistic/linguistictoken.h>

class DWordPhrase;

class LDictionaryToken: public LinguisticToken
{
public:

    DummyLess(LDictionaryToken);

    //---------------------------------------------------------------
    //---------------------------------------------------------------

    LDictionaryToken();
    LDictionaryToken( const LgsString& );
    LDictionaryToken( const LDictionaryToken& );

    LDictionaryToken( DWordPhrase* );

    virtual ~LDictionaryToken();

    //-----------------------------------------------------------------
    //-----------------------------------------------------------------

    DWordPhrase& wordPhrase() const;
    void         wordPhrase( DWordPhrase* );

    //-----------------------------------------------------------------
    //-----------------------------------------------------------------

    const LDictionaryToken& operator=( const LDictionaryToken& );

    //-----------------------------------------------------------------
    //-----------------------------------------------------------------

    int     proposedHeadWord() const;
    void setProposedHeadWord( int );

    //-----------------------------------------------------------------
    //-----------------------------------------------------------------

    int     wordsUsedInMatch() const;
    void setWordsUsedInMatch( int );

private:
    //---------------------------------------------------------------
    //---------------------------------------------------------------

    DWordPhrase*  p_wordPhrase;

    int proposedHeadWord_;
    int wordsUsedInMatch_;
};

typedef LgsVector(LDictionaryToken) LDictionaryTokenVector;
typedef LDictionaryTokenVector::iterator LDictionaryTokenIterator;

//-------------------------------------------------------------------
inline DWordPhrase& LDictionaryToken::wordPhrase() const
{
    return *p_wordPhrase;
}

inline void LDictionaryToken::wordPhrase( DWordPhrase* p )
{
    p_wordPhrase = p;
}
//-------------------------------------------------------------------
inline int LDictionaryToken::proposedHeadWord() const
{
    return proposedHeadWord_;
}
//-------------------------------------------------------------------
inline void LDictionaryToken::setProposedHeadWord( int n )
{
    proposedHeadWord_ = n;
}
//---------------------------------------------------------------------
inline int
LDictionaryToken::wordsUsedInMatch() const
{
    return wordsUsedInMatch_;
}

inline void
LDictionaryToken::setWordsUsedInMatch( int n )
{
    wordsUsedInMatch_ = n;
}

#endif // __ldictionarytoken_h__

