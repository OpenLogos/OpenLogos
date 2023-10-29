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
// File - patternprototypebuilder.cpp
//
// Class - PatternPrototypeBuilder
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/gerdem/patternprototypebuilder.h>
#include <logos_libs/gerdem/patternprototypemap.h>
#include <logos_libs/linguistic/ldictionaryentry.h>

//-------------------------------------------------------------------
PatternPrototypeBuilder::PatternPrototypeBuilder()
{
}
//-------------------------------------------------------------------
PatternPrototypeBuilder::~PatternPrototypeBuilder()
{
}
//-------------------------------------------------------------------
void
PatternPrototypeBuilder::Build( PatternPrototypeMap& aMap ) const
{
    aMap.InsertEntry( "#/#/#", CreateDateEntry     () );
    aMap.InsertEntry( "#.#.#", CreateDateEntry     () );
    aMap.InsertEntry( ".#"   , CreateFractionEntry () );
    aMap.InsertEntry( "#/#"  , CreateFractionEntry () );
    aMap.InsertEntry( "#.#"  , CreateMixedModeEntry() );
    aMap.InsertEntry( "# #/#", CreateMixedModeEntry() );
    aMap.InsertEntry( "#-#"  , CreateHyphenEntry   () );
}
//-----------------------------------------------------------------------
LDictionaryEntry*
PatternPrototypeBuilder::CreateDateEntry() const
{
    LSemantoSyntacticUnit ssu;

    ssu.setCompanyCode( "LOG" );
    ssu.setWordCount( 1 );
    ssu.setWordID   ( -10 );

    ssu.setWordClassCode( 16 );

    ssu.setSetID     ( 10 );
    ssu.setFormCode  ( 1 );
    ssu.setSubSetID  ( 178 );
    ssu.setSuperSetID( 10 );

    LDictionaryEntry* pEntry = new LDictionaryEntry;

    pEntry->setToUnfoundWord();
    pEntry->addSsu( ssu );

    pEntry->setCached( true );

    return pEntry;
}
//-----------------------------------------------------------------------
LDictionaryEntry*
PatternPrototypeBuilder::CreateFractionEntry() const
{
    LDictionaryEntry* pEntry = CreateDefaultEntry();

    for( int i = 0; i < 3; i++ )
    {
        LSemantoSyntacticUnit ssu;

        setDefaultWordPhrase( &ssu );
        setDefaultMorphology( &ssu, i );
        setDefaultMeaning   ( &ssu, i );

        ssu.setSetID   ( 19 );
        ssu.setSubSetID( 919 );

        pEntry->addSsu( ssu );
    }
    return pEntry;
}
//-----------------------------------------------------------------------
LDictionaryEntry*
PatternPrototypeBuilder::CreateDefaultEntry() const
{
    LDictionaryEntry* pEntry = new LDictionaryEntry;

    pEntry->setToUnfoundWord();
    pEntry->setCached( true );

    return pEntry;
}
//-----------------------------------------------------------------------
void
PatternPrototypeBuilder::setDefaultWordPhrase( LSemantoSyntacticUnit* p ) const
{
    p->setCompanyCode( "LOG" );
    p->setWordCount  ( 1 );
    p->setWordID     ( -10 );
}
//-----------------------------------------------------------------------
void
PatternPrototypeBuilder::setDefaultMorphology( LSemantoSyntacticUnit* p,
                                               int index ) const
{
    switch( index )
    {
        case 0: p->setWordClassCode( 16 ); break;
        case 1: p->setWordClassCode( 14 ); break;
        case 2: p->setWordClassCode(  4 ); break;
        default: assert( 0 ); break;
    }
}
//-----------------------------------------------------------------------
void
PatternPrototypeBuilder::setDefaultMeaning( LSemantoSyntacticUnit* p,
                                            int index ) const
{
    p->setFormCode  ( 1 );

    switch( index )
    {
        case 0: p->setSuperSetID(  4 ); break;
        case 1: p->setSuperSetID(  9 ); break;
        case 2: p->setSuperSetID( 13 ); break;
        default: assert( 0 ); break;
    }
}
//-----------------------------------------------------------------------
LDictionaryEntry*
PatternPrototypeBuilder::CreateMixedModeEntry() const
{
    LDictionaryEntry* pEntry = CreateDefaultEntry();

    for( int i = 0; i < 3; i++ )
    {
        LSemantoSyntacticUnit ssu;

        setDefaultWordPhrase( &ssu );
        setDefaultMorphology( &ssu, i );
        setDefaultMeaning   ( &ssu, i );

        ssu.setSetID   ( 18 );
        ssu.setSubSetID( 818 );

        pEntry->addSsu( ssu );
    }
    return pEntry;
}
//-----------------------------------------------------------------------
LDictionaryEntry*
PatternPrototypeBuilder::CreateHyphenEntry() const
{
    LDictionaryEntry* pEntry = CreateDefaultEntry();

    for( int i = 0; i < 3; i++ )
    {
        LSemantoSyntacticUnit ssu;

        setDefaultWordPhrase( &ssu );
        setDefaultMorphology( &ssu, i );
        setDefaultMeaning   ( &ssu, i );

        ssu.setSetID   ( 94 );
        ssu.setSubSetID( 94 );

        pEntry->addSsu( ssu );
    }
    return pEntry;
}
