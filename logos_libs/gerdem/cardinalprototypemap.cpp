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
// File - cardinalprototypemap.cpp
//
// Class - CardinalPrototypeMap (implementation)
//
//---------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/gerdem/cardinalprototypemap.h>
#include <logos_libs/linguistic/lsemantosyntacticunit.h>

//---------------------------------------------------------------------
CardinalPrototypeMap::CardinalPrototypeMap()
{
    InsertEntry( "0"   , createEntry(  0, 20,  20 ) );
    InsertEntry( "1"   , createEntry(  1, 21, 123 ) );
    InsertEntry( "2"   , createEntry(  2, 21, 222 ) );
    InsertEntry( "3"   , createEntry(  3, 21, 234 ) );
    InsertEntry( "4"   , createEntry(  4, 21, 234 ) );
    InsertEntry( "5"   , createEntry(  5, 21, 567 ) );
    InsertEntry( "32"  , createEntry(  6, 23, 567 ) );
    InsertEntry( "100" , createEntry(  7, 40, 789 ) );
    InsertEntry( "1000", createEntry(  8, 51, 789 ) );
    InsertEntry( "1777", createEntry(  9, 52, 789 ) );
    InsertEntry( "2000", createEntry( 10, 53, 789 ) );
    InsertEntry( "2401", createEntry( 11, 54, 890 ) );
}
//---------------------------------------------------------------------
CardinalPrototypeMap::~CardinalPrototypeMap()
{
}
//---------------------------------------------------------------------
LDictionaryEntry*
CardinalPrototypeMap::EntryByPattern( const LgsString& pattern )
{
    int value = atoi( pattern.c_str() );
    LgsString keyword;

    if( value > 2400 )
    {
        keyword = "2401";
    }
    else if( value > 1999 )
    {
        keyword = "2000";
    }
    else if( value > 1776 )
    {
        keyword = "1777";
    }
    else if( value > 999 )
    {
        keyword = "1000";
    }
    else if( value > 99 )
    {
        keyword = "100";
    }
    else if( value > 31 )
    {
        keyword = "32";
    }
    else if( value > 4 )
    {
        keyword = "5";
    }
    else if( value >= 0 )
    {
        keyword = pattern;
    }
    else
    {
        keyword = "2401";
    }

    return EntryByKeyword( keyword );
}
//---------------------------------------------------------------------
LDictionaryEntry*
CardinalPrototypeMap::createEntry( int cardinality,
                                   int setID,
                                   int subsetID )
{
    LDictionaryEntry* pEntry = new LDictionaryEntry;

    for( int i = 0; i < 3; i++ )
    {
        LSemantoSyntacticUnit ssu;

        ssu.setWordCount( 1 );
        ssu.setWordID   ( -30 );

        switch( i )
        {
            case 0:
                ssu.setWordClassCode( LLanguage::ARITHMATE );
                ssu.setSuperSetID( 4 );
                ssu.setFormCode  ( 1 );
                break;
            case 1:
                ssu.setWordClassCode( LLanguage::ARTICLE_DEFINITE );
                ssu.setSuperSetID( 9 );
                ssu.setFormCode  ( 8 );
                break;
            case 2:
                ssu.setWordClassCode( LLanguage::ADJECTIVE );
                ssu.setSuperSetID( 13 );
                ssu.setFormCode  ( 9 );
                break;
        }
        if( (1 == cardinality) && (1 == i) )
        {
            ssu.setFormCode( 7 );
        }
        ssu.setSetID   ( setID );
        ssu.setSubSetID( subsetID );

        pEntry->addSsu( ssu );
    }
    pEntry->setToUnfoundWord();

    pEntry->setCached( true );

    return pEntry;
}
