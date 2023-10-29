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
// File - GWordCapQuery.cpp
//
// Class - GWordCapQuery (implementation)
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/gerdem/gwordcapquery.h>
#include <logos_libs/entity/dwordphrase.h>
#include <logos_libs/sql/sqlstatement.h>
#include <logos_libs/sql/sqlcolumn.h>
#include <logos_libs/linguistic/llanguage.h>
#include <logos_libs/sql/sqlexception.h>
#include <logos_libs/linguistic/llanguage.h>
#include <lgs_db_io/querytodictionary.h>

#include <logos_libs/dbcache/CacheGphraseData.h>
#include <logos_libs/dbcache/CacheGwordQuery.h>

//----------------------------------------------------------------------
GWordCapQuery::GWordCapQuery()
    : v_languageCode( 0 )
{
    v_inputLanguageCode[0] = 0;
    v_inputWord[0] = 0;
    v_lowerWord[0] = 0;

	gphrase_data = NULL;
	gquery = NULL;
	gquery_lower = NULL;

}
//----------------------------------------------------------------------
GWordCapQuery::~GWordCapQuery()
{
	if(gphrase_data) { delete gphrase_data; gphrase_data = NULL; }
	if(gquery) { delete gquery; gquery = NULL; }
	if(gquery_lower) { delete gquery_lower; gquery_lower = NULL; }
}
//----------------------------------------------------------------------
void GWordCapQuery::Open( SqlConnection* aConnection,
                              const LLanguage& language )
{

	BindLanguage(language);

	gphrase_data = new CacheGphraseData(NULL, v_languageCode,false,false,false);

	if(gphrase_data->isValid()) {
		gquery = new CacheGwordQuery(gphrase_data, NULL);
		gquery_lower = new CacheGwordQuery(gphrase_data, NULL);
		return;
	} else {
		delete gphrase_data;
		gphrase_data = NULL;
		gquery = NULL;
		gquery_lower = NULL;
	}

    try
    {
		if(aConnection==NULL)
			return;

        SqlQuery::Open( aConnection );

        LgsString s = "select Word_ID,"
                      " Hash_Code_1, Hash_Code_2,"
					  " Root_Hash_1, Root_Hash_2,"
					  " Hen_Num_1, Hen_Num_2,"
					  " Root_Hen_1, Root_Hen_2,"
					  " Head_Word, Company_Code"
					  " from Word_Phrase"
					  " where Language_Code = :aCode"
					  " and (Word = :aWord or Word = :bWord)" ;

        Statement()->AddToCommandString( s );

        Statement()->Parse();

        BindLanguage( language );

        p_id				= Statement()->BindOutputColumn( 1, SqlColumn::Integer );
        p_hashCode1			= Statement()->BindOutputColumn( 2, SqlColumn::Integer );
        p_hashCode2			= Statement()->BindOutputColumn( 3, SqlColumn::Integer );
        p_rootHashCode1		= Statement()->BindOutputColumn( 4, SqlColumn::Integer );
        p_rootHashCode2		= Statement()->BindOutputColumn( 5, SqlColumn::Integer );
        p_henNum1			= Statement()->BindOutputColumn( 6, SqlColumn::Integer );
        p_henNum2			= Statement()->BindOutputColumn( 7, SqlColumn::Integer );
        p_rootHenNum1		= Statement()->BindOutputColumn( 8, SqlColumn::Integer );
        p_rootHenNum2		= Statement()->BindOutputColumn( 9, SqlColumn::Integer );
        p_headWord			= Statement()->BindOutputColumn(10, SqlColumn::Integer );
        p_companyCode		= Statement()->BindOutputColumn(11, SqlColumn::StringType);
    }
    catch( SqlException& x )
    {
        cout << x.Message() << endl;
        throw( x );
    }
}
//----------------------------------------------------------------------
void GWordCapQuery::Close()
{
	if(gphrase_data==NULL)
		SqlQuery::Close();
}
//---------------------------------------------------------------------
void GWordCapQuery::SetLowerCase()
{
    for( int i = 0; i < strlen( v_inputWord ); i++ )
    {
            v_lowerWord[i] = tolower( v_inputWord[i] );
        }
    v_lowerWord[strlen(v_inputWord)] = 0;
}
//----------------------------------------------------------------------
void
GWordCapQuery::BindLanguage( const LLanguage& language )
{
    v_languageCode = language.id();
}
//----------------------------------------------------------------------
void
GWordCapQuery::BindLanguage()
{
    Statement()->BindInputIntToString( ":aCode",
                                       "%02d",
                                       v_inputLanguageCode,
                                       v_languageCode );
}
//----------------------------------------------------------------------
void GWordCapQuery::BuildInputWords( const char* match )
{
    strcpy( v_inputWord, match );
}
//----------------------------------------------------------------------
void GWordCapQuery::ExecuteWithMatch()
{

	SetLowerCase();

	if(gphrase_data) {
		gquery->query(v_inputWord);
		gquery_lower->query(v_lowerWord);
		return;
	}

	if(Statement()==NULL)
		return;

    try
    {
        BindLanguage();
        Statement()->BindInputString( ":aWord", v_inputWord );
        Statement()->BindInputString( ":bWord", v_lowerWord );

        QueryToDictionary::GetObject()->PrintWordCap( v_inputWord, v_lowerWord );

        Statement()->Execute ();
    }
    catch( SqlException& x )
    {
        cout << x.Message() << endl;
        throw( x );
    }
}
//----------------------------------------------------------------------
bool
GWordCapQuery::FetchIntoWordMatch( DWordPhrase* p )
{

	bool result = true;

	int word_id, h1, h2, rhash1, rhash2, hennum1, hennum2, rhen1, rhen2,
	hword, dummy_hashloc, dummy_wtc, dummy_word_count;
	char cc[20];
	if(gphrase_data) {
		
		result = gquery->fetch(&word_id, &dummy_word_count, 
			&h1, &h2, &rhash1, &rhash2, &hennum1, &hennum2, &rhen1, &rhen2,
			&hword, &dummy_hashloc, &dummy_wtc, cc);
		if(!result)
			result = gquery_lower->fetch(&word_id, &dummy_word_count, 
				&h1, &h2, &rhash1, &rhash2, &hennum1, &hennum2, &rhen1, &rhen2,
				&hword, &dummy_hashloc, &dummy_wtc, cc);

		if(result) {
			p->Initialize(0,
			h1, h2, rhash1, rhash2,	// cached hash code info
			hennum1, hennum2, rhen1, rhen2,		// cached hen num info
			1, hword,
			false, false, false, false, false, false,
			2, 0, 0, 1,
			word_id, 0, 0);
			p->CompanyCode(cc);
			p->Word( v_inputWord );
//printf("\nGWordQuery \"%s\" %d %d %d %d %d %d %d %d %d %d %d %d \"%s\" %d\n", 
//v_inputWord, h1, h2, rhash1, rhash2, hennum1, hennum2,
//rhen1, rhen2, hashloc, hword, word_id, wtc, p->CompanyCode().c_str());
		}
//printf("\nGWordQuery result %d\n", result);
//fflush(stdout);

		return result;
	}


	if(Statement()==NULL)
		return false;

    try
    {
        result = Statement()->Fetch();
    }
    catch( SqlException& x )
    {
        cout << x.Message() << endl;
        throw( x );
    }
    if( result == true )
    {
		p->Initialize(
              0,
              p_hashCode1->AsInteger(),
              p_hashCode2->AsInteger(),
              p_rootHashCode1->AsInteger(),
              p_rootHashCode2->AsInteger(),
			  p_henNum1->AsInteger(),
			  p_henNum2->AsInteger(),
			  p_rootHenNum1->AsInteger(),
			  p_rootHenNum2->AsInteger(),
              1,
              p_headWord->AsInteger(),
              false,
              false,
              false,
              false,
              false,
              false,
              2,
              0,
              0,
              1,
              p_id->AsInteger(),
              0, 0 );
        p->CompanyCode( p_companyCode->AsString() );
        p->Word( v_inputWord );
    }
    return result;
}
