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
// File - GWordQuery.cpp
//
// Class - GWordQuery (implementation)
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/gerdem/gwordquery.h>
#include <logos_libs/entity/dwordphrase.h>
#include <logos_libs/sql/sqlstatement.h>
#include <logos_libs/sql/sqlcolumn.h>
#include <logos_libs/sql/sqlexception.h>
#include <logos_libs/linguistic/llanguage.h>
#include <lgs_db_io/querytodictionary.h>

#include <logos_libs/dbcache/CacheGphraseData.h>
#include <logos_libs/dbcache/CacheGwordQuery.h>

//----------------------------------------------------------------------
GWordQuery::GWordQuery()
    : v_languageCode( 0 )
{
    v_inputWord[0] = 0;
    v_inputLanguageCode[0] = 0;
	bdata = NULL;
	gquery = NULL;
}
//----------------------------------------------------------------------
GWordQuery::~GWordQuery()
{
	if(bdata) { delete bdata; bdata = NULL; }
	if(gquery) { delete gquery; gquery = NULL; }
}
//----------------------------------------------------------------------
void GWordQuery::Open( SqlConnection* aConnection,
                              const LLanguage& language )
{
	v_languageCode = language.id();
/* XXX tranlsation result mismatch between cache and real DB */
	bdata = new CacheGphraseData(NULL, v_languageCode, false, false, false);
	if(bdata->isValid()) {
		gquery = new CacheGwordQuery(bdata, NULL);
		return;
	} else {
		delete bdata;
		bdata = NULL;
		gquery = NULL;
	}
/**/
	// Note:
	// We get matches for ALL companies and then the SMC logic will be used 
	// to select the best possible match
	// Later, the query will be changed to include the list of companies 
	// populated from the company list provided by the user ONLY IF extended
	// search is turned OFF.
    LgsString s = "select"
                  " Word_ID,"
                  " Hash_Code_1, Hash_Code_2,"
				  " Root_Hash_1, Root_Hash_2,"
				  " Hen_Num_1, Hen_Num_2,"
				  " Root_Hen_1, Root_Hen_2,"
				  " Head_Word, Hash_Location, Word_Type_Code, Company_Code"
                  " from Word_Phrase"
                  " where Word = :aWord"
                  " and Language_Code = :aCode";

    try
    {
        SqlQuery::Open( aConnection );

        Statement()->AddToCommandString( s );

        Statement()->Parse();

        v_languageCode = language.id();

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
        p_headLocation		= Statement()->BindOutputColumn(11, SqlColumn::Integer );
		p_wordTypeCode		= Statement()->BindOutputColumn(12, SqlColumn::StringType );
        p_companyCode		= Statement()->BindOutputColumn(13, SqlColumn::StringType );
    }
    catch( SqlException& x )
    {
        cout << x.Message() << endl;
        throw( x );
    }
}
//----------------------------------------------------------------------
void GWordQuery::Close() {
	if(bdata==NULL)
    SqlQuery::Close();

}
//----------------------------------------------------------------------
void
GWordQuery::BuildInputWords( const char* match )
{
    strcpy( v_inputWord, match );
}
//----------------------------------------------------------------------
void GWordQuery::ExecuteWithMatch()
{

	if(bdata) {
		int ret = gquery->query(v_inputWord);
		return;
	}

    try
    {
        Statement()->BindInputIntToString( ":aCode",
                                           "%02d",
                                           v_inputLanguageCode,
                                           v_languageCode );
        Statement()->BindInputString( ":aWord", v_inputWord );

        QueryToDictionary::GetObject()->PrintWord( v_inputWord );

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
GWordQuery::FetchIntoWordMatch( DWordPhrase* p )
{
    bool result = true;

	int word_id, h1, h2, rhash1, rhash2, hennum1, hennum2, rhen1, rhen2,
		hword, hashloc, wtc, dummy_word_count;
	char cc[20];
	if(bdata) {
		result = gquery->fetch(&word_id, &dummy_word_count, 
			&h1, &h2, &rhash1, &rhash2, &hennum1, &hennum2, &rhen1, &rhen2,
			&hword, &hashloc, &wtc, cc);
		if(result) {
			p->Initialize(0,
			h1, h2, rhash1, rhash2,	// cached hash code info
			hennum1, hennum2, rhen1, rhen2,		// cached hen num info
			hashloc, hword,
			false, false, false, false, false, false,
			v_languageCode, 0, 0, 1,
			word_id, wtc, 0);
			p->CompanyCode(cc);
//printf("\nGWordQuery \"%s\" %d %d %d %d %d %d %d %d %d %d %d %d \"%s\" %d\n", 
//v_inputWord, h1, h2, rhash1, rhash2, hennum1, hennum2,
//rhen1, rhen2, hashloc, hword, word_id, wtc, p->CompanyCode().c_str());
		}
//printf("\nGWordQuery result %d\n", result);
//fflush(stdout);

		return result;
	}

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
              p_headLocation->AsInteger(),
              p_headWord->AsInteger(),
              false, false, false, false, false, false,
              v_languageCode,
              0, 0, 1,
              p_id->AsInteger(),
              p_wordTypeCode->AsIntegerFromString(), 0);
        p->CompanyCode( p_companyCode->AsString() );
/*
printf("\nGWordQuery \"%s\" %d %d %d %d %d %d %d %d %d %d %d %d \"%s\" %d\n", 
	   v_inputWord,
              p_hashCode1->AsInteger(),
              p_hashCode2->AsInteger(),
              p_rootHashCode1->AsInteger(),
              p_rootHashCode2->AsInteger(),
			  p_henNum1->AsInteger(),
			  p_henNum2->AsInteger(),
			  p_rootHenNum1->AsInteger(),
			  p_rootHenNum2->AsInteger(),
              p_headLocation->AsInteger(),
              p_headWord->AsInteger(),
              p_id->AsInteger(),
              p_wordTypeCode->AsIntegerFromString(),
			  p->CompanyCode().c_str());
*/
    }
//printf("\nGWordQuery result %d\n", result);
//fflush(stdout);
    return result;
}
