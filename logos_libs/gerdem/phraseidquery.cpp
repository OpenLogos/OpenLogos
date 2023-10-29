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
// File - PhraseIdQuery.cpp
//
// Class - PhraseIdQuery (implementation)
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/gerdem/phraseidquery.h>
#include <logos_libs/entity/dwordphrase.h>
#include <logos_libs/sql/sqlstatement.h>
#include <logos_libs/sql/sqlcolumn.h>
#include <logos_libs/sql/sqlexception.h>
#include <logos_libs/linguistic/llanguage.h>

#include <logos_libs/dbcache/CacheGwordQuery.h>
#include <logos_libs/dbcache/CacheGphraseData.h>

#include <stdio.h>

//----------------------------------------------------------------------
PhraseIdQuery::PhraseIdQuery()
    : v_languageCode( 0 )
{
    v_inputLanguageCode[0] = 0;
    //v_inputWord[0] = 0;
	sprintf(v_companyCode, "%s", "LOG");
	bdata = NULL;
	gquery = NULL;
}
//----------------------------------------------------------------------
PhraseIdQuery::~PhraseIdQuery() {
	if(bdata) { delete bdata; bdata = NULL; }
	if(gquery) { delete gquery; gquery = NULL; }
}
//----------------------------------------------------------------------
void PhraseIdQuery::Open( SqlConnection* aConnection,
                        const LLanguage& language )
{
	v_languageCode = language.id();
/*
	bdata = new CacheGphraseData(NULL, v_languageCode, false, false, false);
	if(bdata->isValid()) {
		gquery = new CacheGwordQuery(bdata, NULL);
		return;
	} else {
		delete bdata;
		bdata = NULL;
		gquery = NULL;
	}
*/
   LgsString s = "select"
              " Word_ID,"
              " Word_Count,"
              " Hash_Code_1, Hash_Code_2,"
			  " Root_Hash_1, Root_Hash_2,"
			  " Hen_Num_1, Hen_Num_2,"
			  " Root_Hen_1, Root_Hen_2,"
              " Hash_Location,"
              " Head_Word, Company_Code"
              " from Word_Phrase"
              " where Word_ID = :aWordId"
              " and Language_Code = :aCode"
			  " and Company_Code = :aCompany";

    try
    {
        SqlQuery::Open( aConnection );

        Statement()->AddToCommandString( s );

        Statement()->Parse();

        v_languageCode = language.id();

        p_id				= Statement()->BindOutputColumn( 1, SqlColumn::Integer );
        p_wordCount			= Statement()->BindOutputColumn( 2, SqlColumn::Integer );
        p_hashCode1			= Statement()->BindOutputColumn( 3, SqlColumn::Integer );
        p_hashCode2			= Statement()->BindOutputColumn( 4, SqlColumn::Integer );
        p_rootHashCode1		= Statement()->BindOutputColumn( 5, SqlColumn::Integer );
        p_rootHashCode2		= Statement()->BindOutputColumn( 6, SqlColumn::Integer );
        p_henNum1			= Statement()->BindOutputColumn( 7, SqlColumn::Integer );
        p_henNum2			= Statement()->BindOutputColumn( 8, SqlColumn::Integer );
        p_rootHenNum1		= Statement()->BindOutputColumn( 9, SqlColumn::Integer );
        p_rootHenNum2		= Statement()->BindOutputColumn(10, SqlColumn::Integer );
        p_headLocation		= Statement()->BindOutputColumn(11, SqlColumn::Integer );
        p_headWord			= Statement()->BindOutputColumn(12, SqlColumn::Integer );
		p_companyCode		= Statement()->BindOutputColumn(13, SqlColumn::StringType );
    }
    catch( SqlException& x )
    {
        cout << x.Message() << endl;
        throw( x );
    }
}
//----------------------------------------------------------------------
void PhraseIdQuery::Close() {
	if(bdata==NULL)
    SqlQuery::Close();

}
//----------------------------------------------------------------------
void PhraseIdQuery::SetCompanyCode(const LgsString & companyCode)
{
    sprintf( v_companyCode, "%s", companyCode.c_str() );
}
//----------------------------------------------------------------------
void
PhraseIdQuery::ExecuteWithMatch (int wordId)
{
    //sprintf( v_inputWord, "%s", firstWord.c_str() );

	/*if(bdata) {
		int ret = gquery->query(v_inputWord, v_companyCode);
		return;
	}*/

    try
    {
        Statement()->BindInputIntToString( ":aCode",
                                           "%02d",
                                           v_inputLanguageCode,
                                           v_languageCode );
        Statement()->BindInputInteger( ":aWordId", &wordId );

		Statement()->BindInputString(":aCompany", v_companyCode);

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
PhraseIdQuery::FetchIntoWordMatch( DWordPhrase* p )
{
    bool result = true;

	/*int word_id, h1, h2, rhash1, rhash2, hennum1, hennum2, rhen1, rhen2,
		hword, hashloc, wtc, word_count;
	char cc[20];
	if(bdata) {
		result = gquery->fetch(&word_id, 
			&word_count, 
			&h1, &h2, &rhash1, &rhash2, &hennum1, &hennum2, &rhen1, &rhen2,
			&hword, &hashloc, &wtc, cc);
		if(result) {
			p->Initialize(0, 
			h1, h2, rhash1, rhash2,	// cached hash code info
			hennum1, hennum2, rhen1, rhen2,		// cached hen num info
//			h1, h2, 0, 0,	// cached hash code info
//			0, 0, 0, 0,		// cached hen num info
			hashloc, hword, 
			false, false, false, false, false, false,
			2, 0, 0,
			word_count,
			word_id, 0, 0);
			p->CompanyCode(cc);
//printf("OCR G-querying \"%s\" %d %d %d %d %d %d \"%s\" %d\n", 
//v_inputWord, h1, h2, hashloc, hword, word_id, wtc, p->CompanyCode().c_str());
		}

		return result;
	}*/

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
		p->Initialize (0,
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
                       false,
                       false,
                       false,
                       false,
                       false,
                       false,
                       2,
                       0,
                       0,
                       p_wordCount->AsInteger(),
                       p_id->AsInteger(),
                       0, 0);
		p->CompanyCode( p_companyCode->AsString() );
    }
    return result;
}
