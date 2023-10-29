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
// File - WordInPhraseQuery.cpp
//
// Class - WordInPhraseQuery (implementation)
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/gerdem/wordinphrasequery.h>
#include <logos_libs/entity/dwordphrase.h>
#include <logos_libs/sql/sqlstatement.h>
#include <logos_libs/sql/sqlcolumn.h>
#include <logos_libs/sql/sqlexception.h>
#include <logos_libs/linguistic/llanguage.h>

#include <logos_libs/dbcache/CacheWordInPhraseData.h>
#include <logos_libs/dbcache/CacheWordInPhraseQuery.h>

//----------------------------------------------------------------------
WordInPhraseQuery::WordInPhraseQuery()
{
	udata = NULL;
	uquery = NULL;
}
//----------------------------------------------------------------------
WordInPhraseQuery::~WordInPhraseQuery()
{
	if(udata) { delete udata; udata = NULL; }
	if(uquery) { delete uquery; uquery = NULL; }
}
//----------------------------------------------------------------------
void WordInPhraseQuery::Open (SqlConnection* aConnection)
{
	udata = new CacheWordInPhraseData(NULL, 0, false, false, false);
	if(udata->isValid()) {
		uquery = new CacheWordInPhraseQuery(udata, 0, 0);
		return;
	} else {
		delete udata;
		udata = NULL;
	}

    
    LgsString s = "select"
               " m.Pat_Number,"
               " m.Word_Class_Code"
               " from Word_In_Phrase w, Morphology m"
               " where"
               " w.Word_ID = :aWordID"
               " and"
               " w.Word_Location = :aLocation"
               " and"
               " w.Usage_ID = m.Usage_ID"
               " and"
               " w.Company_Code = m.Company_Code";

    try
    {
        SqlQuery::Open( aConnection );

        Statement()->AddToCommandString( s );

        Statement()->Parse();

        p_patNumber     = Statement()->BindOutputColumn (1, SqlColumn::Integer);
        p_wordClassCode = Statement()->BindOutputColumn (2, SqlColumn::StringType);
    }
    catch( SqlException& x )
    {
        cout << x.Message() << endl;
        throw( x );
    }
}
//----------------------------------------------------------------------
void WordInPhraseQuery::Close()
{
	if(udata==NULL)
    SqlQuery::Close();

}
//----------------------------------------------------------------------
void WordInPhraseQuery::ExecuteWithWord (int wordID, int location)
{
    v_wordID   = wordID;
    v_location = location;

	if(udata) {
		uquery->query(wordID, location);
		return;
	}
    
    try
    {
        Statement()->BindInputInteger (":aWordID",  &v_wordID);
        Statement()->BindInputInteger (":aLocation",  &v_location);

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
WordInPhraseQuery::FetchIntoPatNumber (int *pPatNumber,
                                       int *pWordClassCode)
{
    //------------------------------------------------------------------
    //------------------------------------------------------------------

    bool result = true;

 	if(udata) {
		result = uquery->fetch(pPatNumber, pWordClassCode);
//printf("OCR G-querying \"%s\" %d %d %d %d %d %d \"%s\" %d\n", 
//v_inputWord, h1, h2, hashloc, hword, word_id, wtc, p->CompanyCode().c_str());

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
        *pPatNumber     = p_patNumber->AsInteger ();
        *pWordClassCode = p_wordClassCode->AsIntegerFromString ();
    }
    return result;
}
