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
// File - transferquery.cpp
//
// Class - TransferQuery (implementation)
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/gerdem/transferquery.h>
#include <logos_libs/linguistic/transfer.h>
#include <logos_libs/sql/sqlstatement.h>
#include <logos_libs/sql/sqlcolumn.h>
#include <logos_libs/sql/sqlexception.h>
#include <logos_libs/linguistic/llanguage.h>

#include <logos_libs/dbcache/CacheTransferData.h>
#include <logos_libs/dbcache/CacheTransferQuery.h>

//----------------------------------------------------------------------
TransferQuery::TransferQuery()
{
	kdata = NULL;
	kquery = NULL;
}
//----------------------------------------------------------------------
TransferQuery::~TransferQuery()
{
		if(kdata) { delete kdata; kdata = NULL; }
		if(kquery) { delete kquery; kquery = NULL; }
}
//----------------------------------------------------------------------
void TransferQuery::Open( SqlConnection* aConnection,
                          const LLanguage& language )
{
	v_languageCode = language.id();

	kdata = new CacheTransferData(NULL, v_languageCode, false, false, false);
	if(kdata->isValid()) {
		kquery = new CacheTransferQuery(kdata, NULL, 0);
		return;
	} else {
		delete kdata;
		kdata = NULL;
	}


        LgsString s = "select "
                                       "  Target_Usage_ID, "
                                       "  Combining_Form_Code, "
                                       "  Alternate_Sequence "
                       " from  Transfer "
                                       " where "
                                       "       Meaning_ID = :aMeaningID"
                                       " and   Company_Code = :aCompany"
                                       " and   Target_Language_Code = :aLanguageCode"
                                       " and   Deactivation_Switch = 'N' ";

    try
    {
        SqlQuery::Open( aConnection );

        Statement()->AddToCommandString( s );

        Statement()->Parse();

        v_languageCode = language.id();

        p_usageID           = Statement()->BindOutputColumn( 1, SqlColumn::Integer );
        p_combiningFormCode = Statement()->BindOutputColumn( 2, SqlColumn::StringType );
        p_alternateSequence = Statement()->BindOutputColumn( 3, SqlColumn::Integer );
    }
    catch( SqlException& x )
    {
        cout << x.Message() << endl;
        throw( x );
    }
}
//----------------------------------------------------------------------
void TransferQuery::Close() {
	if(kdata==NULL)
    SqlQuery::Close();

}
//----------------------------------------------------------------------
void TransferQuery::executeWithMeaningID( int meaningID, const LgsString & company )
{
	sprintf(v_companyCode, "%s", company.c_str());

	mid = meaningID;
//printf("CacheTransferQuery: mid=%d\n", mid);
//fflush(stdout);
//ocr
	if(kdata) {
		kquery->query(v_companyCode, meaningID);
		return;
	}

    try
    {
        Statement()->BindInputIntToString( ":aLanguageCode",
                                           "%02d",
                                           v_inputLanguageCode,
                                           v_languageCode );
        Statement()->BindInputIntToString( ":aMeaningID",
                                           "%d",
                                           v_inputMeaningID,
                                           meaningID );
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
TransferQuery::fetchTransfer( Transfer* pTransfer )
{
    //------------------------------------------------------------------
    //------------------------------------------------------------------

    bool result = true;

	if(kdata) {
		result = kquery->fetch(&tud, &cfc, &as);
        pTransfer->setId                (tud);
        pTransfer->setCombiningFormCode (cfc);
//printf("CacheTransferQuery: %d \"%s\" %d: %d %d %d\n", result, v_companyCode, mid, tud, cfc, as);
//fflush(stdout);
		pTransfer->setSequenceNumber    (as);
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
//printf("CacheTransferQuery: %d \"%s\" %d: %d %d %d\n", result, v_companyCode, mid, 
//p_usageID->AsInteger(), p_combiningFormCode->AsIntegerFromString(),
//p_alternateSequence->AsInteger());
//fflush(stdout);
    if( result == true )
    {
            pTransfer->setId                (p_usageID->AsInteger());
        pTransfer->setCombiningFormCode ( p_combiningFormCode->AsIntegerFromString());
        pTransfer->setSequenceNumber    (p_alternateSequence->AsInteger());
    }
    return result;
}
