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
// File - tranfercountquery.cpp
//
// Class - TransferCountQuery (implementation)
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/gerdem/transfercountquery.h>
#include <logos_libs/sql/sqlstatement.h>
#include <logos_libs/sql/sqlcolumn.h>
#include <logos_libs/sql/sqlexception.h>

#include <logos_libs/dbcache/CacheTransferCountData.h>
#include <logos_libs/dbcache/CacheTransferCountQuery.h>

//----------------------------------------------------------------------
TransferCountQuery::TransferCountQuery()
{
	edata = NULL;
	equery = NULL;
}
//----------------------------------------------------------------------
TransferCountQuery::~TransferCountQuery()
{
	if (edata)
   {
      delete edata;
      edata = NULL;
   }
	if (equery)
   {
      delete equery;
      equery = NULL;
   }
}
//----------------------------------------------------------------------
void TransferCountQuery::Open(SqlConnection* aConnection, int targetLanguageCode)
{
   v_targetCode = targetLanguageCode;

   edata = new CacheTransferCountData(NULL, targetLanguageCode, false, false, false);
   if (edata->isValid())
   {
      equery = new CacheTransferCountQuery(edata, NULL, 0);
      return;
   }
   else
   {
      delete edata;
      edata = NULL;
      equery = NULL;
   }

   LgsString s = " select count(*)"
                 " from Transfer"
                 " where Meaning_ID = :aMeaningID"
                 " and Company_Code = :aCompanyCode"
                 " and Target_Language_Code = :aTarget";

   try
   {
      SqlQuery::Open(aConnection);
      Statement()->AddToCommandString(s);
      Statement()->Parse();
      p_count = Statement()->BindOutputColumn(1, SqlColumn::Integer);
   }
   catch(SqlException& x)
   {
      cout << x.Message() << endl;
      throw(x);
   }
}
//----------------------------------------------------------------------
void TransferCountQuery::Close()
{
   if (edata==NULL)
      SqlQuery::Close();
}
//----------------------------------------------------------------------
int TransferCountQuery::TransferCount(int aMeaningID, const LgsString& aCompanyCode)
{
    bool result = false;

//ocr
	if (edata)
   {
		int ret = equery->query((char *)aCompanyCode.c_str(), aMeaningID);
//printf("\nCacheTransferCountQuery: \"%s\" id=%d ret=%d\n", (char *)aCompanyCode.c_str(),
//	   aMeaningID, ret);
		return ret;
	}

    try
    {
        Statement()->BindInputIntToString(":aTarget", "%02d", v_inputTargetCode, v_targetCode);
        Statement()->BindInputInteger(":aMeaningID", &aMeaningID);
        Statement()->BindInputString(":aCompanyCode", aCompanyCode.c_str());

        Statement()->Execute();
        Statement()->Fetch();
//ocr
//printf("\nCacheTransferCountQuery: \"%s\" id=%d ret=%d\n", (char *)aCompanyCode.c_str(),
//	   aMeaningID, p_count->AsInteger());
    }
    catch(SqlException& x)
    {
        cout << x.Message() << endl;
        throw(x);
    }
    return p_count->AsInteger();
}
