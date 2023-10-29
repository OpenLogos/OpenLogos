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
// File - constantpointerquery.cpp
//
// Class - ConstantPointerQuery (implementation)
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/gerdem/constantpointerquery.h>
//#include <logos_libs/entity/dwordphrase.h>
#include <logos_libs/sql/sqlstatement.h>
#include <logos_libs/sql/sqlcolumn.h>
#include <logos_libs/sql/sqlexception.h>
#include <logos_libs/linguistic/llanguage.h>
//#include <logos_libs/linguistic/ldictionary.h>
#include <logos_libs/linguistic/targetdictionaryunit.h>
#include <logos_libs/entity/dtransfer.h>

#include <logos_libs/dbcache/CacheConstantPointerData.h>
#include <logos_libs/dbcache/CacheConstantPointerQuery.h>
//----------------------------------------------------------------------
ConstantPointerQuery::ConstantPointerQuery()
                     :v_languageCode(0),
                      v_constantID(0)
{
	ndata = NULL;
	nquery = NULL;
}
//----------------------------------------------------------------------
ConstantPointerQuery::~ConstantPointerQuery()
{
	if(ndata) { delete ndata; ndata = NULL; }
	if(nquery) { delete nquery; nquery = NULL; }

}
//----------------------------------------------------------------------
void ConstantPointerQuery::Open(SqlConnection* aConnection, const LLanguage& language, bool useCC)
{
	v_languageCode = language.id();
   v_useCompanyCode = useCC;
   LgsString s;

	ndata = new CacheConstantPointerData(NULL, v_languageCode, false, false, false);
	if(ndata->isValid()) {
		nquery = new CacheConstantPointerQuery(ndata, NULL, 0, 0);
		return;
	} else {
		delete ndata;
		ndata =NULL;
	}

   if (v_useCompanyCode)
   {

      s = "select Primary_Usage_ID, "
          "       Alternate_Usage_ID, "
          "       Combining_Form_Code "
          " from  Constant_Pointer "
          " where Company_Code  = :aCompanyCode"
          " and   Constant_ID  = :aConstantID"
          " and   Constant_Type = :aConstantType"
          " and   Language_Code = :aLanguageCode";
   }
   else
   {
      s = "select Primary_Usage_ID, "
          "       Alternate_Usage_ID, "
          "       Combining_Form_Code "
          " from  Constant_Pointer "
          " where Constant_ID  = :aConstantID"
          " and   Constant_Type = :aConstantType"
          " and   Language_Code = :aLanguageCode";
   }

   try
   {
      SqlQuery::Open(aConnection);

      Statement()->AddToCommandString(s);

      Statement()->Parse();

      v_languageCode = language.id();

      p_primary_usage_id = Statement()->BindOutputColumn(1, SqlColumn::Integer);
      p_alternate_usage_id = Statement()->BindOutputColumn(2, SqlColumn::Integer);
      p_combiningFormCode = Statement()->BindOutputColumn(3, SqlColumn::StringType);
   }
   catch(SqlException& x)
   {
      cout << x.Message() << endl;
      throw(x);
   }
}
//----------------------------------------------------------------------
void ConstantPointerQuery::Close() {
	if(ndata==NULL)
   SqlQuery::Close();
}
//----------------------------------------------------------------------
void ConstantPointerQuery::ExecuteWithConstantID(int constantID, const LgsString& compCode,
                                                 const LgsString& constantType)
{

   try
   {
		if(ndata) {
			if(v_useCompanyCode) {
				nquery->query((char *)compCode.c_str(), constantID, 
					(char *)constantType.c_str());
				return;
			} else {
				nquery->query(NULL, constantID, (char *)constantType.c_str());
				return;
			}
		}

      if (v_useCompanyCode)
      {
         Statement()->BindInputString(":aCompanyCode", compCode.c_str());
      } else {
	  }
      Statement()->BindInputInteger(":aConstantID", &constantID);
      Statement()->BindInputString (":aConstantType", constantType.c_str());
      Statement()->BindInputIntToString(":aLanguageCode", "%02d", v_inputLanguageCode,
                                        v_languageCode);
      Statement()->Execute();
   }
   catch(SqlException& x)
   {
      cout << x.Message() << endl;
      throw(x);
   }
}
//----------------------------------------------------------------------
bool ConstantPointerQuery::fetchTransfers(TargetDictionaryUnit* pUnit)
{
   bool result = true;

	if(ndata) {
		result = nquery->fetch(&puid, &auid, &cfc);
		if(result) {

			if(puid) {
				Transfer primaryTransfer;
				primaryTransfer.setId(puid);
				primaryTransfer.setCombiningFormCode(cfc);
				pUnit->addTransfer(primaryTransfer);
			}
			if(auid) {
				Transfer alternateTransfer;
				alternateTransfer.setId(auid);
				alternateTransfer.setCombiningFormCode(cfc);
				pUnit->addTransfer(alternateTransfer);
			}
		}
		return result;
	}

   try
   {
      result = Statement()->Fetch();
   }
   catch(SqlException& x)
   {
      cout << x.Message() << endl;
      throw(x);
   }

   if (result == true)
   {
      int primary;
      int alternate;

      primary = p_primary_usage_id->AsInteger();
      alternate = p_alternate_usage_id->AsInteger();
      if (primary)
      {
         Transfer primaryTransfer;
         primaryTransfer.setId(primary);
         primaryTransfer.setCombiningFormCode(p_combiningFormCode->AsIntegerFromString());
         pUnit->addTransfer(primaryTransfer);
      }
      if (alternate)
      {
         Transfer alternateTransfer;
         alternateTransfer.setId(alternate);
         alternateTransfer.setCombiningFormCode(p_combiningFormCode->AsIntegerFromString());
         pUnit->addTransfer(alternateTransfer);
      }
   }
   return result;
}
