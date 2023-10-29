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
// File - TargetQuery.cpp
//
// Class - TargetQuery (implementation)
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/gerdem/irregularstemquery.h>
#include <logos_libs/sql/sqlstatement.h>
#include <logos_libs/sql/sqlcolumn.h>
#include <logos_libs/sql/sqlexception.h>
#include <logos_libs/linguistic/llanguage.h>
#include <logos_libs/linguistic/lsemantosyntacticunit.h>

#include <logos_libs/dbcache/CacheIrregularStemData.h>
#include <logos_libs/dbcache/CacheIrregularStemQuery.h>

//----------------------------------------------------------------------
IrregularStemQuery::IrregularStemQuery()
{
	idata = NULL;
	iquery = NULL;
}
//----------------------------------------------------------------------
IrregularStemQuery::~IrregularStemQuery()
{
	if(idata) { delete idata; idata = NULL; }
	if(iquery) { delete iquery; iquery = NULL; }
}
//----------------------------------------------------------------------
void IrregularStemQuery::Open(SqlConnection* aConnection, const LLanguage& language)
{
	idata = new CacheIrregularStemData(NULL, 0, false, false, false);
	if(idata->isValid()) {
		iquery = new CacheIrregularStemQuery(idata, NULL, 0);
		return;
	} else {
		delete idata;
		idata = NULL;
	}

	LgsString s = "select "
              "i.Stem_Word "
              "from  Irregular_Stem i "
              "where i.Usage_ID = :aUsageID "
              "and   i.Company_Code = :aCompanyCode ";
   try
   {
      SqlQuery::Open(aConnection);

      Statement()->AddToCommandString(s);
      Statement()->Parse();

      m_pWord = Statement()->BindOutputColumn(1, SqlColumn::StringType);
   }
   catch(SqlException& x)
   {
      cout << x.Message() << endl;
      throw(x);
   }
}
//----------------------------------------------------------------------
void IrregularStemQuery::Close()
{
	if(idata==NULL)
		SqlQuery::Close();
}
//----------------------------------------------------------------------
void IrregularStemQuery::executeWithUsageID(int usageID, const LgsString& companyCode)
{

	char inputUsageID[10];
	
	if(idata) {
		iquery->query((char *)companyCode.c_str(), usageID);
		return;
	}

	try
   {
      Statement()->BindInputIntToString(":aUsageID", "%d", inputUsageID, usageID);
      Statement()->BindInputString(":aCompanyCode", companyCode.c_str());
      Statement()->Execute();
   }
   catch(SqlException& x)
   {
      cout << x.Message() << endl;
      throw(x);
   }
}
//----------------------------------------------------------------------
bool IrregularStemQuery::fetch(LSemantoSyntacticUnit* targetSSU)
{
   bool result = true;

	if(idata) {
		result = iquery->fetch(stem_word);
		if(result) {
			LgsString s(stem_word);
			targetSSU->setWord(s);
		}
		return result;
	}

   try
   {
      result = Statement()->Fetch();
      if (result)
      {
         targetSSU->setWord(m_pWord->AsString());
      }
   }

   catch(SqlException& x)
   {
      cout << x.Message() << endl;
      throw(x);
   }
   return result;
}
//----------------------------------------------------------------------
bool IrregularStemQuery::isIrregulalrStemPat(int patNumber)
{
   if ((patNumber == 162) || (patNumber == 184) ||
       ((patNumber >= 215) && (patNumber <= 228)) ||
       ((patNumber >= 618) && (patNumber <= 629)))
   {
      return true;
   }
   return false;
}
