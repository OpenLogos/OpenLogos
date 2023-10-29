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
#ifndef _constantcodebuilder_h_
#define _constantcodebuilder_h_

#include <logos_libs/sql/logossql.h>
#include <logos_libs/dbms2restran/transfercode.h>
#include <logos_libs/dbms2restran/genderretriever.h>

class CacheConstantCodeData;
class CacheConstantCodeQuery;

//************************************************************
// Author:              Manoj Agarwala
// History:             11/4/96 - Originally Conceived
//************************************************************

#pragma comment(lib, "wsock32.lib") //becuase of ntohs

class SqlConnection;
class SqlStatement;
class SqlColumn;

class CConstantCodeBuilder
{
public:
        CConstantCodeBuilder(SqlConnection*);
        ~CConstantCodeBuilder();

        bool QueryAndFetch(int constantId,
                           const LgsString& constantType,
                           const LgsString& targetLanguage,
                           const LgsString& companyCd,
                           CTransferCodeVector& transferCodes,
                           bool emptyVector=true);
                           //Query returns true if 1 or more matching rules are
                           //retrieved, returns false otherwise.
                           //All the exiting items are deleted from semtab rules vector.
                           //The results are put into semtabRules vector.
                           //If emptyVector is true, vector is emptied before it is populated

        //Either the query can be done in one step using QueryAndFetch
        //or in 2 step using Query and Fetch
        void Query(int constantId,
                           const LgsString& constantType,
                           const LgsString& targetLanguage,
                           const LgsString& companyCd);

        //Fetch is valid only after Query.
        //maxItems is used to specify the maximun number of items to be Fetched
        //It returns the number of items Fetched. If maxItems=0, implies fetch all
        //If returnValue is less than maxItems, it implies there are no more items
        //If emptyVector is true, vector is emptied before it is populated
        int  Fetch(CTransferCodeVector& transferCodes,
                int maxItems=0,
                bool emptyVector=true);

//Private member functions
private:
        void                    ConnectAndCreateSqlStatement();
        SqlStatement*   getStatement(){return m_pSqlStatement;};

//Private member varaibles
private:
        LgsString                  m_TargetLanguageCode;

        SqlConnection*  m_pSqlConnection;
        SqlStatement*   m_pSqlStatement;

        SqlColumn*              m_pCompanyCodeSqlColumns;
        SqlColumn*              m_pAltUsageSqlColumns;
        SqlColumn*              m_pWordClassSqlColumns;
        SqlColumn*              m_pPatNumberSqlColumns;
        SqlColumn*              m_pAlternatePatNumberSqlColumns;
        SqlColumn*              m_pGenderCodeSqlColumns;
        SqlColumn*              m_pAlternateGenderCodeSqlColumns;
        SqlColumn*              m_pNumericConstraintSqlColumns;

        GenderRetriever  m_genderRetriever; //Used to retrieve the alternate gender

		CacheConstantCodeData *hdata[7];
		CacheConstantCodeQuery *hquery[7];
		int langid;
		char cc[4];
		int auid;
		char wcc[2];
		int pn1;
		int gc1;
		char nc[2];
		int pn2;
		int gc2;
};


#endif

