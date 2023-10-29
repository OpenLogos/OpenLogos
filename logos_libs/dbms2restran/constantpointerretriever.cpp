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
#include <logos_include/logoscommon.h>

#include <logos_libs/dbms2restran/constantpointerretriever.h>

#include <logos_libs/sql/logossql.h>
#include /**/ <memory.h>
#include /**/ <stdio.h>

//************************************************************
// Author:              Manoj Agarwala
// History:             11/4/96 - Originally Conceived
//************************************************************

CConstantPointerRetriever::CConstantPointerRetriever(SqlConnection *pConnection)
: m_pSqlConnection(pConnection),
  m_pSqlStatement(NULL)
{
}

CConstantPointerRetriever::~CConstantPointerRetriever()
{
        if( m_pSqlStatement )
                delete m_pSqlStatement;
}

bool CConstantPointerRetriever::QueryAndFetch(
        int constantId,
        const LgsString& constantType,
        const LgsString& targetLanguage,
        const LgsString& companyCd,
        DConstantPointer& constantPointer)
{
        ConnectAndCreateSqlStatement();

        getStatement()->BindInputString( ":aCC", companyCd.c_str() );
        getStatement()->BindInputInteger( ":aCI", &constantId );
        getStatement()->BindInputString( ":aCT", constantType.c_str() );
        getStatement()->BindInputString( ":aTLC", targetLanguage.c_str() );

    getStatement()->Execute();


        if (!getStatement()->Fetch() )
                return false;

        int nLanguageCode;

        //+1 to ignore the first 0 as in 01
        sscanf(targetLanguage.c_str()+1, "%d", &nLanguageCode);

        constantPointer.SetConstantType(constantType);
        constantPointer.SetConstantID(constantId);
        constantPointer.SetLanguageCode(nLanguageCode);
        constantPointer.SetCompanyCode(companyCd);
        constantPointer.SetCombiningFormCode(m_pCombiningFormCodeColumn->AsIntegerFromString());
        constantPointer.SetPrimaryUsageID(m_pPrimaryUsageIdColumn->AsInteger());
        constantPointer.SetAlternateUsageID(m_pAlternateUsageIdColumn->AsInteger());

        return true;
}


void CConstantPointerRetriever::ConnectAndCreateSqlStatement()
{
        if( !m_pSqlStatement)
        {
        m_pSqlStatement = m_pSqlConnection->CreateStatement();

        LgsString s = "select "
                                                "  Combining_Form_Code, "
                                                "  Primary_Usage_ID, "
                                                "  Alternate_Usage_ID "
                   " from  Constant_Pointer "
                                   " where "
                                   "       Company_Code = :aCC "
                                   " and   Constant_Type = :aCT "
                                   " and   Constant_ID = :aCI "
                                   " and   Language_Code = :aTLC ";

                m_pSqlStatement->AddToCommandString( s );

        m_pSqlStatement->Parse();

                m_pCombiningFormCodeColumn = m_pSqlStatement->BindOutputColumn(1, SqlColumn::StringType );
                m_pPrimaryUsageIdColumn = m_pSqlStatement->BindOutputColumn(2, SqlColumn::Integer );
                m_pAlternateUsageIdColumn = m_pSqlStatement->BindOutputColumn(3, SqlColumn::Integer );

        }//End Of If
}

