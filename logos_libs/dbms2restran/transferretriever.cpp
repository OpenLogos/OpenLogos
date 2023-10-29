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
#include <logos_libs/dbms2restran/transferretriever.h>

#include <logos_libs/sql/logossql.h>


CTransferRetriever::CTransferRetriever(SqlConnection* pConnection)
: m_pSqlConnection(pConnection),
  m_pSqlStatement(NULL)
{
}

CTransferRetriever::~CTransferRetriever()
{
        if( m_pSqlStatement )
                delete m_pSqlStatement;
}

int CTransferRetriever::QueryAndFetch(
        int meaningId,
        const LgsString& targetLanguage,
        const LgsString& companyCd,
        DTransferVector &transferVector)
{
        ConnectAndCreateSqlStatement();

        getStatement()->BindInputString( ":aCC", companyCd.c_str() );
        getStatement()->BindInputInteger( ":aMI", &meaningId );
        getStatement()->BindInputString( ":aTLC", targetLanguage.c_str() );

    getStatement()->Execute();

        int itemsFetched = 0;

        //Fetch all the rows
        while( getStatement()->Fetch() )
        {
                DTransfer  transfer;

                transfer.SetCompanyCode            ( companyCd );
                transfer.SetTargetLanguageCode     ( targetLanguage );
                transfer.SetMeaningID              ( meaningId );

                transfer.SetTargetCountryCode      ( m_pTargetCountryCodeColumn->AsInteger());
                transfer.SetTargetUsageID          ( m_pTargetUsageIdColumn->AsInteger());
                transfer.SetAlternateSequence      ( m_pAlternateSequenceColumn->AsInteger());
                transfer.SetCaseGovernanceCode     ( m_pCaseGovernanceCodeColumn->AsInteger());
                transfer.SetCombiningFormCode      ( m_pCombiningFormCodeColumn->AsIntegerFromString());
                transfer.SetIndividualizationCode  ( m_pIndividualizationCodeColumn->AsInteger());
                transfer.SetAdjectivePlacementCode ( m_pAdjectivePlacementCodeColumn->AsInteger());
                transfer.SetAdverbPlacementCode    ( m_pAdverbPlacementCodeColumn->AsInteger());
                transfer.SetReflexCode             ( m_pReflexCodeColumn->AsInteger());

                itemsFetched++;

                transferVector.push_back(transfer);
        }

        return itemsFetched;
}

void CTransferRetriever::ConnectAndCreateSqlStatement()
{
        if( !m_pSqlStatement)
        {
        m_pSqlStatement = m_pSqlConnection->CreateStatement();

        LgsString s = "select "
                                                "  Target_Country_Code, "
                                                "  Target_Usage_ID, "
                                                "  Alternate_Sequence, "
                                                "  Case_Governance_Code, "
                                                "  Combining_Form_Code, "
                                                "  Individualization_Code, "
                                                "  Adjective_Placement_Code, "
                                                "  Adverb_Placement_Code, "
                                                "  Reflex_Code "
                  " from  Transfer "
                                   " where "
                                   "       Meaning_ID = :aMI "
                                   " and   Company_Code = :aCC "
                                   " and   Target_Language_Code = :aTLC "
                                   " and   (    Alternate_Sequence is NULL "
                                   "         or Alternate_Sequence = 0 ) "
                                   " and   Deactivation_Switch = 'N' ";

                m_pSqlStatement->AddToCommandString( s );

        m_pSqlStatement->Parse();

                m_pTargetCountryCodeColumn      = m_pSqlStatement->BindOutputColumn(1, SqlColumn::StringType );
                m_pTargetUsageIdColumn          = m_pSqlStatement->BindOutputColumn(2, SqlColumn::Integer );
                m_pAlternateSequenceColumn      = m_pSqlStatement->BindOutputColumn(3, SqlColumn::Integer );
                m_pCaseGovernanceCodeColumn     = m_pSqlStatement->BindOutputColumn(4, SqlColumn::StringType );
                m_pCombiningFormCodeColumn      = m_pSqlStatement->BindOutputColumn(5, SqlColumn::StringType );
                m_pIndividualizationCodeColumn  = m_pSqlStatement->BindOutputColumn(6, SqlColumn::StringType );
                m_pAdjectivePlacementCodeColumn = m_pSqlStatement->BindOutputColumn(7, SqlColumn::StringType );
                m_pAdverbPlacementCodeColumn    = m_pSqlStatement->BindOutputColumn(8, SqlColumn::StringType );
                m_pReflexCodeColumn             = m_pSqlStatement->BindOutputColumn(9, SqlColumn::StringType );

        }//End Of If
}

