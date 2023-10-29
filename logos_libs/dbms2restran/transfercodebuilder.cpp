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

#if defined(_WINDOWS) || defined(WIN32)
        #include /**/ <winsock.h> //for ntohs - on Unix it will be different,
                                                 //we will have to enclose it in ifdef
#else //Else part is for UNIX and is not tested
        #include /**/ <sys/types.h>
        #include /**/ <netinet/in.h>
#endif

#include <logos_libs/dbms2restran/transfercodebuilder.h>

#include <logos_libs/dbcache/CacheTransferCodeData.h>
#include <logos_libs/dbcache/CacheTransferCodeQuery.h>

#include <logos_libs/sql/logossql.h>
#include /**/ <memory.h>

//************************************************************
// Author:              Manoj Agarwala
// History:             11/1/96 - Originally Conceived
//************************************************************

CTransferCodeBuilder::CTransferCodeBuilder(SqlConnection *pConnection)
                     :m_pSqlConnection(pConnection),
                      m_pSqlStatement(NULL)
{
	for (int i = 1; i < 7; i++)
   {
		cdata[i] = new CacheTransferCodeData(NULL, i, false, false, false);
		if (cdata[i]->isValid())
      {
			cquery[i] = new CacheTransferCodeQuery(cdata[i], NULL, 0);
		}
      else
      {
			delete cdata[i];
			cdata[i] = NULL;
			cquery[i] = NULL;
      }
	}
	curCacheTransferCodeQuery = NULL;
}

CTransferCodeBuilder::~CTransferCodeBuilder()
{
   if (m_pSqlStatement)
      delete m_pSqlStatement;
	for (int i = 1; i < 7; i++)
   {
		if (cdata[i])
      {
         delete cdata[i];
         cdata[i] = NULL;
      }
		if (cquery[i])
      {
         delete cquery[i];
         cquery[i] = NULL;
      }
   }
}

bool CTransferCodeBuilder::QueryAndFetch(
        int meaningId,
        const LgsString& targetLanguage,
        const LgsString& companyCd,
        CTransferCodeVector& transfercodes,
        bool  emptyVector)
{
        Query(meaningId, targetLanguage, companyCd);
        return Fetch(transfercodes, 0, emptyVector); //0 means fetch all item
}

void CTransferCodeBuilder::Query(
        int meaningId,
        const LgsString& targetLanguage,
        const LgsString& companyCd)
{
	int lid = atoi(targetLanguage.c_str());
//printf("\ntcb.query: mid=%d, cc=\"%s\"\n", meaningId, companyCd.c_str());
//fflush(stdout);
	if(lid>0 && lid < 7) {
		if(cdata[lid]) {
			curCacheTransferCodeQuery = cquery[lid];
			curCacheTransferCodeQuery->query((char *)companyCd.c_str(), meaningId);
			return;
		}
	}

    ConnectAndCreateSqlStatement();

    getStatement()->BindInputString( ":aCC", companyCd.c_str() );
    getStatement()->BindInputInteger( ":aMI", &meaningId );
    getStatement()->BindInputString( ":aTLC", targetLanguage.c_str() );

    getStatement()->Execute();
}

int CTransferCodeBuilder::Fetch(
CTransferCodeVector& transfercodes,
int   maxItems,
bool  emptyVector
)
{
    if ( emptyVector )
            transfercodes.erase(transfercodes.begin(), transfercodes.end());

    int itemsFetched = 0;
    //Fetch all the rows
	if(curCacheTransferCodeQuery) {
		char cc[4];
		int as, o2a, o2b, o3a, o3b, wcc, pn, gc;
	    while((maxItems == 0 //0 means all the items
			|| itemsFetched < maxItems) && 
			curCacheTransferCodeQuery->fetch(cc, &as, &o2a, &o2b, &o3a, &o3b, &wcc, &pn, &gc)) {
	        itemsFetched++;

			CTransferCode tc;

			tc.setCompanyCode(cc);
			tc.setAlternateSeq(as);
			tc.setWordClassCode(wcc);
			tc.setPatNumber(pn);
			tc.setGenderCode(gc);
			tc.setOverflow2a(o2a);
			tc.setOverflow2b(o2b);
			tc.setOverflow3a(o3a);
			tc.setOverflow3b(o3b);
//printf("\ntcb.fetch: %d %d %d %d %d %d\n",
//			tc.getWordClassCode(),
//			tc.getPatNumber(),
//			tc.getGenderCode(),
//			tc.getOverflow2a(),
//			tc.getOverflow2b(),
//			tc.getOverflow3a(),
//			tc.getOverflow3b());
//fflush(stdout);

			transfercodes.push_back(tc);
		}

		return itemsFetched;
	}

    while(    (    maxItems == 0 //0 means all the items
                    || itemsFetched < maxItems
                      )
               && getStatement()->Fetch()
             )
    {
        itemsFetched++;

        CTransferCode tc;

        tc.setCompanyCode( m_pCompanyCodeSqlColumns->AsString());
        tc.setAlternateSeq( m_pAltSeqSqlColumns->AsInteger());
        tc.setWordClassCode( m_pWordClassSqlColumns->AsIntegerFromString());
        tc.setPatNumber( m_pPatNumberSqlColumns->AsInteger());
        tc.setGenderCode( m_pGenderCodeSqlColumns->AsIntegerFromString());
        tc.setOverflow2a( m_pOverflow2aSqlColumns->AsInteger());
        tc.setOverflow2b( m_pOverflow2bSqlColumns->AsInteger());
        tc.setOverflow3a( m_pOverflow3aSqlColumns->AsInteger());
        tc.setOverflow3b( m_pOverflow3bSqlColumns->AsInteger());

//printf("\ntcb.fetch: %d %d %d %d %d %d\n",
//			tc.getWordClassCode(),
//			tc.getPatNumber(),
//			tc.getGenderCode(),
//			tc.getOverflow2a(),
//			tc.getOverflow2b(),
//			tc.getOverflow3a(),
//			tc.getOverflow3b());
//fflush(stdout);

        transfercodes.push_back(tc);
    }

    return itemsFetched;
}


void CTransferCodeBuilder::ConnectAndCreateSqlStatement()
{
    if( !m_pSqlStatement)
    {
    m_pSqlStatement = m_pSqlConnection->CreateStatement();

    LgsString s = "select "
                        "  a.Company_Code, "
                        "  a.Alternate_Sequence, "
                        "  b.Overflow2a, "
                        "  b.Overflow2b, "
                        "  b.Overflow3a, "
                        "  b.Overflow3b, "
                        "  c.Word_Class_Code, "
                        "  c.Pat_Number, "
                        "  c.Gender_Code "
#ifdef USE_ORACLE8
               " from  Transfer a, "
               "       Overflow b, "
               "       Morphology c "
               " where "
                       "       a.Meaning_ID = :aMI "
                       " and   a.Company_Code = :aCC "
                       " and   a.Target_Language_Code = :aTLC "
                       " and   a.Deactivation_Switch = 'N' "
					   " and   ( a.ALTERNATE_SEQUENCE < 2 "
							  " or a.ALTERNATE_SEQUENCE is NULL ) "
                     " and   b.Company_Code (+) = a.Company_Code "
                     " and   b.Transfer_ID (+) = a.Transfer_ID "
                       " and   c.Company_Code = a.Company_Code "
                       " and   c.Usage_ID = a.Target_Usage_ID ";
#else
    // Postgres OUTER JOIN
               " from  Transfer a LEFT OUTER JOIN Overflow b ON b.Company_Code = a.Company_Code AND b.Transfer_ID = a.Transfer_ID, "
               "       Morphology c "
               " where "
                       "       a.Meaning_ID = :aMI "
                       " and   a.Company_Code = :aCC "
                       " and   a.Target_Language_Code = :aTLC "
                       " and   a.Deactivation_Switch = 'N' "
					   " and   ( a.ALTERNATE_SEQUENCE < 2 "
							  " or a.ALTERNATE_SEQUENCE is NULL ) "
                       " and   c.Company_Code = a.Company_Code "
                       " and   c.Usage_ID = a.Target_Usage_ID ";
#endif

    m_pSqlStatement->AddToCommandString( s );

    m_pSqlStatement->Parse();

    m_pCompanyCodeSqlColumns = m_pSqlStatement->BindOutputColumn(1, SqlColumn::StringType );
    m_pAltSeqSqlColumns = m_pSqlStatement->BindOutputColumn(2, SqlColumn::Integer );
    m_pOverflow2aSqlColumns = m_pSqlStatement->BindOutputColumn(3, SqlColumn::Integer );
    m_pOverflow2bSqlColumns = m_pSqlStatement->BindOutputColumn(4, SqlColumn::Integer );
    m_pOverflow3aSqlColumns = m_pSqlStatement->BindOutputColumn(5, SqlColumn::Integer );
    m_pOverflow3bSqlColumns = m_pSqlStatement->BindOutputColumn(6, SqlColumn::Integer );
    m_pWordClassSqlColumns = m_pSqlStatement->BindOutputColumn(7, SqlColumn::StringType );
    m_pPatNumberSqlColumns = m_pSqlStatement->BindOutputColumn(8, SqlColumn::Integer );
    m_pGenderCodeSqlColumns = m_pSqlStatement->BindOutputColumn(9, SqlColumn::StringType );

    }//End Of If
}

