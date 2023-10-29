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

#include <logos_libs/dbms2restran/constantcodebuilder.h>

#include <logos_libs/dbcache/CacheConstantCodeData.h>
#include <logos_libs/dbcache/CacheConstantCodeQuery.h>

#include <logos_libs/sql/logossql.h>
#include /**/ <memory.h>

//************************************************************
// Author:              Manoj Agarwala
// History:             11/4/96 - Originally Conceived
//************************************************************

CConstantCodeBuilder::CConstantCodeBuilder(SqlConnection *pConnection)
: m_pSqlConnection(pConnection),
  m_pSqlStatement(NULL),
  m_genderRetriever(pConnection)
{

	for(int i=1;i<7;i++) {
		hdata[i] = new CacheConstantCodeData(NULL, i, false, false, false);
		if(hdata[i]->isValid()) {
			hquery[i] = new CacheConstantCodeQuery(hdata[i], 0, 0);
			continue;
		} else {
			delete hdata[i];
			hdata[i] = NULL;
			hquery[i] = NULL;
		}
//printf("ccbuilder[%d].hdata[%d] = %d\n", this, i, hdata[i]);
//fflush(stdout);
}
}

static char ctype_static[64];
static int cid_static;

CConstantCodeBuilder::~CConstantCodeBuilder()
{
        if( m_pSqlStatement )
                delete m_pSqlStatement;
	for(int i=1;i<7;i++) {
		if(hdata[i]) { delete hdata[i]; hdata[i] = NULL; }
		if(hquery[i]) { delete hquery[i]; hquery[i] = NULL; }
//printf("ccbuilder[%d].hdata[%d] = %d\n", this, i, hdata[i]);
//fflush(stdout);
	}
}

bool CConstantCodeBuilder::QueryAndFetch(
        int constantId,
        const LgsString& constantType,
        const LgsString& targetLanguage,
        const LgsString& companyCd,
        CTransferCodeVector& transfercodes,
        bool  emptyVector)
{
        Query(constantId, constantType, targetLanguage, companyCd);
        return Fetch(transfercodes, 0, emptyVector); //0 means fetch all item
}

void CConstantCodeBuilder::Query(
        int constantId,
        const LgsString& constantType,
        const LgsString& targetLanguage,
        const LgsString& companyCd)
{
        m_TargetLanguageCode = targetLanguage;

//strcpy(ctype_static, (char *)constantType.c_str());
//cid_static = constantId;
		langid = atoi(targetLanguage.c_str());
		if(langid<7 && langid > 0) {
			if(hdata[langid]) {
				int ret = hquery[langid]->query((char *)constantType.c_str(), 
					constantId);
//printf("\nCacheConstantCodeQuery (%s, %d): ret=%d\n", constantType.c_str(), constantId, ret);
//fflush(stdout);
				return;
			}
}

        ConnectAndCreateSqlStatement();

//        getStatement()->BindInputString( ":aCC", companyCd.c_str() );
        getStatement()->BindInputInteger( ":aCI", &constantId );
        getStatement()->BindInputString( ":aCT", constantType.c_str() );
        getStatement()->BindInputString( ":aTLC", targetLanguage.c_str() );

    getStatement()->Execute();
}

int CConstantCodeBuilder::Fetch(
CTransferCodeVector& transfercodes,
int   maxItems,
bool  emptyVector
)
{
        if ( emptyVector )
                transfercodes.erase(transfercodes.begin(), transfercodes.end());

        int itemsFetched = 0;

		if(langid<7 && langid > 0) {
			if(hdata[langid]) {
				while(1) {
					int ret = hquery[langid]->fetch(cc, &auid, wcc, &pn1, 
						&gc1, nc, &pn2, &gc2);
					if(ret==0) break;
					itemsFetched++;	

					CTransferCode tc;
					tc.setCompanyCode(cc);
					tc.setAlternateSeq(0);	

					int wccint = atoi(wcc);
					tc.setWordClassCode(wccint);

					tc.setPatNumber(pn1);
					tc.setAlternatePatNumber(pn2);

					if(gc1) {
						tc.setGenderCode(gc1);
					} else { //else use the alternate gender
						tc.setGenderCode(gc2);
					}
				// set the overflow codes !!
					tc.setOverflow2a(0);
					tc.setOverflow3a(0);
					tc.setOverflow3b(1); //Default for overflow3b is 1
					tc.setOverflow2b(1); //Default for overflow2b is 1

					int numericConstraint;
					switch (wccint) {
						case 5:		// pronoun
							if(langid!=2) 
								tc.setOverflow2b(tc.getGenderCode());	
							else
								tc.setOverflow2b(8);
							break;

						case 1:		// noun
							if (langid!=2) {   // if not english
								numericConstraint = atoi(nc);

								if( numericConstraint == 1 )
									tc.setOverflow2b( tc.getGenderCode() );
								else if( numericConstraint == 3 )
									tc.setOverflow2b( tc.getGenderCode() + 3);
								else if( numericConstraint == 2 )
									tc.setOverflow2b( tc.getGenderCode() + 6);
							} else {
								tc.setOverflow2b(8);
							}
							break;
						case 2:		// verb
							if((auid == 0) ||
							(m_TargetLanguageCode == LgsString("01") 
								&& tc.getAlternatePatNumber() ==  86) ||
							(m_TargetLanguageCode == LgsString("03") 
								&& tc.getAlternatePatNumber() == 113) ||
							(m_TargetLanguageCode == LgsString("04") 
								&& tc.getAlternatePatNumber() == 170) ||
							(m_TargetLanguageCode == LgsString("05") 
								&& tc.getAlternatePatNumber() == 184) ||
							(m_TargetLanguageCode == LgsString("06") 
								&& tc.getAlternatePatNumber() == 105) 
							) {
								tc.setOverflow2b(4);
							} else {
								tc.setOverflow2b(8);
							}
							break;
						default:	// all other parts of speech
							tc.setOverflow2b(1);
							break;
					}

					transfercodes.push_back(tc);
/*
printf("\nCacheConstantCodeQuery (%s, %d): \"%s\" %d %d %d %d %d %d %d \n", 
	   ctype_static, cid_static, tc.getCompanyCode().c_str(),
	   auid, tc.getWordClassCode(), tc.getPatNumber(),
	   gc1, numericConstraint, 
	   tc.getAlternatePatNumber(),
	   gc2);
fflush(stdout);
*/
				}
				return itemsFetched;
			}	
		}

        //Fetch all the rows
        while(    (    maxItems == 0 //0 means all the items
                        || itemsFetched < maxItems
                          )
                   && getStatement()->Fetch()
                 )
        {
                itemsFetched++;

                CTransferCode tc;

                tc.setCompanyCode( m_pCompanyCodeSqlColumns->AsString());
                tc.setAlternateSeq( 0);
                tc.setWordClassCode( m_pWordClassSqlColumns->AsIntegerFromString());
                tc.setPatNumber( m_pPatNumberSqlColumns->AsInteger());
                tc.setAlternatePatNumber( m_pAlternatePatNumberSqlColumns->AsInteger());

                int alternateUsageId = m_pAltUsageSqlColumns->AsInteger();

                int genderCode = m_pGenderCodeSqlColumns->AsIntegerFromString();

                if( genderCode != 0 )
                {
                        tc.setGenderCode( m_pGenderCodeSqlColumns->AsIntegerFromString());
                }
                //else use the alternate gender
                else
                {
						tc.setGenderCode( m_pAlternateGenderCodeSqlColumns->AsIntegerFromString());
						/*int  genderCode;

                        if( m_genderRetriever.QueryAndFetch( alternateUsageId,
                                                                                      tc.getCompanyCode(),
                                                                                     genderCode) )
                        {
                                tc.setGenderCode( genderCode );
                        }*/

                }




			// set the overflow codes !!
                tc.setOverflow2a( 0);
                tc.setOverflow3a( 0);
                tc.setOverflow3b( 1); //Default for overflow3b is 1
                tc.setOverflow2b( 1); //Default for overflow2b is 1

				int numericConstraint;

				switch ( tc.getWordClassCode() ){

				case 5:		// pronoun
					if (m_TargetLanguageCode != LgsString("02")){   // if not english
						tc.setOverflow2b( tc.getGenderCode() );	
					}
					else {
						tc.setOverflow2b(8);
					}
					break;

				case 1:		// noun
					if (m_TargetLanguageCode != LgsString("02")){   // if not english
                        numericConstraint = m_pNumericConstraintSqlColumns->AsIntegerFromString();

                        if( numericConstraint == 1 )
                                tc.setOverflow2b( tc.getGenderCode() );
                        else if( numericConstraint == 3 )
                                tc.setOverflow2b( tc.getGenderCode() + 3);
                        else if( numericConstraint == 2 )
                                tc.setOverflow2b( tc.getGenderCode() + 6);
					}
					else {
					 	tc.setOverflow2b(8);
					}
					break;

				case 2:		// verb
					if( (alternateUsageId == 0) ||
						(m_TargetLanguageCode == LgsString("01") && tc.getAlternatePatNumber() ==  86) ||
						(m_TargetLanguageCode == LgsString("03") && tc.getAlternatePatNumber() == 113) ||
						(m_TargetLanguageCode == LgsString("04") && tc.getAlternatePatNumber() == 170) ||
						(m_TargetLanguageCode == LgsString("05") && tc.getAlternatePatNumber() == 184) ||
						(m_TargetLanguageCode == LgsString("06") && tc.getAlternatePatNumber() == 105) 
  						)
					{
						tc.setOverflow2b(4);
					}
					else {
						tc.setOverflow2b(8);
					}
					break;

				default:	// all other parts of speech
					tc.setOverflow2b(1);
					break;
				}
/*
printf("\nCacheConstantCodeQuery (%s, %d): \"%s\" %d %d %d %d %d %d %d \n", 
	   ctype_static, cid_static, tc.getCompanyCode().c_str(),
	   alternateUsageId, tc.getWordClassCode(), tc.getPatNumber(),
	   m_pGenderCodeSqlColumns->AsIntegerFromString(), numericConstraint, 
	   tc.getAlternatePatNumber(),
	   m_pAlternateGenderCodeSqlColumns->AsIntegerFromString());
fflush(stdout);
*/      
                transfercodes.push_back(tc);
        }
/*
printf("\nCacheConstantCodeQuery (%s, %d): ret=%d\n", ctype_static, 
	   cid_static, itemsFetched);
fflush(stdout);
*/

        return itemsFetched;
}


void CConstantCodeBuilder::ConnectAndCreateSqlStatement()
{
        if( !m_pSqlStatement)
        {
        m_pSqlStatement = m_pSqlConnection->CreateStatement();

/* dan 9-14-99
Commented out use of company code for constants.
High constants are always LOG and user can not update them.
Low constants can be LOG or created by user, but..
There is no way of knowing the company code when tran calls for contant load.
we need further logic to do this. 
Commenting out the cc for now works but will be a major problem when
later we merge semtab rules and constants at user sites.
*/

        LgsString s = "select "
                        "  a.Company_Code, "
                        "  a.Alternate_Usage_ID, "
                        "  b.Word_Class_Code, "
                        "  b.Pat_Number, "
                        "  b.Gender_Code, "
                        "  b.Numeric_Constraint, "
                        "  c.Pat_Number, "
                        "  c.Gender_Code"
#ifdef USE_ORACLE8
              " from  Constant_Pointer a, "
                        "  Morphology b, "
                        "  Morphology c "
              " where "
//                       "       a.Company_Code = :aCC "
                       "       a.Constant_Type = :aCT "
                       " and   a.Constant_ID = :aCI "
                       " and   a.Language_Code = :aTLC "
//                       " and   b.Company_Code = a.Company_Code "
                       " and   b.Usage_ID = a.Primary_Usage_ID "
//                       " and   c.Company_Code(+) = a.Company_Code "
                       " and   c.Usage_ID(+) = a.Alternate_Usage_ID "; //(+)
//                       for outer Join
#else
// Postgres OUTER JOIN syntax
              " from  Constant_Pointer a LEFT OUTER JOIN Morphology c ON c.usage_id = a.alternate_usage_id, "
                        "  Morphology b "
              " where "
                       "       a.Constant_Type = :aCT "
                       " and   a.Constant_ID = :aCI "
                       " and   a.Language_Code = :aTLC "
	  " and   b.Usage_ID = a.Primary_Usage_ID ";
#endif

        m_pSqlStatement->AddToCommandString( s );

        m_pSqlStatement->Parse();

        m_pCompanyCodeSqlColumns = m_pSqlStatement->BindOutputColumn(1, SqlColumn::StringType );
        m_pAltUsageSqlColumns = m_pSqlStatement->BindOutputColumn(2, SqlColumn::Integer );
        m_pWordClassSqlColumns = m_pSqlStatement->BindOutputColumn(3, SqlColumn::StringType );
        m_pPatNumberSqlColumns = m_pSqlStatement->BindOutputColumn(4, SqlColumn::Integer );
        m_pGenderCodeSqlColumns = m_pSqlStatement->BindOutputColumn(5, SqlColumn::StringType );
        m_pNumericConstraintSqlColumns = m_pSqlStatement->BindOutputColumn(6, SqlColumn::StringType );
        m_pAlternatePatNumberSqlColumns = m_pSqlStatement->BindOutputColumn(7, SqlColumn::Integer );
        m_pAlternateGenderCodeSqlColumns = m_pSqlStatement->BindOutputColumn(8, SqlColumn::StringType );

        }//End Of If
}

