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
#ifndef _semtabrulebuilder_h_
#define _semtabrulebuilder_h_

#include <logos_libs/sql/logossql.h>
#include <logos_libs/semtabrule/semtabrule.h>

//************************************************************
// Description: CSemtabRuleBulder class is used to query
//                              the relational database where semtab rules
//                              are stored as blobs. This class creates
//                              CSemtabRule and returns a vector of the
//                              CSemtabRule, also known as CSemtabRuleVector.
//                              This class is designed so that the SQL query
//                              can be reused.
//
// Author:              Manoj Agarwala
// History:             09/23/96 - Originally Conceived
//************************************************************
#pragma comment(lib, "wsock32.lib") //becuase of ntohs

class SqlConnection;
class SqlStatement;
class SqlColumn;

class CacheSemtabRuleData;
class CacheSemtabRuleQuery;

class CSemtabRuleBuilder
{
public:
        CSemtabRuleBuilder(SqlConnection*);
        ~CSemtabRuleBuilder();

        void QueryAndFetchAll( const LgsString& srcLangCd,
							   const LgsString& trgtLangCd,
							   const LgsString& deactivationSwitch,
							   CSemtabRuleVector& semtabRules);

        void QueryAndFetch( const LgsString& srcLangCd,
							const LgsString& trgtLangCd,
							int wordClass,
							int subset,
							int set,
							const LgsString& companyCd,
							const LgsString& deactivationSwitch,
							CSemtabRuleVector& semtabRules);

//Private member functions
private:

        //Either the query can be done in one step using QueryAndFetch
        //or in 2 step using Query and Fetch
        void Query( const LgsString& srcLangCd,
					const LgsString& trgtLangCd,
					int wordClass, 
					int subset,
					int set,
					const LgsString& companyCd,
					const LgsString& deactivationSwitch);

        //Fetch is valid only after Query.
        //maxItems is used to specify the maximun number of items to be Fetched
        void Fetch(CSemtabRuleVector& semtabRules);

        //Make sure that the colums listed in the select clause
        //are in the same order as listed here.
        //Also list all the columns with LgsString output before
        //EndOfStringColumns and all the colums with integer
        //output between EndOfStringColumns and EndOfIntegerColumns
        enum SqlColumnEnum{     
						COMPANY_CODE,
                        SOURCE_LANGUAGE_CODE,
                        TARGET_LANGUAGE_CODE,
                        DEACTIVATION_SWITCH,
                        COMMENTLINE,
                        EndOfStringColumns, //Make sure all the columns with
							//LgsString output is listed before
							//this tag and all the columns with
							//integer output is listed after this tag
							//and before EndOfIntegerColumns
                        SEMTAB_ID,
                        WORD_CLASS_CODE,
                        SUBSET_ID,
                        SET_ID,
                        WC1,
                        TY1,
                        FM1,
                        WC2,
                        TY2,
                        FM2,
                        WC3,
                        TY3,
                        FM3,
                        WC4,
                        TY4,
                        FM4,
                        WC5,
                        TY5,
                        FM5,
                        WC6,
                        TY6,
                        FM6,
                        WC7,
                        TY7,
                        FM7,
                        WC8,
                        TY8,
                        FM8,
                        WC9,
                        TY9,
                        FM9,
                        TAG11,
                        TAG12,
                        TAG21,
                        TAG22,
                        TAG31,
                        TAG32,
                        TAG41,
                        TAG42,
                        TAG51,
                        TAG52,
                        TAG61,
                        TAG62,
                        TAG71,
                        TAG72,
                        TAG81,
                        TAG82,
                        TAG91,
                        TAG92,
                        RULE_LEVEL,
                        SPECIFICITY,
                        NUMBYTESINDATABLOB,
                        EndOfIntegerColumns,
                        DATA_BLOB,
                        EndOfSqlColumnEnum};

        void                    ExtractFromBlob(CSemtabRule& sr, int blobLength, const unsigned char* blob);
        short                   GetShortFromBlob(int& byteOffset, const unsigned char* blob) const;

        void                    ConnectAndCreateSqlStatement();
        SqlStatement*   getStatement(){return m_pSqlStatement;};

        LgsString                  getColumnValueAsString(int index);
        int                             getColumnValueAsInteger(int index);
        unsigned char*  getColumnValueAsLongRaw(int index);

//Private member varaibles
private:

        bool            fetchAll;

        SqlConnection*  m_pSqlConnection;
        SqlStatement*   m_pSqlStatement;

        SqlColumn*      m_pSqlColumns[EndOfSqlColumnEnum];
                            //Does not do any harm to have couple
                            //of extra entries which are not used

		CacheSemtabRuleData *fdata[7][7];
		CacheSemtabRuleQuery *fquery[7][7];

		char cc[5];
		int srcl;
		int trgl;
		char ds;
		int semid;
		int wcc;
		int subsid;
		int setid;
		int wc1, ty1, fm1;
		int wc2, ty2, fm2;
		int wc3, ty3, fm3;
		int wc4, ty4, fm4;
		int wc5, ty5, fm5;
		int wc6, ty6, fm6;
		int wc7, ty7, fm7;
		int wc8, ty8, fm8;
		int wc9, ty9, fm9;
		int tag11, tag12;
		int tag21, tag22;
		int tag31, tag32;
		int tag41, tag42;
		int tag51, tag52;
		int tag61, tag62;
		int tag71, tag72;
		int tag81, tag82;
		int tag91, tag92;
		int rl;
		int spec;
		int nb;
		unsigned char blob[165];

		LgsString * srclStr;
		LgsString * trglStr;
};

inline LgsString CSemtabRuleBuilder::getColumnValueAsString(int index)
{
        return m_pSqlColumns[index]->AsString();
}

inline int CSemtabRuleBuilder::getColumnValueAsInteger(int index)
{
        return m_pSqlColumns[index]->AsInteger();
}

inline unsigned char* CSemtabRuleBuilder::getColumnValueAsLongRaw(int index)
{
        return m_pSqlColumns[index]->AsLongRaw();
}


#endif

