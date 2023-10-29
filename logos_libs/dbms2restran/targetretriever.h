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
#ifndef _targetretriever_h_
#define _targetretriever_h_

#include <logos_libs/entity/dmorphology.h>
#include <logos_libs/entity/dwordphrase.h>

class SqlConnection;
class SqlStatement;
class SqlColumn;

class CTargetRetriever
{
public:
        CTargetRetriever(SqlConnection*);
        ~CTargetRetriever();

        //Returns true if the information is found, returns false otherwise
        bool QueryAndFetch(     int usageId,
                                                const LgsString& companyCd,
                                                DWordPhrase &wordPhrase,
                                                DMorphology &morphology);

//Private member functions
private:
        void                    ConnectAndCreateSqlStatement();
        SqlStatement*   getStatement(){return m_pSqlStatement;};

//Private member varaibles
private:

        SqlConnection*  m_pSqlConnection;
        SqlStatement*   m_pSqlStatement;

        //Columns from word_phrase
        SqlColumn*              m_pWordIdColumn;
        SqlColumn*              m_pWordColumn;
        //SqlColumn*              m_pFirstWordColumn;
        SqlColumn*              m_pWordCountColumn;
        SqlColumn*              m_pWPLanguageCodeColumn;
        SqlColumn*              m_pWordTypeCodeColumn;
        SqlColumn*              m_pHeadWordColumn;
        SqlColumn*              m_pBlackHoleLocationColumn;
        SqlColumn*              m_pWildCardPositionColumn;
        SqlColumn*              m_pAspireSwitchColumn;
        //SqlColumn*            m_pMonthSwitchColumn;
        //SqlColumn*            m_pPrefixSwitchColumn;
        //SqlColumn*            m_pNeverEosSwitchColumn;
        //SqlColumn*            m_pOrdinalSwitchColumn;
        SqlColumn*              m_pPatExceptionSwitchColumn;

        //Columns from morphology
        SqlColumn*              m_pWordClassCodeColumn;
        SqlColumn*              m_pPatNumberColumn;
        SqlColumn*              m_pGenderCodeColumn;
        SqlColumn*              m_pNumericConstraintColumn;
        SqlColumn*              m_pAuxiliaryCodeColumn;
        //SqlColumn*            m_pInflectionPositionColumn;
        SqlColumn*              m_pVerbPrefixInseparableColumn;
        SqlColumn*              m_pVerbPrefixSparableColumn;
        SqlColumn*              m_pVerbPrefixInseparableLengthColumn;
        SqlColumn*              m_pVerbPrefixSparableLengthColumn;
        SqlColumn*              m_pSourceStemNumberColumn;
        SqlColumn*              m_pRootUsageIdColumn;
        SqlColumn*              m_pSourceAnalysisCodeColumn;
        SqlColumn*              m_pMLanguageCodeColumn;
};

#endif

