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

#include <logos_libs/dbms2restran/targetretriever.h>

#include <logos_libs/sql/logossql.h>


CTargetRetriever::CTargetRetriever(SqlConnection* pConnection)
: m_pSqlConnection(pConnection),
  m_pSqlStatement(NULL)
{
}

CTargetRetriever::~CTargetRetriever()
{
        if( m_pSqlStatement )
                delete m_pSqlStatement;
}

bool CTargetRetriever::QueryAndFetch(
        int usageId,
        const LgsString& companyCd,
        DWordPhrase &wordPhrase,
        DMorphology &morphology)
{
        ConnectAndCreateSqlStatement();

        getStatement()->BindInputString( ":aCC", companyCd.c_str() );
        getStatement()->BindInputInteger( ":aUI", &usageId );

    getStatement()->Execute();

        if( !getStatement()->Fetch() )
                return false;

        wordPhrase.CompanyCode          (companyCd);
        wordPhrase.WordID               (m_pWordIdColumn->AsInteger());
        wordPhrase.Word                 (m_pWordColumn->AsString());
        //wordPhrase.FirstWord            (m_pFirstWordColumn->AsString());
        wordPhrase.WordCount            (m_pWordCountColumn->AsInteger());
        wordPhrase.LanguageCode         (m_pWPLanguageCodeColumn->AsIntegerFromString());
        wordPhrase.WordTypeCode         (m_pWordTypeCodeColumn->AsIntegerFromString());
        wordPhrase.HeadWord             (m_pHeadWordColumn->AsInteger());
        wordPhrase.BlackHoleLocation    (m_pBlackHoleLocationColumn->AsInteger());
        wordPhrase.WildcardPosition     (m_pWildCardPositionColumn->AsInteger());
        wordPhrase.IsAspire             (m_pAspireSwitchColumn->AsString()==LgsString("N")?false : true);
        //wordPhrase.IsMonth              (m_pMonthSwitchColumn->AsString()==LgsString("N")?false : true);
        //wordPhrase.IsPrefix             (m_pPrefixSwitchColumn->AsString()==LgsString("N")?false : true);
        //wordPhrase.IsNeverEos           (m_pNeverEosSwitchColumn->AsString()==LgsString("N")?false : true);
        //wordPhrase.IsOrdinal            (m_pOrdinalSwitchColumn->AsString()==LgsString("N")?false : true);
        wordPhrase.IsPatException       (m_pPatExceptionSwitchColumn->AsString()==LgsString("N")?false : true);

        //Mophology
        morphology.CompanyCode          (companyCd);
        morphology.UsageID              (usageId);
        morphology.WordID                               (m_pWordIdColumn->AsInteger());
        morphology.WordClassCode        (m_pWordClassCodeColumn->AsIntegerFromString());
        morphology.LanguageCode                 (m_pMLanguageCodeColumn->AsIntegerFromString());
        morphology.GenderCode           (m_pGenderCodeColumn->AsIntegerFromString());
        morphology.NumericConstraint    (m_pNumericConstraintColumn->AsIntegerFromString());
        morphology.AuxiliaryCode        (m_pAuxiliaryCodeColumn->AsIntegerFromString());
        morphology.PatNumber            (m_pPatNumberColumn->AsInteger());
        //morphology.InflectionPosition   (m_pInflectionPositionColumn->AsInteger());
        morphology.VerbPrefixInseparable(m_pVerbPrefixInseparableColumn->AsInteger());
        morphology.VerbPrefixSeparable  (m_pVerbPrefixSparableColumn->AsInteger());
        morphology.VerbPrefixInsepLength(m_pVerbPrefixInseparableLengthColumn->AsInteger());
        morphology.VerbPrefixSepLength  (m_pVerbPrefixSparableLengthColumn->AsInteger());
        morphology.SourceStemNumber     (m_pSourceStemNumberColumn->AsInteger());
        morphology.RootUsageID          (m_pRootUsageIdColumn->AsInteger());
        morphology.SourceAnalysisCode   (m_pSourceAnalysisCodeColumn->AsIntegerFromString());

        return true;
}

void CTargetRetriever::ConnectAndCreateSqlStatement()
{
        if( !m_pSqlStatement)
        {
        m_pSqlStatement = m_pSqlConnection->CreateStatement();

        LgsString s = "select "
                         " a.Word_ID, "
                         " Word, "
                         //" First_Word, "
                         " Word_Count, "
                         " a.Language_Code, "
                         " Word_Type_Code, "
                         " Head_Word, "
                         " Black_Hole_Location, "
                         " Wildcard_Position, "
                         " Aspire_Switch, "
                         //" MONTH_SWITCH, "
                         //" PREFIX_SWITCH, "
                         //" NEVEREOS_SWITCH, "
                         //" ORDINAL_SWITCH, "
                         " Pat_Exception_Switch, "
                         " Word_Class_Code, "
                         " Pat_Number, "
                         " Gender_Code, "
                         " Numeric_Constraint, "
                         " Auxiliary_Code, "
                         //" INFLECTION_POSITION, "
                         " Verb_Prefix_Inseparable, "
                         " Verb_Prefix_Separable, "
                         " Verb_Prefix_Inseparable_Length, "
                         " Verb_Prefix_Separable_Length, "
                         " Source_Stem_Number, "
                         " Root_Usage_ID, "
                         " Source_Analysis_Code, "
                         " b.Language_Code "
                " from  Word_Phrase a, "
                "       Morphology  b "
                " where "
                         "       b.Usage_ID = :aUI "
                         " and   b.Company_Code = :aCC "
                         " and   a.Company_Code = b.Company_Code "
                         " and   a.Word_ID = b.Word_ID ";

                m_pSqlStatement->AddToCommandString( s );

        m_pSqlStatement->Parse();

                //Word phrase columns
                m_pWordIdColumn                 = m_pSqlStatement->BindOutputColumn(1, SqlColumn::Integer );
                m_pWordColumn                   = m_pSqlStatement->BindOutputColumn(2, SqlColumn::StringType );
                //m_pFirstWordColumn              = m_pSqlStatement->BindOutputColumn(3, SqlColumn::StringType );
                m_pWordCountColumn              = m_pSqlStatement->BindOutputColumn(3, SqlColumn::Integer );
                m_pWPLanguageCodeColumn         = m_pSqlStatement->BindOutputColumn(4, SqlColumn::StringType );
                m_pWordTypeCodeColumn           = m_pSqlStatement->BindOutputColumn(5, SqlColumn::StringType );
                m_pHeadWordColumn               = m_pSqlStatement->BindOutputColumn(6, SqlColumn::Integer );
                m_pBlackHoleLocationColumn      = m_pSqlStatement->BindOutputColumn(7, SqlColumn::Integer );
                m_pWildCardPositionColumn       = m_pSqlStatement->BindOutputColumn(8, SqlColumn::Integer );
                m_pAspireSwitchColumn           = m_pSqlStatement->BindOutputColumn(9, SqlColumn::StringType );
                //m_pMonthSwitchColumn            = m_pSqlStatement->BindOutputColumn(12, SqlColumn::StringType );
                //m_pPrefixSwitchColumn           = m_pSqlStatement->BindOutputColumn(13, SqlColumn::StringType );
                //m_pNeverEosSwitchColumn         = m_pSqlStatement->BindOutputColumn(14, SqlColumn::StringType );
                //m_pOrdinalSwitchColumn          = m_pSqlStatement->BindOutputColumn(15, SqlColumn::StringType );
                m_pPatExceptionSwitchColumn     = m_pSqlStatement->BindOutputColumn(10, SqlColumn::StringType );

                //morphology columns
                m_pWordClassCodeColumn               = m_pSqlStatement->BindOutputColumn(11, SqlColumn::StringType );
                m_pPatNumberColumn                   = m_pSqlStatement->BindOutputColumn(12, SqlColumn::Integer );
                m_pGenderCodeColumn                  = m_pSqlStatement->BindOutputColumn(13, SqlColumn::StringType );
                m_pNumericConstraintColumn           = m_pSqlStatement->BindOutputColumn(14, SqlColumn::StringType );
                m_pAuxiliaryCodeColumn               = m_pSqlStatement->BindOutputColumn(15, SqlColumn::StringType );
                //m_pInflectionPositionColumn          = m_pSqlStatement->BindOutputColumn(22, SqlColumn::Integer );
                m_pVerbPrefixInseparableColumn       = m_pSqlStatement->BindOutputColumn(16, SqlColumn::Integer );
                m_pVerbPrefixSparableColumn          = m_pSqlStatement->BindOutputColumn(17, SqlColumn::Integer );
                m_pVerbPrefixInseparableLengthColumn = m_pSqlStatement->BindOutputColumn(18, SqlColumn::Integer );
                m_pVerbPrefixSparableLengthColumn    = m_pSqlStatement->BindOutputColumn(19, SqlColumn::Integer );
                m_pSourceStemNumberColumn            = m_pSqlStatement->BindOutputColumn(20, SqlColumn::Integer );
                m_pRootUsageIdColumn                 = m_pSqlStatement->BindOutputColumn(21, SqlColumn::Integer );
                m_pSourceAnalysisCodeColumn          = m_pSqlStatement->BindOutputColumn(22, SqlColumn::StringType );
                m_pMLanguageCodeColumn               = m_pSqlStatement->BindOutputColumn(23, SqlColumn::StringType );

        }//End Of If
}

