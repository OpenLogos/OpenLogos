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
#include <logos_libs/gerdem/targetquery.h>
#include <logos_libs/sql/sqlstatement.h>
#include <logos_libs/sql/sqlcolumn.h>
#include <logos_libs/sql/sqlexception.h>
#include <logos_libs/linguistic/llanguage.h>
#include <logos_libs/linguistic/lsemantosyntacticunit.h>

#include <logos_libs/dbcache/CacheTargetData.h>
#include <logos_libs/dbcache/CacheTargetQuery.h>

//----------------------------------------------------------------------
TargetQuery::TargetQuery() {
	ddata = NULL;
	dquery = NULL;
}
//----------------------------------------------------------------------
TargetQuery::~TargetQuery()
{
	if(ddata) { delete ddata; ddata = NULL; }
	if(dquery) { delete dquery; dquery = NULL; }
}
//----------------------------------------------------------------------
void TargetQuery::Open( SqlConnection* aConnection,
                          const LLanguage& language )
{
	v_languageCode = language.id();

	ddata = new CacheTargetData(NULL, v_languageCode, false, false, false);
	if(ddata->isValid()) {
		dquery = new CacheTargetQuery(ddata, 0);
		return;
	} else {
		delete ddata;
		ddata = NULL;
	}

        LgsString s = "select "
                         " w.Word_ID, "
                         " w.Word, "
                         " w.Word_Count, "
                         " w.Word_Type_Code, "
                         " w.Head_Word, "
                         " w.Black_Hole_Location, "
                         " w.Wildcard_Position, "
                         " w.Aspire_Switch, "
                         " w.Pat_Exception_Switch, "
                         " m.Word_Class_Code, "
                         " m.Pat_Number, "
                         " m.Gender_Code, "
                         " m.Numeric_Constraint, "
                         " m.Auxiliary_Code, "
                         " m.Verb_Prefix_Inseparable, "
                         " m.Verb_Prefix_Separable, "
                         " m.Verb_Prefix_Inseparable_Length, "
                         " m.Verb_Prefix_Separable_Length, "
                         " m.Source_Stem_Number, "
                         " m.Root_Usage_ID, "
                         " m.Source_Analysis_Code "
                         " from  Word_Phrase w, "
                         "       Morphology  m "
                         " where "
                         "       m.Usage_ID = :aUsageID "
                         " and   w.Company_Code = m.Company_Code "
                         " and   w.Word_ID = m.word_id ";
    try
    {
        SqlQuery::Open( aConnection );

        Statement()->AddToCommandString( s );

        Statement()->Parse();

        v_languageCode = language.id();

        //Word phrase columns
        m_pWordId                 = Statement()->BindOutputColumn(1, SqlColumn::Integer );
        m_pWord                   = Statement()->BindOutputColumn(2, SqlColumn::StringType );
        m_pWordCount              = Statement()->BindOutputColumn(3, SqlColumn::Integer );
        m_pWordTypeCode           = Statement()->BindOutputColumn(4, SqlColumn::StringType );
        m_pHeadWord               = Statement()->BindOutputColumn(5, SqlColumn::Integer );
        m_pBlackHoleLocation      = Statement()->BindOutputColumn(6, SqlColumn::Integer );
        m_pWildCardPosition       = Statement()->BindOutputColumn(7, SqlColumn::Integer );
        m_pAspireSwitch           = Statement()->BindOutputColumn(8, SqlColumn::StringType );
        m_pPatExceptionSwitch     = Statement()->BindOutputColumn(9, SqlColumn::StringType );

        //morphology columns
        m_pWordClassCode               = Statement()->BindOutputColumn(10, SqlColumn::StringType );
        m_pPatNumber                   = Statement()->BindOutputColumn(11, SqlColumn::Integer );
        m_pGenderCode                  = Statement()->BindOutputColumn(12, SqlColumn::StringType);
        m_pNumericConstraint           = Statement()->BindOutputColumn(13, SqlColumn::StringType);
        m_pAuxiliaryCode               = Statement()->BindOutputColumn(14, SqlColumn::StringType);
        m_pVerbPrefixInseparable       = Statement()->BindOutputColumn(15, SqlColumn::Integer );
        m_pVerbPrefixSparable          = Statement()->BindOutputColumn(16, SqlColumn::Integer );
        m_pVerbPrefixInseparableLength = Statement()->BindOutputColumn(17, SqlColumn::Integer );
        m_pVerbPrefixSparableLength    = Statement()->BindOutputColumn(18, SqlColumn::Integer );
        m_pSourceStemNumber            = Statement()->BindOutputColumn(19, SqlColumn::Integer );
        m_pRootUsageId                 = Statement()->BindOutputColumn(20, SqlColumn::Integer );
        m_pSourceAnalysisCode          = Statement()->BindOutputColumn(21, SqlColumn::StringType);

    }
    catch( SqlException& x )
    {
        cout << x.Message() << endl;
        throw( x );
    }
}
//----------------------------------------------------------------------
void TargetQuery::Close() {
	if(ddata==NULL)
    SqlQuery::Close();

}
//----------------------------------------------------------------------
void TargetQuery::executeWithUsageID( int usageID )
{

	if(ddata) {
		dquery->query(usageID);
		return;
	}

    try
    {
        Statement()->BindInputIntToString( ":aUsageID",
                                           "%d",
                                           v_inputUsageID,
                                           usageID );

        Statement()->Execute ();
    }
    catch( SqlException& x )
    {
        cout << x.Message() << endl;
        throw( x );
    }
}
//----------------------------------------------------------------------
bool
TargetQuery::fetch()
{
    //------------------------------------------------------------------
    //------------------------------------------------------------------

    bool result = true;

	if(ddata) {
		result = dquery->fetch(&wid, word, &wc, &wtc, &hw, &bhl, &wp, as, pes,
			&wcc, &pn, &gc, &nc, &ac, &vpi, &vps, &vpil, &vpsl, &ssn, 
			&ruid, &sac);
//printf("OCR G-querying \"%s\" %d %d %d %d %d %d \"%s\" %d\n", 
//v_inputWord, h1, h2, hashloc, hword, word_id, wtc, p->CompanyCode().c_str());

		return result;
	}

    try
    {
        result = Statement()->Fetch();
    }
    catch( SqlException& x )
    {
        cout << x.Message() << endl;
        throw( x );
    }
    return result;
}
//----------------------------------------------------------------------
void TargetQuery::setSsu( LSemantoSyntacticUnit* p ) const
{
//ocr
	if(ddata) {
		p->setWordID(wid);
		p->setWord(word);	
		p->setWordCount(wc);
		p->setWordTypeCode(wtc);
		p->setHeadWord(hw);
		p->setBlackHoleLocation(bhl);
		p->setWildcardPosition(wp);
		p->setIsAspire(as[0]=='Y' ? true : false);
		p->setWordClassCode(wcc);
		p->setGenderCode(gc);
		p->setNumericConstraint(nc);
		p->setAuxiliaryCode(ac);
		p->setPatNumber(pn);
		p->setVerbPrefixInseparable(vpi);
		p->setVerbPrefixSeparable(vps);
		p->setVerbPrefixInsepLength(vpil);
		p->setVerbPrefixSepLength(vpsl);
		p->setSourceStemNumber(ssn);
		p->setRootUsageID(ruid);
		p->setSourceAnalysisCode(sac);
//printf("TargetQuery: %d, \"%s\", %d, %d, %d, %d, %d, %c, %d, %d, %d, %d, %d,
//	   %d, %d, %d, %d, %d, %d %d\n",
//	wid, word, wc, wcc, hw, bhl, wp,
//	as[0], wcc, gc, nc, ac, pn, vpi, vps, vpil, vpsl, 
//	ssn, ruid, sac,
//);

		return;
	}

	
     p->setWordID               (m_pWordId->AsInteger());
     p->setWord                 (m_pWord->AsString());
     p->setWordCount            (m_pWordCount->AsInteger());
     p->setWordTypeCode         (m_pWordTypeCode->AsIntegerFromString());
     p->setHeadWord             (m_pHeadWord->AsInteger());
     p->setBlackHoleLocation    (m_pBlackHoleLocation->AsInteger());
     p->setWildcardPosition     (m_pWildCardPosition->AsInteger());
     p->setIsAspire             (m_pAspireSwitch->AsString() == LgsString("Y") ? true : false);
     p->setWordClassCode        (m_pWordClassCode->AsIntegerFromString());
     p->setGenderCode           (m_pGenderCode->AsIntegerFromString());
     p->setNumericConstraint    (m_pNumericConstraint->AsIntegerFromString());
     p->setAuxiliaryCode        (m_pAuxiliaryCode->AsIntegerFromString());
     p->setPatNumber            (m_pPatNumber->AsInteger());
     p->setVerbPrefixInseparable(m_pVerbPrefixInseparable->AsInteger());
     p->setVerbPrefixSeparable  (m_pVerbPrefixSparable->AsInteger());
     p->setVerbPrefixInsepLength(m_pVerbPrefixInseparableLength->AsInteger());
     p->setVerbPrefixSepLength  (m_pVerbPrefixSparableLength->AsInteger());
     p->setSourceStemNumber     (m_pSourceStemNumber->AsInteger());
     p->setRootUsageID          (m_pRootUsageId->AsInteger());
     p->setSourceAnalysisCode   (m_pSourceAnalysisCode->AsIntegerFromString());
/*
printf("TargetQuery: %d, \"%s\", %d, %d, %d, %d, %d, %c, %d, %d, %d, %d, %d,
	   %d, %d, %d, %d, %d, %d %d\n",
	m_pWordId->AsInteger(), m_pWord->AsString(), m_pWordCount->AsInteger(),
	m_pWordTypeCode->AsIntegerFromString(), m_pHeadWord->AsInteger(),
	m_pBlackHoleLocation->AsInteger(), m_pWildCardPosition->AsInteger(),
	m_pAspireSwitch->AsString(), m_pWordClassCode->AsIntegerFromString(),
	m_pGenderCode->AsIntegerFromString(), m_pNumericConstraint->AsIntegerFromString(),
	m_pAuxiliaryCode->AsIntegerFromString(), m_pPatNumber->AsInteger(),
	m_pVerbPrefixInseparable->AsInteger(), m_pVerbPrefixSparable->AsInteger(),
	m_pVerbPrefixInseparableLength->AsInteger(), m_pVerbPrefixSparableLength->AsInteger(),
	m_pSourceStemNumber->AsInteger(), m_pRootUsageId->AsInteger(),
	m_pSourceAnalysisCode->AsIntegerFromString()
	);
*/
 }
