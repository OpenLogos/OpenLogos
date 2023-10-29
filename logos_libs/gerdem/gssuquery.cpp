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
// --------------------------------------------------------------------------
// File - GSsuQuery.cpp (implementation)
// --------------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/gerdem/gssuquery.h>
#include <logos_libs/entity/dwordphrase.h>
#include <logos_libs/sql/sqlstatement.h>
#include <logos_libs/sql/sqlcolumn.h>
#include <logos_libs/sql/sqlexception.h>
#include <logos_libs/linguistic/lsemantosyntacticunit.h>
#include <logos_libs/linguistic/llanguage.h>
#include <logos_libs/SubjectMatterCodes/SubjectMatterCode.h>

#include <logos_libs/dbcache/CacheGssuData.h>
#include <logos_libs/dbcache/CacheGssuQuery.h>

// --------------------------------------------------------------------------
// Default constructor
// --------------------------------------------------------------------------
GSsuQuery::GSsuQuery()
{
   adata = NULL;
   aquery = NULL;
}
// --------------------------------------------------------------------------
// Destructor
// --------------------------------------------------------------------------
GSsuQuery::~GSsuQuery()
{
   if (adata)
   {
      delete adata;
      adata = NULL;
   }
   if (aquery)
   {
      delete aquery;
      aquery = NULL;
   }
}
// --------------------------------------------------------------------------
// Open connection to database
// --------------------------------------------------------------------------
void GSsuQuery::Open(SqlConnection* aConnection, const LLanguage& sourceLanguage,
                     const LLanguage& targetLanguage)
{
	adata = new CacheGssuData(NULL, sourceLanguage.id(), false, false, false);
	if (adata->isValid())
   {
		aquery = new CacheGssuQuery(adata, NULL, 0);
		return;
	}
   else
   {
		delete adata;
		adata = NULL;
		aquery = NULL;
	}

   LgsString s = "select"
                 " mr.Word_Class_Code,"
                 " mr.Gender_Code,"
                 " mr.Pat_Number,"
                 " mr.Source_Stem_Number,"
                 " mr.Usage_ID,"
                 " mr.Auxiliary_Code,"
                 " mn.Meaning_ID,"
                 " mn.Set_ID,"
                 " mn.Subset_ID,"
                 " mn.Superset_ID,"
                 " mn.Priority_Order,"
                 " mn.Form_Code,"
                 " mn.Subject_Matter_Code,"
                 " mn.Atomic_Code,"			// TO BE REMOVED WHEN NEW SMC IS TESTED
                 " mn.Generic_Code, "			// TO BE REMOVED WHEN NEW SMC IS TESTED
			        " mn.transitivity"
                 " from"
                 " Morphology mr, Meaning mn "
                 " where mr.Word_ID = :aWordId"
                 " and   mr.Company_Code = :aCompany"
                 " and   mr.Language_Code = :aSource"
                 " and   mn.Company_Code = mr.Company_Code"
                 " and   mn.Language_Code = mr.Language_Code"
                 " and   mn.Usage_ID = mr.Usage_ID"
                 " and   mr.Source_Analysis_Code = '1'"
                 " and   mn.Primary_Switch = 'Y'"
                 " order by mn.Priority_Order" ;

   try
   {
      SqlQuery::Open(aConnection);
      Statement()->AddToCommandString(s);
      Statement()->Parse();
      v_sourceCode = sourceLanguage.id();

      p_wordClassCode     = Statement()->BindOutputColumn(1, SqlColumn::StringType);
      p_genderCode        = Statement()->BindOutputColumn(2, SqlColumn::StringType);
      p_sourcePatNumber   = Statement()->BindOutputColumn(3, SqlColumn::Integer);
      p_sourceStemNumber  = Statement()->BindOutputColumn(4, SqlColumn::Integer);
      p_usageID           = Statement()->BindOutputColumn(5, SqlColumn::Integer);
      p_auxiliaryCode     = Statement()->BindOutputColumn(6, SqlColumn::StringType);
      p_meaningID         = Statement()->BindOutputColumn(7, SqlColumn::Integer);
      p_setID             = Statement()->BindOutputColumn(8, SqlColumn::Integer);
      p_subSetID          = Statement()->BindOutputColumn(9, SqlColumn::Integer);
      p_superSetID        = Statement()->BindOutputColumn(10, SqlColumn::Integer);
      p_priorityOrder     = Statement()->BindOutputColumn(11, SqlColumn::Integer);
      p_formCode          = Statement()->BindOutputColumn(12, SqlColumn::StringType);
      p_subjectMatterCode = Statement()->BindOutputColumn(13, SqlColumn::StringType);
      p_atomicCode        = Statement()->BindOutputColumn(14, SqlColumn::StringType);	// TO BE REMOVED WHEN NEW SMC IS TESTED
      p_genericCode       = Statement()->BindOutputColumn(15, SqlColumn::StringType);	// TO BE REMOVED WHEN NEW SMC IS TESTED
	  p_transitivity	     = Statement()->BindOutputColumn(16, SqlColumn::StringType);
   }
   catch(SqlException& x)
   {
      cout << x.Message() << endl;
      throw(x);
   }
}
// --------------------------------------------------------------------------
// Close the connection to the database
// --------------------------------------------------------------------------
void GSsuQuery::Close()
{
	if (adata == NULL)
      SqlQuery::Close();
}
// --------------------------------------------------------------------------
void GSsuQuery::ExecuteWithMatch(const DWordPhrase& match)
{
	v_inputWordId = match.WordID();
	if (adata)
   {
		aquery->query((char *)match.CompanyCode().c_str(), match.WordID());
//printf("\nQQQ GSSU.query wordid=%d\n", v_inputWordId);
//fflush(stdout);
		return;
	}

   try
   {
      Statement()->BindInputIntToString(":aSource", "%02d", v_inputSourceCode, v_sourceCode);
      int wordID = match.WordID();
//printf("\nQQQ GSSU.query wordid=%d\n", v_inputWordId);
//fflush(stdout);
      Statement()->BindInputInteger(":aWordId", &wordID);
      Statement()->BindInputString (":aCompany", match.CompanyCode().c_str());

      Statement()->Execute ();
   }
   catch(SqlException& x)
   {
      cout << x.Message() << endl;
      throw(x);
   }
   v_isFetchStarted = false;
}
// --------------------------------------------------------------------------
bool GSsuQuery::fetch()
{
	if (adata)
   {
		return aquery->fetch(&cache_wcc, &cache_gc, &cache_pn, &cache_ssn, 
			&cache_usid, &cache_ac, &cache_mid, &cache_setid, &cache_subsetid, 
			&cache_supersetid, &cache_po, &cache_fc, cache_smc, 
			&cache_acode, &cache_gcode, cache_tr);
	}

   return Statement()->Fetch();
}
// --------------------------------------------------------------------------
// Add to the given semanto-syntactic unit the following information
// --------------------------------------------------------------------------
void GSsuQuery::setSsu(LSemantoSyntacticUnit* p) const
{
	if (adata)
   {
		p->setGenderCode(cache_gc);
		p->setPatNumber(cache_pn);
		p->setSourceStemNumber(cache_ssn);
		p->setUsageID(cache_usid);
		p->setAuxiliaryCode(cache_ac);
		p->setWordClassCode(cache_wcc);
		p->setPriorityOrder(cache_po);
		p->setFormCode(cache_fc);
		p->setMeaningID(cache_mid);
		p->setSetID(cache_setid);
		p->setSubSetID(cache_subsetid);
		p->setSuperSetID(cache_supersetid);
		p->setAtomicCode(cache_acode);		// TO BE REMOVED WHEN NEW SMC IS TESTED
		p->setGenericCode(cache_gcode);	// TO BE REMOVED WHEN NEW SMC IS TESTED
		p->setTransitivity(cache_tr[0]=='T');
//		(p_transitivity->AsString() == "T")? p->setTransitivity(true): p->setTransitivity(false);
		SubjectMatterCode theSMC(cache_smc);
		p->setSubjectMatterCode(theSMC);
//printf("\nQQQ %d: %d %d %d %d %d %d %d %d %d %d %d %d %d %d \"%s\" \"%s\"\n",
//	   v_inputWordId, cache_wcc, cache_gc, cache_pn, cache_ssn, cache_usid, 
//	   cache_ac, cache_mid, cache_setid, cache_subsetid, cache_supersetid, 
//	   cache_po, cache_fc, cache_acode, cache_gcode, cache_tr, cache_smc);

		return;
	}

	int wcc, gc, pn, ssn, usid, ac, mid, setid, subsetid, supersetid, 
		 po, fc, acode, gcode;
	char *tr, *smc;

   gc = p_genderCode->AsIntegerFromString();
   pn = p_sourcePatNumber->AsInteger();
   ssn = p_sourceStemNumber->AsInteger();
   usid = p_usageID->AsInteger();
   ac = p_auxiliaryCode->AsIntegerFromString();
   wcc = p_wordClassCode->AsIntegerFromString();
   po = p_priorityOrder->AsInteger();
   fc = p_formCode->AsIntegerFromString();
   mid = p_meaningID->AsInteger();
   setid = p_setID->AsInteger();
   subsetid = p_subSetID->AsInteger();
   supersetid = p_superSetID->AsInteger();
   acode = p_atomicCode->AsIntegerFromString();
   gcode = p_genericCode->AsIntegerFromString();

   p->setGenderCode(p_genderCode->AsIntegerFromString());
   p->setPatNumber(p_sourcePatNumber->AsInteger());
   p->setSourceStemNumber(p_sourceStemNumber->AsInteger());
   p->setUsageID(p_usageID->AsInteger());
   p->setAuxiliaryCode(p_auxiliaryCode->AsIntegerFromString());
   p->setWordClassCode(p_wordClassCode->AsIntegerFromString());
   p->setPriorityOrder(p_priorityOrder->AsInteger());
   p->setFormCode(p_formCode->AsIntegerFromString());
   p->setMeaningID(p_meaningID->AsInteger());
   p->setSetID(p_setID->AsInteger());
   p->setSubSetID(p_subSetID->AsInteger());
   p->setSuperSetID(p_superSetID->AsInteger());
   p->setUsageID(p_usageID->AsInteger());
   p->setAtomicCode(p_atomicCode->AsIntegerFromString() );		// TO BE REMOVED WHEN NEW SMC IS TESTED
   p->setGenericCode(p_genericCode->AsIntegerFromString());	// TO BE REMOVED WHEN NEW SMC IS TESTED
   (p_transitivity->AsString() == "T")? p->setTransitivity(true): p->setTransitivity(false);
   SubjectMatterCode theSMC(p_subjectMatterCode->AsString());
   p->setSubjectMatterCode(theSMC);
//printf("\nQQQ %d: %d %d %d %d %d %d %d %d %d %d %d %d %d %d \"%s\" \"%s\"\n",
//	   v_inputWordId, wcc, gc, pn, ssn, usid, 
//	   ac, mid, setid, subsetid, supersetid, 
//	   po, fc, acode, gcode, p_transitivity->AsString().c_str(), 
//	   p_subjectMatterCode->AsString().c_str());
}

