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
// File - TargetWordQuery.cpp (implementation)
// --------------------------------------------------------------------------
#include <logos_include/logoscommon.h>
#include <logos_libs/gerdem/targetwordquery.h>
#include <logos_libs/sql/sqlstatement.h>
#include <logos_libs/sql/sqlcolumn.h>
#include <logos_libs/sql/sqlexception.h>
#include <logos_libs/linguistic/lsemantosyntacticunit.h>
#include <logos_libs/linguistic/llanguage.h>

#include <logos_libs/dbcache/CacheTargetWordData.h>
#include <logos_libs/dbcache/CacheTargetWordQuery.h>

// --------------------------------------------------------------------------
// Default constructor
// --------------------------------------------------------------------------
TargetWordQuery::TargetWordQuery()
{
	mdata = NULL;
	mquery = NULL;
}

// --------------------------------------------------------------------------
// Destructor
// --------------------------------------------------------------------------
TargetWordQuery::~TargetWordQuery()
{
		if(mdata) { delete mdata; mdata = NULL; }
		if(mquery) { delete mquery; mquery = NULL; }
}

// --------------------------------------------------------------------------
// Open connection to database
// --------------------------------------------------------------------------
void TargetWordQuery::Open(SqlConnection* aConnection, const LLanguage& targetLanguage)
{
	targetLanguageCode = targetLanguage.id();

	mdata = new CacheTargetWordData(NULL, targetLanguageCode, false, false, false);
	if(mdata->isValid()) {
		mquery = new CacheTargetWordQuery(mdata, NULL, 0);
		return;
	} else {
		delete mdata;
		mdata = NULL;
	}

   LgsString s = "select tw.word "
              "from morphology tm, word_phrase tw, transfer t "
              "where t.company_code = :aCompanyCode "
              "and t.meaning_id = :aMeaningID "
              "and t.target_language_code = :aTargetLanguage "
              "and t.alternate_sequence IS NULL "
              "and tm.company_code = t.company_code "
              "and tm.language_code = t.target_language_code "
              "and tm.usage_id = t.target_usage_id "
              "and tw.company_code = tm.company_code "
              "and tw.language_code = tm.language_code "
              "and tw.word_id = tm.word_id";

   try
   {
      SqlQuery::Open(aConnection);

      Statement()->AddToCommandString(s);

      Statement()->Parse();

      targetLanguageCode = targetLanguage.id();

      m_word = Statement()->BindOutputColumn(1, SqlColumn::StringType);
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
void TargetWordQuery::Close()
{
	if(mdata==NULL)
   SqlQuery::Close();
}

// --------------------------------------------------------------------------
void TargetWordQuery::FetchTargetWord(LSemantoSyntacticUnit& ssu)
{
   char inputMeaningID[10];
   char languageID[10];

	if(mdata) {
		char buffer[80];
		mquery->query((char *)ssu.companyCode().c_str(), ssu.meaningID());
		if(mquery->fetch(buffer)) {
			ssu.setTargetWord(buffer);
		} else {
			ssu.setTargetWord("");
		}
		return;
	}


   try
   {
		Statement()->BindInputString(":aCompanyCode", ssu.companyCode().c_str());
      Statement()->BindInputIntToString(":aMeaningID", "%d", inputMeaningID, ssu.meaningID());
		Statement()->BindInputIntToString(":aTargetLanguage", "%02d", languageID, targetLanguageCode);
      Statement()->Execute();

      if (Statement()->Fetch())
      {
         ssu.setTargetWord(m_word->AsString());
      }
   }
   catch(SqlException& x)
   {
      cout << x.Message() << endl;
      throw(x);
   }
}
