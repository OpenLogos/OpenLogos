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
// File - GFactory.cpp (implementation)
// --------------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/gerdem/gfactory.h>
#include <logos_libs/utility/argumentexception.h>
#include <logos_libs/sql/sqlstatement.h>
#include <logos_libs/sql/sqlcolumn.h>
#include <logos_libs/sql/sqlexception.h>
#include <logos_libs/linguistic/ldictionaryentry.h>
#include <logos_libs/linguistic/lsubjectmatter.h>
#include <logos_libs/linguistic/targetsentenceunit.h>
#include <logos_libs/linguistic/translatedsentenceunit.h>
#include <logos_libs/linguistic/constantsentenceunit.h>
#include <logos_libs/elision/spanish/factory_sp.h>
#include <logos_libs/elision/portuguese/factory_pt.h>
#include <logos_libs/elision/german/factory.h>
#include <logos_libs/elision/italian/factory_it.h>
#include <logos_libs/elision/french/factory_fr.h>
#include <logos_libs/elision/english/factory.h>

#include <logos_libs/dbcache/CacheCompanyData.h>
#include <logos_libs/dbcache/CacheCompanyQuery.h>
#include <logos_libs/dbcache/CacheInflectionData.h>
#include <logos_libs/dbcache/CacheInflectionQuery.h>
#include <logos_libs/dbcache/CacheDerivedFormData.h>
#include <logos_libs/dbcache/CacheDerivedFormQuery.h>

//--------------------------------------------------------------------------
// Constructor
//--------------------------------------------------------------------------
GFactory::GFactory(SqlConnection* dbms)
	: p_connection(dbms)
{
}


//--------------------------------------------------------------------------
// Destructor.
// The factory simply unwinds what it did in its Connect and Setup methods.
//--------------------------------------------------------------------------
GFactory::~GFactory()
{
    v_phraseQuery.Close();
//    v_phraseIdQuery.Close();
    v_wordQuery.Close();
    v_wordCapQuery.Close();
    v_ssuQuery.Close();
    v_transferCountQuery.Close();
    v_constantQuery.Close();
    v_constantQueryNoCC.Close();
    v_transferQuery.Close();
    v_targetQuery.Close();
    v_irregularStemQuery.Close();
    v_targetWordQuery.Close();
    v_stemBuilder.close();
    v_wordInPhraseQuery.Close();
}


//--------------------------------------------------------------------------
// The factory contains a set of SqlQuery objects. Each of these objects needs to be opened. 
// The open actually writes the SQL and parses the statement.
//--------------------------------------------------------------------------
void GFactory::Open(const LDictionary* dictionary)
{
   LinguisticFactory::Open(dictionary);
   try
   {
      v_phraseQuery.Open(p_connection, Dictionary().sourceLanguage());
//      v_phraseIdQuery.Open(p_connection, Dictionary().sourceLanguage());
      v_wordQuery.Open(p_connection, Dictionary().sourceLanguage());
      v_wordCapQuery.Open(p_connection, Dictionary().sourceLanguage());
      v_ssuQuery.Open(p_connection, Dictionary().sourceLanguage(), Dictionary().targetLanguage());
      v_transferCountQuery.Open(p_connection, Dictionary().targetLanguage().id());
      v_constantQuery.Open(p_connection, Dictionary().targetLanguage());
      v_constantQueryNoCC.Open(p_connection, Dictionary().targetLanguage(), false);
      v_transferQuery.Open(p_connection, Dictionary().targetLanguage());
      v_targetQuery.Open(p_connection, Dictionary().targetLanguage());
      v_irregularStemQuery.Open(p_connection, Dictionary().targetLanguage());
      v_targetWordQuery.Open(p_connection, Dictionary().targetLanguage());
      v_stemBuilder.open(p_connection, Dictionary().targetLanguage());
      v_wordInPhraseQuery.Open(p_connection);
   }
   catch(SqlException& x)
   {
      cerr << x.Message() << endl;
      throw(x);
   }
}

//--------------------------------------------------------------------------
// This is a DBMS query that does not use a SqlQuery object. It is only performed once at the 
// top of the program. It creates Atomic objects from atomic code and generic code data.
//--------------------------------------------------------------------------
void GFactory::CompleteSubjectMatter(const LgsString & genericCode,
                                     LSubjectMatterVector* pSMVector)
{
   try
   {
      SqlStatement* pStatement = p_connection->CreateStatement();

      LgsString s = "select Atomic_Code "
                 "from Atomic "
                 "where Generic_Code = :aCode "
                 "order by Search_Sequence";
      pStatement->AddToCommandString(s);
      pStatement->ParseDeferred();
      SqlColumn* pAtomic = pStatement->BindOutputColumn(1,SqlColumn::StringType);
      int code = atoi(genericCode.c_str());
      char inputCode[20];
      pStatement->BindInputIntToString(":aCode","%02d",inputCode,code);
      pStatement->Execute();
      while (pStatement->Fetch())
      {
         LSubjectMatter smc(code, pAtomic->AsIntegerFromString());
         pSMVector->push_back(smc);
      }
      delete pStatement;
   }
   catch(SqlException& x)
   {
      cout << x.Message() << endl;
      throw (x);
   }
}

//--------------------------------------------------------------------------
// This is a DBMS query that does not use a SqlQuery object. It is only performed once at the 
// top of the program. It creates Atomic objects from atomic code and generic code data.
//--------------------------------------------------------------------------
void GFactory::CompleteSubjectMatterVector(LSubjectMatterVector* smcVector)
{
   try
   {
      SqlStatement* pStatement = p_connection->CreateStatement();

      LgsString s = "select Generic_Code, Atomic_Code"
                 " from Atomic order by Search_Sequence";

      pStatement->AddToCommandString(s);
      pStatement->ParseDeferred();

      SqlColumn* pGeneric = pStatement->BindOutputColumn(1, SqlColumn::StringType);
      SqlColumn* pAtomic = pStatement->BindOutputColumn(2, SqlColumn::StringType);


      pStatement->Execute();

      while (pStatement->Fetch())
      {
         LSubjectMatter smc(pGeneric->AsIntegerFromString(), pAtomic->AsIntegerFromString());
         smcVector->push_back(smc);
      }

      delete pStatement;
   }
   catch(SqlException& x)
   {
      cout << x.Message() << endl;
      throw( x );
   }
}

//--------------------------------------------------------------------------
// This is a DBMS query that does not use a SqlQuery object. It is only performed once at the 
// top of the program. It fetches all of company records and converts the data into objects.
//--------------------------------------------------------------------------
void GFactory::CompleteCompany(DCompany* pCompany)
{

   try
   {
      SqlStatement* pStatement = p_connection->CreateStatement();

      LgsString s = "select Description, Restrict_Switch"
                 " from Company"
                 " where Company_Code = :aCode";

      pStatement->AddToCommandString(s);
      pStatement->ParseDeferred();

      SqlColumn* pDescription = pStatement->BindOutputColumn(1, SqlColumn::StringType);
      SqlColumn* pRestrict = pStatement->BindOutputColumn(2, SqlColumn::StringType);

      pStatement->BindInputString(":aCode", pCompany->CompanyCode().c_str());

      pStatement->Execute();

      pCompany->SetDescription(pDescription->AsString());
      pCompany->SetRestrictSwitch(pRestrict->AsBoolean());

      delete pStatement;
   }
   catch(SqlException& x)
   {
      cout << x.Message() << endl;
      throw( x );
   }
}

//--------------------------------------------------------------------------
// This is a DBMS query that does not use a SqlQuery object. It is only performed once at the top 
// of the program. It fetches all of company records and converts the data into objects.
//--------------------------------------------------------------------------
void GFactory::CompleteCompanyVector(DCompanyVector* pCompanyVector)
{

// try cache in shared memory first
	CacheCompanyData *ccd = new CacheCompanyData(NULL, 0, false, false, false);
	if(ccd->isValid()) {
		char cc[4], desc[201], rs[2];
		CacheCompanyQuery *q = new CacheCompanyQuery(ccd);
		if(q->query()) {
			while(q->fetch(cc, desc, rs)) {
				DCompany company;
				company.SetCompanyCode(cc);
				company.SetDescription(desc);
				bool rsw = (rs[0]=='Y');
				company.SetRestrictSwitch(rsw);
				pCompanyVector->push_back(company);
			}
			delete q;
			delete ccd;
			return;
		}
	}
	delete ccd;

	try
   {
      SqlStatement* pStatement = p_connection->CreateStatement();
      LgsString s = "select Company_Code, Description, Restrict_Switch "
                 "from Company order by Search_Sequence";

      pStatement->AddToCommandString( s );
      pStatement->ParseDeferred();
      SqlColumn* pCompanyCode = pStatement->BindOutputColumn(1, SqlColumn::StringType);
      SqlColumn* pDescription = pStatement->BindOutputColumn(2, SqlColumn::StringType);
      SqlColumn* pRestrict = pStatement->BindOutputColumn(3, SqlColumn::StringType);
      pStatement->Execute();
      while (pStatement->Fetch())
      {
         DCompany company;
         company.SetCompanyCode(pCompanyCode->AsString());
         company.SetDescription(pDescription->AsString());
         company.SetRestrictSwitch(pRestrict->AsBoolean());
         pCompanyVector->push_back(company);
      }

      delete pStatement;
   }
   catch(SqlException& x)
   {
      cout << x.Message() << endl;
      throw( x );
   }
}

//--------------------------------------------------------------------------
bool GFactory::validDictionaryToken(LDictionaryToken& token, bool isPhrase)
{
   //bool result = false;

/* 
By commenting out this chunk we try to 
eliminate PhraseIdQuery and move fetch below into
initial PhraseQuery. There
is no performance degradation even in the case of direct
database connection (not mentioning running w/ CacheMgr).
   if (isPhrase)
   {
      v_phraseIdQuery.SetCompanyCode(token.wordPhrase().CompanyCode());
      v_phraseIdQuery.ExecuteWithMatch(token.wordPhrase().WordID());
      if (!v_phraseIdQuery.FetchIntoWordMatch(&(token.wordPhrase())))
      {
         throw ("Database Index Error!");
      }
   }
*/

   //v_ssuQuery.ExecuteWithMatch(token.wordPhrase());
   //for (;;)
   //{
   //   if (v_ssuQuery.fetch())
   //   {
   //      result = true;
   //   }
   //   else
   //   {
   //      break;
   //   }
   //}
   return true;
}

//--------------------------------------------------------------------------
// This member function actually queries the dictionary for all meaning and morphology data 
// that are related to a given word.
// +++ added the BOS flag (for German source only) - but nothing yet implemented
// What should happen is that if BOS is false and the source word begins with a capital
// letter, then for German only only word classes 01, 04 should be considered.
//--------------------------------------------------------------------------
void GFactory::completeEntryComponents(LDictionaryEntry* pEntry,
                                       LDictionaryTokenVector& matches)
{
   LDictionaryTokenIterator endOfMatches = matches.end();
   for( LDictionaryTokenIterator i = matches.begin(); i != endOfMatches; i++ )
   {
      v_ssuQuery.ExecuteWithMatch(i->wordPhrase());
      for (;;)
      {
         LSemantoSyntacticUnit ssu;

         if (v_ssuQuery.fetch())
         {
            LSemantoSyntacticUnit & newSsu = pEntry->addSsu(ssu);

            newSsu.setCompanyCode(i->wordPhrase().CompanyCode());
            newSsu.setWordID(i->wordPhrase().WordID());
            newSsu.setWordCount(i->wordPhrase().WordCount());
            newSsu.setWord(i->wordPhrase().Word());
            newSsu.setWordTypeCode(i->wordPhrase().WordTypeCode());
            newSsu.setProtectionCode(i->wordPhrase().ProtectionCode());
            newSsu.setHashCode1(i->wordPhrase().HashCode1());
            newSsu.setHashCode2(i->wordPhrase().HashCode2());
			newSsu.setRootHashCode1(i->wordPhrase().rootHashCode1());
			newSsu.setRootHashCode2(i->wordPhrase().rootHashCode2());
			newSsu.setHenNum1(i->wordPhrase().henNum1());
			newSsu.setHenNum2(i->wordPhrase().henNum2());
			newSsu.setRootHenNum1(i->wordPhrase().rootHenNum1());
			newSsu.setRootHenNum2(i->wordPhrase().rootHenNum2());
            newSsu.setHashLocation(i->wordPhrase().HashLocation());
            newSsu.setHeadWord(i->wordPhrase().HeadWord());
            newSsu.setBlackHoleLocation(i->wordPhrase().BlackHoleLocation());
            newSsu.setWildcardPosition(i->wordPhrase().WildcardPosition());
            newSsu.setIsAspire(i->wordPhrase().IsAspire());
            newSsu.setIsMonth(i->wordPhrase().IsMonth());
            newSsu.setIsPrefix(i->wordPhrase().IsPrefix());
            newSsu.setIsNeverEos(i->wordPhrase().IsNeverEos());
            newSsu.setIsOrdinal(i->wordPhrase().IsOrdinal());
            if (StringUtil::isAllUpperCase(i->wordPhrase().AsString()))
            {
               newSsu.setMatchedOnFullyCapped(true);
            }

            v_ssuQuery.setSsu(&newSsu);
            v_targetWordQuery.FetchTargetWord(newSsu);
         }
         else
         {
            break;
         }
      }
   }
}

//--------------------------------------------------------------------------
// This member function actually queries the dictionary for all meaning and morphology data 
// that are related to a given word.
//--------------------------------------------------------------------------
void GFactory::completePhraseComponents(LDictionaryEntry* pEntry,
                                        LDictionaryTokenVector& matches)
{
   LDictionaryTokenIterator endOfMatches = matches.end();
   for (LDictionaryTokenIterator i = matches.begin(); i != endOfMatches; i++)
   {
      //v_phraseIdQuery.SetCompanyCode(i->wordPhrase().CompanyCode());
      //v_phraseIdQuery.ExecuteWithMatch(i->wordPhrase().Word());
      //if (!v_phraseIdQuery.FetchIntoWordMatch(&(i->wordPhrase())))
      //{
      //   throw ("Database Index Error!");
      //}
      v_ssuQuery.ExecuteWithMatch(i->wordPhrase());
      for (;;)
      {
         LSemantoSyntacticUnit ssu;

         if (v_ssuQuery.fetch())
         {
            LSemantoSyntacticUnit & newSsu = pEntry->addSsu(ssu);

            newSsu.setCompanyCode(i->wordPhrase().CompanyCode());
            newSsu.setWordID(i->wordPhrase().WordID());
            newSsu.setWordCount(i->wordPhrase().WordCount());
            newSsu.setWord(i->wordPhrase().Word());
            newSsu.setWordTypeCode(i->wordPhrase().WordTypeCode());
            newSsu.setProtectionCode(i->wordPhrase().ProtectionCode());
            newSsu.setHashCode1(i->wordPhrase().HashCode1());
            newSsu.setHashCode2(i->wordPhrase().HashCode2());
			newSsu.setRootHashCode1(i->wordPhrase().rootHashCode1());
			newSsu.setRootHashCode2(i->wordPhrase().rootHashCode2());
			newSsu.setHenNum1(i->wordPhrase().henNum1());
			newSsu.setHenNum2(i->wordPhrase().henNum2());
			newSsu.setRootHenNum1(i->wordPhrase().rootHenNum1());
			newSsu.setRootHenNum2(i->wordPhrase().rootHenNum2());
            newSsu.setHashLocation(i->wordPhrase().HashLocation());
            newSsu.setHeadWord(i->wordPhrase().HeadWord());
            newSsu.setBlackHoleLocation(i->wordPhrase().BlackHoleLocation());
            newSsu.setWildcardPosition(i->wordPhrase().WildcardPosition());
            newSsu.setIsAspire(i->wordPhrase().IsAspire());
            newSsu.setIsMonth(i->wordPhrase().IsMonth());
            newSsu.setIsPrefix(i->wordPhrase().IsPrefix());
            newSsu.setIsNeverEos(i->wordPhrase().IsNeverEos());
            newSsu.setIsOrdinal(i->wordPhrase().IsOrdinal());
            if (StringUtil::isAllUpperCase(i->wordPhrase().AsString()))
            {
               newSsu.setMatchedOnFullyCapped(true);
            }

            v_ssuQuery.setSsu(&newSsu);
            v_targetWordQuery.FetchTargetWord(newSsu);
         }
         else
         {
            break;
         }
      }
   }
}

//--------------------------------------------------------------------------
/* replacing vector w/ map
DDerivedFormVector* GFactory::CreateDerivedForms(int languageCode)
{
   //-----------------------------------------------------------------
   // This is a one time query to load all the derived forms for a
   // given language. They are loaded once at the top of the program.
   // A SqlQuery object is not necessary to do this query.
   //-----------------------------------------------------------------

   DDerivedFormVector* pVector = new DDerivedFormVector;

// try cache in shared memory first
	CacheDerivedFormData *cdfd = new CacheDerivedFormData(NULL, 
		languageCode, false, false, false);
	if(cdfd->isValid()) {
		char dfi[61], endng[32];
		int pn, sn, fc, wcc;
		CacheDerivedFormQuery *q = new CacheDerivedFormQuery(cdfd);
		if(q->query()) {
			while(q->fetch(dfi, &pn, &sn, endng, &wcc, &fc)) {
				DDerivedForm derivedForm;
//printf("dfi=\"%s\", lc=%d, pn=%d, sn=%d endng=\"%s\", wcc=%d, fc=%d\n",
//	   dfi, languageCode, pn, sn,
//	   endng, wcc, fc);

				derivedForm.SetDerivedFormID(dfi);
				derivedForm.SetLanguageCode(languageCode);
				derivedForm.SetPatNumber(pn);
				derivedForm.SetStemNumber(sn);
				derivedForm.SetEnding(endng);
				derivedForm.SetWordClassCode(wcc);
				derivedForm.SetFormCode(fc);

				pVector->push_back(derivedForm);
			}
			delete q;
			delete cdfd;
			return pVector;
		}
	}
	delete cdfd;

	try
   {
      SqlStatement* pStatement = p_connection->CreateStatement();

      LgsString s = "select Derived_Form_ID, Pat_Number,"
                 " Stem_Number, Ending, Word_Class_Code, Form_Code"
                 " from Derived_Form"
                 " where Language_Code = :aCode";

      pStatement->AddToCommandString(s);
      pStatement->ParseDeferred();

      char languageBuffer[10];
      pStatement->BindInputIntToString(":aCode", "%02d", languageBuffer, languageCode);

      SqlColumn* pId = pStatement->BindOutputColumn(1, SqlColumn::StringType);
      SqlColumn* pTn = pStatement->BindOutputColumn(2, SqlColumn::Integer);
      SqlColumn* pSn = pStatement->BindOutputColumn(3, SqlColumn::Integer);
      SqlColumn* pEn = pStatement->BindOutputColumn(4, SqlColumn::StringType);
      SqlColumn* pWc = pStatement->BindOutputColumn(5, SqlColumn::StringType);
      SqlColumn* pFc = pStatement->BindOutputColumn(6, SqlColumn::StringType);

      pStatement->Execute();

      while (pStatement->Fetch())
      {
         DDerivedForm derivedForm;

//printf("dfi=\"%s\", lc=%d, pn=%d, sn=%d endng=\"%s\", wcc=%d, fc=%d\n",
//	   pId->AsString().c_str(), languageCode, pTn->AsInteger(), pSn->AsInteger(),
//	   pEn->AsString().c_str(), pWc->AsIntegerFromString(),
//		pFc->AsIntegerFromString());

         derivedForm.SetDerivedFormID(pId->AsString());
         derivedForm.SetLanguageCode(languageCode);
         derivedForm.SetPatNumber(pTn->AsInteger());
         derivedForm.SetStemNumber(pSn->AsInteger());
         derivedForm.SetEnding(pEn->AsString());
         derivedForm.SetWordClassCode(pWc->AsIntegerFromString());
         derivedForm.SetFormCode(pFc->AsIntegerFromString());

         pVector->push_back(derivedForm);
      }
      delete pStatement;
   }
   catch(SqlException& x)
   {
      cout << x.Message() << endl;
      throw(x);
   }
   return pVector;
}
*/

DDerivedFormMap* GFactory::CreateDerivedFormsMap(int languageCode)
{
   //-----------------------------------------------------------------
   // This is a one time query to load all the derived forms for a
   // given language. They are loaded once at the top of the program.
   // A SqlQuery object is not necessary to do this query.
   //-----------------------------------------------------------------

   DDerivedFormMap *pMap = new DDerivedFormMap;

// try cache in shared memory first
	CacheDerivedFormData *cdfd = new CacheDerivedFormData(NULL, 
		languageCode, false, false, false);
	if(cdfd->isValid()) {
		char dfi[61], endng[32];
		int pn, sn, fc, wcc;
		CacheDerivedFormQuery *q = new CacheDerivedFormQuery(cdfd);
		if(q->query()) {
			while(q->fetch(dfi, &pn, &sn, endng, &wcc, &fc)) {
				DDerivedForm derivedForm;
//printf("dfi=\"%s\", lc=%d, pn=%d, sn=%d endng=\"%s\", wcc=%d, fc=%d\n",
//	   dfi, languageCode, pn, sn,
//	   endng, wcc, fc);

				derivedForm.SetDerivedFormID(dfi);
				derivedForm.SetLanguageCode(languageCode);
				derivedForm.SetPatNumber(pn);
				derivedForm.SetStemNumber(sn);
				derivedForm.SetEnding(endng);
				derivedForm.SetWordClassCode(wcc);
				derivedForm.SetFormCode(fc);

				DerivedFormKey key(pn, sn, endng);
				pMap->insert(DDerivedFormMap::value_type(key, derivedForm));
			}
			delete q;
			delete cdfd;
			return pMap;
		}
	}
	delete cdfd;

	try
   {
      SqlStatement* pStatement = p_connection->CreateStatement();

      LgsString s = "select Derived_Form_ID, Pat_Number,"
                 " Stem_Number, Ending, Word_Class_Code, Form_Code"
                 " from Derived_Form"
                 " where Language_Code = :aCode";

      pStatement->AddToCommandString(s);
      pStatement->ParseDeferred();

      char languageBuffer[10];
      pStatement->BindInputIntToString(":aCode", "%02d", languageBuffer, languageCode);

      SqlColumn* pId = pStatement->BindOutputColumn(1, SqlColumn::StringType);
      SqlColumn* pTn = pStatement->BindOutputColumn(2, SqlColumn::Integer);
      SqlColumn* pSn = pStatement->BindOutputColumn(3, SqlColumn::Integer);
      SqlColumn* pEn = pStatement->BindOutputColumn(4, SqlColumn::StringType);
      SqlColumn* pWc = pStatement->BindOutputColumn(5, SqlColumn::StringType);
      SqlColumn* pFc = pStatement->BindOutputColumn(6, SqlColumn::StringType);

      pStatement->Execute();

      while (pStatement->Fetch())
      {
         DDerivedForm derivedForm;

//printf("dfi=\"%s\", lc=%d, pn=%d, sn=%d endng=\"%s\", wcc=%d, fc=%d\n",
//	   pId->AsString().c_str(), languageCode, pTn->AsInteger(), pSn->AsInteger(),
//	   pEn->AsString().c_str(), pWc->AsIntegerFromString(),
//		pFc->AsIntegerFromString());

         derivedForm.SetDerivedFormID(pId->AsString());
         derivedForm.SetLanguageCode(languageCode);
         derivedForm.SetPatNumber(pTn->AsInteger());
         derivedForm.SetStemNumber(pSn->AsInteger());
         derivedForm.SetEnding(pEn->AsString());
         derivedForm.SetWordClassCode(pWc->AsIntegerFromString());
         derivedForm.SetFormCode(pFc->AsIntegerFromString());

		DerivedFormKey key(pTn->AsInteger(), pSn->AsInteger(), 
			pEn->AsString().c_str());
		pMap->insert(DDerivedFormMap::value_type(key, derivedForm));
      }
      delete pStatement;
   }
   catch(SqlException& x)
   {
      cout << x.Message() << endl;
      throw(x);
   }
   return pMap;
}

//--------------------------------------------------------------------------
LInflectionVector* GFactory::createInflections(int languageCode)
{
   //-----------------------------------------------------------------
   // This is a DBMS query that does not use a SqlQuery object. It is
   // only performed once at the top of the program. It creates all of
   // inflections that are related to a given language.
   //-----------------------------------------------------------------

   LInflectionVector* pVector = new LInflectionVector;

// try cache in shared memory first
	CacheInflectionData *idata = new CacheInflectionData(NULL, languageCode, 
		false, false, false);
	if(idata->isValid()) {
		char ending[32];
		int remseq;
		CacheInflectionQuery *q = new CacheInflectionQuery(idata);
		if(q->query()) {
			while(q->fetch(ending, &remseq)) {
				LInflection inflection(ending, remseq);
				pVector->push_back(inflection);
			}
			delete q;
			delete idata;
			return pVector;
		}
	}
	delete idata;

   try
   {
      SqlStatement* pStatement = p_connection->CreateStatement();

      LgsString s = "select Ending, Removal_Sequence "
                 " from Inflection"
                 " where Language_Code = :aCode"
                 " and Removal_Sequence is not null"
                 " order by Removal_Sequence";

      pStatement->AddToCommandString(s);
      pStatement->ParseDeferred();

      char languageBuffer[10];
      pStatement->BindInputIntToString(":aCode", "%02d", languageBuffer, languageCode);

      SqlColumn* pEnding = pStatement->BindOutputColumn(1, SqlColumn::StringType);
      SqlColumn* pSequence = pStatement->BindOutputColumn(2, SqlColumn::Integer);

      pStatement->Execute();

      while (pStatement->Fetch())
      {
         LInflection inflection(pEnding->AsString(), pSequence->AsInteger());
         pVector->push_back(inflection);
      }
      delete pStatement;
   }
   catch(SqlException& x)
   {
      cout << x.Message() << endl;
      throw(x);
   }
   return pVector;
}

//--------------------------------------------------------------------------
DWordPhraseVector* GFactory::CreatePhraseMatches(const LgsString& firstWord, short firstSpaceCount,
                                                 const LgsString& nextWord)
{
   //-----------------------------------------------------------------
   // This member function actually queries the dictionary for all
   //    word/phrases that begin with the "wordString" arguement.
   //-----------------------------------------------------------------

   DWordPhraseVector* p_wordMatches = new DWordPhraseVector;
   if (!firstWord.empty())
   {
      DWordPhrase wordMatch;
      v_phraseQuery.ExecuteWithMatch(firstWord, firstSpaceCount,
									 nextWord);
      while( v_phraseQuery.FetchIntoWordMatch(&wordMatch))
      {
         p_wordMatches->push_back(wordMatch);
      }
   }
   return p_wordMatches;
}

//--------------------------------------------------------------------------
DWordPhraseVector* GFactory::CreatePhraseCapMatches(const LgsString& firstWord, short firstSpaceCount,
                                                    const LgsString& nextWord)
{
   //-----------------------------------------------------------------
   // This member function actually queries the dictionary for all
   //    word/phrases that begin with the "wordString" arguement.
   //-----------------------------------------------------------------

   DWordPhraseVector* p_wordMatches = new DWordPhraseVector;
   if (!firstWord.empty())
   {
      // First try with the original phrase.
      DWordPhrase wordMatch;
      v_phraseQuery.ExecuteWithMatch(firstWord, firstSpaceCount, nextWord);
      while (v_phraseQuery.FetchIntoWordMatch(&wordMatch))
      {
         p_wordMatches->push_back(wordMatch);
      }

      // Second try with first word lower cased.
      LgsString lower = firstWord;
      StringUtil::toLower(lower);
      v_phraseQuery.ExecuteWithMatch(lower, firstSpaceCount, nextWord);
      while (v_phraseQuery.FetchIntoWordMatch(&wordMatch))
      {
         p_wordMatches->push_back(wordMatch);
      }

      // Third try with first and second word lower cased.
      LgsString nextLower = nextWord;
      bool bNextIsChanged = false;
      if (!nextLower.empty() && StringUtil::containsUpperCase(nextLower))
      {
         bNextIsChanged = true;
         StringUtil::toLower(nextLower);
         v_phraseQuery.ExecuteWithMatch(lower, firstSpaceCount, nextLower);
         while (v_phraseQuery.FetchIntoWordMatch(&wordMatch))
         {
            p_wordMatches->push_back(wordMatch);
         }
      }

      // Finally, if the first word is all upper case
      if (StringUtil::isAllUpperCase(firstWord))
      {
         // Try with first original word and second word lower cased.
         if (bNextIsChanged)
         {
            v_phraseQuery.ExecuteWithMatch(firstWord, firstSpaceCount, nextLower);
            while (v_phraseQuery.FetchIntoWordMatch(&wordMatch))
            {
               p_wordMatches->push_back(wordMatch);
            }
         }

         // Upper case only the first letter of both the first and second word.
         lower[0] = CharUtil::upper(lower[0]);
         if (!nextLower.empty())
         {
            nextLower[0] = CharUtil::upper(nextLower[0]);
         }
         v_phraseQuery.ExecuteWithMatch(lower, firstSpaceCount, nextLower);
         while (v_phraseQuery.FetchIntoWordMatch(&wordMatch))
         {
            p_wordMatches->push_back(wordMatch);
         }
      }
   }
   return p_wordMatches;
}

//--------------------------------------------------------------------------
DWordPhraseVector* GFactory::CreateWordMatches(const LgsString& wordString)
{
   //-----------------------------------------------------------------
   // This member function actually queries the dictionary for all
   // word/phrases that begin with the "wordString" arguement.
   //-----------------------------------------------------------------

   DWordPhraseVector* p_wordMatches = new DWordPhraseVector;
   if (!wordString.empty())
   {
      DWordPhrase wordMatch;
      v_wordQuery.BuildInputWords(wordString.c_str());
      v_wordQuery.ExecuteWithMatch();
      while (v_wordQuery.FetchIntoWordMatch(&wordMatch))
      {
         wordMatch.Word(wordString);
         p_wordMatches->push_back(wordMatch);
      }
   }
   return p_wordMatches;
}

//--------------------------------------------------------------------------
DWordPhraseVector* GFactory::CreateWordCapMatches(const LgsString& wordString)
{
   //-----------------------------------------------------------------
   // This member function actually queries the dictionary for all
   //    word/phrases that begin with the "wordString" arguement.
   //-----------------------------------------------------------------

   DWordPhraseVector* p_wordMatches = new DWordPhraseVector;
   if (!wordString.empty())
   {
      DWordPhrase wordMatch;
      v_wordCapQuery.BuildInputWords(wordString.c_str());
      v_wordCapQuery.ExecuteWithMatch();
      while (v_wordCapQuery.FetchIntoWordMatch(&wordMatch))
      {
         p_wordMatches->push_back(wordMatch);
      }
   }
   return p_wordMatches;
}

//--------------------------------------------------------------------------
bool GFactory::IsSsuTransferred(const LSemantoSyntacticUnit& ssu)
{
   bool isTransferred = true;

   if (v_transferCountQuery.TransferCount(ssu.meaningID(), ssu.companyCode()) == 0)
   {
      isTransferred = false;
   }
   return isTransferred;
}

//--------------------------------------------------------------------------
void GFactory::getAlternateTransfers(TranslatedSentenceUnit* pUnit)
{
   Transfer transfer;
   v_transferQuery.executeWithMeaningID(pUnit->sourceMeaningID(), pUnit->companyCode());

   while (v_transferQuery.fetchTransfer(&transfer))
   {
      pUnit->addTransfer(transfer);
   }
}

//--------------------------------------------------------------------------
LDictionaryEntry* GFactory::createTargetEntry(TargetDictionaryUnit* pUnit)
{
   LDictionaryEntry* pEntry = 0;

   v_targetQuery.executeWithUsageID(pUnit->transferID());

   if (v_targetQuery.fetch())
   {
      pEntry = new LDictionaryEntry();
      LSemantoSyntacticUnit ssu;

      LSemantoSyntacticUnit& newSsu = pEntry->addSsu(ssu);

      v_targetQuery.setSsu(&newSsu);
      if ((Dictionary().targetLanguage().id() == LLanguage::GermanID) &&
          (pUnit->number() == 2) && v_irregularStemQuery.isIrregulalrStemPat(newSsu.patNumber()))
      {
         v_irregularStemQuery.executeWithUsageID(pUnit->transferID(), pUnit->companyCode());
         v_irregularStemQuery.fetch(&newSsu);
      }
   }
   return pEntry;
}

//--------------------------------------------------------------------------
void GFactory::getHighAlternateTransfers(ConstantSentenceUnit* pUnit)
{
   v_constantQuery.ExecuteWithConstantID(pUnit->constantID(), pUnit->companyCode(), "H");
   if (!v_constantQuery.fetchTransfers(pUnit))
   {
      v_constantQueryNoCC.ExecuteWithConstantID(pUnit->constantID(), pUnit->companyCode(), "H");
      v_constantQueryNoCC.fetchTransfers(pUnit);
   }
}

//--------------------------------------------------------------------------
void GFactory::getLowAlternateTransfers(ConstantSentenceUnit* pUnit)
{
   v_constantQuery.ExecuteWithConstantID(pUnit->constantID(), pUnit->companyCode(), "L");
   if (!v_constantQuery.fetchTransfers(pUnit))
   {
      v_constantQueryNoCC.ExecuteWithConstantID(pUnit->constantID(), pUnit->companyCode(), "L");
      v_constantQueryNoCC.fetchTransfers(pUnit);
   }
}

//--------------------------------------------------------------------------
void GFactory::generateStem(LgsString* stem, const TargetSentenceUnit& unit, bool isHeadWordInPhrase)
{
   if (unit.patNumber())
   {
      v_stemBuilder.getStem (stem, unit, isHeadWordInPhrase);
   }
}

//--------------------------------------------------------------------------
void GFactory::generateStemPhrase(LgsString* stem, int location,
                                  const TargetDictionaryUnit& unit)
{
   int patNumber = 0;
   int wordClassCode = 0;

   v_wordInPhraseQuery.ExecuteWithWord(unit.wordID(), location);

   if (v_wordInPhraseQuery.FetchIntoPatNumber(&patNumber, &wordClassCode))
   {
      v_stemBuilder.getStem(stem, patNumber, wordClassCode, unit);
   }
}

//--------------------------------------------------------------------------
RE_Engine<EL_Variable>* GFactory::createElisionEngine(int languageCode, 
                                                      const LgsString& elisionFileName)
{
   RE_Engine<EL_Variable>* ret = 0;
   ifstream ruleInput(elisionFileName.c_str());
   if (!ruleInput.good())
   {
      cout << "elision file does not exist" << endl;
      return 0;
   }

   switch (languageCode)
   {
   case 1: // german
      ret = EL_GE_Factory(&ruleInput).createRuleEngine();
      break;
   case 2: // english
      ret = EL_EN_Factory(&ruleInput).createRuleEngine();
      break;
   case 3: // french
      ret = EL_FR_Factory(&ruleInput).createRuleEngine();
      break;
   case 4: // spanish
      ret = EL_SP_Factory(&ruleInput).createRuleEngine();
      break;
   case 5: // italian
      ret = EL_IT_Factory(&ruleInput).createRuleEngine();
      break;
   case 6: // portuguese
      ret = EL_PT_Factory(&ruleInput).createRuleEngine();
      break;
   default:
      ret = 0;
   }

   ruleInput.close();
   return ret;
}

