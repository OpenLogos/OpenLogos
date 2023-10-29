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
// File - patternmatcherquery.cpp
//
// Class - PatternMatcherQuery (implementation)
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/patternmatcher/patternmatcherquery.h>
#include <logos_libs/sql/sqlstatement.h>
#include <logos_libs/sql/sqlcolumn.h>
#include <logos_libs/sql/sqlexception.h>

#include <logos_libs/dbcache/CachePatternRulesData.h>
#include <logos_libs/dbcache/CachePatternRulesQuery.h>

//----------------------------------------------------------------------
PatternMatcherQuery::PatternMatcherQuery()
{
	cache_data = NULL;
	cache_query = NULL;
}

//----------------------------------------------------------------------
PatternMatcherQuery::~PatternMatcherQuery()
{
	if(cache_data) {
		delete cache_data;
		cache_data = NULL;
	}
	if(cache_query) {
		delete cache_query;
		cache_query = NULL;
	}
}

//----------------------------------------------------------------------
void PatternMatcherQuery::Open(SqlConnection* aConnection,
	const LLanguage& source, const LLanguage& target)
{
}

void PatternMatcherQuery::Open(SqlConnection* aConnection,
	const LLanguage& source, const LLanguage& target,
	LgsVector(LgsString) cc)
{

	sourceLangCode = source.id();
	targetLangCode = target.id();

	cache_data = new CachePatternRulesData(NULL, sourceLangCode,
		targetLangCode, false, false, false);

	if (cache_data->isValid()) {
// extremely clumsy and stupid.
		char ccc[MAX_CC][4];
		LgsVector(LgsString)::iterator i = cc.begin();
		LgsVector(LgsString)::iterator end = cc.end();
		int cnt = 0;
		for(;;) {
			if(i==end) break;
			if(cnt==MAX_CC) {
				printf("WARNING: PatternMatcherQuery: too many company codes\n");
				break;
			}
			strncpy(ccc[cnt], i->c_str(), 3);
			cnt++;
			i++; 
		}
		cache_query = new CachePatternRulesQuery(cache_data, ccc, cnt);
		return;
	} else {
		delete cache_data;
		cache_data = NULL;
	}


   LgsString clist = "(";
   if(cc.empty()) {
      clist += "''";
   } else {
	  LgsVector(LgsString)::iterator i = cc.begin();
	  LgsVector(LgsString)::iterator end = cc.end();
	  for(;;) {
		clist += "'" + (*i) + "'";
		i++;
		if(i==end) break;
		clist += ",";
	  }
   }
   clist += ")";
   LgsString selStatement = "select pmTab.Sequence_Number, "
                            "pmTab.Rule_Type_Code, "
                            "pmTab.Search_Expression,"
                            "pmTab.Replace_Expression, "
                            "pmTab.Comment_Line "
                            "from Pattern_Rules pmTab "
                            "where "
							"pmTab.Company_Code in " + clist + " and "
							"pmTab.Source_Language_Code = :aSourceCode and "
							"pmTab.Target_Language_Code = :aTargetCode and "
							"pmTab.Deactivation_Switch = 'N' "
							"order by pmTab.Sequence_Number ";

   try
   {
      SqlQuery::Open(aConnection);
      Statement()->AddToCommandString(selStatement);
      Statement()->Parse();

      ruleNumber = Statement()->BindOutputColumn(1, SqlColumn::Integer);
      ruleType = Statement()->BindOutputColumn(2, SqlColumn::StringType);
      antecedent = Statement()->BindOutputColumn(3, SqlColumn::StringType);
      consequent = Statement()->BindOutputColumn(4, SqlColumn::StringType);
      description = Statement()->BindOutputColumn(5, SqlColumn::StringType);
   }
   catch(SqlException& x)
   {
      cout << x.Message() << endl;
      throw(x);
   }
}

//----------------------------------------------------------------------
void PatternMatcherQuery::Close()
{
	if(!cache_data)
		SqlQuery::Close();
}

//----------------------------------------------------------------------
void PatternMatcherQuery::executeQuery()
{
	if(cache_data) {
// actual cache query already has been performed during Open(...)
		return;
	}
   try
   {
      Statement()->BindInputIntToString(":aSourceCode", "%02d", sourceLangCodeStr, sourceLangCode);
      Statement()->BindInputIntToString(":aTargetCode", "%02d", targetLangCodeStr, targetLangCode);
      Statement()->Execute();
   }
   catch(SqlException& x)
   {
      cout << x.Message() << endl;
      throw(x);
   }
}

//----------------------------------------------------------------------
bool PatternMatcherQuery::fetchNextRule()
{
   bool result = true;

   if(cache_data) {
	   return cache_query->fetch(&sequence_number, rule_type_code, 
		   search_expression, replace_expression, comment_line, company_code);
   }

   try
   {
      result = Statement()->Fetch();
   }
   catch(SqlException& x)
   {
      cout << x.Message() << endl;
      throw(x);
   }
   return result;
}

//----------------------------------------------------------------------
int PatternMatcherQuery::getRuleNumber()
{
	if(cache_data) {
		return sequence_number;
	}

   return ruleNumber->AsInteger();
}

//----------------------------------------------------------------------
int PatternMatcherQuery::getRuleType()
{
	if(cache_data)
		return atoi(rule_type_code);

   return ruleType->AsIntegerFromString();
}

//----------------------------------------------------------------------
LgsString PatternMatcherQuery::getAntecedent()
{
	if(cache_data) {
		return LgsString(search_expression);
	}

   return antecedent->AsString();
}

//----------------------------------------------------------------------
LgsString PatternMatcherQuery::getConsequent()
{
	if(cache_data) {
		return LgsString(replace_expression);
	}

	return consequent->AsString();
}

//----------------------------------------------------------------------
LgsString PatternMatcherQuery::getDescription()
{
	if(cache_data) {
		return LgsString(comment_line);
	}

   return description->AsString();
}
