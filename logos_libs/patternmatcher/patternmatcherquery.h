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
#ifndef __PatternMatcherQuery_h__
#define __PatternMatcherQuery_h__

//----------------------------------------------------------------------
// File - PatternMatcherQuery.h
//
// Class - PatternMatcherQuery (Abstract)
//
// Description - this file implements the query object for the fetching
//      of a single rule from the pattern matcher rule table. Each rule is
//      then stored in a RE_Rule and passed back to be stored in a RuleBase_
//      collection object.
//
//----------------------------------------------------------------------

#include <logos_libs/sql/sqlquery.h>
#include <logos_libs/sql/sqlcolumn.h>
#include <logos_libs/linguistic/llanguage.h>

class CachePatternRulesData;
class CachePatternRulesQuery;

class PatternMatcherQuery: public SqlQuery
{
public:
   PatternMatcherQuery();
   virtual ~PatternMatcherQuery();

   //------------------------------------------------------------------
   // Open() - builds and parses the SQL statement.
   // Close() - closes the cursor and cleans up memory.
   //------------------------------------------------------------------
   virtual void Open(SqlConnection* aConnection, const LLanguage& source, const LLanguage& target);
   virtual void Open(SqlConnection* aConnection, 
	   const LLanguage& source, const LLanguage& target, LgsVector(LgsString) v);
   virtual void Close();

   void executeQuery();
   bool fetchNextRule();
   int getRuleNumber();
   int getRuleType();
   LgsString getAntecedent();
   LgsString getConsequent();
   LgsString getDescription();

private:
   int sourceLangCode;
   char sourceLangCodeStr[10];
   int targetLangCode;
   char targetLangCodeStr[10];

   SqlColumn* ruleNumber;
   SqlColumn* ruleType;
   SqlColumn* antecedent;
   SqlColumn* consequent;
   SqlColumn* description;

// Cache-related
   CachePatternRulesData *cache_data;
   CachePatternRulesQuery *cache_query;
   int sequence_number;
   char company_code[4];
   char rule_type_code[2];
   char search_expression[501];
   char replace_expression[501];
   char comment_line[501];
};


#endif // __PatternMatcherQuery_h__

