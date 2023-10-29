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
/*******************************************************************
 *
 *    DESCRIPTION:
 *
 *    AUTHOR:
 *
 *    HISTORY:    
 *
 *******************************************************************/

#include <logos_include/logoscommon.h>
#include <logos_libs/sql/logossql.h>
#include <logos_libs/linguistic/llanguage.h>
#include <lgs_db_io/stemgenrulequery.h>

#include <logos_libs/dbcache/CacheStemgenRuleData.h>
#include <logos_libs/dbcache/CacheStemgenRuleQuery.h>

//  initialize pointer data members
StemGenRuleQuery::StemGenRuleQuery() 
                 :_pStatement(0),
                  _pAddEndingColumn(0),
                  _pDropEndingColumn(0),
                  _pReplaceColumn(0),
                  _languageId(0),
                  _openFlag(false)
{
	ldata = NULL;
	lquery = NULL;
}

// be sure to delete pointer data members
StemGenRuleQuery::~StemGenRuleQuery()
{
   if (_pStatement) 
   {
      delete _pStatement;
      _pStatement = 0;
   }

   if (ldata)
   {
	   delete ldata;
	   ldata = NULL;
	   delete lquery;
	   lquery = NULL;
   }
}

// build the query statement, create the bindings, and parse it
void StemGenRuleQuery::Open(SqlConnection* pConnection_, const LLanguage& language_)
{
   _languageId = language_.id();

	ldata = new CacheStemgenRuleData(NULL, _languageId, false, false, false);
	if (ldata->isValid())
   {
		lquery = new CacheStemgenRuleQuery(ldata, 0, 0);
	   _openFlag = true;
		return;
	}
   else
   {
		delete ldata;
		ldata = NULL;
	}

   _pStatement = pConnection_->CreateStatement();

   //  build the SQL Query
   LgsString s = 
                  "select"
                     " add_ending,"
                     " drop_ending,"
                     " replace_rule,"
                     " add_prefix "
                  "from"
                     " stem_generation_rule "
                  "where"
                     " language_code = :lCode"
                     " and pat_number = :pNumber"
                     " and stem_number = :sNumber";

   _pStatement->AddToCommandString(s);
   _pStatement->ParseDeferred();

   //  bind the output to appropriate SqlColumn typed data members
   _pAddEndingColumn = _pStatement->BindOutputColumn(1, SqlColumn::StringType);
   _pDropEndingColumn = _pStatement->BindOutputColumn(2, SqlColumn::StringType);
   _pReplaceColumn = _pStatement->BindOutputColumn(3, SqlColumn::StringType);
   _pPrefixColumn = _pStatement->BindOutputColumn(4, SqlColumn::StringType);

   _openFlag = true;

}

// execute the query for values passed as parameters
// returns false value upon failure
bool StemGenRuleQuery::Execute(int patNumber_, int stemNumber_)
{
   if (!_openFlag) return false;

	if (ldata)
   {
		lquery->query(patNumber_, stemNumber_);
		return true;
	}

   char inputLanguageCode[10];

   _pStatement->BindInputIntToString(":lCode", "%02d", inputLanguageCode, _languageId);
   _pStatement->BindInputInteger(":pNumber", &patNumber_);
   _pStatement->BindInputInteger(":sNumber", &stemNumber_);
   _pStatement->Execute();

   return true;
}

// fetch the next record in the result set
// result passed back in parameter theWordClass
// return false if end of result set or
// query has not been built (Open())
bool StemGenRuleQuery::Fetch(LgsString& theAddEnding, LgsString& theDropEnding,
                             LgsString& theReplace, LgsString& thePrefix)
{
   bool result = false;
   
	if (ldata)
   {
		result = lquery->fetch(ae, de, rr, ap);
		if (result)
      {
			theAddEnding = LgsString(ae);
			theDropEnding = LgsString(de);
			theReplace = LgsString(rr);
			thePrefix = LgsString(ap);
		}
		return result;
	}


   if (_openFlag && (result = _pStatement->Fetch()))
   {
      theAddEnding = _pAddEndingColumn->AsString();
      theDropEnding = _pDropEndingColumn->AsString();
      theReplace = _pReplaceColumn->AsString();
      thePrefix = _pPrefixColumn->AsString();
   }
   
   return result;
}

void StemGenRuleQuery::Close()
{
   _openFlag = false;
}

void StemGenRuleQuery::printGuts(ostream& ostr) const
{
   ostr << "StemGenRuleQuery::printGuts" << endl;
   ostr << "_pDropEndingColumn " << _pDropEndingColumn->AsString() << endl;
   ostr << "_pAddEndingColumn " << _pAddEndingColumn->AsString() << endl;
   ostr << "_pReplaceColumn " << _pReplaceColumn->AsString() << endl;
}
