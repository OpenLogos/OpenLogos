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
// File - WordPhraseQuery.cpp
//
// Class - WordPhraseQuery (implementation)
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/gerdem/wordphrasequery.h>
#include <logos_libs/entity/dwordphrase.h>
#include <logos_libs/sql/sqlstatement.h>
#include <logos_libs/sql/sqlcolumn.h>
#include <logos_libs/sql/sqlexception.h>
#include <logos_libs/linguistic/llanguage.h>

#include <logos_libs/dbcache/CacheEndingLengthData.h>
#include <logos_libs/dbcache/CacheEndingLengthQuery.h>

//----------------------------------------------------------------------
WordPhraseQuery::WordPhraseQuery()
{
	kdata = NULL;
	kquery = NULL;
}
//----------------------------------------------------------------------
WordPhraseQuery::~WordPhraseQuery()
{
	if(kdata) { delete kdata; kdata = NULL; }
	if(kquery) { delete kquery; kquery = NULL; }
}
//----------------------------------------------------------------------
void WordPhraseQuery::Open(SqlConnection* aConnection)
{

	kdata = new CacheEndingLengthData(NULL, 0, false, false, false);
	if(kdata->isValid()) {
		kquery = new CacheEndingLengthQuery(kdata, 0);
		return;
	} else {
		delete kdata;
		kdata = NULL;
	}

   LgsString s = "select"
              " w.Ending_Length"
              " from Word_Phrase w"
              " where"
              " w.Word_ID = :aWordID";

   try
   {
      SqlQuery::Open(aConnection);
      Statement()->AddToCommandString(s);
      Statement()->Parse();

      p_endingLength = Statement()->BindOutputColumn (1, SqlColumn::Integer);
   }
   catch(SqlException& x)
   {
      cout << x.Message() << endl;
      throw(x);
   }
}
//----------------------------------------------------------------------
void WordPhraseQuery::Close()
{
	if(kdata==NULL)
		SqlQuery::Close();
}
//----------------------------------------------------------------------
void WordPhraseQuery::Execute(int wordID)
{
   v_wordID = wordID;

	if(kdata) {
		kquery->query(wordID);
		return;
	}

   try
   {
      Statement()->BindInputInteger(":aWordID",  &v_wordID);
      Statement()->Execute();
   }
   catch(SqlException& x)
   {
      cout << x.Message() << endl;
      throw(x);
   }
}
//----------------------------------------------------------------------
bool WordPhraseQuery::Fetch(int *pEndingLength)
{
   bool result = true;

	if(kdata) {
		result = kquery->fetch(pEndingLength);
		return result;
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
   if (result == true)
   {
      *pEndingLength     = p_endingLength->AsInteger ();
   }
   return result;
}
