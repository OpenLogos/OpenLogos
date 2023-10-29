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
// File - GPhraseQuery.cpp
//
// Class - GPhraseQuery (implementation)
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/gerdem/gphrasequery.h>
#include <logos_libs/entity/dwordphrase.h>
#include <logos_libs/sql/sqlstatement.h>
#include <logos_libs/sql/sqlcolumn.h>
#include <logos_libs/sql/sqlexception.h>
#include <logos_libs/linguistic/llanguage.h>
#include <lgs_db_io/querytodictionary.h>
#include <stdio.h>

#include <logos_libs/dbcache/CacheGphraseData.h>
#include <logos_libs/dbcache/CacheGphraseQuery.h>

char GPhraseQuery::ESCAPE_CHAR = '\\';

//----------------------------------------------------------------------
GPhraseQuery::GPhraseQuery()
             :v_languageCode(0)
{
   v_inputLanguageCode[0] = 0;
   v_inputWord[0] = 0;
   bdata = NULL;
   bquery = NULL;
}
//----------------------------------------------------------------------
GPhraseQuery::~GPhraseQuery()
{
	if(bdata) { delete bdata; bdata = NULL; }
	if(bquery) { delete bquery; bquery = NULL; }
}
//----------------------------------------------------------------------
void GPhraseQuery::Open(SqlConnection* aConnection, const LLanguage& language)
{

	if(bdata==NULL) {
		bdata = new CacheGphraseData(NULL, language.id(), false, false, false);
		if(bdata->isValid()) {
			bquery = new CacheGphraseQuery(bdata, NULL);
			return;
		} else {
			delete bdata;
			bdata = NULL;
		}
	}

   LgsString s = "select"
              " Word_ID, Word, Head_Word, Hash_Location, "
			  " Company_Code, Ending_Length,"
// new fields from former PhraseIdQuery
				" word_count, hash_Code_1, hash_code_2, root_Hash_1, "
				" root_Hash_2, hen_Num_1, hen_Num_2, root_Hen_1, "
				" root_Hen_2, hash_location "
              " from Word_Phrase"
              " where Word like :aWord"
			  " ESCAPE " 
#ifdef USE_POSTGRES 
     "'\\\\'"
#else
     "'\\'"
#endif
              " and Language_Code = :aCode"
			  " and word_count > 1"
              " order by word_count desc, word";
   // TODO: '\\' sequence is DB specific: Postgres requires double quoting, Oracle not
   try
   {
      SqlQuery::Open(aConnection);

      Statement()->AddToCommandString(s);

      Statement()->Parse();

      v_languageCode = language.id();

      p_id           = Statement()->BindOutputColumn(1, SqlColumn::Integer);
      p_word         = Statement()->BindOutputColumn(2, SqlColumn::StringType);
      p_headWord     = Statement()->BindOutputColumn(3, SqlColumn::Integer);
      p_hashLocation = Statement()->BindOutputColumn(4, SqlColumn::Integer);
      p_companyCode  = Statement()->BindOutputColumn(5, SqlColumn::StringType);
      p_endingLength = Statement()->BindOutputColumn(6, SqlColumn::Integer);
// new fields (from former PhraseIdQuery)
		p_wordCount	= Statement()->BindOutputColumn( 7, SqlColumn::Integer );
		p_hashCode1 = Statement()->BindOutputColumn( 8, SqlColumn::Integer );
		p_hashCode2	= Statement()->BindOutputColumn( 9, SqlColumn::Integer );
		p_rootHashCode1	= Statement()->BindOutputColumn( 10, SqlColumn::Integer );
		p_rootHashCode2	= Statement()->BindOutputColumn( 11, SqlColumn::Integer );
		p_henNum1 = Statement()->BindOutputColumn( 12, SqlColumn::Integer );
		p_henNum2 = Statement()->BindOutputColumn( 13, SqlColumn::Integer );
		p_rootHenNum1 = Statement()->BindOutputColumn( 14, SqlColumn::Integer );
		p_rootHenNum2 = Statement()->BindOutputColumn(15, SqlColumn::Integer );
		p_headLocation = Statement()->BindOutputColumn(16, SqlColumn::Integer );
   }
   catch(SqlException& x)
   {
      cout << x.Message() << endl;
      throw(x);
   }
}
//----------------------------------------------------------------------
void GPhraseQuery::Close() {
	if(bdata==NULL)
   SqlQuery::Close();
}
//----------------------------------------------------------------------
inline void GPhraseQuery::CopyCharacters(char *& targetBuf, const LgsString& word)
{
	LgsString::const_iterator endIter = word.end();
	for (LgsString::const_iterator charIter = word.begin(); charIter != endIter; 
		 charIter++)
	{
		if (*charIter == ESCAPE_CHAR || *charIter == '%' || *charIter == '_')
		{
			*targetBuf++ = ESCAPE_CHAR;
		}
		*targetBuf++ = *charIter;
	}
}
//----------------------------------------------------------------------
void GPhraseQuery::CombineWords(const LgsString& firstWord, short firstSpaceCount,
								const LgsString& nextWord, char* buf)
{
	CopyCharacters(buf, firstWord);
    if ((nextWord == "-" || nextWord == "/" || nextWord.empty()) ||
		((firstWord == "." || firstWord == "!" || firstWord == "?") &&
        (nextWord == ")" || nextWord == "}" || nextWord == "]")))
	{
		firstSpaceCount = 0;
	}
	for (; firstSpaceCount; *buf++ = ' ', firstSpaceCount--);
	CopyCharacters(buf, nextWord);
	*buf++ = '%';
	*buf = '\0';
}
//----------------------------------------------------------------------
void GPhraseQuery::ExecuteWithMatch(const LgsString& firstWord, short firstSpaceCount,
									const LgsString& nextWord)
{
   CombineWords(firstWord, firstSpaceCount, nextWord, v_inputWord);
//printf("GPhrase.looking for \"%s\"\n", v_inputWord);
//fflush(stdout);

	if(bdata) {
		int ret = bquery->query(v_inputWord);
//printf("GPhrase.query ret=%d\n", ret);
//fflush(stdout);
		return;
	}

   try
   {
      Statement()->BindInputIntToString(":aCode", "%02d", v_inputLanguageCode, v_languageCode);
      Statement()->BindInputString(":aWord", v_inputWord);

      QueryToDictionary::GetObject()->PrintPhrase( v_inputWord );

      Statement()->Execute();
   }
   catch(SqlException& x)
   {
      cout << x.Message() << endl;
      throw(x);
   }
}
//----------------------------------------------------------------------
bool GPhraseQuery::FetchIntoWordMatch(DWordPhrase* p)
{
   bool result = true;

	if(bdata) {
		char word[80], company_code[4];
		int head_word, hash_location, ending_length, word_id;
		int hash_code_1, hash_code_2, root_hash_1;
		int root_hash_2, hen_1, hen_2;
		int root_hen_1, root_hen_2, word_count;

		result = bquery->fetch(&word_id, word, &head_word, &hash_location, 
			&ending_length, company_code,
			&hash_code_1, &hash_code_2, &root_hash_1, 
			&root_hash_2, &hen_1, &hen_2, 
			&root_hen_1, &root_hen_2, &word_count);

//if(result) {
//printf("GPhrase.fetch result=%d %d %d %d\"%s\"\n", result,
//	   head_word, hash_location, ending_length, word);
//fflush(stdout);
//}
		if(result) {
//printf("\nCacheGphraseQuery: \"%s\"\n", word);
//fflush(stdout);
			p->Initialize (0,
				hash_code_1, hash_code_2, root_hash_1, root_hash_2,
				hen_1, hen_2, root_hen_1, root_hen_2,
				hash_location, head_word,
                       false,
                       false,
                       false,
                       false,
                       false,
                       false,
                       2,
                       0,
                       0,
				word_count, word_id,
				0, 0);
			p->CompanyCode(company_code);
			p->Word(word);
			p->HeadWord(head_word);
			p->HashLocation(hash_location);
			p->EndingLength(ending_length);
		}
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
   if(result == true)
   {
		p->Initialize (0,
			  p_hashCode1->AsInteger(),
              p_hashCode2->AsInteger(),
              p_rootHashCode1->AsInteger(),
              p_rootHashCode2->AsInteger(),
			  p_henNum1->AsInteger(),
			  p_henNum2->AsInteger(),
			  p_rootHenNum1->AsInteger(),
			  p_rootHenNum2->AsInteger(),
                       p_headLocation->AsInteger(),
                       p_headWord->AsInteger(),
                       false,
                       false,
                       false,
                       false,
                       false,
                       false,
                       2,
                       0,
                       0,
                       p_wordCount->AsInteger(),
                       p_id->AsInteger(),
                       0, 0);

		p->CompanyCode(p_companyCode->AsString());
		p->Word(p_word->AsString ());
		p->HeadWord(p_headWord->AsInteger());
		p->HashLocation(p_hashLocation->AsInteger());
		p->EndingLength(p_endingLength->AsInteger());
   }

//if(result) {
//printf("GPhrase.fetch result=%d %d %d %d\"%s\"\n", result,
//	   p_headWord->AsInteger(), p_hashLocation->AsInteger(), 
//	   p_endingLength->AsInteger(), p_word->AsString().c_str());
//fflush(stdout);
//}

   return result;
}
