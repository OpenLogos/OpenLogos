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
#ifndef __GWordQuery_h__
#define __GWordQuery_h__

//----------------------------------------------------------------------
// File - GWordQuery.h
//
// Class - GWordQuery (Abstract)
//
// Description - this file implements the query object for the fetching
//      of a single word match from the word_phrase table. An object
//      of this class remains a part of the Factory object throughout
//      the lifetime of the gerdem program allowing parsing to occur
//      only once.
//
//----------------------------------------------------------------------

#include <logos_libs/sql/sqlquery.h>
#include <logos_libs/sql/sqlcolumn.h>
#include <logos_libs/linguistic/llanguage.h>
#include <logos_libs/entity/dwordphrase.h>

class CacheGphraseData;
class CacheGwordQuery;

class GWordQuery: public SqlQuery
{
public:
        //------------------------------------------------------------------
        //------------------------------------------------------------------

             GWordQuery();
    virtual ~GWordQuery();

        //------------------------------------------------------------------
    // Open() - builds ane parses the SQL statement.
    // Close() - closes the cursor and cleans up memory.
        //------------------------------------------------------------------

    virtual void Open      ( SqlConnection*, const LLanguage& );
    virtual void Close     ();

        //------------------------------------------------------------------
    // BuildInputWords() - is called with each new query, it writes a
    //      source word to the SQL predicate.
    // ExecuteWithMatch() - Executes the actual query.
    // FetchIntoWordMatch() - actually moves the data from the SQL
    //      buffer to the WordMatch object.
        //------------------------------------------------------------------

    void BuildInputWords   ( const char* word );
    void ExecuteWithMatch  ();
    bool FetchIntoWordMatch( DWordPhrase* );

private:

    char       v_inputWord[256];
    char       v_inputLanguageCode[10];
    int        v_languageCode;

    SqlColumn* p_word     ;
    SqlColumn* p_id       ;
    SqlColumn* p_company  ;
    SqlColumn* p_wordCount;
    SqlColumn* p_hashCode1;
	SqlColumn* p_hashCode2;
    SqlColumn* p_rootHashCode1;
    SqlColumn* p_rootHashCode2;
	SqlColumn* p_henNum1;
	SqlColumn* p_henNum2;
	SqlColumn* p_rootHenNum1;
	SqlColumn* p_rootHenNum2;
	SqlColumn* p_headLocation;
    SqlColumn* p_headWord ;
	SqlColumn* p_wordTypeCode;
	SqlColumn* p_companyCode;

	CacheGphraseData *bdata;
	CacheGwordQuery *gquery;
};


#endif // __GWordQuery_h__

