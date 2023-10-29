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
#ifndef __ConstantPointerQuery_h__
#define __ConstantPointerQuery_h__

//----------------------------------------------------------------------
// File - ConstantPointerQuery.h
//
// Class - ConstantPointerQuery (Abstract)
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
//#include <logos_libs/linguistic/llanguage.h>
//#include <logos_libs/entity/dwordphrase.h>

class CacheConstantPointerData;
class CacheConstantPointerQuery;

class ConstantPointerQuery: public SqlQuery
{
public:
   ConstantPointerQuery();
   virtual ~ConstantPointerQuery();

   //------------------------------------------------------------------
   // Open() - builds ane parses the SQL statement.
   // Close() - closes the cursor and cleans up memory.
   //------------------------------------------------------------------
   virtual void Open(SqlConnection* connection, 
                     const class LLanguage& language, bool useCC = true);
   virtual void Close();

   //------------------------------------------------------------------
   // BuildInputWords() - is called with each new query, it writes a
   //                     source word to the SQL predicate.
   // ExecuteWithMatch() - Executes the actual query.
   // FetchIntoWordMatch() - actually moves the data from the SQL
   //                        buffer to the WordMatch object.
   //------------------------------------------------------------------
   void ExecuteWithConstantID(int constantID, const LgsString& compCode, const LgsString& constantType);
   bool fetchTransfers(class TargetDictionaryUnit*);

private:
   int v_languageCode;
   int v_constantID;
   bool v_useCompanyCode;

   char v_inputLanguageCode[10];

   SqlColumn* p_primary_usage_id;
   SqlColumn* p_alternate_usage_id;
   SqlColumn* p_combiningFormCode;

// caching
	CacheConstantPointerData *ndata;
	CacheConstantPointerQuery *nquery;
	int puid, auid, cfc;
};


#endif // __ConstantPointerQuery_h__

