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
// File - GSsuQuery.h (interface)
// --------------------------------------------------------------------------

#ifndef __GSsuQuery_h__
#define __GSsuQuery_h__

#include <logos_libs/sql/sqlquery.h>
#include <logos_libs/sql/sqlcolumn.h>
#include <logos_libs/linguistic/llanguage.h>
#include <logos_libs/entity/dwordphrase.h>
#include <logos_libs/linguistic/lsemantosyntacticunit.h>

class CacheGssuData;
class CacheGssuQuery;

// --------------------------------------------------------------------------
// Definition of a GSSUQuery
// --------------------------------------------------------------------------
class GSsuQuery: public SqlQuery
{
public:
   GSsuQuery();
   virtual ~GSsuQuery();
   virtual void Open(SqlConnection* connection, const LLanguage& sourceLanguage,
                     const LLanguage& targetLanguage);
   virtual void Close();
   void ExecuteWithMatch(const DWordPhrase&);
   bool fetch();
   void setSsu(LSemantoSyntacticUnit*) const;

private:
   char v_inputSourceCode[10];
   int v_sourceCode;
   SqlColumn* p_wordClassCode;
   SqlColumn* p_genderCode;
   SqlColumn* p_inflectionPosition;
   SqlColumn* p_sourcePatType;
   SqlColumn* p_sourcePatNumber;
   SqlColumn* p_sourceStemNumber;
   SqlColumn* p_usageID;
   SqlColumn* p_meaningID;
   SqlColumn* p_atomicCode;		// TO BE REMOVED WHEN NEW SMC IS TESTED
   SqlColumn* p_genericCode;		// TO BE REMOVED WHEN NEW SMC IS TESTED
   SqlColumn* p_subjectMatterCode;
   SqlColumn* p_setID;
   SqlColumn* p_subSetID;
   SqlColumn* p_superSetID;
   SqlColumn* p_priorityOrder;
   SqlColumn* p_formCode;
   SqlColumn* p_auxiliaryCode;
   SqlColumn* p_transitivity;
   bool v_isFetchStarted;
   DWordPhrase* p_wordPhrase;
   int v_inputWordId;

	int cache_wcc, cache_gc, cache_pn, cache_ssn, cache_usid, cache_ac, 
		 cache_mid, cache_setid, cache_subsetid, cache_supersetid, 
		 cache_po, cache_fc, cache_acode, cache_gcode;

	char cache_tr[2], cache_smc[7];
	CacheGssuData *adata;
	CacheGssuQuery *aquery;
};

#endif // __GSsuQuery_h__
