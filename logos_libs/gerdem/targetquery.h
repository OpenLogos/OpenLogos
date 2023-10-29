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
#ifndef __TargetQuery_h__
#define __TargetQuery_h__

//----------------------------------------------------------------------
// File - TargetQuery.h
//
// Class - TargetQuery (Abstract)
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
#include <logos_libs/linguistic/lsemantosyntacticunit.h>

class CacheTargetData;
class CacheTargetQuery;

class TargetQuery: public SqlQuery
{
public:
        //------------------------------------------------------------------
        //------------------------------------------------------------------

             TargetQuery();
    virtual ~TargetQuery();

        //------------------------------------------------------------------
    // Open() - builds ane parses the SQL statement.
    // Close() - closes the cursor and cleans up memory.
        //------------------------------------------------------------------

    virtual void Open      ( SqlConnection*   connection,
                             const LLanguage& language );
    virtual void Close     ();

        //------------------------------------------------------------------
    // BuildInputWords() - is called with each new query, it writes a
    //      source word to the SQL predicate.
    // ExecuteWithMatch() - Executes the actual query.
    // FetchIntoWordMatch() - actually moves the data from the SQL
    //      buffer to the WordMatch object.
        //------------------------------------------------------------------

    void executeWithUsageID( int );
    bool fetch();

    void setSsu( LSemantoSyntacticUnit* ) const;

private:

    int        v_languageCode;

    char v_inputLanguageCode[10];
    char v_inputUsageID     [10];

        SqlColumn*              m_pWordId;
        SqlColumn*              m_pWord;
        SqlColumn*              m_pWordCount;
        SqlColumn*              m_pWordTypeCode;
        SqlColumn*              m_pHeadWord;
        SqlColumn*              m_pBlackHoleLocation;
        SqlColumn*              m_pWildCardPosition;
        SqlColumn*              m_pAspireSwitch;
        SqlColumn*              m_pMonthSwitch;
        SqlColumn*              m_pPrefixSwitch;
        SqlColumn*              m_pNeverEosSwitch;
        SqlColumn*              m_pOrdinalSwitch;
        SqlColumn*              m_pPatExceptionSwitch;

        SqlColumn*              m_pWordClassCode;
        SqlColumn*              m_pPatNumber;
        SqlColumn*              m_pGenderCode;
        SqlColumn*              m_pNumericConstraint;
        SqlColumn*              m_pAuxiliaryCode;
        SqlColumn*              m_pInflectionPosition;
        SqlColumn*              m_pVerbPrefixInseparable;
        SqlColumn*              m_pVerbPrefixSparable;
        SqlColumn*              m_pVerbPrefixInseparableLength;
        SqlColumn*              m_pVerbPrefixSparableLength;
        SqlColumn*              m_pSourceStemNumber;
        SqlColumn*              m_pRootUsageId;
        SqlColumn*              m_pSourceAnalysisCode;

// caching
	CacheTargetData *ddata;
	CacheTargetQuery *dquery;

	int wid, wc, wtc, hw, bhl, wp, wcc, pn, gc, nc, ac, vpi, vps, vpil, vpsl,
		ssn, ruid, sac;
	char word[79], as[2], pes[2];
};


#endif // __TargetQuery_h__

