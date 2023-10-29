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
#ifndef __GWordCapQuery_h__
#define __GWordCapQuery_h__

//----------------------------------------------------------------------
// File - GWordCapQuery.h
//
// Class - GWordCapQuery (Abstract)
//
//----------------------------------------------------------------------

#include <logos_libs/sql/sqlquery.h>
#include <logos_libs/sql/sqlcolumn.h>
#include <logos_libs/linguistic/llanguage.h>
#include <logos_libs/entity/dwordphrase.h>

class CacheGphraseData;
class CacheGwordQuery;

class GWordCapQuery: public SqlQuery
{
public:
        //------------------------------------------------------------------
        //------------------------------------------------------------------

             GWordCapQuery();
    virtual ~GWordCapQuery();

        //------------------------------------------------------------------
        //------------------------------------------------------------------

    virtual void Open      ( SqlConnection*, const LLanguage& );
    virtual void Close     ();

        //------------------------------------------------------------------
        //------------------------------------------------------------------

    void ExecuteWithMatch  ();
    bool FetchIntoWordMatch( DWordPhrase* );

    void BuildInputWords   ( const char* );

protected:

    void SetLowerCase();

        //------------------------------------------------------------------
        //------------------------------------------------------------------

    virtual void BindLanguage      ( const LLanguage& );
    virtual void BindLanguage      ();

private:

    int   v_languageCode;

    char  v_inputLanguageCode[10];
    char  v_inputWord[256];
    char  v_lowerWord[256];

    SqlColumn* p_id       ;
    SqlColumn* p_hashCode1;
    SqlColumn* p_hashCode2;
	SqlColumn* p_rootHashCode1;
	SqlColumn* p_rootHashCode2;
	SqlColumn* p_henNum1;
	SqlColumn* p_henNum2;
	SqlColumn* p_rootHenNum1;
	SqlColumn* p_rootHenNum2;
    SqlColumn* p_headWord ;
	SqlColumn* p_companyCode;

	CacheGphraseData *gphrase_data;
	CacheGwordQuery *gquery;
	CacheGwordQuery *gquery_lower;

};


#endif // __GWordCapQuery_h__

