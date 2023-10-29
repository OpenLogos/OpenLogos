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

#ifndef PatTableQuery_H
#define PatTableQuery_H

#include <logos_libs/sql/sqlquery.h>
#include <logos_libs/sql/sqlcolumn.h>
#include <logos_libs/linguistic/targetsentenceunit.h>
class CachePatData;
class CachePatQuery;
class CachePatData_pn_gc_nc;
class CachePatQuery_pn_gc_nc;
class CachePatData_pn_nc_pc_tc;
class CachePatQuery_pn_nc_pc_tc;
class CachePatData_pn_gc_nc_deg;
class CachePatQuery_pn_gc_nc_deg;
class CachePatData_pn_gc_nc_pc_cc;
class CachePatQuery_pn_gc_nc_pc_cc;
class CachePatData_pn_gc_nc_pc_tc;
class CachePatQuery_pn_gc_nc_pc_tc;
class CachePatData_pn_gc_nc_cc;
class CachePatQuery_pn_gc_nc_cc;
class CachePatData_pn_gc_nc_dc_cc_deg;
class CachePatQuery_pn_gc_nc_dc_cc_deg;
class CachePatData_pn_deg;
class CachePatQuery_pn_deg;
class CachePatData_pn_nc_deg;
class CachePatQuery_pn_nc_deg;
class CachePatData_pn_nc;
class CachePatQuery_pn_nc;

//class SqlStatement;
//class SqlColumn;
//class SqlConnection;
//class TargetSentenceUnit;

class PatTableQuery: public SqlQuery
{
public:

  enum SyntaticControlBit
  {
    DECLENSION_BIT = 1,
    GENDER_BIT     = 2,
    NUMBER_BIT     = 4,
    PERSON_BIT     = 8,
    CASE_BIT       = 16,
    TENSE_BIT      = 32,
    DEGREE7_BIT    = 64,
    DEGREE12_BIT   = 128
  };

  enum ColumnIndex
  {
    DECLENSION = 0,
    GENDER,
    NUMBER,
    PERSON,
    CASE,
    TENSE,
    DEGREE7,
    DEGREE12,
    MAX_COLUMNS
  };

  PatTableQuery(const LgsString& langCode_, long bitMask_ = 0);
  virtual ~PatTableQuery();

  virtual void Open(SqlConnection* connection_);
  virtual void Close();

  int  Execute(int patNumber_, int degree_, int declension_, const TargetSentenceUnit& sconTable_);
  bool Fetch(int& stemNumber_, LgsString& ending_);

protected:

private:
  static char* _columnTags[];
  static char* _columnRefTags[];

  long _bitMask;
  LgsString _languageCode;
  int _stemNumber;
  SqlColumn* _colStemNumber;
  SqlColumn* _colEnding;
  int _patNumber;
  int _declension;
  int _gender; 
  int _number; 
  int _person;
  int _degree7;
  int _degree12;
  char _tense[3];
  char _case[3];

//caching
	CachePatData *odata;
	CachePatQuery *oquery;
	CachePatData_pn_gc_nc *odata_pn_gc_nc;
	CachePatQuery_pn_gc_nc *oquery_pn_gc_nc;
	CachePatData_pn_nc_pc_tc *odata_pn_nc_pc_tc;
	CachePatQuery_pn_nc_pc_tc *oquery_pn_nc_pc_tc;
	CachePatData_pn_gc_nc_deg *odata_pn_gc_nc_deg;
	CachePatQuery_pn_gc_nc_deg *oquery_pn_gc_nc_deg;
	CachePatData_pn_gc_nc_pc_cc *odata_pn_gc_nc_pc_cc;
	CachePatQuery_pn_gc_nc_pc_cc *oquery_pn_gc_nc_pc_cc;
	CachePatData_pn_gc_nc_pc_tc *odata_pn_gc_nc_pc_tc;
	CachePatQuery_pn_gc_nc_pc_tc *oquery_pn_gc_nc_pc_tc;
	CachePatData_pn_gc_nc_cc *odata_pn_gc_nc_cc;
	CachePatQuery_pn_gc_nc_cc *oquery_pn_gc_nc_cc;
	CachePatData_pn_gc_nc_dc_cc_deg *odata_pn_gc_nc_dc_cc_deg;
	CachePatQuery_pn_gc_nc_dc_cc_deg *oquery_pn_gc_nc_dc_cc_deg;
	CachePatData_pn_deg *odata_pn_deg;
	CachePatQuery_pn_deg *oquery_pn_deg;
	CachePatData_pn_nc_deg *odata_pn_nc_deg;
	CachePatQuery_pn_nc_deg *oquery_pn_nc_deg;
	CachePatData_pn_nc *odata_pn_nc;
	CachePatQuery_pn_nc *oquery_pn_nc;
};

#endif // PatTableQuery_H


