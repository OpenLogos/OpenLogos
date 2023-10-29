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

#include <stdio.h>
#include <logos_include/logoscommon.h>
#include <logos_libs/sql/sqlconnection.h>
#include <logos_libs/sql/sqlstatement.h>
#include <logos_libs/sql/sqlcolumn.h>
#include <logos_libs/sql/sqlexception.h>
#include <logos_libs/linguistic/targetsentenceunit.h>
#include <lgs_db_io/pattablequery.h>

#include <logos_libs/dbcache/CachePatData.h>
#include <logos_libs/dbcache/CachePatQuery.h>
#include <logos_libs/dbcache/CachePatData_pn_gc_nc.h>
#include <logos_libs/dbcache/CachePatQuery_pn_gc_nc.h>
#include <logos_libs/dbcache/CachePatData_pn_nc_pc_tc.h>
#include <logos_libs/dbcache/CachePatQuery_pn_nc_pc_tc.h>
#include <logos_libs/dbcache/CachePatData_pn_gc_nc_deg.h>
#include <logos_libs/dbcache/CachePatQuery_pn_gc_nc_deg.h>
#include <logos_libs/dbcache/CachePatData_pn_gc_nc_pc_cc.h>
#include <logos_libs/dbcache/CachePatQuery_pn_gc_nc_pc_cc.h>
#include <logos_libs/dbcache/CachePatData_pn_gc_nc_pc_tc.h>
#include <logos_libs/dbcache/CachePatQuery_pn_gc_nc_pc_tc.h>
#include <logos_libs/dbcache/CachePatData_pn_gc_nc_cc.h>
#include <logos_libs/dbcache/CachePatQuery_pn_gc_nc_cc.h>
#include <logos_libs/dbcache/CachePatData_pn_gc_nc_dc_cc_deg.h>
#include <logos_libs/dbcache/CachePatQuery_pn_gc_nc_dc_cc_deg.h>
#include <logos_libs/dbcache/CachePatData_pn_deg.h>
#include <logos_libs/dbcache/CachePatQuery_pn_deg.h>
#include <logos_libs/dbcache/CachePatData_pn_nc_deg.h>
#include <logos_libs/dbcache/CachePatQuery_pn_nc_deg.h>

#include <logos_libs/dbcache/CachePatData_pn_nc.h>
#include <logos_libs/dbcache/CachePatQuery_pn_nc.h>

char* PatTableQuery::_columnTags[] =
{
   "declention_code",
   "gender_code",
   "number_code",
   "person_code",
   "case_code",
   "tense_code",
   "degree",
   "degree",
   0
};

char* PatTableQuery::_columnRefTags[] =
{
   ":aDeclension_code",
   ":aGender_code",
   ":aNumber_code",
   ":aPerson_code",
   ":aCase_code",
   ":aTense_code",
   ":aDegree",
   ":aDegree",
   0
};

PatTableQuery::PatTableQuery(const LgsString& langCode_, long bitMask_) 
              :_bitMask(bitMask_),
               _languageCode(langCode_),
               _colStemNumber(NULL),
               _colEnding(NULL),
               _stemNumber(0)
{

	odata = NULL;
	oquery = NULL;
	odata_pn_gc_nc = NULL;
	oquery_pn_gc_nc = NULL;

	odata_pn_nc_pc_tc = NULL;
	oquery_pn_nc_pc_tc = NULL;

	odata_pn_gc_nc_deg = NULL;
	oquery_pn_gc_nc_deg = NULL;

	odata_pn_gc_nc_pc_cc = NULL;
	oquery_pn_gc_nc_pc_cc = NULL;

	odata_pn_gc_nc_pc_tc = NULL;
	oquery_pn_gc_nc_pc_tc = NULL;

	odata_pn_gc_nc_cc = NULL;
	oquery_pn_gc_nc_cc = NULL;

	odata_pn_gc_nc_dc_cc_deg = NULL;
	oquery_pn_gc_nc_dc_cc_deg = NULL;

	odata_pn_deg = NULL;
	oquery_pn_deg = NULL;

	odata_pn_nc_deg = NULL;
	oquery_pn_nc_deg = NULL;

	odata_pn_nc = NULL;
	oquery_pn_nc = NULL;
}

PatTableQuery::~PatTableQuery()
{
	if(odata) { delete odata; odata = NULL; }
	if(oquery) { delete oquery; oquery = NULL; }

	if(odata_pn_gc_nc) { delete odata_pn_gc_nc; odata_pn_gc_nc = NULL; }
	if(oquery_pn_gc_nc) { delete oquery_pn_gc_nc; oquery_pn_gc_nc = NULL; }

	if(odata_pn_nc_pc_tc) { delete odata_pn_nc_pc_tc; 
		odata_pn_nc_pc_tc = NULL; }
	if(oquery_pn_nc_pc_tc) { delete oquery_pn_nc_pc_tc; 
		oquery_pn_nc_pc_tc = NULL; }

	if(odata_pn_gc_nc_deg) { delete odata_pn_gc_nc_deg; 
		odata_pn_gc_nc_deg = NULL; }
	if(oquery_pn_gc_nc_deg) { delete oquery_pn_gc_nc_deg; 
		oquery_pn_gc_nc_deg = NULL; }

	if(odata_pn_gc_nc_pc_cc) { delete odata_pn_gc_nc_pc_cc; 
		odata_pn_gc_nc_pc_cc = NULL; }
	if(oquery_pn_gc_nc_pc_cc) { delete oquery_pn_gc_nc_pc_cc; 
		oquery_pn_gc_nc_pc_cc = NULL; }

	if(odata_pn_gc_nc_pc_tc) { delete odata_pn_gc_nc_pc_tc; 
		odata_pn_gc_nc_pc_tc = NULL; }
	if(oquery_pn_gc_nc_pc_tc) { delete oquery_pn_gc_nc_pc_tc; 
		oquery_pn_gc_nc_pc_tc = NULL; }

	if(odata_pn_gc_nc_cc) { delete odata_pn_gc_nc_cc;
		odata_pn_gc_nc_cc = NULL; }
	if(oquery_pn_gc_nc_cc) { delete oquery_pn_gc_nc_cc;
		oquery_pn_gc_nc_cc = NULL; }

	if(odata_pn_gc_nc_dc_cc_deg) { delete odata_pn_gc_nc_dc_cc_deg; 
		odata_pn_gc_nc_dc_cc_deg = NULL; }
	if(oquery_pn_gc_nc_dc_cc_deg) { delete oquery_pn_gc_nc_dc_cc_deg; 
		oquery_pn_gc_nc_dc_cc_deg = NULL; }

	if(odata_pn_deg) { delete odata_pn_deg; odata_pn_deg = NULL; }
	if(oquery_pn_deg) { delete oquery_pn_deg; oquery_pn_deg = NULL; }

	if(odata_pn_nc_deg) { delete odata_pn_nc_deg; odata_pn_nc_deg = NULL; }
	if(oquery_pn_nc_deg) { delete oquery_pn_nc_deg; oquery_pn_nc_deg = NULL; }

	if(odata_pn_nc) { delete odata_pn_nc; odata_pn_nc = NULL; }
	if(oquery_pn_nc) { delete oquery_pn_nc; oquery_pn_nc = NULL; }
}

void PatTableQuery::Open(SqlConnection* connection_)
{
	
	int lid = atoi(_languageCode.c_str());

	odata = new CachePatData(NULL, lid, false, false, false);
	if(odata->isValid()) {
		oquery = new CachePatQuery(odata, 0, 0, 0);
	} else {
		delete odata;
		odata = NULL;
		oquery = NULL;
	}

	odata_pn_gc_nc = new CachePatData_pn_gc_nc(NULL, lid, false, false, false);
	if(odata_pn_gc_nc->isValid()) {
		oquery_pn_gc_nc = new CachePatQuery_pn_gc_nc(odata_pn_gc_nc, 0, 0, 0);
	} else {
		delete odata_pn_gc_nc;
		odata_pn_gc_nc = NULL;
		oquery_pn_gc_nc = NULL;
	}
 
	odata_pn_nc_pc_tc = new CachePatData_pn_nc_pc_tc(NULL, lid, false, false, false);
	if(odata_pn_nc_pc_tc->isValid()) {
		oquery_pn_nc_pc_tc = new CachePatQuery_pn_nc_pc_tc(odata_pn_nc_pc_tc,0,0,0,0);
	} else {
		delete odata_pn_nc_pc_tc;
		odata_pn_nc_pc_tc = NULL;
		oquery_pn_nc_pc_tc = NULL;
	}

	odata_pn_gc_nc_deg = new CachePatData_pn_gc_nc_deg(NULL, lid, false, false, false);
	if(odata_pn_gc_nc_deg->isValid()) {
		oquery_pn_gc_nc_deg = new CachePatQuery_pn_gc_nc_deg(odata_pn_gc_nc_deg, 
			0, 0, 0, 0);
	} else {
		delete odata_pn_gc_nc_deg;
		odata_pn_gc_nc_deg = NULL;
		oquery_pn_gc_nc_deg = NULL;
	}

	odata_pn_gc_nc_pc_cc = new CachePatData_pn_gc_nc_pc_cc(NULL, lid, false, false, 
		false);
	if(odata_pn_gc_nc_pc_cc->isValid()) {
		oquery_pn_gc_nc_pc_cc = new CachePatQuery_pn_gc_nc_pc_cc(odata_pn_gc_nc_pc_cc, 
			0, 0, 0, 0, 0);
	} else {
		delete odata_pn_gc_nc_pc_cc;
		odata_pn_gc_nc_pc_cc = NULL;
		oquery_pn_gc_nc_pc_cc = NULL;
	}

	odata_pn_gc_nc_pc_tc = new CachePatData_pn_gc_nc_pc_tc(NULL, lid, false, false, 
		false);
	if(odata_pn_gc_nc_pc_tc->isValid()) {
		oquery_pn_gc_nc_pc_tc = new CachePatQuery_pn_gc_nc_pc_tc(odata_pn_gc_nc_pc_tc, 
			0, 0, 0, 0, 0);
	} else {
		delete odata_pn_gc_nc_pc_tc;
		odata_pn_gc_nc_pc_tc = NULL;
		oquery_pn_gc_nc_pc_tc = NULL;
	}


	odata_pn_gc_nc_cc = new CachePatData_pn_gc_nc_cc(NULL, lid, false, false, false);
	if(odata_pn_gc_nc_cc->isValid()) {
		oquery_pn_gc_nc_cc = new CachePatQuery_pn_gc_nc_cc(odata_pn_gc_nc_cc, 
			0, 0, 0, 0);
	} else {
		delete odata_pn_gc_nc_cc;
		odata_pn_gc_nc_cc = NULL;
		oquery_pn_gc_nc_cc = NULL;
	}

	odata_pn_gc_nc_dc_cc_deg = new CachePatData_pn_gc_nc_dc_cc_deg(NULL, lid, 
		false, false, false);
	if(odata_pn_gc_nc_dc_cc_deg->isValid()) {
		oquery_pn_gc_nc_dc_cc_deg = new CachePatQuery_pn_gc_nc_dc_cc_deg(
			odata_pn_gc_nc_dc_cc_deg, 0, 0, 0, 0, 0, 0);
	} else {
		delete odata_pn_gc_nc_dc_cc_deg;
		odata_pn_gc_nc_dc_cc_deg = NULL;
		oquery_pn_gc_nc_dc_cc_deg = NULL;
	}

	odata_pn_nc_deg = new CachePatData_pn_nc_deg(NULL, lid, false, false, false);
	if(odata_pn_nc_deg->isValid()) {
		oquery_pn_nc_deg = new CachePatQuery_pn_nc_deg(odata_pn_nc_deg, 0, 0, 0);
	} else {
		delete odata_pn_nc_deg;
		odata_pn_nc_deg = NULL;
		oquery_pn_nc_deg = NULL;
	}

	odata_pn_deg = new CachePatData_pn_deg(NULL, lid, false, false, false);
	if(odata_pn_deg->isValid()) {
		oquery_pn_deg = new CachePatQuery_pn_deg(odata_pn_deg, 0, 0);
	} else {
		delete odata_pn_deg;
		odata_pn_deg = NULL;
		oquery_pn_deg = NULL;
	}

	if(connection_==NULL)
		return;

	try {
      LgsString s = "select"
                 " stem_number,"
                 " ending "
                 "from"
                 " pat_table "
                 "where"
                 " language_code = :aLanguageCode"
                 " and pat_number = :aPatNumber";
		for (int i = 0; i < MAX_COLUMNS; i++) {
			if (_bitMask &  (0x0001 << i)) {
				s.append(LgsString(" and "));
				s.append(LgsString(_columnTags[i]));
				s.append(LgsString(" = "));
				s.append(LgsString(_columnRefTags[i]));
			}
		}
      SqlQuery::Open(connection_);
      Statement()->AddToCommandString(s);
      Statement()->Parse();

      _colStemNumber = Statement()->BindOutputColumn(1, SqlColumn::Integer);
      _colEnding = Statement()->BindOutputColumn(2, SqlColumn::StringType);
   }
   catch(SqlException& x)
   {
      cout << x.Message() << endl;
      throw(x);
   }
}

void PatTableQuery::Close()
{
   SqlQuery::Close();
}

int PatTableQuery::Execute(int patNumber_, int degree_, int declension_, const TargetSentenceUnit& unit_)
{
//dbg
//	if(_bitMask == (NUMBER_BIT | DEGREE7_BIT)) {
//printf("\nCachePatQuery.query: %d %d %d\n", 
//patNumber_, unit_.number(), degree_);
//fflush(stdout);
//	}


	// caching
	if(_bitMask == (NUMBER_BIT | CASE_BIT)) {
//printf("\nCachePatQuery.query: %d %d %d\n", 
//patNumber_, unit_.number(), unit_.caseOf());
//fflush(stdout);
		if(odata) {
			int ret = oquery->query(patNumber_, unit_.number(), unit_.caseOf());
			return 0;
		}
	}
	if(_bitMask == (NUMBER_BIT | GENDER_BIT)) {
//printf("\nCachePatQuery.query: %d %d %d\n", 
//patNumber_, unit_.number(), unit_.caseOf());
//fflush(stdout);
		if(odata_pn_gc_nc) {
			int ret = oquery_pn_gc_nc->query(patNumber_, 
				unit_.gender(), unit_.number());
			return 0;
		}
	}
	if(_bitMask == (NUMBER_BIT | PERSON_BIT | TENSE_BIT)) {
		if(odata_pn_nc_pc_tc) {
			int ret = oquery_pn_nc_pc_tc->query(patNumber_, 
				unit_.number(), unit_.person(), unit_.tense());
//printf("\nCachePatQuery.query: %d %d %d %d\n", 
//patNumber_, unit_.number(), unit_.person(), unit_.tense());
//fflush(stdout);
			return 0;
		}
	}
	if(_bitMask == (NUMBER_BIT | GENDER_BIT | DEGREE7_BIT)) {
//printf("\nCachePatQuery.query: %d %d %d\n", 
//patNumber_, unit_.number(), unit_.caseOf());
//fflush(stdout);
		if(odata_pn_gc_nc_deg) {
			int ret = oquery_pn_gc_nc_deg->query(patNumber_, 
				unit_.gender(), unit_.number(), degree_);
			return 0;
		}
	}
	if(_bitMask == (NUMBER_BIT | GENDER_BIT | PERSON_BIT | CASE_BIT)) {
//printf("\nCachePatQuery.query: %d %d %d\n", 
//patNumber_, unit_.number(), unit_.caseOf());
//fflush(stdout);
		if(odata_pn_gc_nc_pc_cc) {
			int ret = oquery_pn_gc_nc_pc_cc->query(patNumber_, unit_.gender(), 
				unit_.number(), unit_.person(), unit_.caseOf());
			return 0;
		}
	}
	if(_bitMask == (NUMBER_BIT | GENDER_BIT | PERSON_BIT | TENSE_BIT)) {
//printf("\nCachePatQuery.query: %d %d %d %d %d\n", 
//patNumber_, unit_.gender(), unit_.number(), unit_.person(), unit_.tense());
//fflush(stdout);
		if(odata_pn_gc_nc_pc_tc) {
			int ret = oquery_pn_gc_nc_pc_tc->query(patNumber_, unit_.gender(), 
				unit_.number(), unit_.person(), unit_.tense());
			return 0;
		}
	}
	if(_bitMask == (NUMBER_BIT | GENDER_BIT | CASE_BIT)) {
//printf("\nCachePatQuery.query: %d %d %d %d\n", 
//patNumber_, unit_.number(), unit_.gender(), unit_.caseOf());
//fflush(stdout);
		if(odata_pn_gc_nc_cc) {
			int ret = oquery_pn_gc_nc_cc->query(patNumber_, 
				unit_.gender(), unit_.number(), unit_.caseOf());
			return 0;
		}
	}
	if(_bitMask == 
		(NUMBER_BIT | GENDER_BIT | DECLENSION_BIT | CASE_BIT | DEGREE12_BIT)) {
//printf("\nCachePatQuery.query: %d %d %d\n", 
//patNumber_, unit_.number(), unit_.caseOf());
//fflush(stdout);
		if(odata_pn_gc_nc_dc_cc_deg) {
			int ret = oquery_pn_gc_nc_dc_cc_deg->query(patNumber_, 
				unit_.gender(), unit_.number(), declension_, unit_.caseOf(),
				degree_);
			return 0;
		}
	}

	if(_bitMask == (NUMBER_BIT | DEGREE7_BIT)) {
//printf("\nCachePatQuery.query: %d %d %d\n", 
//patNumber_, unit_.number(), degree_);
//fflush(stdout);
		if(odata_pn_nc_deg) {
			int ret = oquery_pn_nc_deg->query(patNumber_, unit_.number(),
				degree_);
			return 0;
		}
	}

	if(_bitMask == DEGREE7_BIT || _bitMask==DEGREE12_BIT) {
//printf("\nCachePatQuery.query: %d %d\n", 
//patNumber_, degree_);
//fflush(stdout);
		if(odata_pn_deg) {
			int ret = oquery_pn_deg->query(patNumber_, degree_);
			return 0;
		}
	}

	if(_bitMask == NUMBER_BIT) {
//printf("\nCachePatQuery.query: %d %d\n", 
//patNumber_, unit_.number());
//fflush(stdout);
		if(odata_pn_nc) {
			int ret = oquery_pn_nc->query(patNumber_, unit_.number());
			return 0;
		}
	}

	try {
      _patNumber = patNumber_;

      Statement()->BindInputString(":aLanguageCode", _languageCode.c_str());
      Statement()->BindInputInteger(":aPatNumber", &_patNumber);

      if (_bitMask & DECLENSION_BIT)
      {
         _declension = declension_;
         Statement()->BindInputInteger(_columnRefTags[DECLENSION], &_declension);
      }
      if (_bitMask & GENDER_BIT)
      {
         _gender = unit_.gender();
         Statement()->BindInputInteger(_columnRefTags[GENDER],&_gender);
      }
      if (_bitMask & NUMBER_BIT)
      {
         _number = unit_.number();
         Statement()->BindInputInteger(_columnRefTags[NUMBER], &_number);
      }
      if (_bitMask & PERSON_BIT)
      {
         _person = unit_.person();
         Statement()->BindInputInteger(_columnRefTags[PERSON], &_person);
      }
      if (_bitMask & CASE_BIT)
      {
         sprintf(_case, "%02d", unit_.caseOf() );
         Statement()->BindInputString(_columnRefTags[CASE], _case);
      }
      if (_bitMask & TENSE_BIT)
      {
         sprintf(_tense, "%02d", unit_.tense() );
         Statement()->BindInputString(_columnRefTags[TENSE], _tense);
      }
      if (_bitMask & DEGREE7_BIT)
      {
         _degree7 = degree_;
         Statement()->BindInputInteger(_columnRefTags[DEGREE7], &_degree7);
      }
      if (_bitMask & DEGREE12_BIT)
      {
         _degree12 = degree_;
         Statement()->BindInputInteger(_columnRefTags[DEGREE12], &_degree12);
      }
      Statement()->Execute();
   }
   catch(SqlException& x)
   {
      cout << x.Message() << endl;
      throw (x);
   }
   
   return 0;
}

bool PatTableQuery::Fetch(int& stemNumber_, LgsString& ending_)
{
   bool result = true;

	if(_bitMask == (NUMBER_BIT | CASE_BIT)) {
		if(odata) {
			char buf[9];
			int ret = oquery->fetch( & _stemNumber, buf);
			stemNumber_ = _stemNumber;
			ending_ = buf;
//if(ret) {
//printf("\nCachePatQuery.fetch: %d \"%s\"\n", stemNumber_, ending_.c_str());
//fflush(stdout);
//}
			return ret;
		}
	}
	if(_bitMask == (NUMBER_BIT | GENDER_BIT)) {
		if(odata_pn_gc_nc) {
			char buf[9];
			int ret = oquery_pn_gc_nc->fetch( & _stemNumber, buf);
			stemNumber_ = _stemNumber;
			ending_ = buf;
//if(ret) {
//printf("\nCachePatQuery.fetch: %d \"%s\"\n", stemNumber_, ending_.c_str());
//fflush(stdout);
//}
			return ret;
		}
	}
	if(_bitMask == (NUMBER_BIT | PERSON_BIT | TENSE_BIT)) {
		if(odata_pn_nc_pc_tc) {
			char buf[9];
			int ret = oquery_pn_nc_pc_tc->fetch( & _stemNumber, buf);
			stemNumber_ = _stemNumber;
			ending_ = buf;
//if(ret) {
//printf("\nCachePatQuery.fetch: %d \"%s\"\n", stemNumber_, ending_.c_str());
//fflush(stdout);
//}
			return ret;
		}
	}
	if(_bitMask == (NUMBER_BIT | GENDER_BIT | DEGREE7_BIT)) {
		if(odata_pn_gc_nc_deg) {
			char buf[9];
			int ret = oquery_pn_gc_nc_deg->fetch( & _stemNumber, buf);
			stemNumber_ = _stemNumber;
			ending_ = buf;
//if(ret) {
//printf("\nCachePatQuery.fetch: %d \"%s\"\n", stemNumber_, ending_.c_str());
//fflush(stdout);
//}
			return ret;
		}
	}
	if(_bitMask == (NUMBER_BIT | GENDER_BIT | PERSON_BIT | CASE_BIT)) {
		if(odata_pn_gc_nc_pc_cc) {
			char buf[9];
			int ret = oquery_pn_gc_nc_pc_cc->fetch( & _stemNumber, buf);
			stemNumber_ = _stemNumber;
			ending_ = buf;
//if(ret) {
//printf("\nCachePatQuery.fetch: %d \"%s\"\n", stemNumber_, ending_.c_str());
//fflush(stdout);
//}
			return ret;
		}
	}
	if(_bitMask == (NUMBER_BIT | GENDER_BIT | PERSON_BIT | TENSE_BIT)) {
		if(odata_pn_gc_nc_pc_tc) {
			char buf[9];
			int ret = oquery_pn_gc_nc_pc_tc->fetch( & _stemNumber, buf);
			stemNumber_ = _stemNumber;
			ending_ = buf;
//if(ret) {
//printf("\nCachePatQuery.fetch: %d \"%s\"\n", stemNumber_, ending_.c_str());
//fflush(stdout);
//}
			return ret;
		}
	}
	if(_bitMask == (NUMBER_BIT | GENDER_BIT | CASE_BIT)) {
		if(odata_pn_gc_nc_cc) {
			char buf[9];
			int ret = oquery_pn_gc_nc_cc->fetch( & _stemNumber, buf);
			stemNumber_ = _stemNumber;
			ending_ = buf;
//if(ret) {
//printf("\nCachePatQuery.fetch: %d \"%s\"\n", stemNumber_, ending_.c_str());
//fflush(stdout);
//}
			return ret;
		}
	}
	if(_bitMask == 
		(NUMBER_BIT | GENDER_BIT | DECLENSION_BIT | CASE_BIT | DEGREE12_BIT)) {
		if(odata_pn_gc_nc_dc_cc_deg) {
			char buf[9];
			int ret = oquery_pn_gc_nc_dc_cc_deg->fetch( & _stemNumber, buf);
			stemNumber_ = _stemNumber;
			ending_ = buf;
//if(ret) {
//printf("\nCachePatQuery.fetch: %d \"%s\"\n", stemNumber_, ending_.c_str());
//fflush(stdout);
//}
			return ret;
		}
	}

	if(_bitMask == (NUMBER_BIT | DEGREE7_BIT)) {
		if(odata_pn_nc_deg) {
			char buf[9];
			int ret = oquery_pn_nc_deg->fetch( & _stemNumber, buf);
			stemNumber_ = _stemNumber;
			ending_ = buf;
//if(ret) {
//printf("\nCachePatQuery.fetch: %d \"%s\"\n", stemNumber_, ending_.c_str());
//fflush(stdout);
//}
			return ret;
		}
	}
	if(_bitMask == DEGREE12_BIT || _bitMask==DEGREE7_BIT) {
		if(odata_pn_deg) {
			char buf[9];
			int ret = oquery_pn_deg->fetch( & _stemNumber, buf);
			stemNumber_ = _stemNumber;
			ending_ = buf;
//if(ret) {
//printf("\nCachePatQuery.fetch: %d \"%s\"\n", stemNumber_, ending_.c_str());
//fflush(stdout);
//}
			return ret;
		}
	}

	if(_bitMask == (NUMBER_BIT)) {
		if(odata_pn_nc) {
			char buf[9];
			int ret = oquery_pn_nc->fetch( & _stemNumber, buf);
			stemNumber_ = _stemNumber;
			ending_ = buf;
//if(ret) {
//printf("\nCachePatQuery.fetch: %d \"%s\"\n", stemNumber_, ending_.c_str());
//fflush(stdout);
//}
			return ret;
		}
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
   
   if (result)
   {
      stemNumber_ = _colStemNumber->AsInteger();
      ending_ = _colEnding->AsString();
//if(_bitMask == (NUMBER_BIT | DEGREE7_BIT)) {
//printf("\nCachePatQuery.fetch: %d \"%s\"\n", stemNumber_, ending_.c_str());
//fflush(stdout);
//}
   }
   
   return result;
}
