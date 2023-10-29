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

#include <logos_include/logoscommon.h>
#include <logos_libs/sql/sqlconnection.h>
#include <logos_libs/sql/sqlstatement.h>
#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif
#include <logos_libs/dbcache/CacheSemtabRuleData.h>
#include <logos_libs/dbcache/CacheKey.h>
#include <logos_libs/dbcache/CacheHashTab.h>

static DWORD t0, t1, t2;
static double thash, tload;

CacheSemtabRuleData::CacheSemtabRuleData(SqlConnection *con, int srcl, int targl,
		bool loader, bool fromFile, bool saveIt) :
	CacheData(con, srcl, targl, "SemtabRule", loader, fromFile, saveIt) {

	row_len = sizeof(struct f_row);
	max_key_len = SEMTAB_RULE_KEYLEN;

	mapAndLoad();

}

void
CacheSemtabRuleData::getSizeFromDB() {
	stmt = (SqlStatement *)oracleConnection->CreateStatement();
	stmtText = " select count(*) "
		" from semtab_data "
		" where source_language_code = :sl " 
                " and target_language_code = :tl ";
          // _fix_me_ why does postgres complain here ?
//		" order by deactivation_switch, word_class_code, subset_id, "
//		" set_id, company_code";

	stmt->AddToCommandString(stmtText);
	stmt->Parse();

	stmt->BindInputIntToString(":sl","%2d",source_language_id,source_language);
	stmt->BindInputIntToString(":tl","%2d",target_language_id,target_language);

	SqlColumn *cnt = stmt->BindOutputColumn(1, SqlColumn::Integer);
	stmt->Execute();
	stmt->Fetch();
	size = cnt->AsInteger() + 1;
	delete stmt;
printf("SemtabRule size from DB = %d\n", size);
fflush(stdout);
}

bool
CacheSemtabRuleData::populateFromDatabase() {
	char keybuf[256];
	int weights[256];
	int nletters = 0;
	int printstep = 1024;

	printf("Loading from database (one # mark - %d records)\n",
		printstep);
	fflush(stdout);

	CLOCK_T t0 = GetTickCount();

        int i;
	for(i=0;i<256;i++)
		weights[i] = 0;

	stmt = (SqlStatement *)oracleConnection->CreateStatement();
	stmtText = " select Company_Code, deactivation_switch, "
		" semtab_id, word_class_code, subset_id, set_id, "
		" wc1, ty1, fm1, wc2, ty2, fm2, wc3, ty3, fm3, "
		" wc4, ty4, fm4, wc5, ty5, fm5, wc6, ty6, fm6, "
		" wc7, ty7, fm7, wc8, ty8, fm8, wc9, ty9, fm9, "
		" tag11, tag12, tag21, tag22, tag31, tag32, "
		" tag41, tag42, tag51, tag52, tag61, tag62, "
		" tag71, tag72, tag81, tag82, tag91, tag92, "
		" rule_level, specificity, numbytesindatablob, data_blob "
		" from semtab_data "
		" where source_language_code = :sl "
                " and target_language_code = :tl "
		" order by deactivation_switch, word_class_code, subset_id, "
		" set_id, company_code";

	stmt->AddToCommandString(stmtText);
	stmt->Parse();

	stmt->BindInputIntToString(":sl","%2d",source_language_id,source_language);
	stmt->BindInputIntToString(":tl","%2d",target_language_id,target_language);

	SqlColumn *cc=stmt->BindOutputColumn(1,SqlColumn::StringType);
	SqlColumn *ds=stmt->BindOutputColumn(2, SqlColumn::StringType);
	SqlColumn *si=stmt->BindOutputColumn(3, SqlColumn::Integer);
	SqlColumn *wcc=stmt->BindOutputColumn(4, SqlColumn::Integer);
	SqlColumn *subsid=stmt->BindOutputColumn(5, SqlColumn::Integer);
	SqlColumn *setid=stmt->BindOutputColumn(6, SqlColumn::Integer);

	SqlColumn *wc1=stmt->BindOutputColumn(7, SqlColumn::Integer);
	SqlColumn *ty1=stmt->BindOutputColumn(8, SqlColumn::Integer);
	SqlColumn *fm1=stmt->BindOutputColumn(9, SqlColumn::Integer);

	SqlColumn *wc2=stmt->BindOutputColumn(10, SqlColumn::Integer);
	SqlColumn *ty2=stmt->BindOutputColumn(11, SqlColumn::Integer);
	SqlColumn *fm2=stmt->BindOutputColumn(12, SqlColumn::Integer);

	SqlColumn *wc3=stmt->BindOutputColumn(13, SqlColumn::Integer);
	SqlColumn *ty3=stmt->BindOutputColumn(14, SqlColumn::Integer);
	SqlColumn *fm3=stmt->BindOutputColumn(15, SqlColumn::Integer);

	SqlColumn *wc4=stmt->BindOutputColumn(16, SqlColumn::Integer);
	SqlColumn *ty4=stmt->BindOutputColumn(17, SqlColumn::Integer);
	SqlColumn *fm4=stmt->BindOutputColumn(18, SqlColumn::Integer);

	SqlColumn *wc5=stmt->BindOutputColumn(19, SqlColumn::Integer);
	SqlColumn *ty5=stmt->BindOutputColumn(20, SqlColumn::Integer);
	SqlColumn *fm5=stmt->BindOutputColumn(21, SqlColumn::Integer);

	SqlColumn *wc6=stmt->BindOutputColumn(22, SqlColumn::Integer);
	SqlColumn *ty6=stmt->BindOutputColumn(23, SqlColumn::Integer);
	SqlColumn *fm6=stmt->BindOutputColumn(24, SqlColumn::Integer);

	SqlColumn *wc7=stmt->BindOutputColumn(25, SqlColumn::Integer);
	SqlColumn *ty7=stmt->BindOutputColumn(26, SqlColumn::Integer);
	SqlColumn *fm7=stmt->BindOutputColumn(27, SqlColumn::Integer);

	SqlColumn *wc8=stmt->BindOutputColumn(28, SqlColumn::Integer);
	SqlColumn *ty8=stmt->BindOutputColumn(29, SqlColumn::Integer);
	SqlColumn *fm8=stmt->BindOutputColumn(30, SqlColumn::Integer);

	SqlColumn *wc9=stmt->BindOutputColumn(31, SqlColumn::Integer);
	SqlColumn *ty9=stmt->BindOutputColumn(32, SqlColumn::Integer);
	SqlColumn *fm9=stmt->BindOutputColumn(33, SqlColumn::Integer);

	SqlColumn *tag11=stmt->BindOutputColumn(34,SqlColumn::Integer);
	SqlColumn *tag12=stmt->BindOutputColumn(35,SqlColumn::Integer);
	SqlColumn *tag21=stmt->BindOutputColumn(36,SqlColumn::Integer);
	SqlColumn *tag22=stmt->BindOutputColumn(37,SqlColumn::Integer);
	SqlColumn *tag31=stmt->BindOutputColumn(38,SqlColumn::Integer);
	SqlColumn *tag32=stmt->BindOutputColumn(39,SqlColumn::Integer);
	SqlColumn *tag41=stmt->BindOutputColumn(40,SqlColumn::Integer);
	SqlColumn *tag42=stmt->BindOutputColumn(41,SqlColumn::Integer);
	SqlColumn *tag51=stmt->BindOutputColumn(42,SqlColumn::Integer);
	SqlColumn *tag52=stmt->BindOutputColumn(43,SqlColumn::Integer);
	SqlColumn *tag61=stmt->BindOutputColumn(44,SqlColumn::Integer);
	SqlColumn *tag62=stmt->BindOutputColumn(45,SqlColumn::Integer);
	SqlColumn *tag71=stmt->BindOutputColumn(46,SqlColumn::Integer);
	SqlColumn *tag72=stmt->BindOutputColumn(47,SqlColumn::Integer);
	SqlColumn *tag81=stmt->BindOutputColumn(48,SqlColumn::Integer);
	SqlColumn *tag82=stmt->BindOutputColumn(49,SqlColumn::Integer);
	SqlColumn *tag91=stmt->BindOutputColumn(50,SqlColumn::Integer);
	SqlColumn *tag92=stmt->BindOutputColumn(51,SqlColumn::Integer);

	SqlColumn *rl = stmt->BindOutputColumn(52,SqlColumn::Integer);
	SqlColumn *spec = stmt->BindOutputColumn(53,SqlColumn::Integer);
	SqlColumn *nb = stmt->BindOutputColumn(54,SqlColumn::Integer);
	SqlColumn *blob = stmt->BindOutputColumn(55,SqlColumn::Long_Row);

	stmt->Execute();

	nrows = 0;

// first load the data and calculate weights
	CacheKey *key = (CacheKey *)keybuf;
	while(stmt->Fetch()) {
		struct f_row *lrp = getRow(nrows);
		strcpy(lrp->company_code, cc->AsCharArray());
		lrp->source_language_code = source_language;
		lrp->target_language_code = target_language;
		strcpy(lrp->deactivation_switch, ds->AsCharArray());
		lrp->semtab_id = si->AsInteger();
		lrp->word_class_code = wcc->AsInteger();
		lrp->subset_id = subsid->AsInteger();
		lrp->set_id = setid->AsInteger();

		lrp->wc1 = wc1->AsInteger();
		lrp->wc2 = wc2->AsInteger();
		lrp->wc3 = wc3->AsInteger();
		lrp->wc4 = wc4->AsInteger();
		lrp->wc5 = wc5->AsInteger();
		lrp->wc6 = wc6->AsInteger();
		lrp->wc7 = wc7->AsInteger();
		lrp->wc8 = wc8->AsInteger();
		lrp->wc9 = wc9->AsInteger();

		lrp->ty1 = ty1->AsInteger();
		lrp->ty2 = ty2->AsInteger();
		lrp->ty3 = ty3->AsInteger();
		lrp->ty4 = ty4->AsInteger();
		lrp->ty5 = ty5->AsInteger();
		lrp->ty6 = ty6->AsInteger();
		lrp->ty7 = ty7->AsInteger();
		lrp->ty8 = ty8->AsInteger();
		lrp->ty9 = ty9->AsInteger();

		lrp->fm1 = fm1->AsInteger();
		lrp->fm2 = fm2->AsInteger();
		lrp->fm3 = fm3->AsInteger();
		lrp->fm4 = fm4->AsInteger();
		lrp->fm5 = fm5->AsInteger();
		lrp->fm6 = fm6->AsInteger();
		lrp->fm7 = fm7->AsInteger();
		lrp->fm8 = fm8->AsInteger();
		lrp->fm9 = fm9->AsInteger();

		lrp->tag11 = tag11->AsInteger();
		lrp->tag21 = tag21->AsInteger();
		lrp->tag31 = tag31->AsInteger();
		lrp->tag41 = tag41->AsInteger();
		lrp->tag51 = tag51->AsInteger();
		lrp->tag61 = tag61->AsInteger();
		lrp->tag71 = tag71->AsInteger();
		lrp->tag81 = tag81->AsInteger();
		lrp->tag91 = tag91->AsInteger();

		lrp->tag12 = tag12->AsInteger();
		lrp->tag22 = tag22->AsInteger();
		lrp->tag32 = tag32->AsInteger();
		lrp->tag42 = tag42->AsInteger();
		lrp->tag52 = tag52->AsInteger();
		lrp->tag62 = tag62->AsInteger();
		lrp->tag72 = tag72->AsInteger();
		lrp->tag82 = tag82->AsInteger();
		lrp->tag92 = tag92->AsInteger();

		lrp->rule_level = rl->AsInteger();
		lrp->specificity = spec->AsInteger();
		lrp->numbytesindatablob = nb->AsInteger();
		memcpy(lrp->data_blob, (const char *)blob->AsLongRaw(), 
			lrp->numbytesindatablob);

// key is (word_class_code, subset_id, set_id, company_code)
		unsigned char *cp = (unsigned char *) lrp->company_code;
		int len = strlen(lrp->company_code);
		for(i=0;i<len;i++) {
			int val = *cp;
			weights[val]++;
			nletters++;
			cp++;
		}

		cp = (unsigned char *) lrp->deactivation_switch;
		weights[*cp]++;
		nletters++;

		cp = (unsigned char *) & lrp->word_class_code;
		for(i=0;i<sizeof(int);i++) {
			int val = *cp;
			weights[val]++;
			nletters++;
			cp++;
		}

		cp = (unsigned char *) & lrp->set_id;
		for(i=0;i<sizeof(int);i++) {
			int val = *cp;
			weights[val]++;
			nletters++;
			cp++;
		}
		cp = (unsigned char *) & lrp->subset_id;
		for(i=0;i<sizeof(int);i++) {
			int val = *cp;
			weights[val]++;
			nletters++;
			cp++;
		}

		nrows++;

		if(nrows%printstep==0) {
			printf("#");
			fflush(stdout);
		}
	}

	delete stmt;

// calc weight and reverse it because it's faster to multiply than divide
// (later, when computing hash runtime).
        int j;
	for(j=0;j<256;j++) {
		if(weights[j])
			weights_array[j] = nletters/((double)weights[j]);
		else
			weights_array[j] = 0.;
	}
	CLOCK_T t1 = GetTickCount();
	tload = (t1-t0)/1000.;
	printf("\nDone loading, %d records, load time %4.1f sec\n", nrows, tload);

	printf("Hashing...");
	fflush(stdout);
	for(j=0;j<nrows;j++) {
//printf("row # %d, usage_id = %d\n", j, lrp->usage_id);
//fflush(stdout);
		struct f_row *lrp = getRow(j);
		makeKey(key, lrp->company_code, lrp->deactivation_switch[0],
			lrp->word_class_code, lrp->subset_id, lrp->set_id, j);
		hashtab->put(key);
	}

	CLOCK_T t2 = GetTickCount();
	thash = (t2-t1)/1000.;
	printf("done, time %4.1f sec\n", thash);

	hashtab->calculateStat();
	hashtab->print();

	fflush(stdout);

	return true;
}

void
CacheSemtabRuleData::makeKey(CacheKey *key, char *cc, char ds, int wcc,
		int subsid, int sid, int val) {
	char *keybuf = (char *)key->getBuffer();
	memset(key, 0, max_key_len);

	char *cp = (char *) & wcc;
	memcpy(keybuf, cp, sizeof(int));
	int len = sizeof(int);
	keybuf += sizeof(int);

	cp = (char *) & subsid;
	memcpy(keybuf, cp, sizeof(int));
	len += sizeof(int);
	keybuf += sizeof(int);

	cp = (char *) & sid;
	memcpy(keybuf, cp, sizeof(int));
	len += sizeof(int);
	keybuf += sizeof(int);

	int tmplen = strlen(cc);
	memcpy(keybuf, cc, tmplen);
	len += tmplen;
	keybuf += tmplen;

	*keybuf = ds;
	keybuf++;
	len++;

	key->setLength(len);
	key->setValue(val);
	key->setNextIndex(-1);

//printf("\"%s\", %c %d %d %d: hash %d, val %d\n", cc, ds, wcc, subsid, sid,
//	   hashtab->computeHashIndex(key), val);
//fflush(stdout);
}
