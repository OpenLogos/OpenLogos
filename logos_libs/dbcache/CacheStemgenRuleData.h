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
#ifndef _CacheStemgenRuleDataClass
#define _CacheStemgenRuleDataClass

#define STEMGEN_RULE_KEYLEN 20	// sizeof(pat_number) + sizeof(stem_number)
			// + 3 * sizeof(int)

#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif
#include <logos_libs/dbcache/CacheData.h>

struct l_row {
	int pat_number;
	int stem_number;
	char add_ending[17];
	char drop_ending[9];
	char replace_rule[14];
	char add_prefix[11];
};

class DLLEXPORT CacheStemgenRuleData : public CacheData {

public:

	CacheStemgenRuleData(SqlConnection *con, int langcode, bool loader, bool ff, bool sav);
	void makeKey(CacheKey *, int pn, int sn, int val);
	virtual bool 	populateFromDatabase();
	virtual void getSizeFromDB();
	inline struct l_row * getRow(int i) {
		struct l_row *lrp = 
		(struct l_row *)((char *)data_array + i*sizeof(struct l_row));
		return lrp;
	}

/*
Statement "L", 5% of DBtime (after ABCDG have been done)

select add_ending, drop_ending, replace_rule, add_prefix
from stem_generation_rule
where
	language_code = :lc and pat_number=:pn and stem_number=:sn

select language_code, count(*),
	max(length(add_ending)) "c1", max(length(drop_ending)) "c2",
	max(length(replace_rule)) "c3",	max(length(add_prefix)) "c4"
from stem_generation_rule
group by language_code

LA  COUNT(*)        c1        c2        c3        c4
-- --------- --------- --------- --------- ---------
01      4152        14         5         8        10
02      1001        10         6         7          
03       956        10         6         7          
04      1318        15         6         6          
05      1069        12         8        13          
06      1174        16         5         4          
*/

	int stem_number;
	char add_ending[17];
	char drop_ending[9];
	char replace_rule[14];
	char add_prefix[11];

	inline int getPatNumber(int i) { return getRow(i)->pat_number; }
	inline int getStemNumber(int i) { return getRow(i)->stem_number; }
	inline char * getAddEnding(int i) {
		return getRow(i)->add_ending; }
	inline char * getDropEnding(int i) {
		return getRow(i)->drop_ending; }
	inline char * getReplaceRule(int i) {
		return getRow(i)->replace_rule; }
	inline char * getAddPrefix(int i) {
		return getRow(i)->add_prefix; }
};

#endif // _CacheStemgenRuleDataClass
