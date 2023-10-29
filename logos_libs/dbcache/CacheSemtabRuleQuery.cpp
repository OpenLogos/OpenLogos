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

#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif

#include <logos_libs/dbcache/CacheSemtabRuleQuery.h>
#include <logos_libs/dbcache/CacheSemtabRuleData.h>
#include <logos_libs/dbcache/CacheKey.h>

CacheSemtabRuleQuery::CacheSemtabRuleQuery(CacheSemtabRuleData *bd, char ds_,int wcc_,int subsid_, int setid_,
		char *cc_) {
	fdata = bd;
	ds[0] = ds_;
	wcc = wcc_;
	subsid = subsid_;
	setid = setid_;
	if(cc_)
		strncpy(cc, cc_, 4);
	start_index = end_index = curr_index = -1;
}
	
CacheSemtabRuleQuery::~CacheSemtabRuleQuery(){}

int
CacheSemtabRuleQuery::query(char ds_,int wcc_,int subsid_, int setid_, char *cc_) {
	char keybuf[256];

	ds[0] = ds_;
	wcc = wcc_;
	subsid = subsid_;
	setid = setid_;
	strncpy(cc, cc_, 4);

	CacheKey *k = (CacheKey *)keybuf;

	start_index = end_index = curr_index = -1;
	
	fdata->makeKey(k, cc, ds[0], wcc, subsid, setid, 0);

	start_index = fdata->getRowIndex(k);
//printf("CacheSemtabRuleQuery start_index %d\n", start_index);
	int ret = 0;
	if(start_index!=-1) {
		end_index = start_index + 1;
		while((wcc==fdata->getWordClassCode(end_index))
			&& (subsid == fdata->getSubsetId(end_index))
			&& (setid == fdata->getSetId(end_index))
			&& (ds[0] == fdata->getDeactivationSwitch(end_index))
			&& (strcmp(cc,fdata->getCompanyCode(end_index))==0))
			end_index++;
		curr_index = start_index;
		ret = end_index-start_index;
	}

	return ret;
}

bool
CacheSemtabRuleQuery::fetch(int *semid,
	int *wc1, int *ty1, int *fm1,
	int *wc2, int *ty2, int *fm2,
	int *wc3, int *ty3, int *fm3,
	int *wc4, int *ty4, int *fm4,
	int *wc5, int *ty5, int *fm5,
	int *wc6, int *ty6, int *fm6,
	int *wc7, int *ty7, int *fm7,
	int *wc8, int *ty8, int *fm8,
	int *wc9, int *ty9, int *fm9,
	int *tag11, int *tag12,
	int *tag21, int *tag22,
	int *tag31, int *tag32,
	int *tag41, int *tag42,
	int *tag51, int *tag52,
	int *tag61, int *tag62,
	int *tag71, int *tag72,
	int *tag81, int *tag82,
	int *tag91, int *tag92,
	int *rl, int *spec, int *nb,
	unsigned char *blob) {

	if(curr_index==-1 || curr_index >= (fdata->getSize()-1))
		return false;

	*semid = fdata->getSemtabId(curr_index);

	*wc1 = fdata->getWc1(curr_index);
	*ty1 = fdata->getTy1(curr_index);
	*fm1 = fdata->getFm1(curr_index);

	*wc2 = fdata->getWc2(curr_index);
	*ty2 = fdata->getTy2(curr_index);
	*fm2 = fdata->getFm2(curr_index);

	*wc3 = fdata->getWc3(curr_index);
	*ty3 = fdata->getTy3(curr_index);
	*fm3 = fdata->getFm3(curr_index);

	*wc4 = fdata->getWc4(curr_index);
	*ty4 = fdata->getTy4(curr_index);
	*fm4 = fdata->getFm4(curr_index);

	*wc5 = fdata->getWc5(curr_index);
	*ty5 = fdata->getTy5(curr_index);
	*fm5 = fdata->getFm5(curr_index);

	*wc6 = fdata->getWc6(curr_index);
	*ty6 = fdata->getTy6(curr_index);
	*fm6 = fdata->getFm6(curr_index);

	*wc7 = fdata->getWc7(curr_index);
	*ty7 = fdata->getTy7(curr_index);
	*fm7 = fdata->getFm7(curr_index);

	*wc8 = fdata->getWc8(curr_index);
	*ty8 = fdata->getTy8(curr_index);
	*fm8 = fdata->getFm8(curr_index);

	*wc9 = fdata->getWc9(curr_index);
	*ty9 = fdata->getTy9(curr_index);
	*fm9 = fdata->getFm9(curr_index);

	*tag11 = fdata->getTag11(curr_index);
	*tag12 = fdata->getTag12(curr_index);
	*tag21 = fdata->getTag21(curr_index);
	*tag22 = fdata->getTag22(curr_index);
	*tag31 = fdata->getTag31(curr_index);
	*tag32 = fdata->getTag32(curr_index);
	*tag41 = fdata->getTag41(curr_index);
	*tag42 = fdata->getTag42(curr_index);
	*tag51 = fdata->getTag51(curr_index);
	*tag52 = fdata->getTag52(curr_index);
	*tag61 = fdata->getTag61(curr_index);
	*tag62 = fdata->getTag62(curr_index);
	*tag71 = fdata->getTag71(curr_index);
	*tag72 = fdata->getTag72(curr_index);
	*tag81 = fdata->getTag81(curr_index);
	*tag82 = fdata->getTag82(curr_index);
	*tag91 = fdata->getTag91(curr_index);
	*tag92 = fdata->getTag92(curr_index);

	*rl = fdata->getRuleLevel(curr_index);
	*spec = fdata->getSpecificity(curr_index);
	*nb = fdata->getNumBytesInDataBlob(curr_index);

	memcpy(blob, fdata->getDataBlob(curr_index), *nb);

	curr_index++;
	return (curr_index <= end_index);
}
