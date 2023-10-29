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
program. If not, write to Globalware AG, Hospitalstraße 6, D-99817 Eisenach.

Linux port modifications and additions by Bernd Kiefer, Walter Kasper,
Deutsches Forschungszentrum fuer kuenstliche Intelligenz (DFKI)
Stuhlsatzenhausweg 3, D-66123 Saarbruecken
*/

#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif

#include <logos_libs/dbcache/CachePatQuery_pn_gc_nc_pc_tc.h>
#include <logos_libs/dbcache/CachePatData_pn_gc_nc_pc_tc.h>
#include <logos_libs/dbcache/CacheKey.h>

CachePatQuery_pn_gc_nc_pc_tc::CachePatQuery_pn_gc_nc_pc_tc(CachePatData_pn_gc_nc_pc_tc *bd,
		int pn_, int gc_, int nc_, int pc_, int tc_) {
	odata_pn_gc_nc_pc_tc = bd;
	pn = pc_;
	gc = gc_;
	nc = nc_;
	pc = pc_;
	tc = tc_;
	start_index = end_index = curr_index = -1;
}
	
CachePatQuery_pn_gc_nc_pc_tc::~CachePatQuery_pn_gc_nc_pc_tc(){}

int
CachePatQuery_pn_gc_nc_pc_tc::query(int pn_, int gc_, int nc_, int pc_, int tc_) {
	char keybuf[256];

	pn = pn_;
	gc = gc_;
	nc = nc_;
	pc = pc_;
	tc = tc_;
	CacheKey *k = (CacheKey *)keybuf;

	start_index = end_index = curr_index = -1;
	
	odata_pn_gc_nc_pc_tc->makeKey(k, pn, gc, nc, pc, tc, 0);

	start_index = odata_pn_gc_nc_pc_tc->getRowIndex(k);
	int ret = 0;
	if(start_index!=-1) {
		end_index = start_index + 1;
		while((pn==odata_pn_gc_nc_pc_tc->getPatNumber(end_index))
			&& (nc==odata_pn_gc_nc_pc_tc->getNumberCode(end_index))
			&& (gc==odata_pn_gc_nc_pc_tc->getGenderCode(end_index))
			&& (pc==odata_pn_gc_nc_pc_tc->getPersonCode(end_index))
			&& (tc==odata_pn_gc_nc_pc_tc->getTenseCode(end_index))) {
			end_index++;
		}
		curr_index = start_index;
		ret = end_index-start_index;
	}
//printf("\nfetching (%d %d %d %d %d): %d %d %d\n", pn, gc, nc, pc, tc,
//	start_index, end_index, odata_pn_gc_nc_pc_tc->getStemNumber(start_index));

	return ret;
}

bool
CachePatQuery_pn_gc_nc_pc_tc::fetch(int *sn, char *ending) {
	if(curr_index==-1 || curr_index >= (odata_pn_gc_nc_pc_tc->getSize()-1))
		return false;

	*sn = odata_pn_gc_nc_pc_tc->getStemNumber(curr_index);
	strcpy(ending, odata_pn_gc_nc_pc_tc->getEnding(curr_index));

	curr_index++;
	return (curr_index <= end_index);
}
