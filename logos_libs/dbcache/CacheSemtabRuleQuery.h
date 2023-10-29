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
#ifndef _CacheSemtabRuleQueryClass
#define _CacheSemtabRuleQueryClass

// this is to replace DB-query called from "semtabrulebuilder.cpp"

class CacheSemtabRuleData;

class DLLEXPORT CacheSemtabRuleQuery {

public:

// For statement F key is (char deactivation_switch, int word_class_code,
// int subset_id, itn set_id, char company_code[4])
	CacheSemtabRuleQuery(CacheSemtabRuleData *bd, char ds, int wcc,int subsid, int setid, char *cc);
	~CacheSemtabRuleQuery();

	int query(char ds, int wcc,int subsid, int setid, char *cc);
	bool fetch(int *semid,
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
		int *rl, int *cpec, int *nb,
		unsigned char *blob);

private:
	CacheSemtabRuleData *fdata;
	char ds[2];
	char cc[4];
	int wcc;
	int subsid;
	int setid;
	int start_index;
	int end_index;
	int curr_index;
};

#endif // _CacheSemtabRuleQueryClass
