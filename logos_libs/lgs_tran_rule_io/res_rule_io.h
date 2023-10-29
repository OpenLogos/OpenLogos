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
#ifndef _RES_RULE_IO_
#define _RES_RULE_IO_

#ifndef _RES_RULE_IO_IMPL_

// res functions
int NENT1(short* ix);
int DISSP1(short* ix);
int NENT2(short* ix);
int DISSP2(short* ix);
int NENTX(short* ix);
int DISSPX(short* ix);
int R1RENT(short* z, short* lev, short* x);
int R2RENT(short* z, short* lev, short* x);

int OVRIN(short* rules, short* rule_buffer, short* rule_number, short* part_number);
int RULEIN(short* rules, short* rule_buffer, short* rule_number);
short * RULEIN_OCR(short rules, short rule_number);
void res_unload();
int res_rule_load(int *res1_mini, int *res2_mini, int fromCache);
int VTRIN(int rules, short* rule_buffer, int rule_number);
int COMIN(short* rules, char* rule_buffer, short* rule_number);
void IDXVAL(short* swc, short* type, short *start, short* number);
void print_res_rule(FILE* file, int type, int rule_number);


// resform funstions
int res_form_load( short* rsfrm_csf, char* vn_formres );


#endif  // !_RES_RULE_IO_IMPL_

#endif //  !_RES_RULE_IO_
