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
#ifndef _TRAN_RULE_IO_
#define _TRAN_RULE_IO_

#define VTRS_TAB_NUMS 460
#define VTR_NUMS 26

//struct vtrbuf {	short vtr[VTRS_TAB_NUMS]; };
struct diag_vtr_struct {
	short dgvtr[21][VTR_NUMS];
	short dgtag;
	short dgvtrx[21][VTR_NUMS];
	short dgtagx;
};

#ifndef _TRAN_RULE_IO_IMPL_

// tran functions
int lstng1(int ix);
int numng1(int ix);
int lstnx1(int ix);
int numnx1(int ix);

int rule_load(int tran_number, int miniflg, int twoPasses, int pass_number);
int rule_unload();

int rulein(short* ruleset, short* matpos,
		short rule_buffer[], short* rule_number, short* ret_flag);
int rulein_ptr(short* ruleset, short* matpos,
		short **rule_buffer, short* rule_number, short* ret_flag);

int ovrin(short* ruleset, char* rule_buffer,
		short rule_number, short part_number);

int vtrin(short* ruleset, short* rule_buffer,
		short* rule_number, short part_number);

int comin(short* ruleset, char* rule_buffer, short rule_number);

void idxval(short *ruleset,
		short *swc, short *type, short *start, short *number);

void vtrld(int *ruleset,  short* rule_number, short *retsw,
		short* vtrs, struct diag_vtr_struct* vtrsDiag);

void vtr_t_ld(short *tblid, short* rule_number,
		short *ret_vtrcnt, short *ret_vtrtbl,
		short *ret_table_num,  short *retsw);

// tranform functions
int tran_form_load( char* vno_formres );
int tran_o3b_load( short* buf );

void print_tran_rule(FILE* file, int type, int rule_number);
void print_table(FILE* file, int type, int table_number, int numParms, short* parms);

#endif  // !_TRAN_RULE_IO_IMPL_

#endif //  !_TRAN_RULE_IO_
