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
void  txmini(
long int locflg,
short int *k7m,
short int *nptpx,
long int i,
short int *cb9
);
void bhset(long,long*);
void blkdata0(void);
void closej(void);
void cnv4fp(void);
void copy44(void);
void flip99(void);
void getadr(short[],short[],short[],long,short*,short*,short*,	short*);
void getptr(long,short*,long);
void irfix(long,long,long,long,long,long*);
void irfwm(long,long,long,long*);
void irinf(long,long,long,long,long*);
void jbfout(void);
void jobid(void);
void left(long,long,long,long*);
void npinit(void);
void nplsap(long,short*,short*);
void npopen(void);
void npread(void);
void npslct(long,short[][4],short[],short[],long,short[],short[],	short[],short*);
void npwrit(void);
void o3btab(long,long,long,long,long*,long);
void overid(long,short[],short[],short*);
void pagef(void);
void samlin(void);
void se0563(long,short*,long,short*);
void sem_diag(short[],short[],short[]);
void semsw1(short,short,short*);
void semsw2(short,short,short*);
void semtr1(void);
void semtr2(void);
void semty(long,long,long,short*,short*);
void sentnx(void);
void sents2(void);
void sentsv(void);
void set26(void);
void slotin(void);
void smov(long);
void specmp(long,long,long,short[],short[],short[],short[],	short*);
void spsrch(
	long int locflg,
	short int sp[],
	short int k7,
	short int ovrflw[],
	short int oflad,
	short int *wc50m, 
	short int wc50el[],
	short int look50[],
	short int chng50[],
	short int *wc50ct,
	short int *retflg
	);
void formod(
	long int locflg,
	long int  gusn,
	long int  k2,
	long int  i3,
	short int *retflg
	);
void sw68(void);
void swi68p(long,short*);
void swov(long);
void t0sw32(void);
void t0sw49(void);
void tagcmp(long,short[],short[],long,short[],short[],long,	short*);
void times(void);
void trat25(void);
void  trtag(
		long int *locflg,
		short int *retflg,
		short int  sp[],
		short int  ovrflw[],
		long int whch50,
		long int  stpprt,
		LOGICAL8 alldon,
		short rule_number
		);
void txsw48(long,long,long);
void txsw67(void);
void vbrnch(long);
void vcfill(long,long,long,long,short*,long,long*);
void vtrctl(void);
void vtrend(void);
void vtrfwr(void);
void vtrjmp(long,long,char *,long);
void vtrtbl(void);
void whicsp(long,short*,short[],short[],long);
int wrt2tr(void);
void xerout(void);
