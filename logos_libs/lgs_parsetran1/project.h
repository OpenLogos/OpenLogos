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
void blk_data(void);
void copy144(void);
void diagno(long);
void elemld(void);
void getprs(long);
void init(void);
void main__(void);
void mopup(void);
void nmatch(long,long,short*);
void prevtr(void);
void rswork_build(void);
void se0537(long);
void semsr1(short*);
void semsw1(short,short,short*);
void semtr1(void);
void slotld(void);
void specmp(long,long,long,short[],short[],short[],short[],
	short*);
void sw68a(void);
void t1load(void);
void t1se26(void);
void t1sw11(short*);
void t1sw12(void);
void t1sw13(void);
void t1sw14(void);
void t1sw15(void);
void t1sw16(void);
void t1sw18(void);
void t1sw19(void);
void t1sw20(void);
void t1sw21(void);
void t1sw22(void);
void t1sw23(void);
void t1sw24(void);
void t1sw25(void);
void t1sw26(void);
void t1sw29(void);
void t1sw30(void);
void t1sw31(short*);
void t1sw33(void);
void t1sw34(void);
void t1sw35(void);
void t1sw36(void);
void t1sw37(void);
void t1sw38(void);
void t1sw39(void);
void t1sw42(void);
void t1sw43(void);
void t1sw44(void);
void t1sw46(void);
void t1sw48(void);
int tablet(void);
void tagcmp(long,short[],short[],long,short[],short[],long,
	short*);
//cat-102 void tmatch(long*,short*);
void tmatch(long*,short*, short);
void vbrnc1(long);
void vtrend(void);
void vtrpro(void);
