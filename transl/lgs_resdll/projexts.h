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
/*projexts.h - max. external structure size (common transl)*/

#ifndef EXTERN
#define EXTERN extern
#endif


#define MAX_ELEMENTS 70

EXTERN struct t_ovptrX_ {
	short int ovptr;
	}	ovptrX_;

EXTERN struct t_valxX_ {
	short int i4x, match;
	}	valxX_;

EXTERN struct t_nloopX_ {
	short int nloop[6], nloop2[6];
	}	nloopX_;

EXTERN struct t_commiX_ {
	short int i4;
	}	commiX_;
EXTERN struct t_commkX_ {
	short int k12;
	}	commkX_;

EXTERN struct t_sw22ptX_ {
	short int sw22pt;
	}	sw22ptX_;
EXTERN struct t_sw22chX_ {
	short int sw22ch;
	}	sw22chX_;


EXTERN struct t_commvX_ {
	short int vn, vtrptr;
	}	commvX_;

EXTERN struct t_negnodX_ {
	short int no3res, negnod[3];
	}	negnodX_;

EXTERN struct t_negmtcX_ {
	short int no2res, negmtc[3];
	}	negmtcX_;

EXTERN struct t_ikX_ {
	short int ik;
	}	ikX_;

EXTERN struct t_jbgusX_ {
	short int jgus[70];
	}	jbgusX_;
EXTERN struct t_jgus63X_ {
	short int jgus63[70];
	}	jgus63X_;

EXTERN struct t_chgsemX_ {
	short int semchg[10][3];
	}	chgsemX_;

EXTERN struct t_tagflwX_ {
	short int tagflw[22];
	}	tagflwX_;

EXTERN struct t_resswX_ {
	short int ressw, minisw;
	}	resswX_;

// note in the following structure the overfl data type was 21 for some
// rules and 22 for other.
EXTERN	struct  {
		short int *rule, overfl[22];
} resrulX_;

EXTERN struct t_vtrfsX_ {
	short int vtrfs[100];
	}	vtrfsX_;

EXTERN struct t_overidX_ {
	short int rulovr[10][13], ovrovr[10][22];
	}	overidX_;


EXTERN	struct  {
	short int savtyr[70][3], savfrm[70][3], savwc[70][3], subchg[70][3], 
	          savsup[70][3], savsub[70][3];
} savtyX_;

EXTERN struct  {
		short int cell[100], csaray[70][40], csasav[70][40];
} cellX_;

EXTERN struct t_osbchgX_ {
	short int osbchg[70][3];
	}	osbchgX_;

EXTERN struct  {
		short int li, lstnod;
} commlX_;

EXTERN	struct  {
		short int srch07, n65nod[3], n65res;
} srch07X_;

EXTERN	struct  {
		short int k12s, i4pli, iksave, i4sav, lvlgus, jgusav[70];
} miniX_;

EXTERN struct t_tag63X_ {
	short int srch63, tag63[10], prm63a[10], prm63b[10], prm63c[10];
	}	tag63X_;

EXTERN struct t_cinitX_ {
	short int cinit[20][26];
	}	cinitX_;


EXTERN struct t_mtcinfX_ {
	short int swcp, iks, jx;
	}	mtcinfX_;

EXTERN struct {
        short swork[MAX_ELEMENTS][15];
        short scont1[MAX_ELEMENTS];
        short scont2[MAX_ELEMENTS];
        short ecount;
        short scont3[MAX_ELEMENTS];
} swX_;
EXTERN struct {
        int targ[MAX_ELEMENTS][3];
} targX_;
/*
 *					state of cap,quote,underline,etc...
 *					in general 0=not set   1=set/true
 *					1 = capitalization
 *						0 = not
 *						1 = initial cap
 *						2 = all cap
 *					2 = single quote
 *					3 = double quote
 *					4 = underline
 *					5 = bold
 *					6 = italics
 *					7 = 0=not found 1=found   in dictionary
 */
EXTERN struct {
        short wstate[MAX_ELEMENTS][7];
} wstateX_;
EXTERN struct t_resnegX_ {
	short int nwcr1[9][8], nwcr2[9][8], nwcr22[9][8];
	}	resnegX_;
EXTERN struct t_resne2X_ {
	short int gnwc1[9][8], gnwc2[9][8], gnwc22[9][8];
	}	resne2X_;
EXTERN struct t_restabX_ {
	short int restab[MAX_ELEMENTS];
	}	restabX_;
EXTERN struct {
        short ofl4r[MAX_ELEMENTS][3];
        short ofl1r[MAX_ELEMENTS][3];
} ofltagX_;
EXTERN struct {
        short xpatfm[MAX_ELEMENTS][3];
        //      char s[420];
} xpatnfX_;
EXTERN struct {
        short hennum[MAX_ELEMENTS][2];
        short root_hennum[MAX_ELEMENTS][2];
} hensavX_;
EXTERN struct {
        short ofl2r[MAX_ELEMENTS][3];
        short ofl3r[MAX_ELEMENTS][3];
		short int auxiliaryCode[MAX_ELEMENTS][3];
} ofl2a3X_;
EXTERN struct {
        short hashcd[MAX_ELEMENTS][2];
        short root_hashcd[MAX_ELEMENTS][2];
} hashX_;
EXTERN struct {
        short patno[MAX_ELEMENTS][3];
        short stemno[MAX_ELEMENTS][3];
} ptdiagX_;

EXTERN struct {
        short respas[MAX_ELEMENTS][3];
} respasX_;
EXTERN struct {
        short scncel[MAX_ELEMENTS][40];
} scncelX_;

