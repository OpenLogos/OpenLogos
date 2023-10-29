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
#  define EXTERN extern
#endif
#define OPADRX 450	
#define SCONX 150
#define SCONX1 20
#define SCONY 450  

#include "parsetrans_ext.h"


EXTERN struct t_flowckX_ {
	short int im1, i3save, i3str, n6, n6jim, n6jims, phrstr, phrlst,strphr;
	}	flowckX_;
EXTERN 	struct  {
	short int k7m, minifg, minilp;
	}minickX_;

EXTERN struct t_diacb3X_ {
	short int k7, oflad;
	}	diacb3X_;

EXTERN struct t_hfdm1X_ {
	short int hfdm1[30];
	}	hfdm1X_;
EXTERN struct t_neg3X_ {
	short int negwc3[9][8];
	}	neg3X_;
EXTERN struct t_gneg3X_ {
	short int gngwc3[9][8];
	}	gneg3X_;
EXTERN struct t_jbc3X_ {
	short int jbc3;
	}	jbc3X_;
EXTERN struct t_pars26X_ {
	short int pars26[9];
	}	pars26X_;
EXTERN struct t_wcnameX_ {
	char wcname[29][9];
	}	wcnameX_;
EXTERN struct t_vtrfvlX_ {
	char vtmnd, vtmrp, vtmel, vtmpp, vtmwk, vtmsl, vtmvc, vtmpu, vtmhi, 
	  vtmlo, vtmsw;
	}	vtrfvlX_;
EXTERN struct t_vtrfbiX_ {
	short int slotel, slotrp;
	}	vtrfbiX_;
EXTERN struct t_sw18bkX_ {
	short int phrnew;
	}	sw18bkX_;
EXTERN struct t_sw36bkX_ {
	short int rel, relcas, relgen, relnum, relper;
	}	sw36bkX_;
EXTERN struct t_sw38bkX_ {
	short int sw38n, gb108;
	}	sw38bkX_;

EXTERN struct t_head26X_ {
	short int headwc, headty, headfr, headhd, headt2, headt3;
	}	head26X_;
EXTERN struct t_loopckX_ {
	short int imatch[ELMMAX], noloop[21], nptpsv, nptpx, call36[2];
	}	loopckX_;
EXTERN struct t_sploopX_ {
	short int st16, li;
	}	sploopX_;
EXTERN struct t_sw14bkX_ {
	short int gen, num, per, sw14n;
	}	sw14bkX_;
EXTERN struct t_sw26nX_ {
	short int sw26n;
	}	sw26nX_;
EXTERN struct t_sw28fmX_ {
	short int sw28fm;
	}	sw28fmX_;
EXTERN struct t_nloop2X_ {
	short int nloop2[21];
	}	nloop2X_;
EXTERN struct t_paglinX_ {
	long int lineno;
	short int pagend, linend;
	}	paglinX_;
EXTERN struct t_sw31bkX_ {
	short int cause, comp, depcl, inform, lpflag, offlag;
	}	sw31bkX_;
EXTERN struct t_getvtrX_ {
	short int getvtr;
	}	getvtrX_;
EXTERN struct t_semargX_ {
	short int nwrks, index, k3p3, pntr9, tran, allsem;
	}	semargX_;
EXTERN struct t_vtrs42X_ {
	short int sw42n;
	}	vtrs42X_;
EXTERN struct t_vwarg1X_ {
	short int look50[3], chng50[3], wc50el[3], isave, omfrm;
	}	vwarg1X_;
EXTERN struct t_commdX_ {
	short int disam, ovrsw, vtrsw, matpos;
	}	commdX_;
EXTERN struct t_baskspX_ {
	short int bsdflt;
	}	baskspX_;
EXTERN struct t_sw42bkX_ {
	short int im1sav, i3keep, likeep;
	}	sw42bkX_;
EXTERN struct t_sp3yX_ {
	short int ovrflw[21];
	}	sp3yX_;
