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

#define	SWK1SZ	15
#define	HF4LIM	70

#include "parsetrans_ext.h"

EXTERN struct t_sw33bkX_ {
	short int altwc;
	}	sw33bkX_;

EXTERN struct t_targX_ {
	long int targ[ORGMAX][3];
	}	targX_;
EXTERN struct t_ofl2a3X_ {
	short int ofl2r[ELMMAX][3];
	short int ofl3r[ELMMAX][3];
	short int auxiliaryCode[ORGMAX][3];
	}	ofl2a3X_;
EXTERN struct t_ofltagX_ {
	short int ofl4r[ORGMAX][3];
	short int ofl1r[ORGMAX][3];
	}	ofltagX_;
EXTERN struct t_ptdiagX_ {
	short int patno[ORGMAX][3], stemno[ORGMAX][3];
	}	ptdiagX_;
EXTERN struct t_respasX_ {
	short int respas[ELMMAX][3];
	}	respasX_;
EXTERN struct t_ofltabX_ {
	short int ofl3i[ELMMAX], ofl4i[ELMMAX];
	}	ofltabX_;
EXTERN struct t_ovc2a3X_ {
	short int ofl2a[ELMMAX], ofl3a[ELMMAX];
	}	ovc2a3X_;
EXTERN struct t_typsvX_ {
	short int typsav[ELMMAX];
	}	typsvX_;
EXTERN struct t_sw36tableX_ {
	short int sw36table[9][9][2];
	}	sw36tableX_;


EXTERN struct t_rsworkX_ {
	short int rswork[ORGMAX][9];
	}	rsworkX_;

EXTERN struct t_sp1yX_ {
	short int ovrflw[21];
	}	sp1yX_;


//EXTERN struct { char c_space_[18]; } flowckX_;
EXTERN	struct  {
		short int im1, i3save, i3str, n6, n6jim, n6jims, phrstr, dum1, dum2;
		}	flowckX_;
//EXTERN struct { char c_space_[6]; } minickX_;
EXTERN struct  {
		short int k7m, minifg, minilp;
	} minickX_;

EXTERN struct  {
		short int nwrks, index, k3p3, pntr9, tran, allsem;
		}	semargX_;
EXTERN struct { char c_space_[6]; } sw23bkX_;
