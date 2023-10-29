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

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <logos_include_res_pt/fcrt.h>
#include "project.h"
#include "logoslib.h"
#include "trans.h"
#include "projexts.h"
#include "parsetrans.h"
#include <string.h>
#include <logos_include_res_pt/jbctrl.h>


EXTERN struct t_sw36bkX_ {
	short int det, rel, relcas, relgen, relnum, relper;
	}	sw36bkX_;
EXTERN struct t_sw31bkX_ {
	short int artd, artind, dative, fakev, inhb43, maktrn, negflg, 
	  subcl;
	}	sw31bkX_;

void /*FUNCTION*/ t1sw23()
{
	struct  {
		short int pcb, pcbwc, pcbswk;
		}	*_sw23bkX_ = (void*)&sw23bkX_;


	if( !(sw21bkX_.case_ == 0 && sw31bkX_.fakev == 0) ){
		if( sw36bkX_.rel != 1 ){

			/*   THE WORD 'AND' CAN NOT OVER-RIDE COMMA */

			if( _sw23bkX_->pcb != 0 ){
				if( _sw23bkX_->pcbwc > swork1X_.swork1[im81X_.im81-vbdataX_.k3p1-One][dct2X_.dct2[prctX_.js[sconX_.scolnk[im81X_.im81-vbdataX_.k3p1-One]-
				  One]-One]-One] )
					goto L_8220;
				}

			_sw23bkX_->pcb = sworkoX_.phcto;
			_sw23bkX_->pcbwc = swork1X_.swork1[im81X_.im81-vbdataX_.k3p1-One][dct2X_.dct2[prctX_.js[sconX_.scolnk[im81X_.im81-vbdataX_.k3p1-One]-
			  One]-One]-One];
			_sw23bkX_->pcbswk = im81X_.im81 - vbdataX_.k3p1;
			}
		}

	/*   POSSIBLE CLAUSE BOUNDARY SAVE PHRASE COUNT FOR USE OF SW 21. */

L_8220:
	vbdataX_.k3n = vbdataX_.k3 + 2;
	return;
} /*end of function*/

