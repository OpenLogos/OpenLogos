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

EXTERN struct t_sw13bkX_ {
	short int loc25, preloc;
	}	sw13bkX_;
EXTERN struct t_sw31bkX_ {
	short int artd, artind, dative, fakev, inhb43, maktrn, negflg, 
	  subcl;
	}	sw31bkX_;
EXTERN struct t_sw24bkX_ {
	short int sw24n;
	}	sw24bkX_;
	/* end of COMMON translations */
void /*FUNCTION*/ t1sw43()
{
	/*
	 *     LAST CHG: 04/17/87 *R1685RKH*  Change T1-4 SWORK limit from 50 to
	 *
	 */
		/*pointer COMMON transls*/
	struct  {
		short int k7, oflad, n3;
		}	*_diacbX_ = (void*)&diacbX_;
	struct  {
		short int vtr[26];
		}	*_vtrX_ = (void*)&vtrX_;
		/*end of pointer COMMON transls*/
	static short n6 = 0;

	/*+ - - - - - - - - - - - - - - - - - - - -    PR 30,40,50 PROJECT12/86
	 *==== COMMON /DIACB/  K7,OFLAD,N3,VTR */
	/*- - - - - - - - - - - - - - - - - - - - -    PR 30,40,50 PROJECT 12/86
	 *+                                        *B0305JGB */
	/*-                                        *B0305JGB */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *
	 *SW43 */
	/*   ***** BEGINNING OF 43 SWITCH ***** */
	/*         TRAN1 ONLY */
	if( sw31bkX_.inhb43 == 0 ){

		if( sworkoX_.phcto > 0 ){
			sworkoX_.phrhdo[sworkoX_.phcto-One] = elscnpX_.elscnp[n6-One];
			sworkoX_.phrndo[sworkoX_.phcto-One] = _diacbX_->n3;
			}
		sworkoX_.phcto += 1;
		sw24bkX_.sw24n = 1;
		_diacbX_->n3 += 1;
		opadroX_.opadro[_diacbX_->n3-One] = -122;
		sworkoX_.phrbgo[sworkoX_.phcto-One] = _diacbX_->n3;
		prtscoX_.sct += 1;
		opadroX_.sconpo[_diacbX_->n3-One] = prtscoX_.sct;
		sconX_.scon[prtscoX_.sct-One][1-One] = 8;

		/*   SW13 PRE-LOCK ?? */
		if( sw13bkX_.preloc != 0 ){
			sconX_.scon[prtscoX_.sct-One][1-One] = -sconX_.scon[prtscoX_.sct-One][1-One];
			sw13bkX_.preloc = 0;
			}

		sworkoX_.phrhdo[sworkoX_.phcto-One] = prtscoX_.sct;
		sworkoX_.sworko[sworkoX_.phcto-One][1-One] = 8;
		sworkoX_.sworko[sworkoX_.phcto-One][2-One] = 1;
		sworkoX_.sworko[sworkoX_.phcto-One][3-One] = 1;

		vbdataX_.k3p1 = 2;
		}
	else{

		/*   ***** END OF 43 SWITCH ***** */

		/*SW43 */
		vbdataX_.k3n = vbdataX_.k3 + 2;
		}
	return;
} /*end of function*/

