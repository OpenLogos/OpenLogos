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


EXTERN struct t_sw14bkX_ {
	short int gen, num, per;
	}	sw14bkX_;
EXTERN struct t_sw36bkX_ {
	short int det, rel, relcas, relgen, relnum, relper;
	}	sw36bkX_;
EXTERN struct t_sw31bkX_ {
	short int artd, artind, dative, fakev, inhb43, maktrn, negflg, 
	  subcl;
	}	sw31bkX_;
EXTERN struct t_sw38bkX_ {
	short int adjnp, sw38n, vnum;
	}	sw38bkX_;
	/* end of COMMON translations */
void /*FUNCTION*/ t1sw31(retflg)
short int *retflg;
{
	/*
	 *+ - - - - - - - - - - - - - - - - - - - -    PR 30,40,50 PROJECT 12/86
	 *
	 *     LAST CHG: 04/17/87 *R1685RKH*  Change T1-4 SWORK limit from 50 to
	 *      CHG 12/30/86  PR304050: TARGET VTR TABLES -88 FUNCTION
	 *      CHG 10/06/86 *B0420DSD: ADD FUNCTION 036 (WAS IN -38SW)
	 *      CHG 08/29/85
	 *
	 */
		/*pointer COMMON transls*/
	struct  {
		short int pcb, pcbwc;
		}	*_sw23bkX_ = (void*)&sw23bkX_;
		/*end of pointer COMMON transls*/
	static short rs = 0;

	/*+                                               10/06/86  *B0420DSD */
	/*-                                               10/06/86  *B0420DSD */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *
	 *SW31 */
	/*   ***** BEGINNING OF -31 SWITCH *****
	 *+                                                         *R1129MBS
	 *      THIS IS A MULTI-FUNCTION SWITCH.
	 *           -88 MEANS STOP EXECUTING VTR AND DO BACKSPACE
	 *           MOST K3P1 VALUES INDICATE A FLAG SETTING
	 *           K3P1 GREATER THAN 131 SETS THE INHB ARRAY */
	/*           SOME OF THE FUNCTIONS ARE FOR THE CHECKER
	 *           PROGRAMS ONLY. */
	/*- - - - - - - - - - - - - - - - - - - - -    PR 30,40,50 PROJECT 12/86
	 *  a -88 in first parameter means quit the VTR and do the backspace */
	*retflg = 0;
	/*+                                                        *R0GBA*GBA
	 *     IF (K3P1 .EQ. -88) THEN
	 *         RETFLG = -88
	 *         RETURN
	 *         END IF
	 *-                                                        *R0GBA*GBA
	 *- - - - - - - - - - - - - - - - - - - - -    PR 30,40,50 PROJECT 12/86 */

	if( vbdataX_.k3p1 >= 131 ){

		/*   FOR FRENCH CONSTANTS */

		inhbX_.inhb[vbdataX_.k3p1-One] = 1;
		}
	else{
		/*-                                                         *R1129MBS */
		if( vbdataX_.k3p1 > 9 ){

			if( vbdataX_.k3p1 == 15 ){
				sw31bkX_.artd = 1;
				goto L_8220;

				}
			else if( vbdataX_.k3p1 == 16 ){
				sw31bkX_.artind = 1;
				goto L_8220;

				}
			else if( vbdataX_.k3p1 == 20 ){
				sw31bkX_.negflg = 1;
				goto L_8220;

				}
			else if( vbdataX_.k3p1 == 21 ){
				sw31bkX_.negflg = 0;
				goto L_8220;

				/*+                                                         *R1129MBS */
				}
			else if( vbdataX_.k3p1 >= 29 && vbdataX_.k3p1 <= 33 ){

				if( srcflgX_.srcflg == 2 ){
					/*+                                                         *R1129MBS */
					rs = vbdataX_.k3p1 - 28;
					if( rs == 2 ){
						sw31bkX_.dative = 0;
						}
					else if( rs == 3 ){
						sw31bkX_.subcl = 1;
						}
					else if( rs == 4 ){
						sw31bkX_.subcl = 0;
						}
					else if( rs == 5 ){
						sw31bkX_.maktrn = 1;
						}
					else{
						sw31bkX_.dative = 1;
						}
					}
				goto L_8220;
				/*                                                10/06/86  *B0420DSD */
				}
			else if( vbdataX_.k3p1 != 36 ){
				if( vbdataX_.k3p1 == 46 ){


					/*            -31 046 :  46 SWITCH WHICH FOLLOWS IMMEDIATELY TO THE RIGHT
					 *              REFERS TO INPUT SWORK (SWORKI) NOT CURRENT SWORK(SWORK) */
					if( vtrfX_.vtrf[vbdataX_.k3+2-One] == -46 && passesX_.passfl == 
					  1 )
						sw3146X_.sw3146 = 1;
					}
				goto L_8220;
				}
			}
		else if( vbdataX_.k3p1 == 1 ){
			goto L_8220;
			}
		else if( vbdataX_.k3p1 == 3 ){

			_sw23bkX_->pcb = 0;
			goto L_8220;
			}
		else if( vbdataX_.k3p1 == 4 ){

			sw31bkX_.fakev = 1;
			goto L_8220;
			}
		else if( vbdataX_.k3p1 == 5 ){

			inhbX_.inhb[136-One] = 0;
			goto L_8220;
			}
		else if( vbdataX_.k3p1 == 6 ){

			/*   THIS IS FOR GERMAN NICHT */
			inhbX_.inhb[160-One] = 0;
			inhbX_.inhb[146-One] = 0;
			goto L_8220;
			}
		else if( vbdataX_.k3p1 == 7 ){

			if( sw36bkX_.rel == 1 )
				sw36bkX_.relnum = 1;
			if( sw36bkX_.rel != 1 )
				sw14bkX_.num = 1;
			goto L_8220;
			}
		else if( vbdataX_.k3p1 == 8 ){

			if( sw36bkX_.rel == 1 )
				sw36bkX_.relnum = 2;
			if( sw36bkX_.rel != 1 )
				sw14bkX_.num = 2;
			goto L_8220;
			}
		else if( vbdataX_.k3p1 == 9 ){

			sworkoX_.sworko[1-One][3-One] = 9;
			}
		else{

			sw31bkX_.inhb43 = 1;
			goto L_8220;
			}

		/*+                                               10/06/86  *B0420DSD */
		sw38bkX_.adjnp = 1;
		}
	/*-                                               10/06/86  *B0420DSD */

	/*   ***** END OF -31 SWITCH ***** */

	/*SW31 */
L_8220:
	vbdataX_.k3n = vbdataX_.k3 + 2;
	return;
} /*end of function*/

