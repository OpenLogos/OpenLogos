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
 /*
 *      THIS IS A MULTI-FUNCTION SWITCH.
	 *           -88 MEANS QUIT THE VTR AND EXECUTE THE BACKSPACE
	 *           MOST K3P1 VALUES INDICATE A FLAG SETTING
	 *           K3P1 GREATER THAN 131 SETS THE INHB ARRAY */
	/*           SOME OF THE FUNCTIONS ARE FOR THE CHECKER
	 *           PROGRAMS ONLY. */
	/*- - - - - - - - - - - - - - - - - - - - -    PR 30,40,50 PROJECT 12/86
	 *  a -88 in first parameter means quit the VTR and do the backspace */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <fcrt.h>
#include "logoslib.h"
#include "trans.h"
#include "projexts.h"
#include <jbctrl.h>
#include "parsetrans_ext.h"


void /*FUNCTION*/ txsw31(
short int *retflg
)
{
	static char pgmnam[9] = "TxSW31  ";
	static short ms = 0;
	static short rs = 0;

	*retflg = 0;

	if( vbdataX_.k3p1 >= 131 ){
		inhbX_.inhb[vbdataX_.k3p1-One] = 1;
		}
	else{
		if( !(vbdataX_.k3p1 >= 10 && vbdataX_.k3p1 <= 19) ){
			if( vbdataX_.k3p1 > 19 ){

				if( vbdataX_.k3p1 >= 22 && vbdataX_.k3p1 <= 25 ){

					if( srcflgX_.srcflg != 1 ){
						rs = vbdataX_.k3p1 - 21;
						if( rs == 2 ){
							sw31bkX_.cause = 0;
							}
						else if( rs == 3 ){
							sw31bkX_.inform = 1;
							}
						else if( rs == 4 ){
							sw31bkX_.inform = 0;
							}
						else{

							sw31bkX_.cause = 1;
							}
						}
					}
				else if( vbdataX_.k3p1 == 46 ){

					/*            -31 046 :  46 SWITCH WHICH FOLLOWS IMMEDIATELY TO THE RIGHT
					 *              REFERS TO INPUT SWORK (SWORKI) NOT CURRENT SWORK(SWORK) */
					if( vtrfX_.vtrf[vbdataX_.k3+2-One] == -46 && 
					  passesX_.passfl == 1 )
						sw3146X_.sw3146 = 1;
					}
				else if( vbdataX_.k3p1 == 28 ){
					sw31bkX_.cause = 0;
					sw31bkX_.inform = 0;
					}
				goto L_9340;
				}
			}

		/*        FOR FUNCTIONS 10-15 CERTAIN FLAGS ARE SET */
		ms = vbdataX_.k3p1 - 9;
		/*-                                                         *R1129MBS
		 *        FOR FUNCTIONS 10-15 CERTAIN FLAGS ARE SET */
		if( ms == 1 ){

			/*        FUNCTION 10 (GERMAN ONLY) TO ENSURE NO MISSING NICHTS */

			if( srcflgX_.srcflg == 1 ){

				if( sw44bkX_.nicht == 1 ){

					/*    PLACES NICHT IN OPADR IF K3P1 = 10 AND NICHT FLAG SET BY SW44 */
					sw44bkX_.nicht = 0;
					vtrnX_.vtrn = 139;
					vbdataX_.k3p1 = 0;
					txload();
					if( errvrsX_.errlvl == 0 )
						vbdataX_.k3n = vbdataX_.k3 + 2;
					return;
					}
				}
			}
		else if( ms == 3 ){


			flagX_.relon = 1;
			}
		else if( ms == 4 ){

			sw31bkX_.depcl = 1;
			}
		else if( ms == 5 ){

			sw31bkX_.depcl = 0;
			}
		else if( ms == 6 ){

			sw31bkX_.comp = 1;
			}
		else if( ms == 8 ){

			sw31bkX_.lpflag = 1;
			}
		else if( ms == 9 ){

			sw31bkX_.lpflag = 0;
			}
		else if( ms == 10 ){


			sw31bkX_.offlag = 1;
			}
		}


L_9340:
	vbdataX_.k3n = vbdataX_.k3 + 2;
	return;
} /*end of function*/

