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
	/*    THE 28 SWITCH IN TRAN3 CONTAINS ONLY FUNCTIONS 3, 4, AND 5
	 *    FUNCTION 2, THE ORIGINAL FUNCTION IN THE TRAN4 28 SWITCH
	 *    DOES NOT EXIST IN TRAN3, AS PER REQUEST #LXXX */
	/*+ SAB 6/87
	 * FOR TRAN2, 28 SW, FUNCTION 6 CHECKS FOR EQUALITY IN CONTENTS OF CELLS.
	 * -28 6 C1 C2 C3. IF CONTENTS OF CELLS C1 AND C2 ARE EQUAL, C3 SET TO 1.
	 * OTHERWISE C3 SET TO 0. IN ANY EVENT, C3 IS ALTERED. C1,C2 UNCHANGED.
	 *- SAB */
	/*    K3P1 IS FUNCTION NUMBER   (FUNCTIONS 3,4,5)
	 *    K3P2 IS RELATIVE POINTER TO NOUN1
	 *    K3P3 IS RELATIVE POINTER TO VERB
	 *    K3P4 IS RELATIVE POINTER TO NOUN2
	 *    K3P5 IS WC09 NUMBER */
	/*+SAB 6/87
	 * FOR FUNCTION 6
	 *    K3P2 IS FIRST CELL
	 *    K3P3 IS SECOND CELL - VALUE COMPARED TO FIRST
	 *    K3P4 IS THIRD CELL. IF EQUAL, SET CELL(K3P4) TO 1
	 *    IF NOT EQUAL, SETT CELL(K3P3) TO 0
	 *- SAB 6/87 */
	/* changes:
	 *      10/12/92 jal: increas size of imatch
	 *     LAST CHG: 04/17/87 *R1685RKH*  Change T1-4 SWORK limit from 50 to
	 *      CHG 08/22/86 *R1561DSD: 100 SCONS
	 *      CHG 04/01/86 */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#define MS_F77
#include <fcrt.h>
#include "logoslib.h"
#include "trans.h"
#include "projexts.h"
#include <jbctrl.h>
#include "parsetrans_ext.h"

EXTERN struct t_sw28fmX_ {
	short int sw28fm;
	}	sw28fmX_;

void /*FUNCTION*/ txsw28(
short *k7
)
{
	static long int _l0;
	static short val9 = 9;
	static char pgmnam[9] = "TxSW28  ";
	static short cbx = 0;
	static short cb9 = 0;
	static short frm2 = 0;
	static short frm3 = 0;
	static short frm4 = 0;
	static short k3p2 = 0;
	static short k3p4 = 0;
	static short k3p5 = 0;
	static short numr = 0;
	static short retsw = 0;
	static short typ21 = 0;
	static short typ22 = 0;
	static short typ23 = 0;
	static short typ31 = 0;
	static short typ32 = 0;
	static short typ33 = 0;
	static short typ41 = 0;
	static short typ42 = 0;
	static short typ43 = 0;
	static short matpos = 0;

	sw28fmX_.sw28fm = 1;
	vbdataX_.k3n = vbdataX_.k3 + 6;
	k3p2 = vtrfX_.vtrf[vbdataX_.k3+2-One];
	semargX_.k3p3 = vtrfX_.vtrf[vbdataX_.k3+3-One];
	k3p4 = vtrfX_.vtrf[vbdataX_.k3+4-One];
	k3p5 = vtrfX_.vtrf[vbdataX_.k3+5-One];

	/*+ SAB 6/87 */
	if( vbdataX_.k3p1 == 6 ){
		/*+  GBA */
		sw28fmX_.sw28fm = 0;
		/*-  GBA */
		if( !((k3p4 < 1) || (k3p4 > 100)) ){
			vbdataX_.vbcell[k3p4-One] = 0;
			if( !((k3p2 < 1) || (k3p2 > 100)) ){
				if( !((semargX_.k3p3 < 1) || (semargX_.k3p3 > 100))
				   ){
					if( vbdataX_.vbcell[k3p2-One] == vbdataX_.vbcell[semargX_.k3p3-One] )
						vbdataX_.vbcell[k3p4-One] = 1;
					}
				}
			}
		}
	else{

		/*                      ORDER IS SUPERSET SET SUBSET */
		frm2 = im81X_.im81 - k3p2;
		cbx = sworkX_.phrhed[frm2-One];
		typ21 = sconX_.scon[cbx-One][13-One];
		typ22 = sconX_.scon[cbx-One][11-One];
		typ23 = sworkX_.swork[frm2-One][2-One];

		/*                      ORDER IS SUPERSET SET SUBSET */
		frm3 = im81X_.im81 - semargX_.k3p3;
		cbx = sworkX_.phrhed[frm3-One];
		typ31 = sconX_.scon[cbx-One][13-One];
		typ32 = sconX_.scon[cbx-One][11-One];
		typ33 = sworkX_.swork[frm3-One][2-One];

		frm4 = im81X_.im81 - k3p4;
		cbx = sworkX_.phrhed[frm4-One];
		typ41 = sconX_.scon[cbx-One][13-One];
		typ42 = sconX_.scon[cbx-One][11-One];
		typ43 = sworkX_.swork[frm4-One][2-One];

		/*    K3P1 IS A FUNCTION PARAMETER */
		if( vbdataX_.k3p1 == 3 || vbdataX_.k3p1 == 4 ){

			/* ----------------------- */

			/*    FUNCTIONS 3, 4 - TEST SET FIRST */
			if( typ22 != typ32 )
				goto L_4082;
			if( vbdataX_.k3p1 != 4 ){

				/*    FUNCTION 3 ONLY */
				if( k3p4 != 0 ){
					if( typ32 != typ42 )
						goto L_4082;
					}

				/*    TEST FOR SUBSET AGREEMENT - FUNCTION 3 */
				if( typ23 != typ33 )
					goto L_4082;
				if( k3p4 == 0 )
					goto L_4072;
				if( typ33 != typ43 )
					goto L_4082;
				goto L_4072;

				/*    FUNCTION 4 TEST ONLY */
				}
			else if( k3p4 != 0 ){
				if( typ32 != typ42 )
					goto L_4082;
				}
			}
		else if( vbdataX_.k3p1 != 5 ){
			goto L_4072;
			}

		/*    TEST SUPERSET - FUNCTIONS 4, 5 */
		if( typ21 != typ31 )
			goto L_4082;
		if( k3p4 != 0 ){
			if( typ31 != typ41 )
				goto L_4082;
			}

L_4072:
		semargX_.pntr9 = k3p5;
		if( semargX_.pntr9 != 0 ){
			if( loopckX_.call36[1-One] == 0 ){
				loopckX_.call36[1-One] = loopckX_.nptpsv;
				if( minickX_.minifg == 1 )
					loopckX_.call36[2-One] = 1;
				}

			/*  IS THERE AN EXPERIMENTAL RULE?? */
			cb9 = semargX_.pntr9;
			txmini(3,&minickX_.k7m,&loopckX_.nptpx,vwarg2X_.i, &cb9);
			idxval((short *)ADR(_l0,1),&val9,&semargX_.pntr9,k7,&numr);
			if( !(numr == 0 && minickX_.k7m == 0) ){

				if( numr != 0 )
				{
					int status = rulein_ptr((short *)ADR(_l0,1),&matpos,
						spX_ptr,k7,&retsw);
					if ( status )
						errvrsX_.errlvl = status;
				}
				if( errvrsX_.errlvl == 0 ){

					if( *k7 != 0 || minickX_.k7m != 0 ){
						sw28fmX_.sw28fm = 0;
						getvtrX_.getvtr = 1;
						}
					}

			}
			return;
			}

		/*                       DEFAULT
		 *                        USE REMAINING VTR */
L_4082:
		if( diagsX_.longdi == 1 )
			{
			fprintf( _spec_fp, " EXIT -28 SW; CONTINUE THIS VTR \n" );
			}

		sw28fmX_.sw28fm = 0;

		}
	return;
} /*end of function*/

