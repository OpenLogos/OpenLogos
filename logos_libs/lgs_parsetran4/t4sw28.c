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
#include <fcrt.h>
#include "project.h"
#include "logoslib.h"
#include "trans.h"
#include "projexts.h"
#include "parsetrans.h"
#include <string.h>
#include <jbctrl.h>

void /*FUNCTION*/ t4sw28()
{
	static long int _l0;
	static short advrb1[3]={8,10,10};
	static short advrb3[3]={166,176,177};
	static short frm65 = 65;
	static short frm81 = 81;
	static short frm85 = 85;
	static short frm94 = 94;
	static short strong[5]={5,26,35,74,83};
	static short val9 = 9;
	static short weak[8]={23,34,41,76,749,4,7,1};
	static char pgmnam[9] = "T4SW28  ";
	static short cb = 0;
	static short cbx = 0;
	static short cb9 = 0;
	static short gb3 = 0;
	static short cb9x = 0;
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

	/*   ***** BEGINNING OF -28 SWITCH ***** */
	/*    K3P1 IS FUNCTION NUMBER
	 *    K3P2 IS RELATIVE POINTER TO NOUN1
	 *    K3P3 IS RELATIVE POINTER TO VERB
	 *    K3P4 IS RELATIVE POINTER TO NOUN2
	 *    K3P5 IS WC09 NUMBER */
	sw28fmX_.sw28fm = 1;
	gb3 = 3;
	vbdataX_.k3n = vbdataX_.k3 + 6;
	k3p2 = vtrfX_.vtrf[vbdataX_.k3+2-One];
	semargX_.k3p3 = vtrfX_.vtrf[vbdataX_.k3+3-One];
	k3p4 = vtrfX_.vtrf[vbdataX_.k3+4-One];
	k3p5 = vtrfX_.vtrf[vbdataX_.k3+5-One];

	/*        ORDER IS SUPERSET SET SUBSET */
	frm2 = im81X_.im81 - k3p2;
	cbx = sworkX_.phrhed[frm2-One];
	typ21 = sconX_.scon[cbx-One][13-One];
	typ22 = sconX_.scon[cbx-One][11-One];
	typ23 = sworkX_.swork[frm2-One][2-One];

	/*        ORDER IS SUPERSET SET SUBSET */
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

	/*         K3P1 IS A FUNCTION PARAMETER */
	if( vbdataX_.k3p1 == 3 || vbdataX_.k3p1 == 4 ){


		/*        FUNCTIONS 3, 4 - TEST SET FIRST */

		if( typ22 != typ32 )
			goto L_3690;
		if( vbdataX_.k3p1 != 4 ){

			/*        FUNCTION 3 ONLY */

			if( k3p4 != 0 ){
				if( typ32 != typ42 )
					goto L_3690;
				}

			/*        TEST FOR SUBSET AGREEMENT - FUNCTION 3 */

			if( typ23 != typ33 )
				goto L_3690;
			if( k3p4 == 0 )
				goto L_3606;
			if( typ33 != typ43 )
				goto L_3690;
			goto L_3606;

			/*        FUNCTION 4 TEST ONLY */

			}
		else if( k3p4 != 0 ){
			if( typ32 != typ42 )
				goto L_3690;
			}
		}
	else if( vbdataX_.k3p1 != 5 ){

		/*         FALL THROUGH TO HERE: K3P1 = 2
		 *         NEW:  TEST FOR IST/SIND FIRST */

		if( typ31 == 1 || typ31 == 11 )
			goto L_3690;

		/*        IS VERB PLURAL ??? */
		formod(2,frm65,gb3,frm3,&retsw);
		if( retsw != 2 ){
			/*        YES,
			 *        ARE BOTH NOUN1 AND NOUN2 PLURAL ??? */
			formod(2,frm85,gb3,frm2,&retsw);
			if( retsw == 2 )
				goto L_3611;

			/*        YES;  IS NOUN2 PLURAL ??? */
			formod(2,frm85,gb3,frm4,&retsw);
			if( retsw == 1 )
				goto L_3604;
			if( retsw == 2 )
				goto L_3690;
			}

		/*        YES, BOTH PLURAL */

		/*        NO, ARE BOTH NOUN1 AND NOUN2 SINGULAR ??? */
		formod(2,frm81,gb3,frm2,&retsw);
		if( retsw == 2 ){

			/*        IS NOUN2 SINGULAR ??? */
			formod(2,frm81,gb3,frm4,&retsw);
			if( retsw == 1 )
				goto L_3606;
			if( retsw == 2 )
				goto L_3690;
			}
		else{

			/*        NOUN1 IS SINGULAR, IS NOUN2 ??? */
			formod(2,frm81,gb3,frm4,&retsw);
			if( retsw == 2 )
				goto L_3690;
			goto L_3604;
			}

L_3611:
		formod(2,frm85,gb3,frm4,&retsw);
		if( retsw == 1 )
			goto L_3606;
		if( retsw == 2 )
			goto L_3690;
		goto L_3636;

		/*        YES, BOTH SINGULAR
		 *        IS NOUN1 ACCUSATIVE ??? */

		/*        SKIP ACCUSATIVE TEST ??? */

L_3604:
		if( typ32 == 26 && typ33 == 399 )
			goto L_3690;
		if( typ32 == 25 && typ33 == 306 )
			goto L_3690;
		if( typ32 == 24 && typ33 == 897 )
			goto L_3690;
		if( typ32 == 87 && typ33 == 461 )
			goto L_3690;

		formod(2,frm94,gb3,frm2,&retsw);
		if( retsw == 2 )
			goto L_3690;

		/*        YES, IS NOUN2 ACCUSATIVE ??? */
		formod(2,frm94,gb3,frm4,&retsw);
		if( retsw != 1 )
			goto L_3606;


		/*        A BRANCH
		 *        SUPERSET OF VERB EQ 2 OR 4 ??? */

L_3636:
		if( !(typ31 != 2 && typ31 != 4) ){

			/*        YES; HUMAN ??? */
			if( typ21 == 5 )
				goto L_3690;
			if( typ41 == 5 )
				goto L_3606;
			}

		/*        NO, CHECK FOR STRONG AGENT
		 *            CHECK FOR STRONG TYPE */
		for( cb=1; cb <= 5; cb++ ){
			if( typ21 == strong[cb-One] )
				goto L_3637;
			if( typ22 == strong[cb-One] )
				goto L_3637;
			if( typ23 == strong[cb-One] )
				goto L_3637;
			}

		if( !(typ21 == 9 && typ22 == 94) )
			goto L_3633;


		/*        FIRST NOUN PASSED STRONG TEST, IS IT 4 OR 7 ??? */
L_3637:
		if( typ21 != 4 && typ21 != 7 )
			goto L_3690;

		/*        IS FIRST NOUN A LABEL ??? */
L_3633:
		if( sworkX_.swork[frm2-One][1-One] == 1 ){
			if( sworkX_.swork[frm2-One][3-One] == 33 ){
				if( typ21 == 1 )
					goto L_3690;
				if( typ22 == 1 )
					goto L_3690;
				if( typ23 == 1 )
					goto L_3690;
				}
			}
		for( cb=1; cb <= 5; cb++ ){
			if( typ41 == strong[cb-One] )
				goto L_3642;
			if( typ42 == strong[cb-One] )
				goto L_3642;
			if( typ43 == strong[cb-One] )
				goto L_3642;
			}

		if( !(typ41 == 9 && typ42 == 94) )
			goto L_3652;


		/*        SECOND NOUN PASSED STRONG TEST, IS IT 4 OR 7 ??? */
L_3642:
		if( !(typ41 == 4 || typ41 == 7) ){

			/*        B BRANCH
			 *          WEAK AGENT */
			for( cb=1; cb <= 8; cb++ ){
				if( typ21 == weak[cb-One] )
					goto L_3653;
				if( typ22 == weak[cb-One] )
					goto L_3653;
				if( typ23 == weak[cb-One] )
					goto L_3653;

				/*        TO WC9 * QUIT */
				}
			goto L_3606;

			/*        FIRST NOUN WEAK, IS SECOND A TYPE 5 ??? */
L_3653:
			if( typ41 == 5 )
				goto L_3606;
			}

		/*        C BRANCH */

L_3652:
		;
		for( cb=1; cb <= 3; cb++ ){
			if( typ21 == advrb1[cb-One] && typ23 == advrb3[cb-One] )
				goto L_3606;
			}
		if( typ22 == 52 && typ23 == 789 )
			goto L_3606;
		goto L_3690;
		}

	/*        TEST SUPERSET - FUNCTIONS 4, 5 */

	if( typ21 != typ31 )
		goto L_3690;
	if( k3p4 != 0 ){
		if( typ31 != typ41 )
			goto L_3690;
		}

	/*        YES */

L_3606:
	semargX_.pntr9 = k3p5;
	cb9x = semargX_.pntr9;
	if( semargX_.pntr9 != 0 ){

		if( loopckX_.call36[1-One] == 0 ){
			loopckX_.call36[1-One] = loopckX_.nptpsv;
			if( minickX_.minifg == 1 )
				loopckX_.call36[2-One] = 1;
			}


		/*        IS THERE AN EXPERIMENTAL RULE? */

		cb9 = semargX_.pntr9;
		txmini(3,&minickX_.k7m,&loopckX_.nptpx,vwarg2X_.i,&cb9);
		if( errvrsX_.errlvl == 0 ){
			idxval((short*)ADR(_l0,1),&val9,&semargX_.pntr9,&diacb4X_.k7,&numr);
			if( !(numr == 0 && minickX_.k7m == 0) ){
				if( numr != 0 )
					rulein_ptr((short*)ADR(_l0,1),&matpos,
						spX_ptr,&diacb4X_.k7, &retsw);

				sw28fmX_.sw28fm = 0;

				/*        K7M WILL BE ZERO IF NO RULE FOUND */

				if( diacb4X_.k7 != 0 || minickX_.k7m != 0 )
					getvtrX_.getvtr = 1;
				}
			}
		return;
		}

	/*        DEFAULT
	 *        USE REMAINING VTR */

L_3690:
	if( diagsX_.deepdi == 1 )
		{
		fprintf( _spec_fp, " EXIT -28 SW; CONTINUE THIS VTR \n" );
		}
	sw28fmX_.sw28fm = 0;

	return;
} /*end of function*/

