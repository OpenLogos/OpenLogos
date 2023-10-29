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
	/*      NEW 12/06/86 *R1614DSD: PERFORM THE VTR-BRANCHING JUMP --
	 *                              LIFTED FROM T1VTRBRN */
	/*     CALLED FROM T1VTRBRN AND VBRNCH -56 AND -66 SWITCHES */
	/*           TEMPORARY POINTER         LG002JAL */
	/*     K3POS = POSITION OF (TRUE,FALSE) PAIR IN VTRF
	 *     COMPLETED ALL NECESSARY TESTS, ANSWER RETURNED IN 'FALCND'
	 *        TRUE:  FALCND = 0
	 *        FALSE: FALCND = 1 */
	/*     BE READY TO RESTORE EXISTING BREAKPOINTS */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <fcrt.h>
#include "trans.h"
#include "logoslib.h"
#include "projexts.h"
#include "parsetrans_ext.h"
#include <string.h>
#include <jbctrl.h>


void /*FUNCTION*/ vtrjmp(k3pos, falcnd, swnam, izpos)
long int k3pos, falcnd;
char *swnam;
long int izpos;
{
	static short int brnptr, gba1, savcon, savend, vbrk1, vbrk2, vbrk3;
	static char pgmnam[9] = "VTRJMP  ";
	static short val9 = 9;

	savend = vbdataX_.vbkend;
	savcon = vbdataX_.vbkcon;
	/*     DEFAULTS: NO WC09, NEW BREAKPOINTS IF ANY */
	vbdataX_.vbkend = 0;
	vbdataX_.vbkcon = 0;
	semargX_.pntr9 = 0;

	/*     IF THE TRUE CODE IS 9,
	 *            TAKE BRANCH TO THE WC9 RULE IF TRUE,
	 *            CONTINUE WITH THIS VTR IF FALSE */
	gba1 = vtrfX_.vtrf[k3pos-One]/10;
	if( vtrfX_.vtrf[k3pos-One] == 9 ){
		if( falcnd == 1 ){
			/*+                                                        *R0GBA*GBA */

			/*     IF THE TRUE CODE NON-RETURNABLE 40- OR 50-TABLE CALL
			 *            CALL THE TABLE              IF TRUE,
			 *            CONTINUE WITH THIS VTR IF FALSE
			 *             RESTORE OLD BREAK POINTS (NO FALSE BRANCH INDICATED) */
			vbdataX_.vbkend = savend;
			vbdataX_.vbkcon = savcon;
			if( opswX_.sw[3-One] == 1 )
				{
				fprintf( _spec_fp, " %3.3s CONDITION FALSE, CONTINUE THIS VTR \n", 
				  swnam );
				}
			}
		else{
			semargX_.pntr9 = vtrfX_.vtrf[k3pos+1-One];
			if( opswX_.sw[3-One] == 1 )
				{
				fprintf( _spec_fp, " %3.3s CONDITION TRUE AT %3ld, BRANCH TO WC9 \n", 
				  swnam, izpos );
				}
			}
		}
	else if( !(gba1 == 95 || gba1 == 94) ){
		if( falcnd != 1 ){
			brnptr = k3pos;
			}
		else if( vtrfX_.vtrf[k3pos-One] == 93 ){
			brnptr = k3pos + 4;
			}
		else{
			brnptr = k3pos + 1;
			}
		if( vtrfX_.vtrf[brnptr-One] != 56 ){
			if( vtrfX_.vtrf[brnptr-One] == 93 ){
				/*                EXTENDED BRANCH ADDRESSES */
				vbrk1 = vtrfX_.vtrf[brnptr+1-One];
				vbrk2 = vtrfX_.vtrf[brnptr+2-One];
				vbrk3 = vtrfX_.vtrf[brnptr+3-One];
				}
			else{
				/*                NORMAL BRANCH- IF A "9" ADRRESS IS USED TO INDICATE
				 *                END OF THE VTR THIS SHOULD BE CHANGED TO "99" TO
				 *                CONFORM TO "093" ADDRESS EXTENSION & NEW VBRKPT LOADING */
				vbrk1 = vtrfX_.vtrf[brnptr-One]/100;
				vbrk2 = (vtrfX_.vtrf[brnptr-One] - (vbrk1*100))/10;
				vbrk3 = vtrfX_.vtrf[brnptr-One] - ((vbrk1*100) + (vbrk2*
				  10));
				if( vbrk1 == 9 )
					vbrk1 = 99;
				if( vbrk2 == 9 )
					vbrk2 = 99;
				if( vbrk3 == 9 )
					vbrk3 = 99;
				}

			/*        ERROR CHECK AND LOAD BREAK PTS.
			 *        TO PREVENT 'BRANCH' BACKWARD */
			if( vbdataX_.vbrkpt[vbrk1-One] < vbdataX_.k3n ){
				errlog(pgmnam,7313,506,9);
				}
			else{
				/*        THE NEXT LINE *IS* THE BRANCH */
				vbdataX_.k3n = vbdataX_.vbrkpt[vbrk1-One];
				/*        SET 'THRU' AND 'BACK' ENTRIES */
				vbdataX_.vbkend = vbdataX_.vbrkpt[vbrk2-One];
				/*+                                                        *R0GBA*GBA */
				if( vbdataX_.vbkend < vbdataX_.k3n ){
					errlog(pgmnam,73135,506,9);
					}
				else{
					/*-                                                        *R0GBA*GBA */
					vbdataX_.vbkcon = vbdataX_.vbrkpt[vbrk3-One];
					/*+                                                        *R0GBA*GBA */
					if( vbdataX_.vbkcon < vbdataX_.vbkend ){
						errlog(pgmnam,73136,506,9);
						/*-                                                        *R0GBA*GBA */

						}
					else if( opswX_.sw[3-One] == 1 ){
						if( falcnd == 0 ){
							fprintf( _spec_fp, " %3.3s SWITCH TEST: CONDITION TRUE AT %3ld\n", 
							  swnam, izpos );
							}
						else{
							fprintf( _spec_fp, " %3.3s SWITCH TEST: CONDITION FALSE\n", 
							  swnam );
							}
						fprintf( _spec_fp, " BRANCH TO -57%3d   EXECUTE UNTIL -57%3d  JUMP -57%3d\n", 
						  vbrk1, vbrk2, vbrk3 );

						}
					}
				}
			}
		else if( opswX_.sw[3-One] == 1 ){
			fprintf( _spec_fp, " %3.3s 056 CONDITION AT %3ld,                              CONTINUE TO THE RIGHT\n", 
			  swnam, izpos );
			/*             NO BREAK PTS NEEDED - JUST RETURN */
			}
		}
	else if( falcnd == 1 ){
		/*+                                                   *JAL
		 *     NOW HANDLE BRANCH CASES W/IN THIS VTR:
		 *     - FIRST FIND THE CORRECT BRANCH FIELD BASED ON TRUE/FALSE AND
		 *         IF 093 EXTENSION USED.
		 *     - CHECK TO SEE IF THE ACTION CODE IS 56. THIS MEANS THAT WE SHOULD
		 *         SKIP THIS 56/66 SWITCH, CLEAR -57 BRANCHPOINTS FOR 'END-AT'
		 *         AND 'CONTINUE-AT', AND RESUME PROCESSING WITH THE SWITCH
		 *         IMMEDIATELY TO THE RIGHT OF THE -56/-66.
		 *     - IF093 MEANS EXTENDED ADDRESS FIELDS. */

		/*             RESTORE OLD BREAK POINTS (NO FALSE BRANCH INDICATED) */
		vbdataX_.vbkend = savend;
		vbdataX_.vbkcon = savcon;
		if( opswX_.sw[3-One] == 1 )
			{
			fprintf( _spec_fp, " %3.3s CONDITION FALSE, CONTINUE THIS VTR \n", 
			  swnam );
			}
		}
	else{
		gba1 *= 10;
		gba1 = vtrfX_.vtrf[k3pos-One] - gba1;
		gba1 *= 1000;
		semargX_.pntr9 = vtrfX_.vtrf[k3pos+1-One] + gba1;
		if( opswX_.sw[3-One] == 1 )
			{
			fprintf( _spec_fp, " %3.3s COND TRUE AT %3ld, BRANCH TO TABLE\n", 
			  swnam, izpos );
			}
		}
	return;
} /*end of function*/

