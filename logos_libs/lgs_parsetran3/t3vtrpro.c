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
	/* CHANGES:
	 *   10/12/92 jal: increase size of imatch to elmmax
	 *   09/04/91 JAL:  ADD -68 SWITCH
	 *   7/06/09 LG002GBA RPHS3:
	 *     LAST CHG: 04/17/87 *R1685RKH*  Change T1-4 SWORK limit from 50 to
	 *      CHG 12/30/86  PR304050: TARGET VTR TABLES
	 *      CHG 11/17/86 *R1614DSD: ADD -66SW
	 *      CHG 06/09/86 *R1530DSD: EXPAND ELEMENTS AND REL PTRS FOR STRETCH
	 *      CHG 03/15/86 */
	/*     SUBROUTINE FOR PROCESSING THE VTRS IN TRAN3 */
	/*     CONTROL INSTRUCTIONS TO THE DRIVER PROGRAM ARE COMMUNICATED */
	/*        GETVTR = 1  WC9 OR WC10 MATCH. BRANCH TO VTR LOOP CHECK.
	 *        ERRLVL = 13  ERROR, GET NEXT VTR ELEMENT
	 *        ERRLVL  NE 13 AND N3 0, ERROR, RETURN TO DRIVER */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*     VTRFM REMEMBERS VTRF 'TYPES'  FROM VTRFWR  06/09/86  *R1530DSD */
	/*-                                               06/09/86  *R1530DSD */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*---------------  PROCESS VTR SUBROUTINE ------------------------------ */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <fcrt.h>
#include "project.h"
#include "logoslib.h"
#include "trans.h"
#include "projexts.h"
#include <jbctrl.h>
#include "parsetrans.h"
#include <string.h>

void /*FUNCTION*/ vtrpro()
{
	static byte vtrfmn;
	static short int k3pos, retflg;
	static long int _l0;
	static short val9 = 9;
	static char pgmnam[9] = "T3VTRPRO";
	static short gb = 0;
	static short iz = 0;
	static short ms = 0;
	static short kz2 = 0;
	static short numr = 0;
	static short retsw = 0;

	while( vbdataX_.k3n <= vwarg2X_.iz4 ){

		if( diagsX_.deepdi == 1 )
			{
			fprintf( _spec_fp, " VTRPRO MAIN DIAGNOSTIC %6d%6d", vbdataX_.k3n, 
			  flowckX_.im1 );
			fprintf( _spec_fp, "\n" );
			for( iz=vbdataX_.k3n; iz <= vwarg2X_.iz4; iz++ ){
				fprintf( _spec_fp, "%6d", vtrfX_.vtrf[iz-One] );
				}
			fprintf( _spec_fp, "\n" );
			}

		vbdataX_.k3 = vbdataX_.k3n;
		vtrnX_.vtrn = vtrfX_.vtrf[vbdataX_.k3-One];
		vbdataX_.k3p1 = vtrfX_.vtrf[vbdataX_.k3+1-One];
		/*+    GET 'MODE' OF THIS VTR ELEMENT             06/09/86  *R1530DSD */
		vtrfmn = vtrfmX_.vtrfm[vbdataX_.k3-One];

		if( diagsX_.deepdi == 1 )
			{
			fprintf( _spec_fp, " IN %6.6s, NOW PROCESSING: %4d AS %c\n", 
			  passesX_.pssnam, vtrnX_.vtrn, vtrfmn );
			}

		/*        ARE WE JUST FINISHED PROCESSING A SLOT ? */
		if( vtrnX_.vtrn == 999 && cnX_.vtrdon == 1 ){
			slotld_x();

			/*        IS THIS THE END OF THE CURRENT VTR ? */
			}
		else if( vtrnX_.vtrn == 999 ){
			goto L_9502;

			/*        ARE WE ABOUT TO PROCESS A SLOT ?
			 *         71 THRU 98 ARE CALLED SLOTS */
			}
		else if( vtrnX_.vtrn >= 71 && vtrnX_.vtrn <= 98 ){
			slotld_x();

			/*        ARE WE LOADING A VC, PUNCT OR A CONSTANT ? */
			}
		else if( vtrnX_.vtrn > 9 ){
			txload();
			vbdataX_.k3n = vbdataX_.k3 + 2;

			/*     IS IT AN ELEMENT TO BE LOADED ?
			 *+       STRETCH MAY HAVE SCRAMBLED VTRN         06/09/86  *R1530DSD */
			}
		else if( vtrfmn == vtrfvlX_.vtmel ){
			/*         WAS: IF (VTRN .GE. (-10) .AND. VTRN .LE. 9) THEN
			 *-                                               06/09/86  *R1530DSD */
			elemld();
			vbdataX_.k3n = vbdataX_.k3 + 2;
			}
		else{

			/*   FALL THRU TO HERE STARTS TRAN3 SWITCH PROCESSING
			 *   COMPUTE MS SWITCH VARIABLE */
			getvtrX_.getvtr = 0;
			/*+                                                        *Rphs3*GBA
			 *   RETURN TO VTRCTL ON CERTAIN SWITCHES */
			if( (vtrnX_.vtrn == -63 || vtrnX_.vtrn == -64) || vtrnX_.vtrn ==  -65 )
				return;

			ms = -vtrnX_.vtrn - 10;

			if( ms == 1 ){
				/*+ */
				txsw11(&retsw);
				if( retsw == 1 )
					return;
				}
			else if( ms == 2 ){

				txsw12();
				}
			else if( ms == 3 ){

				txsw13();
				}
			else if( ms == 4 ){

				txsw14();
				}
			else if( ms == 5 ){

				txsw15();
				}
			else if( ms == 6 ){

				txsw16();
				}
			else{
				if( !(((((((((((ms == 7 || ms == 13) || ms == 17) || 
				  ms == 20) || ms == 27) || ms == 31) || ms == 33) || 
				  ms == 35) || ms == 37) || (ms >= 40 && ms <= 56)) || 
				  ms == 59) || ms == 60) ){
					if( ms == 8 ){

						txsw18();
						goto L_8500;
						}
					else if( ms == 9 ){

						txsw19();
						goto L_8500;
						}
					else if( ms == 10 ){

						txsw20();
						goto L_8500;
						}
					else if( ms == 11 ){

						txsw21();
						goto L_8500;
						}
					else if( ms == 12 ){

//						t3sw22();
						txsw22(&diacb3X_.k7);
						goto L_8500;
						}
					else if( ms == 14 ){

						txsw24();
						goto L_8500;
						}
					else if( ms == 15 ){

						txsw25();
						goto L_8500;
						}
					else if( ms == 16 ){

						txsw26();
						goto L_8500;
						}
					else if( ms == 18 ){

						txsw28(&diacb3X_.k7);
						goto L_8500;
						}
					else if( ms == 19 ){

						txsw29(&diacb3X_.k7);
						goto L_8500;
						}
					else if( ms == 21 ){

						/*- - - - - - - - - - - - - - - - - - - - -    PR 30,40,50 PROJECT 12/86 */
						txsw31(&retflg);
						if( retflg == -88 )
							return;
						/*+ - - - - - - - - - - - - - - - - - - - -    PR 30,40,50 PROJECT 12/86 */
						goto L_8500;
						}
					else if( ms == 22 ){

						t0sw32();
						goto L_8500;
						}
					else if( ms == 23 ){

						txsw33();
						goto L_8500;
						}
					else if( ms == 24 ){

						txsw34();
						goto L_8500;
						}
					else if( ms == 25 ){

						txsw35();
						goto L_8500;
						}
					else if( ms == 26 ){

						t3sw36();
						goto L_8500;
						}
					else if( ms == 28 ){

						t3sw38();
						goto L_8500;
						}
					else if( ms == 29 ){

						txsw39();
						goto L_8500;
						}
					else if( ms == 30 ){
						/*  SW40 DO NOTHING -- ADDED AS A PLACE HOLDER FOR 30 TABLES, ETC.
						 *                     WILL OVERRIDE PHRASE SUPPRESSION. */
						vbdataX_.k3n = vbdataX_.k3 + 2;
						goto L_8500;
						}
					else if( ms == 32 ){

						txsw42(&diacb3X_.k7, &diacb3X_.oflad);
						goto L_8500;
						}
					else if( ms == 34 ){

						txsw44();
						goto L_8500;
						}
					else if( ms == 36 ){

						txsw46();
						goto L_8500;
						}
					else if( ms == 38 ){

						txsw48switch();
						goto L_8500;
						}
					else if( ms == 39 ){

						t0sw49();
						goto L_8500;
						}
					else if( ms == 57 ){

						txsw67();
						goto L_8500;
						}
					else if( ms == 58 ){
						sw68();
						goto L_8500;
						}
					}

				if( (ms >= 44 && ms <= 47) || ms == 56 ){
					/*     USE SWITCH VALUE IN VTRBRNCH CALL, IF -54,-55,-56,-66,-57 SW
					 *-                                               11/26/86  *R1614DSD */
					gb = -vtrnX_.vtrn;
					vbrnch(gb);

					/*+                                               11/26/86  *R1614DSD
					 *     DID THE -56 OR -66 SWITCH CALL FOR A BRANCH TO WC9 ? */
					if( !(semargX_.pntr9 == 0 || (gb != 56 && gb != 66)) ){

						/*-                                               11/26/86  *R1614DSD
						 *+  NON-RETURNABLE BRANCH TO 40- 50-TABLE                 *R0GBA*GBA */
						if( vtrfX_.vtrf[vbdataX_.k3-One] == -56 )
							k3pos = vbdataX_.k3 + 2;
						if( vtrfX_.vtrf[vbdataX_.k3-One] == -66 )
							k3pos = vbdataX_.k3 + 1;
						if( vtrfX_.vtrf[k3pos-One] >= 950 )
							goto L_9503;
						if( vtrfX_.vtrf[k3pos-One] >= 940 )
							goto L_9504;
						/*-                                                        *R0GBA*GBA */
						if( loopckX_.call36[1-One] == 0 ){
							loopckX_.call36[1-One] = loopckX_.nptpsv;
							if( minickX_.minifg == 1 )
								loopckX_.call36[2-One] = 1;
							}

						/*     CAN WE FIND THIS WC9 RULE IN THE MINI OR MAIN FILES ? */
						txmini(3,&minickX_.k7m,&loopckX_.nptpx,vwarg2X_.i, &semargX_.pntr9);
						idxval((short*)ADR(_l0,1),&val9,&semargX_.pntr9,&diacb3X_.k7, &numr);
						if( !(numr == 0 && minickX_.k7m == 0) ){

							rulein_ptr((short*)ADR(_l0,1),&commdX_.matpos,
								spX_ptr, &diacb3X_.k7,&retsw);

							if( diacb3X_.k7 != 0 || minickX_.k7m != 0 )
								goto L_9505;
							}
						}

					}
				else if( diagsX_.longdi == 1){
					fprintf( _spec_fp, "\n ***** ILLEGAL VTR %6d%6d%6d     %c", 
					  vbdataX_.k3n, vbdataX_.k3, vtrnX_.vtrn, vtrfmn );
					fprintf( _spec_fp, "\n   " );
					for( kz2=vbdataX_.k3n; kz2 <= vwarg2X_.iz4; kz2++ ){
						fprintf( _spec_fp, "%4d", vtrfX_.vtrf[kz2-One] );
						}
					fprintf( _spec_fp, "\n" );
					}
				}
			}

L_8500:
		if( errvrsX_.errlvl == 0 ){

			/*     GETVTR IS SET BY THE 22, 28, 29, 36 SWITCHES TO INDICATE THAT A
			 *     WC 9 OR WC10 VTR SHOULD BE PROCESSED. RETURN TO DRIVER TO BEGIN
			 *     PROCESSING THIS NEW VTR. */

			if( getvtrX_.getvtr == 1 )
				return;

			/*     ERRLVL = 13  INSTRUCTS US TO GET NEXT VTR ELEMENT */
			}
		else if( errvrsX_.errlvl == 13 ){
			errvrsX_.errlvl = 0;
			}
		else{
			return;
			}
		}
	goto L_9501;
L_9505:
	getvtrX_.getvtr = 1;
	return;
L_9504:
	getvtrX_.getvtr = 4;
	return;
L_9503:
	getvtrX_.getvtr = 5;
	return;
L_9502:
	if( sworkoX_.phrhdo[sworkoX_.phcto-One] == 0 )
		sworkoX_.phrhdo[sworkoX_.phcto-One] = sworkX_.phrhed[flowckX_.n6-One];

	if( opswX_.sw[3-One] == 1 )
		diagno(7);

	return;

L_9501:
	if( diagsX_.longdi == 1 )
		diagno(7);
	vbdataX_.k3 = vbdataX_.k3n;
	return;
} /*end of function*/

