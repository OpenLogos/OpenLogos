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
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*     THIS SUBROUTINE DOES SOME ODDS AND ENDS TYPE OF WORK BEFORE WE
	 *     PROCESS THE VTR.
	 *     BASICALLY, COMING INTO THIS SUBROUTINE WE WILL SET UP THE
	 *     PROGRAM TO PROCESS A WC9, WC10 OR ANY OTHER RULE COMING FROM
	 *     EITHER THE MINI OR THE MAIN SP FILES. */
	/*      Loop logic improvements                 OM FIX 4/8/87 (next line) */
	/* changes:
	 *      10/12/92 jal: increase size of imatch to elmmax
	 * LAST CHG 04/08/87            Loop Logic Improvements  OGM
	 *      CHG 12/30/86  PR304050: TARGET VTR TABLES
	 *      CHG 09/27/86 *R1568DSD: FINISH B0413, MINI WC09 STRETCH BUG
	 *      CHG 07/15/86 *B0413DSD: DONT APPLY STRETCH INFO TO WC09
	 *      CHG 11/26/85 */
	/*     THIS SUBROUTINE will reset pointers and arrays for the execution
	 *     of the VTR from the rule selected to run.  The rule could have
	 *     been from the Main or the Mini SP rule file.
	 *     THIS SUBROUTINE will also reset pointers and arrays if the rule
	 *     to be executed is from WC9, WC10 (rules that are branched to). */
	/*     BASICALLY, COMING INTO THIS SUBROUTINE WE COULD HAVE THE
	 *     FOLLOWING COMBINATIONS. */
	/*             NON WC9 OR WC10 RULE       WC9 RULE      WC 10 RULE */
	/* MAIN RULE          A                      B               C */
	/* MINI RULE          D                      E               F */
	/*     WE WILL DECIDE WHICH OF THE FOLLOWING WE ARE DEALING WITH.
	 *     ALSO, IN THIS ROUTINE WE WILL SET THE VARIABLEW LI AND I3
	 *     WHICH TELL US THE BEGINNING AND END POSITIONS OF THE ELEMENTS
	 *     MATCHED ON IN THIS RULE. (NOTE, I3 WILL BE SET TO THE BEGINNING
	 *     POSITION FOR THE NEXT MATCH BY VTRFWR, THE SUBROUTINE CALLED BY
	 *     THE DRIVER PROGRAM AFTER T1PREVTR.) */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#define MS_F77
#include <fcrt.h>
#include "logoslib.h"
#include "trans.h"
#include "projexts.h"
#include "parsetrans.h"
#include <jbctrl.h>
#include "parsetrans_ext.h"



void /*FUNCTION*/ vtrpre(short *k7, short *oflad, short ovrflw[])
{
	static long int _n;
	static short t = 0;

	minickX_.minifg = 0;
	minickX_.minilp = 0;

	/*     SKIP TO 981 IF WE HAVE A MINI WC9 RULE OR A MINI WC10
	 *     RULE, BUT NO MAIN WC9 OR WC10 RULE. */
	if( !((vtrs42X_.sw42n != 0 || semargX_.pntr9 != 0) && *k7 == 0) ){


		/*     VTRUNT IS THE DSRN FOR THE VTR FILE (MINI OR MAIN)
		 *     28 FOR THE MAIN FILE, 13 FOR THE MINI */

		vtrptrX_.vtrunt = 1;
		if( semargX_.pntr9 == 0 ){
				diagno(2);
			}



		/*     IF WE DIDN'T MATCH ON A MINI RULE, THEN SKIP TO 995 */
		if( minickX_.k7m == 0 )
			goto L_995;

		/*       THERE WAS AN EXPERIMENTAL RULE MATCH
		 *     FIND OUT IF WE SHOULD USE THE MAIN OR THE MINI RULE
		 *            MAIN: K7M = 0      MINI: K7M = 1 */

		whicsp(*k7,&minickX_.k7m,spX_.sp,ovrflw,*oflad);
		if( minickX_.k7m == 0 )
			goto L_995;
		}

	memcpy(vtrX_.vtr,spzxX_.vtr,sizeof(vtrX_.vtr));
	minickX_.minifg = 1;
	minickX_.minilp = 1;
	vtrptrX_.vtrunt = 2;

	/*       NPTPX IS THE FILE POINTER FOR FILE13, THE EXPERIMENTAL VTR FILE */
	vtrptrX_.nptp = loopckX_.nptpx;
	/*       NO LOOP ON MAIN RULES */
	loopckX_.nptpsv = loopckX_.nptpx;

	/*       I3X RESETS THE END POINT OF MATCH */
	w50valX_.i3 = cbsp2X_.i3x;
	sploopX_.li = cbsp2X_.lix;

	/*       PRINT VTR FOR MINI RULE */
	if( semargX_.pntr9 == 0 ){
		if( diagsX_.longdi == 1 )
			diagno(3);
		}

	/*       SET WC50 FROM EXPERIMENTAL
	 *           TRIPLE STRETCH
	 *+    STRETCH DOESNT APPLY TO WC9                09/27/86  *B0413DSD */
	if( semargX_.pntr9 == 0 ){
		vwarg2X_.wc50m = cbsp2X_.wc5xm;
		for( t=1; t <= 3; t++ ){
			vwarg1X_.wc50el[t-One] = cbsp2X_.wc5xel[t-One];
			vwarg1X_.look50[t-One] = cbsp2X_.look5x[t-One];
			vwarg1X_.chng50[t-One] = cbsp2X_.chng5x[t-One];
			}
		}

	/*     IF WE HAVE A WC10 OR WC9 RULE (MAIN OR MINI), THEN */

L_995:
	if( vtrs42X_.sw42n != 0 || semargX_.pntr9 != 0 ){
		strw10X_.i3stsv = w50valX_.i3;
		sploopX_.li = sw42bkX_.likeep;
		w50valX_.i3 = sw42bkX_.i3keep;
		}
	else{
		/*           RESET KEEP DUE TO MINI RULE */
		sw42bkX_.i3keep = w50valX_.i3;
		sw42bkX_.likeep = sploopX_.li;
		/*           SAVE LAST ELEMENT MATCHED VALUE */
		mtcendX_.mtcend = w50valX_.i3;
		/*           Set CALL36 for the -36056, function 001. */
		loopckX_.call36[1-One] = loopckX_.nptpsv;
		if( minickX_.minifg == 1 )
			loopckX_.call36[2-One] = 1;
		}



	if( diagsX_.longdi == 1 && vwarg2X_.wc50m != 0 )
		{
		fprintf( _spec_fp,
			    " STR1CHG: %5d        STR2CHG: %5d        STR3CHG: %5d\n",
				vwarg1X_.chng50[0],vwarg1X_.chng50[1],vwarg1X_.chng50[2] );
		}

	return;
} /*end of function*/

