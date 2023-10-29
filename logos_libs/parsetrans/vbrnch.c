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
	 *    9/23/94 jal Log#2106 new -54 switch to set scons of addresses
	 *              loaded in a VC.
	 *    2/25/94 jal:Log#2091 Install the -55 counter function to
	 *               increment/decrement CELL values.
	 *      10/7/93 jal: fix to 9/1/93 code for -66 cell contains SCNROW, the
	 *           SCON ROW pointer, not N6JIM, the SWORK pointer.
	 *      9/1/93 Jal: modify -55: fcn 868,allow cells to contain a ptr
	 *          to SCONs of an element.
	 *      9/1/93 jal: modify -66 switch: allow use of CELL instead of REL
	 *          ptr (-8X) to indicate which  scons to test.
	 *      04/17/87 *R1685RKH*  Change T1-4 SWORK limit from 50 to
	 *      12/15/86 *R1614DSD: ADD -66 SW, VTRJMP
	 *      10/29/86 *B0426DSD: CORREX TO *R1557 FOR -540NN-8X456
	 *      10/15/86 *R1557DSD: DONT CHANGE SCONS(4..8) IF SCON IS LOCKED
	 *      10/02/86 *R1568DSD: SCONS 1-99 ACCESSIBLE, ADD FN 350..352
	 *      08/26/86 *R1561DSD: 100 SCONS
	 *      05/19/86 *R1530DSD: USE VTRFM TO DISTINGUISH REL. PTRS &C
	 *      01/21/86 *R1268DSD: LOAD SCON(S) FROM SINGLE CELL
	 *      06/17/87 *SBRAWER:  349,350,351,649,650,651 FUNCTION */
	/*                -56SW TEMPORARY ZPAIR EVALUATION RESULT *JAL */
	/*+          set scons of addressed in a vc       9/23/94 jal Log#2106 */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <fcrt.h>
#include "trans.h"
#include "logoslib.h"
#include "projexts.h"
#include <string.h>
#include "parsetrans_ext.h"
#include <jbctrl.h>



void /*FUNCTION*/ vbrnch(sw)
long int sw;
{
	static char swnam[4];
	static LOGICAL32 nxt777, tstres;
	static short int adxx, curscn, faltst, here, hfptxx, jal, k3p3, 
	  k3p4, kfun, kvalue, n3p1, oplst, opxx, scnrow, scpxx, temp, 
	  valu, xx;
	static long int _l0, izpos, k3pos;
	static short gb = 0;
	static short iz = 0;
	static short ms = 0;
	static short wc = 0;
	static short cell = 0;
	static short end66 = 60;
	static short k3p2 = 0;
	static short cval1 = 0;
	static short cval2 = 0;
	static short cval3 = 0;
	static short izbeg = 0;
	static short izend = 0;
	static short n6jim = 0;
	static short n7jim = 0;
	static short retsw = 0;
	static short vcode = 0;
	static short andcnd = 0;
	static short celval = 0;
	static short elepos = 0;
	static short elmpos = 0;
	static long falcnd = 0;
	static short scnpos = 0;
	static short typtst = 0;
	static short vbrelp = 0;
	static short vbvalu = 0;

	/*    SW IS POSITIVE VALUE OF SWITCH: 54, 55, 56, 66, 57 */
	if( sw == 66 ){
		/*- REP 542 602                         LTS3.1    12/10/86  *R1614DSD2 */

		/*        ***** END OF -56 SWITCH *****
		 *SW56
		 *+ INS 605                             LTS3.1    12/10/86  *R1614DSD2 */

		/*SW66 */

		/*        ***** BEGINNING OF -66 SWITCH ***** */


		/*        THERE ARE 6 (OR MORE) PARAMETERS:
		 *        1) 3 DIGIT CODE FOR 'IF TRUE'
		 *           1ST DIGIT - VTR BREAK POINT (-57 SWITCH) TO START FROM
		 *           2ND DIGIT - VTR BREAK POINT (-57 SWITCH) TO END AT
		 *           3RD DIGIT - VTR BREAK POINT TO CONTINUE EXECUTION AT
		 *                       (9 SIGNIFIES END OF VTR OR '999')
		 *        2) 3 DIGIT CODE FOR 'IF FALSE' (SAME AS FOR IF TRUE) */

		/*        ANY NUMBER OF COMPARISON TRIPLES (3,4,5):
		 *        3) RELATIONAL POINTER TO SCON WE'RE TESTING
		 *        4) SCON POSITION TO COMPARE (1..99)
		 *           FIRST DIGIT TELLS WHAT TEST TO PERFORM:
		 *             0: TO COMPARE SCON POSITION TO (5)
		 *             2: SAME TEST AS '0', BUT SUCCEEDS WHENEVER VALUES DIFFER
		 *             5,6,7: TESTS WHETHER THIS SCON POSITION'S...
		 *                (5) NOUN FORM
		 *                (6) VERB FORM
		 *                (7) OTHER FORM... IS THE VALUE IN (5)
		 *        5) VALUE TO TEST */

		/*        FOLLOWED BY 060 TO MARK THE END OF THE PARAMETERS. */

		/*        A '777' INSERTED BETWEEN TWO TRIPLES SIGNIFIES THAT THE TESTS
		 *        BEFORE '.AND.' THE TESTS AFTER MUST BOTH BE TRUE.
		 *        OTHERWISE '.OR.' IS ASSUMED */

		/*        THERE CAN BE AS MANY TRIPLES OF TEST VALUES (3,4,5)
		 *        AS WILL FIT */



		temp = vtrfX_.vtrf[vbdataX_.k3+1-One];
		if( temp != 9 && (temp < 940 || temp > 959) ){
			if( temp == 93 ){
				izbeg = vbdataX_.k3 + 5;
				}
			else{
				izbeg = vbdataX_.k3 + 2;
				}
			if( vtrfX_.vtrf[izbeg-One] == 93 ){
				izbeg += 4;
				}
			else{
				izbeg += 1;
				}
			}
		else{
			izbeg = vbdataX_.k3 + 3;
			}

		/*        INITIALIZE FALSE CONDITION FLAG TO 'FALSE' */
		falcnd = 1;
		/*     ANDCND IS ALWAYS SET JUST BEFORE AN 'ANDED' TEST IS EXAMINED */
		andcnd = 0;

		iz = izbeg;

		/*               ***    LOOP HERE FOR NEW TRIPLET    *** */

		while( TRUE ){

			/*     ELMPOS IS ALWAYS THE START OF A TEST OR 060, IS NEVER 777 */
			elmpos = vtrfX_.vtrf[iz-One];

			if( elmpos == end66 )
				goto L_83042;
			if( falcnd == 0 && andcnd == 0 )
				break;
			/*           IS THERE A 777 JUST AFTER THIS TEST? REMEMBER FOR LATER USE */
			if( vtrfX_.vtrf[iz+3-One] == 777 ){
				nxt777 = TRUE;
				}
			else{
				nxt777 = FALSE;
				}
			if( andcnd == 1 && falcnd == 1 ){
				/*              USELESS TO DO THE TEST */
				}
			else{
				if( nxt777 )
					andcnd = 1;
				/*                                    ***  DO THE TEST  ***
				 *                         1st parameter is rel ptr or cell */
				if( elmpos < -70 ){
					n6jim = im81X_.im81 - elmpos;
					scnrow = sworkX_.phrhed[n6jim-One];
					}
				else if( elmpos > 0 && elmpos <= 100 ){
					scnrow = vbdataX_.vbcell[elmpos-One];
					n6jim = 1;
					/*                         force a false result if not a valid scon */
					if( scnrow < 1 || scnrow > prtscoX_.sct ){
						if( diagsX_.deepdi == 1 || diagsX_.longdi == 
						  1 )
							{
							fprintf( _spec_fp, "/ERROR:-66 switch, bad SCON pointer found in CELL\n   CELL(%3d)=%4d\n  Forcing this triplet to test false.\n", 
							  elmpos, scnrow );
							}
						tstres = FALSE;
						goto L_8550;
						/*                         force false result if ovrflw SCON doesnt exist */
						}
					else if( vtrfX_.vtrf[iz+1-One] > SCONX1 ){
						if( sconX_.scolnk[scnrow-One] <= 0 ){
							if( diagsX_.deepdi == 1 || diagsX_.longdi == 
							  1 )
								{
								fprintf( _spec_fp, "/ERROR:-66 switch, SCON %4d\n  out of range for SCON table entry at %3d\n Forcing this triplet to test false.\n", 
								  vtrfX_.vtrf[iz+1-One], scnrow );
								}
							tstres = FALSE;
							goto L_8550;
							}
						}
					}
				else{
					if( diagsX_.deepdi == 1 || diagsX_.longdi == 1 )
						{
						fprintf( _spec_fp, "/ERROR:-66 switch, invalid 1st parameter(%3d) in a test triplet.\n  Forcing this triplet to test false.\n", 
						  elmpos );
						}
					tstres = FALSE;
					goto L_8550;
					}

				scnpos = vtrfX_.vtrf[iz+1-One];
				/*                  TYPTST IS FIRST DIGIT OF SCON-COLUMN-POSITION
				 *                  aside FROM SCONX DIGITS); TELLS WHAT TEST TO PERFORM */
				if( scnpos > SCONX ){
					typtst = scnpos/100;
					scnpos += -typtst*100;
					}
				else{
					typtst = 0;
					}

				valu = vtrfX_.vtrf[iz+2-One];

				if( opswX_.sw[3-One] == 1 ){
					if( scnpos <= SCONX1 )
						{
						fprintf( _spec_fp, " SCON(%3d,%3d) = %4d\n", 
						  scnpos, elmpos, sconX_.scon[scnrow-One][scnpos-One] );
						}
					if( scnpos > SCONX1 )
						{
						fprintf( _spec_fp, " SCON(%3d,%3d) = %4d\n", 
						  scnpos, elmpos, sconX_.scono[sconX_.scolnk[scnrow-One]-
						  One][scnpos-SCONX1-One] );
						}
					}

				/*              ASSUME THIS TEST WILL FAIL */
				tstres = FALSE;

				if( typtst == 0 || typtst == 2 ){
					/*                 NORMAL TEST: COMPARE SCON COLUMN VALUE W/ CONSTANT */
					if( scnpos > SCONX1 ){
						/*              5/6/7 TESTS ADDED 12/11/86 */
						if( sconX_.scono[sconX_.scolnk[scnrow-One]-
						  One][scnpos-SCONX1-One] == valu )
							tstres = TRUE;
						}
					else if( sconX_.scon[scnrow-One][scnpos-One] == 
					  valu ){
						tstres = TRUE;
						}
					}
				else if( typtst >= 5 && typtst <= 7 ){
					/*                 5, 6, AND 7 ARE TO TEST NOUN, VERB, OTHER FORM TABLES */
					if( typtst == 5 )
						wc = 1;
					if( typtst == 6 )
						wc = 2;
					if( typtst == 7 )
						wc = 9;

					ms = 4;
					frmarrX_.frmarr[1-One] = wc;
					if( scnpos <= SCONX1 )
						frmarrX_.frmarr[2-One] = sconX_.scon[scnrow-One][scnpos-One];
					if( scnpos > SCONX1 )
						frmarrX_.frmarr[2-One] = sconX_.scono[sconX_.scolnk[scnrow-One]-
						  One][scnpos-SCONX1-One];

					/*                 N6JIM IS IGNORED BECAUSE MS = 4 */
					formod(2,valu,ms,n6jim,&retsw);
					if( retsw == 1 )tstres = TRUE;
					}

				/*                 EXCEPTION TEST - SUCCESS JUST IN CASE THE TEST FAILED */
				if( typtst == 2 )tstres = !tstres;

				/*-                                    ***  END OF THE TEST  *** */

				/*              IF (NORMAL OR REVERSED) TEST IS PASSED, TURNOFF FAIL FLAG */
L_8550:
				if( tstres ){
					falcnd = 0;
					}
				else if( andcnd == 1 ){
					/*                    FAILURE INSIDE 'AND' MUST BE RECORDED EXPLICITLY */
					falcnd = 1;
					}
				}

			/*           SAVE LOCATION OF TEST FOR DIAGNOSTICS */
			izpos = iz;
			/*           SET UP FOR NEXT ITERATION - JUST IF NO FALLTHROUGH
			 *           POINT TO NEXT TEST TRIPLET (SKIP FOLLOWING 777 IF ANY) */
			iz += 3;
			if( nxt777 ){
				/*           FOLLOWING 777; SKIP IT AND REMEMBER THAT THIS IS AN AND */
				iz += 1;
				}
			else{
				/*              NEXT TEST IS NOT IN THIS AND-GROUP */
				andcnd = 0;
				/*           PERFORM NEXT TEST */
				}
			}
		/*           SUCCESS - SCAN FOR END66 TO FIND NEXT SWITCH   12/10/86 */
		while( vtrfX_.vtrf[iz-One] != end66 ){
			iz += 3;
			if( vtrfX_.vtrf[iz-One] == 777 )
				iz += 1;
			}
		/*           FALL OUT OF LOOP */
		goto L_83043;
		/*           WE ARE AT END OF VTR - FALL THRU LOOP */
L_83042:
		;

		/*     IF NO JUMP, FOLLOWING SWITCH WILL BE PERFORMED NEXT  12/10/86 */
L_83043:
		vbdataX_.k3n = iz + 1;

		/*     DO THE VTR JUMP (OR WC9 CALL) PER VTRF(K3+1) OR VTRF(K3+2) */
		k3pos = vbdataX_.k3 + 1;
		strcpy( swnam, "-66" );
		vtrjmp(k3pos,falcnd,swnam,izpos);
		}
	else{
		gb = sw - 53;
		if( gb == 2 ){

			/*        ***** END OF -54 SWITCH ***** */

			/*SW54 */






			/*SW55 */

			/*        ***** BEGINNING OF -55 SWITCH ***** */

			/*        THERE ARE 3 PARAMETERS:
			 *        1) VALUE FROM 1-30 IDENTIFYING VTR BRANCH TEST 'CELL'
			 *           WILL BE TESTED BY SUBSEQUENT -56 SWITCHES
			 *           (1-20 ARE SPECIFIC TO THIS TRAN,
			 *            21-30 ARE PASSED ON TO SUCCEEDING TRANS)
			 *        2) IF POSITIVE - VALUE TO BE PUT IN TEST 'CELL'
			 *           IF NEGATIVE - A RELATIONAL POINTER
			 *        3) 0    - IF PARAM. 2 POSITIVE
			 *           1-20 - PUT THE VALUE FROM THIS SCON INTO TEST 'CELL'
			 *                  (ACTUAL RANGE IS 1-15, REST RESERVED FOR EXPANSION)
			 *           21   - PUT THE FORM VALUE OF ELEMENT POINTED TO
			 *                  BY REL. POINTER INTO TEST 'CELL'
			 *           50   - PUT THE SUBSET CODE OF ELEMENT POINTED TO
			 *                  BY REL. POINTER INTO TEST 'CELL'
			 *+                                                         *R1170MBS
			 *           52   - PUT THE FORMSV CODE OF ELEMENT POINTED TO
			 *                  BY REL. POINTER INTO TEST 'CELL'
			 *-                                                         *R1170MBS */

			vbdataX_.k3n = vbdataX_.k3 + 4;
			vbdataX_.k3p1 = vtrfX_.vtrf[vbdataX_.k3+1-One];
			k3p2 = vtrfX_.vtrf[vbdataX_.k3+2-One];
			k3p3 = vtrfX_.vtrf[vbdataX_.k3+3-One];

			/*          2ND PARAMETER MAY BE POSITIVE IF SETTING FROM PHANTOM SWORK */
			if( k3p2 >= 0 ){
				if( k3p2 != 33 ){
					/*+              Cell increment function     Log #2091   2/25/94 jal */
					if( k3p3 == 900 ){
						vbdataX_.vbcell[vbdataX_.k3p1-One] += k3p2;
						if( opswX_.sw[3-One] == 1 )
							{
							fprintf( _spec_fp, " SW55 - INCREMENTED CELL: %2d WITH VALUE: %3d\n", 
							  vbdataX_.k3p1, k3p2 );
							}
						/*               Cell decrement function     Log #2091   2/25/94 jal */
						}
					else if( k3p3 == 800 ){
						vbdataX_.vbcell[vbdataX_.k3p1-One] -= k3p2;
						if( opswX_.sw[3-One] == 1 )
							{
							fprintf( _spec_fp, " SW55 - DECREMENTED CELL: %2d WITH VALUE: %3d\n", 
							  vbdataX_.k3p1, k3p2 );
							}
						/*-                                            Log#2091  2/25/94 jal */
						}
					else{
						vbdataX_.vbcell[vbdataX_.k3p1-One] = k3p2;
						if( opswX_.sw[3-One] == 1 )
							{
							fprintf( _spec_fp, " SW55 - LOADED CELL: %2d WITH VALUE: %3d\n", 
							  vbdataX_.k3p1, k3p2 );
							}
						}
					return;
					}
				}

			/*           CELL = a value referenced via a rel ptr. */
			if( k3p2 == 33 ){
				n6jim = sav36sX_.sav36s[1-One];
				}
			else{
				n6jim = im81X_.im81 - k3p2;
				}
			if( n6jim != 0 ){

				/*     SCONS 1..99 CAN BE CHANGED BY SWITCH -55
				 *  REMOVE EXTRA CONDITIONS ONCE FN 21, 50, 51, 52 HAVE BEEN REMOVED */
				if( (k3p3 < SCONX && (k3p3 <= 49 || k3p3 >= 53)) && 
				  k3p3 != 21 ){
					if( k3p3 <= SCONX1 )
						vbdataX_.vbcell[vbdataX_.k3p1-One] = sconX_.scon[sworkX_.phrhed[n6jim-One]-
						  One][k3p3-One];
					if( k3p3 > SCONX1 )
						vbdataX_.vbcell[vbdataX_.k3p1-One] = sconX_.scono[sconX_.scolnk[sworkX_.phrhed[n6jim-One]-
						  One]-One][k3p3-SCONX1-One];

					/*         FOLLOWING FUNCTION WILL REPLACE 50, 21/51, 52 */
					}
				else if( k3p3 == 350 ){
					vbdataX_.vbcell[vbdataX_.k3p1-One] = sworkX_.swork[n6jim-One][2-One];
					}
				else if( k3p3 == 351 ){
					vbdataX_.vbcell[vbdataX_.k3p1-One] = sworkX_.swork[n6jim-One][3-One];
					}
				else if( k3p3 == 352 ){
					jal = sconX_.scolnk[sworkX_.swork[n6jim-One][4-One]-
					  One];
					if( jal > 0 )
						vbdataX_.vbcell[vbdataX_.k3p1-One] = formsaX_.formsv[jal-One];
					}
				else if( k3p3 == 21 || k3p3 == 51 ){
					vbdataX_.vbcell[vbdataX_.k3p1-One] = sworkX_.swork[n6jim-One][3-One];
					}
				else if( k3p3 == 50 ){
					vbdataX_.vbcell[vbdataX_.k3p1-One] = sworkX_.swork[n6jim-One][2-One];
					}
				else if( k3p3 == 52 ){
					jal = sconX_.scolnk[sworkX_.swork[n6jim-One][4-One]-
					  One];
					if( jal > 0 )
						vbdataX_.vbcell[vbdataX_.k3p1-One] = formsaX_.formsv[jal-One];

					/*         FUNCTION 456 SET CELL FROM SCONS 4,5 AND 6 */
					}
				else if( k3p3 == 456 ){
					cval1 = sconX_.scon[sworkX_.phrhed[n6jim-One]-
					  One][4-One];
					cval2 = sconX_.scon[sworkX_.phrhed[n6jim-One]-
					  One][5-One];
					cval3 = sconX_.scon[sworkX_.phrhed[n6jim-One]-
					  One][6-One];
					if( cval2 == 0 )
						cval2 = 1;
					vbdataX_.vbcell[vbdataX_.k3p1-One] = (cval1*100) + 
					  (cval2*10) + cval3;

					/*         function 868 set cell with ptr to SCONs of element at K3P2 */
					}
				else if( k3p3 == 868 ){
					vbdataX_.vbcell[vbdataX_.k3p1-One] = sworkX_.swork[n6jim-One][4-One];
					}
				}


			vbrelp = n6jim;


			/*+ REP 411                             LTS3.1    12/10/86  *R1614DSD2 */
			if( opswX_.sw[3-One] == 1 )
				{
				fprintf( _spec_fp, " SW55 - LOADED CELL: %2d WITH VALUE: %5d, VBRELP =%3d\n", 
				  vbdataX_.k3p1, vbdataX_.vbcell[vbdataX_.k3p1-One], 
				  vbrelp );
				}
			/*- REP 411                             LTS3.1    12/10/86  *R1614DSD2 */

			}
		else if( gb == 3 ){

			/*        ***** END OF -55 SWITCH ***** */

			/*SW55
			 *SW56 */

			/*        ***** BEGINNING OF -56 SWITCH ***** */

			/*        THERE ARE 5 (OR MORE) PARAMETERS:
			 *        1) NUMBER OF PAIRS OF TESTS (PARAMETERS 4 AND 5)
			 *        2) 3 DIGIT CODE FOR 'IF TRUE'
			 *           1ST DIGIT - VTR BREAK POINT (-57 SWITCH) TO START FROM
			 *           2ND DIGIT - VTR BREAK POINT (-57 SWITCH) TO END AT
			 *           3RD DIGIT - VTR BREAK POINT TO CONTINUE EXECUTION AT
			 *                       (9 SIGNIFIES END OF VTR OR '999')
			 *        3) 3 DIGIT CODE FOR 'IF FALSE' (SAME AS FOR IF TRUE)
			 *        4) WHICH VTR BRANCH TEST 'CELL' TO INTERROGATE
			 *           1ST DIGIT REPRESENTS THE TYPE OF TEST FOR THIS TEST PAIR
			 *           0 = NORMAL TEST
			 *           1 = TEST IS FOR 'NOT EQUAL' TO
			 *           2 = NORMAL TEST, CONSIDER VALUE TO TEST A 3-DIGIT NUMBER
			 *           3 = SAME IS 2, BUT TEST IS FOR 'NOT EQUAL' TO
			 *           2ND AND 3RD DIGITS ARE THE 'CELL' NO. (1-30)
			 *        5) VALUE TO TEST
			 *           THIS IS A 2-DIGIT VALUE (1ST DIGIT RESERVED FOR SUPERFORM
			 *           TEST BELOW) UNLESS THE TYPE OF TEST (ABOVE) IS 2 OR 3.
			 *           IF 1ST DIGIT IS A 1, 2 OR 3 THEN THE 2ND AND 3RD DIGITS
			 *           REPRESENT A SUPERFORM VALUE (20-99):
			 *           1 = NOUN
			 *           2 = VERB
			 *           3 = OTHER SUPERFORM */

			/*        A '777 777' IN PARAM. 4 AND 5 SIGNIFIES THAT THE TESTS
			 *        BEFORE '.AND.' THE TESTS AFTER MUST BE TRUE,
			 *        OTHERWISE '.OR.' IS ASSUMED */

			/*        THERE CAN BE AS MANY PAIRS OF TEST VALUES (PARAM. 4 AND 5)
			 *        AS WILL FIT */

			/*        SET UP INTERNAL SW POINTERS FOR TEST PAIR LOOPING:
			 *           K3P1 - NO. OF TEST PAIRS.
			 *           IZBEG, IZEND - BEGINNING AND END PTRS TO TEST PAIRS.
			 *                   IF 009,94X,OR 95X CASE,IZBEG = K3+4 AUTOMATICALLY.
			 *                   IF "093" TRUE OR FALSE BRANCH THEN ADDRESS FIELDS
			 *                   ARE EXTENDED.
			 *           K3N - PTS TO BEG OF NEXT SW. */

			vbdataX_.k3p1 = vtrfX_.vtrf[vbdataX_.k3+1-One];
			temp = vtrfX_.vtrf[vbdataX_.k3+2-One];
			if( temp != 9 && (temp < 940 || temp > 959) ){
				if( temp == 93 ){
					izbeg = vbdataX_.k3 + 6;
					}
				else{
					izbeg = vbdataX_.k3 + 3;
					}
				if( vtrfX_.vtrf[izbeg-One] == 93 ){
					izbeg += 4;
					}
				else{
					izbeg += 1;
					}
				}
			else{
				izbeg = vbdataX_.k3 + 4;
				}
			vbdataX_.k3n = izbeg + (vbdataX_.k3p1*2);
			izend = vbdataX_.k3n - 1;

			/*        INITIALIZE FALSE CONDITION FLAG TO 'FALSE' */
			falcnd = 1;
			/*+ */
			andcnd = 0;
			/*- */

			for( iz=izbeg; iz <= izend; iz += 2 ){

				cell = vtrfX_.vtrf[iz-One];
				vbvalu = vtrfX_.vtrf[iz+1-One];

				if( cell == 777 && vbvalu == 777 ){
					andcnd = 1;
					}
				else if( (andcnd == 0) && (falcnd == 0) ){
					break;
					}
				else{
					faltst = 1;
					/*+ INS 482 *SAVE TEST LOC'N FOR DIAGNO LTS3.1    12/10/86  *R1614DSD2 */

					izpos = iz;

					/*- INS 482                             LTS3.1    12/10/86  *R1614DSD2
					 *        1ST DIGIT SPECIFIES TYPE OF TEST - IF IT IS GT 3
					 *+                                                        *R0GBA*GBA
					 *            TYPTST = CELL/100 */
					typtst = (cell - 1)/100;
					/*-                                                        *R0GBA*GBA
					 *+                                                         *R1039MBS */
					if( typtst > 3 )
						cell -= typtst*100;

					/*+ REP 488                             LTS3.1    12/10/86  *R1614DSD2 */
					if( opswX_.sw[3-One] == 1 )
						{
						fprintf( _spec_fp, " CELL%3d  = %4d\n", cell, 
						  vbdataX_.vbcell[cell-One] );
						}
					/*- REP 488                             LTS3.1    12/10/86  *R1614DSD2 */

					if( typtst == 0 ){
						/*-                                                         *R1039MBS
						 *        POSITIVE MATCH TEST - 'TRUE' IF VALUES ARE EQUAL */
						if( vbdataX_.vbcell[cell-One] == vbvalu )
							faltst = 0;
						/*+                                                         *R1039MBS
						 *        CODE DELETED HERE (SAME AS 73081) */
						}
					else if( typtst == 4 ){
						/*-                                                         *R1039MBS */

						/*        EXCEPTION TEST - 'TRUE' FOR ALL VALUES EXCEPT TEST VALUE */
						if( vbdataX_.vbcell[cell-One] != vbvalu )
							faltst = 0;

						}
					else if( typtst < 5 || typtst > 7 ){
						continue;
						}
					else{

						/*            5, 6, AND 7 ARE TO TEST NOUN, VERB, OTHER FORM TABLES */
						if( typtst == 5 )
							wc = 1;
						if( typtst == 6 )
							wc = 2;
						if( typtst == 7 )
							wc = 9;

						ms = 4;
						frmarrX_.frmarr[1-One] = wc;
						frmarrX_.frmarr[2-One] = vbdataX_.vbcell[cell-One];

						/*---- CALL FORMV1 (VBVALU, MS, N6JIM, *7305, *73081) */
						formod(2,vbvalu,ms,n6jim,&retsw);

						if( retsw != 2 )faltst = 0;
						}

					/*        IF TRUE AND NO 'AND' GO OUT */
					if( andcnd == 1 ){
						if( (falcnd == 1) || (faltst == 1) )falcnd = 1;
						andcnd = 0;
						}
					else{
						falcnd = faltst;
						}
					}
				}

			/*        COMPLETED ALL NECESSARY TESTS, ANSWER RETURNED IN 'FALCND'
			 *        TRUE:  FALCND = 0
			 *        FALSE: FALCND = 1 */

			/*+ REP 542 602                         LTS3.1    12/10/86  *R1614DSD2
			 *     DO THE VTR JUMP (OR WC9 CALL) PER VTRF(K3+2) OR VTRF(K3+3) */
			strcpy( swnam, "-56" );
			k3pos = vbdataX_.k3 + 2;
			vtrjmp(k3pos,falcnd,swnam,izpos);
			}
		else if( gb == 4 ){

			/*        ***** END OF -66 SWITCH *****
			 *SW66 */

			/*- INS 605                             LTS3.1    12/10/86  *R1614DSD2
			 *SW57 */

			/*        ***** BEGINNING OF -57 SWITCH ***** */

			/*        THERE IS ONE PARAMETER:
			 *        1) A VTR BREAK POINT NUMBER (1-8) */

			/*        THE -57 SWITCH IS USED TO DENOTE THE STARTING AND ENDING POINTS
			 *        OF SECTIONS OF THE VTR THAT ARE TO BE CONDITIONALLY EXECUTED
			 *        BASED ON THE -56 SWITCH TESTS */

			vbdataX_.k3n = vbdataX_.k3 + 2;

			if( opswX_.sw[3-One] == 1 )
				{
				fprintf( _spec_fp, " SW57 - VTR BREAK POINT, K3: %3d\n", 
				  vbdataX_.k3 );
				}

			/*        HAS SECTION OF THE VTR INDICATED BY -56 SWITCH BEEN EXECUTED? */
			if( !(vbdataX_.k3 < vbdataX_.vbkend || vbdataX_.vbkend == 
			  0) ){

				/*        YES - CONTINUE EXECUTION AT NEW BREAK POINT (MAY BE END) */
				vbdataX_.k3n = vbdataX_.vbkcon;
				/*+ */
				vbdataX_.vbkend = 0;
				vbdataX_.vbkcon = 0;
				/*- */
				if( opswX_.sw[3-One] == 1 )
					{
					fprintf( _spec_fp, " SW57 - CONDITIONAL EXECUTION COMPLETED, BRANCH TO: %4d\n", 
					  vbdataX_.k3n );
					}

				}
			}
		else{


			/*   * * *   VTR BRANCHING   * * * */

			/*        THE -55, -56 AND -57 SWITCHES WORK IN CONJUNCTION TO PERFORM
			 *        'VTR BRANCHING'. THIS IS ACTUALLY CONDITIONAL OR SELECTIVE
			 *        EXECUTION OF SECTIONS OF THE VTR. */

			/*        THE -55 SWITCH STORES VALUES FOR SUBSEQUENT (-56 SW) TESTS.
			 *        THERE ARE NO CONDITIONS AS TO ITS USE - IT CAN BE ANYWHERE
			 *        IN ANY RULE AND NEED NOT BE PRECEDED OR FOLLOWED BY ANYTHING
			 *        IN PARTICULAR. */

			/*        THE -56 SWITCH PERFORMS THE TESTS OF THE VALUES LOADED BY
			 *        PREVIOUS -55 SWITCHES.
			 *        THIS INCLUDES VALUES LOADED BY -55 SWS. IN PREVIOUS RULES AND,
			 *        IN THE CASE OF TEST 'CELLS' 31-50, VALUES LOADED BY -55 SWS.
			 *        IN ANY PREVIOUS TRAN.
			 *        VALUES 51-99ARE PRESERVED FROM SENTENCE TO SENTENCE
			 *        THE RESULTS OF THESE TESTS DETERMINE WHICH SECTIONS
			 *        OF THE VTR WILL BE EXECUTED.
			 *        THIS SWITCH NEED NEED NOT BE PRECEEDED BY ANYTHING, BUT IT
			 *        MUST BE FOLLOWED BY AT LEAST ONE -57 SWITCH.
			 *        THE -56 SWITCH CAN BE NESTED - THAT IS THERE CAN BE ADDITIONAL
			 *        -56 SWITCHES IN THE FOLLOWING SECTIONS OF THE VTR THAT
			 *        ARE TO BE CONDITIONALLY EXECUTED. */

			/*        THE -57 SWITCH IS A 'BREAK POINT' TO INDICATE WHERE IN THE VTR
			 *        TO START AND STOP CONDITIONAL EXECUTION.
			 *        THERE CAN BE UP TO EIGHT -57 SWITCHES IN A RULE.
			 *        (THE '999' AT THE END OF EVERY VTR IS USED IN 'VTR BRANCHING'
			 *         TO INDICATE A 'NULL' BRANCH - DO NOTHING OR EXECUTION DONE.) */

			/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */








			/*SW54 */

			/*        ***** BEGINNING OF -54 SWITCH ***** */

			/*  THE 54 SWITCH WAS DESIGNED AT THE SAME TIME OF VTR BRANCHING AND THE
			 *      EXPANSION OF THE SCON TABLE FROM 14 TO 20 POSITIONS.
			 *  THE -54 SWITCH HAS SEVERAL FUNCTIONS, DISTINGUISHED BY PARAMETER 2
			 * PARAMETER 2 FUNCTIONS ARE:
			 *      0 - 100     LOAD ONE OR MORE SCONS FROM A SINGLE CELL
			 *      100 - 120  W/456 LOADS SCONS 4-5-6 OF VC W/ CELL VALUES
			 *      131 - 998  W/456 LOADS SCONS 4-5-6 OF CONSTANT W/ CELL VALUES
			 *      NEGATIVE VALUE - LOADS SPECIFIED SCONS OF EL POINTED TO
			 * PARAMETER 3 FUNCTION:
			 *     349-351, 649-651 - TRANSFER NUMBER BETWEEN SWORK AND SCON
			 *                        (PARAMETER 2 IS -8X FOR THIS CASE). */



			vbdataX_.k3p1 = vtrfX_.vtrf[vbdataX_.k3+1-One];
			k3p2 = vtrfX_.vtrf[vbdataX_.k3+2-One];

			/*+ *SAB 6/87 */
			k3p3 = vtrfX_.vtrf[vbdataX_.k3+3-One];
			/*- SAB */

			vbdataX_.k3n = vbdataX_.k3 + 3 + (vbdataX_.k3p1*2);

			izbeg = vbdataX_.k3 + 3;
			izend = izbeg + vbdataX_.k3p1*2 - 1;







			/*+                                                01/21/86 *R1268DSD */

			/* THIS FUNCTION WILL LOAD ONE OR MORE SCONS FROM A SINGLE CELL. */



			/*   -54  NO.PAIRS  CELL  PAIR 1   PAIR 2 . . . PAIR 3
			 *        K3P1      K3P2   K3P1*2 .... */

			/*     E.G. -54 002 014 017,-82 018,-83
			 *          RESULTS IN -82'S SCON 17 AND -83'S SCON 18
			 *                  BOTH BEING LOADED FROM CELL 14
			 *+ SAB 6/87
			 *     -54 001 -81 350 62
			 *          RESULTS IN COPYING SCON 62 OF -81 TO THE TYPE FIELD
			 *                  OF -81'S SWORK.
			 *- SAB */

			/*     CHECK THAT PARAMETER 2 IS A CELL */
			if( k3p2 >= 1 && k3p2 <= 100 ){
				celval = vbdataX_.vbcell[k3p2-One];
				/*        FOR ALL PARAMETER PAIRS */
				for( iz=izbeg; iz <= izend; iz += 2 ){
					scnpos = vtrfX_.vtrf[iz-One];
					/*           IF ARG1 IS SCON-POSITION, ASSUME ARG2 IS REL. POINTER
					 *           USE ALL SCONS                        10/15/86  *R1561DSD */
					if( scnpos >= 1 && scnpos <= SCONX ){
						n6jim = im81X_.im81 - vtrfX_.vtrf[iz+1-One];
						elepos = sworkX_.phrhed[n6jim-One];
						/*+                                               10/15/86  *R1557DSD
						 *              DONT CHANGE SCON POS (4..8) FOR LOCKED SCON */
						if( !((scnpos >= 4 && scnpos <= 8) && sconX_.scon[elepos-One][1-One] < 
						  0) ){
							/*-                                               10/15/86  *R1557DSD */
							if( scnpos <= SCONX1 )
								sconX_.scon[elepos-One][scnpos-One] = celval;
							if( scnpos > SCONX1 )
								sconX_.scono[sconX_.scolnk[elepos-One]-
								  One][scnpos-SCONX1-One] = celval;
							}
						}
					}
				/*-                                                         *R1268DSD */






				/*+                                                         *R1372JGB */

				/*           IF K3P2 IS A CONSTANT OR VC, SEARCH OPADRO TO LEFT
				 *           TO CHANGE ITS SCON 4, 5, AND 6 VALUES
				 *           NOTE: IN THIS APPLICATION, ONLY ONE PAIR OF PARAMETERS
				 *                 IS PROCESSED (I.E., ONLY ONE CONSTANT) */

				}
			else if( (k3p2 >= 131 && k3p2 <= 998) || (k3p2 >= 100 && 
			  k3p2 <= 120) ){

				n3p1 = opadroX_.opo + 1;
				oplst = n3p1 - 1;

				for( xx=1; xx <= oplst; xx++ ){
					here = n3p1 - xx;
					if( opadroX_.opadro[here-One] == -k3p2 )
						goto L_83044;
					}
				return;
				/*+            DONT CHANGE SCONS(4..6) IF LOCKED  10/15/86  *R1557DSD */
L_83044:
				curscn = opadroX_.sconpo[here-One];
				if( sconX_.scon[curscn-One][1-One] >= 0 ){
					celval = vbdataX_.vbcell[vtrfX_.vtrf[vbdataX_.k3+4-One]-
					  One];
					k3p3 = vtrfX_.vtrf[vbdataX_.k3+3-One];
					if( k3p3 == 456 ){
						cval1 = celval/100;
						cval2 = (celval - (cval1*100))/10;
						cval3 = celval - ((cval1*100) + (cval2*10));
						sconX_.scon[curscn-One][4-One] = 1;
						if( cval1 != 0 )
							sconX_.scon[curscn-One][4-One] = cval1;
						sconX_.scon[curscn-One][5-One] = 1;
						if( cval2 != 0 )
							sconX_.scon[curscn-One][5-One] = cval2;
						sconX_.scon[curscn-One][6-One] = 1;
						if( cval3 != 0 )
							sconX_.scon[curscn-One][6-One] = cval3;
						}
					else{
						if( k3p3 <= SCONX1 )
							sconX_.scon[curscn-One][k3p3-One] = celval;
						if( k3p3 > SCONX1 )
							sconX_.scono[sconX_.scolnk[curscn-One]-
							  One][k3p3-SCONX1-One] = celval;
						/*                 SEARCH IS THROUGH, WHETHER OR NOT SCON IS LOCKED
						 *-                                               10/15/86  *R1557DSD */

						}
					}
				/*-                                            *R1372JGB */

				/*+          set scons of addressed in a vc       9/23/94 jal Log#2106 */

				/*  -54  VC  -8X  SCON  VALUE
				 *         set SCON = VALUE for all addresses loaded in all VCs of -8X */

				}
			else if( vbdataX_.k3p1 >= VCADLO && vbdataX_.k3p1 <= VCADHI ){
				n6jim = im81X_.im81 - k3p2;
				k3p4 = vtrfX_.vtrf[vbdataX_.k3+4-One];
				for( opxx=sworkX_.phrbeg[n6jim-One]; opxx <= sworkX_.phrend[n6jim-One]; opxx++ ){
					if( opadriX_.opadri[opxx-One] == -vbdataX_.k3p1 ){
						hfptxx = 1;
						while( hfptxx != 0 ){
							getadr(opadriX_.opadri,opadriX_.sconpi,
							  hpdopiX_.hfdopi,opxx,&hfptxx,&adxx,&scpxx,
							  &retsw);
							/*                    If valid address & original element, check it. */
							if( (retsw == 0 && scpxx <= SCONY) && 
							  adxx != 0 ){
								if( k3p3 <= SCONX1 ){
									sconX_.scon[scpxx-One][k3p3-One] = k3p4;
									}
								else if( sconX_.scolnk[scpxx-One] > 
								  0 ){
									sconX_.scono[sconX_.scolnk[scpxx-One]-
									  One][k3p3-SCONX1-One] = k3p4;
									}
								}
							}
						}
					}
				/*                  set K3N to beg of next switch */
				vbdataX_.k3n = vbdataX_.k3 + 5;



				/*+  SAB 6/87 - THE 349 ... 651 FUNCTION IS IMPLEMENTED BELOW. */

				/*  -54  # PAIRS   -8X   FCN   SCN #   FCN   SCN # ...
				 * WHERE THERE ARE "# PAIRS" OF "FCN - SCN#" PAIRS. */

				/* FCN IS AS FOLLOWS: */

				/*       349 - COPY SC# OF -8X TO THE WORD-CLASS FIELD OF SWORK OF -8X
				 *       350 - COPY SC# OF -8X TO THE TYPE FIELD OF SWORK OF -8X
				 *       351 - COPY SC# OF -8X TO THE FORM FIELD OF SWORK OF -8X */

				/*       649 - COPY WORD-CLASS FIELD OF SWORK OF -8X TO SC# OF -8X
				 *       650 - COPY TYPE FIELD OF SWORK OF -8X TO SC# OF -8X
				 *       651 - COPY FORM FIELD OF SWORK OF -8X TO SC# OF -8X */


				/* FOR FUNCTION 349, THE NUMBER MUST BE BETWEEN 1 AND 20 INCLUSIVE.
				 * FOR FUNCTION 350, THE NUMBER MUST BE BETWEEN 1 AND 998 INCLUSIVE.
				 * FOR FUNCTION 351, THE NUMBER MUST BE BETWEEN 1 AND 99 INCLUSIVE.
				 * IF THE NUMBER IS NOT BETWEEN THESE LIMITS, NO ACTION IS TAKEN.
				 * IN THE PROGRAM BELOW, THESE ERROR CHECKS WILL BE APPARENT. */

				/* THIS SWITCH MUST BE USED BEFORE THE ELEMENT IS LOADED. THE
				 * SWORK IS NOT SPECIFICALLY ALTERED HERE, AND USE  OF 349,350,351
				 * AFTER A LOAD WILL PRODUCE UNPREDICTABLE RESULTS. */


				}
			else if( (((((k3p3 == 349) || (k3p3 == 350)) || (k3p3 == 
			  351)) || (k3p3 == 649)) || (k3p3 == 650)) || (k3p3 == 
			  651) ){

				n6jim = im81X_.im81 - k3p2;
				elepos = sworkX_.phrhed[n6jim-One];

				for( iz=izbeg; iz <= izend; iz += 2 ){
					kfun = vtrfX_.vtrf[iz-One];
					scnpos = vtrfX_.vtrf[iz+1-One];
					if( !((scnpos < 1) || (scnpos > SCONX)) ){
						/*         SCNPOS IS SC#
						 *         KFUN IS THE FUNCTION NUMBER (FCN), 349, 350, ETC
						 *         IN WHAT FOLLOWS, KVALUE IS THE NUMBER TO BE PUT INTO THE SCON
						 *         OR INTO THE SWORK FIELD */

						if( kfun == 349 ){
							if( scnpos <= SCONX1 )
								kvalue = sconX_.scon[elepos-One][scnpos-One];
							if( scnpos > SCONX1 )
								kvalue = sconX_.scono[sconX_.scolnk[elepos-One]-
								  One][scnpos-SCONX1-One];
							if( !((kvalue < 1) || (kvalue > 20)) )
								sworkX_.swork[n6jim-One][1-One] = kvalue;
							}
						else if( kfun == 350 ){
							if( scnpos <= SCONX1 )
								kvalue = sconX_.scon[elepos-One][scnpos-One];
							if( scnpos > SCONX1 )
								kvalue = sconX_.scono[sconX_.scolnk[elepos-One]-
								  One][scnpos-SCONX1-One];
							if( !((kvalue < 1) || (kvalue > 998)) )
								sworkX_.swork[n6jim-One][2-One] = kvalue;
							}
						else if( kfun == 351 ){
							if( scnpos <= SCONX1 )
								kvalue = sconX_.scon[elepos-One][scnpos-One];
							if( scnpos > SCONX1 )
								kvalue = sconX_.scono[sconX_.scolnk[elepos-One]-
								  One][scnpos-SCONX1-One];
							if( !((kvalue < 1) || (kvalue > 99)) )
								sworkX_.swork[n6jim-One][3-One] = kvalue;
							}
						else if( kfun == 649 ){
							if( scnpos <= SCONX1 )
								sconX_.scon[elepos-One][scnpos-One] = sworkX_.swork[n6jim-One][1-One];
							if( scnpos > SCONX1 )
								sconX_.scono[sconX_.scolnk[elepos-One]-
								  One][scnpos-SCONX1-One] = sworkX_.swork[n6jim-One][1-One];
							}
						else if( kfun == 650 ){
							if( scnpos <= SCONX1 )
								sconX_.scon[elepos-One][scnpos-One] = sworkX_.swork[n6jim-One][2-One];
							if( scnpos > SCONX1 )
								sconX_.scono[sconX_.scolnk[elepos-One]-
								  One][scnpos-SCONX1-One] = sworkX_.swork[n6jim-One][2-One];
							}
						else if( kfun == 651 ){
							if( scnpos <= SCONX1 )
								sconX_.scon[elepos-One][scnpos-One] = sworkX_.swork[n6jim-One][3-One];
							if( scnpos > SCONX1 )
								sconX_.scono[sconX_.scolnk[elepos-One]-
								  One][scnpos-SCONX1-One] = sworkX_.swork[n6jim-One][3-One];
							}
						}

					}
				}
			else{
				/*- SAB */





				/*  THIS FUNCTION WILL LOAD THE PARTICULAR SCONS OF THE PHRBEG HEAD
				 *       OF THE ELEMENT POINTED TO. */

				/*  MORE THAN ONE SCON POSITION CAN BE FILLED AT A TIME. */

				/*        -54  NO.PAIRS  -8X   PAIR 1   PAIR 2 . . . PAIR 3
				 *             K3P1      K3P2   K3P1*2 .... */

				n6jim = im81X_.im81 - k3p2;
				elepos = sworkX_.phrhed[n6jim-One];

				for( iz=izbeg; iz <= izend; iz += 2 ){

					/*     ARE WE LOADING A CONSTANT OR TRANSFERING FROM ANOTHER SCON? */
					scnpos = vtrfX_.vtrf[iz-One];
					/*+    VTRFM DISTINGUISHES RELATIVE POINTERS      05/19/86  *R1530DSD */
					if( vtrfmX_.vtrfm[iz+1-One] == vtrfvlX_.vtmrp ){

						/*    LOADING THE SAME POSITION FROM ANOTHER ELEMENT
						 *+    DONT CHANGE SCONS(4..8) IF LOCKED          10/29/86  *R1557DSD */
						if( !((sconX_.scon[elepos-One][1-One] < 0 && 
						  scnpos >= 4) && scnpos <= 8) ){
							n7jim = im81X_.im81 - vtrfX_.vtrf[iz+1-One];
							if( scnpos <= SCONX1 )
								sconX_.scon[elepos-One][scnpos-One] = sconX_.scon[sworkX_.phrhed[n7jim-One]-
								  One][scnpos-One];
							if( scnpos > SCONX1 )
								sconX_.scono[sconX_.scolnk[elepos-One]-
								  One][scnpos-SCONX1-One] = sconX_.scono[sconX_.scolnk[sworkX_.phrhed[n7jim-One]-
								  One]-One][scnpos-SCONX1-One];
							}
						/*     IF (VTRF(IZ+1) .LT. -70) GOTO 6150
						 *-                                               05/19/86  *R1530DSD
						 *                                                          *R1032MBS
						 *          456 FUNCTION SETS SCON 4,5 AND 6 FROM A CELL */
						}
					else if( scnpos == 456 ){
						/*+       DONT CHANGE SCONS(4..6) IF LOCKED       10/29/86  *B0426DSD */
						if( sconX_.scon[elepos-One][1-One] >= 0 ){
							celval = vbdataX_.vbcell[vtrfX_.vtrf[iz+1-One]-
							  One];
							cval1 = celval/100;
							cval2 = (celval - (cval1*100))/10;
							cval3 = celval - ((cval1*100) + (cval2*
							  10));
							if( !((cval1 == 0 || cval2 == 0) || cval3 == 
							  0) ){
								sconX_.scon[elepos-One][4-One] = cval1;
								sconX_.scon[elepos-One][5-One] = cval2;
								sconX_.scon[elepos-One][6-One] = cval3;
								/*-                                               10/29/86  *B0426DSD */
								}
							}
						/*-                                                         *R1032MBS */

						/*     LOADING A CONSTANT
						 *+    DONT CHANGE SCONS(4..6) IF LOCKED          10/15/86  *R1557DSD */
						}
					else if( !((sconX_.scon[elepos-One][1-One] < 0 && 
					  scnpos >= 4) && scnpos <= 8) ){
						if( scnpos <= SCONX1 )
							sconX_.scon[elepos-One][scnpos-One] = vtrfX_.vtrf[iz+1-One];
						if( scnpos > SCONX1 )
							sconX_.scono[sconX_.scolnk[elepos-One]-
							  One][scnpos-SCONX1-One] = vtrfX_.vtrf[iz+1-One];
						/*-                                               10/15/86  *R1557DSD */
						}
					/*-                                               10/15/86  *R1557DSD */


					}
				}
			}
		}
	return;

	/*        ***** END OF -57 SWITCH ***** */

	/*SW57 */

	/*     DEBUG INIT (CELL,TYPTST,VBVALU,FALCND,IZBEG,IZEND) */

} /*end of function*/

