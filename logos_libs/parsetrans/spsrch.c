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
	/*  CHANGES:
	 *     11/15/93 AVK:  Handle error return from TRTAG
	 *     08/30/91 *JAL*:  DONT ALLOW MATCH TO EXTEND BEYOND CURRENT CLAUSE
	 *     04/23/87 *R1679RKH*  OFL3B CONVERSION & R1685 SWORK LIMI
	 *     02/25/87 *FIXRKH*  DISABLE STRETCH AND STOPPER MATCH VIA
	 *     02/21/87 *PTRFIXRKH*  FIX PREVIOUS ERROR
	 *     02/14/87 *FIXRKH*  CURRENT STRETCH

	/*     ARGUMENTS: */
	/*        LOCFLG - 1 = MAIN SP RULES, 2 = MINI SP RULES
	 *        SP     - THE SP ARRAY BEING SEARCHED
	 *        K7     - THE POSITION OF THE RULE IN THE SP ARRAY
	 *        OVRFLW - THE SP OVERFLOW ARRAY BEING USED
	 *        OFLAD  - THE POSITION OF THE RULE IN THE OVRFLW ARRAY
 	 *        WC50M  - FLAG FOR STREEEEETCH
	 *        WC50EL - FLAG FOR STREEEEETCH (EXPANDED FOR MULTI)
	 *        LOOK50 -   "   "      "       (   "    "    "  )
	 *        CHNG50 -   "   "      "       (   "    "    "  )
	 *        WC50CT - WHICH 'MULTI' */
	/*        RETFLG = INDICATES RETURN (0 = MATCH,   1 = NO MATCH) */

	/*     TAGPTR - Saves the ovrflow pointer for the tag rule
	 *     TAGLIN - Points where next available tag info. goes
	 *     NSPTR  - Points to SWORK record for the Tag encountered
	 *     SCNPTR = Points to SCON record for the Tag encountered */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <fcrt.h>
#include "trans.h"
#include "logoslib.h"
#include "parsetrans.h"
#include "projexts.h"

#include <string.h>
#include <jbctrl.h>
#include "parsetrans_ext.h"



void /*FUNCTION*/ spsrch(
	long int locflg,
	short int sp[],
	short int k7,
	short int ovrflw[],
	short int oflad,
	short int *wc50m, 
	short int wc50el[],
	short int look50[],
	short int chng50[],
	short int *wc50ct,
	short int *retflg
	)
{
	static LOGICAL8 alldon, stop1, stop2;
	static short int  scon13, scon45, stop[3], stpprt, stpval,taglin;
	static long int  _n;
	void  nmatch(), tmatch();
	static short jbnum[10]={2,5,8,1,4,7,10,13,16,19};
	static char pgmnam[9] = "TRSPSRC ";
	int testv;
	static short k = 0;
	static short gl = 0;
	static short k2 = 0;
	static short k8 = 0;
	static short xx = 0;
	static short ty1 = 0;
	static short ty2 = 0;
	static short ty3 = 0;
	static short igus = 0;
	static short tag1 = 0;
	static short tag2 = 0;
	static short awc10 = 0;
	static short gusk2 = 0;
	static short phri3 = 0;
	static short retsw = 0;
	static short gustop = 0;
	static short ovrflg = 0;
	static short stoper = 0;
	static short wc50i3 = 0;
	static short whch50 = 0;
	static short tagset[21]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

	//     Initialize Tag Processing Variables since they can not
	memset(&tagblkX_,'\0',sizeof(tagblkX_));
	ovrflg = 0;
	tgpairX_.tgpair = 0;
	/*+ Current Stretch                             RKH  02/14/87   FIX */
	stradjX_.stradj = 0;

	if( locflg == 1 )
		prttagX_.prtidx = 0;
	if( locflg == 2 )
		prmtagX_.prmidx = 0;

	/*     STOP1 is flag for 1 stopper only STOP2 is flag for 2 stoppers
	 *     ALLDON indicates stretch completed
	 *     STPVAL indictes amount (0 or 1) to be adjusted for CHNG50 calc. */

	stop1 = FALSE;
	stop2 = FALSE;
	alldon = FALSE;
	stpval = 0;

	/*       TEST 'MATPOS'  TO DISCOVER FIRST ELEMENT IN RULE
	 *        THAT NEEDS CHECKING. */

	if( commdX_.matpos >= 3 && commdX_.matpos <= 5 ){

		/*        MATPOS POINTS TO SECOND ELEMENT, SET ST16 TO 2, MATPOS TO 3 */

		sploopX_.st16 = 2;
		w50valX_.i3 += 1;
		commdX_.matpos = 3;
		}
	else if( commdX_.matpos >= 6 && commdX_.matpos <= 8 ){

		/*        MATPOS POINTS TO THIRD ELEMENT, SET ST16 TO 3, MATPOS TO 6 */

		sploopX_.st16 = 3;
		w50valX_.i3 += 2;
		commdX_.matpos = 6;
		}
	else if( commdX_.matpos == 9 ){

		/*        MATPOS POINTS TO FOURTH ELEMENT, SET ST16 TO 4, MATPOS TO 9 */

		sploopX_.st16 = 4;
		w50valX_.i3 += 3;
		commdX_.matpos = 9;
		}
	else{
		/*            1   2    3   4   5    6   7   8    9
		 *            -----    ---------    ---------    -
		 *             = 0        = 3          = 6      = 9 */


		/*        MATPOS POINTS TO FIRST ELEMENT, NO CHANGE
		 *        ST16 is 1 already in the common SPLOOP coming in (AVK) */

		commdX_.matpos = 0;
		}
	/*-------------------------------------------------------------- */


	/*     ******* MAIN LOOP, MOVES FROM ONE SWORK TO THE NEXT */

L_387:
	;
	for( k=sploopX_.st16; k <= sploopX_.li; k++ ){
		if(tranidX_.tranid == 1){
			phri3 = phrhdiX_.phrhed[w50valX_.i3-One];
		}
		else{
			phri3 = sworkX_.phrhed[w50valX_.i3-One];
		}
		stradjX_.spptr = k;
		stpprt = 0;
		/*            CEB WC50 */
		whch50 = 0;
		/*            WHCH50 = 1  SIGNALS  A WC50 ENCOUNTERED IN THE SP RULE
		 *                        AND WILL BE USED AFTER THE 720 LOOP BELOW */
		w50valX_.step50 = 0;
		/*            STEP50 CONTAINS THE COUNT OF HOW MANY ELEMENTSOF THE
		 *              SWORK STRING FAILED TO MATCH THE STOPPER BUT MATCHED
		 *              THE WC50 ELEMENT OF THE RULE */

		k8 = jbnum[k-One];

		/*            USE OVRFLW IF K GT 3 */
		if( k > 3 ){

			/*            USE OVERFLOW */
			if( ovrflg == 0 ){
				ovrin((short*)&locflg, (char*)ovrflw, k7, oflad);
				ovrflg = 1;
				}
			/*-------------------------------------------------------------- */
			w50valX_.gusn[0] = ovrflw[k8-One];
			w50valX_.gusn[1] = ovrflw[k8+1-One];
			w50valX_.gusn[2] = ovrflw[k8+2-One];
			}
		else{

			/*            NEW WC50 PROCESS */
			w50valX_.gusn[0] = sp[k8-One];
			w50valX_.gusn[1] = sp[k8+1-One];
			w50valX_.gusn[2] = sp[k8+2-One];
			}

		/*            CEB  IS IT A WC50 ? */
		if( w50valX_.gusn[0] >= 51 ){
			/*+ Current Stretch                             RKH  02/14/87   FIX */
			stradjX_.stradj = -1;
			stop1 = TRUE;

			/*-                                             RKH  01/13/87   STOPPER
			 *            IT IS
			 *            SET WC50 TO ITS NEGATIVE EQUIVALENT, E.G., 52 = -2 */
			w50valX_.gusn[0] = 50 - w50valX_.gusn[0];

			whch50 = 1;
			wc50i3 = w50valX_.i3;
			w50valX_.wc50ti = 100;
			w50valX_.cnt50[0] = w50valX_.gusn[0];
			w50valX_.cnt50[1] = w50valX_.gusn[1];
			w50valX_.cnt50[2] = w50valX_.gusn[2];

			/*            FORM MAY BE + */
			if( w50valX_.cnt50[2] != -1 ){
				w50valX_.wc50ti = w50valX_.cnt50[2];
				w50valX_.cnt50[2] = -1;
				}

			/*            RESET GUSN TO STOPPER
			 *                  (NEXT ELEMENT AFTER W50 ELEMENT) */

			/*            USE OVRFLW IF K GT 3 */
			if( (k + 1) > 3 ){

				/*            USE OVERFLOW, STOPPER IS K+1 */
				k8 = jbnum[k+1-One];
				if( ovrflg == 0 ){
					ovrin((short*)&locflg, (char*)ovrflw, k7, oflad);
					ovrflg = 1;
					}

				w50valX_.gusn[0] = ovrflw[k8-One];
				w50valX_.gusn[1] = ovrflw[k8+1-One];
				w50valX_.gusn[2] = ovrflw[k8+2-One];
				}
			else{
				k8 += 3;

				/*            CEB  NEW WC50 PROCESS */
				w50valX_.gusn[0] = sp[k8-One];
				w50valX_.gusn[1] = sp[k8+1-One];
				w50valX_.gusn[2] = sp[k8+2-One];
				}

			/*            SAVE STOPPER VALUES */
			w50valX_.gustov[0] = w50valX_.gusn[0];
			w50valX_.gustov[1] = w50valX_.gusn[1];
			w50valX_.gustov[2] = w50valX_.gusn[2];

			/*+                                             RKH  01/13/87   STOPPER
			 *     Set Next Stopper */

			/*     First Check if the Stopper 1 is the last
			 *     element of the rule */

			if( (k + 1) != sploopX_.li ){

				/*     There are more elements in the SP rule
				 *     Check if the element right after Stopper 1
				 *     is another stretch. In that case this will
				 *     1 stopper case */

				k8 = jbnum[k+2-One];

				if( (k + 2) > 3 ){
					if( ovrflg == 0 ){
						ovrin((short*)&locflg, (char*)ovrflw, k7, oflad);
						ovrflg = 1;
						}
					stop[0] = ovrflw[k8-One];
					stop[1] = ovrflw[k8+1-One];
					stop[2] = ovrflw[k8+2-One];
					}
				else{
					stop[0] = sp[k8-One];
					stop[1] = sp[k8+1-One];
					stop[2] = sp[k8+2-One];
					}

				/*     If Stopper 2 is a stretch do not use 2 stoppers */

				if( stop[0] < 51 ){
					stop2 = TRUE;
					stpval = 1;
					stpprt = 1;
					}
				}
			}

		/*-                                             RKH  01/13/87   STOPPER
		 *     *********  WC-TY-FR 'MATCHING' LOOP  ******* */

		while( (tranidX_.tranid != 1 && 
			    !(w50valX_.i3 > clsnfoX_.clndns[clsnfoX_.clcrnt-One] || 
			      w50valX_.i3 > sworkX_.phct) ) || 
			   (tranidX_.tranid == 1 &&
				  w50valX_.i3 <= spinX_.phrlim )
			  ){
			/*-                                                     *08/30/91*JAL*
			 *              if this is a wc10 rule with stretch dont allow
			 *              match beyond end of original rule??? */
			if( (vtrs42X_.sw42n == 1 && whch50 != 0) && w50valX_.i3 > 
			  mtcendX_.mtcend )
				goto L_900;

			for( k2=1; k2 <= 3; k2++ ){
				commdX_.matpos += 1;

				gusk2 = w50valX_.gusn[k2-One];
				if( gusk2 != -1 ){
					if( gusk2 > 0 ){


						/*     *** POSITIVE WORD CLASS, TYPE OR FORM VALUE *** */

						if( tranidX_.tranid == 1){
							testv = spinX_.nswork[w50valX_.i3-One][k2-One];
						}
						else{
							testv = sworkX_.swork[w50valX_.i3-One][k2-One];
						}
						if( testv != gusk2 ){

							/*        TYPE OR FORM - ADDITIONAL TESTS IF NO MATCH ON SWORK */

							if( k2 != 2 ){
								if( k2 != 3 )
									goto L_824;

								/*        WORD CLASS - ALL DONE */

								/*        FOR NEW GERMAN FORM FIELD SETS */

								if( gusk2 < 20 )
									goto L_824;

								formod(2,(long)gusk2,(long)k2,(long)w50valX_.i3,&retsw);
								if( retsw == 1 )goto L_1002;
								if( retsw == 2 )goto L_824;
								}

							if( gusk2 != sconX_.scon[phri3-One][11-One] ){
								if( gusk2 != sconX_.scon[phri3-One][13-One] )
									goto L_824;
								}
							}

						}
					else if( k2 == 1 ){

						/*     ******* NEGATIVE WORD CLASS MATCHING ***** */

						nmatch(gusk2,w50valX_.i3,&retsw);
						if( retsw == 1 )
							goto L_824;
						}
					else if( k2 == 2 ){

						/*     *** NEGATIVE TYPE VALUE *** */
 
						gustop = 1;
						if( gustop > 2 )
							goto L_824;

						/*--------------------------------------------------------------
						 *   READ TAGSET INTO TAGSET ARRAY
						 *         INSTEAD OF OVRFLW ARRAY TO AVOID OVERWRITING CURRENT DATA. */

						xx = -gusk2;
						if( ovrin((short*)&locflg,(char*)tagset, k7, xx) != 0 )
							return;

						/*     Set up Tag Array for printing */

						if( ((whch50 == 0) || (w50valX_.step50 ==  0)) || (stpprt != 0 && alldon) ){
							if( locflg == 1 ){
								if( prttagX_.prtidx < 4 ){
									prttagX_.prtidx += 1;
									lmove(&prttagX_.prttag[prttagX_.prtidx-One][0],
									  1,tagset,1,42);
									}
								}
							else if( prmtagX_.prmidx < 4 ){
								prmtagX_.prmidx += 1;
								lmove(&prmtagX_.prmtag[prmtagX_.prmidx-One][0],
								  1,tagset,1,42);
								}
							}

						/*-------------------------------------------------------------- */
						tag1 = tagset[0];


						/*+                                              11/14/86 R1616RKH
						 *     All Tags except Tag 8000 are processed in TAG234 Fortran
						 *     after all other SP elements have been matched. The purpose
						 *     of this modification is to speed up execution time and
						 *     allow the rule writer to be able to reference SP elements
						 *     to the right as well as to the left. Another benefit is
						 *     that now all tags can be combined on the same
						 *     line by using tag terminator 9000 or continued on next
						 *     line  by using continuation code 8888 at the end of the tag
						 *     line.
						 *-                                              11/14/86 R1616RKH */
						taglin = 1;
						tagblkX_.tagptr[taglin-One] = xx;
						tagblkX_.nsptr[taglin-One] = w50valX_.i3;
						tagblkX_.scnptr[taglin-One] = phri3;
						/*+                                             RKH  01/15/87   TAGPRT */
						if( tag1 == 8000 ){
							/* TAGSET OF 8000 HAS NEW WC-TY-FRM MATCHING CAPABILITY */
							tmatch(&locflg,retflg, k7);
							}
						else{
							trtag(&locflg,retflg,sp,ovrflw,whch50,stpprt,alldon, k7);
							}
						/*+ AVK: If error is returned, write error message and return */
						if( *retflg == -1 )
							goto L_1003;

						if( *retflg != 0 )
							goto L_824;
						}
					else{

						/*     *** NEGATIVE FORM VALUE ***
						 *+ OFL3B conversion & R1685 Swork limit Change RKH  04/23/87   R1679 */
						scon45 = sconX_.scono[sconX_.scolnk[phri3-One]-One][45-SCONX1-One];
						scon13 = sconX_.scon[phri3-One][13-One];

						/*     TRAN1 FIX */

						if( tranidX_.tranid == 1 ){

							/*     **** The following processing for TRAN1 only ***** */

							/*     German Only : -4 and -5 exclude WC types 14,15,16 */

							if( srcflgX_.srcflg == 1 ){
								if( !((gusk2 != -4) && (gusk2 != -5))  ){
									if( tranidX_.tranid == 1){
										testv = spinX_.nswork[w50valX_.i3-One][0];
									}
									else{
										testv = sworkX_.swork[w50valX_.i3-One][0];
									}
									if( testv == 4 ){

										if( !((gusk2 == -5) && (scon13 == 13)) ){
											if( !(((gusk2 == -4) && 
											  (scon13 >= 13)) && (scon13 <= 16)) )
												goto L_824;
											}
										}
									}
								}
							}

						/*+ OFL3B conversion & R1685 Swork limit Change RKH  04/23/87   R1679 */
						if( -gusk2 != scon45 ){

							if( srcflgX_.srcflg != 1 )
								goto L_824;

							if( tranidX_.tranid == 1){
								testv = spinX_.nswork[w50valX_.i3-One][0];
							}
							else{
								testv = sworkX_.swork[w50valX_.i3-One][0];
							}

							/*     TRAN1 FIX */
							if( tranidX_.tranid == 1 ){

								if( testv == 2 ){
									/*+ OFL3B conversion & R1685 Swork limit Change RKH  04/23/87   R1679 */
									if( gusk2 == -2 && scon45 == 6 )
										goto L_1002;
									}
								if( testv != 4 )
									goto L_824;
								if( gusk2 != -4 )
									goto L_824;
								/*+ OFL3B conversion & R1685 Swork limit Change RKH  04/23/87   R1679 */
								if( !(scon45 == 2 || scon45 == 8) )
									goto L_824;
								/*-                                             RKH  04/23/87   R1679 */
								}
							else if( testv != 2 ){
								goto L_824;
								/*+ OFL3B conversion & R1685 Swork limit Change RKH  04/23/87   R1679 */
								}
							else if( !(gusk2 == -2 && scon45 == 6)  ){
								goto L_824;
								}
							}
						}
					}

L_1002:
				;
				}

			/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */

			/*        ALL THREE SWORK/SP PARAMETERS MATCH FOR LEVEL K */

			/*        WAS IT A WC50 (STRETCH) ???? */

			if( whch50 == 0 )
				goto L_840;

			/*        * * * STRETCH ELEMENT MATCHED * * * */

			/*         WAS 'PAIR MATCH' REQUIRED BY 'TAGSET = 8000' MATCHING LOGIC?
			 *         FLAG 'TGPAIR' SET IN TMATCH PROG.
			 *+ Disable Stretch and Stopper match via 8000 TRKH  02/25/87   FIX
			 *     IF (TGPAIR .EQ. 1 .OR. TGPAIR .EQ. 2) GOTO 824
			 *-                                             RKH  02/25/87   FIX */

			/*        WHICH STRETCH ELEMENT - CONTROL OR STOPPER???
			 *+                                             RKH  01/13/87   STOPPER
			 *                          WHCH50=1 means stopper matched */
			if( whch50 == 1 ){

				//        STOPPER MATCHED, ALL DONE
				/*     IF all done then it is a match */
				if( alldon )
					goto L_1004;

				/*     Check if it is only 1st stopper */
				if( !stop2 )
					goto L_1004;
				w50valX_.gusn[0] = stop[0];
				w50valX_.gusn[1] = stop[1];
				w50valX_.gusn[2] = stop[2];
				w50valX_.i3 += 1;
				if( tranidX_.tranid == 1){
					phri3 = phrhdiX_.phrhed[w50valX_.i3-One];
				}
				else{
					phri3 = sworkX_.phrhed[w50valX_.i3-One];
				}
				alldon = TRUE;
				continue;
				}
			else{

				/*        CONTROL MATCHED, STOPPER FAILED ON ELEMENT 'I3' */
				whch50 = 1;

				/*        FORM FIELD OF CONTROL + */
				if( w50valX_.step50 > w50valX_.wc50ti )
					goto L_900;

				w50valX_.step50 += 1;

				/*        TRY STOPPER ON NEXT ELEMENT
				 *        DO NOT GO BEYOUND END OF SENTENCE */
				if( tranidX_.tranid == 1 && 
					  ((spinX_.nswork[w50valX_.i3-One][0] == 20) && 
				       (spinX_.nswork[w50valX_.i3-One][1] == 10) ) )
					   goto L_900;
				if( tranidX_.tranid != 1 && 
                      ((sworkX_.swork[w50valX_.i3-One][0] == 20) && 
				       (sworkX_.swork[w50valX_.i3-One][1] == 10) ) )
					goto L_900;

				w50valX_.gusn[0] = w50valX_.gustov[0];
				w50valX_.gusn[1] = w50valX_.gustov[1];
				w50valX_.gusn[2] = w50valX_.gustov[2];
				/*        SET I3 ETC FOR NEXT ELEMENT */
				w50valX_.i3 += 1;
				if( tranidX_.tranid == 1){
					phri3 = phrhdiX_.phrhed[w50valX_.i3-One];
				}
				else{
					phri3 = sworkX_.phrhed[w50valX_.i3-One];
				}

				if( diagsX_.deepdi ==  1 )
					{
					fprintf( _spec_fp, " WC50 CONTROL MATCHED, TRY STOPPER - I3, STEP50, PHRI3:%5d%5d%5d", 
					  w50valX_.i3, w50valX_.step50, phri3 );
					fprintf( _spec_fp, ", CONTROL:" );
					for(_n=0L; _n < sizeof(w50valX_.cnt50)/sizeof(short); _n++)
						fprintf( _spec_fp, "%5d", w50valX_.cnt50[_n] );
					for(_n=0L; _n < sizeof(w50valX_.gusn)/sizeof(short); _n++)
						fprintf( _spec_fp, "%5d", w50valX_.gusn[_n] );
					fprintf( _spec_fp, "\n" );
					}

				continue;
				}


			/*        CEB    NO MATCH WAS IT A WC50???????? */
L_824:
			if( whch50 == 0 )
				goto L_900;

			/*        YES,       IF = 2 STOPPER AND CONTROL BOTH FAILED GET NEW RULE */
			if( whch50 == 2 )
				goto L_900;

			/*        STOPPER DID NOT MATCH TRY CONTROL */

			/*+ Current Stretch                             RKH  02/14/87   FIX */
			stradjX_.stradj += 1;
			if( diagsX_.deepdi == 1 )
				{
				fprintf( _spec_fp, " WC50 STOPPER FAILED, TRY CONTROL  -         I3, K, K2:%5d%5d%5d", 
				  w50valX_.i3, k, k2 );
				fprintf( _spec_fp, ", CONTROL:" );
				for(_n=0L; _n < sizeof(w50valX_.cnt50)/sizeof(short); _n++)
					fprintf( _spec_fp, "%5d", w50valX_.cnt50[_n] );
				for(_n=0L; _n < sizeof(w50valX_.gusn)/sizeof(short); _n++)
					fprintf( _spec_fp, "%5d", w50valX_.gusn[_n] );
				fprintf( _spec_fp, "\n" );
				}

			w50valX_.gusn[0] = w50valX_.cnt50[0];
			w50valX_.gusn[1] = w50valX_.cnt50[1];
			w50valX_.gusn[2] = w50valX_.cnt50[2];
			whch50 = 2;

			/*+                                             RKH  01/13/87   STOPPER
			 *     If it was 2nd stopper that failed, reset I3 */

			if( alldon ){
				alldon = FALSE;
				stpprt = 0;
				w50valX_.i3 -= 1;
				}
			}
		goto L_1001;

L_840:
		w50valX_.i3 += 1;

		}
	goto L_862;

	/*     Both Stoppers 1 and 2 matched.
	 *     Reset Flags */
L_1004:
	stop1 = FALSE;
	stop2 = FALSE;
	alldon = FALSE;
	stpprt = 0;
	*wc50m = 1;

	/*        (MULTI-STRETCH)
	 *        LOOK50 AND CHNG50 ARE USED TO ADJUST THE -80 VALUES
	 *        ADJUST FOR WC10 ? */
	awc10 = 0;
	if( vtrs42X_.sw42n == 1 )
		awc10 = 1;

	look50[*wc50ct-One] = -80 - k + awc10;
	chng50[*wc50ct-One] = w50valX_.i3 - wc50i3 - 1 - stpval;

	wc50el[*wc50ct-One] = -k + awc10;

	/*        NEXT WC50 NOW READY
	 *+ Current Stretch                             RKH  02/14/87   FIX */
	stradjX_.stradj = 0;
	*wc50ct += 1;

	if( diagsX_.deepdi == 1 )
		{
		fprintf( _spec_fp, " WC50 STOPPER MATCHED, STRETCH COMPLETED - I3,K,WC50I3:%4d%4d%4d", 
		  w50valX_.i3, k, wc50i3 );
		fprintf( _spec_fp, ", LOOK50:" );
		for(_n=0L; _n < 3; _n++)
			fprintf( _spec_fp, "%4d", look50[_n] );
		for(_n=0L; _n < 3; _n++)
			fprintf( _spec_fp, "%4d", chng50[_n] );
		for(_n=0L; _n < 3; _n++)
			fprintf( _spec_fp, "%4d", wc50el[_n] );
		fprintf( _spec_fp, ", CHNG50:\n" );
		}


	/*        WC50 IS DIFFERENT, IT HAS TWO PARTS THE CONTROL AND
	 *        THE STOPPER.  WE CAN NOT GOTO 840.  THE 860 LOOP MUST
	 *        BE REENTERED‹  IS THIS RULE DONE? */

	if( (k + 1 + stpval) != sploopX_.li ){
		/*        NO
		 *        RESTART THE RULE AT ELEMENT AFTER STOPPER
		 *+                                             RKH  01/13/87   STOPPER */
		sploopX_.st16 = k + 2 + stpval;

		/*     Reset STPVAL for next WC50 Match */

		stpval = 0;
		w50valX_.i3 += 1;
 		goto L_387;
		}

	/*        A MATCH HAS BEEN FOUND FOR ALL LEVELS */

	/*        A WC50 RULE MAY CAUSE A JUMP TO 862, SEE ABOVE */
L_862:
	*retflg = 0;
	return;

L_1003:
	fprintf( _spec_fp, "Tran spsrch: Error in tagset evaluation\n" );
	return;
L_1001:
	commdX_.matpos = 0;

L_900:
	*retflg = 1;

	return;
} /*end of function*/

