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
program. If not, write to Globalware AG, Hospitalstraße 6, D-99817 Eisenach.

Linux port modifications and additions by Bernd Kiefer, Walter Kasper,
Deutsches Forschungszentrum fuer kuenstliche Intelligenz (DFKI)
Stuhlsatzenhausweg 3, D-66123 Saarbruecken
*/
/*
  This was program t1sem1 in tran 1. Moved to parsetrans library and renamed to semtr1
*/

/*                            (THIS ROUTINE IS CALLED BY
	 *                            THE TRAN1 PROGRAM (SWITCH 22).
	 *                            IT SEARCHES SP22 RULES FOR
	 *                            A MATCH, PROCESSES VTR OF
	 *                            MATCHED RULE, AND RETURNS
	 *                            TO THE CALLING TRAN PROGRAM) */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*  CHANGES:
	 *    08/09/94 jal:  L2107 : remove the -96 function (search to right for
	 *        a verb and make that the index) from TRANs 1,3,4.
	 *    04/23/87 *R1679RKH*  OFL3B conversion & R1685 Swork limi
	 *    08/17/86 *R1561DSD: 100 SCONS
	 *    04/03/86 *R1511DSD: GE FORM 90 ACTS LIKE 97 ON WC01,5,7,8
	 *    09/20/85 */

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





void /*FUNCTION*/ semtr1()
{
	static short int lpstrt;
	static short form18[7]={17,28,21,5,3,4,92};
	static short nwc19[9]={115,305,309,325,380,381,382,415,750};
	static short subset[5]={180,740,741,742,978};
	static short zero = 0;
	static short b = 0;
	static short g = 0;
	static short i = 0;
	static short j = 0;
	static short m = 0;
	static short w = 0;
	static short x = 0;
	static short y = 0;
	static short z = 0;
	static short b2 = 0;
	static short gb = 0;
	static short iz = 0;
	static short ms = 0;
	static short wc = 0;
	static short w1 = 0;
	static short w2 = 0;
	static short w3 = 0;
	static short w4 = 0;
	static short iz2 = 0;
	static short set = 0;
	static short wcx = 0;
	static short tran = 0;
	static short wcnt = 0;
	static short wend = 0;
	static short gus99 = 0;
	static short retsw = 0;
	static short sav99 = 0;
	static short start = 0;
	static short total = 0;
	static short type1 = 0;
	static short type2 = 0;
	static short wstrt = 0;
	static short errlvl = 0;
	static short fixflg = 0;
	static short lincnt = 0;
	static short pntrsv = 0;
	static short scn12x = 0;
	static short scn45x = 0;
	static short scon12 = 0;
	static short sc45ms = 0;
	static short supset = 0;
	static short type1x = 0;
	static short type2x = 0;
	static short wrtlin = 0;
	static short savtyp[21][2];
	static short dctwc[3]={4,8,12};
	static short dcttyp[3]={5,9,13};
	static short dctfm[3]={6,10,14};

	retsw = 0;
	scommsX_.switch_ = 0;
	nptpsvX_.nptpsv = 0;
	semargX_.pntr9 = 0;
	if( opswX_.sw[3-One] == 1)
		scommsX_.switch_ = 1;
	gus99 = 0;
	fixflg = 0;
	sav99 = 0;
	srch46X_.srch46 = 0;
	flag32X_.prm32 = 0;
	flag32X_.ad32 = -1;
	memset(semfrmX_.semwrk,'\0',sizeof(semfrmX_.semwrk));
	memset(savtyp,'\0',sizeof(savtyp));


	/*   THE CODE FROM 1010 TO 1050 DOES 2 THINGS FOR EACH TRAN:
	 *   1 ) IF INDEX EQUALS -96,SEARCH SWORK1/NSWORKS FOLLOWING LAST ELEMENT
	 *    MATCHED FOR A VERB WHICH WILL BE USED AS INDEX AND A SEMWRK.
	 *   2 ) CREATES TEMP SWORKS FOR SEARCH OF SP22 RULES(FIRST SEMWRK IS
	 *    A DUMMY CREATED FROM VERBAL ELEMENT(INDEX))
	 *    SEARCH STOPS AT CLAUSE BOUNDARY(E.G. 19008) */

	while( TRUE ){
		wcx = swork1X_.swork1[semargX_.index-One][dctwc[prctX_.js[sconX_.scolnk[semargX_.index-One]-One]-One]-One];
		type1x = swork1X_.swork1[semargX_.index-One][dcttyp[prctX_.js[sconX_.scolnk[semargX_.index-One]-One]-One]-One];
		type2x = sconX_.scon[semargX_.index-One][13-One];
		scn12x = sconX_.scon[semargX_.index-One][12-One];
		/*+ OFL3B conversion & R1685 Swork limit Change RKH  04/23/87   R1679 */
		scn45x = sconX_.scono[sconX_.scolnk[semargX_.index-One]-One][45-SCONX1-One];

		semfrmX_.semwrk[1-One][1-One] = wcx;
		semfrmX_.semwrk[1-One][2-One] = type1x;
		semfrmX_.semwrk[1-One][3-One] = sconX_.scon[semargX_.index-One][11-One];
		if( semfrmX_.semwrk[1-One][3-One] == 0 )
			semfrmX_.semwrk[1-One][3-One] = type2x;
		semfrmX_.semwrk[1-One][4-One] = 0;

		if( wcx > 6 ){

			if( wcx == 7 || wcx == 8 )
				semfrmX_.semwrk[1-One][1-One] = 1;
			if( wcx == 11 && srcflgX_.srcflg == 1 ){

				/*     PR1243  WC11 TRANSFORMATION */

				for( gb=1; gb <= 9; gb++ ){
					if( type1x == nwc19[gb-One] )
						goto L_157;
					}
				semfrmX_.semwrk[1-One][1-One] = 13;
				goto L_160;
L_157:
				semfrmX_.semwrk[1-One][1-One] = 19;
				}
			else{
				if( wcx == 11 )
					semfrmX_.semwrk[1-One][1-One] = 13;
				}
			}
		else if( !((wcx == 2 || wcx == 3) || wcx == 6) ){
			if( wcx != 4 ){

				/*   WORD CLASSES 1 AND 5 - IF SUPERSET = 13 INDEX = WC04
				 *   ANYTHING ELSE WILL BE WC01 */
				semfrmX_.semwrk[1-One][1-One] = 1;
				if( srcflgX_.srcflg == 1 ){
					if( type2x == 13 )
						semfrmX_.semwrk[1-One][1-One] = 4;
					}
				}


			/*   FOR WORD CLASSES 1,4,5,6 - CERTAIN TYPES BECOME WC02
			 *   THIS OVERRIDES PREVIOUS TRANSFORMATIONS */
			if( type2x == 4 || type2x == 7 )
				semfrmX_.semwrk[1-One][1-One] = 2;
			if( type2x >= 14 && type2x <= 16 )
				semfrmX_.semwrk[1-One][1-One] = 2;
			if( srcflgX_.srcflg != 2 ){
				for( ms=1; ms <= 5; ms++ ){
					if( type1x == subset[ms-One] )
						semfrmX_.semwrk[1-One][1-One] = 2;
					}
				}
			}


L_160:
		;
		for( b=2; b <= semargX_.nwrks; b++ ){
			b2 = semtrX_.orig[b-One];
			semfrmX_.semwrk[b-One][1-One] = swork1X_.swork1[b2-One][dctwc[prctX_.js[sconX_.scolnk[b2-One]-One]-One]-One];
			semfrmX_.semwrk[b-One][2-One] = swork1X_.swork1[b2-One][dcttyp[prctX_.js[sconX_.scolnk[b2-One]-One]-One]-One];
			if( msformX_.forms[b-One] != 0 ){
				if( msformX_.forms[b-One] == -91 ){

					/*+ OFL3B conversion & R1685 Swork limit Change RKH  04/23/87   R1679 */
					sc45ms = xpatnfX_.xpatfm[sconX_.scolnk[b2-One]-One][prctX_.js[sconX_.scolnk[b2-One]-One]-One];
					if( sc45ms != 0 )
						semfrmX_.semwrk[b-One][2-One] = sc45ms;
					/*-                                             RKH  04/23/87   R1679 */
					}
				else{
					if( msformX_.forms[b-One] == -92 ){
						semfrmX_.semwrk[b-One][3-One] = formsaX_.formsv[sconX_.scolnk[b2-One]-
						  One];
						}
					else{
						semfrmX_.semwrk[b-One][3-One] = msformX_.forms[b-One];
						}
					goto L_240;
					}
				}
			semfrmX_.semwrk[b-One][3-One] = swork1X_.swork1[b2-One][dctfm[prctX_.js[sconX_.scolnk[b2-One]-One]-One]-One];
L_240:
			semfrmX_.semwrk[b-One][4-One] = b2;
			}


		if( semargX_.k3p3 > 10 ){
			semargX_.pntr9 = semargX_.k3p3;
			semargX_.k3p3 = -99;
			if( wcx == 2 )
				semargX_.k3p3 = 1;
			if( wcx == 14 )
				semargX_.k3p3 = 1;
			}

		/*     IF PNTR EQUALS -99, WORDCLASS AND SUPERSET ARE SEARCHED,
		 *     AND PNTR TRANSFORMED ACCORDINGLY. */

		if( semargX_.k3p3 == -99 ){
			/*   DEFAULT VALUE IS 1 */
			semargX_.k3p3 = 1;
			if( !(wcx != 1 && wcx != 4) ){
				if( !(type2x < 13 || type2x > 16) ){
					ms = type2x - 12;
					if( ms == 2 ){
						semargX_.k3p3 = 6;
						}
					else if( ms == 3 ){
						semargX_.k3p3 = 3;
						}
					else if( ms == 4 ){
						semargX_.k3p3 = 4;
						/*+ OFL3B conversion & R1685 Swork limit Change RKH  04/23/87   R1679 */
						if( scn45x == 4 )
							semargX_.k3p3 = 5;
						}
					else{
						semargX_.k3p3 = 5;
						}
					goto L_1260;
					}
				}
			if( !((wcx != 1 && wcx != 5) && wcx != 7) ){
				if( type2x == 4 || type2x == 7 )
					semargX_.k3p3 = 2;
				}
			}
L_1260:
		scommsX_.setck = 0;
		pntrsv = semargX_.k3p3;

		/*    CERTAIN SEMWRKS ARE TRANSFORMED */
		lpstrt = 2;
L_1265:
		;
		for( w=lpstrt; w <= semargX_.nwrks; w++ ){
			w1 = semfrmX_.semwrk[w-One][1-One];
			w2 = semfrmX_.semwrk[w-One][2-One];
			w3 = semfrmX_.semwrk[w-One][3-One];
			w4 = semfrmX_.semwrk[w-One][4-One];

			/*+    WC01 FORM 90 HAS SAME EFFECT AS FORM 97    04/03/86  *R1411DSD */

			/*     ALL SOURCE; FOR GERMAN SOURCE WC:= 1 MAY BE OVERRIDDEN BELOW
			 *       NOTE: IN TRAN2,3,4, WC 6 IS INCLUDED IN THE NEXT TEST */
			if( (w1 == 5 || w1 == 7) || w1 == 8 )
				semfrmX_.semwrk[w-One][1-One] = 1;

			if( srcflgX_.srcflg == 1 ){
				/*         GERMAN SOURCE ONLY */
				if( w1 == 1 ){
					if( w3 == 97 || w3 == 90 )
						semfrmX_.semwrk[w-One][1-One] = 4;
					/*             NOTE: IN TRAN2,3,4, WC 6 IS INCLUDED IN THE NEXT TEST */
					}
				else if( (w1 == 5 || w1 == 7) || w1 == 8 ){
					if( w3 == 97 || w3 == 90 )
						semfrmX_.semwrk[w-One][1-One] = 4;
					/*                   THIS OVERRIDES THE '= 1' SETTING ABOVE */
					}
				else if( w1 == 8 ){
					if( tran == 4 )
						semfrmX_.semwrk[w-One][1-One] = 8;
					/*                   THIS OVERRIDES THE '= 1' SETTING ABOVE */
					}
				}

			/*     ALL SOURCE LANGUAGES */
			if( w1 == 11 && srcflgX_.srcflg == 1 ){


				/*     TEST TYPE FIELD OF WC11 FOR TRANSFORMATION TO
				 *     WC19 OR WC13 (PR 1243) */

				for( gb=1; gb <= 9; gb++ ){
					if( w2 == nwc19[gb-One] )
						goto L_1274;
					}
				semfrmX_.semwrk[w-One][1-One] = 13;
				goto L_8522;
L_1274:
				semfrmX_.semwrk[w-One][1-One] = 19;
				}
			else{
				if( w1 == 11 )
					semfrmX_.semwrk[w-One][1-One] = 13;

				/*   FOR PROCESS NOUNS, CERTAIN TYPE FIELDS MUST BE NULLED.
				 *   OFL4 AND TYPSAV CODES THAT ARE SET TO ZERO WILL BE SAVED IN SAVTYP */
				if( w1 == 1 ){
					if( w != 2 ){
						if( sconX_.scon[w4-One][13-One] != 4 && sconX_.scon[w4-One][13-One] != 7 ){
							if( w2 != 749 )
								goto L_8522;
							if( sconX_.scon[w4-One][13-One] != 0 ){
								savtyp[w-One][1-One] = sconX_.scon[w4-One][13-One];
								sconX_.scon[w4-One][13-One] = 0;
								}
							}
						else if( !(w2 >= 101 && w2 <= 103) ){
							semfrmX_.semwrk[w-One][2-One] = 0;
							}
						if( sconX_.scon[w4-One][11-One] != 0 ){
							savtyp[w-One][2-One] = sconX_.scon[w4-One][11-One];
							sconX_.scon[w4-One][11-One] = 0;
							}
						}

					}
				else if( w1 == 18 || w1 == 19 ){
					supset = sconX_.scon[w4-One][13-One];
					set = sconX_.scon[w4-One][11-One];
					if( w1 == 19 ){
						if( (set == 76 || set == 46) && srcflgX_.srcflg == 1 )
							goto L_8523;

						/*     ADDITIONAL WC18 TRANSFORMATIONS  PR1272 */

						}
					else if( semfrmX_.semwrk[w-One][2-One] == 325 ){
						goto L_8526;
						}
					else if( semfrmX_.semwrk[w-One][2-One] == 326 ){
						goto L_8525;
						}
					else if( set == 28 ){
						goto L_8524;
						}
					else if( supset == 1 ){
						if( msformX_.forms[w-One] == 0 ){
							semfrmX_.semwrk[w-One][3-One] = 21;
							if( w3 <= 7 ){
								semfrmX_.semwrk[w-One][3-One] = form18[w3-One];
								if( w3 == 7 && w2 == 104 )
									semfrmX_.semwrk[w-One][3-One] = 13;
								}
							}
						semfrmX_.semwrk[w-One][1-One] = 1;
						}
					else if( srcflgX_.srcflg == 1 ){
						if( supset != 11 ){
							if( !((set == 20 || set == 21) || set == 24) ){
								if( semfrmX_.semwrk[w-One][2-One] != 911 )
									goto L_8522;
								}
							}
						semfrmX_.semwrk[w-One][1-One] = 1;
						}
					}
				}
L_8522:
			;
			}
		goto L_8521;
L_8523:
		semfrmX_.semwrk[w-One][1-One] = 13;
		semfrmX_.semwrk[w-One][3-One] = 5;
		goto L_1420;
L_8524:
		semfrmX_.semwrk[w-One][1-One] = 13;
		semfrmX_.semwrk[w-One][3-One] = 5;
		goto L_1420;
L_8525:
		semfrmX_.semwrk[w-One][1-One] = 13;
		semfrmX_.semwrk[w-One][3-One] = 5;
		goto L_1420;
L_8526:
		semfrmX_.semwrk[w-One][1-One] = 19;
		semfrmX_.semwrk[w-One][3-One] = 5;
L_1420:
		z = semargX_.nwrks;
		x = w + 1;
		for( m=x; m <= semargX_.nwrks; m++ ){
			for( y=1; y <= 4; y++ ){
				semfrmX_.semwrk[z+1-One][y-One] = semfrmX_.semwrk[z-One][y-One];
				semtrX_.orig[z+1-One] = semtrX_.orig[z-One];
				}
			z -= 1;
			}

		semfrmX_.semwrk[w+1-One][1-One] = 1;
		semfrmX_.semwrk[w+1-One][2-One] = 995;
		semfrmX_.semwrk[w+1-One][3-One] = 38;
		semfrmX_.semwrk[w+1-One][4-One] = 0;
		semtrX_.orig[w+1-One] = 0;
		semargX_.nwrks += 1;
		/*+                                                        *R0GBA*GBA
		 *   POINT TO THE ADHOC SEMWRK, AND RESET THE LOOP SO THAT THE
		 *   BIGGER NWRKS IS RESET AT THE END POINT */
		lpstrt = w + 1;
		goto L_1265;

L_8521:
		/*        SEMWRKS ARE READY - CALL SEARCH SUBROUTINE */
		/*        RETURN 1 IS A NEW SEARCH DRIVEN BY THE -46 001 FUNCTION */
		semsrc(semsw1,&retsw);
		if( errlvl != 0 )
			return;
		if( retsw != 1 )
			break;
		}


	if( scommsX_.switch_ == 1 )
		{
		fprintf( _spec_fp, "  SEMWRKS = " );
		for( g=1; g <= 20; g++ ){
			fprintf( _spec_fp, "%3d", sgnX_.gn[g-One] );
			}
		fprintf( _spec_fp, "\n");
		for( g=1; g <= 20; g++ ){
			fprintf( _spec_fp, "%3d", semtrX_.orig[g-One] );
			}
		fprintf( _spec_fp, "\n");
		fprintf( _spec_fp, "%3d%3d%3d%3d%3d%3d%3d\n", tran, semargX_.nwrks, 
		  semargX_.index, semargX_.k3p3, start, total, semargX_.pntr9 );
		}

	/*   SET FORMOD'S SUBSW TO 0 */
	subswX_.subsw = 0;

	/*   RESTORE TYPE FIELDS THAT WERE NULLED */
	for( w=3; w <= semargX_.nwrks; w++ ){
		w4 = semfrmX_.semwrk[w-One][4-One];
		if( savtyp[w-One][1-One] != 0 && sconX_.scon[w4-One][13-One] == 0 )
			sconX_.scon[w4-One][13-One] = savtyp[w-One][1-One];
		if( savtyp[w-One][2-One] != 0 && sconX_.scon[w4-One][11-One] == 0 )
			sconX_.scon[w4-One][11-One] = savtyp[w-One][2-One];

		}
	return;
} /*end of function*/

