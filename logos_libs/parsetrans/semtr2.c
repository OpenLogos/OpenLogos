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
/* this was program t2sem1 with function t2sem1
*/	/*     THIS ROUTINE IS CALLED BY THE TRAN 2,3,4 PROGRAMS (SWITCH 22) */
	/*     IT SEARCHES SP22 RULES FOR A MATCH, PROCESSES VTR OF
	 *        MATCHED RULE, AND RETURNS TO THE CALLING TRAN PROGRAM */
	/*  CHANGES:
	 *    08/09/94 jal:  L2107 : remove the -96 function (search to right for
	 *        a verb and make that the index) from TRANs 1,3,4.
	 *    04/23/87 *R1679RKH*  OFL3B conversion & R1685 Swork limi
	 *    08/19/86 *R1561DSD: 100 SCONS
	 *    04/03/86 *R1511DSD: GE FORM 90 ACTS LIKE 97 ON WC01,5,6,7,8
	 *    12/03/85 */
	/*+ OFL3B conversion & R1685 Swork limit Change RKH  04/23/87   R1679 */
	/*-                                             RKH  04/23/87   R1679 */
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




void /*FUNCTION*/ semtr2()
{
	static short int lpstrt;
	static char pgmnam[9] = "semtr2  ";
	static short form18[7]={17,28,21,5,3,4,92};
	static short nwcsem[10]={1,4,2,5,4,3,3,4,2,1};
	static short nwc19[9]={115,305,309,325,380,381,382,415,750};
	static short wcsem[10][5]={0,0,0,0,0,9,11,12,13,0,13,19,0,0,0,
	  3,9,11,12,13,3,6,17,19,0,8,20,19,0,0,13,18,19,0,0,1,4,14,15,
	  16,12,2,0,0,0,0,0,0,0,0};
	static short frmtst[7]={28,74,91,94,76,93,92};
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
	static short o4 = 0;
	static short wc = 0;
	static short w1 = 0;
	static short w2 = 0;
	static short w3 = 0;
	static short w4 = 0;
	static short iz2 = 0;
	static short set = 0;
	static short wcx = 0;
	static short gbm1 = 0;
	static short gbm2 = 0;
	static short wcnt = 0;
	static short wend = 0;
	static short gus99 = 0;
	static short retsw = 0;
	static short sav99 = 0;
	static short scptr = 0;
	static short start = 0;
	static short subsw = 0;
	static short total = 0;
	static short type1 = 0;
	static short type2 = 0;
	static short wstrt = 0;
	static short lincnt = 0;
	static short scn12x = 0;
	static short scn45x = 0;
	static short sc45ms = 0;
	static short scon12 = 0;
	static short sc12ms = 0;
	static short supset = 0;
	static short type1x = 0;
	static short type2x = 0;
	static short wrtlin = 0;
	static short savtyp[21][2];

	if( opswX_.sw[3-One] == 1 )
		scommsX_.switch_ = 1;

	semargX_.pntr9 = 0;
	gus99 = 0;
	sav99 = 0;
	scommsX_.sav14 = 0;
	srch46X_.srch46 = 0;
	flag32X_.prm32 = 0;
	nptpsvX_.nptpsv = 0;
	flag32X_.ad32 = -1;

	memset(semfrmX_.semwrk,'\0',sizeof(semfrmX_.semwrk));
	memset(savtyp,'\0',sizeof(savtyp));


	/*   THE CODE FROM 1010 TO 1050 DOES 2 THINGS FOR EACH TRAN: */

	/*   1) IF INDEX EQUALS -96, SEARCH SWORK/NSWORKS FOLLOWING LAST ELEMENT
	 *         MATCHED FOR A VERB WHICH WILL BE USED AS INDEX AND A SEMWRK. */

	/*   2) CREATES TEMP SWORKS FOR SEARCH OF SP22 RULES (FIRST SEMWRK IS
	 *         A DUMMY CREATED FROM VERBAL ELEMENT(INDEX))
	 *         SEARCH STOPS AT CLAUSE BOUNDARY(E.G. 19008) */


	if( !(semargX_.index != -96 || semargX_.tran != 2) ){
		for( iz=semtrX_.i3sem; iz <= sworkX_.phct; iz++ ){
			wc = sworkX_.swork[iz-One][1-One];
			type1 = sworkX_.swork[iz-One][2-One];
			type2 = sconX_.scon[sworkX_.phrhed[iz-One]-One][13-One];
			scon12 = sconX_.scon[sworkX_.phrhed[iz-One]-One][12-One];
			if( wc == 2 ){
				semargX_.index = iz;

				/*   CHECK THAT THE VERB HAS NOT ALREADY BEEN TRANSFORMED BY 47 SWITCH */
				if( semargX_.k3p3 != -98 ){
					if( sconX_.scon[sworkX_.phrhed[iz-One]-One][14-
					  One] == 2 )
						break;
					}
				for( iz2=1; iz2 <= semargX_.nwrks; iz2++ ){
					if( semtrX_.orig[iz2-One] == -96 )
						goto L_8521;
					}
				}

			if( !(wc != 19 && wc != 20) ){
				if( type1 >= 6 && type1 <= 10 )
					break;
				if( type2 >= 6 && type2 <= 10 )
					break;
				if( (wc == 20 && type1 >= 885) && type1 <= 887 )
					break;
				}
			}
		goto L_8460;
L_8521:
		semtrX_.orig[iz2-One] = iz;
		}

	/*   FIRST SEMWRK IS AN INDEX TO SP22 RULES */
	while( TRUE ){
		wcx = sworkX_.swork[semargX_.index-One][1-One];
		type1x = sworkX_.swork[semargX_.index-One][2-One];
		type2x = sconX_.scon[sworkX_.phrhed[semargX_.index-One]-One][13-One];
		scn12x = sconX_.scon[sworkX_.phrhed[semargX_.index-One]-One][12-One];
		/*+ OFL3B conversion & R1685 Swork limit Change RKH  04/23/87   R1679 */
		scn45x = sconX_.scono[sconX_.scolnk[sworkX_.phrhed[semargX_.index-One]-One]-One][45-SCONX1-One];

		semfrmX_.semwrk[1-One][1-One] = wcx;
		semfrmX_.semwrk[1-One][2-One] = type1x;
		semfrmX_.semwrk[1-One][3-One] = sconX_.scon[sworkX_.phrhed[semargX_.index-One]-One][11-One];
		if( semfrmX_.semwrk[1-One][3-One] == 0 )
			semfrmX_.semwrk[1-One][3-One] = type2x;
		semfrmX_.semwrk[1-One][4-One] = 0;

		if( wcx > 6 ){

			if( wcx == 7 || wcx == 8 )
				semfrmX_.semwrk[1-One][1-One] = 1;
			if( (semargX_.tran == 4 && wcx == 8) && srcflgX_.srcflg == 1 )
				semfrmX_.semwrk[1-One][1-One] = 8;
			if( wcx == 11 && srcflgX_.srcflg == 1 ){

				/*   PR1243 WC11 TRANSFORMATION */

				for( gb=1; gb <= 9; gb++ ){
					if( type1x == nwc19[gb-One] )
						goto L_757;
					}
				semfrmX_.semwrk[1-One][1-One] = 13;
				goto L_760;
L_757:
				semfrmX_.semwrk[1-One][1-One] = 19;
				}
			else{
				if( wcx == 11 )
					semfrmX_.semwrk[1-One][1-One] = 13;
				}
			}
		else if( !(wcx == 2 || wcx == 3) ){
			if( wcx != 4 ){
				if( wcx == 6 ){

					/*   WORD CLASS 6 BECOMES WC01 INDEX */
					semfrmX_.semwrk[1-One][1-One] = 1;
					}
				else{

					/*   WORD CLASSES 1 AND 5 - IF SUPERSET = 13 INDEX = WC04
					 *   ANYTHING ELSE WILL BE WC01 */
					semfrmX_.semwrk[1-One][1-One] = 1;
					if( srcflgX_.srcflg == 1 ){
						if( type2x == 13 )
							semfrmX_.semwrk[1-One][1-One] = 4;
						}
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

L_760:
		;
		for( b=2; b <= semargX_.nwrks; b++ ){
			b2 = semtrX_.orig[b-One];
			semfrmX_.semwrk[b-One][1-One] = sworkX_.swork[b2-One][1-One];
			/*     PR 1478 9/83 */
			if( !(semargX_.tran != 4 || srcflgX_.srcflg != 1) ){
				o4 = sconX_.scon[sworkX_.phrhed[b2-One]-One][13-One];
				wc = sworkX_.swork[b2-One][1-One];
				if( (((wc == 1 || wc == 5) || wc == 6) || wc == 7) && (o4 >= 13 && o4 <= 16) )
					semfrmX_.semwrk[b-One][1-One] = 4;
				}
			semfrmX_.semwrk[b-One][2-One] = sworkX_.swork[b2-One][2-One];
			if( msformX_.forms[b-One] != 0 ){
				if( msformX_.forms[b-One] == -91 ){
					sc12ms = sconX_.scon[sworkX_.phrhed[b2-One]-One][12-One];
					/*+ OFL3B conversion & R1685 Swork limit Change RKH  04/23/87   R1679 */
					sc45ms = sconX_.scono[sconX_.scolnk[sworkX_.phrhed[b2-One]-One]-One][45-SCONX1-One];
					if( sc45ms == 6 )
						semfrmX_.semwrk[b-One][2-One] = 101;
					if( sc45ms == 7 )
						semfrmX_.semwrk[b-One][2-One] = 102;
					if( sc45ms == 8 )
						semfrmX_.semwrk[b-One][2-One] = 103;
					}
				else{
					if( msformX_.forms[b-One] == -92 ){
						semfrmX_.semwrk[b-One][3-One] = formsaX_.formsv[sconX_.scolnk[sworkX_.swork[b2-One][4-One]-
						  One]-One];
						}
					else{
						semfrmX_.semwrk[b-One][3-One] = msformX_.forms[b-One];
						}
					goto L_820;
					}
				}
			semfrmX_.semwrk[b-One][3-One] = sworkX_.swork[b2-One][3-One];
L_820:
			semfrmX_.semwrk[b-One][4-One] = sworkX_.swork[b2-One][4-One];
			}

		if( semargX_.k3p3 > 10 ){
			semargX_.pntr9 = semargX_.k3p3;
			semargX_.k3p3 = -99;
			if( wcx == 2 )
				semargX_.k3p3 = 1;
			if( wcx == 14 )
				semargX_.k3p3 = 1;
			}

		/*   IF PNTR EQUALS -99, WORDCLASS AND SUPERSET ARE SEARCHED,
		 *     AND PNTR TRANSFORMED ACCORDINGLY. */

		if( semargX_.k3p3 == -99 ){
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
			if( !(((wcx != 1 && wcx != 5) && wcx != 6) && wcx != 7)){
				if( type2x == 4 || type2x == 7 )
					semargX_.k3p3 = 2;
				}
			}
L_1260:
		scommsX_.setck = 0;
		scommpX_.pntrsv = semargX_.k3p3;

		lpstrt = 2;
L_1265:
		;
		for( w=lpstrt; w <= semargX_.nwrks; w++ ){
			w1 = semfrmX_.semwrk[w-One][1-One];
			w2 = semfrmX_.semwrk[w-One][2-One];
			w3 = semfrmX_.semwrk[w-One][3-One];
			w4 = semfrmX_.semwrk[w-One][4-One];

			/*+    GE WC01 FORM 90 HAS SAME EFFECT AS FORM 97  04/02/86  *R1511DSD */

			/*     ALL SOURCE; FOR GERMAN SOURCE WC:= 1 MAY BE OVERRIDDEN BELOW */
			if( w1 >= 5 && w1 <= 8 )
				semfrmX_.semwrk[w-One][1-One] = 1;

			if( srcflgX_.srcflg == 1 ){
				/*         GERMAN SOURCE ONLY */
				if( w1 == 1 ){
					if( w3 == 97 || w3 == 90 )
						semfrmX_.semwrk[w-One][1-One] = 4;
					}
				else if( w1 >= 5 && w1 <= 7 ){
					if( w3 == 97 || w3 == 90 )
						semfrmX_.semwrk[w-One][1-One] = 4;
					}
				else if( w1 == 8 ){
					if( semargX_.tran == 4 )
						semfrmX_.semwrk[w-One][1-One] = 8;
					}
				}

			if( w1 == 11 && srcflgX_.srcflg == 1 ){

				/*   TEST TYPE FIELD OF WC11 FOR TRANSFORMATION TO
				 *     WC19 OR WC 13 (PR 1243 - GERMAN SOURCE ONLY) */

				for( gb=1; gb <= 9; gb++ ){
					if( w2 == nwc19[gb-One] )
						goto L_1274;
					}
				semfrmX_.semwrk[w-One][1-One] = 13;
				goto L_8523;
L_1274:
				semfrmX_.semwrk[w-One][1-One] = 19;
				}
			else{
				if( w1 == 11 )
					semfrmX_.semwrk[w-One][1-One] = 13;

				/*   FOR PROCESS NOUNS CERTAIN TYPE FIELDS MUST BE NULLED
				 *     SCON(11) AND (13) VALUES THAT ARE SET TO ZERO ARE SAVED IN SAVTYP */
				if( w1 == 1 ){
					if( w != 2 ){
						scptr = sworkX_.phrhed[semtrX_.orig[w-One]-One];
						supset = sconX_.scon[scptr-One][13-One];
						if( supset != 4 && supset != 7 ){
							if( w2 != 749 )
								goto L_8523;
							if( sconX_.scon[scptr-One][13-One] != 0 ){
								savtyp[w-One][1-One] = sconX_.scon[scptr-One][13-One];
								sconX_.scon[scptr-One][13-One] = 0;
								}
							}
						else if( !(w2 >= 101 && w2 <= 103) ){
							semfrmX_.semwrk[w-One][2-One] = 0;
							}
						if( sconX_.scon[scptr-One][11-One] != 0 ){
							savtyp[w-One][2-One] = sconX_.scon[scptr-One][11-One];
							sconX_.scon[scptr-One][11-One] = 0;
							}
						}
					}
				else if( !(w1 != 18 && w1 != 19) ){
					set = sconX_.scon[sworkX_.phrhed[semtrX_.orig[w-One]-One]-One][11-One];
					if( w1 != 19 ){
						supset = sconX_.scon[sworkX_.phrhed[semtrX_.orig[w-One]-One]-One][13-One];

						/*   ADDITIONAL WC18 TRANSFORMATIONS (PR1272) */

						if( semfrmX_.semwrk[w-One][2-One] == 325 )
							goto L_8527;
						if( semfrmX_.semwrk[w-One][2-One] == 326 )
							goto L_8526;
						if( set == 28 )
							goto L_8525;
						if( supset == 1 ){
							if( msformX_.forms[w-One] == 0 ){
								semfrmX_.semwrk[w-One][3-One] = 21;
								if( w3 <= 7 ){

									/*   TRAN2 (GE) ONLY - TEST EL(S) PRECEDING WC18, FORM 01 */
									if( !((semargX_.tran != 2 || srcflgX_.srcflg != 1) || w3 !=  1) ){
										gbm1 = semtrX_.orig[w-One] - 1;
										gbm2 = semtrX_.orig[w-One] - 2;
										/*+ OFL3B conversion & R1685 Swork limit Change RKH  04/23/87   R1679 */
										if( (sworkX_.swork[gbm1-One][1-One] == 20 &&
											 sconX_.scono[sconX_.scolnk[sworkX_.phrhed[gbm1-One]-
										     One]-One][45-SCONX1-One] == 8) && 
										     sworkX_.swork[gbm2-One][1-One] == 1 ){
											semfrmX_.semwrk[w-One][3-One] = 17;
											/*+ OFL3B conversion & R1685 Swork limit Change RKH  04/23/87   R1679 */
											if( sconX_.scono[sconX_.scolnk[sworkX_.phrhed[gbm2-One]-One]-One][45-SCONX1-
											  One] == 6 )
												semfrmX_.semwrk[w-One][3-One] = 1;
											if( sconX_.scono[sconX_.scolnk[sworkX_.phrhed[gbm2-One]-One]-One][45-SCONX1-One] == 7 )
												semfrmX_.semwrk[w-One][3-One] = 3;
											/*-                                             RKH  04/23/87   R1679 */
											goto L_1385;
											/*-                                             RKH  04/23/87   R1679 */
											}
										else if( sworkX_.swork[gbm1-One][1-One] == 1 ){
											semfrmX_.semwrk[w-One][3-One] = 17;
											/*+ OFL3B conversion & R1685 Swork limit Change RKH  04/23/87   R1679 */
											if( sconX_.scono[sconX_.scolnk[sworkX_.phrhed[gbm1-One]-One]-One][45-SCONX1-
											  One] == 6 )
												semfrmX_.semwrk[w-One][3-One] = 1;
											if( sconX_.scono[sconX_.scolnk[sworkX_.phrhed[gbm1-One]-One]-One][45-SCONX1-
											  One] == 7 )
												semfrmX_.semwrk[w-One][3-One] = 3;
											/*-                                             RKH  04/23/87   R1679 */
											goto L_1385;
											}
										}

									semfrmX_.semwrk[w-One][3-One] = form18[w3-One];
									if( w3 == 7 && w2 == 104 )
										semfrmX_.semwrk[w-One][3-One] = 13;
									}
								}
L_1385:
							semfrmX_.semwrk[w-One][1-One] = 1;
							}
						else if( srcflgX_.srcflg == 1 ){
							if( supset != 11 ){
								if( !((set == 20 || set == 21) || set == 24) ){
									if( semfrmX_.semwrk[w-One][2-One] != 911 )
										goto L_8523;
									}
								}
							semfrmX_.semwrk[w-One][1-One] = 1;
							}
						}
					else if( !(set != 76 && set != 46) ){
						goto L_8524;
						}
					}
				}
L_8523:
			;
			}
		goto L_8522;
L_8524:
		semfrmX_.semwrk[w-One][1-One] = 13;
		semfrmX_.semwrk[w-One][3-One] = 5;
		goto L_1420;
L_8525:
		semfrmX_.semwrk[w-One][1-One] = 13;
		semfrmX_.semwrk[w-One][3-One] = 5;
		goto L_1420;
L_8526:
		semfrmX_.semwrk[w-One][1-One] = 13;
		semfrmX_.semwrk[w-One][3-One] = 5;
		goto L_1420;
L_8527:
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
		 *  RESET THE END CONDITION OF THE LOOP TO THE HIGHER VALUE BY
		 *  RESTARTING IT AT THE AD HOC SEMWRK AND CONTINUING. */
		lpstrt = w + 1;
		goto L_1265;

L_8522:
		/*   CALL SUBROUTINE TO SEARCH SP22 RULES
		 *        RETSW = 1 IS TO A NEW SEARCH DRIVEN BY -46 001 FUNCTION */
		semsrc(semsw2,&retsw);
		if( retsw != 1 )
			break;
		}
	if( errvrsX_.errlvl != 0 )
		return;

L_8460:
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
		fprintf( _spec_fp, "%3d%3d%3d%3d%3d%3d%3d\n", semargX_.tran, 
		  semargX_.nwrks, semargX_.index, semargX_.k3p3, start, 
		  total, semargX_.pntr9 );
		}

	subsw = 0;

	/*   RESTORE TYPE FIELDS THAT WERE NULLED */
	for( w=3; w <= semargX_.nwrks; w++ ){
		scptr = sworkX_.phrhed[semtrX_.orig[w-One]-One];
		if( sconX_.scon[scptr-One][13-One] == 0 && savtyp[w-One][0] != 0 )
			sconX_.scon[scptr-One][13-One] = savtyp[w-One][0];
		if( sconX_.scon[scptr-One][11-One] == 0 && savtyp[w-One][1] != 0 )
			sconX_.scon[scptr-One][11-One] = savtyp[w-One][1];
		}
	return;


} /*end of function*/

