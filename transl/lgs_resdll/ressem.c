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
	/*CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC */
	/*  CHANGES:
	 *    08/06/93 JAL:  add -34 switch
	 *    03/27/86 R1446DSD: SET SEMWRK(2,1)=SAVTYP(JGUSPT,M) */
	/*     THIS ROUTINE IS CALLED BY RESMATCH AND RESSWS
	 *     WHEN THERE IS A -22 SWITCH IN THE MATCHED RULE.
	 *     IT SEARCHES RES22 RULES FOR A MATCH, PROCESSES VTR OF
	 *     RES22 RULE, AND RETURNS TO THE CALLING PROGRAM. */
	/*     LOCFLG  LOCATION OF PROGRAM TO GOTO ( 1 OR 2)
	 *     SWPTR   IS THE POINTER TO THE -22 SWITCH IN THE VTR OF
	 *             THE RES1 OR 2 RULE
	 *     R2VTRF  IS THE VTR ARRAY PASSED FROM THE RES1 OR 2 RULE
	 *     FIRST   IS THE BEGINNING OF THE CURRENT RES1 OR 2 MATCH
	 *     LVL2    IS THE LEVEL OF THE CALLING RES1 OR 2 RULE
	 *     SW22CH  CONTROLS THE FORM SAVE SO THAT IT IS NOT REPEATED
	 *     RETFLG  RETURN FLAG ( 0 = GOOD  1 = SECOND RETURN  -1 ERROR RETURN */

#include "rescommon.h"

void  ressem(
	long int locflg,
	long int swptr,
	short int r2vtrf[],
	long int first,
	long int lvl2,
	short int *sw22ch, 
	short int *retflg)
{
	static short int jguspt, n, om, swrk1m, swrk21, temp, vtrf[100];
	static char pgmnam[9] = "RESSEM  ";
	static short aswork = 0;
	static short bform = 0;
	static short prm1s = 0;
	static short dctad[3]={7,11,15};
	static short dctfm[3]={6,10,14};
	static short dctwc[3]={4,8,12};
	static short dctyp[3]={5,9,13};
	static short jbnum[10]={1,4,7,0,3,6,9,12,15,18};
	static short i = 0;
	static short j = 0;
	static short k = 0;
	static short l = 0;
	static short m = 0;
	static short x = 0;
	static short gb = 0;
	static short iz = 0;
	static short j2 = 0;
	static short k2 = 0;
	static short k3 = 0;
	static short k4 = 0;
	static short k7 = 0;
	static short li = 0;
	static short ll = 0;
	static short ms = 0;
	static short ss = 0;
	static short ty = 0;
	static short uu = 0;
	static short wc = 0;
	static short xx = 0;
	static short xy = 0;
	static short yy = 0;
	static short iz4 = 0;
	static short jb2 = 0;
	static short jb3 = 0;
	static short k3n = 0;
	static short ms1 = 0;
	static short typ = 0;
	static short form = 0;
	static short gusm = 0;
	static short gusn = 0;
//cat-102 temp location to negate gusn before calling OVRIN
short gusn_gusn = 0;
	static short gusx = 0;
	static short im81 = 0;
	static short indx = 0;
	//static short nptp = 0;
	static short pass = 0;
	static short prm1 = 0;
	static short prm2 = 0;
	static short prm3 = 0;
	static short prm4 = 0;
	static short vtrn = 0;
	static short cebwc = 0;
	static short frptr = 0;
	static short funct = 0;
	static short ikm81 = 0;
	static short match = 0;
	static short nwrks = 0;
	static short n6jim = 0;
	static short oflad = 0;
	static short retsw = 0;
	static short rule4 = 0;
	static short sixty = 0;
	static short sixt1 = 0;
	static short srch1 = 0;
	static short srch2 = 0;
	static short start = 0;
	static short resnum = 0;
	static short total = 0;
	static short typex = 0;
	static short typtr = 0;
	static short wcptr = 0;
	static short celnum = 0;
	static short celval = 0;
	static short frptr2 = 0;
	static short jgusav = 0;
	static short listop = 0;
	static short negflg = 0;
	static short ovrflg = 0;
	static short tagpos = 0;
	static short tagval = 0;
	static short thirt1 = 0;
	static short twelve = 0;
	static short twent1 = 0;
	static short tyd234 = 0;
	static short typtr2 = 0;
	static short wcptr2 = 0;
	static short tstnum = 0;
	static short *rule2 = NULL;
	static short overft[21]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
	static short overf2[21]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
	static short semwrk[6][7]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
	short int dummy2=0;

	*retflg = 0;

	if( locflg == 2 ){
 
		if( aswork != 0 ){
			swX_.swork[aswork-One][dctfm[jbgusX_.jgus[aswork-One]-One]-One] = prm1s;
			savtyX_.savfrm[aswork-One][jbgusX_.jgus[aswork-One]-One] = prm1s;
			}
		}
	else{
 
		pass = 0;
		im81 = first - 81;

		indx = im81 - (r2vtrf[swptr+1-One]);
		jgusav = 0;
		srch1 = 0;
		srch2 = 0;

		/*  CREATE FIRST SET OF SEMWRKS FOR FIRST REL POINTER */

		l = 1;
		j = r2vtrf[swptr+1-One];

		if( j == 3 || j == 4 ){
			if( j == 3 )
				semwrk[0][0] = 3;
			if( j == 4 )
				semwrk[0][0] = 2;
			semwrk[0][3] = 0;
			semwrk[0][6] = 0;

			if( cellX_.cell[11-One] == 0 ){
				semwrk[0][1] = cellX_.cell[2];
				semwrk[0][2] = cellX_.cell[4];
				semwrk[0][4] = cellX_.cell[22-One];
				semwrk[0][5] = cellX_.cell[23-One];
				}
			else{
				semwrk[0][1] = cellX_.cell[13-One];
				semwrk[0][2] = cellX_.cell[15-One];
				semwrk[0][4] = cellX_.cell[32-One];
				semwrk[0][5] = cellX_.cell[33-One];

				}
			}
		else{

			m = im81 - j;
			k = -80 - j;

			jguspt = jbgusX_.jgus[m-One];
			if( jbgusX_.jgus[m-One] <= 0 )
				jguspt = 1;
			/*+                                               03/27/86  R1446DSD
			 *     SAVE WORD-CLASS */
			swrk1m = swX_.swork[m-One][dctwc[jguspt-One]-One];
			/*-                                               03/27/86  R1446DSD */
			semwrk[0][0] = swX_.swork[m-One][dctwc[jguspt-One]-One];
			if( chgsemX_.semchg[k-One][0] != 0 )
				semwrk[0][0] = chgsemX_.semchg[k-One][0];
			semwrk[0][1] = swX_.swork[m-One][dctyp[jguspt-One]-One];
			if( chgsemX_.semchg[k-One][1] != 0 )
				semwrk[0][1] = chgsemX_.semchg[k-One][1];
			semwrk[0][2] = swX_.swork[m-One][dctfm[jguspt-One]-One];
			if( chgsemX_.semchg[k-One][2] != 0 )
				semwrk[0][2] = chgsemX_.semchg[k-One][2];
			semwrk[0][3] = m;
			semwrk[0][4] = ofltagX_.ofl1r[m-One][jguspt-One];
			semwrk[0][5] = ofltagX_.ofl4r[m-One][jguspt-One];
			semwrk[0][6] = savtyX_.savtyr[m-One][jguspt-One];
			/*                                                          *B0249JGB-
			 *+                                               03/24/86  R1446DSD
			 *     AFTER COPY, WE WILL USE THE REAL TYPE FOR FIRST SEMWRK(2) ONLY */
			swrk21 = savtyX_.savtyr[m-One][jguspt-One];
			}
		/*-                                               03/24/86  R1466DSD */

		/*   SECOND SEMWRK IS A COPY OF THE FIRST */
		for( m=1; m <= 7; m++ ){
			semwrk[1][m-One] = semwrk[0][m-One];
			}
		/*+                                               03/27/86  R1446DSD
		 *     FOR ENGLISH-SOURCE VERBS (WC MAY HAVE BEEN CHANGED TO 3),
		 *     REPLACE THE FIRST SEMWRK(2) VALUE AFTER COPY TO SECOND SEMWRK */
		if( srcflgX_.srcflg == 2 ){
			if( swrk1m == 2 || swrk1m == 3 )
				semwrk[0][1] = swrk21;
			}

		for( l=2; l <= 5; l++ ){
			j = r2vtrf[swptr+l-One];

			if( j == 0 )
				goto L_8101;
			if( j == 2 ){
				semwrk[l+1-One][0] = 1;
				if( cellX_.cell[11-One] == 1 ){
					semwrk[l+1-One][1] = cellX_.cell[35-One];
					semwrk[l+1-One][2] = cellX_.cell[14-One];
					semwrk[l+1-One][3] = 0;
					semwrk[l+1-One][4] = cellX_.cell[36-One];
					semwrk[l+1-One][5] = cellX_.cell[34-One];
					semwrk[l+1-One][6] = 0;
					}
				else{
					semwrk[l+1-One][1] = cellX_.cell[25-One];
					semwrk[l+1-One][2] = cellX_.cell[3];
					semwrk[l+1-One][3] = 0;
					semwrk[l+1-One][4] = cellX_.cell[26-One];
					semwrk[l+1-One][5] = cellX_.cell[24-One];
					semwrk[l+1-One][6] = 0;
					}
				}
			else{

				m = im81 - j;
				k = -j - 80;
				jguspt = jbgusX_.jgus[m-One];
				if( jbgusX_.jgus[m-One] <= 0 )
					jguspt = 1;

				/*   CREATE SECOND SET OF SEMWRKS FOR FIRST REL POINTER.
				 *   CREATE SEMWRKS FOR REMAINING POINTERS (IF ANY)
				 *   THESE ARE THE ELEMENTS THAT RES22 WILL ATTEMPT TO MATCH */
				semwrk[l+1-One][0] = swX_.swork[m-One][dctwc[jguspt-One]-One];
				if( chgsemX_.semchg[k-One][0] != 0 )
					semwrk[l+1-One][0] = chgsemX_.semchg[k-One][0];
				semwrk[l+1-One][1] = swX_.swork[m-One][dctyp[jguspt-One]-One];
				if( chgsemX_.semchg[k-One][1] != 0 )
					semwrk[l+1-One][1] = chgsemX_.semchg[k-One][1];
				semwrk[l+1-One][2] = swX_.swork[m-One][dctfm[jguspt-One]-One];
				if( chgsemX_.semchg[k-One][2] != 0 )
					semwrk[l+1-One][2] = chgsemX_.semchg[k-One][2];
				semwrk[l+1-One][3] = m;
				/*   LOAD OVERFLOW 1 AND 4 */
				semwrk[l+1-One][4] = ofltagX_.ofl1r[m-One][jguspt-One];
				semwrk[l+1-One][5] = ofltagX_.ofl4r[m-One][jguspt-One];
				semwrk[l+1-One][6] = savtyX_.savtyr[m-One][jguspt-One];
				}
			/*                                                          *B0249JGB+ */
			}

		nwrks = 6;
		goto L_80;
L_8101:
		nwrks = l;

L_80:
		while( TRUE ){

			if( opswX_.sw[8-One] == 1 )
				{
				fprintf( _spec_fp, "*SEMWRK VALUES*\n    " );
				for( j=1; j <= nwrks; j++ ){
					for( i=1; i <= 4; i++ ){
						fprintf( _spec_fp, "%4d", semwrk[j-One][i-One] );
						}
					}
				fprintf( _spec_fp, "\n" );

				for( j=1; j <= nwrks; j++ ){
					for( i=5; i <= 7; i++ ){
						fprintf( _spec_fp, "%4d", semwrk[j-One][i-One] );
						}
					}
				fprintf( _spec_fp, "\n\n" );
				}

			zapit((short*)chgsemX_.semchg,60,(byte)0);

			/*   IF PRM5 OF -22SW IS 1 WE MUST CHECK ALL NODES
			 *   FOR SEMTAB MATCH AND OTHER SPECIAL CONDITIONS. */

			srch2 = 0;
			if( r2vtrf[swptr+5-One] == 1 ){
				x = jbgusX_.jgus[indx-One] + 1;
				/* GBA IF(X .GT. 3) GO TO 150 */
				if( x <= 3 ){
					for( m=x; m <= 3; m++ ){
						ss = ofltagX_.ofl4r[indx-One][m-One];
						if( (ss == 7 || ss == 11) || ss == 12 )
							goto L_110;
						if( swX_.swork[indx-One][dctyp[m-One]-One] == 102 ){

							gusm = 85 - 19;
							gusx = swX_.swork[indx-One][dctfm[m-One]-One];
							cebwc = swX_.swork[indx-One][dctwc[m-One]-One];

							resformod(2,cebwc,gusm,&gusx,&match,&retsw);
							if( errvrsX_.errlvl != 0 )
								goto L_8000;
							if( match == 1 )
								goto L_110;
							}

						}
					goto L_120;
L_110:
					srch2 = m;
					}
				}

			/*   SEARCH RES22 RULES FOR A MATCH */

L_120:
			while( TRUE ){
				pass += 1;

				/*   PASS 1 CHECKS THE SEMWRK(2) TYPE
				 *   PASS 2 CHECKS SEMWRK(5) (OFL1R)
				 *   PASS 3 CHECKS SEMWRK(6) (OFL4R)
				 *   PASS 4 CHECKS SEMWRK(7) (SAVTYR) */

				if( pass > 4 )
					break;
				/*+       ENGLISH SOURCE MATCHING ORDER DIFFERS: SEMWRK 5,7,6,2 */
				if( srcflgX_.srcflg == 2 ){
					if( pass == 1 )
						goto L_200;
					if( pass == 2 )
						goto L_230;
					if( pass == 3 )
						goto L_220;
					if( pass == 4 )
						goto L_180;
					}
				/*-
				 *                 GERMAN MATCHING: SEMWRK 2,5,6,7 */
				if( pass != 2 ){
					if( pass == 3 )
						goto L_220;
					if( pass == 4 )
						goto L_230;
					goto L_180;
					}
				/*-
				 *   USE OVERFLOW 1 */
L_200:
				x = 5;
				/*+ */
				if( srcflgX_.srcflg == 2 )
					goto L_240;
				/*-
				 *   TO PREVENT TWO SEARCHES ON EQUAL VALUES */
				if( semwrk[0][4] == semwrk[0][1] )
					continue;
				goto L_240;

				/*   USE OVERFLOW 4 */
L_220:
				x = 6;
				/*+
				 *    TO PREVENT TWO SEARCHES ON EQUAL TYPE VALUES */
				if( srcflgX_.srcflg == 1 && semwrk[0][5] == 
				  semwrk[0][1] )
					continue;
				if( srcflgX_.srcflg == 2 && semwrk[0][5] == 
				  semwrk[0][6] )
					continue;
				if( semwrk[0][5] == semwrk[0][4] )
					continue;
				goto L_240;
L_230:
				x = 7;

				if( srcflgX_.srcflg == 2 ){
					if( semwrk[0][6] == semwrk[0][4] )
						continue;
					goto L_240;
					}
				else if( semwrk[0][6] == semwrk[0][1] ){
					continue;
					}
				else if( semwrk[0][6] == semwrk[0][4] ){
					continue;
					}
				else if( semwrk[0][6] == semwrk[0][5] ){
					continue;
					}
				else{
					goto L_240;
					}
L_180:
				x = 2;
				/*+ */
				if( srcflgX_.srcflg != 1 ){
					/*            ENGLISH SOURCE: TO PREVENT SEARCHES ON TWO EQUAL VALUES */
					if( semwrk[0][1] == semwrk[0][4] )
						continue;
					if( semwrk[0][1] == semwrk[0][6] )
						continue;
					if( semwrk[0][1] == semwrk[0][5] )
						continue;
					}

L_240:
				if( semwrk[0][x-One] > 0 ){
					IDXVAL(&semwrk[0][0],&semwrk[0][x-One], &start,&total);

					start += 1;

					if( total != 0 ){

						for( j2=1; j2 <= total; j2++ ){
							k7 = start - j2;

							resnum = 3;
							rule2 = RULEIN_OCR(resnum, k7);
							if(rule2){
								errvrsX_.errlvl = 0;
								li = rule2[0];
								oflad = rule2[11-One];
								rule4 = rule2[3];
								ovrflg = 0;
								negflg = 0;

								if( li < nwrks )
									break;
								if( li <= nwrks ){

									if( rule4 != -1 ){
										if( ((rule4 != semwrk[0][1] && 
										  rule4 != semwrk[0][4]) && 
										  rule4 != semwrk[0][5]) && 
										  rule4 != semwrk[0][6] )
											goto L_8102;
										}
									/*                                         QUICK WC CHECK TO MATCH */

									rulchs(&rule2[4],&semwrk[1][0], 2,&retsw);
									if( retsw != 2 ){

										for( k=2; k <= li; k++ ){
											k4 = semwrk[k-One][3];
											jb2 = jbnum[k-One];

											for( k2=1; k2 <= 3; k2++ ){
												jb3 = jb2 + k2;
												gusn = rule2[jb3-One];

												if( k > 3 ){
													if( ovrflg == 0 ){

														errvrsX_.errlvl = OVRIN(&resnum, overf2, &k7, &oflad);
														if( errvrsX_.errlvl != 0 ) goto L_8102;

														rulchs(overf2,&semwrk[3][0], 3,&retsw);
														if( retsw == 2 ) goto L_8102;
														ovrflg = 1;
													}
												gusn = overf2[jb3-One];
												}

												if( gusn != -1 ){

												gusx = semwrk[k-One][k2-One];
												if( gusx != gusn ){

												if( k2 == 1 ){

												/*          NEGATIVE WORD CLASS MATCHING */
												if( gusn < -9 || gusn > -2 )
												goto L_8102;

												/*   EG - SPECIAL TEST -7 WC FOR WC1 MATCHING
												 *+                                                         *R1003MBS */
												if( !(gusn != -7 || srcflgX_.srcflg != 2) ){
													if( gusx == 1 ){
														gb = semwrk[k-One][k2+2-One];
														if( ((gb == 3 || gb == 5) || gb == 15) || gb == 17 )
														goto L_8103;
														goto L_8102;
													}
													else if( gusx == 19 ){
														if( semwrk[k-One][k2+1-One] == 21 )
														goto L_8103;
														goto L_8102;
													}
												}

												match2(&gusx,resnegX_.nwcr22[-gusn-One],8,&retsw);
												if( retsw == 2 )goto L_8102;

												if( k4 == commlX_.lstnod )
												negflg = 1;

												if( srcflgX_.srcflg == 1 ){

												if( gusn == -4 ){
												if( gusx == 14 ){
												if( semwrk[k-One][k2+5-One] != 9 )goto L_8102;
												}
												}

												}
												else if( gusn == -5 ){
												if( gusx == 1 ){
												if( semwrk[k-One][k2+2-One] != 30 )
												goto L_8102;
												}

												}
												else if( gusn == -8 ){
												if( gusx == 5 ){
												if( semwrk[k-One][k2+5-One] != 5 )
												goto L_8102;
												}
												}

												}
												else if( k2 == 2 ){

												/*   CHECK OTHER THREE TYPES */
												if( gusn != semwrk[k-One][4] &&
												    gusn != semwrk[k-One][5] &&
												    gusn != semwrk[k-One][6] ){
												if( gusn >= -1 )
												goto L_8102;

												/*  TRY MATCHING WITH HENUMS OR HASHCD'S */
											resnum = 3;
											gusn_gusn = - gusn;
											errvrsX_.errlvl = OVRIN(&resnum, overft, &k7, &gusn_gusn);
											if( errvrsX_.errlvl != 0 )goto L_8102;

											if( overft[0] < 6000 ){

											if( overft[0] == 1000 || overft[0] == 2000){
											
												if( k4 != 0 ){
													//   hennum test ONLY WC'S 1 2 3 4 6 CAN HAVE HENUM VALUES  
													if( overft[0] == 2000 ){
														if( hensavX_.hennum[k4-One][0] != -1 ){
															match4(&hensavX_.hennum[k4-One][0],&overft[1],11,&retsw);
															if( retsw == 1 )goto L_8103;
															// check root
															if( hensavX_.root_hennum[k4-One][0] != -1 ){
																match4(&hensavX_.root_hennum[k4-One][0],&overft[1],11,&retsw);
																if( retsw == 1 )goto L_8103;
															}
														}
													}
													// hash code test
													else {
														if( hashX_.hashcd[k4-One][0] != 0 )														{
															match4(&hashX_.hashcd[k4-One][0],&overft[1],11,&retsw);
															if( retsw == 1 ) goto L_8103;
															// check root
															if( hashX_.root_hashcd[k4-One][0] != 0 )														{
																match4(&hashX_.root_hashcd[k4-One][0],&overft[1],11,&retsw);
																if( retsw == 1 ) goto L_8103;
															}
														}
													}
												}
												goto L_8102;		// here is no match
											}




												/*   TRY TAGSET (A GROUP OF ACCEPTABLE TYPES) */

												tagpos = 1;
											}
											
											else{

												funct = overft[0] - 6000;
												tagpos = 1;
												if( !(funct == 10 || funct == 12) )
												goto L_8102;

												/*   FUNCTIONS 10 AND 12  TYPE FIELD TESTS.
												 *      PRM1 = POINTER
												 *      PRM2 TO 9000 (EQUALS END) = TYPE FIELDS
												 *      FOR FUNCTION 10  IF TYPE FIELD MATCHES TAGSET IS A MATCH
												 *      FOR FUNCTION 12  IF TYPE FIELD MATCHES TAGSET IS NOT A MATCH */

												/*          1ST DIGIT OF PRM1 (WHICH IS THE RELATIVE POINTER) IS
												 *          A FLAG :  = 0 MEANS TEST ALL NODES IN THE SWORK
												 *                    = 1 MEANS TEST THE JGUS NODE */
												prm1 = overft[tagpos+1-One];
												tstnum = 0;
												if( prm1 > 1000 )
												valths(&tstnum,&prm1, prm1);
												n6jim = semwrk[(prm1-79)-One][3];

												ms = tagpos + 2;

												match = 0;
												for( m=ms; m <= 22; m++ ){
													typex = overft[m-One];
													if( typex == 9000 )
													break;
													for( n=1; n <= 3; n++ ){
														yy = n;
														if( tstnum == 1 )
														yy = jbgusX_.jgus[n6jim-One];

														if( typex == ofltagX_.ofl4r[n6jim-One][yy-One] )
														match = 1;
														if( typex == ofltagX_.ofl1r[n6jim-One][yy-One] )
														match = 1;
														if( typex == savtyX_.savtyr[n6jim-One][yy-One] )
														match = 1;
														if( typex == swX_.swork[n6jim-One][dctyp[yy-One]-One] )
														match = 1;
														if( typex == savtyX_.subchg[n6jim-One][yy-One] )
														match = 1;
														if( tstnum == 1 )
														break;
													}
												}

												tagpos = m + 1;
												if( funct == 10 ){
													if( match != 1 )
													goto L_8102;
													/*                                            FUNCTION 12
													 *    BTT */
												}
												else if( match == 0 ){
													match = 1;
												}
												else{
													goto L_8104;
												}

												if( overft[tagpos-One] == 0 )
												goto L_8103;
											}

												while( TRUE ){
												tagval = overft[tagpos-One];

												if( tagval/1000 != 1 ){

												for( i=tagpos; i <= 22; i++ ){
													typex = overft[i-One];
													if( typex == 0 )
													break;
													/*                                EXCLUSION TAG? */
													if( typex/1000 != 1 ){
													if( typex == semwrk[k-One][1] )
													goto L_428;
													if( typex == semwrk[k-One][4] )
													goto L_428;
													if( typex == semwrk[k-One][5] )
													goto L_428;
													if( typex == semwrk[k-One][6] )
													goto L_428;
													}
												}
												goto L_8102;

												/*   SPECIAL TEST TO SEE IF EXCLUSION TAG FOLLOWS 'NORMAL' MATCH
												 *   IF A 'NORMAL' TAG FOLLOWS, RULE IS A MATCH */
L_428:
												tagpos = i + 1;
												if( overft[tagpos-One]/1000 != 1 )
												break;
												}


												/*   EXCLUSION TAGS MUST BE SATISFIED BEFORE DECLARING MATCH.
												 *   LOOP THROUGH TAGS TO FIND NUMBER OF EXCLUSION TAGS TO TEST. */
												for( ll=tagpos; ll <= 22; ll++ ){
												if( overft[ll-One]/1000 != 1 )
												break;
												}

												/*   LOOP THROUGH EXCLUSION TAG VALUES */
												ll -= 1;

												for( uu=tagpos; uu <= ll; uu++ ){
													tyd234 = overft[uu-One] - 1000;
													if( tyd234 == semwrk[k-One][1] )
													goto L_435;
													if( tyd234 == semwrk[k-One][4] )
													goto L_435;
													if( tyd234 == semwrk[k-One][5] )
													goto L_435;
													if( tyd234 == semwrk[k-One][6] )
													goto L_435;

													/*   ALL EXCLUSION VALUES SATISFIED.  TAG VALUE IS A MATCH */
												}
												break;


												/*   EXCLUSION VALUES NOT SATISFIED.  RESET TAGPOS AND GO TO TOP OF TAG */
L_435:
												tagpos = ll + 1;
												}

											}
												

												

												}
												else if( k2 != 3 ){
													goto L_8102;

													/*   FOR SUPERFORMS */
												}
												else if( gusn < 20 ){
													goto L_8102;
												}
												else{
													gusm = gusn - 19;
													cebwc = semwrk[k-One][0];

													resformod(2,cebwc,gusm,&gusx,&match,&retsw);
													if( errvrsX_.errlvl != 0 )
													goto L_8000;
													if( match != 1 )
													goto L_8102;
												}
												}
												}
												else if( k2 == 1 ){
													if( k4 == commlX_.lstnod )
													negflg = 1;
												}

L_8103:
												;
												}

											/*   ALL THREE SEMWRK/RES22 PARAMETERS MATCH FOR LEVEL K */

											}
										goto L_540;

L_8104:
										match = 0;
										}
									}
								}

							/*   RETURN TO HERE MEANS NO MATCH */
L_8102:
							;
							/*         TRY NEXT PASS */
							}
						}
					}
				}

			if( opswX_.sw[8-One] == 1 )
				{
				fprintf( _spec_fp, " NO MATCH IN RESSEM\n" );
				}

			/*   SRCH2 >  0  POINTS TO NEW ELEMENT SEARCH
			 *  SRCH2 = -1  CURRENT SEARCH IS ON NEW ELEMENT
			 *  SRCH1 =  1  MEANS FIRST ELEMENT MATCHED */

			if( srch2 == 0 )
				break;
			if( srch2 != -1 )
				goto L_145;
			jbgusX_.jgus[indx-One] = jgusav;
			if( srch1 != 1 )
				break;

			/*   A MATCH HAS BEEN FOUND
			 *   RESTORE OLD END POSITIONS */
L_540:
				// diagnostic
			rsdiag(4,&resnum,&dummy2,&k7);

			/*  IF SRCH2 FLAG IS POS GO BACK AND SEARCH AGAIN */

			if( srch2 <= 0 )
				goto L_565;
			srch1 = 1;
L_145:
			jgusav = jbgusX_.jgus[indx-One];
			jbgusX_.jgus[indx-One] = srch2;
			srch2 = -1;
			for( l=1; l <= 2; l++ ){
				semwrk[0][0] = swX_.swork[indx-One][dctwc[jbgusX_.jgus[indx-One]-One]-One];
				semwrk[0][1] = swX_.swork[indx-One][dctyp[jbgusX_.jgus[indx-One]-One]-One];
				semwrk[0][2] = swX_.swork[indx-One][dctfm[jbgusX_.jgus[indx-One]-One]-One];
				semwrk[0][4] = ofltagX_.ofl1r[indx-One][jbgusX_.jgus[indx-One]-One];
				semwrk[0][5] = ofltagX_.ofl4r[indx-One][jbgusX_.jgus[indx-One]-One];
				semwrk[0][6] = savtyX_.savtyr[indx-One][jbgusX_.jgus[indx-One]-One];
				}
			pass = 0;
			}
		*retflg = 1;
		goto L_7800;






		/*   IF A MATCH HAS BEEN MADE ON THE LAST ELEMENT OF THE RULE,
		 *   DON'T LEAVE THE NODES OPEN.
		 *   NEGFLG MEANS IT MATCHED ON A NEGWC.  DO LEAVE THE NODES OPEN. */
L_565:
		if( negflg != 1 ){
			for( ms=1; ms <= li; ms++ ){
				if( semwrk[ms-One][3] == commlX_.lstnod )
					negnodX_.no3res = 0;
				}
			}






		///////////////////////////////////////////////////////////////////////////////////////////////////
		/*   LOAD VTRF ARRAY WITH VTR22 ELEMENTS
		 *    IZ INDICATES CURRENT ELEMENT
		 *    NPTP INDICATES VTR22 STRING BEING PROCESSED
		 *    26 IS MAX NUM OF VTR ELEMENTS IN ONE STRING */

		resnum = 3;

		if( 0 == (iz4 = VTRIN(3, vtrf, rule2[12-1])) )
			goto L_8000;

		k3n = 1;
		aswork = 0;
		prm1s = 0;

		while( k3n <= iz4 ){
			k3 = k3n;
			vtrn = vtrf[k3-One];
			if( vtrn == 999 )
				break;
			prm1 = vtrf[k3+1-One];

			if( vtrn == -18 ){

				/*                                                          *J0195
				 *   THE -18 SWITCH IS USED TO SET CELLS */
				prm2 = vtrf[k3+2-One];
				prm3 = vtrf[k3+3-One];
				prm4 = vtrf[k3+4-One];
				n6jim = semwrk[(-79-prm3)-One][3];
				if( prm3 == 0 )
					n6jim = 0;
				k3n = k3 + 5;

				celnum = prm1;
				if( prm4 != 0 && cellX_.cell[11-One] != 0 )
					celnum += 10;

				/*   WHEN PRM1 = 4, PRM4 = 2 IS A FUNCTION NUMBER WHICH MEANS TEST
				 *   CELLS 3 AND 4 (OR 13 AND 14) BEFORE SETTING CELL 4 */
				if( !(prm1 != 4 || prm4 != 2) ){
					if( cellX_.cell[celval-One] != 0 )
						goto L_690;
					if( cellX_.cell[celval-1-One] != 1 && cellX_.cell[celval-1-One] != 3 )
						goto L_690;
					}

				if( prm2 == 0 ){

					if( !(celnum < 3 || celnum > 5) ){
						ms = celnum - 2;

						}
					else if( celnum < 13 || celnum > 15 ){

						if( celnum == 29 || celnum == 39 ){
							xx = jbgusX_.jgus[n6jim-One];
							cellX_.cell[celnum-One] = ofltagX_.ofl1r[n6jim-One][xx-One];
							}
						goto L_690;
						}
					else{
						ms = celnum - 12;
						}

					xx = jbgusX_.jgus[n6jim-One];
					yy = (xx*4) + 2;

					if( ms == 2 ){

						/*   FOR CELLS 4 AND 14 USE THE FORM (SIGNIFIES NUMBER OF A NOUN) */
						form = swX_.swork[n6jim-One][yy-One];
						celval = form;
						if( (form == 13 || form == 50) || form == 60 )
							celval = 1;
						if( form == 12 || form == 14 )
							celval = 2;
						if( form == 10 )
							celval = 3;
						}
					else if( ms == 3 ){

						/*   FOR CELLS 5 AND 15 USE THE FORM (SIGNIFIES THE TENSE OF A VERB) */
						form = swX_.swork[n6jim-One][yy-One];
						celval = form;
						}
					else{

						/*   FOR CELLS 3 AND 13, LOAD SET CODE INTO CELL */
						celval = savtyX_.savtyr[n6jim-One][xx-One];
						}
					}
				else{

					celval = prm2;
					}

				cellX_.cell[celnum-One] = celval;

				if( celnum == 11 && celval == 1 ){
					celnum = 13;
					cellX_.cell[celnum-One] = 1;
					if( n6jim != 0 )
						csaset(celnum,n6jim,&retsw);
					celnum = 11;
					}

				/*   SET CELL ARRAY ALSO */
				if( n6jim != 0 ){
					csaset(celnum,n6jim,&retsw);
					if( errvrsX_.errlvl != 0 )
						goto L_8000;

					/*             IF CELL(3) OR (13) IS BEING SET, ALSO SET (22) OR (32)
					 *                                                   AND (23) OR (33) */
					if( (prm1 == 3 || prm1 == 13) && prm2 == 0 ){
						celnum += 19;
						cellX_.cell[celnum-One] = ofltagX_.ofl1r[n6jim-One][xx-One];
						csaset(celnum,n6jim,&retsw);

						celnum += 1;
						cellX_.cell[celnum-One] = ofltagX_.ofl4r[n6jim-One][xx-One];
						csaset(celnum,n6jim,&retsw);

						}

					/*                                                AND (25) OR (35)
					 *                                                AND (26) OR (36) */
					if( (prm1 == 4 || prm1 == 14) && prm2 == 0 ){
						celnum += 20;
						cellX_.cell[celnum-One] = ofltagX_.ofl4r[n6jim-One][xx-One];
						csaset(celnum,n6jim,&retsw);

						celnum += 1;
						cellX_.cell[celnum-One] = swX_.swork[n6jim-One][(xx*4)+1-One];
						csaset(celnum,n6jim,&retsw);

						celnum += 1;
						cellX_.cell[celnum-One] = ofltagX_.ofl1r[n6jim-One][xx-One];
						csaset(celnum,n6jim,&retsw);

						}
					}

L_690:
		;
				}
			else if( vtrn == -34 ){


				/************************************************************************
				 *   BEGINNING OF -34 SWITCH.
				 *    FUNCTION:  TO LOAD THE WC, TYPE OR FORM FROM THE 34 SW ARGUMENTS
				 *               INTO THE SWORK. */

				/*     SET THE OUTGOING VALUES FOR TRAN1
				 *     CHECK THE TYPE FIELD RANGES AND SET THE CORRECT FIELDS
				 *            IN TRAN1                     IN RES, PASS TOTRAN1
				 *  WC:       SWORK(4,8 OR 12,N6JIM)       SWORK(4,8 OR 12,N6JIM)
				 *  SUPERSET: SCON(13,ELSCNP(N6JIM))       OFL4R(JGUS(N6JIM),N6JIM)
				 *  SET:      SCON(11,ELSCNP(N6JIM))       SWORK(DCT4(JGUS(N6JIM),N6JIM)
				 *  SUBSET:   SWORK(DCTYP(JS(N6JIM)),N6JIM) OFL1R(JGUS(N6JIM),N6JIM)
				 *  FORM:     SWORK(6,10 OR 14,N6JIM)      SWORK(6,10 OR 14,N6JIM)
				 ************************************************************************ */

				/*          FUNCTION - PRM4 POINTS TO HEAD */

				x = x;
				prm1 = vtrf[k3+1-One];
				prm2 = vtrf[k3+2-One];
				prm3 = vtrf[k3+3-One];
				prm4 = vtrf[k3+4-One];
				n6jim = semwrk[-79-(prm4)-One][3];
				k3n = k3 + 5;

				/*           DETERMINE WHICH PART OF THE SWORK WE SHOULD BE LOOKING AT. */

				om = jbgusX_.jgus[n6jim-One];

				if( prm1 > 0 ){
					swX_.swork[n6jim-One][dctwc[om-One]-One] = prm1;
					savtyX_.savwc[n6jim-One][om-One] = prm1;
					}

				if( prm2 > 0 ){
					if( prm2 <= 16 ){
						ofltagX_.ofl4r[n6jim-One][om-One] = prm2;
						savtyX_.savsup[n6jim-One][om-One] = prm2;
						if( ofltagX_.ofl1r[n6jim-One][om-One] <= 16 )
							ofltagX_.ofl1r[n6jim-One][om-One] = prm2;
						if( swX_.swork[n6jim-One][dctyp[om-One]-
						  One] <= 16 )
							swX_.swork[n6jim-One][dctyp[om-One]-
							  One] = prm2;
						}
					else if( prm2 <= 99 ){
						swX_.swork[n6jim-One][dctyp[om-One]-One] = prm2;
						savtyX_.savtyr[n6jim-One][om-One] = prm2;
						if( ofltagX_.ofl1r[n6jim-One][om-One] <= 99 )
							ofltagX_.ofl1r[n6jim-One][om-One] = prm2;
						if( ofltagX_.ofl4r[n6jim-One][om-One] > 16 )
							ofltagX_.ofl4r[n6jim-One][om-One] = prm2;
						}
					else{
						ofltagX_.ofl1r[n6jim-One][om-One] = prm2;
						savtyX_.savsub[n6jim-One][om-One] = prm2;
						if( swX_.swork[n6jim-One][dctyp[om-One]-
						  One] > 99 )
							swX_.swork[n6jim-One][dctyp[om-One]-
							  One] = prm2;
						if( ofltagX_.ofl4r[n6jim-One][om-One] > 99 )
							ofltagX_.ofl4r[n6jim-One][om-One] = prm2;
						}
					}

				if( prm3 > 0 ){
					swX_.swork[n6jim-One][dctfm[om-One]-One] = prm3;
					savtyX_.savfrm[n6jim-One][om-One] = prm3;
					}


				/*   ***** END OF -34 SWITCH ***** */

				}
			else if( vtrn == -36 ){

				/*       START OF -36 SWITCH */

				prm2 = vtrf[k3+2-One];
				prm1s = prm1;
				k3n = k3 + 3;
				/*                          PRM2 IS A REL. PTR. */
				bform = -79 - prm2;
				aswork = semwrk[bform-One][3];
				if( aswork != *sw22ch )
					*sw22ch = aswork;
				}
			else if( vtrn == -46 ){

				/*   -46 SWITCH
				 *     FUNCTION: CHANGE WC TYPE AND/OR FORM OF
				 *     ELEMENT POINTED BY FIRST PARAMETER
				 *     2ND PARAMETER = NEW WC
				 *     3RD PARAMETER = NEW TYPE
				 *     4TH PARAMETER = NEW FORM
				 *+ */
				prm1 = vtrf[k3+1-One];
				prm2 = vtrf[k3+2-One];
				prm3 = vtrf[k3+3-One];
				prm4 = vtrf[k3+4-One];

				if( prm1 != 32 ){

					ikm81 = first - 81;
					xx = semwrk[(-79-prm1)-One][3];
					wcptr = 4*jbgusX_.jgus[xx-One];
					typtr = wcptr + 1;
					frptr = wcptr + 2;

					if( prm2 != 0 ){
						if( prm2 > 0 ){

							swX_.swork[xx-One][wcptr-One] = prm2;
							}
						else{

							xy = semwrk[(-79-prm2)-One][3];
							wcptr2 = 4*jbgusX_.jgus[xy-One];
							swX_.swork[xx-One][wcptr-One] = swX_.swork[xy-One][wcptr2-One];
							}
						}

					if( prm3 != 0 ){
						if( prm3 <= 0 ){

							xy = semwrk[(-79-prm3)-One][3];
							typtr2 = (4*jbgusX_.jgus[xy-One]) + 1;
							swX_.swork[xx-One][typtr-One] = swX_.swork[xy-One][typtr2-One];

							}
						else if( prm3 >= 844 && prm3 <= 866 ){
							if( resswX_.ressw == 1 ){
								if( prm3 >= 862 ){
									if( prm3 == 862 ){
										temp = savtyX_.subchg[xx-One][jbgusX_.jgus[xx-One]-One];
										if( temp == 850 || temp == 852 || temp == 858 ||
											temp == 861 || temp == 864 )
											savtyX_.subchg[xx-One][jbgusX_.jgus[xx-One]-
											  One] = prm3;
										}
									if( !(savtyX_.subchg[xx-One][jbgusX_.jgus[xx-One]-One] < 844 ||
										  savtyX_.subchg[xx-One][jbgusX_.jgus[xx-One]-One] > 848) ){
										if( prm3 == 862 &&
											savtyX_.subchg[xx-One][jbgusX_.jgus[xx-One]-One] == 844 )
											savtyX_.subchg[xx-One][jbgusX_.jgus[xx-One]-One] = 846;
										goto L_840;
										}
									}
								}
							/*                                                          *1510BT
							 *  DON'T ALTER SUBCHG IF IT ALREADY HAS A VALUE */
							if( !((savtyX_.subchg[xx-One][jbgusX_.jgus[xx-One]-One] == 852 ||
								   savtyX_.subchg[xx-One][jbgusX_.jgus[xx-One]-One] == 858) ||
								   savtyX_.subchg[xx-One][jbgusX_.jgus[xx-One]-One] == 861) ){
								if( !((prm3 == 845 &&
									savtyX_.subchg[xx-One][jbgusX_.jgus[xx-One]-One] == 846) &&
									swX_.swork[xx-One][4*jbgusX_.jgus[xx-One]-One] == 13) )
									savtyX_.subchg[xx-One][jbgusX_.jgus[xx-One]-One] = prm3;
								}
							}
						else{

							swX_.swork[xx-One][typtr-One] = prm3;
							if( prm3 <= 16 )
								ofltagX_.ofl4r[xx-One][jbgusX_.jgus[xx-One]-One] = prm3;
							}
						}

L_840:
					if( prm4 != 0 ){
						if( prm4 > 0 ){

							swX_.swork[xx-One][frptr-One] = prm4;
							}
						else{

							xy = semwrk[(-79-prm4)-One][3];
							frptr2 = (4*jbgusX_.jgus[xy-One]) + 2;
							swX_.swork[xx-One][frptr-One] = swX_.swork[xy-One][frptr2-One];
							}
						}
					}
				else if( negnodX_.no3res == 9 ){

					ms1 = 0;
					twelve = 0;
					twent1 = 0;
					thirt1 = 0;
					sixty = 0;
					sixt1 = 0;

					for( ms=4; ms <= 12; ms += 4 ){
						ms1 += 1;
						wc = swX_.swork[commlX_.lstnod-One][ms-One];
						ty = swX_.swork[commlX_.lstnod-One][ms+1-One];

						if( wc != 0 ){
							if( negnodX_.negnod[ms1-One] != 0 ){

								if( wc != 2 && wc != 12 )
									goto L_900;
								if( ty != 60 && ty != 61 ){
									if( wc == 2 ){
										if( ty != 21 && ty != 31 )
											goto L_900;
										if( ty == 21 )
											twent1 = ms1;
										if( ty == 31 )
											thirt1 = ms1;

										}
									else if( wc == 12 ){
										twelve = ms1;
										}
									else{
										goto L_900;
										}
									}
								else if( sixty == 0 ){
									sixty = ms1;
									}
								else{
									sixt1 = ms1;

									}
								}
							}

						}
					/*                                                          *1510BT
					 *  DON'T ALTER SUBCHG IF IT ALREADY HAS A VALUE */
					if( !((savtyX_.subchg[commlX_.lstnod-One][ms1-One] == 852 ||
						   savtyX_.subchg[commlX_.lstnod-One][ms1-One] == 858) ||
						   savtyX_.subchg[commlX_.lstnod-One][ms1-One] == 861) ){
						if( sixty != 0 )
							savtyX_.subchg[commlX_.lstnod-One][sixty-One] = 847;
						if( sixt1 != 0 )
							savtyX_.subchg[commlX_.lstnod-One][sixt1-One] = 847;
						if( twent1 != 0 ){
							savtyX_.subchg[commlX_.lstnod-One][twent1-One] = 844;
							if( thirt1 == 0 && twelve == 0 )
								savtyX_.subchg[commlX_.lstnod-One][twent1-One] = 846;
							}

						if( thirt1 != 0 ){
							savtyX_.subchg[commlX_.lstnod-One][thirt1-One] = 845;
							if( twent1 == 0 && twelve == 0 )
								savtyX_.subchg[commlX_.lstnod-One][thirt1-One] = 847;
							}

						if( twelve != 0 ){
							if( twent1 == 0 && thirt1 == 0 )
								savtyX_.subchg[commlX_.lstnod-One][twelve-One] = 848;

							}
						}
					}

L_900:
				k3n = k3 + 5;
				}
			else{
				goto L_660;
				}
			}
		goto L_7800;

L_660:
		if( opswX_.sw[8-One] == 1 )
			{
			fprintf( _spec_fp, "\n *** ILLEGAL VTRS22 ELEMENT %6d\n", 
			  vtrn );
			}
		goto L_7800;

L_8000:
		x = x;
		if( opswX_.sw[8-One] == 1 )
			{
			fprintf( _spec_fp, "--ERROR IN RESSEM   ERR,LOCFLG,RETSW = %8ld%8ld%8d\n", 
			  errvrsX_.err, locflg, retsw );
			}
		*retflg = -1;
		}
L_7800:
	x = x;
	return;

} /*end of function*/

