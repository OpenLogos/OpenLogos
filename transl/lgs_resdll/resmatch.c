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

/*   THIS SUBROUTINE IS CALLED BY RES1AND2
 *    TO DETERMINE WHETHER THE RULE PASSED TO IT IS A
 *    VALID MATCH */

/*     RETURN  RETFLG  =  0   NOMATCH
 *                        1   MATCH
 *                       -1   ERROR */

/*  CHANGES:
 *                                                      *01/07/92*JAL*
 *      01/07/92*JAL* : FIX CHANGE MADE BY LG002GBA, R1722 BELOW.
 **     87/09/03 LG002GBA RNODE: INITIALIZE NEGNOD AND N65NOD
 **                                     BEFORE SETTING THEM
 **     87/03/26 LG002GBA RT005: READ IN OVERFLOW PORTION
 **                                      ONLY AFTER DETAILED MATCH
 **                                      OF MAIN PORTION.WANG EFFICIENCY
 **     87/03/18 LG002GBA R1722: ADD FORMS 11,12 TO WC-7 LOGIC
 *      02/11/87 *B0001GBA: ALLOW WC7 RULES WITH NEGATIVE WC IN
 *                              FINAL POSITION TO LEAVE NODES OPEN
 *      02/11/87 *R1637GBA: ALLOW MULITPLE 6300 - 6699 TAGSETS.
 *      02/03/87 *R1681GBA: HAVE WC -2 LEAVE NODES OPEN. */


#include "rescommon.h"



void   resmtc(retflg, k12_k12)
	short int *retflg;
	short k12_k12;
{
	struct {
	short int hshrul[31], hshtag[21][20];
	}	hshcomX_;
	static short int hshtct, hshtsv, indx, oflad, t63ind;
	static long int _n;
	static short zero = 0;
	static short jbnum[10]={1,4,7,0,3,6,9,12,15,18};
	static long tempi = 0;
	static short k = 0;
	static short m = 0;
	static short n = 0;
	static short x = 0;
	static short y = 0;
	static short z = 0;
	static short gb = 0;
	static short iz = 0;
	static short k2 = 0;
	static short k6 = 0;
	static short k9 = 0;
	static short ll = 0;
	static short rs = 0;
	static short uu = 0;
	static short wc = 0;
	static short jx2 = 0;
	static short gusm = 0;
	static short gusn = 0;
//cat-102
short gusn_gusn; // temp location to invert gusn before calling OVRIN

	static short gusx = 0;
	static short cebwc = 0;
	static short match = 0;
	static short negwc = 0;
	static short o1val = 0;
	static short o4val = 0;
	static short retsw = 0;
	static short temp2 = 0;
	static short typex = 0;
	static short locflg = 0;
	static short styval = 0;
	static short subval = 0;
	static short swcpd4 = 0;
	static short tagcon = 0;
	static short tagpos = 0;
	static short tagval = 0;
	static short tyd234 = 0;


	/*+                                                         *J0289MBS
	 *+                                                        *R1637*GBA
	 *  THESE VARIABLES NOW IN AN ARRAY TO ACCOMODATE MULTIPLE 6300S
	 *  INITIALIZED IN RES1AND2
	 *     TAG63 = 0
	 *     PRM63 = 0
	 *     TYP63 = 0
	 *+                                                         *TY0071
	 *     TYP65 = 0
	 * INITIALIZE INDEX TO TAG6300 ARRAYS TO 1 */
	t63ind = 1;

	/*   SECOND LOOP IN THE MATCHING PROCESS.
	 *    LOOPS THROUGH ATTEMPTING TO MATCH THE PRESENT RULE
	 *    AGAINST CONSECUTIVE SETS OF SWORKS.  BEFORE A RULE
	 *    IS CONSIDERED A FAILURE, ALL NODES ARE CHECKED. */

	oflad = resrulX_.rule[11-One];
	if( tag63X_.srch63 != 1 ){
		hshtct = 0;
		memset(&hshcomX_,'\0',sizeof(hshcomX_));
		hshcomX_.hshrul[0] = resrulX_.rule[0];
		}
	x = 1;
	if( tag63X_.srch63 == 1 )
		x = 2;
	for( k=x; k <= commlX_.li; k++ ){
		k9 = jbnum[k-One];
		if( k == 4 && oflad != 0 ){
			temp2 = resswX_.ressw;
			if( resswX_.minisw == 1 )
				temp2 = 4;
			errvrsX_.errlvl = OVRIN(&temp2,resrulX_.overfl, &k12_k12, &oflad);
			if( errvrsX_.errlvl != 0 )
			{
					errvrsX_.errlvl = 0;
					goto L_1680;
			}

			if( tag63X_.srch63 != 1 ){
				for( indx=1; indx <= 21; indx += 3 ){
					hshcomX_.hshrul[indx+10-One] = resrulX_.overfl[indx-One];
					hshcomX_.hshrul[indx+11-One] = resrulX_.overfl[indx+1-One];
					if( hshcomX_.hshrul[indx+11-One] <= -2 )
						hshcomX_.hshrul[indx+11-One] = -2;
					hshcomX_.hshrul[indx+12-One] = resrulX_.overfl[indx+2-One];
					}
			}
			
			wc = resrulX_.rule[1];
			if( (wc == 10 || wc == 11) || wc == 7 ){
				rulchk(resrulX_.overfl,&swX_.swork[ikX_.ik+2-One][0],
				  7,&retsw);
				}
			else{
				rulchk(resrulX_.overfl,&swX_.swork[ikX_.ik+3-One][0],
				  7,&retsw);
				}
			if( retsw == 2 )
				goto L_1680;
			}

		/*   THIRD OF THE MATCHING LOOPS.
		 *    TRIES TO MATCH THE PRESENT RULE ELEMENT AGAINST THE PRESENT
		 *    SWORK.  1-3 CORRESPOND TO WC TYPE AND FORM. */

		/*                                CEBWC FOR NEW VERB AND NOUN FORMS */

		while( TRUE ){
			cebwc = swX_.swork[mtcinfX_.iks-One][mtcinfX_.swcp-One];
			hshtsv = hshtct;
			negwc = 0;
			swcpd4 = mtcinfX_.swcp/4;
			o4val = ofltagX_.ofl4r[mtcinfX_.iks-One][swcpd4-One];
			o1val = ofltagX_.ofl1r[mtcinfX_.iks-One][swcpd4-One];
			styval = savtyX_.savtyr[mtcinfX_.iks-One][swcpd4-One];
			subval = savtyX_.subchg[mtcinfX_.iks-One][swcpd4-One];

			for( k2=1; k2 <= 3; k2++ ){

				gusx = swX_.swork[mtcinfX_.iks-One][mtcinfX_.swcp-One];
				k6 = k9 + k2;
				gusn = resrulX_.rule[k6-One];
				if( k > 3 )	gusn = resrulX_.overfl[k6-One];

				if( k2 == 1 && (gusn == -1 || gusn == -2) )
					negwc = 1;


				/*     BEGIN MATCHING SWORK AND RULE   PARAMETERS */
				if( gusn == -1 ){

					if( !(tag63X_.srch63 == 1 && srch07X_.srch07 == 0) ){

						/*   -1 WC IN THE LAST ELEMENT - LEAVE THE NODES OPEN */

						if( !(k2 != 1 || k != commlX_.li) ){
							for( y=swcpd4; y <= 3; y++ ){
								if( swX_.swork[mtcinfX_.iks-One][y-One] >= 
								  0 )
									negnodX_.negnod[y-One] = 1;
								}
							negnodX_.no3res = 1;
							}
						}

					}
				else if( gusx != gusn ){

					if( k2 == 1 ){

						if( gusn >= 0 )	goto L_1340;

						/*   NEGATIVE WC MATCHING */

						if( !((gusn >= (-9)) && (gusn <= (-2))) ) goto L_1680;


						/*   EG - SPECIAL TEST -7 WC FOR WC1 MATCHING
						 *+                                                         *R1003MBS
						 *+                                                         *R1166MBS */
						if( srcflgX_.srcflg == 2 ){
							if( gusn == -7 ){
								if( gusx == 1 ){
									gb = swX_.swork[mtcinfX_.iks-One][mtcinfX_.swcp+2-One];
									if( ((((gb == 3 || gb == 5) || 
									  gb == 15) || gb == 11) || gb == 
									  12) || gb == 17 )
										goto L_1180;
									/*+                                                        *R1722*GBA
									 *-                                                        *R1722*GBA
									 *                                                          *R1365JCB */
									if( gb == 33 && o1val == 228 )
										goto L_1180;
									/*                                                          *R1365JCB */
									goto L_1340;

									}
								else if( gusx == 19 ){
									if( swX_.swork[mtcinfX_.iks-One][mtcinfX_.swcp+1-One] == 
									  21 )
										goto L_1180;
									goto L_1340;
									}
								/*-                                                         *R1003MBS */

								/*   SPECIAL TEST FOR -8   ENGLISH SOURCE RES1 */
								}
							else if( gusn == -8 ){

								if( resswX_.ressw == 2 ){
									if( (gusx == 19 && swX_.swork[mtcinfX_.iks-One][mtcinfX_.swcp+1-One] == 
									  21) && swX_.swork[mtcinfX_.iks-One][mtcinfX_.swcp+2-One] == 
									  51 )
										goto L_1180;
									}
								else if( gusx == 19 ){
									rs = o1val;
									if( (rs == 821 || rs == 825) || 
									  rs == 250 )
										goto L_1180;
									goto L_1340;
									}
								}
							}

						if( resswX_.ressw != 2 ){
							match2(&gusx,resnegX_.nwcr1[-gusn-One],8,&retsw);
							if( retsw == 1 )goto L_1080;
							if( retsw == 2 )goto L_1340;
							}

						match2(&gusx,resnegX_.nwcr2[-gusn-One],8,&retsw);
						if( retsw == 2 )goto L_1340;

L_1080:
						if( srcflgX_.srcflg != 1 ){

							if( gusn == -5 ){
								if( gusx == 1 ){
									if( swX_.swork[mtcinfX_.iks-One][mtcinfX_.swcp+2-One] != 30 )
										goto L_1340;
									}
								}
							}


						/*   IS THIS THE LAST ELEMENT OF THE RULE??? */
L_1180:
						if( k == commlX_.li ){
							/*+                                                         *J0289MBS
							 *+                                                        *R0001*GBA
							 *     IF (SRCH63 .EQ. 1) GO TO 1300 */
							if( !(tag63X_.srch63 == 1 && srch07X_.srch07 != 1) ){
								if( gusn != -3 ){
									/*+                                                        *R1681*GBA */
									if( gusn != -2 ){
										/*-                                                        *R1681*GBA */
										if( !(gusn >= -9 && gusn <= 
										  -7) )
											goto L_1300;
										}
									}


								/*   IS THERE MORE THAN ONE NODE THAT MATCHES THIS NEGATIVE WC? */

								/*   NEGNOD(1,2,3) WILL INDICATE WHICH NODES MATCH */
								z = 1;
								/*+                                                        *RNODE*GBA */
								for( m=1; m <= 3; m++ ){
									/*       NEGNOD(M) = 0 */
									srch07X_.n65nod[m-One] = 0;
									}
								/*-                                                        *RNODE*GBA */
								for( m=4; m <= 12; m += 4 ){

									if( swX_.swork[mtcinfX_.iks-One][z-One] >= 
									  0 ){

										wc = swX_.swork[mtcinfX_.iks-One][m-One];

										if( wc != 0 ){
											/*   EG - SPECIAL TEST -7 WC FOR WC1 MATCHING
											 *+                                                         *R1003MBS */
											if( gusn != -7 || srcflgX_.srcflg != 
											  2 ){
												/*-                                                         *R1003MBS */

												/*   SPECIAL TEST FOR -8   ENGLISH SOURCE RES1
												 *+                                                         *R1166MBS */
												if( !(srcflgX_.srcflg != 2 || gusn != -8)){
												if( resswX_.ressw == 2 ){
												if( (wc == 19 && swX_.swork[mtcinfX_.iks-One][m+1-One] == 
												  21) && swX_.swork[mtcinfX_.iks-One][m+2-One] == 
												  51 )
												goto L_1215;
												}
												else if( wc == 19 ){
												rs = ofltagX_.ofl1r[mtcinfX_.iks-One][m/4-One];
												if( (rs == 821 || rs == 825) || rs == 250 )
													goto L_1215;
												goto L_1220;
												}
												}
												}
											else if( wc == 1 ){
												gb = swX_.swork[mtcinfX_.iks-One][m+2-One];
												if( ((((gb == 3 || gb == 5) || 
													    gb == 15) || gb == 11) || 
												  gb == 12) || gb == 17 ) goto L_1215;
												goto L_1220;
												}
											else if( wc == 19 ){
												if( swX_.swork[mtcinfX_.iks-One][m+1-One] == 21 )
												goto L_1215;
												goto L_1220;
												}

											/*-                                                         *R1166MBS */
											if( resswX_.ressw == 1 ){
												match2(&wc,&resnegX_.nwcr1[-gusn-One][0],8,&retsw);
												if( retsw == 1 )goto L_1215;
												if( retsw == 2 )goto L_1220;
												}
											if( resswX_.ressw == 2 ){
												match2(&wc,&resnegX_.nwcr2[-gusn-One][0],8,&retsw);
												if( retsw == 2 )goto L_1220;
												}

L_1215:
											if( srch07X_.srch07 == 0 ){
												negnodX_.negnod[z-One] = 1;
												}
											else{
												srch07X_.n65nod[z-One] = 1;
												}
											}
										}

L_1220:
									z += 1;
									}

								/*   MORE THAN ONE NODE MATCHES.
								 *   IF RULE IS -9 OR -3, LEAVE THESE NODES OPEN */

								if( srch07X_.srch07 == 0 ){
									negnodX_.no3res = -gusn;
									}
								else{
									srch07X_.n65res = -gusn;
									}
								}
							}
						}
					else if( k2 != 3 ){


						if( (gusn >= 1) && (gusn <= 16) ){
							x = o4val;
							y = 0;
							}
						else if( (gusn >= 17) && (gusn <= 99) ){
							x = styval;
							y = 0;
							}
						else if( gusn == 997 ){

							/*   GUSN IS FROM THE RULE.  GUSX IS FROM THE SWORK */

							/*        A MATCH FOR TYPE 997 IS:
							 *         WC -5 IN THE RULE, AND
							 *         WC3 SUPERSETS 10,11,13,14,15 OR SUPERSET 12, SET 20
							 *         FOR ENGLISH SOURCE WC19 SET 36 */
							if( k > 3 ){
								/*+                                                         *B0120MBS */
								if( resrulX_.overfl[k6-1-One] != -5 )
									goto L_1300;
								/*-                                                         *B0120MBS */
								}
							else if( resrulX_.rule[k6-1-One] != -5 ){
								goto L_1300;
								}

							gusx = o4val;
							if( swX_.swork[mtcinfX_.iks-One][mtcinfX_.swcp-1-One] != 
							  3 && savtyX_.savwc[mtcinfX_.iks-One][swcpd4-One] != 
							  3 ){

								if( srcflgX_.srcflg != 2 )goto L_1300;

								if( swX_.swork[mtcinfX_.iks-One][mtcinfX_.swcp-1-One] != 
								 19 && savtyX_.savwc[mtcinfX_.iks-One][swcpd4-One] != 19 )
									goto L_1300;
								gusx = styval;
								/*+                                                         *R1011MBS */
								if( (gusx == 21 || gusx == 32) || gusx == 36 )
									goto L_1300;
								/*-                                                         *R1011MBS */
								goto L_1340;
								}
							else if( (gusx < 10) || (gusx > 15) ){
								goto L_1340;
								}
							else{
								/*        ONLY TYPES 10 11 13 14 15 APPLY */
								iz = gusx - 9;
								if( (iz == 1 || iz == 2) || (iz >=  4 && iz <= 6) )
									goto L_1300;
								if( styval == 20 )
									goto L_1300;
								goto L_1340;
								}
							}
						else if( gusn == 998 ){

							/*        998 TYPE TEST */

							if( (swX_.swork[mtcinfX_.iks-One][mtcinfX_.swcp-1-One] == 
							  12) && (gusx != 8) )
								goto L_1340;
							goto L_1300;
							}
						else if( gusn >= 100 ){
							x = o1val;
							y = subval;
							}
						else{

							/*  NEGATIVE TYPE MATCHING */

							/*   MOVE THE CORRECT TAGSET INTO TAGFLW (RES1, RES2 OR RES2X) */

							while( TRUE ){

								if( resswX_.ressw == 2 )
									temp2 = 2;
								if( resswX_.minisw == 1 )
									temp2 = 4;
								if( tag63X_.srch63 == 1 )
									temp2 = 2;
								if( resswX_.ressw == 1 )
									temp2 = 1;

								gusn_gusn = - gusn;
								errvrsX_.errlvl = OVRIN(&temp2,tagflwX_.tagflw, &k12_k12, &gusn_gusn);
								if( errvrsX_.errlvl != 0 ) goto L_1340;

								if( tag63X_.srch63 != 1 ){
									hshtct += 1;
									lmove(&hshcomX_.hshtag[hshtct-One][0],1,tagflwX_.tagflw,1,40);
									}


								tagpos = 1;
								tagval = tagflwX_.tagflw[tagpos-One];

								tagcon = 0;
								if( tagval == 8888 ){
									tagcon = 1;
									tagpos = 2;
									tagval = tagflwX_.tagflw[tagpos-One];
									}


								if( tagval == 1000 || tagval == 2000 )
								{
									//                       ONLY WC 1 2 3 4 6 CAN HAVE HENUM VALUES  
									
									{
										if( hensavX_.hennum[mtcinfX_.iks-One][0] != -1 ){
											match4(&hensavX_.hennum[mtcinfX_.iks-One][0], &tagflwX_.tagflw[1],11,&retsw);
											if( retsw == 1 ) goto L_1300;
											// check root
											if( hensavX_.root_hennum[mtcinfX_.iks-One][0] != -1 ){
												match4(&hensavX_.root_hennum[mtcinfX_.iks-One][0], &tagflwX_.tagflw[1],11,&retsw);
												if( retsw == 1 ) goto L_1300;
												}
											}
									}
									// if hash code test
									if( tagval == 1000 )
									{
										if( hashX_.hashcd[mtcinfX_.iks-One][0] != 0 ){
											match4(&hashX_.hashcd[mtcinfX_.iks-One][0], &tagflwX_.tagflw[1],11,&retsw);
											if( retsw == 1 ) goto L_1300;
											// check root
											if( hashX_.root_hashcd[mtcinfX_.iks-One][0] != 0 ){
												match4(&hashX_.root_hashcd[mtcinfX_.iks-One][0], &tagflwX_.tagflw[1],11,&retsw);
												if( retsw == 1 ) goto L_1300;
											}
										}
									}

									goto L_1340;		// here if no match
								}




								/*   IS THIS A 6000 SERIES TAGSET? */
								while( !(tagval < 6000 || tagval >  7000) ){

									if( tagval >= 6300 && tagval <= 6699 ){
										if( tagval >= 6500 ){
											tag63X_.tag63[t63ind-One] = tagval;
											tag63X_.prm63a[t63ind-One] = tagflwX_.tagflw[tagpos+1-One];
											tag63X_.prm63b[t63ind-One] = tagflwX_.tagflw[tagpos+2-One];
											tag63X_.prm63c[t63ind-One] = tagflwX_.tagflw[tagpos+3-One];
											tagpos += 4;
											}
										else{
											tag63X_.tag63[t63ind-One] = tagval;
											tag63X_.prm63a[t63ind-One] = tagflwX_.tagflw[tagpos+1-One];
											tag63X_.prm63b[t63ind-One] = tagflwX_.tagflw[tagpos+2-One];
											tag63X_.prm63c[t63ind-One] = tagflwX_.tagflw[tagpos+3-One];
											tagpos += 3;
											}
										t63ind += 1;
										}
									else{
										/*   CALL 6000 SERIES TAG SUBROUTINE
										 *             RETSW = 1 IF THE 6000 TAG IS A MATCH
										 *             TAGPOS IS THE NEXT POSITION ON THE TAG AFTER THE 6000 TAG */
										rs6000(&retsw,&tagpos);
										if( retsw == 0 ) goto L_1340;
										}


									/*   ANY MORE TAGSET VALUES?  IF NOT, THIS IS A MATCH. */
									tagval = tagflwX_.tagflw[tagpos-One];
									if( tagval == 0 )goto L_11903;
									}

								/*    OTHER TAGSET MATCHING CAN BE EXECUTED ALONE OR AFTER THE
								 *    6000 TAGSET HAS BEEN SATISFIED */

								/*   IS THE FIRST TAG AN EXCLUSION VALUE? */
								while( TRUE ){
									if( tagval/1000 != 1 ){

										for( m=tagpos; m <= 22; m++ ){
											typex = tagflwX_.tagflw[m-One];
											if( typex == 0 )
												goto L_11904;
											/*                                EXCLUSION TAG? */
											if( typex/1000 != 1 ){
												if( typex == gusx )
												goto L_1005;
												if( typex == o4val )
												goto L_1005;
												if( typex == o1val )
												goto L_1005;
												if( typex == styval )
												goto L_1005;
												if( typex == subval )
												goto L_1005;
												}
											}
										goto L_1340;

										/*   SPECIAL TEST TO SEE IF EXCLUSION TAG FOLLOWS 'NORMAL' MATCH
										 *   IF A 'NORMAL' TAG FOLLOWS, RULE IS A MATCH */
L_1005:
										tagpos = m + 1;
										if( tagflwX_.tagflw[tagpos-One]/1000 != 1 )
											goto L_1300;
										}


									/*   EXCLUSION TAGS MUST BE SATISFIED BEFORE DECLARING MATCH.
									 *   LOOP THROUGH TAGS TO FIND NUMBER OF EXCLUSION TAGS TO TEST. */
									for( ll=tagpos; ll <= 22; ll++ ){
										if( tagflwX_.tagflw[ll-One]/1000 != 1 )
											break;
										}

									/*   LOOP THROUGH EXCLUSION TAG VALUES */
									ll -= 1;

									for( uu=tagpos; uu <= ll; uu++ ){
										tyd234 = tagflwX_.tagflw[uu-One] - 1000;
										if( tyd234 == gusx )
											goto L_1018;
										if( tyd234 == o4val )
											goto L_1018;
										if( tyd234 == o1val )
											goto L_1018;
										if( tyd234 == styval )
											goto L_1018;
										if( tyd234 == subval )
											goto L_1018;

										/*   ALL EXCLUSION VALUES SATISFIED.  TAG VALUE IS A MATCH */
										}
									goto L_1300;


									/*   EXCLUSION VALUES NOT SATISFIED.  RESET TAGPOS AND GO TO TOP OF TAG */
L_1018:
									tagpos = ll + 1;
									tagval = tagflwX_.tagflw[tagpos-One];
									}
L_11904:
								if( tagcon == 0 )
									goto L_1340;
								gusn -= 1;
								continue;
L_11903:
								if( tagcon == 0 )
									goto L_1300;
								gusn -= 1;
								}
							}
						gusx = x;
						if( gusx != gusn ){
							if( y == 0 )
								goto L_1340;
							if( gusn != y )
								goto L_1340;
							}

						/*            SUPERFORM TESTS */

						}
					else if( gusn < 20 ){
						goto L_1340;
						}
					else{
						gusm = gusn - 19;

						resformod(2,cebwc,gusm,&gusx,&match,&retsw);
						if( errvrsX_.errlvl != 0 )
							goto L_8000;
						if( match != 1 )
							goto L_1340;
						}
					}

L_1300:
				mtcinfX_.swcp += 1;

				}

			if( !(tag63X_.srch63 == 1 && srch07X_.srch07 == 0) ){
				swcpd4 = mtcinfX_.swcp/4;
				jbgusX_.jgus[mtcinfX_.iks-One] = swcpd4;
				/*               FOR RES1 - -1 AND -2 SHOULD LEAVE NODES OPEN */
				if( resswX_.ressw == 1 && negwc == 1 )
					jbgusX_.jgus[mtcinfX_.iks-One] = -1;

				}


			jgus63X_.jgus63[mtcinfX_.iks-One] = mtcinfX_.swcp/4;
			mtcinfX_.swcp = 0;

			mtcinfX_.iks += 1;

			if( tag63X_.srch63 != 0 )
				break;
			if( (ikX_.ik + commlX_.li) != mtcinfX_.iks )
				goto L_1480;


			/*   A POSSIBLE MATCH
			 *    IF THIS RULE CONTAINS A -22 SWITCH, A MATCH MUST BE FOUND
			 *    IN RES22 BEFORE CONSIDERING THIS RULE A MATCH.
			 *    THIS LOGIC IS NOW IN RSVTR */

			commlX_.lstnod = ikX_.ik + commlX_.li - 1;


			if( resswX_.ressw == 1 )
				tempi = 1;
			if( resswX_.ressw == 2 )
				tempi = 2;
			if( resswX_.minisw == 1 )
				tempi = 4;

			ressws(tempi,&retsw);
			if( errvrsX_.errlvl != 0 )
				goto L_8000;
			if( retsw == 1 )
				goto L_1540;


			ressem(1,sw22ptX_.sw22pt,vtrfX_.vtrf,ikX_.ik,commlX_.li,
			       &sw22chX_.sw22ch,&retsw);
			if( errvrsX_.errlvl != 0 )
				goto L_8000;
			if( retsw != 1 )
				goto L_1540;


			/*   ON A BAD RETURN FROM SEMRES (NO MATCH),
			 *   GO BACK UP TO CONTINUE SEARCH. */

			mtcinfX_.iks -= 1;

			/*   RETURN TO HERE INDICATES NO MATCH ON THE PRESENT RULE.
			 *   BEFORE GIVING UP ON THIS RULE, CHECK FOR ANOTHER OPEN
			 *   NODE IN THE PRESENT ELEMENT. */

L_1340:
			if( swX_.scont3[mtcinfX_.iks-One] == 1 )
				goto L_1680;
			if( k == 1 )
				goto L_1680;
			jx2 = mtcinfX_.jx + 1;
			if( jx2 > 3 )
				goto L_1680;
			mtcinfX_.swcp = 4*mtcinfX_.jx;
			mtcinfX_.jx += 1;
			hshtct = hshtsv;

			for( n=jx2; n <= 3; n++ ){
				mtcinfX_.swcp += 4;
				if( swX_.swork[mtcinfX_.iks-One][n-One] > 0 )
					goto L_11905;
				}
			goto L_1680;
L_11905:
			;
			}
		if( (ikX_.ik + commlX_.li - 1) == mtcinfX_.iks )
			break;


L_1480:
		;
		for( mtcinfX_.jx=1; mtcinfX_.jx <= 3; mtcinfX_.jx++ ){
			mtcinfX_.swcp += 4;
			if( swX_.swork[mtcinfX_.iks-One][mtcinfX_.jx-One] > 
			  0 )
				goto L_11906;
			}
		goto L_1680;


L_11906:
		;
		}

	/*   FALL THROUGH THIS LOOP IS A MATCH */
L_1540:
	*retflg = 0;
	return;



L_1680:
	*retflg = 1;
	return;

L_8000:
	if(opswX_.sw[8-One] == 1 )
		{
		fprintf( _spec_fp, "--ERROR IN RESMATCH ERR,LOCFLG,RETSW = %8ld%8d%8d\n", 
		  errvrsX_.err, locflg, retsw );
		}
	*retflg = -1;
	return;

} /*end of function*/

