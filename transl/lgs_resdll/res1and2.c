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
	/** LAST CHANGE 87/04/24 LG002GBA RGA07: FOR MINI, READ IN OVERFLOW
	 **                                      TO CHECK OVERRIDE
	 **      CHANGE 87/03/26 LG002GBA RT005: REMOVE READ OF OVERFLOW --
	 **                                      EFFICIENCY FOR WANG --
	 **                                      READ IN AFTER DETAILED MATCH
	 **                                        OF MAIN PORTION
	 ** LAST CHANGE 87/03/17 LG002GBA R0954: ADD LOOP CONTROL
	 *      CHG 02/19/87 *RFFFFGBA: ALLOW SKIP OF WC LVL WC BLOCKS
	 *      CHG 02/09/87 *R1637GBA: PERMIT MULTIPLE RES6300 AND RES6500
	 *                              ALSO ADD SEINIT ARRAY TO REINITIALIZE
	 *                              THE JGUS ARRAY FOR EACH MATCH IN SE */
	/*      LOCFLG =     NOT USED 05/13/85
	 *      RETFLG =     0  EVERYTHING OK
	 *                  -1  ERROR */
	/*CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	 * ***  RES1 ***
	 * ***  RES2 ***
	 *     THIS PROGRAM IS CALLED FROM RESMAIN.
	 *      THE FIRST TIME RESSW = 1 THIS IS THE RES1 PASS
	 *      THE SECOND TIME RESSW = 2 THIS IS THE RES2 PASS
	 *      LOADED IN ALL TRANSLATION EXECS, AND TESTRES EXEC */
	/*      THE MAIN PURPOSE OF THIS PROGRAM IS TO ESTABLISH
	 *      A PATH THROUGH THE SWORKS OF A SENTENCE BY
	 *      CHOOSING THE CORRECT PART OF SPEECH FOR A WORD
	 *      THAT IS ENTERED IN THE DICTIONARY AS MORE THAN ONE
	 *      PART OF SPEECH.
	 *      THIS PATH IS ESTABLISHED BY MATCHING SWORKS AGAINST
	 *      RES2 RULES.  ONCE A MATCH IS MADE IN RES THE ELEMENTS
	 *      INVOLVED ARE RESOLVED,I.E.,  THE PART OF SPEECH THAT
	 *      THE RES RULE MATCHED ON IS CONSIDERED TO BE THE
	 *      CORRECT PART OF SPEECH FOR THIS PARTICULAR SENTENCE. */
	/*      THE DIFFERENCE BETWEEN THE RES1 PASS AND THE RES2 PASS
	 *      IS THAT THE SWORKS ARE SET BACK TO THE ORIGINAL AFTER RES1
	 *      UNLESS THEY ARE ACTED ON BY A SPECIAL SWITCH, THE -13 SWITCH. */
	/*      VARIABLE DEFINITIONS:
	 *       DISSP1, DISSP2 - NUMBER OF FIRST RULE FOR EACH WORD CLASS
	 *       ECOUNT - NUMBER OF WORDS IN PRESENT SENTENCE
	 *       JGUS - 1, 2 OR 3 - NUMBER OF SWORK RESOLVED TO
	 *       NWCR2 - NEGATIVE WORD CLASS DATA
	 *       NENT1, NENT2 - NUMBER OF RULES FOR EACH WORD CLASS
	 *CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC */

#include "rescommon.h"


void reslv2(locflg, retflg)
long int locflg;
short int *retflg;
{
	static short int gba, loopct, loopnd, looprl, loopse, seinit[70], 
	  wc2, wc2ind;
	
	static char pgmnam[9] = "RES1AND2";
	static short zero = 0;
	static long tempi = 0;
	static short i = 0;
	static short m = 0;
	static short n = 0;
	static short x = 0;
	static short z = 0;
	static short gb = 0;
	static short ii = 0;
	static short iz = 0;
	static short i2 = 0;
	static short kk = 0;
	static short l2 = 0;
	static short mm = 0;
	static short ms = 0;
	static short m1 = 0;
	static short n1 = 0;
	static short vs = 0;
	static short wc = 0;
	static short iis = 0;
	static short ms1 = 0;
	static short sub = 0;
	static short done = 0;
	static short iist = 0;
	static short nwc2 = 0;
	static short gbptr = 0;
	static short kksav = 0;
	static short oflad = 0;
	static short retsw = 0;
	static short rsnum = 0;
	static short swcps = 0;
	static short displm = 0;
	static short origli = 0;
	static short specif = 0;
	static short specix = 0;



	*retflg = 0;
	done = 0;
	commiX_.i4 = 1;
	miniX_.iksave = 0;
	sw22chX_.sw22ch = 0;
	negmtcX_.no2res = 0;
	miniX_.lvlgus = 1;
	iist = 1;
	specif = -1;
	miniX_.k12s = 0;

	memset(chgsemX_.semchg,'\0',sizeof(chgsemX_.semchg));

	/*           SET JS TO FIRST NON-ZERO SWORK AND INITIALIZE JGUS. */
	for( l2=1; l2 <= swX_.ecount; l2++ ){
		jbgusX_.jgus[l2-One] = 0;
		swX_.scont3[l2-One] = swX_.scont2[l2-One];
		}
	jbgusX_.jgus[swX_.ecount-One] = 1;

	loopse = 0;


	/*               INITIALIZE OVERRIDE INFO */
	while( TRUE ){
		memset(overidX_.rulovr,'\0',sizeof(overidX_.rulovr));
		memset(overidX_.ovrovr,'\0',sizeof(overidX_.ovrovr));
		/*+ REMEMBER THE INITIAL STATE OF JGUS FOR THIS SE         *R1637*GBA */
		lmove(seinit,1,jbgusX_.jgus,1,140);
		if( commiX_.i4 > miniX_.i4sav )
			zapit(nloopX_.nloop,24,(byte)zero);

		/*   CHECK FOR A RES2X MATCH FOR THIS STARTING ELEMENT */
		valxX_.i4x = commiX_.i4;
		resswX_.minisw = 1;
		if( resswX_.ressw == 2 ){
			res2x(1,&retsw);
			if( errvrsX_.errlvl != 0 )
				goto L_8000;
			}
		resswX_.minisw = 0;

		while( TRUE ){
			ikX_.ik = commiX_.i4;
			if( ikX_.ik >= swX_.ecount )
				goto L_2220;

			if( ikX_.ik > loopse ){
				loopct = 0;
				loopnd = 0;
				looprl = 0;
				}

			mtcinfX_.iks = ikX_.ik;


			/*     TEST FOR POSITIVE VALUES IN FIRST 3 POSITIONS OF EACH SWORK */

			for( ii=iist; ii <= 3; ii++ ){
				iis = ii;
				mtcinfX_.swcp = 4*ii;
				swcps = mtcinfX_.swcp;
				if( swX_.swork[mtcinfX_.iks-One][ii-One] > 0 )
					goto L_680;
				}
			goto L_1740;

L_680:
			wc = swX_.swork[mtcinfX_.iks-One][mtcinfX_.swcp-One];

			if( resswX_.ressw == 1 ){
				nwc2 = NENT1(&wc);
				displm = DISSP1(&wc) + nwc2;
			}
			else if( resswX_.ressw == 2 ){
				nwc2 = NENT2(&wc);
				displm = DISSP2(&wc) + nwc2;
			}

			kksav = 1;
			if( nwc2 > 0 ){
				if( kksav <= nwc2 ){


					/*   FIRST OF 3 LOOPS IN THE MATCHING PROCESS.
					 *    LOOPS THROUGH ALL THE RULES IN THE PRESENT WORD
					 *    CLASS, LOOKING FOR THE MOST SPECIFIC MATCH. */

					kk = kksav;
					while( TRUE ){
						mtcinfX_.jx = 1;
						commkX_.k12 = displm - kk;
						/*+ RECALL JGUS'S INITIAL STATE FOR THIS SENTENCE ELEMENT  *R1637*GBA */
						lmove(jbgusX_.jgus,1,seinit,1,140);

						for( mm=1; mm <= 6; mm++ ){
							/*+         FIRST ZERO NLOOP MEANS THE REST ARE ZERO       *RFFFF*GBA */
							if( nloopX_.nloop[mm-One] == 0 )
								break;
							if( commkX_.k12 == nloopX_.nloop[mm-One] )
								goto L_1700;
							}

//			errvrsX_.errlvl = RULEIN(&resswX_.ressw, resrulX_.rule, &commkX_.k12);
			resrulX_.rule = RULEIN_OCR(resswX_.ressw, commkX_.k12);
			errvrsX_.errlvl = resrulX_.rule ? 0:1;
						if( errvrsX_.errlvl == 0 ){
/* for debugging only 
							if(commkX_.k12 == 2003){
								x = x;								
							}
*/
							oflad = resrulX_.rule[10];
							specix = resrulX_.rule[11];

							/*          RES IS ALLOWED A LEVEL OF UP TO 40 (ACTUAL LEVEL+10,20..) */
							commlX_.li = resrulX_.rule[0];
							origli = commlX_.li;
							if( commlX_.li > 10 ){
								sub = ((commlX_.li - 1)/10)*10;
								commlX_.li -= sub;
								}

							if( commlX_.li <= (swX_.ecount - ikX_.ik + 1) ){

								/*         IF LEVEL IS LESS, WE ARE DONE TRYING TO MATCH ON THIS NODE */
								if( origli < miniX_.lvlgus )
									break;

								wc2 = resrulX_.rule[4];
								wc2ind = ikX_.ik + 1;
								if( wc2 > 0 ){
									/*       THE IF CONDITION IS STRUCTURED THIS WAY FOR EFFICIENCY. */
									if( !((wc2 == swX_.swork[wc2ind-One][3]  || 
									       wc2 == swX_.swork[wc2ind-One][7]) || 
									       wc2 == swX_.swork[wc2ind-One][11]) ){
										if( resswX_.ressw == 1 )
											gba = R1RENT(&wc2, &origli, &wc);
										if( resswX_.ressw == 2 )
											gba = R2RENT(&wc2, &origli, &wc);
										if( gba > 1 )
											kk += gba - 1;
										goto L_1700;
										}
									}

								/*                    QUICK CHECK CAN IT MATCH ? */
								rulchk(&resrulX_.rule[2-One],&swX_.swork[ikX_.ik-One][1-One],
								       3,&retsw);
											// if it can not match ...
								if( retsw == 2 ){
									goto L_1700;
								}

									/*   IF THE LEVEL OF THIS RULE IS GREATER THAN A PREVIOUS MATCH,
									 *   IGNORE THE SPECIFICITY TEST */
									if( origli <= miniX_.lvlgus ){

										/*   IF A PREVIOUS RULE MATCHED ON IS MORE SPECIFIC THAN THIS
										 *   ONE, SKIP THIS ONE. */
										if( specif >= specix )
											goto L_1700;
										}


									mtcinfX_.iks = ikX_.ik;
									negnodX_.no3res = 0;

									zapit(negnodX_.negnod,6,(byte)0);

									if( resswX_.ressw == 2 ){

										/*   DOES THIS RULE MATCH AN OVERRIDE?  IF YES, SKIP IT.
										 *   LOOP THROUGH THE RULES, COMPARING FIELDS.  THEY MUST BE EQUAL.
										 *   ONE EXCEPTION: 2 NEGATIVE TYPE FIELDS (TAGSETS) ARE CONSIDERED EQUAL
										 *+
										 *   NEW: UP TO 10 OVERRIDE RULES MAY BE STORED PER WORD CLASS.  LOOP */

										gbptr = ovptrX_.ovptr + 1;

										for( gb=1; gb <= ovptrX_.ovptr; gb++ ){
											gbptr -= 1;

											ms1 = 0;
											for( ms=1; ms <= 10; ms++ ){
												ms1 += 1;
												if( ms1 == 3 )
												ms1 = 0;

												if( resrulX_.rule[ms-One] != 
												  overidX_.rulovr[gbptr-One][ms-One] ){
												if( ms1 != 0 )
												goto L_8101;
												if( resrulX_.rule[ms-One] >= -1 ||
													overidX_.rulovr[gbptr-One][ms-One] >= -1 )
												goto L_8101;
												}

												}

											if( oflad == 0 )goto L_1700;

						errvrsX_.errlvl = OVRIN(&resswX_.ressw, resrulX_.overfl, &commkX_.k12, &oflad);
											if( errvrsX_.errlvl != 0 )goto L_8102;
											ms1 = 1;
											for( ms=1; ms <= 21; ms++ ){
												ms1 += 1;
												if( ms1 == 3 )
												ms1 = 0;

												if( resrulX_.overfl[ms-One] != 
												    overidX_.ovrovr[gbptr-One][ms-One] ){
												if( ms1 != 0 )
												    goto L_8101;
												if( resrulX_.overfl[ms-One] >= -1 || 
													overidX_.ovrovr[gbptr-One][ms-One] >= -1 )
												    goto L_8101;
												}

												}
											goto L_1700;

L_8101:
											;
											}
										goto L_900;
L_8102:
										errvrsX_.errlvl = 0;
										goto L_1700;
										}
L_900:
									memset(tag63X_.tag63,'\0',sizeof(tag63X_.tag63));
									memset(tag63X_.prm63a,'\0',sizeof(tag63X_.prm63a));
									memset(tag63X_.prm63b,'\0',sizeof(tag63X_.prm63b));
									memset(tag63X_.prm63c,'\0',sizeof(tag63X_.prm63c));

									resmtc(&retsw, commkX_.k12);
									if( retsw != 1 ){

										//  IF THERE IS AT LEAST ONE TAG6300 IN THIS SEARCH
										if( tag63X_.tag63[1-One] != 0 ){
										
											rsdiag(2,&resswX_.ressw,&commiX_.i4,&commkX_.k12);

											tag63X_.srch63 = 1;
											rs6300(&retsw);
											tag63X_.srch63 = 0;
											srch07X_.srch07 = 0;
											if( retsw == 0 ) goto L_1680;
											}

										/*   THIS IS REALLY A MATCH 
										 *   IF NO3RES WAS SET FOR THIS MATCH - LEAVE NEGNOD FLAGS ON
										 *   OTHERWISE, TURN THEM OFF */

										if( ikX_.ik == loopse ){
											/*-                                                        *R0GBA*GBA */
											loopct += 1;
											/* GBA   IF (LOOPCT .GT. 20 .OR. LOOPRL .EQ. K12) THEN */
											if( loopct > 40 )
												goto L_8103;
											}
										looprl = commkX_.k12;
										loopse = ikX_.ik;
										loopnd = ii;

										if( negnodX_.no3res == 0 ){
											zapit(negmtcX_.negmtc,6,(byte)0);
											negmtcX_.no2res = 0;
											}
										else{

											negmtcX_.no2res = negnodX_.no3res;
											lmove(negmtcX_.negmtc,1,negnodX_.negnod,1,6);
											zapit(negnodX_.negnod,6,(byte)0);
											}

										/*   SAVE THIS VTR (EXCEPT FOR THE -46 SWITCHES PRECEDING THE -22
										 *   IN VTRFS */

										iz = sw22ptX_.sw22pt;
										if( sw22ptX_.sw22pt == 0 ) iz = 1;
										memset(vtrfsX_.vtrfs,'\0',sizeof(vtrfsX_.vtrfs));
										for( m=1; m <= 100; m++ ){
											vtrfsX_.vtrfs[m-One] = vtrfX_.vtrf[iz-One];
											if( vtrfsX_.vtrfs[m-One] == 999 )
												break;
											iz += 1;
											}


										/*                                   CALL TO WRITE DIAGNOSTICS */
										rsdiag(1,&resswX_.ressw,&commiX_.i4,&commkX_.k12);

										miniX_.k12s = commkX_.k12;
										miniX_.i4pli = commiX_.i4 + commlX_.li - 1;
										miniX_.i4sav = commiX_.i4;
										for( iz=commiX_.i4; iz <= miniX_.i4pli; iz++ ){
											miniX_.jgusav[iz-One] = jbgusX_.jgus[iz-One];
											}
										miniX_.iksave = mtcinfX_.iks - 1;

										/*   KEEP LOOKING FOR A MORE SPECIFIC RULE. */
										miniX_.lvlgus = origli;
										specif = specix;
										}

L_1680:
									mtcinfX_.swcp = swcps;
									}
								}


L_1700:
						kk += 1;
						if( kk > nwc2 )
							goto L_1720;
						}


					if( iis >= 3 )
						goto L_1740;
					iist = iis + 1;
					commiX_.i4 = ikX_.ik;
					continue;
					}
				}

L_1720:
			if( iis >= 3 )
				goto L_1740;
			iist = iis + 1;
			commiX_.i4 = ikX_.ik;
			}
L_8103:
		if( opswX_.sw[8-One] == 1 )
			{
			fprintf( _spec_fp, "* LOOPING DETECTED -- SKIPPING TO NEXT SENTENCE ELEMENT\n" );
			}
		commiX_.i4 += 1;
		iist = 1;
		continue;



		/*   DOES A MINI RULE PREVAIL ??? */
L_1740:
		if( resswX_.ressw == 2 ){
			res2x(2,&retsw);
			if( errvrsX_.errlvl != 0 )
				goto L_8000;
			}

		if( miniX_.k12s == 0 ){


			/*   NO MATCH THIS ELEMENT SET TO FIRST WC */

			if( resswX_.ressw != 1 ){

				swX_.scont3[commiX_.i4-One] = 1;
				for( i=1; i <= 2; i++ ){
					if( swX_.swork[commiX_.i4-One][i-One] >= 1 )
						goto L_2120;
					}
				goto L_2180;

L_2120:
				i += 1;

				for( i2=i; i2 <= 3; i2++ ){
					swX_.swork[commiX_.i4-One][i2-One] = -1;
					}
				}

L_2180:
			iist = 1;



			commiX_.i4 += 1;
			}
		else{
			commkX_.k12 = miniX_.k12s;

			for( i=miniX_.i4sav; i <= miniX_.i4pli; i++ ){
				jbgusX_.jgus[i-One] = miniX_.jgusav[i-One];
				}

			commvX_.vn = 1;
			commiX_.i4 = miniX_.iksave;

			while( vtrfsX_.vtrfs[commvX_.vn-One] != 999 ){
				/*+                                                         *R1050MBS
				 *          -26 IS A 2 PARAMETER CHECKER-ONLY SWITCH */
				if( vtrfsX_.vtrfs[commvX_.vn-One] == -26 ){
					commvX_.vn += 3;
					}
				else{
					/*-                                                         *R1050MBS */
					vs = -(vtrfsX_.vtrfs[commvX_.vn-One]) - 10;
					if( vs == 3 ){

						rsnum = 13;
						}
					else if( vs == 7 ){
						rsnum = 17;
						}
					else if( vs == 8 ){
						rsnum = 18;
						}
					else if( vs == 11 ){
						rsnum = 21;
						}
					else if( vs == 12 ){
						rsnum = 22;
						}
					else if( vs == 18 ){
						rsnum = 28;
						}
					else if( vs == 20 ){
						rsnum = 30;
						}
					else if( vs == 21 ){
						rsnum = 31;
						}
					else if( vs == 24 ){
						rsnum = 34;
						}
					else if( vs == 26 ){
						rsnum = 36;
						}
					else if( vs == 31 ){
						rsnum = 41;
						}
					else if( vs == 32 ){
						rsnum = 42;
						}
					else if( vs == 35 ){
						rsnum = 45;
						}
					else if( vs == 36 ){
						rsnum = 46;
						}
					else{
						goto L_1800;
						}

					tempi = rsnum;
					ressws(tempi,&retsw);
					if( errvrsX_.errlvl != 0 )
						goto L_8000;
					}
				}
			goto L_1940;

L_1800:
			if(opswX_.sw[8-One] == 1 ) errlog(pgmnam,1800,0,9);


			/*   VTR PROCESSING IS COMPLETE. */


L_1940:
			if( miniX_.iksave > swX_.ecount ) done = 1;
			resswX_.minisw = 0;

			if( opswX_.sw[8-One] == 1 && resswX_.ressw == 2 )
				{
				fprintf( _spec_fp, "\n\nCELL VALUES  " );
				for( x=1; x <= 10; x++ )fprintf( _spec_fp, "%4d", cellX_.cell[x-One] );
				fprintf( _spec_fp, "      " );
				for( x=11; x <= 20; x++ )fprintf( _spec_fp, "%4d", cellX_.cell[x-One] );
				fprintf( _spec_fp, "\n             " );
				for( x=21; x <= 30; x++ )fprintf( _spec_fp, "%4d", cellX_.cell[x-One] );
				fprintf( _spec_fp, "      " );
				for( x=31; x <= 40; x++ )fprintf( _spec_fp, "%4d", cellX_.cell[x-One] );
				fprintf( _spec_fp, "\n                                 " );
				for( x=41; x <= 50; x++ )
					fprintf( _spec_fp, "%4d", cellX_.cell[x-One] );
				fprintf( _spec_fp, "\n             " );
				for( x=51; x <= 60; x++ )fprintf( _spec_fp, "%4d", cellX_.cell[x-One] );
				fprintf( _spec_fp, "      " );
				for( x=61; x <= 70; x++ )fprintf( _spec_fp, "%4d", cellX_.cell[x-One] );
				fprintf( _spec_fp, "\n             " );
				for( x=71; x <= 80; x++ )fprintf( _spec_fp, "%4d", cellX_.cell[x-One] );
				fprintf( _spec_fp, "      " );
				for( x=81; x <= 90; x++ )fprintf( _spec_fp, "%4d", cellX_.cell[x-One] );
				fprintf( _spec_fp, "\n                                 " );
				for( x=91; x <= 100; x++ )
					fprintf( _spec_fp, "%4d", cellX_.cell[x-One] );
				fprintf(_spec_fp, "\n");
				}
			miniX_.lvlgus = 1;
			iist = 1;


			i2 = miniX_.i4pli - 1;

			for( iz=miniX_.i4sav; iz <= i2; iz++ ){
				/*                                                          *J0195 */
				if( jbgusX_.jgus[iz-One] != -1 ){
					/*                                                          *J0195
					 *+                                                         *R1078MBS
					 *         IF RES1 AND ELEMENT IS BACKSPACED ON AND NOT LOCKED
					 *                 OR  SCONT2 HAS BEEN SET TO 5 IN A PREVIOUS LOCK
					 *         LEAVE THE SWORK OPEN */
					if( resswX_.ressw == 1 ){
						if( swX_.scont2[iz-One] == 5 )
							continue;
						if( iz >= commiX_.i4 && swX_.scont2[iz-One] < 4 )
							continue;
						}
					/*-                                                         *R1078MBS */
					swX_.scont3[iz-One] = 1;

					for( i=1; i <= 3; i++ ){
						if( i != jbgusX_.jgus[iz-One] )
							swX_.swork[iz-One][i-One] = -1;
						}
					}
				}

			/*+                                                         *R1078MBS
			 *         IF RES1 AND ELEMENT IS BACKSPACED ON AND NOT LOCKED
			 *         LEAVE IT OPEN */
			if( !((resswX_.ressw == 1 && miniX_.i4pli >= commiX_.i4) && 
			  swX_.scont2[miniX_.i4pli-One] < 4) ){
				/*-                                                         *R1078MBS
				 *   LAST ELEMENT - IF NO2RES IS SET, LEAVE NEGNOD NODES OPEN */
				swX_.scont3[miniX_.i4pli-One] = 0;
				for( i=1; i <= 3; i++ ){
					if( negmtcX_.negmtc[i-One] != 1 ){
						if( i != jbgusX_.jgus[miniX_.i4pli-One] ){
							swX_.swork[miniX_.i4pli-One][i-One] = -1;
							continue;
							}
						}
					swX_.scont3[miniX_.i4pli-One] += 1;
					}
				}


			/*   RESET FOR NEXT MATCH */

			specif = -1;
			miniX_.k12s = 0;

			/*   AT EOS ???? */
			if( done == 1 )
				break;

			}
		}



L_2220:
	if( resswX_.ressw != 1 ){

		tempi = swX_.ecount*2;
		memcpy(&swX_.scont2,&swX_.scont3,tempi);

		}

	goto L_7000;



L_8000:
	if( opswX_.sw[8-One] == 1 )
		{
		fprintf( _spec_fp, "--ERROR IN RES1AND2 ERR,LOCFLG,RETSW = %8ld%8ld%8d\n", 
		  errvrsX_.err, locflg, retsw );
		}
	*retflg = -1;

L_7000:
	return;


} /*end of function*/


