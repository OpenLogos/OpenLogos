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
	/*  changes:
	 *      11/23/93 AVK: If german phrasal transfer, use pat of head element
	 *      10/12/92 jal: increase size of imatch to elmmax
	 *      04/23/87 *R1679RKH*  OFL3B conversion & R1685 Swork limi
	 *      CHG 08/22/86 *R1561DSD: 100 SCONS
	 *      CHG 04/01/86 */
	/*SW36 */
	/*   ***** BEGINNING OF -36 SWITCH ***** */
	/*     MANY DIFFERENT FUNCTIONS DEPENDING ON
	 *     VALUE OF FIRST PARAMETER. */

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


void /*FUNCTION*/ t3sw36()
{
	static short int diacb3k7, addr, adr2, jz2, m, relptr, savadr, sc1, sc45, scnrow, sctemp, setscn, subfcn;
	static short int jbc3 = 3;
	static long int _l0;
	int dictype, taddress;
	static short val9 = 9;
	static char pgmnam[9] = "T3SW36  ";
	static short gz = 0;
	static short iz = 0;
	static short kz = 0;
	static short ms = 0;
	static short om = 0;
	static short wc = 0;
	static short xx = 0;
	static short zz = 0;
	static short cb9 = 0;
	static short iz2 = 0;
	static short typ = 0;
	static short gb36 = 0;
	static short k3p2 = 0;
	static short numr = 0;
	static short sc12 = 0;
	static short sr36 = 0;
	static short chk91 = 0;
	static short chk94 = 0;
	static short retsw = 0;
	static short formck = 0;
	static short sw36no = 0;

	vbdataX_.k3n = vbdataX_.k3 + 3;

	k3p2 = vtrfX_.vtrf[vbdataX_.k3+2-One];

	if( flag32X_.ad32 == -1 ){

		if( !(vbdataX_.k3p1 >= 100 && vbdataX_.k3p1 <= 998) ){

			
			
			if( vbdataX_.k3p1 >= 11 && vbdataX_.k3p1 <= 14 ){
				/*   FUNCTION 11-14 (GERMAN ONLY) FOR DECLENTION OF NOUNS BY NUMBER */

				if( srcflgX_.srcflg == 1 ){

					/*   WC 1 5 6 AND 7 ONLY */
					sw36no = im81X_.im81 - k3p2;
					iz2 = sworkX_.phrhed[sw36no-One];
					iz = sconX_.scon[iz2-One][1-One];
					if( iz > 0 ){
						if( iz <= 7 ){


							if( !(iz >= 2 && iz <= 4) ){


								/*            set source case in SC24 and propigate */

								xx = im81X_.im81 - k3p2;
								scnrow = sworkX_.swork[xx-One][4-One];
								if( sconX_.scon[scnrow-One][1-One] >= 
								  0 ){
									sconX_.scono[sconX_.scolnk[scnrow-One]-
									  One][24-SCONX1-One] = vbdataX_.k3p1 - 
									  10;
									/*                   propigate the changes
									 *                    apply only to unset (scon=0) and unlocked scons
									 *                    concatenated in the input OPADR
									 *                     i.e. subfcn 20 of SW48 */
									subfcn = 20;
									relptr = k3p2;
									setscn = 24;
									txsw48(subfcn,relptr,setscn);
									}

								/*             only consider of target number can be plur or sing */
								if( sconX_.scon[iz2-One][3-One] <= 3 ){

									iz = nounsX_.gernum[vbdataX_.k3p1-10-One][formsaX_.formsv[sconX_.scolnk[sworkX_.swork[sw36no-One][4-One]-
									  One]-One]-One];
									if( !(iz < 1 || iz > 99) ){

										flowckX_.n6jim = im81X_.im81 - 
										  k3p2;
										flowckX_.phrstr = sworkX_.phrbeg[flowckX_.n6jim-One];
										flowckX_.phrlst = sworkX_.phrend[flowckX_.n6jim-One];
										for( zz=flowckX_.phrstr; zz <= flowckX_.phrlst; zz++ ){
											if( sconX_.scon[opadriX_.sconpi[zz-One]-One][1-One] >= 0 ){
												if( sconX_.scon[opadriX_.sconpi[zz-One]-One][1-One] == 1 )
												iz = nounsX_.gernum[vbdataX_.k3p1-10-One][formsaX_.formsv[sconX_.scolnk[opadriX_.sconpi[zz-One]-
												  One]-One]-One];
												sconX_.scon[opadriX_.sconpi[zz-One]-One][5-One] = iz;
												/*                FOR MULTI LOADED VC, SET ALL COMPONENT ELEMENTS */
												if( opadriX_.opadri[zz-One] <= -100 && opadriX_.opadri[zz-One] >= -120 ){
												if( hpdopiX_.hfdopi[zz-One] >= HFPOLO && hpdopiX_.hfdopi[zz-One] <= HFPOHI ){
												iz2 = iz;
												adr2 = hpdopiX_.hfdopi[zz-One] - HFPOL1;
												jz2 = hfdoaX_.hfpoad[adr2-One][HFPADX-One];
												for( m=1; m <= jz2; m++ ){
												sc1 = sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][1-One];
												if( sc1 >= 0 ){
												if( sc1 == 1 ){
												sctemp = sconX_.scolnk[hfdoaX_.sconhf[adr2-One][m-One]-One];
												if( sctemp > 0 && 
												  sctemp <= elemctX_.elemct )
												iz2 = nounsX_.gernum[vbdataX_.k3p1-10-One][formsaX_.formsv[sctemp-One]-
												  One];
												}
												sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-
												  One][5-One] = iz2;
												}
												}
												}
												}
												}
											}
										}
									}
								}
							}
						}
					}
				goto L_940;
				}





			else if( vbdataX_.k3p1 == 7 ){
				/*   FUNCTION 7 CHANGE FORM IF RELON = 1 */
				if( flagX_.relon != 0 ){
					flowckX_.n6jim = im81X_.im81 - k3p2;
					sr36 = 96;

					formod(2,sr36,jbc3,flowckX_.n6jim,&retsw);
					if( retsw == 2 ){
						sworkoX_.sworko[sworkoX_.phcto-One][2-One] = 104;
						}
					else{

						/*+ OFL3B conversion & R1685 Swork limit Change RKH  04/23/87   R1679 */
						sc45 = sconX_.scono[sconX_.scolnk[sworkX_.phrhed[flowckX_.n6jim-One]-One]-One][45-SCONX1-One];
						if( sc45 == 6 )
							sworkoX_.sworko[sworkoX_.phcto-One][2-One] = 101;
						if( sc45 == 7 )
							sworkoX_.sworko[sworkoX_.phcto-One][2-One] = 102;
						if( sc45 == 8 )
							sworkoX_.sworko[sworkoX_.phcto-One][2-One] = 103;
						}
					}
				goto L_940;
				}






			else if( vbdataX_.k3p1 == 9 ){
				/*     FUNCTION 9 - NULLIFY ADDRRESSES OF PHRASE */
				flowckX_.n6jim = im81X_.im81 - k3p2;
				flowckX_.phrstr = sworkX_.phrbeg[flowckX_.n6jim-One];
				flowckX_.phrlst = sworkX_.phrend[flowckX_.n6jim-One];
				for( ms=flowckX_.phrstr; ms <= flowckX_.phrlst; ms++ ){
					opadriX_.opadri[ms-One] = -140;
					/*+ Deactivate SCONPI for -140 in OPADRI         RKH  05/02/87   R1691 */
					if( opadriX_.sconpi[ms-One] != sworkX_.phrhed[flowckX_.n6jim-One] )
						opadriX_.sconpi[ms-One] = 1;
					/*-                                             RKH  05/02/87   R1691 */
					hpdopiX_.hfdopi[ms-One] = 0;
					}
				goto L_940;
				}






			else if( vbdataX_.k3p1 >= 17 && vbdataX_.k3p1 <= 20 ){
				/*   FOR FUNCTIONS 17-20 CERTAIN CONDITIONS ARE TESTED AND IF THEY
				 *   ARE MET, A BRANCH TO THE WC9 RULE INDICATED BY K3P2 TAKES PLACE. */

				if( vbdataX_.k3p1 == 20 ){

					/*   FOR FUNCTION 20 CONDITION IS COMP = 1 */
					if( sw31bkX_.comp == 0 )
						goto L_940;
					sw31bkX_.comp = 0;
					/*   FOR 17-19 THE FIRST CONDITION IS THAT DEPCL BE = TO 1 */
					}
				else if( sw31bkX_.depcl == 0 ){
					goto L_940;
					}
				else{
					ms = vbdataX_.k3p1 - 16;

					/*   FOR FUNCTION 17, IF DEPCL = 1 THAT IS THE ONLY CONDITION. */
					if( ms != 1 ){
						if( ms == 3 ){

							/*   FOR FUNCTION 19 SEARCH THE INCOMING NSWORKS BACKWARDS, STARTING
							 *   TO THE LEFT OF THE FIRST RULE ELEMENT. IF A 19004 -1 IS FOUND FIRST
							 *   BRANCH TO WC9.  IF A 01 - 0191 AND A 01 - 0194 ARE FOUND FIRST,
							 *   CONTINUE WITH THE CURRENT VTR. */

							flowckX_.n6jim = vwarg2X_.i - 1;
							xx = flowckX_.n6jim;
							chk91 = 0;
							chk94 = 0;
							for( ms=1; ms <= flowckX_.n6jim; ms++ ){
								wc = sworkX_.swork[xx-One][1-One];
								typ = sworkX_.swork[xx-One][2-One];
								if( !(wc != 1 && wc != 19) ){
									if( wc == 1 ){
										if( chk91 != 1 ){
											formck = 91;
											formod(2,formck,jbc3,xx,&retsw);
											if( retsw != 2 ){
												chk91 = 1;
												goto L_440;
												}
											}
										if( chk94 != 1 ){
											formck = 94;
											formod(2,formck,jbc3,xx,&retsw);
											if( retsw != 2 )
												chk94 = 1;
											}
L_440:
										if( chk91 == 1 && chk94 == 1 )
											break;
										}
									else if( typ == 4 ){
										goto L_840;
										}
									}
								xx -= 1;
								}
							goto L_940;
							}
						else{

							/*   FOR FUNCTION 18, SEARCH THE INCOMING NSWORKS BACKWARDS STARTING
							 *   WITH THE FIRST ELEMENT OF THIS RULE.  IF A 19004 -1 IS FOUND FIRST
							 *   BRANCH TO WC9.  IF A 01 - 0191 IS FOUND, CONTINUE WITH CURRENT VTR. */

							flowckX_.n6jim = vwarg2X_.i;
							xx = flowckX_.n6jim;
							for( ms=1; ms <= flowckX_.n6jim; ms++ ){
								wc = sworkX_.swork[xx-One][1-One];
								typ = sworkX_.swork[xx-One][2-One];
								if( !(wc != 1 && wc != 19) ){
									if( wc == 1 ){
										formck = 94;
										formod(2,formck,jbc3, xx,&retsw);
										if( retsw == 1 )
											break;
										}
									else if( typ == 4 ){
										goto L_840;
										}
									}

								xx -= 1;
								}
							goto L_940;
							}
						}
					}

L_840:

				if( loopckX_.call36[1-One] == 0 ){
					loopckX_.call36[1-One] = loopckX_.nptpsv;
					if( minickX_.minifg == 1 )
						loopckX_.call36[2-One] = 1;
					}


				/*                  MINI MINI MINI */

				/*                          IS THERE AN EXPERIMENTAL RULE?? */
				cb9 = k3p2;
				semargX_.pntr9 = k3p2;
				txmini(3,&minickX_.k7m,&loopckX_.nptpx,vwarg2X_.i, &cb9);
				if( errvrsX_.errlvl != 0 )
					return;
				idxval((short*)ADR(_l0,1),&val9,&k3p2,&diacb3k7,&numr);
				if( numr == 0 && minickX_.k7m == 0 )
					goto L_940;
				if( numr != 0 )
					rulein_ptr((short*)ADR(_l0,1),&commdX_.matpos,
						spX_ptr,&diacb3k7,  &retsw);

				/*                     K7M WILL BE ZERO IF NO RULE FOUND */
				if( diacb3k7 != 0 || minickX_.k7m != 0 )
					getvtrX_.getvtr = 1;
				return;
				}
			else{







				if( vbdataX_.k3p1 == 33 ){
					/* --------------------------------
					 *     FUNCTION 33 */

					/*    THIS FUNCTION SAVES THE WC, 3 TYPES AND FORM FOR LATER USE IN
					 *    SEMTAB WHEN A SEMTAB LOAD OF -99XXX IS CALLED. IF THE CALL IS MADE
					 *    WITHOUT THE -36033 BEING INVOKED BEFORE, NO SEMWRK WILL BE CREATED
					 *     R1768MBS 7/1/87
					 *     POS 11 OF THE ARRAY WILL BE SET WHEN THE ELEMENT IS LOADED */

					flowckX_.n6jim = im81X_.im81 - k3p2;
					/*             SAVE THE INCOMING SWORK POINTER */
					sav36sX_.sav36s[1-One] = flowckX_.n6jim;
					/*             SAVE THE 4 SWORK VALUES */
					lmove(&sav36sX_.sav36s[2-One],1,&sworkX_.swork[flowckX_.n6jim-One][1-One],1,8);
					/*             SAVE FORMSV */
					sav36sX_.sav36s[6-One] = formsaX_.formsv[sconX_.scolnk[flowckX_.n6jim-One]-One];
					om = sworkX_.phrhed[flowckX_.n6jim-One];
					sav36sX_.sav36s[7-One] = sconX_.scon[om-One][11-One];
					sav36sX_.sav36s[8-One] = sconX_.scono[sconX_.scolnk[om-One]-One][45-SCONX1-One];
					sav36sX_.sav36s[9-One] = sconX_.scon[om-One][13-One];
					sav36sX_.sav36s[10-One] = 0;
					sav36sX_.sav36s[11-One] = 0;
					}




				else if( vbdataX_.k3p1 == 46 ){
					flowckX_.n6jim = im81X_.im81 - k3p2;
					sworkX_.swork[flowckX_.n6jim-One][3-One] = formsaX_.formsv[sconX_.scolnk[sworkX_.swork[flowckX_.n6jim-One][4-One]-One]-One];
					}





				else if( vbdataX_.k3p1 == 53 ){
					/*   FUNCTION 53  P.R.1227 */
					flowckX_.n6jim = im81X_.im81 - k3p2;
					if( sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][1-One] < 0 )
						sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][1-One] *= -1;
					}




				else if( !((vbdataX_.k3p1 == 50 || vbdataX_.k3p1 == 51) || vbdataX_.k3p1 == 66) ){

					if( vbdataX_.k3p1 == 56 ){
						/*GBNEW+
						 *          FUNCTION 56 - SAVE RULE ADDRESS TO PREVENT LOOPS
						 *+ */
						if( k3p2 == 0 ){

							/*                 FUNCTION 056000 SAVE THIS RULE'S ADDRESS
							 *                                0GM 10/86  FIX -36056 FOR MINI/MAIN  +
							 *   If IDENSP or IDEN42 are set, then we found identical rules in the
							 *   mini/main files for the non WC10 rules and the WC10 rules respectiv. */

							if( minickX_.minifg == 1 ){
								if( !((spcompX_.idensp == 1 && vtrs42X_.sw42n == 0) || 
									(spcompX_.iden42 == 1 && vtrs42X_.sw42n ==   1)) )
									goto L_740;
								}
							/*                                0GM 10/86  FIX -36056 FOR MINI/MAIN  - */

							if( loopckX_.noloop[1-One] == 0 ){
								loopckX_.noloop[1-One] = vwarg2X_.i;
								if( vtrs42X_.sw42n == 1 )
									loopckX_.noloop[1-One] = vwarg1X_.isave;
								}
							for( gb36=2; gb36 <= 21; gb36++ ){
								if( loopckX_.noloop[gb36-One] == 0 )
									goto L_981;
								}
							goto L_700;
L_981:
							loopckX_.noloop[gb36-One] = loopckX_.nptpsv;
							/*                                0GM 10/86  FIX -36056 FOR MINI/MAIN  + */
							if( vtrs42X_.sw42n == 0 && spcompX_.idensp == 
							  1 ){
								loopckX_.noloop[gb36-One] = spcompX_.nptpm;

								}
							else if( vtrs42X_.sw42n == 1 && spcompX_.iden42 == 1 ){
								loopckX_.noloop[gb36-One] = spcompX_.nptp42;
								}
							else{
								/*                                0GM 10/86  FIX -36056 FOR MINI/MAIN  - */
								if( diagsX_.deepdi == 1 )
									{
									fprintf( _spec_fp, " IN SW36,NOLOOP VAL =  " );
									for( gz=1; gz <= 21; gz++ ){
										fprintf( _spec_fp, "%6d", 
										  loopckX_.noloop[gz-One] );
										}
									fprintf( _spec_fp, "\n" );
									}
								goto L_940;
								}

							/*             NLOOP2 STORES ADDRESS OF MINI RULE MATCH */

L_740:
							if( nloop2X_.nloop2[1-One] == 0 ){
								nloop2X_.nloop2[1-One] = vwarg2X_.i;
								if( vtrs42X_.sw42n == 1 )
									nloop2X_.nloop2[1-One] = vwarg1X_.isave;
								}
							for( gb36=2; gb36 <= 21; gb36++ ){
								if( nloop2X_.nloop2[gb36-One] == 0 )
									goto L_982;
								}
							goto L_940;
L_982:
							nloop2X_.nloop2[gb36-One] = loopckX_.nptpsv;

							if( diagsX_.deepdi == 1 )
								{
								fprintf( _spec_fp, " IN 36 SW,NLOOP2 =  " );
								for( gz=1; gz <= 21; gz++ ){
									fprintf( _spec_fp, "%6d", nloop2X_.nloop2[gz-One] );
									}
								fprintf( _spec_fp, "\n" );
								}
							goto L_940;
							}
						else{

							/*        CALL36(1) IS THE ADDRESS RECORDED IN -42 SWITCH OF ORIGINAL
							 *        RULE THAT CALLED A WORD CLASS 9/10 RULE (USE WITH -36056001)
							 *        CALL36(2) IS 0 UNLESS ORIGINAL CALLING RULE IS FROM MINI (=1)
							 *                                0GM 10/86  FIX -36056 FOR MINI/MAIN  +
							 *       idensp=1 indicates that both the main WC and mini WC rules
							 *       which called the WC 10 rule are identical. */

							/*       IS CALLING RULE IN CALL36(1) FROM A MINI (CALL36(2) = 1)? */
							if( !(loopckX_.call36[2-One] == 1 && spcompX_.idensp != 1) ){
								/*                                0GM 10/86  FIX -36056 FOR MINI/MAIN  - */

								/*             SAVE CALLING RULE FROM CALL36(1) IN NOLOOP ARRAY */

								if( loopckX_.noloop[1-One] == 0 ){
									loopckX_.noloop[1-One] = vwarg2X_.i;
									if( vtrs42X_.sw42n == 1 )
										loopckX_.noloop[1-One] = vwarg1X_.isave;
									}

								for( gb36=2; gb36 <= 21; gb36++ ){
									if( loopckX_.noloop[gb36-One] == 0 )
										goto L_983;

									}
								goto L_700;
L_983:
								loopckX_.noloop[gb36-One] = loopckX_.call36[1-One];
								/*                                0GM 10/86  FIX -36056 FOR MINI/MAIN  + */
								if( spcompX_.idensp != 1 )
									goto L_940;
								loopckX_.noloop[gb36-One] = spcompX_.nptpm;
								}

							/*      FOR MINI: SAVE MINI CALLING RULE IN NLOOP2 ARRAY */

							if( nloop2X_.nloop2[1-One] == 0 ){
								nloop2X_.nloop2[1-One] = vwarg2X_.i;
								if( vtrs42X_.sw42n == 1 )
									nloop2X_.nloop2[1-One] = vwarg1X_.isave;
								}
							for( gb36=2; gb36 <= 21; gb36++ ){
								if( nloop2X_.nloop2[gb36-One] == 0 )
									goto L_984;

								}
							goto L_700;
L_984:
							nloop2X_.nloop2[gb36-One] = loopckX_.call36[1-One];
							goto L_940;
							}

						/*            OVERLOADING NOLOOP ARRAY */

L_700:
						if( diagsX_.longdi == 1 )
							{
							fprintf( _spec_fp, " -36056 OVERLOADING NOLOOP ARRAY\n" );
							}
						}
					}
				goto L_940;
				}
			}
		}









	/*  IF K3P1 IS IN CONSTANT RANGE (131-998) CHANGE INCOMING OPADRI ADDRESS */
	flowckX_.n6jim = im81X_.im81 - k3p2;

	addr = -vbdataX_.k3p1;
	if( flag32X_.ad32 != -1 ){
		addr = vbdataX_.k3p1 + 1000*flag32X_.ad32;
		flag32X_.ad32 = -1;
		}

	if( sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][1-One] >= 0 ){
		flowckX_.phrstr = sworkX_.phrbeg[flowckX_.n6jim-One];
		flowckX_.phrlst = sworkX_.phrend[flowckX_.n6jim-One];

		for( kz=flowckX_.phrstr; kz <= flowckX_.phrlst; kz++ ){
			if( opadriX_.sconpi[kz-One] == sworkX_.phrhed[flowckX_.n6jim-One] )
				goto L_985;
			}
		goto L_940;
L_985:
		savadr = opadriX_.opadri[kz-One];
		opadriX_.opadri[kz-One] = addr;
		if( addr < 0 && addr > -1000 ){
			sconX_.scono[sconX_.scolnk[opadriX_.sconpi[kz-One]-One]-One][60-SCONX1-One] = -addr;
			taddress = -addr;
			dictype = 2;
		}
		else{
			taddress = addr;
			dictype = 3;
		}
		errvrsX_.err = TARG_CODES(&trgflgX_.trgflg,&dictype,
			       &taddress,
				   LOGCC,
				   (short *)&trgcdsX_,diagsX_.longdi, _spec_fp);
		if( errvrsX_.err == 0 ){
			sconX_.scono[sconX_.scolnk[opadriX_.sconpi[kz-One]-One]-One][59-SCONX1-One] = trgcdsX_.tcpatm[1-One];
			}
		else{
			errlog(pgmnam,5361,taddress,13);
			opadriX_.opadri[kz-One] = savadr;
			}

		}

L_940:
	if( diagsX_.deepdi == 1)
		{
		fprintf( _spec_fp, " SW36 %5d%5d%5d%5d%5d%5d%5d%5d\n", k3p2, 
		  vbdataX_.k3n, vtrnX_.vtrn, flowckX_.n6jim, flowckX_.phrstr, 
		  flowckX_.phrlst, sw31bkX_.depcl, xx );
		}

	return;
} /*end of function*/

