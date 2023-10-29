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
	 *      03/06/91 *JAL*:  ADD CONDITION TO SET SCON7 IF SCON1 = 12
	 *      02/29/91 *JAL*:  SET CONDITIONS LIMITING THE SETTING OF CASE
	 *           (I.E.SCON7) FOR ENGLISH SOURCE, FRN,SPA,AND ITA TARGETS.
	 *      04/17/87 *R1685RKH*  CHANGE T1-4 SWORK LIMIT FROM 50 TO
	 *      07/18/86 *R1530DSD: EXPAND RELATIVE POINTERS FOR STRETCH
	 *      12/02/85 */
	/*+                    FLAG TO INHIBIT SETTING OF SCON7 *02/28/91*JAL* */
	/*     FUNCTION - ESTABLISHES PER, NUM, GEN, AND CASE
	 *     OF NON-HEAD NOUN (E.G. PAINT SHED), ALSO NON-NOUNS.
	 *     A RIGHT ORIENTED SWITCH. NO FLAG SETTING, NO SWORK */

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



void /*FUNCTION*/ t3sw38()
{
	static byte vtrfmn;
	static short int addr2, adr2, jz2, m, relptr, sc4val, setsc7, setscn, subfcn, x;
	static short gb = 0;
	static short iz = 0;
	static short ms = 0;
	static short gbx = 0;
	static short sc1 = 0;
	static short sc7 = 0;
	static short jgb4 = 0;
	static short sr38 = 0;
	static short temp = 0;
	static short mcase = 0;
	static short retsw = 0;
	static short gbcomp = 0;
	static short gbtemp = 0;
	static short mrelcs = 0;
	static short sconlc = 0;

	vtrnX_.vtrn = vtrfX_.vtrf[vbdataX_.k3+2-One];
	vbdataX_.k3n = vbdataX_.k3 + 4;

	/*     ESTABLISH ELEMENT TO RIGHT OF SW */
	vbdataX_.k3p1 = vtrfX_.vtrf[vbdataX_.k3+3-One];

	/*     SWITCH PARAMETER */
	if( vtrnX_.vtrn <= 10 ){

		elemld();
		if( errvrsX_.errlvl != 0 )
			return;

		/*     SCON NOW ESTABLISHED FOR ELEMENT ON RIGHT */

		/*     TEST FOR LOCKED SCON */

		vbdataX_.k3p1 = vtrfX_.vtrf[vbdataX_.k3+1-One];
		/*+    IS THIS VTRF ELEMENT A RELATIVE PTR?       06/06/86  *R1530DSD */
		vtrfmn = vtrfmX_.vtrfm[vbdataX_.k3+1-One];

		sconlc = sworkX_.phrhed[flowckX_.n6jim-One];

		/*            set source case in SC24 and propigate */

		if( sconX_.scon[sconlc-One][1-One] >= 0 ){
			sconX_.scono[sconX_.scolnk[sconlc-One]-One][24-SCONX1-One] = sw21bkX_.case_;
			/*                   propigate the changes
			 *                    apply only to unset (scon=0) and unlocked scons
			 *                    concatenated in the input OPADR
			 *                     i.e. subfcn 20 of SW48 */
			subfcn = 20;
			relptr = im81X_.im81 - flowckX_.n6jim;
			setscn = 24;
			txsw48(subfcn,relptr,setscn);
			}

		/*+           INHIBIT THE SETTING OF SCON7? 1=YES    *02/28/91*JAL* */
		setsc7 = 1;
		if( srcflgX_.srcflg == 2 ){
			if( trgflgX_.trgflg == 3 || trgflgX_.trgflg == 4 || 
			    trgflgX_.trgflg == 5 || trgflgX_.trgflg == 6){
				setsc7 = 0;
				if( ((sconX_.scon[sconlc-One][1-One] == 2 ||
					  sconX_.scon[sconlc-One][1-One] == 5) ||
					  sconX_.scon[sconlc-One][1-One] == 12) ||
					  sconX_.scon[sconlc-One][1-One] == 18 ){
					setsc7 = 1;
					}
				else if( sconX_.scon[sconlc-One][1-One] == 1 ){
					if( sconX_.scon[sconlc-One][13-One] == 15 ||
						sconX_.scon[sconlc-One][13-One] == 16 )
						setsc7 = 1;
					}
				}
			}

		if( srcflgX_.srcflg == 1 &&
			(trgflgX_.trgflg == 5 || trgflgX_.trgflg == 3) ){
			if( sconX_.scono[sconX_.scolnk[sconlc-One]-One][45-SCONX1-One] == 9 ){
				sconX_.scon[sconlc-One][7-One] = 5;
				goto L_5590;
				}
			}

		/*  SET GENDER IF ALTWC IS VERB */
		if( sconX_.scon[sconlc-One][8-One] == 2 ){
			if( sconX_.scon[sconlc-One][1-One] == 1 ||
				sconX_.scon[sconlc-One][1-One] == 2){
				if( targ25X_.targ25[sconX_.scolnk[sworkX_.swork[flowckX_.n6jim-One][4-One]-
				  One]-One] != -1 ){
					sc4val = targ25X_.targ25[sconX_.scolnk[sworkX_.swork[flowckX_.n6jim-One][4-One]-One]-One];
					for( ms=flowckX_.phrstr; ms <= flowckX_.phrlst; ms++ ){
						if( sconX_.scon[opadriX_.sconpi[ms-One]-One][1-One] > 0 ){
							sconX_.scon[opadriX_.sconpi[ms-One]-One][4-One] = sc4val;
						}
						/*                   IF THIS IS A MULTI LOADED VC, SET ELEMENTS IN THE VC */
						if( opadriX_.opadri[ms-One] <= -100 &&
							opadriX_.opadri[ms-One] >= -120 ){
							if( hpdopiX_.hfdopi[ms-One] >= HFPOLO && 
							    hpdopiX_.hfdopi[ms-One] <= HFPOHI ){
								adr2 = hpdopiX_.hfdopi[ms-One] - HFPOL1;
								jz2 = hfdoaX_.hfpoad[adr2-One][HFPADX-One];
								for( m=1; m <= jz2; m++ ){
									if( sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-
									  One][1-One] >= 0 )
										sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-
										  One][4-One] = sc4val;
									}
								}
							}
						}
					targ25X_.targ25[sconX_.scolnk[sworkX_.swork[flowckX_.n6jim-One][4-One]-
					  One]-One] = -1;
					}
				}
			}
		if( srcflgX_.srcflg == 1 ){

			if( vbdataX_.k3p1 >= 0 ){
				if( sconX_.scon[sconlc-One][5-One] != 2 ){
					if( sconX_.scon[sconlc-One][3-One] <= 3 ){
						/*- 89/08/22                                                 *R0GBA*GBA
						 *     WC 1,5,6,7 ONLY */
						iz = sconX_.scon[sconlc-One][1-One];
						if( !(iz <= 0 || iz > 7) ){
							if( !(iz >= 2 && iz <= 4) ){
								/*     FOR DECLENTION OF NOUNS BY NUMBER
								 *     IF K3P1 IS 1-4, K3P1 IS USED TO COMPUTE SCON(5)
								 *     IF K3P1 EQUALS 0, THE CASE FLAG IS USED. */
								if( vbdataX_.k3p1 > 0 ){
									ms = vbdataX_.k3p1;
									if( ms < 1 || ms > 4 )
										goto L_5360;
									}
								else if( sw21bkX_.case_ == 0 || sw21bkX_.case_ > 7 ){
									goto L_5360;
									}
								else{

									/*    JGB4 WILL SIGNAL A SPECIAL FORMOD FUNCTION - FRMARR USED */
									jgb4 = 4;
									frmarrX_.frmarr[1-One] = sworkX_.swork[flowckX_.n6jim-One][1-One];
									frmarrX_.frmarr[2-One] = formsaX_.formsv[sconX_.scolnk[sworkX_.swork[flowckX_.n6jim-One][4-One]-
									  One]-One];

									if( sw21bkX_.case_ == 1 )
										goto L_5360;
									if( sw21bkX_.case_ == 5 ){
										/*     FOR CASE = 5,6,7 - TEST NFORM TABLE */

										sr38 = 87;
										formod(2,sr38,jgb4,flowckX_.n6jim, &retsw);
										if( retsw == 1 )
											goto L_5340;

										sr38 = 88;
										formod(2,sr38,jgb4,flowckX_.n6jim,&retsw);
										if( retsw == 1 )
											goto L_5340;
										if( retsw == 2 )
											goto L_5320;
										}
									else if( sw21bkX_.case_ != 6 ){
										if( sw21bkX_.case_ == 7 )
											goto L_5280;
										ms = sw21bkX_.case_;
										goto L_5180;
										}

									sr38 = 86;
									formod(2,sr38,jgb4,flowckX_.n6jim,&retsw);
									if( retsw == 1 )
										goto L_5340;

									sr38 = 88;
									formod(2,sr38,jgb4,flowckX_.n6jim,&retsw);
									if( retsw == 1 )
										goto L_5340;
									if( retsw == 2 )
										goto L_5320;

L_5280:
									sr38 = 86;
									formod(2,sr38,jgb4,flowckX_.n6jim, &retsw);
									if( retsw != 1 ){
										sr38 = 87;
										formod(2,sr38,jgb4,flowckX_.n6jim,&retsw);
										if( retsw != 1 )
											goto L_5320;
										}

L_5340:
									sconX_.scon[sconlc-One][5-One] = 2;
									goto L_5350;

L_5320:
									sconX_.scon[sconlc-One][5-One] = 1;
									goto L_5350;
									}
L_5180:
								iz = nounsX_.gernum[ms-One][formsaX_.formsv[sconX_.scolnk[sworkX_.swork[flowckX_.n6jim-One][4-One]-
								  One]-One]-One];
								if( iz >= 1 && iz <= 99 )
									sconX_.scon[sconlc-One][5-One] = iz;

								/*   IF SCON(5) IS SET BY CASE FLAG - LOCK THE SCON
								 *   NO LOCK FOR SUPERSETS 13-16 */

L_5350:
								if( srcflgX_.srcflg == 1 && 
									(trgflgX_.trgflg == 5 || trgflgX_.trgflg == 3) ){
									temp = sconX_.scon[sconlc-One][5-One];
									for( iz=flowckX_.phrstr; iz <= flowckX_.phrlst; iz++ ){
										if( sconX_.scon[opadriX_.sconpi[iz-One]-One][1-One] >= 0 )
											sconX_.scon[opadriX_.sconpi[iz-One]-One][5-One] = temp;
										/*                   IF THIS IS A MULTI LOADED VC, SET ELEMENTS IN THE VC */
										if( opadriX_.opadri[iz-One] <= -100 && opadriX_.opadri[iz-One] >= -120 ){
											if( hpdopiX_.hfdopi[iz-One] >= HFPOLO && hpdopiX_.hfdopi[iz-One] <= HFPOHI ){
												adr2 = hpdopiX_.hfdopi[iz-One] - HFPOL1;
												jz2 = hfdoaX_.hfpoad[adr2-One][HFPADX-One];
												for( m=1; m <= jz2; m++ ){
												if( sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]- One][1-One] >= 0 )
												sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][5-One] = temp;
												}
												}
											}
										}
									}

								if( !(sconX_.scon[sconlc-One][13-One] >= 13 &&
									  sconX_.scon[sconlc-One][13-One] <= 16) ){
									if( sw21bkX_.case_ > 0 && vbdataX_.k3p1 == 0 )
										sconX_.scon[sconlc-One][1-One] = -sconX_.scon[sconlc-One][1-One];
									}
								}
							}
						}
					}
				}
			}

L_5360:
		if( sconX_.scon[sconlc-One][9-One] != 7 ){
			if( sconX_.scon[sconlc-One][1-One] > 0 ){
				if( vbdataX_.k3p1 < 0 ){

					/*     FUNCTION 5 *****/
					if( vtrfmn == vtrfvlX_.vtmrp ){
						temp = sconX_.scon[sworkX_.phrhed[im81X_.im81-vbdataX_.k3p1-One]-
						  One][7-One];
						}
					else if( vbdataX_.k3p1 == -99 ){
						mrelcs = sw36bkX_.relcas;
						mcase = sw21bkX_.case_;
						if( trgflgX_.trgflg == 1 ){
							if( sw36bkX_.rel == 0 ){
								if( sw21bkX_.case_ == 5 )
									mcase = 3;
								if( sw21bkX_.case_ == 6 || sw21bkX_.case_ == 7 )
									mcase = 2;
								temp = mcase;
								}
							else{
								if( sw36bkX_.relcas == 5 )
									mrelcs = 3;
								if( sw36bkX_.relcas == 6 || sw36bkX_.relcas == 7 )
									mrelcs = 2;
								temp = mrelcs;
								}
							}
						else if( sw36bkX_.rel == 0 ){
							temp = mcase;
							}
						else{
							temp = mrelcs;
							}
						}
					}
				else if( vbdataX_.k3p1 == 0 ){
					goto L_5580;
					}
				else{

					/*     FUNCTION 4 ***** */

					temp = vbdataX_.k3p1;
					}


				/*     FUNCTION 6 **** */

				/*     RESET PHRASE START AND STOP FOR RELATIVE POINTER */
				ms = sworkX_.swork[flowckX_.n6jim-One][1-One];
				if( temp != vbdataX_.k3p1 )
					ms = 0;
				for( iz=flowckX_.phrstr; iz <= flowckX_.phrlst; iz++ ){
					sc1 = sconX_.scon[opadriX_.sconpi[iz-One]-One][1-One];
					sc7 = sconX_.scon[opadriX_.sconpi[iz-One]-One][7-One];
					if( sc1 >= 0 ){
						if( ms == 2 ){
							if( sc1 >= 99 && sc1 <= 120 )
								continue;
							}
						/*             EG ONLY  SET FLAG WHEN SCON 7, VC 108 BETWEEN 14 AND 17 */
						if( ((srcflgX_.srcflg == 2 && sc1 == 108) && 
						      sc7 >= 14) && sc7 <= 17 )
							sw38bkX_.gb108 = sc7;
						if( setsc7 == 1 )
							sconX_.scon[opadriX_.sconpi[iz-One]-One][7-One] = temp;
						/*                   IF THIS IS A MULTI LOADED VC, SET ELEMENTS IN THE VC */
						if( opadriX_.opadri[iz-One] <= -100 &&
							opadriX_.opadri[iz-One] >= -120 ){
							if( hpdopiX_.hfdopi[iz-One] >= HFPOLO && 
							    hpdopiX_.hfdopi[iz-One] <= HFPOHI ){
								adr2 = hpdopiX_.hfdopi[iz-One] - HFPOL1;
								jz2 = hfdoaX_.hfpoad[adr2-One][HFPADX-One];
								for( m=1; m <= jz2; m++ ){
									sc1 = sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][1-One];
									sc7 = sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][7-One];
									if( sc1 >= 0 ){
										if( ms == 2 ){
											if( sc1 >= 99 && sc1 <= 120 )
												continue;
											}
										if( ((srcflgX_.srcflg == 2 && sc1 == 108) && sc7 >= 14) && sc7 <= 17 )
											sw38bkX_.gb108 = sc7;
										if( setsc7 == 1 )
											sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][7-One] = temp;
										}
									}
								}
							}
						}
					}
				if( srcflgX_.srcflg == 1 ){
					/*                    CALCULATE SCON5 FOR ALL NON-HEAD NOUNS OF
					 *                    THE PHRASE FROM THE NEW CASE VALUE (=SCON(7)) */
					for( ms=flowckX_.phrstr; ms <= flowckX_.phrlst; ms++ ){
						if( sconlc != opadriX_.sconpi[ms-One] ){
							sc1 = sconX_.scon[opadriX_.sconpi[ms-One]-One][1-One];
							if( sc1 == 1 || (trgflgX_.trgflg == 5 && (sc1 == 4 || sc1 == 14)) )
								sconX_.scon[opadriX_.sconpi[ms-One]-
								  One][5-One] = nounsX_.gernum[temp-One][formsaX_.formsv[sconX_.scolnk[opadriX_.sconpi[ms-One]-
								  One]-One]-One];
							/*                   IF THIS IS A MULTI LOADED VC, SET ELEMENTS IN THE VC */
							if( opadriX_.opadri[ms-One] <= -100 && 
							    opadriX_.opadri[ms-One] >= -120 ){
								if( hpdopiX_.hfdopi[ms-One] >= HFPOLO && 
								    hpdopiX_.hfdopi[ms-One] <= HFPOHI ){
									adr2 = hpdopiX_.hfdopi[ms-One] - HFPOL1;
									jz2 = hfdoaX_.hfpoad[adr2-One][HFPADX-One];
									for( m=1; m <= jz2; m++ ){
										sc1 = sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][1-One];
										if( !(sc1 != 1 || sc1 < 0) ){
											if( sc1 == 1 || 
												(trgflgX_.trgflg == 5 && (sc1 == 4 || sc1 == 14)) )
												sconX_.scon[hfdoaX_.sconhf[addr2-One][m-One]-
												  One][5-One] = nounsX_.gernum[temp-One][formsaX_.formsv[sconX_.scolnk[hfdoaX_.sconhf[adr2-One][m-One]-
												  One]-One]-One];
											}
										}
									}
								}
							}
						}
					}

				if( temp == vbdataX_.k3p1 ){
					if( srcflgX_.srcflg == 1 )
						sw21bkX_.case_ = 0;
					}
				}
			else if( vtrfmn == vtrfvlX_.vtmrp ){

				temp = sconX_.scon[sworkX_.phrhed[im81X_.im81-vbdataX_.k3p1-One]-One][7-One];
				for( iz=flowckX_.phrstr; iz <= flowckX_.phrlst; iz++ ){
					if( sconX_.scon[opadriX_.sconpi[iz-One]-One][1-One] > 0 )
						sconX_.scon[opadriX_.sconpi[iz-One]-One][1-One] =
						-sconX_.scon[opadriX_.sconpi[iz-One]-One][1-One];
					if( setsc7 == 1 )
						sconX_.scon[opadriX_.sconpi[iz-One]-One][7-One] = temp;
					/*                   IF THIS IS A MULTI LOADED VC, SET ELEMENTS IN THE VC */
					if( opadriX_.opadri[iz-One] <= -100 && opadriX_.opadri[iz-One] >= -120 ){
						if( hpdopiX_.hfdopi[iz-One] >= HFPOLO && hpdopiX_.hfdopi[iz-One] <= HFPOHI ){
							adr2 = hpdopiX_.hfdopi[iz-One] - HFPOL1;
							jz2 = hfdoaX_.hfpoad[adr2-One][HFPADX-One];
							for( m=1; m <= jz2; m++ ){
								if( sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][1-One] >= 0 ){
									sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-
									  One][1-One] = -sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][1-One];
									if( setsc7 == 1 )
										sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][7-One] = temp;
									}
								}
							}
						}
					}
				}
			}
		}
	else{

		sw38bkX_.sw38n = 1;
		txload();
		if( errvrsX_.errlvl != 0 )
			return;

		sw38bkX_.sw38n = 0;

		/*          CONSTANT TO RIGHT */

		vbdataX_.k3p1 = vtrfX_.vtrf[vbdataX_.k3+1-One];
		/*+    TELLS IS THIS VTRF ELEMENT A RELATIVE PTR  06/06/86  *R1530DSD */
		vtrfmn = vtrfmX_.vtrfm[vbdataX_.k3+1-One];

		/*+           INHIBIT THE SETTING OF SCON7? 1=YES    *02/28/91*JAL* */
		setsc7 = 1;
		if( srcflgX_.srcflg == 2 ){
			if( trgflgX_.trgflg == 3 || trgflgX_.trgflg == 4 ||
				trgflgX_.trgflg == 5 || trgflgX_.trgflg == 6){
				setsc7 = 0;
				if( ((sconX_.scon[prtscoX_.sct-One][1-One] == 2 || 
				      sconX_.scon[prtscoX_.sct-One][1-One] == 5) || 
				      sconX_.scon[prtscoX_.sct-One][1-One] == 12) || 
				      sconX_.scon[prtscoX_.sct-One][1-One] == 18 ){
					setsc7 = 1;
					}
				else if( sconX_.scon[prtscoX_.sct-One][1-One] == 1 ){
					if( sconX_.scon[prtscoX_.sct-One][13-One] == 15 ||
						sconX_.scon[prtscoX_.sct-One][13-One] == 16 )
						setsc7 = 1;
					}
				}
			}
		if( setsc7 != 0 ){
			/*-                                                     *02/28/91*JAL* */
			if( vtrfmn == vtrfvlX_.vtmrp ){
				/*        RELATIVE POINTER */
				sconX_.scon[prtscoX_.sct-One][7-One] = sconX_.scon[sworkX_.phrhed[im81X_.im81-vbdataX_.k3p1-One]-
				  One][7-One];
				}
			else if( vbdataX_.k3p1 > 0 ){
				sconX_.scon[prtscoX_.sct-One][7-One] = vbdataX_.k3p1;
				}
			else if( vbdataX_.k3p1 == 0 || vbdataX_.k3p1 == -99 ){
				if( sw36bkX_.rel == 0 ){
					sconX_.scon[prtscoX_.sct-One][7-One] = sw21bkX_.case_;
					}
				else{
					sconX_.scon[prtscoX_.sct-One][7-One] = sw36bkX_.relcas;
					}
				}
			else if( vbdataX_.k3p1 == -98 ){
				if( sw25bkX_.hedcas == 0 ){

					sconX_.scon[prtscoX_.sct-One][7-One] = sw21bkX_.case_;
					}
				else{
					sconX_.scon[prtscoX_.sct-One][7-One] = sw25bkX_.hedcas;

					}
				}
			}
		}

	/*       IF K3P1(PARM) IS ZERO OR REL POINTER */

	/*    ENGLISH SOURCE ONLY - RESET SCON 7, VC 108 OF NOUN */

L_5580:
	if( !(sw38bkX_.gb108 == 0 ||
		  sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][1-One] != 1) ){
		flowckX_.phrstr = sworkX_.phrbeg[flowckX_.n6jim-One];
		flowckX_.phrlst = sworkX_.phrend[flowckX_.n6jim-One];
		for( gb=flowckX_.phrstr; gb <= flowckX_.phrlst; gb++ ){
			if( opadriX_.opadri[gb-One] == -108 )
				goto L_5601;
			}
		goto L_5590;
L_5601:
		gbx = opadriX_.sconpi[gb-One];
		gbtemp = sw38bkX_.gb108 - 13;
		gbcomp = (gbtemp*10) + 30;
		if( setsc7 == 1 )
			sconX_.scon[gbx-One][7-One] += gbcomp;
		sw38bkX_.gb108 = 0;
		/*                   IF THIS IS A MULTI LOADED VC, SET ELEMENTS IN THE VC */
		if( opadriX_.opadri[gb-One] <= -100 &&
			opadriX_.opadri[gb-One] >= -120 ){
			if( hpdopiX_.hfdopi[gb-One] >= HFPOLO &&
				hpdopiX_.hfdopi[gb-One] <= HFPOHI ){
				adr2 = hpdopiX_.hfdopi[gb-One] - HFPOL1;
				jz2 = hfdoaX_.hfpoad[adr2-One][HFPADX-One];
				for( m=1; m <= jz2; m++ ){
					if( sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][1-One] >= 0 ){
						gbx = hfdoaX_.sconhf[adr2-One][m-One];
						if( setsc7 == 1 )
							sconX_.scon[gbx-One][7-One] += gbcomp;
						}
					}
				}
			}
		}

L_5590:
	if( diagsX_.deepdi == 1 )
		{
		fprintf( _spec_fp, " SW38 %5d%5d%5d%5d%5d%5d\n", flowckX_.phrstr, 
		  flowckX_.phrlst, sw21bkX_.case_, sw36bkX_.relcas, sworkX_.phrhed[flowckX_.n6jim-One], sconlc );
		}

	return;
} /*end of function*/

