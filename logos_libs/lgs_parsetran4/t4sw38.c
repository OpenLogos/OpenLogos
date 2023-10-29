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
	 *      03/06/91 *JAL*:  ADD CONDITION TO SET SCON7 IF SCON1 = 12
	 *      02/29/91 *JAL*:  SET CONDITIONS LIMITING THE SETTING OF CASE
	 *           (I.E.SCON7) FOR ENGLISH SOURCE, FRN,SPA,AND ITA TARGETS.
	 *      04/17/87 *R1685RKH*  CHANGE T1-4 SWORK LIMIT FROM 50 TO
	 *      08/24/86 *R1561DSD: 100 SCONS
	 *      07/18/86 *R1530DSD: EXPAND RELATIVE POINTERS FOR STRETCH
	 *      12/02/85 */
	/*+                    FLAG TO INHIBIT SETTING OF SCON7 *02/28/91*JAL* */
	/*-                                                     *02/28/91*JAL* */
	/*+    VTRFM REMEMBERS VTRF 'TYPES'  FROM VTRFWR  06/06/86  *R1530DSD */

	/*      FUNCTION - ESTABLISHES
	 *      PER,NUM,GEN,CASE OF NON-HEAD NOUN (E.G.
	 *      PAINT SHED). ALSO NON-NOUNS. A RIGHT ORIENTED SWITCH. NO FLAG
	 *      SETTING, NO NSWOR */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <fcrt.h>
#include "project.h"
#include "logoslib.h"
#include "trans.h"
#include "projexts.h"
#include "parsetrans.h"
#include <string.h>
#include <jbctrl.h>



void /*FUNCTION*/ t4sw38()
{
	static byte vtrfmn;
	static short int adr2, jz2, m, setsc7, t25;
	static char pgmnam[9] = "T4SW38  ";
	static short gb = 0;
	static short iz = 0;
	static short ms = 0;
	static short gbx = 0;
	static short sc1 = 0;
	static short sc7 = 0;
	static short ofl2 = 0;
	static short sc13 = 0;
	static short temp = 0;
	static short mcase = 0;
	static short gbcomp = 0;
	static short gbtemp = 0;
	static short mrelcs = 0;
	static short sconlc = 0;

	vtrnX_.vtrn = vtrfX_.vtrf[vbdataX_.k3+2-One];
	vbdataX_.k3n = vbdataX_.k3 + 4;

	/*     ESTABLISH ELEMENT TO RIGHT OF SW */
	vbdataX_.k3p1 = vtrfX_.vtrf[vbdataX_.k3+3-One];

	/*     SWITCH PARAMETER */
	if( vtrnX_.vtrn > 10 ){

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
					sconX_.scon[prtscoX_.sct-One][7-One] = sw14bkX_.case_;
					}
				else{
					sconX_.scon[prtscoX_.sct-One][7-One] = sw36bkX_.relcas;
					}
				}
			else if( vbdataX_.k3p1 == -98 ){
				if( sw25bkX_.hedcas == 0 ){

					sconX_.scon[prtscoX_.sct-One][7-One] = sw14bkX_.case_;
					}
				else{
					sconX_.scon[prtscoX_.sct-One][7-One] = sw25bkX_.hedcas;

					}
				}
			}
		}
	else{
		if( vtrfX_.vtrf[vbdataX_.k3+1-One] == 50 ){

			/*   FUNCTION 50 FOR ALT WC OF COMP/SUP ADJS.
			 *    OFL3 = 8 IS COMPARATIVE.  OFL3 = 9 IS SUPERLATIVE.
			 *    THIS IS A GERMAN - ENGLISH FUNCTION. */
			if( srcflgX_.srcflg == 1 ){
				flowckX_.n6jim = flowckX_.im1 - vtrnX_.vtrn;
				sc13 = sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-
				  One][13-One];

				/*   DETERMINE WHETHER TO LOAD ALT WC. THIS DEPENDS ON SUPERSET AND OFL2
				 *     LOAD ALTWC??
				 *   SUPERSET 13 - YES */
				if( sc13 != 13 ){
					/*   SUPERSET 15 - NO */
					if( sc13 != 15 ){
						/*   SUPERSET 14   OFL2 = 4 - NO, OTHERWISE YES. */
						ofl2 = sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-
						  One][3-One];
						if( sc13 == 14 ){
							if( ofl2 != 4 )
								goto L_4084;
							/*   SUPERSET 16   OFL2 = 14 - YES, OTHERWISE NO. */
							}
						else if( sc13 != 16 ){
							goto L_4084;
							}
						else if( ofl2 == 14 ){
							goto L_4084;
							}
						}

					vbdataX_.k3p1 = 0;
					}

L_4084:
				sw38bkX_.compld = 99;
				}
			}

		elemld();
		if( errvrsX_.errlvl != 0 )
			return;

		/*     SCON NOW ESTABLISHED FOR ELEMENT ON RIGHT */

		/*     TEST FOR LOCKED SCON */
		sconlc = sworkX_.phrhed[flowckX_.n6jim-One];
		vbdataX_.k3p1 = vtrfX_.vtrf[vbdataX_.k3+1-One];
		/*+    IS THIS VTRF ELEMENT A RELATIVE PTR?       06/06/86  *R1530DSD */
		vtrfmn = vtrfmX_.vtrfm[vbdataX_.k3+1-One];
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
						sconX_.scon[sconlc-One][13-One] ==  16 )
						setsc7 = 1;
					}
				}
			}

		if( vbdataX_.k3p1 != 50 ){


			/*  SET GENDER IF ALTWC IS VERB */
			if( sconX_.scon[sconlc-One][8-One] == 2 ){
				if( sconX_.scon[sconlc-One][1-One] == 1 || 
					sconX_.scon[sconlc-One][1-One] == 2 ){
					t25 = targ25X_.targ25[sconX_.scolnk[sworkX_.swork[flowckX_.n6jim-One][4-One]-
					  One]-One];
					if( t25 != -1 ){
						for( ms=flowckX_.phrstr; ms <= flowckX_.phrlst; ms++ ){
							if( sconX_.scon[opadriX_.sconpi[ms-One]- One][1-One] > 0 ){
								sconX_.scon[opadriX_.sconpi[ms-One]- One][4-One] = t25;
								if( sconX_.scon[sconlc-One][1-One] == 2 )
									sconX_.scon[opadriX_.sconpi[ms-One]-One][5-One] = 1;
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
										  One][1-One] > 0 ){
											sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-
											  One][4-One] = t25;
											if( sconX_.scon[sconlc-One][1-One] == 
											  2 )
												sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-
												  One][5-One] = 1;
											}
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

				if( !(vbdataX_.k3p1 < 1 || vbdataX_.k3p1 > 4) ){

					if( sconX_.scon[sconlc-One][5-One] != 2 ){
						/*+ 89/08/22                                                 *R0GBA*GBA */
						if( sconX_.scon[sconlc-One][3-One] <= 3 ){
							/*- 89/08/22                                                 *R0GBA*GBA
							 *             WC 1 5 6 7 AND 8 ONLY */
							iz = sconX_.scon[sconlc-One][1-One];
							if( iz > 0 ){
								if( iz <= 8 ){
									if( !(iz >= 2 && iz <= 4) ){

										/*             FOR DECLENTION OF NOUNS BY NUMBER */
										iz = nounsX_.gernum[vbdataX_.k3p1-One][formsaX_.formsv[sconX_.scolnk[sworkX_.swork[flowckX_.n6jim-One][4-One]-
										  One]-One]-One];
										if( iz >= 1 && iz <= 99 )
											sconX_.scon[sconlc-One][5-One] = iz;
										}
									}
								}
							}
						}
					}
				}

			if( sconX_.scon[sconlc-One][9-One] != 7 ){
				if( sconX_.scon[sconlc-One][1-One] > 0 ){
					if( vbdataX_.k3p1 < 0 ){
						/*     FUNCTION 5 *****
						 *+                                               06/06/86  *R1530DSD */
						if( vtrfmn == vtrfvlX_.vtmrp ){
							temp = sconX_.scon[sworkX_.phrhed[im81X_.im81-vbdataX_.k3p1-One]-
							  One][7-One];
							/*-                                               06/06/86  *R1530DSD */
							}
						else if( vbdataX_.k3p1 == -99 ){
							mrelcs = sw36bkX_.relcas;
							mcase = sw14bkX_.case_;
							if( trgflgX_.trgflg == 1 ){
								if( sw36bkX_.rel == 0 ){
									if( sw14bkX_.case_ == 5 )
										mcase = 3;
									if( sw14bkX_.case_ == 6 || sw14bkX_.case_ ==  7 )
										mcase = 2;
									temp = mcase;
									}
								else{
									if( sw36bkX_.relcas == 5 )
										mrelcs = 3;
									if( sw36bkX_.relcas == 6 || sw36bkX_.relcas ==  7 )
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
					else if( vbdataX_.k3p1 != 0 ){
						temp = vbdataX_.k3p1;
						}
					else if( sconX_.scon[opadriX_.sconpi[iz-One]-One][7-One] != 0 ){
						goto L_4500;
						}
					else if( sw36bkX_.rel > 1 ){
						goto L_4500;
						}
					else{
						temp = sw14bkX_.case_;
						if( sw36bkX_.rel == 1 )
							temp = sw36bkX_.relcas;
						}

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
							if( !((srcflgX_.srcflg == 1 && 
								sconX_.scon[opadriX_.sconpi[iz-One]-One][7-One] > 0) &&
								sconX_.scon[opadriX_.sconpi[iz-One]-One][1-One] == 4) ){
								/*              EG ONLY  SET FLAG IF VC108, AND SCON 7BETWEEN 14 AND 17 */
								if( ((srcflgX_.srcflg == 2 && sc1 ==  108) && 
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
											sc1 = sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-
											  One][1-One];
											sc7 = sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-
											  One][7-One];
											if( sc1 >= 0 ){
												if( ms == 2 ){
												if( sc1 >= 99 && sc1 <= 120 )
												continue;
												}
												if( !((srcflgX_.srcflg == 
												  1 && sconX_.scon[opadriX_.sconpi[iz-One]-
												  One][7-One] > 0) && 
												  sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-
												  One][1-One] == 4) ){
												if( ((srcflgX_.srcflg == 
												  2 && sc1 == 108) && 
												  sc7 >= 14) && sc7 <= 17 )
												sw38bkX_.gb108 = sc7;
												if( setsc7 == 1 )
												sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-
												  One][7-One] = temp;
												}
												}
											}
										}
									}
								}
							}
						}
					if( srcflgX_.srcflg == 1 ){
						/*        COMPUTE SCON5 FOR ALL NON-HEAD ELEMENTS OF THE PHRASE FROM
						 *        THE NEW CASE VALUE (=SCON(7)) */
						for( ms=flowckX_.phrstr; ms <= flowckX_.phrlst; ms++ ){
							if( sconlc != opadriX_.sconpi[ms-One] ){
								sc1 = sconX_.scon[opadriX_.sconpi[ms-One]-
								  One][1-One];
								if( !(sc1 != 1 || sc1 < 0) ){
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
												if( sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-
												  One][1-One] >= 0 ){
												sc1 = sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-
												  One][1-One];
												if( !(sc1 != 1 || sc1 < 0) )
												sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-
												  One][5-One] = nounsX_.gernum[temp-One][formsaX_.formsv[sconX_.scolnk[hfdoaX_.sconhf[adr2-One][m-One]-
												  One]-One]-One];
												}
												}
											}
										}
									}
								}
							}
						}
					/*+                                               06/06/86  *R1530DSD */
					}
				else if( vtrfmn == vtrfvlX_.vtmrp ){
					/*       WAS: IF (K3P1 .LE. -72 .AND. K3P1 .GE. -90) GOTO 4265
					 *-                                               06/06/86  *R1530DSD */

					temp = sconX_.scon[sworkX_.phrhed[im81X_.im81-vbdataX_.k3p1-One]-
					  One][7-One];
					for( iz=flowckX_.phrstr; iz <= flowckX_.phrlst; iz++ ){
						if( sconX_.scon[opadriX_.sconpi[iz-One]-One][1-One] > 0 )
							sconX_.scon[opadriX_.sconpi[iz-One]-One][1-
							  One] = -sconX_.scon[opadriX_.sconpi[iz-One]-One][1-One];
						if( !((sconX_.scon[opadriX_.sconpi[iz-One]-
						  One][7-One] > 0 && srcflgX_.srcflg == 1) && 
						  (sconX_.scon[opadriX_.sconpi[iz-One]-One][1-
						  One] == 4 || sconX_.scon[opadriX_.sconpi[iz-One]-
						  One][1-One] == -4)) ){
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
										if( sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-
										  One][1-One] >= 0 ){
											sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-
											  One][1-One] = -sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-
											  One][1-One];
											if( !((sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-
											  One][7-One] > 0 && srcflgX_.srcflg == 1) && (sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-
											  One][1-One] == 4 || 
											  sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-
											  One][1-One] == -4)) ){
												if( setsc7 == 1 )
												sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-
												  One][7-One] = temp;
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

	/*       IF K3P1(PARM) IS ZERO OR REL POINTER */

	/*    ENGLISH SOURCE ONLY - RESET SCON 7, VC 108 OF NOUN FOR FPRINT */

L_4500:
	if( !(sw38bkX_.gb108 == 0 || 
		sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][1-One] != 1) ){
		flowckX_.phrstr = sworkX_.phrbeg[flowckX_.n6jim-One];
		flowckX_.phrlst = sworkX_.phrend[flowckX_.n6jim-One];
		for( gb=flowckX_.phrstr; gb <= flowckX_.phrlst; gb++ ){
			if( opadriX_.opadri[gb-One] == -108 )
				goto L_55022;
			}
		goto L_6150;
L_55022:
		gbx = opadriX_.sconpi[gb-One];
		gbtemp = sw38bkX_.gb108 - 13;
		gbcomp = (gbtemp*10) + 30;
		if( setsc7 == 1 )
			sconX_.scon[gbx-One][7-One] += gbcomp;
		sw38bkX_.gb108 = 0;
		/*                   IF THIS IS A MULTI LOADED VC, SET ELEMENTS IN THE VC */
		if( opadriX_.opadri[gb-One] <= -100 && opadriX_.opadri[gb-One] >= -120 ){
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

L_6150:
	if( diagsX_.longdi == 1 )
		{
		fprintf( _spec_fp, " SW38 %5d%5d%5d%5d%5d%5d\n", flowckX_.phrstr, 
		  flowckX_.phrlst, sw14bkX_.case_, sw36bkX_.relcas, sworkX_.phrhed[flowckX_.n6jim-One], 
		  sconlc );
		}

	return;
} /*end of function*/

