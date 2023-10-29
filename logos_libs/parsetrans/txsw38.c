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
	/*      FUNCTION:  ESTABLISHES PER, NUM, GEN, AND CASE
	 *      OF NON-HEAD NOUN (E.G. PAINT SHED). ALSO NON-NOUNS.
	 *      RIGHT ORIENTED SWITCH. NO FLAG SETTING, NO SWORK */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <fcrt.h>
#include "logoslib.h"
#include "trans.h"
#include "projexts.h"
#include <jbctrl.h>
#include "parsetrans_ext.h"

EXTERN struct t_sw38bkX_ {
	short int sw38n, compld, gb108, r38;
	}	sw38bkX_;


void /*FUNCTION*/ txsw38()
{
	static short jbc3 =3;
	static byte vtrfm1, vtrfmn;
	static short int adr2, jz2, m2, relptr, setsc7, setscn, subfcn, t25, x;
	static char pgmnam[9] = "TxSW38  ";
	static short m = 0;
	static short gb = 0;
	static short iz = 0;
	static short ms = 0;
	static short wc = 0;
	static short xx = 0;
	static short gbx = 0;
	static short sc1 = 0;
	static short sc7 = 0;
	static short tgp = 0;
	static short tmp = 0;
	static short ofl2 = 0;
	static short ofl3 = 0;
	static short sc13 = 0;
	static short sr38 = 0;
	static short temp = 0;
	static short xcas = 0;
	static short mcase = 0;
	static short retsw = 0;
	static short setp1 = 0;
	static short gbcomp = 0;
	static short gbtemp = 0;
	static short mrelcs = 0;
	static short sconlc = 0;

	vtrnX_.vtrn = vtrfX_.vtrf[vbdataX_.k3+2-One];
	vbdataX_.k3n = vbdataX_.k3 + 4;
	/*     WHAT SORT OF ITEM IS SECOND PARAMETER? */
	vtrfmn = vtrfmX_.vtrfm[vbdataX_.k3+2-One];
	/*     ESTABLISH ELEMENT TO RIGHT OF SW */
	vbdataX_.k3p1 = vtrfX_.vtrf[vbdataX_.k3+3-One];
	/*     WHAT SORT OF ITEM IS THIRD PARAMETER? */
	vtrfm1 = vtrfmX_.vtrfm[vbdataX_.k3+3-One];

	/*     SWITCH PARAMETER */

	if( vtrnX_.vtrn > 10 ){

		sw38bkX_.sw38n = 1;
		txload();
		if( errvrsX_.errlvl != 0 )
			return;
		sw38bkX_.sw38n = 0;

		/*          CONSTANT TO RIGHT */

		vbdataX_.k3p1 = vtrfX_.vtrf[vbdataX_.k3+1-One];
		/*     WHAT SORT OF ITEM IS THIS PARAMETER?       06/19/86  *R1530DSD */
		vtrfm1 = vtrfmX_.vtrfm[vbdataX_.k3+1-One];

		/*+           INHIBIT THE SETTING OF SCON7? 1=YES    *02/28/91*JAL* */
		setsc7 = 1;
		if( srcflgX_.srcflg == 2 ){
			//  test for portugues as it is based on spanish which is being tested
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
			/*-                                                     *02/28/91*JAL*
			 *+    IS THIS VTRF ELEMENT A RELATIVE PTR        06/19/86  *R1530DSD */
			if( vtrfm1 == vtrfvlX_.vtmrp ){
				sconX_.scon[prtscoX_.sct-One][7-One] = sconX_.scon[sworkX_.phrhed[im81X_.im81-vbdataX_.k3p1-One]-
				  One][7-One];
				}
			else if( vbdataX_.k3p1 > 0 ){
				sconX_.scon[prtscoX_.sct-One][7-One] = vbdataX_.k3p1;
				}
			else if( vbdataX_.k3p1 == -99 ){
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
	else{
		setp1 = 0;

		/*     SET THE FLAG R38 IF THIS IS A SPECIAL APPLICATION OF THE 38 SW
		 *     WHERE NO LOADING TAKES PLACE, BUT ALL THE OTHER FUNCTIONS ARE GO.
		 *     (ALSO, NO VC'S ARE HANDLED FOR SPECIAL APPLICATION OF SW38) */
		sw38bkX_.r38 = 0;
		/*+          IS VTRN ELEMENT A RELATIVE PTR       06/19/86  *R1530DSD */
		if( vtrfmn == vtrfvlX_.vtmrp )
			sw38bkX_.r38 = 1;
		/*-                                               06/19/86  *R1530DSD */

		/*   FOR COMP / SUP ADJS
		 *    THIS IS A GERMAN-ENGLISH FUNCTION
		 *    OFL3 = 8 IS COMPARATIVE.  OFL3 = 9 IS SUPERLATIVE. */
		if( srcflgX_.srcflg == 1 ){

			flowckX_.n6jim = flowckX_.im1 - vtrnX_.vtrn;
			if( sw38bkX_.r38 == 1 )
				flowckX_.n6jim = im81X_.im81 - vtrnX_.vtrn;
			sc13 = sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][13-One];

			/*   K3P1 = 50 IS FOR ALT WC OF COMP/SUP ADJS.
			 *   1ST DETERMINE WHETHER OR NOT TO LOAD ALT WC.
			 *   THIS DEPENDS ON SUPERSET AND OFL2 */
			if( vtrfX_.vtrf[vbdataX_.k3+1-One] == 50 ){
				/*   LOAD ALT WC???
				 *    SUPERSET 13 - YES */
				if( sc13 != 13 ){
					/*    SUPERSET 15 - NO */
					if( sc13 != 15 ){
						/*    SUPERSET 14   OFL2 = 4 - NO,  OTHERWISE YES. */
						ofl2 = sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][3-One];
						if( sc13 == 14 ){
							if( ofl2 != 4 )
								goto L_5504;
							/*    SUPERSET 16   OFL2 = 14 - YES,  OTHERWISE NO. */
							}
						else if( sc13 != 16 ){
							goto L_5504;
							}
						else if( ofl2 == 14 ){
							goto L_5504;
							}
						}

					vbdataX_.k3p1 = 0;
					/*+    UNDEFINED VTR TYPE                         06/19/86  *R1530DSD */
					vtrfm1 = vtrfvlX_.vtmnd;
					/*-                                               06/19/86  *R1530DSD */
					sw38bkX_.compld = 99;
					goto L_5510;
					}
				}

L_5504:
			ofl3 = sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][12-One];
			if( !(ofl3 != 8 && ofl3 != 9) ){

				if( vtrfX_.vtrf[vbdataX_.k3+1-One] != 50 ){

					if( sc13 < 13 || sc13 > 16 )
						goto L_5510;
					wc = sworkX_.swork[flowckX_.n6jim-One][1-One];
					if( !(wc == 14 || wc == 15) ){
						if( wc > 7 )
							goto L_5510;
						if( wc == 2 || wc == 3 )
							goto L_5510;
						}

					/*    FOR OFL3 = 13 - CHECK TARGET PAT.  IF = 30,31,32,33 OR 35
					 *    SET FLAG FOR SCON(7) SETTING AFTER RETURN FROM LOAD. */
					if( sc13 == 13 ){

						/*+               TARGPN NO LONGER EXISTS USE SC59      *10/31/91*JAL*
						 *     TGP = TARGPN(SCOLNK(SWORK(4,N6JIM))) */
						tgp = sconX_.scono[sconX_.scolnk[sworkX_.swork[flowckX_.n6jim-One][4-One]-
						  One]-One][59-SCONX1-One];
						/*-                                                     *10/31/91*JAL* */
						if( tgp < 30 || tgp > 35 )
							goto L_5510;
						if( tgp != 34 ){

							setp1 = ofl3 - 3;
							xx = 0;
							goto L_5506;
							}
						}
					}

				/*    LOAD MORE (-263) OR MOST (-330) INTO VC 110 OF THIS PHRBEG.
				 *    IF THERE IS A VC 110 USE IT.
				 *    IF NOT, SET A FLAG FOR THE LOAD PORTION (8660) TO USE. */
				if( ofl3 == 8 )
					xx = -263;
				if( ofl3 == 9 )
					xx = -330;

				/*   IS THERE IS A VC 110 IN THIS PHRBEG? */
L_5506:
				flowckX_.phrstr = sworkX_.phrbeg[flowckX_.n6jim-One];
				flowckX_.phrlst = sworkX_.phrend[flowckX_.n6jim-One];
				for( m=flowckX_.phrstr; m <= flowckX_.phrlst; m++ ){
					if( opadriX_.opadri[m-One] == -110 )
						goto L_5508;
					}

				sw38bkX_.compld = xx;
				if( sw38bkX_.r38 == 1 )
					sw38bkX_.compld = 0;
				if( sw38bkX_.r38 == 1 && diagsX_.longdi == 1 )
					{
					fprintf( _spec_fp, " SW38 IS TRYING TO LOAD AN ILLEGAL VC110 *‹*‹*‹*‹*‹*‹*\n" );
					}
				goto L_5510;

				/*   YES.  USE IT. */
L_5508:
				hpdopiX_.hfdopi[m-One] = xx;
				}
			}
L_5510:
		if( sw38bkX_.r38 != 1 ){
			elemld();
			if( errvrsX_.errlvl != 0 )
				return;
			/*-                                               06/19/86  *R1530DSD */


			/*     SW38 IS USED WITHOUT A LOAD. THEREFORE, SOME OF THE VARIABLES NORM
			 *     ESTABLISHED IN THE LOAD PORTION MUST BE DONE HERE. ALSO, A CHECK
			 *     TO SEE IF THE ALT. WC. WAS INSTALLED MUST BE DUPLICATED. */

			}
		else if( !(vbdataX_.k3p1 == 0 || vbdataX_.k3p1 > 4) ){
			flowckX_.n6jim = im81X_.im81 - vtrnX_.vtrn;
			flowckX_.phrstr = sworkX_.phrbeg[flowckX_.n6jim-One];
			flowckX_.phrlst = sworkX_.phrend[flowckX_.n6jim-One];
			for( m=flowckX_.phrstr; m <= flowckX_.phrlst; m++ ){
				if( opadriX_.sconpi[m-One] != 0 ){
					tmp = sconX_.scon[opadriX_.sconpi[m-One]-One][1-One];
					if( tmp >= 0 ){
						if( tmp < 99 || tmp > 120 )
							sconX_.scon[opadriX_.sconpi[m-One]-One][8-One] = vbdataX_.k3p1;
						/*                   IF THIS IS A MULTI LOADED VC, SET ELEMENTS IN THE VC */
						if( opadriX_.opadri[m-One] <= -100 && opadriX_.opadri[m-One] >= 
						  -120 ){
							if( hpdopiX_.hfdopi[m-One] >= HFPOLO && 
							  hpdopiX_.hfdopi[m-One] <= HFPOHI ){
								adr2 = hpdopiX_.hfdopi[m-One] - HFPOL1;
								jz2 = hfdoaX_.hfpoad[adr2-One][HFPADX-One];
								for( m2=1; m2 <= jz2; m2++ ){
									if( sconX_.scon[hfdoaX_.sconhf[adr2-One][m2-One]-
									  One][1-One] >= 0 )
										sconX_.scon[hfdoaX_.sconhf[adr2-One][m2-One]-
										  One][8-One] = vbdataX_.k3p1;
									}
								}
							}
						}
					}
				}
			}

		/*     SCON NOW ESTABLISHED FOR ELEMENT ON RIGHT */

		/*     TEST FOR LOCKED SCON */
		sconlc = sworkX_.phrhed[flowckX_.n6jim-One];
		vbdataX_.k3p1 = vtrfX_.vtrf[vbdataX_.k3+1-One];
		vtrfm1 = vtrfmX_.vtrfm[vbdataX_.k3+1-One];


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
			//  test for portugues as it is based on spanish which is being tested
			if( trgflgX_.trgflg == 3 || trgflgX_.trgflg == 4 || 
			    trgflgX_.trgflg == 5 || trgflgX_.trgflg == 6 ){
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
		/*-                                                     *02/28/91*JAL*
		 *        IF K3P1 = 50, PROCESSING IS COMPLETE */
		if( vbdataX_.k3p1 != 50 ){
			if( setp1 != 0 ){
				vbdataX_.k3p1 = setp1;
				/*        UNDEFINED VTRF TYPE */
				vtrfm1 = vtrfvlX_.vtmnd;
				}
			if( srcflgX_.srcflg == 1 && trgflgX_.trgflg == 5 ){
				if( sconX_.scono[sconX_.scolnk[sconlc-One]-One][45-SCONX1-One] == 9 ){
					sconX_.scon[sconlc-One][7-One] = 5;
					goto L_6150;
					}
				}
			/*-                                                        *R1954*GBA
			 *  SET GENDER IF ALTWC IS NOUN (WC 02 OR 01) */
			if( sconX_.scon[sconlc-One][8-One] == 2 ){
				if( sconX_.scon[sconlc-One][1-One] == 1 || sconX_.scon[sconlc-One][1-One] == 2 ){
					t25 = targ25X_.targ25[sconX_.scolnk[sworkX_.swork[flowckX_.n6jim-One][4-One]-One]-One];
					if( t25 != -1 ){
						for( ms=flowckX_.phrstr; ms <= flowckX_.phrlst; ms++ ){
							if( sconX_.scon[opadriX_.sconpi[ms-One]-One][1-One] > 0 )
								sconX_.scon[opadriX_.sconpi[ms-One]-One][4-One] = t25;
							/*                   IF THIS IS A MULTI LOADED VC, SET ELEMENTS IN THE VC */
							if( opadriX_.opadri[ms-One] <= -100 && 
							  opadriX_.opadri[ms-One] >= -120 ){
								if( hpdopiX_.hfdopi[ms-One] >= HFPOLO && 
								  hpdopiX_.hfdopi[ms-One] <= HFPOHI ){
									adr2 = hpdopiX_.hfdopi[ms-One] - HFPOL1;
									jz2 = hfdoaX_.hfpoad[adr2-One][HFPADX-One];
									for( m=1; m <= jz2; m++ ){
										if( sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][1-One] >= 0 )
											sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][4-One] = t25;
										}
									}
								}
							}
						targ25X_.targ25[sconX_.scolnk[sworkX_.swork[flowckX_.n6jim-One][4-One]-One]-One] = -1;
						}
					}
				}

			if( srcflgX_.srcflg == 1 ){

				if( vbdataX_.k3p1 >= 0 ){
					/*  DON'T RESET SCON(5) IF ALREADY SET TO 2 */
					if( sconX_.scon[sconlc-One][5-One] != 2 ){
						/*+ 89/08/22                                                 *R0GBA*GBA */
						if( sconX_.scon[sconlc-One][3-One] <= 3 ){
							/*- 89/08/22                                                 *R0GBA*GBA
							 *     WC 1,4,5,6,7 ONLY */
							iz = sconX_.scon[sconlc-One][1-One];
							if( !(iz <= 0 || iz > 7) ){
								if( !(iz == 2 || iz == 3) ){
									/*     FOR DECLENTION OF NOUNS BY NUMBER
									 *     IF K3P1 IS 1-4, K3P1 IS USED TO COMPUTE SCON(5)
									 *     IF K3P1 EQUALS 0, THE CASE FLAG IS USED. */
									if( vbdataX_.k3p1 > 0 ){
										ms = vbdataX_.k3p1;
										if( ms < 1 || ms > 4 )
											goto L_5880;
										}
									else if( sw21bkX_.case_ == 0 || 
									  sw21bkX_.case_ > 7 ){
										goto L_5880;
										}
									else if( sw21bkX_.case_ == 1 ){
										goto L_5880;
										}
									else{
										if( sw21bkX_.case_ == 5 ){
											/*     FOR CASE = 5,6,7 - TEST NFORM TABLE */
											sr38 = 87;
											formod(2,sr38,jbc3,flowckX_.n6jim,&retsw);
											if( retsw == 1 )
												goto L_5860;

											sr38 = 88;
											formod(2,sr38,jbc3, flowckX_.n6jim,&retsw);
											if( retsw == 1 )
												goto L_5860;
											if( retsw == 2 )
												goto L_5840;
											}
										else if( sw21bkX_.case_ != 6 ){
											if( sw21bkX_.case_ ==  7 )
												goto L_5800;
											ms = sw21bkX_.case_;
											goto L_5700;
											}

										sr38 = 86;
										formod(2,sr38,jbc3,flowckX_.n6jim,&retsw);
										if( retsw == 1 )
											goto L_5860;

										sr38 = 88;
										formod(2,sr38,jbc3,flowckX_.n6jim,&retsw);
										if( retsw == 1 )
											goto L_5860;
										if( retsw == 2 )
											goto L_5840;

L_5800:
										sr38 = 86;
										formod(2,sr38,jbc3,flowckX_.n6jim,&retsw);
										if( retsw != 1 ){

											sr38 = 87;
											formod(2,sr38,jbc3,flowckX_.n6jim,&retsw);
											if( retsw != 1 )
												goto L_5840;
											}

L_5860:
										sconX_.scon[sconlc-One][5-One] = 2;
										goto L_5870;

L_5840:
										sconX_.scon[sconlc-One][5-One] = 1;
										goto L_5870;
										}
L_5700:
									iz = nounsX_.gernum[ms-One][formsaX_.formsv[sconX_.scolnk[sworkX_.swork[flowckX_.n6jim-One][4-One]-
									  One]-One]-One];
									if( iz >= 1 && iz <= 99 )
										sconX_.scon[sconlc-One][5-One] = iz;
									/*   IF SCON(5) IS SET BY CASE FLAG - LOCK THE SCON
									 *   IF SUPERSET IS 13-16, DO NOT LOCK */
L_5870:
									x = x;
									/*+                                                        *R1954*GBA */
									if( srcflgX_.srcflg == 1 && trgflgX_.trgflg == 5 ){
										temp = sconX_.scon[sconlc-One][5-One];
										for( iz=flowckX_.phrstr; iz <= flowckX_.phrlst; iz++ ){
											if( sconX_.scon[opadriX_.sconpi[iz-One]-One][1-One] >= 0 )
												sconX_.scon[opadriX_.sconpi[iz-One]-One][5-One] = temp;
											/*                   IF THIS IS A MULTI LOADED VC, SET ELEMENTS IN THE VC */
											if( opadriX_.opadri[iz-One] <= -100 &&
												opadriX_.opadri[iz-One] >= -120 ){
												if( hpdopiX_.hfdopi[iz-One] >= HFPOLO &&
													hpdopiX_.hfdopi[iz-One] <= HFPOHI ){
												adr2 = hpdopiX_.hfdopi[iz-One] - HFPOL1;
												jz2 = hfdoaX_.hfpoad[adr2-One][HFPADX-One];
												for( m=1; m <= jz2; m++ ){
												if( sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-
												        One][1-One] >= 0 )
												    sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-
												        One][5-One] = temp;
												}
												}
												}
											}
										}
									/*-                                                        *R1954*GBA */
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

L_5880:
			;
			for( iz=flowckX_.phrstr; iz <= flowckX_.phrlst; iz++ ){
				if( sconX_.scon[opadriX_.sconpi[iz-One]-One][1-One] >  0 ){
					sconX_.scon[opadriX_.sconpi[iz-One]-One][4-One] = sconX_.scon[sconlc-One][4-One];
					sconX_.scon[opadriX_.sconpi[iz-One]-One][5-One] = sconX_.scon[sconlc-One][5-One];
					if( srcflgX_.srcflg == 1 ){
						if( sconX_.scon[opadriX_.sconpi[iz-One]-One][1-One] == 1 &&
							vbdataX_.k3p1 > 0 )
							sconX_.scon[opadriX_.sconpi[iz-One]-One][5-
							  One] = nounsX_.gernum[vbdataX_.k3p1-One][formsaX_.formsv[sconX_.scolnk[opadriX_.sconpi[iz-One]-
							  One]-One]-One];
						}
					sconX_.scon[opadriX_.sconpi[iz-One]-One][6-One] = sconX_.scon[sconlc-One][6-One];
					/*                   IF THIS IS A MULTI LOADED VC, SET ELEMENTS IN THE VC */
					if( opadriX_.opadri[iz-One] <= -100 && opadriX_.opadri[iz-One] >= 
					  -120 ){
						if( hpdopiX_.hfdopi[iz-One] >= HFPOLO && hpdopiX_.hfdopi[iz-One] <= 
						  HFPOHI ){
							adr2 = hpdopiX_.hfdopi[iz-One] - HFPOL1;
							jz2 = hfdoaX_.hfpoad[adr2-One][HFPADX-One];
							for( m=1; m <= jz2; m++ ){
								if( sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][1-One] >= 0 ){
									sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][4-One] = sconX_.scon[sconlc-One][4-One];
									sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][5-One] = sconX_.scon[sconlc-One][5-One];
									if( srcflgX_.srcflg == 1 ){
										if( sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][1-One] == 1 && vbdataX_.k3p1 >  0 )
											sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][5-One] = nounsX_.gernum[vbdataX_.k3p1-One][formsaX_.formsv[sconX_.scolnk[hfdoaX_.sconhf[adr2-One][m-One]-
											  One]-One]-One];
										}
									sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][6-One] = sconX_.scon[sconlc-One][6-One];
									}
								}
							}
						}
					}
				}

			if( sconX_.scon[sconlc-One][9-One] != 7 ){
				if( sconX_.scon[sconlc-One][1-One] > 0 ){

					if( vtrfm1 == vtrfvlX_.vtmrp ){
						temp = sconX_.scon[sworkX_.phrhed[im81X_.im81-vbdataX_.k3p1-One]-One][7-One];
						}
					else if( vbdataX_.k3p1 < 0 ){

						/*     FUNCTION 5 ***** */
						if( vbdataX_.k3p1 == -99 ){
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
						else if( vbdataX_.k3p1 == -98 ){
							/*     ELSE UNDEFINED FUNCTION, SO MESSAGE */

							/**** OBTAIN CASE FROM HEDCAS*** */

							xcas = sw25bkX_.hedcas;
							if( sw25bkX_.hedcas == 0 )
								xcas = sw21bkX_.case_;
							temp = xcas;
							}
						else{
							goto L_6181;
							}
						}
					else if( vbdataX_.k3p1 == 0 ){
						goto L_6140;
						}
					else{
						temp = vbdataX_.k3p1;
						}
					/*     FUNCTION 6 ****
					 *     RESET PHRBEG START AND STOP FOR RELATIVE POINTER */
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
							if( !((srcflgX_.srcflg == 1 && sc7 > 0) && sc1 == 4) ){
								/*          EG ONLY - SET FLAG WHEN SCON 7 OF VC 108BETWEEN 14 AND 17 */
								if( ((sc1 == 108 && srcflgX_.srcflg == 2) && sc7 >= 14) && sc7 <= 17 )
									sw38bkX_.gb108 = sc7;
								if( setsc7 == 1 )
									sconX_.scon[opadriX_.sconpi[iz-One]-One][7-One] = temp;
								/*                   IF THIS IS A MULTI LOADED VC, SET ELEMENTS IN THE VC */
								if( opadriX_.opadri[iz-One] <= -100 && opadriX_.opadri[iz-One] >= -120 ){
									if( hpdopiX_.hfdopi[iz-One] >= 
									  HFPOLO && hpdopiX_.hfdopi[iz-One] <= HFPOHI ){
										adr2 = hpdopiX_.hfdopi[iz-One] - HFPOL1;
										jz2 = hfdoaX_.hfpoad[adr2-One][HFPADX-One];
										for( m=1; m <= jz2; m++ ){
											sc1 = sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][1-One];
											sc7 = sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][7-One];
											if( sc1 >= 0 ){
												if( !((srcflgX_.srcflg == 1 && sc7 > 0) && 
												  sc1 == 4) ){
												if( ((sc1 == 108 && srcflgX_.srcflg == 2) && sc7 >= 14) && sc7 <= 17 )
												sw38bkX_.gb108 = sc7;
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
					/*+                                                        *R1954*GBA */
					if( srcflgX_.srcflg == 1 ){
						for( ms=flowckX_.phrstr; ms <= flowckX_.phrlst; ms++ ){
							if( sconlc != opadriX_.sconpi[ms-One] ){
								sc1 = sconX_.scon[opadriX_.sconpi[ms-One]-One][1-One];
								if( sc1 == 1 || 
									(trgflgX_.trgflg == 5 && (sc1 == 4 || sc1 == 14)) )
									sconX_.scon[opadriX_.sconpi[ms-One]-
									  One][5-One] = nounsX_.gernum[temp-One][formsaX_.formsv[sconX_.scolnk[opadriX_.sconpi[ms-One]-
									  One]-One]-One];
								/*                   IF THIS IS A MULTI LOADED VC, SET ELEMENTS IN THE VC */
								if( opadriX_.opadri[ms-One] <= -100 && 
								  opadriX_.opadri[ms-One] >= -120 ){
									if( hpdopiX_.hfdopi[ms-One] >= HFPOLO && hpdopiX_.hfdopi[ms-One] <= HFPOHI ){
										adr2 = hpdopiX_.hfdopi[ms-One] - HFPOL1;
										jz2 = hfdoaX_.hfpoad[adr2-One][HFPADX-One];
										for( m=1; m <= jz2; m++ ){
											sc1 = sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][1-One];
											if( sc1 == 1 || 
												(trgflgX_.trgflg == 5 && (sc1 == 4 || sc1 == 14)) )
												sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-
												  One][5-One] = nounsX_.gernum[temp-One][formsaX_.formsv[sconX_.scolnk[hfdoaX_.sconhf[adr2-One][m-One]-
												  One]-One]-One];
											}
										}
									}
								}
							}
						}
					if( temp == vbdataX_.k3p1 ){

						/*     FUNCTION 4 ***** */
						if( srcflgX_.srcflg == 1 )
							sw21bkX_.case_ = 0;
						}
					}
				else if( vtrfm1 == vtrfvlX_.vtmrp ){
					/*-                                               06/19/86  *R1530DSD
					 *       K3P1 IS REL PTR */
					temp = sconX_.scon[sworkX_.phrhed[im81X_.im81-vbdataX_.k3p1-One]-
					  One][7-One];
					for( iz=flowckX_.phrstr; iz <= flowckX_.phrlst; iz++ ){
						if( sconX_.scon[opadriX_.sconpi[iz-One]-One][1-
						  One] > 0 )
							sconX_.scon[opadriX_.sconpi[iz-One]-One][1-
							  One] = -sconX_.scon[opadriX_.sconpi[iz-One]-
							  One][1-One];
						if( !((srcflgX_.srcflg == 1 && (sconX_.scon[opadriX_.sconpi[iz-One]-
						  One][7-One] > 0)) && (sconX_.scon[opadriX_.sconpi[iz-One]-
						  One][1-One] == 4 || sconX_.scon[opadriX_.sconpi[iz-One]-
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
										if( sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][1-One] >= 0 ){
											if( !((srcflgX_.srcflg == 
											  1 && (sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][7-One] > 0)) && 
											  (sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][1-One] == 4 || 
											  sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][1-One] == -4)) ){
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
				}
			}
		}
	/*      IF K3P1(PARM) IS ZERO OR REL POINTER */

	/*    ENGLISH SOURCE ONLY - RESET SCON 7, VC 108 */

L_6140:
	if( sw38bkX_.gb108 == 0 || sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][1-One] != 1 )
		goto L_6150;
	flowckX_.phrstr = sworkX_.phrbeg[flowckX_.n6jim-One];
	flowckX_.phrlst = sworkX_.phrend[flowckX_.n6jim-One];
	for( gb=flowckX_.phrstr; gb <= flowckX_.phrlst; gb++ ){
		if( opadriX_.opadri[gb-One] == -108 )
			goto L_55092;
		}
	goto L_6181;
L_55092:
	gbx = opadriX_.sconpi[gb-One];
	gbtemp = sw38bkX_.gb108 - 13;
	gbcomp = (gbtemp*10) + 30;
	if( setsc7 == 1 )
		sconX_.scon[gbx-One][7-One] += gbcomp;
	sw38bkX_.gb108 = 0;
	/*                   IF THIS IS A MULTI LOADED VC, SET ELEMENTS IN THE VC */
	if( opadriX_.opadri[gb-One] <= -100 && opadriX_.opadri[gb-One] >= 
	  -120 ){
		if( hpdopiX_.hfdopi[gb-One] >= HFPOLO && hpdopiX_.hfdopi[gb-One] <= HFPOHI ){
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
	goto L_6150;

L_6181:
	if( diagsX_.deepdi == 1 )
		{
		fprintf( _spec_fp, " SW38 ERROR, UNDEFINED ARG1 %5d\n", vbdataX_.k3p1 );
		}

L_6150:
	if( diagsX_.deepdi == 1 )
		{
		fprintf( _spec_fp, " SW38 %5d%5d%5d%5d%5d%5d\n", flowckX_.phrstr, 
		  flowckX_.phrlst, sw21bkX_.case_, sw36bkX_.relcas, sworkX_.phrhed[flowckX_.n6jim-One], 
		  sconlc );
		}
	return;
} /*end of function*/

