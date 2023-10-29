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

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <logos_include_res_pt/fcrt.h>
#include "project.h"
#include "logoslib.h"
#include "trans.h"
#include "projexts.h"
#include "parsetrans.h"
#include <string.h>
#include <logos_include_res_pt/jbctrl.h>

EXTERN struct t_pluralX_ {
	short int plural[14];
	}	pluralX_;
EXTERN struct t_sw14bkX_ {
	short int gen, num, per;
	}	sw14bkX_;
EXTERN struct t_sw36bkX_ {
	short int det, rel, relcas, relgen, relnum, relper;
	}	sw36bkX_;
EXTERN struct t_swkadX_ {
	short int swkad;
	}	swkadX_;
EXTERN struct t_isizeX_ {
	short int isize;
	}	isizeX_;
EXTERN struct t_sw38bkX_ {
	short int adjnp, sw38n, vnum;
	}	sw38bkX_;



void /*FUNCTION*/ t1sw38()
{
		/*pointer COMMON transls*/
	struct  {
		short int k7, oflad, n3;
		}	*_diacbX_ = (void*)&diacbX_;
	struct  {
		short int vtr[26];
		}	*_vtrX_ = (void*)&vtrX_;
	struct  {
		short int im1, i3save, i3str, n6, n6jim, n6jims, phrstr;
		}	*_flowckX_ = (void*)&flowckX_;
	struct  {
		short int hedcas, hedgen, hednum, hedper, hedsc3, ngen, nnum, 
		  relngn, relnnm, relnpr, sw25n, tw25;
		}	*_sw25bkX_ = (void*)&sw25bkX_;
		/*end of pointer COMMON transls*/
	static short zero = 0;
	static char pgmnam[9] = "T1SW38  ";
	static short ms = 0;
	static short om = 0;
	static short s5 = 0;
	static short wc = 0;
	static short xx = 0;
	static short of3 = 0;
	static short om1 = 0;
	static short sc13 = 0;
	static short sr38 = 0;
	static short tmp2 = 0;
	static short formx = 0;
	static short retsw = 0;
	static short setp1 = 0;
	static short temp1 = 0;
	static short sconlc = 0;

	/*   ***** BEGINNING OF -38 SWITCH ***** */
	/*         FUNCTION:  ESTABLISHES PER, NUM, GEN, CASE OF
	 *         NON-HEAD NOUNS AND NON-NOUNS.
	 *         A RIGHT ORIENTED SWITCH.  NO FLAG SETTING, NO NSWORK */
	vtrnX_.vtrn = vtrfX_.vtrf[vbdataX_.k3+2-One];
	vbdataX_.k3n = vbdataX_.k3 + 4;
	setp1 = 0;

	/*   ESTABLISH ELEMENT TO RIGHT OF SW */
	vbdataX_.k3p1 = vtrfX_.vtrf[vbdataX_.k3+3-One];

	if( vtrnX_.vtrn >= 100 ){

		/*   NOT AN ELEMENT TO RIGHT, IT IS A VARIABLE CONSTANT */
		sw38bkX_.sw38n = 1;
		t1load();
		if( errvrsX_.errlvl != 0 )
			return;
		sw38bkX_.sw38n = 0;
		}
	else{

		/*   GERMAN SOURCE FUNCTION FOR COMPARATIVE / SUPERLATIVE ADJ.
		 *   (WC 1,4,5,6,7 SUPERSET 13)
		 *   THIS MUST BE DONE BEFORE LOAD OF ELEMENT TO THE RIGHT */

		if( srcflgX_.srcflg == 1 ){

			_flowckX_->n6jim = _flowckX_->im1 - vtrnX_.vtrn;
			sc13 = sconX_.scon[_flowckX_->n6jim-One][13-One];

			if( vtrfX_.vtrf[vbdataX_.k3+1-One] == 50 ){

				/*   CHECK TO SEE IF CONSTANT 233 SHOULD BE LOADED
				 *   AFTER THE ELEMENT TO THE RIGHT. THIS IS FOR
				 *   ' LY' TYPE CONSTANTS. SET FLAG OM TO BE THE HEAD AT
				 *   THE END OF THE -38SW */

				if( sc13 == 15 )
					om = 1;

				/*   K3P1 = 50 IS FOR ALT WC OF COMP / SUP ADJS */

				/*   1ST CHECK WHETHER OR NOT TO ALLOW ALT WC.
				 *   SUPERSET = 13 - YES */
				if( sc13 != 13 ){

					/*   SUPERSET = 15 - NO */
					if( sc13 != 15 ){

						/*   SUPERSET = 14   TMP2 - 4 - NO, OTHERWISE YES */
						tmp2 = ofl2X_.ofl2i[sconX_.scolnk[_flowckX_->n6jim-One]-One];
						if( sc13 == 14 ){
							if( tmp2 != 4 )
								goto L_60;

							/*   SUPERSET = 16   TMP2 = 14 - YES, OTHERWISE NO */
							}
						else if( sc13 != 16 ){
							goto L_60;
							}
						else if( tmp2 == 14 ){
							goto L_60;
							}
						}

					vbdataX_.k3p1 = 0;
					}
				}

L_60:
			of3 = sconX_.scon[_flowckX_->n6jim-One][12-One];
			if( !(of3 != 8 && of3 != 9) ){

				if( vtrfX_.vtrf[vbdataX_.k3+1-One] != 50 ){
					if( sconX_.scon[_flowckX_->n6jim-One][13-One] < 13 ||
						sconX_.scon[_flowckX_->n6jim-One][13-One] > 16 )
						goto L_160;
					wc = swork1X_.swork1[_flowckX_->n6jim-One][dct2X_.dct2[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
					  One]-One]-One];
					if( !(wc == 14 || wc == 15) ){
						if( wc > 7 )
							goto L_160;
						if( wc == 2 || wc == 3 )
							goto L_160;
						}


					/*   FOR SUPERSET 14,15,16 - NO NEED TO CHECK TARGET PAT */

					if( sconX_.scon[_flowckX_->n6jim-One][13-One] == 13 ){

						/*+            REPLACE TARGPN WITH SC59                 *10/31/91*JAL*
						 *     IF (TARGPN(SCOLNK(N6JIM)) .LT. 30 .OR.
						 *    *    TARGPN(SCOLNK(N6JIM)) .GT. 35) GOTO 160
						 *     IF (TARGPN(SCOLNK(N6JIM)) .EQ. 34) GOTO 100 */
/*5-11-98						if( sconX_.scono[sconX_.scolnk[_flowckX_->n6jim-One]-
						    One][59-SCONX1-One] < 30 ||
						    sconX_.scono[sconX_.scolnk[_flowckX_->n6jim-One]-
						    One][59-SCONX1-One] > 35 )
							goto L_160;
*/
						if( sconX_.scono[sconX_.scolnk[_flowckX_->n6jim-One]-One][59-SCONX1-One] != 34 ){

							/*   FOR PAT 31, 32, 33, 35 - SET SETP1 TO 5 OR 6
							 *   SETP1 WILL BE USED TO SET K3P1 ON RETURN FROM SCON LOAD. */

							setp1 = of3 - 3;
							xx = 0;
							goto L_120;
							}
						}
					}

				/*   FOR PAT 34 - LOAD 263 (MORE) OR 330 (MOST) INTO OPADRO */

				if( of3 == 8 )
					xx = -263;
				if( of3 == 9 )
					xx = -330;

L_120:
				prtscoX_.sct += 1;
				/*+                                         *B0305JGB */
				if( prtscoX_.sct > SCONY ){
					prtscoX_.sct = SCONY;
					prtscoX_.scterr += 1;
					if( opswX_.sw[3-One] == 1 || opswX_.sw[10-One] == 1 )
						{
						fprintf( _spec_fp, " T1SW38,OVERLOADING SCON ARRAY,SCTERR =%4d\n", 
						  prtscoX_.scterr );
						}
					if( prtscoX_.scterr == 1 )
						errlog(pgmnam,140,500,13);
					return;
					}
				else{
					/*-                                         *B0305JGB */
					zapit(&sconX_.scon[prtscoX_.sct-One][1-One],40,
					  (byte)zero);
					sconX_.scon[prtscoX_.sct-One][1-One] = 110;
					sconX_.scon[prtscoX_.sct-One][2-One] = -(xx);
					_diacbX_->n3 += 1;
					opadroX_.opadro[_diacbX_->n3-One] = -110;
					hpdopoX_.hfdopo[_diacbX_->n3-One] = xx;
					opadroX_.sconpo[_diacbX_->n3-One] = prtscoX_.sct;
					}
				}
			}

L_160:
		elemld();
		if( errvrsX_.errlvl != 0 )
			return;
		}

	/*   SCON NOW ESTABLISHED FOR ELEMENT ON RIGHT
	 *   TEST FOR LOCKED SCON, EXIT IF LOCKED */

	sconlc = _flowckX_->n6jim;
	if( sconX_.scon[sconlc-One][1-One] >= 1 ){

		if( !(srcflgX_.srcflg == 1 && sconX_.scon[sconlc-One][3-One] > 3) ){
			vbdataX_.k3p1 = vtrfX_.vtrf[vbdataX_.k3+1-One];
			if( vbdataX_.k3p1 != 50 ){
				if( setp1 != 0 )
					vbdataX_.k3p1 = setp1;
				_flowckX_->phrstr = supresX_.n3sv + 1;

				if( srcflgX_.srcflg == 2 ){

					/*   ENGLISH SOURCE FUNCTION - */

					if( sconX_.scon[sconlc-One][1-One] == 1 ){

						xx = sconX_.scon[sconlc-One][13-One];
						if( !((xx == 13 || xx == 15) || xx == 16) ){
							if( swork1X_.swork1[_flowckX_->n6jim-One][dct3X_.dct3[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
							  One]-One]-One] != 13 )
								goto L_240;
							}

						if( sw36bkX_.det == 0 )
							sconX_.scon[sconlc-One][3-One] = 9;
						if( sw36bkX_.det == 2 )
							sconX_.scon[sconlc-One][3-One] = 6;
						}
					}

L_240:
				if( sconX_.scon[sconlc-One][1-One] != 1 && sconX_.scon[sconlc-One][1-One] != 
				  7 ){

					/*   IF WC OF ELEMENT IS NOT 01, LOAD SCON WITH FLAGS,
					 *   CHECKING REL SETTING. */

					if( srcflgX_.srcflg == 1 ){

						if( sconX_.scon[sconlc-One][1-One] == 12 ){

							/*   CHANGE SCON(5) FOR WC 12 */
							formx = swork1X_.swork1[_flowckX_->n6jim-One][dct3X_.dct3[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
							  One]-One]-One];
							if( formx <= 19 ){
								if( (((((formx == 1 || formx == 4) || 
								  formx == 5) || formx == 8) || formx == 16)
								  || formx == 17) || formx == 19 ){
									sconX_.scon[sconlc-One][5-One] = 2;
									sw38bkX_.vnum = 1;
									goto L_720;
									}
								else if( ((((formx == 2 || formx == 3) ||
									   formx == 6) || formx == 7) || 
								       formx == 9) || formx == 13 ){
									sconX_.scon[sconlc-One][5-One] = 1;
									sw38bkX_.vnum = 1;
									goto L_720;
									}
								else if( (((formx >= 10 && formx <= 12) ||
									        formx == 14) || formx == 15) ||
											formx == 18 ){
									}
								}
							}
						}

					/*   NON-NOUNS ARE SET BY FLAGS */

					if( sw36bkX_.rel > 0 ){

						sconX_.scon[sconlc-One][4-One] = sw36bkX_.relgen;
						sconX_.scon[sconlc-One][6-One] = sw36bkX_.relper;

						if( srcflgX_.srcflg != 1 )
							sconX_.scon[sconlc-One][5-One] = sw36bkX_.relnum;
						}
					else{
						sconX_.scon[sconlc-One][4-One] = sw14bkX_.gen;
						sconX_.scon[sconlc-One][6-One] = sw14bkX_.per;

						if( srcflgX_.srcflg != 1 )
							sconX_.scon[sconlc-One][5-One] = sw14bkX_.num;
						}
					}
				else{
					if( srcflgX_.srcflg != 1 ){
						/*+ OFL3B conversion & R1685 Swork limit Change RKH  04/23/87   R1679
						 *     Ignore for now. Seems Invalid BES/CMP 10/86
						 *     SCON(12,SCONLC) = OFL2I(SCOLNK(N6JIM))
						 *-                                             RKH  04/23/87   R1679 */
						}

					/*   SUPPRESS NSWORK FORMATION FOR ELEMENT TO RIGHT
					 *   NUMBER PRESET TO SINGULAR, RESET TO PLURAL AFTER
					 *   CHECK OF FORM */

					if( srcflgX_.srcflg != 1 ){
						if( sw36bkX_.rel == 1 ){

							if( sw36bkX_.relnum != 0 ){
								sconX_.scon[sconlc-One][5-One] = sw36bkX_.relnum;
								sw36bkX_.relnum = 0;
								goto L_680;
								}
							}
						else if( sw14bkX_.num != 0 ){

							sconX_.scon[sconlc-One][5-One] = sw14bkX_.num;
							sw14bkX_.num = 0;
							goto L_680;
							}
						}

					if( srcflgX_.srcflg == 1 ){

						sconX_.scon[sconlc-One][5-One] = 1;
						for( s5=1; s5 <= 14; s5++ ){
							if( formsaX_.formsv[sconX_.scolnk[_flowckX_->n6jim-One]-
							  One] == pluralX_.plural[s5-One] )
								goto L_861;
							}
						goto L_440;
L_861:
						sconX_.scon[sconlc-One][5-One] = 2;

L_440:
						if( srcflgX_.srcflg == 1 ){

							if( vbdataX_.k3p1 >= 0 ){

								/*   WC 1 AND 7 ONLY */
								if( !(sconX_.scon[sconlc-One][1-One] != 1 && 
									  sconX_.scon[sconlc-One][1-One] != 7) ){

									/*   FOR DECLENTION OF NOUNS BY NUMBER
									 *   WHEN K3P1 IS 1-4, K3P1 IS USED TO COMPUTE SCON(5)
									 *   WHEN K3P1 IS 0, THE CASE FLAG IS USED */

									if( vbdataX_.k3p1 > 0 ){
										ms = vbdataX_.k3p1;
										if( ms < 1 || ms > 4 )
											goto L_680;
										}
									else if( case_X_.case_ == 0 || 
									         case_X_.case_ > 7 ){
										goto L_680;
										}
									else{

										temp1 = dct3X_.dct3[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
										  One]-One];
										if( case_X_.case_ == 1 )
											goto L_680;
										if( case_X_.case_ == 5 ){

											/*   FOR CASE = 5,6,7 - TEST NFORM TABLE */
											sr38 = 87;
											formod(2,sr38,(long)temp1,_flowckX_->n6jim,&retsw);
											if( retsw == 1 )
												goto L_660;

											sr38 = 88;
											formod(2,sr38,(long)temp1,_flowckX_->n6jim,&retsw);
											if( retsw == 1 )
												goto L_660;
											if( retsw == 2 )
												goto L_640;
											}
										else if( case_X_.case_ != 6 ){
											if( case_X_.case_ == 7 )
												goto L_600;
											ms = case_X_.case_;
											goto L_500;
											}

										sr38 = 86;
										formod(2,sr38,(long)temp1,_flowckX_->n6jim,&retsw);
										if( retsw == 1 )
											goto L_660;

										sr38 = 88;
										formod(2,sr38,(long)temp1,_flowckX_->n6jim,&retsw);
										if( retsw == 1 )
											goto L_660;
										if( retsw == 2 )
											goto L_640;

L_600:
										sr38 = 86;
										formod(2,sr38,(long)temp1,_flowckX_->n6jim,&retsw);
										if( retsw != 1 ){
											sr38 = 87;
											formod(2,sr38,(long)temp1,_flowckX_->n6jim,&retsw);
											if( retsw != 1 )
												goto L_640;
											}
L_660:
										sconX_.scon[sconlc-One][5-One] = 2;
										goto L_680;

L_640:
										sconX_.scon[sconlc-One][5-One] = 1;
										goto L_680;
										}

L_500:
									sconX_.scon[sconlc-One][5-One] = nounsX_.gernum[ms-One][formsaX_.formsv[sconX_.scolnk[_flowckX_->n6jim-One]-
									  One]-One];
									}
								}
							}
						}



L_680:
					 /*   GENDER FROM DICTIONARY */
					 /*   HAVE WE LOADING A HI FREQ CONSTANT? IF YES, THEN SKIP. */
/*5-11-98  comment out test as there is no distinction in RDBMS as to what is a hi constant
		   so we will always do the setting
			if( !((-swkadX_.swkad) > 120 && (-swkadX_.swkad) <= isizeX_.isize) )
*/
			if( (swkadX_.swkad) > -120 )
				sconX_.scon[sconlc-One][4-One] = ofl2X_.ofl2i[sconX_.scolnk[_flowckX_->n6jim-One]-One];

					/*   PERSON ALWAYS TO 3. */
					sconX_.scon[sconlc-One][6-One] = 3;
					}

				/*   CASE LOADED FROM CASE FLAG OR IF 0 SCON = K3P1 */

L_720:
				if( vbdataX_.k3p1 > 0 )
					sconX_.scon[sconlc-One][7-One] = vbdataX_.k3p1;
				if( vbdataX_.k3p1 != -97 ){

					if( vbdataX_.k3p1 == -99 && sw36bkX_.rel == 0 )
						sconX_.scon[sconlc-One][7-One] = case_X_.case_;
					if( vbdataX_.k3p1 == -99 && sw36bkX_.rel > 0 )
						sconX_.scon[sconlc-One][7-One] = sw36bkX_.relcas;

					if( vbdataX_.k3p1 <= -72 && vbdataX_.k3p1 >= -90 )
						sconX_.scon[sconlc-One][7-One] = sconX_.scon[elscnpX_.elscnp[im81X_.im81-vbdataX_.k3p1-One]-
						  One][7-One];

					if( vbdataX_.k3p1 == -98 && _sw25bkX_->hedcas != 0 )
						sconX_.scon[sconlc-One][7-One] = _sw25bkX_->hedcas;
					if( vbdataX_.k3p1 == -98 && _sw25bkX_->hedcas == 0 )
						sconX_.scon[sconlc-One][7-One] = case_X_.case_;
					}
				else if( sconX_.scon[sconlc-One][13-One] == 14 ){

					if( case_X_.semc14 != 0 ){

						sconX_.scon[sconlc-One][7-One] = case_X_.semc14;
						case_X_.semc14 = 0;
						om = _diacbX_->n3 - _flowckX_->phrstr;
						for( ms=1; ms <= om; ms++ ){
							om1 = _diacbX_->n3 - ms + 1;
							if( opadroX_.opadro[om1-One] == -115 )
								goto L_780;
							}
						goto L_820;

L_780:
						hpdopoX_.hfdopo[om1-One] = -175;
						}

					/*   IF SEMCAS HAS BEEN SET IN SEMTAB, USE THIS */

					}
				else if( case_X_.semcas != 0 ){
					sconX_.scon[sconlc-One][7-One] = case_X_.semcas;
					case_X_.semcas = 0;
					}
				}
			}
		}

L_820:
	if( srcflgX_.srcflg == 1 )
		case_X_.case_ = 0;

	if( opswX_.sw[10-One] == 1 )
		{
		fprintf( _spec_fp, " SW38 \n%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d\n%5d", 
		  sw14bkX_.gen, sw14bkX_.num, sw14bkX_.per, case_X_.case_, 
		  sw36bkX_.relnum, sw36bkX_.relper, sw36bkX_.relcas, vbdataX_.k3p1, 
		  vbdataX_.k3n, of3, sconX_.scono[sconX_.scolnk[_flowckX_->n6jim-One]-
		  One][59-SCONX1-One], _diacbX_->n3, opadroX_.opadro[_diacbX_->n3-One], 
		  _flowckX_->n6jim );
		for( xx=0; xx < 14; xx++ ){
			fprintf( _spec_fp, "%4d", sconX_.scon[sconlc-One][xx] );
			}
		fprintf( _spec_fp, "\n" );
		}

	/*   LOAD CONSTANT TO THE RIGHT OF THE ELEMENT
	 *   BUT FIRST CHECK TO SEE THAT IT HASN'T BEEN TRANSFORMED IN SP5 */

	if( om == 1 ){

		om = opadroX_.opadro[_diacbX_->n3-One];
//5-11-98 constants 		if( !(om > 130 || (om <= -100 && om >= (-isizeX_.isize))) ){
		if( !(om > 130 || (om <= -100)) ){
			/*                        add concatenating constant before ly */
			_diacbX_->n3 += 1;
			opadroX_.opadro[_diacbX_->n3-One] = -864;
			opadroX_.sconpo[_diacbX_->n3-One] = sconlc;
			/*                        add ly constant */
			_diacbX_->n3 += 1;
			opadroX_.opadro[_diacbX_->n3-One] = -233;
			opadroX_.sconpo[_diacbX_->n3-One] = sconlc;
			om = 0;

			}
		}
	return;
} /*end of function*/

