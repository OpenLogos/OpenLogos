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
	 *   09/04/91 JAL:  ADD -68 SWITCH
	 *    87/06/08 LG002GBA RPHS3: PHASE 3 OF 30.40.50 TBLS
	 *    04/01/87 *R1684RKH*  ELEMENT LOAD CONFUSION
	 *      CHG 12/30/86  PR304050: TARGET VTR TABLES
	 *      CHG 12/16/86 *R1614DSD: ADD -66SW
	 *      CHG 08/17/86 *R1561DSD: 100 SCONS
	 *      CHG 11/26/85 */
	/*     SUBROUTINE FOR PROCESSING THE VTRS IN TRAN1 */
	/*   THIS SUBROUTINE WILL PROCESS THE VTR.
	 *   CONTROL INSTRUCTIONS TO THE DRIVER PROGRAM ARE COMMUNICATED WITH
	 *   THE ERRLVL AND GETVTR VALUE ARGUMENT */
	/*  ERRLVL & GETVTR = 0  NORMAL COMPLETION OF VTR, GET THE NEXT SP RULE.
	 *           GETVTR = 1  WC9 OR WC10 MATCH. BRANCH TO VTR LOOP CHECK.
	 *          ERRLVL NE 0 ABNORMAL TERMINATION. END TRAN PROCESSING */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*+ Element Load Confusion                      RKH  04/01/87   R1684
	 *     VTRFM REMEMBERS VTRF 'TYPES'  FROM VTRFWR */
	/*-                                             RKH  04/01/87   R1684 */
	/*+ - - - - - - - - - - - - - - - - - - - -    PR 30,40,50 PROJECT 12/86
	 *==== COMMON /DIACB/  K7,OFLAD,N3,VTR */
	/*- - - - - - - - - - - - - - - - - - - - -    PR 30,40,50 PROJECT 12/86 */
	/*+ Element Load Confusion                      RKH  04/01/87   R1684 */
	/*-                                             RKH  04/01/87   R1684 */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*---------------  PROCESS VTR SUBROUTINE ------------------------------- */

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

EXTERN struct t_vtrfvlX_ {
	byte vtmnd, vtmrp, vtmel, vtmpp, vtmwk, vtmsl, vtmvc, vtmpu, vtmhi, 
	  vtmlo, vtmsw;
	}	vtrfvlX_;
EXTERN struct t_getvtrX_ {
	short int getvtr;
	}	getvtrX_;
EXTERN struct t_loopckX_ {
	short int imatch[ELMMAX], noloop[21], nptpsv, nptpx, call36[2];
	}	loopckX_;
EXTERN struct t_sw11bkX_ {
	short int cn[30], hf4[HF4LIM][100], hf4ct, hf4m1[HF4LIM], slot[30][160], 
	  sltflg, vtrdon;
	}	sw11bkX_;


void /*FUNCTION*/ vtrpro()
{
	static byte vtrfmn;
	static short int k3pos, matpos, retflg, total;
	static long int _l0;
		/*pointer COMMON transls*/
	struct  {
		short int k7, oflad, n3;
		}	*_diacbX_ = (void*)&diacbX_;
	struct  {
		short int i, wc42m, wc50m, iz4, swx, s11prt;
		}	*_vwarg2X_ = (void*)&vwarg2X_;
	struct  {
		short int vtr[26];
		}	*_vtrX_ = (void*)&vtrX_;
	struct  {
		short int im1, i3save, i3str, n6, n6jim, n6jims, phrstr;
		}	*_flowckX_ = (void*)&flowckX_;
	struct  {
		short int k7m, minifg;
		}	*_minickX_ = (void*)&minickX_;
	struct  {
		short int nwrks, index, k3p3, pntr9;
		}	*_semargX_ = (void*)&semargX_;
		/*end of pointer COMMON transls*/
	static char pgmnam[9] = "T1VTRPRO";
	static short gb = 0;
	static short iz = 0;
	static short ms = 0;
	static short xx = 0;
	static short kz2 = 0;
	static short retsw = 0;
	static short val9 = 9;

	while( vbdataX_.k3n <= _vwarg2X_->iz4 ){

		if( opswX_.sw[10-One] == 1 )
			{
			fprintf( _spec_fp, " VTRPRO MAIN DIAGNOSTIC \n%4d%4d%4d%4d%4d%4d%4d%4d%4d%7d", 
			  _vwarg2X_->i, w50valX_.i3, _flowckX_->im1, vbdataX_.k3, 
			  vbdataX_.k3n, _flowckX_->n6, _flowckX_->n6jim, _diacbX_->n3, 
			  vtrnX_.vtrn, opadroX_.opadro[_diacbX_->n3-One] );
			for( xx=131; xx <= 170; xx++ ){
				fprintf( _spec_fp, "%2d", inhbX_.inhb[xx-One] );
				}
			for( iz=vbdataX_.k3n; iz <= _vwarg2X_->iz4; iz++ ){
				fprintf( _spec_fp, "%2d", vtrfX_.vtrf[iz-One] );
				}
			fprintf( _spec_fp, "\n" );
			}

		vbdataX_.k3 = vbdataX_.k3n;
		vtrnX_.vtrn = vtrfX_.vtrf[vbdataX_.k3-One];
		/*+ Element Load Confusion                      RKH  04/01/87   R1684
		 *     Get mode of the VTR element */
		vtrfmn = vtrfmX_.vtrfm[vbdataX_.k3-One];
		/*-                                             RKH  04/01/87   R1684 */
		vbdataX_.k3p1 = vtrfX_.vtrf[vbdataX_.k3+1-One];

		if( opswX_.sw[10-One] == 1 )
			{
			fprintf( _spec_fp, " IN %6.6s, NOW PROCESSING: %4d\n", 
			  passesX_.pssnam, vtrnX_.vtrn );
			}

		/*     ARE WE JUST FINISHED PROCESSING A SLOT? */
		if( (vtrnX_.vtrn == 999) && (sw11bkX_.vtrdon == 1) ){
			slotld();

			/*   IS THIS THE END OF THE CURRENT VTR??
			 *+                                                        *Rphs3*GBA
			 *     IF (VTRN .EQ. 999)              THEN
			 *                                     CALL VTREND
			 *                                     GOTO 9500
			 *                                     ENDIF */
			}
		else if( vtrnX_.vtrn == 999 ){
			return;
			/*-                                                        *Rphs3*GBA */

			/*     ARE WE ABOUT TO PROCESS A SLOT?
			 *   71 THRU 98 ARE CALLED SLOTS */
			}
		else if( (vtrnX_.vtrn >= 71) && (vtrnX_.vtrn <= 98) ){
			slotld();


			/*     ARE WE LOADING A VC, PUNCT OR A CONSTANT? */
			}
		else if( vtrnX_.vtrn > 9 ){
			t1load();
			vbdataX_.k3n = vbdataX_.k3 + 2;

			/*   IS IT AN ELEMENT TO BE LOADED?? */

			/*+ Element Load Confusion                      RKH  04/01/87   R1684 */
			}
		else if( vtrfmn == vtrfvlX_.vtmel ){
			/*-                                             RKH  04/01/87   R1684 */
			elemld();
			vbdataX_.k3n = vbdataX_.k3 + 2;

			/*   FALL THRU TO HERE STARTS TRAN1 SWITCH PROCESSING
			 *   COMPUTE MS SWITCH VARIABLE
			 *+                                                        *Rphs3*GBA
			 *   RETURN TO VTRCTL ON CERTAIN SWITCHES */
			}
		else if( (vtrnX_.vtrn == -63 || vtrnX_.vtrn == -64) || vtrnX_.vtrn == 
		  -65 ){
			return;
			}
		else{
			/*     IF (VTRN.EQ.-56) THEN
			 *       X = VTRF(K3 + 2)
			 *       X = (X / 100) * 100
			 *       IF (X .EQ. 9) GOTO 9500
			 *     ENDIF
			 *     IF (VTRN.EQ.-66) THEN
			 *       X = VTRF(K3 + 1)
			 *       X = (X / 100) * 100
			 *       IF (X .EQ. 9) GOTO 9500
			 *     ENDIF
			 *-                                                        *Rphs3*GBA */
			getvtrX_.getvtr = 0;
			ms = -vtrnX_.vtrn - 10;
			/*   COMPUTED GOTO FOR TRAN1 SWITCHES
			 *               1    2    3    4    5    6    7    8    9   10 */

			if( ms == 1 ){
				/*+ */
				t1sw11(&retsw);
				if( retsw == 1 )
					return;
				}
			else if( ms == 2 ){

				t1sw12();
				}
			else if( ms == 3 ){

				t1sw13();
				}
			else if( ms == 4 ){

				t1sw14();
				}
			else{
				if( ms != 5 ){
					if( ms == 6 ){

						t1sw16();
						goto L_8500;
						}
					else{
						if( !((((((((ms == 7 || ms == 17) || ms == 
						  18) || ms == 31) || ms == 35) || ms == 37) || 
						  (ms >= 40 && ms <= 57)) || ms == 59) || 
						  ms == 60) ){
							if( ms == 8 ){

								t1sw18();
								goto L_8500;
								}
							else if( ms == 9 ){

								txsw19();
								goto L_8500;
								}
							else if( ms == 10 ){

								txsw20();
								goto L_8500;
								}
							else if( ms == 11 ){

								t1sw21();
								goto L_8500;
								}
							else if( ms == 12 ){

//								t1sw22();
							    txsw22(&diacbX_.k7);
								goto L_8500;
								}
							else if( ms == 13 ){

								t1sw23();
								goto L_8500;
								}
							else if( ms == 14 ){

								t1sw24();
								goto L_8500;
								}
							else if( ms == 15 ){

								t1sw25();
								goto L_8500;
								}
							else if( ms == 16 ){

								t1sw26();
								goto L_8500;
								}
							else if( ms == 19 ){

								t1sw29();
								goto L_8500;
								}
							else if( ms == 20 ){

								t1sw30();
								goto L_8500;
								}
							else if( ms == 21 ){

								/*+ - - - - - - - - - - - - - - - - - - - -    PR 30,40,50 PROJECT 12/86
								 * retflg = -88  means skip out of the vtr and process the backspace */
								t1sw31(&retflg);
								if( retflg == -88 )
									return;
								/*+ - - - - - - - - - - - - - - - - - - - -    PR 30,40,50 PROJECT 12/86 */
								goto L_8500;
								}
							else if( ms == 22 ){

								t0sw32();
								goto L_8500;
								}
							else if( ms == 23 ){

								t1sw33();
								goto L_8500;
								}
							else if( ms == 24 ){

								t1sw34();
								goto L_8500;
								}
							else if( ms == 25 ){

								txsw35();
								goto L_8500;
								}
							else if( ms == 26 ){

								t1sw36();
								goto L_8500;
								}
							else if( ms == 27 ){

								t1sw37();
								goto L_8500;
								}
							else if( ms == 28 ){

								t1sw38();
								goto L_8500;
								}
							else if( ms == 29 ){

								t1sw39();
								goto L_8500;
								}
							else if( ms == 30 ){
								/*  SW40 DO NOTHING -- ADDED AS A PLACE HOLDER FOR 30 TABLES, ETC.
								 *                     WILL OVERRIDE PHRASE SUPPRESSION. */
								vbdataX_.k3n = vbdataX_.k3 + 2;
								goto L_8500;
								}
							else if( ms == 32 ){

								t1sw42();
								goto L_8500;
								}
							else if( ms == 33 ){

								t1sw43();
								/*     THIS SWITCH WILL CAUSE THE 15 SW. TO RUN AFTER IT'S DONE. */
								goto L_2150;
								}
							else if( ms == 34 ){

								t1sw44();
								goto L_8500;
								}
							else if( ms == 36 ){

								t1sw46();
								goto L_8500;
								}
							else if( ms == 38 ){

								t1sw48();
								goto L_8500;
								}
							else if( ms == 39 ){

								t0sw49();
								goto L_8500;
								}
							else if( ms == 58 ){


								sw68a();
								goto L_8500;
								}
							}
						/*                                                10/07/86  *R1614DSD */
						if( !((ms >= 44 && ms <= 47) || ms == 56) )
							goto L_9505;

						/*+                                               12/16/86  *R1614DSD
						 *    USE SWITCH VALUE IN VTRBRNCH CALL, IF -54, -55, -56, -66, OR -57 SW */
						gb = -vtrnX_.vtrn;
						vbrnc1(gb);
						/*        DID -56 (OR -66) CALL FOR WC09? */
						if( _semargX_->pntr9 == 0 || (gb != 56 && gb != 66) )
							goto L_8500;
						/*        DO WC9 BRANCH HERE, NOT IN VTRBR
						 *+  NON-RETURNABLE BRANCH TO 40- 50-TABLE                 *R0GBA*GBA */
						if( vtrfX_.vtrf[vbdataX_.k3-One] == -56 )
							k3pos = vbdataX_.k3 + 2;
						if( vtrfX_.vtrf[vbdataX_.k3-One] == -66 )
							k3pos = vbdataX_.k3 + 1;
						if( vtrfX_.vtrf[k3pos-One] >= 950 )
							goto L_9502;
						if( vtrfX_.vtrf[k3pos-One] >= 940 )
							goto L_9503;
						/*-                                                        *R0GBA*GBA */
						_diacbX_->k7 = 0;
						if( loopckX_.call36[1-One] == 0 ){
							loopckX_.call36[1-One] = loopckX_.nptpsv;
							if( _minickX_->minifg == 1 )
								loopckX_.call36[2-One] = 1;
							}
						idxval((short*)ADR(_l0,1),&val9,&_semargX_->pntr9, &_diacbX_->k7,&total);
						if( total != 0 )
						{
							rulein_ptr((short*)ADR(_l0,1),&matpos,
								spX_ptr,&_diacbX_->k7, &retsw);
						}
						txmini(3,&_minickX_->k7m,&loopckX_.nptpx,_vwarg2X_->i, &_semargX_->pntr9);
						if( errvrsX_.errlvl != 0 )
							return;
						if( _diacbX_->k7 != 0 || _minickX_->k7m !=  0 )
							goto L_9504;

						goto L_8500;
						}
					}

L_2150:
				t1sw15();
				}
			}

L_8500:
		if( errvrsX_.errlvl == 0 ){

			/*     GETVTR IS SET BY THE 22, 28, 29, 36 SWITCHES TO INDICATE THAT A
			 *     WC 9 OR WC10 VTR SHOULD BE PROCESSED. RETURN TO DRIVER TO BEGIN
			 *     PROCESSING THIS NEW VTR. */

			if( getvtrX_.getvtr == 1 )
				return;

			/*----------------------------------------------------------------------
			 *     ERRLVL = 13  INSTRUCTS US TO GET NEXT VTR ELEMENT */
			}
		else if( errvrsX_.errlvl == 13 ){
			errvrsX_.errlvl = 0;
			}
		else{
			return;
			}
		}
	goto L_9501;

	/*                    ILLEGAL SWITCH */

L_9505:
	if( opswX_.sw[3-One] == 1 || opswX_.sw[9-One] == 1 )
		{
		fprintf( _spec_fp, "\n ***** ILLEGAL VTR %6d%6d%6d", vbdataX_.k3n, 
		  vbdataX_.k3, vtrnX_.vtrn );
		fprintf( _spec_fp, "\n   " );
		for( kz2=vbdataX_.k3n; kz2 <= _vwarg2X_->iz4; kz2++ ){
			fprintf( _spec_fp, "%4d", vtrfX_.vtrf[kz2-One] );
			}
		fprintf( _spec_fp, "\n" );
		}
	return;
L_9504:
	getvtrX_.getvtr = 1;
	return;
L_9503:
	getvtrX_.getvtr = 4;
	return;
L_9502:
	getvtrX_.getvtr = 5;
	return;

L_9501:
	/*        VTR PROCESSING IS DONE. WRITE DIAGS & GET THE NEXT RULE. */
	diagno(7);

	vbdataX_.k3 = vbdataX_.k3n;
	return;
} /*end of function*/

