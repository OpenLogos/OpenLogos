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

EXTERN struct t_vtrfvlX_ {
	byte vtmnd, vtmrp, vtmel, vtmpp, vtmwk, vtmsl, vtmvc, vtmpu, vtmhi, 
	  vtmlo, vtmsw;
	}	vtrfvlX_;
EXTERN struct t_hfdm1X_ {
	short int hfdm1[30];
	}	hfdm1X_;
EXTERN struct t_vtrfbiX_ {
	short int slotel, slotrp;
	}	vtrfbiX_;
EXTERN struct t_sw11bkX_ {
	short int cn[30], hf4[HF4LIM][100], hf4ct, hf4m1[HF4LIM], slot[30][160], 
	  sltflg, vtrdon;
	}	sw11bkX_;
EXTERN struct t_endslotX_ {
	long int endslot;
	}	endslotX_;
	/* end of COMMON translations */
void /*FUNCTION*/ t1sw11(retflg)
short int *retflg;
{
	static byte vtrfmn;
	static short int basem1, flg380, gb, notflg, sldump, start, tmp4[HF4LIM][100], 
	  tmp4ct, tmp4m1[HF4LIM], ty, x;
	static long int ctix;
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
		/*end of pointer COMMON transls*/
	static char pgmnam[9] = "T1SW11  ";
	static short iz = 0;
	static short ms = 0;
	static short st = 0;
	static short cn1 = 0;
	static short cn2 = 0;
	static short iz2 = 0;
	static short st3 = 0;
	static short mscn = 0;
	static short eleven = 0;
	static short slotad = 0;
	static short slota2 = 0;

	/* CHANGES:
	 *    12/09/93 AVK: Store last slot loaded in a common for later use,
	 *                  use last slot loaded if -11000.
	 *    10/28/91 JAL: PROTECT AGAINST INTERPRETING SWITCH PARAMETER AS
	 *           A SLOT UNLOAD (70-100).
	 *    87/06/08 LG002GBA RPHS3: PHASE 3 FOR 30.40.50 TBLS
	 *    05/26/87 *R1725RKH*  -31011 IS A VALID STOPPER FOR -11 S
	 *    04/17/87 *R1685RKH*  CHANGE T1-4 SWORK LIMIT FROM 50 TO
	 *    04/01/87 *R1684RKH*  EL LOAD CONFUSION */
	/*-                                             RKH  04/17/87   R1685
	/*   ***** BEGINNING OF -11 SWITCH ***** */
	/*          FUNCTION : LOAD ALL DATA TO RIGHT OF SWITCH,
	 *          INCLUDING SWITCHES BUT EXCLUDING -41 SWITCH INTO THE SLOT ARR
	 *          DESIGNATED BY THE PARAMETER OF THE -11 SWITCH */
	/*     SLOTS ARE NUMBERED 71-100.  SLOTAD SUBTRACTS 70 AND
	 *     LOADS THE SLOT ARRAYS 1-30. */
	sldump = 0;
	*retflg = 0;

	if( vbdataX_.k3p1 != 0 ){
		/*							<* If parameter1 != 0 *> */

		slotad = vbdataX_.k3p1 - 70;
		/*							<* Use it to determine slot address *> */
		endslotX_.endslot = vbdataX_.k3p1;
		/*							<* Save the last slot loaded *> */

		/*							<* Else if parameter1 = 0 *> */

		}
	else if( endslotX_.endslot == 0 ){

		slotad = 1;
		/*							<* Else use the first slot *> */
		fprintf( _spec_fp, "No slot loaded yet, using first slot\n" );
		/*							<* Inform user *> */
		}
	else{
		/*							<* If last slot loaded was saved *> */
		slotad = endslotX_.endslot - 70;
		/*	                        /	* Use the saved value *> */
		}
	/*+ Store last slot loaded in a common variable.
	 *  Note that we are storing the slot address seen by the linguist, ie. 71-100. (AVK)
	 *+ */
	eleven = 0;

	/*   PHRASE SUPPRESSION */

	if( vtrfX_.vtrf[vbdataX_.k3-2-One] != -40 && _diacbX_->n3 == supresX_.n3sv )
		supresX_.phsup = 1;
	if( vtrfX_.vtrf[vbdataX_.k3-2-One] == -24 )
		supresX_.phsup = 1;

	/*   IF -26 SWITCH ASSUMED PHSUP WAS OFF, CORRECT PHR26 */
	if( supresX_.phsup == 1 && sw26bkX_.phr26 == (sworkoX_.phcto + 
	  1) )
		sw26bkX_.phr26 -= 1;

	if( slotad != 4 ){

		st = vbdataX_.k3 + 2;
		cn1 = sw11bkX_.cn[slotad-One];

		/*     CN1 IS COUNTER FOR ITEMS LOADED INTO SLOT ARRAY
		 *     COUNTER IS POSITIONED SO THAT FOR EACH ADDITIONAL LOAD TO THIS
		 *     SLOT, THE -1000 IS OVERLAYED. */

		/*     START MAJOR SLOT ARRAY LOAD LOOP.  IZ IS CURRENT VTR WE ARE
		 *     LOADING.  SLOT ARRAY IS LOADED WITH:
		 *       1) ALL THE ELEMENTS TO THE RIGHT OF SWITCH - INCLUDING 999
		 *          EXCLUDING -41 SWITCH.
		 *       2) IM1
		 *       3) -1000 (SIGNALS END OF LOAD IN UNLOAD PORTION) */

		/*+                                                        *R0GBA*GBA
		 *  HEY, WHY NOT PRINT A DIAGNOSTIC FOR SLOT LOADS???
		 *   THIS WILL BE THE BEFORE PICTURE */
		if( opswX_.sw[3-One] == 1  ){
			if( cn1 > 0 ){
				fprintf( _spec_fp, " BEFORE- SLOTAD,SLOTA2,SLDUMP%4d%4d%4d", 
				  slotad, slota2, sldump );
				fprintf( _spec_fp, " SLOT\n" );
				for( gb=1; gb <= cn1; gb++ ){
					fprintf( _spec_fp, "%6d", sw11bkX_.slot[slotad-One][gb-One] );
					}
				fprintf( _spec_fp, "\n" );
				}
			else{
				fprintf( _spec_fp, " BEFORE- SLOTAD,SLOTA2,SLDUMP%4d%4d%4d SLOT IS EMPTY\n", 
				  slotad, slota2, sldump );
				}
			}
		/*-                                                        *R0GBA*GBA */
		for( iz=st; iz <= _vwarg2X_->iz4; iz++ ){
			vtrnX_.vtrn = vtrfX_.vtrf[iz-One];

			/*+                                                         *T0009MBS
			 *+ El Load Confusion                           RKH  04/01/87   R1684
			 *         AN ELEMENT CAN STRETCH TO '-11' --
			 *            IF THIS HAPPENS, NOT A -11 SW */
			vtrfmn = vtrfmX_.vtrfm[iz-One];

			if( !(vtrfmn == vtrfvlX_.vtmel || (((((vtrnX_.vtrn != 
			  -11 && vtrnX_.vtrn != -57) && vtrnX_.vtrn != -65) && 
			  vtrnX_.vtrn != -64) && vtrnX_.vtrn != -63) && vtrnX_.vtrn != 
			  -31)) ){
				/*+ -31011 is a valid Stopper for -11 SW        RKH  05/26/87   R1725 */
				if( !(vtrnX_.vtrn == -31 && vtrfX_.vtrf[iz+1-One] != 
				  11) )
					goto L_473;
				}

			if( !(vtrnX_.vtrn <= 70 || vtrnX_.vtrn >= 100) ){
				/*         MAKE ATTEMPT TO DISTINGUISH BETWEEN A REAL SLOT UNLOAD
				 *         AND A SWITCH PARAMETER IN THE SLOT RANGE.  THIS WILL
				 *         NOT BE FOOLPROOF; TO MAKE IT SO MEANS KNOWING NUMBER OF
				 *         PARAMETERS FOR EACH SWITCH OR SETTING VTRFM IN VTRFWR()
				 *         TO INDICATE A SLOT.   THIS SLOT UNLOAD ASSUMES
				 *         SLOT UNLOAD IS FOLLOWED BY "000" & NEVER PRECEDED BY SWITCH */
				if( vtrfX_.vtrf[iz+1-One] == 0 ){
					if( !(vtrfX_.vtrf[iz-1-One] < 0 && vtrfX_.vtrf[iz-1-One] > 
					  -70) ){

						/*         IF PREVIOUS VTR ELS WERE NOT FROM A SLOT DUMP THEN
						 *         DO HOUSEKEEPING TO RECORD PROPER POINTER */

						if( sldump != (iz - 2) && iz != st ){
							sw11bkX_.slot[slotad-One][cn1-One] = 999;
							cn1 += 1;
							sw11bkX_.slot[slotad-One][cn1-One] = _flowckX_->im1;
							cn1 += 1;
							}

						/*         SET SLDUMP TO CURRENT IZ POSITION.  LATER WE WILL USE
						 *         SLDUMP TO TELL US WHAT HAS JUST BEEN PROCESSED IN -11 SWITCH */

						sldump = iz;



						/*   SLOT WITHIN THIS SLOT. UNLOAD ITS ELEMENTS INTO SLOT BEING LOADED
						 *   AND SET HFDM1(VTRN) TO -1 TO INDICATE THIS SLOT IS EMPTY */

						slota2 = vtrnX_.vtrn - 70;
						sw11bkX_.sltflg = 1;
						if( hfdm1X_.hfdm1[slota2-One] == -1 )
							goto L_474;

						/*   SUBTRACTION OF 1 PREVENTS LOAD OF -1000 AT END OF SLOT */
						cn2 = sw11bkX_.cn[slota2-One] - 1;

						for( iz2=1; iz2 <= cn2; iz2++ ){
							sw11bkX_.slot[slotad-One][cn1-One] = sw11bkX_.slot[slota2-One][iz2-One];
							cn1 += 1;
							if( cn1 >= 159 )
								goto L_280;
							}
						hfdm1X_.hfdm1[slota2-One] = -1;
						sw11bkX_.cn[slota2-One] = 1;
						if(  opswX_.sw[3-One] == 1 )
							{
							fprintf( _spec_fp, " SLOTAD,SLOTA2,SLDUMP%4d%4d%4d", 
							  slotad, slota2, sldump );
							fprintf( _spec_fp, " SLOT\n" );
							for( gb=1; gb <= 160; gb++ ){
								fprintf( _spec_fp, "%6d", sw11bkX_.slot[slotad-One][gb-One] );
								}
							fprintf( _spec_fp, "\n" );
							}
						goto L_474;
						}
					}
				}

			/*     SLTFLG EQUAL TO 1 MEANS A SLOT WITHIN A SLOT HAS BEEN PROCESSED.
			 *     SKIP OVER THIS 0, I.E. DO NOT LOAD IT INTO SLOT. */

			if( sw11bkX_.sltflg == 1 ){
				sw11bkX_.sltflg = 0;

				}
			else if( vtrnX_.vtrn == 999 && sldump == (iz - 2) ){
				goto L_475;
				}
			else{
				/*-                                                         *JGB31JGB
				 *+ El Load Confusion                           RKH  04/01/87   R1684
				 *     NOW STORE THE VTR ELEMENT
				 *     IF STRETCHABLE VTRF ELEMENT, BIAS IT */
				if( vtrfmn == vtrfvlX_.vtmel ){
					sw11bkX_.slot[slotad-One][cn1-One] = vtrnX_.vtrn + 
					  vtrfbiX_.slotel;
					}
				else if( vtrfmn == vtrfvlX_.vtmrp ){
					sw11bkX_.slot[slotad-One][cn1-One] = vtrnX_.vtrn + 
					  vtrfbiX_.slotrp;
					}
				else{
					sw11bkX_.slot[slotad-One][cn1-One] = vtrnX_.vtrn;
					}
				/*-                                             RKH  04/01/87   R1684 */

				cn1 += 1;
				if( vtrnX_.vtrn == 999 )
					break;

				if( cn1 > 158 )
					goto L_280;
				}

L_474:
			;
			}
		goto L_340;
L_475:
		cn1 -= 1;
		goto L_360;

L_280:
		if( opswX_.sw[3-One] == 1  )
			{
			fprintf( _spec_fp, " SW11 OVERLOADING SLOT \n" );
			}

		*retflg = 1;
		goto L_460;

		/*-                                             RKH  05/26/87   R1725
		 *-                                             RKH  04/01/87   R1684
		 *-                                                         *T0009MBS */

		/*   ANOTHER ELEVEN SWITCH ENCOUNTERED - THIS IS NOT LOADED INTO SLOT */
L_473:
		vbdataX_.k3n = iz;
		eleven = 1;
		/*+                                                         *JGB31JGB
		 *      DON'T WANT 999/IM1 IF LAST PROCESSED VTR EL WAS A SLOT DUMP */

		if( sldump == (iz - 2) ){
			cn1 -= 1;
			goto L_360;
			}
		else{
			/*-                                                         *JGB31JGB */

			sw11bkX_.slot[slotad-One][cn1-One] = 999;
			cn1 += 1;
			}

L_340:
		sw11bkX_.slot[slotad-One][cn1-One] = _flowckX_->im1;

L_360:
		x = x;
		if( sw11bkX_.cn[slotad-One] != cn1 + 1 ){
			hfdm1X_.hfdm1[slotad-One] = 1;

			/*               -1000 SIGNALS END OF THIS SLOTAD */

			sw11bkX_.slot[slotad-One][cn1+1-One] = -1000;
			sw11bkX_.cn[slotad-One] = cn1 + 1;
			}

		/*   COUNT OF ITEMS LOADED IN SLOT ARRAY IS CN1. */

		if( eleven == 1 )
			goto L_460;

		/*   LOAD 74 - VTR ELEMENTS LOADED INTO HF4 ARRAY */

		/* CHECK FOR OVERLOAD */
		}
	else if( sw11bkX_.hf4ct >= HF4LIM ){
		if( opswX_.sw[3-One] == 1  ){
			fprintf( _spec_fp, " SW11 OVERLOADING SLOT 74\n" );
			}
		else{
			errlog(pgmnam,20,511,10);
			}
		goto L_420;
		}
	else{

		/* NO OVERLOAD */
		x = x;
		basem1 = _flowckX_->im1;
		sw11bkX_.sltflg = 0;
		notflg = 0;
		tmp4ct = 0;
		flg380 = 0;
		ctix = HF4LIM*100*2;
		zapit(&tmp4[1-One][1-One],ctix,(byte)0);
		ctix = HF4LIM*2;
		zapit(&tmp4m1[1-One],ctix,(byte)0);
		st3 = vbdataX_.k3 + 2;
L_45:
		;
		for( iz=st3; iz <= _vwarg2X_->iz4; iz++ ){
			/*        CHECK FOR -11 SW STOPPER */
			if( vtrfmX_.vtrfm[iz-One] != vtrfvlX_.vtmel && (((((vtrfX_.vtrf[iz-One] == 
			  -11 || vtrfX_.vtrf[iz-One] == -57) || vtrfX_.vtrf[iz-One] == 
			  -63) || vtrfX_.vtrf[iz-One] == -64) || vtrfX_.vtrf[iz-One] == 
			  -65) || (vtrfX_.vtrf[iz-One] == -31 && vtrfX_.vtrf[iz+1-One] == 
			  11)) )
				goto L_140;

			/*  CHECK IF COPYING ANOTHER SLOT INTO SLOT 74 */
			if( !(vtrfX_.vtrf[iz-One] <= 70 || vtrfX_.vtrf[iz-One] >= 
			  100) ){
				slota2 = vtrfX_.vtrf[iz-One] - 70;
				/*  CHECK IF THE SLOT IS EMPTY */
				if( hfdm1X_.hfdm1[slota2-One] == -1 )
					goto L_477;
				/*  IF I JUST STARTED UNLOADING A SLOT, THEN START A NEW LINE */
				if( sw11bkX_.sltflg == 0 ){
					sw11bkX_.sltflg = 1;
					notflg = 0;
					tmp4ct += 1;
					if( tmp4ct >= HF4LIM )
						goto L_478;
					ms = 1;
					}
				cn2 = sw11bkX_.cn[slota2-One] - 1;
				start = 1;
				while( start <= cn2 ){
					for( iz2=start; iz2 <= cn2; iz2++ ){
						tmp4[tmp4ct-One][ms-One] = sw11bkX_.slot[slota2-One][iz2-One];
						if( sw11bkX_.slot[slota2-One][iz2-One] == 
						  999 )
							goto L_479;
						ms += 1;
						}
					break;
L_479:
					tmp4m1[tmp4ct-One] = sw11bkX_.slot[slota2-One][iz2+1-One];
					tmp4ct += 1;
					if( tmp4ct >= HF4LIM )
						goto L_480;
					ms = 1;
					start = iz2 + 2;
					}

				hfdm1X_.hfdm1[slota2-One] = -1;
				sw11bkX_.cn[slota2-One] = 1;
				tmp4ct -= 1;

				/* THIS IS IF I'M NOT COPYING ANOTHER SLOT INTO SLOT 74
				 *   DID I JUST COME FROM SLOT UNLOADING? */
				}
			else if( sw11bkX_.sltflg == 1 ){
				sw11bkX_.sltflg = 0;
				notflg = 0;
				}
			else{

				/*   NOT UNLOADING A SLOT */
				x = x;
				if( notflg == 0 ){
					sw11bkX_.sltflg = 0;
					notflg = 1;
					tmp4ct += 1;
					ms = 1;
					}
				/*        STORE; IF STRETCHABLE VTRF ELEMENT, BIAS IT */
				tmp4m1[tmp4ct-One] = basem1;
				if( vtrfmX_.vtrfm[iz-One] == vtrfvlX_.vtmel ){
					tmp4[tmp4ct-One][ms-One] = vtrfX_.vtrf[iz-One] + 
					  vtrfbiX_.slotel;
					}
				else if( vtrfmX_.vtrfm[iz-One] == vtrfvlX_.vtmrp ){
					tmp4[tmp4ct-One][ms-One] = vtrfX_.vtrf[iz-One] + 
					  vtrfbiX_.slotrp;
					}
				else{
					tmp4[tmp4ct-One][ms-One] = vtrfX_.vtrf[iz-One];
					}
				tmp4[tmp4ct-One][ms+1-One] = 999;
				tmp4[tmp4ct-One][ms+2-One] = 0;
				/*-                                             RKH  04/01/87   R1684 */
				ms += 1;
				}

			}
		goto L_476;
L_477:
		st3 += 2;
		goto L_45;
L_476:
		flg380 = 1;

		/*     A SECOND -11 */

L_140:
		vbdataX_.k3n = iz;

		/*     NORMAL END OF OVERFLOW ADDRESS */

		if( tmp4ct > 0 ){
			for( ctix=tmp4ct; ctix >= 1; ctix-- ){
				sw11bkX_.hf4ct += 1;
				/* CHECK FOR OVERLOAD */
				if( sw11bkX_.hf4ct >= HF4LIM )
					goto L_482;
				lmove(&sw11bkX_.hf4[sw11bkX_.hf4ct-One][1-One],1,&tmp4[ctix-One][1-One],
				  1,200);
				sw11bkX_.hf4m1[sw11bkX_.hf4ct-One] = tmp4m1[ctix-One];
				}
			goto L_481;
L_482:
			if( opswX_.sw[3-One] == 1  ){
				fprintf( _spec_fp, " SW11 OVERLOADING SLOT 74\n" );
				}
			else{
				errlog(pgmnam,20,511,5);
				}
			goto L_420;
			}
L_481:
		if( flg380 == 1 )
			goto L_380;
		goto L_460;
L_480:
		if( opswX_.sw[3-One] == 1  ){
			fprintf( _spec_fp, " SW11 OVERLOADING SLOT 74\n" );
			}
		else{
			errlog(pgmnam,20,511,5);
			}
		goto L_420;
L_478:
		if( opswX_.sw[3-One] == 1  ){
			fprintf( _spec_fp, " SW11 OVERLOADING SLOT 74\n" );
			}
		else{
			errlog(pgmnam,20,511,5);
			}
		goto L_420;
		}

L_380:
	if( sworkoX_.phrhdo[sworkoX_.phcto-One] != 0 ){
		if( supresX_.phsups == 0 )
			goto L_420;
		if( sw34bkX_.sw34n == 1 )
			goto L_420;
		if( sworkoX_.phcto == supresX_.phcts )
			goto L_420;
		}
	sworkoX_.phrhdo[sworkoX_.phcto-One] = elscnpX_.elscnp[_flowckX_->n6-One];
	if( sworkoX_.phrhdo[sworkoX_.phcto-One] == 0 )
		sworkoX_.phrhdo[sworkoX_.phcto-One] = opadroX_.sconpo[sworkoX_.phrbgo[sworkoX_.phcto-One]-
		  One];

	mscn = cn1 + 1;

L_420:
	if( opswX_.sw[3-One] == 1 )
		{
		fprintf( _spec_fp, " SW11 %6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d", 
		  slotad, eleven, _flowckX_->im1, cn1, st, vtrnX_.vtrn, vbdataX_.k3p1, 
		  vbdataX_.k3n, sworkoX_.phrhdo[sworkoX_.phcto-One], elscnpX_.elscnp[_flowckX_->n6-One], 
		  supresX_.phsups, sw34bkX_.sw34n, sworkoX_.phcto, _flowckX_->n6 );
		fprintf( _spec_fp, "\n" );
		for( ty=1; ty <= mscn; ty++ ){
			fprintf( _spec_fp, "%6d", sw11bkX_.slot[slotad-One][ty-One] );
			}
		fprintf( _spec_fp, "\n" );
		}


	vtrend();
	/*+                                                        *Rphs3*GBA */
	if( iz >= _vwarg2X_->iz4 || vtrnX_.vtrn == 999 ){
		vbdataX_.k3 = _vwarg2X_->iz4;
		vbdataX_.k3n = _vwarg2X_->iz4;
		}
	/*-                                                        *Rphs3*GBA */
	*retflg = 1;

L_460:
	;
	/*+                                                        *R0GBA*GBA
	 *  HEY, WHY NOT PRINT A DIAGNOSTIC FOR SLOT LOADS???
	 *   THIS WILL BE THE AFTER PICTURE */
	if( opswX_.sw[3-One] == 1 ){
		if( cn1 > 0 ){
			fprintf( _spec_fp, " AFTER- SLOTAD,SLOTA2,SLDUMP%4d%4d%4d", 
			  slotad, slota2, sldump );
			fprintf( _spec_fp, " SLOT\n" );
			for( gb=1; gb <= cn1; gb++ ){
				fprintf( _spec_fp, "%6d", sw11bkX_.slot[slotad-One][gb-One] );
				}
			fprintf( _spec_fp, "\n" );
			}
		else{
			fprintf( _spec_fp, " AFTER- SLOTAD,SLOTA2,SLDUMP%4d%4d%4d SLOT IS EMPTY\n", 
			  slotad, slota2, sldump );
			/*-                                                        *R0GBA*GBA */
			}
		}
	return;
} /*end of function*/

