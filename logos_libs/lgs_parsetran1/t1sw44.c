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
program. If not, write to Globalware AG, Hospitalstraße 6, D-99817 Eisenach.

Linux port modifications and additions by Bernd Kiefer, Walter Kasper,
Deutsches Forschungszentrum fuer kuenstliche Intelligenz (DFKI)
Stuhlsatzenhausweg 3, D-66123 Saarbruecken
*/
	/*   CHANGES:
	 *C backed out> 4/26/94  jal: when appending a constant to an already
	 *C           > filled VC,
	 *C           > give the constant a new set of SCONs in the SCON table
	 *C           > (SCT position), not the one of whatever was 1st loaded
	 *C backed out> into the VC, as was previously the case.
	 *     4/19/94 jal:  When loading an element (-8x) into an empty VC,
	 *              copy the elements SCON(10) and SCON(8) values to the
	 *              VC entry.  Oddly enough, elements loaded into an empty
	 *              VC in TRAN1, get a new set of SCONs in the SCON table,
	 *              just like with constants.
	 *     LAST CHG: 05/22/87 *R1683RKH*  Copy Function of -44 Switch
	 *          CHG: 04/17/87 *R1685RKH*  Change T1-4 SWORK1 limit from 50 to
	 *      CHG 08/17/86 *R1561DSD: 100 SCONS
	 *      CHG 05/06/86 R1531DSD: HANDLE -44-96XXXYYY: XXX VC, YYY CONSTANT
	 *      CHG 05/01/86 */
	/*         FUNCTION: SETTING OF VARIABLE CONSTANTS */

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

EXTERN struct t_hfdm1X_ {
	short int hfdm1[30];
	}	hfdm1X_;
EXTERN struct t_sw36bkX_ {
	short int det, rel, relcas, relgen, relnum, relper;
	}	sw36bkX_;
EXTERN struct t_sw11bkX_ {
	short int cn[30], hf4[HF4LIM][100], hf4ct, hf4m1[HF4LIM], slot[30][160], 
	  sltflg, vtrdon;
	}	sw11bkX_;


void /*FUNCTION*/ t1sw44()
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
		short int nwrks, index, k3p3, pntr9;
		}	*_semargX_ = (void*)&semargX_;

	static short zero = 0;
	static char pgmnam[9] = "T1SW44  ";
	static short q = 0;
	static short gb = 0;
	static short gg = 0;
	static short iz = 0;
	static short jz = 0;
	static short kk = 0;
	static short xx = 0;
	static short adr_ = 0;
	static short gbq = 0;
	static short gbx = 0;
	static short gmm = 0;
	static short iz2 = 0;
	static short jzz = 0;
	static short kgb = 0;
	static short k3p2 = 0;
	static short k3p4 = 0;
	static short n3p1 = 0;
	static short gbad1 = 0;
	static short gbad2 = 0;
	static short im144 = 0;
	static short oplst = 0;
	static short im8144 = 0;
	static short k3p144 = 0;
	static short k3p344 = 0;
	static short phrbeg = 0;
	static short phrlst = 0;
	static short sl44ad = 0;
	static short vtrn44 = 0;

	vbdataX_.k3n = vbdataX_.k3 + 5;
	k3p2 = vtrfX_.vtrf[vbdataX_.k3+2-One];
	_semargX_->k3p3 = vtrfX_.vtrf[vbdataX_.k3+3-One];
	k3p4 = vtrfX_.vtrf[vbdataX_.k3+4-One];
	/*+ Copy Function of -44 Switch                 RKH  05/22/87   R1683 */

	/*     If K3P4 is a VC then it is the Copy Function */

	if( k3p4 >= 100 && k3p4 <= 120 ){
		copy144();
		return;
		}
	else{
		/*-                                             RKH  05/22/87   R1683 */

		/*+                                                         *B0121MBS
		 *          A SLOT LOAD?  IF IT IS EMPTY SKIP OUT. */
		if( _semargX_->k3p3 >= 71 && _semargX_->k3p3 <= 98 ){
			if( hfdm1X_.hfdm1[_semargX_->k3p3-70-One] == -1 )
				goto L_6840;
			}
		/*-                                                         *B0121MBS
		 *   K3P1 -96 IN TRAN1 MEANS ADVANCE-SET A VC THAT HAS NOT ALREADY BEEN
		 *   LOADED.  AN OPADRO POSITION IS CREATED THAT WILL LATER BE NULLED
		 *   DURING THE ACTUAL VC LOAD */

		if( vbdataX_.k3p1 == -96 ){

			/*   HAS THIS VC ALREADY BEEN ADVANCED-SET? */

			if( sw44bkX_.pre44 != 0 ){
				for( gb=1; gb <= sw44bkX_.pre44; gb++ ){
					gbad1 = sw44bkX_.ad44vc[gb-One][1-One];
					gbad2 = sw44bkX_.ad44vc[gb-One][2-One];
					if( sw44bkX_.ad44vc[gb-One][1-One] == k3p2 )
						goto L_9002;
					}

				/*   ARRAY AD44VC CONTAINS PARAMETERS FOR UP TO 5 ADVANCED-SET VC'S
				 *   PRE44 IS THE COUNTER FOR ARRAY AD44VC */

				if( sw44bkX_.pre44 == 5 )
					goto L_6490;
				goto L_6486;
L_9002:
				iz = sw44bkX_.ad44vc[gb-One][2-One];
				goto L_6562;
				}
L_6486:
			sw44bkX_.pre44 += 1;
			prtscoX_.sct += 1;
			/*+                                          *B0305JGB */
			if( prtscoX_.sct > SCONY ){

				/*+                                         *B0305JGB */
				if( prtscoX_.sct > SCONY ){
					prtscoX_.sct = SCONY;
					prtscoX_.scterr += 1;
					if( opswX_.sw[3-One] == 1  )
						{
						fprintf( _spec_fp, " T1SW44, OVERLOADING SCON ARRAY, SCTERR =%4d\n", 
						  prtscoX_.scterr );
						}
					if( prtscoX_.scterr == 1 )
						errlog(pgmnam,7730,500,13);
					/*-                                         *B0305JGB
					 *   ***** END OF -44 SWITCH ***** */

					/*SW44 */
					}
				return;
				}
			else{
				/*-                                          *B0305JGB */
				zapit(&sconX_.scon[prtscoX_.sct-One][1-One],40,(byte)zero);
				_diacbX_->n3 += 1;
				opadroX_.opadro[_diacbX_->n3-One] = -k3p2;
				iz = _diacbX_->n3;
				opadroX_.sconpo[_diacbX_->n3-One] = prtscoX_.sct;
				sconX_.scon[prtscoX_.sct-One][1-One] = k3p2;
				/*+    SET VC FROM REL PTR, PUNCT, OR CONSTANT    05/04/86  *R1531DSD */
				if( _semargX_->k3p3 <= -71 ){
					_flowckX_->n6jim = im81X_.im81 - _semargX_->k3p3;
					sconX_.scon[prtscoX_.sct-One][2-One] = _flowckX_->n6jim;
					sconX_.scon[prtscoX_.sct-One][10-One] = sconX_.scon[(_flowckX_->n6jim)-One][10-One];
					}
				else if( _semargX_->k3p3 >= 121 ){
					/*             EITHER PUNCTUATION OR A CONSTANT */
					sconX_.scon[prtscoX_.sct-One][2-One] = _semargX_->k3p3;
					sconX_.scon[prtscoX_.sct-One][10-One] = 0;
					}
				else{
					if( opswX_.sw[3-One] == 1 )
						{
						fprintf( _spec_fp, " IN SW44 - USING STRANGE VALUE IN NNN3\n" );
						}
					errlog(pgmnam,7385,0,0);
					sconX_.scon[prtscoX_.sct-One][2-One] = _semargX_->k3p3;
					sconX_.scon[prtscoX_.sct-One][10-One] = 0;
					}
				/*-                                               05/04/86  *R1531DSD */

				/*   FILL FIRST OPEN POSITION IN AD44VC ARRAY */

				for( kgb=1; kgb <= 5; kgb++ ){
					if( sw44bkX_.ad44vc[kgb-One][1-One] == 0 )
						goto L_9003;
					}
				goto L_6490;
L_9003:
				sw44bkX_.ad44vc[kgb-One][1-One] = k3p2;
				sw44bkX_.ad44vc[kgb-One][2-One] = _diacbX_->n3;
				/*+    RECORD 4TH PARAMETER                       05/06/86  *R1531DSD
				 *     AD44VC(3,KGB) = K3P4
				 *-                                               05/06/86  *R1531DSD */
				goto L_6562;
				}

L_6490:
			if( opswX_.sw[3-One] == 1 )
				{
				fprintf( _spec_fp, " -44-96  PRE-SET ARRAY OVERLOADED - EXIT SW44\n" );
				}
			goto L_6840;

			}
		else if( vbdataX_.k3p1 == -97 ){

			/*   K3P1 = -97 (SEARCH OUTPUT OPADRO BACKWARDS FOR V.C. K3P2) */

			/*   N3 IS LAST ELEMENT IN OPADRO
			 *   PHRLST IS LAST PHRASE TO SEARCH
			 *   OPLST IS LAST OPADRO ELEMENT TO LOOK AT */

			n3p1 = _diacbX_->n3 + 1;
			oplst = n3p1 - 1;

			/*   LOOK FOR VC K3P2 IN OPADRO */

			for( iz2=1; iz2 <= oplst; iz2++ ){
				if( opadroX_.opadro[n3p1-iz2-One] == (-k3p2) )
					goto L_9001;
				}
			goto L_6840;
L_9001:
			iz = n3p1 - iz2;
			}
		else{
			goto L_6840;
			}

		/*GBIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII */

		/*   IS K3P3 A SLOT? */
L_6562:
		if( _semargX_->k3p3 < 71 || _semargX_->k3p3 > 98 )
			goto L_6580;

		/*   K3P3 IS A SLOT - IF EMPTY, EXIT SWITCH */
		sl44ad = _semargX_->k3p3 - 70;
		/*+                                                         *B0121MBS
		 *          HFDM1 TESTED AT THE TOP OF THE PROGRAM
		 *-                                                         *B0121MBS */

		/*   CHECK CONTENTS OF SLOT - ESTABLISH END POSITION */
		for( kk=1; kk <= 160; kk++ ){
			gbx = sw11bkX_.slot[sl44ad-One][kk-One];
			/*     IF (GBX .LE. -11 .AND. GBX .GE. -48)  GOTO 6840 */

			/*   -1000 SIGNALS NO MORE VALUES LEFT IN SLOT */
			if( gbx == -1000 )
				break;
			}

		/*   SLOT IS NOT EMPTY - TURN ON SLOT44 FLAG */
		sw44bkX_.slot44 = 1;

		/*   SAVE VALUES OF IM1,IM81,VTRN, K3P1, AND K3P3, AND PROCESS SLOT */
		im144 = _flowckX_->im1;
		im8144 = im81X_.im81;
		vtrn44 = vtrnX_.vtrn;
		k3p144 = vbdataX_.k3p1;
		k3p344 = _semargX_->k3p3;

		/*   KK IS TOTAL NUMBER OF ELS IN SLOT K3P3 */
		gmm = 1;
		gbq = -1;
		}

	/*   LOOP THROUGH SLOT TO ESTABLISH NEW IM1 VALUE */
L_6571:
	;
	for( gg=gmm; gg <= kk; gg++ ){
		if( sw11bkX_.slot[sl44ad-One][gg-One] == 999 )
			break;
		}

	/*   IM1 WAS SAVED IN THE SLOT ARRAY AFTER THE 999 */
	_flowckX_->im1 = sw11bkX_.slot[sl44ad-One][gg+1-One];
	im81X_.im81 = _flowckX_->im1 - 80;
	gmm = gg + 2;

	/*   NOW UNLOAD SLOT ARRAY AND PROCESS */
L_6577:
	gbq += 2;
	_semargX_->k3p3 = sw11bkX_.slot[sl44ad-One][gbq-One];

	/*GBIIIIIIIINEWNEWNEWNEWNEWIIIIIIIIIIIIIII */

	/*   IF A SWITCH, SKIP OVER */
	if( _semargX_->k3p3 > -11 || _semargX_->k3p3 < -48 ){

		/*OM+    SIMPLIFIED THE LOGIC AND SUBSTITUTED 7362 WITH 7360 */
		vtrnX_.vtrn = _semargX_->k3p3;
		vbdataX_.k3p1 = sw11bkX_.slot[sl44ad-One][gbq+1-One];


		if( _semargX_->k3p3 > 9 ){
			/*        K3P3 IS A CONSTANT */
			t1load();
			}
		else{
			/*        K3P3 IS AN ELEMENT */
			elemld();
			_semargX_->k3p3 -= 80;
			}
		if( errvrsX_.errlvl != 0 )
			return;
		}
	else{

		/*   DEFAULT SETTING: GBQ = GBQ + 2
		 *      GBQ = GBQ + 2 */

		/*   SWITCH HAS TWO PARAMETERS: INCREMENT NEW GBQ BY ONE */
		if( _semargX_->k3p3 == -36 )
			gbq += 1;

		/*   SWITCH HAS THREE PARAMETERS: INCREMENT NEW GBQ BY TWO */
		if( (_semargX_->k3p3 == -14 || _semargX_->k3p3 == -34) || 
		  _semargX_->k3p3 == -48 )
			gbq += 2;

		/*   SWITCH HAS FOUR PARAMETERS: INCREMENT NEW GBQ BY THREE */
		if( ((_semargX_->k3p3 == -42 || _semargX_->k3p3 == -44) || 
		  _semargX_->k3p3 == -46) || _semargX_->k3p3 == -26 )
			gbq += 3;

		/*   SWITCH HAS FIVE PARAMETERS: INCREMENT NEW GBQ BY FOUR */
		if( _semargX_->k3p3 == -16 )
			gbq += 4;

		/*   SWITCH HAS SIX PARAMETERS: INCREMENT NEW GBQ BY FIVE */
		if( _semargX_->k3p3 == -47 )
			gbq += 5;

		/*   -22 SWITCH: CALCULATE GBQ POSITION */
		if( _semargX_->k3p3 == -22 )
			gbq = (gbq + 2) + sw11bkX_.slot[sl44ad-One][gbq-1-One]*
			  2;

		goto L_6830;
		}

	/*OM- */

	/*   K3P2 FOUND IN OPADRO */

L_6580:
	if( hpdopoX_.hfdopo[iz-One] != 0 ){
		if( k3p4 == -97 || k3p4 == -96 ){

			/*   K3P4 EQUALS -97 OR -96; ADD NEW ADDRESSES TO OLD */

			if( hpdopoX_.hfdopo[iz-One] < HFPOLO || hpdopoX_.hfdopo[iz-One] > 
			  HFPOHI ){

				/*   HFDOPO CONTAINS A CONSTANT; PUT IT IN A NEW HFPOAD */

				jz = 1;
				hfdoaX_.adct += 1;
				if( hfdoaX_.adct > HFPADY )
					goto L_7700;
				adr_ = hfdoaX_.adct;
				hfdoaX_.hfpoad[adr_-One][jz-One] = hpdopoX_.hfdopo[iz-One];
				hfdoaX_.sconhf[adr_-One][jz-One] = opadroX_.sconpo[iz-One];
				jz += 1;
				if( jz >= HFPADX )
					goto L_7720;
				if( _semargX_->k3p3 < 0 )
					goto L_6760;
				}
			else{

				/*   GET ADDRESS FOR HFPOAD FROM HFDOPO */

				adr_ = hpdopoX_.hfdopo[iz-One] - HFPOL1;
				jz = hfdoaX_.hfpoad[adr_-One][HFPADX-One] + 1;
				if( _semargX_->k3p3 <= 0 )
					goto L_6760;
				}

			/*   K3P3 IS A CONSTANT; PUT IT IN SAME HFPOAD */

			hfdoaX_.hfpoad[adr_-One][jz-One] = -_semargX_->k3p3;
			hfdoaX_.sconhf[adr_-One][jz-One] = opadroX_.sconpo[iz-One];
			if( sw44bkX_.slot44 == 1 )
				hfdoaX_.sconhf[adr_-One][jz-One] = sw44bkX_.conspo;
			/*C+              give this new const its own SCON position  4/26/94 jal
			 *      ELSE
			 *         SCT = SCT + 1
			 *         IF (SCT .GT. SCONY) GO TO 7730
			 *         CALL ZAPIT (SCON(1,SCT),40,ZERO)
			 *         SCONHF(JZ,ADR) = SCT
			 *         SCON(1,SCT) = 21
			 *         SCON(2,SCT) = K3P3
			 *      ENDIF
			 *C-                                                         4/26/94 jal */
			hfdoaX_.hfpoad[adr_-One][HFPADX-One] = jz;
			hpdopoX_.hfdopo[iz-One] = HFPOL1 + adr_;
			goto L_6780;
			}
		}

	/*   K3P4 NOT EQUAL -97 OR -96; NEW ADDRESSES OVERLAY OLD */

	if( _semargX_->k3p3 >= 0 ){

		/*   K3P3 IS A CONSTANT; PLACE DIRECTLY IN HFDOPO */

		hpdopoX_.hfdopo[iz-One] = -_semargX_->k3p3;
		if( sw44bkX_.slot44 == 1 )
			opadroX_.sconpo[iz-One] = sw44bkX_.conspo;
		goto L_6780;

		/*   K3P3 POINTS TO A PHRASE */

		}
	else if( hpdopoX_.hfdopo[iz-One] == 0 ){
		_flowckX_->n6jim = im81X_.im81 - _semargX_->k3p3;
		hpdopoX_.hfdopo[iz-One] = swork1X_.swork1[_flowckX_->n6jim-One][dctX_.dct[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
		  One]-One]-One];

		/*+1             copy attributes from original     4/19/94 jal */
		sconX_.scon[opadroX_.sconpo[iz-One]-One][10-One] = sconX_.scon[elscnpX_.elscnp[_flowckX_->n6jim-One]-
		  One][10-One];
		sconX_.scon[opadroX_.sconpo[iz-One]-One][8-One] = sconX_.scon[elscnpX_.elscnp[_flowckX_->n6jim-One]-
		  One][8-One];
		if( sw44bkX_.slot44 == 1 )
			opadroX_.sconpo[iz-One] = elscnpX_.elscnp[_flowckX_->n6jim-One];
		goto L_6780;

		}
	else if( hpdopoX_.hfdopo[iz-One] < HFPOLO || hpdopoX_.hfdopo[iz-One] > 
	  HFPOHI ){

		/*   PLACE ELEMENTS OF PHRASE IN NEW HFPOAD */

		jz = 1;
		hfdoaX_.adct += 1;
		if( hfdoaX_.adct > HFPADY )
			goto L_7700;
		adr_ = hfdoaX_.adct;
		}
	else{

		/*   USE OLD ADDRESS; BLANK OUT OLD ELEMENTS */

		jz = 1;
		adr_ = hpdopoX_.hfdopo[iz-One] - HFPOL1;
		for( jzz=1; jzz <= 20; jzz++ ){
			hfdoaX_.hfpoad[adr_-One][jzz-One] = 0;
			}
		}

	/*   K3P3 POINTS TO A PHRASE; ADD ELEMENTS IN PHRASE(N6JIM) TO HFPOAD */

L_6760:
	_flowckX_->n6jim = im81X_.im81 - _semargX_->k3p3;
	hfdoaX_.hfpoad[adr_-One][jz-One] = swork1X_.swork1[_flowckX_->n6jim-One][dctX_.dct[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
	  One]-One]-One];

	/*OM+
	 *   IF NO ELSCNP, CREATE ONE
	 *     IF (ELSCNP(N6JIM) .NE. 0) GOTO 6770
	 *     ASSIGN 6770 TO SSAVE
	 *     SCON44 = 1
	 *     K3P144 = K3P1
	 *     K3P1 = K3P3
	 *     N6JIM = IM81 - K3P3
	 *     GOTO 7362
	 *   IF ELSCNP WAS CREATED IN THIS SWITCH, RESET K3P1 VALUE
	 *6770  IF (SCON44 .EQ. 1) K3P1 = K3P144
	 *     SCON44 = 0
	 *OM- */

	hfdoaX_.sconhf[adr_-One][jz-One] = elscnpX_.elscnp[_flowckX_->n6jim-One];
	hfdoaX_.hfpoad[adr_-One][HFPADX-One] = jz;
	hpdopoX_.hfdopo[iz-One] = HFPOL1 + adr_;

L_6780:
	if( k3p4 > 0 )
		sconX_.scon[opadroX_.sconpo[iz-One]-One][7-One] = k3p4;
	if( k3p4 == -99 ){
		sconX_.scon[opadroX_.sconpo[iz-One]-One][7-One] = case_X_.case_;
		if( sw36bkX_.rel > 0 )
			sconX_.scon[opadroX_.sconpo[iz-One]-One][7-One] = sw36bkX_.relcas;
		}
	else{

		if( k3p4 == -13 ){
			if( sconX_.scon[opadroX_.sconpo[iz-One]-One][1-One] < 
			  0 )
				goto L_6830;
			sconX_.scon[opadroX_.sconpo[iz-One]-One][1-One] = -sconX_.scon[opadroX_.sconpo[iz-One]-
			  One][1-One];
			}

		/*   IF K3P4 NOT A REL PNTR CONTINUE VTRF PROCESSING */

		if( !(k3p4 > -80 || k3p4 < -90) ){
			_flowckX_->n6jim = im81X_.im81 - k3p4;

			for( iz2=sworkoX_.phrbgo[_flowckX_->n6jim-One]; iz2 <= sworkoX_.phrndo[_flowckX_->n6jim-One]; iz2++ ){
				hfdoaX_.hfpoad[adr_-One][jz-One] = opadroX_.opadro[iz2-One];
				hfdoaX_.sconhf[adr_-One][jz-One] = opadroX_.sconpo[iz2-One];
				jz += 1;
				if( jz >= HFPADX )
					goto L_7720;
				}
			hfdoaX_.hfpoad[adr_-One][HFPADX-One] = jz - 1;
			}
		}

	/*   IF K3P3 WAS NOT A SLOT, GOTO 6840 */

L_6830:
	if( sw44bkX_.slot44 != 1 )
		goto L_6840;

	/*   GG IS END OF CURRENT SLOT-UNLOAD VTR STRING */

	if( (gbq + 2) < gg )
		goto L_6577;
	gbq += 2;

	/*   FALL THROUGH TO HERE: PROCESS NEXT VTR IN SLOT - IF NONE, EXIT SWIT */

	if( sw11bkX_.slot[sl44ad-One][gg+2-One] != -1000 )
		goto L_6571;
	goto L_6835;


	/*                 ERROR LOGGING FOR ARRAY OVERFLOWS */

L_7700:
	if( opswX_.sw[3-One] == 1 )
		{
		fprintf( _spec_fp, " IN SW44 - HFDOPO 65 TO 74 ALREADY TAKEN\n" );
		}
	errlog(pgmnam,7700,502,0);
	return;

L_7720:
	if( opswX_.sw[3-One] == 1 )
		{
		fprintf( _spec_fp, " IN SW44 - HFOAD ARRAY IS OVERFLOWING\n" );
		}
	errlog(pgmnam,7720,502,0);
	return;

	/*   RESET IM1,IM81,VTRN K3P1, AND K3P3 VALUES IF A SLOT WAS PROCESSED */

L_6835:
	_flowckX_->im1 = im144;
	im81X_.im81 = im8144;
	vtrnX_.vtrn = vtrn44;
	vbdataX_.k3p1 = k3p144;
	_semargX_->k3p3 = k3p344;
	sw44bkX_.slot44 = 0;
	hfdm1X_.hfdm1[sl44ad-One] = -1;
	sw11bkX_.cn[sl44ad-One] = 1;

L_6840:
	;

	if( opswX_.sw[10-One] == 1 ){

		fprintf( _spec_fp, " ** END SW44  %4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d\n", 
		  _flowckX_->n6jim, phrbeg, phrlst, vbdataX_.k3p1, k3p2, _semargX_->k3p3, 
		  k3p4, iz, adr_, xx, hpdopoX_.hfdopo[iz-One], opadroX_.opadro[iz-One], 
		  hfdoaX_.adct );
		fprintf( _spec_fp, " HFPOAD = " );
		for( q=1; q <= 20; q++ ){
			fprintf( _spec_fp, "%6d", hfdoaX_.hfpoad[adr_-One][q-One] );
			}
		fprintf( _spec_fp, "\n" );
		}
	return;
} /*end of function*/

