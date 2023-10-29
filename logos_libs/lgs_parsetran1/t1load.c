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
	/*    CHANGES:
	 *     09/01/93 jal:set Black Hole flags if constant is a left Bracket
	 *                  marking a BH fill string.
	 *     05/02/87 *R1691RKH*  Deactivate SCONPO for -140 in OPADR
	 *      CHG 08/24/86 *R1561DSD: 100 SCONS
	 *      CHG 05/06/86 *R1531DSD: LOADING PRELOADED VC AS IF NNN1 = 0
	 *      CHG 03/27/86 *R1507DSD: RECORD VTRN FOR LINGUISTS' REFERENCE
	 *      CHG 12/03/85 */
	/*     SUBROUTINE FOR LOADING A VC, CONSTANT OR PUNCT INTO THE OPADRO */
	/*   THIS SUBROUTINE IS CALLED DIRECTLY BY T1VTRPRO PROGRAM AS WELL
	 *   AS BY THE FOLLOWING SWITCHES: 38,44
	 *   CONTROL IS RESTORED WITH NO SPECIAL CONDITIONS. */

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

EXTERN struct t_sw48bhX_ {
	short int bhpt48, bhct48;
	}	sw48bhX_;
EXTERN struct t_sw38bkX_ {
	short int adjnp, sw38n, vnum;
	}	sw38bkX_;
	/* end of COMMON translations */


void /*FUNCTION*/ t1load()
{
	static short int gbvcx;
	int taddress,dictype;
	struct  {
		short int k7, oflad, n3;
		}	*_diacbX_ = (void*)&diacbX_;
	struct  {
		short int vtr[26];
		}	*_vtrX_ = (void*)&vtrX_;
	struct  {
		short int im1, i3save, i3str, n6, n6jim, n6jims, phrstr;
		}	*_flowckX_ = (void*)&flowckX_;
	static char pgmnam[9] = "T1LOAD  ";
	static short jz = 0;
	static short xx = 0;
	static short adr_ = 0;
	static short gbvc = 0;

	/*   ----- START VC, PUNCT AND CONSTANT LOAD LOGIC ------------------- */
	/*         99 THRU 120 ARE CALLED VARIABLE CONSTANTS */
	if( (vtrnX_.vtrn >= 99) && (vtrnX_.vtrn <= 120) ){

		/*          HAS THIS VC BEEN PRESET? */
		if( sw44bkX_.pre44 != 0 ){
			for( gbvcx=1; gbvcx <= 5; gbvcx++ ){
				if( vtrnX_.vtrn == sw44bkX_.ad44vc[gbvcx-One][1-One] )
					goto L_9001;
				/*-                                               05/06/86  *R1531DSD */
				}
			goto L_7570;
			/*+            IMPLEMENT LOAD OF VC FROM CONSTANT 05/06/86  *R1531DSD */
L_9001:
			gbvc = gbvcx;
			sw44bkX_.prevc = 1;
			/*             MUST SUPPRESS ANY ARGUMENT */
			vbdataX_.k3p1 = 0;
			/*             SET N6JIM = POSITION OF PRELOADED SCON SO THE 38 SWITCH
			 *                      CAN USE IT WHEN SETTING SCONLC FOR A VC LOAD
			 *             N6JIM = SCONPO(AD44VC(2,GBVC))
			 *             GO CLEAN OUT ADVANCE-LOAD BUCKET (GOTO WAS TO 7570)
			 *             GOTO 7630
			 *+ Deactivate SCONPO for -140 in OPADRO         RKH  05/02/87   R1691 */
			goto L_7585;
			}

		/*          IF AN ELEMENT IS BEING LOADED DIRECTLY INTO THE VC,
		 *          REASSIGN THE SCON, AND SET THE SCONPO AND OPADRO. */
L_7570:
		if( vbdataX_.k3p1 >= 0 ){


			prtscoX_.sct += 1;
			if( prtscoX_.sct > SCONY )
				goto L_8870;

			/*          SET N6JIM = SCT SO THAT THE 38 SWITCH CAN USE
			 *          IT WHEN SETTING SCONLC FOR A VC LOAD */
			_flowckX_->n6jim = prtscoX_.sct;

			sconX_.scon[prtscoX_.sct-One][1-One] = vtrnX_.vtrn;
			sconX_.scon[prtscoX_.sct-One][2-One] = vbdataX_.k3p1;
			}
		else{
			_diacbX_->n3 += 1;
			/*+                                               08/24/86  *R1561DSD */
			if( _diacbX_->n3 > OPADRX ){
				if( _diacbX_->n3 == (OPADRX + 1) )
					errlog(pgmnam,7570,501,5);
				return;
				}
			else{
				/*-                                               08/24/86  *R1561DSD */
				opadroX_.opadro[_diacbX_->n3-One] = -vtrnX_.vtrn;
				_flowckX_->n6jim = im81X_.im81 - vbdataX_.k3p1;
				hpdopoX_.hfdopo[_diacbX_->n3-One] = swork1X_.swork1[_flowckX_->n6jim-One][dctX_.dct[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
				  One]-One]-One];
				opadroX_.sconpo[_diacbX_->n3-One] = _flowckX_->n6jim;
				/*          IF THE VC HAS BEEN PRELOADED, A NEW SCON MUST
				 *          BE CREATED */
				if( sw44bkX_.prevc == 0 )
					goto L_7590;
				prtscoX_.sct += 1;

				if( prtscoX_.sct > SCONY )
					goto L_8870;

				sconX_.scon[prtscoX_.sct-One][1-One] = vtrnX_.vtrn;
				sconX_.scon[prtscoX_.sct-One][2-One] = vbdataX_.k3p1;
				sconX_.scon[prtscoX_.sct-One][10-One] = sconX_.scon[opadroX_.sconpo[_diacbX_->n3-One]-
				  One][10-One];
				goto L_7590;
				}
			}

		/*+ Deactivate SCONPO for -140 in OPADRO         RKH  05/02/87   R1691 */
L_7585:
		_diacbX_->n3 += 1;
		/*-                                             RKH  05/02/87   R1691
		 *+                                               08/24/86  *R1561DSD */
		if( _diacbX_->n3 > OPADRX ){
			if( _diacbX_->n3 == (OPADRX + 1) )
				errlog(pgmnam,7580,501,5);
			return;
			}
		else{
			/*-                                               08/24/86  *R1561DSD */
			opadroX_.opadro[_diacbX_->n3-One] = -vtrnX_.vtrn;
			/*+ Deactivate SCONPO for -140 in OPADRO         RKH  05/02/87   R1691 */
			if( sw44bkX_.prevc == 0 ){
				/*-                                             RKH  05/02/87   R1691 */
				opadroX_.sconpo[_diacbX_->n3-One] = prtscoX_.sct;

				if( vbdataX_.k3p1 != 0 ){

					if( inhbX_.inhb[vbdataX_.k3p1-One] != 1 ){
						hpdopoX_.hfdopo[_diacbX_->n3-One] = -vbdataX_.k3p1;
						if( vbdataX_.k3p1 == 160 )
							inhbX_.inhb[160-One] = 1;
						if( vbdataX_.k3p1 == 146 )
							inhbX_.inhb[146-One] = 1;
						if( vbdataX_.k3p1 == 579 )
							inhbX_.inhb[579-One] = 1;
						}
					}
				}
			}

L_7590:
		if( sw44bkX_.prevc != 0 ){
			xx = sw44bkX_.ad44vc[gbvc-One][2-One];
			/*+    SET N6JIM FOR SCONLC IN -38 FOR VC LOAD    05/06/86  *R1531DSD */
			_flowckX_->n6jim = opadroX_.sconpo[xx-One];
			/*-                                               05/06/86  *R1531DSD */

			/*                    MOVE PRELOADED VC VALUES INTO THE OPADRO
			 *                    IF VC IS EMPTY, JUST LOAD IT */
			if( hpdopoX_.hfdopo[_diacbX_->n3-One] == 0 ){
				opadroX_.opadro[_diacbX_->n3-One] = opadroX_.opadro[xx-One];
				hpdopoX_.hfdopo[_diacbX_->n3-One] = hpdopoX_.hfdopo[xx-One];
				opadroX_.sconpo[_diacbX_->n3-One] = opadroX_.sconpo[xx-One];
				}
			else{

				/*                    NOT EMPTY, ADD THE ADDRESS ALREADY LOADED TO
				 *                    THE PRELOADED VALUE(S) */
				if( hpdopoX_.hfdopo[xx-One] >= HFPOLO &&
					hpdopoX_.hfdopo[xx-One] <= HFPOHI ){
					adr_ = hpdopoX_.hfdopo[xx-One] - HFPOL1;
					jz = hfdoaX_.hfpoad[adr_-One][HFPADX-One];
					}
				else{
					jz = 1;
					hfdoaX_.adct += 1;
					if( hfdoaX_.adct > HFPADY ){
						if( diagsX_.shrtdi != 0 )
							{
							fprintf( _spec_fp, " HFPOAD ARRAY FULL, CANNOT LOAD VC \n" );
							}
						errlog(pgmnam,7591,502,1);
						return;
						}
					else{
						adr_ = hfdoaX_.adct;
						hfdoaX_.hfpoad[adr_-One][jz-One] = hpdopoX_.hfdopo[xx-One];
						hfdoaX_.sconhf[adr_-One][jz-One] = opadroX_.sconpo[xx-One];
						}
					}
				jz += 1;
				hfdoaX_.hfpoad[adr_-One][jz-One] = hpdopoX_.hfdopo[_diacbX_->n3-One];
				hfdoaX_.sconhf[adr_-One][jz-One] = opadroX_.sconpo[_diacbX_->n3-One];

				hpdopoX_.hfdopo[_diacbX_->n3-One] = adr_ + HFPOL1;
				hfdoaX_.hfpoad[adr_-One][HFPADX-One] = jz;

				}

			/*     NOW CLEAN OUT THIS ADVANCE-LOAD BUCKET */
			opadroX_.opadro[sw44bkX_.ad44vc[gbvc-One][2-One]-One] = -140;
			hpdopoX_.hfdopo[sw44bkX_.ad44vc[gbvc-One][2-One]-One] = 0;
			/*+ Deactivate SCONPO for -140 in OPADRO         RKH  05/02/87   R1691 */
			opadroX_.sconpo[sw44bkX_.ad44vc[gbvc-One][2-One]-One] = 1;
			/*-                                             RKH  05/02/87   R1691 */
			sw44bkX_.ad44vc[gbvc-One][0] = 0;
			sw44bkX_.ad44vc[gbvc-One][1] = 0;
			sw44bkX_.pre44 -= 1;
			sw44bkX_.prevc = 0;
			}
		return;
		}
	else{

			//                                    PUNCTUATION?
		if( (vtrnX_.vtrn >= 121) && (vtrnX_.vtrn <= 130) ){

			prtscoX_.sct += 1;
			if( prtscoX_.sct > SCONY )	goto L_8870;
			sconX_.scon[prtscoX_.sct-One][1-One] = 20;

			taddress = vtrnX_.vtrn;
			dictype = 2;
			errvrsX_.err = TARG_CODES(&trgflgX_.trgflg,&dictype,
			                         &taddress,
									 LOGCC,
									 hfreqX_.hfdo,diagsX_.longdi, _spec_fp);
			if( errvrsX_.err == 0 ){
				sconX_.scon[prtscoX_.sct-One][3-One] = hfreqX_.hfdo[2];
				}
			else{
				errlog(pgmnam,7700,taddress,1);
				return;
				}
			}
			//                                     HI FREQ CONSTANT?
		else if( vtrnX_.vtrn < 131 ){
			return;

			/*  -----------------------------------------------------------------
			 *   ****  CONSTANT LOAD  **** */

			/*        IS FRENCH CONSTANT INHIBITED ??? */
			}
		else if( inhbX_.inhb[vtrnX_.vtrn-One] == 1 ){
			if( !((vtrnX_.vtrn == 146 || vtrnX_.vtrn == 160) ||
				   vtrnX_.vtrn == 579) )
				inhbX_.inhb[vtrnX_.vtrn-One] = 0;
			return;
			}
		else{

			prtscoX_.sct += 1;
			if( prtscoX_.sct > SCONY )	goto L_8870;
			sconX_.scon[prtscoX_.sct-One][1-One] = 21;

			/*+    RECORD VTRN FOR LINGUISTS' REFERENCE       03/27/86  *R1507DSD */
			sconX_.scon[prtscoX_.sct-One][2-One] = vtrnX_.vtrn;
			}


		/*  -----------------------------------------------------------------
		 *       ASSIGN OPADRO, SCONPOS, ETC...  FOR VC, PUNCT AND CONSTANTS. */

		if( (sw38bkX_.sw38n == 0) && (vbdataX_.k3p1 <= 20) )
			sconX_.scon[prtscoX_.sct-One][8-One] = vbdataX_.k3p1;

		if( sw44bkX_.slot44 != 1 ){

			_diacbX_->n3 += 1;
			if( _diacbX_->n3 > OPADRX ){
				if( _diacbX_->n3 == (OPADRX + 1) )
					errlog(pgmnam,7800,501,5);
				return;
				}
			else{
				opadroX_.opadro[_diacbX_->n3-One] = -vtrnX_.vtrn;
				opadroX_.sconpo[_diacbX_->n3-One] = prtscoX_.sct;
				/*                  set Black Hole flags if constant is a left Bracket
				 *                  marking a BH fill string. */
				if( vtrnX_.vtrn == BFLFBR ){
					sconX_.scon[prtscoX_.sct-One][BFBHFL-One] = sw48bhX_.bhpt48;
					sconX_.scon[prtscoX_.sct-One][BFBHPO-One] = sw48bhX_.bhct48;
					}
				}
			}

		sw44bkX_.conspo = prtscoX_.sct;
		if( vtrnX_.vtrn == 160 )
			inhbX_.inhb[160-One] = 1;
		if( vtrnX_.vtrn == 146 )
			inhbX_.inhb[146-One] = 1;
		if( vtrnX_.vtrn == 579 )
			inhbX_.inhb[579-One] = 1;
		return;
		}


	//            SCT GREATER THAN MAX, JUST PUT OUT AN ERROR MESSAGE.
L_8870:
	if( prtscoX_.sct > SCONY )
	{
		prtscoX_.sct = SCONY;
		prtscoX_.scterr += 1;

		if( diagsX_.longdi != 0)
			{
			fprintf( _spec_fp, " T1LOAD, OVERLOADING SCON ARRAY, SCTERR =%4d\n", 
			  prtscoX_.scterr );
			}
		if( prtscoX_.scterr == 1 )
			errlog(pgmnam,8875,500,13);
	}

	return;
} /*end of function*/

