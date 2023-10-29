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
	/*    CALLED ONLY BY T1VTRPRO */
	/*     LAST CHG: 04/01/87 *R1684RKH*  El Load Confusion
	 *      CHG 04/29/86 *B04XYDSD - USE ALL OF VTRF(460), VTRFS(460)
	 *      CHG 08/29/85 */
	/*  THIS SUBROUTINE WILL GET THE TAKE CARE OF UNLOADING SLOTS INTO THE
	 *  VTR FOR PROCESSING.
	 *  BEFORE A SLOT CAN BE UNLOADED INTO THE VTR, WHAT IS LEFT OF THE VTR
	 *  MUST FIRST BE SAVED. FOR EXAMPLE, IN THIS VTR:
	 *             089 000  -44 -81 102 -82 -97 -01 000 999
	 *  WHEN WE UNLOAD THE CONTENTS OF SLOT 89 (IF SOMETHING IS IN IT), THE
	 *  44 SWITCH AND THE LOAD TO THE RIGHT OF IT MUST BE SAVED IN VTRFS SO
	 *  THAT WE CAN INSERT THE VTR STRING SAVED IN THE 89 SLOT.
	 *  SINCE A SLOT CAN CONTAIN MORE THAN ONE VTR STRING, IT WILL UNLOAD
	 *  ONE, PROCESS IT, AND THEN GET THE NEXT VTR STRING, PROCESS IT, ETC...
	 *  ONCE THIS IS DONE, THE VTR SAVED IN VTRFS IS RESTORED AND GOES ON
	 *  PROCESSING NORMALLY. */
	/*        WHEN VTRDON = 1, WE ARE PROCESSING THE VTRS OF A SLOT.
	 *        WHEN VTRDON = 0, WE ARE NOT. */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*+                                               04/28/86  *R04XYDSD */
	/*+ El Load Confusion                           RKH  04/01/87   R1684
	 *     VTRFM REMEMBERS VTRF 'TYPES'  FROM VTRFWR */
	/*     SLOTEL,SLOTRP BIASSES MARK 'E'LEMENT, 'REL-'PTR SAVED IN SLOT */
	/*-                                             RKH  04/01/87   R1684 */
	/*+ El Load Confusion                           RKH  04/01/87   R1684 */
	/*-                                             RKH  04/01/87   R1684 */
	/*+                                               04/28/86  *R04XYDSD */
	/*-                                               04/28/86  *R04XYDSD
	 *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*   ***** A SLOT *****
	 *   ARE WE IN THE MIDST OF PROCESSING  A SLOT? */

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

#define	HF4LIM	70

EXTERN struct t_vtrfvlX_ {
	byte vtmnd, vtmrp, vtmel, vtmpp, vtmwk, vtmsl, vtmvc, vtmpu, vtmhi, vtmlo, vtmsw;
	}	vtrfvlX_;
EXTERN struct t_hf4lX_ {
	short int hf4l;
	}	hf4lX_;
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


void /*FUNCTION*/ slotld()
{
	static byte vtrfms[460];
	static short int iz4sv, vtrfs[460];
	static long int zlen;
	struct  {
		short int i, wc42m, wc50m, iz4, swx, s11prt;
		}	*_vwarg2X_ = (void*)&vwarg2X_;
	struct  {
		short int im1, i3save, i3str, n6, n6jim, n6jims, phrstr;
		}	*_flowckX_ = (void*)&flowckX_;
	static short iz = 0;
	static short izs = 0;
	static short k3x = 0;
	static short izs2 = 0;
	static short hf4ls = 0;
	static short vtrns = 0;
	static short savim1 = 0;
	static short slotad = 0;
	static short slotsv = 0;
	static short zero = 0;

	if( sw11bkX_.vtrdon == 1 ){

		/*----------------------------------------------------------------------- */

		/*     RETURN FROM VTRF UNLOAD - IF VTRN = 999 AND VTRDON = 1
		 *     CHECK TO SEE IF THERE IS ANYTHING ELSE LEFT IN THE SLOT TO PROCESS
		 *     IF NOT, THEN RESTORE THE SAVED VTR. */

		if( slotsv != 4 ){

			if( hfdm1X_.hfdm1[slotsv-One] != (-1) ){
				vtrnX_.vtrn = vtrns;
				goto L_8000;
				}
			}

		sw11bkX_.cn[slotsv-One] = 1;

		if( hf4lX_.hf4l != 0 )
			goto L_7880;
		goto L_8160;


		/*   SPECIAL SLOTS ?? */

		}
	else if( vtrnX_.vtrn == 74 ){
		vtrns = vtrnX_.vtrn;

		/*     LOAD NEXT H4F INTO VTRF */

		if( sw11bkX_.hf4ct == 0 ){
			vbdataX_.k3n = vbdataX_.k3 + 2;
			return;
			}
		else{

			k3x = vbdataX_.k3 + 2;

			/*+                                               04/28/86  *R04XYDSD */
			zlen = 461 - k3x;
			zapit(&vtrfs[k3x-One],zlen,(byte)zero);
			for( iz=k3x; iz <= 460; iz++ ){
				/*+ El Load Confusion                           RKH  04/01/87   R1684 */
				vtrfms[iz-One] = vtrfmX_.vtrfm[iz-One];
				/*-                                             RKH  04/01/87   R1684
				 *-                                               04/28/86  *R04XYDSD */
				vtrfs[iz-One] = vtrfX_.vtrf[iz-One];
				}
			sw11bkX_.vtrdon = 1;
			savim1 = _flowckX_->im1;
			/*+                                               04/28/86  *R04XYDSD */
			iz4sv = _vwarg2X_->iz4;
			goto L_7880;
			}
		}
	else{

		/*     SLOT ADDRESSES 1-30 CORRESPOND TO SLOTS 71-100. */
		slotad = vtrnX_.vtrn - 70;

		/*     IF HFDM1 FOR THIS SLOT POSITION IS -1 - NOTHING HAS BEEN
		 *     LOADED INTO THIS SLOT. */
		if( hfdm1X_.hfdm1[slotad-One] == (-1) ){
			vbdataX_.k3n = vbdataX_.k3 + 2;
			return;
			}
		else{

			/*     THE VTR ELEMENTS TO THE RIGHT OF THE SLOT ABOUT TO BE
			 *     UNLOADED ARE SAVED IN VTRFS. */

			k3x = vbdataX_.k3 + 2;
			/*+                                               04/28/86  *R04XYDSD */
			zlen = 461 - k3x;
			zapit(&vtrfs[k3x-One],zlen,(byte)zero);
			for( iz=k3x; iz <= 460; iz++ ){
				/*-                                               04/28/86  *R04XYDSD
				 *+ El Load Confusion                           RKH  04/01/87   R1684 */
				vtrfms[iz-One] = vtrfmX_.vtrfm[iz-One];
				/*-                                             RKH  04/01/87   R1684 */
				vtrfs[iz-One] = vtrfX_.vtrf[iz-One];
				if( vtrfs[iz-One] == 999 )
					break;
				}

			/*     VTRDON IS A FLAG THAT IS IMPORTANT AT THE TOP OF THE VTRF
			 *     PROCESSING LOOP.  IT INDICATES THAT THE VTR JUST PROCESSED IS
			 *     FROM A SLOT. */
			sw11bkX_.vtrdon = 1;

			/*     IM1 IS SAVED FOR THE VTRFS. */

			savim1 = _flowckX_->im1;
			/*+                                               04/28/86  *R04XYDSD */
			iz4sv = _vwarg2X_->iz4;
			}
		}
	/*-                                               04/28/86  *R04XYDSD */

	/*     SLOTAD RECALCULATED FOR RETURN FROM 31082 (PREVIOUS
	 *     UNLOAD OF SAME SLOT) */
L_8000:
	slotad = vtrnX_.vtrn - 70;
	izs = hfdm1X_.hfdm1[slotad-One];
	izs2 = 1;
/* ocr 05.31.01 */
	if(izs>160) {
/* When this happens - smthg went terribly wrong, we dont
	know what exactly, so just drop this sentence */
		errvrsX_.errlvl = 6;
		return;
	}

	/*     THIS LOOP UNLOADS ELEMENTS FROM THE SLOT INTO VTRF */

	for( iz=izs; iz <= 160; iz++ ){
		/*+ El Load Confusion                           RKH  04/01/87   R1684
		 *     IF STRETCHABLE VTRF ELEMENT, REMOVE BIAS */
		if( sw11bkX_.slot[slotad-One][iz-One] > vtrfbiX_.slotel + 20 ){
			vtrfX_.vtrf[izs2-One] = sw11bkX_.slot[slotad-One][iz-One];
			vtrfmX_.vtrfm[izs2-One] = vtrfvlX_.vtmnd;
			/*              '+20' LEAVES ROOM FOR POSITIVE ELEMENT (TRAN5) */
			}
		else if( sw11bkX_.slot[slotad-One][iz-One] <= vtrfbiX_.slotrp ){
			vtrfX_.vtrf[izs2-One] = sw11bkX_.slot[slotad-One][iz-One] - 
			  vtrfbiX_.slotrp;
			vtrfmX_.vtrfm[izs2-One] = vtrfvlX_.vtmrp;
			}
		else{
			vtrfX_.vtrf[izs2-One] = sw11bkX_.slot[slotad-One][iz-One] - 
			  vtrfbiX_.slotel;
			vtrfmX_.vtrfm[izs2-One] = vtrfvlX_.vtmel;
			}
		/*-                                             RKH  04/01/87   R1684 */
		if( vtrfX_.vtrf[izs2-One] == 999 )
			break;
		izs2 += 1;
		}

	/*     IM1 WAS SAVED IN THE SLOT ARRAY AFTER THE 999 - IT IS NOW
	 *     SET FOR VTRF PROCESSING */

	_flowckX_->im1 = sw11bkX_.slot[slotad-One][iz+1-One];
	im81X_.im81 = _flowckX_->im1 - 80;
	_vwarg2X_->iz4 = izs2;
	hfdm1X_.hfdm1[slotad-One] = iz + 2;
	vtrns = vtrnX_.vtrn;
	slotsv = slotad;

	vbdataX_.k3n = 1;

	if( diagsX_.deepdi == 1 )
		{
		fprintf( _spec_fp, " AFTER UNLOAD %6d%6d%6d%6d%6d\n", izs, 
		  izs2, _vwarg2X_->iz4, hfdm1X_.hfdm1[slotad-One], sw11bkX_.cn[slotad-One] );
		}
	/*     ONLY THE LAST LOAD WILL BE FOLLOWED BY A -1000.
	 *     I.E. A -1000 INDICATES THERE IS NOTHING LEFT IN THE SLOT */

	if( sw11bkX_.slot[slotad-One][iz+2-One] == (-1000) ){
		hfdm1X_.hfdm1[slotad-One] = -1;
		sw11bkX_.cn[slotad-One] = 1;

		}
	return;
	/*-                                               04/28/86  *R04XYDSD */
L_7880:
	hf4lX_.hf4l += 1;
	if( hf4lX_.hf4l > sw11bkX_.hf4ct ){
		sw11bkX_.hf4ct = 0;
		hf4lX_.hf4l = 0;
		}
	else{

		hf4ls = sw11bkX_.hf4ct - hf4lX_.hf4l + 1;
		for( iz=1; iz <= 100; iz++ ){
			/*+ El Load Confusion                           RKH  04/01/87   R1684
			 *+    IF STRETCHABLE VTRF ELEMENT, REMOVE BIAS */
			if( sw11bkX_.hf4[hf4ls-One][iz-One] > vtrfbiX_.slotel + 
			  20 ){
				vtrfX_.vtrf[iz-One] = sw11bkX_.hf4[hf4ls-One][iz-One];
				vtrfmX_.vtrfm[iz-One] = vtrfvlX_.vtmnd;
				/*              '+20' LEAVES ROOM FOR POSITIVE ELEMENT (TRAN5) */
				}
			else if( sw11bkX_.hf4[hf4ls-One][iz-One] <= vtrfbiX_.slotrp ){
				vtrfX_.vtrf[iz-One] = sw11bkX_.hf4[hf4ls-One][iz-One] - 
				  vtrfbiX_.slotrp;
				vtrfmX_.vtrfm[iz-One] = vtrfvlX_.vtmrp;
				}
			else{
				vtrfX_.vtrf[iz-One] = sw11bkX_.hf4[hf4ls-One][iz-One] - 
				  vtrfbiX_.slotel;
				vtrfmX_.vtrfm[iz-One] = vtrfvlX_.vtmel;
				}

			/*-                                             RKH  04/01/87   R1684 */
			}

		slotsv = 4;
		vbdataX_.k3n = 1;
		_flowckX_->im1 = sw11bkX_.hf4m1[hf4ls-One];
		im81X_.im81 = _flowckX_->im1 - 80;
		_vwarg2X_->iz4 = 100;
		return;
		}

	/*+                                               04/28/86  *R04XYDSD */
L_8160:
	;
	for( iz=k3x; iz <= 460; iz++ ){
		/*+ El Load Confusion                           RKH  04/01/87   R1684 */
		vtrfmX_.vtrfm[iz-One] = vtrfms[iz-One];
		/*-                                             RKH  04/01/87   R1684
		 *-                                               04/28/86  *R04XYDSD */
		vtrfX_.vtrf[iz-One] = vtrfs[iz-One];
		if( vtrfX_.vtrf[iz-One] == 999 )
			break;
		}

	sw11bkX_.vtrdon = 0;
	_flowckX_->im1 = savim1;
	im81X_.im81 = _flowckX_->im1 - 80;
	/*+                                               04/28/86  *R04XYDSD */
	_vwarg2X_->iz4 = iz4sv;
	/*-                                               04/28/86  *R04XYDSD */
	vbdataX_.k3n = k3x;
	return;
} /*end of function*/

