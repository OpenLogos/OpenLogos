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
	/* LAST CHG 06/09/86 *R1530DSD: MARK ELEMENTS AND REL. PTRS LOADED
	 *      CHG 04/29/86 *B04XYDSD - USE ALL OF VTRF(460), VTRFS(460) */
	/*+                                               04/28/86  *R04XYDSD */
	/*-                                               04/28/86  *R04XYDSD
	 *+    VTRFM REMEMBERS VTRF 'TYPES'  FROM VTRFWR  06/09/86  *R1530DSD */
	/*     SLOTEL,SLOTRP BIASSES MARK 'E'LEMENT, 'REL-'PTR SAVED IN SLOT */
	/*-                                               06/09/86  *R1530DSD */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*     CALLED BY VTRPRO
	 *     THIS SUBROUTINE WILL UNLOAD THE CONTENT OF THE SLOT INTO THE VTRF
	 *     ARRAY SO THAT IT CAN BE PROCESSED. IT RETURNS CONTROL TO VTRPRO.
	 *     VTRPRO WILL RETURN TO SLOTLD ONCE THE SLOT HAS BEEN PROCESSED IN
	 *     ORDER TO PICK UP THE BALANCE OF THE VTR. */
	/*     ARE WE RETURNING TO THIS SUBROUTINE AFTER HAVING PROCESSED A SLOT?
	 *     IF YES, THEN WE SHOULD RESTORE THE REST OF THE VTR SO THAT IT CAN
	 *     BE PROCESSED? */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#define MS_F77
#include <fcrt.h>
#include "logoslib.h"
#include "trans.h"
#include "projexts.h"
#include "parsetrans.h"
#include <jbctrl.h>
#include "parsetrans_ext.h"



void /*FUNCTION*/ slotld_x()
{
	static byte vtrfms[460];
	static short int iz4sv, vtrfs[460];
	static long int zlen;
	static short iz = 0;
	static short izs = 0;
	static short k3x = 0;
	static short izs2 = 0;
	static short vtrns = 0;
	static short savim1 = 0;
	static short slotad = 0;
	static short slotsv = 0;
	static short zero = 0;

	if( !(vtrnX_.vtrn == 999 && cnX_.vtrdon == 1) ){

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
			zlen = 461 - k3x;
			zapit(&vtrfs[k3x-One],zlen,(byte)zero);
			for( iz=k3x; iz <= 460; iz++ ){
				vtrfms[iz-One] = vtrfmX_.vtrfm[iz-One];
				vtrfs[iz-One] = vtrfX_.vtrf[iz-One];
				if( vtrfs[iz-One] == 999 )
					break;
				}

			/*     VTRDON IS A FLAG THAT IS IMPORTANT AT THE TOP OF THE VTRF
			 *     PROCESSING LOOP.  IT INDICATES THAT THE VTR JUST PROCESSED IS
			 *     FROM A SLOT. */
			cnX_.vtrdon = 1;

			/*     IM1 IS SAVED FOR THE VTRFS. */
			savim1 = flowckX_.im1;
			iz4sv = vwarg2X_.iz4;
			}

		/*     RETURN FROM VTRF UNLOAD - IF VTRN = 999 AND VTRDON = 1 */
		}
	else if( hfdm1X_.hfdm1[slotsv-One] == (-1) ){

		cnX_.cn[slotsv-One] = 1;



		for( iz=k3x; iz <= 460; iz++ ){
			vtrfmX_.vtrfm[iz-One] = vtrfms[iz-One];
			vtrfX_.vtrf[iz-One] = vtrfs[iz-One];
			if( vtrfX_.vtrf[iz-One] == 999 )
				break;
			}

		cnX_.vtrdon = 0;
		flowckX_.im1 = savim1;
		im81X_.im81 = flowckX_.im1 - 80;
		vwarg2X_.iz4 = iz4sv;
		vbdataX_.k3n = k3x;
		return;
		}
	else{
		vtrnX_.vtrn = vtrns;
		}

	/*     SLOTAD RECALCULATED FOR RETURN FROM 31082 (PREVIOUS
	 *     UNLOAD OF SAME SLOT) */
	slotad = vtrnX_.vtrn - 70;
	izs = hfdm1X_.hfdm1[slotad-One];
	izs2 = 1;

	/*     THIS LOOP UNLOADS ELEMENTS FROM THE SLOT INTO VTRF */

	for( iz=izs; iz <= 160; iz++ ){
		/*+    IF STRETCHABLE VTRF ELEMENT, REMOVE BIAS   06/09/86  *R1530DSD */
		if( cnX_.slot[slotad-One][iz-One] > vtrfbiX_.slotel + 20 ){
			vtrfX_.vtrf[izs2-One] = cnX_.slot[slotad-One][iz-One];
			vtrfmX_.vtrfm[izs2-One] = vtrfvlX_.vtmnd;
			/*              '+20' LEAVES ROOM FOR POSITIVE ELEMENT (TRAN5) */
			}
		else if( cnX_.slot[slotad-One][iz-One] <= vtrfbiX_.slotrp ){
			vtrfX_.vtrf[izs2-One] = cnX_.slot[slotad-One][iz-One] - vtrfbiX_.slotrp;
			vtrfmX_.vtrfm[izs2-One] = vtrfvlX_.vtmrp;
			}
		else{
			vtrfX_.vtrf[izs2-One] = cnX_.slot[slotad-One][iz-One] - vtrfbiX_.slotel;
			vtrfmX_.vtrfm[izs2-One] = vtrfvlX_.vtmel;
			}

		if( vtrfX_.vtrf[izs2-One] == 999 )
			break;
		izs2 += 1;
		}

	/*     IM1 WAS SAVED IN THE SLOT ARRAY AFTER THE 999 - IT IS NOW
	 *     SET FOR VTRF PROCESSING */

	flowckX_.im1 = cnX_.slot[slotad-One][iz+1-One];
	im81X_.im81 = flowckX_.im1 - 80;

	vwarg2X_.iz4 = izs2;
	hfdm1X_.hfdm1[slotad-One] = iz + 2;
	vtrns = vtrnX_.vtrn;
	slotsv = slotad;

	vbdataX_.k3n = 1;

	if( diagsX_.deepdi == 1 )
		{
		fprintf( _spec_fp, " AFTER UNLOAD %6d%6d%6d%6d%6d\n", izs, 
		  izs2, vwarg2X_.iz4, hfdm1X_.hfdm1[slotad-One], cnX_.cn[slotad-One] );
		}
	/*     ONLY THE LAST LOAD WILL BE FOLLOWED BY A -1000.
	 *     I.E. A -1000 INDICATES THERE IS NOTHING LEFT IN THE SLOT */

	if( cnX_.slot[slotad-One][iz+2-One] == (-1000) ){
		hfdm1X_.hfdm1[slotad-One] = -1;
		cnX_.cn[slotad-One] = 1;

		}
	return;
} /*end of function*/

