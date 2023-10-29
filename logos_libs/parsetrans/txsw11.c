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
	/*     FUNCTION:  LOAD ALL DATA TO RIGHT OF SWITCH, INCLUDING
	 *       SWITCHES BUT EXCLUDING -41 SWITCH INTO THE SLOT ARRAY
	 *       DESIGNATED BY THE PARAMETER OF THE -11 SWITCH. */
	/*     SLOTS ARE NUMBERED 71-100.  SLOTAD SUBTRACTS 70 AND
	 *       LOADS THE SLOT ARRAYS 1-30. */
	/** LAST CHANGE 87/06/09 LG002GBA Rphs3:
	 *     LAST CHG: 05/26/87 *R1725RKH*  -31011 is a valid Stopper for -11 S
	 *          CHG: 04/17/87 *R1685RKH*  Change T1-4 SWORK limit from 50 to
	 *     VTRFM REMEMBERS VTRF 'TYPES'  FROM VTRFWR */
	/*     SLOTEL,SLOTRP BIASSES MARK 'E'LEMENT, 'REL-'PTR SAVED IN SLOT */
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



void /*FUNCTION*/ txsw11(
short int *retflg
)
{
	static byte vtrfmn;
	static short int cn2, gb, iz2, sldump, slota2, x;
	static char pgmnam[9] = "TxSW11  ";
	static short iz = 0;
	static short sltflg = 0;
	static short st = 0;
	static short cn1 = 0;
	static short mscn = 0;
	static short eleven = 0;
	static short slotad = 0;

	sldump = 0;
	*retflg = 0;
	slotad = vbdataX_.k3p1 - 70;
	eleven = 0;
	st = vbdataX_.k3 + 2;
	/*+                                                     *09/06/89*JAL*
	 *-------------------------------------------------------------------
	 *       SLOT LOAD ILLEGAL IF CURRENTLY IN A "CLAUSE MOVE STATE",
	 *                   I.E. IF IN PROCESS OF IDENTIFYING AND RELOCATING A
	 *                   CLAUSE.  NSWORKS WILL MOVE AND SLOT UNLOADS COULD
	 *                   WREAK HAVOC.  SKIP -11SW WITHOUT SLOT LOADING. */
	if( clsmovX_.clmcnt == 0 ){

		/*       NEW‹ NEW PHRASE SUPRESSION */

		if( vtrfX_.vtrf[vbdataX_.k3-2-One] != -40 && opadroX_.opo == phsupX_.n3sv )
			phsupX_.phsup = 1;
		if( vtrfX_.vtrf[vbdataX_.k3-2-One] == -24 )
			phsupX_.phsup = 1;

		/*       IF SW26 ASSUMED PHSUP WAS OFF, CORRECT PHR26 */

		if( phsupX_.phsup == 1 && sw26bkX_.phr26 == (sworkoX_.phcto + 1) )
			sw26bkX_.phr26 -= 1;

		cn1 = cnX_.cn[slotad-One];

		/*     CN1 IS COUNTER FOR ITEMS LOADED INTO SLOT ARRAY -
		 *       COUNTER IS POSITIONED SO THAT FOR EACH ADDITIONAL LOAD TO THIS
		 *       SLOT, THE -1000 IS OVERLAYED. */

		/*     START MAJOR SLOT ARRAY LOAD LOOP.  IZ IS CURRENT VTR WE ARE
		 *       LOADING.  SLOT ARRAY IS LOADED WITH:
		 *       1) ALL THE ELEMENTS TO THE RIGHT OF SWITCH - INCLUDING 999
		 *          EXCLUDING -41 SWITCH.
		 *       2) IM1
		 *       3) -1000 (SIGNALS END OF LOAD IN UNLOAD PORTION) */

		for( iz=st; iz <= vwarg2X_.iz4; iz++ ){
			vtrnX_.vtrn = vtrfX_.vtrf[iz-One];
			/*+       AN ELEMENT CAN STRETCH TO '-11'         06/17/86  *R1530DSD */
			vtrfmn = vtrfmX_.vtrfm[iz-One];
			if( !(vtrfmn == vtrfvlX_.vtmel || (((((vtrfX_.vtrf[iz-One] != 
			  -11 && vtrfX_.vtrf[iz-One] != -57) && vtrfX_.vtrf[iz-One] != 
			  -65) && vtrfX_.vtrf[iz-One] != -64) && vtrfX_.vtrf[iz-One] != 
			  -63) && vtrfX_.vtrf[iz-One] != -31)) ){
				/*+ -31011 is a valid Stopper for -11 SW        RKH  05/26/87   R1725 */
				if( !(vtrfX_.vtrf[iz-One] == -31 && vtrfX_.vtrf[iz+1-One] != 
				  11) )
					goto L_262;
				}

			/************************************************************************
			 *GB+ */
			if( !(vtrnX_.vtrn <= 70 || vtrnX_.vtrn >= 100) ){
				/*         MAKE ATTEMPT TO DISTINGUISH BETWEEN A REAL SLOT UNLOAD
				 *         AND A SWITCH PARAMETER IN THE SLOT RANGE.  THIS WILL
				 *         NOT BE FOOLPROOF; TO MAKE IT SO MEANS KNOWING NUMBER OF
				 *         PARAMETERS FOR EACH SWITCH OR SETTING VTRFM IN VTRFWR()
				 *         TO INDICATE A SLOT.   THIS SLOT UNLOAD ASSUMES
				 *         SLOT UNLOAD IS FOLLOWED BY "000" & NEVER PRECEDED BY SWITCH */
				if( vtrfX_.vtrf[iz+1-One] == 0 ){
					if( !(vtrfX_.vtrf[iz-1-One] < 0 && vtrfX_.vtrf[iz-1-One] > -70) ){

						/*         IF PREVIOUS VTR ELS WERE NOT FROM A SLOT DUMP THEN
						 *         DO HOUSEKEEPING TO RECORD PROPER POINTER */

						if( sldump != (iz - 2) && iz != st ){
							cnX_.slot[slotad-One][cn1-One] = 999;
							cn1 += 1;
							cnX_.slot[slotad-One][cn1-One] = flowckX_.im1;
							cn1 += 1;
							}

						/*         SET SLDUMP TO CURRENT IZ POSITION.  LATER WE WILL USE
						 *         SLDUMP TO TELL US WHAT HAS JUST BEEN PROCESSED IN -11 SWITCH */

						sldump = iz;



						/*   SLOT WITHIN THIS SLOT. UNLOAD ITS ELEMENTS INTO SLOT BEING LOADED
						 *   AND SET HFDM1(VTRN) TO -1 TO INDICATE THIS SLOT IS EMPTY */

						slota2 = vtrnX_.vtrn - 70;
						sltflg = 1;
						if( hfdm1X_.hfdm1[slota2-One] == -1 )
							goto L_263;

						/*   SUBTRACTION OF 1 PREVENTS LOAD OF -1000 AT END OF SLOT */
						cn2 = cnX_.cn[slota2-One] - 1;

						for( iz2=1; iz2 <= cn2; iz2++ ){
							cnX_.slot[slotad-One][cn1-One] = cnX_.slot[slota2-One][iz2-One];
							cn1 += 1;
							if( cn1 >= 159 )
								goto L_120;
							}
						hfdm1X_.hfdm1[slota2-One] = -1;
						cnX_.cn[slota2-One] = 1;
						if( diagsX_.deepdi == 1  )
							{
							fprintf( _spec_fp, " SLOTAD,SLOTA2,SLDUMP%4d%4d%4d", 
							  slotad, slota2, sldump );
							fprintf( _spec_fp, " SLOT\n" );
							for( gb=1; gb <= 160; gb++ ){
								fprintf( _spec_fp, "%6d", cnX_.slot[slotad-One][gb-One] );
								}
							fprintf( _spec_fp, "\n" );
							}
						goto L_263;
						}
					}
				}

			/*     SLTFLG EQUAL TO 1 MEANS A SLOT WITHIN A SLOT HAS BEEN PROCESSED.
			 *     SKIP OVER THIS 0, I.E. DO NOT LOAD IT INTO SLOT. */

			if( sltflg == 1 ){
				sltflg = 0;
				/************************************************************************
				 *GB- */

				}
			else if( vtrnX_.vtrn == 999 && sldump == (iz - 2) ){
				goto L_264;
				}
			else{
				/*-                                                         *JGB31JGB
				 *     NOW STORE THE VTR ELEMENT
				 *+    IF STRETCHABLE VTRF ELEMENT, BIAS IT       06/17/86  *R1530DSD */
				if( vtrfmn == vtrfvlX_.vtmel ){
					cnX_.slot[slotad-One][cn1-One] = vtrnX_.vtrn + vtrfbiX_.slotel;
					}
				else if( vtrfmn == vtrfvlX_.vtmrp ){
					cnX_.slot[slotad-One][cn1-One] = vtrnX_.vtrn +  vtrfbiX_.slotrp;
					}
				else{
					cnX_.slot[slotad-One][cn1-One] = vtrnX_.vtrn;
					}

				cn1 += 1;
				if( vtrnX_.vtrn == 999 )
					break;
				if( cn1 > 158 )
					goto L_120;
				}

L_263:
			;
			}
		goto L_180;
L_264:
		cn1 -= 1;
		goto L_200;

L_120:
		if( diagsX_.deepdi == 1  ){
			fprintf( _spec_fp, " SW11 OVERLOADING SLOT \n" );
			}
		errlog(pgmnam,1580,511,9);
		goto L_240;
		/*-                                             RKH  05/26/87   R1725
		 *-                                               06/17/86  *R1530DSD
		 *     ANOTHER ELEVEN SWITCH ENCOUNTERED - THIS IS NOT LOADED INTO SLOT
		 *       ALSO, THIS COULD BE A -57 SWITCH. */

L_262:
		vbdataX_.k3n = iz;
		eleven = 1;
		/*+                                                         *JGB31JGB
		 *      DON'T WANT 999/IM1 IF LAST PROCESSED VTR EL WAS A SLOT DUMP */

		if( sldump == (iz - 2) ){
			cn1 -= 1;
			goto L_200;
			}
		else{
			/*-                                                         *JGB31JGB */
			cnX_.slot[slotad-One][cn1-One] = 999;
			cn1 += 1;
			}

L_180:
		cnX_.slot[slotad-One][cn1-One] = flowckX_.im1;
L_200:
		x = x;
		if( cnX_.cn[slotad-One] != cn1 + 1 ){
			hfdm1X_.hfdm1[slotad-One] = 1;

			/*       -1000 SIGNALS END OF THIS SLOTAD */

			cnX_.slot[slotad-One][cn1+1-One] = -1000;
			cnX_.cn[slotad-One] = cn1 + 1;

			/*       COUNT OF ITEMS LOADED IN SLOT ARRAY IS CN1. */

			mscn = cn1 + 1;
			}

		if( diagsX_.deepdi == 1  )
			{
			fprintf( _spec_fp, " SW11 %6d%6d%6d%6d%6d%6d%6d%6d%6d%6d", 
			  slotad, eleven, flowckX_.im1, cn1, st, vtrnX_.vtrn, 
			  vbdataX_.k3p1, vbdataX_.k3n, hfdm1X_.hfdm1[slotad-One], 
			  cnX_.cn[slotad-One] );
			fprintf( _spec_fp, "\n" );
			for( iz=1; iz <= mscn; iz++ ){
				fprintf( _spec_fp, "%6d", cnX_.slot[slotad-One][iz-One] );
				}
			fprintf( _spec_fp, "\n" );
			}

		if( eleven == 1 )
			goto L_260;
		}
	else{
		/*                   SEND ERROR MESSAGE */
		if( diagsX_.deepdi == 1  ){
			fprintf( _spec_fp, "              ***** ERROR ******\n   SW11 CALLED WHILE PARSING A CLAUSE FOR RELOCATION\n              ******************\n" );
			}
		errlog(pgmnam,11,111,9);
		/*                   FIND END OF -11 SW FOR RETURN TO VTRPRO */
		for( iz=st; iz <= vwarg2X_.iz4; iz++ ){
			vtrnX_.vtrn = vtrfX_.vtrf[iz-One];
			vtrfmn = vtrfmX_.vtrfm[iz-One];
			/*                        IF THIS SWITCH IS A -11SW STOPPER RETURN */
			if( (((((vtrfmn != vtrfvlX_.vtmel && vtrfX_.vtrf[iz-One] == 
			  -11) || vtrfX_.vtrf[iz-One] == -57) || vtrfX_.vtrf[iz-One] == 
			  -65) || vtrfX_.vtrf[iz-One] == -64) || vtrfX_.vtrf[iz-One] == 
			  -63) || (vtrfX_.vtrf[iz-One] == -31 && vtrfX_.vtrf[iz+1-One] == 
			  11) )
				goto L_261;
			/*                        IF 999 THEN AT VTR END. */
			if( vtrfX_.vtrf[iz-One] == 999 )
				break;
			/*                        VTR IS FINISHED BY DEFAULT */
			}
		goto L_240;
L_261:
		vbdataX_.k3n = iz;
		goto L_260;
		}


	/*    9400 MEANS THAT WE ARE DONE PROCESSING THE VTR. THEREFORE, WE CALL
	 *    VTREND AND THEN EXIT THE VTRPRO PROGRAM UPON RETURNING TO IT BY
	 *    SETTING RETFLG = 1. */

L_240:
	*retflg = 1;

	/*     1300  MEANS THAT THERE ARE OTHER -11 SWITCHES TO BE PROCESSED IN
	 *           THIS VTR. */

L_260:
	;
	if( iz >= vwarg2X_.iz4 || vtrnX_.vtrn == 999 ){
		vbdataX_.k3 = vwarg2X_.iz4;
		vbdataX_.k3n = vwarg2X_.iz4;
		}
	return;
} /*end of function*/

