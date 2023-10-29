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
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*    SUBROUTINE FOR LOADING A VC, CONSTANT OR PUNCT INTO THE OPADRI */
	/*    THIS SUBROUTINE IS CALLED DIRECTLY BY T4VTRPRO AS WELL AS
	 *    BY THE 31 AND 38 SWITCH. */
	/*------------ START VC, PUNCT AND CONSTANT LOAD LOGIC ------------------ */
	/*                   99 THRU 120 ARE CALLED VARIABLE CONSTANTS (IE VC'S) */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*   CHANGES:
	 *     09/01/93 jal:set Black Hole flags if constant is a left Bracket
	 *                  marking a BH fill string.
	 *    04/17/87 *R1685RKH*  Change T1-4 SWORK limit from 50 to
	 *    08/24/86 *R1561DSD: 100 SCONS
	 *    05/15/86 *B0406DSD: REDIMENSION HFPOAD,SCONHF FROM (100,5)
	 *    03/27/86 *R1507DSD: RECORD VTRN FOR LINGUISTS' REFERENCE
	 *    12/03/85 */
	/*                  switch 48 868 settings for filling target Black holes
	 *                     BHPT48 = direct ptr to SCON of targetted Black Hole
	 *                     BHCT48 = count of which BH to fill. */

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




void /*FUNCTION*/ txload()
{
	static long int _l0;
	static char pgmnam[9] = "TxLOAD  ";
	static short iz = 0;
	static short jz = 0;
	int taddress;


	if( (vtrnX_.vtrn >= 99) && (vtrnX_.vtrn <= 120) ){

		/*               IT IS A VARIABLE CONSTANT */

		prtscoX_.sct += 1;
		if( prtscoX_.sct <= SCONY ){
			sconX_.scon[prtscoX_.sct-One][1-One] = vtrnX_.vtrn;
			sconX_.scon[prtscoX_.sct-One][2-One] = vbdataX_.k3p1;
			sconX_.scon[prtscoX_.sct-One][3-One] = 0;
			if( vbdataX_.k3p1 > 98 ){

				/*         IF A HIFREQ CONSTANT, HAS IT'S LOADING BEEN INHIBITED? */
				if( !(vbdataX_.k3p1 <= 500 && 
					  inhbX_.inhb[vbdataX_.k3p1-One] == 1) )
					hpdopoX_.hfdopo[opadroX_.opo+1-One] = -vbdataX_.k3p1;

				vbdataX_.k3p1 = 0;
				sconX_.scon[prtscoX_.sct-One][2-One] = 0;
				goto L_7060;

				/*        IT IS AN ELEMENT WE ARE LOADING INTO THE VC. */

				}
			else if( vbdataX_.k3p1 == 0 ){
				goto L_7060;
				}
			else{

				/*   WHICH OPADRI IS THE HEAD??? */

				flowckX_.n6jim = im81X_.im81 - vbdataX_.k3p1;
				vbdataX_.k3p1 = 0;
				flowckX_.phrstr = sworkX_.phrbeg[flowckX_.n6jim-One];
				flowckX_.phrlst = sworkX_.phrend[flowckX_.n6jim-One];


				hfdoaX_.adct += 1;
				if( opswX_.sw[3] == 1 )
					{
					fprintf( _spec_fp, "    NOW %2d HFDOPO VC\\\"S (%2ld-%2ld)\n", 
					  hfdoaX_.adct, HFPOLO, HFPOHI );
					}
				/*                                                          *1117BT */
				if( hfdoaX_.adct > HFPADY ){
					if( opswX_.sw[3] == 1 )
						{
						fprintf( _spec_fp, " IN ELEMLD - HFDOPO %2ld TO %2ld ALREADY TAKEN \n", 
						  HFPOLO, HFPOHI );
						}
					/*                                                          *1117BT */
					return;
					}
				else{

					jz = 1;

					for( iz=flowckX_.phrstr; iz <= flowckX_.phrlst; iz++ ){
						/*+                                                         *R1117MBS */
						if( !((opadriX_.opadri[iz-One] <= -99 && opadriX_.opadri[iz-One] >= 
						  -120) && hpdopiX_.hfdopi[iz-One] == 0) ){
							if( opadriX_.opadri[iz-One] != -140 ){
								/*-                                                         *R1117MBS */
								hfdoaX_.hfpoad[hfdoaX_.adct-One][jz-One] = opadriX_.opadri[iz-One];
								if( hpdopiX_.hfdopi[iz-One] != 0 )
									hfdoaX_.hfpoad[hfdoaX_.adct-One][jz-One] = hpdopiX_.hfdopi[iz-One];
								hfdoaX_.sconhf[hfdoaX_.adct-One][jz-One] = opadriX_.sconpi[iz-One];
								jz += 1;
								}
							}
						}

					hfdoaX_.hfpoad[hfdoaX_.adct-One][HFPADX-One] = jz - 1;
					hpdopoX_.hfdopo[opadroX_.opo+1-One] = hfdoaX_.adct + HFPOL1;
					goto L_7060;
					}
				}
			}

		}
		/*                   121 THRU 130 ARE PUNCTUATION */
	else if( (vtrnX_.vtrn >= 121) && (vtrnX_.vtrn <= 130) ){
		prtscoX_.sct += 1;
		if( prtscoX_.sct <= SCONY ){
			sconX_.scon[prtscoX_.sct-One][1-One] = 20;

			taddress = vtrnX_.vtrn;
			errvrsX_.err = 0;
/*7-7-99 does not look like it is needed. so comment out and see how translation does. 
			errvrsX_.err = TARG_CODES(&trgflgX_.trgflg,ADR(_l0,2),
					&taddress,LOGCC,
					hfreqX_.hfdo,diagsX_.longdi, _spec_fp);
*/
			if( errvrsX_.err != 0 ){
				errlog(pgmnam,7030,taddress,1);
				return;
				}

			sconX_.scon[prtscoX_.sct-One][3-One] = 0;
			goto L_7060;
			}

		}
		//                   HI FREQ CONSANTS?
	else if( vtrnX_.vtrn < 131 ){
		return;
		}

		/*                    IS FRENCH CONSTANT INHIBITED ??? */
	else if( vtrnX_.vtrn <= 500 && inhbX_.inhb[vtrnX_.vtrn-One] == 1 ){
		return;
		}

	else{
		prtscoX_.sct += 1;
		if( prtscoX_.sct <= SCONY ){
			sconX_.scon[prtscoX_.sct-One][1-One] = 21;

			taddress = vtrnX_.vtrn;
			errvrsX_.err = 0;
/*7-7-99 does not look like it is needed. so comment out and see how translation does. 
			errvrsX_.err = TARG_CODES(&trgflgX_.trgflg,ADR(_l0,2),
			  &taddress,LOGCC,hfreqX_.hfdo,diagsX_.longdi, _spec_fp);
*/
			if( errvrsX_.err != 0 ){
				errlog(pgmnam,7050,taddress,1);
				return;
				}

			/*+    RECORD VTRN FOR LINGUISTS' REFERENCE       03/27/86  *R1507DSD */
			sconX_.scon[prtscoX_.sct-One][2-One] = vtrnX_.vtrn;
			sconX_.scon[prtscoX_.sct-One][3-One] = 0;
			goto L_7060;
			}
		}

	/*            SCT GREATER THAN NO.SCONS, JUST PUT OUT AN ERROR MESSAGE.
	 *+                                         *B0305JGB */
	if( prtscoX_.sct > SCONY ){
		prtscoX_.sct = SCONY;
		prtscoX_.scterr += 1;
		if( diagsX_.longdi != 0)
			{
			fprintf( _spec_fp, " TxLOAD, OVERLOADING SCON ARRAY, SCTERR =%4d\n", 
			  prtscoX_.scterr );
			}
		if( prtscoX_.scterr == 1 )
			errlog(pgmnam,8870,500,13);
		}

	return;

L_7060:
	opadroX_.opo += 1;
	if( opadroX_.opo > OPADRX ){
		errlog(pgmnam,8860,501,5);
		}
	else{
		sconX_.scon[prtscoX_.sct-One][8-One] = vbdataX_.k3p1;
		opadroX_.opadro[opadroX_.opo-One] = -vtrnX_.vtrn;
		opadroX_.sconpo[opadroX_.opo-One] = prtscoX_.sct;
		/*                  set Black Hole flags if constant is a left Bracket
		 *                  marking a BH fill string. */
		if( vtrnX_.vtrn == BFLFBR ){
			sconX_.scon[prtscoX_.sct-One][BFBHFL-One] = sw48bhX_.bhpt48;
			sconX_.scon[prtscoX_.sct-One][BFBHPO-One] = sw48bhX_.bhct48;

			}
		}
	return;
} /*end of function*/

