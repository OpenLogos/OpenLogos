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
/* *           4/27/94 jal:  copy the alternate WC (scon(8)) and SCON(10) from
 * *      #C          each transfer item in the source VC to it corresponding
 * *      #C          transfer item in the target VC.  Do this only when a
 * *      #C          single source item (address) is copied to an empty VC.
 * *      #C          This is the only case where the copied VC items do not
 * *      #C          refer to the original's SCONs, but get new ones.
 * *
 * *         Rev 1.0   04/05/94 15:18:18   pvcs
	/*  CHANGES:
	 *     4/27/94 jal:  copy the alternate WC (scon(8)) and SCON(10) from
	 *          each transfer item in the source VC to it corresponding
	 *          transfer item in the target VC.  Do this only when a
	 *          single source item (address) is copied to an empty VC.
	 *          This is the only case where the copied VC items do not
	 *          refer to the original's SCONs, but get new ones.
	 *     LAST CHG: 05/22/87 *R1683RKH*  Copy Function of -44 Switch */
	/*     New Function : COPY FUNCTION of -44 Switch */
	/*     K3P1, K3P3 = REL POINTERS
	 *     K3P2, K3P4 = VC's
	 *     Move VC K3P2 to K3P4 (See R1683 Description for details) */
	/*     Flags to tell if there are many elements in a VC 
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
	byte vtmnd, vtmrp, vtmel, vtmpp, vtmwk, vtmsl, vtmvc, vtmpu, vtmhi, vtmlo, vtmsw;
	}	vtrfvlX_;

void /*FUNCTION*/ copy144()
{
	static LOGICAL8 smany, tmany;
	static byte vtrfm1, vtrfm3;
	static short int ad1, adr_, bytlft, jj, k3p2, k3p3, k3p4, last1, 
	  nxtpos, phrbeg, phrlst, srchfd, tgtadr, tgthfd, vc1loc, vc2loc;
	static long int bytes;


	static char pgmnam[9] = "COPY44  ";
	static short zero = 0;

	k3p2 = vtrfX_.vtrf[vbdataX_.k3+2-One];
	k3p3 = vtrfX_.vtrf[vbdataX_.k3+3-One];
	k3p4 = vtrfX_.vtrf[vbdataX_.k3+4-One];

	vtrfm1 = vtrfmX_.vtrfm[vbdataX_.k3+1-One];
	vtrfm3 = vtrfmX_.vtrfm[vbdataX_.k3+3-One];

	/*     Check if K3P2 VC exists in K3P1's OPADRO */

	if( vbdataX_.k3p1 < 0 ){
		phrbeg = sworkoX_.phrbgo[im81X_.im81-vbdataX_.k3p1-One];
		phrlst = sworkoX_.phrndo[im81X_.im81-vbdataX_.k3p1-One];
		}
	else{
		phrbeg = sworkoX_.phrbgo[im81X_.im81+vbdataX_.k3p1-One];
		phrlst = sworkoX_.phrndo[im81X_.im81+vbdataX_.k3p1-One];
		}

	for( jj=phrbeg; jj <= phrlst; jj++ ){
		vc1loc = jj;
		if( opadroX_.opadro[jj-One] == -k3p2 )
			goto L_60;

		/*     VC K3P2 was not found in OPADRO, simply return */

		}
	return;

	/*     VC K3P2 exists in OPADRO. Do the same check for K3P4 */

L_60:
	;

	/*     Check if K3P4 VC exists in K3P3's OPADRO */

	if( k3p3 < 0 ){
		phrbeg = sworkoX_.phrbgo[im81X_.im81-k3p3-One];
		phrlst = sworkoX_.phrndo[im81X_.im81-k3p3-One];
		}
	else{
		phrbeg = sworkoX_.phrbgo[im81X_.im81+k3p3-One];
		phrlst = sworkoX_.phrndo[im81X_.im81+k3p3-One];
		}

	for( jj=phrbeg; jj <= phrlst; jj++ ){
		vc2loc = jj;
		if( opadroX_.opadro[jj-One] == -k3p4 )
			goto L_100;

		/*     VC K3P4 was not found in OPADRO, simply return */

		}
	return;

	/*     Both VC's were found. Move or Copy contents */

L_100:
	;
	srchfd = hpdopiX_.hfdopi[vc1loc-One];
	tgthfd = hpdopiX_.hfdopi[vc2loc-One];

	/*     If source and target are both zeroes then return */

	if( !(srchfd == 0 && tgthfd == 0) ){

		/*     If SRCHFD is 0 and target is not then overlay of 08X
		 *     and do nothing for -8X */

		if( srchfd == 0 ){
			if( vtrfm3 == vtrfvlX_.vtmpp )
				hpdopiX_.hfdopi[vc2loc-One] = 0;
			}
		else{

			smany = FALSE;
			tmany = FALSE;
			if( srchfd >= HFPOLO && srchfd <= HFPOHI )
				smany = TRUE;
			if( tgthfd >= HFPOLO && tgthfd <= HFPOHI )
				tmany = TRUE;

			/*     If the target is to be overlayed, do it now */

			if( vtrfm3 == vtrfvlX_.vtmpp ){
				if( tmany ){
					tgtadr = tgthfd - HFPOL1;
					zapit(&hfdoaX_.hfpoad[tgtadr-One][1-One],SIZ,(byte)zero);
					zapit(&hfdoaX_.sconhf[tgtadr-One][1-One],SIZ,(byte)zero);
					}
				else{
					hpdopiX_.hfdopi[vc2loc-One] = 0;
					}
				}

			/*     Find out which case is to be performed
			 *         CASE 1 - MOVE single element to single element
			 *         CASE 2 - MOVE single element to many elements
			 *         CASE 3 - MOVE many elements to a single element
			 *         CASE 4 - Move many elements to many elements */

			if( smany && tmany ){

				/* CASE 4
				 *     Many elements to be moved to many elements */


				/*     Find next available position for target */

				adr_ = tgthfd - HFPOL1;
				nxtpos = hfdoaX_.hfpoad[adr_-One][HFPADX-One] + 1;

				/*     Find the string that is to be moved */

				ad1 = srchfd - HFPOL1;
				last1 = hfdoaX_.hfpoad[ad1-One][HFPADX-One];
				bytes = 2*last1;

				/*     Do we have enough space */

				bytlft = HFPADX - nxtpos - last1 - 1;
				if( bytlft >= 0 ){

					lmove(&hfdoaX_.hfpoad[adr_-One][nxtpos-One],1,
					  &hfdoaX_.hfpoad[ad1-One][1-One],1,bytes);
					lmove(&hfdoaX_.sconhf[adr_-One][nxtpos-One],1,
					  &hfdoaX_.sconhf[ad1-One][1-One],1,bytes);
					hfdoaX_.hfpoad[adr_-One][HFPADX-One] = nxtpos + 
					  last1 - 1;
					goto L_200;
					}
				}
			else if( tmany ){

				/* CASE 2 : Single element to be moved to many elements */


				if( vtrfm3 == vtrfvlX_.vtmrp ){

					/*         Case 2.1 - Target is -8X for append */

					adr_ = tgthfd - HFPOL1;
					nxtpos = hfdoaX_.hfpoad[adr_-One][HFPADX-One] +  1;

					/*         Check if there is 1 more position available */

					if( nxtpos < HFPADX ){

						hfdoaX_.hfpoad[adr_-One][nxtpos-One] = srchfd;
						hfdoaX_.sconhf[adr_-One][nxtpos-One] = opadroX_.sconpo[vc1loc-One];

						hfdoaX_.hfpoad[adr_-One][HFPADX-One] = nxtpos;
						goto L_200;
						}
					}
				else{

					/*         Case 2.2 - Target is 08X for replacing */

					hpdopiX_.hfdopi[vc2loc-One] = srchfd;
					opadroX_.sconpo[vc2loc-One] = opadroX_.sconpo[vc1loc-One];
					adr_ = tgthfd - HFPOL1;
					zapit(&hfdoaX_.hfpoad[adr_-One][1-One],SIZ,(byte)zero);
					zapit(&hfdoaX_.sconhf[adr_-One][1-One],SIZ,(byte)zero);
					goto L_200;
					}
				}
			else{
				if( smany ){

					/* CASE 3 : Many elements to be moved to a single element */


					/*     Find the next available HFPOAD */

					hfdoaX_.adct += 1;
					if( hfdoaX_.adct <= HFPADY ){

						/*     Move the Target element from HFDOPI to HFPOAD
						 *     only if the target element is not zero and of -8X type */

						if( vtrfm3 == vtrfvlX_.vtmrp && tgthfd != 0 ){
							hfdoaX_.hfpoad[hfdoaX_.adct-One][1-One] = tgthfd;
							hfdoaX_.sconhf[hfdoaX_.adct-One][1-One] = opadroX_.sconpo[vc2loc-One];
							nxtpos = 2;
							}
						else{
							nxtpos = 1;
							}

						/*     Find the string that is to be moved */

						ad1 = srchfd - HFPOL1;
						last1 = hfdoaX_.hfpoad[ad1-One][HFPADX-One];
						bytes = 2*last1;

						/*     Do we have enough space */

						bytlft = HFPADX - nxtpos - last1;
						if( bytlft < 0 )
							goto L_240;

						lmove(&hfdoaX_.hfpoad[hfdoaX_.adct-One][nxtpos-One],
						  1,&hfdoaX_.hfpoad[ad1-One][1-One],1,bytes);
						lmove(&hfdoaX_.sconhf[hfdoaX_.adct-One][nxtpos-One],
						  1,&hfdoaX_.sconhf[ad1-One][1-One],1,bytes);
						hfdoaX_.hfpoad[hfdoaX_.adct-One][HFPADX-One] = nxtpos + last1 - 1;
						hpdopiX_.hfdopi[vc2loc-One] = hfdoaX_.adct + HFPOL1;
						goto L_200;
						}

					/* CASE 1 : Single element has to be moved to a single location */

					}
				else if( vtrfm3 == vtrfvlX_.vtmpp || tgthfd == 0 ){

					/*         Target is either 0 or 08X type for overlay */

					hpdopiX_.hfdopi[vc2loc-One] = srchfd;
					/*+2                         copy attributes          4/27/94 jal */
					sconX_.scon[opadroX_.sconpo[vc2loc-One]-One][8-One] 
				   =sconX_.scon[opadroX_.sconpo[vc1loc-One]-One][8-One];
					sconX_.scon[opadroX_.sconpo[vc2loc-One]-One][10-One] 
				   =sconX_.scon[opadroX_.sconpo[vc1loc-One]-One][10-One];
					goto L_200;
					}
				else{

					/*         Target is non zero & -8X type for appending
					 *         Create a HFPOAD array with source and target elements */

					hfdoaX_.adct += 1;
					if( hfdoaX_.adct <= HFPADY ){

						hfdoaX_.hfpoad[hfdoaX_.adct-One][1-One] = tgthfd;
						hfdoaX_.sconhf[hfdoaX_.adct-One][1-One] = opadroX_.sconpo[vc2loc-One];

						hfdoaX_.hfpoad[hfdoaX_.adct-One][2-One] = srchfd;
						hfdoaX_.sconhf[hfdoaX_.adct-One][2-One] = opadroX_.sconpo[vc1loc-One];

						hpdopiX_.hfdopi[vc2loc-One] = hfdoaX_.adct + 
						  HFPOL1;
						hfdoaX_.hfpoad[hfdoaX_.adct-One][HFPADX-One] = 2;
						goto L_200;
						}
					}

				/*     No more HFDOPI positions available */

				if( opswX_.sw[10-One] == 1  )
					{
					fprintf( _spec_fp, " IN COPY44 - HFDOPI 65 to 74   Already Taken\n" );
					}
				errlog(pgmnam,160,160,0);
				return;
				}

			/*     ************** Error Returns ****************** */

			/*     Come here if HFPOAD array is overloading */

L_240:
			;
			if( opswX_.sw[10-One] == 1  )
				{
				fprintf( _spec_fp, " IN COPY44 - Overloading HFPOAD array\n" );
				}
			errlog(pgmnam,240,240,0);
			return;

L_200:
			;

			/*     The move has been done. Now erase source if needed */

			if( vtrfm1 == vtrfvlX_.vtmpp ){
				if( smany ){
					adr_ = srchfd - HFPOL1;
					zapit(&hfdoaX_.hfpoad[adr_-One][1-One],SIZ,(byte)zero);
					zapit(&hfdoaX_.sconhf[adr_-One][1-One],SIZ,(byte)zero);
					}
				hpdopiX_.hfdopi[vc1loc-One] = 0;
				}
			}
		}
	return;
} /*end of function*/

