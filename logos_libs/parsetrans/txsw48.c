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
/*   Version of SW48 that applies to all trans */
	/*                     -48 02X functions */
	/*    ADD -48SW FUNCTION 044   FOR SETTING SCON5  OGM 12/17/86  R1628+ */
	/*    ADD -48SW FUNCTION 044   FOR SETTING SCON5  OGM 12/17/86 R1628- */
	/*                  switch 48 868 settings for filling target Black holes
	 *                     BHPT48 = direct ptr to SCON of targetted Black Hole
	 *                     BHCT48 = count of which BH to fill. */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*SW48 */
	/*   ***** BEGINNING OF -48 SWITCH ***** */
	/*            1    2    3    4    5    6    7    8    9    10 */


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <fcrt.h>
#include "trans.h"
#include "logoslib.h"
#include "projexts.h"
#include <string.h>
#include "parsetrans_ext.h"
#include <jbctrl.h>




void /*FUNCTION*/ txsw48(prm1, prm2, prm3)
long int prm1, prm2, prm3;
{
	static LOGICAL32 chkscn, useinp;
	static short int adxx, hfptxx, newval, optmp, opxx, phrfrs, ret, scncol, scnrow, scpxx, swk;
	static char pgmnam[9] = "TXSW48  ";

	if( !((prm1 >= 1 && prm1 <= 19) || (prm1 >= 24 && prm1 <= 30))){
		if( prm1 >= 20 && prm1 <= 23 ){

			/*        -48 02X -8X SCON   apply SCON value to all elements
			 *                           concatenated under -8X head element
			 *            020   - apply only to unset (scon=0) and unlocked scons
			 *                    concatenated in the input OPADR
			 *            021   - apply only to unset (scon=0) and unlocked scons
			 *                    concatenated in the output OPADR
			 *            022   - apply only to unlocked scons
			 *                    concatenated in the input OPADR
			 *            023   - apply only to unlocked scons
			 *                    concatenated in the output OPADR */

			swk = im81X_.im81 - prm2;
			scnrow = sworkX_.swork[swk-One][4-One];
			scncol = prm3;
			/*                   flag for checking if value is set or not */
			if( prm1 == 20 || prm1 == 21 ){
				chkscn = TRUE;
				}
			else{
				chkscn = FALSE;
				}
			/*                   flag: use input or output OPADR/SCONP arrays */
			if( prm1 == 20 || prm1 == 22 ){
				useinp = TRUE;
				}
			else{
				useinp = FALSE;
				}
			/*                   get value to propogate */
			if( scncol <= SCONX1 ){
				newval = sconX_.scon[scnrow-One][scncol-One];
				}
			else{
				newval = sconX_.scono[sconX_.scolnk[scnrow-One]-One][scncol-
				  SCONX1-One];
				}
			/*                   set range of concatenated elements in OPADR/SCONP arrays */
			if( useinp ){
				phrfrs = sworkX_.phrbeg[swk-One];
				flowckX_.phrlst = sworkX_.phrend[swk-One];
				}
			else{
				phrfrs = sworkoX_.phrbgo[swk-One];
				flowckX_.phrlst = sworkoX_.phrndo[swk-One];
				}
			/*                   loop thru each constituent in OPADR/SCONP & set */
			for( opxx=phrfrs; opxx <= flowckX_.phrlst; opxx++ ){
				/*                   get the  next address
				 *                   check all address here, may be a VC */
				hfptxx = 1;
				while( hfptxx != 0 ){
					if( useinp ){
						getadr(opadriX_.opadri,opadriX_.sconpi,hpdopiX_.hfdopi,
						  opxx,&hfptxx,&adxx,&scpxx,&ret);
						}
					else{
						getadr(opadroX_.opadro,opadroX_.sconpo,hpdopoX_.hfdopo,
						  optmp,&hfptxx,&adxx,&scpxx,&ret);
						}
					/*                  If valid address & original element, check it. */
					if( ret == 0 ){
						/*                     skip locked scons */
						if( sconX_.scon[scpxx-One][1-One] >= 0 ){
							/*                     set the appropriate SCON (check setting if requested) */
							if( scncol <= SCONX1 ){
								if( chkscn ){
									if( sconX_.scon[scpxx-One][scncol-One] == 
									  0 )
										sconX_.scon[scpxx-One][scncol-One] = newval;
									}
								else{
									sconX_.scon[scpxx-One][scncol-One] = newval;
									}
								}
							else if( sconX_.scolnk[scpxx-One] > 0 ){
								if( chkscn ){
									if( sconX_.scono[sconX_.scolnk[scpxx-One]-
									  One][scncol-SCONX1-One] == 0 )
										sconX_.scono[sconX_.scolnk[scpxx-One]-
										  One][scncol-SCONX1-One] = newval;
									}
								else{
									sconX_.scono[sconX_.scolnk[scpxx-One]-
									  One][scncol-SCONX1-One] = newval;
									}
								}
							}
						}
					}
				}
			}
		}

	return;
} /*end of function*/

