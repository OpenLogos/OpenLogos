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

EXTERN struct t_sw48bhX_ {
	short int bhpt48, bhct48;
	}	sw48bhX_;

void /*FUNCTION*/ t1sw48()
{
	static short int jl6, jl7, n7jim, xx;

	static char pgmnam[9] = "T1SW48  ";
	static short gb = 0;
	static short gbx = 0;
	static short k3p2 = 0;
	static short phrbeg = 0;
	static short phrlst = 0;

	k3p2 = vtrfX_.vtrf[vbdataX_.k3+2-One];
	semargX_.k3p3 = vtrfX_.vtrf[vbdataX_.k3+3-One];
	vbdataX_.k3n = vbdataX_.k3 + 4;
	flowckX_.n6jim = im81X_.im81 - k3p2;

	if( vbdataX_.k3p1 == 43 ){

		for( gb=sworkoX_.phrbgo[flowckX_.n6jim-One]; gb <= sworkoX_.phrndo[flowckX_.n6jim-One]; gb++ ){
			gbx = opadroX_.sconpo[gb-One];
			if( sconX_.scon[gbx-One][0] >= 0 )
				sconX_.scon[gbx-One][3-One] = semargX_.k3p3;
			}
		}
	else if( vbdataX_.k3p1 == 45 ){

		/*                   -48045  COPY HENNUM & HASH CODE OF NN2 TO NN3
		 *                   CHANGES ARE MADE FOR HEAD OF PHRASE  ONLY. */
		n7jim = im81X_.im81 - semargX_.k3p3;
		jl7 = sconX_.scolnk[swork1X_.swork1[n7jim-One][4-One]-One];
		jl6 = sconX_.scolnk[swork1X_.swork1[flowckX_.n6jim-One][4-One]-One];
		hensavX_.henum2[jl7-One][0] = hensavX_.henum2[jl6-One][0];
		hensavX_.henum2[jl7-One][1] = hensavX_.henum2[jl6-One][1];
		hensavX_.root_henum2[jl7-One][0] = hensavX_.root_henum2[jl6-One][0];
		hensavX_.root_henum2[jl7-One][1] = hensavX_.root_henum2[jl6-One][1];
		hashX_.hashcd[jl7-One][0] = hashX_.hashcd[jl6-One][0];
		hashX_.hashcd[jl7-One][1] = hashX_.hashcd[jl6-One][1];
		hashX_.root_hashcd[jl7-One][0] = hashX_.root_hashcd[jl6-One][0];
		hashX_.root_hashcd[jl7-One][1] = hashX_.root_hashcd[jl6-One][1];
		}

	else if( vbdataX_.k3p1 == 46 ){
		/*                   -48046  COPY target address and SCON60 of NN2 to
		 *                           same of NN3. */
		n7jim = im81X_.im81 - semargX_.k3p3;
		/*               copy the target address and scon60 */
		swork1X_.swork1[n7jim-One][dctX_.dct[prctX_.js[sconX_.scolnk[n7jim-One]-
		  One]-One]-One] = swork1X_.swork1[(flowckX_.n6jim)-One][dctX_.dct[prctX_.js[sconX_.scolnk[flowckX_.n6jim-One]-
		  One]-One]-One];
		sconX_.scono[sconX_.scolnk[n7jim-One]-One][60-SCONX1-One] = sconX_.scono[sconX_.scolnk[flowckX_.n6jim-One]-
		  One][60-SCONX1-One];
		sconX_.scono[sconX_.scolnk[n7jim-One]-One][59-SCONX1-One] = sconX_.scono[sconX_.scolnk[flowckX_.n6jim-One]-
		  One][59-SCONX1-One];
		}
	else if( vbdataX_.k3p1 == 868 ){

		/*             -48 868  indicate which Black Hole will receive
		 *               subsequent OPADR strings bracketed by -868 on the
		 *               left and -869 on the right. */

		/*                  zero the BH variables if K3P2=0 */
		if( k3p2 == 0 ){
			sw48bhX_.bhpt48 = 0;
			sw48bhX_.bhct48 = 0;
			/*                  K3P2=800 or 900 means directional search for
			 *                      the K3P3'th Black Hole to lft or rt, respectively */
			}
		else if( k3p2 == 900 ){
			sw48bhX_.bhpt48 = BFMVRT;
			sw48bhX_.bhct48 = semargX_.k3p3;
			}
		else if( k3p2 == 800 ){
			sw48bhX_.bhpt48 = BFMVLF;
			sw48bhX_.bhct48 = semargX_.k3p3;
			/*                  K3P2 can be a ptr to a Cell */
			}
		else if( k3p2 > 0 && k3p2 <= 100 ){
			xx = vbdataX_.vbcell[k3p2-One];
			if( xx > 0 && xx <= SCONY ){
				/*                  otherwise K3P2 should be a relative pointer */
				sw48bhX_.bhpt48 = xx;
				sw48bhX_.bhct48 = semargX_.k3p3;
				}
			else if( diagsX_.deepdi == 1 || diagsX_.longdi == 1 ){
				fprintf( _spec_fp, "\nERROR in switch -48 fcn 868 :\n  Cell contains invalid SCON pointer (CELL(%3d)=%3d\n\nERROR in switch -48 fcn 868 :\n  Cell contains invalid SCON pointer (CELL(%3d)=\n", 
				  k3p2, semargX_.k3p3, xx );
				}
			}
		else if( k3p2 < -70 && k3p2 >= -90 ){
			sw48bhX_.bhpt48 = im81X_.im81 - k3p2;
			sw48bhX_.bhct48 = semargX_.k3p3;

			}
		else{
			fprintf( _spec_fp, "\nERROR in switch -48 fcn 868 :\n  invalid parameters: %3d %3d\n", 
			  k3p2, semargX_.k3p3 );
			}

		/*             1    2    3    4    5    6    7    8    9   10 */
		}
	else if( !((((((vbdataX_.k3p1 >= 1 && vbdataX_.k3p1 <= 3) || (vbdataX_.k3p1 >= 
	  5 && vbdataX_.k3p1 <= 7)) || vbdataX_.k3p1 == 10) || vbdataX_.k3p1 == 
	  11) || (vbdataX_.k3p1 >= 14 && vbdataX_.k3p1 <= 20)) || vbdataX_.k3p1 == 
	  22) ){
		if( vbdataX_.k3p1 == 4 ){


			if( swork1X_.swork1[flowckX_.n6jim+1-One][dct2X_.dct2[prctX_.js[sconX_.scolnk[flowckX_.n6jim+1-One]-
			  One]-One]-One] == 19 ){

				if( sconX_.scon[flowckX_.n6jim+1-One][13-One] != 1 && 
					sconX_.scon[flowckX_.n6jim+1-One][11-One] != 1 )
					goto L_9000;
				}
			else if( swork1X_.swork1[flowckX_.n6jim+1-One][dct2X_.dct2[prctX_.js[sconX_.scolnk[flowckX_.n6jim+1-One]-
			  One]-One]-One] != 20 ){
				goto L_9000;
				}

			swork1X_.swork1[flowckX_.n6jim+1-One][dct3X_.dct3[prctX_.js[sconX_.scolnk[flowckX_.n6jim+1-One]-One]-One]-One] = semargX_.k3p3;
			}
		else if( vbdataX_.k3p1 == 8 ){

			formsaX_.formsv[sconX_.scolnk[flowckX_.n6jim-One]-One] = semargX_.k3p3;
			}
		else if( vbdataX_.k3p1 == 9 ){

			sconX_.scon[elscnpX_.elscnp[flowckX_.n6jim-One]-One][9-One] = semargX_.k3p3;
			}
		else if( vbdataX_.k3p1 == 12 ){

			flowckX_.n6jim = im81X_.im81 - semargX_.k3p3;
			sconX_.scon[flowckX_.n6jim-One][12-One] = k3p2;

			/*   SO NO OVERLAY BY NOUN GENDER IN SCON LOAD */
			xpatnfX_.xpatfm[sconX_.scolnk[flowckX_.n6jim-One]-One][prctX_.js[sconX_.scolnk[flowckX_.n6jim-One]-One]-One] = 0;
			}
		else if( vbdataX_.k3p1 == 13 ){

			flowckX_.n6jim = im81X_.im81 - semargX_.k3p3;
			ofl2X_.ofl2i[sconX_.scolnk[flowckX_.n6jim-One]-One] = k3p2;
			sconX_.scon[flowckX_.n6jim-One][3-One] = k3p2;
			}
		else if( vbdataX_.k3p1 == 21 || vbdataX_.k3p1 == 23 ){

			/*        -48 02X -8X SCON   apply SCON value to all elements
			 *                           concatenated under -8X head element
			 *            021   - apply only to unset (scon=0) and unlocked scons
			 *                    concatenated in the output OPADR
			 *            023   - apply only to unlocked scons
			 *                    concatenated in the output OPADR */

			txsw48(vbdataX_.k3p1,k3p2,semargX_.k3p3);

			}
		}


L_9000:
		if(diagsX_.longdi!=0)
		{
		fprintf( _spec_fp, " END SW48 %5d%5d%5d\n", vbdataX_.k3p1, 
		  k3p2, semargX_.k3p3 );
		}
	return;
} /*end of function*/

