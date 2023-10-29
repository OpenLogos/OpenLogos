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
EXTERN struct t_head26X_ {
	short int headwc, headty, headfr, headhd, headt2, headt3;
	}	head26X_;

EXTERN struct t_sw26nX_ {
	short int sw26n;
	}	sw26nX_;
	/* end of COMMON translations */
void /*FUNCTION*/ t1sw26()
{
	static byte vtrfmn;
	struct  {
		short int nwrks, index, k3p3, pntr9;
		}	*_semargX_ = (void*)&semargX_;
		/*end of pointer COMMON transls*/
	static short xx = 0;
	static short k3p2 = 0;
	static short k3p4 = 0;

	vbdataX_.k3n = vbdataX_.k3 + 5;
	k3p2 = vtrfX_.vtrf[vbdataX_.k3+2-One];
	_semargX_->k3p3 = vtrfX_.vtrf[vbdataX_.k3+3-One];
	k3p4 = vtrfX_.vtrf[vbdataX_.k3+4-One];
	/*+ Rel Ptr Confusion                           RKH  04/01/87   R1713
	 *     Get mode of the VTR element */
	vtrfmn = vtrfmX_.vtrfm[vbdataX_.k3+1-One];
	/*-                                             RKH  04/01/87   R1713 */

	/* -26 SWITCH SETS THE WC, TYPE, FORM AND SOMETIMES THE HEAD ELEMENT
	 *     OF THE NEXT SWORKO CREATED. */

	/*     K3P2 = WC VALUE,  K3P3 = TYPE VALUE,  K3P4 = FORM VALUE */

	/*     K3P3 WILL SET THE CORRECT TYPE FIELD (SET, SUPERSET OR SUBSET)
	 *     THESE PARAMETERS CAN BE RELATIVE POINTERS, IN WHICH CASE THE
	 *     SWORKO FIELDS WILL BE SET TO THE VALUES OF THE ELEMENT POINTED
	 *     TO.  IN THE CASE OF THE TYPE FIELD, A RELATIVE POINTER WILL
	 *     CAUSE ALL THREE TYPE FIELDS TO BE SET. */

	/*     K3P1 SETS THE HEAD FOR THE NEXT SWORKO CREATED.
	 *     IT ALSO INDICATES WHETHER THIS -26 SWITCH SHOULD OVERRIDE
	 *     A PREVIOUS ONE IN THIS CHAIN.
	 *     K3P1  =  000  MEANS SET CODES, DO NOT OVERRIDE
	 *           =  -8X  MEANS SET HEAD AND CODES, DO NOT OVERRIDE
	 *           =  070  MEANS SET CODES, OVERRIDE
	 *           =  08X  MEANS SET HEAD AND CODES, OVERRIDE */

	/*     THIS SWITCH WILL OPERATE BY SETTING FLAGS WHICH WILL BE USED
	 *     AND ZEROED WHENEVER PHCTO IS ABOUT BE INCREMENTED.
	 *     (BEGINNING OF VTR, -18 SWITCH, -24 SWITCH) */

	/*     VARIABLES:
	 *     HEADWC, HEADTY, HEADFR, HEADT2, HEADT3, HEADHD
	 *       WC     TYPE    FORM    SET   SUPERSET  HEAD  OF THE NEXT SWORKO */

	/*     SW26N IS A FLAG THAT INDICATES THAT VALUES HAVE BEEN SET.
	 *     SW26N  =  1 MEANS A -26 000 HAS SET THE VALUES
	 *     SW26N  =  2 MEANS A -26 070, -8X OR 08X HAS SET THE VALUES
	 *     PHR26 WILL INDICATE WHICH SWORKO IS BEING SET */

	if( supresX_.phsup == 1 ){
		sw26bkX_.phr26 = sworkoX_.phcto;
		}
	else{

		/*     THIS -26 IS SETTING VALUES FOR THE NEXT SWORKO.
		 *     IF THERE ARE SW26 VALUES SET FOR THE CURRENT SWORKO,
		 *     MAKE THE FINAL SETTING AND ZERO THE OLD VALUES. */

		if( sworkoX_.phcto == sw26bkX_.phr26 )
			t1se26();
		sw26bkX_.phr26 = sworkoX_.phcto + 1;
		head26X_.headwc = 0;
		head26X_.headty = 0;
		head26X_.headfr = 0;
		head26X_.headhd = 0;
		head26X_.headt2 = 0;
		head26X_.headt3 = 0;
		sw26nX_.sw26n = 0;
		}

	/*   IS THIS AN OVERRIDE?
	 *+ Rel Ptr Confusion                           RKH  04/01/87   R1713 */
	if( vtrfmn != vtrfvlX_.vtmpp ){
		/*+ Rel Pointer Confusion                       RKH  04/10/87   R1713 */
		if( vbdataX_.k3p1 != 70 ){
			/*-                                             RKH  04/10/87   R1713
			 *-                                             RKH  04/01/87   R1713 */

			/*   -26 -8X CAN OVERRIDE A PREVIOUS -26 000
			 *+ Rel Ptr Confusion                           RKH  04/01/87   R1713 */
			if( vtrfmn == vtrfvlX_.vtmrp ){
				/*-                                             RKH  04/01/87   R1713 */
				if( sw26nX_.sw26n == 2 )
					goto L_4410;

				/*   -26 000 CAN ONLY ACT IF IT IS THE FIRST -26 SWITCH IN THE CHAIN */
				}
			else if( vbdataX_.k3p1 != 0 ){
				goto L_4410;
				}
			else if( sw26nX_.sw26n != 0 ){
				goto L_4410;
				}
			}
		}

	/*   SET WC, TYPE AND FORM */

	if( k3p2 != 0 ){
		if( k3p2 > 0 ){

			head26X_.headwc = k3p2;
			}
		else{

			head26X_.headwc = swork1X_.swork1[im81X_.im81-k3p2-One][dct2X_.dct2[prctX_.js[sconX_.scolnk[im81X_.im81-k3p2-One]-
			  One]-One]-One];
			}
		}

	if( _semargX_->k3p3 != 0 ){
		if( _semargX_->k3p3 > 0 ){

			if( _semargX_->k3p3 >= 100 )
				head26X_.headty = _semargX_->k3p3;
			if( _semargX_->k3p3 >= 17 && _semargX_->k3p3 <= 99 )
				head26X_.headt2 = _semargX_->k3p3;
			if( _semargX_->k3p3 <= 16 )
				head26X_.headt3 = _semargX_->k3p3;
			}
		else{

			xx = im81X_.im81 - _semargX_->k3p3;
			head26X_.headty = swork1X_.swork1[xx-One][dct4X_.dct4[prctX_.js[sconX_.scolnk[xx-One]-
			  One]-One]-One];
			/*+           ONLY ALLOW DIRECT SETTING OF SCON11 & 13  *10/09/91*JAL*
			 *     HEADT2 = SCON(11,XX)
			 *     HEADT3 = SCON(13,XX)
			 *-                                                     *10/09/91*JAL* */
			}
		}

	if( k3p4 != 0 ){
		if( k3p4 > 0 ){

			head26X_.headfr = k3p4;
			}
		else{

			head26X_.headfr = swork1X_.swork1[im81X_.im81-k3p4-One][dct3X_.dct3[prctX_.js[sconX_.scolnk[im81X_.im81-k3p4-One]-
			  One]-One]-One];
			}
		}

	sw26nX_.sw26n = 2;
	if( vbdataX_.k3p1 == 0 )
		sw26nX_.sw26n = 1;
	if( !(vbdataX_.k3p1 == 0 || vbdataX_.k3p1 == 70) ){

		/*   SET THE HEAD */
		head26X_.headhd = im81X_.im81 - vbdataX_.k3p1;
		if( vbdataX_.k3p1 > 0 )
			head26X_.headhd = im81X_.im81 + vbdataX_.k3p1;
		}

L_4410:
	if( opswX_.sw[10-One] == 1 )
		{
		fprintf( _spec_fp, " SW26 %4d%4d%4d%4d%4d%4d%4d%4d\n", head26X_.headwc, 
		  head26X_.headty, head26X_.headfr, head26X_.headhd, head26X_.headt2, 
		  head26X_.headt3, sw26nX_.sw26n, sw26bkX_.phr26 );
		}

	return;
} /*end of function*/

