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
	/*   -26 SWITCH SETS THE WC, TYPE, FORM AND SOMETIMES THE HEAD ELEMENT
	 *    OF THE NEXT SWORK CREATED.
	 *     K3P2 = WC VALUE,  K3P3 = TYPE VALUE,  K3P4 = FORM VALUE
	 *      K3P3 WILL SET THE CORRECT TYPE FIELD (SET, SUPERSET OR SUBSET)
	 *      THESE PARAMETERS CAN BE RELATIVE POINTERS, IN WHICH CASE THE
	 *      SWORK FIELDS WILL BE SET TO THE VALUES OF THE ELEMENT POINTED
	 *      TO.  IN THE CASE OF THE TYPE FIELD, A RELATIVE POINTER WILL
	 *      CAUSE ALL THREE TYPE FIELDS TO BE SET.
	 *     K3P1 SETS THE HEAD FOR THE NEXT SWORK CREATED.
	 *      IT ALSO INDICATES WHETHER THIS -26 SWITCH SHOULD OVERRIDE
	 *      A PREVIOUS ONE IN THIS CHAIN.
	 *      K3P1  =  000  MEANS SET CODES, DO NOT OVERRIDE
	 *            =  -8X  MEANS SET HEAD AND CODES, DO NOT OVERRIDE
	 *            =  070  MEANS SET CODES, OVERRIDE
	 *            =  08X  MEANS SET HEAD AND CODES, OVERRIDE */

	/*    THIS SWITCH WILL OPERATE BY SETTING FLAGS WHICH WILL BE USED
	 *    AND ZEROED WHENEVER PHCTO IS ABOUT BE INCREMENTED
	 *    (BEGINNING OF VTR, -18 SWITCH, -24 SWITCH)
	 *     VARIABLES:
	 *     HEADWC, HEADTY, HEADFR, HEADT2, HEADT3, HEADHD
	 *       WC     TYPE    FORM    SET   SUPERSET  HEAD  OF THE NEXT SWORK
	 *     SW26N IS A FLAG THAT INDICATES THAT VALUES HAVE BEEN SET.
	 *     SW26N  =  1 MEANS A -26 000 HAS SET THE VALUES
	 *     SW26N  =  2 MEANS A -26 070, -8X OR 08X HAS SET THE VALUES
	 *     PHR26 WILL INDICATE WHICH SWORK IS BEING SET */
	/*  CHANGES:
	 *    10/09/91 JAL:  DONT CHANGE SCON(11) OR SCON(13) IF TYPE FIELD
	 *            K3P3 IS A RELATIVE POINTER.
	*/
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





void /*FUNCTION*/ txsw26()
{
	static byte vtrfmn;
	static short xx = 0;
	static short k3p2 = 0;
	static short k3p4 = 0;

	vbdataX_.k3n = vbdataX_.k3 + 5;

	k3p2 = vtrfX_.vtrf[vbdataX_.k3+2-One];
	semargX_.k3p3 = vtrfX_.vtrf[vbdataX_.k3+3-One];
	k3p4 = vtrfX_.vtrf[vbdataX_.k3+4-One];
	/*+ Rel Ptr Confusion                           RKH  04/01/87   R1713
	 *     Get mode of the VTR element */
	vtrfmn = vtrfmX_.vtrfm[vbdataX_.k3+1-One];


	if( phsupX_.phsup == 1 ){
		sw26bkX_.phr26 = sworkoX_.phcto;
		}
	else{

		/*   THIS -26 IS SETTING VALUES FOR THE NEXT SWORK.
		 *   IF THERE ARE SW26 VALUES SET FOR THE CURRENT SWORK,
		 *   MAKE THE FINAL SETTING AND ZERO THE OLD VALUES. */

		if( sworkoX_.phcto == sw26bkX_.phr26 )
			set26();

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
	 *+ Rel Ptr Confusion VTMPP means Positive Ptr. RKH  04/01/87   R1713 */
	if( vtrfmn != vtrfvlX_.vtmpp ){
		/*+ Rel Pointer Confusion                       RKH  04/10/87   R1713 */
		if( vbdataX_.k3p1 != 70 ){
			/*   -26 -8X CAN OVERRIDE A PREVIOUS -26 000
			 *+ Rel Ptr Confusion                           RKH  04/01/87   R1713 */
			if( vtrfmn == vtrfvlX_.vtmrp ){
				if( sw26nX_.sw26n == 2 )
					goto L_4390;

				/*   -26 000 CAN ONLY ACT IF IT IS THE FIRST -26 SWITCH IN THE CHAIN */
				}
			else if( vbdataX_.k3p1 != 0 ){
				goto L_4390;
				}
			else if( sw26nX_.sw26n != 0 ){
				goto L_4390;
				}
			}
		}

	/*   SET WC, TYPE AND FORM */
	if( k3p2 != 0 ){
		if( k3p2 > 0 ){
			head26X_.headwc = k3p2;
			}
		else{
			head26X_.headwc = sworkX_.swork[im81X_.im81-k3p2-One][1-One];
			}
		}

	if( semargX_.k3p3 != 0 ){
		if( semargX_.k3p3 > 0 ){
			if( semargX_.k3p3 >= 100 )
				head26X_.headty = semargX_.k3p3;
			if( semargX_.k3p3 >= 17 && semargX_.k3p3 <= 99 )
				head26X_.headt2 = semargX_.k3p3;
			if( semargX_.k3p3 <= 16 )
				head26X_.headt3 = semargX_.k3p3;
			}
		else{
			xx = im81X_.im81 - semargX_.k3p3;
			head26X_.headty = sworkX_.swork[xx-One][2-One];
			}
		}

	if( k3p4 != 0 ){
		if( k3p4 > 0 ){
			head26X_.headfr = k3p4;
			}
		else{
			head26X_.headfr = sworkX_.swork[im81X_.im81-k3p4-One][3-One];
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

L_4390:
	if( diagsX_.deepdi == 1 )
		{
		fprintf( _spec_fp, " SW26 %4d%4d%4d%4d%4d%4d%4d%4d\n", head26X_.headwc, 
		  head26X_.headty, head26X_.headfr, head26X_.headhd, head26X_.headt2, 
		  head26X_.headt3, sw26nX_.sw26n, sw26bkX_.phr26 );
		}

	return;
} /*end of function*/

