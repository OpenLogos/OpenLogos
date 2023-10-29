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

EXTERN struct t_sw13bkX_ {
	short int loc25, preloc;
	}	sw13bkX_;



void /*FUNCTION*/ t1sw13()
{
	/*
	 * LAST CHG 08/17/86 *R1561DSD: 100 SCONS
	 *      CHG 03/27/86 R1507DSD - NO UNLOCKING CONSTANT SCON BY ACCIDENT
	 *      CHG 08/29/85
	 *
	 */

	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *
	 *SW13 */
	/*   ***** BEGINNING OF -13 SWITCH ****** */
	/*          FUNCTION : LOCKS WC SCON OF SWORK POINTED TO
	 *          BY PARAMETER BY SETTING SCON WC TO NEGATIVE OF ITSELF
	 *          ONCE A SCON IS LOCKED IT MAY NEVER BE CHANGED */
	/*   -96 FUNCTION  - LOCK THE NEXT SCON TO BE CREATED */
	if( vbdataX_.k3p1 == -96 ){
		sw13bkX_.preloc = 1;

		/*   -95 FUNCTION - SET FLAG FOR NEXT -25 SWITCH TO LOCK ITS SCON */
		}
	else if( vbdataX_.k3p1 == -95 ){
		sw13bkX_.loc25 = 1;
		}
	else if( vbdataX_.k3p1 < 0 ){

		if( sconX_.scon[elscnpX_.elscnp[im81X_.im81-vbdataX_.k3p1-One]-
		  One][1-One] >= 0 )
			sconX_.scon[elscnpX_.elscnp[im81X_.im81-vbdataX_.k3p1-One]-
			  One][1-One] = -sconX_.scon[elscnpX_.elscnp[im81X_.im81-vbdataX_.k3p1-One]-
			  One][1-One];
		}
	else if( sconX_.scon[prtscoX_.sct-One][1-One] >= 0 ){
		sconX_.scon[prtscoX_.sct-One][1-One] = -sconX_.scon[prtscoX_.sct-One][1-One];
		}

	vbdataX_.k3n = vbdataX_.k3 + 2;
	return;
} /*end of function*/

