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




void /*FUNCTION*/ t1sw33()
{
	/*
	 *     CHG 08/17/86 *R1561DSD: 100 SCONS
	 *     CHG 08/29/85
	 *
	 */
		/*pointer COMMON transls*/
	struct  {
		short int im1, i3save, i3str, n6, n6jim, n6jims, phrstr;
		}	*_flowckX_ = (void*)&flowckX_;
		/*end of pointer COMMON transls*/

	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *
	 *SW33 */
	/*   ***** BEGINNING OF -33 SWITCH ***** */
	/*          FUNCTION: SETS ALTWC FLAG IN ORDER TO ALLOW ADVANCE SETTING
	 *          OF SCON(8). */
	/*+    RESTRUCTURED ALL                           11/04/86        DSD */
	if( vbdataX_.k3p1 >= 0 ){
		sw33bkX_.altwc = vbdataX_.k3p1;
		}
	else{
		_flowckX_->n6jim = im81X_.im81 - vbdataX_.k3p1;

		if( sconX_.scon[_flowckX_->n6jim-One][1-One] >= 0 )
			sconX_.scon[_flowckX_->n6jim-One][8-One] = 2;
		}
	/*   *****  END OF -33 SWITCH *****
	 *SW33 */
	vbdataX_.k3n = vbdataX_.k3 + 2;
	/*-    RESTRUCTURED ALL                           11/04/86        DSD */
	return;
} /*end of function*/

