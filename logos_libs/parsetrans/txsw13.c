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
	/*     FUNCTION: LOCKS SCON (1) OF HEAD
	 *     OF PHRBEG POINTED TO BY PARAMETER */
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

void /*FUNCTION*/ txsw13()
{
	static short iz = 0;
	if( vbdataX_.k3p1 < 0 ){

		flowckX_.phrstr = sworkX_.phrbeg[im81X_.im81-vbdataX_.k3p1-One];
		flowckX_.phrlst = sworkX_.phrend[im81X_.im81-vbdataX_.k3p1-One];
		for( iz=flowckX_.phrstr; iz <= flowckX_.phrlst; iz++ ){
			if( sconX_.scon[opadriX_.sconpi[iz-One]-One][1-One] >= 0 )
				sconX_.scon[opadriX_.sconpi[iz-One]-One][1-One] = -sconX_.scon[opadriX_.sconpi[iz-One]-One][1-One];
			}
		}
	else if( sconX_.scon[prtscoX_.sct-One][1-One] >= 0 ){
		sconX_.scon[prtscoX_.sct-One][1-One] = -sconX_.scon[prtscoX_.sct-One][1-One];
		}

	vbdataX_.k3n = vbdataX_.k3 + 2;
	return;
} /*end of function*/

