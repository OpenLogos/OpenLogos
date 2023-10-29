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
	/*      PHSUP WHEN 1 DOES NOT BUMP UP PHCTO
	 *      RESULTING IN NO NEW NSWORK.
	 *      K3P1 = 0, SUPRRESS PHRASE
	 *      K3P1 = 1, SUPPRESS PHRASE
	 *      K3P1 = 2, DO NOT SUPPRESS PHRASE */
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

void /*FUNCTION*/ txsw20()
{
	if( vbdataX_.k3p1 == 2 && tranidX_.tranid != 1){

		/*   A SW20 WITH A 2 IN FIRST PARAM CAUSES PHRASE SUPPRESSED
		 *   BY A PREVIOUS SW20 (WITH K3P1=1) TO BE ALLOWED */

		if( sworkoX_.phcto == sw26bkX_.phr26 )
			set26();

		if( sworkoX_.phcto > 0 ){
			sworkoX_.phrndo[sworkoX_.phcto-One] = opadroX_.opo;
			/*+                 link SWORKO to consituents in SWORKI 3/08/94 jal */
			if( vtrs42X_.sw42n == 1 ){
				swklnkX_.swklnk[sworkoX_.phcto-One] = sw42bkX_.im1sav;
				}
			else{
				swklnkX_.swklnk[sworkoX_.phcto-One] = flowckX_.im1;
				}
			}
		sworkoX_.phcto += 1;
		sworkoX_.phrbgo[sworkoX_.phcto-One] = opadroX_.opo + 1;
		sworkoX_.phrhdo[sworkoX_.phcto-One] = sworkX_.phrhed[flowckX_.n6-One];
		sw25bkX_.tw25 = 0;
		}
	else{
		phsupX_.phsup = 1;
		supresX_.phsup = 1;

		/*   IF SW26 ASSUMED PHSUP WAS OFF, CORRECT PHR26 */
		if( sw26bkX_.phr26 == (sworkoX_.phcto + 1) )
			sw26bkX_.phr26 -= 1;
		}

	vbdataX_.k3n = vbdataX_.k3 + 2;
	return;
} /*end of function*/

