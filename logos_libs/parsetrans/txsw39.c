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
	/*      FUNCTION-ADJECTIVE SWITCH
	 *      LOADS ELEMENT TO LEFT OP SWITCH WITH PER,
	 *      NUM, GEN, CASE OF HEAD NOUN (-25). */
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


void /*FUNCTION*/ txsw39()
{
	static short iz = 0;
	static short sconlc = 0;

	sconlc = sworkX_.phrhed[vtrfX_.vtrf[vbdataX_.k3-2-One]-One];

	/*     K3P1 NOW POINTS TO ADJECTIVE(NON-NOUN) WHICH
	 *     IS TO ASSUME HEAD NOUN'S VALUES
	 *     FIRST CHECK FOR LOCKED SCON */

	if( sconX_.scon[sconlc-One][1-One] >= 0 ){

		if( vbdataX_.k3p1 >= 0 ){
			/*        NOW .... WE ARE CHECKING FOR LOCKED CONDITION OF HEAD ONLY */
			vtrnX_.vtrn = vtrfX_.vtrf[vbdataX_.k3-2-One];
			/*        SWITCH IS LEFT ORIENTED */
			flowckX_.phrstr = sworkX_.phrbeg[flowckX_.im1-vtrnX_.vtrn-One];
			flowckX_.phrlst = sworkX_.phrend[flowckX_.im1-vtrnX_.vtrn-One];
			for( iz=flowckX_.phrstr; iz <= flowckX_.phrlst; iz++ ){
				if( sconX_.scon[opadriX_.sconpi[iz-One]-One][1-One] >=  0 ){
					sconX_.scon[opadriX_.sconpi[iz-One]-One][4-One] = sw25bkX_.hedgen;
					sconX_.scon[opadriX_.sconpi[iz-One]-One][5-One] = sw25bkX_.hednum;
					sconX_.scon[opadriX_.sconpi[iz-One]-One][6-One] = sw25bkX_.hedper;
					}
				}
			}
		/*     NON-NOUN OF PHRBEG NOW HAS ITS SCON
		 *     VALUES SAME AS HEAD NOUNS
		 *        CHECK FOR REL POINTER FOR K3P1
		 *        ASSUME REL POINTER & CHANGE LOOP PARMETER */
		flowckX_.phrstr = sworkX_.phrbeg[im81X_.im81-vbdataX_.k3p1-One];
		flowckX_.phrlst = sworkX_.phrend[im81X_.im81-vbdataX_.k3p1-One];
		for( iz=flowckX_.phrstr; iz <= flowckX_.phrlst; iz++ ){
			sconX_.scon[opadriX_.sconpi[iz-One]-One][4-One] = sw25bkX_.hedgen;
			sconX_.scon[opadriX_.sconpi[iz-One]-One][5-One] = sw25bkX_.hednum;
			sconX_.scon[opadriX_.sconpi[iz-One]-One][6-One] = sw25bkX_.hedper;
			}
		}
	if( diagsX_.deepdi == 1 )
		{
		fprintf( _spec_fp, " SW39 %6d%6d%6d%6d%6d%6d%6d%6d\n", vtrnX_.vtrn, 
		  vbdataX_.k3p1, sw25bkX_.hedgen, sw25bkX_.hednum, sw25bkX_.hedper, 
		  sw25bkX_.hedcas, flowckX_.phrstr, flowckX_.phrlst );
		}

	vbdataX_.k3n = vbdataX_.k3 + 2;
	return;
} /*end of function*/

