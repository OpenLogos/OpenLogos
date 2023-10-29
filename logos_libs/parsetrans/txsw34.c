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
	/*      FUNCTION - K3P4 POINTS TO HEAD
	 *      IF 0, HEAD IS SWORK TO LEFT: PARM 1,2,3,= WC,T,F
	 *      STREAMLINE PARAMETERS; K3P1 DONE ALREADY ABOVE */
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


void /*FUNCTION*/ txsw34()
{
	static short iz = 0;
	static short k3p2 = 0;
	static short k3p4 = 0;

	vbdataX_.k3n = vbdataX_.k3 + 5;
	if( sw25bkX_.tw25 != 1 ){

		phsupX_.phcts = sworkoX_.phcto;
		k3p2 = vtrfX_.vtrf[vbdataX_.k3+2-One];
		semargX_.k3p3 = vtrfX_.vtrf[vbdataX_.k3+3-One];
		k3p4 = vtrfX_.vtrf[vbdataX_.k3+4-One];
		flowckX_.n6jim = im81X_.im81 - k3p4;

		if( vbdataX_.k3p1 != 0 ){
			if( vbdataX_.k3p1 > 0 ){

				sworkoX_.sworko[sworkoX_.phcto-One][1-One] = vbdataX_.k3p1;
				}
			else{

				sworkoX_.sworko[sworkoX_.phcto-One][1-One] = sworkX_.swork[im81X_.im81-vbdataX_.k3p1-One][1-One];
				}
			}

		if( k3p2 != 0 ){
			if( k3p2 <= 0 ){

				sworkoX_.sworko[sworkoX_.phcto-One][2-One] = sworkX_.swork[im81X_.im81-k3p2-One][2-One];

				}
			else if( k3p2 <= 16 ){

				sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][13-One] = k3p2;
				sworkoX_.sworko[sworkoX_.phcto-One][2-One] = sworkX_.swork[flowckX_.n6jim-One][2-One];

				/*     IN CASE TYPE STORED IN WRONG PLACE - CHECK ALL THREE */
				if( sworkoX_.sworko[sworkoX_.phcto-One][2-One] <=  16 )
					sworkoX_.sworko[sworkoX_.phcto-One][2-One] = k3p2;
				if( sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][11-One] <= 16 )
					sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][11-One] = k3p2;

				}
			else if( k3p2 > 99 ){

				sworkoX_.sworko[sworkoX_.phcto-One][2-One] = k3p2;
				if( sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][11-One] > 99 )
					sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][11-One] = k3p2;
				if( sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][13-One] > 99 )
					sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][13-One] = k3p2;
				}
			else{
				sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][11-One] = k3p2;
				sworkoX_.sworko[sworkoX_.phcto-One][2-One] = sworkX_.swork[flowckX_.n6jim-One][2-One];
				if( sworkoX_.sworko[sworkoX_.phcto-One][2-One] <= 99 )
					sworkoX_.sworko[sworkoX_.phcto-One][2-One] = k3p2;
				if( sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][13-One] > 16 )
					sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][13-One] = k3p2;
				}
			}

		if( semargX_.k3p3 != 0 ){
			if( semargX_.k3p3 > 0 ){

				sworkoX_.sworko[sworkoX_.phcto-One][3-One] = semargX_.k3p3;
				}
			else{

				sworkoX_.sworko[sworkoX_.phcto-One][3-One] = sworkX_.swork[im81X_.im81-semargX_.k3p3-One][3-One];
				}
			}

		sworkoX_.sworko[sworkoX_.phcto-One][4-One] = sworkX_.swork[flowckX_.n6jim-One][4-One];
		sworkoX_.phrhdo[sworkoX_.phcto-One] = sworkX_.phrhed[flowckX_.n6jim-One];
		}

	if( diagsX_.deepdi == 1 )
		{
		fprintf( _spec_fp, " SW34 %5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d", 
		  k3p2, semargX_.k3p3, k3p4, flowckX_.n6jim, vbdataX_.k3n, 
		  sw26nX_.sw26n, sworkoX_.phcto, phsupX_.phcts, im81X_.im81, 
		  flowckX_.n6jim, sworkoX_.phrbgo[sworkoX_.phcto-One] );
		for( iz=1; iz <= 4; iz++ ){
			fprintf( _spec_fp, "%5d", sworkoX_.sworko[sworkoX_.phcto-One][iz-One] );
			}
		fprintf( _spec_fp, "\n" );
		}

	return;
} /*end of function*/

