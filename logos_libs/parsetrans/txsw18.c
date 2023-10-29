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
	/*  CHANGES:
	 *      02/13/91 *JAL*  APPLY HEAD CASE TO SUBORDINATE ELEMENTS ONLY
	 *               IF GERMAN TARGET.
	/*      FUNCTION: SUSPENDS USUAL CONCATENATION
	 *      OF ITEMS TO RIGHT OF SWITCH; ONE PARAMETER, NN1
	 *      POINTS TO HEAD. */
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

	/*COMMON translations*/
EXTERN struct t_sw18bkX_ {
	short int phrnew;
	}	sw18bkX_;

void /*FUNCTION*/ txsw18()
{
	static short iz = 0;
	static short curhed = 0;
	static short sconiz = 0;

	sw18bkX_.phrnew = 1;

	/*      PHRNEN USED TO PREVENT CONCATENATION OF
	 *      ELEMENTS ON RIGHT; SETS UP NEW PHRASES INDIVIDUALLY.
	 *      CALCULATE PARAMETER/HEAD, I.E. N6JIM; K3+1 POINTS TO HEAD. */

	flowckX_.n6jim = flowckX_.im1 - vbdataX_.k3p1 - 80;
	if( phsupX_.phcts != sworkoX_.phcto ){

		/*          SET SWORK WC/T/F/LI. */
		sworkoX_.sworko[sworkoX_.phcto-One][1-One] = sworkX_.swork[flowckX_.n6jim-One][1-One];
		sworkoX_.sworko[sworkoX_.phcto-One][2-One] = sworkX_.swork[flowckX_.n6jim-One][2-One];
		sworkoX_.sworko[sworkoX_.phcto-One][3-One] = sworkX_.swork[flowckX_.n6jim-One][3-One];
		}

	sworkoX_.phrhdo[sworkoX_.phcto-One] = sworkX_.phrhed[im81X_.im81-vbdataX_.k3p1-One];

	if( tranidX_.tranid ==  2 )
	{
		sworkoX_.sworko[sworkoX_.phcto-One][4-One] = sworkX_.swork[flowckX_.n6jim-One][4-One];

		/*      LEVEL OF SWORK IS DETERMINED BY NO. OF ELEMENTS
		 *      IN SWORK. IF PARAMETER IS -3, E.G. IT IS THIRD
		 *      SWORK; HEAD IS ALWAYS ON RIGHT; THEREFORE THREE ELEMENTS.
		 *      MAKE NOUN PHRBEG ELEMENTS AGREE IN NUMBER,
		 *      PERSON, CASE, AND GENDER */

		if( sw25bkX_.sw25n == 1 ){
			sw25bkX_.sw25n = 0;
			for( iz=flowckX_.strphr; iz <= opadroX_.opo; iz++ ){
				sconiz = opadroX_.sconpo[iz-One];
				if( sconiz > 0 ){
					if( sconX_.scon[sconiz-One][1-One] > 0 ){
						if( sconX_.scon[sconiz-One][9-One] != 1 ){
							if( trgflgX_.trgflg == 1 )
								sconX_.scon[sconiz-One][7-One] = sw25bkX_.hedcas;
							}
						}
					}
				}
			}

		/*      SAME FOR VERB PHRBEG */

		if( sw21bkX_.sw21n == 1 ){
			sw21bkX_.sw21n = 0;
			curhed = sworkoX_.phrhdo[sworkoX_.phcto-One];
			for( iz=flowckX_.strphr; iz <= opadroX_.opo; iz++ ){
				sconiz = opadroX_.sconpo[iz-One];
				if( sconiz > 0 ){
					if( sconX_.scon[sconiz-One][1-One] > 0 ){
						if( sconX_.scon[sconiz-One][9-One] != 1 ){
							sconX_.scon[sconiz-One][4-One] = sconX_.scon[curhed-One][4-One];
							sconX_.scon[sconiz-One][5-One] = sconX_.scon[curhed-One][5-One];
							sconX_.scon[sconiz-One][6-One] = sconX_.scon[curhed-One][6-One];
							}
						}
					}
				}
			}

		flowckX_.strphr = opadroX_.opo + 1;
	}
	if( diagsX_.deepdi == 1 )
	{
		fprintf( _spec_fp, " SW18  %6d%6d%6d%6d%6d%6d%6d%6d%6d", flowckX_.n6jim, 
		  sworkoX_.phcto, vbdataX_.k3p1, opadroX_.opo, flowckX_.strphr, 
		  sw21bkX_.sw21n, phsupX_.phcts, sw25bkX_.sw25n, flowckX_.im1 );
		for( vwarg2X_.i=1; vwarg2X_.i <= 4; vwarg2X_.i++ ){
			fprintf( _spec_fp, "%6d", sworkoX_.sworko[sworkoX_.phcto-One][vwarg2X_.i-One] );
			}
		fprintf( _spec_fp, "%6d\n", sworkoX_.phrhdo[sworkoX_.phcto-One] );
		scnprt();
	}


	vbdataX_.k3n = vbdataX_.k3 + 2;
	return;
} /*end of function*/

