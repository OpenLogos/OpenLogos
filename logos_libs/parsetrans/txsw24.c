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
	 /*     FUNCTION: BREAKS VTR RULE INTO MORE THAN 1 SWORK
	 *     BY CREATING SWORK FOR VTR TO LEFT OF SWITCH.
	 *     PARAMETER IS NUMBER OF ELEMENTS. */

	/*  CHANGES:
	 *      6/10/93 jal: if pass1 of 2 skip target scon setting.
	 *      02/13/91 *JAL*  APPLY HEAD CASE TO SUBORDINATE ELEMENTS ONLY
	 *               IF GERMAN TARGET.
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


void /*FUNCTION*/ txsw24()
{
	static short iz = 0;
	static short s4 = 0;
	static short xx = 0;
	static short yy = 0;
	static short n6x = 0;
	static short curhed = 0;
	static short sconiz = 0;

	n6x = flowckX_.im1 + sploopX_.li;

	/*     LI NOT A GOOD DISPLACEMENT FOR WC 10 RULE */
	if( vtrs42X_.sw42n == 1 )
		n6x = sw42bkX_.im1sav + sploopX_.li;

	if( sworkoX_.phcto > 0 )
		sworkoX_.phrndo[sworkoX_.phcto-One] = opadroX_.opo;
	sworkoX_.sworko[sworkoX_.phcto+1-One][1-One] = sworkX_.swork[n6x-One][1-One];
	sworkoX_.sworko[sworkoX_.phcto+1-One][2-One] = sworkX_.swork[n6x-One][2-One];
	sworkoX_.sworko[sworkoX_.phcto+1-One][3-One] = sworkX_.swork[n6x-One][3-One];
	sworkoX_.sworko[sworkoX_.phcto+1-One][4-One] = sworkX_.swork[n6x-One][4-One];
	sworkoX_.phrbgo[sworkoX_.phcto+1-One] = opadroX_.opo + 1;
	sworkoX_.phrhdo[sworkoX_.phcto+1-One] = sworkX_.phrhed[flowckX_.n6-One];

	/*     WHICH NSWRK ARE WE AFTER?  N6JIM POINTS TO IT */
	flowckX_.n6jim = flowckX_.im1 - vbdataX_.k3p1 - 80;

	/*+-   IF (PHCTS .EQ. PHCTO) GOTO 3600        *03/19/90*JAL */
	if( phsupX_.phcts != sworkoX_.phcto ){
		sworkoX_.sworko[sworkoX_.phcto-One][1-One] = sworkX_.swork[flowckX_.n6jim-One][1-One];
		sworkoX_.sworko[sworkoX_.phcto-One][2-One] = sworkX_.swork[flowckX_.n6jim-One][2-One];
		sworkoX_.sworko[sworkoX_.phcto-One][3-One] = sworkX_.swork[flowckX_.n6jim-One][3-One];
		sworkoX_.sworko[sworkoX_.phcto-One][4-One] = sworkX_.swork[flowckX_.n6jim-One][4-One];
		sworkoX_.phrhdo[sworkoX_.phcto-One] = sworkX_.phrhed[im81X_.im81-vbdataX_.k3p1-One];
		}


	if( sworkoX_.phcto == sw26bkX_.phr26 )
		set26();

	/*            if pass1 of 2 skip target scon setting */
	if( passesX_.passfl == 1 && passesX_.passct == 1 ){
		sw25bkX_.sw25n = 0;
		sw21bkX_.sw21n = 0;
		}
	else{
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

		/*            SAME FOR VERB PHRBEG */

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
		}

	flowckX_.strphr = opadroX_.opo + 1;
	/*+1                link SWORKO to consituents in SWORKI 3/08/94 jal */
	swklnkX_.swklnk[sworkoX_.phcto-One] = flowckX_.n6jim;
	sworkoX_.phcto += 1;
	sw25bkX_.tw25 = 0;

	if( diagsX_.deepdi == 1 )
		{
		fprintf( _spec_fp, " SW24 %6d%6d%6d%6d", flowckX_.n6jim, 
		  n6x, flowckX_.strphr, opadroX_.opo );
		for( xx=1; xx <= 4; xx++ ){
			fprintf( _spec_fp, "%6d", sworkoX_.sworko[sworkoX_.phcto-One][xx-One] );
			}
		for( xx=1; xx <= 4; xx++ ){
			fprintf( _spec_fp, "%6d", sworkoX_.sworko[sworkoX_.phcto-1-One][xx-One] );
			}
		fprintf( _spec_fp, "%6d%6d%6d%6d%6d\n", sworkoX_.phcto, phsupX_.phcts, 
		  sworkoX_.phrhdo[sworkoX_.phcto-1-One], sworkoX_.phrhdo[sworkoX_.phcto-One], 
		  head26X_.headwc );
		}


	vbdataX_.k3n = vbdataX_.k3 + 2;
	return;
} /*end of function*/

