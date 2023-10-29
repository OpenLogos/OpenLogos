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
	 *       6/10/93 jal: if pass1 of 2 skip target scon setting.
	 *      02/13/91 *JAL*  APPLY HEAD CASE TO SUBORDINATE ELEMENTS ONLY
	 *               IF GERMAN TARGET.
	 *      87/06/08 LG002GBA RPHS3:  PHASE 3 OF 30.40.50 TBLS
	 *      04/17/87 *R1685RKH*  CHANGE T1-4 SWORK LIMIT FROM 50 TO
	 *      CHG 08/17/86 *R1561DSD: 100 SCONS
	 *      CHG 08/29/85 */

	/*     THIS SUBROUTINE IS CALLED WHEN A VTR IS ENDED NORMALLY.
	 *     IT CAUSES THE SCONS OF THE ELEMENTS OF THE PHRASE TO AGREE
	 *     WHENEVER IT IS CALLED FOR. */
	/*     IT IS CALLED BY T1VTRPRO AND T1SW11 */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <fcrt.h>
#include "logoslib.h"
#include "trans.h"
#include "projexts.h"
#include "parsetrans.h"
#include <string.h>
#include <jbctrl.h>
#include "parsetrans_ext.h"

EXTERN struct t_sw38bkX_ {
	short int adjnp, sw38n, vnum;
	}	sw38bkX_;



void /*FUNCTION*/ vtrend()
{
	static short iz = 0;
	static short nn = 0;
	static short curhed = 0;
	static short sconiz = 0;

	// for tran1 only
	if (tranidX_.tranid != 1 )
	{
		return;
	}

	if( diagsX_.deepdi == 1 )
		{
		fprintf( _spec_fp, " PHRASE HEAD INFORM *%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d\n", 
		  sworkoX_.phcto, supresX_.phcts, flowckX_.phrstr, supresX_.n3sw25, 
		  sworkoX_.phrhdo[sworkoX_.phcto-One], supresX_.phsups, flowckX_.n6, 
		  elscnpX_.elscnp[flowckX_.n6-One], sworkoX_.phrbgo[sworkoX_.phcto-One], 
		  sworkoX_.phrndo[sworkoX_.phcto-One], sw16bkX_.sw16n, sw25bkX_.sw25n, 
		  sw38bkX_.sw38n, sw21bkX_.sw21n );
		}

	/*   SET PHRASE HEAD POSITION FOR TRAN2 */

	/*   FOR DELAYED SETTING OF PHRASE HEAD (SEE SW34) */
	for( nn=1; nn <= sworkoX_.phcto; nn++ ){
		if( sworkoX_.phrhdo[nn-One] >= 900 )
			sworkoX_.phrhdo[nn-One] = elscnpX_.elscnp[sworkoX_.phrhdo[nn-One]-900-One];
		}
	if( sworkoX_.phcto != supresX_.phcts ){

		if( sworkoX_.phrhdo[sworkoX_.phcto-One] != 0 ){
			if( supresX_.phsups == 0 )
				goto L_8360;
			if( sw34bkX_.sw34n == 1 )
				goto L_8360;
			}
		sworkoX_.phrhdo[sworkoX_.phcto-One] = elscnpX_.elscnp[flowckX_.n6-One];
		if( sworkoX_.phrhdo[sworkoX_.phcto-One] == 0 )
			sworkoX_.phrhdo[sworkoX_.phcto-One] = opadroX_.sconpo[sworkoX_.phrbgo[sworkoX_.phcto-One]-One];
		}

	/*   MAKE NOUN PHRASE ELEMENTS
	 *   AGREE IN NUMBER,PERSON,CASE,
	 *   AND GENDER */

	/*   NO SW38 IF SW25 OR SW21 ON */
L_8360:
	if( sw25bkX_.sw25n == 1 || sw21bkX_.sw21n == 1 )
		sw38bkX_.sw38n = 0;

	if( passesX_.passfl == 1 && passesX_.passct == 1 ){
		sw25bkX_.sw25n = 0;
		supresX_.n3sw25 = 0;
		sw21bkX_.sw21n = 0;
		sw38bkX_.sw38n = 0;
		}
	else{

		if( sw38bkX_.sw38n != 0 ){

			curhed = sworkoX_.phrhdo[sworkoX_.phcto-One];
			for( iz=flowckX_.phrstr; iz <= diacbX_.n3; iz++ ){
				sconiz = opadroX_.sconpo[iz-One];
				if( sconiz > 0 ){
					if( sconX_.scon[sconiz-One][1-One] > 0 ){
						sconX_.scon[sconiz-One][4-One] = sconX_.scon[curhed-One][4-One];
						sconX_.scon[sconiz-One][5-One] = sconX_.scon[curhed-One][5-One];
						sconX_.scon[sconiz-One][6-One] = sconX_.scon[curhed-One][6-One];
						}
					}
				}
			sw38bkX_.sw38n = 0;
			}

		if( sw25bkX_.sw25n == 1 ){
			sw25bkX_.sw25n = 0;
			/*                       SEE SW25 C.F. N3SW25 */
			if( supresX_.n3sw25 != 0 )
				flowckX_.phrstr = supresX_.n3sw25;
			supresX_.n3sw25 = 0;

			for( iz=flowckX_.phrstr; iz <= diacbX_.n3; iz++ ){
				sconiz = opadroX_.sconpo[iz-One];
				if( sconiz > 0 ){
					if( sconX_.scon[sconiz-One][1-One] > 0 ){
						if( sconX_.scon[sconiz-One][9-One] != 1 ){
							if( srcflgX_.srcflg == 2 )
								sconX_.scon[sconiz-One][3-One] = sw25bkX_.hedsc3;
							sconX_.scon[sconiz-One][4-One] = sw25bkX_.hedgen;
							sconX_.scon[sconiz-One][5-One] = sw25bkX_.hednum;
							sconX_.scon[sconiz-One][6-One] = sw25bkX_.hedper;
							/*                       SET CASE ONLY FOR GERMAN TARGET.  02/11/91*JAL */
							if( trgflgX_.trgflg == 1 ){
								/*                       FOR EG, IF VC 108 THEN DONT SET CASE. */
								if( !(sconX_.scon[sconiz-One][1-One] == 108 && 
									  srcflgX_.srcflg == 2) ){
									if( sw25bkX_.hedcas != 0 )
										sconX_.scon[sconiz-One][7-One] = sw25bkX_.hedcas;
									}
								}
							}
						}
					}
				}
			}

		/*   SAME FOR VERB PHRASE */

		if( sw21bkX_.sw21n == 1 ){
			sw21bkX_.sw21n = 0;
			curhed = sworkoX_.phrhdo[sworkoX_.phcto-One];
			for( iz=flowckX_.phrstr; iz <= diacbX_.n3; iz++ ){
				sconiz = opadroX_.sconpo[iz-One];
				if( sconiz > 0 ){
					if( sconX_.scon[sconiz-One][1-One] > 0 ){
						if( sconX_.scon[sconiz-One][9-One] != 1 ){
							sconX_.scon[sconiz-One][4-One] = sconX_.scon[curhed-One][4-One];
							sconX_.scon[sconiz-One][5-One] = sconX_.scon[curhed-One][5-One];
							sconX_.scon[sconiz-One][6-One] = sconX_.scon[curhed-One][6-One];
							if( !(sconX_.scon[sconiz-One][1-One] >= 99 &&
								  sconX_.scon[sconiz-One][1-One] <= 120) ){
								if( sconX_.scon[sconiz-One][7-One] == 0 )
									sconX_.scon[sconiz-One][7-One] = sconX_.scon[curhed-One][7-One];
								}
							}
						}
					}
				}
			}
		}

	return;
} /*end of function*/

