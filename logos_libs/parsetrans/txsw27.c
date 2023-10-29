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
	/*     LAST CHG: 05/02/87 *R1691RKH*  Deactivate SCONPI for -140 in OPADR
	 *          CHG: 04/17/87 *R1685RKH*  Change T1-4 SWORK limit from 50 to
	 *      CHG 10/15/86 *R1556DSD: -27-8X NULLS OPADR, SAVES SCONPI FOR FN 1
	 *      CHG 08/30/86 *R1561DSD: 100 SCONS
	 *      CHG 08/29/85 */
	/*-                                             RKH  04/17/87   R1685
	 *+ Deactivate SCONPI for -140 in OPADRI         RKH  05/02/87   R1691
	 *-                                             RKH  05/02/87   R1691 */
	/*     SPACE TO SAVE SCONPI FOR -27001            10/15/86  *R1556DSD */
	/*+ Deactivate SCONPI for -140 in OPADRI         RKH  05/02/87   R1691
	 *-                                             RKH  05/02/87   R1691 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#define MS_F77
#include <fcrt.h>
#include "logoslib.h"
#include "trans.h"
#include "projexts.h"
#include <jbctrl.h>
#include "parsetrans_ext.h"

EXTERN struct t_sw27bkX_ {
	short int vchold[12][3];
	}	sw27bkX_;


void /*FUNCTION*/ txsw27()
{
	static short zero = 0;
	static short m = 0;
	static short n = 0;
	static short ct = 0;
	static short str = 0;

	if( vbdataX_.k3p1 == 1 ){

		for( m=1; m <= 12; m++ ){
			if( sw27bkX_.vchold[m-One][1-One] == 0 )
				break;
			str = opadroX_.opo + 1;

			for( n=1; n <= opadroX_.opo; n++ ){
				str -= 1;
				if( opadroX_.opadro[str-One] == sw27bkX_.vchold[m-One][1-One] )
					goto L_9341;
				}
			continue;
L_9341:
			hpdopoX_.hfdopo[str-One] = sw27bkX_.vchold[m-One][2-One];
			/*     RESTORE SCONPI FROM -27-8X                 10/15/86  *R1556DSD */
			opadroX_.sconpo[str-One] = sw27bkX_.vchold[m-One][3-One];
			}

		memset(sw27bkX_.vchold,'\0',sizeof(sw27bkX_.vchold));

		}

	else{

		flowckX_.n6jim = im81X_.im81 - vbdataX_.k3p1;
		flowckX_.phrstr = sworkX_.phrbeg[flowckX_.n6jim-One];
		flowckX_.phrlst = sworkX_.phrend[flowckX_.n6jim-One];
		ct = 0;
		/*     SPACE TO SAVE SCONPI FOR -27001            10/15/86  *R1556DSD */
		memset(sw27bkX_.vchold,'\0',sizeof(sw27bkX_.vchold));

		for( m=flowckX_.phrstr; m <= flowckX_.phrlst; m++ ){

				/*== + ADD -116 TO THE LIST FOR STEVE SCHNEIDERMAN  OLIVER MELLET 11/86
				*+   MAKE IT FOR ALL VCS 111 TO 120                       */
			if( !(opadriX_.opadri[m-One] > -111 || opadriX_.opadri[m-One] < -120) ){

				if( hpdopiX_.hfdopi[m-One] != 0 ){
					/*     FOUND A VC WITH SOMETHING IN IT-SAVE IT IN VCHOLD AND NULLIFY IT. */
					ct += 1;

					if( ct <= 12 )
					{
						sw27bkX_.vchold[ct-One][1-One] = opadriX_.opadri[m-One];
						sw27bkX_.vchold[ct-One][2-One] = hpdopiX_.hfdopi[m-One];
						/*+    SAVE SCONPI FOR -27001, NULL OPADRI         10/15/86  *R1556DSD */
						sw27bkX_.vchold[ct-One][3-One] = opadriX_.sconpi[m-One];
						opadriX_.opadri[m-One] = -140;
						/*+ Deactivate SCONPI for -140 in OPADRI         RKH  05/02/87   R1691 */
						if( opadriX_.sconpi[m-One] != sworkX_.phrhed[flowckX_.n6jim-One] )
							opadriX_.sconpi[m-One] = 1;
						hpdopiX_.hfdopi[m-One] = 0;
					}
					else
					{
						if( diagsX_.longdi == 1 )
						{
							fprintf( _spec_fp, "TOO MANY VCS TO SAVE, OVERLOADING VCHOLD\n" );
						}
					}
				}
				else
				{
					if(tranidX_.tranid == 2)
					{
										/*      THE VC IS EMPTY, NULL IT ANYWAY, THEN GET OUT. */
						opadriX_.opadri[m-One] = -140;
						/*+ Deactivate SCONPI for -140 in OPADRI         RKH  05/02/87   R1691 */
						if( opadriX_.sconpi[m-One] != sworkX_.phrhed[flowckX_.n6jim-One] )
							opadriX_.sconpi[m-One] = 1;
					}
				}


				}

			}
		}


	vbdataX_.k3n = vbdataX_.k3 + 2;
	return;
} /*end of function*/

