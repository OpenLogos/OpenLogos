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
#include <fcrt.h>
#include "logoslib.h"
#include "trans.h"
#include "projexts.h"
#include <jbctrl.h>
#include "parsetrans_ext.h"


void /*FUNCTION*/ txsw46()
{
	static short int scipos, swipos;
	static short xx = 0;
	static short k3p2 = 0;
	static short k3p4 = 0;
	static short n7jim = 0;

	flowckX_.n6jim = im81X_.im81 - vbdataX_.k3p1;
	vbdataX_.k3n = vbdataX_.k3 + 5;
	k3p2 = vtrfX_.vtrf[vbdataX_.k3+2-One];
	semargX_.k3p3 = vtrfX_.vtrf[vbdataX_.k3+3-One];
	k3p4 = vtrfX_.vtrf[vbdataX_.k3+4-One];
	/*                        IF 2PASS STRATEGY OFF THEN IGNORE SETTING
	 *                        OF SWORKI, I.E. WHEN SW3146 = 1.
	 *      IF (SW3146.EQ.1 .AND. (PASSFL.NE.1 .OR. PASSCT.NE.1)) GOTO 1300 */
	if( sw3146X_.sw3146 == 1 ){
		if( passesX_.passfl != 1 || passesX_.passct != 1 )
			goto L_1300;
		swipos = sworkiX_.sc2swi[sworkX_.swork[flowckX_.n6jim-One][4-One]-One];
		if( swipos < 1 )
			goto L_1300;
		scipos = sconX_.scolnk[sworkX_.swork[flowckX_.n6jim-One][4-One]-One];
		}

	if( k3p2 != 0 ){
		if( k3p2 > 0 ){
			if( sw3146X_.sw3146 == 1 ){
				sworkiX_.sworki[swipos-One][1-One] = k3p2;
				}
			else{
				sworkX_.swork[flowckX_.n6jim-One][1-One] = k3p2;
				}
			}
		else if( sw3146X_.sw3146 == 1 ){
			sworkiX_.sworki[swipos-One][1-One] = sworkX_.swork[im81X_.im81-k3p2-One][1-One];
			}
		else{
			sworkX_.swork[flowckX_.n6jim-One][1-One] = sworkX_.swork[im81X_.im81-k3p2-One][1-One];
			}
		}

	if( semargX_.k3p3 != 0 ){
		if( semargX_.k3p3 <= 0 ){
			n7jim = im81X_.im81 - semargX_.k3p3;
			if( sw3146X_.sw3146 == 1 ){
				sworkiX_.sworki[swipos-One][2-One] = sworkX_.swork[n7jim-One][2-One];
				sconinX_.sconin[scipos-One][2-One] = sconX_.scon[sworkX_.phrhed[n7jim-One]-One][11-One];
				sconinX_.sconin[scipos-One][3-One] = sconX_.scon[sworkX_.phrhed[n7jim-One]-One][13-One];
				}
			else{
				sworkX_.swork[flowckX_.n6jim-One][2-One] = sworkX_.swork[n7jim-One][2-One];
				sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][11-One] = sconX_.scon[sworkX_.phrhed[n7jim-One]-One][11-One];
				sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][13-One] = sconX_.scon[sworkX_.phrhed[n7jim-One]-One][13-One];
				}

			}
		else if( semargX_.k3p3 > 16 ){
			if( semargX_.k3p3 > 99 ){
				if( sw3146X_.sw3146 == 1 ){
					sworkiX_.sworki[swipos-One][2-One] = semargX_.k3p3;
					if( sconinX_.sconin[scipos-One][3-One] > 99 )
						sconinX_.sconin[scipos-One][3-One] = semargX_.k3p3;
					if( sconinX_.sconin[scipos-One][2-One] > 99 )
						sconinX_.sconin[scipos-One][2-One] = semargX_.k3p3;
					}
				else{
					sworkX_.swork[flowckX_.n6jim-One][2-One] = semargX_.k3p3;
					if( sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-
					  One][13-One] > 99 )
						sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-
						  One][13-One] = semargX_.k3p3;
					if( sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-
					  One][11-One] > 99 )
						sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-
						  One][11-One] = semargX_.k3p3;
					}
				}
			else if( sw3146X_.sw3146 == 1 ){
				if( sworkiX_.sworki[swipos-One][2-One] > 16 && 
					sworkiX_.sworki[swipos-One][2-One] <= 99 )
					sworkiX_.sworki[swipos-One][2-One] = semargX_.k3p3;
				sconinX_.sconin[scipos-One][2-One] = semargX_.k3p3;
				if( sconinX_.sconin[scipos-One][3-One] > 16 && 
					sconinX_.sconin[scipos-One][3-One] < 99 )
					sconinX_.sconin[scipos-One][3-One] = semargX_.k3p3;
				}
			else{
				if( sworkX_.swork[flowckX_.n6jim-One][2-One] > 16 && 
				    sworkX_.swork[flowckX_.n6jim-One][2-One] <= 99 )
					sworkX_.swork[flowckX_.n6jim-One][2-One] = semargX_.k3p3;
				sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][11-One] = semargX_.k3p3;
				if( sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][13-One] > 16 &&
					sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][13-One] <= 99 )
					sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][13-One] = semargX_.k3p3;
				}
			}
		else if( sw3146X_.sw3146 == 1 ){
			if( sworkiX_.sworki[swipos-One][2-One] <= 16 )
				sworkiX_.sworki[swipos-One][2-One] = semargX_.k3p3;
			sconinX_.sconin[scipos-One][3-One] = semargX_.k3p3;
			if( sconinX_.sconin[scipos-One][2-One] <= 16 )
				sconinX_.sconin[scipos-One][2-One] = semargX_.k3p3;
			}
		else{
			if( sworkX_.swork[flowckX_.n6jim-One][2-One] <= 16 )
				sworkX_.swork[flowckX_.n6jim-One][2-One] = semargX_.k3p3;
			    sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][13-One] = semargX_.k3p3;
			if( sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][11-One] <= 16 )
				sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][11-One] = semargX_.k3p3;
			}
		}

	if( k3p4 < 0 ){

		if( sw3146X_.sw3146 == 1 ){
			sworkiX_.sworki[swipos-One][3-One] = sworkX_.swork[im81X_.im81-k3p4-One][3-One];
			if( k3p4 == semargX_.k3p3 && k3p4 == k3p2 )
				formsaX_.prsfrm[sconX_.scolnk[sworkX_.swork[flowckX_.n6jim-One][4-One]-
				  One]-One] = formsaX_.formsv[sconX_.scolnk[sworkX_.swork[im81X_.im81-k3p4-One][4-One]-
				  One]-One];
			}
		else{
			sworkX_.swork[flowckX_.n6jim-One][3-One] = sworkX_.swork[im81X_.im81-k3p4-One][3-One];
			if( k3p4 == semargX_.k3p3 && k3p4 == k3p2 )
				formsaX_.formsv[sconX_.scolnk[sworkX_.swork[flowckX_.n6jim-One][4-One]-
				  One]-One] = formsaX_.formsv[sconX_.scolnk[sworkX_.swork[im81X_.im81-k3p4-One][4-One]-One]-One];
			}
		}
	else if( k3p4 > 0 ){

		if( sw3146X_.sw3146 == 1 ){
			sworkiX_.sworki[swipos-One][3-One] = k3p4;
			}
		else{
			sworkX_.swork[flowckX_.n6jim-One][3-One] = k3p4;
			}
		}


L_1300:
	sw3146X_.sw3146 = 0;

	return;
} /*end of function*/

