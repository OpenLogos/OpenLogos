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



void /*FUNCTION*/ rswork_build()
{
	static short int w1, w2, wout;
	static long int swpt;
	static short wskip[15]={1,2,3,0,0,0,7,0,0,0,11,0,0,0,15};

	/*     SWORK1 IS KNOWN IN TRAN 2,3,4 AS RSWORK (ACCESSED BY -29 SWITCH)
	 *     DROP SUBSCRIPTS 1,2,3,7,11 AND 15 WHEN SENDING - NOT NEEDED LATER
	 *     N.B. SWORK1 MAY HAVE BEEN REORDERED BY A -68 SWITCH.  TO KEEP IT
	 *     IN SYNCH WITH OFL1R,OFL4R,AND TARG IN LATER TRANS (I.E. ACCESSED
	 *     VIA SCOLNK() ), WRITE IT OUT IN ITS ORIGINAL ORDER. */
	/*        COMMON: /SW/ SWORK1 */
	/*                         FIRST DETERMINE ORIGINAL SWORK ORDER
	 *     CALL ZAPIT(REORDR,ELMMAX*2,0)
	 *     DO 610  CURPOS = 1, ELEMCT
	 *        ORIGPO = SCOLNK(CURPOS)
	 *        IF (ORIGPO.GT.0 .AND. ORIGPO.LE.ELMMAX) THEN
	 *           REORDR(ORIGPO) = CURPOS
	 *           ENDIF
	 *610   CONTINUE */
	memset(rsworkX_.rswork,'\0',sizeof(rsworkX_.rswork));
	swpt = 0;
	for( w1=1; w1 <= elemctX_.elemct; w1++ ){
		/*                        SKIP IT IF NOT AN ORIGINAL ELEMENT. SINCE
		 *                        ORIGINAL ELS CAN ONLY BE SHIFTED UP THIS
		 *                        WILL RESTORE INPUT ORDER. */
		if( sconX_.scolnk[w1-One] <= elemctX_.origct ){
			/*                        RESTORE SET TO TYPE FIELD */
			swork1X_.swork1[w1-One][(prctX_.js[sconX_.scolnk[w1-One]-
			  One]*4)+1-One] = typsvX_.typsav[sconX_.scolnk[w1-One]-One];
			/*                        LOAD ONLY NECESSARY FIELDS INTO WRKARR */
			swpt += 1;
			wout = 0;
			for( w2=1; w2 <= 15; w2++ ){

				if( w2 != wskip[w2-One] ){
					wout += 1;
					rsworkX_.rswork[swpt-One][wout-One] = swork1X_.swork1[w1-One][w2-One];
					}
				}
			}
		}

	return;
} /*end of function*/

