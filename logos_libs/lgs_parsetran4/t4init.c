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
	/*     CHANGES:
	 *      10/12/92 jal: increase size of imatch to elmmax
	 *      09/16/91 JAL  INIT N6JIMS (POSITION OF LAST PHRASE LOADED)
	 *      03/22/91 JAL   ADD FLAG FOR LONG AND DEEP DIAGNOSTICS.
	 *      05/07/90 JAL:  ADD SAVSW TO /OPSW/ ADD ALLOW DIAGNOSTICS
	 *                     ON A RANGE OF INPUT LINES. PASSED IN JCDLIN().
	 *      09/15/89 JAL CLAUSE WORK
	 *      04/17/87 *R1685RKH*  CHANGE T1-4 SWORK LIMIT FROM 50 TO
	 *      10/16/86 *R1539DSD: CLEAR 60 CELLS AT START OF TRANS
	 *      10/15/86 *R1556DSD: -27-8X NULLS OPADR, SAVES SCONPO FOR FN 1
	 *      08/30/86 *R1561DSD: 100 SCONS */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <fcrt.h>
#include "project.h"
#include "logoslib.h"
#include "trans.h"
#include "projexts.h"
#include "parsetrans.h"
#include <string.h>
#include <jbctrl.h>

void /*FUNCTION*/ init()
{
	static short int xx;
	static short zero = 0;
	static short iz = 0;

	memset(inhbX_.inhb,'\0',sizeof(inhbX_.inhb));
	memset(hpdopoX_.hfdopo,'\0',sizeof(hpdopoX_.hfdopo));
	memset(loopckX_.noloop,'\0',sizeof(loopckX_.noloop));
	memset(nloop2X_.nloop2,'\0',sizeof(nloop2X_.nloop2));
	memset(loopckX_.imatch,'\0',sizeof(loopckX_.imatch));
	memset(sw27bkX_.vchold,'\0',sizeof(sw27bkX_.vchold));
	minickX_.minifg = 0;

	/*+   NEW FOR VTRBRNCH PROGRAM: VBCELL IS THE 200 BYTE ARRAY WHICH
	 *    CONTAINS 100 2 BYTE 'CELLS' USED BY THE -55,-56,-57 SWITCHES.
	 *    INITIALIZATION TAKES PLACE AS FOLLOWS:
	 *          CELLS   1-10   CLEAR FOR EACH VTR
	 *          CELLS  11-60   CLEAR FOR EACH TRAN
	 *          CELLS  61-100  CLEAR AT BEGINNING OF TRAN 1 ONLY
	 *          CELLS 101-200  EXTRA-SENTENTIAL: CLEAR TIME TO BE DEFINED
	 *     CLEAR 60 CELLS                             10/16/86  *R1539 */
	zapit(vbdataX_.vbcell,120,(byte)zero);

	//                                        GERMAN ONLY */
	if( (sworkX_.swork[1-One][2-One] == 909) && (srcflgX_.srcflg == 1) )
		 sworkX_.swork[1-One][3-One] = 9;



	sw28fmX_.sw28fm = 0;
	loopckX_.call36[1-One] = 0;
	loopckX_.call36[2-One] = 0;
	sw44bkX_.buckct = 0;
	sw38bkX_.compld = 0;
	sw25bkX_.hedper = 3;
	minickX_.k7m = 0;
	sw26nX_.sw26n = 1;
	sw38bkX_.sw38n = 0;
	sw3146X_.sw3146 = 0;
	sw25bkX_.hedgen = 0;
	sw25bkX_.hedcas = 1;
	sw25bkX_.hednum = 0;

	for( vwarg2X_.i=1; vwarg2X_.i <= 30; vwarg2X_.i++ ){
		hfdm1X_.hfdm1[vwarg2X_.i-One] = -1;
		cnX_.cn[vwarg2X_.i-One] = 1;
		}

	sw19bkX_.tense = 0;
	sw14bkX_.case_ = 1;
	sw14bkX_.num = 0;
	sw36bkX_.rel = 0;
	sw38bkX_.gb108 = 0;

	sw36bkX_.relcas = 1;
	sw36bkX_.relnum = 0;
	sw14bkX_.per = 3;
	sw14bkX_.gen = 0;

	sw38bkX_.sw38n = 0;
	vwarg2X_.i = 0;
	w50valX_.i3 = 1;
	flowckX_.i3save = 1;
	sploopX_.li = 0;

	/*             INIT CLAUSE ID INIDICATOR - ASSUME MAIN CLAUSE FIRST */
	clsnfoX_.clcrnt = 1;
	/*             INIT PARENT CELLS 11-60 FOR ALL ACTIVE CLAUSES
	 *             ZERO CLAUSE, OUTPUT-SWORK, BOUNDARY POINTERS */
	for( xx=2; xx <= clsnfoX_.cltotl; xx++ ){
		zapit(&clsnfoX_.clpcel[xx-One][11-One],100,(byte)zero);
		clsoutX_.clbgno[xx-One] = 0;
		clsoutX_.clndno[xx-One] = 0;
		}
	clsoutX_.clbgno[1-One] = 1;
	/*        SET SEARCH BOUNDARIES FOR -67006 SWITCH */
	for( vwarg2X_.i=1; vwarg2X_.i <= CLSMAX; vwarg2X_.i++ ){
		clsnopX_.cl676r[vwarg2X_.i-One] = 999;
		clsnopX_.cl676l[vwarg2X_.i-One] = 0;
		}

		if( diagsX_.anydi == 1 ){
		diagno(1);
		diagno(5);
		diagno(11);
		}


	/*        SET FUNCTION AND WORD SWITCMES */

	phsupX_.phsup = 0;
	sworkoX_.phcto = 1;
	sworkoX_.phrbgo[1-One] = 1;
	sworkoX_.phrndo[1-One] = 1;

	/*        SET VTR TRANSFER-TO-BEGINNING SWITCH */
	opadroX_.opo = 0;
	/*        N6JIMS IS THE LAST PHRASE LOADED */
	flowckX_.n6jims = 0;


	return;
} /*end of function*/

