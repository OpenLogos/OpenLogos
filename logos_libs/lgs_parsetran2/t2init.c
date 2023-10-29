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
	/*     THIS SUBROUTINE INITIALIZES  VARIABLES AT THE BEGINNING OF THE
	 *     PROCESSING OF EACH SENTENCE. */
	/*     IT IS CALLED BY T2DRIVER AS WE BEGIN TO PROCESS A NEW SENTENCE. */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*    CHANGES:
	 *    03/08/94 jal: initialize noun phrase,translation memory data.
	 *    10/08/92 jal:  increase size of IMATCH to ELMMAX.
	 *      09/16/91 JAL  INIT N6JIMS (POSITION OF LAST PHRASE LOADED)
	 *      03/22/91 JAL   ADD FLAG FOR LONG AND DEEP DIAGNOSTICS.
	 *      05/07/90 JAL:  ADD SAVSW TO /OPSW/ ADD ALLOW DIAGNOSTICS
	 *                     ON A RANGE OF INPUT LINES. PASSED IN JCDLIN().
*/
  
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <fcrt.h>
#include "project.h"
#include "logoslib.h"
#include "trans.h"
#include "projexts.h"
#include <string.h>
#include <jbctrl.h>


void /*FUNCTION*/ init()
{
	static short int xx;
	static long int zaplen;
	static short zero = 0;
	static char pgmnam[9] = "T2INIT  ";
	static byte blank = ' ';

	/*                  ZERO INHB HFDOPO ARRAYS */
	zapit(inhbX_.inhb,1000,(byte)zero);
	zapit(&sav36sX_.sav36s[1-One],22,(byte)zero);
	zaplen = 2*OPADRX;
	zapit(hpdopoX_.hfdopo,zaplen,(byte)zero);
	zapit(loopckX_.imatch,ELMMAX*2,(byte)zero);
	zapit(loopckX_.noloop,42,(byte)zero);
	zapit(nloop2X_.nloop2,42,(byte)zero);
	/*     SPACE TO SAVE SCONPO FOR -27-8X/-27001     10/15/86  *R1556DSD */
	zapit((short*)sw27bkX_.vchold,72,(byte)zero);

	/*+   NEW FOR VTRBRNCH PROGRAM: VBCELL IS THE 200 BYTE ARRAY WHICH
	 *    CONTAINS 100 2 BYTE 'CELLS' USED BY THE -55, -56, -57 SWITCHES.
	 *    INITIALIZATION TAKES PLACE AS FOLLOWS:
	 *          CELLS   1-10   CLEAR FOR EACH VTR
	 *          CELLS  11-60   CLEAR FOR EACH TRAN
	 *          CELLS  61-100  CLEAR AT BEGINNING OF TRAN 1 ONLY
	 *          CELLS 101-200  EXTRA-SENTENTIAL: CLEAR TIME TO BE DEFINED
	 *     CLEAR 60 CELLS                             10/16/86  *R1539 */
	zapit(vbdataX_.vbcell,120,(byte)zero);
	minickX_.minifg = 0;
	loopckX_.call36[1-One] = 0;
	loopckX_.call36[2-One] = 0;
	sw25bkX_.adlock = 0;
	sw44bkX_.buckct = 0;
	sw31bkX_.offlag = 0;
	sw31bkX_.lpflag = 0;
	head26X_.headwc = 0;
	head26X_.headty = 0;
	head26X_.headfr = 0;
	head26X_.headhd = 0;
	head26X_.headt2 = 0;
	head26X_.headt3 = 0;
	opadroX_.opo = 0;
	minickX_.k7m = 0;
	sw26nX_.sw26n = 0;
	sw26bkX_.phr26 = 0;
	sw38bkX_.compld = 0;
	sw38bkX_.sw38n = 0;
	sw25bkX_.hedper = 3;
	sw25bkX_.hedgen = 0;
	sw25bkX_.hedcas = 1;
	sw25bkX_.hednum = 0;

	for( vwarg2X_.i=1; vwarg2X_.i <= 30; vwarg2X_.i++ ){
		cnX_.cn[vwarg2X_.i-One] = 1;
		hfdm1X_.hfdm1[vwarg2X_.i-One] = -1;
		}

	sw14bkX_.sw14n = 0;
	sw19bkX_.tense = 0;
	sw21bkX_.case_ = 1;
	sw14bkX_.num = 0;
	phsupX_.phcts = 0;
	sw38bkX_.gb108 = 0;
	sw36bkX_.relcas = 1;
	sw36bkX_.relnum = 0;
	sw14bkX_.per = 3;
	sw14bkX_.gen = 0;
	sw36bkX_.rel = 0;
	sw38bkX_.sw38n = 0;
	sw25bkX_.sw25n = 0;
	sw21bkX_.sw21n = 0;
	sw3146X_.sw3146 = 0;
	vwarg2X_.i = 0;
	w50valX_.i3 = 1;
	flowckX_.i3save = 1;
	sploopX_.li = 0;
	sw31bkX_.comp = 0;
	sw31bkX_.depcl = 0;
	sw31bkX_.inform = 0;
	sw31bkX_.cause = 0;

	/*        SET FUNCTION AND WORD SWITCHES */

	phsupX_.phsup = 0;
	sworkoX_.phcto = 0;
	flowckX_.strphr = 1;
	/*        INITIALIZE CLAUSE INFORMATION
	 *           ALL CLAUSES START WITH MAIN CLAUSE ID. SEE TRANS MACLIB
	 *           SET UP FOR MAIN CLAUSE. */
	clsnfoX_.clcrnt = 1;
	clsmovX_.clmcnt = 0;
	/*C              INIT HERE ONLY IF NOT PASS2 OF 2 PASS STRATEGY
	 *      IF (PASSFL.NE.1 .OR. PASSCT.NE.2) THEN */
	clsnfoX_.cltotl = 1;
	for( vwarg2X_.i=1; vwarg2X_.i <= ELMMAX; vwarg2X_.i++ ){
		clsconX_.clsid[vwarg2X_.i-One] = 1;
		}
	zapit(clsconX_.cmchld,ELMMAX*2,(byte)0);
	zapit(clsconX_.achild,ELMMAX*2,(byte)0);
	clsnfoX_.clprnt[1-One] = 0;
	clsnfoX_.clmrkr[1-One] = 0;
	clsnfoX_.clansc[1-One] = 0;
	clsnfoX_.clbgns[1-One] = 1;
	clsnfoX_.clndns[1-One] = sworkX_.phct;
	/*         ENDIF
	 *             ZERO CLAUSE OUTPUT-SWORK BOUNDARY POINTERS */
	for( xx=1; xx <= CLSMAX; xx++ ){
		clsoutX_.clbgno[xx-One] = 0;
		clsoutX_.clndno[xx-One] = 0;
		}
	clsoutX_.clbgno[1-One] = 1;
	/*        SET SEARCH BOUNDARIES FOR -67006 SWITCH */
	for( vwarg2X_.i=1; vwarg2X_.i <= CLSMAX; vwarg2X_.i++ ){
		clsnopX_.cl676r[vwarg2X_.i-One] = 999;
		clsnopX_.cl676l[vwarg2X_.i-One] = 0;
		}

	/*        N6JIMS IS THE LAST PHRASE LOADED */
	flowckX_.n6jims = 0;
	/*+             init NP translation memory data    3/08/94 jal */
	if( tmnphrX_.tmnpfl == 1 )
		npinit();

	/*        INITIALIZE DIAGNOS FOR SCREEN */
		if( diagsX_.anydi == 1 ){
		diagno(1);
		diagno(5);
		diagno(13);

		}
	return;
} /*end of function*/

