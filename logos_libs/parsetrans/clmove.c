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
/******************************************************************
 *  CLSMOV -  EXTRACT A CLAUSE FROM ORIGINAL POSITION AND RELOCATE IT
 *            IN THE NSWORKS.
 *  PROCESS:
 *     BREAK THE CURRENT SENTENCE INTO 4 SUBSTRINGS BASED ON PRE-MOVE
 *     DISTINCTIONS (THEIR ORDER REFLECTS PRE-MOVE ORDER):
 *       SUB1- ALL SWORKS THAT PRECEDE THE CLAUSE TO MOVE(MOVECLAUSE)
 *       SUB2- THE MOVE CLAUSE.
 *       SUB3- SWORKS WHICH FOLLOW THE MOVECLAUSE BEFORE RELOCATION
 *             BUT WILL PRECEDE THE CLAUSE AFTER RELOCATION.
 *       SUB4- SWORKS WHICH FOLLOW THE MOVECLAUSE BEFORE & AFTER. */

/*     THE FINAL ORDER WILL BE: 1,3,2,4.
 *     THE BULK OF PROCESSING IS THE REORDERING OF THE SOURCE ARRAYS
 *     (I.E. THE PHRASE ARRAYS) AND THE CREATION OF 3 NEW SWORKS TO
 *     REFLECT THE NEW CLAUSE ANALYSIS.  MOST OF THE CHANGES NECESSARY
 *     TO ADD THE 3 NEW CLAUSE ELEMENTS(CM,BOS,EOS) HAVE
 *     ALREADY BEEN DONE BY THE -67 001 SWITCH.
 *     MOST SUBSTRING PARAMETERS NEEDED FOR RELOCATION ARE PRE-CALCULATED
 *     AND KEPT IN THE CLSMOV ARRAYS (SEE "I/O AND LOCAL"). */


/*  CLSMOV(EOSWC,EOSTY,EOSFM,EOSFMS)
 *  INPUT:
 *    EOSWC,EOSTY,EOSFM - WC,TYPE,FORM OF EOS ELEMENT APPENDED TO CLAUSE
 *    EOSFMS  - FORMSAVE VALUE OF THE EOS ELEMENT. */

/*  OUTPUT:
 *     CLSNFO - INFO FOR RELOCATED CLAUSES.  SEE TRANS MACLIB MEMBER */

/*  I/O AND LOCAL:
 *     CLSMOV - (TRANS MACLIB).   CLAUSE INFO VITAL FOR MOVING. */

/*           FOLLOWING ARRAYS REFLECT PRE-MOVE STRING PARAMETERS:
 *     STRBEG - HOLD BEGIN SWORK PTR FOR EACH SUBSTRING TO BE MOVED.
 *     STREND -  "   END     "    "             "                "  .
 *     STRLEN - SIZE OF NTH STRING.
 *     STRCNT - NUMBER OF SUBSTRINGS TO MOVE.
 *     NSBEGB - BYTE COUNT WHICH BEGINS THE MOVE CLAUSE IN SWORK.
 *     NSTOTB - TOTAL BYTES TO MOVE IN SWORK ARRAY.
 *     PHBEGB - BYTE COUNT WHICH BEGINS THE MOVE CLAUSE IN PHRASE.
 *     PHTOTB - TOTAL BYTES TO MOVE IN PHRASE ARRAY. */

/*     NSW2   - PTR TO TARGET SWORK BEING REORDERED.
 *     WRKSPC - TEMP SPACE TO HOLD ORIGINAL ARRAY WHILE TARGET BEING MADE */

/*  CHANGES:
 *  11/16/91 *JAL* :  LOAD CLAUSE WORD COUNT INTO SCON3 OF CLAUSE MARKER
 *  11/16/91 *JAL* :  MOVE AS MUCH AS POSSIBLE TO 67001 SWITCH TO BE
 *                    PROCESSED AS SOON AS POSSIBLE.
 *  08/16/91 *JAL* :  DONT REORDER THE TARGET ARRAYS.
 *  02/21/91 *JAL* :  ACCESS HASHCD AND HENUM2 VALUES VIA SCOLNK TO
 *                    ACCOUNT FOR NEW CLAUSE ELEMENTS ADDED BEYOND THE
 *                    100TH SCON */

/******************************************************************** */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#define MS_F77
#include <logos_include_res_pt/fcrt.h>
#include "logoslib.h"
#include "trans.h"
#include "projexts.h"
#include "parsetrans.h"
#include <logos_include_res_pt/jbctrl.h>
#include "parsetrans_ext.h"





void /*FUNCTION*/ clmove(eoswc, eosty, eosfm, eosfms)
long int eoswc, eosty, eosfm, eosfms;
{
	static short int anydi, begns, endns, frstch, i, k, movcnt, moveid, 
	  nxtpos, wrkspc[OPADRX];
	static long int nsbegb, nstotb, nsw2, nsw2sv, phbegb, phtotb, 
	  strbeg[4], strcnt, strend[4], strlen_[4];
	static char pgmnam[9] = "T2CLMOVE";

	if( tranidX_.tranid != 2 ){
		return;
	}


	/*                            LOCAL VARS & PARMS */
	/*                            WORK ARRAY MUST BE AS BIG AS LARGEST ARRAY
	 *                            TO REORDER. MUST BE I*2 ARRAY TO HANDLE
	 *                            SPECIAL NEEDS OF REORDERING PHRBEG(). */
	/*                            COMMON VARS */
	/*                            COMMON BLOCKS */
	/*-----------------           INITIAL PARAMETER */
	/*                            GET NEW CLAUSES' BOUNDING SWORKS */
	begns = clsnfoX_.clbgns[clsnfoX_.clcrnt-One];
	endns = clsnfoX_.clndns[clsnfoX_.clcrnt-One];
	/*                            WEIRD CASE CHECK:
	 *                            IS LAST SWORK OF CHILD THE LAST
	 *                            SWORK OF PARENT CLAUSE,TOO? */
	if( endns == clsnfoX_.clndns[clsnfoX_.clprnt[clsnfoX_.clcrnt-One]-One] ){
		/*                            LEAVE THE EOS SWORK FOR PARENT,IF POSSIBLE. */
		if( (sworkX_.swork[endns-One][1-One] == 20 && sworkX_.swork[endns-One][2-One] == 10) 
			&& clsnfoX_.clbgns[clsnfoX_.clcrnt-One] < endns ){
			endns -= 1;
			clsnfoX_.clndns[clsnfoX_.clcrnt-One] = endns;
			}
		}

	/*                            GET THE ID OF MOVED CLAUSE */
	moveid = clsnfoX_.clcrnt;
	if( moveid != clsmovX_.clmid[clsmovX_.clmcnt-One] ){
		if( anydi == 1 )
			{
			fprintf( _spec_fp, "\n*************  ERROR -  T2CLMOVE *******************\n  CLMID IN MOVE ARRAY NOT EQUAL TO CURRENT CLAUSE.  \n  CLCRNT,CLMID(CLMCNT),CLMCNT = %4d %4d %4d\n  CONTINUING.  RESULTS UNPREDICTABLE ???\n****************************************************\n", 
			  clsnfoX_.clcrnt, clsmovX_.clmid[clsmovX_.clmcnt-One], 
			  clsmovX_.clmcnt );
			}
		errlog(pgmnam,1,0,10);
		}

	/*-------------------         DETERMINE WHERE CLAUSE GOES
	 *                            EITHER BEFORE 1ST CHILD OR AT END OF SWORK.
	 *                            CLMORD() INDICATES 1ST CHILD OF THIS CLAUSE */

	/*                            CALL CLSPOS(BEGNS,ENDNS,NXTPOS) */
	frstch = clsmovX_.clmord[clsmovX_.clmcnt-One];
	if( frstch == 0 ){
		nxtpos = sworkX_.phct + 1;
		}
	else{
		nxtpos = clsnfoX_.clbgns[frstch-One];
		}

	/*-------------------         SET UP MOVE PARAMETERS. */

	/*                            HOW MANY SUBSTRINGS MUST BE MOVED? */
	strcnt = 4;
	if( nxtpos > sworkX_.phct ){
		strcnt = 3;
		nxtpos = sworkX_.phct + 1;
		if( endns == sworkX_.phct )
			strcnt = 2;
		}
	/*                            SUBSTRING1 - PRECEDING MOVE CLAUSE. */
	strbeg[1-One] = 1;
	strend[1-One] = begns - 1;
	strlen_[1-One] = begns - 1;
	/*                            SUB2 - MOVE-CLAUSE STRING */
	strbeg[2-One] = begns;
	strend[2-One] = endns;
	strlen_[2-One] = endns - begns + 1;
	/*                           INIT SUB3 AND SUB4 TO ZERO DEFAULT */
	zapit((short*)&strbeg[3-One],4,(byte)0);
	zapit((short*)&strlen_[3-One],4,(byte)0);
	/*                            SUB3 - STRING THAT SWAPS POS W/ SUB2 */
	if( strcnt > 2 ){
		strbeg[3-One] = endns + 1;
		strend[3-One] = nxtpos - 1;
		strlen_[3-One] = strend[3-One] - strbeg[3-One] + 1;
		/*                            SUB4 - FOLLOWING FINAL MOVE-CLAUSE POS */
		if( strcnt > 3 ){
			strbeg[4-One] = nxtpos;
			strend[4-One] = sworkX_.phct;
			strlen_[4-One] = strend[4-One] - strbeg[4-One] + 1;
			}
		}
	/*                            TOTAL SUBSTRING SIZES- MEASURE IN BYTES
	 *                            SWORK-1ST BYTE AND TOTAL BYTES TO MOVE. */
	nsbegb = strbeg[2-One]*8 - 7;
	nstotb = (strend[strcnt-One] - strbeg[2-One] + 1)*8;
	/*                            PHRBEG,PHRHED */
	phbegb = strbeg[2-One]*2 - 1;
	phtotb = (strend[strcnt-One] - strbeg[2-One] + 1)*2;

	/*-------------------   REORDER ALL SOURCE ARRAYS. */

	/*                            FIRST MOVE ARRAY TO PARALLEL TEMP ARRAY.
	 *                            THEN COPY EACH SUBSTRING FROM TEMP ARRAY
	 *                            BACK TO REAL ARRAY IN FINAL ORDER.
	 *                            UP TO 4 SECTIONS PER ARRAY PLUS CLAUSE
	 *                            MARKER, BOS AND EOS ELEMENTS.
	 *                            FINAL SUBSTRING ORDER =  1,3,2,4 */


	/*                            -------- PHRBEG -------- */

	lmove(wrkspc,phbegb,sworkX_.phrbeg,phbegb,phtotb);
	nsw2 = strbeg[2-One];

	/*                            CLAUSE MARKER ELEMENT */
	sworkX_.phrbeg[nsw2-One] = clsmovX_.clmopc[clsmovX_.clmcnt-One];
	nsw2 += 1;
	/*                            SUBSTRING 3 */
	if( strcnt > 2 ){
		lmove(sworkX_.phrbeg,nsw2*2-1,wrkspc,strbeg[3-One]*2-1,strlen_[3-One]*
		  2);
		nsw2 += strlen_[3-One];
		}
	/*                            BOS ELEMENT */
	sworkX_.phrbeg[nsw2-One] = clsmovX_.clmopc[clsmovX_.clmcnt-One] + 
	  1;
	nsw2 += 1;
	/*                            SUBSTRING 2 THE CLAUSE */
	lmove(sworkX_.phrbeg,nsw2*2-1,wrkspc,strbeg[2-One]*2-1,strlen_[2-One]*
	  2);
	nsw2 += strlen_[2-One];
	/*                            THE CLAUSE EOS ELEMENT */
	sworkX_.phrbeg[nsw2-One] = clsmovX_.clmopc[clsmovX_.clmcnt-One] + 
	  2;
	nsw2 += 1;
	/*                            APPEND SUBSTRING 4 */
	if( strcnt > 3 )
		lmove(sworkX_.phrbeg,nsw2*2-1,wrkspc,strbeg[4-One]*2-1,strlen_[4-One]*
		  2);

	/*                            -------- PHREND -------- */

	lmove(wrkspc,phbegb,sworkX_.phrend,phbegb,phtotb);
	nsw2 = strbeg[2-One];

	/*                            CLAUSE MARKER ELEMENT */
	sworkX_.phrend[nsw2-One] = clsmovX_.clmopc[clsmovX_.clmcnt-One];
	nsw2 += 1;
	/*                            SUBSTRING 3 */
	if( strcnt > 2 ){
		lmove(sworkX_.phrend,nsw2*2-1,wrkspc,strbeg[3-One]*2-1,strlen_[3-One]*
		  2);
		nsw2 += strlen_[3-One];
		}
	/*                            BOS ELEMENT */
	sworkX_.phrend[nsw2-One] = clsmovX_.clmopc[clsmovX_.clmcnt-One] + 
	  1;
	nsw2 += 1;
	/*                            SUBSTRING 2 THE CLAUSE */
	lmove(sworkX_.phrend,nsw2*2-1,wrkspc,strbeg[2-One]*2-1,strlen_[2-One]*
	  2);
	nsw2 += strlen_[2-One];
	/*                            THE CLAUSE EOS ELEMENT */
	sworkX_.phrend[nsw2-One] = clsmovX_.clmopc[clsmovX_.clmcnt-One] + 
	  2;
	nsw2 += 1;
	/*                            APPEND SUBSTRING 4 */
	if( strcnt > 3 )
		lmove(sworkX_.phrend,nsw2*2-1,wrkspc,strbeg[4-One]*2-1,strlen_[4-One]*
		  2);

	/*                            -------- PHRHED -------- */

	lmove(wrkspc,phbegb,sworkX_.phrhed,phbegb,phtotb);
	nsw2 = strbeg[2-One];

	/*                            CLAUSE MARKER ELEMENT */
	sworkX_.phrhed[nsw2-One] = clsnfoX_.clmrkr[clsmovX_.clmid[clsmovX_.clmcnt-One]-
	  One];
	nsw2 += 1;
	/*                            SUBSTRING 3 */
	if( strcnt > 2 ){
		lmove(sworkX_.phrhed,nsw2*2-1,wrkspc,strbeg[3-One]*2-1,strlen_[3-One]*
		  2);
		nsw2 += strlen_[3-One];
		}
	/*                            BOS ELEMENT */
	sworkX_.phrhed[nsw2-One] = clsmovX_.clmscb[clsmovX_.clmcnt-One];
	nsw2 += 1;
	/*                            SUBSTRING 2 THE CLAUSE */
	lmove(sworkX_.phrhed,nsw2*2-1,wrkspc,strbeg[2-One]*2-1,strlen_[2-One]*
	  2);
	nsw2 += strlen_[2-One];
	/*                            THE CLAUSE EOS ELEMENT */
	sworkX_.phrhed[nsw2-One] = clsmovX_.clmsce[clsmovX_.clmcnt-One];
	nsw2 += 1;
	/*                            APPEND SUBSTRING 4 */
	if( strcnt > 3 )
		lmove(sworkX_.phrhed,nsw2*2-1,wrkspc,strbeg[4-One]*2-1,strlen_[4-One]*
		  2);

	/*                            -------- SWORK -------- */

	lmove(wrkspc,nsbegb,(short*)sworkX_.swork,nsbegb,nstotb);
	nsw2 = strbeg[2-One];
	/*                            CLAUSE MARKER ELEMENT */
	sworkX_.swork[nsw2-One][1-One] = clsmovX_.clmwtf[clsmovX_.clmcnt-One][1-One];
	sworkX_.swork[nsw2-One][2-One] = clsmovX_.clmwtf[clsmovX_.clmcnt-One][2-One];
	sworkX_.swork[nsw2-One][3-One] = clsmovX_.clmwtf[clsmovX_.clmcnt-One][3-One];
	sworkX_.swork[nsw2-One][4-One] = clsnfoX_.clmrkr[clsmovX_.clmid[clsmovX_.clmcnt-One]-
	  One];
	nsw2 += 1;
	/*                            SUBSTRING 3 */
	if( strcnt > 2 ){
		lmove((short*)sworkX_.swork,nsw2*8-7,wrkspc,strbeg[3-One]*
		  8-7,strlen_[3-One]*8);
		nsw2 += strlen_[3-One];
		}
	/*                            BOS ELEMENT */
	sworkX_.swork[nsw2-One][1-One] = clsmovX_.clmbos[clsmovX_.clmcnt-One][1-One];
	sworkX_.swork[nsw2-One][2-One] = clsmovX_.clmbos[clsmovX_.clmcnt-One][2-One];
	sworkX_.swork[nsw2-One][3-One] = clsmovX_.clmbos[clsmovX_.clmcnt-One][3-One];
	sworkX_.swork[nsw2-One][4-One] = clsmovX_.clmscb[clsmovX_.clmcnt-One];
	nsw2 += 1;
	/*                            SUBSTRING 2 THE CLAUSE */
	lmove((short*)sworkX_.swork,nsw2*8-7,wrkspc,strbeg[2-One]*8-7,
	  strlen_[2-One]*8);
	nsw2sv = nsw2;
	nsw2 += strlen_[2-One];
	/*                            THE CLAUSE EOS ELEMENT */
	sworkX_.swork[nsw2-One][1-One] = eoswc;
	sworkX_.swork[nsw2-One][2-One] = eosty;
	sworkX_.swork[nsw2-One][3-One] = eosfm;
	sworkX_.swork[nsw2-One][4-One] = clsmovX_.clmsce[clsmovX_.clmcnt-One];
	nsw2 += 1;
	/*                            APPEND SUBSTRING 4 */
	if( strcnt > 3 )
		lmove((short*)sworkX_.swork,nsw2*8-7,wrkspc,strbeg[4-One]*
		  8-7,strlen_[4-One]*8);
	/*C
	 *C                            -------- SWORKI --------
	 *C
	 *C                             ONLY IF PASS1 OF 2 PASS STRATEGY
	 *         IF (PASSFL.NE.1 .OR. PASSCT.NE.1) GOTO 600
	 *         CALL LMOVE (WRKSPC,NSBEGB,SWORKI,NSBEGB,NSTOTB)
	 *         NSW2 = STRBEG(2)
	 *C                            CLAUSE MARKER ELEMENT
	 *         SWORKI(1,NSW2) = CLMWTF(1,CLMCNT)
	 *         SWORKI(2,NSW2) = CLMWTF(2,CLMCNT)
	 *         SWORKI(3,NSW2) = CLMWTF(3,CLMCNT)
	 *         SWORKI(4,NSW2) = CLMRKR(CLMID(CLMCNT))
	 *         NSW2 = NSW2 + 1
	 *C                            SUBSTRING 3
	 *         IF (STRCNT.GT.2)THEN
	 *           CALL LMOVE (SWORKI,NSW2*8-7,WRKSPC,STRBEG(3)*8-7,STRLEN(3)*8)
	 *             NSW2 = NSW2 + STRLEN(3)
	 *         ENDIF
	 *C                            BOS ELEMENT
	 *         SWORKI(1,NSW2) = CLMBOS(1,CLMCNT)
	 *         SWORKI(2,NSW2) = CLMBOS(2,CLMCNT)
	 *         SWORKI(3,NSW2) = CLMBOS(3,CLMCNT)
	 *         SWORKI(4,NSW2) = CLMSCB(CLMCNT)
	 *         NSW2 = NSW2 + 1
	 *C                            SUBSTRING 2 THE CLAUSE
	 *         CALL LMOVE (SWORKI,NSW2*8-7,WRKSPC,STRBEG(2)*8-7,STRLEN(2)*8)
	 *         NSW2SV = NSW2
	 *         NSW2 = NSW2 + STRLEN(2)
	 *C                            THE CLAUSE EOS ELEMENT
	 *         SWORKI(1,NSW2) = EOSWC
	 *         SWORKI(2,NSW2) = EOSTY
	 *         SWORKI(3,NSW2) = EOSFM
	 *         SWORKI(4,NSW2) = CLMSCE(CLMCNT)
	 *         NSW2 = NSW2 + 1
	 *C                            APPEND SUBSTRING 4
	 *         IF (STRCNT.GT.3)
	 *     *     CALL LMOVE (SWORKI,NSW2*8-7,WRKSPC,STRBEG(4)*8-7,STRLEN(4)*8)
	 *600      CONTINUE */


	/*                            -------- CLBGNS, CLNDNS  ---------- */

	/*                            UPDATE SWORK BOUNDARY POINTERS FOR ANY
	 *                            CLAUSES ALREADY MOVED. */
	for( i=1; i <= clsnfoX_.cltotl; i++ ){
		/*                            STRING 1: UPDATE ENDPTR OF ANY CLAUSE THAT
		 *                            BEGINS TO LEFT OF MOVED CLAUSES PREV. POS.
		 *                            INCLUDES ALL CLAUSES WITHIN WHICH MOVED
		 *                            CLAUSE WAS NESTED. */
		if( clsnfoX_.clbgns[i-One] < strbeg[2-One] ){
			if( clsnfoX_.clndns[i-One] >= strbeg[3-One] ){
				/*                            STRING 2 */
				clsnfoX_.clndns[i-One] += -strlen_[2-One] + 1;
				}
			else if( clsnfoX_.clndns[i-One] > strbeg[2-One] ){
				clsnfoX_.clndns[i-One] = strbeg[2-One] - 1;
				}
			}
		else if( (strcnt < 3) || (clsnfoX_.clbgns[i-One] < strbeg[3-One]) ){
			clsnfoX_.clbgns[i-One] += strlen_[3-One] + 1;
			clsnfoX_.clndns[i-One] += strlen_[3-One] + 1;
			/*                            STRING 3: */
			}
		else if( (strcnt < 4) || (clsnfoX_.clbgns[i-One] < strbeg[4-One]) ){
			clsnfoX_.clbgns[i-One] += -strlen_[2-One] + 1;
			clsnfoX_.clndns[i-One] += -strlen_[2-One] + 1;
			/*                            STRING 4: */
			}
		else{
			clsnfoX_.clbgns[i-One] += 3;
			clsnfoX_.clndns[i-One] += 3;
			}
		}
	/*                            SET CLUASE BOUNDARIES OF MOVED CLAUSE */
	if( strcnt >= 3 ){
		clsnfoX_.clbgns[moveid-One] = strbeg[2-One] + 1 + strlen_[3-One];
		}
	else{
		clsnfoX_.clbgns[moveid-One] = strbeg[2-One] + 1;
		}
	clsnfoX_.clndns[moveid-One] = clsnfoX_.clbgns[moveid-One] + strlen_[2-One] + 
	  1;

	/*----------------            A LITTLE (LOT OF) HOUSEKEEPING */

	/*                            SET CLSCON ARRAYS
	 *                            ASSUMES WRKSPC STILL = ORIGINAL SWORK */
	for( k=nsw2sv; k <= (nsw2sv + strlen_[2-One] - 1); k++ ){
		clsconX_.clsid[sconX_.scolnk[sworkX_.swork[k-One][4-One]-One]-
		  One] = moveid;
		}
	/*                            SET SCON TABLE FOR EOS */
	sconX_.scon[clsmovX_.clmsce[clsmovX_.clmcnt-One]-One][1-One] = eoswc;
	if( eosty <= 16 ){
		sconX_.scon[clsmovX_.clmsce[clsmovX_.clmcnt-One]-One][13-One] = eosty;
		}
	else if( eosty <= 99 ){
		sconX_.scon[clsmovX_.clmsce[clsmovX_.clmcnt-One]-One][11-One] = eosty;
		}
	else{
		sconX_.scon[clsmovX_.clmsce[clsmovX_.clmcnt-One]-One][2-One] = eosty;
		}
	/*                            SET FORMSAVE FOR EOS */
	formsaX_.formsv[clsmovX_.clmelc[clsmovX_.clmcnt-One]+2-One] = eosfms;
	/*                            LOAD CLAUSE SIZE INTO SCON3 OF CLAUSE MARKE
	 *                            ACCORDING TO A CONVERSION FORMULA */
	movcnt = strlen_[2-One]/2;
	if( movcnt > 8 )
		movcnt = 8;
	sconX_.scon[clsmovX_.clmelc[clsmovX_.clmcnt-One]-One][3-One] = movcnt;
	/*                            PARSE CONTINUES AT LAST POS BEFORE CLAUSE. */
	flowckX_.i3save = clsmovX_.clmpos[clsmovX_.clmcnt-One];
	/*                            SWORK COUNTER */
	sworkX_.phct += 3;
	/*                            POP CLSMOV STACK COUNTER */
	clsmovX_.clmcnt -= 1;
	/*                            RESET CURRENT CLAUSE ID TO THE PARENTS' */
	clsnfoX_.clcrnt = clsnfoX_.clprnt[clsnfoX_.clcrnt-One];

	return;
} /*end of function*/

