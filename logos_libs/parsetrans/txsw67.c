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

/****************************************************************
 * TXSW67 -  HANDLES ALL IDENTIFICATION AND RELOCATION OF
 *           CLAUSES IN TRAN2.
 * FUNCTION:
 *     THE 1ST PARAMETER DEFINES THE FUNCTION:
 *           -67 P1 P2 P3 ... */

/*     P1=001: BEGINNING THE PARSING OF A NEW CLAUSE.
 *     ------
 *         P2=  PTR TO FIRST SWORK OF THE CLAUSE (-8X FORMAT).
 *         P3=  PTR TO ANTECEDENT
 *         P4,P5,P6= WC,TYPE,FORM OF CLAUSE MARKER ELEMENT(REPLACES
 *                   THE CLAUSE ONCE IT IS MOVED).
 *              NOTE:  CAN BE IN -8X FORMAT.
 *         P7=  CONSTANT TARGET ADDRESS OF CLAUSE MARKER ELEMENT.
 *         P8,P9,P10= WC,TYPE,FORM OF BOS SWORK  TO PRECEDE CLAUSE
 *                   IN ITS NEW POSITION.
 *         SAVE CURRENT PARSE POSITION (I). CREATE SCON,ETC. FOR
 *         NEW CLAUSE MARKER ELEMENT. */

/*     P1=002:  END OF CLAUSE FOUND; RELOCATE IT.
 *     -------
 *         P2=  PTR TO LAST SWORK/ELEMENT IN CLAUSE (-8X FORMAT).
 *         P3,P4,P5= WC,TY,FM OF EOS ELEMENT TO BE PLACED AT END OF
 *                   RELOCATED CLAUSE. */

/*     P1=003:  RESET THE VALUES OF THE CLAUSE MARKER ELEMENT
 *     -------
 *         P2,P3,P4= WC,TYPE,FORM OF CLAUSE MARKER ELEMENT(REPLACES
 *                   THE CLAUSE ONCE IT IS MOVED).
 *              NOTE: P2-P4 CAN BE IN -8X FORMAT.
 *         P5=  CONSTANT TARGET ADDRESS OF CLAUSE MARKER ELEMENT.
 *         NOTE: 0,-1 CAUSE NO ACTION. DON'T CARE. DEFAULT USED. */

/*     IF THE NESTED-CLAUSE,STACK LIMIT IS REACHED THE STACK COUNTER
 *     WILL BE INCREMENTED AND DECREMENTED AS USUAL BUT NO NSWORKS WILL
 *     BE MOVED NOR DATA STORED.  NORMAL OPERATION RESUMES WHEN THE
 *     COUNT RETURNS BELOW THE LIMIT.  THIS WAY OVERLOAD CLAUSES REMAIN
 *     NESTED AND INTACT FOR RELOCATION IN LATER PARSE. */

/*     P1=005:  MARK THE RELATIVE PRONOUN FOR THE CURRENT CLAUSE.
 *     -------
 *        P2 =  -8X,  THE SWORK POINTER TO RELATIVE PRONOUN ELEMENT. */

/*     P1=006:  SAVE "SNAPSHOT" OF PARENT CELLS FOR THE CHILD CLAUSE.
 *     -------
 *        P2 =  1ST CELL IN RANGE TO CAPTURE
 *        P3 =  LAST CELL IN RANGE TO CAPTURE
 *        P4 =  SEARCH DIRECTION FOR LOCATING CHILD CLAUSES.
 *           -96 = SEARCH OPADRI TO THE RIGHT STARTING WITH PHRASE P5.
 *           -97 = SEARCH  "      "  "  LEFT    "       "    "" .
 *        P5 =  -8X,  POINTER TO FIRST ELEMENT OF THE SEARCH. */

/*     P1=044:  Load an element from a child clause into a selected vc
 *     ------   in the parent clause.
 *        P2=   Indicates target element in the parent clause.
 *                001 = antecedent
 *                002 = clause marker
 *        P3=   VC of the antecedent into which element P4 is loaded.
 *        P4=   Element in current clause to load (-8X), or a constant
 *              address to load.
 *        P5=   Function flag.
 *                000 = Overlay anything already loaded in target vc, and
 *                      Search the target phrase left to right for vc.
 *                -97 = Append to anything already loaded in target vc.
 *                      Search the target phrase left to right for vc.
 *                -90 = Overlay anything already loaded in target vc, and
 *                      Search the target phrase right to left for vc.
 *                -91 = Append to anything already loaded in target vc.
 *                      Search the target phrase right to left for vc. */


/*     P1=054:  SET SCON OF THE CLAUSE MARKER FOUND IN THE PARENT OF THE
 *     -------  CURRENT CLAUSE. */

/*              (FORMATTED LIKE THE -54 SWITCH)
 *        P2 =  NUMBER OF INPUT PAIRS FOLLOWING.
 *        P3 =  FUNCTION NUMBER:
 *              1 = SET CLAUSE MARKER SCON FROM A CELL
 *              2 = SET THE CLAUSE MARKER SCON WITH THE CONSTANT
 *                  VALUE INDICATED IN THE 2ND INPUT PARAMETER.
 *        P4 =  SCON TO BE SET
 *        P5 =  VALUE TO PLACE IN SCON P4.  EITHER A CELL OR A CONSTANT
 *              DEPENDING UPON P3 */

/*     P1=055:  SET CELLS WITH INFORMATION FROM THE PARENT CLAUSE.
 *     ------- */

/*              (FORMATTED LIKE THE -55 SWITCH)
 *        P2 =  NUMBER OF CELL TO BE SET.
 *        P3 =  VALUE TO BE PLACED IN CELL P2.  SEE P4.
 *        P4 =  FUNCTION NUMBER:
 *              1 = GET THE VALUE IN CELL P3 OF THE
 *                  PARENT CLAUSE.  PARENT CELLS (CLPCEL()) ARE
 *                  STORED BY THE -67006 SWITCH.
 *              2 = LOAD SCON P3 OF THE ANTECEDENT ELEMENT.
 *              3 = LOAD SCON P3 OF THE CLAUSE MARKER ELEMENT. */

/*   11/10/93 jal: Allow -67001 & 003 to set CLS MRKR address via a rel ptr.
 *   9/27/93 jal: allow -67001, -67002,-67003 (the clause move functions)
 *        in all TRANS.
 *   5/3/93 jal: make the -67044... switch. Load element into a vc of
 *        the antecedent. */

/**************************************************************** */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <fcrt.h>
#include "trans.h"
#include "logoslib.h"
#include "projexts.h"
#include <string.h>
#include "parsetrans_ext.h"
#include <jbctrl.h>



void /*FUNCTION*/ txsw67()
{
	static short int addr, anfind, chldct, chldpt, clmphr, clmptr, 
	  clmscn, fcn, flclan, func, incr, k3fmsv, k3p10, k3p2, k3p3, 
	  k3p4, k3p5, k3p6, k3p7, k3p8, k3p9, last, movct, n6jim, opct, 
	  oplast, opstep, opstop, opstrt, ovrflw, ovrlay, pairpt, phrlst, 
	  phrstr, reserv, scleft, scnpbg, scnpnd, scnpt, scpt[3], strt, 
	  swct, swstep, swstop, swstrt, targlf, targrt, targvc, tmpptr, 
	  tmpscn, type, val, vbpt, xx, yy, zaplen;
	static long int _d_m, _do0, _do1, _do10, _do11, _do2, _do3, _do4, 
	  _do5, _do6, _do7, _do8, _do9, _l0, movsiz, tempi4;
	static double _d_l;
	void  elemld();
	static char pgmnam[9] = "TXSW67  ";


	
	
	/*                            FIRST PARAMETER DETERMINES FUNCTION
	 *            1    2    3    4    5    6    7    8    9   10 */
	if( vbdataX_.k3p1 == 1 ){

		/*--------------------------  CLAUSE BEGIN FUNCTION */

		/*                            SET POINTER TO START OF NEXT SWITCH */
		vbdataX_.k3n = vbdataX_.k3 + 11;

		/*                            ERROR CHECKING FIRST */

		/*                            CHECK FOR CLAUSE LIMIT EXCEEDED */
		if( clsnfoX_.cltotl >= CLSMAX ){
			errvrsX_.err = 1;
			}
		else{
			/*                            CAN SCON HANDLE 3 NEW ELEMENTS? */
			if( prtscoX_.sct + 3 > SCONY ){
				/*                            DOES RESERVED AREA HELP? ALSO
				 *                            IF PASS1 OF 2 THEN SCTSAV NOT
				 *                            SCT IS TABLE COUNTER. */
				reserv = (elemctX_.origct + AD1MAX) - elemctX_.elemct;
				if( reserv < 0 )
					reserv = 0;
				/*          IF (PASSFL.EQ.1 .AND. PASSCT.EQ.1 .AND. PASSKP.EQ.0)THEN
				 *             SCLEFT = SCONY - SCTSAV
				 *          ELSE */
				scleft = SCONY - prtscoX_.sct;
				/*             ENDIF */
				if( scleft + reserv < 3 ){
					prtscoX_.scterr += 1;
					errvrsX_.err = 2;
					goto L_9000;
					}
				}
			/*                            CAN OPADRI HANDLE 3 NEW ELEMENTS? */
			if( opadriX_.opi + 3 > OPADRX ){
				prtscoX_.scterr += 1;
				errvrsX_.err = 3;
				/*                            CAN SCONO HANDLE 3 NEW ELEMENTS? */
				}
			else if( elemctX_.elemct + 3 > ELMMAX ){
				errvrsX_.err = 4;
				}
			else{
				/*                            IF CLMSTK IS FULL, WRITE MSG, SKIP SWITCH
				 *                            & SKIP ALL OTHER -67SW IN THIS VTR. */
				if( clsmovX_.clmcnt >= CLMSTK ){
					clsmovX_.clmcnt += 1;
					if( opswX_.sw[3-One] == 1 || opswX_.sw[14-One] == 1 ){
						fprintf( _spec_fp, "              ***** WARNING ****\n   -67001 SWITCH, TOO MANY NESTED CLAUSES IDENTIFIED.\n    STACK OVERFLOW WARNING. SKIP SWITCH. CONTINUE VTR.\n              ******************\n" );
						}
					else{
						errvrsX_.err = vtrfX_.vtrf[vbdataX_.k3+2-One];
						errlog(pgmnam,101,errvrsX_.err,5);
						}
					}
				else{
					/*                            GET ALL PARMS */
					k3p2 = vtrfX_.vtrf[vbdataX_.k3+2-One];
					k3p3 = vtrfX_.vtrf[vbdataX_.k3+3-One];
					k3p4 = vtrfX_.vtrf[vbdataX_.k3+4-One];
					k3p5 = vtrfX_.vtrf[vbdataX_.k3+5-One];
					k3p6 = vtrfX_.vtrf[vbdataX_.k3+6-One];
					k3p7 = vtrfX_.vtrf[vbdataX_.k3+7-One];
					k3p8 = vtrfX_.vtrf[vbdataX_.k3+8-One];
					k3p9 = vtrfX_.vtrf[vbdataX_.k3+9-One];
					k3p10 = vtrfX_.vtrf[vbdataX_.k3+10-One];

					/*--------------------        SET OR INITIALIZE AS MUCH AS POSSIBLE */

					/*                           ALLOCATE SCONS FOR 3 NEW ELEMENTS (CM,
					 *                           BOS,EOS), & UPDATE SCON COUNTER.
					 *                           USE RESERVED SCON AREA 1ST, THEN APPEND. */
					for( xx=1; xx <= 3; xx++ ){
						/*                           RESERVED AREA AVAILABLE */
						if( elemctX_.elemct + xx <= elemctX_.origct + 
						  AD1MAX ){
							scpt[xx-One] = elemctX_.elemct + xx;
							/*                           OTHERWISE MUST BE APPENDED */
							}
						else{
							/*C                           IF PASS1 OF 2, SET SCON BASED ON SAVED
							 *C                           VALUE OF SCON COUNT (SCTSAV) FROM TRAN1.
							 *              IF (PASSFL.EQ.1 .AND. PASSCT.EQ.1 .AND. PASSKP.EQ.0) THEN
							 *                  SCTSAV = SCTSAV + 1
							 *                  IF (SCTSAV.GT.SCT) SCT = SCTSAV
							 *                  SCPT(XX) = SCTSAV
							 *              ELSE */
							prtscoX_.sct += 1;
							scpt[xx-One] = prtscoX_.sct;
							/*              ENDIF */
							}
						}
					/*                            ALLOCATE SCONO SPACE */
					elemctX_.elemct += 3;
					/*                            ALLOCATE OPADRI SPACE */
					opadriX_.opi += 3;

					/*-------------------     ADD NEW ELEMENTS TO TARGET  ARRAYS */


					/*                            -------  OPADRI --------- */

					/*                            INSERT ADDR FOR CLAUSE MARKER */

					opadriX_.opadri[opadriX_.opi-2-One] = -140;
					if( k3p7 > 0 ){
						opadriX_.opadri[opadriX_.opi-2-One] = -k3p7;
						}
					else if( k3p7 < -70 ){
						/*                            set addr from another elmnt 11/10/93 jal */
						tmpptr = im81X_.im81 - k3p7;
						tmpscn = sworkX_.swork[tmpptr-One][4-One];
						for( xx=sworkX_.phrbeg[tmpptr-One]; xx <= sworkX_.phrend[tmpptr-One]; xx++ ){
							if( opadriX_.sconpi[xx-One] == tmpscn )
								goto L_10001;
							}
						goto L_10000;
L_10001:
						opadriX_.opadri[opadriX_.opi-2-One] = opadriX_.opadri[xx-One];
						}
					/*                            INSERT NULL BOS */
L_10000:
					opadriX_.opadri[opadriX_.opi-1-One] = -140;
					/*                            INSERT NULL EOS */
					opadriX_.opadri[opadriX_.opi-One] = -140;

					/*                            -------  SCONPI --------- */

					/*                            CLAUSE MARKER */
					opadriX_.sconpi[opadriX_.opi-2-One] = scpt[1-One];
					/*                            BOS */
					opadriX_.sconpi[opadriX_.opi-1-One] = scpt[2-One];
					/*                            EOS */
					opadriX_.sconpi[opadriX_.opi-One] = scpt[3-One];

					/*                            -------  HFDOPI --------- */

					/*                            INSERT NULL FOR CLAUSE MARKER */
					hpdopiX_.hfdopi[opadriX_.opi-2-One] = 0;
					/*                            INSERT NULL FOR BOS */
					hpdopiX_.hfdopi[opadriX_.opi-1-One] = 0;
					/*                            INSERT NULL FOR EOS */
					hpdopiX_.hfdopi[opadriX_.opi-One] = 0;
					/*                            SET SCON-SCONO LINK LIST ARRAYS */
					sconX_.scolnk[scpt[1-One]-One] = elemctX_.elemct - 2;
					sconX_.scolnk[scpt[2-One]-One] = elemctX_.elemct - 1;
					sconX_.scolnk[scpt[3-One]-One] = elemctX_.elemct;
					/*                            SET SCON TABLE FOR CM  WITH WC & TYPE */
					if( k3p4 > -70 ){
						sconX_.scon[scpt[1-One]-One][1-One] = k3p4;
						}
					else{
						sconX_.scon[scpt[1-One]-One][1-One] = sworkX_.swork[im81X_.im81-k3p4-One][1-One];
						}
					type = k3p5;
					if( k3p5 < -70 )
						type = sworkX_.swork[im81X_.im81-k3p5-One][2-One];
					if( type <= 16 ){
						sconX_.scon[scpt[1-One]-One][13-One] = type;
						}
					else if( type <= 99 ){
						sconX_.scon[scpt[1-One]-One][11-One] = type;
						}
					else{
						sconX_.scon[scpt[1-One]-One][2-One] = type;
						}
					/*                            SET SCON TABLE FOR BOS WITH WC & TYPE */
					if( k3p8 > -70 ){
						sconX_.scon[scpt[2-One]-One][1-One] = k3p8;
						}
					else{
						sconX_.scon[scpt[2-One]-One][1-One] = sworkX_.swork[im81X_.im81-k3p8-One][1-One];
						}
					type = k3p9;
					if( k3p9 < -70 )
						type = sworkX_.swork[im81X_.im81-k3p9-One][2-One];
					if( type <= 16 ){
						sconX_.scon[scpt[2-One]-One][13-One] = type;
						}
					else if( type <= 99 ){
						sconX_.scon[scpt[2-One]-One][11-One] = type;
						}
					else{
						sconX_.scon[scpt[2-One]-One][2-One] = type;
						}
					/*                            SET FORMSAVE FOR CM */
					if( k3p6 > -70 ){
						formsaX_.formsv[elemctX_.elemct-2-One] = k3p6;
						}
					else{
						formsaX_.formsv[elemctX_.elemct-2-One] = 
							formsaX_.formsv[sconX_.scolnk[sworkX_.swork[im81X_.im81-k3p6-One][4-One]-One]-One];
						}
					/*                            SET FORMSAVE FOR BOS */
					if( k3p10 > -70 ){
						formsaX_.formsv[elemctX_.elemct-1-One] = k3p10;
						}
					else{
						formsaX_.formsv[elemctX_.elemct-1-One] = formsaX_.formsv[sconX_.scolnk[sworkX_.swork[im81X_.im81-k3p10-One][4-One]-
						  One]-One];
						}
					/*                            SET HENUM, HASHCODE OF CM,EOS,BOS TO
					 *                            IMPOSSIBLE MATCH AS DEFAULT. */
					hensavX_.henum2[elemctX_.elemct-2-One][1-One] = -1;
					hashX_.hashcd[elemctX_.elemct-2-One][1-One] = 0;
					hensavX_.henum2[elemctX_.elemct-1-One][1-One] = -1;
					hashX_.hashcd[elemctX_.elemct-1-One][1-One] = 0;
					hensavX_.henum2[elemctX_.elemct-One][1-One] = -1;
					hashX_.hashcd[elemctX_.elemct-One][1-One] = 0;
					/*                            SET NWSTRNG FOR EACH CREATED W */
					memcpy(sent_wordsX_.source_word[elemctX_.elemct-2-One],"* CLS-MRKR *    ",16); 
					memcpy(sent_wordsX_.source_word[elemctX_.elemct-1-One],"* CLS-BOS *     ",16); 
					memcpy(sent_wordsX_.source_word[elemctX_.elemct-One],  "* CLS-EOS *     ",16); 


					/*--------------------        SET OR INITIALIZE ALL CLSNFO ARRAYS
					 *                                                  ------- */

					/*                            ASSIGN CLAUSE ID */
					clsnfoX_.cltotl += 1;
					/*                            SET /CLSNFO/ WITH ID OF PARENT CLAUSE */
					clsnfoX_.clprnt[clsnfoX_.cltotl-One] = clsnfoX_.clcrnt;
					/*                            CURRENT CLAUSE IS NOW THE CHILD CLAUSE */
					clsnfoX_.clcrnt = clsnfoX_.cltotl;
					/*                            SET CLAUSE BOUNDARIES.   ASSUME THE
					 *                            CHILD CLAUSE EXTENDS TO END OF PARENT
					 *                            CLAUSE. 67002 WILL SET REAL END SWORK,
					 *                            AND CHECK FOR EOS AT END OF PRNT & CHILD. */
					clsnfoX_.clbgns[clsnfoX_.clcrnt-One] = im81X_.im81 - k3p2;
					clsnfoX_.clndns[clsnfoX_.clcrnt-One] = 
						clsnfoX_.clndns[clsnfoX_.clprnt[clsnfoX_.clcrnt-One]-One];
					/*                            ANTECEDENT PTR
					 *                            ANT PTR = 0 MEANS NO ANTECEDENT.
					 *                            SET ANTECEDENT SCON TARGET BOUNDARY PTRS */
					if( k3p3 > -70 ){
						clsnfoX_.clansc[clsnfoX_.clcrnt-One] = 0;
						clsnfoX_.clansw[clsnfoX_.clcrnt-One] = 0;
						clsnfoX_.clanbg[clsnfoX_.clcrnt-One] = 0;
						clsnfoX_.clannd[clsnfoX_.clcrnt-One] = 0;
						}
					else{
						clsnfoX_.clansc[clsnfoX_.clcrnt-One] = sworkX_.swork[im81X_.im81-k3p3-One][4-One];
						clsnfoX_.clansw[clsnfoX_.clcrnt-One] = im81X_.im81 - k3p3;
						clsnfoX_.clanbg[clsnfoX_.clcrnt-One] = opadriX_.sconpi[sworkX_.phrbeg[im81X_.im81-k3p3-One]-
						  One];
						clsnfoX_.clannd[clsnfoX_.clcrnt-One] = opadriX_.sconpi[sworkX_.phrend[im81X_.im81-k3p3-One]-
						  One];
						}
					/*                            CLMRKR = SCON POINTER OF CLAUSE MARKER */
					clsnfoX_.clmrkr[clsnfoX_.clcrnt-One] = scpt[1-One];
					/*                            INIT SAVE SPACE FOR THIS CLAUSES CELLS. */
					memset(clsnfoX_.clcell[clsnfoX_.clcrnt-One],'\0',sizeof(clsnfoX_.clcell[0]));
					/*                            INIT SAVE SPACE FOR PARENT CELLS */
					memset(clsnfoX_.clpcel[clsnfoX_.clcrnt-One],'\0',sizeof(clsnfoX_.clpcel[0]));
					/*                            RELATIVE PRONOUN SCON POINTER */
					clsnfoX_.clrelp[clsnfoX_.clcrnt-One] = 0;

					/*--------------------        SET AND INITIALIZE CLSCON ARRAYS.
					 *                                               ------
					 *                            CLAUSE ID FOR: CLAUSE MARKER */
					clsconX_.clsid[elemctX_.elemct-2-One] = clsnfoX_.clprnt[clsnfoX_.clcrnt-One];
					/*                                           BOS */
					clsconX_.clsid[elemctX_.elemct-1-One] = clsnfoX_.clcrnt;
					/*                                           EOS */
					clsconX_.clsid[elemctX_.elemct-One] = clsnfoX_.clcrnt;
					/*                             CHILD PTR FROM THE CM ELEMENT */
					clsconX_.cmchld[elemctX_.elemct-2-One] = clsnfoX_.clcrnt;
					/*                             CHILD PTR FROM THE ANTECEDENT ELEMENT */
					if( k3p3 < -70 )
						clsconX_.achild[sconX_.scolnk[sworkX_.swork[im81X_.im81-k3p3-One][4-One]-
						  One]-One] = clsnfoX_.clcrnt;

					/*--------------------        SET AND INITIALIZE CLMOV ARRAYS.
					 *                                               ------ */
					clsmovX_.clmcnt += 1;
					clsmovX_.clmid[clsmovX_.clmcnt-One] = clsnfoX_.clcrnt;
					if( vtrs42X_.sw42n == 0 ){
						clsmovX_.clmpos[clsmovX_.clmcnt-One] = vwarg2X_.i;
						}
					else{
						clsmovX_.clmpos[clsmovX_.clmcnt-One] = vwarg1X_.isave;
						}
					/*                            SAVE THE BOS,EOS SCON POINTERS
					 *                            CLAUSE MARKER SCON ALREADY IN CLMRKR */
					clsmovX_.clmscb[clsmovX_.clmcnt-One] = scpt[2-One];
					clsmovX_.clmsce[clsmovX_.clmcnt-One] = scpt[3-One];
					/*                            SAVE PTR TO CM ELEMENT IN SCONO */
					clsmovX_.clmelc[clsmovX_.clmcnt-One] = elemctX_.elemct - 2;
					/*                            SAVE PTR TO CM ADDRESS IN OPADRI */
					clsmovX_.clmopc[clsmovX_.clmcnt-One] = opadriX_.opi - 2;
					/*                            IF THIS IS THE FIRST CHILD CLAUSE THEN
					 *                            MARK THE PARENT STILL IN MOVE STACK SO
					 *                            FINAL LEFT-RIGHT ORDER IS MAINTAINED. */
					if( clsmovX_.clmcnt > 1 &&
						clsmovX_.clmord[clsmovX_.clmcnt-1-One] == 0 )
						clsmovX_.clmord[clsmovX_.clmcnt-1-One] = clsnfoX_.clcrnt;
					clsmovX_.clmord[clsmovX_.clmcnt-One] = 0;
					/*                            CLAUSE MARKER WC */
					if( k3p4 > -70 ){
						clsmovX_.clmwtf[clsmovX_.clmcnt-One][1-One] = k3p4;
						}
					else{
						clsmovX_.clmwtf[clsmovX_.clmcnt-One][1-One] = sworkX_.swork[im81X_.im81-k3p4-One][1-One];
						}
					/*                            TYPE */
					if( k3p5 > -70 ){
						clsmovX_.clmwtf[clsmovX_.clmcnt-One][2-One] = k3p5;
						}
					else{
						clsmovX_.clmwtf[clsmovX_.clmcnt-One][2-One] = sworkX_.swork[im81X_.im81-k3p5-One][2-One];
						}
					/*                            FORM */
					if( k3p6 > -70 ){
						clsmovX_.clmwtf[clsmovX_.clmcnt-One][3-One] = k3p6;
						}
					else{
						clsmovX_.clmwtf[clsmovX_.clmcnt-One][3-One] = sworkX_.swork[im81X_.im81-k3p6-One][3-One];
						}
					/*                            BOS WC */
					if( k3p8 > -70 ){
						clsmovX_.clmbos[clsmovX_.clmcnt-One][1-One] = k3p8;
						}
					else{
						clsmovX_.clmbos[clsmovX_.clmcnt-One][1-One] = sworkX_.swork[im81X_.im81-k3p8-One][1-One];
						}
					/*                            BOS TYPE */
					if( k3p9 > -70 ){
						clsmovX_.clmbos[clsmovX_.clmcnt-One][2-One] = k3p9;
						}
					else{
						clsmovX_.clmbos[clsmovX_.clmcnt-One][2-One] = sworkX_.swork[im81X_.im81-k3p9-One][2-One];
						}
					/*                            BOS FORM */
					if( k3p10 > -70 ){
						clsmovX_.clmbos[clsmovX_.clmcnt-One][3-One] = k3p10;
						}
					else{
						clsmovX_.clmbos[clsmovX_.clmcnt-One][3-One] = sworkX_.swork[im81X_.im81-k3p10-One][3-One];
						}
					/*                            CHECK THAT POSITION WHERE NEXT RULE
					 *                            STARTS(AS DEFINED BY -41SW) IS NOT TO
					 *                            LEFT OF CLAUSE'S 1ST ELEMENT. */

					if( w50valX_.i3 < clsnfoX_.clbgns[clsnfoX_.clcrnt-One] ){
						w50valX_.i3 = clsnfoX_.clbgns[clsnfoX_.clcrnt-One];
						if( diagsX_.longdi == 1 ){
							fprintf( _spec_fp, "              ***** WARNING ****\n   ATTEMPTED BACKSPACE TO LEFT OF ELEMENT DEFINED AS \n   BEGINNING OF CLAUSE BY -67001 SWITCH. FORCING\n   NEXT RULE TO START AT BEGINNING OF CLAUSE.   \n              ******************\n" );
							}
						else{
							errvrsX_.err = k3p2;
							errlog(pgmnam,122,errvrsX_.err,5);
							}
						}
					/*                            DIAGNOSTICS */
					if( diagsX_.longdi == 1 ){
						fprintf( _spec_fp, " SW67 001:   SET BEGINNING OF A CLAUSE\n          ----  AFTER PROCESSING THE 67 001 SWITCH ----\n" );
						diagno(11);
						}
					}
				goto L_9999;
				}
			}
		}
	else if( vbdataX_.k3p1 == 2 ){

		/*--------------------------  CLAUSE END AND RELOCATE FUNCTION */

		/*                            SET POINTER TO NEXT SWITCH */
		vbdataX_.k3n = vbdataX_.k3 + 6;

		 /*                            IF STACK OVERLOADED DECREMENT COUNTER
		 *                            AND RETURN. */
		if( clsmovX_.clmcnt > CLMSTK ){
			clsmovX_.clmcnt -= 1;
			goto L_9999;
			/*                            IF NO CORRESPONDING -67001 ALREADY. */
			}
		else if( clsmovX_.clmcnt < 1 ){
			errvrsX_.err = 5;
			}
		else{
			/*                            GET INPUT PARAMETERS */
			k3p2 = vtrfX_.vtrf[vbdataX_.k3+2-One];
			k3p3 = vtrfX_.vtrf[vbdataX_.k3+3-One];
			k3p4 = vtrfX_.vtrf[vbdataX_.k3+4-One];
			k3p5 = vtrfX_.vtrf[vbdataX_.k3+5-One];

			if( k3p3 < -70 )
				k3p3 = sworkX_.swork[im81X_.im81-k3p3-One][1-One];
			if( k3p4 < -70 )
				k3p4 = sworkX_.swork[im81X_.im81-k3p4-One][2-One];
			k3fmsv = k3p5;
			if( k3p5 < -70 ){
				k3p5 = sworkX_.swork[im81X_.im81-k3p5-One][3-One];
				k3fmsv = formsaX_.formsv[sconX_.scolnk[sworkX_.swork[im81X_.im81-k3p5-One][4-One]-
				  One]-One];
				}
			clsnfoX_.clndns[clsnfoX_.clcrnt-One] = im81X_.im81 - k3p2;
			/*                            RELOCATE THE DEMARKED CLAUSE */
			if( diagsX_.longdi == 1){
				fprintf( _spec_fp, " SW67 002:   MOVE A CLAUSE.\n\n     *********  BEFORE MOVING THE CLAUSE *********\n\n" );
				diagno(10);
				diagno(9);
				}
				/*                            RELOCATE */
			clmove(k3p3,k3p4,k3p5,k3fmsv);

			if( diagsX_.longdi == 1){
				fprintf( _spec_fp, "\n     *********  AFTER MOVING THE CLAUSE ********* \n\n" );
				diagno(10);
				diagno(9);
				}

			goto L_9999;
			}
		}
	else{
		if( vbdataX_.k3p1 == 3 ){

			/*--------------------------  SET CLAUSE MARKER SWORK */

			/*                            SET POINTER TO NEXT SWITCH */
			vbdataX_.k3n = vbdataX_.k3 + 6;

			if( clsnfoX_.clcrnt != clsmovX_.clmid[clsmovX_.clmcnt-One] ){
				if( diagsX_.anydi == 1 )
					{
					fprintf( _spec_fp, "\n*************  ERROR -  T2SW67 003  ****************\n  CLMID IN MOVE ARRAY NOT EQUAL TO CURRENT CLAUSE.  \n  CLCRNT,CLMID(CLMCNT),CLMCNT = %4d %4d %4d\n  CONTINUING.  RESULTS UNPREDICTABLE ???? \n****************************************************\n", 
					  clsnfoX_.clcrnt, clsmovX_.clmid[clsmovX_.clmcnt-One], 
					  clsmovX_.clmcnt );
					}
				errlog(pgmnam,3001,0,10);
				}

			/*                            IF STACK OVERLOADED FOR THIS VTR RETURN */
			if( clsmovX_.clmcnt <= CLMSTK ){
				/*                            GET INPUT PARAMETERS */
				k3p2 = vtrfX_.vtrf[vbdataX_.k3+2-One];
				k3p3 = vtrfX_.vtrf[vbdataX_.k3+3-One];
				k3p4 = vtrfX_.vtrf[vbdataX_.k3+4-One];
				k3p5 = vtrfX_.vtrf[vbdataX_.k3+5-One];

				/*                            CLAUSE MARKER WC */
				if( k3p2 > 0 || k3p2 < -1 ){
					if( k3p2 > -70 ){
						clsmovX_.clmwtf[clsmovX_.clmcnt-One][1-One] = k3p2;
						}
					else{
						clsmovX_.clmwtf[clsmovX_.clmcnt-One][1-One] = sworkX_.swork[im81X_.im81-k3p2-One][1-One];
						}
					}
				/*                            CLAUSE MARKER TYPE */
				if( k3p3 > 0 || k3p3 < -1 ){
					if( k3p3 > -70 ){
						clsmovX_.clmwtf[clsmovX_.clmcnt-One][2-One] = k3p3;
						}
					else{
						clsmovX_.clmwtf[clsmovX_.clmcnt-One][2-One] = sworkX_.swork[im81X_.im81-k3p3-One][2-One];
						}
					}
				/*                            CLAUSE MARKER FORM */
				if( k3p4 > 0 || k3p4 < -1 ){
					if( k3p4 > -70 ){
						clsmovX_.clmwtf[clsmovX_.clmcnt-One][3-One] = k3p4;
						}
					else{
						clsmovX_.clmwtf[clsmovX_.clmcnt-One][3-One] = sworkX_.swork[im81X_.im81-k3p4-One][3-One];
						}
					}

				/*                            SET SCON TABLE FOR CM  WITH WC & TYPE */
				sconX_.scon[clsnfoX_.clmrkr[clsnfoX_.clcrnt-One]-One][1-
				  One] = clsmovX_.clmwtf[clsmovX_.clmcnt-One][1-One];
				type = clsmovX_.clmwtf[clsmovX_.clmcnt-One][2-One];
				if( type <= 16 ){
					sconX_.scon[clsnfoX_.clmrkr[clsnfoX_.clcrnt-One]-
					  One][13-One] = type;
					}
				else if( type <= 99 ){
					sconX_.scon[clsnfoX_.clmrkr[clsnfoX_.clcrnt-One]-
					  One][11-One] = type;
					}
				else{
					sconX_.scon[clsnfoX_.clmrkr[clsnfoX_.clcrnt-One]-
					  One][2-One] = type;
					}
				/*                            SET FORMSAVE FOR CM */
				formsaX_.formsv[clsmovX_.clmelc[clsmovX_.clmcnt-One]-
				  One] = clsmovX_.clmwtf[clsmovX_.clmcnt-One][3-One];

				/*                            CHANGE TARGET ADDRESS IF REQUESTED */
				if( k3p5 > 0 ){
					opadriX_.opadri[clsmovX_.clmopc[clsmovX_.clmcnt-One]-
					  One] = -k3p5;
					}
				else if( k3p5 < -70 ){
					/*                            set addr from another elmnt 11/10/93 jal */
					tmpptr = im81X_.im81 - k3p5;
					tmpscn = sworkX_.swork[tmpptr-One][4-One];
					for( xx=sworkX_.phrbeg[tmpptr-One]; xx <= sworkX_.phrend[tmpptr-One]; xx++ ){
						if( opadriX_.sconpi[xx-One] == tmpscn )
							goto L_10002;
						}
					goto L_313;
L_10002:
					opadriX_.opadri[clsmovX_.clmopc[clsmovX_.clmcnt-One]-One]
						   = opadriX_.opadri[xx-One];
L_313:
					;

					}
				}
			}
		else if( !(((vbdataX_.k3p1 == 4 || (vbdataX_.k3p1 >= 7 && 
		             vbdataX_.k3p1 <= 43)) || 
		            (vbdataX_.k3p1 >= 45 && vbdataX_.k3p1 <= 53)) ||
					(vbdataX_.k3p1 >= 56 && vbdataX_.k3p1 <= 60)) ){
			if( vbdataX_.k3p1 == 5 ){


				/*--------------------------  MARK RELATIVE PRONOUN FOR CURR CLAUSE */

				/*                            SET POINTER TO START OF NEXT SWITCH */
				vbdataX_.k3n = vbdataX_.k3 + 3;

				k3p2 = vtrfX_.vtrf[vbdataX_.k3+2-One];
				/*                            LOAD SCON POINTER OF INDICATED ELEMENT */
				if( k3p2 < -70 )
					clsnfoX_.clrelp[clsnfoX_.clcrnt-One] = sworkX_.swork[im81X_.im81-k3p2-One][4-One];
				}
			else{
				if( vbdataX_.k3p1 == 6 ){

					/*--------------------------  SEND CELL INFO FROM PARENT TO CHILD */

					/*                            SET POINTER TO START OF NEXT SWITCH */
					vbdataX_.k3n = vbdataX_.k3 + 6;

					k3p2 = vtrfX_.vtrf[vbdataX_.k3+2-One];
					k3p3 = vtrfX_.vtrf[vbdataX_.k3+3-One];
					k3p4 = vtrfX_.vtrf[vbdataX_.k3+4-One];
					k3p5 = vtrfX_.vtrf[vbdataX_.k3+5-One];
					/*                            IF RANGES ARE ZERO THEN JUST SET THE
					 *                            SEARCH BOUNDARIES FOR THE NEXT SEARCH. */
					if( k3p2 == 0 || k3p3 == 0 ){
						if( k3p4 == -96 ){
							clsnopX_.cl676r[clsnfoX_.clcrnt-One] = im81X_.im81 - 
							  k3p5;
							}
						else if( k3p4 == -97 ){
							clsnopX_.cl676l[clsnfoX_.clcrnt-One] = im81X_.im81 - 
							  k3p5;
							}
						goto L_9999;
						/*                             CHECK RANGES */
						}
					else if( (((k3p2 < 1 || k3p2 > 100) || k3p3 < 1) ||
						        k3p3 > 100) || k3p2 > k3p3 ){
						errvrsX_.err = 10;
						}
					else{
						/*                             TO SPEED UP SEARCH: CHECK HOW MANY
						 *                             CHILDREN CLAUSES IN THIS PARENT */
						chldct = 0;
						for( xx=2; xx <= clsnfoX_.cltotl; xx++ ){
							if( clsnfoX_.clprnt[xx-One] == clsnfoX_.clcrnt )
								chldct += 1;
							}
						if( chldct != 0 ){
							/*                             GET SEARCH BOUNDARIES */
							swstrt = im81X_.im81 - k3p5;
							if( k3p4 == -96 ){
								swstop = clsnfoX_.clndns[clsnfoX_.clcrnt-One];
								if( clsnopX_.cl676r[clsnfoX_.clcrnt-One] <= 
								  swstop )
									swstop = clsnopX_.cl676r[clsnfoX_.clcrnt-One];
								if( swstrt > swstop ){
									errvrsX_.err = 12;
									goto L_9500;
									}
								else{
									swstep = 1;
									}
								}
							else if( k3p4 == -97 ){
								swstop = clsnfoX_.clbgns[clsnfoX_.clcrnt-One];
								if( clsnopX_.cl676l[clsnfoX_.clcrnt-One] >= 
								  swstop )
									swstop = clsnopX_.cl676l[clsnfoX_.clcrnt-One];
								if( swstrt < swstop ){
									errvrsX_.err = 12;
									goto L_9500;
									}
								else{
									swstep = -1;
									}
								}
							else{
								errvrsX_.err = 11;
								goto L_9500;
								}
							movsiz = (k3p3 - k3p2 + 1)*2;
							movct = 0;

							if( diagsX_.longdi == 1 ){
								fprintf( _spec_fp, "SW67 006: CELLBEG,CELLEND,SWSTRT,SWSTOP,CL676R,CL676L,MOVSIZ,CHLDCT = %4d  %4d  %4d  %4d  %4d  %4d  %4ld  %4d  \n", 
								  k3p2, k3p3, swstrt, swstop, clsnopX_.cl676r[clsnfoX_.clcrnt-One], 
								  clsnopX_.cl676l[clsnfoX_.clcrnt-One], 
								  movsiz, chldct );
								/*                      PRINT THE CURRENT CELL ARRAY */
								diagno(16);
								}
							for( swct=swstrt, _do0=DOCNT(swct,swstop,_do1 = swstep); _do0 > 0 ; swct += _do1, _do0-- ){
								if( k3p3 == -97 ){
									opstrt = sworkX_.phrend[swct-One];
									opstop = sworkX_.phrbeg[swct-One];
									opstep = -1;
									}
								else{
									opstrt = sworkX_.phrbeg[swct-One];
									opstop = sworkX_.phrend[swct-One];
									opstep = 1;
									}
								for( opct=opstrt, _do2=DOCNT(opct,opstop,_do3 = opstep); _do2 > 0 ; opct += _do3, _do2-- ){
									scnpt = opadriX_.sconpi[opct-One];
									if( sconX_.scolnk[scnpt-One] != 0 ){
										chldpt = clsconX_.cmchld[sconX_.scolnk[scnpt-One]-
										  One];
										if( chldpt != 0 && chldpt <= clsnfoX_.cltotl ){
											lmove(&clsnfoX_.clpcel[chldpt-One][k3p2-One],
											  1,&vbdataX_.vbcell[k3p2-One],
											  1,movsiz);
											if( diagsX_.longdi == 1 || diagsX_.deepdi == 1 )
												{
												fprintf( _spec_fp, 
												  "       CELLS SENT TO CLAUSE# %2d  OPCT= %3d\n", 
												  chldpt, opct );
												}
											movct += 1;
											if( movct >= chldct )
												goto L_690;
											}
										}
									}
								}
							}
L_690:
						if( (diagsX_.longdi == 1) && movct > 0 )
							diagno(17);
						/*                            SAVE THE SEARCH START POS AS STOPPER
						 *                            POS FOR THE NEXT CALL. */
						if( k3p4 == -96 ){
							clsnopX_.cl676r[clsnfoX_.clcrnt-One] = 999;
							}
						else{
							clsnopX_.cl676l[clsnfoX_.clcrnt-One] = swstrt + 1;
							}
						goto L_9999;
						}
					}
				else if( vbdataX_.k3p1 == 44 ){

					/*----------- -67 044 ------  load an phrase into a vc in the parent clause */

					/*                            SET POINTER TO START OF NEXT SWITCH */
					vbdataX_.k3n = vbdataX_.k3 + 6;
					/*                               ERROR TO LOAD WHILE PARSE IS IN
					 *                               "CLAUSE RELOCATION STATE" */
					if( tranidX_.tranid == 2 && clsmovX_.clmcnt != 0 ){
						/*                   SEND ERROR MESSAGE */
						if( diagsX_.deepdi == 1 || diagsX_.longdi == 1 ){
							fprintf( _spec_fp, "              ***** ERROR ******\n   -67 044 CALLED WHILE PARSING A CLAUSE FOR RELOCATION\n              ******************\n" );
							}
						else{
							zaplen = clsmovX_.clmcnt;
							errlog(pgmnam,4400,zaplen,9);
							}
						}

					k3p2 = vtrfX_.vtrf[vbdataX_.k3+2-One];
					k3p3 = vtrfX_.vtrf[vbdataX_.k3+3-One];
					k3p4 = vtrfX_.vtrf[vbdataX_.k3+4-One];
					k3p5 = vtrfX_.vtrf[vbdataX_.k3+5-One];

					/*                           locate the target vc */
					targvc = 0;
					/*                           K3P2 = 1= target is the antecedent */
					if( k3p2 != 1 ){
						/*                        see if clmrkr is still unconcatenated */
						clmscn = clsnfoX_.clmrkr[clsnfoX_.clcrnt-One];
						clmphr = 0;
						for( xx=clsoutX_.clbgno[clsnfoX_.clprnt[clsnfoX_.clcrnt-One]-One];
						     xx <= clsoutX_.clndno[clsnfoX_.clprnt[clsnfoX_.clcrnt-One]-One];
							 xx++ ){
							if( sworkoX_.phrhdo[xx-One] == clmscn )
								goto L_10009;
							}
						goto L_10008;
L_10009:
						clmphr = xx;
						/*                        CLMRKR phrase unconcatenated: find target vc */
L_10008:
						if( clmphr > 0 ){
							if( k3p5 == 0 || k3p5 == -97 ){
								phrstr = sworkoX_.phrbgo[clmphr-One];
								phrlst = sworkoX_.phrndo[clmphr-One];
								incr = 1;
								}
							else{
								phrlst = sworkoX_.phrbgo[clmphr-One];
								phrstr = sworkoX_.phrndo[clmphr-One];
								incr = -1;
								}
							for( xx=phrstr, _do4=DOCNT(xx,phrlst,_do5 = incr); _do4 > 0 ; xx += _do5, _do4-- ){
								if( opadroX_.opadro[xx-One] == -k3p3 )
									goto L_10010;
								}
							/*                       error:couldnt find target vc */
							errvrsX_.err = 4403;
							goto L_9500;
L_10010:
							targvc = xx;
							}
						else{
							for( xx=clsoutX_.clbgno[clsnfoX_.clprnt[clsnfoX_.clcrnt-One]-
							  One]; xx <= clsoutX_.clndno[clsnfoX_.clprnt[clsnfoX_.clcrnt-One]-
							  One]; xx++ ){
								for( yy=sworkoX_.phrbgo[xx-One];
								     yy <= sworkoX_.phrndo[xx-One];
									 yy++ ){
									if( opadroX_.sconpo[yy-One] == clmscn )
										goto L_4425;
									}
								}
							/*                       error: couldnt find clmarker */
							errvrsX_.err = 4404;
							goto L_9500;
L_4425:
							;
							/*                       got the clmrkr and its current phrase. Now
							 *                       locate the target vc.  To maximize chances of
							 *                       finding correct vc,search outward from CLMRKR
							 *                       and take the closest one. */
							phrstr = sworkoX_.phrbgo[xx-One];
							phrlst = sworkoX_.phrndo[xx-One];
							clmptr = yy;
							/*                             search left */
							targlf = 0;
							for( xx=clmptr; xx >= phrstr; xx-- ){
								if( opadroX_.opadro[xx-One] == -k3p3 )
									goto L_10011;
								}
							goto L_4427;
L_10011:
							targlf = xx;
							/*                             search right */
L_4427:
							;
							for( xx=clmptr; xx <= phrlst; xx++ ){
								if( opadroX_.opadro[xx-One] == -k3p3 ){
									if( targvc == 0 || xx - clmptr < 
									  clmptr - targvc )
										goto L_10012;
									}
								}
							goto L_4429;
L_10012:
							targrt = xx;
							/*                        take the vc closest to Clause marker */
L_4429:
							;
							/*                        error if couldnt find target vc */
							if( targlf == 0 && targrt == 0 ){
								errvrsX_.err = 4405;
								goto L_9500;
								}
							else if( targrt == 0 ){
								targvc = targlf;
								}
							else if( targlf == 0 ){
								targvc = targrt;
								}
							else{
								targvc = targrt;
								if( clmptr - targlf < targrt - clmptr )
									targvc = targlf;
								}
							}
						/*                           antecedent still not concatenated */
						}
					else if( clsnfoX_.clansw[clsnfoX_.clcrnt-One] ==0 ){

						/*                        K3P2 =2=target is the clause marker */

						anfind = 0;
						phrlst = 0;
						if( k3p5 == 0 || k3p5 == -97 ){
							scnpbg = clsnfoX_.clanbg[clsnfoX_.clcrnt-One];
							scnpnd = clsnfoX_.clannd[clsnfoX_.clcrnt-One];
							opstrt = sworkoX_.phrbgo[clsoutX_.clbgno[clsnfoX_.clprnt[clsnfoX_.clcrnt-One]-
							  One]-One];
							oplast = sworkoX_.phrndo[clsoutX_.clndno[clsnfoX_.clprnt[clsnfoX_.clcrnt-One]-
							  One]-One];
							incr = 1;
							}
						else{
							scnpnd = clsnfoX_.clanbg[clsnfoX_.clcrnt-One];
							scnpbg = clsnfoX_.clannd[clsnfoX_.clcrnt-One];
							oplast = sworkoX_.phrbgo[clsoutX_.clbgno[clsnfoX_.clprnt[clsnfoX_.clcrnt-One]-
							  One]-One];
							opstrt = sworkoX_.phrndo[clsoutX_.clndno[clsnfoX_.clprnt[clsnfoX_.clcrnt-One]-
							  One]-One];
							incr = -1;
							}
						for( xx=opstrt, _do6=DOCNT(xx,oplast,_do7 = incr); _do6 > 0 ; xx += _do7, _do6-- ){
							if( opadroX_.sconpo[xx-One] == scnpbg )
								goto L_10006;
							}
						goto L_10005;
L_10006:
						for( yy=xx, _do8=DOCNT(yy,oplast,_do9 = incr); _do8 > 0 ; yy += _do9, _do8-- ){
							if( opadroX_.opadro[yy-One] == -k3p3 )
								targvc = yy;
							if( opadroX_.sconpo[yy-One] == clsnfoX_.clansc[clsnfoX_.clcrnt-One] )
								anfind = yy;
							if( opadroX_.sconpo[yy-One] == scnpnd )
								goto L_10007;
							}
						goto L_10005;
L_10007:
						phrlst = yy;
						/*                          error:couldnt find the vc or antecedent */
L_10005:
						if( targvc == 0 ){
							errvrsX_.err = 4401;
							goto L_9500;
							}
						else if( anfind == 0 ){
							errvrsX_.err = 4402;
							goto L_9500;
							}
						else{
							if( phrlst == 0 ){
								if( diagsX_.deepdi == 1 || diagsX_.longdi == 1 ){
									fprintf( _spec_fp, "\nWARNING: couldnt find end of the antecedentphrase in the opadr.\nVC may be incorrect. ERROR= %5ld %5d %5d %5d %5d \n", 
									  errvrsX_.err, scnpbg, scnpnd, 
									  opstrt, oplast );
									errlog(pgmnam,4412,4412,9);
									}
								}
							/*                          flag if targvc is CLANBG or CLANND. This SCONPO
							 *                          value will change and CLANBG or CLANND must
							 *                          be updated at the end. */
							flclan = 0;
							if( opadroX_.sconpo[targvc-One] == clsnfoX_.clanbg[clsnfoX_.clcrnt-One] ){
								flclan = 1;
								}
							else if( opadroX_.sconpo[targvc-One] == 
							  clsnfoX_.clannd[clsnfoX_.clcrnt-One] ){
								flclan = 2;
								}
							}
						}
					else{
						phrstr = sworkoX_.phrbgo[clsnfoX_.clansw[clsnfoX_.clcrnt-One]-One];
						phrlst = sworkoX_.phrndo[clsnfoX_.clansw[clsnfoX_.clcrnt-One]-One];
						if( k3p5 == 0 || k3p5 == -97 ){
							strt = phrstr;
							last = phrlst;
							incr = 1;
							}
						else{
							strt = phrlst;
							last = phrstr;
							incr = -1;
							}
						for( xx=strt, _do10=DOCNT(xx,last,_do11 = incr); _do10 > 0 ; xx += _do11, _do10-- ){
							/*                           antecedent is already concatenated. find it in
							 *                           OPADRO using SCONPO values. */
							if( opadroX_.opadro[xx-One] == -k3p3 )
								goto L_10004;
							}
						goto L_10003;
L_10004:
						targvc = xx;
						}
					/*                        ERROR if couldnt find the target VC */
L_10003:
					if( targvc == 0 ){
						errvrsX_.err = 4406;
						}
					else{

						/*                        FILL THE TARGET VC
						 *                            At this point we have:
						 *                            TARGVC=OPADRO position of target vc
						 *                            K3P4=Rel ptr to SWORK phrase to load, or
						 *                                 constant address.
						 *                            K3P5=Overlay/append option. */

						/*                         set overlay or append flag. */
						ovrlay = 1;
						if( k3p5 == -97 || k3p5 == -91 )
							ovrlay = 0;

						/*                         set source phrase ptrs */
						if( k3p4 < -70 && k3p4 > -90 ){
							n6jim = im81X_.im81 - k3p4;
							phrstr = sworkX_.phrbeg[n6jim-One];
							phrlst = sworkX_.phrend[n6jim-One];
							func = 1;
							/*                         create a new constant */
							}
						else if( prtscoX_.sct >= SCONY ){
							prtscoX_.scterr += 1;
							errvrsX_.err = 4420;
							goto L_9500;
							}
						else{
							prtscoX_.sct += 1;
							addr = k3p4;
							if( flag32X_.ad32 != -1 ){
								addr = k3p4 + 1000*flag32X_.ad32;
								flag32X_.ad32 = -1;
								}
							sconX_.scon[prtscoX_.sct-One][1-One] = 21;
							if( addr > 120 && addr < 131 )
								sconX_.scon[prtscoX_.sct-One][1-One] = 20;
							sconX_.scon[prtscoX_.sct-One][2-One] = addr;

							phrstr = addr;
							phrlst = prtscoX_.sct;
							func = 2;
							}

						ovrflw = 1;

						vcfill(func,targvc,phrstr,phrlst,&ovrlay,ovrflw,&errvrsX_.err);
						if( errvrsX_.err == 1 ){
							errvrsX_.err = 4408;
							}
						else if( errvrsX_.err == 2 ){
							errvrsX_.err = 4407;
							}
						else{
							/*                         reset CLANBG,CLANND if they have changed. */
							if( flclan > 0 ){
								if( flclan == 1 ){
									clsnfoX_.clanbg[clsnfoX_.clcrnt-One] = opadroX_.sconpo[targvc-One];
									}
								else if( flclan == 2 ){
									clsnfoX_.clannd[clsnfoX_.clcrnt-One] = opadroX_.sconpo[targvc-One];


									}
								}
							goto L_9999;
							}
						}
					}
				else if( vbdataX_.k3p1 == 54 ){

					/*----------- -67 054 ------  SET SCONS OF THE CLAUSE MARKER */


					k3p2 = vtrfX_.vtrf[vbdataX_.k3+2-One];
					k3p3 = vtrfX_.vtrf[vbdataX_.k3+3-One];
					/*                            SET POINTER TO START OF NEXT SWITCH */
					vbdataX_.k3n = vbdataX_.k3 + 4 + k3p2*2;
					/*                            SKIP IF NO PARENT CLAUSE OR NO CLAUSE MARKE */
					if( clsnfoX_.clprnt[clsnfoX_.clcrnt-One] < 1 || 
					  clsnfoX_.clprnt[clsnfoX_.clcrnt-One] > clsnfoX_.cltotl )
						goto L_9999;
					if( clsnfoX_.clmrkr[clsnfoX_.clcrnt-One] == 0 )
						goto L_9999;

					fcn = k3p3;
					if( fcn < 1 || fcn > 2 ){
						errvrsX_.err = 7;
						}
					else{
						/*                       LOOP THROUGH EACH ASSIGNMENT PAIR */
						pairpt = vbdataX_.k3 + 4;
						for( xx=1; xx <= k3p2; xx++ ){
							scnpt = vtrfX_.vtrf[pairpt-One];
							/*                       GET THE NEW VALUE BASED ON FUNCTION */
							if( fcn == 1 ){
								vbpt = vtrfX_.vtrf[pairpt+1-One];
								if( vbpt < 1 || vbpt > 100 )
									goto L_10013;
								val = vbdataX_.vbcell[vbpt-One];
								}
							else{
								val = vtrfX_.vtrf[pairpt+1-One];
								}
							/*                        SET THE CLAUSE MARKER SCON */
							if( scnpt <= SCONX1 ){
								sconX_.scon[clsnfoX_.clmrkr[clsnfoX_.clcrnt-One]-
								  One][scnpt-One] = val;
								if( diagsX_.longdi == 1 )
									{
									fprintf( _spec_fp, "SW67 054:, SETTING SCON %3d OF ELEMENT %3d EQUAL TO %5d\n", 
									  scnpt, clsnfoX_.clmrkr[clsnfoX_.clcrnt-One], 
									  val );
									}
								}
							else{
								sconX_.scono[sconX_.scolnk[clsnfoX_.clmrkr[clsnfoX_.clcrnt-One]-
								  One]-One][scnpt-SCONX1-One] = val;
								if( diagsX_.longdi == 1 )
									{
									fprintf( _spec_fp, "SW67 054:, SETTING SCON %3d OF ELEMENT %3d EQUAL TO %5d\n", 
									  scnpt, clsnfoX_.clmrkr[clsnfoX_.clcrnt-One], 
									  val );
									}
								}
							/*                         POINT TO NEXT PAIR */
							pairpt += 2;
							}
						goto L_9999;
L_10013:
						errvrsX_.err = 8;
						}
					}
				else if( vbdataX_.k3p1 == 55 ){

					/*--------------------------  LOAD CELLS WITH PARENT INFORMATION */

					/*                            SET POINTER TO START OF NEXT SWITCH */
					vbdataX_.k3n = vbdataX_.k3 + 5;

					k3p2 = vtrfX_.vtrf[vbdataX_.k3+2-One];
					k3p3 = vtrfX_.vtrf[vbdataX_.k3+3-One];
					k3p4 = vtrfX_.vtrf[vbdataX_.k3+4-One];
					/*                             CHECK FOR VALID PARAMETERS */
					if( k3p2 < 1 || k3p2 > 100 ){
						errvrsX_.err = 10;
						}
					else{
						fcn = k3p4;
						if( fcn < 1 || fcn > 3 ){
							errvrsX_.err = 9;
							}
						else{

							if( fcn == 1 ){
								vbdataX_.vbcell[k3p2-One] = clsnfoX_.clpcel[clsnfoX_.clcrnt-One][k3p3-One];
								}
							else if( fcn == 2 ){
								if( k3p3 <= SCONX1 ){
									vbdataX_.vbcell[k3p2-One] = sconX_.scon[clsnfoX_.clansc[clsnfoX_.clcrnt-One]-
									  One][k3p3-One];
									}
								else{
									vbdataX_.vbcell[k3p2-One] = sconX_.scono[sconX_.scolnk[clsnfoX_.clansc[clsnfoX_.clcrnt-One]-
									  One]-One][k3p3-SCONX1-One];
									}
								}
							else if( fcn == 3 ){
								if( k3p3 <= SCONX1 ){
									vbdataX_.vbcell[k3p2-One] = sconX_.scon[clsnfoX_.clmrkr[clsnfoX_.clcrnt-One]-
									  One][k3p3-One];
									}
								else{
									vbdataX_.vbcell[k3p2-One] = sconX_.scono[sconX_.scolnk[clsnfoX_.clmrkr[clsnfoX_.clcrnt-One]-
									  One]-One][k3p3-SCONX1-One];
									}
								}
							if( diagsX_.longdi == 1 )
								{
								fprintf( _spec_fp, "SW67 055:, SETTING CELL %3d EQUAL TO %4d   FOR FUNCTION %3d\n", 
								  k3p2, vbdataX_.vbcell[k3p2-One], 
								  fcn );
								}

							goto L_9999;
							}
						}
					}
				else{
					goto L_9999;
					}
				/*                            REPORT ERROR AND CONTINUE THE VTR */
L_9500:
				;
				if( diagsX_.deepdi == 1 || diagsX_.longdi == 1 ){
					fprintf( _spec_fp, " ********** ERROR IN TXSW67   ********\n" );

					if( errvrsX_.err == 6 )
						{
						fprintf( _spec_fp, "  SWITCH 67 FUNCTION:%3dIS ILLEGAL IN TRAN%2ld\n", 
						  vbdataX_.k3p1, tranidX_.tranid );
						}
					if( errvrsX_.err == 7 )
						{
						fprintf( _spec_fp, "  SWITCH 67 054: FUNCTION%3d IS ILLEGAL.\n",fcn );
						}
					if( errvrsX_.err == 8 )
						{
						fprintf( _spec_fp, "  SWITCH 67 054 001 ... %3d IS AN ILLEGAL CELL.\n",vbpt );
						}
					if( errvrsX_.err == 9 )
						{
						fprintf( _spec_fp, "  SWITCH 67 055: %3d IS AN ILLEGAL FUNCTION.\n",fcn );
						}
					if( errvrsX_.err == 10 )
						{
						fprintf( _spec_fp, "  SWITCH 67 006: BAD CELL RANGES %3d %3d\n", 
						          k3p2, k3p3 );
						}
					if( errvrsX_.err == 11 )
						{
						fprintf( _spec_fp, "  SWITCH 67 006: BAD SEARCH DIRECTION PARAMETER =%3d\n", 
						  k3p4 );
						}
					if( errvrsX_.err == 12 )
						{
						fprintf( _spec_fp, "  SWITCH 67 006: ERROR CALCULATING SEARCH BOUNDARIES\n  SWSTOP,SWSTRT,CL676R,CL676L,CLCRNT =%5d  %5d  %5d  %5d  %5d  \n", 
						  swstop, swstrt, clsnopX_.cl676r[clsnfoX_.clcrnt-One], 
						  clsnopX_.cl676l[clsnfoX_.clcrnt-One], clsnfoX_.clcrnt );
						}
					if( errvrsX_.err == 4401 )
						{
						fprintf( _spec_fp, "  SWITCH 67 044: Couldnt find target VC =%4d\n", 
						  -k3p2 );
						}
					if( errvrsX_.err == 4402 )
						{
						fprintf( _spec_fp, "  SWITCH 67 044: Couldnt find antecedent, SCON =%4d\n", 
						  clsnfoX_.clansc[clsnfoX_.clcrnt-One] );
						}
					if( errvrsX_.err == 4403 )
						{
						fprintf( _spec_fp, "  SWITCH 67 044: Couldnt find target VC =%4d\n", 
						  -k3p2 );
						}
					if( errvrsX_.err == 4404 )
						{
						fprintf( _spec_fp, "  SWITCH 67 044: Couldnt find clause marker, SCON =%4d\n", 
						  clsnfoX_.clmrkr[clsnfoX_.clcrnt-One] );
						}
					if( errvrsX_.err == 4405 )
						{
						fprintf( _spec_fp, "  SWITCH 67 044: Couldnt find target VC =%4d\n", 
						  -k3p2 );
						}
					if( errvrsX_.err == 4406 )
						{
						fprintf( _spec_fp, "  SWITCH 67 044: Couldnt find target VC =%4d\n", 
						  -k3p2 );
						}
					if( errvrsX_.err == 4407 )
						{
						fprintf( _spec_fp, "  SWITCH 67 044: No more VC overflow arrays, ADCT=%4d\n", 
						  hfdoaX_.adct );
						}
					if( errvrsX_.err == 4408 )
						{
						fprintf( _spec_fp, "  SWITCH 67 044: Overflow of VC \n" );
						}
					if( errvrsX_.err == 4420 )
						{
						fprintf( _spec_fp, "  SWITCH 67 044: OVERLOADING SCON ARRAY, SCTERR =%4d\n", 
						  prtscoX_.scterr );
						}

					fprintf( _spec_fp, "   IGNORE THIS SWITCH & CONTINUE THE VTR.\n *******************************************\n" );
					errlog(pgmnam,9500,errvrsX_.err,9);
					}
				else{
					errlog(pgmnam,9500,errvrsX_.err,9);
					}

				/*                    default loads for some 044 errors */
				if( errvrsX_.err >= 4401 && errvrsX_.err <= 4407 ){
					fprintf( _spec_fp, "DEFAULT LAODING THE SOURCE ELEMENT AT %3d\n", 
					  k3p4 );
					vtrnX_.vtrn = -(im81X_.im81 - k3p4);
					vbdataX_.k3p1 = 0;
					elemld();
					if( errvrsX_.errlvl != 0 ){
						fprintf( _spec_fp, "ERROR: ATTEMPTING TO LOAD SOURCE ELEMENT\n" );
						errlog(pgmnam,9651,errvrsX_.err,9);
						}
					}
				}
			}
		goto L_9999;
		}

	/*------                      ERRORS */

	/*                            REPORT ERROR & ABORT THE RULE */
L_9000:
	;
	if( diagsX_.deepdi == 1 || diagsX_.longdi == 1 ){
		fprintf( _spec_fp, " ********** ERROR IN TXSW67   ********\n" );
		if( errvrsX_.err == 1 )
			{
			fprintf( _spec_fp, "   CLAUSE MAXIMUM EXCEEDED. \n" );
			}
		if( errvrsX_.err == 2 )
			{
			fprintf( _spec_fp, "   NO SPACE IN SCON TABLE FOR ADDED CLAUSE ELEMENTS.\n" );
			}
		if( errvrsX_.err == 3 )
			{
			fprintf( _spec_fp, "   NO SPACE IN OPADRI FOR ADDED CLAUSE ELEMENTS.\n" );
			}
		if( errvrsX_.err == 4 )
			{
			fprintf( _spec_fp, "   NO SPACE IN SCONO FOR ADDED CLAUSE ELEMENTS.\n" );
			}
		if( errvrsX_.err == 5 )
			{
			fprintf( _spec_fp, "   -67 002 ILLEGAL WITHOUT A PRECEDING -67001.\n" );
			}
		if( errvrsX_.err == 6 )
			{
			fprintf( _spec_fp, "  SWITCH 67 FUNCTION:%3dIS ILLEGAL IN TRAN%2ld\n", 
			  vbdataX_.k3p1, tranidX_.tranid );
			}
		if( errvrsX_.err == 6 ){
			fprintf( _spec_fp, "   IGNORE THIS SWITCH & CONTINUE THE VTR.\n *******************************************\n" );
			}
		else{
			fprintf( _spec_fp, "   ABORTING RULE & CONTINUING W/O EXECUTING THE SWITCH.\n              ******************\n" );
			}
		}
	else{
		errlog(pgmnam,9000,errvrsX_.err,9);
		}
	/*                            SET K3N TO CAUSE VTR ABORT ON RETURN */
	vbdataX_.k3n = vwarg2X_.iz4 + 1;

	/*------ */

L_9999:
	;
	return;
} /*end of function*/



