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
#include "trans.h"
#include "logoslib.h"
#include "projexts.h"

/****************************************************************
 * TXSW68 -  CREATE AND INSERT AN SWORK IN TRAN 2,3,4 */

/* FUNCTION:
 *     THE 1ST PARAMETER DEFINES THE FUNCTION:
 *           -68 P1 P2 P3 P4 P5 P6 P7 P8 P9 */

/*     P1=001: CREATE AND INSERT A COMPETELY USABLE SWORK ELEMENT.
 *     ------
 *         P2=  PTR TO SWORK WHICH WILL IMMEDIATELY PRECEDE THE NEW
 *              SWORK
 *         *****  NOTE:  VTRFWR SETS P2 TO THE SWORK WHERE NEW
 *         *****         SWORK WILL BE INSERTED. I.E. SWORK at P2 and
 *         *****         all subsequent SWORKS WILL BE SHifted right.
 *         P3=  WC       OF NEW ELEMENT.
 *         P4=  SUPERSET OF NEW ELEMENT.
 *         P5=  SET      OF NEW ELEMENT.
 *         P6=  SUBSET   OF NEW ELEMENT.
 *         P7=  FORM     OF NEW ELEMENT (ALSO THE FORMSAVE VALUE).
 *         P8=  TARGET ADDRESS    OF NEW ELEMENT. */


/*     PARAMETER  P2   MUST BE IN THE -8X FORMAT.
 *     PARAMETERS P3 - P8 MAY BE CONSTANT VALUES OR -8X FORMAT POINTERS. */

/* ****  CAVEAT  *****  6/4/93
 *     If an element is inserted to the left of a NULL stretch element (i.e.
 *     a stretch that matches on no SWORKs), relative pointers to the right
 *     the newly inserted element may be incorrect. */

/*     This can occur because VTRFWR rewrites relative pointers to account for
 *     the size of stretch elements. A pointer to a Null stretch element will
 *     be changed to the same value as the element that follows it.  When SW68
 *     goes to reset these pointers to account for shifting strecth elements,
 *     it cannot differentiate between a pointer to the Null stretch element
 *     and the following element. At present the following element will be
 *     selected.  The solution is to not adjust relative pointers up front
 *     in VTRFWR, but rather on the fly (perhaps with a look up into an array
 *     that accounts for stretches).
 * *******************  6/4/93 */

/* PROCESS:
 *  1-SHIFT CERTAIN ARRAYS RIGHT TO MAKE SPACE FOR THE NEW ELEMENT.
 *    THESE ARRAYS ARE: SWORK,PHRBEG,PHREND,PHRHED
 *  2-FILL THE ARRAY VALUES FOR THE NEWLY INSERTED ELEMENT.
 *  3-SET THE ARRAY VALUES FOR THE ALL OTHER INFORMATION NECESSARY FOR
 *    THIS NEW ELEMENT.  THIS INFORMATION IS LOADED AT THE END OF THE
 *    CURRENT ARRAYS.  SCOLNK(PHRHED(NEW_SWORK_PTR)) WILL BE THE GENERIC
 *    POINTER TO THIS INFORMATION FOR THE NEW SWORK.
 *    THESE ARRAYS INCLUDE:  SCON, SCONO, FORMSV, WSTRNG, JS, ????
 *  4-FINALLY, UPDATE SWORK POINTERS IN THE CLAUSE ARRAYS FOR ANY SWORKS
 *    TO THE RIGHT OF THE NEW ELEMENT. */

/*  IN THE EVENT OF A SERIOUS ERROR: TERMINATE NORMAL PROCESSING OF THIS
 *  VTR, CAUSE EXECUTION OF -36056000 TO PREVENT FURTHER MATCH ON THIS
 *  RULE, AND SET I3SAVE SO NEXT MATCH IS ON 1ST ELEMENT OF RULE AGAIN
 *  OR THE LAST UNLOADED ELEMENT. */

/*  **** NOTE *****
 *     ANY CHANGES TO THIS SWITCH SHOULD CONSIDER VTRFWR().  VTRFWR()
 *     RESETS PARAMETER2. */

/* INPUT: */

/* OUTPUT: */

/* CHANGES:
 *    8/2/93  jal: remove code that accounted for stretches when calculating
 *                relative ptr K3P2, and right shifted it. This is now done
 *                by VTRFWR.
 *    6/21/93 jal: remove error if inserting to left of an element
 *                 already loaded.
 *    6/4/93 jal:  increment LIKEEP so it = current level of rule.
 *    6/4/93 jal:  fix to resetting of remaining relative pointers in
 *         the vtr.  See CAVEAT above. */

/**************************************************************** */


#include <string.h>
#include "parsetrans_ext.h"
#include <jbctrl.h>



void /*FUNCTION*/ sw68()
{
	static char tmpchr[17];
	static short int cnt50, conv8x[ELMMAX], e, el, end8x, form68, 
	  frmsv, inse8x, inselm, inso8x, insoff, inspos, k, k3p2, k3p3, 
	  k3p4, k3p5, k3p6, k3p7, k3p8, newpos, nxtelm, oldpos, scn1168, 
	  scn1368, scn168, scn268, scnpos, sizeof_[20], skip, strflg, 
	  sw42n, swipos, tmp, tmparr[ELMMAX][4], tmplvl, type68, wc50tm, 
	  wc68, xx, yy;
	static long int _l0, shftby, shftsw, strtby, strtsw, tempi4;
	static char pgmnam[9] = "TXSW68  ";
	static short zero = 0;

	/*                            SCNPOS = POINTER TO SCON OF NEW SWORK */
	/*                            TEMPORARY ARRAYS */
	/*                            SIZEOF=# SWORKS matching each element in rule */
	/*                            NEED K3,K3N,K3P1 */
	/*                            NEED I FOR CURRENT RULE START POSITION
	 *                            IZ4 = END OF VTR IN VTRF()
	 *                            WC50MS = SAVED WC50M= FLAGS WC50 IN THIS RU */
	/*                            NEED SWORK , IM81, IZ4 */
	/*                           26SW VARIABLES, HEADHD IS NEEDED */
	/*                           IMPORTANT POINTERS
	 *                             USE I3SAVE TO RESET POS OF NEXT RULE */
	/*                 BSDFLT, IF 0 THEN I3SAVE = I3
	 *                 SW41P1, FIRST PARAMETER OF THE 41SWITCH */
	/*                      LIKEEP: LEVEL OF ORIGINAL RULE */
	/*-----------------------------------------------------------------------
	 *                            CAN ANOTHER ELEMENT BE ADDED? */
	if( elemctX_.elemct >= ELMMAX ){
		errvrsX_.err = 1;
		/*                            ACCOUNT FOR CLAUSES WAITING TO BE MOVED */
		}
	else if( tranidX_.tranid == 2 && elemctX_.elemct + (clsmovX_.clmcnt*
	  3) >= ELMMAX ){
		errvrsX_.err = 5;
		/*                            TRAN1 CAN ONLY ADD AD1MAX ELEMENTS CUZ
		 *                            THATS SIZE OF RESERVED SPACE IN SCONS */
		}
	else if( tranidX_.tranid == 1 && elemctX_.elemct >= elemctX_.origct + 
	  AD1MAX ){
		errvrsX_.err = 6;
		/*                            CAN SCON TABLE HANDLE A NEW ELEMENTS? */
		}
	else if( prtscoX_.sct >= SCONY && elemctX_.elemct >= elemctX_.origct + 
	  AD1MAX ){
		prtscoX_.scterr += 1;
		errvrsX_.err = 2;
		}
	else{
		/*C                            FOR PASS 1 OF 2 DOES SCON TABLE HAVE SPACE
		 *      IF(PASSFL.EQ.1 .AND. PASSCT.EQ.1) THEN
		 *         IF(SCTSAV.GE.SCONY .AND. ELEMCT.GE.ORIGCT+AD1MAX) THEN
		 *             SCTERR = SCTERR + 1
		 *             ERR = 2
		 *             GOTO 9000
		 *         ENDIF
		 *      ENDIF
		 *                            CHECK FIRST PARAMETER */
		vbdataX_.k3p1 = vtrfX_.vtrf[vbdataX_.k3+1-One];
		if( vbdataX_.k3p1 == 1 ){
			/*                            FIRST PARAMETER DETERMINES FUNCTION */

			/*--------------------------  CREATE AND INSERT AN SWORK */

			/*                            SET POINTER TO START OF NEXT SWITCH */
			vbdataX_.k3n = vbdataX_.k3 + 9;
			/*                            GET PARAMETERS */
			k3p2 = vtrfX_.vtrf[vbdataX_.k3+2-One];
			k3p3 = vtrfX_.vtrf[vbdataX_.k3+3-One];
			k3p4 = vtrfX_.vtrf[vbdataX_.k3+4-One];
			k3p5 = vtrfX_.vtrf[vbdataX_.k3+5-One];
			k3p6 = vtrfX_.vtrf[vbdataX_.k3+6-One];
			k3p7 = vtrfX_.vtrf[vbdataX_.k3+7-One];
			k3p8 = vtrfX_.vtrf[vbdataX_.k3+8-One];
			/*------
			 *                 SET UP NECESSARY PARAMETERS
			 *------
			 *                            INSERT POSITION IS JUST BEFORE ELEMENT K3P2
			 *                            K3P2 is not yet adjusted for stretches (6/3/93). */

			/*                            INSPOS = ABSOLUTE POSITION IN SWORK ARRAY
			 *                                     WHERE NEW ELEMENT IS ADDED
			 *                            INSOFF = OFFSET OF INSPOS FROM 1ST SWORK
			 *                                     MATCHED ON BY THE RULE. (INSPOS-I)
			 *                            INSO8X = -8X FORM OF INSOFF.
			 *                            INSELM = Rule element before which new
			 *                                     Swork will be inserted (from the
			 *                                     switch parameter K3P2).
			 *                            INSE8X = -8X form of INSELM */

			inselm = -80 - k3p2;
			inse8x = k3p2;
			/*                            Calc INSOFF by accounting for any stretches
			 *+         vtrfwr already accounted for stretches & right shift  8/2/93 jal */
			insoff = inselm;
			/*      IF (WC50MS.EQ.0 .AND. WC42M.EQ.0) THEN
			 *         INSOFF = INSELM
			 *      ELSE
			 *         INSOFF=1
			 *         CNT50=1
			 *         DO 1050 XX=1,INSELM-1
			 *            IF (CNT50.LT.4 .AND. WC50EL(CNT50).EQ.-XX) THEN
			 *               INSOFF = INSOFF + CHNG50(CNT50) + 1
			 *               CNT50 = CNT50 + 1
			 *            ELSE
			 *               INSOFF= INSOFF+1
			 *            ENDIF
			 *1050     CONTINUE
			 *      ENDIF
			 *-                                                        8/2/93 jal */
			inso8x = -80 - insoff;
			inspos = im81X_.im81 - inso8x;
			if( passesX_.passfl == 1 && passesX_.passct == 1 )
				swi68p(inspos,&swipos);

			/*                            CANNOT INSERT TO LEFT OF ELEMENT ALREADY
			 *                            LOADED
			 *      IF (INSPOS.LE.N6JIMS) THEN
			 *         ERR = 4
			 *         GOTO 9000
			 *      ENDIF */

			/*                         GET NEW SWORK VALUES BEFORE MOVING ARRAYS. */

			/*                         WC */
			if( k3p3 > -70 ){
				wc68 = k3p3;
				}
			else{
				wc68 = sworkX_.swork[im81X_.im81-k3p3-One][1-One];
				}
			/*                         TYPE/SUBSET */
			if( k3p6 > -70 ){
				type68 = k3p6;
				}
			else{
				type68 = sworkX_.swork[im81X_.im81-k3p6-One][2-One];
				}
			/*                         FORM */
			if( k3p7 > -70 ){
				form68 = k3p7;
				}
			else{
				form68 = sworkX_.swork[im81X_.im81-k3p7-One][3-One];
				}
			/*                         WC */
			if( k3p3 > -70 ){
				scn168 = k3p3;
				}
			else{
				scn168 = sconX_.scon[sworkX_.swork[im81X_.im81-k3p3-One][4-One]-
				  One][1-One];
				}
			/*                         SUPERSET */
			if( k3p4 > -70 ){
				scn1368 = k3p4;
				}
			else{
				scn1368 = sconX_.scon[sworkX_.swork[im81X_.im81-k3p4-One][4-One]-
				  One][13-One];
				}
			/*                         SET */
			if( k3p5 > -70 ){
				scn1168 = k3p5;
				}
			else{
				scn1168 = sconX_.scon[sworkX_.swork[im81X_.im81-k3p5-One][4-One]-
				  One][11-One];
				}
			/*                         SUBSET */
			if( k3p6 > -70 ){
				scn268 = k3p6;
				}
			else{
				scn268 = sconX_.scon[sworkX_.swork[im81X_.im81-k3p6-One][4-One]-
				  One][2-One];
				}
			/*                         FORMSAV */
			if( k3p7 > -70 ){
				frmsv = k3p7;
				}
			else{
				frmsv = formsaX_.formsv[sconX_.scolnk[sworkX_.swork[im81X_.im81-k3p7-One][4-One]-
				  One]-One];
				}
			/*-------
			 *             SHIFT ARRAYS
			 *-------
			 *                            BYTE TO START INSERT AT */
			strtby = (inspos)*2 - 1;
			strtsw = (inspos)*8 - 7;
			/*                            BYTES TO SHIFT RIGHT */
			shftby = (sworkX_.phct - inspos + 1)*2;
			shftsw = shftby*4;

			/*                           SHIFT ARRAYS RIGHT TO MAKE SPACE */

			lmove((short*)tmparr,1,sworkX_.phrbeg,strtby,shftby);
			lmove(sworkX_.phrbeg,strtby+2,(short*)tmparr,1,shftby);

			lmove((short*)tmparr,1,sworkX_.phrend,strtby,shftby);
			lmove(sworkX_.phrend,strtby+2,(short*)tmparr,1,shftby);

			lmove((short*)tmparr,1,sworkX_.phrhed,strtby,shftby);
			lmove(sworkX_.phrhed,strtby+2,(short*)tmparr,1,shftby);

			lmove((short*)tmparr,1,(short*)sworkX_.swork,strtsw,shftsw);
			lmove((short*)sworkX_.swork,strtsw+8,(short*)tmparr,1,
			  shftsw);
			/*                           INPUT SWORK IF PASS1 OF 2 PASS STRATEGY */
			if( (passesX_.passfl == 1 && passesX_.passct == 1) && 
			  swipos > 0 ){
				strtsw = (swipos)*8 - 7;
				shftsw = (elemctX_.parsct - swipos + 1)*8;
				lmove((short*)tmparr,1,(short*)sworkiX_.sworki,strtsw,
				  shftsw);
				lmove((short*)sworkiX_.sworki,strtsw+8,(short*)tmparr,
				  1,shftsw);
				}
			/*------
			 *                           INCREMENT VITAL COUNTER VALUES
			 *------ */
			elemctX_.elemct += 1;
			sworkX_.phct += 1;
			opadriX_.opi += 1;
			if( (passesX_.passfl == 1 && passesX_.passct == 1) && 
			  swipos > 0 )
				elemctX_.parsct += 1;

			/*                          INCR SW68CT TO ACCOUNT FOR NEW SWORK */
			swtc68X_.sw68ct += 1;

			/*                    FIND THE SCON POSITION FOR NEW ELEMENT. */

			/*                           USE RESERVED SPACE IF STILL AVAILABLE */
			if( elemctX_.elemct <= elemctX_.origct + AD1MAX ){
				scnpos = elemctX_.elemct;
				/*                            NONE AVAILABLE ADD TO END OF TABLE */
				}
			else{
				/*C                           IF PASS 1 OF 2 PASS STRATEGY THEN ADD THE
				 *C                           SCON TO END OF TABLE AS INPUT TO THIS TRAN
				 *C                           (I.E. SCTSAV). TARGET CONSTANT SCONS ADDED
				 *C                           BY PASS1 WILL ONLY BE OVERWRITTEN BY PASS2
				 *         IF (PASSFL.EQ.1 .AND. PASSCT.EQ.1 .AND. PASSKP.EQ.0) THEN
				 *            SCTSAV = SCTSAV + 1
				 *            SCNPOS = SCTSAV
				 *            IF (SCT.LT.SCTSAV) SCT = SCTSAV
				 *                            JUST ADD TO END OF SCON TABLE
				 *         ELSE */
				prtscoX_.sct += 1;
				scnpos = prtscoX_.sct;
				/*         ENDIF */
				}
			/*-------
			 *                           INSERT NEW ELEMENT VALS INTO SHIFTED ARRAYS
			 *                           PHRBEG,PHREND,PHRHED,SWORK
			 *------- */
			sworkX_.phrbeg[inspos-One] = opadriX_.opi;
			sworkX_.phrend[inspos-One] = opadriX_.opi;
			sworkX_.phrhed[inspos-One] = scnpos;
			sworkX_.swork[inspos-One][4-One] = scnpos;
			/*                         WC */
			sworkX_.swork[inspos-One][1-One] = wc68;
			/*                         TYPE */
			sworkX_.swork[inspos-One][2-One] = type68;
			/*                         FORM */
			sworkX_.swork[inspos-One][3-One] = form68;
			/*                         INPUT SWORK ONLY IF PASS1 OF 2 PASSES */
			if( (passesX_.passfl == 1 && passesX_.passct == 1) && 
			  swipos > 0 ){
				sworkiX_.sworki[swipos-One][4-One] = scnpos;
				sworkiX_.sworki[swipos-One][1-One] = wc68;
				sworkiX_.sworki[swipos-One][2-One] = type68;
				sworkiX_.sworki[swipos-One][3-One] = form68;
				}
			/*-------
			 *                          AFFIX NEW ELEMENT TO END OF NON-SHIFT ARRAYS
			 *------- */
			opadriX_.opadri[opadriX_.opi-One] = -k3p8;
			opadriX_.sconpi[opadriX_.opi-One] = scnpos;
			hpdopiX_.hfdopi[opadriX_.opi-One] = 0;
			/*                          INITIALIZE ITS SCONS */
			zapit(&sconX_.scon[scnpos-One][1-One],SCONX1*2,(byte)0);
			sconX_.scolnk[scnpos-One] = elemctX_.elemct;
			zapit(&sconX_.scono[elemctX_.elemct-One][1-One],SCONX-
			  SCONX1*2,(byte)0);
			/*                         WC */
			sconX_.scon[scnpos-One][1-One] = scn168;
			/*                         SUPERSET */
			sconX_.scon[scnpos-One][13-One] = scn1368;
			/*                         SET */
			sconX_.scon[scnpos-One][11-One] = scn1168;
			/*                         SUBSET */
			sconX_.scon[scnpos-One][2-One] = scn268;
			/*                         FORMSAV */
			formsaX_.formsv[elemctX_.elemct-One] = frmsv;

			if( (passesX_.passfl == 1 && passesX_.passct == 1) && 
			  swipos > 0 ){

				sconinX_.sconin[elemctX_.elemct-One][1-One] = scn268;
				sconinX_.sconin[elemctX_.elemct-One][2-One] = scn1168;
				sconinX_.sconin[elemctX_.elemct-One][3-One] = scn1368;
				/*		parallel formsave data sent to TRAN1 */
				formsaX_.prsfrm[elemctX_.elemct-One] = frmsv;
				/*		  map from new SCON to TRAN1 input SWORK for new 68 element */
				sworkiX_.sc2swi[scnpos-One] = swipos;
				/*                 SWORKI following new 68 element have been shifted over 1 */
				for( xx=1; xx <= (scnpos - 1); xx++ ){
					if( sworkiX_.sc2swi[xx-One] >= swipos )
						sworkiX_.sc2swi[xx-One] += 1;
					}
				}
			/*                          SET FALSE INPUT WORD INTO NWSTRG */
			memcpy(sent_wordsX_.source_word[elemctX_.elemct-One],"* SWITCH68 *    ",16);
			/*                          JS = POS CHOSEN FROM DICTIONARY ENTRY */
			prctX_.js[elemctX_.elemct-One] = 1;
			/*                            SET HENUM, HASHCODE OF NEW ELEMENT TO
			 *                            IMPOSSIBLE MATCH AS DEFAULT. */
			hensavX_.henum2[elemctX_.elemct-One][1-One] = -1;
			hashX_.hashcd[elemctX_.elemct-One][1-One] = 0;

			/*                          RESET AFFECTED POINTERS */

			/*                          PTRS IN CLAUSE INFORMATION STACK */
			if( tranidX_.tranid > 1 ){
				for( k=clsnfoX_.clcrnt; k <= clsnfoX_.cltotl; k++ ){
					if( clsnfoX_.clbgns[k-One] >= inspos )
						clsnfoX_.clbgns[k-One] += 1;
					if( clsnfoX_.clndns[k-One] >= inspos )
						clsnfoX_.clndns[k-One] += 1;
					if( clsnopX_.cl676l[k-One] >= inspos )
						clsnopX_.cl676l[k-One] += 1;
					if( clsnopX_.cl676r[k-One] >= inspos )
						clsnopX_.cl676r[k-One] += 1;
					if( clsnfoX_.clansw[k-One] >= inspos )
						clsnfoX_.clansw[k-One] += 1;
					}
				}
			/*                          PTRS IN CLAUSE MOVE STACK IN TRAN2 */
			if( tranidX_.tranid == 2 && clsmovX_.clmcnt > 0 ){
				for( k=1; k <= clsmovX_.clmcnt; k++ ){
					if( clsmovX_.clmpos[k-One] >= inspos )
						clsmovX_.clmpos[k-One] += 1;
					}
				}
			/*                           INCR 26SW HEAD PTR IF RIGHT OF NEW ELEMNT */
			if( head26X_.headhd >= inspos )
				head26X_.headhd += 1;
			/*                           N6 POINTS TO LAST ELEMENT OF THE RULE */
			if( inspos <= flowckX_.n6 )
				flowckX_.n6 += 1;

			/*                 ADJUSTING REL PTRS IN REMAINDER OF VTR:
			 *                          REL. PTRS (-8X) IN THE REST OF THE VTR MAY
			 *                          NEED ADJUSTMENT IF THEY POINT TO THE RIGHT OF
			 *                          INSPOS AND THERE ARE STRETCHES AT OR AFTER INSPOS.
			 *                          VTRFWR HAS ALREADY CHANGED ORIGINAL -8X PTRS TO
			 *                          ACCOUNT FOR STRETCHES.  HERE WE CREATE A
			 *                          TABLE TO CONVERT CURRENT -8X TO NEW -8X VALUES. */

			/*                          OLDPOS = 1ST SWORK THAT MATCHES AN ELEMENT OF THE
			 *                               RULE BEFORE THE -68 SWITCH WAS EXECUTED.
			 *                          CONV8X(X1) = CONVERSION FROM OLDPOS TO NEWPOS, I.E.
			 *                               -8X BEFORE -68SW = CONV8X(-8X), AFTER -68SW.
			 *                          STRFLG = 0, NO WC50 TO RIGHT OF NEW SWORK */

			/*                          SEE HEADER NOTES ON CAVEAT. Null stretches may
			 *                          throw off resetting of rel ptrs. */


			/*                          IF no stretches then no resetting necessary. */
			if( !(vwarg2X_.wc50ms == 0 && vwarg2X_.wc42m == 0)
			   ){

				/*                          First: calc # of SWORKS matching each element
				 *                          in the rule. */
				cnt50 = 1;
				strflg = 0;
				for( el=1; el <= sw42bkX_.likeep; el++ ){
					if( cnt50 < 4 && vwarg1X_.wc50el[cnt50-One] == 
					  -el ){
						sizeof_[el-One] = vwarg1X_.chng50[cnt50-One] + 
						  1;
						cnt50 += 1;
						if( el >= inselm )
							strflg = 1;
						}
					else{
						sizeof_[el-One] = 1;
						}
					}
				/*                          IF NO STRETCH FOLLOWS NEW 68 SWORK SKIP resetting */
				if( strflg != 0 ){
					/*                          set conversion array starting after ins element */
					zapit(conv8x,ELMMAX*2,(byte)zero);
					oldpos = insoff;
					newpos = oldpos;
					nxtelm = inselm + 1;
					for( el=nxtelm; el <= sw42bkX_.likeep; el++ ){
						if( el == nxtelm ){
							newpos += 1;
							}
						else{
							newpos += sizeof_[el-2-One];
							}
						oldpos += sizeof_[el-1-One];
						conv8x[oldpos-One] = newpos;
						}
					/*                          END8X1 = LAST 8X VALUE SET IN CONV8X */
					end8x = -80 - oldpos;

					/*                          LOOP THRU REST OF VTRF CHANGING ANY POINTERS */
					skip = 0;
					for( tmp=vbdataX_.k3n; tmp <= vwarg2X_.iz4; tmp++ ){
						if( skip > 0 ){
							skip -= 1;
							}
						else if( vtrfX_.vtrf[tmp-One] == 999 ){
							break;
							/*                          Dont reset insert ptr for -68 switch */
							}
						else if( vtrfmX_.vtrfm[tmp-One] == -68 && 
						  vtrfmX_.vtrfm[tmp+1-One] == 1 ){
							/*             TMP=TMP+3 */
							skip = 2;
							}
						else if( vtrfmX_.vtrfm[tmp-One] == vtrfvlX_.vtmrp ){
							if( vtrfX_.vtrf[tmp-One] < inso8x && vtrfX_.vtrf[tmp-One] >= 
							  end8x ){
								yy = -80 - vtrfX_.vtrf[tmp-One];
								if( conv8x[yy-One] > 0 ){
									vtrfX_.vtrf[tmp-One] = -80 - conv8x[yy-One];
									}
								else{
									if( diagsX_.deepdi == 1 || diagsX_.longdi == 
									  1 )
										{
										fprintf( _spec_fp, "*** ERROR 1651 IN TXSW68 CONVERTING RELATIVE POINTERS. CONTINUING\n   TMP,K3N,IZ4,VTRF(TMP) =%3d   %3d   %3d   %3d\n\n", 
										  tmp, vbdataX_.k3n, vwarg2X_.iz4, 
										  vtrfX_.vtrf[tmp-One] );
										}
									tempi4 = tmp;
									errlog(pgmnam,1551,tempi4,10);
									}
								}
							}
						}
					}

				/*                          ADJUST THE WC50 STRETCH ARRAYS TO CORRESPOND
				 *                          TO NEW SWORK LAYOUT. ARRAYS MAY BE USED BY
				 *                          VTRFWR() TO CONVERT REL.PTRS. FOR ADDED VTRS
				 *                          TABLES. ALSO USED BELOW TO RECALC STARTING
				 *                          SWORK OF NEXT MATCH (I3SAVE) */

				for( tmp=1; tmp <= 3; tmp++ ){
					wc50tm = vwarg1X_.wc50el[tmp-One];
					if( wc50tm > -100 && wc50tm <= -inselm ){
						vwarg1X_.wc50el[tmp-One] = wc50tm - 1;
						vwarg1X_.look50[tmp-One] -= 1;
						}
					}
				}

			/*                          RECALCULATE THE NEW START SWORK FOR NEXT RULE
			 *                          BASED ON THE UPDATED STRETCH MARKERS (WC50EL)
			 *                          -41SW APPLIES TO THE NEW SWORK (NOT ORIGINAL)
			 *                          I3SAVE = STARTING SWORK FOR NEXT RULE.
			 *                          SW41P1 = FIRST PARAMETER OF 41SWITCH.
			 *                          LIKEEP = LEVEL OF ORIGINAL RULE */

			/*                          INCR LIKEEP TO ACCOUNT FOR NEW SWORK */
			sw42bkX_.likeep += 1;
			if( backspX_.sw41p1 >= 100 ){
				tmplvl = backspX_.sw41p1 - 100;
				}
			else{
				tmplvl = sw42bkX_.likeep - backspX_.sw41p1;
				}
			cnt50 = 1;
			skip = 0;
			for( tmp=1; tmp <= tmplvl; tmp++ ){
				if( cnt50 < 4 && vwarg1X_.wc50el[cnt50-One] == -tmp ){
					skip += vwarg1X_.chng50[cnt50-One];
					cnt50 += 1;
					}
				else{
					skip += 1;
					}
				}
			if( sw42n == 0 ){
				flowckX_.i3save = vwarg2X_.i + skip;
				}
			else{
				flowckX_.i3save = vwarg1X_.isave + skip;
				}

			/*                          DIAGNOSTICS */

			if( diagsX_.longdi == 1 ){
				fprintf( _spec_fp, " ** SW68  -   COMPLETED SUCCESSFULLY ***\n" );
				/*                          PRINT SWORK */
				diagno(4);
				/*                          PRINT CLSNFO */
				if( tranidX_.tranid == 2 ){
					diagno(13);
					}
				else if( tranidX_.tranid == 3 ){
					diagno(9);
					}
				else if( tranidX_.tranid == 4 ){
					diagno(11);
					}
				/*                          PRINT SCON OF NEW ELEMENT */
				fprintf( _spec_fp, "\nSCONS    1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20\n" );
				fprintf( _spec_fp, "%3d", scnpos );
				fprintf( _spec_fp, "   " );
				for( e=1; e <= 20; e++ ){
					fprintf( _spec_fp, "%4d", sconX_.scon[scnpos-One][e-One] );
					}
				fprintf( _spec_fp, "\n" );
				}
			goto L_9999;
			}
		else{
			errvrsX_.err = 3;
			}
		}

	/*------                      ERRORS */

	if( diagsX_.deepdi == 1 || diagsX_.longdi == 1 ){
		fprintf( _spec_fp, "              ***** ERROR IN TXSW68 ****\n" );
		if( errvrsX_.err == 1 )
			{
			fprintf( _spec_fp, "   MAXIMUM NUMBER OF ELEMENTS ALREADY DEFINED (ELEMCT= %3d)\n", 
			  elemctX_.elemct );
			}
		if( errvrsX_.err == 2 )
			{
			fprintf( _spec_fp, "   NO SPACE IN SCON TABLE FOR ANOTHER ELEMENT (SCNPOS = %4d)\n", 
			  scnpos );
			}
		if( errvrsX_.err == 3 )
			{
			fprintf( _spec_fp, "   INVALID PARAMETER IN 68SWITCH.\n" );
			}
		if( errvrsX_.err == 4 )
			{
			fprintf( _spec_fp, "   ATTEMPT TO INSERT AN SWORK INTO POSITION: %3d\n   TO THE LEFT OF THE LAST ELEMENT LOADED AT: %3d\n", 
			  inspos, flowckX_.n6jims );
			}
		if( errvrsX_.err == 5 )
			{
			fprintf( _spec_fp, "   W/ CLAUSES TO BE MOVED THERE WILL BE TOO MANY ELEMNTS\n   ELEMCT = %3d  CLMCNT = %3d\n", 
			  elemctX_.elemct, clsmovX_.clmcnt );
			}
		if( errvrsX_.err == 6 )
			{
			fprintf( _spec_fp, "  NO MORE SPACE TO ADD ELEMENTS IN TRAN1. THE LIMIT OF \n   %3ld ELEMENTS HAS BEEN REACHED. CONSULT A PROGRAMMER.\n   ELEMENT COUNT =%3d  ORIGINAL COUNT = %3d\n", 
			  AD1MAX, elemctX_.elemct, elemctX_.origct );
			}
		fprintf( _spec_fp, "   ABORTING VTR PROCESSING. BACKSPACING TO 1ST \n   SWORK IN RULE OR LAST ELEMENT NOT LOADED.\n              ******************\n" );
		}
	errlog(pgmnam,9000,errvrsX_.err,10);
	/*                            BACKSPACE TO 1ST SWORK OF CURRENT RULE
	 *                            OR 1ST UNLOADED ELEMENT */
	if( vwarg2X_.i > flowckX_.n6jims ){
		flowckX_.i3save = vwarg2X_.i;
		}
	else{
		flowckX_.i3save = flowckX_.n6jims;
		}
	/*                            MAKE SURE NO MATCH AGAIN ON THIS RULE
	 *                            -36056000 INHIBITS REMATCH ON THIS RULE */
	vtrfX_.vtrf[vbdataX_.k3n-One] = -36;
	vtrfX_.vtrf[vbdataX_.k3n+1-One] = 56;
	vtrfX_.vtrf[vbdataX_.k3n+2-One] = 0;
	vtrfX_.vtrf[vbdataX_.k3n+3-One] = 999;
	/*                            SET K3N TO STOP VTR PROCESSING UPON RETURN */
	vwarg2X_.iz4 = vbdataX_.k3n + 2;



	/*------ */

L_9999:
	;
	return;
} /*end of function*/



/****************************************************************
 * SWI68P - Given insert position of new 68 sw element in the SWORK
 *	array find the corresponding insertion point in SWORKI, the
 *       source element array passed from PARSE to TRAN */

/*  FUNCTION:  SWI6P(SWKPOS,SWIPOS) */

/*  INPUT:  SWKPOS = insertion position in current SWORK */

/*  OUTPUT: SWIPOS = insertion position in SWORKI
 *                 = 0 , no insertion pos found. */


void /*FUNCTION*/ swi68p(long int swkpos, short int *swipos)
{
	static short int curscn, eosswk, swi, swixx, swk, xclbos, xclmid, xx, yy;
	static char pgmnam[9] = "SWI68P  ";
	swi = elemctX_.elemct + 1;
	swk = swkpos;
	eosswk = clsnfoX_.clndns[clsnfoX_.clcrnt-One];
	xclbos = 0;
	xclmid = 0;
	while( swk < eosswk ){
		curscn = sworkX_.swork[swk-One][4-One];
		/*	   	check if this swork a clause marker */
		for( yy=1; yy <= clsnfoX_.cltotl; yy++ ){
			if( curscn == clsnfoX_.clmrkr[yy-One] )
				goto L_1001;
			}
		goto L_1000;
L_1001:
		xclmid = yy;

		/*		find leftmost source element concated w/ curr swork */
L_1000:
		for( xx=sworkX_.phrbeg[swk-One]; xx <= sworkX_.phrend[swk-One]; xx++ ){
			swixx = sworkiX_.sc2swi[opadriX_.sconpi[xx-One]-One];
			if( swixx > 0 && swixx < swi )
				swi = swixx;
			}
		/*  		done if source elemnt found and not clmrker or clbos */
		if( (swi < elemctX_.elemct && xclmid == 0) && xclbos == 0 )
			break;
		/*		last swk was a clmarker, jump to 1st swork of clause. */
		if( xclmid > 0 ){

			swk = clsnfoX_.clbgns[xclmid-One];
			eosswk = clsnfoX_.clndns[xclmid-One];
			xclbos = 1;
			xclmid = 0;
			}
		else{
			swk += 1;
			xclbos = 0;
			}
		}


	*swipos = swi;
	if( *swipos > elemctX_.elemct ){
		*swipos = 0;
		}
	else if( *swipos == 1 ){
		*swipos = 2;

		}
	return;
} /*end of function*/

