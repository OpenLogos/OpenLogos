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
 *    1/31/97 jal: PARSE-Make SCONIN like SCONO arrays- append dont shift
 *    9/27/93 jal: No longer insert new element into JS array but append it
 *           to the end at ELEMCT and access it through SCOLNK.
 *    8/2/93  jal: remove code that accounted for stretches when calculating
 *                relative ptr K3P2, and right shifted it. This is now done
 *                by VTRFWR.
 *    6/21/93 jal: remove error if inserting to left of an element
 *                 already loaded.
 *    6/4/93 jal:  increment LIKEEP so it = current level of rule.
 *    6/4/93 jal:  fix to resetting of remaining relative pointers in
 *         the vtr.  See CAVEAT above.
 *    4/30/93 jal:  if pass1 of 2 passes, initialize the SCONIN
 *         array (saves SCON 2,11,13) for the new element.Includes
 *         shifting SCONIN array to make space for new element. */

/**************************************************************** */

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

EXTERN struct t_vtrfvlX_ {
	byte vtmnd, vtmrp, vtmel, vtmpp, vtmwk, vtmsl, vtmvc, vtmpu, vtmhi, vtmlo, vtmsw;
	}	vtrfvlX_;
EXTERN struct t_head26X_ {
	short int headwc, headty, headfr, headhd, headt2, headt3;
	}	head26X_;
EXTERN struct t_sw42bkX_ {
	short int im1sav, i3keep, likeep;
	}	sw42bkX_;
EXTERN struct t_vwarg1X_ {
	short int look50[3], chng50[3], wc50el[3], isave, omfrm;
	}	vwarg1X_;


void /*FUNCTION*/ sw68a()
{
	static short int clmcnt, cnt50, conv8x[ELMMAX], e, el, end8x, 
	  form68, frmsv, frstby, inse8x, inselm, inso8x, insoff, inspos, 
	  j, k3p2, k3p3, k3p4, k3p5, k3p6, k3p7, k3p8, lastby, newpos, 
	  nxtelm, oldpos, scn1168, scn1368, scn168, scn268, scnpos, si, 
	  sizeof_[20], skip, strflg, sw42n, swptr, tmp, tmparr[4*ELMMAX], 
	  tmplvl, tmpsiz, type68, wc50tm, wc68, xx, yy;
	static long int _d_m, _do0, _do1, _do2, _do3, _do4, _do5, shftsz, 
	  strtby, tempi4;
	static double _d_l;
	struct  {
		short int i, wc42m, wc50m, iz4, swx, s11prt, wc50ms;
		}	*_vwarg2X_ = (void*)&vwarg2X_;
	struct  {
		short int im1, i3save, i3str, n6, n6jim, n6jims, phrstr, phrlst, 
		  strphr;
		}	*_flowckX_ = (void*)&flowckX_;
	struct  {
		short int pcb, pcbwc, pcbswk;
		}	*_sw23bkX_ = (void*)&sw23bkX_;
	static char pgmnam[9] = "TXSW68  ";
	static short zero = 0;

	/*  DECLARATIONS for common input to  errlog().                                   */
	/*                            SCNPOS = POINTER TO SCON OF NEW SWORK */
	/*                            TMPARR - USED FOR SHIFTING SCRATCH SPACE
	 *                  ***** IF YOU CHANGE DIMENSION CHANGE "TMPSIZ" BELOW */
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
	else if(  elemctX_.elemct + (clmcnt*3) >= ELMMAX ){
		errvrsX_.err = 5;
		/*                            TRAN1 CAN ONLY ADD AD1MAX ELEMENTS CUZ
		 *                            THATS SIZE OF RESERVED SPACE IN SCONS */
		}
	else if(  elemctX_.elemct >= elemctX_.origct + AD1MAX ){
		errvrsX_.err = 6;

		/*                            CAN SCON TABLE HANDLE A NEW ELEMENTS? */
		}
	else if( prtscoX_.sct >= SCONY && elemctX_.elemct >= elemctX_.origct +  AD1MAX ){
		prtscoX_.scterr += 1;
		errvrsX_.err = 2;
		}
	else{
		/*                            CHECK FIRST PARAMETER */
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
			 *         DO 1050 XX=1,INSELM
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
				wc68 = swork1X_.swork1[im81X_.im81-k3p3-One][dct2X_.dct2[prctX_.js[sconX_.scolnk[im81X_.im81-k3p3-One]-
				  One]-One]-One];
				}
			/*                         TYPE */
			if( k3p6 > -70 ){
				type68 = k3p6;
				}
			else{
				type68 = swork1X_.swork1[im81X_.im81-k3p6-One][dct4X_.dct4[prctX_.js[sconX_.scolnk[im81X_.im81-k3p6-One]-
				  One]-One]-One];
				}
			/*                         FORM */
			if( k3p7 > -70 ){
				form68 = k3p7;
				}
			else{
				form68 = swork1X_.swork1[im81X_.im81-k3p7-One][dct3X_.dct3[prctX_.js[sconX_.scolnk[im81X_.im81-k3p7-One]-
				  One]-One]-One];
				}
			/*                         WC */
			if( k3p3 > -70 ){
				scn168 = k3p3;
				}
			else{
				scn168 = sconX_.scon[im81X_.im81-k3p3-One][1-One];
				}
			/*                         SUPERSET */
			if( k3p4 > -70 ){
				scn1368 = k3p4;
				}
			else{
				scn1368 = sconX_.scon[im81X_.im81-k3p4-One][13-One];
				}
			/*                         SET */
			if( k3p5 > -70 ){
				scn1168 = k3p5;
				}
			else{
				scn1168 = sconX_.scon[im81X_.im81-k3p5-One][11-One];
				}
			/*                         SUBSET */
			if( k3p6 > -70 ){
				scn268 = k3p6;
				}
			else{
				scn268 = sconX_.scon[im81X_.im81-k3p6-One][2-One];
				}
			/*                         FORMSAV */
			if( k3p7 > -70 ){
				frmsv = k3p7;
				}
			else{
				frmsv = formsaX_.formsv[sconX_.scolnk[im81X_.im81-k3p7-One]-
				  One];
				}
			/*-------
			 *             SHIFT ARRAYS RIGHT TO MAKE SPACE
			 *------- */


			/*                            BYTE TO START INSERT AT */
			strtby = (inspos*2) - 1;
			/*                            BYTES TO SHIFT RIGHT */
			shftsz = (swork1X_.phct - inspos + 1)*2;

			lmove(tmparr,1,sconX_.scolnk,strtby,shftsz);
			lmove(sconX_.scolnk,strtby+2,tmparr,1,shftsz);
			/*+                                             9/27/93  jal
			 *????   JS IS SHIFTED ONLY TO AVOID ALL THE CHANGES NECESSARY IN
			 *       THE REST OF TRAN TO ACCESS JS INDIRECTLY VIA SCOLNK.
			 *         EX:  JS(SCOLNK(N6JIM)) & NOT JS(N6JIM)
			 *      CALL LMOVE (TMPARR,1,JS,STRTBY,SHFTSZ)
			 *      CALL LMOVE (JS,STRTBY+2,TMPARR,1,SHFTSZ)
			 *                            SET SIZE OF TEMP HOLDING ARRAY */
			tmpsiz = 4*ELMMAX*2;
			/*                            SHIFT THE SWORK1 ARRAY */
			frstby = (inspos - 1)*30 + 1;
			lastby = swork1X_.phct*30;
			for( xx=lastby, _do0=DOCNT(xx,frstby,_do1 = -tmpsiz); _do0 > 0 ; xx += _do1, _do0-- ){
				shftsz = tmpsiz;
				if( xx - shftsz + 1 < frstby )
					shftsz = xx - frstby + 1;
				strtby = xx - shftsz + 1;
				lmove(tmparr,1,(short*)swork1X_.swork1,strtby,shftsz);
				lmove((short*)swork1X_.swork1,strtby+30,tmparr,1,
				  shftsz);
				}
			/*                            SHIFT THE INPUT SWORK1 ARRAY, BUT
			 *                            ONLY IF PASS1 OF 2 PASS STRATEGY */
			if( passesX_.passfl == 1 && passesX_.passct == 1 ){
				frstby = (inspos - 1)*30 + 1;
				lastby = elemctX_.parsct*30;
				for( xx=lastby, _do2=DOCNT(xx,frstby,_do3 = -tmpsiz); _do2 > 0 ; xx += _do3, _do2-- ){
					shftsz = tmpsiz;
					if( xx - shftsz + 1 < frstby )
						shftsz = xx - frstby + 1;
					strtby = xx - shftsz + 1;
					lmove(tmparr,1,(short*)swork1X_.swrk1i,strtby,
					  shftsz);
					lmove((short*)swork1X_.swrk1i,strtby+30,tmparr,
					  1,shftsz);
					}
				}
			/*                          SHIFT THE SCON ARRAY */
			frstby = (inspos - 1)*SCONX1*2 + 1;
			lastby = swork1X_.phct*SCONX1*2;
			for( xx=lastby, _do4=DOCNT(xx,frstby,_do5 = -tmpsiz); _do4 > 0 ; xx += _do5, _do4-- ){
				shftsz = tmpsiz;
				if( xx - shftsz + 1 < frstby )
					shftsz = xx - frstby + 1;
				strtby = xx - shftsz + 1;
				lmove(tmparr,1,(short*)sconX_.scon,strtby,shftsz);
				lmove((short*)sconX_.scon,strtby+SCONX1*2,tmparr,1,
				  shftsz);
				}

			/*                         shift SCONIN array if pass1 of 2 passes.
			 *      IF (PASSFL.EQ.1 .AND. PASSCT.EQ.1) THEN
			 *         FRSTBY = (INSPOS-1) * SCINSZ * 2 + 1
			 *         LASTBY = ELEMCT * SCINSZ * 2
			 *         DO 1120 XX = LASTBY, FRSTBY, -TMPSIZ
			 *            SHFTSZ = TMPSIZ
			 *            IF (XX-SHFTSZ+1.LT.FRSTBY) SHFTSZ = XX-FRSTBY+1
			 *            STRTBY = XX - SHFTSZ + 1
			 *            CALL LMOVE (TMPARR,1,SCONIN,STRTBY,SHFTSZ)
			 *            CALL LMOVE (SCONIN,STRTBY+SCINSZ*2,TMPARR,1,SHFTSZ)
			 *1120     CONTINUE
			 *      ENDIF
			 *------
			 *                           INCREMENT VITAL COUNTER VALUES
			 *------ */
			elemctX_.elemct += 1;
			swork1X_.phct += 1;
			if( passesX_.passfl == 1 && passesX_.passct == 1 )
				elemctX_.parsct += 1;

			/*                          UPDATE SW68CT TO ACCOUNT FOR THE NEW SWORK */
			swtc68X_.sw68ct += 1;
			/*                           SPACE WAS MADE IN SCONS AT INSPOS */
			scnpos = inspos;
			/*-------
			 *                           INSERT NEW ELEMENT VALS INTO SHIFTED ARRAYS
			 *                           PHRBEG,PHREND,PHRHED,SWORK
			 *-------
			 *                         WC */
			swork1X_.swork1[inspos-One][dct2X_.dct2[1-One]-One] = wc68;
			swork1X_.swork1[inspos-One][dct2X_.dct2[2-One]-One] = wc68;
			swork1X_.swork1[inspos-One][dct2X_.dct2[3-One]-One] = wc68;
			/*                         TYPE */
			swork1X_.swork1[inspos-One][dct4X_.dct4[1-One]-One] = type68;
			swork1X_.swork1[inspos-One][dct4X_.dct4[2-One]-One] = type68;
			swork1X_.swork1[inspos-One][dct4X_.dct4[3-One]-One] = type68;
			/*                         FORM */
			swork1X_.swork1[inspos-One][dct3X_.dct3[1-One]-One] = form68;
			swork1X_.swork1[inspos-One][dct3X_.dct3[2-One]-One] = form68;
			swork1X_.swork1[inspos-One][dct3X_.dct3[3-One]-One] = form68;
			/*                          TARGET ADDRESS */
			swork1X_.swork1[inspos-One][dctX_.dct[1-One]-One] = -k3p8;
			swork1X_.swork1[inspos-One][dctX_.dct[2-One]-One] = -k3p8;
			swork1X_.swork1[inspos-One][dctX_.dct[2-One]-One] = -k3p8;

			/*                         SET INPUT SWORK1 IF PASS1 OF 2 PASSES */
			if( passesX_.passfl == 1 && passesX_.passct == 1 ){
				/*                         WC */
				swork1X_.swrk1i[inspos-One][dct2X_.dct2[1-One]-One] = wc68;
				swork1X_.swrk1i[inspos-One][dct2X_.dct2[2-One]-One] = wc68;
				swork1X_.swrk1i[inspos-One][dct2X_.dct2[3-One]-One] = wc68;
				/*                         TYPE */
				swork1X_.swrk1i[inspos-One][dct4X_.dct4[1-One]-One] = type68;
				swork1X_.swrk1i[inspos-One][dct4X_.dct4[2-One]-One] = type68;
				swork1X_.swrk1i[inspos-One][dct4X_.dct4[3-One]-One] = type68;
				/*                         FORM */
				swork1X_.swrk1i[inspos-One][dct3X_.dct3[1-One]-One] = form68;
				swork1X_.swrk1i[inspos-One][dct3X_.dct3[2-One]-One] = form68;
				swork1X_.swrk1i[inspos-One][dct3X_.dct3[3-One]-One] = form68;
				/*                          TARGET ADDRESS */
				swork1X_.swrk1i[inspos-One][dctX_.dct[1-One]-One] = -k3p8;
				swork1X_.swrk1i[inspos-One][dctX_.dct[2-One]-One] = -k3p8;
				swork1X_.swrk1i[inspos-One][dctX_.dct[3-One]-One] = -k3p8;
				/*		add new final scon-sworki mapping */
				sworkiX_.sc2swi[elemctX_.elemct-One] = elemctX_.elemct;
				}
			/*-------
			 *                          AFFIX NEW ELEMENT TO END OF NON-SHIFT ARRAYS
			 *-------
			 *                          INITIALIZE ITS SCONS */
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
			/*+                                         4/30/93 jal
			 *                         initialize SCONIN for pass1 of 2 passes. */
			if( passesX_.passfl == 1 && passesX_.passct == 1 ){
				sconinX_.sconin[elemctX_.elemct-One][1-One] = scn268;
				sconinX_.sconin[elemctX_.elemct-One][2-One] = scn1168;
				sconinX_.sconin[elemctX_.elemct-One][3-One] = scn1368;
				}
			/*-                                         4/30/93 jal
			 *                         FORMSAV */
			formsaX_.formsv[elemctX_.elemct-One] = frmsv;
			if( passesX_.passfl == 1 && passesX_.passct == 1 )
				formsaX_.prsfrm[elemctX_.elemct-One] = frmsv;

			/*                          SET FALSE INPUT WORD INTO NWSTRG */
			memcpy(sent_wordsX_.source_word[elemctX_.elemct-One],"* SWITCH68 *    ",16);
			/*                          JS = POS CHOSEN FROM DICTIONARY ENTRY
			 *+-      JS(INSPOS) = 1                    9/27/93 jal */
			prctX_.js[elemctX_.elemct-One] = 1;
			/*                            SET HENUM, HASHCODE OF NEW ELEMENT TO
			 *                            IMPOSSIBLE MATCH AS DEFAULT. */
			hensavX_.henum2[elemctX_.elemct-One][1-One] = -1;
			hashX_.hashcd[elemctX_.elemct-One][1-One] = 0;

			/*                          RESET AFFECTED POINTERS */

			/*                           INCR 26SW HEAD PTR IF RIGHT OF NEW ELEMNT */
			if( head26X_.headhd >= inspos )
				head26X_.headhd += 1;
			/*                           N6 POINTS TO LAST ELEMENT OF THE RULE */
			if( inspos <= _flowckX_->n6 )
				_flowckX_->n6 += 1;
			/*                           PCBSWK - pts to SWRK1I of element that is a
			 *                           possible clause boundary. (see -23 and -21). */
			if( _sw23bkX_->pcb != 0 ){
				if( _sw23bkX_->pcbswk > inspos )
					_sw23bkX_->pcbswk += 1;
				}


			/*                MAKE ADJUSTMENTS IF STRETCHES FOLLOW THE NEW ELEMENT */


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
			if( !(_vwarg2X_->wc50ms == 0 && _vwarg2X_->wc42m == 0)
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

					/*winnt  add skip varialble instead on incrementin
					 *winnt  the loop counter
					 *                          LOOP THRU REST OF VTRF CHANGING ANY POINTERS */
					skip = 0;
					for( tmp=vbdataX_.k3n; tmp <= _vwarg2X_->iz4; tmp++ ){

						if( skip > 0 ){
							skip -= 1;
							}
						else if( vtrfX_.vtrf[tmp-One] == 999 ){
							break;
							/*                          Dont reset insert ptr for -68 switch
							 *                          SKIP first 2 parameters of this switch
							 *winnt bug IF (VTRFM(TMP).EQ.-68 .AND. VTRFM(TMP+1).EQ.1)THEN */
							}
						else if( vtrfX_.vtrf[tmp-One] == -68 && vtrfX_.vtrf[tmp+1-One] == 
						  1 ){
							/*            TMP=TMP+3 */
							skip = 2;
							/*winnt bug IF (VTRFM(TMP).NE.VTMRP) GOTO 1560 */
							}
						else if( vtrfX_.vtrf[tmp-One] == vtrfvlX_.vtmrp ){
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
										  tmp, vbdataX_.k3n, _vwarg2X_->iz4, 
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

			/*                          UPDATE LIKEEP TO ACCOUNT FOR THE NEW SWORK */
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
				_flowckX_->i3save = _vwarg2X_->i + skip;
				}
			else{
				_flowckX_->i3save = vwarg1X_.isave + skip;
				}

			/*                          DIAGNOSTICS */

			if( diagsX_.longdi ){
				fprintf( _spec_fp, " ** SW68  -   COMPLETED SUCCESSFULLY ***\n" );
				/*                          PRINT SWORK */
					fprintf( _spec_fp, "                S W O R K  R E C O R D S                               PAT         STEM                       OFL3I OFL4I TYPSAV\n" );
					for( swptr=1; swptr <= swork1X_.phct; swptr++ ){

						si = sconX_.scolnk[swptr-One];
						fprintf( _spec_fp, " %2d", swptr );
						fprintf( _spec_fp, " " );
						for( j=1; j <= 15; j++ ){
							fprintf( _spec_fp, "%3d", swork1X_.swork1[swptr-One][j-One] );
							}
						for( j=1; j <= 3; j++ ){
							fprintf( _spec_fp, "%3d", ptdiagX_.patno[si-One][j-One] );
							}
						for( j=1; j <= 3; j++ ){
							fprintf( _spec_fp, "%3d", ptdiagX_.stemno[si-One][j-One] );
							}
						fprintf( _spec_fp, "   " );
						fprintf( _spec_fp, "%16.16s", sent_wordsX_.source_word[si-One]);
						fprintf( _spec_fp, "%4d%3d%6d   \n", ofltabX_.ofl3i[si-One], 
						  ofltabX_.ofl4i[si-One], typsvX_.typsav[si-One] );
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

	if( diagsX_.longdi == 1 ){
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
			  inspos, _flowckX_->n6jims );
			}
		if( errvrsX_.err == 5 )
			{
			fprintf( _spec_fp, "   W/ CLAUSES TO BE MOVED THERE WILL BE TOO MANY ELEMNTS\n   ELEMCT = %3d  CLMCNT = %3d\n", 
			  elemctX_.elemct, clmcnt );
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
	if( _vwarg2X_->i > _flowckX_->n6jims ){
		_flowckX_->i3save = _vwarg2X_->i;
		}
	else{
		_flowckX_->i3save = _flowckX_->n6jims;
		}
	/*                            MAKE SURE NO MATCH AGAIN ON THIS RULE
	 *                            -36056000 INHIBITS REMATCH ON THIS RULE */
	vtrfX_.vtrf[vbdataX_.k3n-One] = -36;
	vtrfX_.vtrf[vbdataX_.k3n+1-One] = 56;
	vtrfX_.vtrf[vbdataX_.k3n+2-One] = 0;
	vtrfX_.vtrf[vbdataX_.k3n+3-One] = 999;
	/*                            SET K3N TO STOP VTR PROCESSING UPON RETURN */
	_vwarg2X_->iz4 = vbdataX_.k3n + 2;




L_9999:
	;
	return;
} /*end of function*/



