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
	/*  CHANGE: */
	/*    10/14/93 jal: add ERRLOG() to ERR41 if diags on.
	 *    6/1/93  jal:  Clean up the control flow a little.
	 *    5/27/93 jal:  Modify sw68 handling. Dont change the WC50 arrays in
	 *                  anticipation of the effects of a 68 SW.  The 68 switch
	 *                  will update rel ptrs in the remainder of the vtr, to
	 *                  account for the effects of
	 *                  right shifting stretch elements on the relative pointers.
	 *   *02/11/92*JAL: IF A 68SWITCH IN VTR, THEN SAVE THE INCOMING WC50EL
	 *                  AND LOOK50 ARRAYS.  SINCE SW68 ADDS AN SWORK, THESE
	 *                  ARRAYS MUST BE UPDATED FOR EACH 68 SWITCH TO ACCOUNT
	 *                  FOR ANY RIGHT SHIFTED STRETCHES.  RESTORE ORIGINAL
	 *                  WC50EL, LOOK50 BEFORE RETURNING.
	 *    09/24/91 JAL: REMOVE SETTING OF LI = LVLCHG
	 *    09/24/91 JAL: -68SW, ADJUST REL.PTRS. TO ACCOUNT FOR ANY STRETCHES
	 *                  ADJUST STRETCH MARKERS(WC50EL,LOOK50) TO ACCOUNT
	 *                  FOR NEW LOCATION OF SWORKS AFTER -68SW IS RUN.
	 *                  68SW ADDS AN SWORK WHICH = ADDING ELEMENT TO RULE.
	 *                  RESET WC50EL,LOOK50 B4 RETURNING SO ANY TABLES
	 *                  LOADED B4 68 SWITCH WILL BE ADJUSTED CORRECTLY.
	 *    01/16/90 JAL: IF -67001 EXISTS IN THE VTR ALLOW BACKSPACE TO
	 *                  INCLUDE STRETCHES EVEN WHEN CLMCNT= 0.
	 *    09/08/89 JAL: ALTER -41 TO ALLOW SKIPPING OVER STRETCHES
	 *                         FOR NEW "CLAUSE RELOCATION STRATEGY"
	 ** LAST CHANGE 87/06/08 LG002GBA Rphs3: phase 3 of 30.40.50 tables
	 *     LAST CHG: 06/05/87 *R1683RKH*  -44 SW COPY Function
	 *          CHG: 04/15/87 *R1713RKH*  SW26 Bug for Rel Ptrs
	 *      CHG 12/12/86 *B0435DSD2: SET -57 BRANCH POINTS BY VALUE NOT ORDER
	 *      CHG 06/17/86 *R1530DSD: EXPAND ELEMENTS AND REL. PTRS FOR STRETCH */
	/*        THIS PROGRAM CONSISTS OF THE 'VTRF' WRITE LOOP
	 *        FROM THE MAIN TRAN PROGRAM */
	/*        IT CONVERTS THE 'VTR' INPUT LINE(S) FROM THE SPRULE
	 *        INTO ONE 'VTRF' OUTPUT ARRAY */
	 /*+                                                        *Rphs3*GBA
	 *                 BSDFLT, IF 0 THEN I3SAVE = I3
	 *                 SW41P1, FIRST PARAMETER OF THE 41SWITCH */
	/*-                                                        *Rphs3*GBA */
	/*- - - - - - - - - - - - - - - - - - - - -    PR 30,40,50 PROJECT 12/86
	 *        'VTRFWR' REFERENCES FROM THIS COMMON: LI */
	/* - - - - - - - - - - - - - - PROJECT 30,40,50 TABLE */
	/*     COMMON /VWARG2/ I, WC42M, WC50M, IZ4, SWX, S11PRT
	 * - - - - - - - - - - - - - - PROJECT 30,40,50 TABLE
	 *        'VTRFWR' REFERENCES FROM THIS COMMON: I3 */
	/*         NEW FOR VTRBRNCH SUBROUTINE */
	/*+        FOR VTRFM, I.E. VTRF MODE INFO         05/27/86  *R1530DSD
	 *   This flags whether an array member of the vtrf is a rel pointer or
	 *    not.  This became necessary when stretch rules matched beyond the
	 *    16th element causing possible confusion between a -11 element and
	 *    an -11 switch. */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
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
#include "parsetrans.h"
#include <jbctrl.h>



void /*FUNCTION*/ vtrfwr()
{
	static short int add, gb, k2, parm, pos, ptr, retflg, retsw, starti, 
	  tot50, total, vtrtmp[26], xist67;
	static long int _l0, err41, tmpcnt;
	static short zero = 0;
	static char pgmnam[9] = "VTRFWR  ";
	static long tmp4 = 0;
	static long savfil = 0;
	static long vtrall = 0;
	static long vtrmov = 0;
	static short t = 0;
	static short x = 0;
	static short y = 0;
	static short iz = 0;
	static short ms = 0;
	static short om = 0;
	static short xx = 0;
	static short adj = 0;
	static short cbx = 0;
	static short chg = 0;
	static short exp = 0;
	static short iz2 = 0;
	static short iz3 = 0;
	static short om1 = 0;
	static short tmp = 0;
	static short tri = 0;
	static short val = 0;
	static short incr = 0;
	static short nexv = 0;
	static short pcnt = 0;
	static short tend = 0;
	static short iz4m2 = 0;
	static short limit = 0;
	static short lstel = 0;
	static short valp1 = 0;
	static short valp2 = 0;
	static short valp3 = 0;
	static short valp4 = 0;
	static short chklim = 0;
	static short flgs11 = 0;
	static short f28sav = 0;
	static short iz2sav = 0;
	static short lvlchg = 0;
	static short val22x = 0;
	static short val22y = 0;
	static short vtrsv[26]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	  0,0,0,0,0,0,0};

	flag32X_.prm32 = 0;
	flag32X_.ad32 = -1;

	/*          VTRF MODE MOSTLY UNDEFINED */
	zapit((short*)vtrfmX_.vtrfm,460,vtrfvlX_.vtmnd);

	zapit(vbdataX_.vbrkpt,198,(byte)zero);
	/*                     INIT -67001 EXISTENCE FLAG       *09/24/91*JAL* */
	xist67 = 0;

	/*   load the VTR  array if this is a word class 9 rules */
	if( semargX_.pntr9 > 1 ){
		vtrld(&vtrptrX_.vtrunt,&vtrptrX_.nptp,&retsw, &vtrX_, NULL);
		if( retsw == 0 ){
			if( vtrptrX_.vtrunt == 2 )
				memcpy(spzxX_.vtr, vtrX_.vtr, sizeof(vtrX_.vtr)); 
				diagno(2);
			}
		else{
			fprintf( _spec_fp, " WC09 RULE IS MISSING A 30 TABLE ENTRY\n" );
			errvrsX_.errlvl = 8;
			return;
			}
		}

	/*   expand the VTR array with any 30/40/50 tables called
	 *+                                                        *Rphs3*GBA
	 *     CALL VTREXP (VTRUNT,RETFLG)
	 *-                                                        *Rphs3*GBA */

	vwarg2X_.iz4 = 0;
	/*        NO CHANGE TO SW22 PARAMETER IF WC50EL NOT LOADED */

	zapit(vtrfX_.vtrf,920,(byte)zero);
	iz3 = 1;

	/*     WC50EL(3) = THE LOCATION OF THE CONTROL ELEMENT FOR EACH STRETCH
	 *     THESE ARE INTIALIZED TO -100
	 *     CHNG50(3) = THE AMOUNT OF STRETCH FOR EACH CONTROL/STOPPER COMBINA
	 *               = -1 IF MATCH IS ONLY ON THE STOPPER
	 *               =  0 IF MATCH IS ONLY ON ONE CONTROL AND THE STOPPER
	 *               =  1 IF MATCH IS ON TWO CONTROLS AND THE STOPPER ...
	 *               INTIALIZED TO -200
	 *     LOOK50(3) = -8X VERSION OF WC50EL ARRAY, INITIALIZED TO ZERO
	 *     I = THE BEGINNING SWORK OF THE FIRST ELEMENT IN THE CURRENT RULE.
	 *     I3 = SWORK AT WHICH TO START THE NEXT RULE. VTRFWR WILL CALCULATE
	 *          IN ANY BACKSPACING.
	 *     ISAVE = SAVED VALUE OF I OF THE RULE WHICH CALLS THE WC10 RULE
	 *     WC50SV(3) = SAVED VALUE OF WC50EL ARRAY FROM THE CALLING RULE
	 *     CH50SV(3) =   "       "    CHNG50   "         "        " */


	/*           IS THIS A WC10 RULE, AND DID THE CALLING RULE STRETCH AT ALL
	 *      WRITE(6,1) WC42M,I,ISAVE,I3STSV,WC50EL,WC50SV,CHNG50,CH50SV
	 *1     FORMAT(' LOCATION 1',4I4,/3I4,5X,3I4,/3I4,5X,3I4) */

	if( vwarg2X_.wc42m != 0 ){

		/*        CHECK TO SEE IF THE WC10 RULE SHOULD USE THE WC50EL AND CHNG50
		 *          VALUES OF THE CALLING RULE. IF NOT, DO NOTHING.
		 *        BEFORE THIS, TRANSFORM THE WC50EL TABLE TO REFLECT ANY OFFSET
		 *          INTRODUCED BY THE DISPLACEMENT IN THE 42 SWITCH CALL. */

		om1 = vwarg2X_.i - vwarg1X_.isave;
		for( om=1; om <= 3; om++ ){
			strw10X_.wc50sv[om-One] += om1;
			}
		lstel = -(strw10X_.i3stsv - vwarg2X_.i);

		/*           NOW COMPARE: */
		for( om=1; om <= 3; om++ ){
			/*   WAS THERE A STRECH AT THIS LEVEL I.E. FIRST, SECOND OR THIRD STRETCH */
			if( vwarg1X_.wc50el[om-One] == -100 )
				goto L_9001;
			if( vwarg1X_.wc50el[om-One] != strw10X_.wc50sv[om-One] || 
			  vwarg1X_.chng50[om-One] != strw10X_.ch50sv[om-One] )
				goto L_100;
			}
		goto L_60;
L_9001:
		if( lstel <= strw10X_.wc50sv[om-One] )
			goto L_100;

		/*         WE CAN USE THE STRETCH VALUES IN THE WC10 RULE THAT WERE USED
		 *         BY THE  CALLING RULE (AFTER BEING ADJUSTED FOR DISPLACEMENT) */

L_60:
		lmove(vwarg1X_.wc50el,1,strw10X_.wc50sv,1,6);
		lmove(vwarg1X_.chng50,1,strw10X_.ch50sv,1,6);

		/*     LASTLY, REGENERATE THE LOOK50 ARRAY */

		for( om=1; om <= 3; om++ ){
			vwarg1X_.look50[om-One] = vwarg1X_.wc50el[om-One] - 
			  80;
			}
		}

	/*      WRITE(6,2) WC50EL, CHNG50, LOOK50
	 *2     FORMAT(' LOCATION 2',3I4,4X,3I4,4X,3I4) */

L_100:
	iz2 = iz3;

	/*        'DO 8000' LOOP LOADS FAKE ARRAY, EXAMINING FOR -41
	 *        SWITCH, TO BE HANDLED NOW, BEFORE LOADING. */

	/*        SPECIAL HANDLING FOR -22 AND -42 SWITCHES ALSO */

	/*        MOVE FROM 'VTR' OF RULE MATCHED TO 'VTRF' WORK ARRAY */

	/*        FOR WC50 (STRETCH) - EXPAND THE 'VTRF' WHERE NECESSARY */

	/*        CHECK FOR -67001 SWITCH TO ALLOW BACKSPACING OVER
	 *        STRETCHES WHEN -67001 SW EXISTS AND NO CLAUSE HAS
	 *        YET BEEN IDENTIFIED(I.E. CLMCNT=0). */

	/*        IZ -   RELATIVE DISPLACEMENT IN INPUT 'VTR' ARRAY
	 *        IZ2 -  (LOCAL VARIABLE)
	 *        IZ3 -  DETERMINES STARTING POS. IN INPUT 'VTR' ARRAY
	 *        IZ4 -  ABSOLUTE DISPLACEMENT IN OUTPUT 'VTRF' ARRAY */


	for( iz=iz2; iz <= 460; iz++ ){

		val = vtrX_.vtr[iz-One];
		valp1 = vtrX_.vtr[iz+1-One];

		/*+                                                     *01/16/90*JAL* */
		if( val == -67 && valp1 == 1 )
			xist67 = 1;
		/*-                                                     *01/16/90*JAL*
		 *+    NOTE WHETHER REL PRT OR ELEMENT            06/17/86  *R1530DSD */
		if( val >= -90 && val <= -71 ){
			vtrfmX_.vtrfm[vwarg2X_.iz4+1-One] = vtrfvlX_.vtmrp;
			}
		else if( val >= -10 && val <= 9 ){
			vtrfmX_.vtrfm[vwarg2X_.iz4+1-One] = vtrfvlX_.vtmel;
			}
		/*-                                               06/17/86  *R1530DSD */

		if( !(val == 0 || val == 999) ){

			/*        IF -11 099 IS ENCOUNTERED, THIS MEANS THAT A VTR STRING
			 *        MUST BE READ IN AND INCLUDED INTO THIS 'VTRF'.
			 *        THE STRING IS SELECTED BASED ON THE VALUE OF THE
			 *        FORM CODE OF THE FIRST NSWORK IN THE MATCH (SAVED IN 'OMFRM'). */

			if( !(val != -11 || valp1 != -99) )
				goto L_9003;


			valp1 = vtrX_.vtr[iz+1-One];
			valp2 = vtrX_.vtr[iz+2-One];
			valp3 = vtrX_.vtr[iz+3-One];
			valp4 = vtrX_.vtr[iz+4-One];
			if( val == -42 )
				goto L_440;
			if( val == -22 )
				goto L_1000;
			if( val == -26 )
				goto L_2800;
			if( val == -44 )
				goto L_2900;
			if( val == -64 )
				goto L_420;
			if( val == -65 )
				goto L_420;
			if( val == -68 )
				goto L_600;
			/*               IF no stretches, then no need to adjust rel ptrs. */
			if( !(vwarg2X_.wc50ms == 0 && vwarg2X_.wc42m == 0)
			   ){




				/*-                                             RKH  06/05/87   R1683 */

				/*        IS THIS AN ELEMENT LOAD?  ?
				 *+                                               05/19/86  *R1530DSD */
				if( val >= 0 && val <= 9 ){
					vtrfmX_.vtrfm[vwarg2X_.iz4+1-One] = vtrfvlX_.vtmel;
					/*-                                               05/19/86  *R1530DSD */
					}
				else if( !(val > -1 || val < -10) ){
					/*+                                               05/19/86  *R1530DSD */
					vtrfmX_.vtrfm[vwarg2X_.iz4+1-One] = vtrfvlX_.vtmel;
					/*-                                               05/19/86  *R1530DSD */

					for( tri=1; tri <= 3; tri++ ){

						if( val == vwarg1X_.wc50el[tri-One] )
							goto L_9002;

						}

					/*        ONE OF THE WC50 ELEMENTS??? */
					tmp = 0;
					for( t=1; t <= 3; t++ ){
						if( val < vwarg1X_.wc50el[t-One] )
							tmp += vwarg1X_.chng50[t-One];
						}
					val -= tmp;

					/*        ONE OF THE WC50 ELEMENTS, OF THE -80 FORM ????
					 *                                                          *T0005 */
					}
				else if( !(val > -71 || val < -90) ){
					/*+    THIS IS A RELATIVE POINTER                 05/19/86  *R1530DSD */
					vtrfmX_.vtrfm[vwarg2X_.iz4+1-One] = vtrfvlX_.vtmrp;
					/*-                                               05/19/86  *R1530DSD
					 *                                                          *T0005 */
					tmp = 0;
					for( t=1; t <= 3; t++ ){
						if( val < vwarg1X_.look50[t-One] )
							tmp += vwarg1X_.chng50[t-One];
						}
					val -= tmp;
					}
				}
			}

		/*        VAL IS LOADED WITH 'VTR' ELEMENT
		 *        NPTP IS FILE POINTER FOR THE VTR FILE
		 *        IZ  -  NAMES CURRENT 'VTR' INPUT ELEMENT
		 *        IZ3 -  NEXT STARTING POS. OF '8000' LOOP
		 *        IZ4 -  NAMES CURRENT 'VTRF' OUTPUT ELEMENT */

		vwarg2X_.iz4 += 1;

		vtrfX_.vtrf[vwarg2X_.iz4-One] = val;

		/*        IS THIS 999 THE END OF THE COMPLETE VTR? OR IS ONLY THE END OF
		 *        THE SW11-99 VTR UNLOAD. IF SO, FINISH PROCESSING THE VTR. */

		if( val == 999 )
			goto L_9006;


		/*+       SET VBRKPT EVERY TIME A -57 SWITCH IS ENCOUNTERED */
		if( val == -57 )
			goto L_9005;
		/*- */
		if( val == -41 )
			goto L_9004;

		}
	goto L_9000;

	/*        SW42 */

L_440:
	;

	/*        CHANGE FIRST POINTER - IF NEC. */
	tmp = 0;
	for( t=1; t <= 3; t++ ){
		if( valp3 > -vwarg1X_.wc50el[t-One] )
			tmp += vwarg1X_.chng50[t-One];
		}
	valp3 += tmp;

	/*        CHANGE SECOND POINTER - IF NEC. */
	tmp = 0;
	for( t=1; t <= 3; t++ ){
		if( valp4 > -vwarg1X_.wc50el[t-One] )
			tmp += vwarg1X_.chng50[t-One];
		}
	valp4 += tmp;

	/*        LOAD VTRF WITH SW42 PARAMETERS */
	vwarg2X_.iz4 += 5;
	vtrfX_.vtrf[vwarg2X_.iz4-4-One] = val;
	vtrfX_.vtrf[vwarg2X_.iz4-3-One] = valp1;
	vtrfX_.vtrf[vwarg2X_.iz4-2-One] = valp2;
	vtrfX_.vtrf[vwarg2X_.iz4-1-One] = valp3;
	vtrfX_.vtrf[vwarg2X_.iz4-One] = valp4;

	/*+    PARAMETERS 3 AND 4 ARE SWORK POINTERS      05/19/86  *R1530DSD */
	vtrfmX_.vtrfm[vwarg2X_.iz4-4-One] = vtrfvlX_.vtmsw;
	vtrfmX_.vtrfm[vwarg2X_.iz4-1-One] = vtrfvlX_.vtmwk;
	vtrfmX_.vtrfm[vwarg2X_.iz4-One] = vtrfvlX_.vtmwk;
	/*-                                               05/19/86  *R1530DSD */

	iz3 = iz + 5;
	goto L_100;
	/*-                                                 5/27/93 jal */

	/*        VAL IS LOADED WITH 'VTR' ELEMENT
	 *        NPTP IS FILE POINTER FOR THE VTR FILE
	 *        IZ  -  NAMES CURRENT 'VTR' INPUT ELEMENT
	 *        IZ3 -  NEXT STARTING POS. OF '8000' LOOP
	 *        IZ4 -  NAMES CURRENT 'VTRF' OUTPUT ELEMENT */

	/*        ALTER -80 VALUES
	 *        BIG CHANGE TO VTRF LOAD */

	/*        SW22 */

L_1000:
	;
	/*                    THIS IS A SWITCH */
	vtrfmX_.vtrfm[vwarg2X_.iz4+1-One] = vtrfvlX_.vtmsw;

	/*        NEW STREETCH FOR SW22 */

	vwarg2X_.iz4 += 4;
	iz4m2 = vwarg2X_.iz4 - 2;
	vtrfX_.vtrf[vwarg2X_.iz4-3-One] = val;
	vtrfX_.vtrf[iz4m2-One] = valp1;
	if( valp1 > 900 )
		valp1 -= 900;
	/*                    account for no execute flag */
	if( valp1 > SW20NX )
		valp1 -= SW20NX;
	vtrfX_.vtrf[vwarg2X_.iz4-One] = valp3;

	if( !(valp2 > -71 || valp2 < -90) ){
		vtrfmX_.vtrfm[vwarg2X_.iz4-1-One] = vtrfvlX_.vtmrp;
		tmp = 0;
		for( t=1; t <= 3; t++ ){
			if( valp2 < vwarg1X_.look50[t-One] )
				tmp += vwarg1X_.chng50[t-One];
			}
		valp2 -= tmp;
		}

	vtrfX_.vtrf[vwarg2X_.iz4-1-One] = valp2;

	/*         *  *  *  *  *   START OF SW22 ELEMENT LOAD  *  *  *  *  * */

	pcnt = 2*valp1;
	iz3 = iz + 4 + pcnt;

	nexv = iz + 3;
	/*            set limit on the ADDITIONAL SEMWRKs that can be sent as
	 *            a result of stretches that containt >1 element. */
	limit = SW20NX - valp1 - 1;

	for( incr=1; incr <= pcnt; incr += 2 ){

		val22x = vtrX_.vtr[nexv+incr-One];
		val22y = vtrX_.vtr[nexv+incr+1-One];



		for( tri=1; tri <= 3; tri++ ){

			if( val22x == vwarg1X_.wc50el[tri-One] )
				goto L_9007;

			}

		/*        ELEMENT LEFT OR RIGHT OF STOPPER?? */

		if( val22x >= -10 ){

			tmp = 0;
			for( t=1; t <= 3; t++ ){
				if( val22x < vwarg1X_.wc50el[t-One] )
					tmp += vwarg1X_.chng50[t-One];
				}
			val22x -= tmp;
			}

		vwarg2X_.iz4 += 2;
		vtrfX_.vtrf[vwarg2X_.iz4-1-One] = val22x;
		vtrfX_.vtrf[vwarg2X_.iz4-One] = val22y;
		goto L_9008;

		/*        adjust number of 'SEMWRK' VALUES sent */
L_9007:
		chklim = vwarg1X_.chng50[tri-One];
		if( chklim > limit )
			chklim = limit;
		/*                   update number of sworks that can now be added */
		limit -= chklim;
		vtrfX_.vtrf[iz4m2-One] += chklim;

		if( vwarg1X_.chng50[tri-One] >= 0 ){

			/*        FOR MULTI-STRETCH, ADJUST FOR PREVIOUS STRETCHES ALSO */
			if( tri != 1 ){
				tend = tri - 1;
				for( t=1; t <= tend; t++ ){
					val22x -= vwarg1X_.chng50[t-One];
					}
				}

			vwarg2X_.iz4 += 2;
			vtrfX_.vtrf[vwarg2X_.iz4-1-One] = val22x;
			vtrfX_.vtrf[vwarg2X_.iz4-One] = val22y;

			if( vwarg1X_.chng50[tri-One] != 0 ){

				/*        EXPAND THE WC50 ELEMENT */
				for( exp=1; exp <= chklim; exp++ ){
					vwarg2X_.iz4 += 2;
					vtrfX_.vtrf[vwarg2X_.iz4-1-One] = val22x - exp;
					vtrfX_.vtrf[vwarg2X_.iz4-One] = val22y;
					}
				}
			}

L_9008:
		;
		/*        *  *  *  *   END OF SW22 ELEMENT LOAD   *  *  *  * */

		}
	goto L_100;

	/*    -26 SWITCH HAS POSITIVE REL POINTERS IN K3P1 - ADJUST FOR STRETCH */

L_2800:
	;
	vtrfmX_.vtrfm[vwarg2X_.iz4+1-One] = vtrfvlX_.vtmsw;
	valp2 = vtrX_.vtr[iz+2-One];
	valp3 = vtrX_.vtr[iz+3-One];
	valp4 = vtrX_.vtr[iz+4-One];

	if( valp1 > -71 || valp1 < -90 ){

		if( !(valp1 > 90 || valp1 < 71) ){
			/*-                                             RKH  04/15/87   R1713 */
			vtrfmX_.vtrfm[vwarg2X_.iz4+2-One] = vtrfvlX_.vtmpp;
			/*+ SW26 Bug for Rel Ptrs                       RKH  04/15/87   R1713 */

			/*-                                             RKH  04/15/87   R1713 */
			tmp = 0;
			for( t=1; t <= 3; t++ ){
				if( valp1 > -vwarg1X_.look50[t-One] )
					tmp += vwarg1X_.chng50[t-One];
				}
			valp1 += tmp;
			}
		}
	else{

		vtrfmX_.vtrfm[vwarg2X_.iz4+2-One] = vtrfvlX_.vtmrp;

		tmp = 0;
		for( t=1; t <= 3; t++ ){
			if( valp1 < vwarg1X_.look50[t-One] )
				tmp += vwarg1X_.chng50[t-One];
			}
		valp1 -= tmp;
		}
	/*+ SW26 Bug for Rel Ptrs                       RKH  04/15/87   R1713 */

	/*     Check Parms */


	/*     2nd element may be a rel pointer */

	if( !(valp2 > -71 || valp2 < -90) ){

		vtrfmX_.vtrfm[vwarg2X_.iz4+3-One] = vtrfvlX_.vtmrp;

		tmp = 0;
		for( t=1; t <= 3; t++ ){
			if( valp2 < vwarg1X_.look50[t-One] )
				tmp += vwarg1X_.chng50[t-One];
			}
		valp2 -= tmp;
		}


	/*     3rd element may be a rel pointer */

	if( !(valp3 > -71 || valp3 < -90) ){

		vtrfmX_.vtrfm[vwarg2X_.iz4+4-One] = vtrfvlX_.vtmrp;

		tmp = 0;
		for( t=1; t <= 3; t++ ){
			if( valp3 < vwarg1X_.look50[t-One] )
				tmp += vwarg1X_.chng50[t-One];
			}
		valp3 -= tmp;
		}


	/*     4th element may be a rel pointer */

	if( !(valp4 > -71 || valp4 < -90) ){

		vtrfmX_.vtrfm[vwarg2X_.iz4+5-One] = vtrfvlX_.vtmrp;

		tmp = 0;
		for( t=1; t <= 3; t++ ){
			if( valp4 < vwarg1X_.look50[t-One] )
				tmp += vwarg1X_.chng50[t-One];
			}
		valp4 -= tmp;
		}



	vwarg2X_.iz4 += 5;
	/*-                                             RKH  04/15/87   R1713 */
	vtrfX_.vtrf[vwarg2X_.iz4-4-One] = val;
	vtrfX_.vtrf[vwarg2X_.iz4-3-One] = valp1;
	vtrfX_.vtrf[vwarg2X_.iz4-2-One] = valp2;
	vtrfX_.vtrf[vwarg2X_.iz4-1-One] = valp3;
	vtrfX_.vtrf[vwarg2X_.iz4-One] = valp4;

	iz3 = iz + 5;
	goto L_100;

	/*     -44 Switch */

L_2900:
	;

	/*         -44 Switch can have positive rel pointers as parms 1 & 3 */

	vtrfmX_.vtrfm[vwarg2X_.iz4+1-One] = vtrfvlX_.vtmsw;

	valp2 = vtrX_.vtr[iz+2-One];
	valp3 = vtrX_.vtr[iz+3-One];
	valp4 = vtrX_.vtr[iz+4-One];

	/*         First element may be a rel pointer -8x or 08x type */

	if( valp1 > -71 || valp1 < -90 ){

		if( !(valp1 > 90 || valp1 < 71) ){
			vtrfmX_.vtrfm[vwarg2X_.iz4+2-One] = vtrfvlX_.vtmpp;
			tmp = 0;
			for( t=1; t <= 3; t++ ){
				if( valp1 > -vwarg1X_.look50[t-One] )
					tmp += vwarg1X_.chng50[t-One];
				}
			valp1 += tmp;
			}
		}
	else{

		vtrfmX_.vtrfm[vwarg2X_.iz4+2-One] = vtrfvlX_.vtmrp;

		tmp = 0;
		for( t=1; t <= 3; t++ ){
			if( valp1 < vwarg1X_.look50[t-One] )
				tmp += vwarg1X_.chng50[t-One];
			}
		valp1 -= tmp;
		}


	/*     3rd element may be a rel pointer (-8x or 08x type) */

	if( valp3 > -71 || valp3 < -90 ){

		if( !(valp3 > 90 || valp3 < 71) ){
			vtrfmX_.vtrfm[vwarg2X_.iz4+4-One] = vtrfvlX_.vtmpp;
			tmp = 0;
			for( t=1; t <= 3; t++ ){
				if( valp3 > -vwarg1X_.look50[t-One] )
					tmp += vwarg1X_.chng50[t-One];
				}
			valp3 += tmp;
			}
		}
	else{

		vtrfmX_.vtrfm[vwarg2X_.iz4+4-One] = vtrfvlX_.vtmrp;

		tmp = 0;
		for( t=1; t <= 3; t++ ){
			if( valp3 < vwarg1X_.look50[t-One] )
				tmp += vwarg1X_.chng50[t-One];
			}
		valp3 -= tmp;
		}



	/*     2nd & 4th element may be a VC */

	if( valp2 >= 100 && valp2 <= 120 )
		vtrfmX_.vtrfm[vwarg2X_.iz4+3-One] = vtrfvlX_.vtmvc;
	if( valp4 >= 100 && valp4 <= 120 )
		vtrfmX_.vtrfm[vwarg2X_.iz4+5-One] = vtrfvlX_.vtmvc;



	vtrfX_.vtrf[vwarg2X_.iz4+1-One] = val;
	vtrfX_.vtrf[vwarg2X_.iz4+2-One] = valp1;
	vtrfX_.vtrf[vwarg2X_.iz4+3-One] = valp2;
	vtrfX_.vtrf[vwarg2X_.iz4+4-One] = valp3;
	vtrfX_.vtrf[vwarg2X_.iz4+5-One] = valp4;

	vwarg2X_.iz4 += 5;

	iz3 = iz + 5;
	goto L_100;

	/*     SW64, SW65 */

	/*     DON'T ADJUST REL PTRS. WHICH ARE PARAMETERS IN
	 *                                        40- AND 50-TABLE CALLS
	 *     THEY WILL BE ADJUSTED *AFTER* THEY HAVE BEEN SUBSTITUTED IN
	 *                                        THE CALLED TABLE */

L_420:
	;
	vtrfX_.vtrf[vwarg2X_.iz4+1-One] = val;
	vtrfX_.vtrf[vwarg2X_.iz4+2-One] = valp1;
	vtrfX_.vtrf[vwarg2X_.iz4+3-One] = valp2;
	vtrfX_.vtrf[vwarg2X_.iz4+4-One] = valp3;
	vwarg2X_.iz4 += 4;
	iz3 = iz + 4;
	if( valp3 != 0 ){
		for( gb=1; gb <= valp3; gb++ ){
			vtrfX_.vtrf[vwarg2X_.iz4+1-One] = vtrX_.vtr[iz3-One];
			vwarg2X_.iz4 += 1;
			iz3 += 1;
			}
		}
	goto L_100;

	/*        SW68:
	 *           shift insertion pointer right
	 *           ADJUST RELATIVE POINTERS if stretches exist */


L_600:
	;
	vtrfX_.vtrf[vwarg2X_.iz4+1-One] = val;
	vtrfX_.vtrf[vwarg2X_.iz4+2-One] = valp1;
	/*                       SET INSERTION PTR TO INSERTION POSITION NOT
	 *                       THE PRECEDING RULE ELEMENT. */
	if( valp1 == 1 )
		vtrX_.vtr[iz+2-One] -= 1;
	/*                       adjust rel ptrs only if stretches exist */
	if( vwarg2X_.wc50ms == 0 && vwarg2X_.wc42m == 0 ){
		for( pos=2; pos <= 8; pos++ ){
			vtrfX_.vtrf[vwarg2X_.iz4+pos+1-One] = vtrX_.vtr[iz+pos-One];
			}
		}
	else{
		/*                       PARAMETERS 2-8 CAN BE REL PTRS */
		for( pos=2; pos <= 8; pos++ ){
			parm = vtrX_.vtr[iz+pos-One];
			if( parm < -70 && parm > -91 ){
				add = 0;
				for( t=1; t <= 3; t++ ){
					if( parm < vwarg1X_.look50[t-One] )
						add += vwarg1X_.chng50[t-One];
					}
				vtrfX_.vtrf[vwarg2X_.iz4+pos+1-One] = parm - add;
				vtrfmX_.vtrfm[vwarg2X_.iz4+pos+1-One] = vtrfvlX_.vtmrp;
				}
			else{
				vtrfX_.vtrf[vwarg2X_.iz4+pos+1-One] = parm;
				}
			}
		}

	vwarg2X_.iz4 += 9;
	iz3 = iz + 9;
	goto L_100;

	/*        LOAD STRING OF ELEMENTS
	 *        ELEMENT IS DROPPED IF CHNG50 IS NEG */
L_9002:
	if( vwarg1X_.chng50[tri-One] >= 0 ){

		vwarg2X_.iz4 += 2;
		vtrfX_.vtrf[vwarg2X_.iz4-1-One] = val;

		/*        WC50EL HAS BEEN LOADED */
		vtrfX_.vtrf[vwarg2X_.iz4-One] = valp1;

		/*        NO CHANGE TO VTR IF CHNG50 IS ZERO */
		if( vwarg1X_.chng50[tri-One] != 0 ){
			chg = vwarg1X_.chng50[tri-One];

			for( adj=1; adj <= chg; adj++ ){
				vwarg2X_.iz4 += 2;
				vtrfX_.vtrf[vwarg2X_.iz4-1-One] = val - adj;
				vtrfX_.vtrf[vwarg2X_.iz4-One] = valp1;
				}
			}
		}

	/*        RESTART 8000 LOOP */
	iz3 = iz + 2;
	goto L_100;
	/*+                                                     *01/18/90*JAL* */

	/*        FALL THRU TO HERE MEANS -41 SWITCH ENCOUNTERED.
	 *        'BACKSPACE' NO. OF SWORK ELEMENTS SET DOWN BY
	 *        -41 SW. PARAMETER (IZ+1) */

	/*        I3 INDICATES ON WHICH SWORK TO START NEXT SEARCH.
	 *                    ** NOTE **
	 *        IF A STRETCH ELEMENT EXISTS IN THE CURRENT RULE THEN
	 *        I3 IS SET TO FIRST SWORK ELEMENT OF THE FIRST STRETCH.
	 *        OTHERWISE IT IS SET TO THE SWORK IMMEDIATELY FOLLOWING THE
	 *        ONE THAT MATCHED THE LAST ELEMENT OF THE RULE. */

	/*        ALLOW PARSING OVER STRETCH ELEMENTS FOR WHEN IN CLAUSE
	 *        MOVE STATE ONLY. */

L_9004:
	err41 = 0;
	backspX_.sw41p1 = vtrX_.vtr[iz+1-One];

	if( backspX_.sw41p1 < 100 ){
		lvlchg = sploopX_.li - backspX_.sw41p1;
		}
	else{
		lvlchg = backspX_.sw41p1 - 100;
		}
	/*                            FIND START OF ORIGINAL RULE.
	 *                            FOR BACKSPACING, STARTING POINT OF A MATCH
	 *                            IS GOVERNED BY ORIGINAL CALLING RULE
	 *                            IN CASE OF WC10 RULE FROM 42 SWITCH.
	 *                            STARTI - ORIGINAL START POINT (LOCAL VAR). */
	starti = vwarg2X_.i;
	if( vtrs42X_.sw42n != 0 )
		starti = vwarg1X_.isave;
	/*                            BYPASS CLAUSE MUCK IN TRAN3,TRAN4 */
	if( tranidX_.tranid == 1 || tranidX_.tranid > 2 ){
		/*                          I IS STARTING SWORK OF THE RULE */
		w50valX_.i3 = starti + lvlchg;

		/*                           IF NOT IN CLAUSE MOVE STATE:
		 *                           ILLEGAL TO ADVANCE OVER STRETCHES.
		 *                           USE I3 ASSUMPTIONS ABOVE TO TEST. */
		}
	else if( clsmovX_.clmcnt == 0 && xist67 == 0 ){
		/*                           -41 10X TYPE:
		 *                           STRETCHES ALLOWED BUT FINAL I3 VALUE
		 *                           MUST BE .LE. PRE BACKSPACE I3 VALUE. */
		if( backspX_.sw41p1 >= 100 ){
			if( starti + lvlchg > w50valX_.i3 ){
				/*                           -41 00X TYPE:
				 *                           THERE CAN BE NO STRETCHES IN ENTIRE RULE
				 *                           I3 MUST ACCOUNT FOR THE WHOLE RULE. */
				err41 = 1111;
				goto L_6900;
				}
			}
		else if( w50valX_.i3 < starti + sploopX_.li ){
			err41 = 2222;
			goto L_6900;
			}
		/*                          I IS STARTING SWORK OF THE RULE */
		w50valX_.i3 = starti + lvlchg;

		/*                          CLAUSE MOVE STATE:
		 *                          TO CALCULATE I3 LOOP THROUGH ALL ELEMENTS
		 *                          IN THE RULE WE WILL ADVANCE OVER AND
		 *                          ACCOUNT FOR  "HIDDEN" STRETCH
		 *                          IN ANY STRETCH ELEMENTS INCLUDED.
		 *                          IF THIS IS A WC10 RULE USE ORIGINAL
		 *                          STRETCH VALUES IN SAVE ARRAYS. */
		}
	else{
		total = 0;
		tot50 = 1;
		for( ptr=1; ptr <= lvlchg; ptr++ ){
			tmp4 = vwarg1X_.wc50el[tot50-One];
			if( vtrs42X_.sw42n != 0 )
				tmp4 = strw10X_.wc50sv[tot50-One];
			/*                          IS THIS A WC50 STRETCH ELEMENT */
			if( tmp4 == -ptr ){
				tmpcnt = vwarg1X_.chng50[tot50-One];
				if( vtrs42X_.sw42n != 0 )
					tmpcnt = strw10X_.ch50sv[tot50-One];
				/*                          ACCOUNT FOR HIDDEN STRETCH ELEMENTS */
				total += tmpcnt + 1;
				tot50 += 1;
				/*                          THERE ARE AT MOST 3 STRETCHES: DONE LOOP */
				if( tot50 > 3 )
					goto L_9009;
				}
			else{
				/*                          NOT A STRETCH ELEMENT, SKIPPING 1 ELEMENT. */
				total += 1;
				}
			}
		goto L_6150;
L_9009:
		total += lvlchg - ptr;
L_6150:
		;

		/*                          'I' IS STARTING POINT OF MATCH */
		w50valX_.i3 = starti + total;

		}
	sploopX_.li = w50valX_.i3 - starti;


	/*                             REPORT ANY -41SW ERRORS */
L_6900:
	;
	if( err41 > 0 ){
		if( opswX_.sw[3-One] == 1 || opswX_.sw[14-One] == 1 ){
			fprintf( _spec_fp, "              ***** ERROR ******\n   ATTEMPT TO BYPASS A STRETCH IN NORMAL PARSING.\n   CHECK -41SW.  BACKSPACING FULLY & CONTINUING.\n   ERROR IN VTRFWR.\n              ******************\n" );
			errlog(pgmnam,err41,tmp4,10);
			}
		else{
			errlog(pgmnam,err41,tmp4,3);
			}
		/*                          SET UP FULL BACKSPACE & CONTINUE */
		w50valX_.i3 = vwarg2X_.i;
		if( vtrs42X_.sw42n != 0 )
			w50valX_.i3 = vwarg1X_.isave;
		sploopX_.li = 0;
		}
	/*-                                                     *01/18/90*JAL* */

	vwarg2X_.iz4 -= 1;
	/*+                                                        *Rphs3*GBA
	 *   IF BACKSPACE IS A DEFAULT, THEN OVERRIDE IT. */
	if( backspX_.bsdflt == 0 )
		flowckX_.i3save = w50valX_.i3;
	/*   MARK IT AS NOT A DEFAULT. */
	backspX_.bsdflt = 1;
	/*-                                                        *Rphs3*GBA */

	/*        SETTING IZ3 TO IZ + 2 ENSURES WE WONT LOAD -41SW AND ITS
	 *        PARAMETER INTO 'VTRF' AFTER RETURN FROM -41 PROCESSING */
	iz3 = iz + 2;
	goto L_100;

	 /*+ REP 779 780 *SET -57 BRANCH POINTS BY VALUE   12/12/86  *B0435DSD2 */
L_9005:
	vbdataX_.vbrkpt[valp1-One] = vwarg2X_.iz4;
	/*- REP 779 780 *SET -57 BRANCH POINTS BY VALUE   12/12/86  *B0435DSD2 */
	iz3 = iz + 1;
	goto L_100;

	/*    FOR VTR BRANCHING:
	 *    WHEN '999' IS FOUND - SET LAST VBRKPT TO ITS POSITION */

L_9006:
	vbdataX_.vbrkpt[99-One] = vwarg2X_.iz4;
	if( flgs11 != 1 )
		goto L_9000;
	vwarg2X_.iz4 -= 1;
	vtrptrX_.vtrunt = savfil;
	iz3 = iz2sav;
	vtrptrX_.nptp = f28sav;
	flgs11 = 0;
	lmove(vtrX_.vtr,1,vtrsv,1,52);
	vwarg2X_.s11prt = 1;
	goto L_100;

	/*        SAVE THE VTR STRING CURRENTLY BEING PROCESSED.
	 *        ALSO SAVE WHERE WE WILL BEGIN PROCESSING NEXT
	 *        AND SET THE 'FLGS11' FLAG. */

L_9003:
	lmove(vtrsv,1,vtrX_.vtr,1,52);
	iz2sav = iz + 2;
	f28sav = vtrptrX_.nptp;
	flgs11 = 1;

	/*        SAVE THE INFO OF WHICH VTR FILE YOU WERE READING FROM:
	 *        MINI OR MAIN VTR. */
	savfil = vtrptrX_.vtrunt;
	/*        THE SW11-99 VTR STRINGS ARE ONLY HELD IN THE MAIN VTR. */
	vtrptrX_.vtrunt = 1;


	/*+                                                *B1261JGB
	 *        IS THERE SUCH A FORM? IF NO, USE DEFAULT */

	frmarrX_.frmarr[1-One] = 1;
	for( gb=1; gb <= 20; gb++ ){
		om = 21 - gb;

		if( s11frmX_.s11frm[om-One][1-One] == vwarg1X_.omfrm )
			goto L_300;
		if( s11frmX_.s11frm[om-One][1-One] != 0 ){
			frmarrX_.frmarr[2-One] = vwarg1X_.omfrm;

			/*        FORM VALUE OMFRM MUST BE SATISFIED IN NOUN FORM TABLE
			 *        IN ORDER FOR A XGEREN2H VTR TO BE INSERTED */

			k2 = 4;
			formod(2,s11frmX_.s11frm[om-One][1-One],k2,w50valX_.i3,&retflg);
			if( retflg == 1 )goto L_300;
			}
		}

	/*        NO MATCH, USE DEFAULT */
	om = 1;

	/*        READ IN THE DESIRED VTR RECORD. */
L_300:
	vtrptrX_.nptp = s11frmX_.s11frm[om-One][2-One];
//cat-102	vtrin((short*)&vtrptrX_.vtrunt,vtrtmp,&vtrptrX_.nptp);
//cat-102 couldn't trace back rule number, assuming that this call
//cat-102 gets 1st vtr potion, and all vtrs are loaded by vtrld somewhere else
	vtrin((short*)&vtrptrX_.vtrunt,vtrtmp, &vtrptrX_.nptp, vtrptrX_.nptp);
	if( errvrsX_.errlvl != 0 )
		return;

	lmove(vtrX_.vtr,1,vtrtmp,1,52);

	if( opswX_.sw[3-One] == 1 )
		{
		fprintf( _spec_fp, " FORM AND -11-99 VTR\n %4d", s11frmX_.s11frm[om-One][1-One] );
		fprintf( _spec_fp, "    " );
		for( gb=1; gb <= 26; gb++ ){
			fprintf( _spec_fp, "%4d", vtrtmp[gb-One] );
			}
		fprintf( _spec_fp, "\n" );
		}

	/*        NOW BRANCH BACK INTO THE MAIN LOOP AND BEGIN INCORPORATING
	 *        THE SW11-99 VTR STRING INTO 'VTRF'.
	 *        AFTER THAT, PICK UP WHERE YOU LEFT OFF.
	 *        ALSO PRINT OUT VTRF TO SHOW THE CONTENTS THE STRING. */

	iz3 = 1;
	goto L_100;

L_9000:
	;

	return;

} /*end of function*/

