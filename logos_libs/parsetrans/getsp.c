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
	/*  CHANGES:
	 *    01/24/94 jal: limit number of tables that can be called per
	 *           rule to avoid infinite loop problem. Init here per rule.
	 *    08/31/93 jal: initialize the BH filler flags set by SW48
	 *     10/28/91 JAL: IF NO MATCH ON THIS ELEMENT EVEN IN DEFAULT RULES
	 *          THEN SET K7 AND K7M = 0 AND RETURN.
	 *     LAST CHG: 04/25/87 *R1515RKH*  Cell initialization for WC10 & Non
	 *          CHG: 04/17/87 *R1685RKH*  Change T1-4 SWORK limit from 50 to
	 *      CHG 12/30/86  PR304050: TARGET VTR TABLES
	 *      CHG 12/06/86 *R1616RKH:
	 *      CHG 08/22/86 *R1561DSD: 100 SCONS
	 *      CHG 12/04/85 */
	/*     THIS SUBROUTINE WILL FIND A RULE TO MATCH ON.
	 *     IT IS CALLED BY THE DRIVER PROGRAM AND THE 42 SWITCH.
	 *     IT TAKES CARE OF LOOKING UP THE MAIN AND THE MINI FILES FOR
	 *     RULES. */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*             RULE DECLARATIONS, NEEDED FOR SPXINDX3, PASS1 & 2 */
	/*                  switch 48 868 settings for filling target Black holes
	 *                     BHPT48 = direct ptr to SCON of targetted Black Hole
	 *                     BHCT48 = count of which BH to fill. */
	/*                        NXT LINE: 0GM 10/86  FIX -36056 FOR MINI/MAIN */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#define MS_F77
#include <fcrt.h>
#include "logoslib.h"
#include "trans.h"
#include "projexts.h"
#include "parsetrans.h"
#include <jbctrl.h>
#include "parsetrans_ext.h"

EXTERN struct t_spX_ *spX_ptr = &spX_;

#ifdef TRANSTAT
#include <fcntl.h>
EXTERN int rulestatfile = 0;
EXTERN int rulestat[];
EXTERN char *rulestatfilename;
#endif /* TRANSTAT */

void /*FUNCTION*/ getsp_x(short *k7,short *oflad, short ovrflw[])
{
	static long int _l0;
	short ictr,jctr;
	static char pgmnam[9] = "getsp_x ";
	static short zero = 0;
	static short t = 0;
	static short x = 0;
	static short gb = 0;
	static short gz = 0;
	static short j2 = 0;
	static short cb9 = 0;
	static short nswc = 0;
	static short retsw = 0;
	static short tysav = 0;
	static short litemp = 0;
	static short negpas = 0;
	static short phtemp = 0;
	static short ty2sav = 0;
	static short wc50ct = 0;

	int ret;

	if( vtrs42X_.sw42n == 1 )
		negpas = 4;
	semargX_.pntr9 = 0;

	vwarg2X_.i = flowckX_.i3str;

	if( diagsX_.deepdi == 1 ){
		fprintf( _spec_fp, " AT 240 NOLOOP VAL =  " );
		for( gz=1; gz <= 21; gz++ ){
			fprintf( _spec_fp, "%6d", loopckX_.noloop[gz-One] );
			}
		fprintf( _spec_fp, "\n" );
		fprintf( _spec_fp, " AT 240, NLOOP2 VAL =  " );
		for( gz=1; gz <= 21; gz++ ){
			fprintf( _spec_fp, "%6d", nloop2X_.nloop2[gz-One] );
			}
		fprintf( _spec_fp, "\n" );
		}
	/*+ Cell initialization for WC10 & Non WC10     RKH  04/25/87   R1515 */

	/*    Clear first ten cells of VBCELL array with each new element
	 *    if I has been incremented */

	if( loopckX_.noloop[1-One] != vwarg2X_.i && nloop2X_.nloop2[1-One] !=  vwarg2X_.i )
		zapit(vbdataX_.vbcell,20,(byte)zero);

	/*-                                             RKH  04/25/87   R1515
	 *            initialize the BH filler flags set by SW48 */
	sw48bhX_.bhpt48 = 0;
	sw48bhX_.bhct48 = 0;
	/*            init ctr to limit number of tables per rule 01/24/94 jal */
	tablctX_.tablct = 0;

	if( vtrs42X_.sw42n != 1 )
		minickX_.minifg = 0;

	if( vtrs42X_.sw42n != 1 ){
		loopckX_.call36[1-One] = 0;
		loopckX_.call36[2-One] = 0;
		/*                     NXT 4 LINES: 0GM 10/86  FIX -36056 FOR MINI/MAIN */
		spcompX_.idensp = 0;
		spcompX_.iden42 = 0;
		spcompX_.nptpm = 0;
		spcompX_.nptp42 = 0;
		/*    CLEAR NOLOOP ARRAYS IF I HAS BEEN INCREMENTED AND NO WC10 MATCH */
		if( loopckX_.noloop[0] != vwarg2X_.i && nloop2X_.nloop2[0] !=  vwarg2X_.i ){
			memset(loopckX_.noloop,'\0',sizeof(loopckX_.noloop));
			memset(nloop2X_.nloop2,'\0',sizeof(nloop2X_.nloop2));
			}
		}

	flowckX_.im1 = vwarg2X_.i - 1;
	im81X_.im81 = vwarg2X_.i - 81;

	/*                PREPARE FOR STREEEEEEEEEEETCH RULE */
	vwarg2X_.wc50m = 0;
	wc50ct = 0;

	if(tranidX_.tranid == 1)
	{
		prctjX_.j = prctX_.js[sconX_.scolnk[vwarg2X_.i-One]-One];

	/*     Set up NSWORK array in NSWORK format to use same SPSRCH
	 *     by all 4 TRANS */
		for( ictr=1; ictr <= swork1X_.phct; ictr++ ){
			jctr = 4*prctX_.js[sconX_.scolnk[ictr-One]-One];
			spinX_.nswork[ictr-One][1-One] = swork1X_.swork1[ictr-One][jctr-One];
			spinX_.nswork[ictr-One][2-One] = swork1X_.swork1[ictr-One][jctr+1-One];
			spinX_.nswork[ictr-One][3-One] = swork1X_.swork1[ictr-One][jctr+2-One];
			spinX_.nswork[ictr-One][4-One] = ictr;
			}
		spinX_.phrlim = swork1X_.phct;
		for( ictr=1; ictr <= spinX_.phrlim; ictr++ ){
			spinX_.phrase[ictr-One] = ictr;
			phrhdiX_.phrhed[ictr-One] = ictr;
			}
	}

	/*MINI
	 *           CAN WE FIND AN EXPERIMENTAL RULE???
	 *                  K7M WILL EQUAL 0 IF NO (EXP.) RULE FOUND
	 *                  NPTPX WILL BE EQ TO F13 BEFORE READ */

	if( vtrs42X_.sw42n == 1 ){
		/*         CAN WE FIND AN WC10 RULE IN THE MINI SP? */
		txmini(2,&minickX_.k7m,&loopckX_.nptpx,vwarg2X_.i,&cb9);
		if( errvrsX_.errlvl != 0 )
			return;
		}
	else{
		/*         CAN WE FIND A NON-WC10 RULE IN THE MINI SP? */
		txmini(1,&minickX_.k7m,&loopckX_.nptpx,vwarg2X_.i,&cb9);
		if( errvrsX_.errlvl != 0 )
			return;

		/* *********** DETERMINE SECTION OF SP RULES TO BE SEARCHED ************* */

		if(tranidX_.tranid == 1)
		{
			/*        RESET J (MAY HAVE BEEN SET IN TR1MINI) */
			prctjX_.j = prctX_.js[sconX_.scolnk[vwarg2X_.i-One]-One];

			/*    IF ILLEGAL WC CHANGE TO WC01 TO PREVENT BOMBING */
			if( swork1X_.swork1[vwarg2X_.i-One][4*prctjX_.j-One] < 1 ||
				swork1X_.swork1[vwarg2X_.i-One][4*prctjX_.j-One] > 20 ){
				swork1X_.swork1[vwarg2X_.i-One][4*prctjX_.j-One] = 1;
				spinX_.nswork[vwarg2X_.i-One][prctjX_.j-One] = 1;
				}

			ex42nX_.xswc = swork1X_.swork1[vwarg2X_.i-One][4*prctjX_.j-One];
			ex42nX_.xtyc = swork1X_.swork1[vwarg2X_.i-One][4*prctjX_.j+1-One];
		}
		else
		{
			/*    IF ILLEGAL WORD CLASS, CHANGE TO WC01 TO PREVENT BOMBING */
			if( sworkX_.swork[vwarg2X_.i-One][1-One] < 1 || sworkX_.swork[vwarg2X_.i-One][1-One] > 20 )
				sworkX_.swork[vwarg2X_.i-One][1-One] = 1;

			ex42nX_.xswc = sworkX_.swork[vwarg2X_.i-One][1-One];
			ex42nX_.xtyc = sworkX_.swork[vwarg2X_.i-One][2-One];
		}

		tysav = ex42nX_.xtyc;
		negpas = 0;
		}


		/*        LOOP THROUGH ALL SP ENTRIES FOR THIS SWC */

	while( TRUE ){
		if( ex42nX_.xtyc == 0 )
			goto L_8880;

		w50valX_.wc50ti = 100;
		w50valX_.step50 = 0;

		/*     READ IN PROPER INDEX FOR START AND NUMBER OF RULES */

		idxval((short*)ADR(_l0,1),&ex42nX_.xswc,&ex42nX_.xtyc, &commdX_.disam,&nswc);
		commdX_.disam += 1;

L_320:
		if( diagsX_.deepdi == 1 )
			{
			fprintf( _spec_fp, " WORD CLASS %5d%5d%5d%5d%5d%5d%5d\n", 
			  ex42nX_.xswc, nswc, vwarg2X_.i, ex42nX_.xtyc, commdX_.disam, 
			  negpas, vtrs42X_.sw42n );
			}

		if( nswc != 0 ){

			commdX_.matpos = 0;
			j2 = 0;
			while( TRUE ){
				j2 += 1;

				/*STR---IN -----------------------------------------------
				 *                  NO WC50 CARRY OVER TO NEW RULE */
				vwarg2X_.wc50m = 0;
				/*                  COUNT THE WC50 IN THIS RULE */
				wc50ct = 1;
				/*                  THERE MAY BE 0, 1, 2 OR 3 'WC50' (STRETCH) PER RULE
				 *                  SET HERE FOR 0 OR 1 */
				for( t=1; t <= 3; t++ ){
					vwarg1X_.chng50[t-One] = 0;
					vwarg1X_.wc50el[t-One] = -100;
					vwarg1X_.look50[t-One] = -200;
					}

				/*STR---OUT -------------------------------------------- */

				*k7 = commdX_.disam - j2;
				ret = rulein_ptr((short*)ADR(_l0,1),&commdX_.matpos,
					spX_ptr, k7, &retsw);


				if( retsw != 99 ) {

					sploopX_.li = spX_ptr->sp[1-One];
					if( sploopX_.li != 0 ){

						/*         IS THIS RULE TOO LONG FOR THE NUMBER OF NSWORKS LEFT?
						 *         'X' IS THE NUMBER OF STRETCH ELEMENTS ALLOWED
						 *         (3 FOR TRIPLE-STRETCH  3-13-84) */
						x = 3;

						/*        ADDITIONAL STRETCH CALCULATION FOR EOS CONDITION */

						if(tranidX_.tranid == 1)
						{
							phtemp = swork1X_.phct - flowckX_.im1;
						}
						else
						{
							phtemp = sworkX_.phct - flowckX_.im1;
						}
						if( phtemp <= 3 )
							x = phtemp - 1;
						litemp = sploopX_.li - x;

						if( vtrs42X_.sw42n == 1 ){
							if( (litemp - 1) > phtemp )
								goto L_8840;
							sploopX_.st16 = 2;
							}
						else if( litemp > phtemp ){
							goto L_8840;
							}
						else{
							sploopX_.st16 = 1;
							}
						*oflad = spX_ptr->sp[11-One];

						/*        TRY TO MATCH FOR LI LEVELS, THREE ENTRIES IN EACH SUB-LEVEL */

						w50valX_.i3 = vwarg2X_.i;
						w50valX_.wc50st = w50valX_.i3;


					if(tranidX_.tranid != 1)
					{
						if( vtrs42X_.sw42n == 1 ){
							w50valX_.i3 += sploopX_.st16 - 2;
							}
						else{
							w50valX_.i3 += sploopX_.st16 - 1;
							}
					}

						/*------------------------------------------------------------------
						 *      SP RULE MATCHING SUBROUTINE  SPSRC3 IS ABOUT TO BE CALLED */

						/*    CHECK NOLOOP ARRAY TO PREVENT LOOPING */

						if( vtrs42X_.sw42n == 1 ){
							if( vwarg1X_.isave != loopckX_.noloop[1-One] )
								goto L_380;
							}
						else if( vwarg2X_.i != loopckX_.noloop[1-One] ){
							goto L_380;
							}

						for( gb=2; gb <= 21; gb++ ){
							if( loopckX_.noloop[gb-One] == 0 )
								break;
							if( spX_ptr->sp[12-One] == loopckX_.noloop[gb-One] )
								goto L_8840;
							}

L_380:
						spsrch(1,spX_ptr->sp,*k7,ovrflw,
						  *oflad,&vwarg2X_.wc50m,vwarg1X_.wc50el,
						  vwarg1X_.look50,vwarg1X_.chng50,&wc50ct,
						  &retsw);
						/*         RETSW = 0 - RULE MATCHED
						 *         RETSW = 1 - NO MATCH FOUND */
						if( errvrsX_.errlvl != 0 )
							return;

						if( retsw != 0 )
							goto L_8860;

						/*+ - - - - - - - - - - - - - -   PR 30,40,50 PROJECT 12/86
						 *     THE CALL TO VTRLD WILL DETERMINE IF THE RULE SHOULD NOT ALLOW THE
						 *     THE MATCH BECAUSE OF THE PRESENCE OF A -63001 SWITCH WITHOUT THE
						 *     APPROPRIATE 30 TABLE. */

						vtrld(ADR(_l0,1),&spX_ptr->sp[12-One],&retsw, &vtrX_, NULL);

						if( retsw == 0 ){
							/*        A MATCH HAS BEEN FOUND FOR ALL LEVELS */

							/*        IS THIS AN OVERIDE RULE???
							 *        IF IT ISN'T, THEN YOU HAVE A MATCH. RETURN TO DRIVER. */
							/*                RETSW = 0 - NO OVERRIDE
							 *                RETSW = 1 - YES, AN OVERRIDE (MINI WINS OVER MAIN) */

							overid(*k7,spX_ptr->sp,ovrflw, &retsw);

							if( retsw == 0 ) {
#ifdef TRANSTAT
	rulestat[*k7]++;
#endif /* TRANSTAT */

								return;
							}

							/*        YES, CONTINUE SEARCH */
							if( diagsX_.longdi == 1 )
								{
								fprintf( _spec_fp, " THIS IS AN OVERIDE RULE \n" );
								}
							}
						}

L_8840:
					commdX_.matpos = 0;
					}

L_8860:
				if(tranidX_.tranid == 1)
					prctjX_.j = prctX_.js[sconX_.scolnk[vwarg2X_.i-One]-One];
				if( j2 >= nswc )
					break;
				}
			}


		/*    TRY NEGATIVE TYPE */

L_8880:
		while( negpas != 3 ){
			if( negpas == 0 ){
				negpas = 1;
				if(tranidX_.tranid == 1)
				{
					ex42nX_.xtyc = sconX_.scon[vwarg2X_.i-One][11-One];
					if( ex42nX_.xtyc != tysav )
						goto L_24002;
				}
				else
				{
					ex42nX_.xtyc = sconX_.scon[sworkX_.phrhed[vwarg2X_.i-One]-One][11-One];
					if( ex42nX_.xtyc != tysav )
						goto L_24002;
				}
			}
			else if( negpas == 1 ){
				negpas = 2;
				if(tranidX_.tranid == 1)
				{
					ex42nX_.xtyc = sconX_.scon[vwarg2X_.i-One][13-One];
					if( !(ex42nX_.xtyc == tysav || ex42nX_.xtyc == ty2sav) )
						goto L_24003;
				}
				else
				{
					ex42nX_.xtyc = sconX_.scon[sworkX_.phrhed[vwarg2X_.i-One]-One][13-One];
					if( !(ex42nX_.xtyc == tysav || ex42nX_.xtyc == ty2sav))
						goto L_24003;
				}
			}
			else{
				goto L_24004;
				}
			}
		break;

		/*        * * * IF NO MATCH VIA -42 SWITCH THIS TEST WILL RETURN
		 *                TO PROCESSING REST OF INITIAL VTR VIA -42 SWITCH  * * * */

L_24004:
		if( negpas == 4 )
			goto L_24005;

		/*              NEG TYPE POINTERS FOR MAIN PASS1, NUMNG1 = NUMBER OF
		 *              NEG TYPE RULES, LSTNG1 = POINTER TO LAST ONE. */
		nswc = numng1(ex42nX_.xswc-One);
		commdX_.disam = lstng1(ex42nX_.xswc-One) + 1;

		negpas = 3;
		goto L_320;

L_24002:
		ty2sav = ex42nX_.xtyc;
L_24003:
		;
		}


	/*    NO RULE MATCHED  ON THE CURRENT ELEMENT.  SERIOUS ERROR
	 *           SET: K7=0 ,  THE MAIN RULE PTR
	 *           SET: K7M=0 , THE MINI RULE PTR */

	if( diagsX_.longdi == 1  ){
		fprintf( _spec_fp, "\n***  ERROR IN GETSP():                  ****\n***  NO RULE MATCHED ON SWORK #%3d   ****\n***  SERIOUS ERROR SINCE DEFAULT SHOULD ****\n***  HAVE MATCHED. FORCING LOAD AND     ****\n***  CONTINUING.",
				vwarg2X_.i);
		if(tranidX_.tranid == 1)
		{
			  fprintf( _spec_fp, "****\n***  SWORK =%3d %3d %3d          ****\n", 
					  spinX_.nswork[vwarg2X_.i-One][0],
					  spinX_.nswork[vwarg2X_.i-One][1], 
					  spinX_.nswork[vwarg2X_.i-One][2]);
		}
		else
		{
			  fprintf( _spec_fp, "****\n***  SWORK =%3d %3d %3d          ****\n", 
					  sworkX_.swork[vwarg2X_.i-One][0],
					  sworkX_.swork[vwarg2X_.i-One][1], 
					  sworkX_.swork[vwarg2X_.i-One][2] );
		}
		errlog(pgmnam,8681,0,10);
		}
	errvrsX_.errlvl = 0;
	*k7 = 0;
	minickX_.k7m = 0;
	return;

L_24005:
	negpas = 0;
	*k7 = 0;
	return;
} /*end of function*/

#ifdef TRANSTAT
void init_rulestat() {
/*
 each of tranX thread has its own copies of "rulestat" array,
 "rulestatX" file, and "rulestatfile" file descriptor
*/
	int nhits, nhitrules, i;

	rulestatfile = _open(rulestatfilename, O_RDWR|O_CREAT|O_BINARY, 0666);
	if(rulestatfile==-1) {
		perror(rulestatfilename);
	} else {
/*
 if there was no file or it was somehow corrupted (wrong size) -
	zero statistics and write initialized array to the disk
*/
		if(_read(rulestatfile, rulestat, 20000*sizeof(int))==0) {
			_lseek(rulestatfile, 0, SEEK_SET);
			memset(rulestat, 0, 20000*sizeof(int));
			_write(rulestatfile, rulestat, 20000*sizeof(int));
		}
		_lseek(rulestatfile, 0, SEEK_SET);
		for(nhitrules=nhits=i=0;i<20000;i++) {
			if(rulestat[i]) {
				nhitrules++;
				nhits += rulestat[i];
			}
		}
		printf("\n**** initial %s nhitrules = %d, nhits = %d\n", 
			rulestatfilename, nhitrules, nhits);
	}
}

void write_down_rule_stat() {
	int nhits, nhitrules, i;

	_write(rulestatfile, rulestat, 20000*sizeof(int));
	for(nhitrules=nhits=i=0;i<20000;i++) {
		if(rulestat[i]) {
			nhitrules++;
			nhits += rulestat[i];
		}
	}

	printf("\n**** final %s nhitrules = %d, nhits = %d\n", 
		rulestatfilename, nhitrules, nhits);

	_close(rulestatfile);
}
#endif /* TRANSTAT */
