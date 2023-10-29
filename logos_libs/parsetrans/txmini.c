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
	 *      08/30/91 JAL: DISALLOW RULE TO MATCH BEYOND END OF CURRENT CLAUSE
	/*         FOR THREE STREEEEEEEEEEETCH IN ONE RULE (TRIPLE STREEEEEETCH)
	 *         (ALLOWING FOR FUTURE EXPANSION, CURRENTLY HANDLES TWO) */
	/*               K7SAVE USED IN OVERID ROUTINE */
	/*             CBSP2 COMMON SET HERE, USED BY WHICSP AND TRAN4X */
	/*                     K7SAVE USED IN OVERID ROUTINE */
	/*             SPCB COMMON IS USED IN WHICSP ROUTINE */


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




void  txmini(
long int locflg,
short int *k7m,
short int *nptpx,
long int i,
short int *cb9
)
{
	static long int _l0;
	static short val9 = 9;
	static char pgmnam[9] = "TxMINI  ";
	static short k = 0;
	static short t = 0;
	static short x = 0;
	static short ct = 0;
	static short iz = 0;
	static short j2 = 0;
	static short k7 = 0;
	static short rs = 0;
	static short gbm = 0;
	static short im1 = 0;
	static short lt2 = 0;
	static short swc = 0;
	static short tyc = 0;
	static short ty2sav = 0;
	static short nswc = 0;
	static short numr = 0;
	static short exsp3 = 0;
	static short retsw = 0;
	static short tysav = 0;
	static short litemp = 0;
	static short negpas = 0;
	static short phtemp = 0;
	static short wc50ct = 0;
	static short k9;


	/*     If Mini option has not been specified then return */

	if( miniflX_.minifl != 1 ){
		*k7m = 0;
		return;
		}

		/*        K7X AND K7M CONTROL COMPARISION OF RULES
		 *        K7SAVE CONTROLS DELETION OF RULES */
	ct = 1;
	memset(cbsp3X_.k7save,'\0',sizeof(cbsp3X_.k7save));


	
	if( locflg == 2 ){
		/*        FOR SW42 RULES SEARCH */

		swc = ex42nX_.xswc;
		tyc = ex42nX_.xtyc;
		negpas = 4;
		sploopX_.st16 = 2;
		}



	else if( locflg == 3 ){

		/*          ENTRY FROM WC9 IN MAIN TRAN
		 *      Get the index for the wc9 rule and vtr. If it doesn't exist
		 *      set K7 and return.  Otherwise,  get the rule and the 1st vtr line
		 *      and jump back to 320 to set up for case of experimental win or
		 *      an override rule. */

		exsp3 = 3;
		*k7m = 0;

		idxval((short*)ADR(_l0,2),&val9,cb9,&k7,&numr);
		*k7m = k7;
		if( k7 == 0 )
			return;

		rulein((short*)ADR(_l0,2),&commdX_.matpos,spxxX_.spx,&k7,&retsw);

		/*                                 030,040 PHASE II BUG FIX  3/10/87 +
		 *     LOAD THE VTR'S FIRST RECORD JUST TO ESTABLISH WETHER THIS IS AN
		 *     OVERRIDE MINI RULE OR NOT.  IT WILL STILL BE FORMALLY LOADED IN
		 *     VTRFWR BY THE CALL TO VTRLD. */
		if( vtrin((short*)ADR(_l0,2),
				spzxX_.vtr, &spxxX_.spx[11], spxxX_.spx[11]) != 0 )
			return;

		/*          NEEDED BY MAIN TRAN IF EXPERIMENTAL WINS */
		sploopX_.li = spxxX_.spx[0];

		w50valX_.i3 = i + sploopX_.li;
		if(tranidX_.tranid == 1)
			prctjX_.j = prctX_.js[sconX_.scolnk[w50valX_.i3-One]-One];
		goto L_320;
		}

	
	
	else{

		/*         IF ILLEGAL WORD CLASS, CHANGE TO WC 01 TO PREVENT BOMBING */
		if(tranidX_.tranid == 1)
		{
			k9 = 4*prctX_.js[sconX_.scolnk[i-One]-One] - 1;

			if( swork1X_.swork1[i-One][k9+1-One] > 20 || 
				swork1X_.swork1[i-One][k9+1-One] < 1 ){
					swork1X_.swork1[i-One][k9+1-One] = 1;
					spinX_.nswork[i-One][1-One] = 1;
				}
			swc = swork1X_.swork1[i-One][k9+1-One];
			tyc = swork1X_.swork1[i-One][k9+2-One];
		}
		else
		{
			if( sworkX_.swork[i-One][1-One] < 1 || 
				sworkX_.swork[i-One][1-One] > 20 )
				sworkX_.swork[i-One][1-One] = 1;

				swc = sworkX_.swork[i-One][1-One];
				tyc = sworkX_.swork[i-One][2-One];
		}

		sploopX_.st16 = 1;
		tysav = tyc;
		negpas = 0;
	}





			/*      LOOP THROUGH ALL SP ENTRIES FOR THIS SWC */
L_100:
	if( tyc == 0 )
		goto L_300;


	/*        K7X AND K7M CONTROL COMPARISION OF RULES IN WHICSP ROUTINE */

	spcbX_.k7x = 0;
	*k7m = 0;
	w50valX_.wc50ti = 100;
	w50valX_.step50 = 0;

	*nptpx = 0;
	im1 = i - 1;
	if(tranidX_.tranid == 1)
			prctjX_.j = prctX_.js[sconX_.scolnk[i-One]-One];

	idxval((short*)ADR(_l0,2),&swc,&tyc,&commdX_.disam,&nswc);
	commdX_.disam += 1;
	rulein((short*)ADR(_l0,2),&commdX_.matpos,spxxX_.spx,&k7,&retsw);

L_120:
	if( diagsX_.deepdi == 1 )
		{
		fprintf( _spec_fp, " txmini: WORD CLASS %6d%6d%6d%6d%6d%6d%6d\n", 
		  swc, nswc, i, tyc, commdX_.disam, negpas, sworkX_.phct );
		}

	if( nswc == 0 )
		goto L_300;

	commdX_.matpos = 0;
	j2 = 0;
L_160:
	j2 += 1;

	cbsp2X_.wc5xm = 0;
	/*                   COUNT THE WC50 IN THIS RULE */
	wc50ct = 1;
	/*                   THERE MAY BE 0, 1, 2 OR 3 'WC50' (STRETCH) PER RULE
	 *                   SET FOR 0 OR 1 */
	for( t=0; t < 3; t++ ){
		cbsp2X_.chng5x[t] = 0;
		cbsp2X_.wc5xel[t] = -100;
		cbsp2X_.look5x[t] = -200;
		}

	/*                        NO WC50 CARRY OVER TO NEW RULE
	 *STROOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO */
	k7 = commdX_.disam - j2;
	rulein((short*)ADR(_l0,2),&commdX_.matpos,spxxX_.spx,&k7,&retsw);
	if( retsw == 99 )
		goto L_280;

	/*          GET LEVEL */
	sploopX_.li = spxxX_.spx[0];
	k = 0;
	if( sploopX_.li != 0 ){

		/*         IS THIS RULE TOO LONG FOR THE NUMBER OF NSWORKS LEFT?
		 *         'X' IS THE NUMBER OF STRETCH ELEMENTS ALLOWED
		 *         (3 FOR TRIPLE-STRETCH  3-13-84) */

		x = 3;

		/*        ADDITIONAL STRETCH CALCULATION FOR EOS CONDITION
		 *        ARE THERE ENOUGH SWORKS LEFT IN CURENT CLAUSE FOR THIS RULE? */

		if(tranidX_.tranid == 1)
		{
			phtemp = swork1X_.phct - im1;
		}
		else{
			phtemp = clsnfoX_.clndns[clsnfoX_.clcrnt-One] - im1;
		}
		if( phtemp <= 3 )
			x = phtemp - 1;
		litemp = sploopX_.li - x;

		if( negpas == 4 ){
			if( (litemp - 1) > phtemp )
				goto L_260;
			sploopX_.st16 = 2;
			}
		else if( litemp > phtemp ){
			goto L_260;
			}
		else{
			sploopX_.st16 = 1;
			}

		spcbX_.oflax = spxxX_.spx[10];


		/*        TRY TO MATCH FOR LI LEVELS, THREE ENTRIES IN EACH SUB-LEVEL */

	if( diagsX_.deepdi == 1 )
			{
			fprintf( _spec_fp, " AT 360 TRAN MINI - I3,ST16,K7:%5d%5d%5d", 
			  w50valX_.i3, sploopX_.st16, k7 );
			fprintf( _spec_fp, ",SPX:" );
			for( iz=1; iz <= 12; iz++ ){
				fprintf( _spec_fp, "%5d", spxxX_.spx[iz-One] );
				}
			fprintf( _spec_fp, "\n" );
			}

		w50valX_.i3 = i;
		w50valX_.wc50st = w50valX_.i3;

		if( diagsX_.deepdi == 1 )
			prctjX_.j = prctX_.js[sconX_.scolnk[w50valX_.i3-One]-One];



		/*         ******* MAIN LOOP, MOVES FROM ONE SWORK TO THE NEXT */

		/*         THE '760' LOOP IS REPLACED BY 'SPSRC3' SUBROUTINE
		 *         PERFORMS THE RULE MATCHING LOGIC FOR BOTH MINI AND MAIN TRANS */


		/*         CHECK NLOOP2 ARRAY TO PREVENT LOOPING (-36056) */
		if( i == nloop2X_.nloop2[1-One] ){
			for( gbm=2; gbm <= 21; gbm++ ){
				if( nloop2X_.nloop2[gbm-One] == 0 )
					break;
				if( spxxX_.spx[11] == nloop2X_.nloop2[gbm-One] )
					goto L_260;
				}
			}

		spsrch(2,spxxX_.spx,k7,spyxX_.ovrfx,spcbX_.oflax,
		  &cbsp2X_.wc5xm,cbsp2X_.wc5xel,cbsp2X_.look5x,cbsp2X_.chng5x,
		  &wc50ct,&retsw);
		if( errvrsX_.errlvl != 0 )
			return;

		/*         RETSW = 0 - RULE MATCHED
		 *         RETSW = 1 - NO MATCH FOUND */
		if( retsw != 0 )
			goto L_280;

		/*+ - - - - - - - - - - - - - -   PR 30,40,50 PROJECT 12/86
		 *     THE CALL TO VTRLD WILL DETERMINE IF THE RULE SHOULD NOT ALLOW THE
		 *     THE MATCH BECAUSE OF THE PRESENCE OF A -63001 SWITCH WITHOUT THE
		 *     APPROPRIATE 30 TABLE. */

		vtrld(ADR(_l0,2),&spxxX_.spx[11],&retsw, &spzxX_, NULL);
		if( retsw == 0 ){
					/*        A MATCH HAS BEEN FOUND FOR ALL LEVELS */
			*k7m = k7;

			/*                    NO LEVEL 1 DONT CARES */
			if( sploopX_.li <= 1 ){
				if( !((spxxX_.spx[2] != -1) || (spxxX_.spx[3] != -1)))
				{
					//         IS THIS A FAKE RULE ???
					if( spzxX_.vtr[0] == 999 )
						*k7m = 0;
				}
			}
			goto L_320;
		}

	}

	/* ---------------------------------------------------------------------- */

	/*     NO MATCH: 3 THINGS CAN HAPPEN
	 *     1. YOU'RE STILL LOOKING THROUGH A BLOCK OF RULES: CONTINUE
	 *     2. YOU'RE FINISHED WITH THIS BLOCK, MOVE ON TO THE NEXT BLOCK FOR
	 *        NEGPAS 2 OR 3. (SKIP STEP 2 + 3 IF YOU'RE IN A WC10 SEARCH)
	 *     3. YOU'RE FINISHED WITH THE ABOVE, NOW TRY THE NEG. TYPE RULES. */

L_260:
	commdX_.matpos = 0;

L_280:
	if( j2 < nswc )
		goto L_160;


	/*         TRY NEGATIVE TYPE */

L_300:
	while( negpas < 3 ){
		if( negpas == 0 ){
			negpas = 1;
			if(tranidX_.tranid == 1)
			{
				tyc = sconX_.scon[i-One][11-One];
				if( tysav != tyc ){
					if(tranidX_.tranid == 1)
					{
						ty2sav = tyc;
					}
					else
					{
						tysav = tyc;
					}
					goto L_100;
				}
			}
			else
			{
				tyc = sconX_.scon[sworkX_.phrhed[i-One]-One][11-One];
				if( tysav != tyc ){
					if(tranidX_.tranid == 1)
					{
						ty2sav = tyc;
					}
					else
					{
						tysav = tyc;
					}
					goto L_100;
				}
			}
		}
		else if( negpas == 1 ){
			negpas = 2;
			if(tranidX_.tranid == 1)
			{
				tyc = sconX_.scon[i-One][13-One];
				if( !(tyc == tysav || tyc == ty2sav) )
					goto L_100;
			}
			else
			{
				tyc = sconX_.scon[sworkX_.phrhed[i-One]-One][13-One];
				if( tyc != tysav )
					goto L_100;
			}
		}
		else{
				/*              NEG TYPE POINTERS FOR MINI PASS1, NUMNX1 = NUMBER OF
				 *              NEG TYPE RULES, LSTNX1 = POINTER TO LAST ONE. */
				nswc = numnx1(swc-One);
				commdX_.disam = lstnx1(swc-One) + 1;
				negpas = 3;
				goto L_120;
			}
		}

L_320:
	if( *k7m == 0 )
		return;

	//         I3X USED BY MAIN TRAN TO RESET IF EXPERIMENTAL WINS
	cbsp2X_.i3x = w50valX_.i3;
	cbsp2X_.lix = sploopX_.li;

	/*         USED IN WHICSP ROUTINE */
	spcbX_.k7x = k7;
	*nptpx = spxxX_.spx[11];
	spcbX_.lvl = spxxX_.spx[0];

	if( diagsX_.longdi == 1 ){
		fprintf( _spec_fp, "\n***** SPX MATCH STARTING AT %3d LEVEL %3d              ON ELEMENT%3djj        %6.6s", 
				i, spxxX_.spx[0], sworkX_.swork[i-One][3],passesX_.pssnam );

		print_tran_rule(_spec_fp, 2, *k7m);
		fprintf( _spec_fp, "\n" );
		}

	//         IS THIS A OVERIDE ???
	if( spzxX_.vtr[0] == 999 ){
		/*         YES, SAVE POINTER */
		cbsp3X_.k7save[ct-One] = k7;
		ct += 1;
		*k7m = 0;

		if( diagsX_.longdi == 1 )
			{
			fprintf( _spec_fp, " THIS IS AN OVERRIDE RULE\n" );
			}
		/*          GET NEXT RULE */
		goto L_260;
	}

	return;

} /*end of function*/

