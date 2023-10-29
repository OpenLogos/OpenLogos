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
program. If not, write to Globalware AG, Hospitalstraße 6, D-99817 Eisenach.

Linux port modifications and additions by Bernd Kiefer, Walter Kasper,
Deutsches Forschungszentrum fuer kuenstliche Intelligenz (DFKI)
Stuhlsatzenhausweg 3, D-66123 Saarbruecken
*/
	/*      THIS SWITCH INITIATES A SPECIAL SEARCH OF WC 10 FOR MATCHE
	 *      ON SPECIAL SEMANTIC RULES....HAS FOUR (4) PARAMETERS:
	 *      K3P1 IS WORDCLASS OF RULES TO BE SEARCHED, USUALLY WC 10
	 *      K3P2 IS BLOCK OR SEGMENT TYPE WITHIN K3P1 WHERE SESRCH BEG
	 *      K3P3 IS ELEMENT OF SWORK WHERE SEARCH IS TO BEGIN
	 *      K3P4 IS ELEMENT OF SWORK WHERE SEARCH IS TO END */
/* *         Rev 1.2   10/26/95 12:15:10   jonathan
 * *      Request Number:	PR000
 * *      Description:
 * *      save I3 before going to search for a wc10 rule.  If no rule is found then
 * *      I3 must be restored for use by TXSW67 in calling rule.
 * *
 * *> */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <fcrt.h>
#include "logoslib.h"
#include "trans.h"
#include "projexts.h"
#include <jbctrl.h>
#include "parsetrans_ext.h"




void /*FUNCTION*/ txsw42(
short *k7,
short *oflad
)
{
	static short int i3temp;
	static long int _n;
	static short ms = 0;
	static short k3p2 = 0;
	static short k3p4 = 0;
	static short disam = 0;
	static short im81sv = 0;
	static short i3sv42 = 0;
	static short lisv42 = 0;
	short int ovrflw[22];

	vbdataX_.k3n = vbdataX_.k3 + 5;
	if( vtrs42X_.sw42n != 1 )
		vwarg1X_.isave = vwarg2X_.i;
	vtrs42X_.sw42n = 1;

	/*    CALL36 WORKS WITH -36056001 */

	if( loopckX_.call36[1-One] == 0 ){
		loopckX_.call36[1-One] = loopckX_.nptpsv;
		if( minickX_.minifg == 1 )
			loopckX_.call36[2-One] = 1;
		}

	/* RESET  SEARCH RESULT FLAG */
	k3p2 = vtrfX_.vtrf[vbdataX_.k3+2-One];
	semargX_.k3p3 = vtrfX_.vtrf[vbdataX_.k3+3-One];
	k3p4 = vtrfX_.vtrf[vbdataX_.k3+4-One];

	if( vwarg2X_.wc50m == 1 )
		vwarg2X_.wc42m = 1;
	if( vwarg2X_.wc42m != 0 ){
		lmove(strw10X_.wc50sv,1,vwarg1X_.wc50el,1,6);
		lmove(strw10X_.ch50sv,1,vwarg1X_.chng50,1,6);
		}

	/* IDENTIFY PARAMETERS */
	ex42nX_.xswc = vbdataX_.k3p1;
	ex42nX_.xtyc = k3p2;
	flowckX_.i3str = semargX_.k3p3 + flowckX_.im1;

	/*             SAVE OLD END POSITIONS */
	lisv42 = sploopX_.li;
	i3sv42 = flowckX_.i3save;
	flowckX_.i3save = 0;
	sw42bkX_.im1sav = flowckX_.im1;
	im81sv = im81X_.im81;
	i3temp = w50valX_.i3;

	/*       GOTO SPECIAL SEARCH  OF A WC 10 RULE */

	getsp_x(k7, oflad, ovrflw);
	if( errvrsX_.errlvl == 0 ){

		/*  IF K7 AND K7M = 0, THEN NO MATCH ON EITHER MAIN OR MINI RULES
		 *  IF K7 = 1 AND K7M = 0, THEN ONLY MAIN RULE MATCH
		 *  IF K7 = 0 AND K7M = 1, THEN ONLY MINI RULE MATCH
		 *  IF K7 = 1 AND K7M = 1, THEN MAIN AND MINI RULE MATCH */


		if( diagsX_.deepdi == 1 && *k7 == 0 )
			{
			fprintf( _spec_fp, " SW42 %5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d\n", 
			  vtrs42X_.sw42n, ex42nX_.xswc, ex42nX_.xtyc, flowckX_.i3str, 
			  disam, minickX_.k7m, vwarg2X_.i, w50valX_.i3, cbsp2X_.i3x, 
			  sw42bkX_.i3keep, sw42bkX_.likeep );
			}

		/*                 WAS WC10 MATCH FOUND IN EXPERIMENTAL RULES?? */
		if( minickX_.k7m != 0 && diagsX_.longdi == 1 )
			{
			fprintf( _spec_fp, " EXPERIMENTAL WINS VIA SW42 %5d%5d%5d", 
			  minickX_.k7m, cbsp2X_.wc5xm, cbsp2X_.i3x );
			fprintf( _spec_fp, "       " );
			for(_n=0L; _n < sizeof(cbsp2X_.look5x)/sizeof(short); _n++)
				fprintf( _spec_fp, "%5d", cbsp2X_.look5x[_n] );
			for(_n=0L; _n < sizeof(cbsp2X_.chng5x)/sizeof(short); _n++)
				fprintf( _spec_fp, "%5d", cbsp2X_.chng5x[_n] );
			for(_n=0L; _n < sizeof(cbsp2X_.wc5xel)/sizeof(short); _n++)
				fprintf( _spec_fp, "%5d", cbsp2X_.wc5xel[_n] );
			fprintf( _spec_fp, "   \n" );
			}

		if( *k7 != 0 || minickX_.k7m != 0 ){

			getvtrX_.getvtr = 1;

			}
		else{

			vtrs42X_.sw42n = 0;
			if( vwarg2X_.wc42m != 0 ){
				vwarg2X_.wc50m = vwarg2X_.wc42m;
				vwarg2X_.wc42m = 0;
				lmove(vwarg1X_.wc50el,1,strw10X_.wc50sv,1,6);
				lmove(vwarg1X_.chng50,1,strw10X_.ch50sv,1,6);
				for( ms=1; ms <= 3; ms++ ){
					vwarg1X_.look50[ms-One] = vwarg1X_.wc50el[ms-One] - 80;
					}
				}

			flowckX_.i3save = i3sv42;
			flowckX_.im1 = sw42bkX_.im1sav;
			sploopX_.li = lisv42;
			im81X_.im81 = im81sv;
			vwarg2X_.i = vwarg1X_.isave;
			w50valX_.i3 = i3temp;
			}
		}
	return;
} /*end of function*/

