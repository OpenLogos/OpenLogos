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
	/*
	 *-                                                11/25/86    R1616RKH
	 *
	 *     Last Change: 12/06/86 R1616RKH
	 *
	 *        CALLED FROM 'T1SPSRC1' - SP SEARCH/MATCH PROGRAM
	 *           AND FROM 'TMATCH'  -  WC-TY-FRM TAGSET MATCHING PROG.
	 *
	 *        PERFORMS NEGATIVE WORD CLASS MATCHING LOGIC FOR TRAN1
	 *
	 *        A 'NEGATIVE WORD CLASS' IS A GENERAL TERM THAT REFERS TO
	 *        ALL THE CRITERIA (WC, TYPE, FORM, SUPER SET, ETC.) THAT
	 *        ARE TO BE CONSIDERED IN DETERMINING IF THIS IS A 'MATCH'.
	 *
	 *        NWRDCL - THE NEG. WORD CLASS TO USE IN MATCH TESTS
	 *                 (GUSNK2 IN 'SPSRC1' OR WCTST IN 'TMATCH')
	 *
	 *        RETFLG - INDICATES RETURN (0 = MATCH, 1 = NO MATCH)
	 */


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

EXTERN struct t_neg1X_ {
	short int negwc1[9][8];
	}	neg1X_;


void /*FUNCTION*/ nmatch(nwrdcl, i3, retflg)
long int nwrdcl, i3;
short int *retflg;
{
	static short int scon13, swkfrm;
	static short k9 = 0;
	static short wc = 0;
	static short retsw = 0;
	static short supset = 0;
	static short swktyp = 0;

	*retflg = 0;

	/*        ******* NEGATIVE WORD CLASS MATCHING ***** */

	k9 = 4*prctX_.js[sconX_.scolnk[i3-One]-One];
	/*+                                                11/25/86    R1616RKH
	 *     NWRDCL = GUSK2 */
	wc = swork1X_.swork1[i3-One][k9-One];
	swktyp = swork1X_.swork1[i3-One][k9+1-One];
	swkfrm = swork1X_.swork1[i3-One][k9+2-One];
	scon13 = sconX_.scon[i3-One][13-One];
	/*-                                                11/25/86    R1616RKH */

	if( srcflgX_.srcflg == 1 ){

		/*        * * *  GERMAN SOURCE  * * * */

		if( nwrdcl == -2 ){
			if( wc == 2 || wc == 12 )
				goto L_999;

			if( wc != 20 )
				goto L_800;
			if( scon13 == 3 )
				goto L_999;
			if( scon13 >= 5 && scon13 <= 7 )
				goto L_999;
			if( scon13 == 10 )
				goto L_999;
			goto L_800;

			}
		else if( nwrdcl == -3 ){
			if( wc != 20 )
				goto L_800;
			if( scon13 >= 3 && scon13 <= 8 )
				goto L_999;
			if( scon13 == 10 )
				goto L_999;
			goto L_800;
			/*+                                                         *R1064MBS */
			}
		else if( nwrdcl == -5 ){
			if( swkfrm != 1 )
				goto L_999;
			}
		}
	else if( srcflgX_.srcflg != 2 ){
		goto L_999;
		/*        * * *  ENGLISH SOURCE  * * * */

		}
	else if( nwrdcl == -2 ){
		if( wc == 2 || wc == 12 )
			goto L_999;

		if( wc == 20 ){
			supset = swktyp;
			if( supset >= 885 && supset <= 887 )
				goto L_999;

			if( scon13 != 0 )
				supset = scon13;
			if( supset == 8 || supset == 10 )
				goto L_999;
			goto L_800;

			}
		else if( wc != 18 ){
			goto L_800;
			}
		else if( swktyp == 101 ){
			goto L_999;
			}
		else{
			goto L_800;
			}

		/*        NEG. WORD CLASS TABLE - CONTAINS PERMITTED WORD CLASSES */

		}
	else if( nwrdcl == -3 ){
		if( wc != 1 )
			goto L_800;
		goto L_999;
		}

	match2(&wc,&neg1X_.negwc1[-nwrdcl-One][1-One],8,&retsw);
	if( retsw != 2 )
		goto L_800;

	/*        NO MATCH */
L_999:
	*retflg = 1;
	return;

	/*        A MATCH */
L_800:
	*retflg = 0;
	return;

} /*end of function*/

