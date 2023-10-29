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
	 *     LAST CHG: 04/17/87 *R1685RKH*  Change T1-4 SWORK limit from 50 to
	 *      CHG 08/24/86 *R1561DSD: 100 SCONS
	 *      CHG 11/23/85
	 *
	 *        CALLED FROM 'SPSRC4' (P0616) - SP SEARCH/MATCH PROGRAM
	 *           AND FROM 'TMATCH' - 8000 TAGSET MATCHING PROG.
	 *
	 *        PERFORMS NEGATIVE WORD CLASS MATCHING LOGIC FOR TRAN4
	 *
	 *        A 'NEGATIVE WORD CLASS' IS A GENERAL TERM THAT REFERS TO
	 *        ALL THE CRITERIA (WC, TYPE, FORM, SUPER SET, ETC.) THAT
	 *        ARE TO BE CONSIDERED IN DETERMINING IF THIS IS A 'MATCH'.
	 *
	 *        ARGUMENTS:
	 *
	 *        NWRDCL - THE NEG. WORD CLASS TO USE IN MATCH TESTS
	 *                 (GUSN(K2) IN 'SPSRC4' OR WCTST IN 'TMATCH')
	 *
	 *        I3    -  CURRENT SWORK ELEMENT
	 *
	 *        RETFLG - INDICATES RETURN (0 = MATCH, 1 = NO MATCH)
	 *
	 *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *
	 *
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
#include "parsetrans.h"
#include <string.h>
#include <jbctrl.h>




void /*FUNCTION*/ nmatch(
long int nwrdcl,
long int i3,
short int *retflg
)
{
	static short exnwc2[9]={2,3,4,8,12,13,14,16,20};
	static short gb = 0;
	static short wc = 0;
	static short set = 0;
	static short nstyp = 0;
	static short phri3 = 0;
	static short retsw = 0;
	static short supset = 0;

	*retflg = 0;

	/*        ******* NEGATIVE WORD CLASS MATCHING ***** */

	wc = sworkX_.swork[i3-One][1-One];
	nstyp = sworkX_.swork[i3-One][2-One];

	phri3 = sworkX_.phrhed[i3-One];
	supset = sconX_.scon[phri3-One][13-One];
	set = sconX_.scon[phri3-One][11-One];

	if( nwrdcl != -2 ){

		if( srcflgX_.srcflg == 1 ){
			if( nwrdcl == -6 && wc != 20 )
				goto L_800;

			if( nwrdcl == -3 ){

				/*   GERMAN SOURCE - -3 INCLUDES  WC01 AND WC18 TYPES 11,20,21,24,911 */
				if( wc == 1 )
					goto L_800;
				if( wc != 18 )
					goto L_999;
				if( supset == 11 )
					goto L_800;
				if( (set == 20 || set == 21) || set == 24 )
					goto L_800;
				if( nstyp == 911 )
					goto L_800;
				goto L_999;
				}
			}

		/*        NEG. WORD CLASS TABLE - CONTAINS PERMITTED WORD CLASSES */

		match2(&wc,&neg4X_.negwc4[-nwrdcl-One][1-One],8,&retsw);
		if( retsw == 2 )
			goto L_999;
		}

	else if( srcflgX_.srcflg == 1 ){

		/*        FOR GERMAN SOURCE, WORD CLASS '-2', USE EXCLUDE TABLE */
		for( gb=1; gb <= 9; gb++ ){
			if( wc == exnwc2[gb-One] )
				goto L_999;
			}
		}


	else if( ((((wc == 2 || wc == 3) || wc == 4) || wc == 12) || wc == 13) || wc == 14 ){
		goto L_999;
		}

	
	else if( wc == 20 ){

		if( nstyp >= 885 && nstyp <= 887 )
			goto L_999;

		if( supset == 8 || supset == 10 )
			goto L_999;
		if( supset == 1 )
			goto L_999;
		}

	/*        A MATCH */
L_800:
	*retflg = 0;
	return;

	/*        NO MATCH */
L_999:
	*retflg = 1;
	return;

} /*end of function*/

