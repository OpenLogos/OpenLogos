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
	 *      CHG 08/19/86 *R1561DSD: 100 SCONS
	 *      CHG 10/17/85
	 *
	 *        CALLED FROM 'SPSRC2' (P0615) - SP SEARCH/MATCH PROGRAM
	 *           AND FROM 'TMATCH' - WC/TYP/FRM TAGSET MATCHING PROG.
	 *
	 *        PERFORMS NEGATIVE WORD CLASS MATCHING LOGIC FOR TRAN2
	 *
	 *        A 'NEGATIVE WORD CLASS' IS A GENERAL TERM THAT REFERS TO
	 *        ALL THE CRITERIA (WC, TYPE, FORM, SUPER SET, ETC.) THAT
	 *        ARE TO BE CONSIDERED IN DETERMINING IF THIS IS A 'MATCH'.
	 *
	 *        ARGUMENTS:
	 *
	 *        NWRDCL - THE NEG. WORD CLASS TO USE IN MATCH TESTS
	 *                 (GUSN(K2) IN 'SPSRC2' OR WCTST IN 'TMATCH')
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
#include <jbctrl.h>


void /*FUNCTION*/ nmatch(
long int nwrdcl, 
long int i3,
short int *retflg
)
{
	static short wc = 0;
	static short nsfrm = 0;
	static short nstyp = 0;
	static short phri3 = 0;
	static short retsw = 0;
	static short supset = 0;

	/*        ******* NEGATIVE WORD CLASS MATCHING ***** */
	phri3 = sworkX_.phrhed[i3-One];
	supset = sconX_.scon[phri3-One][13-One];

	wc = sworkX_.swork[i3-One][1-One];
	nstyp = sworkX_.swork[i3-One][2-One];
	nsfrm = sworkX_.swork[i3-One][3-One];

	/*         WC -2 IS THE SAME FOR GERMAN AND ENGLISH */
	if( nwrdcl == -2 || srcflgX_.srcflg == 1 ){

		if( nwrdcl < -5 ){

			if( nwrdcl == -8 ){
				if( wc == 1 || wc == 13 )
					goto L_800;
				if( wc == 19 && nsfrm != 1 )
					goto L_800;
				goto L_999;
				}
			}
		else if( wc == 2 || wc == 12 ){
			goto L_999;
			}
		else if( wc == 6 && srcflgX_.srcflg == 1 ){
			goto L_999;
			}
		else if( srcflgX_.srcflg == 2 && (wc == 4 || wc == 17) ){
			goto L_999;
			}
		else if( (nwrdcl == -2 || nwrdcl == -5) && wc == 18 ){

			if( srcflgX_.srcflg == 2 )
				goto L_800;
			if( supset == 1 )
				goto L_999;
			goto L_800;
			}
		else if( nwrdcl == -5 && wc == 4 ){
			goto L_999;
			}
		else if( wc != 20 ){
			goto L_800;
			}
		else if( nstyp >= 885 && nstyp <= 887 ){
			goto L_999;
			}
		else{

			if( supset == 0 )
				supset = nstyp;
			if( nwrdcl == -3 ){

				if( supset >= 6 && supset <= 8 )
					goto L_999;
				goto L_800;
				}
			else if( supset == 8 || supset == 10 ){
				goto L_999;
				}
			else if( supset == 1 && srcflgX_.srcflg == 2 ){
				goto L_999;
				}
			else{
				goto L_800;
				}
			}
		}

	/*        NEG. WORD CLASS TABLE - CONTAINS PERMITTED WORD CLASSES */

	match2(&wc,&neg2X_.negwc2[-nwrdcl-One][1-One],8,&retsw);
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

