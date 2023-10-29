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
/*           TRGETPTR FORTRAN
 *                                          Date Created : 10/86
 *     This subroutine will return the exact SWORK position of
 *     an element pointed to by a Tagset line during matching
 *     in SP 1,2,3 or 4.
 *     PTRIN must be normalized to a value 1,2,3 etc. */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <fcrt.h>
#include "trans.h"
#include "logoslib.h"
#include <jbctrl.h>
#include "projexts.h"
#include "parsetrans_ext.h"


void /*FUNCTION*/ getptr(long int ptrin, short int *ptrout, long int locflg)
{
	static short int cntloc[3], cntpos, stramt[3], strflg, t, tmp;

	*ptrout = 0;

	/*     Check if Mini or Main and set variables */

	if( locflg == 1 ){
		strflg = vwarg2X_.wc50m;
		lmove(cntloc,1,vwarg1X_.wc50el,1,6);
		lmove(stramt,1,vwarg1X_.chng50,1,6);
		}
	else{
		strflg = cbsp2X_.wc5xm;
		lmove(cntloc,1,cbsp2X_.wc5xel,1,6);
		lmove(stramt,1,cbsp2X_.chng5x,1,6);
		}

	/*     This is a simple operation if no stretch is in effect */

	*ptrout = vwarg2X_.i + ptrin - 1;
	if( strflg != 0 ){

		/*     Part of the SP rule stretched. So we must adjust the
		 *     return value of the pointer accordingly. */

		/*     WC50EL(3) = The location of the control element for each
		 *     stretch. These are initialized to -100 */

		/*     CHNG50(3) = The amount of stretch for each Control/Stopper
		 *                 Combination.
		 *               = -1 If match is only on the stopper
		 *               =  0 If match is only on one control and stopper
		 *               =  1 If match is on two control and the stopper
		 *               This variable is initialized to 200 */

		/*     LOOK50(3) = -8X version of WC50EL array, initialized to 0 */

		/*     I  = The beginning location of first SP element in the
		 *          incoming SWORK array. */

		/*     I3 = Location of last SP element in the rule. */

		/*     ISAVE = Saved value of I of the rule which calls the WC10
		 *             rule */

		/*     WC50SV(3) = Saved value of WC50EL array from the calling
		 *                 rule. */

		/*     CH50SV(3) = Saved value of CHNG50 array from the calling
		 *                 rule. */


		/*                Change Pointer if necessary */

		tmp = 0;
		for( t=1; t <= 3; t++ ){
			cntpos = -cntloc[t-One];
			//   7/30/87 - CHANGE PTROUT/PTRIN FOR CORRECT CALCULATION */
			if( ptrin > cntpos )
				tmp += stramt[t-One];
			}
		// Current Stretch                             RKH  02/14/87   FIX */
		*ptrout += tmp;
		}

	/*     If we are pointing to an SP element to the
	 *     the right of current SP pointer then we
	 *     must adjust for current stretch. */

	if( ptrin >= stradjX_.spptr ) *ptrout += stradjX_.stradj;

	if( diagsX_.deepdi == 1 )
	{
		fprintf( _spec_fp, "        IN getptr, PTRIN = %4d   PTROUT = %4d\n", 
		  ptrin, *ptrout );
	}

	return;
} /*end of function*/

