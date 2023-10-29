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
#include <jbctrl.h>
#include "parsetrans_ext.h"



/*  BHSET():
 *   set the target, black-hole flag with the number of black holes in
 *   the current transfer of a given source word. */

/*   Calculated as a function of the following dictionary values:
 *     WC        (SCON1)
 *     OFL4      (SCON13)
 *     OFL3B     (SCON12)
 *     FORMFIELD (FORMSV)
 *     OFL2A     (SCON15) */

/* * NOTE:
 *     1- These values must have already been assigned to the
 *        variables in parenthesis.
 *     2- This routine should not be called for black holes in constants
 *        until they have been included in the constant dictionaries. */


/*   Usage:   BHSET(SCPTR,RC) */

/*   Input:
 *    SCPTR (I*2) = ptr to source word (element). */

/*   Output:
 *    SCON(100,SCPTR) = number of black holes in the transfer.
 *    RC (I*4)   =  return code:  = 0, ok
 *                                = 1, error */

/*  CHANGES: */





void /*FUNCTION*/ bhset(scptr, rc)
long int scptr, *rc;
{
	static short int bhscpt, ofl4x;

	/*                   Black-hole SCON pointer */
	/*----------------------------------------------------------------- */
	/*             set ptr to Black hole SCON
	 *             SCON(100) = SCONO(80)
	 *             If changing SCONS,check that SCONO or SCON used. */
	bhscpt = TXBHCT - SCONX1;

	*rc = 0;
	/*             check for quick returns
	 *                 only source elements are set here. */
	if( sconX_.scolnk[scptr-One] > 0 ){
		/*                 OFL2A (SC15) =0 --> no black holes */
		if( sconX_.scon[scptr-One][15-One] <= 0 ){
			sconX_.scono[sconX_.scolnk[scptr-One]-One][bhscpt-One] = 0;
			}
		else{


			if( srcflgX_.srcflg == 1 ){
				/*             German SOURCE */

				if( trgflgX_.trgflg != 1 ){
					if( trgflgX_.trgflg == 2 ){

						/*                 English TARGET;  check wc */
						if( sconX_.scon[scptr-One][1-One] == 1 ){
							/*                     wc=1;  check ofl4, ofl2a */
							if( sconX_.scon[scptr-One][13-One] == 4 &&
								sconX_.scon[scptr-One][15-One] == 1 ){
								sconX_.scono[sconX_.scolnk[scptr-One]-
								  One][bhscpt-One] = 1;
								return;
								}
							}
						else if( sconX_.scon[scptr-One][1-One] == 2 ){
							/*                     wc=2;  check ofl2a */
							if( sconX_.scon[scptr-One][15-One] == 1 ){
								sconX_.scono[sconX_.scolnk[scptr-One]-
								  One][bhscpt-One] = 1;
								return;
								}
							}
						else if( sconX_.scon[scptr-One][1-One] != 3 ){
							if( sconX_.scon[scptr-One][1-One] == 4 ){
								/*                     wc=4;  check ofl2a=1, ofl4, ofl3b */
								if( sconX_.scon[scptr-One][15-One] == 1 ){
									if( sconX_.scon[scptr-One][13-One] == 14 ||
										sconX_.scon[scptr-One][13-One] == 15 ){
										sconX_.scono[sconX_.scolnk[scptr-One]-
										  One][bhscpt-One] = 1;
										return;
										}
									else if( sconX_.scon[scptr-One][13-One] == 16 &&
										     sconX_.scon[scptr-One][12-One] !=  4 ){
										sconX_.scono[sconX_.scolnk[scptr-One]-
										  One][bhscpt-One] = 1;
										return;
										}
									}
								}
							}
						}
					else if( trgflgX_.trgflg >= 3 && trgflgX_.trgflg <= 5 ){

						/*                 Romance TARGET;  check wc */
						if( sconX_.scon[scptr-One][1-One] == 1 ){
							/*                     wc=1;  check ofl4,ofl2a */
							if( sconX_.scon[scptr-One][13-One] == 4 ){
								if( sconX_.scon[scptr-One][15-One] <= 6 ){
									sconX_.scono[sconX_.scolnk[scptr-One]-
									  One][bhscpt-One] = 1;
									return;
									}
								}
							else if( sconX_.scon[scptr-One][15-One] <= 3 ){
								sconX_.scono[sconX_.scolnk[scptr-One]-
								  One][bhscpt-One] = 1;
								return;
								}
							}
						else if( sconX_.scon[scptr-One][1-One] == 2 ){
							/*                     wc=2;  check ofl2a */
							if( sconX_.scon[scptr-One][15-One] <= 6 ){
								sconX_.scono[sconX_.scolnk[scptr-One]-
								  One][bhscpt-One] = 1;
								return;
								}
							}
						else if( sconX_.scon[scptr-One][1-One] != 3 ){
							if( sconX_.scon[scptr-One][1-One] == 4 ){
								/*                     wc=4;  check ofl2a,ofl4,ofl3b */
								if( sconX_.scon[scptr-One][15-One] <= 6 ){
									ofl4x = sconX_.scon[scptr-One][13-One];
									if( ofl4x == 14 || ofl4x == 15 ){
										sconX_.scono[sconX_.scolnk[scptr-One]-
										  One][bhscpt-One] = 1;
										return;
										}
									else if( ofl4x == 16 && sconX_.scon[scptr-One][12-One] != 4 ){
										sconX_.scono[sconX_.scolnk[scptr-One]-
										  One][bhscpt-One] = 1;
										return;
										}
									}
								}
							}
						}
					}
				}
			else if( srcflgX_.srcflg == 2 ){

				/*             English SOURCE ;   check target */

				if( !(trgflgX_.trgflg == 1 || trgflgX_.trgflg == 2) ){
					if( trgflgX_.trgflg >= 3 && trgflgX_.trgflg <= 6 ){

						/*             Romance TARGET;  check wc */
						if( sconX_.scon[scptr-One][1-One] == 1 ){
							/*                wc=1;  check  ofl2a=1,2,3 */
							if( sconX_.scon[scptr-One][15-One] <= 3 ){
								sconX_.scono[sconX_.scolnk[scptr-One]-
								  One][bhscpt-One] = 1;
								return;
								}
							}
						else if( sconX_.scon[scptr-One][1-One] == 2 ){
							/*                wc=2;  check  ofl2a=1,2,3,4,5,6 */
							if( sconX_.scon[scptr-One][15-One] <= 6 ){
								sconX_.scono[sconX_.scolnk[scptr-One]-
								  One][bhscpt-One] = 1;
								return;
								}
							}
						else if( sconX_.scon[scptr-One][1-One] != 3 ){
							if( sconX_.scon[scptr-One][1-One] == 4 ){
								/*                wc=4;  check  ofl2a=1,2,3,4,5,6; form!=23 */
								if( sconX_.scon[scptr-One][15-One] <= 6 ){
									if( formsaX_.formsv[sconX_.scolnk[scptr-One]-
									  One] != 23 ){
										sconX_.scono[sconX_.scolnk[scptr-One]-
										  One][bhscpt-One] = 1;
										return;
										}
									}
								}
							}
						}
					}
				}


			/*             Default, no black-hole setting */
			sconX_.scono[sconX_.scolnk[scptr-One]-One][bhscpt-One] = 0;

			}
		}
	return;
}
