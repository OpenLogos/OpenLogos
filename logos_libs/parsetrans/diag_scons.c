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

	print scon table to diagnostic file.

*/

	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*    CHANGES:
	 *       7/13/93 jal: if black hole flag is set on (SCON100) print it. This
	 *             addition needed for gersrc which only printed scons 1-60.
	 *      04/09/91 *JAL*:  FOR ENGLISH SOURCE, PRINT SCON 61 TO 100 IF
	 *                       THEY ARE NON ZERO.
	 *      04/17/87 *R1685RKH*  CHANGE T1-4 SWORK LIMI         t from 50 to
	 *      CHG 10/28/86 *B0422DSD: PRINT SCONS 21..60 IF NEEDED
	 *      CHG 09/04/86
	 *      CHG 08/28/86 *R1561DSD: 100 SCONS
	 *      CHG 02/24/86 */
	/*        CALLED BY TRAN1 */
	/*        FUNCTION:
	 *           TO PRINT THE 'SCON' RECORDS AND ASSOCIATED DATA
	 *           'SCT' IS THE NUMBER OF SCON RECORDS FOR THIS SENTENCE */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <fcrt.h>
#include "logoslib.h"
#include "trans.h"
#include "parsetrans_ext.h"
#include "parsetrans.h"
#include <string.h>
#include <jbctrl.h>

void /*FUNCTION*/ scnprt()
{
	static short int k, l, nz, scnptr, scnsiz;

	static short i = 0;
	static short j = 0;
	static short adcnum = 0;
	static short aelcnt = 0;

	fprintf( _spec_fp, " THE SCON FOR %6.6s\n",passesX_.pssnam );

	fprintf( _spec_fp, "              1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20\n" );

	for( j=1; j <= prtscoX_.sct; j++ ){

		/*        TO REDUCE SCON PRT - SKIP OVER EMPTY VCS. */

		if( !(sconX_.scon[j-One][1-One] >= 99 && 
			  sconX_.scon[j-One][1-One] <= 120) ){
			if( !(sconX_.scon[j-One][1-One] <= -99 && 
				  sconX_.scon[j-One][1-One] >= -120) )
				goto L_60;

			}
		if( !((sconX_.scon[j-One][1-One] == 108 &&
			   srcflgX_.srcflg == 2) && trgflgX_.trgflg == 1) ){
			/*         PRINT SCONS OF VC'S WITH 4,5,6 VALUES */
			if( ((sconX_.scon[j-One][2-One] == 0 && 
				  sconX_.scon[j-One][4-One] == 0) &&
				  sconX_.scon[j-One][5-One] == 0) && 
				  sconX_.scon[j-One][6-One] == 0)
				goto L_1001;
			}

		/*    Each Scon has 100 positions, to save space print out as follows:
		 *    FOR GERMAN SOURCE IGNORE THE LAST 40 POSITIONS UNTIL
		 *    RES/TRAN INFORMATION PASSING IS IN PLACE.
		 *    PRINT IN LINES OF 20 SCON VALUES. ONLY PRINT UP UNTIL THE LAST
		 *    NON ZERO SCON VALUE. */

L_60:
		fprintf( _spec_fp, "     %3d  ", j );
		for( i=1; i <= 20; i++ ){
			fprintf( _spec_fp, "%5d", sconX_.scon[j-One][i-One] );
			}
		fprintf( _spec_fp, "\n" );
		/*                       CHECK TO SEE IF POSITIONS 21-? ARE ZEROES: */
		scnptr = sconX_.scolnk[j-One];
		if( scnptr <= ELMMAX && scnptr > 0 ){
			scnsiz = 60;
			/*                       if black hole flag on (SCON100) print it. */
			if( sconX_.scono[scnptr-One][SCONX-SCONX1-One] > 0 )
				scnsiz = SCONX;
			if( srcflgX_.srcflg == 2 )
				scnsiz = SCONX;
			/*                       FIND LAST GROUP OF 20 SCON VALUES WHICH
			 *                       CONTAINS A NON ZERO ENTRY, THEN PRINT'EM */
			for( nz=scnsiz; nz >= 21; nz-- ){
				if( sconX_.scono[scnptr-One][nz-SCONX1-One] != 0 )
					goto L_1002;
				}
			goto L_1001;
			/*                       PRINT IN LINES OF 20 */
L_1002:
			i = (((nz - 1) - SCONX1)/20) + 1;
			for( l=1; l <= i; l++ ){
				if( scnsiz - SCONX1 - (20*l) >= 0 ){
					fprintf( _spec_fp, "          " );
					for( k=(20*l) + 1; k <= (20*(l + 1)); k++ ){
						fprintf( _spec_fp, "%5d", sconX_.scono[scnptr-One][k-SCONX1-One] );
						}
					fprintf( _spec_fp, "\n" );
					}
				else{
					fprintf( _spec_fp, "          " );
					for( k=(20*l) + 1; k <= scnsiz; k++ ){
						fprintf( _spec_fp, "%5d", sconX_.scono[scnptr-One][k-SCONX1-One] );
						}
					fprintf( _spec_fp, "\n" );
					}
				}
			}
L_1001:
		;

		}
	return;

} /*end of function*/

