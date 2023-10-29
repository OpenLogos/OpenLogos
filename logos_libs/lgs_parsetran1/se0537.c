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
	 *       CALLED BY THE TRAN PROGRAMS TO OPEN
	 *       THE MAIN SEMCODE FILE CREATED BY THE SEMT0564 MODULE
	 *       AND THEN CREATE THE TEMPORARY SEMTENCE COPY FOR USE
	 *       BY THE MATCH LOGIC SEMT.P0563
	 *
	 *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *
	 *
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
EXTERN struct t_smcdptX_ {
	long int f32;
	}	smcdptX_;


void /*FUNCTION*/ se0537(locflg)
long int locflg;
{
	static short int semcod[21][6], x;
	static char pgmnam[9] = "SEMT0537";
	static long f31 = 0;


	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*          GERMAN-ENGLISH ONLY */
	if( !(srcflgX_.srcflg != 1 || trgflgX_.trgflg != 2) ){

		errvrsX_.err = 0;
		if( locflg != 1 ){


			/*500   F32 = 1 */
			x = x;
			/*winnt CALL A4SETC (F32,1) */

			while( TRUE ){
				x = x;
				/*winnt CALL LREADD (31, SEMCOD, ERR)
				 *---- IF (ERR .NE. 0) GOTO 7000 */
				if( errvrsX_.err != 0 )
					goto L_7001;

				/*winnt      CALL LWRITE (32, SEMCOD, ERR)
				 *---- IF (ERR .NE. 0) GOTO 7000 */
				if( errvrsX_.err != 0 )
					break;

				/*          STOP AFTER -1 RECORD IS READ AND WRITTEN */
				if( (semcod[1-One][1-One] == -1) && (semcod[1-One][2-One] == 
				  -1) )
					return;
				}
			errlog(pgmnam,700,0,1);
			return;
L_7001:
			errlog(pgmnam,600,0,1);

			/*          MAIN LIST OF SEMCODS
			 *winnt      CALL LOPEN (31, F31, ERR)
			 *---- IF (ERR .NE. 0) GOTO 7000 */
			}
		else if( errvrsX_.err != 0 ){
			errlog(pgmnam,100,0,1);

			/*---- F31 = 1
			 *winnt      CALL A4SETC (F31,1) */

			/*          TEMPORARY FOR EACH SENTENCE
			 *winnt      CALL LOPEN (32, F32, ERR)
			 *---- IF (ERR .NE. 0) GOTO 7000 */
			}
		else if( errvrsX_.err != 0 ){
			errlog(pgmnam,200,0,1);
			}
		}
	return;
} /*end of function*/

