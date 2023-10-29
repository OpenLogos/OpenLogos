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
	/*               K7XM IS SET IN EXSPX FOR OVERIDE MULTI */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*        TIE IN WHICHSP ('GOTO 40' - THEY WERE EQUAL) */
	/*          RETFLG = 0 - NO OVERRIDE
	 *          RETFLG = 1 - YES, AN OVERRIDE (MINI WINS OVER MAIN) */
	/*          WAS AN OVERIDE FOUND IN MINI RULES?? */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <fcrt.h>
#include "trans.h"
#include "logoslib.h"
#include "parsetrans.h"
#include "projexts.h"
#include "parsetrans_ext.h"
#include <jbctrl.h>




void /*FUNCTION*/ overid(k7, sp, ovrfl, retflg)
long int k7;
short int sp[], ovrfl[], *retflg;
{
	static long int _l0, _n;
	static short j = 0;
	static short m = 0;
	static short ct = 0;
	static short k7x = 0;
	static short lvl = 0;
	static short oflad = 0;
	static short oflax = 0;
	static short retsw = 0;
	static short k7save = 0;
	static short matpos = 0;
	static short gusn[3]={0,0,0};
	static short gusx[3]={0,0,0};

	*retflg = 0;
	ct = 0;
	while( TRUE ){
		ct += 1;
		k7x = cbsp3X_.k7save[ct-One];

		if( k7x == 0 )
			return;

		/*          YES, MATCH IT AGAINST RULE K7 */
		oflad = sp[11-One];
		/*         FIRST MOVE INTO SPX ARRAY THE NEXT OVERRIDE RULE */
		rulein((short *)ADR(_l0,2),&matpos,spxxX_.spx,&k7x,&retsw);
		oflax = spxxX_.spx[11-One];
		if( oflax != 0 )
			ovrin((short*)ADR(_l0,2), (char*)spyxX_.ovrfx, k7x, oflax);
		lvl = spxxX_.spx[0];

		if( sp[0] == lvl ){

			for( j=1; j <= lvl; j++ ){

				if( j > 3 ){

					m = 3*(j - 4) + 1;

					gusn[0] = ovrfl[m-One];
					gusn[1] = ovrfl[m+1-One];
					gusn[2] = ovrfl[m+2-One];

					gusx[0] = spyxX_.ovrfx[m-One];
					gusx[1] = spyxX_.ovrfx[m+1-One];
					gusx[2] = spyxX_.ovrfx[m+2-One];
					}
				else{

					m = 3*(j - 1) + 2;

					gusn[0] = sp[m-One];
					gusn[1] = sp[m+1-One];
					gusn[2] = sp[m+2-One];

					gusx[0] = spxxX_.spx[m-One];
					gusx[1] = spxxX_.spx[m+1-One];
					gusx[2] = spxxX_.spx[m+2-One];
					}

				if( gusn[0] != gusx[0] )
					goto L_20;
				if( gusn[2] != gusx[2] )
					goto L_20;
				if( gusn[1] != gusx[1] ){

					/*           TAG SETS ??? */

					/*     CASE   OVERIDE?    CONDITION */

					/*      A      NO          ONE IS A  -1  OTHER IS POSITIVE */

					/*      B      NO          BOTH NEGATIVE BUT ONLY ONE IS  -1 */

					/*      C      NO          BOTH POSTIVE BUT NOT EQUAL */

					/*      D      YES         ALL OTHER COMBINATIONS */

					/*          TYPE FIELD ONLY */
					if( gusn[1] == -1 ){

						if( gusx[1] > 0 )
							goto L_20;
						if( gusx[1] < -1 )
							goto L_20;

						/*          YES */
						}
					else if( gusx[1] == -1 ){

						if( gusn[1] > 0 )
							goto L_20;
						if( gusn[1] < -1 )
							goto L_20;
						}
					else if( gusn[1] >= 0 ){
						if( gusx[1] >= 0 ){
							if( gusn[1] != gusx[1] )
								goto L_20;

							/*          NO */
							}
						}
					}

				/*          NO */
				}
			break;


			/*          OLD RULE WINS */
L_20:
			if( diagsX_.longdi == 1 )
				{
				fprintf( _spec_fp, "\n  NOT EQUAL IN OVERRIDE%6d%6d%6d", 
				  k7x, k7, j );
				for(_n=0L; _n < sizeof(gusn)/sizeof(short); _n++)
					fprintf( _spec_fp, "%6d", gusn[_n] );
				for(_n=0L; _n < sizeof(gusx)/sizeof(short); _n++)
					fprintf( _spec_fp, "%6d", gusx[_n] );
				fprintf( _spec_fp, "%6d%6d\n", ct, k7save );
				}

			}
		}

	/*          THEY ARE EQUAL, OVERRIDE RULE */
	*retflg = 1;
	if( diagsX_.longdi == 1 )
		{
		fprintf( _spec_fp, " EQUAL IN OVERRIDE%6d%6d\n", k7, k7x );
		}

	return;


} /*end of function*/

