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
	/* LAST CHG 10/19/86 *B0421DSD: REMOVE EXTRA INITIALIZATION OF GUSN
	 *      CHG 10/08/86 */
	/*     1.    SELECT RULE TO KEEP BY ITS NORMAL SORT ORDER
	 *    2.    check to  see if the two rules are identical for future
	 *          reference by the -36 56 function. */
	/*                        NXT LINE: 0GM 10/86  FIX -36056 FOR MINI/MAIN */
	/*                        NXT LINE: 0GM 10/86  FIX -36056 FOR MINI/MAIN */
	/*             SPCB COMMON IS SET IN EXSPX ROUTINE */
	/*             SPCB2 COMMON SET HERE, USED BY WHICSP AND TRAN4X */
	/*                        NXT LINE: 0GM 10/86  FIX -36056 FOR MINI/MAIN */
	/*                                0GM 10/86  FIX -36056 FOR MINI/MAIN  + */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <fcrt.h>
#include "trans.h"
#include "logoslib.h"
#include "parsetrans.h"
#include "projexts.h"
#include <jbctrl.h>
#include "parsetrans_ext.h"



void /*FUNCTION*/ whicsp(k7, k7m, sp, ovrfl, oflad)
long int k7;
short int *k7m, sp[], ovrfl[];
long int oflad;
{
	static short int gusx[3];
	static long int _l0;
	static short i = 0;
	static short j = 0;
	static short m = 0;
	static short x = 0;
	static short cb = 0;
	static short rs = 0;
	static short retsw = 0;
	static short stopfl = 0;
	static short matpos = 0;
	static short gusn[3]={0,0,0};
	static short tempx[21]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
	static short tempm[21]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

	stopfl = 0;
	/*     CLEAR THE APPROPRIATE IDEN FLAGS IF A MAIN WC OR WC9/10 MATCH.
	 *     THESE FLAGS REMEMBER IF THE MAIN AND MINI RULES MATCHED ON WERE
	 *     IDENTICAL OR NOT.  THE IDEN42 IS SET WHEN THE COMPARISON IS DONE
	 *     FOR WC9 AND WC10 RULES.  IDENSP IS SET FOR ALL THE OTHER WC RULES.
	 *     THIS INFO. IS NEEDED WHEN THE -36056000 OR -36056001 FUNCTIONS IN
	 *     THE VTR IS INVOKED. */

	if( sp[2-One] == 10 || sp[2-One] == 9 ){
		spcompX_.iden42 = 0;
		spcompX_.nptp42 = 0;
		}
	else{
		spcompX_.idensp = 0;
		spcompX_.nptpm = 0;
		}

	*k7m = 0;


	for( j=1; j <= spcbX_.lvl; j++ ){
		if( j > 3 ){

			m = 3*(j - 4) + 1;
			gusn[1-One] = ovrfl[m-One];
			gusn[2-One] = ovrfl[m+1-One];
			gusn[3-One] = ovrfl[m+2-One];

			gusx[1-One] = spyxX_.ovrfx[m-One];
			gusx[2-One] = spyxX_.ovrfx[m+1-One];
			gusx[3-One] = spyxX_.ovrfx[m+2-One];
			}
		else{

			m = 3*(j - 1) + 2;
			gusn[1-One] = sp[m-One];
			gusn[2-One] = sp[m+1-One];
			gusn[3-One] = sp[m+2-One];

			/*      MAKE SURE THAT THE OVERRIDE LOGIC IN T1OVERID HAS NOT SUPPLANTED
			 *      THE MINI RULE. REREAD IT'S SP AND OVERFLOW (IF APPLICABLE). */
			rulein((short *)ADR(_l0,2),&matpos,spxxX_.spx,&spcbX_.k7x,&retsw);
			if( spxxX_.spx[11-One] != 0 ){
				spcbX_.oflax = spxxX_.spx[11-One];
				ovrin((short*)ADR(_l0,2),spyxX_.ovrfx, spcbX_.k7x, spcbX_.oflax);
				}
			gusx[1-One] = spxxX_.spx[m-One];
			gusx[2-One] = spxxX_.spx[m+1-One];
			gusx[3-One] = spxxX_.spx[m+2-One];
			}


		for( i=1; i <= 3; i++ ){

			/*          CASE 3 AND 4 */
			if( (gusn[i-One] == -1) && (gusx[i-One] > 0) )
				goto L_140;
			if( (gusx[i-One] == -1) && (gusn[i-One] > 0) )
				goto L_180;

			/*          CASES 5 AND 6 */
			if( (gusn[i-One] < -1) && (gusx[i-One] > 0) )
				goto L_140;
			if( (gusx[i-One] < -1) && (gusn[i-One] > 0) )
				goto L_180;

			/*          CASES 10 AND 11 */
			if( (gusn[i-One] == -1) && (gusx[i-One] < -1) )
				goto L_140;
			if( (gusx[i-One] == -1) && (gusn[i-One] < -1) )
				goto L_180;

			if( i == 2 ){

				/*          TYPE FIELD ONLY */

				/*                                0GM 10/86  FIX -36056 FOR MINI/MAIN  + */
				if( !((gusx[i-One] == -1) && (gusn[i-One] == -1)) ){
					if( (gusn[i-One] < 0) || (gusx[i-One] < 0) ){
						/*      We need to establish if the two rules are identical, even at the
						 *      tagset level, for possible reference by the -36056 function.
						 *      Normally, tagsets are not compared when trying to determine if th
						 *      Main SP rule or the Mini SP rule should be chosen. */

						/*      If a previous of compares at the tagset level failed, ignore any
						 *      subsequent comparisons. */
						if( stopfl == 0 ){
							ovrin((short*)ADR(_l0,1),
								tempm, spcbX_.k7x, (short)-gusn[i-One]);
							ovrin((short*)ADR(_l0,2),
								tempx, spcbX_.k7x, (short)-gusx[i-One]);

							ccompx((char *)&tempm[1-One],1,(char *)&tempx[1-One],1,42,&retsw);
							 /*              RETSW    =  0     means they are the same. */
							if( retsw != 0 )
								stopfl = 1;

							}
						/*                                0GM 10/86  FIX -36056 FOR MINI/MAIN  +
						 *          ABOVE WILL FILTER OUT TAG SETS */
						}
					else if( gusn[i-One] < gusx[i-One] ){
						goto L_140;
						}
					else if( gusx[i-One] < gusn[i-One] ){
						goto L_180;
						}
					}

				}
			else if( gusn[i-One] < gusx[i-One] ){
				goto L_140;
				}
			else if( gusx[i-One] < gusn[i-One] ){

				/*          THEY ARE EQUAL */
				goto L_180;
				}

			/*          THEY ARE EQUAL */
			}


		if( j == 1 ){
			if( sp[1-One] > spcbX_.lvl )
				goto L_180;
			if( sp[1-One] < spcbX_.lvl )
				goto L_140;
			}
		}
	/*                                0GM 10/86  FIX -36056 FOR MINI/MAIN  -
	 *      If you complete the 120 loop, and STOPFL = 0, the rules are iden. */
	if( stopfl == 0 ){
		if( sp[2-One] == 10 || sp[2-One] == 9 ){
			spcompX_.iden42 = 1;
			spcompX_.nptp42 = sp[12-One];
			}
		else{
			spcompX_.idensp = 1;
			spcompX_.nptpm = sp[12-One];
			}
		}
	/*                                0GM 10/86  FIX -36056 FOR MINI/MAIN  -
	 *         EXPERIMENTAL RULE WINS IN A TIE */

L_140:
	*k7m = 1;

	if( diagsX_.longdi == 1 )
		{
		  fprintf( _spec_fp, "             EXPERIMENTAL RULE WINS IN WHICSP            %5d%5d%5d%5d\n", 
		  spcompX_.idensp, spcompX_.iden42, spcompX_.nptpm, spcompX_.nptp42 );
		}



	return;

	/*           OLD RULE WINS
	 *           TEMP NO STREEEEETCH IN REGULAR SEARCH NOW */
L_180:
	cbsp2X_.wc5xm = 0;
	if( diagsX_.longdi == 1 )
		{
		fprintf( _spec_fp, "             REGULAR RULE WINS IN WHICSP                 %5d%5d%5d%5d\n", 
		  spcompX_.idensp, spcompX_.iden42, spcompX_.nptpm, spcompX_.nptp42 );
		}
	return;

} /*end of function*/

