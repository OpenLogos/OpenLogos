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
		/*                                        IS TO MATCH ON THE
		 *                                        APPROPRIATE TABLE */

		/*    WC =     THE WC VALUES OF THE SWORK WE ARE MATCHING AGAINST.
		 *             SOME WC VALUES (E.G. 01) CHECK THE NOUN FORM TABLE.
		 *             SOME WC VALUES (E.G. 02) CHECK THE VERB FORM TABLE.
		 *             WC 99 MEANS CHECK THE CELL FORM TABLE.
		 *    ENTRY  = WHICH TABLE RECORD TO CHECK.
		 *    VAL    = WHAT VALUE TO LOOK FOR IN THE TABLE.
		 *    MATCH  = 0 FOR NO MATCH,   1 FOR A MATCH. */

		 
		 /*    LOCFLG = LOCATION IN PROGRAM TO GO  (1 = LOAD  2 = TEST)
	 *    WC =     THE WC VALUES OF THE SWORK WE ARE MATCHING AGAINST.
	 *             SOME WC VALUES (E.G. 01) CHECK THE NOUN FORM TABLE.
	 *             SOME WC VALUES (E.G. 02) CHECK THE VERB FORM TABLE.
	 *             WC 99 MEANS CHECK THE CELL FORM TABLE.
	 *    ENTRY  = WHICH TABLE RECORD TO CHECK.
	 *    VAL    = WHAT VALUE TO LOOK FOR IN THE TABLE.
	 *    MATCH  = 0 FOR NO MATCH,   1 FOR A MATCH.
	 *    RETFLG = RETURN VALUE (0 = GOOD   -1 = ERROR) */
	/*   THIS SUBROUTINE IS CALLED TO CHECK THE RES FORM TABLES. */
	/*    THERE ARE 3 DIFFERENT TABLES TO CHECK, DEPENDING ON THE
	 *    WORD CLASS OF THE ELEMENT THAT IS BEING MATCHED. */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <logos_include_res_pt/fcrt.h>
#include "project.h"
#include "projexts.h"
#include <string.h>
#include <logos_include_res_pt/jbctrl.h>



void resformod(locflg, wc, entry, val, match, retflg)
long int locflg, wc, entry;
short int *val, *match, *retflg;
{
	static char formpl[2][80][13];
	static short int cellfr[80][26], tmp2;
	static long int _l0, _l1;
	static char pgmnam[9] = "RESFORMO";
	static short m = 0;
	static short x = 0;
	static short retsw = 0;
	static short wcplc = 0;

	*retflg = 0;
	if( locflg == 1 )
	{
		if ( res_form_load( 2 == srcflgX_.srcflg ? cellfr : NULL, formpl) )
		{
			lmove((short*)cinitX_.cinit, 1, &cellfr[61-One][1-One], 1,20*52);
			goto L_7000;
		}
		errlog(pgmnam,120,2065,2);

	}
	else if( locflg == 2 ){

		*match = 0;

		if( wc != 99 ){

			if( srcflgX_.srcflg == 2 ){
				/*               1   2   3   4   5   6   7   8   9   10 */
				if( ((((wc == 1 || wc == 5) || wc == 7) || wc == 14) || 
				  wc == 16) || wc == 18 )
					goto L_240;
				if( ((((((((wc == 2 || wc == 3) || wc == 6) || (wc >= 
				  8 && wc <= 10)) || wc == 12) || wc == 13) || wc == 
				  15) || wc == 17) || wc == 19) || wc == 20 )
					goto L_260;
				if( wc == 4 || wc == 11 )
					goto L_7000;
				}

			if( srcflgX_.srcflg != 1 )
				goto L_240;
			/*               1   2   3   4   5   6   7   8   9   10 */
			if( !(((((((wc == 2 || wc == 3) || wc == 6) || wc == 12) || 
			  wc == 13) || wc == 17) || wc == 19) || wc == 20) ){
				if( (wc >= 8 && wc <= 11) || wc == 15 )
					goto L_7000;
				goto L_240;
				}

L_260:
			wcplc = 2;
			goto L_300;

L_240:
			wcplc = 1;

L_300:
			frmtst(val,(char*)formpl[wcplc-One][entry-One],&retsw);
			}
			 /*      IF THE RANGE IS 1 TO 60 USE THE CELLFRM ARRAY. */
		else if( entry > 0 && entry <= 60 ){
			match2(val,&cellfr[entry-One][1-One],26,&retsw);
			}
		else{
			/*    USE THE CINIT ARRAY. */
			tmp2 = entry - 60;
			match2(val,&cinitX_.cinit[tmp2-One][1-One],26,&retsw);
			}
		if( retsw == 1 )
			*match = 1;
		goto L_7000;
		}
	else{
		errlog(pgmnam,10,0,21);
		}


	if( opswX_.sw[3-One] == 1 || opswX_.sw[8-One] == 1 )
		{
		fprintf( _spec_fp, "--ERROR IN RSFORMOD ERR,LOCFLG,RETSW = %8ld%8ld%8d\n", 
		  errvrsX_.err, locflg, retsw );
		}
	*retflg = -1;


L_7000:
	return;

} /*end of function*/

