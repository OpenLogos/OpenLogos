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
/*     THIS ROUTINE CHECKS INCOMING FORMS AGAINST FORM FIELD
 *     SET CODES FOR ALL TRANS. */

/*   GUSN IS THE TABLE ENTRY TO BE CHECKED
 *   K2 IS THE POINTER TO THE FORM FIELD IN THE SWORK (ALWAYS 3 EXCEPT:)
 *    IF K2 = 4,  THE WC AND GUSX VALUES ARE NOT TAKEN FROM
 *    THE SWORK, BUT FROM THE FRMARR ARRAY
 *   I3 IS THE SWORK POINTER.  IF K2 IS 4, THIS PARAMETER IS IGNORED */



/*     GUSN IS THE FORM SET CODE IN SP PATTERN.
 *     GUSX IS THE FORM OF SWORK BEING PROCESSED. */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <fcrt.h>
#include "trans.h"
#include "logoslib.h"
#include "projexts.h"
#include <string.h>
#include "parsetrans_ext.h"
#include "parsetrans.h"
#include <jbctrl.h>


EXTERN struct t_sw28fmX_ {
	short int sw28fm;
	}	sw28fmX_;


void /*FUNCTION*/ formod(
long int locflg,
long int  gusn,
long int  k2,
long int  i3,
short int *retflg)
{
	long templ_k2;
	static char formpl[3][80][13];
	static char pgmnam[9] = "TxFORMOD";
	static short j = 0;
	static short wc = 0;
	static short k18 = 0;
	static short gusx = 0;
	static short place = 0;
	static short retsw = 0;
	static int _aini = 1;

	*retflg = 2;

	if( locflg == 1 ){
		if ( !tran_form_load((char*)formpl) )
		{
			errlog(pgmnam,1000,0,1);
		}
		return;
		}

			/*         FORM CODES RANGE FROM 20 - 99 */
	if( (gusn < 20) || (gusn > 99) ){
		*retflg = 2;
		return;
		}



	if( locflg == 2 ){

		if( k2 == 4 ){
			wc = frmarrX_.frmarr[0];
			gusx = frmarrX_.frmarr[2-One];
			}
		else{
			gusx = sworkX_.swork[i3-One][k2-One];
			k18 = k2 - 2;
			wc = sworkX_.swork[i3-One][k18-One];
			}


		if (tranidX_.tranid == 1 ){
			if( k2 == 16 ){
				wc = frmarrX_.frmarr[1-One];
				gusx = frmarrX_.frmarr[2-One];
				}
			else{
				templ_k2 = k2;
				if( k2 == 3 )
					templ_k2 = 4*prctX_.js[sconX_.scolnk[i3-One]-One] + 2;

				gusx = swork1X_.swork1[i3-One][templ_k2-One];
				k18 = templ_k2 - 2;
				wc = swork1X_.swork1[i3-One][k18-One];
				}

			if( srcflgX_.srcflg == 2 && wc == 4 ){
				wc = 2;
				/*+                 Both passes of 2 pass mode       3/27/97 jal
				 *+                 TRAN1,PASS2 only.      Log #2096    2/23/94 jal*/
				if(passesX_.passfl == 1 ) wc = 1;
				}
		}

			/*    WC'S 5,6,7,8 (GERMAN SOURCE) ARE TREATED LIKE WC1 IN SP2
			 *    WORD CLASS 6 (ENGLISH SOURCE) GOES TO OTHER FORM TABLE */
		else if (tranidX_.tranid == 2 ){
			if( srcflgX_.srcflg == 2 ){
				if( (wc == 5 || wc == 7) || wc == 8 )
					wc = 1;
				if( (wc == 4 || wc == 14) || wc == 17 )
					wc = 2;
				}
			else{

				if( (wc >= 5) && (wc <= 8) )
					wc = 1;
				if( srcflgX_.srcflg == 1 && (wc == 6 || wc == 16) )
					wc = 2;
				if( srcflgX_.srcflg == 1 && (wc == 15 || wc == 17) )
					wc = 1;
				}
		}
			/*         WORD CLASSES 5,6,7 USE NOUN TABLE IN SP3 */
		else if (tranidX_.tranid == 3 ){
			if( wc >= 5 && wc <= 7 )
				wc = 1;
		}


					/*         WORD CLASSES 5,6,7,8 USE NOUN TABLE IN SP4 */
		else if (tranidX_.tranid == 4 ){

			if( srcflgX_.srcflg == 1 && wc == 8 )
				wc = 2;

			if( wc >= 5 && wc <= 8 )
				wc = 1;

			if( srcflgX_.srcflg == 1 && (((wc == 3 || wc == 4) || wc == 13) || wc == 14) )
				wc = 2;
			if( srcflgX_.srcflg == 2 ){
				if( wc == 3 || wc == 4 )
					wc = 2;
				if( wc >= 12 && wc <= 14 )
					wc = 2;
				}
		}
		else {
		}

		}


	
	
	else if( locflg == 3 ){
			gusx = semfrmX_.semwrk[i3-One][k2-One];
			k18 = k2 - 2;
			wc = semfrmX_.semwrk[i3-One][k18-One];
			if( srcflgX_.srcflg == 1 ){
				if( !(wc != 3 && wc != 8) ){
					if( i3 == 2 && semfrmX_.semwrk[0][0] == wc )
						wc = 2;
					}
				}
		}



	else{
			errlog(pgmnam,10,0,21);
			return;
		}



			if( tranidX_.tranid == 4 && srcflgX_.srcflg == 2 && wc == 18 )
				wc = 1;

			j = gusn - 19;
			place = 2;
			if( wc != 1 ){

				if (tranidX_.tranid == 1){
					if( (wc == 2) || (wc == 12) ){
						place = 1;
					}
					else if( wc != 5 ){
						if( !(srcflgX_.srcflg != 1 && wc == 7) ){
							place = 3;
							}
						}
				}
				else if (tranidX_.tranid == 2){
					if( (wc == 2 || wc == 12) || (srcflgX_.srcflg == 2 && wc == 17) ){
						place = 1;
						}
					else if( !(wc == 5 || wc == 7) ){
						place = 3;
						}
				}

				else {
					if( (wc == 2 || wc == 12) || (srcflgX_.srcflg == 2 &&  wc == 17) ){
						place = 1;
					}
					else if( srcflgX_.srcflg == 1 && (((wc == 4 || wc == 14) || wc == 16) || wc == 17)) {
						place = 1;
					}
					else if( wc == 5 || wc == 7 ){
						place = 2;
					}
					else if( (tranidX_.tranid == 3 || tranidX_.tranid == 4) && sw28fmX_.sw28fm == 1 ){
						*retflg = 2;
						return;
								}
					else{
						place = 3;
						}
				}			
			}


			if( gusx != 0 ){
				frmtst(&gusx,(char*)formpl[place-One][j-One],&retsw);
				if( retsw != 2 ){
					*retflg = 1;
					return;
					}
				}

			if( diagsX_.deepdi == 1 )
				{
				fprintf( _spec_fp, " NO MATCH IN txformod%7d%7d\n", place, j );
				}
			
			/*         GUSX MAY BE = TO THE DISPLACEMENT GUSN */
			if( tranidX_.tranid == 4 && gusx == gusn ){
				*retflg = 1;
				return;
				}


			*retflg = 2;
			
		
	return;

} /*end of function*/

