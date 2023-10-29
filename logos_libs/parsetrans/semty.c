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
	/* CHANGES:
	/*              SUPSCN = list of SCONs in each superscon set. */
	/*                                800 =  superscon 1
	 *                                801 =  superscon 2
	 *                                802 =  superscon 3
	 *              SSCFRS,SSCLST = ptr to list of scons in SUPSCN */
	/*              SSCCNT = number of superscons */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */

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
#include <jbctrl.h>



void /*FUNCTION*/ semty(
	long int k,
	long int k4,
	long int g,
	short int *gusn,
	short int *match)
{
	long int scptr = 0;
	static short int equal, exclud, g2, k2, opid, parm1, parm2, supval,x, xx;
	static short ms = 0;
	static short pt = 0;
	static short igus = 0;
	static short retsw = 0;
	static short igusav = 0;
	static short rultyp = 0;
	static short htst[2];
	static short typtst[3]={0,0,0};
	static char pgmnam[9] = "SEMTY   ";
	static short supscn[34]={31,32,33,34,35,37,38,39,40,41,42,43,31,32,33,34,35,37,38,39,40,41,43,32,33,34,35,37,38,39,40,41,42,43};
	static short sscfrs[3]={1,13,24};
	static short ssclst[3]={12,23,34};
	static short ssccnt = 3;

	pt = typtrX_.typtr + ((g - 2)*2);

	ms = 16 + curr_sp5rule_ptr->spdata[15] + (g - 1);
	igus = curr_sp5rule_ptr->spdata[ms-One];
	igusav = igus;


	if( *gusn < 6000 )
	{
		if( tranidX_.tranid != 1){
			scptr = sworkX_.phrhed[semtrX_.orig[k-One]-One];
		}
		else{
			scptr = k4;
		}
		typtst[0] = semfrmX_.semwrk[k-One][1];
		typtst[1] = sconX_.scon[scptr-One][11-One];
		typtst[2] = sconX_.scon[scptr-One][13-One];

		/*   IN CASE ANY OF THE TYPTST VALUES ARE ZERO SET THEM TO A FAKE VALUE
		 *   SO MATCH2 ROUTINE WON'T QUIT */
		if( typtst[0] == 0 )typtst[0] = 1999;
		if( typtst[1] == 0 )typtst[1] = 1999;
	}

	
	
	
					/*   SPECIFIC TYPE */
	if( *gusn < 1000 ){
		match2(gusn,typtst,3,&retsw);
		if( retsw == 1 )
			goto L_2260;
		if( retsw == 2 )
			goto L_2300;
		}


	/*   A GROUP OF TYPE CODES IS SIGNALLED BY A 1000 IN THE TYPE FIELD. */
	if( *gusn == 1000 ){

		rultyp = curr_sp5rule_ptr->spkey[pt-One];

		match2(&rultyp,typtst,3,&retsw);
		if( retsw == 1 )
			goto L_2260;

		rultyp = curr_sp5rule_ptr->spkey[pt+1-One];
		if( rultyp != 0 ){

			match2(&rultyp,typtst,3,&retsw);
			if( retsw == 1 )
				goto L_2260;

			/*   THE REST OF THE TAGSET IS IN THE DATA PORTION */
			if( igus != 0 ){

				while( TRUE ){
					rultyp = curr_sp5rule_ptr->spdata[igus-One];
					if( rultyp == -1 )
						break;

					match2(&rultyp,typtst,3,&retsw);
					if( retsw == 1 )
						goto L_2260;

					igus += 1;
					}
				}
			}
		}

	
	/*   2000 IN TYPE FIELD MEANS CHECK SEMCODES */
	else if( *gusn == 2000 ){
		se0563(k4,&igus,igusav,&retsw);
		if( retsw == 1 )goto L_2260;
		}


		/*   HENUM OR HASH CODE MATCH
		 *   3000 IN THE TYPE FIELD MEANS CHECK BOTH
		 *   5000 IN THE TYPE FIELD MEANS CHECK ONLY THE HENUM */
	else if( *gusn == 3000 || *gusn == 5000 ){

			/*   TO AVOID SEARCH OF DUMMY SEMWRKS */
		if( k4 != 0 ){

			htst[0] = hensavX_.henum2[sconX_.scolnk[k4-One]-One][0];
			htst[1] = hensavX_.henum2[sconX_.scolnk[k4-One]-One][1];
			if( htst[0] != -1 ){
				ccompx((char*)htst,1,(char *)&curr_sp5rule_ptr->spkey[pt-One],1,4,&retsw);
				if( retsw == 0 )goto L_2260;   // 0=match!

				if( igus != 0 ){
					match4(htst,&curr_sp5rule_ptr->spdata[igus-One],64,&retsw);
					if( retsw == 1 )goto L_2260;  //1=match!
					}

				// check root hash code
				htst[0] = hensavX_.root_henum2[sconX_.scolnk[k4-One]-One][0];
				htst[1] = hensavX_.root_henum2[sconX_.scolnk[k4-One]-One][1];
				ccompx((char*)htst,1,(char *)&curr_sp5rule_ptr->spkey[pt-One],1,4,&retsw);
				if( retsw == 0 )goto L_2260;   // 0=match!

				if( igus != 0 ){
					match4(htst,&curr_sp5rule_ptr->spdata[igus-One],64,&retsw);
					if( retsw == 1 )goto L_2260;  //1=match!
					}

				}

			if( *gusn == 3000 ){

				igus = igusav;

				htst[0] = hashX_.hashcd[sconX_.scolnk[k4-One]-One][0];
				htst[1] = hashX_.hashcd[sconX_.scolnk[k4-One]-One][1];
				if( htst[0] != 0 ){

					ccompx((char *)htst,1,(char *)&curr_sp5rule_ptr->spkey[pt-One], 1,4,&retsw);
					if( retsw == 0 )goto L_2260;

					if( igus != 0 )	{
						match4(htst,&curr_sp5rule_ptr->spdata[igus-One],64,&retsw);
						if( retsw == 1 )goto L_2260;
					}
					// check root hash code
					htst[0] = hashX_.root_hashcd[sconX_.scolnk[k4-One]-One][0];
					htst[1] = hashX_.root_hashcd[sconX_.scolnk[k4-One]-One][1];
					ccompx((char*)htst,1,(char *)&curr_sp5rule_ptr->spkey[pt-One],1,4,&retsw);
					if( retsw == 0 )goto L_2260;   // 0=match!

					if( igus != 0 ){
						match4(htst,&curr_sp5rule_ptr->spdata[igus-One],64,&retsw);
						if( retsw == 1 )goto L_2260;  //1=match!
					}

				}
			}
		}
			goto L_2300;   // no match
	}




		/*   4000 IN THE TYPE FIELD MEANS CHECK 2 TYPE FIELDS */
		else if( *gusn == 4000 ){
			rultyp = curr_sp5rule_ptr->spkey[pt-One];
			match2(&rultyp,typtst,3,&retsw);
			if( retsw != 2 ){

				rultyp = curr_sp5rule_ptr->spkey[pt+1-One];
				match2(&rultyp,typtst,3,&retsw);
				if( retsw == 1 )
					goto L_2260;
				}

			if( igus != 0 ){

				while( TRUE ){
					rultyp = curr_sp5rule_ptr->spdata[igus-One];
					if( rultyp == -1 )
						break;

					match2(&rultyp,typtst,3,&retsw);
					if( retsw != 2 ){
						rultyp = curr_sp5rule_ptr->spdata[igus+1-One];
						match2(&rultyp,typtst,3,&retsw);
						if( retsw == 1 )
							goto L_2260;
						}
					igus += 2;
					}
				}
			}


		else if( *gusn == 6012 ){

			/*    6012 TAGSET IS AN EXCLUSION TAG
			 *         6012 008X 0YYY
			 *         008X POINTS TO THE SP LINE OF THE RULE. THIS IS THE ELEMENT
			 *              TO TEST (MUST BE THE CURRENT ELEMENT OR A PREVIOUS ONE)
			 *         0YYY IS THE TYPE VALUES TO TEST FOR.  IF FOUND, THIS IS A
			 *              NO MATCH */

			parm1 = curr_sp5rule_ptr->spkey[pt-One];
			parm2 = curr_sp5rule_ptr->spkey[pt+1-One];

			while( TRUE ){
				g2 = parm1 - 79;
				if( g2 != g ){
					k2 = sgnX_.gn[g2-One];
					if( k2 == 0 )
						break;
					if( tranidX_.tranid != 1){
						scptr = sworkX_.phrhed[semtrX_.orig[k2-One]-One];
					}
					else {
						scptr = semfrmX_.semwrk[k2-One][4-One];
					}
					typtst[0] = semfrmX_.semwrk[k2-One][1];
					typtst[1] = sconX_.scon[scptr-One][11-One];
					typtst[2] = sconX_.scon[scptr-One][13-One];
					if( typtst[1] == 0 )typtst[1] = 1999;
					if( typtst[2] == 0 )typtst[2] = 1999;
					}

				match2(&parm2,typtst,3,&retsw);
				if( retsw == 1 )
					break;

				if( igus == 0 ) goto L_2260;
				parm1 = curr_sp5rule_ptr->spdata[igus-One];
				if( parm1 == -1 ) goto L_2260;
				parm2 = curr_sp5rule_ptr->spdata[igus+1-One];
				igus += 2;
				}
			}


		else if( *gusn == 6050 ){

			/*     <* Exit with failure if no parameter,value pairs found *> */
			if( curr_sp5rule_ptr->spkey[pt-One] != 0 ){

				/*	<* Get type of test *> */
				opid = curr_sp5rule_ptr->spkey[pt-One]/1000;
				/*	<* Get the two parameters: cell number and value *> */
				parm1 = curr_sp5rule_ptr->spkey[pt-One] - opid*1000;
				parm2 = curr_sp5rule_ptr->spkey[pt+1-One] - 8000;

				/*	<* Perform the required test *> */
				while( TRUE ){
					if( opid == 0 ){
						if( vbdataX_.vbcell[parm1-One] != parm2 )
							break;
						}
					else if( opid == 1 ){
						fprintf( _spec_fp, "Not implemented yet\n" );
						}
					else if( opid != 2 ){
						fprintf( _spec_fp, "Not implemented yet\n" );
						}
					else if( vbdataX_.vbcell[parm1-One] == parm2 ){
						break;
						}
					/*	<* If end of tagset, test succeeded *> */
					if( igus == 0 || curr_sp5rule_ptr->spdata[igus-One] == -1 )
						goto L_2260;

					/*	<* Get type of test *> */
					opid = curr_sp5rule_ptr->spdata[igus-One]/1000;
					/*	<* Get the two parameters: cell number and value *> */
					parm1 = curr_sp5rule_ptr->spdata[igus-One] - opid*1000;
					parm2 = curr_sp5rule_ptr->spdata[igus+1-One] - 8000;
					/*	<* Increment the index to get two more parameters *> */
					igus += 2;
					/*	<* Loop back to test next 2 parameters *> */
					}
				}
			}

		else if( *gusn >= 6081 && *gusn <= 6090 ){
			/*   608X TAGSET TESTS FOR SCON VALUES
			 *        608X 00YY 8ZZZ
			 *          8X POINTS TO THE ELEMENT IN THE SP LINE
			 *          YY IS THE SCON NUMBER TO TEST.  IF THE 1ST DIGIT OF THIS
			 *             PARAMETER IS 2, A MATCH MEANS NO MATCH ON THE TAG
			 *         ZZZ IS THE VALUE TO TEST FOR IN THE SCON */

			parm1 = curr_sp5rule_ptr->spkey[pt-One];
			parm2 = curr_sp5rule_ptr->spkey[pt+1-One] - 8000;

			if( tranidX_.tranid != 1){
				scptr = sworkX_.phrhed[semtrX_.orig[k-One]-One];
			}
			else{
				scptr = k4;
			}

			g2 = *gusn - 6079;
			if( g2 != g ){
				k2 = sgnX_.gn[g2-One];
				if( k2 == 0 )
					goto L_2300;
				if( tranidX_.tranid != 1){
					scptr = sworkX_.phrhed[semtrX_.orig[k2-One]-One];
				}
				else {
					scptr = semfrmX_.semwrk[k2-One][4-One];
				}
			}

			while( TRUE ){
				if( parm1 > 2000 ){
					parm1 -= 2000;
					exclud = 1;
					}
				else{
					exclud = 0;
					}


				equal = 0;
				if( parm1 <= SCONX1 ){
					if( sconX_.scon[scptr-One][parm1-One] == parm2 )
						equal = 1;
					}
				else if( parm1 <= SCONX ){
					if( sconX_.scono[sconX_.scolnk[scptr-One]-One][parm1-SCONX1-One] == parm2 )
						equal = 1;
					/*                    superscon test - against a list of SCONs */
					}
				else{
					supval = parm1 - 799;
					if( supval < 1 || supval > ssccnt ){
						if( diagsX_.anydi )
						{
							fprintf( _spec_fp, "              ***** ERROR ******\n Invalid superscon parameter in sp5 608X tagset  \n  Bad parameter = %3d\n              ******************\n",  parm1 );
						}
						errvrsX_.linenm = parm1;
						errlog(pgmnam,6092,errvrsX_.linenm,9);
						}
					else{
						for( xx=sscfrs[supval-One]; xx <= ssclst[supval-One]; xx++ ){
							parm1 = supscn[xx-One];
							if( parm1 <= SCONX1 ){
								if( sconX_.scon[scptr-One][parm1-One] != parm2 )
									goto L_6201;
								}
							else if( sconX_.scono[sconX_.scolnk[scptr-One]-One][parm1-SCONX1-One] != parm2 ){
								goto L_6201;
								}
							}
						/*                  Fall thru = match on all SCONS in superscon set */
						equal = 1;
						}
					}
				/*                  was match succssful */
L_6201:
				if( exclud == 0 ){
					if( equal != 1 )
						goto L_6200;
					}
				else if( equal != 0 ){
					goto L_6200;
					}

				/*  THIS TERM MATCHES */
				if( igus == 0 )
					goto L_2260;
				parm1 = curr_sp5rule_ptr->spdata[igus-One];
				if( parm1 == -1 )
					goto L_2260;
				if( parm1 != 7777 )
					goto L_2260;
				/* IF THIS TERM IS FOLLOWED BY AN .AND. */
				parm1 = curr_sp5rule_ptr->spdata[igus+1-One];
				parm2 = curr_sp5rule_ptr->spdata[igus+2-One] - 8000;
				igus += 3;
				continue;

				/*  THIS TERM DOES NOT MATCH */
L_6200:
				while( TRUE ){
					if( igus == 0 )
						goto L_2300;
					parm1 = curr_sp5rule_ptr->spdata[igus-One];
					if( parm1 == -1 )
						goto L_2300;
					if( parm1 != 7777 )
						break;
					/* IF THIS TERM IS FOLLOWED BY AN .AND. DECLARE THE NEXT TERM FALSE */
					parm1 = curr_sp5rule_ptr->spdata[igus+1-One];
					parm2 = curr_sp5rule_ptr->spdata[igus+2-One] - 8000;
					igus += 3;
					}
				parm2 = curr_sp5rule_ptr->spdata[igus+1-One] - 8000;
				igus += 2;
				}
			}


L_2300:
	*match = 0;
	return;

L_2260:
	*match = 1;
	return;

} /*end of function*/

