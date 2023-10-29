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
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/** CREATED     87/03/25 LG002GBA:
	 *     SUBROUTINE FOR CONTROLLING THE EXECUTION OF VTRS */
	/*     CONTROL INSTRUCTIONS TO THE DRIVER PROGRAM ARE COMMUNICATED */
	/*        GETVTR = 1  WC9 OR WC10 MATCH. BRANCH TO VTR LOOP CHECK.
	 *        ERRLVL = 13  ERROR, GET NEXT VTR ELEMENT
	 *        ERRLVL  NE 13 AND N3 0, ERROR, RETURN TO DRIVER */
	/*  CHANGES:
	 *    01/24/94 jal: limit number of tables that can be called per
	 *           rule to avoid infinite loop problem. check here against
	 *           the limit TBLOOP before getting next table.
	 *    09/25/91 JAL:  LI IS ALREADY THE LEVEL VALUE DESIRED. */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/* PARAMETERS */
	/*   PARAMETER FOR STACK DIMENSION, VTR DIMENSION, AND STACK LENGTHS */
	/* DECLARATIONS */
	/*   STANDARD DATA FIELDS FOR ERROR CONDITIONS */
	/*   STANDARD SWITCH ARRAY */
	/*   WORK AREAS */
	/*   DATA AREAS FOR VTR BRANCHING */
	/*   POINTER AREAS FOR ANALYZING SWITCHES IN THE VTR */
	/*   STANDARD AREAS FOR LABELLING PURPOSES */
	/*   DATA AREAS FOR VTR */
	/*   DATA AREAS FOR VTR SUPPORT INFORMATION */
	/*   DATA AREAS FOR STACK */
	/*                 NOTE: THE STORAGE FOR STKVF, STKVFM IS POOLED
	 *                       IE. NEW INFO WILL BE PLACED IN FIRST FREE SPACE */
	/*   DATA AREAS FOR STACK CONTROL */
	/*                 NOTE: STKVBG SAVES THE BEGINNING POSITION FOR
	 *                       EACH POOL ENTRY */
	/*   DATA AREAS FOR COMMUNICATION BETWEEN VTRCTL AND VTRTBL */
	/*   DATA AREAS FOR VARIOUS COMMONS */
	/*   DATA AREAS FOR VERY TEMPORARY USE */
	/* COMMONS */
	/*   COMMONS FOR VTR */
	/*   COMMON FOR VTR SUPPORT INFORMATION */
	/*   COMMON FOR ERROR CONDITIONS */
	/*   COMMONS FOR FLAGS AND SWITCHES */
	/*   COMMON FOR VTR BRANCHING */
	/*   COMMON FOR VTR ANALYSIS */
	/*   COMMON     FOR COMMUNICATION BETWEEN VTRCTL AND VTRTBL */
	/* EQUIVALENCES */
	/* DATA VALUE ASSIGNMENTS */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */
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

#define	POOLDM	(STKDIM*VTRDIM/4)
#define	STKDIM	20
#define	VTRLEN	(2*VTRDIM)



void /*FUNCTION*/ vtrctl()
{
	static byte stkvfm[POOLDM];
	static short int dflt30, level, qnum, stkbrk[STKDIM][99], stkcnt, 
	  stkcon[STKDIM], stkdon[STKDIM], stkend[STKDIM], stkiz4[STKDIM], 
	  stkk3[STKDIM], stkk3n[STKDIM], stkpt9[STKDIM], stkvbg[STKDIM], 
	  stkvf[POOLDM], svtrbg, work, x, z,k7;
	static long int _l0, errval, pos41, work4;
	void sserch(), vtrpro();
	static char pgmnam[9] = "VTRCTL  ";
	static short sw41 = -41;


	/*   ZERO THE STACK COUNTER */
	stkcnt = 0;
	dflt30 = 0;


	/*   EXECUTE THE PRESENT VTR */

	while( TRUE ){

		vtrpro();
		if( errvrsX_.errlvl != 0 )
			return;

		/*   IF THE FLAG IS SET FOR WC09, THEN RETURN TO CALLER */

		if( getvtrX_.getvtr == 1 )
			return;

		/*   IF THE CURRENT SWITCH IS 999 (END OF CURRENT VTR)
		 *     CHECK STACK COUNTER
		 *     IF STACK COUNTER = 0 (NO VTRS ARE STACKED)
		 *       RETURN TO CALLER
		 *     ELSE (SOME VTRS ARE STACKED)
		 *       RETREIVE STACKED INFO FOR MOST RECENT STACK ENTRY (POP VTR)
		 *       SET VTR AND VARIABLES
		 *       REDUCE STACK COUNTER BY 1 (REFLECT THE POP FROM STACK)
		 *     ENDIF */

		vtrnX_.vtrn = vtrfX_.vtrf[vbdataX_.k3-One];
		if( vbdataX_.k3 >= vwarg2X_.iz4 || vtrnX_.vtrn == 999 ){
			if( stkcnt == 0 )
			{
				vtrend();
				return;
			}
			vwarg2X_.iz4 = stkiz4[stkcnt-One];
			svtrbg = stkvbg[stkcnt-One];
			work4 = 2*vwarg2X_.iz4;
			lmove(vtrfX_.vtrf,1,&stkvf[svtrbg-One],1,work4);
			work4 = vwarg2X_.iz4;
			lmove((short*)vtrfmX_.vtrfm,1,(short*)&stkvfm[svtrbg-One], 1,work4);
			vbdataX_.k3 = stkk3[stkcnt-One];
			vbdataX_.k3n = stkk3n[stkcnt-One];
			lmove(&vbdataX_.vbrkpt[1-One],1,&stkbrk[stkcnt-One][1-One],1,198);
			vbdataX_.vbkend = stkend[stkcnt-One];
			vbdataX_.vbkcon = stkcon[stkcnt-One];
			cnX_.vtrdon = stkdon[stkcnt-One];
			semargX_.pntr9 = stkpt9[stkcnt-One];

			stkcnt -= 1;


			/*   ELSEIF THE CURRENT SWITCH IS -63, -64, OR -65 (SEND TO 30/40/50)
			 *     ACQUIRE THE CORRECT TABLE ENTRY
			 *     IF THERE WAS NO VTR
			 *       IF DEFAULTABLE 30 TABLE ENTRY
			 *         DEVELOP THE DEFAULT
			 *       ENDIF
			 *     ENDIF
			 *     IF WE HAVE A NEW VTR TO EXECUTE
			 *       IF TABLE ENTRY IS NONRETURNABLE
			 *         SET STACK COUNTER = 0
			 *       ELSE
			 *         ADD 1 TO STACK COUNTER
			 *         SET STACK ENTRY TO CURRENT VTR AND RELATED VARIABLES
			 *       ENDIF
			 *       RESET THE RELATED VARIABLES TO START OF NEW VTR.
			 *     ENDIF */

			}
		else if( (vtrnX_.vtrn == -63 || vtrnX_.vtrn == -64) || vtrnX_.vtrn == -65 ){
			/*                                                   01/24/94  jal
			 *               if limit on tables/rule reached, assume infinite loop
			 *               abort this rule, by returning to caller for next rule. */
			tablctX_.tablct += 1;
			if( tablctX_.tablct > TBLOOP )
				break;

			/*  NOW GO GET THE TABLE ENTRY */

			ctltblX_.cursw = vbdataX_.k3;
			vtrtbl();
			vbdataX_.k3n = ctltblX_.cursw;

			/*  LOGIC TO DEFAULT A MISSING 30-TABLE ENTRY IF NECESSARY */
			if( (ctltblX_.tblvln <= 0 && dflt30 == 0) && vtrnX_.vtrn ==  -63 ){
				qnum = vtrfX_.vtrf[vbdataX_.k3+3-One];
				if( qnum == 2 ){
					dflt30 = 1;
					/*           DEFAULT THE 30-TABLE ENTRY */
					work4 = vwarg2X_.iz4*2;
/*					sserch(vtrfX_.vtrf,&work4,&sw41,ADR(_l0,2),&pos41); 
*/
					level = sploopX_.li;
					if( diagsX_.longdi != 0 )
						{
						fprintf( _spec_fp, "\n DEFAULT 30 TABLE -- %2d LEVEL\n", level );
						}
					ctltblX_.tblvln = (level*4) + 2;
					ctltblX_.tblvtr[1-One] = 999;
					z = 0;
					if( level > 0 ){
						for( x=1; x <= (level*2); x += 2 ){
							z -= 1;
							ctltblX_.tblvtr[x-One] = z;
							ctltblX_.tblvtr[x+1-One] = 0;
							ctltblX_.tblvtr[x+2-One] = 999;
							}
						}
					}
				}

			if( ctltblX_.tblvln > 0 ){

				/*  IF THE SEND IS NONRETURNABLE 40/50, FORGET THE PAST
				 *    FOR -64,-65 THE CALL IN NONRETURNABLE IF SWITCH ELEMENT 2
				 *    HAS A 1 AS THE HUNDREDS DIGIT */

				work = (vtrfX_.vtrf[vbdataX_.k3+1-One]/100)*100;
				if( work == 100 ){
					stkcnt = 0;
					}
				else{
					if( stkcnt == 0 )
						svtrbg = 1;
					if( stkcnt > 0 )
						svtrbg = stkvbg[stkcnt-One] + stkiz4[stkcnt-One];
					work = svtrbg + vwarg2X_.iz4;
					if( stkcnt < STKDIM && work <= POOLDM ){
						stkcnt += 1;
						stkvbg[stkcnt-One] = svtrbg;

						work4 = 2*vwarg2X_.iz4;
						lmove(&stkvf[svtrbg-One],1,vtrfX_.vtrf,1,work4);
						work4 = vwarg2X_.iz4;
						lmove((short*)&stkvfm[svtrbg-One],1,(short*)vtrfmX_.vtrfm, 1,work4);
						stkk3[stkcnt-One] = vbdataX_.k3;
						stkk3n[stkcnt-One] = vbdataX_.k3n;
						stkiz4[stkcnt-One] = vwarg2X_.iz4;
						lmove(&stkbrk[stkcnt-One][1-One],1,&vbdataX_.vbrkpt[1-One],1,198);
						stkend[stkcnt-One] = vbdataX_.vbkend;
						stkcon[stkcnt-One] = vbdataX_.vbkcon;
						stkdon[stkcnt-One] = cnX_.vtrdon;
						stkpt9[stkcnt-One] = semargX_.pntr9;
						}
					else{
						/*             ERROR ‹‹‹‹  ALREADY MAX NUMBER OF ENTRIES IN STACK
						 *             ERRLVL = 13 GET NEXT ELEMENT IN VTR */
						errvrsX_.errlvl = 13;
						errlog(pgmnam,5099,13,0);
						}
					}

				/*  AND REPLACE THE CURRENT VTR (IT IS ALREADY SAVED IF RETURNABLE)
				 *    REPLACE IT WITH THE TABLE VTR
				 *    RESET ALL THE POINTERS */

				memcpy(vtrX_.vtr,ctltblX_.tblvtr,sizeof(vtrX_.vtr)); 
				vbdataX_.vbkend = 0;
				vbdataX_.vbkcon = 0;
				cnX_.vtrdon = 0;
				semargX_.pntr9 = 0;
				zapit(vbdataX_.vbrkpt,198,(byte)0);
				vbdataX_.k3 = 1;
				vbdataX_.k3n = 1;
				vwarg2X_.iz4 = ctltblX_.tblvln/2;
				vtrfwr();
				}



			}
			/*   ELSEIF THE CURRENT SWITCH IS -56, -66 (NONRETURNABLE 40/50)
			 *     ACQUIRE THE CORRECT TABLE ENTRY
			 *     SET STACK COUNTER = 0
			 *     RESET THE RELATED VARIABLES TO START OF NEW VTR. */
		else if( vtrnX_.vtrn == -56 || vtrnX_.vtrn == -66 ){

			/*  FOR VTRPRO TO PASS THE SWITCH BACK HERE, THE -56 -66 SWITCH MUST
			 *    BE A NONRETURNABLE CALL TO THE 40 OR 50 TABLE. */


			/*  NOW GO GET THE TABLE ENTRY */

			ctltblX_.cursw = vbdataX_.k3;
			vtrtbl();
			vbdataX_.k3n = ctltblX_.cursw;
			if( ctltblX_.tblvln > 0 ){

				/*          SINCE THIS MUST BE NONRETURNABLE, FORGET THE PAST */
				stkcnt = 0;

				/*  AND REPLACE THE CURRENT VTR (IT IS ALREADY SAVED) WITH THE TABLE
				 *    RESET ALL THE POINTERS */

				memcpy(vtrX_.vtr,ctltblX_.tblvtr,sizeof(vtrX_.vtr)); 
				vbdataX_.vbkend = 0;
				vbdataX_.vbkcon = 0;
				cnX_.vtrdon = 0;
				semargX_.pntr9 = 0;
				zapit(vbdataX_.vbrkpt,198,(byte)0);
				vbdataX_.k3 = 1;
				vbdataX_.k3n = 1;
				vwarg2X_.iz4 = ctltblX_.tblvln/2;
				vtrfwr();
				}
			}


		else{
			/*  ERROR ‹‹‹‹  BAD RETURN FROM CALLED PROGRAM ‹‹‹‹
			 *              ERRLVL = 13 GET NEXT ELEMENT IN VTR */
			errval = vtrnX_.vtrn;
			errvrsX_.errlvl = 13;
			errlog(pgmnam,7099,13,errval);
			for (x=0;x<300;x++)
				printf("%5d",vtrfX_.vtrf[x]);
			printf("\n");

			diag_write_shorts("vtr array which contains the bad value for the rule \n",
							vtrfX_.vtrf, 300, 20, 5,
							"");
/* need to figure out how to get the rule number before we print it
			if(tranidX_.tranid == 1)
				k7 = diacbX_.k7;
			else if(tranidX_.tranid == 2)
				k7 = diacb2X_.k7;
			else if(tranidX_.tranid == 3)
				k7 = diacb3X_.k7;
			else if(tranidX_.tranid == 4)
				k7 = diacb4X_.k7;
			else
				k7 = diacbX_.k7;
			print_tran_rule(_spec_fp, 1, k7);
*/

			}


		if( errvrsX_.errlvl == 0 ){
					/*     GETVTR IS SET BY THE 22, 28, 29, 36 SWITCHES TO INDICATE THAT A
					 *     WC 9 OR WC10 VTR SHOULD BE PROCESSED. RETURN TO DRIVER TO BEGIN
					 *     PROCESSING THIS NEW VTR. */
			if( getvtrX_.getvtr == 1 )
				return;
			}
					/*  ERRLVL = 13  INSTRUCTS US TO GET NEXT VTR ELEMENT
					 *  JUST LEAVING THE POINTERS AS IS WILL CAUSE VTRPRO TO BEGIN
					 *  AT THE NEXT SWITCH IN THE VTR. */
		else if( errvrsX_.errlvl == 13 ){
			errvrsX_.errlvl = 0;
			if( vbdataX_.k3n <= vbdataX_.k3 )
				vbdataX_.k3n = vbdataX_.k3 + 1;
			}
		else{
			return;
			}
		}


	errlog(pgmnam,5002,0,9);
	if(diagsX_.longdi != 0 )
	{
		fprintf( _spec_fp, "\nERROR: exceeded limit on tables per rule.(limit= %4ld).\n       Aborting rule and continuing.\n\n", TBLOOP );
	}
	return;


} /*end of function*/

