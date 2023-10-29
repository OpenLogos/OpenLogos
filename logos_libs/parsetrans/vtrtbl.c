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
	/** CREATED     87/03/26 LG002GBA:
	 *     SUBROUTINE FOR ACQUIRING THE 30/40/50 TABLE VTRS */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*                    DECLARE LINGUISTIC DB ARRAYS - BOTH PASSES. USE
	 *                    DUMMY SIZE PARAMETERS (REAL ONES USED IN TXOPEN())
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *
	 * PROGRAM LOGIC */
	/*   ANALYZE THE CURRENT SWITCH TO DETERMINE EXACTLY WHAT IT IS THAT
	 *     NEEDS TO BE DONE HERE.
	 *     SW-63 -- SEND TO 30 TABLE ENTRY
	 *       THERE CAN BE NO PARMS
	 *     SW-64 -- SEND TO 40 TABLE ENTRY
	 *       GET NUMBER OF PARMS
	 *     SW-65 -- SEND TO 50 TABLE ENTRY
	 *       GET NUMBER OF PARMS
	 *     SW-56 -- WITH 9XX IN 3RD SWITCH ELEMENT =
	 *                NONRETURNABLE SEND TO 40 OR 50 TABLE
	 *              WITH 94X IN 3RD SWITCH ELEMENT = SEND TO 40 TABLE
	 *              WITH 95X IN 3RD SWITCH ELEMENT = SEND TO 50 TABLE
	 *       THERE CAN BE NO PARMS
	 *     SW-66 -- WITH 9XX IN 2ND SWITCH ELEMENT =
	 *                NONRETURNABLE SEND TO 40 OR 50 TABLE
	 *              WITH 94X IN 2ND SWITCH ELEMENT = SEND TO 40 TABLE
	 *              WITH 95X IN 2ND SWITCH ELEMENT = SEND TO 50 TABLE
	 *       THERE CAN BE NO PARMS */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <fcrt.h>
#include "trans.h"
#include "logoslib.h"
#include "projexts.h"
#include "parsetrans_ext.h"
#include <string.h>
#include <jbctrl.h>
#include <logos_libs/lgs_tran_rule_io/tran_rule_io.h>

extern void search_and_replace(short int *, int, short int, short int);

void /*FUNCTION*/ vtrtbl()
{
	static short int cursw2, entry, prmtot, pt, retflg, retsw, st, 
	  tblnum, tblrec, vtrelm, vtrval, x, y, z;
	static long int _l0, _l1, fileid, poswrk, tblid;

	short int *vtraddr, newval;
	long int *arglen, *strlen;

	void sserch(), vtr_t_ld();

	// void search_and_replace(); 

	static char pgmnam[9] = "VTRTBL  ";

	cursw2 = ctltblX_.cursw;
	if( vtrfX_.vtrf[cursw2-One] == -63 ){
		tblnum = 30;
		tblid = 1;
		entry = (1000*(vtrfX_.vtrf[cursw2+1-One])) + vtrfX_.vtrf[cursw2+2-One];
		prmtot = 0;
		ctltblX_.cursw = cursw2 + 4;
		/*   IF IT IS A THIRTY TABLE, COMPARE MOD .EQ. 'TRAN1', 'TRAN2', ETC. */
		}
	else if( vtrfX_.vtrf[cursw2-One] == -64 ){
		tblnum = 40;
		tblid = 2;
		x = (vtrfX_.vtrf[cursw2+1-One]/100)*100;
		y = vtrfX_.vtrf[cursw2+1-One] - x;
		entry = (1000*y) + vtrfX_.vtrf[cursw2+2-One];
		prmtot = vtrfX_.vtrf[cursw2+3-One];
		ctltblX_.cursw = cursw2 + 4 + prmtot;
		}
	else if( vtrfX_.vtrf[cursw2-One] == -65 ){
		tblnum = 50;
		tblid = 3;
		x = (vtrfX_.vtrf[cursw2+1-One]/100)*100;
		y = vtrfX_.vtrf[cursw2+1-One] - x;
		entry = (1000*y) + vtrfX_.vtrf[cursw2+2-One];
		prmtot = vtrfX_.vtrf[cursw2+3-One];
		ctltblX_.cursw = cursw2 + 4 + prmtot;
		}
	else if( vtrfX_.vtrf[cursw2-One] == -56 ){
		/* CLEAR THE ONES DIGIT */
		x = (vtrfX_.vtrf[cursw2+2-One]/10)*10;
		/* GET THE ONES DIGIT */
		z = vtrfX_.vtrf[cursw2+2-One] - x;
		/* CLEAR AND GET THE TENS DIGIT */
		y = x - ((x/100)*100);
		if( y == 40 ){
			tblnum = 40;
			tblid = 2;
			}
		if( y == 50 ){
			tblnum = 50;
			tblid = 3;
			}
		entry = (1000*z) + vtrfX_.vtrf[cursw2+3-One];
		prmtot = 0;
		ctltblX_.cursw = cursw2 + 4;
		}
	else if( vtrfX_.vtrf[cursw2-One] == -66 ){
		/* CLEAR THE ONES DIGIT */
		x = (vtrfX_.vtrf[cursw2+1-One]/10)*10;
		/* GET THE ONES DIGIT */
		z = vtrfX_.vtrf[cursw2+1-One] - x;
		/* CLEAR AND GET THE TENS DIGIT */
		y = x - ((x/100)*100);
		if( y == 40 ){
			tblnum = 40;
			tblid = 2;
			}
		if( y == 50 ){
			tblnum = 50;
			tblid = 3;
			}
		entry = (1000*z) + vtrfX_.vtrf[cursw2+2-One];
		prmtot = 0;
		ctltblX_.cursw = cursw2 + 3;
		}

	/*     LOOK AT THE INDEX TO DECIDE IF MINI OR MAIN
	 *     CHECK WHICH PASS, THEN CHECK FOR MINI B4 MAIN */

	tblrec = 0;

	tblnum = -1;
	if( tblid == 1 )
		tblnum = 30;
	if( tblid == 2 )
		tblnum = 40;
	if( tblid == 3 )
		tblnum = 50;
	/*winnt			use new routine to load vtr array and set count
	 *winnt			return 0 = table gotten ok
	 *winnt				   1 = index does not point to entry in table indexes
	 *winnt				   2 = value in table indexes does not point to valid
	 *winnt					   record. */
	vtr_t_ld(&tblid,&entry,&vtrelm,ctltblX_.tblvtr,&fileid,&retsw);
	if( retsw == 1 ){
		ctltblX_.tblvln = 0;
		}
	else if( retsw == 2 ){
		errvrsX_.errlvl = 9;
		ctltblX_.tblvln = 0;
		}
	else{
		/*winnt
		 *				number of bytes in TBLVTR holding integer values */
		ctltblX_.tblvln = vtrelm*2;

		/*winnt			replace some parameters in the vtr table line
		 *winnt			was done below in the 5010 section */
		if( prmtot < 0 || prmtot > 99 ){
			errlog(pgmnam,5000,0,9);
			if( opswX_.sw[3-One] != 0 )
				{
				fprintf( _spec_fp, " %3d TABLE ENTRY %6dHAS A CALL WHICH EXCEEDS            99 ARGUMENTS\n", 
				  tblnum, entry );
				}
			}
		else if( prmtot > 0 ) {
			for( x=1; x <= prmtot; x++ ) {
				y = -1000 - x;
				newval = vtrfX_.vtrf[cursw2+3+x-One];
				search_and_replace(ctltblX_.tblvtr, vtrelm*2, y, newval); 

/*				arglen = ADR(_l0,vtrelm*2);
				strlen = ADR(_l1,2);
				vtraddr = &vtrfX_.vtrf[cursw2+3+x-One];
				while( TRUE ){
					sserch(ctltblX_.tblvtr, arglen, &y, strlen, &poswrk);
					if( poswrk <= 0 )
						break;
					lmove(ctltblX_.tblvtr, poswrk, vtraddr, 1, 2);
				}
*/
			}
		}



		/*   NOW WRITE OUT THE TABEL VTR. */
		if( opswX_.sw[3-One] != 0  )
		{
			// Fixing problem of parameters were not substituted in
			// the table printout (problem reprted by Patty on 7/13/98)
			// (the above piece of code substitutes parameters in ctltblX_.tblvtr,
			// and the table is printed out from the master copy)

			// Two arguments have been added
			// because now we need access to current switch
			// to make parameter substitutions
			print_table(_spec_fp, tblid, entry,
				prmtot,						// #of parms
				&vtrfX_.vtrf[cursw2+3]		// address of the 1st parameter
				);
		}
	}

	retflg = 0;

	return;
} /*end of function*/

void search_and_replace(short int *arr, int arrsz, short int s, short int newval){
	int i;

	for(i=0;i<arrsz;i++) {
		if(arr[i]==s) {
			arr[i] = newval;
		}
	}
}

