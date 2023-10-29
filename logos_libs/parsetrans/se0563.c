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
#include <string.h>
#include "parsetrans_ext.h"
#include <jbctrl.h>



void /*FUNCTION*/ se0563(k4, igus, igusav, retflg)
long int k4;
short int *igus;
long int igusav;
short int *retflg;
{
	static short int semcod[21][6];
	long int _i;
	struct  {
		short int spkey[53], spdata[128];
		}	*_sp5rulX_ = (void*)&sp5rulX_;
	struct t_smcdptX_ {
		long int f32;
		}	smcdptX_;

	static char pgmnam[9] = "SEMT0563";
	static short tmp = 0;
	static short scdpt = 0;
	static int _aini = 1;
	if( _aini ){ /* Do 1 TIME INITIALIZATIONS! */
		for(_i=0L; _i < sizeof(semcod)/sizeof(short); _i++)
			((short*)semcod)[_i] = 0;
		_aini = 0;
	}

	/*     ROUTINE TO MATCH SEMODES TO THE CURRENT RULE
	 *        INPUT:
	 *           K4        = ORIGINAL SWORK POINTER
	 *           IGUS      = POINTER TO SPDATA AREA
	 *           IGUSAV    = SAVED STARTING IGUS
	 *        OUTPUT:
	 *           RETFLG  1 = MATCH
	 *                   0 = NOMATCH OR READD ERROR (NOT FATAL, CONTINUE) */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	*retflg = 0;

	/*                                     GERMAN-ENGLISH ONLY */
	if( srcflgX_.srcflg == 1 ){

		/*                                     READ IN THE LIST OF SEMCODES
		 *                                     FOR THIS SWORK
		 *---- F32 = (K4 * 3) - 3 + JS(SCOLNK(K4))
		 *     WRITE(6,99)RMORST
		 *     WRITE(7,99)RMORST
		 *99    FORMAT(' IN 563, RMORST = ',I6) */

		tmp = (k4*3) - 3 + prctX_.js[sconX_.scolnk[k4-One]-One] + 
		  rmorstX_.rmorst - 1;
		/*winnt      CALL A4SET2 (F32,TMP) */
		smcdptX_.f32 = tmp;
		/*winnt      CALL LREADD(32, SEMCOD, ERR) */
		if( errvrsX_.err == 0 ){

			/*                        SET BACK TO START OF RULE SEMCODES */
			*igus = igusav;


			while( TRUE ){
				scdpt = 1;

				if( curr_sp5rule_ptr->spdata[*igus-One] == -1 )
					return;
				/*                                                     PRIME INDEX */
				while( semcod[scdpt-One][1-One] != 0 ){
					if( semcod[scdpt-One][1-One] == curr_sp5rule_ptr->spdata[*igus-One] ){
						/*                                                     PART OF SPEECH */
						if( curr_sp5rule_ptr->spdata[*igus+1-One] != 0 ){
							if( semcod[scdpt-One][2-One] != curr_sp5rule_ptr->spdata[*igus+1-One] )
								goto L_270;
							}
						/*                                                     MAJOR DIVISION */
						if( curr_sp5rule_ptr->spdata[*igus+2-One] != 0 ){
							if( semcod[scdpt-One][3-One] != curr_sp5rule_ptr->spdata[*igus+2-One] )
								goto L_270;
							}
						/*                                                     MINOR DIVISION */
						if( curr_sp5rule_ptr->spdata[*igus+3-One] == 0 )
							goto L_700;
						if( semcod[scdpt-One][4-One] == curr_sp5rule_ptr->spdata[*igus+3-One] )
							goto L_700;
						}

					/*                        POINT TO NEXT SEMCODE FROM TABLE
					 *                              (SKIP 4 BYTE ADDRESS) */
L_270:
					scdpt += 1;
					}

				/*                        POINT TO NEXT SEMCODE FROM RULE */
				*igus += 4;
				}

L_700:
			*retflg = 1;

			}
		else{
			errlog(pgmnam,300,0,0);
			if( opswX_.sw[3-One] == 1 )
				{
				fprintf( _spec_fp, " ERROR IN SEMT0563 MATCH SEMCODE ROUTINE ERR,F32 = %5ld%5ld\n", 
				  errvrsX_.err, smcdptX_.f32 );
				}
			}
		}
	return;
} /*end of function*/

