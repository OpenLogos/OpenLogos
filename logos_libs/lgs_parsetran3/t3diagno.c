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
program. If not, write to Globalware AG, Hospitalstraße 6, D-99817 Eisenach.

Linux port modifications and additions by Bernd Kiefer, Walter Kasper,
Deutsches Forschungszentrum fuer kuenstliche Intelligenz (DFKI)
Stuhlsatzenhausweg 3, D-66123 Saarbruecken
*/


/*  SUBROUTINE DIAGNO(DIAN) */

/* INPUT:
 *  DIAN =
 *        1  = INIT PRINT PARAMETERS
 *        2  =  PRINT A RULE MATCH
 *        3  =  PRINT A  RULE MATCH
 *        4  =  PRINT SWORK ARRAY
 *        5  =  PRINT SWORK ARRAY W/ NWSTRG
 *        6  =  PRINT VTRF UNTIL SW=999
 *        7  =  PRINT OPADRO,SWORKO AND SCONS LOADED BY A RULE
 *        8  =  PRINT PHRBGO,PHRNDO,OPADRO,SCONPO,HFDOPO, AND
 *              ANY NON-ZERO HFPOAD VC'S, ALL AS-IS ARRAY BLOCKS.
 *        9  =  PRINT ONLY CLSNFO ARRAYS.
 *       10  =  PRINT ONLY CLSNFO AND CLSCON ARRAYS.
 *       11  =  PRINT SWORKO AND NWSTRG FOR HEAD OF EACH PHRASE
 *       12  =  PRINT OPADRO,SCONPO,HFDOPO,HFPOAD ASSOCIATED WITH EACH
 *              SWORKO.  ACCOMODATES NEW NON-SEQUENTIAL LINK OF SOURCE
 *              AND TARGET ARRAYS.
 *       13  =  PRINT: PHCT,SWORK,PHRHED,PHRBEG,
 *                          OPI,OPADRI,SCONPI,HFDOPI
 *       16  =  PRINT THE CURRENT CELL ARRRAY
 *       17  =  PRINT ONLY PARENT CELL ARRAYS, BUT FOR ALL CLAUSES.
 *	18  =  PARSE data input to TRAN1 :SWORKI, SCONIN,... */


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <fcrt.h>
#include "project.h"
#include "logoslib.h"
#include "parsetrans.h"
#include "trans.h"
#include "projexts.h"
#include <jbctrl.h>
#include <res_stat_file.h>
#include <logos_libs/lgs_tran_rule_io/tran_rule_io.h>



void /*FUNCTION*/ diagno(dian)
long int dian;
{
	static short int e, g, hf, hfct, hfx, hfy, i2, il, jj, jm1, k, 
	  ke, lines, nz, postmp[3], scnptr, scnsiz, sconpx[OPADRX], tmpct, ty, tyx, y;
	static long int _l0, tags;
	static short ns = 0;
	static short op = 0;
	static short i = 0;
	static short j = 0;
	static short x = 0;
	static short ii = 0;
	static short iz = 0;
	static short rs = 0;
	static short xx = 0;
	static long err = 0;
	static short iii = 0;
	static short iz2 = 0;
	static short iz3 = 0;
	static short lt2 = 0;
	static short lvl = 0;
	static short phctmp = 0;
	static short nswsav[4]={0,0,0,0};

   //**********Rule statistics*******************/
   if( diagsX_.stats == 1 && dian == 2 )
   {
      tran3_stat( _spec_fp, 1, diacb3X_.k7, source_sentX_.sentid, source_sentX_.sentbuf, source_sentX_.sentlng );
   }


	if( dian == 1 ){
		}

	
	
	else if( dian == 2 && diagsX_.longdi == 1 ){
			fprintf( _spec_fp, "\n***** A MATCH STARTING AT %3d LEVEL %3d    ON ELEMENT%3djj            %6.6s", 
					vwarg2X_.i,
					spX_.sp[0],
					sworkX_.swork[vwarg2X_.i-One][3],
					passesX_.pssnam);
			print_tran_rule(_spec_fp, 1, diacb3X_.k7);
			fprintf( _spec_fp, "\n" );

			}

				
	else if( dian == 5 || dian == 4 ){
			diag_write_swork();
			}

	
	else if( dian == 6 ){
				/*------  DIAN = 6,  PRINT VTRF TO 999 */
				if( !(vwarg2X_.wc50m == 0 && vwarg2X_.wc42m == 0) ){
					print_tran_rule(_spec_fp, 1, diacb3X_.k7);
					}
				}
			
	
	else if( dian == 7 ){
		// print output loaded elements
		diag_ouput_loaded_elements();
		}
			
	
	else if( dian == 8 ){
		}
			
		
	else if( dian == 9 || dian == 10 ){
		/*                            WRITE CLSNFO ARRAYS */
		diag_clause_array(1);
		}
		
	
	else if( dian == 11 ){
		diag_write_sworko();
		}
			
	
	
	else if( dian == 12 ){
				/*------   DIAN = 12 ,  PRINT OPADRO,SCONPO,HFODPO,HFPOAD ASSOCIATED
				 *                      WITH EACH PHRASE */
				diag_write_parsetran_outarrays();
				}
			
	
	else if( dian == 13 ){
				/*------  DIAN = 13,   PRINT: SWORK,PHRBEG,PHREND,OPADRI,SCONPI,HFDOPI */
			diag_clause_state();
			}
			
	
	else if( dian == 14 || dian == 15 ){
			/*                            WRITE CLSNFO ARRAYS */
			diag_clause_array(1);
			}
	
	else if( dian == 16 ){
					/*-------      DIAN = 16, PRINT VBCELL ARRAY */
			diag_vbcell_array();
			}

	else if( dian == 17 ){
					/*-------      DIAN = 17, PRINT PARENT CELLS FOR THE CURRENT CLAUSE */
			diag_clause_parent();
			}
				
	
	else if( dian == 18 ){
			diag_write_sworki();
			}
	
	return;
} /*end of function*/
