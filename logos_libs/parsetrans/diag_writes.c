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
Functions to write out diagnostics during translation

*/


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <fcrt.h>
#include "logoslib.h"
#include "trans.h"
#include "parsetrans_ext.h"
#include "parsetrans.h"
#include <string.h>
#include <jbctrl.h>


// write out PRINT OUTPUT ARRAYS FOR LOADED ELEMENTS
void diag_ouput_loaded_elements()
{
	short i,iz,phctmp;
	static short ns = 0;
	static short op = 0;
	static short nswsav[4]={0,0,0,0};

				/*------  DIAN = 7,   PRINT OUTPUT ARRAYS FOR LOADED ELEMENTS */

	if( op <= opadroX_.opo ){
		fprintf( _spec_fp, "           %6.6s", passesX_.pssnam);
		fprintf( _spec_fp, " OPADRO " );
		for( i=op; i <= opadroX_.opo; i++ ){
			fprintf( _spec_fp, "%6d", opadroX_.opadro[i-One] );
			}
		fprintf( _spec_fp, "\n" );
		op = opadroX_.opo + 1;
	}
	if(tranidX_.tranid != 4)
	{
		phctmp = sworkoX_.phcto;
		if( sworkoX_.phrhdo[sworkoX_.phcto-One] == 0 )
			phctmp = sworkoX_.phcto - 1;
		if( ns > phctmp ){

			for( i=1; i <= 4; i++ ){
				if( nswsav[i-One] != sworkoX_.sworko[phctmp-One][i-One] )
					goto L_1160;

				/*                THEY ARE THE SAME */
				}
			goto L_1180;

			/*                THEY ARE DIFFERENT */
	L_1160:
			fprintf( _spec_fp, "          SWORKO %3d", phctmp );
			fprintf( _spec_fp, "    " );
			for( i=1; i <= 4; i++ ){
				fprintf( _spec_fp, "%3d", sworkoX_.sworko[phctmp-One][i-One] );
				}
			fprintf( _spec_fp, "\n" );
			/*                       SAVE NEW ONE */
			}
		else{

			fprintf( _spec_fp, "          SWORKO %3d", phctmp );
			fprintf( _spec_fp, "    " );
			for( iz=ns; iz <= phctmp; iz++ ){
				for( i=1; i <= 4; i++ ){
					fprintf( _spec_fp, "%3d", sworkoX_.sworko[iz-One][i-One] );
					}
				}
			fprintf( _spec_fp, "\n" );
			ns = phctmp + 1;
			}

		/*                         PHCTO MAY NOT CHANGE BUT PHRBEG ID CAN */
		for( i=1; i <= 4; i++ ){
			nswsav[i-One] = sworkoX_.sworko[phctmp-One][i-One];

			}
	}

L_1180:
	if( diagsX_.deepdi == 1 )
		scnprt();
}



// write out clause state
void diag_clause_state()
{
	short i;
	/*------  DIAN = 13,   PRINT: SWORK,PHRBEG,PHREND,OPADRI,SCONPI,HFDOPI */

	diag_write_swork();

	fprintf( _spec_fp, " PHRBEG: STARTING OPADRI POSITION OF EACH PHRASE \n " );
	for( i=1; i <= sworkX_.phct; i++ ){
		fprintf( _spec_fp, "%6d", sworkX_.phrbeg[i-One] );
		}
	fprintf( _spec_fp, "\n" );

	fprintf( _spec_fp, " PHREND: ENDING   OPADRI POSITION OF EACH PHRASE \n " );
	for( i=1; i <= sworkX_.phct; i++ ){
		fprintf( _spec_fp, "%6d", sworkX_.phrend[i-One] );
		}
	fprintf( _spec_fp, "\n" );

	fprintf( _spec_fp, " OPADRI          THE CURRENT NUMBER OF OPADRI ELEMENTS = %3d", 
	  opadriX_.opi );
	fprintf( _spec_fp, "\n" );
	for( i=1; i <= opadriX_.opi; i++ ){
		fprintf( _spec_fp, "%6d", opadriX_.opadri[i-One] );
		}
	fprintf( _spec_fp, "\n" );

	fprintf( _spec_fp, " SCONPI\n" );
	for( i=1; i <= opadriX_.opi; i++ ){
		fprintf( _spec_fp, "%6d", opadriX_.sconpi[i-One] );
		}
	fprintf( _spec_fp, "\n" );

	fprintf( _spec_fp, " HFDOPI\n" );
	for( i=1; i <= opadriX_.opi; i++ ){
		fprintf( _spec_fp, "%6d", hpdopiX_.hfdopi[i-One] );
		}
	fprintf( _spec_fp, "\n" );
}



// write out clause parent array
void diag_vbcell_array()
{
	short k,ke,xx,lines;

	/*-------      DIAN = 16, PRINT VBCELL ARRAY */
	fprintf( _spec_fp, "\n     CURRENT CELLS (1-??)   (TRAILING ZEROES ARE NOT  PRINTED )\n" );
	/*                   DETERMINE HOW MANY NON-ZERO LINES TO PRINT */
	for( k=100; k >= 21; k-- ){
		if( vbdataX_.vbcell[k-One] != 0 )
			break;
		}
	/*                           PRINT NON-ZERO LINES FOR THIS CLAUSE */
	fprintf( _spec_fp, "         " );
	for( xx=1; xx <= 20; xx++ ){
		fprintf( _spec_fp, "%4d", vbdataX_.vbcell[xx-One] );
		}
	fprintf( _spec_fp, "\n" );
	lines = (k + 19)/20;
	for( k=2; k <= lines; k++ ){
		ke = k*20;
		fprintf( _spec_fp, "         " );
		for( xx=ke - 19; xx <= ke; xx++ ){
			fprintf( _spec_fp, "%4d", vbdataX_.vbcell[xx-One] );
			}
		fprintf( _spec_fp, "\n" );

		}
}



// write out clause parent array
void diag_clause_parent()
{
	short j,k,ke,xx,lines;
	/*-------      DIAN = 17, PRINT PARENT CELLS FOR ALL CLAUSES */

	fprintf( _spec_fp, "\n       CLAUSE  PARENT   \n         ID    CELLS    ( TRAILING ZEROES ARE NOT PRINTED )\n" );
	/*                   DETERMINE HOW MANY NON-ZERO LINES TO PRINT */
	for( j=2; j <= clsnfoX_.cltotl; j++ ){
		for( k=100; k >= 21; k-- ){
			if( clsnfoX_.clpcel[j-One][k-One] !=  0 )
				break;
			}
		/*                          PRINT NON-ZERO LINES FOR THIS CLAUSE */
		fprintf( _spec_fp, "         %3d", j );
		fprintf( _spec_fp, "  " );
		for( xx=1; xx <= 20; xx++ ){
			fprintf( _spec_fp, "%4d", clsnfoX_.clpcel[j-One][xx-One] );
			}
		fprintf( _spec_fp, "\n" );
		lines = (k + 19)/20;
		for( k=2; k <= lines; k++ ){
			ke = k*20;
			fprintf( _spec_fp, "              " );
			for( xx=ke - 19; xx <= ke; xx++ ){
				fprintf( _spec_fp, "%4d", clsnfoX_.clpcel[j-One][xx-One] );
				}
			fprintf( _spec_fp, "\n" );
			}
		}
}



// write out sworki array
void diag_write_sworki()
{
	short iz,postmp[3],i;

	/*-------      DIAN = 18, PRINT the parse data input to TRAN */
	fprintf( _spec_fp, "\n       *****  INPUT FROM %6.6s *****\n                SWORKS\\t   SC2   SC11  SC13\n", 
	  passesX_.pssnam );
	for( iz=1; iz <= elemctX_.parsct; iz++ ){
		postmp[1-One] = -1;
		postmp[2-One] = -1;
		postmp[3-One] = -1;
		postmp[prctX_.js[sconX_.scolnk[sworkiX_.sworki[iz-One][4-One]-One]-One]-One] = 1;
		fprintf( _spec_fp, "%2d", iz );
		fprintf( _spec_fp, " " );
		for( i=1; i <= 3; i++ ){
			fprintf( _spec_fp, "%2d", postmp[i-One] );
			}
		fprintf( _spec_fp, " " );
		for( i=1; i <= 3; i++ ){
			fprintf( _spec_fp, "%2d", sworkiX_.sworki[iz-One][i-One] );
			}
		fprintf( _spec_fp, " " );
		for( i=1; i <= 3; i++ ){
			fprintf( _spec_fp, "%2d", sconinX_.sconin[iz-One][i-One] );
			}
		fprintf( _spec_fp, "   \n" );
		}

}

/*
    diag_level = 1 then print addtional information
*/
void /*FUNCTION*/ diag_clause_array(int diag_level)
{
	short j,xx,k,tmpct,lines,ke;
			/*                            WRITE CLSNFO ARRAYS */

	fprintf( _spec_fp, "CLSNFO ARRAYS - NUMBER OF CLAUSES IDENTIFIED   (INCLUDING MAIN CLAUSE) =%2d\n                NUMBER OF CLAUSES MOVED        (EXCLUDING MAIN CLAUSE) =%2ld\n", 
	  clsnfoX_.cltotl, clsnfoX_.cltotl - clsmovX_.clmcnt - 1 );
	fprintf( _spec_fp, "                NUMBER OF CLAUSES STILL TO BE MOVED                    = %2d\n              BEGIN  ENDING BEGIN  ENDING\n       CLAUSE INPUT  INPUT  OUTPUT OUTPUT PARENT CLMRKR ANTCDN ANTCDN ANTCDN ANTCDN RELPRO\n         ID   SWORK  SWORK  SWORK  SWORK  CLAUSE SCONS  SWORK  SCONPT OPIBEG OPIEND SCON  \n", 
	  clsmovX_.clmcnt );
	for( j=1; j <= clsnfoX_.cltotl; j++ ){
		fprintf( _spec_fp, "         %3d    %3d    %3d    %3d    %3d    %3d    %3d    %3d    %3d    %3d    %3d    %3d  \n", 
		  j, clsnfoX_.clbgns[j-One], clsnfoX_.clndns[j-One], 
		  clsoutX_.clbgno[j-One], clsoutX_.clndno[j-One], 
		  clsnfoX_.clprnt[j-One], clsnfoX_.clmrkr[j-One], 
		  clsnfoX_.clansw[j-One], clsnfoX_.clansc[j-One], 
		  clsnfoX_.clanbg[j-One], clsnfoX_.clannd[j-One], 
		  clsnfoX_.clrelp[j-One] );
		}

	fprintf( _spec_fp, "       CLAUSE  PARENT   \n         ID    CELLS    ( TRAILING ZEROES ARE NOT PRINTED )\n" );
	for( j=2; j <= clsnfoX_.cltotl; j++ ){
		/*                   DETERMINE HOW MANY NON-ZERO LINES TO PRINT */
		for( k=100; k >= 21; k-- ){
			if( clsnfoX_.clpcel[j-One][k-One] != 0 )
				break;
			}
		/*                           PRINT NON-ZERO LINES FOR THIS CLAUSE */
		fprintf( _spec_fp, "         %3d", j );
		fprintf( _spec_fp, "  " );
		for( xx=1; xx <= 20; xx++ ){
			fprintf( _spec_fp, "%4d", clsnfoX_.clpcel[j-One][xx-One] );
			}
		fprintf( _spec_fp, "\n" );
		lines = (k + 19)/20;
		for( k=2; k <= lines; k++ ){
			ke = k*20;
			fprintf( _spec_fp, "              " );
			for( xx=ke - 19; xx <= ke; xx++ ){
				fprintf( _spec_fp, "%4d", clsnfoX_.clpcel[j-One][xx-One] );
				}
			fprintf( _spec_fp, "\n" );
			}
		}

	fprintf( _spec_fp, "\nCURRENT CLAUSE ID = %2d\n", clsnfoX_.clcrnt );


	if( diag_level == 1 ){

		/*                            WRITE CLSCON ARRAYS */
		fprintf( _spec_fp, "CLSCON ARRAYS   (CLSID IS INITIALIZED TO 1. ENTRY NOT PRINTED IF CLSID=1 AND BOTH CMCHLD AND ACHILD = 0\n" );
		tmpct = 0;
		for( j=1; j <= SCONY; j++ ){
			if( ((sconX_.scolnk[j-One] != 0) && (sconX_.scolnk[j-One] < ELMMAX))
				&& ((clsconX_.clsid[sconX_.scolnk[j-One]-One] > 1) ||
				   ((clsconX_.clsid[sconX_.scolnk[j-One]-One] == 1) && 
				   ((clsconX_.cmchld[sconX_.scolnk[j-One]-One] != 0) || 
				   (clsconX_.achild[sconX_.scolnk[j-One]-One] != 0)))) ){
				if( tmpct == 0 ){
					tmpct = 1;
					fprintf( _spec_fp, "    SCON#  CLSID CMCHLD ACHILD\n" );
					}
				fprintf( _spec_fp, "  %3d    %3d    %3d    %3d\n", 
					 j, 
					 clsconX_.clsid[sconX_.scolnk[j-One]-One],
					 clsconX_.cmchld[sconX_.scolnk[j-One]-One],
					 clsconX_.achild[sconX_.scolnk[j-One]-One] );
				}

			}
		}
  return;
  }
