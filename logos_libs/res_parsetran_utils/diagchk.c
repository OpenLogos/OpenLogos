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
/*Translated by FOR_C, v3.6 (W), on 02/10/1998 at 12:23:10 */
/*FOR_C Options SET: com=u ct=p do=r for=ms fx=9bfiklmnstuxz"$ fxm=$a fxs=a */
/*                   fxv=ar io=p op=ai out=c pf s=djsvz str=l w - prototypes */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <logos_include_res_pt/jbctrl.h>
#include <logos_include_res_pt/logoslib.h>

/*		routine to check if the diagnostics should be turned on
 *		based on the input line number
 * */




void diag_check(proc_level)
long int proc_level;
{
	static short int x;
	static long first_time = 0;

	/*      include "c_calls.h" */
	if( opswX_.diag_line_start > 0 ){
		/*							not at lines with diag */
		if( source_sentX_.sentid < opswX_.diag_line_start ){
			/*							past the lines for diags so turn off */
			}
		else if( source_sentX_.sentid > opswX_.diag_line_end ){
			zapit(opswX_.sw,60,(byte)0);
			diagsX_.longdi = 0;
			diagsX_.deepdi = 0;
			diagsX_.shrtdi = 0;
			diagsX_.anydi = 0;
			opswX_.diag_line_start = 0;
			}
		else{
			/*							in the area of diags, turn them on */
			lmove(opswX_.sw,1,opswX_.savsw,1,60);
			}
		}


	if( opswX_.sw[2] != 0 || diagsX_.shrtdi != 0 ){
		if( first_time == 0 ){
			first_time = 1;
			if( proc_level == 1 ){
				fprintf( _spec_fp, "*RES   START*\n" );
				}
			else{
				fprintf( _spec_fp, "*%s  START*\n", passesX_.pssnam
				   );
				}

			}

		fprintf( _spec_fp, "%5ld %3ld", source_sentX_.sentid, source_sentX_.sentlng );
		fprintf( _spec_fp, " " );
		for( x=1; x <= 300; x++ ){
			fprintf( _spec_fp, "%c", source_sentX_.sentbuf[x-1] );
			}
		fprintf( _spec_fp, "\n" );

		}

	return;
} /*end of function*/

