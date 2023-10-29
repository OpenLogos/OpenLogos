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
#include <logos_include_res_pt/logoslib.h>
#include <logos_include_res_pt/jbctrl.h>

/*		Routine to exit res and trans with return code set
 * */
int labort(proc_level)
long int proc_level;
{
	static short int x;

	/*      include "c_calls.h" */
	/* converted to if-then-else for transl to C, cwl, 2/5/98
	 *		write_head:	select case (proc_level)
	 *			case(1)
	 *			  write(6,*)'*RES   ERROR*'
	 *			case(2)
	 *			  write(6,*)'*TRAN1 ERROR*'
	 *			case(3)
	 *			  write(6,*)'*TRAN2 ERROR*'
	 *			case(4)
	 *			  write(6,*)'*TRAN3 ERROR*'
	 *			case(5)
	 *			  write(6,*)'*TRAN4 ERROR*'
	 *			case default
	 *			end select write_head */
	/*		write_head:	select case (proc_level) */
	if( proc_level == 1 ){
		fprintf( _spec_fp, "*RES   ERROR*\n" );
		}
	else if( proc_level == 2 ){
		fprintf( _spec_fp, "*TRAN1 ERROR*\n" );
		}
	else if( proc_level == 3 ){
		fprintf( _spec_fp, "*TRAN2 ERROR*\n" );
		}
	else if( proc_level == 4 ){
		fprintf( _spec_fp, "*TRAN3 ERROR*\n" );
		}
	else if( proc_level == 5 ){
		fprintf( _spec_fp, "*TRAN4 ERROR*\n" );
		}
	else{
		/*			default */
		}

	if( (opswX_.sw[2] != 0 || opswX_.sw[13] != 0) || opswX_.sw[15] != 
	  0 ){
		fprintf( _spec_fp, "ERROR: Program will terminate.\n" );
		fprintf( _spec_fp, "%5ld %3ld", source_sentX_.sentid, source_sentX_.sentlng );
		fprintf( _spec_fp, " " );
		for( x=1; x <= 300; x++ ){
			fprintf( _spec_fp, "%c", source_sentX_.sentbuf[x-1] );
			}
		fprintf( _spec_fp, "\n" );
		}

	return lexit(1); /* lexit(1);  lexit does not take arguments */

} /*end of function*/

