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
  Log errors for res and 4 tran programs. 
  write errors to console and diagnotic file.
*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <logos_include_res_pt/logoslib.h>
#include <logos_include_res_pt/jbctrl.h>
#include <lgs_db_io/jobcntrlarginterface.h>


void errlog(
char *pgmnam,
long linnum, long logval, long lvlval)
{
	static char filename[301];
	static long int len, retflg, x, jobid;
	char *header=" ****************** ERROR DETECTED ***************************\n";
	char *footer=" *************************************************************\n";

	fprintf( _spec_fp, header );
	printf( header );

    memset(filename,' ',sizeof(filename));
    inputFile(filename);
    jobid = jobID();

	if( source_sentX_.sentlng > 0 ){
		fprintf( _spec_fp, " jobid=%10ld file=%300.300s\n sentence ID=%7ld character count=%7ld", 
		  jobid, filename, source_sentX_.sentid, source_sentX_.sentlng );
		fprintf( _spec_fp, "\n " );
		for( x=1; x <= 300; x++ ){
			fprintf( _spec_fp, "%c", source_sentX_.sentbuf[x-1] );
			}
		fprintf( _spec_fp, "\n" );
		printf( " jobid=%10ld file=%300.300s\n sentence ID=%7ld character count=%7ld", 
		  jobid, filename, source_sentX_.sentid, source_sentX_.sentlng );
		printf( "\n " );
		for( x=1; x <= 300; x++ ){
			printf( "%c", source_sentX_.sentbuf[x-1] );
			}
		printf( "\n" );
		}

	fprintf( _spec_fp, " ERROR, MOD NAME, PGM NAME,   LINE,   TYPE, LEVEL,\n %5ld  %8.8s  %8.8s  %6ld  %6ld %6ld    *\n\n", 
	  errvrsX_.err, errvrsX_.mod, pgmnam, linnum, logval, lvlval );
	printf( " ERROR, MOD NAME, PGM NAME,   LINE,   TYPE, LEVEL,\n %5ld  %8.8s  %8.8s  %6ld  %6ld %6ld    \n\n", 
	  errvrsX_.err, errvrsX_.mod, pgmnam, linnum, logval, lvlval );


	fprintf( _spec_fp, footer );
	printf( footer );


	return;
} /*end of function*/

