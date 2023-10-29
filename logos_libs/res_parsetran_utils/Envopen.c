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
/*envopen.c - revised version of fortopen.c for C*/
/*by c w lightfoot, cobalt blue, inc.,  2/12/98*/
#include <stdio.h>
#include <stdlib.h>
#define MS_F77
#include <logos_include_res_pt/fcrt.h>
#include <logos_include_res_pt/logoslib.h>

FILE *env_open(env_name) /*open file for writing, given envir. var*/
char *env_name;  /*envirn. var. whose definition is the filename to open*/
{
	char *evardef;
	FILE *fp;

	evardef = getenv(env_name);
	if( evardef==NULL || *evardef=='\0' ){
		printf( "ERROR: missing definition for environmental variable: %s\n", 
		            env_name );
		exit( 1 );
		}

#ifdef WANTED
	/*NOTE: normally DOS/PC C compilers allow / for dir. separators*/
	for( p=evardef; *p; p++ ){
		if( *p == '/' )
			*p = '\\'; /*map / to \ */
		}
#endif

	if( (fp=fopen(evardef,"w"))==NULL ){
		printf( "ERROR: unable to create file: %s\n", evardef );
		exit( 1 );
		}

	return( fp );
} /*end of function*/

