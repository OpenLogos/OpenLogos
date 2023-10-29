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
//
//    program with functions to write to the diagnostic files of translation
//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef _MSC_VER
#include <fcrt.h>
#endif
#ifndef EXTERN
#define EXTERN extern
#endif

#include <logos_include_res_pt/parsetrans_ext.h>
#include <logos_include_res_pt/logoslib.h>
#include <logos_include_res_pt/jbctrl.h>


// write out a list of values which are short datatypes
void diag_write_shorts(char *header,
					   short values[], int num_of_values, int values_per_line,
					   int format_size,
					   char *footer)
{
	int line_ct=0;
	int ct2=0;
	int lines=0;
	int valpt=0;

	fprintf( _spec_fp,"%s", header);

	if (num_of_values > 0 )
	{
		lines = num_of_values / values_per_line + 1;
		if ((lines * values_per_line) == num_of_values) lines--;
		for(line_ct=0; line_ct<lines; line_ct++)
		{
			for(ct2=0; ct2 < values_per_line; ct2++,valpt++)
			{
				if (valpt >= num_of_values) break;
				fprintf(_spec_fp,"%*d",format_size, values[valpt]);
			}
		fprintf( _spec_fp,"\n");
		}
	}

	fprintf( _spec_fp,"%s\n", footer);

}
// write out a list of character arrays (not strings)
void diag_write_char_arrays(char *header,
					   char values[], 
					   int size_of_values,
					   int num_of_values, int values_per_line,
					   int format_size,
					   char *footer)
{
	int line_ct=0;
	int ct2=0;
	int lines=0;
	int valpt=0;
	int x;
	int leading_spaces=0;
	int format_size_new;

	fprintf( _spec_fp,"%s", header);

	if (num_of_values > 0 )
	{
		format_size_new = size_of_values;
		if(format_size_new >  format_size)
			format_size_new = format_size;
		leading_spaces = format_size - format_size_new;

		lines = num_of_values / values_per_line + 1;
		if ((lines * values_per_line) == num_of_values) lines--;
		for(line_ct=0; line_ct<lines; line_ct++)
		{
			for(ct2=0; ct2 < values_per_line; ct2++,valpt++)
			{
				if (valpt >= num_of_values) break;

				for(x=0;x<leading_spaces;x++){
					fprintf(_spec_fp," ");
				}
				for(x=0;x<format_size_new;x++){
					fprintf(_spec_fp,"%1.1s",&values[valpt*size_of_values+x]);
				}
			}
		fprintf( _spec_fp,"\n");
		}
	}

	fprintf( _spec_fp,"%s\n", footer);

}

// write out sworko array
void diag_write_swork()
{

	int iz,iz2,iz3;

	fprintf( _spec_fp,
		    "\n               ***** THE SWORK TABLE IN %6.6s *****\n", 
			passesX_.pssnam );

	for( iz2=0; iz2 < sworkX_.phct; iz2+=5 )
	{
		iz3 = iz2 + 5;
		if(iz3 > sworkX_.phct) iz3 = sworkX_.phct; 
		for( iz=iz2; iz < iz3; iz++ )
		{
			fprintf( _spec_fp, "%4d", sworkX_.swork[iz][0] );
			fprintf( _spec_fp, "%5d", sworkX_.swork[iz][1] );
			fprintf( _spec_fp, "%4d", sworkX_.swork[iz][2] );
			fprintf( _spec_fp, "%3d       ", sworkX_.swork[iz][3] );			
		}
		fprintf( _spec_fp, "\n" );
		for( iz=iz2; iz < iz3; iz++ )
		{
			fprintf( _spec_fp, "    %16.16s   ",
			        sent_wordsX_.source_word[sconX_.scolnk[sworkX_.swork[iz][3]-One]-One] );
		}
		fprintf( _spec_fp, "\n" );
	}

}




// write out sworko array
void diag_write_sworko()
{
	int iz,iz2,iz3;

	fprintf( _spec_fp,
		    "\n               ***** THE SWORKO TABLE IN %6.6s *****\n", 
			passesX_.pssnam );

	for( iz2=0; iz2 < sworkoX_.phcto; iz2+=5 )
	{
		iz3 = iz2 + 5;
		if(iz3 > sworkoX_.phcto) iz3 = sworkoX_.phcto; 
		for( iz=iz2; iz < iz3; iz++ )
		{
			fprintf( _spec_fp, "%4d", sworkoX_.sworko[iz][0] );
			fprintf( _spec_fp, "%5d", sworkoX_.sworko[iz][1] );
			fprintf( _spec_fp, "%4d", sworkoX_.sworko[iz][2] );
			fprintf( _spec_fp, "%3d       ", sworkoX_.sworko[iz][3] );			
		}
		fprintf( _spec_fp, "\n" );
		for( iz=iz2; iz < iz3; iz++ )
		{
			fprintf( _spec_fp, "    %16.16s   ",
			        sent_wordsX_.source_word[sconX_.scolnk[sworkoX_.sworko[iz][3]-One]-One] );
		}
		fprintf( _spec_fp, "\n" );
	}

}




/*-----  DIAN = 11 ,  PRINT OPADRO,SCONPO,HFDOPO,HFPOAD ASSOCIATED WITH
 *                    ITS CORRESPONDING SWORKO */

void diag_write_parsetran_outarrays()
{

	int i,x,xx,hfct,hf,hfx,hfy,scon_pos,swork_pos;
	char temp_cc[3];


fprintf( _spec_fp,"\n             ***** OUTPUT TARGET ARRAYS IN %6.6s *****\n\n",passesX_.pssnam );
			
// LOOP THRU EACH OUTPUT SWORK ELEMENT 
for( i=1; i <= sworkoX_.phcto; i++ )
{
	fprintf( _spec_fp, "\n (%2d", i );
	fprintf( _spec_fp, ") SWORKO =" );
	for( x=1; x <= 4; x++ ){
		fprintf( _spec_fp, "%6d", sworkoX_.sworko[i-One][x-One] );
		}
	fprintf( _spec_fp, "   %16.16s",
			sent_wordsX_.source_word[sconX_.scolnk[sworkoX_.sworko[i-One][4-One]-One]-One]);
	fprintf( _spec_fp, "%6d%6d ", sworkoX_.phrbgo[i-One], sworkoX_.phrndo[i-One] );
	fprintf( _spec_fp, "\n" );
	

	fprintf( _spec_fp, "      OPADRO  " );
	for( x=sworkoX_.phrbgo[i-One]; x <= sworkoX_.phrndo[i-One]; x++ ){
		fprintf( _spec_fp, "%6d", opadroX_.opadro[x-One] );
		}
	fprintf( _spec_fp, "\n" );

	if (diagsX_.deepdi != 0){
		fprintf( _spec_fp, "      OPADROcc" );
		for( x=sworkoX_.phrbgo[i-One]; x <= sworkoX_.phrndo[i-One]; x++ )
		{
			scon_pos = opadroX_.sconpo[x-One];
			swork_pos = sconX_.scon[scon_pos-One][10-One];
			memset(temp_cc,' ',sizeof(temp_cc));
			if (opadroX_.opadro[x-One] > 0){
				if(swork_pos > 0){
					memcpy(temp_cc, cmpsmcX_.cmpcod[swork_pos-One][prctX_.js[swork_pos-One]-One],sizeof(temp_cc) );
				}
			}
			fprintf( _spec_fp, "   %3.3s", temp_cc );
		}
		fprintf( _spec_fp, "\n" );
	}

	fprintf( _spec_fp, "      SCONPO  " );
	for( x=sworkoX_.phrbgo[i-One]; x <= sworkoX_.phrndo[i-One]; x++ ){
		fprintf( _spec_fp, "%6d", opadroX_.sconpo[x-One] );
		}
	fprintf( _spec_fp, "\n" );

	fprintf( _spec_fp, "      HFDOPO  " );
	for( x=sworkoX_.phrbgo[i-One]; x <= sworkoX_.phrndo[i-One]; x++ ){
		fprintf( _spec_fp, "%6d", hpdopoX_.hfdopo[x-One] );
		}
	fprintf( _spec_fp, "\n" );


	/*                             WRITE HFDOPO OVERFLOW VC'S IF ANY EXIST */
	for( xx=sworkoX_.phrbgo[i-One]; xx <= sworkoX_.phrndo[i-One]; xx++ ){
		hf = hpdopoX_.hfdopo[xx-One];
		if( !(hf < HFPOLO || hf > HFPOHI) ){
			hfy = hf - HFPOL1;
			hfct = hfdoaX_.hfpoad[hfy-One][HFPADX-One];
			if( hfct != 0 ){
				/*                   FOUND NON-NULL VC OVERFLOW, PRINT IT */
				fprintf( _spec_fp, "      HFPOAD: %2d", 
				  hf );
				fprintf( _spec_fp, "   OPADRHF" );
				for( hfx=1; hfx <= hfct; hfx++ ){
					fprintf( _spec_fp, "%6d", hfdoaX_.hfpoad[hfy-One][hfx-One] );
					}
				fprintf( _spec_fp, "\n" );
				fprintf( _spec_fp, "                   SCONHF" );
				for( hfx=1; hfx <= hfct; hfx++ ){
					fprintf( _spec_fp, "%6d", hfdoaX_.sconhf[hfy-One][hfx-One] );
					}
				fprintf( _spec_fp, "\n" );
				}
			}
		}
}

}





// write out the vtr portion of a sp rule
//	(left here for semtab rules)
void diag_write_rule_vtr(short vtrs[])
{
	int x;

	char *indents={"        "};

	fprintf( _spec_fp,indents);
	x = -1;
	do					
	{
		x++;
		if(x != 0)
		{
			if (vtrs[x] == -63 || vtrs[x] == -56 ||
				vtrs[x] == -22 ||
				vtrs[x] == -66 || vtrs[x] == -57)
			{
				fprintf( _spec_fp, "\n" );
				fprintf( _spec_fp,indents);
			}
		}
		fprintf( _spec_fp, "%3d ", vtrs[x]); 
	}while(vtrs[x] != 999);

	fprintf( _spec_fp,"\n");
}
