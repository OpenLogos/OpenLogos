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
/*          COMMON DIAGNOSTIC ROUTINE FOR RES MODULE */

#include "rescommon.h"
#include <logos_libs/lgs_tran_rule_io/res_rule_io.h>
#include <logos_include_res_pt/res_stat_file.h>

//#include <configdatafileinterface/configdatainterfacemain.h>


/* write the diag printing the claus status array
  */

void res_diag_clause_status()
{
	int i,j,k,ws;
	char sw_active[4];


	if( diagsX_.longdi == 0 && diagsX_.shrtdi == 0){
		return;
	}


		fprintf( _spec_fp, "\n*RESOLVED SWORK RECORDS*\n");
		fprintf( _spec_fp, "                                                          " );
		fprintf( _spec_fp, " **** M A J O R  P A T H ***** -  *** M I N O R   P A T H *****\n" );
		fprintf( _spec_fp, "                                                    " );
		fprintf( _spec_fp, "  CL  UV  VB  SJ  VB  VB  CL   N  PR  DC -  CL  UV  VB  SJ  VB  VB  CL   N  PR  SP\n" );
		fprintf( _spec_fp, "         wc typ fr sbs sps patstm schg com  o2b o3b| " );
		fprintf( _spec_fp, "  A       T   F   F   X   B   X       S -   A       T   F   F   X   B   X\n" );
		ws = 1;
		for( i=1; i <= swX_.ecount; i++ ){
 
			strcpy(sw_active,"___");
			for( k=0; k < 3; k++ ){
				if( swX_.swork[i-One][k] == 1 ){
				memset(sw_active+k,'1',1);
				}
			}

			fprintf( _spec_fp, "%3d %3.3s ", i, sw_active);

			for( k=1; k <= 3; k++ ){
				if( swX_.swork[i-One][k-One] == 1 ){
					fprintf( _spec_fp, "%3d%4d%3d%4d%4d%4d%3d%5d %3.3s %4d%4d|", 
						  swX_.swork[i-One][(k*4)-One],
						  swX_.swork[i-One][(k*4)+1-One],
						  swX_.swork[i-One][(k*4)+2-One],
						  ofltagX_.ofl1r[i-One][k-One],
						  ofltagX_.ofl4r[i-One][k-One], 
						  ptdiagX_.patno[i-One][k-One], 
						  ptdiagX_.stemno[i-One][k-One], 
						  savtyX_.subchg[i-One][k-One], 
						  cmpsmcX_.cmpcod[i-One][k-One], 
						  /*cmpsmcX_.gencod[i-One][k-One],
						  cmpsmcX_.smccod[i-One][k-One],*/
						  ofl2a3X_.ofl2r[i-One][k-One], 
						  ofl2a3X_.ofl3r[i-One][k-One]
						  );
					for( j=0; j < 10; j++ ){
						fprintf( _spec_fp, "%4d", cellX_.csaray[i-One][j] );
					}
					fprintf( _spec_fp, " -" );
					for( j=10; j < 20; j++ ){
						fprintf( _spec_fp, "%4d", cellX_.csaray[i-One][j] );
					}

						break;
					}
				}

			fprintf( _spec_fp, " %2d %20.20s\n",
				      sent_wordsX_.source_word_count[i-One],
			          sent_wordsX_.source_word[i-One]);

			}
    fprintf( _spec_fp, "\n" );


}





/* write the diag printing the hash code and hennum values
  */

void res_diag_hash_hennum()
{
	int i;

	if( diagsX_.longdi == 0 ){
		return;
	}

	fprintf( _spec_fp, "   WSTRNG             HENUM        root      HASHCOD      root\n");
	for( i=1; i <= swX_.ecount; i++ ) 
	{
		fprintf( _spec_fp, "    %16.16s  %4d %4d %4d %4d    %4d %4d %4d %4d\n",
							sent_wordsX_.source_word[i-One],
							hensavX_.hennum[i-One][0],
							hensavX_.hennum[i-One][1],
							hensavX_.root_hennum[i-One][0],
							hensavX_.root_hennum[i-One][1],
							hashX_.hashcd[i-One][0],
							hashX_.hashcd[i-One][1],
							hashX_.root_hashcd[i-One][0],
							hashX_.root_hashcd[i-One][1]
							);
	}
    fprintf( _spec_fp, "\n" );

}



/* write the diag printing the sworks
  */

void res_diag_swork()
{
	int i,k,ws;
	char sw_active[4];

	if( diagsX_.longdi == 0 && diagsX_.shrtdi == 0 ){
		return;
	}

		fprintf( _spec_fp, "\n*SWORK RECORDS*\n         xx  wc typ fr sbs sps pat stm schg com o2b o3b meaningID| wc typ fr sbs sps pat stm schg com o2b o3b meaningID| wc typ fr sbs sps pat stm schg com o2b o3b meaningID|\n" );	// TO BE REMOVED WHEN NEW SMC IS TESTED
		//fprintf( _spec_fp, "\n*SWORK RECORDS*\n         xx  wc typ fr sbs sps pat stm schg com smc o2b o3b meaningID| wc typ fr sbs sps pat stm schg com smc o2b o3b meaningID| wc typ fr sbs sps pat stm schg com smc o2b o3b meaningID|\n" );		// this is the line to keep
		ws = 1;
		for( i=1; i <= swX_.ecount; i++ ){
			/*				determine which ssu of each swork are active */
			strcpy(sw_active,"___");
			for( k=0; k < 3; k++ ){
				if( swX_.swork[i-One][k] == 1 ){
				memset(sw_active+k,'1',1);
				}
			}

			fprintf( _spec_fp, "%3d %3.3s%4d ", i, sw_active,swX_.swork[i-One][6]);
			for( k=1; k <= 3; k++ ){
				fprintf( _spec_fp, 
					  "%3d%4d%3d%4d%4d%4d%4d%5d %3.3s %4d%4d%10d|",
					  swX_.swork[i-One][(k*4)-One],			// xx
		 			  swX_.swork[i-One][(k*4)+1-One],		// wc
					  swX_.swork[i-One][(k*4)+2-One],		// typ
					  ofltagX_.ofl1r[i-One][k-One],			// fr
					  ofltagX_.ofl4r[i-One][k-One],			// sbs
					  ptdiagX_.patno[i-One][k-One],			// pat
					  ptdiagX_.stemno[i-One][k-One],		// stm
					  savtyX_.subchg[i-One][k-One],			// schg
					  cmpsmcX_.cmpcod[i-One][k-One],		// com
					  // add SMC from swork here
					  /*cmpsmcX_.gencod[i-One][k-One],		// TO BE REMOVED WHEN NEW SMC IS TESTED
					  cmpsmcX_.smccod[i-One][k-One],		// TO BE REMOVED WHEN NEW SMC IS TESTED*/
					  ofl2a3X_.ofl2r[i-One][k-One],			// o2b
					  ofl2a3X_.ofl3r[i-One][k-One],			// o3b
					  targX_.targ[i-One][k-One]);			// meaningID
				}
			fprintf( _spec_fp, " %2d %16.16s\n",
				      sent_wordsX_.source_word_count[i-One],
			          sent_wordsX_.source_word[i-One]);

			}
    fprintf( _spec_fp, "\n" );

}


/************************************************************************************/

void rsdiag(long locflg, short *resnum, short *swork_num, short *rule_rec)
{
   //**********Rule statistics*******************/
   if( diagsX_.stats == 1 )
   {
      res_stat( _spec_fp, *resnum, *rule_rec, source_sentX_.sentid, source_sentX_.sentbuf, source_sentX_.sentlng );
   }

	if (diagsX_.longdi == 0)
   {
		return;
	}

	if( locflg == 1 )
	{
		fprintf( _spec_fp, "\nMATCH AT %2d", *swork_num);
	}
	else if( locflg == 2 )
   {
		fprintf( _spec_fp, " 6300 RULE AT %2d", *swork_num);
	}
	else if( locflg == 3 )
	{
		fprintf( _spec_fp, "  ** OVERRIDE ** %2d", *swork_num);
	}
	else if( locflg == 4 )
	{
		fprintf( _spec_fp, "  *** RES22 MATCH" );
	}
	else if( locflg == 5 )
	{
		fprintf( _spec_fp, "   MATCH ON SKIP RULE AT ELEMENT %4d\n", *swork_num );
	}
	else if( locflg == 6 )
	{
		fprintf( _spec_fp, "   MATCH ON QUIT RULE AT ELEMENT %4d\n", *swork_num );
	}
	else if( locflg == 7 )
	{
		fprintf( _spec_fp, "   MATCH ON WC07 RULE AT ELEMENT %4d\n", *swork_num );
	}

	else
   {
		return;
	}

	print_res_rule(_spec_fp, *resnum, *rule_rec);
	return;

} /*end of function*/

