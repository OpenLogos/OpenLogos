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
	/*  DIAGNO OPTIONS FOR DIAN:
	 *    1  =  INITIALIZE PRINT PARAMETERS?
	 *    2  =  DIAGNOSTICS FOR MAIN RULE
	 *    3  =     "        "   MINI  "
	 *    4  =
	 *    5  =
	 *    6  =  PRINT VTRF LOADED (FROM CURRENT POS TO 999)
	 *    7  =  FOR A GIVEN RULE PRINT:
	 *            OPADR,
	 *            IF SW18 THEN WRITE SCONS FOR ALL LOADED ELEMENTS
	 *            WRITE SWORKO
	 *            IF SW10 THEN WRITE SCON
	 *    8  =  PRINT PHRBGO,PHRNDO,OPADRO,SCONPO,HFDOPO AND HFPOAD IF
	 *                ANY OVERFLOW VC'S EXIST. ALL AS-IS INTERNALLY.
	 *    9  =  PRINT SWORKO W/  NWSTRG (I.E. CHARS 1-8 OF HEAD WORD)
	 *   10  =  PRINT SWORKO
	 *   11  =  PRINT OPADRO,SCONPO,HFDOPO,HFPOAD ASSOCIATED WITH EACH
	 *          SWORKO.  ACCOMODATES NEW NON-SEQUENTIAL LINK OF SOURCE
	 *          AND TARGET ARRAYS.
	 *   12  =  PRINT INPUT SWORK: SWORK1 RECORDS
	 *   13  =  PRINT SWRK1I */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <logos_include_res_pt/fcrt.h>
#include "project.h"
#include "logoslib.h"
#include "trans.h"
#include "projexts.h"
#include "parsetrans.h"
#include <string.h>
#include <logos_include_res_pt/jbctrl.h>
#include <logos_include_res_pt/res_stat_file.h>
#include <logos_libs/lgs_tran_rule_io/tran_rule_io.h>


void tran_diag_swork_in()
{
	int i,k,ws;
	char sw_active[4];

	if( diagsX_.longdi == 0 ){
		return;
	}

		fprintf( _spec_fp, "\n*SWORK RECORDS*\n         xx  wc typ fr sbs sps patstm com o2b o3b| wc typ fr sbs sps patstm com o2b o3b| wc typ fr sbs sps patstm com o2b o3b|\n" );
		ws = 1;
		for( i=1; i <= swork1X_.phct; i++ ){
			/*				determine which ssu of each swork are active */
			strcpy(sw_active,"___");
			for( k=0; k < 3; k++ ){
				if( swork1X_.swork1[i-One][k] > 0 ){
				memset(sw_active+k,'1',1);
				}
			}

			fprintf( _spec_fp, "%3d %3.3s%4d ", i, sw_active,swork1X_.swork1[i-One][6]);
			for( k=1; k <= 3; k++ ){
				fprintf( _spec_fp, "%3d%4d%3d%4d%4d%4d%3d %3.3s%4d%4d|", 
					  swork1X_.swork1[i-One][(k*4)-One],
					  swork1X_.swork1[i-One][(k*4)+1-One],
					  swork1X_.swork1[i-One][(k*4)+2-One],
					  ofltagX_.ofl1r[i-One][k-One],
					  ofltagX_.ofl4r[i-One][k-One], 
					  ptdiagX_.patno[i-One][k-One], 
					  ptdiagX_.stemno[i-One][k-One], 
					  cmpsmcX_.cmpcod[i-One][k-One], 
					  ofl2a3X_.ofl2r[i-One][k-One], 
					  ofl2a3X_.ofl3r[i-One][k-One]
					  );
				}
			fprintf( _spec_fp, " %2d %16.16s\n",
				      sent_wordsX_.source_word_count[i-One],
			          sent_wordsX_.source_word[i-One]);

			}
    fprintf( _spec_fp, "\n" );

}




////////////////////////////////////////////////////////////////////////
void /*FUNCTION*/ diagno(dian)
long int dian;
{
	static short int e, g, hf, hfct, hfx, hfy, i2, il, jj, jm1, k, 
	  nz, scnptr, scnsiz, sconpx[OPADRX], ty, xx1, xx2,  y;
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
	static short tyx = 0;
	static long err = 0;
	static short iii = 0;
	static short iz2 = 0;
	static short iz3 = 0;
	static short lt2 = 0;
	static short level = 0;
	static short sw10 = 0;
	static short phctmp = 0;
	static short nswsav[4]={0,0,0,0};




   //**********Rule statistics*******************/
   if( diagsX_.stats == 1 && dian == 2 )
   {
      tran1_stat( _spec_fp, 1, diacbX_.k7, source_sentX_.sentid, source_sentX_.sentbuf, source_sentX_.sentlng );
   }
	

	if( diagsX_.anydi == 0 ){
		return;
	}


	if( dian == 2 && diagsX_.longdi == 1 ){
		fprintf( _spec_fp, "\n***** A MATCH STARTING AT %3d LEVEL %3d       ON ELEMENT%3djj              %6.6s", 
				 vwarg2X_.i,
				 spX_.sp[0],
				 vwarg2X_.i,
				 passesX_.pssnam);
		print_tran_rule(_spec_fp, 1, diacbX_.k7);
		}

	else if( dian == 6 ){
			// used to write out VRTF LOADED:
		}


	else if( dian == 7 ){
		// print output loaded elements
		diag_ouput_loaded_elements();
		}


	else if( dian == 8 ){
			}

	else if( dian == 9 ){

		if( diagsX_.deepdi == 1 )
				tran_diag_swork_in();
//		diag_write_sworko();
		}

	else if( dian == 10 ){
		diag_write_sworko();
		}

	else if( dian == 11 ){

		/*-----  DIAN = 11 ,  PRINT OPADRO,SCONPO,HFDOPO,HFPOAD ASSOCIATED WITH 
		 *                    ITS CORRESPONDING SWORKO */
		diag_write_parsetran_outarrays();
		}
			

	else if( dian == 12 ){
				/*          dian = 12,  PRINT INPUT SWORK1 records */

				fprintf( _spec_fp, "\n*-SWORK RECORDS*                                                        PAT         STEM    LVL                   OFL3I OFL4I TYPSAV\n" );
				for( i=1; i <= swork1X_.phct; i++ ){
					fprintf( _spec_fp, "%3d ", i );
					for( j=0; j <= 2; j++ ){
						fprintf( _spec_fp, "%3d", swork1X_.swork1[i-One][j] );
						}
					fprintf( _spec_fp, " " );

					for( lt2=3; lt2 <= 12; lt2+=4 ){
						fprintf( _spec_fp, "%3d", swork1X_.swork1[i-One][lt2] );
						fprintf( _spec_fp, "%4d", swork1X_.swork1[i-One][lt2+1] );
						fprintf( _spec_fp, "%3d ", swork1X_.swork1[i-One][lt2+2] );
						fprintf( _spec_fp, "%4d   ", swork1X_.swork1[i-One][lt2+3] );
						}

					for( j=1; j <= 3; j++ ){
						fprintf( _spec_fp, "%3d", ptdiagX_.patno[sconX_.scolnk[i-One]-One][j-One] );
						}
					for( j=1; j <= 3; j++ ){
						fprintf( _spec_fp, "%3d", ptdiagX_.stemno[sconX_.scolnk[i-One]-One][j-One] );
						}
					fprintf( _spec_fp, "   %2d", sent_wordsX_.source_word_count[sconX_.scolnk[i-One]-One] );
					fprintf( _spec_fp, " %16.16s", sent_wordsX_.source_word[sconX_.scolnk[i-One]-One] );
					fprintf( _spec_fp, "%3d%6d   %2d\n",
						ofltabX_.ofl3i[sconX_.scolnk[i-One]-One],
						ofltabX_.ofl4i[sconX_.scolnk[i-One]-One], 
					    typsvX_.typsav[sconX_.scolnk[i-One]-One] );
					}

				tran_diag_swork_in();
				}


	else if( dian == 13 ){
				/*-----  DIAN = 13 ,  PRINT SWRK1I */

				fprintf( _spec_fp, "\n       *****  INPUT FROM %6.6s *****\n                SWORKS\\t   SC2   SC11  SC13\n", 
				  passesX_.pssnam );
				for( iz=1; iz <= elemctX_.parsct; iz++ ){
					xx = dct2X_.dct2[prctX_.js[sconX_.scolnk[iz-One]-One]-One];
					xx1 = xx + 1;
					xx2 = xx + 2;
					fprintf( _spec_fp, "%2d  ", iz );
					for( i=1; i <= 3; i++ ){
						fprintf( _spec_fp, "%2d", swork1X_.swrk1i[iz-One][i-One] );
						}
					fprintf( _spec_fp, " %2d %2d   %2d", swork1X_.swrk1i[iz-One][xx-One], 
					  swork1X_.swrk1i[iz-One][xx1-One], swork1X_.swrk1i[iz-One][xx2-One] );
					fprintf( _spec_fp, " " );
					for( i=1; i <= 3; i++ ){
						fprintf( _spec_fp, "%3d", sconinX_.sconin[iz-One][i-One] );
						}
					fprintf( _spec_fp, " \n" );

					}

				}

	return;


} /*end of function*/

////////////////////////////////////////////////////////////
void diag_write_target_info()
{

	static char diagwd[200];
	int i,jb,target_address,rcode,_l0, _l1;


	if( diagsX_.anydi == 0 ){
		return;
	}


	fprintf( _spec_fp, " \n*THE TRATAB* \n                          SWKAD    WC  O2A  O2B  O3A  O3B  TGPN  TG25     TARGET WORD \n" );

	for( i=1; i <= (swork1X_.phct); i++ ){

		jb = prctX_.js[sconX_.scolnk[i-One]-One];
		target_address = targX_.targ[sconX_.scolnk[i-One]-One][jb-One];

		if(target_address > 0) {
			memset(diagwd,' ',sizeof(diagwd));
			rcode = TARG_PHRASE(&trgflgX_.trgflg,ADR(_l0,1),
					 &target_address,
				 	 cmpsmcX_.cmpcod[sconX_.scolnk[i-One]-One][jb-One],
					 ADR(_l1,100),diagwd);
		}
		else
		{
			memcpy( diagwd, "-unfound/number-", 16 );
		}

		fprintf( _spec_fp, "%3d  %16.16s   %7d%6d%5d%5d%5d%5d%6d%6d        %16s\n",
			    i,
				sent_wordsX_.source_word[sconX_.scolnk[i-One]-One],

				target_address,
				trgcdsX_.word_class,
				ovc2a3X_.ofl2a[i-One],
				ofl2X_.ofl2i[i-One],
				ovc2a3X_.ofl3a[i-One],
				ofltabX_.ofl3i[i-One],
				targpnX_.targpn[sconX_.scolnk[i-One]-One],
				targ25X_.targ25[sconX_.scolnk[i-One]-One],
				diagwd
				);
	}
	fprintf( _spec_fp, "\n");

}

