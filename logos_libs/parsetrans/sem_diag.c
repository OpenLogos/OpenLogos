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
#include <string.h>
#include <math.h>
#define MS_F77
#include <fcrt.h>
#include "trans.h"
#include "logoslib.h"
#include "projexts.h"
#include <jbctrl.h>

/*   write diagnostic for passed  semtab  rule */


void /*FUNCTION*/ sem_diag(short spkey[], short spdata[], short spvtr[])
{
	static char cmpcod[4], spcom[200];
	static long int cmntct, cnt, elemct, lvl, ms, pt, pt2, sp_type, 
	  tag_ovr_cnt, tagpt, tagpt_ovr, temp_elements[10][3], tempi4, 
	  testpo;

	if( !(diagsX_.longdi == 0 && diagsX_.deepdi == 0) ){


		lvl = spkey[7];

		/*					get the company code */
					testpo = 17;
					tempi4 = lvl - 2;
					if( tempi4 > 0 )
						testpo += tempi4*6;
					tempi4 = lvl - 1;
					if( tempi4 > 0 )
						testpo += tempi4*4;
		lmove(cmpcod,1,spkey,testpo,3);

		/*					get the comment */
		memset(spcom,' ',sizeof(spcom));
		cmntct = spdata[15]*2;
		if( cmntct > 80 ) cmntct = 80;
		lmove(spcom,1,spdata,33,cmntct);
		spcom[cmntct] = 0;

		for( pt=1; pt <= lvl; pt++ ){
			for( pt2=1; pt2 <= 3; pt2++ ){
				if( pt == 1 ){
					temp_elements[pt-One][pt2-One] = spkey[pt2-One];
					}
				else if( pt == 2 ){
					temp_elements[pt-One][pt2-One] = spkey[5+pt2-1-One];
					}
				else if( pt == 3 ){
					temp_elements[pt-One][pt2-One] = spkey[9+pt2-1-One];
					}
				else{
					temp_elements[pt-One][pt2-One] = spkey[12+(pt-4)*3+pt2-1-One];
					}
				}
			}
		fprintf( _spec_fp,
			    "SEMTAB RULE: LEVEL = %3ld COMPANY CODE = %3.3s\n%s\n", 
				lvl, cmpcod, spcom );

		for( pt=0; pt < lvl; pt++ ){
				fprintf( _spec_fp, "%3d", temp_elements[pt][0] );
				fprintf( _spec_fp, "%5d", temp_elements[pt][1] );
				fprintf( _spec_fp, "%3d    ", temp_elements[pt][2] );
			}
		fprintf( _spec_fp, "\n" );

		for( elemct=2; elemct <= lvl; elemct++ ){
			if( elemct == 2 ){
				sp_type = spkey[5];
				}
			else{
				sp_type = spkey[10+((elemct-3)*3)-One];
				}
			if( ((((sp_type == 1000 || sp_type == 2000) || sp_type == 
			  3000) || sp_type == 4000) || sp_type == 5000) || sp_type == 
			  6000 ){
				/*						point to first 2 tags */
				tagpt = (4 + ((lvl - 1)*3) + 1) + 1 + ((elemct - 2)*2);
				/*						point to the rest of the tags in the data */
				ms = 16 + spdata[16-One] + (elemct - 1);
				tagpt_ovr = spdata[ms-One];
				/*						determine the count of over flow tags */
				tag_ovr_cnt = 0;
				pt = tagpt_ovr;
				while( spdata[pt-One] != -1 ){
					pt += 1;
					}
				if( pt > tagpt_ovr )
					tag_ovr_cnt = pt - tagpt_ovr;
				/*						write the tags */
				fprintf( _spec_fp, "tagset= %5ld", sp_type );
				for( pt=tagpt; pt <= (tagpt + 1); pt++ ){
					fprintf( _spec_fp, "%5d", spkey[pt-One] );
					}
				for( pt=tagpt_ovr; pt <= (tagpt_ovr + tag_ovr_cnt - 1); pt++ ){
					fprintf( _spec_fp, "%5d", spdata[pt-One] );
					}
				fprintf( _spec_fp, "\n" );
				}
			}
		/*				now do the vtr */
		spvtr[spvtr[0]] = 999;
		diag_write_rule_vtr(spvtr);	

		}
	return;
} /*end of function*/

