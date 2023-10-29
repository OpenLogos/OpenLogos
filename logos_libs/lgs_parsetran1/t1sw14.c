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


EXTERN struct t_sw14bkX_ {
	short int gen, num, per;
	}	sw14bkX_;
EXTERN struct t_sw36bkX_ {
	short int det, rel, relcas, relgen, relnum, relper;
	}	sw36bkX_;


void /*FUNCTION*/ t1sw14()
{
	struct  {
		short int im1, i3save, i3str, n6, n6jim, n6jims, phrstr;
		}	*_flowckX_ = (void*)&flowckX_;
		/*end of pointer COMMON transls*/
	static short vtrp = 0;
	static short vtrq = 0;
	static short vtrr = 0;

	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *
	 *SW14 */
	/*   ***** BEGINNING OF -14 SWITCH ***** */
	/*          THREE PARAMETERS: GENDER, NUMBER, PERSON. INSTRUCTS
	 *          SYSTEM TO ESTABLISH NO, GEN, PERSON. IF ANY
	 *          PARAMETER IS NEGATIVE IT IS A POINTER TO AN SWORK. */
	vbdataX_.k3n = vbdataX_.k3 + 4;
	vtrp = vtrfX_.vtrf[vbdataX_.k3+1-One];
	vtrq = vtrfX_.vtrf[vbdataX_.k3+2-One];
	vtrr = vtrfX_.vtrf[vbdataX_.k3+3-One];

	/*     CHECK EACH PARAMETER AND CURRENT SETTING OF REL FLAG */

	if( sw36bkX_.rel > 0 ){

		if( vtrp > 0 )
			sw36bkX_.relgen = vtrp;
		if( vtrq > 0 )
			sw36bkX_.relnum = vtrq;
		if( vtrr > 0 )
			sw36bkX_.relper = vtrr;
		if( vtrp < 0 )
			sw36bkX_.relgen = sconX_.scon[elscnpX_.elscnp[im81X_.im81-vtrp-One]-
			  One][4-One];
		if( vtrq < 0 )
			sw36bkX_.relnum = sconX_.scon[elscnpX_.elscnp[im81X_.im81-vtrq-One]-
			  One][5-One];
		if( vtrr < 0 )
			sw36bkX_.relper = sconX_.scon[elscnpX_.elscnp[im81X_.im81-vtrr-One]-
			  One][6-One];
		}
	else{

		if( vtrp > 0 )
			sw14bkX_.gen = vtrp;
		if( vtrq > 0 )
			sw14bkX_.num = vtrq;
		if( vtrr > 0 )
			sw14bkX_.per = vtrr;
		if( vtrp < 0 )
			sw14bkX_.gen = sconX_.scon[elscnpX_.elscnp[im81X_.im81-vtrp-One]-
			  One][4-One];
		if( vtrq < 0 )
			sw14bkX_.num = sconX_.scon[elscnpX_.elscnp[im81X_.im81-vtrq-One]-
			  One][5-One];
		if( vtrr < 0 )
			sw14bkX_.per = sconX_.scon[elscnpX_.elscnp[im81X_.im81-vtrr-One]-
			  One][6-One];
		}

	if( opswX_.sw[10-One] == 1 )
		{
		fprintf( _spec_fp, " SW14 %6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d\n", 
		  vbdataX_.k3n, vtrnX_.vtrn, vbdataX_.k3p1, vtrp, vtrq, vtrr, 
		  sw14bkX_.gen, sw14bkX_.num, sw14bkX_.per, sw36bkX_.relgen, 
		  sw36bkX_.relnum, case_X_.case_, sw36bkX_.rel, sw36bkX_.relcas );
		}
	return;
} /*end of function*/

