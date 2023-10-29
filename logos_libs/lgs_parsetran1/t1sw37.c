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
	/*   ***** BEGINNING OF -37 SWITCH ***** */
	/*          FUNCTION : TO SET ARITH FLAG FOR RUSSIAN NUMBER(CASE)
	 *          RIGHT ORIENTED EL. TO RT. IS A RUSSIAN NUMBER */

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

EXTERN struct t_s37tabX_ {
	short int s37tab[15];
	}	s37tabX_;
EXTERN struct t_sw36bkX_ {
	short int det, rel, relcas, relgen, relnum, relper;
	}	sw36bkX_;
EXTERN struct t_sw37bkX_ {
	short int arith;
	}	sw37bkX_;

void /*FUNCTION*/ t1sw37()
{
	struct  {
		short int im1, i3save, i3str, n6, n6jim, n6jims, phrstr;
		}	*_flowckX_ = (void*)&flowckX_;

	static short xw = 0;
	static short xx = 0;
	static short xy = 0;
	static short case_ = 0;

	if( sw37bkX_.arith == 0 ){
		xx = case_;
		if( sw36bkX_.rel == 1 )
			xx = sw36bkX_.relcas;
		if( (xx == 1) || (xx == 4) ){
			_flowckX_->n6jim = _flowckX_->im1 - vtrfX_.vtrf[vbdataX_.k3+2-One];
			if( vbdataX_.k3p1 < 0 )
				_flowckX_->n6jim = im81X_.im81 - vbdataX_.k3p1;

			/*          TEST FORM FIELD OF ELEMENT POINTED TO */
			if( swork1X_.swork1[_flowckX_->n6jim-One][dct3X_.dct3[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
			  One]-One]-One] == 1 )
				sw37bkX_.arith = 2;
			if( swork1X_.swork1[_flowckX_->n6jim-One][dct3X_.dct3[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
			  One]-One]-One] == 2 )
				sw37bkX_.arith = 0;
			if( swork1X_.swork1[_flowckX_->n6jim-One][dct3X_.dct3[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
			  One]-One]-One] == 3 )
				sw37bkX_.arith = 1;
			sconX_.scon[elscnpX_.elscnp[_flowckX_->n6jim-One]-One][9-
			  One] = 1;
			}
		}

	if( vbdataX_.k3p1 == 2 ){
		xw = elscnpX_.elscnp[_flowckX_->n6jim-One];
		xy = s37tabX_.s37tab[sconX_.scon[xw-One][7-One]+1-One];
		sconX_.scon[xw-One][7-One] = xy;
		}

	if( opswX_.sw[10-One] == 1 )
		{
		scnprt();

		fprintf( _spec_fp, " SW37 %5d%5d%5d%5d%5d%5d%5d%5d%5d\n", 
		  sw37bkX_.arith, case_, sw36bkX_.relcas, xx, xy, xw, _flowckX_->n6jim, 
		  _flowckX_->im1, sw36bkX_.rel );
		}

		
	vbdataX_.k3n = vbdataX_.k3 + 2;
	return;
} /*end of function*/

