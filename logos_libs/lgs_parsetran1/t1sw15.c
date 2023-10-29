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
EXTERN struct t_sw31bkX_ {
	short int artd, artind, dative, fakev, inhb43, maktrn, negflg, 
	  subcl;
	}	sw31bkX_;
EXTERN struct t_sw35bkX_ {
	short int pass, relpas;
	}	sw35bkX_;
EXTERN struct t_sw37bkX_ {
	short int arith;
	}	sw37bkX_;
	/* end of COMMON translations */
void /*FUNCTION*/ t1sw15()
{
		/*pointer COMMON transls*/
	struct  {
		short int hedcas, hedgen, hednum, hedper, hedsc3, ngen, nnum, 
		  relngn, relnnm, relnpr, sw25n, tw25;
		}	*_sw25bkX_ = (void*)&sw25bkX_;
	struct  {
		short int pcb, pcbwc;
		}	*_sw23bkX_ = (void*)&sw23bkX_;
		/*end of pointer COMMON transls*/
	static short sw10 = 0;

	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *
	 *SW15 */
	/*   ***** BEGINNING OF -15 SWITCH ***** */
	/*          FUNCTION - TO RE-INITIALIZE
	 *          CASE, TENSE, PER, NO, GEN AT BEGINNING OF NEW CLAUSE */
	case_X_.case_ = 1;
	/*+                                                         *R1071MBS
	 *          INHB(136) = 0 REMOVED HERE
	 *-                                                         *R1071MBS */
	_sw23bkX_->pcb = 0;
	sw31bkX_.fakev = 0;
	sw37bkX_.arith = 0;
	sw33bkX_.altwc = 0;
	sw19bkX_.tense = 0;
	sw14bkX_.num = 0;
	sw14bkX_.per = 3;
	sw14bkX_.gen = 0;
	sw21bkX_.case_ = 0;
	_sw25bkX_->hedper = 3;
	_sw25bkX_->hednum = 0;
	_sw25bkX_->hedgen = 0;
	_sw25bkX_->hedcas = 1;
	_sw25bkX_->hedsc3 = 0;
	sw31bkX_.negflg = 0;
	sw36bkX_.rel = 0;
	sw36bkX_.relcas = 1;
	sw36bkX_.relnum = 0;
	sw36bkX_.relper = 3;
	sw36bkX_.relgen = 0;
	sw35bkX_.relpas = 0;
	sw35bkX_.pass = 0;
	sw36bkX_.det = 0;
	sw31bkX_.dative = 0;
	sw31bkX_.subcl = 0;

	/*     CHK PARAMETER; SET CLAUSE OR SUBCLAUSE FLGS */
	if( vbdataX_.k3p1 == 2 )
		sw31bkX_.subcl += 1;
	/*10+ */
	if( sw10 == 1 )
		{
		fprintf( _spec_fp, " SW15 %6d\n", sw31bkX_.subcl );
		}
	/*10- */

	/*   ***** END OF -15 SWITCH ***** */

	/*SW15 */
	vbdataX_.k3n = vbdataX_.k3 + 2;
	return;
} /*end of function*/

