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
	/*   ***** BEGINNING OF -16 SWITCH ***** */
	/*     FUNCTION:  TO CHANGE SCONS OF ELEMENT POINTED TO BY VTRF(K3+5) */

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


void /*FUNCTION*/ t1sw16()
{
	struct  {
		short int hedcas, hedgen, hednum, hedper, hedsc3, ngen, nnum, 
		  relngn, relnnm, relnpr, sw25n, tw25;
		}	*_sw25bkX_ = (void*)&sw25bkX_;

	static short xx = 0;
	static short im1 = 0;
	static short case_ = 0;
	static short nsct = 0;
	static short vtrc = 0;
	static short vtrg = 0;
	static short vtrp = 0;
	static short vtrnu = 0;

	vbdataX_.k3n = vbdataX_.k3 + 6;
	xx = 0;
	sw16bkX_.sw16n = 1;
	nsct = prtscoX_.sct;

	if( vtrfX_.vtrf[vbdataX_.k3+5-One] > 0 ){

		xx = vtrfX_.vtrf[vbdataX_.k3+5-One];

		/*   IS XX A V.C. ?? */
		if( xx >= 131 ){

			/*   WAS CONSTANT INHIBITED ??? */
//5-8-98 last50 is constant test and not valid with rdbms if( !((xx >= 131 && xx <= -last50X_.last50) && inhbX_.inhb[xx-One] == 0) )
			if( !(inhbX_.inhb[xx-One] == 0) )
				goto L_2760;

			}
		}
	else{
		nsct = elscnpX_.elscnp[im81X_.im81-vtrfX_.vtrf[vbdataX_.k3+5-One]-One];
		}

	if( sconX_.scon[nsct-One][1-One] > 0 ){

		vtrg = vtrfX_.vtrf[vbdataX_.k3+1-One];
		vtrnu = vtrfX_.vtrf[vbdataX_.k3+2-One];
		vtrp = vtrfX_.vtrf[vbdataX_.k3+3-One];
		vtrc = vtrfX_.vtrf[vbdataX_.k3+4-One];

		/*   GENDER - SCON(4) */

		if( vtrg != 0 ){
			if( vtrg >= 0 ){

				sconX_.scon[nsct-One][4-One] = vtrg;

				}
			else if( vtrg == -99 ){
				sconX_.scon[nsct-One][4-One] = sw14bkX_.gen;

				}
			else if( vtrg == -98 ){
				sconX_.scon[nsct-One][4-One] = _sw25bkX_->hedgen;
				}
			else{

				/*   VTRG IS A RELATIONAL POINTER */

				sconX_.scon[nsct-One][4-One] = sconX_.scon[elscnpX_.elscnp[im81X_.im81-vtrg-One]-
				  One][4-One];
				}
			}

		/*   NUMBER - SCON(5) */

		if( vtrnu != 0 ){
			if( vtrnu >= 0 ){

				sconX_.scon[nsct-One][5-One] = vtrnu;

				}
			else if( vtrnu == -99 ){
				sconX_.scon[nsct-One][5-One] = sw14bkX_.num;

				}
			else if( vtrnu == -98 ){
				sconX_.scon[nsct-One][5-One] = _sw25bkX_->hednum;
				}
			else{

				/*   VTRG IS A RELATIONAL POINTER */

				sconX_.scon[nsct-One][5-One] = sconX_.scon[elscnpX_.elscnp[im81X_.im81-vtrnu-One]-
				  One][5-One];
				}
			}

		/*   PERSON - SCON(6) */

		if( vtrp != 0 ){
			if( vtrp >= 0 ){

				sconX_.scon[nsct-One][6-One] = vtrp;

				}
			else if( vtrp == -99 ){
				sconX_.scon[nsct-One][6-One] = sw14bkX_.per;

				}
			else if( vtrp == -98 ){
				sconX_.scon[nsct-One][6-One] = _sw25bkX_->hedper;
				}
			else{

				/*   VTRP IS A RELATIONAL POINTER */

				sconX_.scon[nsct-One][6-One] = sconX_.scon[elscnpX_.elscnp[im81X_.im81-vtrp-One]-
				  One][6-One];
				}
			}

		/*   CASE/TENSE - SCON(7) */

		if( vtrc == 0 )
			goto L_2760;
		if( vtrc >= 0 ){

			sconX_.scon[nsct-One][7-One] = vtrc;
			goto L_2760;

			}
		else if( vtrc == -99 ){
			sconX_.scon[nsct-One][7-One] = case_;
			goto L_2760;

			}
		else if( vtrc == -98 ){
			sconX_.scon[nsct-One][7-One] = _sw25bkX_->hedcas;
			goto L_2760;
			}

		/*   EVEN IF SCON IS LOCKED, SCON(7) CAN BE RESET. */
		}
	else if( (vtrfX_.vtrf[vbdataX_.k3+4-One] >= 0 || vtrfX_.vtrf[vbdataX_.k3+4-One] == 
	  -98) || vtrfX_.vtrf[vbdataX_.k3+4-One] == -99 ){
		goto L_2760;
		}

	/*   VTRC IS A RELATIONAL POINTER */

	sconX_.scon[nsct-One][7-One] = sconX_.scon[elscnpX_.elscnp[im1-vtrc-80-One]-One][7-One];

L_2760:
	;
	if( opswX_.sw[10-One] == 1 )
		{
		fprintf( _spec_fp, " SW16 %6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d", 
		  vbdataX_.k3n, vtrg, vtrnu, vtrp, vtrc, sw36bkX_.relper, 
		  sw36bkX_.relcas, nsct, sw36bkX_.rel, xx, inhbX_.inhb[xx-One] );
		for( xx=4; xx <= 7; xx++ ){
			fprintf( _spec_fp, "%6d", sconX_.scon[nsct-One][xx-One] );
			}
		fprintf( _spec_fp, "\n" );
		}

	return;
} /*end of function*/

