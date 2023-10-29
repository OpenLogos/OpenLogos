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

EXTERN struct t_sw26nX_ {
	short int sw26n;
	}	sw26nX_;

void /*FUNCTION*/ t1sw34()
{
	struct  {
		short int im1, i3save, i3str, n6, n6jim, n6jims, phrstr;
		}	*_flowckX_ = (void*)&flowckX_;
	struct  {
		short int nwrks, index, k3p3, pntr9;
		}	*_semargX_ = (void*)&semargX_;
	struct  {
		short int hedcas, hedgen, hednum, hedper, hedsc3, ngen, nnum, 
		  relngn, relnnm, relnpr, sw25n, tw25;
		}	*_sw25bkX_ = (void*)&sw25bkX_;
		/*end of pointer COMMON transls*/
	static short iz = 0;
	static short k3p2 = 0;
	static short k3p4 = 0;

	/*   ***** BEGINNING OF -34 SWITCH ***** */
	/*          FUNCTION - K3P4 POINTS TO HEAD
	 *          IF 0, HEAD IS SWORK1 TO LEFT: PARM 1,2,3,= WC,T,F */
	vbdataX_.k3n = vbdataX_.k3 + 5;
	if( _sw25bkX_->tw25 != 1 ){
		k3p2 = vtrfX_.vtrf[vbdataX_.k3+2-One];
		_semargX_->k3p3 = vtrfX_.vtrf[vbdataX_.k3+3-One];
		k3p4 = vtrfX_.vtrf[vbdataX_.k3+4-One];
		_flowckX_->n6jim = im81X_.im81 - k3p4;
		if( supresX_.phsup != 1 )
			supresX_.phcts = sworkoX_.phcto;
		sw34bkX_.sw34n = 1;

		if( vbdataX_.k3p1 != 0 ){
			if( vbdataX_.k3p1 > 0 ){
				sworkoX_.sworko[sworkoX_.phcto-One][1-One] = vbdataX_.k3p1;
				}
			else{
				sworkoX_.sworko[sworkoX_.phcto-One][1-One] = swork1X_.swork1[im81X_.im81-vbdataX_.k3p1-One][dct2X_.dct2[prctX_.js[sconX_.scolnk[im81X_.im81-vbdataX_.k3p1-One]-
				  One]-One]-One];
				}
			}

		if( k3p2 != 0 ){
			if( k3p2 <= 0 ){
				sworkoX_.sworko[sworkoX_.phcto-One][2-One] = swork1X_.swork1[im81X_.im81-k3p2-One][dct4X_.dct4[prctX_.js[sconX_.scolnk[im81X_.im81-k3p2-One]-
				  One]-One]-One];

				}
			else if( k3p2 <= 16 ){
				/*OM+ */

				sconX_.scon[elscnpX_.elscnp[_flowckX_->n6jim-One]-
				  One][13-One] = k3p2;
				sworkoX_.sworko[sworkoX_.phcto-One][2-One] = swork1X_.swork1[_flowckX_->n6jim-One][dct4X_.dct4[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
				  One]-One]-One];

				/*     IN CASE TYPE STORED IN WRONG PLACE - CHECK ALL THREE */

				if( sworkoX_.sworko[sworkoX_.phcto-One][2-One] <= 
				  16 )
					sworkoX_.sworko[sworkoX_.phcto-One][2-One] = k3p2;
				if( sconX_.scon[elscnpX_.elscnp[_flowckX_->n6jim-One]-
				  One][11-One] <= 16 )
					sconX_.scon[elscnpX_.elscnp[_flowckX_->n6jim-One]-
					  One][11-One] = k3p2;

				}
			else if( k3p2 > 99 ){

				sworkoX_.sworko[sworkoX_.phcto-One][2-One] = k3p2;
				if( sconX_.scon[elscnpX_.elscnp[_flowckX_->n6jim-One]-
				  One][11-One] > 99 )
					sconX_.scon[elscnpX_.elscnp[_flowckX_->n6jim-One]-
					  One][11-One] = k3p2;
				if( sconX_.scon[elscnpX_.elscnp[_flowckX_->n6jim-One]-
				  One][13-One] > 99 )
					sconX_.scon[elscnpX_.elscnp[_flowckX_->n6jim-One]-
					  One][13-One] = k3p2;
				}
			else{
				sconX_.scon[elscnpX_.elscnp[_flowckX_->n6jim-One]-
				  One][11-One] = k3p2;
				sworkoX_.sworko[sworkoX_.phcto-One][2-One] = swork1X_.swork1[_flowckX_->n6jim-One][dct4X_.dct4[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
				  One]-One]-One];
				if( sworkoX_.sworko[sworkoX_.phcto-One][2-One] <= 
				  99 )
					sworkoX_.sworko[sworkoX_.phcto-One][2-One] = k3p2;
				if( sconX_.scon[elscnpX_.elscnp[_flowckX_->n6jim-One]-
				  One][13-One] > 16 )
					sconX_.scon[elscnpX_.elscnp[_flowckX_->n6jim-One]-
					  One][13-One] = k3p2;
				}
			}
		/*OM- */

		if( _semargX_->k3p3 != 0 ){
			if( _semargX_->k3p3 > 0 ){

				sworkoX_.sworko[sworkoX_.phcto-One][3-One] = _semargX_->k3p3;
				}
			else{
				sworkoX_.sworko[sworkoX_.phcto-One][3-One] = swork1X_.swork1[im81X_.im81-_semargX_->k3p3-One][dct3X_.dct3[prctX_.js[sconX_.scolnk[im81X_.im81-_semargX_->k3p3-One]-
				  One]-One]-One];
				}
			}

		sworkoX_.sworko[sworkoX_.phcto-One][4-One] = _flowckX_->n6jim;
		sworkoX_.phrhdo[sworkoX_.phcto-One] = elscnpX_.elscnp[_flowckX_->n6jim-One];

		/*   IF N6JIM HAS NOT YET BEEN LOADED (I.E. IT DOES NOT YET HAVE
		 *   A SCON POSITION), PHRHDO(PHCTO) WILL NOT BE SET TILL END OF TRAN1 */

		if( elscnpX_.elscnp[_flowckX_->n6jim-One] == 0 )
			sworkoX_.phrhdo[sworkoX_.phcto-One] = _flowckX_->n6jim + 
			  900;
		}

	 /*----------------NOTE THAT N6JIM GETS PRINTED TWICE------------ */
	if( opswX_.sw[10-One] == 1 )
		{
		fprintf( _spec_fp, " SW34 %5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d", 
		  k3p2, _semargX_->k3p3, k3p4, _flowckX_->n6jim, vbdataX_.k3n, 
		  sw26nX_.sw26n, sworkoX_.phcto, supresX_.phcts, sw34bkX_.sw34n, 
		  im81X_.im81, _flowckX_->n6jim, sworkoX_.phrhdo[sworkoX_.phcto-One] );
		for( iz=1; iz <= 4; iz++ ){
			fprintf( _spec_fp, "%5d", sworkoX_.sworko[sworkoX_.phcto-One][iz-One] );
			}
		fprintf( _spec_fp, "\n" );
		}
	return;
} /*end of function*/

