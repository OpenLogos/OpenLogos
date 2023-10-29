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


void /*FUNCTION*/ t1sw46()
{
	struct  {
		short int im1, i3save, i3str, n6, n6jim, n6jims, phrstr;
		}	*_flowckX_ = (void*)&flowckX_;
	struct  {
		short int nwrks, index, k3p3, pntr9;
		}	*_semargX_ = (void*)&semargX_;
		/*end of pointer COMMON transls*/
	static short ii = 0;
	static short ms = 0;
	static short om = 0;
	static short wc = 0;
	static short k3p2 = 0;
	static short k3p4 = 0;
	static short n7jim = 0;

	_flowckX_->n6jim = im81X_.im81 - vbdataX_.k3p1;
	vbdataX_.k3n = vbdataX_.k3 + 5;
	k3p2 = vtrfX_.vtrf[vbdataX_.k3+2-One];
	_semargX_->k3p3 = vtrfX_.vtrf[vbdataX_.k3+3-One];
	k3p4 = vtrfX_.vtrf[vbdataX_.k3+4-One];
	/*                        IF 2PASS STRATEGY OFF THEN IGNORE SETTING
	 *                        OF SWORKI, I.E. WHEN SW3146 = 1. */
	if( !(sw3146X_.sw3146 == 1 && (passesX_.passfl != 1 || passesX_.passct != 
	  1)) ){

		if( k3p2 != 0 ){
			if( k3p2 <= 0 ){

				om = swork1X_.swork1[im81X_.im81-k3p2-One][dct2X_.dct2[prctX_.js[sconX_.scolnk[im81X_.im81-k3p2-One]-
				  One]-One]-One];
				if( sw3146X_.sw3146 == 1 ){
					swork1X_.swrk1i[_flowckX_->n6jim-One][dct2X_.dct2[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
					  One]-One]-One] = om;
					/*     SCON(1,N6JIM) = OM                3/30/93 jal */
					}
				else{
					swork1X_.swork1[_flowckX_->n6jim-One][dct2X_.dct2[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
					  One]-One]-One] = om;
					}

				}
			else if( sw3146X_.sw3146 == 1 ){
				swork1X_.swrk1i[_flowckX_->n6jim-One][dct2X_.dct2[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
				  One]-One]-One] = k3p2;
				}
			else{
				swork1X_.swork1[_flowckX_->n6jim-One][dct2X_.dct2[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
				  One]-One]-One] = k3p2;
				}
			}
		/*     SCON(1,N6JIM) = K3P2               3/30/93 jal */

		if( _semargX_->k3p3 != 0 ){
			if( _semargX_->k3p3 <= 0 ){

				n7jim = im81X_.im81 - _semargX_->k3p3;
				if( sw3146X_.sw3146 == 1 ){
					swork1X_.swrk1i[_flowckX_->n6jim-One][dct4X_.dct4[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
					  One]-One]-One] = swork1X_.swork1[n7jim-One][dct4X_.dct4[prctX_.js[sconX_.scolnk[n7jim-One]-
					  One]-One]-One];
					sconinX_.sconin[sconX_.scolnk[_flowckX_->n6jim-One]-
					  One][2-One] = sconX_.scon[n7jim-One][11-One];
					sconinX_.sconin[sconX_.scolnk[_flowckX_->n6jim-One]-
					  One][3-One] = sconX_.scon[n7jim-One][13-One];
					}
				else{
					swork1X_.swork1[_flowckX_->n6jim-One][dct4X_.dct4[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
					  One]-One]-One] = swork1X_.swork1[n7jim-One][dct4X_.dct4[prctX_.js[sconX_.scolnk[n7jim-One]-
					  One]-One]-One];
					sconX_.scon[_flowckX_->n6jim-One][11-One] = sconX_.scon[n7jim-One][11-One];
					sconX_.scon[_flowckX_->n6jim-One][13-One] = sconX_.scon[n7jim-One][13-One];
					}

				}
			else if( _semargX_->k3p3 > 16 ){

				if( _semargX_->k3p3 > 99 ){

					if( sw3146X_.sw3146 == 1 ){
						swork1X_.swrk1i[_flowckX_->n6jim-One][dct4X_.dct4[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
						  One]-One]-One] = _semargX_->k3p3;
						if( sconX_.scon[_flowckX_->n6jim-One][13-One] > 
						  99 )
							sconinX_.sconin[sconX_.scolnk[_flowckX_->n6jim-One]-
							  One][3-One] = _semargX_->k3p3;
						if( sconX_.scon[_flowckX_->n6jim-One][11-One] > 
						  99 )
							sconinX_.sconin[sconX_.scolnk[_flowckX_->n6jim-One]-
							  One][2-One] = _semargX_->k3p3;
						}
					else{
						swork1X_.swork1[_flowckX_->n6jim-One][dct4X_.dct4[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
						  One]-One]-One] = _semargX_->k3p3;
						if( sconX_.scon[_flowckX_->n6jim-One][13-One] > 
						  99 )
							sconX_.scon[_flowckX_->n6jim-One][13-One] = _semargX_->k3p3;
						if( sconX_.scon[_flowckX_->n6jim-One][11-One] > 
						  99 )
							sconX_.scon[_flowckX_->n6jim-One][11-One] = _semargX_->k3p3;
						}
					}
				else if( sw3146X_.sw3146 == 1 ){
					if( swork1X_.swrk1i[_flowckX_->n6jim-One][dct4X_.dct4[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
					  One]-One]-One] > 16 && swork1X_.swrk1i[_flowckX_->n6jim-One][dct4X_.dct4[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
					  One]-One]-One] <= 99 )
						swork1X_.swrk1i[_flowckX_->n6jim-One][dct4X_.dct4[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
						  One]-One]-One] = _semargX_->k3p3;
					sconinX_.sconin[sconX_.scolnk[_flowckX_->n6jim-One]-
					  One][2-One] = _semargX_->k3p3;
					if( sconX_.scon[_flowckX_->n6jim-One][13-One] > 
					  16 && sconX_.scon[_flowckX_->n6jim-One][13-One] <= 
					  99 )
						sconinX_.sconin[sconX_.scolnk[_flowckX_->n6jim-One]-
						  One][3-One] = _semargX_->k3p3;
					}
				else{
					if( swork1X_.swork1[_flowckX_->n6jim-One][dct4X_.dct4[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
					  One]-One]-One] > 16 && swork1X_.swork1[_flowckX_->n6jim-One][dct4X_.dct4[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
					  One]-One]-One] <= 99 )
						swork1X_.swork1[_flowckX_->n6jim-One][dct4X_.dct4[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
						  One]-One]-One] = _semargX_->k3p3;
					sconX_.scon[_flowckX_->n6jim-One][11-One] = _semargX_->k3p3;
					if( sconX_.scon[_flowckX_->n6jim-One][13-One] > 
					  16 && sconX_.scon[_flowckX_->n6jim-One][13-One] <= 
					  99 )
						sconX_.scon[_flowckX_->n6jim-One][13-One] = _semargX_->k3p3;
					}
				}
			else if( sw3146X_.sw3146 == 1 ){
				if( swork1X_.swrk1i[_flowckX_->n6jim-One][dct4X_.dct4[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
				  One]-One]-One] <= 16 )
					swork1X_.swrk1i[_flowckX_->n6jim-One][dct4X_.dct4[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
					  One]-One]-One] = _semargX_->k3p3;
				sconinX_.sconin[sconX_.scolnk[_flowckX_->n6jim-One]-
				  One][3-One] = _semargX_->k3p3;
				if( sconX_.scon[_flowckX_->n6jim-One][11-One] <= 16 )
					sconinX_.sconin[sconX_.scolnk[_flowckX_->n6jim-One]-
					  One][2-One] = _semargX_->k3p3;
				}
			else{
				if( swork1X_.swork1[_flowckX_->n6jim-One][dct4X_.dct4[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
				  One]-One]-One] <= 16 )
					swork1X_.swork1[_flowckX_->n6jim-One][dct4X_.dct4[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
					  One]-One]-One] = _semargX_->k3p3;
				sconX_.scon[_flowckX_->n6jim-One][13-One] = _semargX_->k3p3;
				if( sconX_.scon[_flowckX_->n6jim-One][11-One] <= 16 )
					sconX_.scon[_flowckX_->n6jim-One][11-One] = _semargX_->k3p3;
				}
			}
		/*OM- */


		if( k3p4 != 0 ){
			if( k3p4 > 0 ){

				if( sw3146X_.sw3146 == 1 ){
					wc = swork1X_.swrk1i[_flowckX_->n6jim-One][dct2X_.dct2[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
					  One]-One]-One];
					/*             FOR ENGLISH SOURCE - CHANGE ALL WC 02S IN THE SWORK1 */
					if( wc != 2 || srcflgX_.srcflg != 2 ){
						swork1X_.swrk1i[_flowckX_->n6jim-One][dct3X_.dct3[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
						  One]-One]-One] = k3p4;
						}
					else{
						for( ms=1; ms <= 3; ms++ ){
							wc = swork1X_.swrk1i[_flowckX_->n6jim-One][ms*4-One];
							if( wc == 2 )
								swork1X_.swrk1i[_flowckX_->n6jim-One][(ms*4)+2-One] = k3p4;
							}
						}
					}
				else{
					wc = swork1X_.swork1[_flowckX_->n6jim-One][dct2X_.dct2[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
					  One]-One]-One];
					/*             FOR ENGLISH SOURCE - CHANGE ALL WC 02S IN THE SWORK1 */
					if( wc != 2 || srcflgX_.srcflg != 2 ){
						swork1X_.swork1[_flowckX_->n6jim-One][dct3X_.dct3[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
						  One]-One]-One] = k3p4;
						}
					else{
						for( ms=1; ms <= 3; ms++ ){
							wc = swork1X_.swork1[_flowckX_->n6jim-One][ms*4-One];
							if( wc == 2 )
								swork1X_.swork1[_flowckX_->n6jim-One][(ms*4)+2-One] = k3p4;
							}
						}
					}

				}
			else if( sw3146X_.sw3146 == 1 ){
				swork1X_.swrk1i[_flowckX_->n6jim-One][dct3X_.dct3[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
				  One]-One]-One] = swork1X_.swork1[im81X_.im81-k3p4-One][dct3X_.dct3[prctX_.js[sconX_.scolnk[im81X_.im81-k3p4-One]-
				  One]-One]-One];
				if( k3p4 == k3p2 && k3p4 == _semargX_->k3p3 )
					formsaX_.formsv[sconX_.scolnk[_flowckX_->n6jim-One]-
					  One] = formsaX_.formsv[sconX_.scolnk[im81X_.im81-k3p4-One]-
					  One];
				}
			else{
				swork1X_.swork1[_flowckX_->n6jim-One][dct3X_.dct3[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
				  One]-One]-One] = swork1X_.swork1[im81X_.im81-k3p4-One][dct3X_.dct3[prctX_.js[sconX_.scolnk[im81X_.im81-k3p4-One]-
				  One]-One]-One];
				if( k3p4 == k3p2 && k3p4 == _semargX_->k3p3 )
					formsaX_.prsfrm[sconX_.scolnk[_flowckX_->n6jim-One]-
					  One] = formsaX_.formsv[sconX_.scolnk[im81X_.im81-k3p4-One]-
					  One];
				}
			}
		}

	if( opswX_.sw[10-One] == 1 )
		{
		fprintf( _spec_fp, "  SW46 DIAGS: %5d%5d", prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
		  One], dct4X_.dct4[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
		  One]-One] );
		for( ii=1; ii <= 15; ii++ ){
			fprintf( _spec_fp, "%4d", swork1X_.swork1[_flowckX_->n6jim-One][ii-One] );
			}
		for( ii=1; ii <= 20; ii++ ){
			fprintf( _spec_fp, "%4d", sconX_.scon[_flowckX_->n6jim-One][ii-One] );
			}
		fprintf( _spec_fp, "\n" );
		}

	sw3146X_.sw3146 = 0;

	return;
} /*end of function*/

