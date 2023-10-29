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
	/*  CHANGES:
	 *      6/10/93 jal: if pass1 of 2 skip target scon setting.
	 *      02/13/91 *JAL*  APPLY HEAD CASE TO SUBORDINATE ELEMENTS ONLY
	 *               IF GERMAN TARGET.
	 *      04/17/87 *R1685RKH*  CHANGE T1-4 SWORK1 LIMIT FROM 50 TO
	 *      08/17/86 *R1561DSD: 100 SCONS
	 *      09/23/85 */
	/*   ***** BEGINNING OF -24 SWITCH ***** */
	/*         FUNCTION: BREAKS VTR RULE INTO MORE THAN 1 SWORKO
	 *         BY CREATING SWORKO FOR VTR TO LEFT OF SWITCH
	 *         PARAMETER IS NUMBER OF ELEMENTS (LI) */

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


EXTERN struct t_head26X_ {
	short int headwc, headty, headfr, headhd, headt2, headt3;
	}	head26X_;


void /*FUNCTION*/ t1sw24()
{
	struct  {
		short int k7, oflad, n3;
		}	*_diacbX_ = (void*)&diacbX_;
	struct  {
		short int vtr[26];
		}	*_vtrX_ = (void*)&vtrX_;
	struct  {
		short int im1, i3save, i3str, n6, n6jim, n6jims, phrstr;
		}	*_flowckX_ = (void*)&flowckX_;
	struct  {
		short int hedcas, hedgen, hednum, hedper, hedsc3, ngen, nnum, 
		  relngn, relnnm, relnpr, sw25n, tw25;
		}	*_sw25bkX_ = (void*)&sw25bkX_;
		/*end of pointer COMMON transls*/
	static short iz = 0;
	static short curhed = 0;
	static short sconiz = 0;

	sworkoX_.sworko[sworkoX_.phcto+1-One][1-One] = swork1X_.swork1[_flowckX_->n6-One][dct2X_.dct2[prctX_.js[sconX_.scolnk[_flowckX_->n6-One]-
	  One]-One]-One];
	sworkoX_.sworko[sworkoX_.phcto+1-One][2-One] = swork1X_.swork1[_flowckX_->n6-One][dct4X_.dct4[prctX_.js[sconX_.scolnk[_flowckX_->n6-One]-
	  One]-One]-One];
	sworkoX_.sworko[sworkoX_.phcto+1-One][3-One] = swork1X_.swork1[_flowckX_->n6-One][dct3X_.dct3[prctX_.js[sconX_.scolnk[_flowckX_->n6-One]-
	  One]-One]-One];
	sworkoX_.sworko[sworkoX_.phcto+1-One][4-One] = _flowckX_->n6;
	if( sworkoX_.phcto > 0 )
		sworkoX_.phrndo[sworkoX_.phcto-One] = _diacbX_->n3;
	sworkoX_.phrbgo[sworkoX_.phcto+1-One] = _diacbX_->n3 + 1;
	sworkoX_.phrhdo[sworkoX_.phcto+1-One] = elscnpX_.elscnp[_flowckX_->n6-One];

	_flowckX_->n6jim = im81X_.im81 - vbdataX_.k3p1;
	if( supresX_.phcts != sworkoX_.phcto ){

		sworkoX_.sworko[sworkoX_.phcto-One][1-One] = swork1X_.swork1[_flowckX_->n6jim-One][dct2X_.dct2[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
		  One]-One]-One];
		sworkoX_.sworko[sworkoX_.phcto-One][2-One] = swork1X_.swork1[_flowckX_->n6jim-One][dct4X_.dct4[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
		  One]-One]-One];
		sworkoX_.sworko[sworkoX_.phcto-One][3-One] = swork1X_.swork1[_flowckX_->n6jim-One][dct3X_.dct3[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
		  One]-One]-One];
		sworkoX_.sworko[sworkoX_.phcto-One][4-One] = _flowckX_->n6jim;
		}

	sworkoX_.phrhdo[sworkoX_.phcto-One] = elscnpX_.elscnp[im81X_.im81-vbdataX_.k3p1-One];

	if( sworkoX_.phcto == sw26bkX_.phr26 )
		t1se26();

	/*            if pass1 of 2 skip target scon setting */
	if( passesX_.passfl == 1 && passesX_.passct == 1 ){
		_sw25bkX_->sw25n = 0;
		supresX_.n3sw25 = 0;
		sw21bkX_.sw21n = 0;
		}
	else{

		/*   MAKE NOUN PHRASE ELEMENTS AGREE IN NUMBER, PERSON,
		 *   CASE AND GENDER */

		if( _sw25bkX_->sw25n == 1 ){

			if( supresX_.n3sw25 != 0 )
				_flowckX_->phrstr = supresX_.n3sw25;
			supresX_.n3sw25 = 0;
			_sw25bkX_->sw25n = 0;

			for( iz=_flowckX_->phrstr; iz <= _diacbX_->n3; iz++ ){
				sconiz = opadroX_.sconpo[iz-One];
				if( sconiz > 0 ){
					if( sconX_.scon[sconiz-One][1-One] > 0 ){
						if( sconX_.scon[sconiz-One][9-One] != 1 ){
							sconX_.scon[sconiz-One][4-One] = _sw25bkX_->hedgen;
							sconX_.scon[sconiz-One][5-One] = _sw25bkX_->hednum;
							sconX_.scon[sconiz-One][6-One] = _sw25bkX_->hedper;
							if( !(srcflgX_.srcflg == 1 && trgflgX_.trgflg == 3) ){
								if( trgflgX_.trgflg == 1 )
									sconX_.scon[sconiz-One][7-One] = _sw25bkX_->hedcas;
								}
							}
						}
					}
				}
			}

		/*   SAME FOR VERB PHRASE */

		if( sw21bkX_.sw21n == 1 ){
			sw21bkX_.sw21n = 0;
			curhed = sworkoX_.phrhdo[sworkoX_.phcto-One];
			for( iz=_flowckX_->phrstr; iz <= _diacbX_->n3; iz++ ){
				sconiz = opadroX_.sconpo[iz-One];
				if( sconiz > 0 ){
					if( sconX_.scon[sconiz-One][1-One] > 0 ){
						if( sconX_.scon[sconiz-One][9-One] != 1 ){
							sconX_.scon[sconiz-One][4-One] = sconX_.scon[curhed-One][4-One];
							sconX_.scon[sconiz-One][5-One] = sconX_.scon[curhed-One][5-One];
							sconX_.scon[sconiz-One][6-One] = sconX_.scon[curhed-One][6-One];
							sconX_.scon[sconiz-One][7-One] = sconX_.scon[curhed-One][7-One];
							}
						}
					}
				}
			}
		}

	_flowckX_->phrstr = _diacbX_->n3 + 1;
	sworkoX_.phcto += 1;
	_sw25bkX_->tw25 = 0;
	sw34bkX_.sw34n = 0;

	if( opswX_.sw[10-One] == 1 )
		{
		scnprt(sconX_.scon,sconX_.scono);
		fprintf( _spec_fp, " SW24 %6d%6d%6d%6d%6d%6d%6d%6d", _flowckX_->n6jim, 
		  sworkoX_.phrhdo[sworkoX_.phcto-One], sworkoX_.phcto, _diacbX_->n3, 
		  _flowckX_->phrstr, head26X_.headwc, supresX_.phcts, _flowckX_->n6 );
		for( iz=1; iz <= 4; iz++ ){
			fprintf( _spec_fp, "%6d", sworkoX_.sworko[sworkoX_.phcto-One][iz-One] );
			}
		for( iz=1; iz <= 4; iz++ ){
			fprintf( _spec_fp, "%6d", sworkoX_.sworko[sworkoX_.phcto-1-One][iz-One] );
			}
		fprintf( _spec_fp, "\n" );
		}

	vbdataX_.k3n = vbdataX_.k3 + 2;
	return;
} /*end of function*/

