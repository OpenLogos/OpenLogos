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
	/*   ***** BEGINNING -18 SWITCH ***** */
	/*            FUNCTION: SUSPENDS USUAL CONCATENATION
	 *            OF ITEMS TO RIGHT OF SWITCH; ONE PARAMETER, NNN1
	 *            POINTS TO HEAD. */
	/*   PHRNEW USED TO PREVENT CONCATENATION OF
	 *   ELEMENTS ON RIGHT; SETS UP NEW PHRASES INDIVIDUALLY.
	 *   CALCULATE PARAMETER/HEAD, I.E. N6JIM; K3+1 POINTS TO HEAD. */
	/*
	 *  CHANGES:
	 *      02/13/91 *JAL*  APPLY HEAD CASE TO SUBORDINATE ELEMENTS ONLY
	 *               IF GERMAN TARGET.
	 *      CHG: 04/17/87 *R1685RKH*  CHANGE T1-4 SWORK1 LIMIT FROM 50 TO
	 *      CHG 08/17/86 *R1561DSD: 100 SCONS
	 *      CHG 08/17/86
	 *
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

EXTERN struct t_sw18bkX_ {
	short int phrnew;
	}	sw18bkX_;
EXTERN struct t_sw26nX_ {
	short int sw26n;
	}	sw26nX_;


void /*FUNCTION*/ t1sw18()
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

	sw18bkX_.phrnew = 1;
	_flowckX_->n6jim = _flowckX_->im1 - vbdataX_.k3p1 - 80;
	if( supresX_.phcts != sworkoX_.phcto ){

		sworkoX_.sworko[sworkoX_.phcto-One][1-One] = swork1X_.swork1[_flowckX_->n6jim-One][dct2X_.dct2[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
		  One]-One]-One];
		sworkoX_.sworko[sworkoX_.phcto-One][2-One] = swork1X_.swork1[_flowckX_->n6jim-One][dct4X_.dct4[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
		  One]-One]-One];
		sworkoX_.sworko[sworkoX_.phcto-One][3-One] = swork1X_.swork1[_flowckX_->n6jim-One][dct3X_.dct3[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
		  One]-One]-One];
		sworkoX_.sworko[sworkoX_.phcto-One][4-One] = _flowckX_->n6jim;
		}
	sworkoX_.phrhdo[sworkoX_.phcto-One] = elscnpX_.elscnp[_flowckX_->n6jim-One];

	/*   MAKE NOUN PHRASE ELEMENTS AGREE IN NUMBER, PERSON, CASE, AND GENDER */

	if( _sw25bkX_->sw25n == 1 ){
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

	_flowckX_->phrstr = _diacbX_->n3 + 1;

	if( opswX_.sw[10-One] == 1 ) scnprt(sconX_.scon,sconX_.scono);

	vbdataX_.k3n = vbdataX_.k3 + 2;
	return;
} /*end of function*/

