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



void /*FUNCTION*/ t1sw39()
{
		/*pointer COMMON transls*/
	struct  {
		short int sw1, sw2, sw3, sw4, sw5, sw6, sw7, sw8, sw9, sw10, 
		  sw11, sw12, sw13, sw14, sw15, sw16, sw17, sw18, sw19, sw20, 
		  sw21, sw22, sw23, sw24, sw25, sw26, sw27, sw28, sw29, sw30;
		}	*_opswX_ = (void*)&opswX_;
	struct  {
		short int hedcas, hedgen, hednum, hedper, hedsc3, ngen, nnum, 
		  relngn, relnnm, relnpr, sw25n, tw25;
		}	*_sw25bkX_ = (void*)&sw25bkX_;
		/*end of pointer COMMON transls*/
	static short xx = 0;
	static short nsct = 0;
	static short vtrn = 0;

	/* LAST CHG 08/17/86 *R1561DSD: 100 SCONS
	 *     CHG 08/29/85 */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *
	 *SW39 */
	/*   ***** BEGINNING OF -39 SWITCH ***** */
	/*         FUNCTION:  ADJECTIVE SWITCH LOADS ELEMENT TO
	 *         RIGHT OF SWITCH WITH PER, NUM, GEN, CASE OF
	 *         HEAD NOUN (-25). RETURN FROM SCON LOAD RESET
	 *         K3P1 TO SWITCH PARAMETER */
	nsct = elscnpX_.elscnp[im81X_.im81-vbdataX_.k3p1-One];

	/*     K3P1 NOW POINTS TO ADJECTIVE (NON-NOUN) WHICH
	 *     IS TO ASSUME HEAD NOUN'S VALUES */

	sconX_.scon[nsct-One][4-One] = _sw25bkX_->hedgen;
	sconX_.scon[nsct-One][5-One] = _sw25bkX_->hednum;
	sconX_.scon[nsct-One][6-One] = _sw25bkX_->hedper;
	/*10+ */
	if( _opswX_->sw10 == 1 )
		{
		fprintf( _spec_fp, " SW39 \n%6d%6d%6d%6d%6d%6d%6d", vtrn, 
		  vbdataX_.k3p1, _sw25bkX_->hedgen, _sw25bkX_->hednum, _sw25bkX_->hedper, 
		  _sw25bkX_->hedcas, vbdataX_.k3n );
		for( xx=4; xx <= 6; xx++ ){
			fprintf( _spec_fp, "%6d", sconX_.scon[nsct-One][xx-One] );
			}
		fprintf( _spec_fp, "\n" );
		}
	/*10- */

	/*   ***** END OF -39 SWITCH ***** */

	/*SW39 */
	vbdataX_.k3n = vbdataX_.k3 + 2;
	return;
} /*end of function*/

