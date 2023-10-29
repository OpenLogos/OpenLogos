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
	/*   ***** BEGINNING OF -30 SWITCH ***** */
	/*          FUNCTION: TO REPLACE AN SWORK1
	 *          IMPROPERLY SELECTED BY RESOUTION WITH THE
	 *          REJECTED BUT CORRECT SWORK1 (TRAN1 ONLY) */

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



void /*FUNCTION*/ t1sw30()
{
	static short int  x;
	struct  {
		short int im1, i3save, i3str, n6, n6jim, n6jims, phrstr;
		}	*_flowckX_ = (void*)&flowckX_;

	int taddress,dictype;
	static char pgmnam[9] = "T1SW30  ";
	static short iz = 0;
	static short xx = 0;
	static short iz2 = 0;
	static short ret = 0;
	static short k3p2 = 0;

	vbdataX_.k3n = vbdataX_.k3 + 3;
	k3p2 = vtrfX_.vtrf[vbdataX_.k3+2-One];
	_flowckX_->n6jim = im81X_.im81 - vbdataX_.k3p1;

	if( sconX_.scolnk[_flowckX_->n6jim-One] <= elemctX_.origct ){

		/*   1ST PARAMETER IS REL POINTER TO SWORK1
		 *   2ND PARAMETER IS NEW WC TO LOOK FOR */

		/*   CHECK EACH SWORK1 WC AGAINST K3P2; IF EQUAL, CHANGE JS */

		iz2 = 4;
L_4630:
		;
		for( iz=iz2; iz <= 12; iz += 4 ){
			if( swork1X_.swork1[_flowckX_->n6jim-One][iz-One] == 
			  k3p2 )
				goto L_4660;
			}
		goto L_9001;

		/*       IS THE WC ALREADY RESOLVED TO?
		 *       IF YES - FOR GERMAN SOURCE, LEAVE IT
		 *                FOR ENGLISH SOURCE, LOOK FOR ANOTHER */

L_4660:
		xx = iz/4;
		if( xx != prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-One] )
			goto L_4670;
		if( srcflgX_.srcflg != 2 )
			goto L_4710;
		if( iz == 12 )
			goto L_4710;
		iz2 = iz + 4;
		goto L_4630;

L_9001:
		iz = 20;
		goto L_4710;

L_4670:
		taddress = targX_.targ[sconX_.scolnk[_flowckX_->n6jim-One]-One][xx-One];

		if( taddress < 0 ){
			taddress = -taddress;
			dictype = 2;
		}
		else if( taddress != 0 ){
			dictype = 1;
			swork1X_.swork1[_flowckX_->n6jim-One][iz+3-One] = _flowckX_->n6jim;
		}
		errvrsX_.err = TARG_CODES(&trgflgX_.trgflg,&dictype,
			       &taddress,
				   cmpsmcX_.cmpcod[_flowckX_->n6jim-One][xx-One],
				   (short *)&trgcdsX_,diagsX_.longdi, _spec_fp);
		if( errvrsX_.err != 0 ){
			errlog(pgmnam,4675,taddress,1);
			return;
			}

		prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-One] = xx;

		sconX_.scon[_flowckX_->n6jim-One][2-One] = swork1X_.swork1[_flowckX_->n6jim-One][iz+1-One];
		sconX_.scon[_flowckX_->n6jim-One][11-One] = swork1X_.swork1[_flowckX_->n6jim-One][iz+1-One];
		swork1X_.swork1[_flowckX_->n6jim-One][iz+1-One] = ofltagX_.ofl1r[sconX_.scolnk[_flowckX_->n6jim-One]-One][xx-One];
		sconX_.scon[_flowckX_->n6jim-One][13-One] = ofltagX_.ofl4r[sconX_.scolnk[_flowckX_->n6jim-One]-One][xx-One];

		if( ret != 0 ){
			ovc2a3X_.ofl2a[sconX_.scolnk[_flowckX_->n6jim-One]-One] = trgcdsX_.tcov2a;
			ofl2X_.ofl2i[sconX_.scolnk[_flowckX_->n6jim-One]-One] = trgcdsX_.tcov2b;
			sconX_.scon[_flowckX_->n6jim-One][3-One] = trgcdsX_.tcov2b;
			ovc2a3X_.ofl3a[sconX_.scolnk[_flowckX_->n6jim-One]-One] = trgcdsX_.tcov3a;
			sconX_.scon[_flowckX_->n6jim-One][12-One] = trgcdsX_.tcov3b;
			if( ofl2a3X_.ofl3r[sconX_.scolnk[_flowckX_->n6jim-One]-One][xx-One] != 0 )
				sconX_.scon[_flowckX_->n6jim-One][12-One] = ofl2a3X_.ofl3r[sconX_.scolnk[_flowckX_->n6jim-One]-
				  One][xx-One];
			targ25X_.targ25[sconX_.scolnk[_flowckX_->n6jim-One]-One] = trgcdsX_.tcgenm;
			}

		formsaX_.formsv[sconX_.scolnk[_flowckX_->n6jim-One]-One] = swork1X_.swork1[_flowckX_->n6jim-One][iz+2-One];



L_4710:
	if( diagsX_.deepdi == 1 )
		{
		fprintf( _spec_fp, " SW30  %6ld%6d%6d%6d", taddress, 
		  _flowckX_->n6jim, iz, prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-One] );
		for( xx=4; xx <= 15; xx++ ){
			fprintf( _spec_fp, "%6d", swork1X_.swork1[_flowckX_->n6jim-One][xx-One] );
			}
		fprintf( _spec_fp, "%6d\n", targ25X_.targ25[sconX_.scolnk[_flowckX_->n6jim-One]-One] );
		}

	}
	else 	if( diagsX_.longdi != 0){
		fprintf( _spec_fp, "\n  *****  WARNING   IN %8.8s  ******\n     SW29 INVALID FOR ELEMENT# %3d\n     ONLY %3dORIGINAL ELEMENTS IN THE SENTENCE\n     IGNORING THIS SWITCH AND CONTINUING.        \n\n", 
		  pgmnam, _flowckX_->n6jim, elemctX_.origct );
		}

	return;
} /*end of function*/

