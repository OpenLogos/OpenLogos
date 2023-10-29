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
	/*      FUNCTION : TO SET OR RESET CASE FLAG; RELCAS */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <fcrt.h>
#include "logoslib.h"
#include "trans.h"
#include "projexts.h"
#include <jbctrl.h>
#include "parsetrans_ext.h"


void /*FUNCTION*/ txsw12()
{
	static char pgmnam[9] = "TxSW12  ";
	static short nsct = 0;
	static short vtrp = 0;
	static short twelve = 0;

	twelve = 0;
	if( vbdataX_.k3p1 >= 1 ){
		twelve = 1;
		if( sw36bkX_.rel == 0 )
			if( tranidX_.tranid == 4 )
				sw14bkX_.case_ = vbdataX_.k3p1; 
			else
				sw21bkX_.case_ = vbdataX_.k3p1; 
		if( sw36bkX_.rel == 1 )
			sw36bkX_.relcas = vbdataX_.k3p1;

		}
	else if( vbdataX_.k3p1 == 0 ){

		vtrnX_.vtrn = vtrfX_.vtrf[vbdataX_.k3+2-One];

		/* GET SWORK ITEM FOLLOWING PARMETER OF -12 SWITCH */

		vbdataX_.k3p1 = vtrfX_.vtrf[vbdataX_.k3+3-One];
		vbdataX_.k3n = vbdataX_.k3 + 4;

		/* PREPARE ALTERNATE WORD CLASS REPRESENTED BY K3 + 3 */

		elemld();
		if( errvrsX_.errlvl != 0 )
			return;

		vtrp = vtrfX_.vtrf[vbdataX_.k3+1-One];

		/* BELOW,NEXT,SET CASE TO SCON(OFL2) OF ELEMENT TO RIGHT FO SWITCH PARMET
		 *     IF  EL    52          =
		 *+                                                        *RCASE*GBA */
		if( srcflgX_.srcflg == 1 ){
			if( sw36bkX_.rel == 0 )
				if( tranidX_.tranid == 4 )
					sw14bkX_.case_ = formsaX_.formsv[sconX_.scolnk[sworkX_.phrhed[flowckX_.n6jim-One]-One]-One];
				else
					sw21bkX_.case_ = formsaX_.formsv[sconX_.scolnk[sworkX_.phrhed[flowckX_.n6jim-One]-One]-One];
			if( sw36bkX_.rel == 1 )
				sw36bkX_.relcas = formsaX_.formsv[sconX_.scolnk[sworkX_.phrhed[flowckX_.n6jim-One]-One]-One];
			}
		else{
			if( sw36bkX_.rel == 0 )
				if( tranidX_.tranid == 4 )
					sw14bkX_.case_ = sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][3-One];
				else
					sw21bkX_.case_ = sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][3-One];
			if( sw36bkX_.rel == 1 )
				sw36bkX_.relcas = sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][3-One];
			}

		if( diagsX_.deepdi == 1 )
			{
			fprintf( _spec_fp, " SW12 %6d%6d%6d%6d%6d\n", vtrnX_.vtrn, 
			  vbdataX_.k3p1, vtrp, sw21bkX_.case_, sw36bkX_.relcas );
			}

		if( twelve != 1 )
			return;
		}
	else{
		twelve = 1;
		if( tranidX_.tranid == 4 )
			nsct = sworkX_.phrhed[flowckX_.im1-vbdataX_.k3p1-80-One];
		else
			nsct = sworkX_.phrhed[im81X_.im81-vbdataX_.k3p1-One];
		/*+                                                        *RCASE*GBA
		 *     IF (REL .EQ. 0) CASE = SCON(3,NSCT)
		 *     IF (REL .GT. 0) RELCAS = SCON(3,NSCT) */
		if( srcflgX_.srcflg == 1 ){
			if( sw36bkX_.rel == 0 )
				if( tranidX_.tranid == 4 )
					sw14bkX_.case_ = formsaX_.formsv[sconX_.scolnk[nsct-One]-One];
				else
					sw21bkX_.case_ = formsaX_.formsv[sconX_.scolnk[nsct-One]-One];
			if( sw36bkX_.rel > 0 )
				sw36bkX_.relcas = formsaX_.formsv[sconX_.scolnk[nsct-One]-One];
			}
		else{
			if( sw36bkX_.rel == 0 )
				if( tranidX_.tranid == 4 )
					sw14bkX_.case_ = sconX_.scon[nsct-One][3-One];
				else
					sw21bkX_.case_ = sconX_.scon[nsct-One][3-One];
			if( sw36bkX_.rel > 0 )
				sw36bkX_.relcas = sconX_.scon[nsct-One][3-One];
			}
		}

	vbdataX_.k3n = vbdataX_.k3 + 2;
	return;

} /*end of function*/

