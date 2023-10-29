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
	/*     FUNCTION: THREE PARAMETERS: GENDER, NUMBER, PERSON.
	 *     INSTRUCTS SYSTEM TO ESTABLISH NO, GEN, PERSON.
	 *     IF ANY PARAMETER IS NEG. IT IS A POINTER TO SCON OF
	 *     PHRASE BEING USED. SYSTEM FLAGS WILL BE SET. */
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


void /*FUNCTION*/ txsw14()
{
	static short vtrp = 0;
	static short vtrq = 0;
	static short vtrr = 0;

	vbdataX_.k3n = vbdataX_.k3 + 4;

	/*     IN TRAN2 NO SCON IS LOADED: OPADR2 WILL BE LOADED
	 *     VIA NORMAL ELEMENT PROCESSING IN LOOP. */

	if( !((sw36bkX_.rel > 1) && (sw36bkX_.relcas == 1)) ){
		if( (sw21bkX_.case_ != 1) || (sw36bkX_.rel != 0) )
			goto L_1880;
		}

	vtrp = vtrfX_.vtrf[vbdataX_.k3+1-One];
	vtrq = vtrfX_.vtrf[vbdataX_.k3+2-One];
	vtrr = vtrfX_.vtrf[vbdataX_.k3+3-One];
	sw14bkX_.sw14n = 1;

	/*     CHECK EQ. PARAMETER AND CURRENT SETTING OF REL. FLAG */
	if( (vtrp > 0) && (sw36bkX_.rel == 0) )
		sw14bkX_.gen = vtrp;
	if( (vtrq > 0) && (sw36bkX_.rel == 0) )
		sw14bkX_.num = vtrq;
	if( (vtrr > 0) && (sw36bkX_.rel == 0) )
		sw14bkX_.per = vtrr;
	if( (vtrp > 0) && (sw36bkX_.rel == 1) )
		sw36bkX_.relgen = vtrp;
	if( (vtrq > 0) && (sw36bkX_.rel == 1) )
		sw36bkX_.relnum = vtrq;
	if( (vtrr > 0) && (sw36bkX_.rel == 1) )
		sw36bkX_.relper = vtrr;

	/*     END OF PARAMETERS POSITIVE CHECK
	 *     PHRASE HEAD OF ELEMENT ON RIGHT
	 *     SUBTRACT 80 FROM REL. POINTERS */

	if( (vtrp < 0) && (sw36bkX_.rel == 0) )
		sw14bkX_.gen = sconX_.scon[sworkX_.phrhed[flowckX_.im1-vtrp-80-One]-One][4-One];

	if( (vtrq < 0) && (sw36bkX_.rel == 0) )
		sw14bkX_.num = sconX_.scon[sworkX_.phrhed[flowckX_.im1-vtrq-80-One]-One][5-One];

	if( (vtrr < 0) && (sw36bkX_.rel == 0) )
		sw14bkX_.per = sconX_.scon[sworkX_.phrhed[flowckX_.im1-vtrr-80-One]-One][6-One];

	if( (vtrp < 0) && (sw36bkX_.rel == 1) )
		sw36bkX_.relgen = sconX_.scon[sworkX_.phrhed[flowckX_.im1-vtrp-80-One]-One][4-One];

	if( (vtrq < 0) && (sw36bkX_.rel == 1) )
		sw36bkX_.relnum = sconX_.scon[sworkX_.phrhed[flowckX_.im1-vtrq-80-One]-One][5-One];

	if( (vtrr < 0) && (sw36bkX_.rel == 1) )
		sw36bkX_.relper = sconX_.scon[sworkX_.phrhed[flowckX_.im1-vtrr-80-One]-One][6-One];

	/*     PARAMETERS ARE RELATIONAL POINTERS TO SCONS
	 *     SWITCH PLUS THREE PARAMETERS PLUS ELEMENT & ITS W/C PARM. */

L_1880:
	if( diagsX_.deepdi == 1 )
		{
		fprintf( _spec_fp, " SW14 %6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d\n", 
		  vbdataX_.k3n, vtrnX_.vtrn, vbdataX_.k3p1, vtrp, vtrq, 
		  vtrr, sw14bkX_.gen, sw14bkX_.num, sw14bkX_.per, sw36bkX_.relgen, 
		  sw36bkX_.relnum, sw36bkX_.relper, sw21bkX_.case_, sw36bkX_.relcas, 
		  sw36bkX_.rel );
		}
	return;
} /*end of function*/

