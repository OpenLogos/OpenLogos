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
	/*     FUNCTION: TO RE-INITIALIZE CASE, TENSE, PERSON
	 *     NUMBER, AND GENDER AT BEGINNING OF NEW CLAUSE */
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


void /*FUNCTION*/ txsw15()
{
	sw25bkX_.hedper = 3;
	sw25bkX_.adlock = 0;
	sw31bkX_.offlag = 0;
	sw14bkX_.sw14n = 0;
	inhbX_.inhb[136-One] = 0;
	sw25bkX_.hednum = 0;
	sw25bkX_.hedgen = 0;
	sw25bkX_.hedcas = 1;
	sw21bkX_.case_ = 1;
	sw19bkX_.tense = 0;
	sw14bkX_.num = 0;
	sw14bkX_.per = 3;
	sw14bkX_.gen = 0;
	sw14bkX_.case_ = 1;
	sw36bkX_.rel = 0;
	sw36bkX_.relcas = 1;
	sw36bkX_.relnum = 0;
	sw35bkX_.pass = 0;
	sw35bkX_.relpas = 0;
	sw36bkX_.relper = 3;
	sw36bkX_.relgen = 0;
	sw31bkX_.lpflag = 0;
	vbdataX_.k3n = vbdataX_.k3 + 2;
	return;
} /*end of function*/

