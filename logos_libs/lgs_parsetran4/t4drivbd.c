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
#include <fcrt.h>
#include "project.h"
#include "logoslib.h"
#include "trans.h"
#include "projexts.h"
#include "parsetrans.h"
#include <string.h>
#include <jbctrl.h>

void /*FUNCTION*/ blk_data()
{
	long int _i, _r;
	static int _aini = 1;
	if( _aini ){ /* Do 1 TIME INITIALIZATIONS! */
		for(_i=0L; _i < sizeof(hfdm1X_.hfdm1)/sizeof(short); _i++)
			hfdm1X_.hfdm1[_i] = 0;
		jbc3X_.jbc3 = 3;
		{ static struct{ long rc; short ini; } _rs0[] = {
			8,	0,
			8,	0,
			8,	0,
			1,	1,
			1,	6,
			6,	0,
			1,	3,
			1,	6,
			1,	19,
			5,	0,
			1,	19,
			1,	20,
			6,	0,
			1,	1,
			1,	2,
			1,	12,
			5,	0,
			1,	1,
			1,	4,
			1,	14,
			1,	15,
			1,	16,
			3,	0,
			1,	2,
			1,	12,
			6,	0,
			0L, 0 };
		for(_i=_r=0L; _i < sizeof(neg4X_.negwc4)/sizeof(short); _i++)
			((short*)neg4X_.negwc4)[_i] = RC_INI(_rs0); }
		{ static struct{ long rc; short ini; } _rs1[] = {
			8,	0,
			8,	0,
			8,	0,
			1,	1,
			1,	6,
			6,	0,
			1,	3,
			1,	6,
			1,	19,
			5,	0,
			8,	0,
			1,	11,
			1,	13,
			1,	19,
			5,	0,
			1,	1,
			1,	4,
			1,	14,
			1,	15,
			1,	16,
			3,	0,
			1,	2,
			1,	3,
			1,	4,
			1,	8,
			1,	12,
			1,	13,
			1,	14,
			1,	16,
			0L, 0 };
		for(_i=_r=0L; _i < sizeof(gneg4X_.gngwc4)/sizeof(short); _i++)
			((short*)gneg4X_.gngwc4)[_i] = RC_INI(_rs1); }
		vtrfvlX_.vtmnd = 'N';
		vtrfvlX_.vtmrp = 'R';
		vtrfvlX_.vtmel = 'E';
		vtrfvlX_.vtmpp = '3';
		vtrfvlX_.vtmwk = '4';
		vtrfvlX_.vtmsl = '5';
		vtrfvlX_.vtmvc = '6';
		vtrfvlX_.vtmpu = '7';
		vtrfvlX_.vtmhi = '8';
		vtrfvlX_.vtmlo = '9';
		vtrfvlX_.vtmsw = 'W';
		vtrfbiX_.slotel = -10100;
		vtrfbiX_.slotrp = -10300;
		_aini = 0;
	}

} /*end of function*/

