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
#include <string.h>



void /*FUNCTION*/ blk_data()
{
	long int _i, _r;
	static int _aini = 1;
	if( _aini ){ /* Do 1 TIME INITIALIZATIONS! */
		{ static struct{ long rc; short ini; } _rs0[] = {
			1,	20,
			1,	40,
			1,	50,
			1,	42,
			1,	52,
			1,	30,
			1,	32,
			1,	21,
			1,	41,
			1,	51,
			1,	43,
			1,	53,
			1,	31,
			1,	33,
			1,	60,
			1,	62,
			1,	70,
			1,	72,
			1,	80,
			1,	82,
			1,	61,
			1,	63,
			1,	71,
			1,	73,
			1,	81,
			1,	83,
			0L, 0 };
		for(_i=_r=0L; _i < sizeof(sc12vX_.sc12v)/sizeof(short); _i++)
			sc12vX_.sc12v[_i] = RC_INI(_rs0); }
		{ static struct{ long rc; short ini; } _rs1[] = {
			1,	2,
			1,	2,
			1,	2,
			1,	2,
			1,	2,
			1,	2,
			1,	2,
			1,	3,
			1,	3,
			1,	3,
			1,	3,
			1,	3,
			1,	3,
			1,	3,
			1,	4,
			1,	4,
			1,	4,
			1,	4,
			1,	4,
			1,	4,
			1,	5,
			1,	5,
			1,	5,
			1,	5,
			1,	5,
			1,	5,
			0L, 0 };
		for(_i=_r=0L; _i < sizeof(sc12v2X_.sc12v2)/sizeof(short); _i++)
			sc12v2X_.sc12v2[_i] = RC_INI(_rs1); }
		for(_i=0L; _i < sizeof(hfdm1X_.hfdm1)/sizeof(short); _i++)
			hfdm1X_.hfdm1[_i] = 0;
		{ static struct{ long rc; short ini; } _rs2[] = {
			8,	0,
			8,	0,
			8,	0,
			1,	19,
			1,	20,
			6,	0,
			1,	3,
			1,	6,
			1,	19,
			5,	0,
			1,	19,
			1,	20,
			6,	0,
			1,	11,
			1,	13,
			1,	19,
			5,	0,
			1,	1,
			1,	14,
			1,	15,
			1,	16,
			4,	0,
			1,	2,
			1,	12,
			6,	0,
			0L, 0 };
		for(_i=_r=0L; _i < sizeof(neg2X_.negwc2)/sizeof(short); _i++)
			((short*)neg2X_.negwc2)[_i] = RC_INI(_rs2); }
		{ static struct{ long rc; short ini; } _rs3[] = {
			8,	0,
			8,	0,
			8,	0,
			8,	0,
			8,	0,
			1,	19,
			1,	20,
			6,	0,
			1,	11,
			1,	13,
			1,	19,
			5,	0,
			8,	0,
			1,	2,
			1,	6,
			1,	12,
			5,	0,
			0L, 0 };
		for(_i=_r=0L; _i < sizeof(gneg2X_.gngwc2)/sizeof(short); _i++)
			((short*)gneg2X_.gngwc2)[_i] = RC_INI(_rs3); }
		jbc3X_.jbc3 = 3;

		{ static struct{ long rc; char *ini; } _rs4[] = {
			1,	"NP      ",
			1,	"V       ",
			1,	"PREF    ",
			1,	"4       ",
			1,	"5       ",
			1,	"ADV     ",
			1,	"7       ",
			1,	"8       ",
			1,	"9       ",
			1,	"10      ",
			1,	"11      ",
			1,	"AUX     ",
			1,	"PREP    ",
			1,	"14      ",
			1,	"15      ",
			1,	"16      ",
			1,	"17      ",
			1,	"INTRG   ",
			1,	"CONJ    ",
			1,	"PUNC    ",
			0L, 0 };
		for(_i=_r=0L; _i < 20; _i++)
			memcpy ( (char*)wcnameX_.wcname+_i*9, RC_INI(_rs4), 8 ); }

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

	/* LAST CHG 06/21/86 R1530DSD: NEW COMMONS VTRFVL, VTRFBI FOR STRETCH
	 *      CHG 09/30/85 */
	/*      include "c_calls.h" */
	/*     INTEGER*2 ZERO */
	/*     COMMON /ZERO/   ZERO */
	/*     VTRFM VALS:     NOT DEF, REL PTR, ELEMENT, POS REL PTR, SWORK,
	 *           SLOT, VC,   PUNCT, HI-FREQ CONST, LO-FREQ, SWITCH */
	/*   ENG SRC */
	/*   GER SRC */
	/*     DATA ZERO/0/ */
	/*     VALUES FOR VTRFM                           06/09/86  *R1530DSD */
	/*     BIASSES TO MARK ELEMENT OR REL PTR IN SLOT OR HF4 */
} /*end of function*/

