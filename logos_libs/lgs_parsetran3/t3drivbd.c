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
	/* LAST CHG 06/09/86 R1530DSD: NEW COMMONS VTRFVL, VTRFBI FOR STRETCH
	 *      CHG 09/30/85 */
	/*      include "c_calls.h" */
	/*     INTEGER*2 SC12V(26),SC12V2(26) */
	/*     COMMON /SC12V/  SC12V
	 *     COMMON /SC12V2/ SC12V2 */
	/*     VTRFM VALS:     NOT DEF, REL PTR, ELEMENT, POS REL PTR, SWORK,
	 *           SLOT, VC,   PUNCT, HI-FREQ CONST, LO-FREQ, SWITCH */
	/*     DATA SC12V/20,40,50,42,52,30,32,21,41,51,43,53,31,33,
	 *    *           60,62,70,72,80,82,61,63,71,73,81,83/
	 *     DATA SC12V2/2,2,2,2,2,2,2,3,3,3,3,3,3,3,4,4,4,4,4,4,
	 *    * 5,5,5,5,5,5/ */
	/*   ENG SRC */
	/*   GER SRC */
	/*     VALUES FOR VTRFM                           06/09/86  *R1530DSD */
	/*     BIASSES TO MARK ELEMENT OR REL PTR IN SLOT OR HF4 */

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
		for(_i=0L; _i < sizeof(hfdm1X_.hfdm1)/sizeof(short); _i++)
			hfdm1X_.hfdm1[_i] = 0;
		jbc3X_.jbc3 = 3;
		{ static struct{ long rc; short ini; } _rs0[] = {
			8,	0,
			8,	0,
			8,	0,
			8,	0,
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
			1,	4,
			1,	14,
			1,	15,
			1,	16,
			3,	0,
			1,	2,
			1,	12,
			6,	0,
			0L, 0 };
		for(_i=_r=0L; _i < sizeof(neg3X_.negwc3)/sizeof(short); _i++)
			((short*)neg3X_.negwc3)[_i] = RC_INI(_rs0); }
		{ static struct{ long rc; short ini; } _rs1[] = {
			8,	0,
			8,	0,
			1,	1,
			1,	5,
			1,	6,
			1,	18,
			4,	0,
			1,	18,
			1,	19,
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
			8,	0,
			1,	2,
			1,	12,
			6,	0,
			0L, 0 };
		for(_i=_r=0L; _i < sizeof(gneg3X_.gngwc3)/sizeof(short); _i++)
			((short*)gneg3X_.gngwc3)[_i] = RC_INI(_rs1); }
		{ static struct{ long rc; char *ini; } _rs2[] = {
			1,	"NP      ",
			1,	"V       ",
			1,	"3       ",
			1,	"4       ",
			1,	"5       ",
			1,	"ADV     ",
			1,	"7       ",
			1,	"8       ",
			1,	"9       ",
			1,	"10      ",
			1,	"11      ",
			1,	"AUX     ",
			1,	"13      ",
			1,	"14      ",
			1,	"15      ",
			1,	"16      ",
			1,	"17      ",
			1,	"INTRG   ",
			1,	"19      ",
			1,	"PUNC    ",
			1,	"N(ADV)  ",
			1,	"N(ADV)  ",
			1,	"N(ADV)  ",
			1,	"N(ADV)  ",
			1,	"N(ADV)  ",
			1,	"N(CONV) ",
			1,	"PREDADJ ",
			1,	"ADJ/ADV ",
			1,	"ADV     ",
			0L, 0 };
		for(_i=_r=0L; _i < 29; _i++)
			memcpy( (char*)wcnameX_.wcname+_i*9, RC_INI(_rs2), 8 ); }
		{ static struct{ long rc; short ini; } _rs3[] = {
			1,	43,
			1,	63,
			1,	73,
			1,	75,
			1,	79,
			1,	53,
			1,	90,
			1,	97,
			1,	98,
			0L, 0 };
		for(_i=_r=0L; _i < sizeof(pars26X_.pars26)/sizeof(short); _i++)
			pars26X_.pars26[_i] = RC_INI(_rs3); }
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

