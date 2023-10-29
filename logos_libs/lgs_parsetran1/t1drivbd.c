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

EXTERN struct t_vtrfvlX_ {
	byte vtmnd, vtmrp, vtmel, vtmpp, vtmwk, vtmsl, vtmvc, vtmpu, vtmhi, 
	  vtmlo, vtmsw;
	}	vtrfvlX_;
EXTERN struct t_astrkX_ {
	char astrk[3];
	}	astrkX_;
EXTERN struct t_eform1X_ {
	short int eform1[10];
	}	eform1X_;
EXTERN struct t_hf4lX_ {
	short int hf4l;
	}	hf4lX_;
EXTERN struct t_hfdm1X_ {
	short int hfdm1[30];
	}	hfdm1X_;
struct t_nexX_ {
	short int nex[3];
	}	nexX_;
EXTERN struct t_neg1X_ {
	short int negwc1[9][8];
	}	neg1X_;
EXTERN struct t_gneg1X_ {
	short int gngwc1[9][8];
	}	gneg1X_;
EXTERN struct t_onevX_ {
	short int onev[3][4];
	}	onevX_;
EXTERN struct t_pluralX_ {
	short int plural[14];
	}	pluralX_;
EXTERN struct t_s37tabX_ {
	short int s37tab[15];
	}	s37tabX_;
EXTERN struct t_set36X_ {
	short int set36[9];
	}	set36X_;
EXTERN struct t_scomaX_ {
	short int addr, addr1, adr_, ae;
	}	scomaX_;
EXTERN struct t_scommbX_ {
	short int b2, be;
	}	scommbX_;
EXTERN struct t_scommcX_ {
	short int chwc;
	}	scommcX_;
EXTERN struct t_scommdX_ {
	short int dat1;
	}	scommdX_;
EXTERN struct t_form18X_ {
	short int form18[6];
	}	form18X_;
EXTERN struct t_scommgX_ {
	short int g11, g6, g8, gus99, gusn, gustop;
	}	scommgX_;
EXTERN struct t_scommhX_ {
	short int h1, h2;
	}	scommhX_;
EXTERN struct t_scommiX_ {
	short int igus, iz, iz2, iz3, iz4;
	}	scommiX_;
EXTERN struct t_scommjX_ {
	short int j2;
	}	scommjX_;
EXTERN struct t_scommlX_ {
	short int li;
	}	scommlX_;
EXTERN struct t_scommmX_ {
	short int me, ms;
	}	scommmX_;
EXTERN struct t_scommnX_ {
	short int newg, newg11, newg6, newg8, nptp;
	}	scommnX_;
EXTERN struct t_scommoX_ {
	short int oflad;
	}	scommoX_;
EXTERN struct t_scommwX_ {
	short int w1, w2, w3, w4, wc, wcind, wcm1, wcx, we;
	}	scommwX_;
EXTERN struct t_scommxX_ {
	short int xe, xx, xy, xzz;
	}	scommxX_;
EXTERN struct t_scommyX_ {
	short int ye;
	}	scommyX_;
EXTERN struct t_scommzX_ {
	short int ze;
	}	scommzX_;
EXTERN struct t_semadX_ {
	short int l100;
	}	semadX_;
EXTERN struct t_scomfX_ {
	short int fm46;
	}	scomfX_;
EXTERN struct t_scomkX_ {
	short int k3p1, k3p2, k3p3, k3p4;
	}	scomkX_;
EXTERN struct t_scomnX_ {
	short int n4, n6jim, nn4;
	}	scomnX_;
EXTERN struct t_scompX_ {
	short int phrlst, phrstr, prm2;
	}	scompX_;
EXTERN struct t_scomsX_ {
	short int semsct, supm12, supset;
	}	scomsX_;
EXTERN struct t_scomtX_ {
	short int ty46, type3;
	}	scomtX_;
EXTERN struct t_scomvX_ {
	short int vtrc, vtrg, vtrnu, vtrp, vtrpnt;
	}	scomvX_;
EXTERN struct t_scomwX_ {
	short int wc46;
	}	scomwX_;
EXTERN struct t_scomxX_ {
	short int x1;
	}	scomxX_;
EXTERN struct t_scomyX_ {
	short int yy;
	}	scomyX_;
EXTERN struct t_scomzX_ {
	short int zzc, zzg, zznu, zzp, zzpnt;
	}	scomzX_;
 struct t_wcnameX_ {
	char wcname[20][8];
	}	wcnameX_;
 struct t_wcnam1X_ {
	char wcnam1[20][8];
	}	wcnam1X_;
EXTERN struct t_vtrfbiX_ {
	short int slotel, slotrp;
	}	vtrfbiX_;
	/* end of COMMON translations */
void /*FUNCTION*/ blk_data()
{
	long int _i, _r;
	static int _aini = 1;
	if( _aini ){ /* Do 1 TIME INITIALIZATIONS! */
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
		strcpy( astrkX_.astrk, "* " );
		{ static struct{ long rc; short ini; } _rs0[] = {
			1,	7,
			1,	11,
			1,	15,
			0L, 0 };
		for(_i=_r=0L; _i < sizeof(dctX_.dct)/sizeof(short); _i++)
			dctX_.dct[_i] = RC_INI(_rs0); }
		{ static struct{ long rc; short ini; } _rs1[] = {
			1,	4,
			1,	8,
			1,	12,
			0L, 0 };
		for(_i=_r=0L; _i < sizeof(dct2X_.dct2)/sizeof(short); _i++)
			dct2X_.dct2[_i] = RC_INI(_rs1); }
		{ static struct{ long rc; short ini; } _rs2[] = {
			1,	6,
			1,	10,
			1,	14,
			0L, 0 };
		for(_i=_r=0L; _i < sizeof(dct3X_.dct3)/sizeof(short); _i++)
			dct3X_.dct3[_i] = RC_INI(_rs2); }
		{ static struct{ long rc; short ini; } _rs3[] = {
			1,	5,
			1,	9,
			1,	13,
			0L, 0 };
		for(_i=_r=0L; _i < sizeof(dct4X_.dct4)/sizeof(short); _i++)
			dct4X_.dct4[_i] = RC_INI(_rs3); }
		{ static struct{ long rc; short ini; } _rs4[] = {
			1,	2,
			1,	4,
			1,	6,
			0L, 0 };
		for(_i=_r=0L; _i < sizeof(dct5X_.dct5)/sizeof(short); _i++)
			dct5X_.dct5[_i] = RC_INI(_rs4); }
		{ static struct{ long rc; short ini; } _rs5[] = {
			1,	1,
			1,	2,
			1,	4,
			1,	7,
			1,	8,
			1,	13,
			1,	28,
			1,	33,
			1,	38,
			1,	0,
			0L, 0 };
		for(_i=_r=0L; _i < sizeof(eform1X_.eform1)/sizeof(short); _i++)
			eform1X_.eform1[_i] = RC_INI(_rs5); }
		for(_i=0L; _i < sizeof(hfdm1X_.hfdm1)/sizeof(short); _i++)
			hfdm1X_.hfdm1[_i] = 0;
		hf4lX_.hf4l = 0;
		{ static struct{ long rc; short ini; } _rs6[] = {
			1,	20,
			1,	1,
			1,	1,
			1,	0,
			1,	1,
			1,	-1,
			1,	-1,
			1,	0,
			1,	20,
			1,	10,
			1,	1,
			1,	0,
			0L, 0 };
		for(_i=_r=0L; _i < sizeof(onevX_.onev)/sizeof(short); _i++)
			((short*)onevX_.onev)[_i] = RC_INI(_rs6); }
		{ static struct{ long rc; short ini; } _rs7[] = {
			1,	1,
			1,	2,
			1,	4,
			0L, 0 };
		for(_i=_r=0L; _i < sizeof(nexX_.nex)/sizeof(short); _i++)
			nexX_.nex[_i] = RC_INI(_rs7); }
		{ static struct{ long rc; short ini; } _rs8[] = {
			1,	12,
			1,	13,
			1,	14,
			1,	15,
			1,	16,
			1,	18,
			1,	19,
			1,	24,
			1,	26,
			1,	27,
			1,	36,
			1,	57,
			1,	85,
			1,	87,
			0L, 0 };
		for(_i=_r=0L; _i < sizeof(pluralX_.plural)/sizeof(short); _i++)
			pluralX_.plural[_i] = RC_INI(_rs8); }
		{ static struct{ long rc; short ini; } _rs9[] = {
			1,	0,
			1,	15,
			1,	16,
			1,	17,
			1,	5,
			1,	5,
			1,	18,
			1,	11,
			1,	19,
			1,	20,
			1,	21,
			1,	11,
			1,	22,
			1,	23,
			1,	24,
			0L, 0 };
		for(_i=_r=0L; _i < sizeof(s37tabX_.s37tab)/sizeof(short); _i++)
			s37tabX_.s37tab[_i] = RC_INI(_rs9); }
		{ static struct{ long rc; short ini; } _rs10[] = {
			1,	30,
			1,	32,
			1,	34,
			1,	36,
			1,	38,
			1,	40,
			1,	57,
			1,	58,
			1,	59,
			0L, 0 };
		for(_i=_r=0L; _i < sizeof(set36X_.set36)/sizeof(short); _i++)
			set36X_.set36[_i] = RC_INI(_rs10); }
		{ static struct{ long rc; short ini; } _rs15[] = {
			1,	60,
			1,	28,
			1,	3,
			1,	5,
			1,	3,
			1,	4,
			0L, 0 };
		for(_i=_r=0L; _i < sizeof(form18X_.form18)/sizeof(short); _i++)
			form18X_.form18[_i] = RC_INI(_rs15); }
		{ static struct{ long rc; short ini; } _rs16[] = {
			8,	0,
			8,	0,
			8,	0,
			1,	3,
			1,	11,
			1,	12,
			1,	13,
			4,	0,
			1,	3,
			1,	6,
			1,	19,
			5,	0,
			1,	19,
			1,	20,
			6,	0,
			1,	14,
			1,	15,
			1,	16,
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
		for(_i=_r=0L; _i < sizeof(neg1X_.negwc1)/sizeof(short); _i++)
			((short*)neg1X_.negwc1)[_i] = RC_INI(_rs16); }
		{ static struct{ long rc; short ini; } _rs17[] = {
			8,	0,
			8,	0,
			8,	0,
			1,	1,
			1,	4,
			6,	0,
			1,	3,
			1,	6,
			1,	17,
			1,	19,
			4,	0,
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
		for(_i=_r=0L; _i < sizeof(gneg1X_.gngwc1)/sizeof(short); _i++)
			((short*)gneg1X_.gngwc1)[_i] = RC_INI(_rs17); }
		{ static struct{ long rc; short ini; } _rs18[] = {
			1,	1,
			1,	4,
			1,	2,
			1,	5,
			1,	4,
			1,	3,
			1,	3,
			1,	4,
			1,	2,
			1,	1,
			0L, 0 };
		for(_i=_r=0L; _i < sizeof(nwcsemX_.nwcsem)/sizeof(short); _i++)
			nwcsemX_.nwcsem[_i] = RC_INI(_rs18); }
		{ static struct{ long rc; short ini; } _rs19[] = {
			5,	0,
			1,	9,
			1,	11,
			1,	12,
			1,	13,
			1,	0,
			1,	13,
			1,	19,
			3,	0,
			1,	3,
			1,	9,
			1,	11,
			1,	12,
			1,	13,
			1,	3,
			1,	6,
			1,	17,
			1,	19,
			1,	0,
			1,	8,
			1,	20,
			1,	19,
			2,	0,
			1,	13,
			1,	18,
			1,	19,
			2,	0,
			1,	1,
			1,	4,
			1,	14,
			1,	15,
			1,	16,
			1,	12,
			1,	2,
			3,	0,
			5,	0,
			0L, 0 };
		for(_i=_r=0L; _i < sizeof(swcsemX_.wcsem)/sizeof(short); _i++)
			((short*)swcsemX_.wcsem)[_i] = RC_INI(_rs19); }
		scomaX_.addr = 0;
		scommbX_.b2 = 0;
		scommcX_.chwc = 0;
		scommdX_.dat1 = 0;
		scommgX_.g11 = 0;
		scommhX_.h1 = 0;
		scommiX_.igus = 0;
		scommjX_.j2 = 0;
		scommlX_.li = 0;
		scommmX_.me = 0;
		scommnX_.newg = 0;
		scommoX_.oflad = 0;
		scommwX_.w1 = 0;
		scommxX_.xe = 0;
		scommyX_.ye = 0;
		scommzX_.ze = 0;
		semadX_.l100 = 0;
		scomfX_.fm46 = 0;
		scomkX_.k3p1 = 0;
		scomnX_.n4 = 0;
		scompX_.phrlst = 0;
		scomsX_.semsct = 0;
		scomtX_.ty46 = 0;
		scomvX_.vtrc = 0;
		scomwX_.wc46 = 0;
		scomxX_.x1 = 0;
		scomyX_.yy = 0;
		scomzX_.zzc = 0;
		{ static struct{ long rc; char *ini; } _rs20[] = {
			1,	"N       ",
			1,	"V       ",
			1,	"PREF/AV ",
			1,	"ADJ     ",
			1,	"PRON    ",
			1,	"ADV     ",
			1,	"7       ",
			1,	"8       ",
			1,	"9       ",
			1,	"10      ",
			1,	"11      ",
			1,	"AUX     ",
			1,	"PREP    ",
			1,	"DET     ",
			1,	"15      ",
			1,	"ARITH   ",
			1,	"NEG     ",
			1,	"RL/DT/P ",
			1,	"CONJ    ",
			1,	"PUNC    ",
			0L, 0 };
		memcpy(&wcnam1X_,_rs20,sizeof(wcnam1X_)); }
		{ static struct{ long rc; char *ini; } _rs21[] = {
			1,	"N       ",
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
			1,	"DET     ",
			1,	"15      ",
			1,	"16      ",
			1,	"17      ",
			1,	"RL/DT/P ",
			1,	"CONJ    ",
			1,	"PUNC    ",
			0L, 0 };
		memcpy(&wcnameX_,_rs21,sizeof(wcnam1X_)); }
		_aini = 0;
	}

} /*end of function*/

