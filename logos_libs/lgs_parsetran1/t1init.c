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
	/*3333
	 *   CHANGES:
	 *      11/05/91 JAL  INIT SELECTIVELY IF 2 PASS STRATEGY IS ACTIVE
	 *                    AND THIS IS 2ND PASS.
	 *      10/03/91 JAL  INIT SCOLNK FOR ALL PHCT ELEMENTS.
	 *      03/22/91 JAL   ADD FLAG FOR LONG AND DEEP DIAGNOSTICS.
	 *      05/07/90 JAL:  ADD SAVSW TO /OPSW/ ADD ALLOW DIAGNOSTICS
	 *                     ON A RANGE OF INPUT LINES. PASSED IN JCDLIN().
	 *      04/17/87 *R1685RKH*  CHANGE T1-4 SWORK1 LIMIT FROM 50 TO
	 *      10/16/86 *R1539DSD: CLEAR 100 CELLS AT START OF TRAN1 ONLY
	 *      08/17/86 *R1561DSD: 100 SCONS */
	/*     CALLED BY T1DRIVE AT THE BEGINNING OF THE PROCESSING OF EACH SENTE
	 *     INITIALIZES THE VARIABLES USED BY ALL OF THE SUBROUTINES WHICH PRO
	 *     THE SENTENCE. */
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

#define	HF4LIM	70
EXTERN struct t_hf4lX_ {
	short int hf4l;
	}	hf4lX_;
EXTERN struct t_hfdm1X_ {
	short int hfdm1[30];
	}	hfdm1X_;
EXTERN struct t_nexX_ {
	short int nex[3];
	}	nexX_;
EXTERN struct t_head26X_ {
	short int headwc, headty, headfr, headhd, headt2, headt3;
	}	head26X_;
EXTERN struct t_loopckX_ {
	short int imatch[ELMMAX], noloop[21], nptpsv, nptpx, call36[2];
	}	loopckX_;
EXTERN struct t_sploopX_ {
	short int st16, li;
	}	sploopX_;
EXTERN struct t_sw14bkX_ {
	short int gen, num, per;
	}	sw14bkX_;
EXTERN struct t_sw26nX_ {
	short int sw26n;
	}	sw26nX_;
EXTERN struct t_sw36bkX_ {
	short int det, rel, relcas, relgen, relnum, relper;
	}	sw36bkX_;
EXTERN struct t_sw13bkX_ {
	short int loc25, preloc;
	}	sw13bkX_;
EXTERN struct t_nloop2X_ {
	short int nloop2[21];
	}	nloop2X_;
EXTERN struct t_sw11bkX_ {
	short int cn[30], hf4[HF4LIM][100], hf4ct, hf4m1[HF4LIM], slot[30][160], 
	  sltflg, vtrdon;
	}	sw11bkX_;
EXTERN struct t_sw31bkX_ {
	short int artd, artind, dative, fakev, inhb43, maktrn, negflg, 
	  subcl;
	}	sw31bkX_;
EXTERN struct t_sw35bkX_ {
	short int pass, relpas;
	}	sw35bkX_;
EXTERN struct t_sw37bkX_ {
	short int arith;
	}	sw37bkX_;
EXTERN struct t_sw38bkX_ {
	short int adjnp, sw38n, vnum;
	}	sw38bkX_;


void /*FUNCTION*/ init()
{
	static short int tmp;
		/*pointer COMMON transls*/
	struct  {
		short int k7, oflad, n3;
		}	*_diacbX_ = (void*)&diacbX_;
	struct  {
		short int i, wc42m, wc50m, iz4, swx, s11prt;
		}	*_vwarg2X_ = (void*)&vwarg2X_;
	struct  {
		short int im1, i3save, i3str, n6, n6jim, n6jims, phrstr;
		}	*_flowckX_ = (void*)&flowckX_;
	struct  {
		short int k7m, minifg;
		}	*_minickX_ = (void*)&minickX_;
	struct  {
		short int hedcas, hedgen, hednum, hedper, hedsc3, ngen, nnum, 
		  relngn, relnnm, relnpr, sw25n, tw25, nper;
		}	*_sw25bkX_ = (void*)&sw25bkX_;
	struct  {
		short int pcb, pcbwc;
		}	*_sw23bkX_ = (void*)&sw23bkX_;
		/*end of pointer COMMON transls*/
	static short zero = 0;
	static short ij = 0;
	static short xx = 0;
	static short ecm1 = 0;
	static byte blank = ' ';


	/*                 INIT SCON ARRAY LINK LIST
	 *                 IT IS ASSUMED THE 1ST N ORIGINAL ELEMENTS ARE LOADED
	 *                 IN THE 1ST N LOCATIONS.  (N=PHCT) */
	for( tmp=1; tmp <= swork1X_.phct; tmp++ ){
		sconX_.scolnk[tmp-One] = tmp;
		}
	zapit(&sconX_.scolnk[swork1X_.phct+1-One],(SCONY-swork1X_.phct)* 2,(byte)0);

	/*                         SET ELEMENT COUNTERS */
	elemctX_.origct = swork1X_.phct;
	elemctX_.elemct = swork1X_.phct;
	elemctX_.sntbrk = 0;

	/*+   NEW FOR VTRBRNCH PROGRAM: VBCELL IS THE 200 BYTE ARRAY WHICH
	 *    CONTAINS 100 2 BYTE 'CELLS' USED BY THE -55,-56,-57 SWITCHES.
	 *    INITIALIZATION TAKES PLACE AS FOLLOWS:
	 *          CELLS   1-10   CLEAR FOR EACH VTR
	 *          CELLS  11-60   CLEAR FOR EACH TRAN
	 *          CELLS  61-100  CLEAR AT BEGINNING OF TRAN 1 ONLY
	 *          CELLS 101-200  EXTRA-SENTENTIAL: CLEAR TIME TO BE DEFINED
	 *     CLEAR 100 CELLS IN TRAN1 ONLY              10/16/86  *R1539
	 *+              separate parse/Tran passes strategy   jal 1/1/97 */
	memset(vbdataX_.vbcell,0,sizeof(vbdataX_.vbcell));
	/*C	   dont init Cells 61-100 at beginning of 2nd pass
	 *      IF (PASSFL.EQ.1 .AND. PASSCT.EQ.2) THEN
	 *          CALL ZAPIT (VBCELL, 120, ZERO)
	 *      ELSE
	 *          CALL ZAPIT (VBCELL, 200, ZERO)
	 *      ENDIF
	 *-                                                    jal 1/1/97 */
	if( jbctrlX_.jcimpe == 1 )
		vbdataX_.vbcell[100-One] = 1;
	zapit(inhbX_.inhb,2000,(byte)zero);
	zapit(hpdopoX_.hfdopo,OPADRX*2,(byte)zero);
	zapit((short*)hfdoaX_.hfpoad,HFPADX*HFPADY*2,(byte)zero);
	zapit((short*)hfdoaX_.sconhf,HFPADX*HFPADY*2,(byte)zero);
	zapit((short*)sw44bkX_.ad44vc,20,(byte)zero);
	zapit(loopckX_.imatch,ELMMAX*2,(byte)zero);
	zapit(loopckX_.noloop,42,(byte)zero);
	zapit(nloop2X_.nloop2,42,(byte)zero);
	memset(opadroX_.company_code,' ',sizeof(opadroX_.company_code));
	_minickX_->minifg = 0;
	prtscoX_.scterr = 0;
	hfdoaX_.adct = 0;
	sw38bkX_.adjnp = 0;
	sw33bkX_.altwc = 0;
	sw31bkX_.artind = 0;
	sw31bkX_.artd = 0;
	sw37bkX_.arith = 0;
	zapit(loopckX_.call36,4,(byte)zero);
	case_X_.case_ = 1;
	sw36bkX_.det = 0;
	sw31bkX_.dative = 0;
	sw31bkX_.fakev = 0;
	sw14bkX_.gen = 0;
	head26X_.headwc = 0;
	head26X_.headty = 0;
	head26X_.headfr = 0;
	head26X_.headhd = 0;
	head26X_.headt2 = 0;
	head26X_.headt3 = 0;
	_sw25bkX_->hedcas = 1;
	_sw25bkX_->hedgen = 0;
	_sw25bkX_->hednum = 0;
	_sw25bkX_->hedper = 3;
	_sw25bkX_->hedsc3 = 0;
	sw11bkX_.hf4ct = 0;
	hf4lX_.hf4l = 0;
	_vwarg2X_->i = 0;
	_flowckX_->i3save = 1;
	inhbX_.inhb[146-One] = 1;
	inhbX_.inhb[160-One] = 1;
	inhbX_.inhb[579-One] = 1;
	_minickX_->k7m = 0;
	sploopX_.li = 0;
	sw13bkX_.loc25 = 0;
	sw31bkX_.maktrn = 0;
	supresX_.n3up = 1;
	sw31bkX_.negflg = 0;
	_sw25bkX_->ngen = 0;
	_sw25bkX_->nnum = 0;
	_sw25bkX_->nper = 0;
	sw14bkX_.num = 0;
	sw35bkX_.pass = 0;
	_sw23bkX_->pcb = 0;
	sw14bkX_.per = 3;
	sworkoX_.phcto = 0;
	supresX_.phcts = 0;
	sw26bkX_.phr26 = 0;
	sw13bkX_.preloc = 0;
	sw44bkX_.pre44 = 0;
	sw44bkX_.prevc = 0;
	sw36bkX_.rel = 0;
	sw36bkX_.relcas = 1;
	sw36bkX_.relgen = 0;
	sw36bkX_.relnum = 0;
	sw35bkX_.relpas = 0;
	sw36bkX_.relper = 3;
	case_X_.semc14 = 0;
	case_X_.semcas = 0;
	sw44bkX_.slot44 = 0;
	sw11bkX_.sltflg = 0;
	sw31bkX_.subcl = 0;
	sw21bkX_.sw21n = 0;
	_sw25bkX_->sw25n = 0;
	sw26nX_.sw26n = 0;
	sw38bkX_.sw38n = 0;
	sw3146X_.sw3146 = 0;
	sw19bkX_.tense = 0;
	sw21bkX_.case_ = 0;
	sw38bkX_.vnum = 0;
	w50valX_.i3 = 1;
	flowckX_.i3save = 1;
	for( _vwarg2X_->i=1; _vwarg2X_->i <= 30; _vwarg2X_->i++ ){
		sw11bkX_.cn[_vwarg2X_->i-One] = 1;
		hfdm1X_.hfdm1[_vwarg2X_->i-One] = -1;
		}

	/*+              separate parse/Tran passes strategy   jal 1/1/97
	 *                      IF PASS2 OF 2 PASS STRATEGY, THEN  FORMSV AND
	 *                      TYPSAV COME FROM PASS1,  SWORK1 COMES FROM
	 *                      SAVED VERSION, AND SWITCHES ALREADY SET. */

	/*      IF (PASSFL.EQ.1 .AND. PASSCT.EQ.2) GO TO 300
	 *-              separate parse/Tran passes strategy   jal 1/1/97 */


	/* 		for 2nd of 2 pass mode get the POS selections from PARSE */
	if( passesX_.passfl == 1 && passesX_.passct == 2 )
		getprs(1);

	/* -----------------------------------------------------------------
	 *        SET TYPES BACK TO ORIGINAL DICTIONARY DATA
	 *        ALSO SAVES FORM FOR PLURAL TESTS IN SW25N + SW38N */

	memset(typsvX_.typsav,0,sizeof(typsvX_.typsav));
	
	for( ij=1; ij <= ELMMAX; ij++ ){
		elscnpX_.elscnp[ij-One] = ij;
		}

	for( ij=1; ij <= swork1X_.phct; ij++ ){

		for( prctjX_.j=1; prctjX_.j <= 3; prctjX_.j++ ){
			if( swork1X_.swork1[ij-One][prctjX_.j-One] > 0 )
				goto L_60;
			}

		prctjX_.j = 1;

L_60:
		prctX_.js[sconX_.scolnk[ij-One]-One] = prctjX_.j;

		formsaX_.formsv[ij-One] = 0;
		formsaX_.formsv[ij-One] = swork1X_.swork1[ij-One][dct3X_.dct3[prctjX_.j-One]-One];

		/*        TYPE CODES ARE STORED ACCORDING TO RANGE;
		 *        SUPERSETS (1-16)    ARE STORED IN SCON 13
		 *        SETS      (17-99)   ARE STORED IN SCON 11
		 *        SUBSETS   (100-999) ARE STORED IN SCON 2
		 *        N.B. WC'S WHICH DON'T USE SUBSETS DUPLICATE THE SET OR SUPERSET */

		if( ofltagX_.ofl1r[sconX_.scolnk[ij-One]-One][prctjX_.j-One] != 0 ){
			typsvX_.typsav[ij-One] = swork1X_.swork1[ij-One][dct4X_.dct4[prctjX_.j-One]-One];
			swork1X_.swork1[ij-One][dct4X_.dct4[prctjX_.j-One]-One] = ofltagX_.ofl1r[sconX_.scolnk[ij-One]-
			  One][prctjX_.j-One];
			}

		}
	/*--------------------------------------------------------------------- */


	if( swork1X_.swork1[swork1X_.phct-One][dct3X_.dct3[prctX_.js[sconX_.scolnk[swork1X_.phct-One]-
	  One]-One]-One] == 9 )
		swork1X_.swork1[1-One][dct4X_.dct4[prctX_.js[sconX_.scolnk[1-One]-
		  One]-One]-One] = 909;

	/*        RESET SWORK1(1 OR 2 OR 3) FOR POSSIBLE ERRORS IN RES2 */

	ecm1 = swork1X_.phct - 1;
	for( _vwarg2X_->i=1; _vwarg2X_->i <= ecm1; _vwarg2X_->i++ ){
		swork1X_.swork1[_vwarg2X_->i-One][prctX_.js[sconX_.scolnk[_vwarg2X_->i-One]-One]-One] 
			                  = nexX_.nex[prctX_.js[sconX_.scolnk[_vwarg2X_->i+1-One]-One]-One];
		}



	/*----------------------- INTIALIZE PHRASE INFORMATION ------------------ */

	_diacbX_->n3 = 0;
	supresX_.phsup = 0;
	sworkoX_.phrbgo[1-One] = 1;
	sworkoX_.phrhdo[1-One] = 0;
	_flowckX_->phrstr = 1;

	/*        N6JIMS IS THE LAST PHRASE LOADED */
	_flowckX_->n6jims = 0;

	//              init NP for WordSearch
	npinit();

	return;
} /*end of function*/

