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
	/* CHANGES:
	 *     Log 2125 - 9/19/94 jal: remove NNUM,NGEN,NPER
	 *     4/1/93 jal:  if pass1 of 2pass, set SWRK1I if setting SWORKO for
	 *                  a possible clause boundary (see -23 strategy).
	 *     LAST CHG: 04/23/87 *R1679RKH*  OFL3B conversion & R1685 Swork limi
	 *      CHG 08/17/86 *R1561DSD: 100 SCONS
	 *     CHG 08/29/85 */
	/*   ***** BEGINNING OF -21 SWITCH ***** */
	/*         FUNCTION: A VERB SWITCH. LOADS SCON WITH -25 SW. DATA
	 *         ESTABLISHES TENSE. RELATES TO SWORK1 TO RIGHT OF SWITCH. */

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


EXTERN struct t_sw14bkX_ {
	short int gen, num, per;
	}	sw14bkX_;
EXTERN struct t_sw36bkX_ {
	short int det, rel, relcas, relgen, relnum, relper;
	}	sw36bkX_;
EXTERN struct t_sw31bkX_ {
	short int artd, artind, dative, fakev, inhb43, maktrn, negflg, 
	  subcl;
	}	sw31bkX_;
EXTERN struct t_sw35bkX_ {
	short int pass, relpas;
	}	sw35bkX_;
EXTERN struct t_sw38bkX_ {
	short int adjnp, sw38n, vnum;
	}	sw38bkX_;
	/* end of COMMON translations */
void /*FUNCTION*/ t1sw21()
{
	static short int skipit;
	struct  {
		short int k7, oflad, n3;
		}	*_diacbX_ = (void*)&diacbX_;
	struct  {
		short int vtr[26];
		}	*_vtrX_ = (void*)&vtrX_;
	struct  {
		short int im1, i3save, i3str, n6, n6jim, n6jims, phrstr;
		}	*_flowckX_ = (void*)&flowckX_;
	struct  {
		short int hedcas, hedgen, hednum, hedper, hedsc3, ngen, nnum, 
		  relngn, relnnm, relnpr, sw25n, tw25, nper;
		}	*_sw25bkX_ = (void*)&sw25bkX_;
	struct  {
		short int pcb, pcbwc, pcbswk;
		}	*_sw23bkX_ = (void*)&sw23bkX_;
		/*end of pointer COMMON transls*/
	static char pgmnam[9] = "T1SW21  ";
	static short iz = 0;
	static short xx = 0;
	static short formx = 0;
	static short sconlc = 0;

	vtrnX_.vtrn = vtrfX_.vtrf[vbdataX_.k3+2-One];
	vbdataX_.k3n = vbdataX_.k3 + 4;
	sw21bkX_.sw21n = 1;
	vbdataX_.k3p1 = vtrfX_.vtrf[vbdataX_.k3+3-One];
	inhbX_.inhb[146-One] = 1;

	/*   CREATE A SCON RECORD FOR THIS ELEMENT.  SSAVE IS FOR COMPUTED
	 *   GOTO WHICH IS THE RETURN FROM THE SCON LOAD. */

	elemld();
	if( errvrsX_.errlvl == 0 ){

		/*   LOAD SCON WITH FLAGS OR REL FLAGS */

		sconlc = _flowckX_->n6jim;
		if( sw36bkX_.rel == 2 )
			sw36bkX_.rel = 0;
		if( sw36bkX_.rel > 0 ){

			if( sw36bkX_.relgen != 0 ){
				sconX_.scon[sconlc-One][4-One] = sw36bkX_.relgen;
				sw36bkX_.relgen = 0;

				}
			else if( _sw25bkX_->relngn != 0 ){
				sconX_.scon[sconlc-One][4-One] = _sw25bkX_->relngn;
				_sw25bkX_->relngn = 0;
				}

			if( sw36bkX_.relnum != 0 ){
				sconX_.scon[sconlc-One][5-One] = sw36bkX_.relnum;
				sw36bkX_.relnum = 0;

				}
			else if( _sw25bkX_->relnnm != 0 ){
				sconX_.scon[sconlc-One][5-One] = _sw25bkX_->relnnm;
				_sw25bkX_->relnnm = 0;
				}

			if( sw36bkX_.relper != 0 ){
				sconX_.scon[sconlc-One][6-One] = sw36bkX_.relper;
				sw36bkX_.relper = 0;

				}
			else if( _sw25bkX_->relnpr != 0 ){
				sconX_.scon[sconlc-One][6-One] = _sw25bkX_->relnpr;
				_sw25bkX_->relnpr = 0;
				}

			if( sw35bkX_.relpas != 0 ){
				sw35bkX_.relpas = 0;

				/*   IF PASSIVE, VERB NO LONGER GOVERNS CASE */

				}
			else if( srcflgX_.srcflg != 1 ){
				sw36bkX_.relcas = sconX_.scon[sconlc-One][3-One];
				/*+                                                         *R0745MBS */
				if( sw36bkX_.relcas >= 5 && sw36bkX_.relcas <= 8 )
					sw36bkX_.relcas -= 4;
				}
			}
		else{
			/*                set flag to skip SCON setting from NGEN,NNUM, NPER setting */
			skipit = 0;
			if( srcflgX_.srcflg == 2 )
				skipit = 1;


			if( sw14bkX_.gen != 0 ){
				sconX_.scon[sconlc-One][4-One] = sw14bkX_.gen;
				sw14bkX_.gen = 0;

				}
			else if( !(_sw25bkX_->ngen == 0 || skipit == 1) ){
				sconX_.scon[sconlc-One][4-One] = _sw25bkX_->ngen;
				_sw25bkX_->ngen = 0;
				}


			if( sw38bkX_.vnum != 0 ){
				sconX_.scon[sconlc-One][5-One] = sw38bkX_.vnum;
				sw38bkX_.vnum = 0;

				}
			else if( sw14bkX_.num != 0 ){
				sconX_.scon[sconlc-One][5-One] = sw14bkX_.num;
				sw14bkX_.num = 0;

				}
			else if( !(_sw25bkX_->nnum == 0 || skipit == 1) ){
				sconX_.scon[sconlc-One][5-One] = _sw25bkX_->nnum;
				_sw25bkX_->nnum = 0;
				}


			if( sw14bkX_.per != 0 ){
				sconX_.scon[sconlc-One][6-One] = sw14bkX_.per;
				sw14bkX_.per = 0;

				}
			else if( !(_sw25bkX_->nper == 0 || skipit == 1) ){
				sconX_.scon[sconlc-One][6-One] = _sw25bkX_->nper;
				_sw25bkX_->nper = 0;
				}


			if( sw35bkX_.pass != 0 ){
				sw35bkX_.pass = 0;
				/*   IF PASSIVE FLAG ON, VERB NO LONGER GOVERNS CASE */
				}
			else if( srcflgX_.srcflg != 1 ){
				case_X_.case_ = sconX_.scon[sconlc-One][3-One];
				if( case_X_.case_ >= 5 && case_X_.case_ <= 8 )
					case_X_.case_ -= 4;
				}
			}

		if( trgflgX_.trgflg == 2 ){
			if( !(vtrfX_.vtrf[vbdataX_.k3+1-One] != 2 &&
				  vtrfX_.vtrf[vbdataX_.k3+1-One] != 3) ){
				/*+ OFL3B conversion & R1685 Swork limit Change RKH  04/23/87   R1679 */
				if( sconX_.scon[sconlc-One][12-One] != 6 ){

					for( iz=_flowckX_->phrstr; iz <= _diacbX_->n3; iz++ ){
						if( opadroX_.opadro[iz-One] == -104 )
							goto L_3364;
						}
					goto L_3368;

L_3364:
					if( hpdopoX_.hfdopo[iz-One] != 0 ){
						sconX_.scon[sconlc-One][7-One] = 1;
						goto L_3369;
						}
					}
				}
			}

L_3368:
		sconX_.scon[sconlc-One][7-One] = vtrfX_.vtrf[vbdataX_.k3+1-One];

L_3369:
		if( srcflgX_.srcflg == 1 ){

			/*   CHANGE SCON(5) ACCORDING TO THE FORM */
			formx = swork1X_.swork1[_flowckX_->n6jim-One][dct3X_.dct3[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
			  One]-One]-One];
			if( formx <= 19 ){

				if( (((((((formx == 1 || formx == 4) || formx == 5) || 
				  formx == 8) || formx == 11) || formx == 12) || formx == 
				  16) || formx == 17) || formx == 19 ){

					sconX_.scon[sconlc-One][5-One] = 2;
					}
				else if( (((formx == 2 || formx == 3) || formx == 
				  6) || formx == 9) || formx == 13 ){

					sconX_.scon[sconlc-One][5-One] = 1;
					}
				else if( (((formx == 7 || formx == 10) || formx == 
				  14) || formx == 15) || formx == 18 ){
					}
				}
			}

		if( sw19bkX_.tense > 0 )
			sconX_.scon[sconlc-One][7-One] = sw19bkX_.tense;

		if( srcflgX_.srcflg == 2 ){

			/*   AUXILIARY FUNCTIONS */
			if( _sw23bkX_->pcb > 0 ){
				if( sw36bkX_.rel == 0 ){

					sworkoX_.sworko[_sw23bkX_->pcb-One][1-One] = 20;
					sconX_.scon[sworkoX_.phrhdo[_sw23bkX_->pcb-One]-
					  One][13-One] = 8;
					sworkoX_.sworko[_sw23bkX_->pcb-One][3-One] = 1;
					if( passesX_.passfl == 1 && passesX_.passct == 
					  1 ){
						swork1X_.swrk1i[_sw23bkX_->pcbswk-One][dct2X_.dct2[prctX_.js[sconX_.scolnk[_sw23bkX_->pcbswk-One]-
						  One]-One]-One] = 20;
						sconinX_.sconin[sconX_.scolnk[_sw23bkX_->pcbswk-One]-
						  One][3-One] = 8;
						swork1X_.swrk1i[_sw23bkX_->pcbswk-One][dct3X_.dct3[prctX_.js[sconX_.scolnk[_sw23bkX_->pcbswk-One]-
						  One]-One]-One] = 1;
						}

					if( _sw23bkX_->pcbwc == 20 ){
						if( sw31bkX_.fakev == 1 ){
							sworkoX_.sworko[_sw23bkX_->pcb-One][3-One] = 2;
							if( passesX_.passfl == 1 && passesX_.passct == 
							  1 )
								swork1X_.swrk1i[_sw23bkX_->pcbswk-One][dct3X_.dct3[prctX_.js[sconX_.scolnk[_sw23bkX_->pcbswk-One]-
								  One]-One]-One] = 2;
							}
						sw31bkX_.fakev = 0;
						}
					}
				}
			}

		sw21bkX_.case_ = 1;
		if( sw36bkX_.rel == 0 )
			_sw23bkX_->pcb = 0;
		if( sw36bkX_.rel == 1 )
			sw36bkX_.rel = 2;
		sw19bkX_.tense = 0;

		if( sw33bkX_.altwc == 2 ){
			sworkoX_.sworko[sworkoX_.phcto-One][1-One] = 1;
			sworkoX_.sworko[sworkoX_.phcto-One][3-One] = 45;
			sw33bkX_.altwc = 0;

			/*   ***** IF ALTWC IS 3 OR MORE SET SCON(8) EQ TO ALTWC */
			}
		else if( sw33bkX_.altwc < 3 ){

			if( vbdataX_.k3p1 > 1 )
				sw33bkX_.altwc = 0;
			}
		else{
			sconX_.scon[sconlc-One][8-One] = sw33bkX_.altwc;
			sw33bkX_.altwc = 0;
			}
		/*+                                                         *R1071MBS
		 *     INHB(136) = 0 REMOVED HERE */
		if( opswX_.sw[10-One] == 1 )
			{
			fprintf( _spec_fp, " SW21 %5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d", 
			  vtrnX_.vtrn, vbdataX_.k3p1, case_X_.case_, sw14bkX_.gen, 
			  sw14bkX_.num, sw14bkX_.per, sw33bkX_.altwc, sw36bkX_.relgen, 
			  sw36bkX_.relnum, sw36bkX_.relper, sw19bkX_.tense, vbdataX_.k3n, 
			  sw36bkX_.rel );
			for( xx=4; xx <= 8; xx++ ){
				fprintf( _spec_fp, "%5d", sconX_.scon[sconlc-One][xx-One] );
				}
			for( xx=1; xx <= 4; xx++ ){
				fprintf( _spec_fp, "%5d", sworkoX_.sworko[sworkoX_.phcto-One][xx-One] );
				}
			fprintf( _spec_fp, "%5d%5d%5d\n", _sw25bkX_->nnum, _sw25bkX_->ngen, 
			  _sw25bkX_->nper );
			}
		}
	return;
} /*end of function*/

