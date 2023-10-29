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
	/*  CHANGES:
	 *    09/20/94 jal         LOG#2125
	 *              for english source - only set number (SC5) based on
	 *              SC3 if this is a nominal element.
	 *    10/21/93 jal:  Dont set SC5 based on SC3 if the noun was
	 *           originally an arithmate (i.e. SC1=16).
	 *    10/21/93 jal:  DET was used to set SC3 for all ENG src targets.
	 *           Changed it to affect only german targets. It is used to
	 *           set weak,strong and mixed inflection for nouns and adj.
	 *     5/13/93 jal:  fix to new scon protection strategy (4/5/93).
	 *                   Save incoming SCON 4,5,6,7 values. Dont
	 *                   change a SCON's value if incoming .NE.0
	 *     4/5/93 JAL: Dont change SCONs 4,5,6, or 7 if already non-zero.
	 *     4/5/93 JAL: DO only source work (no SCONS) if pass1 of 2 passes.
	 *     01/07/92*JAL*: REMOVE ALL SETTING OF SC4 OF THE HEAD.
	 *         REMOVE DEPENDENCIES ON 16SW IN SEMTAB AND ON SC3 OF HEAD.
	 *     04/17/87 *R1685RKH*  CHANGE T1-4 SWORK1 LIMIT FROM 50 TO
	 *     08/17/86 *R1561DSD: 100 SCONS
	 *     11/23/85 */
	/*   ***** BEGINNING OF -25 SWITCH ***** */
	/*         FUNCTION:  CAPTURES HEAD NOUN DATA
	 *         A RIGHT ORIENTED SW. EXCEPT IF K3P1 IS NEGATIVE. */

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

EXTERN struct t_eform1X_ {
	short int eform1[10];
	}	eform1X_;
EXTERN struct t_pluralX_ {
	short int plural[14];
	}	pluralX_;
EXTERN struct t_form25X_ {
	short int eform2[10][3];
	}	form25X_;
EXTERN struct t_sw14bkX_ {
	short int gen, num, per;
	}	sw14bkX_;
EXTERN struct t_sw36bkX_ {
	short int det, rel, relcas, relgen, relnum, relper;
	}	sw36bkX_;
EXTERN struct t_swkadX_ {
	short int swkad;
	}	swkadX_;
EXTERN struct t_isizeX_ {
	short int isize;
	}	isizeX_;
EXTERN struct t_sw13bkX_ {
	short int loc25, preloc;
	}	sw13bkX_;
EXTERN struct t_sw31bkX_ {
	short int artd, artind, dative, fakev, inhb43, maktrn, negflg, 
	  subcl;
	}	sw31bkX_;
EXTERN struct t_sw37bkX_ {
	short int arith;
	}	sw37bkX_;
EXTERN struct t_sw38bkX_ {
	short int adjnp, sw38n, vnum;
	}	sw38bkX_;
EXTERN struct t_tr1ptrX_ {
	long int f12, f73;
	}	tr1ptrX_;
	/* end of COMMON translations */
void /*FUNCTION*/ t1sw25()
{
	static short int gender, sc1, sc4in, sc5in, sc6in, sc7in;
	struct  {
		short int im1, i3save, i3str, n6, n6jim, n6jims, phrstr;
		}	*_flowckX_ = (void*)&flowckX_;
	struct  {
		short int hedcas, hedgen, hednum, hedper, hedsc3, ngen, nnum, 
		  relngn, relnnm, relnpr, sw25n, tw25, nper;
		}	*_sw25bkX_ = (void*)&sw25bkX_;
		/*end of pointer COMMON transls*/
	static char pgmnam[9] = "T1SW25  ";
	static short m = 0;
	static short s5 = 0;
	static short xx = 0;
	static short sc3 = 0;
	static short sconlc = 0;

	vtrnX_.vtrn = vtrfX_.vtrf[vbdataX_.k3+2-One];
	vbdataX_.k3p1 = vtrfX_.vtrf[vbdataX_.k3+3-One];
	_sw25bkX_->sw25n = 1;

	/*   GOTO CREATE A SCON RECORD FOR THIS ELEMENT.
	 *   SSAVE IS ASSIGNED A LABEL GOTO FOR RETURN FROM SCON LOAD */

	vbdataX_.k3n = vbdataX_.k3 + 4;
	elemld();
	if( errvrsX_.errlvl == 0 ){

		sconlc = _flowckX_->n6jim;
		/*                   save incoming SCON 4,5,6,7 values. Dont
		 *                   change a SCON's value if incoming .NE.0 */
		sc4in = sconX_.scon[sconlc-One][4-One];
		sc5in = sconX_.scon[sconlc-One][5-One];
		sc6in = sconX_.scon[sconlc-One][6-One];
		sc7in = sconX_.scon[sconlc-One][7-One];

		vbdataX_.k3p1 = vtrfX_.vtrf[vbdataX_.k3+1-One];
		/*            if pass1 of 2 passes limit changes to source only */
		if( !(passesX_.passfl == 1 && passesX_.passct == 1) ){

			if( sconX_.scon[sconlc-One][1-One] >= 0 ){
				if( sc7in == 0 ){
					if( srcflgX_.srcflg == 2 ){
						if( vbdataX_.k3p1 == 0 && sw36bkX_.rel == 
						  0 )
							sconX_.scon[sconlc-One][7-One] = case_X_.case_;
						if( vbdataX_.k3p1 == 0 && sw36bkX_.rel > 0 )
							sconX_.scon[sconlc-One][7-One] = sw36bkX_.relcas;
						}

					if( vbdataX_.k3p1 > 1 )
						sconX_.scon[sconlc-One][7-One] = vbdataX_.k3p1;
					if( vbdataX_.k3p1 == -99 && sw36bkX_.rel == 0 )
						sconX_.scon[sconlc-One][7-One] = case_X_.case_;
					if( vbdataX_.k3p1 == -99 && sw36bkX_.rel > 0 )
						sconX_.scon[sconlc-One][7-One] = sw36bkX_.relcas;
					}
				if( vbdataX_.k3p1 < -80 )
					case_X_.case_ = sconX_.scon[sworkoX_.phrhdo[_flowckX_->im1-vbdataX_.k3p1-80-One]-
					  One][7-One];

				/*    RESET NUMBER */
				if( sw36bkX_.rel == 1 ){

					if( sw36bkX_.relnum != 0 ){
						if( sc5in == 0 )
							sconX_.scon[sconlc-One][5-One] = sw36bkX_.relnum;
						sw36bkX_.relnum = 0;
						goto L_4321;
						}
					}
				else if( sw14bkX_.num != 0 ){


					if( sc5in == 0 )
						sconX_.scon[sconlc-One][5-One] = sw14bkX_.num;
					sw14bkX_.num = 0;
					goto L_4321;
					}

				if( srcflgX_.srcflg != 2 ){

					if( sc5in == 0 ){
						sconX_.scon[sconlc-One][5-One] = 1;
						for( s5=1; s5 <= 14; s5++ ){
							if( formsaX_.formsv[sconX_.scolnk[_flowckX_->n6jim-One]-
							  One] == pluralX_.plural[s5-One] )
								goto L_4322;
							}
						goto L_4321;
L_4322:
						sconX_.scon[sconlc-One][5-One] = 2;
						}
					}
				else if( sc5in == 0 ){

					if( swork1X_.swork1[_flowckX_->n6jim-One][dct3X_.dct3[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
					  One]-One]-One] == 2 )
						sconX_.scon[sconlc-One][5-One] = 2;
					if( swork1X_.swork1[_flowckX_->n6jim-One][dct3X_.dct3[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
					  One]-One]-One] == 14 )
						sconX_.scon[sconlc-One][5-One] = 2;
					}


				/*   TEST OFL2(SCON(3,SCONLC)) OVERRIDING PREVIOUS SETTING
				 *          OF SCON(5,SCONLC) */

L_4321:
				if( srcflgX_.srcflg == 2 ){
					if( (sconX_.scon[sconlc-One][13-One] == 15 || 
					  sconX_.scon[sconlc-One][13-One] == 16) && sconX_.scon[sconlc-One][8-One] == 
					  2 ){
						if( sc4in == 0 )
							sconX_.scon[sconlc-One][4-One] = targ25X_.targ25[sconX_.scolnk[_flowckX_->n6jim-One]-
							  One];
						goto L_4135;
						}
					}

				/*          Setting Number (SC5) based on SC3 */

				/*+                                                   10/21/93 jal
				 *              dont set SC5 if element originally an arithmate */
				if( sconX_.scon[sconlc-One][3-One] == 9 &&
					sconX_.scon[sconlc-One][12-One] == 9 )
					goto L_4136;
				/*                                       LOG#2125  09/20/94 jal
				 *                                       LOG#2155  05/04/94 jal
				 *              for eng source - only set num for nominal elements */
				if( srcflgX_.srcflg == 2 ){
					sc1 = sconX_.scon[sconlc-One][1-One];
					if( sc1 != 1 && sc1 != 5 )
						goto L_4135;
					if( sc1 == 1 && (sconX_.scon[sconlc-One][13-One] == 
					  15 || sconX_.scon[sconlc-One][13-One] == 16) )
						goto L_4135;
					}

				sc3 = sconX_.scon[sconlc-One][3-One];
				/*                      singular */
				if( sc3 >= 4 && sc3 <= 6 ){
					if( sc5in == 0 )
						sconX_.scon[sconlc-One][5-One] = 1;
					/*                      plural */
					}
				else if( sc3 >= 7 && sc3 <= 9 ){
					if( sc5in == 0 )
						sconX_.scon[sconlc-One][5-One] = 2;
					}

				/*            Setting Person    to 3rd */

L_4135:
				if( sc6in == 0 )
					sconX_.scon[sconlc-One][6-One] = 3;
				}


L_4136:
			;
			_sw25bkX_->hednum = sconX_.scon[sconlc-One][5-One];
			_sw25bkX_->hedgen = sconX_.scon[sconlc-One][4-One];
			_sw25bkX_->hedper = sconX_.scon[sconlc-One][6-One];
			_sw25bkX_->hedcas = sconX_.scon[sconlc-One][7-One];
			}
		/*        CLEAR OUT IN CASE ELEMENT IS LOCKED SO IT WON'T REMAIN
		 *        SET FOR THE NEXT UNRELATED LOAD. */
		if( sw36bkX_.rel == 1 )
			sw36bkX_.relnum = 0;
		if( sw36bkX_.rel != 1 )
			sw14bkX_.num = 0;
		gender = 0;

		/*        IF PREVIOUS PHRASE WAS SUPPRESSED AND SWORKO WAS SET BY
		 *        A PREVIOUS SW25, THIS SW25 WILL SET SCONS OF ELEMENTS
		 *        LOADED IN THIS VTR */

		if( sworkoX_.phcto == supresX_.phcts ){
			supresX_.n3sw25 = supresX_.n3sv + 1;
			/*           if pass1 of 2 passes limit changes to source only */
			if( passesX_.passfl == 1 && passesX_.passct == 1 )
				goto L_4200;
			}
		else{
			sworkoX_.sworko[sworkoX_.phcto-One][1-One] = swork1X_.swork1[_flowckX_->n6jim-One][dct2X_.dct2[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
			  One]-One]-One];
			sworkoX_.sworko[sworkoX_.phcto-One][2-One] = swork1X_.swork1[_flowckX_->n6jim-One][dct4X_.dct4[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
			  One]-One]-One];
			sworkoX_.sworko[sworkoX_.phcto-One][3-One] = swork1X_.swork1[_flowckX_->n6jim-One][dct3X_.dct3[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
			  One]-One]-One];
			sworkoX_.sworko[sworkoX_.phcto-One][4-One] = _flowckX_->n6jim;
			sworkoX_.phrhdo[sworkoX_.phcto-One] = sconlc;
			supresX_.phcts = sworkoX_.phcto;

			/*        SET 'TW25' FLAG:
			 *        IT TELLS US THAT A 20-25 SW COMBINATION HAS OCCURRED
			 *        IN THIS CHAINING FUNCTION. IT WILL BE USED TO
			 *        INHIBIT THE ACTION OF ANY 34 SW IN THE CHAINING FUNCTION */

			if( supresX_.phsup == 1 )
				_sw25bkX_->tw25 = 1;

			if( srcflgX_.srcflg == 2 ){

				if( sw31bkX_.artd == 1 || sw31bkX_.artind == 1 ){
					if( sw31bkX_.artd == 1 )
						xx = 2;
					if( sw31bkX_.artind == 1 )
						xx = 3;
					}
				else if( sw38bkX_.adjnp == 0 ){
					goto L_4160;
					}
				else{
					xx = 1;
					}

				for( m=1; m <= 10; m++ ){
					if( sworkoX_.sworko[sworkoX_.phcto-One][3-One] == 
					  eform1X_.eform1[m-One] )
						goto L_4323;
					if( diagsX_.deepdi == 1 )
						{
						fprintf( _spec_fp, " IN EFORM LOOP %5d%5d\n", 
						  sworkoX_.sworko[sworkoX_.phcto-One][3-One], 
						  eform1X_.eform1[m-One] );
						}
					}
				goto L_4159;
L_4323:
				sworkoX_.sworko[sworkoX_.phcto-One][3-One] = form25X_.eform2[m-One][xx-One];

L_4159:
				sw38bkX_.adjnp = 0;
				sw31bkX_.artd = 0;
				sw31bkX_.artind = 0;
				}
			/*           if pass1 of 2 passes limit changes to source only */
L_4160:
			if( passesX_.passfl == 1 && passesX_.passct == 1 )
				goto L_4200;

			if( srcflgX_.srcflg == 2 ){
				/*+             DET sets SC3 for weak,strong,mixed case.  10/21/93 jal */
				if( trgflgX_.trgflg == 1 ){
					if( sw36bkX_.det == 0 )
						sconX_.scon[sconlc-One][3-One] = 9;
					if( sw36bkX_.det == 2 )
						sconX_.scon[sconlc-One][3-One] = 6;
					sw36bkX_.det = 0;
					}
				_sw25bkX_->hedsc3 = sconX_.scon[sconlc-One][3-One];
				}
			}


		if( srcflgX_.srcflg == 2 ){
			if( sw36bkX_.rel > 0 ){
				_sw25bkX_->relngn = sconX_.scon[sconlc-One][4-One];
				_sw25bkX_->relnnm = sconX_.scon[sconlc-One][5-One];
				_sw25bkX_->relnpr = 3;
				/*               NNUM ETC. ARE SET TO COMMUNICATE TO FOLLOWING VERB */
				}
			else if( case_X_.case_ == 1 ){
				_sw25bkX_->ngen = sconX_.scon[sconlc-One][4-One];
				_sw25bkX_->nnum = sconX_.scon[sconlc-One][5-One];
				_sw25bkX_->nper = 3;
				}
			}


		/*    CHANGE TO -25 SW (TO ACCOMODATE -33 SW)
		 *    CHECK ARITH FLAG */

		if( sw37bkX_.arith == 1 ){
			if( sconX_.scon[sconlc-One][1-One] >= 0 ){
				sconX_.scon[sconlc-One][7-One] = 2;
				sconX_.scon[sconlc-One][5-One] = 1;
				_sw25bkX_->hedcas = 2;
				_sw25bkX_->hednum = 1;
				}
			}
		else if( sw37bkX_.arith != 0 ){
			if( sconX_.scon[sconlc-One][1-One] >= 0 ){
				sconX_.scon[sconlc-One][7-One] = 2;
				sconX_.scon[sconlc-One][5-One] = 2;
				_sw25bkX_->hedcas = 2;
				_sw25bkX_->hednum = 2;
				}
			}
		sw37bkX_.arith = 0;

		/*   LOC25 SET BY -13 -95 SWITCH */

L_4200:
		;
		if( sw13bkX_.loc25 != 0 && sconX_.scon[sconlc-One][1-One] >= 
		  0 )
			sconX_.scon[sconlc-One][1-One] = -sconX_.scon[sconlc-One][1-One];
		sw13bkX_.loc25 = 0;

		if( srcflgX_.srcflg == 1 )
			case_X_.case_ = 0;
		inhbX_.inhb[136-One] = 0;

		if( opswX_.sw[10-One] == 1 )
			{
			fprintf( _spec_fp, " SW25 %5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d", 
			  vtrnX_.vtrn, vbdataX_.k3n, vbdataX_.k3p1, sconlc, _sw25bkX_->hedcas, 
			  _flowckX_->im1, sw37bkX_.arith, sw14bkX_.gen, sw14bkX_.num, 
			  sw14bkX_.per, sw36bkX_.relgen, sw36bkX_.relnum, sw36bkX_.relper, 
			  sw36bkX_.relcas, case_X_.case_ );
			for( xx=4; xx <= 7; xx++ ){
				fprintf( _spec_fp, "%5d", sconX_.scon[sconlc-One][xx-One] );
				}
			for( xx=1; xx <= 4; xx++ ){
				fprintf( _spec_fp, "%5d", sworkoX_.sworko[sworkoX_.phcto-One][xx-One] );
				}
			fprintf( _spec_fp, "%5d%5d%5d%5d%5d\n", _sw25bkX_->nnum, 
			  _sw25bkX_->ngen, _sw25bkX_->nper, _flowckX_->phrstr, 
			  supresX_.n3sw25 );
			}

		}
	return;
} /*end of function*/

