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
	 *    11/23/93 AVK:  If german phrasal transfer, use pat of head element
	/*   ***** BEGINNING OF -36 SWITCH ***** */
	/*         FUNCTION: SETS REL EQ 1 OR PARAMETER 1.
	 *         SETS SCON (CASE) OF VTR TO RT. EQ PARAMETER 2,
	 *         OR IF PARM 2 IS RELATIONAL POINTER, SETS SCON (CASE)
	 *         EQ OFL2 OF ADDRESSED ELEMENT. */

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

EXTERN struct t_set36X_ {
	short int set36[9];
	}	set36X_;
EXTERN struct t_getvtrX_ {
	short int getvtr;
	}	getvtrX_;
EXTERN struct t_loopckX_ {
	short int imatch[ELMMAX], noloop[21], nptpsv, nptpx, call36[2];
	}	loopckX_;
EXTERN struct t_sw36bkX_ {
	short int det, rel, relcas, relgen, relnum, relper;
	}	sw36bkX_;
EXTERN struct t_vtrs42X_ {
	short int sw42n;
	}	vtrs42X_;
EXTERN struct t_vwarg1X_ {
	short int look50[3], chng50[3], wc50el[3], isave, omfrm;
	}	vwarg1X_;
EXTERN struct t_swkadX_ {
	short int swkad;
	}	swkadX_;
EXTERN struct t_commdX_ {
	short int disam, ovrsw, vtrsw, matpos;
	}	commdX_;
EXTERN struct t_nloop2X_ {
	short int nloop2[21];
	}	nloop2X_;
EXTERN struct t_sw31bkX_ {
	short int artd, artind, dative, fakev, inhb43, maktrn, negflg, 
	  subcl;
	}	sw31bkX_;

void /*FUNCTION*/ t1sw36()
{
	static short int addr, savadr, x;
	static long int _l0;
	int taddress,dictype;
	static short val9 = 9;
	static char pgmnam[9] = "T1SW36  ";
	static short gz = 0;
	static short ms = 0;
	static short rs = 0;
	static short xx = 0;
	static short zz = 0;
	static short cb9 = 0;
	static short gb36 = 0;
	static short k3p2 = 0;
	static short numr = 0;
	static short sc13 = 0;
	static short retsw = 0;
	static short sconlc = 0;
	static short tabl36[9][2]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

	vbdataX_.k3n = vbdataX_.k3 + 3;
	k3p2 = vtrfX_.vtrf[vbdataX_.k3+2-One];
	flowckX_.n6jim = im81X_.im81 - k3p2;

	if( flag32X_.ad32 == -1 ){
		if( vbdataX_.k3p1 == 24 ){

			if( sw31bkX_.negflg == 0 )
				goto L_5500;
			}
		else if( vbdataX_.k3p1 >= 27 && vbdataX_.k3p1 <= 31 ){

			if( srcflgX_.srcflg != 2 )
				goto L_5500;
			rs = vbdataX_.k3p1 - 26;
			if( rs == 2 ){
				if( sw31bkX_.fakev != 1 )
					goto L_5500;
				}
			else if( rs == 3 ){
				if( sw31bkX_.subcl != 1 )
					goto L_5500;
				}
			else if( rs == 4 ){
				if( sw21bkX_.case_ != 1 )
					goto L_5500;
				}
			else if( rs == 5 ){
				if( sw31bkX_.dative != 1 )
					goto L_5500;
				}
			else if( sw31bkX_.maktrn != 1 ){
				goto L_5500;
				}
			}
		else if( vbdataX_.k3p1 == 43 ){



			/*   FUNCTION 43 - LOCKS THE SCON FOR N6JIM, IF FORM IS 9 UNINHIBITS
			 *   THE CONSTANT 579, IF SUPERSET IS NOT 4,7, OR 13, SETS SCON(8)
			 *   TO 2 (ALT WC). */

			if( srcflgX_.srcflg == 1 ){

				if( sconX_.scon[elscnpX_.elscnp[flowckX_.n6jim-One]-One][1-One] >= 0 ){

					sc13 = sconX_.scon[elscnpX_.elscnp[flowckX_.n6jim-One]-One][13-One];
					if( swork1X_.swork1[flowckX_.n6jim-One][dct3X_.dct3[prctX_.js[sconX_.scolnk[flowckX_.n6jim-One]-
					  One]-One]-One] == 9 ){

						/*   PR 1251  FOR ARITHMATE, LOCK SCON AND GET OUT
						 *+ OFL3B conversion & R1685 Swork limit Change RKH  04/23/87   R1679 */
						if( !(sconX_.scono[sconX_.scolnk[flowckX_.n6jim-One]-
						  One][45-SCONX1-One] == 5 && sc13 == 13) ){

							/*     INHB(579) = 0 */
							if( !((sc13 == 4 || sc13 == 7) || sc13 == 13) )
								sconX_.scon[elscnpX_.elscnp[flowckX_.n6jim-One]-One][8-One] = 2;
							}
						}
					sconX_.scon[elscnpX_.elscnp[flowckX_.n6jim-One]-
					  One][1-One] = -sconX_.scon[elscnpX_.elscnp[flowckX_.n6jim-One]-One][1-One];
					}
				}
			goto L_5500;
			}
		else if( vbdataX_.k3p1 == 44 ){

			/*   FUNCTION 44 -  CHANGE FORM ACCORDING TO SW36 TABLE */

			if( srcflgX_.srcflg == 1 ){
				zz = sconX_.scon[flowckX_.n6jim-One][11-One];
				for( ms=1; ms <= 9; ms++ ){
					if( zz == set36X_.set36[ms-One] )
						goto L_5495;
					}
				goto L_5500;

				/*   READ THE CORRESPONDING LINE FROM SW36G TABLE.
				 *   ARRAY CONTAINS GROUPS OF 2 NUMBERS, WHERE 1 IS PRESENT FORM
				 *   AND 2 IS THE NEW FORM TO CHANGE TO IF 1 IS FOUND. */
				/*winnt		do not read the table. It is loaded in common now. */
L_5495:
				lmove((short*)tabl36,1,&sw36tableX_.sw36table[ms-One][0][0],1,36);

				xx = swork1X_.swork1[flowckX_.n6jim-One][dct3X_.dct3[prctX_.js[sconX_.scolnk[flowckX_.n6jim-One]-
				  One]-One]-One];
				for( ms=1; ms <= 9; ms++ ){
					if( xx == tabl36[ms-One][1-One] )
						goto L_5499;
					}
				goto L_5500;
L_5499:
				swork1X_.swork1[flowckX_.n6jim-One][dct3X_.dct3[prctX_.js[sconX_.scolnk[flowckX_.n6jim-One]-
				  One]-One]-One] = tabl36[ms-One][2-One];
				}
			goto L_5500;
			}
		else if( vbdataX_.k3p1 == 45 ){

			/*   EG ONLY */
			if( (k3p2 < 0 || k3p2 > 2) || srcflgX_.srcflg != 2 ){
				xx = sconX_.scon[flowckX_.n6jim-One][12-One];
				if( xx == 1 || xx == 2 )
					sw36bkX_.det = xx;
				}
			else{
				sw36bkX_.det = k3p2;
				}
			goto L_5500;

			/*   50, 51 AND 66 ARE FLAGS FOR THE CHECKER PROGRAMS */
			}
		else if( (vbdataX_.k3p1 == 50 || vbdataX_.k3p1 == 51) || vbdataX_.k3p1 == 66 ){
			goto L_5500;
			}
		else if( vbdataX_.k3p1 == 56 ){

			/*          FUNCTION 56 - FIRST, PARAMETER TWO = 1 */

			/*+ */
			if( k3p2 == 0 ){

				/*        FUNCTION 56000 - SAVE CURRENT RULE ADDRESS TO PREVENT LOOPS
				 *                                0GM 10/86  FIX -36056 FOR MINI/MAIN  +
				 *   If IDENSP or IDEN42 are set, then we found identical rules in the
				 *   mini/main files for the non WC10 rules and the WC10 rules respectiv. */

				if( minickX_.minifg == 1 ){
					if( !((spcompX_.idensp == 1 && vtrs42X_.sw42n == 0) ||
						  (spcompX_.iden42 == 1 && vtrs42X_.sw42n == 1)) )
						goto L_5486;
					}
				/*                                0GM 10/86  FIX -36056 FOR MINI/MAIN  - */

				if( loopckX_.noloop[1-One] == 0 ){
					loopckX_.noloop[1-One] = vwarg2X_.i;
					if( vtrs42X_.sw42n == 1 )
						loopckX_.noloop[1-One] = vwarg1X_.isave;
					}
				for( gb36=2; gb36 <= 21; gb36++ ){
					if( loopckX_.noloop[gb36-One] == 0 )
						goto L_54872;
					}
				goto L_54841;
L_54872:
				loopckX_.noloop[gb36-One] = loopckX_.nptpsv;
				/*                                0GM 10/86  FIX -36056 FOR MINI/MAIN  + */
				if( vtrs42X_.sw42n == 0 && spcompX_.idensp == 1 ){
					loopckX_.noloop[gb36-One] = spcompX_.nptpm;

					}
				else if( vtrs42X_.sw42n == 1 && spcompX_.iden42 == 
				  1 ){
					loopckX_.noloop[gb36-One] = spcompX_.nptp42;
					}
				else{
					/*                                0GM 10/86  FIX -36056 FOR MINI/MAIN  - */

					if( diagsX_.deepdi == 1 )
						{
						fprintf( _spec_fp, " IN SW36, NOLOOP VAL =  " );
						for( gz=1; gz <= 21; gz++ ){
							fprintf( _spec_fp, "%6d", loopckX_.noloop[gz-One] );
							}
						fprintf( _spec_fp, "\n" );
						}
					/*10- */
					goto L_5500;
					}

				/*        NLOOP2 STORES ADDRESS OF MINI RULE MATCH */
L_5486:
				if( nloop2X_.nloop2[1-One] == 0 ){
					nloop2X_.nloop2[1-One] = vwarg2X_.i;
					if( vtrs42X_.sw42n == 1 )
						nloop2X_.nloop2[1-One] = vwarg1X_.isave;
					}
				/*+1                          up loop bound to 21    5/25/94  jal */
				for( gb36=2; gb36 <= 21; gb36++ ){
					if( nloop2X_.nloop2[gb36-One] == 0 )
						goto L_54873;
					}
				goto L_54841;
L_54873:
				nloop2X_.nloop2[gb36-One] = loopckX_.nptpsv;

				if( diagsX_.deepdi == 1 )
					{
					fprintf( _spec_fp, " IN 36 SW, NLOOP2 = \n      " );
					for( gz=1; gz <= 21; gz++ ){
						fprintf( _spec_fp, "%6d", nloop2X_.nloop2[gz-One] );
						}
					fprintf( _spec_fp, " \n" );
					}
				goto L_5500;
				}
			else{

				/*       CALL36(1) IS THE ADDRESS RECORDED IN 42 SWITCH OF ORIGINAL
				 *       RULE THAT CALLED A WORD CLASS 10 RULE (USE WITH -36056001)
				 *       CALL36(2) IS ZERO UNLESS ORIGINAL CALLING RULE IS FROM MINI (=1)
				 *                                0GM 10/86  FIX -36056 FOR MINI/MAIN  +
				 *       idensp=1 indicates that both the main WC and mini WC rules
				 *       which called the WC 10 rule are identical. */

				if( !(loopckX_.call36[2-One] == 1 && spcompX_.idensp != 1) ){
					/*                                0GM 10/86  FIX -36056 FOR MINI/MAIN  - */

					/*        HERE LOAD CALL36(1) INTO NOLOOP ARRAY */

					if( loopckX_.noloop[1-One] == 0 ){
						loopckX_.noloop[1-One] = vwarg2X_.i;
						if( vtrs42X_.sw42n == 1 )
							loopckX_.noloop[1-One] = vwarg1X_.isave;
						}
					for( gb36=2; gb36 <= 21; gb36++ ){
						if( loopckX_.noloop[gb36-One] == 0 )
							goto L_54874;

						}
					goto L_54841;
L_54874:
					loopckX_.noloop[gb36-One] = loopckX_.call36[1-One];
					/*                                0GM 10/86  FIX -36056 FOR MINI/MAIN  + */
					if( spcompX_.idensp != 1 )
						goto L_5500;
					loopckX_.noloop[gb36-One] = spcompX_.nptpm;
					}

				/*        MINI: LOAD CALL36(1) INTO NLOOP2 ARRAY */

				if( nloop2X_.nloop2[1-One] == 0 ){
					nloop2X_.nloop2[1-One] = vwarg2X_.i;
					if( vtrs42X_.sw42n == 1 )
						nloop2X_.nloop2[1-One] = vwarg1X_.isave;
					}
				for( gb36=2; gb36 <= 21; gb36++ ){
					if( nloop2X_.nloop2[gb36-One] == 0 )
						goto L_54875;

					}
				goto L_54841;
L_54875:
				nloop2X_.nloop2[gb36-One] = loopckX_.call36[1-One];
				goto L_5500;
				}

			/*        OVERLOADING NOLOOP ARRAY */
L_54841:
			if( diagsX_.longdi == 1 )
				{
				fprintf( _spec_fp, " -36056 OVERLOADING NOLOOP ARRAY\n" );
				}
			goto L_5500;
			}
		else if( vbdataX_.k3p1 < 11 || vbdataX_.k3p1 > 14 ){
			goto L_5360;
			}
		else{
			/*   CHANGE THE VALUE OF SCON5 FOR THE ELEMENT POINTED TO. */

			/*----------------------------------------
			 *---- N6JIM = HAS BEEN CALCULATED ABOVE */
			flowckX_.n6jim = im81X_.im81 - vtrfX_.vtrf[vbdataX_.k3+2-One];
			/*---------------------------------------- */
			ms = nounsX_.gernum[vbdataX_.k3p1-10-One][formsaX_.formsv[sconX_.scolnk[flowckX_.n6jim-One]-			  One]-One];
			if( !((swork1X_.swork1[flowckX_.n6jim-One][dct2X_.dct2[prctX_.js[sconX_.scolnk[flowckX_.n6jim-One]-
			                   One]-One]-One] != 1) &&
			      (swork1X_.swork1[flowckX_.n6jim-One][dct2X_.dct2[prctX_.js[sconX_.scolnk[flowckX_.n6jim-One]-
			                   One]-One]-One] != 7)) ){
				if( sconX_.scon[flowckX_.n6jim-One][3-One] <= 3 ){
					if( ms != 0 )
						sconX_.scon[flowckX_.n6jim-One][5-One] = ms;
					}
				}
			goto L_5500;
			}


		if( loopckX_.call36[1-One] == 0 ){
			loopckX_.call36[1-One] = loopckX_.nptpsv;
			if( minickX_.minifg == 1 )
				loopckX_.call36[2-One] = 1;
			}

		/*MINIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
		 *                  MINI MINI MINI */

		/*                     IS THERE AN EXPERIMENTAL RULE?? */
		cb9 = k3p2;
		semargX_.pntr9 = k3p2;
		txmini(3,&minickX_.k7m,&loopckX_.nptpx,vwarg2X_.i,&cb9);
		if( errvrsX_.errlvl != 0 )
			return;
		idxval((short*)ADR(_l0,1),&val9,&k3p2,&diacbX_.k7,&numr);
		if( numr == 0 && minickX_.k7m == 0 )
			goto L_5500;
		if( numr != 0 )
		{
			rulein_ptr((short*)ADR(_l0,1),&commdX_.matpos,
				spX_ptr,&diacbX_.k7, &retsw);
		}



		/*                    K7M WILL BE ZERO IF NO RULE FOUND */
		if( diacbX_.k7 != 0 || minickX_.k7m != 0 )
			getvtrX_.getvtr = 1;
		return;
		}

L_5360:
	if( vbdataX_.k3p1 <= 100 && flag32X_.ad32 == -1 ){

		sw36bkX_.rel = vbdataX_.k3p1;

		/*   IF PARM 2 IS NEG. IT IS A RELATIONAL POINTER
		 *   OTHERWISE ASSUME PARM 2 IS POS. AND CONTAINS CASE
		 *   SET SCON(CASE) OF ELEMENT TO RIGHT EQ PARM.
		 *   SET REL FLAGS ALWAYS */

		sw36bkX_.relcas = 1;
		sw36bkX_.relnum = 0;
		sw36bkX_.relper = 3;
		sw36bkX_.relgen = 0;
		if( k3p2 != 0 ){
			vbdataX_.k3n = vbdataX_.k3 + 5;

			/*   VTRN NORMALLY AT SWITCH CONTENTS; NOW SET TO RIGHT ELEMENT */

			vtrnX_.vtrn = vtrfX_.vtrf[vbdataX_.k3+3-One];
			vbdataX_.k3p1 = vtrfX_.vtrf[vbdataX_.k3+4-One];

			elemld();
			if( errvrsX_.errlvl != 0 )
				return;


			/*   LOAD SCON FOR ELEMENT ON RT. OR SW.
			 *   SETS SCON(CASE) TO SECOND PARAMETER, BUT RECHECKS
			 *   TO SEE IF IT IS TO BE RESET TO OFL2.
			 *   IF IT IS TO BE RESET TO OFL2, FIND OFL2
			 *   VIA SWKAD, POINTING TO DICTIONARY OFL2. */


			sconlc = flowckX_.n6jim;
			if( k3p2 > 0 ){
				sconX_.scon[sconlc-One][7-One] = k3p2;
				}
			else{
				 //   CASE INFO  STORED IN FORMSV
				sconX_.scon[sconlc-One][7-One] = formsaX_.formsv[sconX_.scolnk[sconlc-One]-One];
				}
			}
		}
	else{

		/*   AN INCOMING SWORK1 ADDRESS IS BEING CHANGED */

		/*------------------------------------------
		 *---- N6JIM = HAS BEEN CALCULATED ABOVE - TWICE */
		flowckX_.n6jim = im81X_.im81 - vtrfX_.vtrf[vbdataX_.k3+2-One];
		/*------------------------------------------ */
		addr = -vbdataX_.k3p1;
		if( flag32X_.ad32 != -1 ){
			addr = vbdataX_.k3p1 + 1000*flag32X_.ad32;
			flag32X_.ad32 = -1;
			}
		savadr = swork1X_.swork1[flowckX_.n6jim-One][((4*prctX_.js[sconX_.scolnk[flowckX_.n6jim-One]-One])+3)-One];
		swork1X_.swork1[flowckX_.n6jim-One][((4*prctX_.js[sconX_.scolnk[flowckX_.n6jim-One]-One])+3)-One] = addr;
		if( addr < 0 && addr > -1000 )
			sconX_.scono[sconX_.scolnk[flowckX_.n6jim-One]-One][60-SCONX1-One] = -addr;


		//+             SET SCON59 WITH TARG MAIN PAT           *10/10/91*JAL*
		if( addr < 0 && addr > -1000 ){
			taddress = -addr;
			dictype = 2;
		}
		else{
			taddress = addr;
			dictype = 3;
		}
		errvrsX_.err = TARG_CODES(&trgflgX_.trgflg,&dictype,
						&taddress,
				   cmpsmcX_.cmpcod[flowckX_.n6jim-One][((4*prctX_.js[sconX_.scolnk[flowckX_.n6jim-One]-One])+3)-One],
						(short*)&trgcdsX_.tcwdad,diagsX_.longdi, _spec_fp);
		if( errvrsX_.err != 0 ){
			errlog(pgmnam,5399,taddress,13);
			swork1X_.swork1[flowckX_.n6jim-One][((4*prctX_.js[sconX_.scolnk[flowckX_.n6jim-One]-One])+3)-One] = savadr;
			}
		else{
			sconX_.scono[sconX_.scolnk[flowckX_.n6jim-One]-One][59-SCONX1-One] = trgcdsX_.tcpatm[0];
			sconX_.scon[flowckX_.n6jim-One][3-One] = 0;
			/*              SET IN GENDER FOR NOUN CONSTANTS */
			if( trgcdsX_.tcov2b != 0 )
				sconX_.scon[flowckX_.n6jim-One][4-One] = trgcdsX_.tcov2b;
			}

		}

L_5500:
	if( diagsX_.deepdi == 1 )
		{
		fprintf( _spec_fp, " SW36 \n%6d%6d%6d%6d%6d%6d\n", vbdataX_.k3n, 
		  vtrnX_.vtrn, k3p2, flowckX_.n6jim, swkadX_.swkad, sconX_.scon[sconlc-One][7-One] );
		}

	return;
} /*end of function*/

