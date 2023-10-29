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
	 *      11/23/93 AVK: If german phrasal transfer, use pat of head element
	 *      10/18/93 jal: Dont go the HF dict if K3P1 .lt. 121, in other
	 *          words only load dict codes if explicit hi freq const load.
	 *      10/10/91 JAL: SET SCON59 FROM HFDO IF HIGH FREQ CONST LOAD.
	 *      CHG: 04/17/87 *R1685RKH*  CHANGE T1-4 SWORK1 LIMIT FROM 50 TO
	 *      CHG 08/19/86 *R1561DSD: 100 SCONS
	 *      CHG 09/25/85 */
	/*     SUBROUTINE FOR LOADING AN ELEMENT INTO THE OPADRO */
	/*   THIS SUBROUTINE IS CALLED DIRECTLY BY T1VTRPRO PROGRAM AS WELL
	 *   AS BY THE FOLLOWING SWITCHES: 12,21,25,36,38,44
	 *   CONTROL IS RESTORED WITH NO SPECIAL CONDITIONS. */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*   ----- START SUBROUTINE FOR OPADRO AND SWORKO LOAD FOR ELEMENTS ----- */
	/*     THIS SUBROUTINE IS INVOKED DURING VTR (SWITCH)
	 *     PROCESSING WHEN ANY VTR ELEMENT IS EXAMINED.
	 *     REPLACE SSAVE LOGIC WITH A RETURN STATEMENT. RESOLVE THE 8220
	 *     GOTO WITH SOME LOGIC AFTER THE ELEMENT CALL. ALL THE OTHER SSAVEs
	 *     SHOULD BE SIMPLE RETURNS: i.e. NOT GOTOs TO A DIFFERENT PART OF
	 *     P0008. */

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

EXTERN struct t_sw18bkX_ {
	short int phrnew;
	}	sw18bkX_;
EXTERN struct t_swkadX_ {
	short int swkad;
	}	swkadX_;
EXTERN struct t_isizeX_ {
	short int isize;
	}	isizeX_;
EXTERN struct t_sw13bkX_ {
	short int loc25, preloc;
	}	sw13bkX_;


void /*FUNCTION*/ elemld()
{
		/*pointer COMMON transls*/
	struct  {
		short int k7, oflad, n3;
		}	*_diacbX_ = (void*)&diacbX_;
	struct  {
		short int im1, i3save, i3str, n6, n6jim, n6jims, phrstr;
		}	*_flowckX_ = (void*)&flowckX_;
	struct  {
		short int hedcas, hedgen, hednum, hedper, hedsc3, ngen, nnum, 
		  relngn, relnnm, relnpr, sw25n, tw25;
		}	*_sw25bkX_ = (void*)&sw25bkX_;

	static char pgmnam[9] = "T1ELEMLD";
	static short ij = 0;
	static short pp = 0;
	static short temp = 0;
	int taddress,dictype;

	/*    SET SWORK1 POSITION AS REQUIRED */
	_flowckX_->n6jim = _flowckX_->im1 - vtrnX_.vtrn;
/* ocr 05.31.01 */
	if(_flowckX_->n6jim<1) {
/* When it happens smthg went wrong and we want just
	drop this sentence */
		errvrsX_.errlvl = 6;
		return;
	}

	swkadX_.swkad = swork1X_.swork1[_flowckX_->n6jim-One][dctX_.dct[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
	  One]-One]-One];
	/*                      N6JIMS IS LAST PHRBEG LOADED */
	_flowckX_->n6jims = _flowckX_->n6jim;


	/*   IF PARAMETER K3P1 OF ELEMENT IS A CONSTANT, USE IT AND REPLACE THE
	 *   APPROPRIATE SCON VALUES.
	 *   IT CAN BE EITHER A HIGH FREQ CONSTANT, OR A LOW FREQ CONSTANT. IF TH
	 *   LO FREQ, THEN SEMTRAN HAS REPLACED THE SWORK1 ADDRESS. FOR HIGH FREQ
	 *   CONSTANT, REPLACE THE SCON4 SETTING FROM THE CONSTANT TARGET FILE AN
	 *   CLEAR THE SCON3 SETTING. */

	/*          ARE WE REPLACING THE ELEMENT WITH A CONSTANT?
	 *+                                                        *R0GBA*GBA
	 *  CHECK FOR A SCON LOCK OR ELSE */
	if( sconX_.scon[_flowckX_->n6jim-One][1-One] < 0 )
		vbdataX_.k3p1 = 0;
	/*-                                                        *R0GBA*GBA */
	if( vbdataX_.k3p1 > 120 )
		swkadX_.swkad = -vbdataX_.k3p1;


	/*          SW13 PRE-LOCK ?? */
	if( sw13bkX_.preloc != 0 ){
		sconX_.scon[_flowckX_->n6jim-One][1-One] = -sconX_.scon[_flowckX_->n6jim-One][1-One];
		sw13bkX_.preloc = 0;
		}


	/*          ALTERNATE W/C */
	sconX_.scon[_flowckX_->n6jim-One][8-One] = vbdataX_.k3p1;
	if( vbdataX_.k3p1 > 120 )
		sconX_.scon[_flowckX_->n6jim-One][8-One] = 1;
	if( vbdataX_.k3p1 == 1 && sw33bkX_.altwc > 0 ){
		sconX_.scon[_flowckX_->n6jim-One][8-One] = sw33bkX_.altwc;
		sw33bkX_.altwc = 0;
		}


	/*                  IF LOADING A HI FREQ CONSTANT, THEN READ HFDO DICT.
	 *winnt      IF (SWKAD .GT. 0 .OR. SWKAD .LE. -ISIZE) GOTO 7440 */
	if( swkadX_.swkad <= 0 ){
		/*+                                                         10/18/93 jal
		 *                  skip lookup if hi freq addr not set by this switch */
		if( vbdataX_.k3p1 >= 121 ){

			taddress = -swkadX_.swkad;
			dictype = 2;
			errvrsX_.err = TARG_CODES(&trgflgX_.trgflg,&dictype,
						  &taddress,
						  cmpsmcX_.cmpcod[_flowckX_->n6jim-One][prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-One]-One],
						  hfreqX_.hfdo,diagsX_.longdi, _spec_fp);
			if( errvrsX_.err == 0 ){
				sconX_.scon[_flowckX_->n6jim-One][3-One] = 0;
				if( hfreqX_.hfdo[3] != 0 )
					sconX_.scon[_flowckX_->n6jim-One][3] = hfreqX_.hfdo[3];

				sconX_.scono[sconX_.scolnk[_flowckX_->n6jim-One]-One][59-SCONX1-One] = hfreqX_.hfdo[7-One];
				}
			else{
				errlog(pgmnam,7400,taddress,10);
				swkadX_.swkad = swork1X_.swork1[_flowckX_->n6jim-One][dctX_.dct[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
				                One]-One]-One];
				}
			}
		}

	if( sw44bkX_.slot44 != 1 ){

		/*   IS EACH ELEMENT A PHRASE??? */
		if( sw18bkX_.phrnew != 0 ){
			if( sworkoX_.phcto == sw26bkX_.phr26 )
				t1se26();
			if( sworkoX_.phcto > 0 )
				sworkoX_.phrndo[sworkoX_.phcto-One] = _diacbX_->n3;
			sworkoX_.phcto += 1;
			sworkoX_.sworko[sworkoX_.phcto-One][1-One] = swork1X_.swork1[_flowckX_->n6jim-One][dct2X_.dct2[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
			  One]-One]-One];
			sworkoX_.sworko[sworkoX_.phcto-One][2-One] = swork1X_.swork1[_flowckX_->n6jim-One][dct4X_.dct4[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
			  One]-One]-One];
			sworkoX_.sworko[sworkoX_.phcto-One][3-One] = swork1X_.swork1[_flowckX_->n6jim-One][dct3X_.dct3[prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
			  One]-One]-One];
			sworkoX_.sworko[sworkoX_.phcto-One][3] = _flowckX_->n6jim;
			sworkoX_.phrbgo[sworkoX_.phcto-One] = _diacbX_->n3 + 1;
			sworkoX_.phrhdo[sworkoX_.phcto-One] = _flowckX_->n6jim;
			_sw25bkX_->tw25 = 0;
			}

		_diacbX_->n3 += 1;
		if( _diacbX_->n3 > OPADRX )
			goto L_8710;
		opadroX_.opadro[_diacbX_->n3-One] = swkadX_.swkad;
		opadroX_.sconpo[_diacbX_->n3-One] = _flowckX_->n6jim;
		}
	elscnpX_.elscnp[_flowckX_->n6jim-One] = _flowckX_->n6jim;

L_8710:
	if( _diacbX_->n3 == (OPADRX + 1) ){
		if( sworkoX_.phrbgo[sworkoX_.phcto-One] > OPADRX )
			sworkoX_.phrbgo[sworkoX_.phcto-One] = OPADRX;
		sworkoX_.phrndo[sworkoX_.phcto-One] = OPADRX;
		opadroX_.opo = OPADRX;
		if( diagsX_.longdi == 1  )
			{
			fprintf( _spec_fp, " T1ELEMLD:  OVERLOADING OPADRO ARRAY. SKIP LOAD AND CONTINUE\n" );
			}
		errlog(pgmnam,8715,501,10);

		}
	return;
} /*end of function*/

