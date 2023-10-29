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
	 *   04/01/94 jal: link SWORKO to consituents in SWORKI via SWKLNK()
	 *     10/10/91 JAL: SET SCON59 = TARG PAT FOR HIGH FREQ CONST LOAD

	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*     SUBROUTINE FOR LOADING AN ELEMENT INTO THE OPADRI */
	/*     THIS SUBROUTINE IS CALLED BY:
	 *          THE LOOPCK LOGIC IN THE DRIVER PROGRAM,
	 *          THE VTRPRO SUBROUTINE,
	 *          SW12, SW21, SW25, SW38 SUBROUTINES. */
	/*     BASICALLY, THE OPADRI, HFDOPO AND THE SCONPI ARRAY GET FILLED
	 *     HERE FOR THE PHRASE HEAD.
	 *     THERE IS A LARGE SPECIAL BLOCK OF LOGIC HERE FOR HANDLING
	 *     SUPERLATIVES OF ENGLISH TARGETS ( MORE, -LY ), WHICH COULD BE
	 *     LUMPED INTO A SPECIAL SUBROUTINE. */
	/*        LOAD SWORKO AND OPADRO FOR VTR ELEMENT */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <fcrt.h>
#include "project.h"
#include "logoslib.h"
#include "trans.h"
#include "projexts.h"
#include <jbctrl.h>
#include "parsetrans.h"
#include <string.h>





void /*FUNCTION*/ elemld()
{
	static short int adrsav;
	int taddress, dictype;

	struct  {
		short int im1, i3save, i3str, n6, n6jim, n6jims, phrstr, phrlst;
		}	*_flowckX_ = (void*)&flowckX_;

	static char pgmnam[9] = "T3ELEMLD";
	static short kz = 0;
	static short sconlc = 0;

	_flowckX_->n6jim = _flowckX_->im1 - vtrnX_.vtrn;

	/*        N6JIMS IS LAST PHRASE LOADED */

	_flowckX_->n6jims = _flowckX_->n6jim;
	sconlc = sworkX_.phrhed[_flowckX_->n6jim-One];

	/*        IS EACH ELEMENT A PHRASE??? */

	if( sw18bkX_.phrnew != 0 ){
		if( sworkoX_.phcto == sw26bkX_.phr26 )
			set26();
		if( sworkoX_.phcto > 0 ){
			sworkoX_.phrndo[sworkoX_.phcto-One] = opadroX_.opo;
			/*+1                link SWORKO to consituents in SWORKI 4/01/94 jal */
			swklnkX_.swklnk[sworkoX_.phcto-One] = _flowckX_->n6jim - 1;
			}
		sworkoX_.phcto += 1;
		sworkoX_.sworko[sworkoX_.phcto-One][1-One] = sworkX_.swork[_flowckX_->n6jim-One][1-One];
		sworkoX_.sworko[sworkoX_.phcto-One][2-One] = sworkX_.swork[_flowckX_->n6jim-One][2-One];
		sworkoX_.sworko[sworkoX_.phcto-One][3-One] = sworkX_.swork[_flowckX_->n6jim-One][3-One];
		sworkoX_.sworko[sworkoX_.phcto-One][3] = sworkX_.swork[_flowckX_->n6jim-One][3];
		sworkoX_.phrbgo[sworkoX_.phcto-One] = opadroX_.opo + 1;
		sworkoX_.phrhdo[sworkoX_.phcto-One] = sworkX_.phrhed[_flowckX_->n6jim-One];
		sw25bkX_.tw25 = 0;
		}

	_flowckX_->phrstr = sworkX_.phrbeg[_flowckX_->n6jim-One];
	_flowckX_->phrlst = sworkX_.phrend[_flowckX_->n6jim-One];

	for( kz=_flowckX_->phrstr; kz <= _flowckX_->phrlst; kz++ ){
		opadroX_.opo += 1;
		if( opadroX_.opo > OPADRX )
			break;

		opadroX_.opadro[opadroX_.opo-One] = opadriX_.opadri[kz-One];
		opadroX_.sconpo[opadroX_.opo-One] = opadriX_.sconpi[kz-One];
		hpdopoX_.hfdopo[opadroX_.opo-One] = hpdopiX_.hfdopi[kz-One];
		/*+                                                         *R1768MBS
		 *   IF THIS ELEMENT HAS BEEN SAVED BY A -36 033 SAVE THE OPADRI
		 *    POSITION OF THE PHRBEG HEAD */
		if( _flowckX_->n6jim == sav36sX_.sav36s[1-One] ){
			if( opadriX_.sconpi[kz-One] == sworkX_.phrhed[_flowckX_->n6jim-One] )
				sav36sX_.sav36s[11-One] = opadroX_.opo;
			}
		/*-                                                         *R1768MBS */
		if( vbdataX_.k3p1 != 0 ){
			if( opadriX_.sconpi[kz-One] != 0 ){
				if( sconX_.scon[opadriX_.sconpi[kz-One]-One][1-One] >=0 ){

					/*          SET IN ALTERNATE WORD CLASS */

					if( !(sconX_.scon[opadriX_.sconpi[kz-One]-One][1-One] >= 99 
					   && sconX_.scon[opadriX_.sconpi[kz-One]-One][1-One] <= 120) ){
						if( (vbdataX_.k3p1 != 0) && (vbdataX_.k3p1 <= 4) )
							sconX_.scon[opadriX_.sconpi[kz-One]-One][8-One] = vbdataX_.k3p1;

						/*        BELOW: IF PARAMETER IS A CONSTANT, SUBSTITUTE IT FOR HEAD OF
						 *                          PHRASE IN OPADRI */

						if( vbdataX_.k3p1 >= 121 ){
							if( opadriX_.sconpi[kz-One] == sworkX_.phrhed[_flowckX_->n6jim-One] ){

								/*   IF SCON 14 EQUALS 1, OPADRI HAS ALREADY BEEN TRANSFORMED BY SW22
								 *     IF (SCON(14,SCONPI(KZ)) .EQ. 1) GOTO 7980 */
								adrsav = opadroX_.opadro[opadroX_.opo-One];
								opadroX_.opadro[opadroX_.opo-One] = -vbdataX_.k3p1;
								/*       SUBSTITUTE CONSTANT AND ITS GOVERNING SCON VALUES */

								taddress = vbdataX_.k3p1;
								dictype = 2;
								errvrsX_.err = TARG_CODES(&trgflgX_.trgflg,
								               &dictype,&taddress,
											   LOGCC,
								               hfreqX_.hfdo,diagsX_.longdi, _spec_fp);

								if( errvrsX_.err == 0 ){
									sconX_.scon[opadriX_.sconpi[kz-One]-One][3-One] = 0;

									/*           SET IN GENDER FOR NOUN CONSTANTS */
									if( hfreqX_.hfdo[3] != 0 )
										sconX_.scon[opadriX_.sconpi[kz-One]-One][3] = hfreqX_.hfdo[3];
									/*+                    SET SCON59 = TARG MAIN PAT,TCPATM(1) 10/10/91*JAL* */
									sconX_.scono[sconX_.scolnk[opadriX_.sconpi[kz-One]-
									  One]-One][59-SCONX1-One] = hfreqX_.hfdo[6];
									}
								else{
									errlog(pgmnam,7980,taddress,6);
									opadroX_.opadro[opadroX_.opo-One] = adrsav;
									}
								}
							}
						}
					}
				}
			}


		}

	if( opadroX_.opo > OPADRX ){
		/*                         SET UP TO MINIMIZE CHANCE OF BOMB */
		if( sworkoX_.phrbgo[sworkoX_.phcto-One] > OPADRX )
			sworkoX_.phrbgo[sworkoX_.phcto-One] = OPADRX;
		sworkoX_.phrndo[sworkoX_.phcto-One] = OPADRX;
		opadroX_.opo = OPADRX;
		if( diagsX_.deepdi == 1 || diagsX_.longdi == 1 )
			{
			fprintf( _spec_fp, " T3ELEMLD:  OVERLOADING OPADRO ARRAY. SKIP LOAD AND CONTINUE.\n" );
			}
		errlog(pgmnam,8715,501,10);
		}
	return;
} /*end of function*/

