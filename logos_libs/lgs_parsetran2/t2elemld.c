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
	 *   03/08/94 jal: link SWORKO to consituents in SWORKI via SWKLNK().
	 *     10/10/91 JAL: SET SCON59 = TARG PAT FOR HIGH FREQ CONST LOAD
	 *     CHG: 09/08/89 JAL:  ERROR MESSAGE IF LOADING VC WHILE PARSE IS
	 *                         IN "CLAUSE RELOCATIONS STATE".
	 *     CHG: 04/17/87 *R1685RKH*  CHANGE T1-4 SWORK LIMIT FROM 50 TO
	 *      CHG 08/24/86 *R1561DSD: 100 SCONS
	 *      CHG 05/15/86 *B0406DSD: RESET OM
	 *      CHG 12/03/85 */
	/*     SUBROUTINE FOR LOADING AN ELEMENT INTO THE OPADRI */
	/*     THIS SUBROUTINE IS CALLED BY:
	 *          THE LOOPCK LOGIC IN THE DRIVER PROGRAM,
	 *          THE VTRPRO SUBROUTINE,
	 *          SW12, SW21, SW25, SW36, SW38 SUBROUTINES. */
	/*     BASICALLY, THE OPADRI, HFDOPI AND THE SCONPI ARRAY GET FILLED
	 *     HERE FOR THE PHRBEG HEAD.
	 *     THERE IS A LARGE SPECIAL BLOCK OF LOGIC HERE FOR HANDLING
	 *     SUPERLATIVES OF ENGLISH TARGETS ( MORE, -LY ), WHICH COULD BE
	 *     LUMPED INTO A SPECIAL SUBROUTINE. */
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
#include "parsetrans.h"
#include <jbctrl.h>


void /*FUNCTION*/ elemld()
{
	static short int adrsav;
	static long int _l0, zaplen;
	int taddress,dictype;
	static short zero = 0;
	static char pgmnam[9] = "T2ELEMLD";
	static short kz = 0;
	static short om = 0;
	static short kz2 = 0;
	static short tmp = 0;
	static short sconlc = 0;

	 /*                               ERROR TO LOAD WHILE PARSE IS IN
	 *                               "CLAUSE RELOCATION STATE" */
	if( clsmovX_.clmcnt != 0 ){
		if( diagsX_.shrtdi != 0){
			fprintf( _spec_fp, "              ***** ERROR ******\n   ELEMENT LOAD WHILE PARSING A CLAUSE FOR RELOCATION\n              ******************\n" );
			}
		zaplen = clsmovX_.clmcnt;
		errlog(pgmnam,44,zaplen,5);
		}


	/*        LOAD SWORKO AND OPADRO FOR VTR ELEMENT */

	flowckX_.n6jim = flowckX_.im1 - vtrnX_.vtrn;

	/*        N6JIMS IS LAST PHRBEG LOADED */

	flowckX_.n6jims = flowckX_.n6jim;
	sconlc = sworkX_.phrhed[flowckX_.n6jim-One];

	/*        IS EACH ELEMENT A PHRBEG??? */

	if( sw18bkX_.phrnew != 0 ){
		if( sworkoX_.phcto == sw26bkX_.phr26 )
			set26();
		if( sworkoX_.phcto > 0 ){
			sworkoX_.phrndo[sworkoX_.phcto-One] = opadroX_.opo;
			/*+1                link SWORKO to consituents in SWORKI 3/08/94 jal */
			swklnkX_.swklnk[sworkoX_.phcto-One] = flowckX_.n6jim - 1;
			}
		sworkoX_.phcto += 1;
		sworkoX_.sworko[sworkoX_.phcto-One][1-One] = sworkX_.swork[flowckX_.n6jim-One][1-One];
		sworkoX_.sworko[sworkoX_.phcto-One][2-One] = sworkX_.swork[flowckX_.n6jim-One][2-One];
		sworkoX_.sworko[sworkoX_.phcto-One][3-One] = sworkX_.swork[flowckX_.n6jim-One][3-One];
		sworkoX_.sworko[sworkoX_.phcto-One][3] = sworkX_.swork[flowckX_.n6jim-One][3];
		sworkoX_.phrbgo[sworkoX_.phcto-One] = opadroX_.opo + 1;
		sworkoX_.phrhdo[sworkoX_.phcto-One] = sworkX_.phrhed[flowckX_.n6jim-One];
		sw25bkX_.tw25 = 0;
		}

	flowckX_.phrstr = sworkX_.phrbeg[flowckX_.n6jim-One];
	flowckX_.phrlst = sworkX_.phrend[flowckX_.n6jim-One];
	om = 0;
	tmp = 0;

	for( kz=flowckX_.phrstr; kz <= flowckX_.phrlst; kz++ ){

		/*     LOAD CONSTANTS AND VC'S NEEDED IN THE PHRBEG FOR COMP/SUP. THIS IS
		 *     DIRECTLY RELATED TO THE -38 050 FUNCTION.
		 *     ONE TEST IS TO SEE IF "MORE, MOST" SHOULD BE INCLUDED BEFORE THE W
		 *     WHICH WAS SENT TO BE LOADED.
		 *     THE OTHER IS TO SEE IF "LY" SHOULD BE ADDED AFTER THE WORD.
		 *     THIS IS DONE IN TRANS 2 AND 4. */

		/*     IF COMPLD IS ALREADY SET, THIS MEANS THAT THE 38 050 HAS ALREADY A
		 *     THE VALUE AND WE ONLY NEED TO CREATE A VC 110 TO LOAD IT IN. */

		if( sw38bkX_.compld != 0 ){

			kz2 = opadriX_.sconpi[kz-One];
			if( sw38bkX_.compld == 99 ){
				if( sconX_.scon[kz2-One][1-One] != 4 )
					goto L_8690;
				tmp = sw38bkX_.compld;

				/*     FIRST FUNCTION
				 *     IF COMPLD WAS A 99, IT MEANS THAT COMPLD MUST BE ASSIGNED AN APPRO
				 *     CONSTANT VALUE (ONLY IF OFL3 IS A 8 OR A 9). */

				if( sconX_.scon[kz2-One][12-One] != 8 && sconX_.scon[kz2-One][12-One] != 9 )
					goto L_8686;
				if( sconX_.scon[kz2-One][12-One] == 8 )
					sw38bkX_.compld = -263;
				if( sconX_.scon[kz2-One][12-One] == 9 )
					sw38bkX_.compld = -330;

				/*        IS PRECEDING ELEMENT A VC 110? IF NO, LOAD ONE. */

				if( opadroX_.opadro[opadroX_.opo-One] == -110 )
					goto L_8684;
				}
			else if( kz2 != sconlc ){
				goto L_8690;
				}



			prtscoX_.sct += 1;
			if( prtscoX_.sct > SCONY ){
				prtscoX_.sct = SCONY;
				prtscoX_.scterr += 1;
				if( diagsX_.shrtdi != 0)
					{
					fprintf( _spec_fp, " T2ELEMLD, OVERLOADING SCON ARRAY, SCTERR =%4d\n", 
					  prtscoX_.scterr );
					}
				if( prtscoX_.scterr == 1 )
					errlog(pgmnam,8683,500,13);

				sw38bkX_.compld = 0;
				goto L_8690;
				}
			else{
				zaplen = 2*(SCONX - SCONX1);
				zapit(&sconX_.scon[prtscoX_.sct-One][0],2*SCONX1,(byte)zero);
				if( sconX_.scolnk[prtscoX_.sct-One] <= ELMMAX )
					zapit(&sconX_.scono[sconX_.scolnk[prtscoX_.sct-One]-One][0],zaplen,(byte)zero);


				sconX_.scon[prtscoX_.sct-One][1-One] = 110;

				opadroX_.opo += 1;
				if( opadroX_.opo > OPADRX )
					break;

				opadroX_.sconpo[opadroX_.opo-One] = prtscoX_.sct;
				opadroX_.opadro[opadroX_.opo-One] = -110;
				}

			/*        NOW FILL THE VC 110 */

L_8684:
			hpdopoX_.hfdopo[opadroX_.opo-One] = sw38bkX_.compld;
			sconX_.scon[opadroX_.sconpo[opadroX_.opo-One]-One][2-One] = -sw38bkX_.compld;
			sw38bkX_.compld = tmp;

			/*        NOW DON'T SET THE OM FLAG IF COMPLD WASN'T A 99 TO BEGIN WITH
			 *         ADDITION OF 'LY' ONLY APPLIES TO ADVERBS.) */

			if( tmp != 99 ){
				sw38bkX_.compld = 0;
				goto L_8690;
				}

L_8686:
			if( sconX_.scon[kz2-One][13-One] == 15 )
				om = 1;

			}

L_8690:
		opadroX_.opo += 1;
		if( opadroX_.opo > OPADRX )
			break;

		opadroX_.opadro[opadroX_.opo-One] = opadriX_.opadri[kz-One];
		opadroX_.sconpo[opadroX_.opo-One] = opadriX_.sconpi[kz-One];
		hpdopoX_.hfdopo[opadroX_.opo-One] = hpdopiX_.hfdopi[kz-One];
		/*+                                                         *R1768MBS
		 *   IF THIS ELEMENT HAS BEEN SAVED BY A -36 033 SAVE THE OPADRI
		 *    POSITION OF THE PHRBEG HEAD */
		if( flowckX_.n6jim == sav36sX_.sav36s[0] ){
			if( opadriX_.sconpi[kz-One] == sworkX_.phrhed[flowckX_.n6jim-One] )
				sav36sX_.sav36s[10] = opadroX_.opo;
			}
		/*-                                                         *R1768MBS */

		if( vbdataX_.k3p1 != 0 ){
			if( opadriX_.sconpi[kz-One] != 0 ){
				if( sconX_.scon[opadriX_.sconpi[kz-One]-One][1-One] >= 0 ){

					/*        SET IN ALTERNATE WORD CLASS */

					if( !(sconX_.scon[opadriX_.sconpi[kz-One]-One][1-One] >= 99 &&
						  sconX_.scon[opadriX_.sconpi[kz-One]-One][1-One] <= 120) ){

						/*        IF 36052 HAS BEEN CALLED, ONLY SET A.W.C. OF THE HEAD */

						if( !(sw36bkX_.exawc == 1 && sconlc != opadriX_.sconpi[kz-One])){
							if( vbdataX_.k3p1 != 0 && vbdataX_.k3p1 <= 4 )
								sconX_.scon[opadriX_.sconpi[kz-One]-One][8-One] = vbdataX_.k3p1;
							sw36bkX_.exawc = 0;

							/*     END TRAN2 SCON LOAD BLOCK
							 *         BELOW IF PARAMETER IS A CONSTANT, SUBSTITUTE IT FOR HEAD OF
							 *         PHRBEG IN OPADRI */

							if( vbdataX_.k3p1 >= 121 ){
								if( opadriX_.sconpi[kz-One] == sworkX_.phrhed[flowckX_.n6jim-One] ){

									/*   IF SCON 14 EQUALS 1, OPADRI HAS ALREADY BEEN TRANSFORMED BY SW22
									 *     IF (SCON(14,SCONPI(KZ)) .EQ. 1) GOTO 8700 */

									adrsav = opadroX_.opadro[opadroX_.opo-One];
									opadroX_.opadro[opadroX_.opo-One] = -vbdataX_.k3p1;

									/*       SUBSTITUTE CONSTANT AND ITS GOVERNING SCON VALUES */

									taddress = vbdataX_.k3p1;
									dictype = 2;
									errvrsX_.err = TARG_CODES(&trgflgX_.trgflg,&dictype,&taddress,
											cmpsmcX_.cmpcod[flowckX_.n6jim-One][prctX_.js[sconX_.scolnk[flowckX_.n6jim-One]-One]-One],
												   hfreqX_.hfdo,diagsX_.longdi, _spec_fp);
									if( errvrsX_.err == 0 ){
										sconX_.scon[opadriX_.sconpi[kz-One]-One][3-One] = 0;
										/*        SET IN GENDER FOR NOUN CONSTANTS */
										if( hfreqX_.hfdo[3] != 0 )
											sconX_.scon[opadriX_.sconpi[kz-One]-One][3] = hfreqX_.hfdo[3];
										/*+                    SET SCON59 = TARG MAIN PAT,TCPATM(1) 10/10/91*JAL* */
										sconX_.scono[sconX_.scolnk[opadriX_.sconpi[kz-One]-
										  One]-One][59-SCONX1-One] = hfreqX_.hfdo[6];
										}
									else{
										errlog(pgmnam,8700,taddress,6);
										opadroX_.opadro[opadroX_.opo-One] = adrsav;
										}
									}
								}
							}
						}
					}
				}
			}

		/*        LOAD IN LY CONSTANT IF NEEDED. */
		if( om == 1 ){

			opadroX_.opo += 1;
			if( opadroX_.opo <= OPADRX ){

				opadroX_.opadro[opadroX_.opo-One] = -233;
				opadroX_.sconpo[opadroX_.opo-One] = sconlc;
				om = 0;
				}
			}
		}
	if( opadroX_.opo > OPADRX ){
		if( sworkoX_.phrbgo[sworkoX_.phcto-One] > OPADRX )
			sworkoX_.phrbgo[sworkoX_.phcto-One] = OPADRX;
		sworkoX_.phrndo[sworkoX_.phcto-One] = OPADRX;
		opadroX_.opo = OPADRX;
		if( diagsX_.shrtdi != 0)
			{
			fprintf( _spec_fp, " T2ELEMLD:  OVERLOADING OPADRO ARRAY.  SKIP LOAD AND CONTINUE.\n" );
			}
		errlog(pgmnam,8700,501,10);
		}
	else{

		if( sw38bkX_.r38 != 1 )
			sw38bkX_.compld = 0;

		}
	return;
} /*end of function*/

