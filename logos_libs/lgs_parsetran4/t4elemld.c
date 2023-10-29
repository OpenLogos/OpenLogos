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
	/*          LOAD NSWRK2 AND OPADRO FOR VTR ELEMENT */
	/*  CHANGES:
	 *     10/10/91 JAL: SET SCON59 = TARG PAT FOR HIGH FREQ CONST LOAD
	 *     08/15/90 *JAL*:  FOR TR4 ONLY UPDATE PHRBGO() IN MAIN WHEN
	 *         PARSING A NEW CLAUSE. SINCE THERE IS NO -20SW IN TR4,
	 *         KNOWING PHRASE BOUNDARIES IS DIFFICULT.
	 *     04/17/87 *R1685RKH*  CHANGE T1-4 SWORK LIMIT FROM 50 TO
	 *     08/24/86 *R1561DSD: 100 SCONS
	 *     12/03/85 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <fcrt.h>
#include "project.h"
#include "logoslib.h"
#include "trans.h"
#include "projexts.h"
#include "parsetrans.h"
#include <string.h>
#include <jbctrl.h>



void /*FUNCTION*/ elemld()
{
	static short int adrsav;
	static long int _l0, zaplen;
	static short zero = 0;
	static char pgmnam[9] = "T4ELEMLD";
	static short kz = 0;
	static short om = 0;
	static short kz2 = 0;
	static short tmp = 0;
	static short sconlc = 0;
	int taddress,dictype;


	flowckX_.n6jim = flowckX_.im1 - vtrnX_.vtrn;

	/*   N6JIMS IS LAST PHRASE LOADED */
	flowckX_.n6jims = flowckX_.n6jim;

	sconlc = sworkX_.phrhed[flowckX_.n6jim-One];


	flowckX_.phrstr = sworkX_.phrbeg[flowckX_.n6jim-One];
	flowckX_.phrlst = sworkX_.phrend[flowckX_.n6jim-One];
	tmp = 0;
	om = 0;
	/*+         SET PHRBGO IN MAIN() ONLY, WHEN ENTERING    *08/15/91*JAL*
	 *          A NEW CLAUSE. TR4 HAS NO -20  SO DIFFICULT TO BOUND PHRASES.
	 *     IF (PHCTO.GT.0) PHRNDO(PHCTO) = OPO
	 *     PHCTO = PHCTO + 1
	 *     PHRBGO(PHCTO) = OPO + 1
	 *-                                                      *08/15/91*JAL* */

	for( kz=flowckX_.phrstr; kz <= flowckX_.phrlst; kz++ ){

		/* ------------------ BEGINNING OF SUPERLATIVE HANDLING ---------------- */

		/*     LOAD CONSTANTS AND VC'S NEEDED IN THE PHRASE FOR COMP/SUP. THIS IS
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
					goto L_6890;
				tmp = sw38bkX_.compld;
				/*     FIRST FUNCTION
				 *     IF COMPLD WAS A 99, IT MEANS THAT COMPLD MUST BE ASSIGNED AN APPRO
				 *     CONSTANT VALUE (ONLY IF OFL3 IS A 8 OR A 9). */
				if( sconX_.scon[kz2-One][12-One] != 8 && sconX_.scon[kz2-One][12-One] != 
				  9 )
					goto L_6896;
				if( sconX_.scon[kz2-One][12-One] == 8 )
					sw38bkX_.compld = -263;
				if( sconX_.scon[kz2-One][12-One] == 9 )
					sw38bkX_.compld = -330;

				/*     IS PRECEDING ELEMENT A VC 110? IF NO, LOAD ONE. */

				if( opadroX_.opadro[opadroX_.opo-One] == -110 )
					goto L_6894;
				}
			else if( kz2 != sconlc ){
				goto L_6890;
				}
			prtscoX_.sct += 1;
			/*+                                        *B0305JGB */
			if( prtscoX_.sct > SCONY ){
				sw38bkX_.compld = 0;
				prtscoX_.sct = SCONY;
				prtscoX_.scterr += 1;
				if( opswX_.sw[3-One] == 1 || opswX_.sw[20-One] == 1 )
					{
					fprintf( _spec_fp, " T4ELEMLD, OVERLOADING SCON ARRAY, SCTERR =%4d\n", 
					  prtscoX_.scterr );
					}
				if( prtscoX_.scterr == 1 )
					errlog(pgmnam,6892,500,13);
				goto L_6890;
				}
			else{
				/*-                                        *B0305JGB
				 *+                                               08/24/86  *R1561DSD */
				zaplen = 2*(SCONX - SCONX1);
				zapit(&sconX_.scon[prtscoX_.sct-One][1-One],2*SCONX1,
				  (byte)zero);
				if( sconX_.scolnk[prtscoX_.sct-One] <= ELMMAX )
					zapit(&sconX_.scono[sconX_.scolnk[prtscoX_.sct-One]-
					  One][1-One],zaplen,(byte)zero);
				/*-                                               08/24/86  *R1561DSD */
				sconX_.scon[prtscoX_.sct-One][1-One] = 110;
				opadroX_.opo += 1;
				if( opadroX_.opo > OPADRX )
					goto L_8710;
				opadroX_.sconpo[opadroX_.opo-One] = prtscoX_.sct;
				opadroX_.opadro[opadroX_.opo-One] = -110;
				}

			/*     NOW FILL THE VC 110 */
L_6894:
			hpdopoX_.hfdopo[opadroX_.opo-One] = sw38bkX_.compld;
			sconX_.scon[opadroX_.sconpo[opadroX_.opo-One]-One][2-One] = -sw38bkX_.compld;
			/*     NOW DON'T SET OM FLAG IF THE COMPLD WASN'T A 99 TO BEGIN WITH
			 *     ADDITION OF 'LY' ONLY APPLIES TO ADVERBS.) */
			if( tmp != 99 ){
				sw38bkX_.compld = 0;
				goto L_6890;
				}
L_6896:
			if( sconX_.scon[kz2-One][1-One] == 4 ){
				if( sconX_.scon[kz2-One][13-One] == 15 )
					om = 1;
				sw38bkX_.compld = tmp;
				}
			}

		/*----------------- END OF SUPERLATIVE HANDLING ---------------------- */

L_6890:
		opadroX_.opo += 1;
		if( opadroX_.opo > OPADRX )
			goto L_8710;

		if( sconlc == opadriX_.sconpi[kz-One] )
			sworkoX_.phrhdo[sworkoX_.phcto-One] = opadroX_.opo;
		opadroX_.opadro[opadroX_.opo-One] = opadriX_.opadri[kz-One];
		opadroX_.sconpo[opadroX_.opo-One] = opadriX_.sconpi[kz-One];
		hpdopoX_.hfdopo[opadroX_.opo-One] = hpdopiX_.hfdopi[kz-One];

		if( vbdataX_.k3p1 != 0 ){
			if( opadriX_.sconpi[kz-One] != 0 ){
				if( sconX_.scon[opadriX_.sconpi[kz-One]-One][1-One] >= 0 ){

					/*          SET IN ALTERNATE WORD CLASS */

					if( !(sconX_.scon[opadriX_.sconpi[kz-One]-One][1-One] >= 99 &&
						  sconX_.scon[opadriX_.sconpi[kz-One]-One][1-One] <= 120) ){

						if( (vbdataX_.k3p1 != 0) && (vbdataX_.k3p1 <= 4) )
							sconX_.scon[opadriX_.sconpi[kz-One]-One][8-One] = vbdataX_.k3p1;

						/*     END TRANN2 SCON LOAD BLOCK
						 *          BELOW IF PARAMETER IS A CONSTANT,SUBSTITUTE IT FOR HEAD OF
						 *                          PHRSE IN OPADRI */

						if( vbdataX_.k3p1 >= 121 ){
							if( opadriX_.sconpi[kz-One] == sworkX_.phrhed[flowckX_.n6jim-One] ){

								/*   IF SCON 14 EQUALS 1, OPADRI HAS ALREADY BEEN TRANSFORMED BY SW22
								 *     IF (SCON(14,SCONPI(KZ)) .EQ. 1) GOTO 6900 */

								adrsav = opadroX_.opadro[opadroX_.opo-One];
								opadroX_.opadro[opadroX_.opo-One] = -vbdataX_.k3p1;

								/*       SUBSTITUTE CONSTANT AND ITS GOVERNING SCON VALUES */
								taddress = vbdataX_.k3p1;
								dictype = 2;
								errvrsX_.err = TARG_CODES(&trgflgX_.trgflg,&dictype,&taddress,
									LOGCC,
								  hfreqX_.hfdo,diagsX_.longdi, _spec_fp);
								if( errvrsX_.err == 0 ){
									sconX_.scon[opadriX_.sconpi[kz-One]-One][3-One] = 0;

									/*           SET IN GENDER FOR NOUN CONSTANTS */
									if( hfreqX_.hfdo[4-One] != 0 )
										sconX_.scon[opadriX_.sconpi[kz-One]-
										  One][4-One] = hfreqX_.hfdo[4-One];
									/*+                    SET SCON59 = TARG MAIN PAT,TCPATM(1) 10/10/91*JAL* */
									sconX_.scono[sconX_.scolnk[opadriX_.sconpi[kz-One]-
									  One]-One][59-SCONX1-One] = hfreqX_.hfdo[7-One];
									}
								else{
									errlog(pgmnam,6890,taddress,6);
									opadroX_.opadro[opadroX_.opo-One] = adrsav;
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
			if( opadroX_.opo > OPADRX )
				goto L_8710;
			opadroX_.opadro[opadroX_.opo-One] = -233;
			opadroX_.sconpo[opadroX_.opo-One] = sconlc;
			om = 0;
			}
		}
	sw38bkX_.compld = 0;


L_8710:
	if( opadroX_.opo == (OPADRX + 1) ){
		if( sworkoX_.phrbgo[sworkoX_.phcto-One] > OPADRX )
			sworkoX_.phrbgo[sworkoX_.phcto-One] = OPADRX;
		sworkoX_.phrndo[sworkoX_.phcto-One] = OPADRX;
		opadroX_.opo = OPADRX;
		if( diagsX_.longdi != 0)
			{
			fprintf( _spec_fp, " T4ELEMLD: OVERLOADING OPADRO ARRAY. SKIP LOAD AND CONTINUE.\n" );
			}
		errlog(pgmnam,8715,501,10);

		}
	return;
} /*end of function*/

