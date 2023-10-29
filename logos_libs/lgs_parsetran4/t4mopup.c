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
/********************************************************************
 *  T4MOPUP-  FINAL ASSIGNMENTS AND CLEAN UP BEFORE WRITING OUTPUT
 *            FOR THE NEXT STEP (FPRINT). */

/*  CHANGES:
 *    7/15/93 jal: move the Black Hole counter flag from SCONO(TXBHCT)
 *              to SCON(FPBHCT) so that FPRINT can easily get at it. */


/******************************************************************** */

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



void /*FUNCTION*/ mopup()
{
	static short int bhscxx, hfct, hfx, hfy, oi, opadr, vcad, xx;
	static long int opbyt;

	/*                            SAVE PTR TO LAST OPADR OF LAST PHRASE. */
	sworkoX_.phrndo[sworkoX_.phcto-One] = opadroX_.opo;
	/*                            SAVE PTR TO LAST CLAUSE ELEMENT IN NSWRK2 */
	clsoutX_.clndno[clsnfoX_.clcrnt-One] = sworkoX_.phcto;

	if( diagsX_.longdi == 1 ){
		fprintf( _spec_fp, "\n\n          ------------   %6.6s PARSING COMPLETE   -------------\n\n     ----   BEFORE CLEANING UP THE OUTPUT   ----\n", 
		  passesX_.pssnam );
		fprintf( _spec_fp, " TOTAL NUMBER OF CLAUSES IN SENTENCE = %3d\n", 
		  clsnfoX_.cltotl );
		fprintf( _spec_fp, " TOTAL NUMBER OF PHRASES (PHCTO)      = %3d\n TOTAL NUMBER OF OPADR2 ELEMENTS (OPO)= %3d\n", 
		  sworkoX_.phcto, opadroX_.opo );
		/*                            PRINT CLSNFO AND CLSCON IF NOT EMPTY. */
		diagno(10);
		/*                            PRINT PHRASE,AND TARGET ARRAYS */
		diagno(8);
		}


	/*                            RETURN CLAUSES MOVED IN TRAN2 TO MAIN CLAUS
	 *                            CLRTRN HANDLES DIAGNOSTICS AS WELL
	 *                            BUT, IF PASS1 OF 2 PASS STRATEGY DONT CHANG
	 *                            TARGET OUTPUT, PASS2 USES OPADRI AND IS
	 *                            RESPONSIBLE FOR REAL OPADRO. */
	if( !((passesX_.passfl == 1 && passesX_.passct == 1) && passesX_.passkp == 0) ){

		clrtrn();

		/*                            REMOVE NULL ADRRESSES, UNLOAD VC INTO OPADR */

		/*                            CLEAN UP ONLY IF CLRTRN() HAS NOT. */
		if( clsnfoX_.cltotl <= 1 ){

			/*                       TEMPORARY MOVE TO INPUT ARRAYS */
			opbyt = opadroX_.opo*2;
			lmove(opadriX_.opadri,1,opadroX_.opadro,1,opbyt);
			lmove(opadriX_.sconpi,1,opadroX_.sconpo,1,opbyt);
			/*                       INITIALIZE PTRS */
			opadriX_.opi = opadroX_.opo;
			opadroX_.opo = 0;

			for( oi=1; oi <= opadriX_.opi; oi++ ){
				opadr = opadriX_.opadri[oi-One];
				/*                            IS THIS A VC */
				if( opadr >= -120 && opadr <= -100 ){
					vcad = hpdopoX_.hfdopo[oi-One];
					/*                            IS THIS AN OVERFLOW VC */
					if( vcad >= HFPOLO && vcad <= HFPOHI ){
						/*                            NON-VC.  SINGLE TRANSFER */
						hfy = vcad - HFPOL1;
						hfct = hfdoaX_.hfpoad[hfy-One][HFPADX-One];
						if( hfct != 0 ){
							/*                            SINGL VC ADDRESS. LOAD IF NON-NULL.
							 *                            MOVE ADDRESSES BACK INTO OPADR
							 *                            IF NOT NULL OR VC */
							for( hfx=1; hfx <= hfct; hfx++ ){
								if( hfdoaX_.hfpoad[hfy-One][hfx-One] != -140 &&
								   (hfdoaX_.hfpoad[hfy-One][hfx-One] < -120 ||
								    hfdoaX_.hfpoad[hfy-One][hfx-One] > -100) ){
									opadroX_.opo += 1;
									opadroX_.opadro[opadroX_.opo-One] = hfdoaX_.hfpoad[hfy-One][hfx-One];
									opadroX_.sconpo[opadroX_.opo-One] = hfdoaX_.sconhf[hfy-One][hfx-One];
									}
								}
							}
						}
					else if( vcad != 0 && vcad != -140 ){
						opadroX_.opo += 1;
						opadroX_.opadro[opadroX_.opo-One] = vcad;
						opadroX_.sconpo[opadroX_.opo-One] = opadriX_.sconpi[oi-One];
						}
					}
				else if( opadr != -140 ){
					opadroX_.opo += 1;
					opadroX_.opadro[opadroX_.opo-One] = opadr;
					opadroX_.sconpo[opadroX_.opo-One] = opadriX_.sconpi[oi-One];
					}
				}
			}
		/*                            Move Black Hole count flag from SCONO to
		 *                            SCON for FPRINT. */
		bhscxx = TXBHCT - SCONX1;
		for( xx=1; xx <= prtscoX_.sct; xx++ ){
			if( sconX_.scolnk[xx-One] == 0 ){
				sconX_.scon[xx-One][FPBHCT-One] = 0;
				}
			else{
				sconX_.scon[xx-One][FPBHCT-One] = sconX_.scono[sconX_.scolnk[xx-One]-
				  One][bhscxx-One];
				}
			}
		}

	/*                            PRINT DIAGNOSTICS FOR END OF SENTENCE */

		if( diagsX_.anydi == 1 ){
			fprintf( _spec_fp, "\n\n ***********     %6.6s PROCESSING COMPLETE      **********\n\n\n", 
			  passesX_.pssnam );
			scnprt();
			diagno(9);
			if( passesX_.passfl == 1 && passesX_.passct == 1 )
				diagno(18);
		}

		return;
} /*end of function*/

