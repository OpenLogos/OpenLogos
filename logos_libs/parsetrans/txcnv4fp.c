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
#include <fcrt.h>
#include "trans.h"
#include "logoslib.h"
#include "projexts.h"

/*  CNV4FP() =  Convert Tran data 4 Fprint compatibility */

/*     called by T4mopup at end of TRAN4 processing
 *     called by T1mopup at end of TRAN1 processing if WordSearch only.
 *                For Wordsearch, Tran1 output goes directly to FPRINT. */

#include "parsetrans_ext.h"
#include <jbctrl.h>


void /*FUNCTION*/ cnv4fp()
{
	static short int bhscxx, hfct, hfx, hfy, oi, opadr, sct, vcad,xx;
	static long int opbyt;

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
	/*                            Move Black Hole count flag from SCONO to
	 *                            SCON for FPRINT. */
	bhscxx = TXBHCT - SCONX1;
	for( xx=1; xx <= sct; xx++ ){
		if( sconX_.scolnk[xx-One] == 0 ){
			sconX_.scon[xx-One][FPBHCT-One] = 0;
			}
		else{
			sconX_.scon[xx-One][FPBHCT-One] = sconX_.scono[sconX_.scolnk[xx-One]-
			  One][bhscxx-One];
			}
		}
	return;
} /*end of function*/

