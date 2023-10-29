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
	/*     CHANGES:
	 *      4/30/93 jal: for pass 2, restore SCON 2,11,13 saved from pass1.
	 *      05/02/87 *R1691RKH*  Deactivate SCONPO for -140 in OPADR
	 *      04/17/87 *R1685RKH*  Change T1-4 SWORK1 limit from 50 to
	 *      08/17/86 *R1561DSD: 100 SCONS
	 *      11/23/85 */
	/*     T1MOPUP PUTS THE FINISHING TOUCHES (IF NEEDED) ONCE A SENTENCE
	 *     HAS BEEN FULLY PROCESSED. A LOT OF WRITE STATEMENTS,ETC... */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */

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

EXTERN struct t_onevX_ {
	short int onev[3][4];
	}	onevX_;


void /*FUNCTION*/ mopup()
{
	static short int bypas, junk, tempi4;
	static short i = 0;
	static short j = 0;
	static short gb = 0;
	static short iz = 0;
	int crtUnit = 0;
	int element = 0;
	int sconptr = 0;


	/*    IF A VC HAS BEEN ADVANCED SET, BUT NOT ACTUALLY LOADED, NULL IT */
	if( sw44bkX_.pre44 != 0 ){
		for( gb=1; gb <= 5; gb++ ){
			if( sw44bkX_.ad44vc[gb-One][2-One] != 0 ){
				opadroX_.opadro[sw44bkX_.ad44vc[gb-One][2-One]-One] = -140;
				hpdopoX_.hfdopo[sw44bkX_.ad44vc[gb-One][2-One]-One] = 0;
				/*+ Deactivate SCONPO for -140 in OPADRO         RKH  05/02/87   R1691 */
				opadroX_.sconpo[sw44bkX_.ad44vc[gb-One][2-One]-One] = 1;
				}
			}
		}

	opadroX_.opo = diacbX_.n3;
	if( sworkoX_.phcto == sw26bkX_.phr26 )
		t1se26();

	/*   CHECK FOR EOS SWORKO (20 010 -1). IF NONE - CREATE ONE. */
	if( sworkoX_.sworko[sworkoX_.phcto-One][1-One] != 20 || 
		sconX_.scon[sworkoX_.phrhdo[sworkoX_.phcto-One]-One][13-One] != 10 ){
		sworkoX_.phrndo[sworkoX_.phcto-One] = opadroX_.opo;
		sworkoX_.phcto += 1;
		flowckX_.n6jim = swork1X_.phct;
		sworkoX_.sworko[sworkoX_.phcto-One][1-One] = 20;
		sworkoX_.sworko[sworkoX_.phcto-One][2-One] = 10;
		sworkoX_.sworko[sworkoX_.phcto-One][3-One] = swork1X_.swork1[flowckX_.n6jim-One][6-One];
		sworkoX_.sworko[sworkoX_.phcto-One][4-One] = elscnpX_.elscnp[flowckX_.n6jim-One];
		sworkoX_.phrhdo[sworkoX_.phcto-One] = elscnpX_.elscnp[flowckX_.n6jim-One];
		if( opadroX_.opo < OPADRX ){
			opadroX_.opo += 1;
			opadroX_.opadro[opadroX_.opo-One] = -140;
			opadroX_.sconpo[opadroX_.opo-One] = 1;
			}
		sworkoX_.phrbgo[sworkoX_.phcto-One] = opadroX_.opo;
		sworkoX_.phrndo[sworkoX_.phcto-One] = opadroX_.opo;
		}

	/*    ADJUST LAST + 1 PHRASE POSITION */

	sworkoX_.phrndo[sworkoX_.phcto-One] = opadroX_.opo;

	/*     TEST HERE IF WE ARE TO BYPAS TRANS 2,3 AND 4.
	 *     THIS IS TO CATCH SIMPLE CASE OF A SINGLE WC1 SWORK SURROUNDED BY
	 *     BOS AND EOS (WC20) SWORKS. */

	bypas = 0;

	if( sworkoX_.phcto <= 3 ){
		for( j=1; j <= 3; j++ ){
			for( i=1; i <= 3; i++ ){
				if( onevX_.onev[j-One][i-One] != -1 ){
					if( sworkoX_.sworko[j-One][i-One] != onevX_.onev[j-One][i-One] )
						goto L_8871;
					}
				}
			}
		bypas = 1;
		}

L_8871:

	/*                   IF word search then set NP list and convert
	 *                   output arrays for FPRINT compatibility
	 */
	if (jcaddX_.wrdsrc == 1 && (passesX_.passfl != 1 || passesX_.passct == 2) )
	{
		if( diagsX_.longdi == 1 ){
			fprintf( _spec_fp, "\n\n -----------     %6.6s PROCESSING COMPLETE (before reordering for term search)      ----------\n", passesX_.pssnam );
			scnprt();
			diagno(9);
			diagno(8);
			diagno(11);
			if( passesX_.passfl == 1 && passesX_.passct == 1 )
				diagno(13);
			}

		/* since function npslct() modifies the SWORKO structure and term search needs it as it is now,
		   we save the necessary info into another structure
		*/
		tran1Units_.totalSworkElements = sworkoX_.phcto;

		for (crtUnit=0; crtUnit<sworkoX_.phcto; crtUnit++)
		{
			tran1Units_.units[crtUnit].headWordPOS = sworkoX_.sworko[crtUnit][0];
			tran1Units_.units[crtUnit].headWordSSSet = sworkoX_.sworko[crtUnit][1];
			tran1Units_.units[crtUnit].headWordForm = sworkoX_.sworko[crtUnit][2];
			tran1Units_.units[crtUnit].headWordSconPointer = sworkoX_.sworko[crtUnit][3];
			element = 0;
			for (sconptr=sworkoX_.phrbgo[crtUnit]; sconptr<=sworkoX_.phrndo[crtUnit]; sconptr++)
			{
				tran1Units_.units[crtUnit].elements[element] = opadroX_.sconpo[sconptr-One];
				tran1Units_.units[crtUnit].opadro[element] = opadroX_.opadro[sconptr-One];
				element++;
			}
		}

		/* this function modifies its parameters! */
		npslct(1,
				 sworkoX_.sworko,
				 sworkoX_.phrbgo,
				 sworkoX_.phrndo,
				 sworkoX_.phcto,
				 opadroX_.opadro,
				 opadroX_.sconpo,
				 hpdopoX_.hfdopo,
				 &junk);

		cnv4fp();
	}


	/*       PARSE: Set up data as expected by PARSE2 */
	if( passesX_.passfl == 1 && passesX_.passct == 1 ){
		for( i=1; i <= elemctX_.elemct; i++ ){
			tempi4 = dct2X_.dct2[prctX_.js[sconX_.scolnk[i-One]-One]-One];
			lmove(&sworkiX_.sworki[i-One][1-One],1,&swork1X_.swrk1i[i-One][tempi4-One],1,6);
			sworkiX_.sworki[i-One][4-One] = i;
			}
		}



	if( diagsX_.anydi == 1 ){
		fprintf( _spec_fp, "\n\n -----------     %6.6s PROCESSING COMPLETE      ----------\n", passesX_.pssnam );
		scnprt();
		diagno(9);
		diagno(8);
		diagno(11);
		if( passesX_.passfl == 1 && passesX_.passct == 1 )
			diagno(13);
		}

	return;
} /*end of function*/


