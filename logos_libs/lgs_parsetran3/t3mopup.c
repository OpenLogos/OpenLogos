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
program. If not, write to Globalware AG, Hospitalstraße 6, D-99817 Eisenach.

Linux port modifications and additions by Bernd Kiefer, Walter Kasper,
Deutsches Forschungszentrum fuer kuenstliche Intelligenz (DFKI)
Stuhlsatzenhausweg 3, D-66123 Saarbruecken
*/
	/*     T2MOPUP PUTS THE FINISHING TOUCHES (IF NEEDED) ONCE A SENTENCE
	 *     HAS BEEN PROCESSED.  A LOT OF WRITE STATEMENTS, ETC... */

	 /* *         Rev 1.2   10/19/94 17:16:02   jonathan
 * *      Request Number:	PR0000
 * *      Description:
 * *      If last SWORK is not 20 010 ?? then one is created, and OPADRO address
 * *      is added. This fix set the new SCONPO which was unset before.
 * *
 * *> */


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



void /*FUNCTION*/ mopup()
{
	static short int junk;
	static long int tempi4;
	static short iz = 0;

	/*+1              link SWORKO to consituents in SWORKI 4/01/94 jal
	 *                necessary cuz SWKLNK set only when PHCTO is incremented */
	swklnkX_.swklnk[sworkoX_.phcto-One] = sworkX_.phct;

	if( sworkoX_.phcto == sw26bkX_.phr26 )
		set26();


	/*        CHECK FOR EOS SWORK.  IF NONE - CREATE ONE. */

	if( (sworkoX_.sworko[sworkoX_.phcto-One][1-One] != 20) ||
		(sworkoX_.sworko[sworkoX_.phcto-One][2-One] != 10) ){
		sworkoX_.phrndo[sworkoX_.phcto-One] = opadroX_.opo;
		/*+1              link SWORKO to consituents in SWORKI 4/01/94 jal */
		swklnkX_.swklnk[sworkoX_.phcto-One] = sworkX_.phct - 1;
		swklnkX_.swklnk[sworkoX_.phcto+1-One] = sworkX_.phct;
		sworkoX_.phcto += 1;
		flowckX_.n6jim = sworkX_.phct;
		if( opadroX_.opo < OPADRX ){
			opadroX_.opo += 1;
			opadroX_.opadro[opadroX_.opo-One] = -140;
			opadroX_.sconpo[opadroX_.opo-One] = 1;
			}
		sworkoX_.phrbgo[sworkoX_.phcto-One] = opadroX_.opo;
		sworkoX_.phrndo[sworkoX_.phcto-One] = opadroX_.opo;
		sworkoX_.phrhdo[sworkoX_.phcto-One] = sworkX_.phrhed[flowckX_.n6jim-One];
		sworkoX_.sworko[sworkoX_.phcto-One][1-One] = 20;
		sworkoX_.sworko[sworkoX_.phcto-One][2-One] = 10;
		sworkoX_.sworko[sworkoX_.phcto-One][3-One] = sworkX_.swork[flowckX_.n6jim-One][3-One];
		sworkoX_.sworko[sworkoX_.phcto-One][4-One] = sworkX_.swork[flowckX_.n6jim-One][4-One];
		}

	/*                            SAVE INFO FOR LAST CLAUSE */
	tempi4 = CLSCEL*2;
	if( clsnfoX_.cltotl > 1 )
		lmove(&clsnfoX_.clcell[clsnfoX_.clcrnt-One][1-One],1,vbdataX_.vbcell, CLSCL1,tempi4);
	clsoutX_.clndno[clsnfoX_.clcrnt-One] = sworkoX_.phcto;

	sworkoX_.phrndo[sworkoX_.phcto-One] = opadroX_.opo;


	/*+      add Translation memory, Noun Phrase call    04/01/94 jal */
	if( tmnphrX_.tmnpfl == 1 )
		npslct(3,sworkoX_.sworko,sworkoX_.phrbgo,sworkoX_.phrndo,sworkoX_.phcto,
		  opadroX_.opadro,opadroX_.sconpo,hpdopoX_.hfdopo,&junk);


		if( diagsX_.anydi == 1 ){
			scnprt();
			diagno(10);
			diagno(11);
			diagno(8);
			diagno(12);

			if( passesX_.passfl == 1 && passesX_.passct == 1 )
				diagno(18);
			if( diagsX_.deepdi == 1 )
				diagno(11);
		}


	return;
} /*end of function*/

