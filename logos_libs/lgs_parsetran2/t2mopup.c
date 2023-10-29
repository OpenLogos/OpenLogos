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
	/*  RETCOD:   0 = OK
	 *           99 = CLAUSE MOVE STACK WAS NOT EMPTY. STACK WAS CLEARED, */
	/*  CHANGES:
	 *   03/08/94 jal: link SWORKO to consituents in SWORKI via SWKLNK().
	 *      03/08/94 jal: add Translation memory, Noun Phrase call
	 *      11/15/91 JAL ADD RETCOD AND RETURN IF CLAUSES REMAIN TO BE MOVED.
	 *      09/15/89 JAL CLAUSE WORK
	 *      04/17/87 *R1685RKH*  CHANGE T1-4 SWORK LIMIT FROM 50 TO
	 *      08/19/86 *R1561DSD: 100 SCONS
	 *      08/29/85 */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*     T2MOPUP PUTS THE FINISHING TOUCHES (IF NEEDED) ONCE A SENTENCE
	 *     HAS BEEN PROCESSED.  A LOT OF WRITE STATEMENTS, ETC... */
	/*     INTEGER TEMPI4 */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
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
#include <jbctrl.h>



void /*FUNCTION*/ mopup(
long int *retcod
)
{
	static short int endns, junk;
	static short iz = 0;
	static char pgmnam[9] = "T2MOPUP ";

	*retcod = 0;
	/*                       IF ANY CLAUSES STILL UNMOVED, FORCE A MOVE
	 *                       AND RETURN TO CONTINUE PARSING. */
	if( clsmovX_.clmcnt > 0 ){
		errlog(pgmnam,10,0,10);
		if( diagsX_.anydi == 1 )
			{
			fprintf( _spec_fp, "\n***********  ERROR  IN T2MOPUP ********************\n AT END OF SENTENCE AND STILL %3d CLAUSES\n LEFT TO MOVE, I.E. MISSING -67002 SWITCH. FORCE A \n MOVE AND CONTINUE PARSING.                        \n***************************************************\n", 
			  clsmovX_.clmcnt );
			}
		/*                        DONT MOVE THE EOS OF THE PARENT IF POSSIBLE. */
		endns = clsnfoX_.clndns[clsnfoX_.clcrnt-One];
		if( (sworkX_.swork[endns-One][1-One] == 20 &&
			 sworkX_.swork[endns-One][2-One] == 10) &&
			 clsnfoX_.clbgns[clsnfoX_.clcrnt-One] < endns )
			clsnfoX_.clndns[clsnfoX_.clcrnt-One] = endns - 1;

		if( diagsX_.anydi == 1 ){
			fprintf( _spec_fp, "\n***********  ERROR  IN T2MOPUP ********************\n AT END OF SENTENCE BUT STILL %3d CLAUSES LEFT\n TO MOVE, I.E. MISSING -67002 SWITCH.  FORCE A\n MOVE AND CONTINUE THE PARSE.           \n***************************************************\n", 
			  clsmovX_.clmcnt );
			fprintf( _spec_fp, " SW67 002:   MOVE A CLAUSE.\n\n     *********  BEFORE MOVING THE CLAUSE *********\n\n" );
			diagno(10);
			diagno(9);
			}
		clmove(20,10,1,1);
		if( diagsX_.anydi == 1 ){
			fprintf( _spec_fp, "\n     *********  AFTER MOVING THE CLAUSE ********* \n\n" );
			diagno(10);
			diagno(9);
			}
		*retcod = 99;
		}
	else{

		/*+1              link SWORKO to consituents in SWORKI 3/08/94 jal
		 *                necessary cuz SWKLNK set only when PHCTO is incremented */
		swklnkX_.swklnk[sworkoX_.phcto-One] = sworkX_.phct;

		if( sworkoX_.phcto == sw26bkX_.phr26 )
			set26();

		/*        CHECK FOR EOS SWORK.  IF NONE - CREATE ONE. */

		if( (sworkoX_.sworko[sworkoX_.phcto-One][1-One] != 20) || 
		    (sworkoX_.sworko[sworkoX_.phcto-One][2-One] != 10) ){
			sworkoX_.phrndo[sworkoX_.phcto-One] = opadroX_.opo;
			/*+1              link SWORKO to consituents in SWORKI 3/08/94 jal */
			swklnkX_.swklnk[sworkoX_.phcto-One] = sworkX_.phct - 1;
			swklnkX_.swklnk[sworkoX_.phcto+1-One] = sworkX_.phct;
			sworkoX_.phcto += 1;
			flowckX_.n6jim = sworkX_.phct;
			if( opadroX_.opo < OPADRX ){
				opadroX_.opo += 1;
				opadroX_.opadro[opadroX_.opo-One] = -140;
				opadroX_.sconpo[opadroX_.opo-One] = 1;
				}
			sworkoX_.phrndo[sworkoX_.phcto-One] = opadroX_.opo;
			sworkoX_.phrbgo[sworkoX_.phcto-One] = opadroX_.opo;
			sworkoX_.phrhdo[sworkoX_.phcto-One] = sworkX_.phrhed[flowckX_.n6jim-One];
			sworkoX_.sworko[sworkoX_.phcto-One][1-One] = 20;
			sworkoX_.sworko[sworkoX_.phcto-One][2-One] = 10;
			sworkoX_.sworko[sworkoX_.phcto-One][3-One] = sworkX_.swork[flowckX_.n6jim-One][3-One];
			sworkoX_.sworko[sworkoX_.phcto-One][4-One] = sworkX_.swork[flowckX_.n6jim-One][4-One];
			}

		sworkoX_.phrndo[sworkoX_.phcto-One] = opadroX_.opo;
		/*                            SAVE INFO FOR LAST CLAUSE */
		clsoutX_.clndno[clsnfoX_.clcrnt-One] = sworkoX_.phcto;


		/*+      add Translation memory, Noun Phrase call    03/08/94 jal */
		if( tmnphrX_.tmnpfl == 1 )
			npslct(2,sworkoX_.sworko,sworkoX_.phrbgo,sworkoX_.phrndo,
			       sworkoX_.phcto,opadroX_.opadro,opadroX_.sconpo,hpdopoX_.hfdopo, &junk);


		if( diagsX_.anydi == 1 ){
			fprintf( _spec_fp, "\n\n -----------     %6.6s PROCESSING COMPLETE      ----------\n", 
			  passesX_.pssnam );
			scnprt();
			diagno(14);
			diagno(13);
			diagno(8);
			diagno(15);

			if( passesX_.passfl == 1 && passesX_.passct == 1 )
				diagno(18);
			}


		}
	return;
} /*end of function*/

