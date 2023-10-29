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
	/*     FUNCTION: A VERB SWITCH. LOADS SCON WITH -25 SW.
	 *     (DATA, PER, NUM, GEN.)  ESTABLISHES TENSE.
	 *     RELATES TO SWORK TO RIGHT OF SWITCH. */


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <fcrt.h>
#include "logoslib.h"
#include "trans.h"
#include "projexts.h"
#include <jbctrl.h>
#include "parsetrans_ext.h"



void /*FUNCTION*/ txsw21()
{
	static char pgmnam[9] = "T2SW21  ";
	static short iz = 0;
	static short xx = 0;
	static short yx = 0;
	static short sconlc = 0;

	vtrnX_.vtrn = vtrfX_.vtrf[vbdataX_.k3+2-One];
	vbdataX_.k3n = vbdataX_.k3 + 4;
	vbdataX_.k3p1 = vtrfX_.vtrf[vbdataX_.k3+3-One];

	if(tranidX_.tranid == 2){
		sw21bkX_.sw21n = 1;
		sw31bkX_.offlag = 0;
	}

	elemld();
	if( errvrsX_.errlvl != 0 ){
		return;
	}
		/*     ABOVE ESTABLISHES SWORK TO RIGHT OF SWITCH
		 *     CHECK FOR LOCKED SCON
		 *     LOAD SCON WITH FLAGS OR REL FLAGS */

		sconlc = sworkX_.phrhed[flowckX_.n6jim-One];
		if( sw36bkX_.rel == 2 )
			sw36bkX_.rel = 0;
		vbdataX_.k3p1 = vtrfX_.vtrf[vbdataX_.k3+1-One];
		flowckX_.phrstr = sworkX_.phrbeg[flowckX_.n6jim-One];
		flowckX_.phrlst = sworkX_.phrend[flowckX_.n6jim-One];

		if( sw36bkX_.rel == 1 ){

			if( !(tranidX_.tranid == 2 && sw14bkX_.sw14n != 1) ){
				for( iz=flowckX_.phrstr; iz <= flowckX_.phrlst; iz++ ){
					if( sconX_.scon[opadriX_.sconpi[iz-One]-One][1-One] >= 0 ){
						if( sw36bkX_.relgen != 0 )
							sconX_.scon[opadriX_.sconpi[iz-One]-One][4-One] = sw36bkX_.relgen;
						if( sw36bkX_.relnum != 0 )
							sconX_.scon[opadriX_.sconpi[iz-One]-One][5-One] = sw36bkX_.relnum;
						if( sw36bkX_.relper != 3 )
							sconX_.scon[opadriX_.sconpi[iz-One]-One][6-One] = sw36bkX_.relper;
						}
					}
				}

				/* IF PASSIVE, VERB NO LONGER GOVERNS  CASE. */
			if( sw35bkX_.relpas != 0 ){
				sw35bkX_.relpas = 0;
				}
			else{
				if( tranidX_.tranid == 4 || srcflgX_.srcflg != 1 ){
					sw36bkX_.relcas = sconX_.scon[sconlc-One][3-One];
					if( sw36bkX_.relcas >= 5 && sw36bkX_.relcas <= 8 )
						sw36bkX_.relcas -= 4;
					}
				}
			}
		else{
			if( !(tranidX_.tranid == 2 && sw14bkX_.sw14n != 1) ){
				for( iz=flowckX_.phrstr; iz <= flowckX_.phrlst; iz++ ){
					if( sconX_.scon[opadriX_.sconpi[iz-One]-One][1-One] >= 0 ){
						if( sw14bkX_.gen != 0 )
							sconX_.scon[opadriX_.sconpi[iz-One]-One][4-One] = sw14bkX_.gen;
						if( sw14bkX_.num != 0 )
							sconX_.scon[opadriX_.sconpi[iz-One]-One][5-One] = sw14bkX_.num;
						if( sw14bkX_.per != 3 )
							sconX_.scon[opadriX_.sconpi[iz-One]-One][6-One] = sw14bkX_.per;
						}
					}
				}

				/* IF PASSIVE FLAG ON, VERB NO LONGER GOVERNS CASE */
			if( sw35bkX_.pass != 0 ){
				sw35bkX_.pass = 0;
				}
			else{
				if( tranidX_.tranid == 4 ){
					sw14bkX_.case_ = sconX_.scon[sconlc-One][3-One];
					if( sw14bkX_.case_ >= 5 && sw14bkX_.case_ <= 8 )
						sw14bkX_.case_ -= 4;			
				}
				else{
					if(srcflgX_.srcflg != 1 ){
						sw21bkX_.case_ = sconX_.scon[sconlc-One][3-One];
						if( sw21bkX_.case_ >= 5 && sw21bkX_.case_ <= 8 )
							sw21bkX_.case_ -= 4;
						}
					}
				
				}
			}


		if( !(vbdataX_.k3p1 == 0 && sw19bkX_.tense == 0) ){
			xx = vbdataX_.k3p1;
			if( sw19bkX_.tense > 0 )
				xx = sw19bkX_.tense;

			for( iz=flowckX_.phrstr; iz <= flowckX_.phrlst; iz++ ){
				yx = sconX_.scon[opadriX_.sconpi[iz-One]-One][1-One];
				if( yx >= 0 ){
					if( !(yx >= 99 && yx <= 120) )
						sconX_.scon[opadriX_.sconpi[iz-One]-One][7-
						  One] = xx;
					}
				}
			}

		/*        AUXILIARY FUNCTIONS */
		if( sw36bkX_.rel == 1 )
			sw36bkX_.rel = 2;

		sw19bkX_.tense = 0;
		if( tranidX_.tranid == 2 ){
			sw14bkX_.sw14n = 0;
		}

		if( diagsX_.deepdi == 1 )
			{
			fprintf( _spec_fp, " SW21 %6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d", 
			  vtrnX_.vtrn, vbdataX_.k3p1, sw21bkX_.case_, sw14bkX_.gen, 
			  sw14bkX_.num, sw14bkX_.per, sw36bkX_.rel, sw36bkX_.relgen, 
			  sw36bkX_.relnum, sw36bkX_.relper, sw19bkX_.tense, vbdataX_.k3n, 
			  flowckX_.phrstr, flowckX_.phrlst, sworkoX_.phcto, 
			  xx, flowckX_.n6jim );
			for( iz=1; iz <= 13; iz++ ){
				fprintf( _spec_fp, "%6d", sconX_.scon[sconlc-One][iz-One] );
				}
			fprintf( _spec_fp, "%6d\n", sconlc );
			scnprt();
			}

	return;
} /*end of function*/

