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
	/*      FUNCTION:  CAPTURES HEAD NOUN DATA
	 *      A RIGHT ORIENTED SW. EXCEPT IF K3P1 IS NEGATIVE. */
	/*      CHG: 02/13/91 *JAL* IF PARM1 .NE. 0 THEN APPLY CASE OF HEAD TO
	 *           SUBORDINATE ELEMENTS ONLY IF GERMAN TARGET.
*/

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


void /*FUNCTION*/ t4sw25()
{
	static byte vtrfmn;
	static short int edcase, ms;
	static char pgmnam[9] = "T4SW25  ";
	static short iz = 0;
	static short sconlc = 0;

    vtrnX_.vtrn = vtrfX_.vtrf[vbdataX_.k3+2-One];
	vbdataX_.k3p1 = vtrfX_.vtrf[vbdataX_.k3+3-One];
	sw25bkX_.sw25n = 1;
	vbdataX_.k3n = vbdataX_.k3 + 4;

	elemld();
	if( errvrsX_.errlvl == 0 ){

		sconlc = sworkX_.phrhed[flowckX_.n6jim-One];
		vbdataX_.k3p1 = vtrfX_.vtrf[vbdataX_.k3+1-One];
		/*+    VTRFM REMEMBERS VTRF 'TYPES'  FROM VTRFWR  06/06/86  *R1530DSD */
		vtrfmn = vtrfmX_.vtrfm[vbdataX_.k3+1-One];

		if( phsupX_.phsup != 1 ){
			}

		if( sconX_.scon[sconlc-One][8-One] == 2 ){
			if( sconX_.scon[sconlc-One][1-One] == 1 || sconX_.scon[sconlc-One][1-One] == 2 ){
				if( targ25X_.targ25[sconX_.scolnk[sworkX_.swork[flowckX_.n6jim-One][4-One]-One]-One] != -1 ){
					for( ms=flowckX_.phrstr; ms <= flowckX_.phrlst; ms++ ){
						if( sconX_.scon[opadriX_.sconpi[ms-One]-One][1-One] > 0 )
							sconX_.scon[opadriX_.sconpi[ms-One]-One][4-
							  One] = targ25X_.targ25[sconX_.scolnk[sworkX_.swork[flowckX_.n6jim-One][4-One]-One]-One];
						}
					targ25X_.targ25[sconX_.scolnk[sworkX_.swork[flowckX_.n6jim-One][4-One]-One]-One] = -1;
					}
				}
			}
		/*                       IF SCON IS LOCKED, EXIT */
		if( sconX_.scon[sconlc-One][1-One] >= 0 ){
			/*                       WHICH CASE TO USE? */
			if( sw36bkX_.rel == 0 ){
				edcase = sw14bkX_.case_;
				}
			else{
				edcase = sw36bkX_.relcas;
				}
			if( vbdataX_.k3p1 > 0 ){
				/*                       FUNCTION 4 ***** */
				for( iz=flowckX_.phrstr; iz <= flowckX_.phrlst; iz++ ){

					/*+    IS PARAM1 RELATIVE PTR?                    06/06/86  *R1530DSD */
					if( sconX_.scon[opadriX_.sconpi[iz-One]-One][1-One] >= 0 )
						sconX_.scon[opadriX_.sconpi[iz-One]-One][7-One] = vbdataX_.k3p1;
					}
				}
			else if( vtrfmn == vtrfvlX_.vtmrp ){
				/*     WAS: ELSEIF (K3P1 .LT. 0 .AND. K3P1 .NE. -99) THEN
				 *-                                               06/06/86  *R1530DSD
				 *            FUNCTION 6 ****
				 *            RESET PHRASE START AND STOP FOR RELATIVE POINTER */
				flowckX_.phrstr = sworkX_.phrbeg[im81X_.im81-vbdataX_.k3p1-One];
				flowckX_.phrlst = sworkX_.phrend[im81X_.im81-vbdataX_.k3p1-One];
				for( iz=flowckX_.phrstr; iz <= flowckX_.phrlst; iz++ ){

					if( sconX_.scon[opadriX_.sconpi[iz-One]-One][1-One] >= 0 )
						sconX_.scon[opadriX_.sconpi[iz-One]-One][7-One] = sconX_.scon[sworkX_.phrhed[im81X_.im81-vbdataX_.k3p1-One]-
						  One][7-One];
					}
				}
			else if( vbdataX_.k3p1 == -99 ){
				/*            FUNCTION 5 ***** */
				for( iz=flowckX_.phrstr; iz <= flowckX_.phrlst; iz++ ){

					/*                       ENGLISH SOURCE ONLY */
					if( sconX_.scon[opadriX_.sconpi[iz-One]-One][1-One] >= 0 )
						sconX_.scon[opadriX_.sconpi[iz-One]-One][7-One] = edcase;
					}
				}
			else if( vbdataX_.k3p1 == 0 && srcflgX_.srcflg == 2 ){
				/*                       ONLY SET CASE IF GERMAN TARGET */
				if( trgflgX_.trgflg == 1 ){
					for( iz=flowckX_.phrstr; iz <= flowckX_.phrlst; iz++ ){
						/*                       SET CASE, REL OR MAIN */
						if( sconX_.scon[opadriX_.sconpi[iz-One]-One][1-
						  One] >= 0 && sconX_.scon[opadriX_.sconpi[iz-One]-One][7-One] == 0 )
							sconX_.scon[opadriX_.sconpi[iz-One]-One][7-One] = edcase;

						}
					}
				}
			}

		if( diagsX_.deepdi == 1 )
			{
			fprintf( _spec_fp, " SW25 %6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d\n%6d\n", 
			  vtrnX_.vtrn, vbdataX_.k3n, vbdataX_.k3p1, prtscoX_.sct, 
			  sw25bkX_.hedcas, sw25bkX_.hednum, sw25bkX_.hedgen, sw25bkX_.hedper, 
			  sw14bkX_.gen, sw14bkX_.num, sw14bkX_.per, sw36bkX_.relgen, 
			  sw36bkX_.relnum, sw36bkX_.relper, sw36bkX_.relcas, sconlc, 
			  sw36bkX_.rel, flowckX_.phrstr, flowckX_.phrlst, sworkoX_.phrhdo[sworkoX_.phcto-One], 
			  phsupX_.phsup );
			scnprt();
			}

		}
	return;
} /*end of function*/

