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
	 *      10/04/91 JAL: GERSRC CHANGES RE SC5,SC7 SETTING WHEN ADLOCK.NE.0
	 *      CHG: 01/24/91 JAL: GERSRC,ALL TRGS: IF SCON(3).GT.3 THEN SET
	 *            SCON(7)=4 AND LEAVE SCON(5) ALONE.
	 *      CHG: 91/01/15 JAL: FOR GERENG, IF SCON(3).GT.3 SET SCON(7)=4
	 *           AND LEAVE SCON(5) ALONE.
	 *      CHG: 89/07/14 LG002GBA R1955: SETS SCONS 456 OF PHRBEG

	 /*     FUNCTION: CAPTURES HEAD NOUN DATA
	 *     RIGHT ORIENTED SW. UNLESS K3P1 IS NEGATIVE. */
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


void /*FUNCTION*/ txsw25()
{

	static byte vtrfmn;
	static short int ms, scxx;
	static char pgmnam[9] = "TxSW25  ";
	static short x = 0;
	static short iz = 0;
	static short sr25 = 0;
	static short retsw = 0;
	static short sconlc = 0;
	static short jbc3 = 3;

	vtrnX_.vtrn = vtrfX_.vtrf[vbdataX_.k3+2-One];
	vbdataX_.k3p1 = vtrfX_.vtrf[vbdataX_.k3+3-One];
	sw25bkX_.sw25n = 1;
	vbdataX_.k3n = vbdataX_.k3 + 4;

	elemld();
	if( errvrsX_.errlvl == 0 ){

		sconlc = sworkX_.phrhed[flowckX_.n6jim-One];
		vbdataX_.k3p1 = vtrfX_.vtrf[vbdataX_.k3+1-One];
		/*+    VTRFM REMEMBERS VTRF 'TYPES'  FROM VTRFWR  06/19/86  *R1530DSD */
		vtrfmn = vtrfmX_.vtrfm[vbdataX_.k3+3-One];
		/*-                                               06/19/86  *R1530DSD */

		/*        CHECK TO SEE IF -20 SWITCH APPEARED IN SAME VTR AS -25
		 *        IF -20 DID OCCUR, DO NOT SET UP NEW SWORK. */
		if( phsupX_.phcts != sworkoX_.phcto ){

			sworkoX_.sworko[sworkoX_.phcto-One][1-One] = sworkX_.swork[flowckX_.n6jim-One][1-One];
			sworkoX_.sworko[sworkoX_.phcto-One][2-One] = sworkX_.swork[flowckX_.n6jim-One][2-One];
			sworkoX_.sworko[sworkoX_.phcto-One][3-One] = sworkX_.swork[flowckX_.n6jim-One][3-One];
			sworkoX_.sworko[sworkoX_.phcto-One][4-One] = sworkX_.swork[flowckX_.n6jim-One][4-One];
			sworkoX_.phrhdo[sworkoX_.phcto-One] = sworkX_.phrhed[flowckX_.n6jim-One];

			phsupX_.phcts = sworkoX_.phcto;

			/*   SET -20/-25 COMBINATION FLAG IF APPROPRIATE */
			if( phsupX_.phsup == 1 )
				sw25bkX_.tw25 = 1;
			}

		/*+ GBA  SET NOUN GENDER IF SEMTAB CHANGED THE TRANSFER. */
		if( sconX_.scon[sconlc-One][8-One] == 2 ){
			if( sconX_.scon[sconlc-One][1-One] == 1 || sconX_.scon[sconlc-One][1-One] == 
			  2 ){
				if( targ25X_.targ25[sconX_.scolnk[sworkX_.swork[flowckX_.n6jim-One][4-One]-
				  One]-One] != -1 ){
					for( ms=flowckX_.phrstr; ms <= flowckX_.phrlst; ms++ ){
						if( sconX_.scon[opadriX_.sconpi[ms-One]-One][1-
						  One] > 0 )
							sconX_.scon[opadriX_.sconpi[ms-One]-One][4-
							  One] = targ25X_.targ25[sconX_.scolnk[sworkX_.swork[flowckX_.n6jim-One][4-One]-
							  One]-One];
						}
					targ25X_.targ25[sconX_.scolnk[sworkX_.swork[flowckX_.n6jim-One][4-One]-
					  One]-One] = -1;
					}
				}
			}
		/*- GBA */
		if( sw25bkX_.adlock != 0 ){
			if( sconX_.scon[sconlc-One][1-One] >= 1 ){
				/*               GERMAN SOURCE LOGIC FOR SETTING SCON(7),(5),(24) */
				if( srcflgX_.srcflg == 1 ){
					/*                       set SC24(source case) = ADLOCK */
					sconX_.scono[sconX_.scolnk[sconlc-One]-One][24-
					  SCONX1-One] = sw25bkX_.adlock;
					/*                       SET SC7 IF SCON45 = 9 (PERS. PRONOUN) */
					if( sconX_.scono[sconX_.scolnk[sconlc-One]-One][45-
					  SCONX1-One] == 9 ){
						if( trgflgX_.trgflg == 2 ){
							sconX_.scon[sconlc-One][7-One] = 4;
							}
						else{
							sconX_.scon[sconlc-One][7-One] = 5;
							}
						goto L_3950;
						/*                      ALL TRG - IF NUM OF TRGT IS INVARIANT THEN
						 *                      DONT CHANGE SCON(5) OR SCON(7) */
						}
					else if( sconX_.scon[sconlc-One][3-One] > 3 ){
						goto L_3950;
						}
					}

				/*           FROM HERE TO 3950, SET SCON(7) AND SCON(5) BASED ON ADLOCK
				 *           IF ADDLOCK > 4 THEN SCON(7) = 4 AND SCON(5) IS SET USING THE
				 *           AMOUNT ADLOCK EXCEEDS 4 INCONJUCTION WITH THE FORM TABLES.
				 *           IF ADLOCK < 4 THEN SCON(7) = ADLOCK AND SCON(5) IS SET FROM
				 *           ADLOCK AND THE GERNUM TABLE. */

				if( sw25bkX_.adlock <= 4 ){
					if( srcflgX_.srcflg == 2 )
						sconX_.scon[sconlc-One][7-One] = sw25bkX_.adlock;
					iz = nounsX_.gernum[sw25bkX_.adlock-One][formsaX_.formsv[sconX_.scolnk[sworkX_.swork[flowckX_.n6jim-One][4-One]-
					  One]-One]-One];
					if( iz > 0 )
						sconX_.scon[sconlc-One][5-One] = iz;
					}
				else{

					/*          FOR ADLOCK = 5, 6 OR 7 - TEST FORM TABLE AND SET SCON(7) = 4 */

					if( srcflgX_.srcflg == 2 )
						sconX_.scon[sconlc-One][7-One] = 4;
					x = sw25bkX_.adlock - 4;
					if( x != 2 ){
						if( x == 3 )
							goto L_3910;
						sr25 = 87;
						/*---- CALL FORMO2 (SR25, JBC3, N6JIM, *3940, *3880) */
						formod(2,sr25,jbc3,flowckX_.n6jim, &retsw);
						if( retsw == 1 )goto L_3940;
						sr25 = 88;
						/*---- CALL FORMO2 (SR25, JBC3, N6JIM, *3940, *3930) */
						formod(2,sr25,jbc3,flowckX_.n6jim,
						  &retsw);
						if( retsw == 1 )goto L_3940;
						if( retsw == 2 )goto L_3930;
						}
					sr25 = 86;
					/*---- CALL FORMO2 (SR25, JBC3, N6JIM, *3940, *3900) */
					formod(2,sr25,jbc3,flowckX_.n6jim,&retsw);
					if( retsw == 1 )
						goto L_3940;
					sr25 = 88;
					/*---- CALL FORMO2 (SR25, JBC3, N6JIM, *3940, *3930) */
					formod(2,sr25,jbc3,flowckX_.n6jim,&retsw);
					if( retsw == 1 )
						goto L_3940;
					if( retsw == 2 )
						goto L_3930;
L_3910:
					sr25 = 86;
					/*---- CALL FORMO2 (SR25, JBC3, N6JIM, *3940, *3920) */
					formod(2,sr25,jbc3,flowckX_.n6jim,&retsw);
					if( retsw != 1 ){
						sr25 = 87;
						/*---- CALL FORMO2 (SR25, JBC3, N6JIM, *3940, *3930) */
						formod(2,sr25,jbc3,flowckX_.n6jim,
						  &retsw);
						if( retsw != 1 )
							goto L_3930;
						}
L_3940:
					sconX_.scon[sconlc-One][5-One] = 2;
					goto L_3950;

L_3930:
					sconX_.scon[sconlc-One][5-One] = 1;
					}


L_3950:
				;
				for( iz=flowckX_.phrstr; iz <= flowckX_.phrlst; iz++ ){
					/*+                                                        *R1955*GBA */
					if( sconX_.scon[opadriX_.sconpi[iz-One]-One][1-
					  One] > 0 ){
						if( srcflgX_.srcflg == 1 ){
							scxx = opadriX_.sconpi[iz-One];
							sconX_.scon[scxx-One][4-One] = sconX_.scon[sconlc-One][4-One];
							sconX_.scon[scxx-One][5-One] = sconX_.scon[sconlc-One][5-One];
							sconX_.scon[scxx-One][6-One] = sconX_.scon[sconlc-One][6-One];
							/*                 set scon24 only if not yet set */
							if( sconX_.scono[sconX_.scolnk[scxx-One]-
							  One][24-SCONX1-One] == 0 )
								sconX_.scono[sconX_.scolnk[scxx-One]-
								  One][24-SCONX1-One] = sw25bkX_.adlock;
							}
						sconX_.scon[opadriX_.sconpi[iz-One]-One][1-
						  One] = -sconX_.scon[opadriX_.sconpi[iz-One]-
						  One][1-One];
						}
					/*-                                                        *R1955*GBA */
					}
				}
			}

		sw25bkX_.hednum = sconX_.scon[sconlc-One][5-One];
		sw25bkX_.hedgen = sconX_.scon[sconlc-One][4-One];
		sw25bkX_.hedper = sconX_.scon[sconlc-One][6-One];
		sw25bkX_.hedcas = sconX_.scon[sconlc-One][7-One];
		/*+                                                        *R1955*GBA */
		if( srcflgX_.srcflg == 1 ){
			for( iz=flowckX_.phrstr; iz <= flowckX_.phrlst; iz++ ){
				if( sconX_.scon[opadriX_.sconpi[iz-One]-One][1-One] > 
				  0 ){
					sconX_.scon[opadriX_.sconpi[iz-One]-One][4-One] = sconX_.scon[sconlc-One][4-One];
					sconX_.scon[opadriX_.sconpi[iz-One]-One][5-One] = sconX_.scon[sconlc-One][5-One];
					sconX_.scon[opadriX_.sconpi[iz-One]-One][6-One] = sconX_.scon[sconlc-One][6-One];
					}
				}
			}
		/*-                                                        *R1955*GBA */

		if( vbdataX_.k3p1 < 0 ){

			/*+    IS PARAM1 RELATIVE PTR?                    06/19/86  *R1530DSD */
			if( vtrfmn == vtrfvlX_.vtmrp ){

				/*     RESET PHRBEG START AND STOP FOR RELATIVE POINTER */
				flowckX_.phrstr = sworkX_.phrbeg[im81X_.im81-vbdataX_.k3p1-One];
				flowckX_.phrlst = sworkX_.phrend[im81X_.im81-vbdataX_.k3p1-One];
				for( iz=flowckX_.phrstr; iz <= flowckX_.phrlst; iz++ ){
					if( sconX_.scon[opadriX_.sconpi[iz-One]-One][1-
					  One] >= 0 )
						sconX_.scon[opadriX_.sconpi[iz-One]-One][7-
						  One] = sconX_.scon[sworkX_.phrhed[im81X_.im81-vbdataX_.k3p1-One]-
						  One][7-One];
					}
				/*     IF UNDEFINED FUNCTION, IGNORE IT */
				}
			else if( vbdataX_.k3p1 == -99 ){
				/*-                                               06/19/86  *R1530DSD */
				for( iz=flowckX_.phrstr; iz <= flowckX_.phrlst; iz++ ){
					if( sconX_.scon[opadriX_.sconpi[iz-One]-One][1-
					  One] >= 0 ){
						sconX_.scon[opadriX_.sconpi[iz-One]-One][7-
						  One] = sw36bkX_.relcas;
						if( sw36bkX_.rel == 0 )
							sconX_.scon[opadriX_.sconpi[iz-One]-One][7-
							  One] = sw21bkX_.case_;
						}
					}
				}
			else{
				return;
				}
			}
		else if( vbdataX_.k3p1 != 0 ){
			for( iz=flowckX_.phrstr; iz <= flowckX_.phrlst; iz++ ){
				if( sconX_.scon[opadriX_.sconpi[iz-One]-One][1-One] >= 
				  0 ){
					sconX_.scon[opadriX_.sconpi[iz-One]-One][7-One] = vbdataX_.k3p1;
					sw25bkX_.hedcas = vbdataX_.k3p1;
					}
				}
			}

		/*    PR 1741  IF NO -20 SWITCH, SET ADLOCK TO 0 */
		if( phsupX_.phsup == 0 )
			sw25bkX_.adlock = 0;

		if( diagsX_.deepdi == 1 )
			{
			fprintf( _spec_fp, " SW25 %6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d\n%6d", 
			  vtrnX_.vtrn, vbdataX_.k3n, vbdataX_.k3p1, prtscoX_.sct, 
			  sw25bkX_.hedcas, sw25bkX_.hednum, sw25bkX_.hedgen, sw14bkX_.gen, 
			  sw14bkX_.num, sw14bkX_.per, sw36bkX_.relgen, sw36bkX_.relnum, 
			  sw36bkX_.relper, sw36bkX_.relcas, sconlc, sw36bkX_.rel, 
			  flowckX_.phrstr, flowckX_.phrlst, sworkoX_.phrhdo[sworkoX_.phcto-One], 
			  phsupX_.phsup, sw14bkX_.sw14n );
			for( iz=1; iz <= 4; iz++ ){
				fprintf( _spec_fp, "%6d", sworkoX_.sworko[sworkoX_.phcto-One][iz-One] );
				}
			fprintf( _spec_fp, "\n" );
			scnprt();
		}
	}
	return;
} /*end of function*/

