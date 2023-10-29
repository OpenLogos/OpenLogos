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
	/*        FUNCTION: SETTING OF VARIABLE CONSTANTS */
	/*  CHANGES:
	 *     10/19/91*JAL:  ADD FUNCTION 002 TO CHANGE VC LABELS
	 *           CONCATENATED IN A GIVEN SWORK.
	 *     06/05/87 *R1683RKH*  -44 SW COPY FUNCTION
	 *     04/17/87 *R1685RKH*  CHANGE T1-4 SWORK LIMI          t from 50 to
	 *      CHG 08/22/86 *R1561DSD: 100 SCONS
	 *      CHG 07/18/86 *R1530DSD: EXTEND RELATIVE POINTERS FOR STRETCH
	 *      CHG 05/15/86 *B0406DSD: REDIMENSION HFPOAD,SCONHF FROM (100,5) */
	/*+                FUNCTION 2 LOCAL VARS.               *10/19/91*JAL* */
	/*-                                                     *10/19/91*JAL* */
	/*+    VTRFM REMEMBERS VTRF 'TYPES'  FROM VTRFWR  06/13/86  *R1530DSD */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */

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



void /*FUNCTION*/ txsw44()
{
	static short int deepdi, longdi, optr, temp, vc1, vc2, vcnew[20],vcold[20], vptr;
	static long int currvc, pairct, pairpt;
	struct  {
		short int sct, scterr;
		}	*_prtscoX_ = (void*)&prtscoX_;
	static short zero = 0;
	static char pgmnam[9] = "TxSW44  ";
	static short m = 0;
	static short q = 0;
	static short gb = 0;
	static short hz = 0;
	static short iz = 0;
	static short jz = 0;
	static short kw = 0;
	static short ms = 0;
	static short om = 0;
	static short xx = 0;
	static short adr_ = 0;
	static short hz2 = 0;
	static short iz2 = 0;
	static short iz3 = 0;
	static short jz1 = 0;
	static short jz2 = 0;
	static short om1 = 0;
	static short om2 = 0;
	static short adr2 = 0;
	static short dump = 0;
	static short k3p2 = 0;
	static short k3p4 = 0;
	static short n3p1 = 0;
	static short n6p1 = 0;
	static short oplst = 0;
	static short subset = 0;
	static short supset = 0;
	static short bucket[4][2]={0,0,0,0,0,0,0,0};

	vbdataX_.k3n = vbdataX_.k3 + 5;
	k3p2 = vtrfX_.vtrf[vbdataX_.k3+2-One];
	semargX_.k3p3 = vtrfX_.vtrf[vbdataX_.k3+3-One];
	k3p4 = vtrfX_.vtrf[vbdataX_.k3+4-One];

	if( vbdataX_.k3p1 == 2 ){
		/*+                                                     *10/19/91*JAL* */

		/*     FUNCTION 2 - CHANGE THE VC LABELS(NUMBERS) AS INDICATED
		 *                  BY THE PARAMETER LIST FOR ALL VCS
		 *                  CONCATENATED WITH THE SWORK POINTED TO
		 *                  BY THE SECOND PARAMETER.
		 *           -44  002  -8X  PAIRCOUNT  VCOLD VCNEW  VCOLD VCNEW ... */

		flowckX_.n6jim = im81X_.im81 - k3p2;
		pairct = semargX_.k3p3;
		vbdataX_.k3n = vbdataX_.k3 + 3 + pairct*2 + 1;
		if( pairct > 20 ){
			errlog(pgmnam,8001,pairct,10);
			if( longdi == 1  )
				{
				fprintf( _spec_fp, "\n*** ERROR SW44 FUNCTION 002 - %3ld PAIRS HAVE BEEN INDICATED (MAX IS 20).\n    ASSUMING 20 PAIRS ANDCONTINUING.\n", 
				  pairct );
				}
			pairct = 20;
			}
		else if( pairct < 1 ){
			errlog(pgmnam,8002,pairct,10);
			if( longdi == 1  )
				{
				fprintf( _spec_fp, "\n*** ERROR SW44 FUNCTION 002 -  ILLEGAL PAIR COUNT PARAMETER: %3ld\n     ABORT SWITCH AND CONTINUE.\n", 
				  pairct );
				}
			vbdataX_.k3n = vbdataX_.k3 + 3;
			return;
			}
		/*                           LOAD VC PAIRS INTO ARRAYS, CHECK RANGES. */
		pairpt = vbdataX_.k3 + 4;
		for( temp=1; temp <= pairct; temp++ ){
			vc1 = vtrfX_.vtrf[pairpt-One];
			if( !(vc1 >= 100 && vc1 <= 120) )
				goto L_8052;
			vcold[temp-One] = -vc1;
			pairpt += 1;
			vc2 = vtrfX_.vtrf[pairpt-One];
			if( !((vc2 >= 100 && vc2 <= 120) || vc2 == 140) )
				goto L_8051;
			vcnew[temp-One] = -vc2;
			pairpt += 1;
			}
		/*                  SEARCH THE SWORK CONSTITUENTS AND CHANGE VCS */
		for( optr=sworkX_.phrbeg[flowckX_.n6jim-One]; optr <= sworkX_.phrend[flowckX_.n6jim-One]; optr++ ){
			currvc = opadriX_.opadri[optr-One];
			if( currvc >= -120 && currvc <= -100 ){
				for( vptr=1; vptr <= pairct; vptr++ ){
					if( currvc == vcold[vptr-One] )
						goto L_8054;
					}
				goto L_8053;
L_8054:
				opadriX_.opadri[optr-One] = vcnew[vptr-One];
				}
L_8053:
			;
			/*SW44 */
			}
		return;
L_8052:
		errlog(pgmnam,8006,pairpt,10);
		if( longdi == 1  )
			{
			fprintf( _spec_fp, "\n*** ERROR SW44 FUNCTION 002 -  ILLEGAL VC VALUE.\n     ABORT SWITCH AND ATTEMPT TO CONTINUE.\n" );
			}
		vbdataX_.k3n = pairpt;
		return;
L_8051:
		errlog(pgmnam,8007,pairpt,10);
		if( longdi == 1  )
			{
			fprintf( _spec_fp, "\n*** ERROR SW44 FUNCTION 002 -  ILLEGAL VC VALUE.\n     ABORT SWITCH AND ATTEMPT TO CONTINUE.\n" );
			}
		vbdataX_.k3n = pairpt;
		return;
		/*     If K3P4 is a VC then it is the Copy Function */

		}
	else if( k3p4 >= 100 && k3p4 <= 120 ){
		copy44();
		return;
		}
	else{

		if( vtrfmX_.vtrfm[vbdataX_.k3+1-One] != vtrfvlX_.vtmrp ){
			if( vbdataX_.k3p1 == -97 ){

				/*   OPO IS LAST ELEMENT LOADED IN OPADRI
				 *   PHRLST IS LAST PHRASE TO SEARCH
				 *   OPLST IS LAST OPADRI ELEMENT TO LOOK AT */

				xx = 2;
				/*     N3P1 = N3LST + 1 */
				n3p1 = opadroX_.opo + 1;

				/*OM+   THE ABOVE CHANGE MADE FOR STEVE ON 11/84  OLIVER */
				oplst = n3p1 - 1;

				/*   CLAUSE BOUNDARY SETTING. LOOK AT OUTGOING SWORKO
				 *   FOR CB TO SET THE LIMIT ON THE LOOP. */

				om2 = sworkoX_.phcto - 1;
				om = om2;
				for( om1=1; om1 <= om2; om1++ ){
					subset = sworkoX_.sworko[om-One][2-One];
					supset = sconX_.scon[opadroX_.sconpo[sworkoX_.phrbgo[om-One]-
					  One]-One][13-One];
					if( sworkoX_.sworko[om-One][1-One] == 20 && ((subset >= 
					  885 && subset <= 887) || (supset >= 6 && supset <= 
					  8)) )
						goto L_6075;
					om -= 1;
					}
				om1 = oplst;
				goto L_6079;
L_6075:
				om1 = oplst - sworkoX_.phrbgo[om-One];

				/*   LOOK FOR VC K3P2 IN OPADRI */
L_6079:
				;
				for( iz2=1; iz2 <= om1; iz2++ ){
					if( opadroX_.opadro[n3p1-iz2-One] == -k3p2 )
						goto L_8055;
					}

				if( srcflgX_.srcflg == 1 ){

					if( semargX_.k3p3 == 139 )
						sw44bkX_.nicht = 1;
					}
				goto L_6250;
L_8055:
				iz = n3p1 - iz2;
				if (tranidX_.tranid == 2){
					kw = hpdopoX_.hfdopo[n3p1-iz2-One];
				}
				else{
					kw = hpdopiX_.hfdopi[n3p1-iz2-One];
				}
				goto L_6260;
				}
			else if( vbdataX_.k3p1 == -96 ){

				/*   K3P1 = -96 (SEARCH INCOMING OPADRI FOR V.C. K3P2)
				 *   LOOK FOR CLAUSE BOUNDARY OR EOS TO SET SEARCH LIMIT
				 *   N6JIMS EQUALS LAST PHRASE LOADED */

				xx = 3;
				n6p1 = mtcendX_.mtcend;
				if( srcflgX_.srcflg == 2 ){

					n6p1 = flowckX_.n6jims + 1;
					if( flowckX_.n6jims < vwarg2X_.i )
						n6p1 = vwarg2X_.i;
					if( semargX_.k3p3 < 0 )
						n6p1 = im81X_.im81 - semargX_.k3p3 + 1;
					}

				flowckX_.phrstr = sworkX_.phrbeg[n6p1-One];
				flowckX_.phrlst = opadriX_.opi;

				/*  CLAUSE BOUNDARY SETTING FOR K3P1 = -96. LOOK FOR THE CB TO THE RIGHT
				 *   OF THE LAST ELEMENT IN THE SP RULE MATCHED ON. CHECK FOR EOS COND. */

				om1 = n6p1 + 1;
				if( om1 > sworkX_.phct )
					om1 = sworkX_.phct;
				for( om=om1; om <= sworkX_.phct; om++ ){
					subset = sworkX_.swork[om-One][2-One];
					supset = sconX_.scon[opadriX_.sconpi[sworkX_.phrbeg[om-One]-
					  One]-One][13-One];
					if( sworkX_.swork[om-One][1-One] == 20 && ((subset >= 
					  885 && subset <= 887) || (supset >= 6 && supset <= 
					  8)) )
						goto L_6190;
					}
				goto L_6195;
L_6190:
				flowckX_.phrlst = sworkX_.phrbeg[om-One];

L_6195:
				;
				for( iz=flowckX_.phrstr; iz <= flowckX_.phrlst; iz++ ){
					if( opadriX_.opadri[iz-One] == -k3p2 )
						goto L_8056;
					}
				goto L_6250;
L_8056:
				kw = hpdopiX_.hfdopi[iz-One];
				goto L_6260;
				}
			else if( vbdataX_.k3p1 == 1 ){

				/*   UNLOAD THE BUCKET INTO A NEW VC. CREATE IT'S SCON, ETC... */
				if( sw44bkX_.buckct == 0 )
					goto L_6900;
				kw = 0;
				xx = 2;
				dump = 1;
				goto L_6290;
				}
			}

		/*   K3P1 IS A RELATIVE POINTER (SEARCH INCOMING PHRASE FOR V.C. K3P2) */
		xx = 1;
		flowckX_.phrstr = sworkX_.phrbeg[im81X_.im81-vbdataX_.k3p1-One];
		flowckX_.phrlst = sworkX_.phrend[im81X_.im81-vbdataX_.k3p1-One];
		if( k3p2 == 104 && semargX_.k3p3 == 140 ){
			for( iz=flowckX_.phrstr; iz <= flowckX_.phrlst; iz++ ){
				if( opadriX_.opadri[iz-One] == -k3p2 ){
					hpdopiX_.hfdopi[iz-One] = -semargX_.k3p3;
					sconX_.scon[opadriX_.sconpi[iz-One]-One][2-One] = semargX_.k3p3;
					}
				}
			goto L_6900;
			}
		else{
			/*+                                                        *R0GBA*GBA
			 *     DO 5920 IZ = PHRSTR,PHRLST
			 *     IF (OPADRI(IZ) .EQ. -K3P2) GO TO 5940
			 *920  CONTINUE */
			if( k3p4 == -98 || k3p4 == -95 ){
				for( iz=flowckX_.phrlst; iz >= flowckX_.phrstr; iz-- ){
					if( opadriX_.opadri[iz-One] == -k3p2 )
						goto L_5940;
					}
				goto L_6250;
				}
			else{
				for( iz=flowckX_.phrstr; iz <= flowckX_.phrlst; iz++ ){
					if( opadriX_.opadri[iz-One] == -k3p2 )
						goto L_5940;
					/*-                                                        *R0GBA*GBA */
					}
				goto L_6250;
				}
L_5940:
			if( semargX_.k3p3 >= 100 && semargX_.k3p3 <= 120 ){

				/*   K3P3 IS A VC, CALCULATE POSITION IN OPADRI */
				for( iz2=flowckX_.phrstr; iz2 <= flowckX_.phrlst; iz2++ ){
					if( opadriX_.opadri[iz2-One] == -semargX_.k3p3 )
						goto L_6760;
					}
				goto L_6900;

				/*   K3P2 AND K3P3 ARE BOTH VC'S
				 *   ADD ELEMENTS FROM K3P3 TO THOSE IN K3P2 */
L_6760:
				hz = hpdopiX_.hfdopi[iz-One];
				hz2 = hpdopiX_.hfdopi[iz2-One];
				if( hz2 == 0 )
					goto L_6900;
				if( hz >= HFPOLO && hz <= HFPOHI ){

					/*   VC K3P2 HAS MORE THAN ONE ELEMENT, ADD ELEMENTS FROM VC K3P3
					 *   INTO ITS HFPOAD */
					adr_ = hz - HFPOL1;
					jz = hfdoaX_.hfpoad[adr_-One][HFPADX-One];
					if( !(hz >= HFPOLO && hz <= HFPOHI) )
						goto L_6820;
					}
				else if( hz == 0 ){

					/*   VC K3P2 IS EMPTY */
					hpdopiX_.hfdopi[iz-One] = hz2;
					opadriX_.sconpi[iz-One] = opadriX_.sconpi[iz2-One];
					hpdopiX_.hfdopi[iz2-One] = 0;
					goto L_6900;
					}
				else{

					/*   VC K3P2 HAS ONE ELEMENT WHICH MUST BE PLACED IN A NEW HFPOAD
					 *   WITH ELEMENT(S) FROM VC K3P3 */
					jz = 1;
					hfdoaX_.adct += 1;
					if( diagsX_.longdi == 1 )
						{
						fprintf( _spec_fp, "    NOW %2d HFDOPO VC\\\"S (%2ld-%2ld)\n", 
						  hfdoaX_.adct, HFPOLO, HFPOHI );
						}
					if( hfdoaX_.adct > HFPADY )
						goto L_7700;
					adr_ = hfdoaX_.adct;
					hfdoaX_.hfpoad[adr_-One][jz-One] = hz;
					hfdoaX_.sconhf[adr_-One][jz-One] = opadriX_.sconpi[iz-One];
					if( !(hz2 >= HFPOLO && hz2 <= HFPOHI) )
						goto L_6820;
					}

				/*   VC K3P3 HAS MORE THAN ONE ELEMENT TO ADD TO VC K3P2 */
				adr2 = hz2 - HFPOL1;
				jz1 = jz + 1;
				jz2 = hfdoaX_.hfpoad[adr2-One][HFPADX-One];
				jz += jz2;
				if( jz >= HFPADX )
					goto L_7720;
				for( iz3=jz1; iz3 <= jz; iz3++ ){
					for( gb=1; gb <= jz2; gb++ ){
						hfdoaX_.hfpoad[adr_-One][iz3-One] = hfdoaX_.hfpoad[adr2-One][gb-One];
						hfdoaX_.sconhf[adr_-One][iz3-One] = hfdoaX_.sconhf[adr2-One][gb-One];
						}
					}
				goto L_6880;

				/*   VC K3P3 HAS ONLY ONE ELEMENT TO ADD TO VC K3P2 */
L_6820:
				jz += 1;
				if( jz >= HFPADX )
					goto L_7720;
				hfdoaX_.hfpoad[adr_-One][jz-One] = hz2;
				hfdoaX_.sconhf[adr_-One][jz-One] = opadriX_.sconpi[iz2-One];

				/*   SAVE HFPOAD ADDRESS AND ZERO OUT HFDOPI OF VC K3P3 */
L_6880:
				hfdoaX_.hfpoad[adr_-One][HFPADX-One] = jz;
				hpdopiX_.hfdopi[iz-One] = adr_ + HFPOL1;
				hpdopiX_.hfdopi[iz2-One] = 0;
				goto L_6900;
				}
			else if( semargX_.k3p3 == 0 ){
				goto L_6660;
				}
			else{
				kw = hpdopiX_.hfdopi[iz-One];
				goto L_6260;
				}
			}

		/*   BUCKET IS DUMPED AT CLAUSE BOUNDARY BY A LATER CALL TO 44SW */

L_6250:
		if( k3p2 != 104 ){
			sw44bkX_.buckct += 1;
			if( sw44bkX_.buckct > 4 ){
				if( diagsX_.longdi == 1 )
					{
					fprintf( _spec_fp, "                    IN SW44, THE SAVE BUCKET HAS OVERFLOWED, NO SAVE\n" );
					}
				errlog(pgmnam,6255,0,0);
				}
			else{
				bucket[sw44bkX_.buckct-One][1-One] = k3p2;
				bucket[sw44bkX_.buckct-One][2-One] = semargX_.k3p3;
				if( semargX_.k3p3 < 0 )
					bucket[sw44bkX_.buckct-One][2-One] = im81X_.im81 - 
					  semargX_.k3p3;
				}
			}
		goto L_6900;

		/*   K3P2 FOUND IN OPADRI
		 *+                                                        *R0GBA*GBA
		 *CCC  IF (K3P4 .EQ. -97 .OR. K3P4 .EQ. -96) GOTO 6300 */
L_6260:
		if( (k3p4 == -97 || k3p4 == -96) || k3p4 == -98 )
			goto L_6300;
		/*-                                                        *R0GBA*GBA */
		if( semargX_.k3p3 >= 0 )
			goto L_6540;

		/*   K3P3 IS A PHRBEG; USE HFPOAD IN CASE MORE THAN ONE ELEMENT */
		if( kw < HFPOLO || kw > HFPOHI )
			goto L_6420;
		jz = 1;
		adr_ = kw - HFPOL1;

		/*   K3P3 POINTS TO A PHRASE; ADD ELEMENTS OF PHRASE TO HFPOAD.
		 *   ENTRY HERE MEANS JZ AND ADR ALREADY CALCULATED */
		flowckX_.n6jim = im81X_.im81 - semargX_.k3p3;
		flowckX_.phrstr = sworkX_.phrbeg[flowckX_.n6jim-One];
		flowckX_.phrlst = sworkX_.phrend[flowckX_.n6jim-One];
		iz2 = flowckX_.phrlst - flowckX_.phrstr + 1;
		if( iz2 == 1 )
			goto L_6600;
		}

	/*   MOVE OPADRI ADDRESSES TO HFPOAD AND SCON POSITIONS TO SCONHF */
L_6450:
	;
	for( iz2=flowckX_.phrstr; iz2 <= flowckX_.phrlst; iz2++ ){
		hfdoaX_.hfpoad[adr_-One][jz-One] = opadriX_.opadri[iz2-One];
		if( hpdopiX_.hfdopi[iz2-One] != 0 )
			hfdoaX_.hfpoad[adr_-One][jz-One] = hpdopiX_.hfdopi[iz2-One];
		if( hpdopiX_.hfdopi[iz2-One] < HFPOLO || hpdopiX_.hfdopi[iz2-One] > 
		  HFPOHI ){
			hfdoaX_.sconhf[adr_-One][jz-One] = opadriX_.sconpi[iz2-One];
			jz += 1;
			if( jz >= HFPADX )
				goto L_7720;
			}
		else{
			adr2 = hpdopiX_.hfdopi[iz2-One] - HFPOL1;
			jz2 = hfdoaX_.hfpoad[adr2-One][HFPADX-One];
			for( m=1; m <= jz2; m++ ){
				hfdoaX_.hfpoad[adr_-One][jz-One] = hfdoaX_.hfpoad[adr2-One][m-One];
				hfdoaX_.sconhf[adr_-One][jz-One] = hfdoaX_.sconhf[adr2-One][m-One];
				jz += 1;
				if( jz >= HFPADX )
					goto L_7720;
				}
			}
		}
	hfdoaX_.hfpoad[adr_-One][HFPADX-One] = jz - 1;

	/*   LOAD HFDOP WITH HFPOAD ADDRESS */
L_6480:
	if( xx == 2 ){
		hpdopoX_.hfdopo[iz-One] = HFPOL1 + adr_;
		}
	else{
		hpdopiX_.hfdopi[iz-One] = HFPOL1 + adr_;
		}

L_6660:
	if( k3p4 <= 0 ){

		if( k3p4 == -13 ){
			if( sconX_.scon[opadriX_.sconpi[iz-One]-One][1-One] < 
			  0 )
				goto L_6900;
			sconX_.scon[opadriX_.sconpi[iz-One]-One][1-One] = -sconX_.scon[opadriX_.sconpi[iz-One]-
			  One][1-One];
			}

		if( k3p4 == -99 ){
			sconX_.scon[opadriX_.sconpi[iz-One]-One][7-One] = sw21bkX_.case_;
			if( sw36bkX_.rel > 0 )
				sconX_.scon[opadriX_.sconpi[iz-One]-One][7-One] = sw36bkX_.relcas;

			/*   IF K3P4 NOT A REL PNTR CONTINUE VTRF PROCESSING */

			/*+                                                06/13/86  *R1530DSD */
			}
		else if( vtrfmX_.vtrfm[vbdataX_.k3+4-One] == vtrfvlX_.vtmrp ){
			/*-                                                06/13/86  *R1530DSD */
			flowckX_.n6jim = im81X_.im81 - k3p4;
			flowckX_.phrstr = sworkX_.phrbeg[flowckX_.n6jim-One];
			flowckX_.phrlst = sworkX_.phrend[flowckX_.n6jim-One];

			for( iz2=flowckX_.phrstr; iz2 <= flowckX_.phrlst; iz2++ ){
				hfdoaX_.hfpoad[adr_-One][jz-One] = opadriX_.opadri[iz2-One];
				jz += 1;
				if( jz >= HFPADX )
					goto L_7720;
				}
			hfdoaX_.hfpoad[adr_-One][HFPADX-One] = jz - 1;

			}
		}
	else{
		sconX_.scon[opadriX_.sconpi[iz-One]-One][7-One] = k3p4;
		if( semargX_.k3p3 == 0 ){
			for( ms=flowckX_.phrstr; ms <= flowckX_.phrlst; ms++ ){
				if( opadriX_.opadri[ms-One] == -k3p2 ){
					if( hpdopiX_.hfdopi[ms-One] != 0 )
						goto L_8057;
					}
				}
			goto L_6900;
L_8057:
			sconX_.scon[opadriX_.sconpi[ms-One]-One][7-One] = k3p4;
			}
		}

L_6900:
	if( dump == 0 )
		goto L_6910;
	dump += 1;
	if( dump > sw44bkX_.buckct )
		goto L_8058;
L_6290:
	k3p2 = bucket[dump-One][1-One];
	semargX_.k3p3 = bucket[dump-One][2-One];
	opadroX_.opo += 1;
	iz = opadroX_.opo;
	opadroX_.opadro[iz-One] = -k3p2;
	prtscoX_.sct += 1;

	if( prtscoX_.sct > SCONY ){
		prtscoX_.sct = SCONY;
		prtscoX_.scterr += 1;
		if( diagsX_.longdi == 1 )
			{
			fprintf( _spec_fp, " T3SW44, OVERLOADING SCON ARRAY, SCTERR =%4d\n", 
			  prtscoX_.scterr );
			}
		if( prtscoX_.scterr == 1 )
			errlog(pgmnam,7730,500,0);
		}

	opadroX_.sconpo[opadroX_.opo-One] = prtscoX_.sct;
	zapit(&sconX_.scon[prtscoX_.sct-One][1-One],40,(byte)zero);
	sconX_.scon[prtscoX_.sct-One][1-One] = k3p2;

L_6300:
	if( kw != 0 )
		goto L_6320;
	if( semargX_.k3p3 <= 70 )
		goto L_6420;

	/*   K3P3 IS A CONSTANT */
L_6540:
	if( xx == 2 ){
		hpdopoX_.hfdopo[iz-One] = -semargX_.k3p3;
		}
	else{
		hpdopiX_.hfdopi[iz-One] = -semargX_.k3p3;
		}
	goto L_6660;

	/*   ENTRY HERE WILL LOAD NEW HFPOAD */
L_6420:
	flowckX_.n6jim = im81X_.im81 - semargX_.k3p3;
	if( dump != 0 && semargX_.k3p3 > 0 )
		flowckX_.n6jim = semargX_.k3p3;
	flowckX_.phrstr = sworkX_.phrbeg[flowckX_.n6jim-One];
	flowckX_.phrlst = sworkX_.phrend[flowckX_.n6jim-One];
	iz2 = flowckX_.phrlst - flowckX_.phrstr + 1;
	if( iz2 != 1 )
		goto L_8059;

	/*   LOAD HFDOP WITH OPADRI ELEMENT */
L_6600:
	if( xx == 2 ){
		hpdopoX_.hfdopo[iz-One] = opadriX_.opadri[flowckX_.phrstr-One];
		opadroX_.sconpo[iz-One] = opadriX_.sconpi[flowckX_.phrstr-One];
		}
	else{
		hpdopiX_.hfdopi[iz-One] = opadriX_.opadri[flowckX_.phrstr-One];
		opadriX_.sconpi[iz-One] = opadriX_.sconpi[flowckX_.phrstr-One];
		}
	goto L_6660;
L_6320:
	if( kw < HFPOLO || kw > HFPOHI ){

		jz = 1;
		hfdoaX_.adct += 1;
		if( diagsX_.longdi == 1 )
			{
			fprintf( _spec_fp, "    NOW %2d HFDOPO VC\\\"S (%2ld-%2ld)\n", 
			  hfdoaX_.adct, HFPOLO, HFPOHI );
			}
		if( hfdoaX_.adct > HFPADY )
			goto L_7700;
		adr_ = hfdoaX_.adct;
		hfdoaX_.hfpoad[adr_-One][jz-One] = kw;
		hfdoaX_.sconhf[adr_-One][jz-One] = opadriX_.sconpi[iz-One];
		jz += 1;
		if( jz >= HFPADX )
			goto L_7720;
		}
	else{
		adr_ = kw - HFPOL1;
		jz = hfdoaX_.hfpoad[adr_-One][HFPADX-One] + 1;
		}
	if( semargX_.k3p3 < 0 ){
		flowckX_.n6jim = im81X_.im81 - semargX_.k3p3;
		flowckX_.phrstr = sworkX_.phrbeg[flowckX_.n6jim-One];
		flowckX_.phrlst = sworkX_.phrend[flowckX_.n6jim-One];
		for( iz2=flowckX_.phrstr; iz2 <= flowckX_.phrlst; iz2++ ){
			hfdoaX_.hfpoad[adr_-One][jz-One] = opadriX_.opadri[iz2-One];
			if( hpdopiX_.hfdopi[iz2-One] != 0 )
				hfdoaX_.hfpoad[adr_-One][jz-One] = hpdopiX_.hfdopi[iz2-One];
			if( hpdopiX_.hfdopi[iz2-One] < HFPOLO || hpdopiX_.hfdopi[iz2-One] > 
			  HFPOHI ){
				hfdoaX_.sconhf[adr_-One][jz-One] = opadriX_.sconpi[iz2-One];
				jz += 1;
				if( jz >= HFPADX )
					goto L_7720;
				}
			else{
				adr2 = hpdopiX_.hfdopi[iz2-One] - HFPOL1;
				jz2 = hfdoaX_.hfpoad[adr2-One][HFPADX-One];
				for( m=1; m <= jz2; m++ ){
					hfdoaX_.hfpoad[adr_-One][jz-One] = hfdoaX_.hfpoad[adr2-One][m-One];
					hfdoaX_.sconhf[adr_-One][jz-One] = hfdoaX_.sconhf[adr2-One][m-One];
					jz += 1;
					if( jz >= HFPADX )
						goto L_7720;
					}
				}
			}
		hfdoaX_.hfpoad[adr_-One][HFPADX-One] = jz - 1;
		goto L_6480;
		}
	else{
		hfdoaX_.hfpoad[adr_-One][jz-One] = -semargX_.k3p3;
		hfdoaX_.sconhf[adr_-One][jz-One] = opadriX_.sconpi[iz-One];
		hfdoaX_.hfpoad[adr_-One][HFPADX-One] = jz;
		goto L_6480;
		}
L_8059:
	jz = 1;
	hfdoaX_.adct += 1;
	if( diagsX_.longdi == 1 )
		{
		fprintf( _spec_fp, "    NOW %2d HFDOPO VC\\\"S (%2ld-%2ld)\n", 
		  hfdoaX_.adct, HFPOLO, HFPOHI );
		}
	if( hfdoaX_.adct > HFPADY )
		goto L_7700;
	adr_ = hfdoaX_.adct;
	goto L_6450;
L_8058:
	dump = 0;
	sw44bkX_.buckct = 0;

L_6910:
	if( diagsX_.deepdi == 1 ){
		fprintf( _spec_fp, " ** END SW44  %4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d\n", 
		  flowckX_.n6jims, flowckX_.n6jim, flowckX_.phrstr, 
		  flowckX_.phrlst, vbdataX_.k3p1, k3p2, semargX_.k3p3, k3p4, 
		  iz, adr_, xx, hpdopiX_.hfdopi[iz-One], opadriX_.opadri[iz-One], 
		  hfdoaX_.adct );
		fprintf( _spec_fp, " HFPOAD = " );
		for( q=1; q <= HFPADY; q++ ){
			fprintf( _spec_fp, "%6d", hfdoaX_.hfpoad[adr_-One][q-One] );
			}
		fprintf( _spec_fp, "\n" );

		}
	return;


L_7700:
	if( diagsX_.longdi == 1 )
		{
		fprintf( _spec_fp, " IN SW44 - HFDOPO %2ld TO %2ld ALREADY TAKEN \n", 
		  HFPOLO, HFPOHI );
		}
	errlog(pgmnam,7700,502,0);
	return;

L_7720:
	if( diagsX_.longdi == 1 )
		{
		fprintf( _spec_fp, " IN SW44 - HFOAD ARRAY IS OVERFLOWING\n" );
		}
	errlog(pgmnam,7720,0,0);
	return;
} /*end of function*/

