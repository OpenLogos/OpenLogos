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
	/*   ***** BEGINNING OF -44 SWITCH ***** */
	/*      FUNCTION:  SETTING OF VARIABLE CONSTANTS */
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
#include "parsetrans.h"
#include <string.h>
#include <jbctrl.h>


void /*FUNCTION*/ t4sw44()
{
	static short int deepdi, longdi, optr, temp, vc1, vc2, vcnew[20], vcold[20], vptr;
	static long int _n, currvc, pairct, pairpt, tempi4, zaplen;
	static char pgmnam[9] = "T4SW44  ";
	static short zero = 0;
	static short m = 0;
	static short q = 0;
	static short hz = 0;
	static short iz = 0;
	static short jz = 0;
	static short kw = 0;
	static short ms = 0;
	static short om = 0;
	static short xx = 0;
	static short xy = 0;
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


	/*                  002 = RELABEL FUNCTION */
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
			if( longdi == 1 || deepdi == 1 )
				{
				fprintf( _spec_fp, "\n*** ERROR SW44 FUNCTION 002 - %3ld PAIRS HAVE BEEN INDICATED (MAX IS 20).\n    ASSUMING 20 PAIRS ANDCONTINUING.\n", 
				  pairct );
				}
			pairct = 20;
			}
		else if( pairct < 1 ){
			errlog(pgmnam,8002,pairct,10);
			if( longdi == 1 || deepdi == 1 )
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
			}
		return;



L_8052:
		errlog(pgmnam,8006,pairpt,10);
		if( diagsX_.longdi == 1 )
			{
			fprintf( _spec_fp, "\n*** ERROR SW44 FUNCTION 002 -  ILLEGAL VC VALUE.\n     ABORT SWITCH AND ATTEMPT TO CONTINUE.\n" );
			}
		vbdataX_.k3n = pairpt;
		return;
L_8051:
		errlog(pgmnam,8007,pairpt,10);
		if( diagsX_.longdi == 1 )
			{
			fprintf( _spec_fp, "\n*** ERROR SW44 FUNCTION 002 -  ILLEGAL VC VALUE.\n     ABORT SWITCH AND ATTEMPT TO CONTINUE.\n" );
			}
		vbdataX_.k3n = pairpt;
		return;
		/*                  IF K3P4 IS A VC THEN IT IS THE COPY FUNCTION */
		}
	else if( k3p4 >= 100 && k3p4 <= 120 ){
		copy44();
		return;
		}
	else{
		if( vtrfmX_.vtrfm[vbdataX_.k3+1-One] != vtrfvlX_.vtmrp ){
			if( vbdataX_.k3p1 == -97 ){

				/*    K3P1 = -97 (SEARCH OUTPUT OPADRO BACKWARDS FOR V.C. K3P2)
				 *    OPO IS LAST ELEMENT LOADED IN OPADRI
				 *    PHRLST IS LAST PHRASE TO SEARCH
				 *    OPLST IS LAST OPADRI ELEMENT TO LOOK AT */

				xx = 2;
				/*     N3P1 = N3LST + 1 */

				/*OM+   THIS CHANGE MADE FOR STEVE ON 11/84  OLIVER */
				n3p1 = opadroX_.opo + 1;
				oplst = n3p1 - 1;

				/*    CLAUSE BOUNDARY SETTING. LOOK AT INCOMING SWORK (SINCE THERE IS
				 *    NO INCOMING SWORK). SEE IF A CB EXISTS AND FIND IT'S POSITION
				 *    IN THE OUTGOING OPADRI TO SET THE LIMIT ON THE LOOP. */

				om = vwarg2X_.i;
				for( om1=1; om1 <= vwarg2X_.i; om1++ ){
					subset = sworkX_.swork[om-One][2-One];
					supset = sconX_.scon[opadriX_.sconpi[sworkX_.phrbeg[om-One]-
					  One]-One][13-One];

					if( sworkX_.swork[om-One][1-One] == 20 && (((subset >= 
					  885 && subset <= 887) || supset == 1) || (supset >= 
					  6 && supset <= 8)) )
						goto L_4995;

					om -= 1;
					}
				om1 = oplst;
				goto L_5000;

L_4995:
				om2 = opadriX_.sconpi[sworkX_.phrbeg[om-One]-One];

				/*   LOOP THRU OUTGOING SCONPI TO FIND OPADRO */
				for( om=1; om <= opadroX_.opo; om++ ){
					if( opadroX_.sconpo[om-One] == om2 )
						goto L_4999;
					}
				om1 = oplst;
				goto L_5000;
L_4999:
				om1 = oplst - om;

				/*    LOOK FOR VC K3P2 IN OPADRI */
L_5000:
				;
				for( iz2=1; iz2 <= om1; iz2++ ){
					if( opadroX_.opadro[n3p1-iz2-One] == -k3p2 )
						goto L_8055;
					}

				if( srcflgX_.srcflg == 1 ){

					if( semargX_.k3p3 == 139 )
						sw44bkX_.nicht = 1;
					}
				goto L_5170;
L_8055:
				iz = n3p1 - iz2;
				kw = hpdopoX_.hfdopo[n3p1-iz2-One];
				goto L_5180;
				}
			else if( vbdataX_.k3p1 == -96 ){

				/*   K3P1 = -96 (SEARCH INCOMING OPADRI FOR V.C. K3P2) */

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

				/*  SEARCH FOR CLAUSE BOUNDARY IN SWORK. START TO RIGHT OF SP RULE. */
				om1 = n6p1 + 1;
				if( om1 > sworkX_.phct )
					om1 = sworkX_.phct;
				for( om=om1; om <= sworkX_.phct; om++ ){
					subset = sworkX_.swork[om-One][2-One];
					supset = sconX_.scon[opadriX_.sconpi[sworkX_.phrbeg[om-One]-
					  One]-One][13-One];

					if( sworkX_.swork[om-One][1-One] == 20 && (((subset >= 
					  885 && subset <= 887) || supset == 1) || (supset >= 
					  6 && supset <= 8)) )
						goto L_5090;

					}
				goto L_5095;

L_5090:
				flowckX_.phrlst = sworkX_.phrbeg[om-One];

L_5095:
				;
				for( iz=flowckX_.phrstr; iz <= flowckX_.phrlst; iz++ ){
					if( opadriX_.opadri[iz-One] == -k3p2 )
						goto L_8056;
					}
				goto L_5170;
L_8056:
				kw = hpdopiX_.hfdopi[iz-One];
				goto L_5180;
				}
			else if( vbdataX_.k3p1 == 1 ){

				/*   UNLOAD THE BUCKET INTO A NEW VC. CREATE IT'S SCON, ETC... */
				if( sw44bkX_.buckct == 0 )
					goto L_5820;
				kw = 0;
				xx = 2;
				dump = 1;
				goto L_5215;
				}
			}

		/*    K3P1 IS A RELATIVE POINTER (SEARCH INCOMING PHRASE FOR V.C. K3P2) */
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
			goto L_5820;
			}
		else{

			/*+                                                        *R0GBA*GBA
			 *     DO 4840 IZ = PHRSTR,PHRLST
			 *     IF (OPADRI(IZ) .EQ. -K3P2) GOTO 4860
			 *840  CONTINUE */
			if( k3p4 == -98 || k3p4 == -95 ){
				for( iz=flowckX_.phrlst; iz >= flowckX_.phrstr; iz-- ){
					if( opadriX_.opadri[iz-One] == -k3p2 )
						goto L_4860;
					}
				goto L_5170;
				}
			else{
				for( iz=flowckX_.phrstr; iz <= flowckX_.phrlst; iz++ ){
					if( opadriX_.opadri[iz-One] == -k3p2 )
						goto L_4860;
					/*-                                                        *R0GBA*GBA */
					}
				goto L_5170;
				}

L_4860:
			if( semargX_.k3p3 >= 100 && semargX_.k3p3 <= 120 ){

				/*    K3P3 IS A VC, CALCULATE POSITION IN OPADRI */
				for( iz2=flowckX_.phrstr; iz2 <= flowckX_.phrlst; iz2++ ){
					if( opadriX_.opadri[iz2-One] == -semargX_.k3p3 )
						goto L_5680;
					}
				goto L_5820;

				/*    K3P2 AND K3P3 ARE BOTH VC'S
				 *    ADD ELEMENTS FROM K3P3 TO THOSE IN K3P2 */
L_5680:
				hz = hpdopiX_.hfdopi[iz-One];
				hz2 = hpdopiX_.hfdopi[iz2-One];
				if( hz2 == 0 )
					goto L_5820;
				/*                                                          *1117BT */
				if( hz >= HFPOLO && hz <= HFPOHI ){

					/*    VC K3P2 HAS MORE THAN ONE ELEMENT, ADD ELEMENTS FROM VC K3P3
					 *    INTO ITS HFPOAD */
					adr_ = hz - HFPOL1;
					jz = hfdoaX_.hfpoad[adr_-One][HFPADX-One];
					/*                                                          *1117BT */
					if( !(hz >= HFPOLO && hz <= HFPOHI) )
						goto L_5740;
					/*                                                          *1117BT */
					}
				else if( hz == 0 ){

					/*    VC K3P2 IS EMPTY */
					hpdopiX_.hfdopi[iz-One] = hz2;
					opadriX_.sconpi[iz-One] = opadriX_.sconpi[iz2-One];
					hpdopiX_.hfdopi[iz2-One] = 0;
					goto L_5820;
					}
				else{

					/*    VC K3P2 HAS ONE ELEMENT WHICH MUST BE PLACED IN A NEW HFPOAD
					 *    WITH ELEMENT(S) FROM VC K3P3 */
					jz = 1;
					hfdoaX_.adct += 1;
					if( diagsX_.longdi == 1 )
						{
						fprintf( _spec_fp, "    NOW %2d HFDOPO VC\\\"S (%2ld-%2ld)\n", 
						  hfdoaX_.adct, HFPOLO, HFPOHI );
						}
					/*                                                          *1117BT */
					if( hfdoaX_.adct > HFPADY )
						goto L_7700;
					/*                                                          *1117BT */
					adr_ = hfdoaX_.adct;
					hfdoaX_.hfpoad[adr_-One][jz-One] = hz;
					hfdoaX_.sconhf[adr_-One][jz-One] = opadriX_.sconpi[iz-One];
					/*                                                          *1117BT */
					if( !(hz2 >= HFPOLO && hz2 <= HFPOHI) )
						goto L_5740;
					/*                                                          *1117BT */
					}

				/*    VC K3P3 HAS MORE THAN ONE ELEMENT TO ADD TO VC K3P2 */
				adr2 = hz2 - HFPOL1;
				jz1 = jz + 1;
				jz2 = hfdoaX_.hfpoad[adr2-One][HFPADX-One];
				jz += jz2;
				if( jz >= HFPADX )
					goto L_7720;
				for( iz3=jz1; iz3 <= jz; iz3++ ){
					for( xy=1; xy <= jz2; xy++ ){
						hfdoaX_.hfpoad[adr_-One][iz3-One] = hfdoaX_.hfpoad[adr2-One][xy-One];
						hfdoaX_.sconhf[adr_-One][iz3-One] = hfdoaX_.sconhf[adr2-One][xy-One];
						}
					}
				goto L_5800;
				/*                                                          *1117BT */

				/*    VC K3P3 HAS ONLY ONE ELEMENT TO ADD TO VC K3P2 */
L_5740:
				jz += 1;
				if( jz >= HFPADX )
					goto L_7720;
				hfdoaX_.hfpoad[adr_-One][jz-One] = hz2;
				hfdoaX_.sconhf[adr_-One][jz-One] = opadriX_.sconpi[iz2-One];

				/*    SAVE HFPOAD ADDRESS AND ZERO OUT HFDOPI OF VC K3P3 */
L_5800:
				hfdoaX_.hfpoad[adr_-One][HFPADX-One] = jz;
				hpdopiX_.hfdopi[iz-One] = adr_ + HFPOL1;
				hpdopiX_.hfdopi[iz2-One] = 0;
				goto L_5820;
				}
			else if( semargX_.k3p3 == 0 ){
				goto L_5580;
				}
			else{
				kw = hpdopiX_.hfdopi[iz-One];
				goto L_5180;
				}
			}

		/*   BUCKET IS DUMPED AT CLAUSE BOUNDARY BY A LATER CALL TO 44SW */

L_5170:
		if( k3p2 != 104 ){
			sw44bkX_.buckct += 1;

			if( sw44bkX_.buckct > 4 && diagsX_.longdi == 1 )
				{
				fprintf( _spec_fp, "                    IN SW44, THE SAVE BUCKET HAS OVERFLOWED, NO SAVE\n" );
				}

			if( sw44bkX_.buckct <= 4 ){
				bucket[sw44bkX_.buckct-One][1-One] = k3p2;
				bucket[sw44bkX_.buckct-One][2-One] = semargX_.k3p3;
				if( semargX_.k3p3 < 0 )
					bucket[sw44bkX_.buckct-One][2-One] = im81X_.im81 - 
					  semargX_.k3p3;
				}
			}
		goto L_5820;

		/*    K3P2 FOUND IN OPADRI
		 *+                                                        *R0GBA*GBA
		 *CCC  IF (K3P4 .EQ. -97 .OR. K3P4 .EQ. -96) GOTO 5220 */
L_5180:
		if( (k3p4 == -97 || k3p4 == -96) || k3p4 == -98 )
			goto L_5220;
		/*-                                                        *R0GBA*GBA */
		if( semargX_.k3p3 >= 0 )
			goto L_5460;

		/*    K3P3 IS A PHRASE; USE HFPOAD IN CASE MORE THAN ONE ELEMENT
		 *                                                          *1117BT */
		if( kw < HFPOLO || kw > HFPOHI )
			goto L_5340;
		/*                                                          *1117BT */
		jz = 1;
		adr_ = kw - HFPOL1;
		}

	/*    K3P3 POINTS TO A PHRASE; ADD ELEMENTS OF PHRBEG TO HFPOAD.
	 *    ENTRY HERE MEANS JZ AND ADR ALREADY CALCULATED */
L_5360:
	flowckX_.n6jim = im81X_.im81 - semargX_.k3p3;
	if( semargX_.k3p3 > 0 && dump != 0 )
		flowckX_.n6jim = semargX_.k3p3;
	flowckX_.phrstr = sworkX_.phrbeg[flowckX_.n6jim-One];
	flowckX_.phrlst = sworkX_.phrend[flowckX_.n6jim-One];
	iz2 = flowckX_.phrlst - flowckX_.phrstr + 1;
	if( iz2 == 1 ){

		/*    LOAD HFDOP WITH OPADRI ELEMENT */
		if( xx == 2 ){
			hpdopoX_.hfdopo[iz-One] = opadriX_.opadri[flowckX_.phrstr-One];
			opadroX_.sconpo[iz-One] = opadriX_.sconpi[flowckX_.phrstr-One];
			}
		else{
			hpdopiX_.hfdopi[iz-One] = opadriX_.opadri[flowckX_.phrstr-One];
			opadriX_.sconpi[iz-One] = opadriX_.sconpi[flowckX_.phrstr-One];
			}
		goto L_5580;
		}
	else{

		/*   LOOP THROUGH THE ELEMENT POINTED TO BY K3P3
		 *   MOVE OPADRI ADDRESSES TO THE HFPOAD AND SCON POSITIONS
		 *   TO THE SCONHF OF THE TARGET VC */

		for( iz2=flowckX_.phrstr; iz2 <= flowckX_.phrlst; iz2++ ){

			if( !((opadriX_.opadri[iz2-One] <= -99 && opadriX_.opadri[iz2-One] >= -120) && hpdopiX_.hfdopi[iz2-One] == 0) ){
				/*+                                                     *08/17/90*JAL*
				 *                  IGNORE A -140 UNLESS IT IS A CLAUSE MARKER POINTING
				 *                  TO A RELOCATED CLAUSE. */
				if( opadriX_.opadri[iz2-One] == -140 ){
					tempi4 = opadriX_.sconpi[iz2-One];
					if( tempi4 <= 0 || tempi4 > SCONY )
						goto L_8057;
					if( sconX_.scolnk[tempi4-One] <= 0 || sconX_.scolnk[tempi4-One] > ELMMAX )
						goto L_8057;
					if( clsconX_.cmchld[sconX_.scolnk[tempi4-One]-One] <= 0 )
						goto L_8057;
					}
				/*-                                                     *08/17/90*JAL*
				 *                  LOAD HFPOAD WITH THIS SIGNIFICANT OPADRI VALUE. */
				hfdoaX_.hfpoad[adr_-One][jz-One] = opadriX_.opadri[iz2-One];
				hfdoaX_.sconhf[adr_-One][jz-One] = opadriX_.sconpi[iz2-One];
				/*                  IF VC HAS SOMETHING IN IT, LOAD HFDOPI INSTEAD */
				if( hpdopiX_.hfdopi[iz2-One] != 0 )
					hfdoaX_.hfpoad[adr_-One][jz-One] = hpdopiX_.hfdopi[iz2-One];
				/*                  IF ONE ELEMENT IN VC THEN HFPOAD OK,JUST SET SCONPI */
				if( hpdopiX_.hfdopi[iz2-One] < HFPOLO || hpdopiX_.hfdopi[iz2-One] > 
				  HFPOHI ){
					jz += 1;
					if( jz >= HFPADX )
						goto L_7720;
					}
				else{
					/*                  THE VC HAS >1 ELEMENT THAT MUST BE LOADED FROM HFPOAD */
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
			/*                  END OF LOOP TO MOVE OPADRI ELEMENTS INTO NEW VC */
L_8057:
			;
			}
		/*                  SET NUMBER OF VC ELEMENTS. */
		hfdoaX_.hfpoad[adr_-One][HFPADX-One] = jz - 1;
		}

	/*    LOAD HFDOP WITH HFPOAD ADDRESS */

L_5400:
	if( xx == 2 ){
		hpdopoX_.hfdopo[iz-One] = HFPOL1 + adr_;
		}
	else{
		hpdopiX_.hfdopi[iz-One] = HFPOL1 + adr_;
		}

L_5580:
	if( k3p4 > 0 ){
		sconX_.scon[opadriX_.sconpi[iz-One]-One][7-One] = k3p4;
		if( semargX_.k3p3 == 0 ){
			for( ms=flowckX_.phrstr; ms <= flowckX_.phrlst; ms++ ){
				if( opadriX_.opadri[ms-One] == -k3p2 ){
					if( hpdopiX_.hfdopi[ms-One] != 0 )
						goto L_8058;
					}
				}
			goto L_5820;
L_8058:
			sconX_.scon[opadriX_.sconpi[ms-One]-One][7-One] = k3p4;
			}

		}
	else if( k3p4 == -13 ){
		if( sconX_.scon[opadriX_.sconpi[iz-One]-One][1-One] >= 0 )
			sconX_.scon[opadriX_.sconpi[iz-One]-One][1-One] = -sconX_.scon[opadriX_.sconpi[iz-One]-
			  One][1-One];

		}
	else if( k3p4 == -99 ){
		sconX_.scon[opadriX_.sconpi[iz-One]-One][7-One] = sw14bkX_.case_;
		if( sw36bkX_.rel > 0 )
			sconX_.scon[opadriX_.sconpi[iz-One]-One][7-One] = sw36bkX_.relcas;

		/*  IF K3P4 NOT A REL PNTR CONTINUE VTRF PROCESSING */

		/*+                                                06/06/86  *R1530DSD */
		}
	else if( vtrfmX_.vtrfm[vbdataX_.k3+4-One] == vtrfvlX_.vtmrp ){
		/*-                                                06/06/86  *R1530DSD */
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

L_5820:
	if( dump == 0 )
		goto L_5830;
	dump += 1;
	if( dump > sw44bkX_.buckct )
		goto L_8059;
L_5215:
	k3p2 = bucket[dump-One][1-One];
	semargX_.k3p3 = bucket[dump-One][2-One];
	opadroX_.opo += 1;
	iz = opadroX_.opo;
	opadroX_.opadro[iz-One] = -k3p2;
	prtscoX_.sct += 1;
	/*+                                         *B0305JGB */
	if( prtscoX_.sct > SCONY ){
		prtscoX_.sct = SCONY;
		prtscoX_.scterr += 1;
		if( diagsX_.longdi )
			{
			fprintf( _spec_fp, " T4SW44, OVERLOADING SCON ARRAY, SCTERR =%4d\n", 
			  prtscoX_.scterr );
			}
		if( prtscoX_.scterr == 1 )
			errlog(pgmnam,7730,500,0);
		}
	/*-                                         *B0305JGB */
	opadroX_.sconpo[opadroX_.opo-One] = prtscoX_.sct;
	/*+                                               08/24/86  *R1561DSD */
	zaplen = 2*(SCONX - SCONX1);
	zapit(&sconX_.scon[prtscoX_.sct-One][1-One],2*SCONX1,(byte)zero);
	if( sconX_.scolnk[prtscoX_.sct-One] <= ELMMAX )
		zapit(&sconX_.scono[sconX_.scolnk[prtscoX_.sct-One]-One][1-
		  One],zaplen,(byte)zero);
	/*-                                               08/24/86  *R1561DSD */
	sconX_.scon[prtscoX_.sct-One][1-One] = k3p2;

L_5220:
	if( kw != 0 )
		goto L_5240;
	/*     IF K3P3 HOLDS A IM1 POSITION OR A REL POINTER */
	if( semargX_.k3p3 < 70 )
		goto L_5340;

	/*    K3P3 IS A CONSTANT */
L_5460:
	if( xx == 2 ){
		hpdopoX_.hfdopo[iz-One] = -semargX_.k3p3;
		}
	else{
		hpdopiX_.hfdopi[iz-One] = -semargX_.k3p3;
		}
	goto L_5580;
	/*                                                          *1117BT */
L_5240:
	if( kw < HFPOLO || kw > HFPOHI ){

		jz = 1;
		hfdoaX_.adct += 1;
		if( diagsX_.longdi == 1 )
			{
			fprintf( _spec_fp, "    NOW %2d HFDOPO VC\\\"S (%2ld-%2ld)\n", 
			  hfdoaX_.adct, HFPOLO, HFPOHI );
			}
		/*                                                          *1117BT */
		if( hfdoaX_.adct > HFPADY )
			goto L_7700;
		/*                                                          *1117BT */
		adr_ = hfdoaX_.adct;
		hfdoaX_.hfpoad[adr_-One][jz-One] = kw;
		hfdoaX_.sconhf[adr_-One][jz-One] = opadriX_.sconpi[iz-One];
		jz += 1;
		if( jz >= HFPADX )
			goto L_7720;
		}
	else{
		/*                                                          *1117BT */
		adr_ = kw - HFPOL1;
		jz = hfdoaX_.hfpoad[adr_-One][HFPADX-One] + 1;
		}
	if( semargX_.k3p3 < 0 ){
		/*             LOOP TO MOVE THE ELEMENT'S OPADRI AND SCONPI VALUES INTO
		 *             INTO THE CORRECT HFPOAD AND SCONHF OF THE TARGET VC.
		 *             ONLY ARRIVE HERE IF TARGET VC ALREADY LOADED. */
		flowckX_.n6jim = im81X_.im81 - semargX_.k3p3;
		flowckX_.phrstr = sworkX_.phrbeg[flowckX_.n6jim-One];
		flowckX_.phrlst = sworkX_.phrend[flowckX_.n6jim-One];
		for( iz2=flowckX_.phrstr; iz2 <= flowckX_.phrlst; iz2++ ){
			/*                  IGNORE EMPTY VC */
			if( !((opadriX_.opadri[iz2-One] <= -100 && opadriX_.opadri[iz2-One] >= -120) && hpdopiX_.hfdopi[iz2-One] == 0) ){
				/*+                                                     *08/17/90*JAL*
				 *                  IGNORE A -140 UNLESS IT IS A CLAUSE MARKER POINTING
				 *                  TO A RELOCATED CLAUSE. */
				if( opadriX_.opadri[iz2-One] == -140 ){
					tempi4 = opadriX_.sconpi[iz2-One];
					if( tempi4 <= 0 || tempi4 > SCONY )
						goto L_8060;
					if( sconX_.scolnk[tempi4-One] <= 0 || sconX_.scolnk[tempi4-One] > ELMMAX )
						goto L_8060;
					if( clsconX_.cmchld[sconX_.scolnk[tempi4-One]-One] <= 0 )
						goto L_8060;
					}
				/*-                                                     *08/17/90*JAL*
				 *                  LOAD HFPOAD WITH THIS SIGNIFICANT OPADRI VALUE. */
				hfdoaX_.hfpoad[adr_-One][jz-One] = opadriX_.opadri[iz2-One];
				/*                  IF VC HAS SOMETHING IN IT, LOAD HFDOPI INSTEAD */
				if( hpdopiX_.hfdopi[iz2-One] != 0 )
					hfdoaX_.hfpoad[adr_-One][jz-One] = hpdopiX_.hfdopi[iz2-One];
				/*                  IF ONE ELEMENT IN VC THEN HFPOAD OK,JUST SET SCONPI */
				if( hpdopiX_.hfdopi[iz2-One] < HFPOLO || hpdopiX_.hfdopi[iz2-One] > HFPOHI ){
					hfdoaX_.sconhf[adr_-One][jz-One] = opadriX_.sconpi[iz2-One];
					jz += 1;
					if( jz >= HFPADX )
						goto L_7720;
					}
				else{
					/*                  THE VC HAS >1 ELEMENT THAT MUST BE LOADED FROM HFPOAD */
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
			/*                  END OF LOOP TO MOVE OPADRI ELEMENTS INTO NEW VC */
L_8060:
			;
			}
		/*                  SET NUMBER OF VC ELEMENTS. */
		hfdoaX_.hfpoad[adr_-One][HFPADX-One] = jz;
		goto L_5400;
		}
	else{
		hfdoaX_.hfpoad[adr_-One][jz-One] = -semargX_.k3p3;
		hfdoaX_.sconhf[adr_-One][jz-One] = opadriX_.sconpi[jz-One];
		hfdoaX_.hfpoad[adr_-One][HFPADX-One] = jz;
		goto L_5400;
		}

	/*    ENTRY HERE WILL LOAD HFPOAD FOR A PREVIOUSLY UNUSED VC */

L_5340:
	jz = 1;
	hfdoaX_.adct += 1;
	if( diagsX_.longdi == 1 )
		{
		fprintf( _spec_fp, "    NOW %2d HFDOPO VC\\\"S (%2ld-%2ld)\n", 
		  hfdoaX_.adct, HFPOLO, HFPOHI );
		}
	/*                                                          *1117BT */
	if( hfdoaX_.adct > HFPADY )
		goto L_7700;
	/*                                                          *1117BT */
	adr_ = hfdoaX_.adct;
	goto L_5360;
L_8059:
	dump = 0;
	sw44bkX_.buckct = 0;

L_5830:
	if( diagsX_.deepdi == 1 ){

		fprintf( _spec_fp, " ** END SW44  %4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d", 
		  flowckX_.n6jims, flowckX_.n6jim, flowckX_.phrstr, flowckX_.phrlst, 
		  vbdataX_.k3p1, k3p2, semargX_.k3p3, k3p4, iz, adr_, xx, 
		  hpdopiX_.hfdopi[iz-One], opadriX_.opadri[iz-One], hfdoaX_.adct, 
		  n6p1 );
		fprintf( _spec_fp, "\n BUCKET:" );
		for(_n=0L; _n < sizeof(bucket)/sizeof(short); _n++)
			fprintf( _spec_fp, "%4d", ((short*)bucket)[_n] );
		fprintf( _spec_fp, "%4d\n", sw44bkX_.buckct );

		if( adr_ > 0 )
			{
			fprintf( _spec_fp, " HFPOAD \n " );
			for( q=1; q <= HFPADX; q++ ){
				fprintf( _spec_fp, "%6d", hfdoaX_.hfpoad[adr_-One][q-One] );
				}
			fprintf( _spec_fp, "\n" );
			}
		}
	return;

	/*                 ERROR LOGGING FOR ARRAY OVERFLOWS */

L_7700:
	if( diagsX_.deepdi == 1 )
		{
		fprintf( _spec_fp, " IN SW44 - HFDOPO %2ld TO %2ld ALREADY TAKEN \n", 
		  HFPOLO, HFPOHI );
		}
	errlog(pgmnam,7700,502,0);
	return;

L_7720:
	if( diagsX_.deepdi == 1 )
		{
		fprintf( _spec_fp, " IN SW44 - HFPOAD ARRAY IS OVERFLOWING\n" );
		}
	errlog(pgmnam,7720,502,0);
	return;
} /*end of function*/

