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
	/*  CHANGES:
	 *     08/31/93 jal: fcn 868 = 2 params that indicate which Black Hole
	 *              should be filled.
	 *     03/30/93 JAL: add function 46 = copy target address and SCON60 of
	 *              NN2 to same of NN3.
	 *     02/21/91 *JAL* :  ACCESS HASHCD AND HENUM2 VALUES VIA SCOLNK TO
	 *                    ACCOUNT FOR NEW CLAUSE ELEMENTS ADDED BEYOND THE
	 *                    100TH SCON
	 *     07/17/90 *JAL*  FUNCTION 45 = COPY THE HENNUM AND HASH CODE OF
	 *              ELEMENT NN2 TO NN3.
	 *    ADD -48SW FUNCTION 044   TO SET SCON5 (TR2-4)  OGM 12/17/86  R1628+
	 *     08/29/85 */
	/*    ADD -48SW FUNCTION 044   FOR SETTING SCON5  OGM 12/17/86  R1628+ */
	/*    ADD -48SW FUNCTION 044   FOR SETTING SCON5  OGM 12/17/86 R1628- */
	/*                  switch 48 868 settings for filling target Black holes
	 *                     BHPT48 = direct ptr to SCON of targetted Black Hole
	 *                     BHCT48 = count of which BH to fill. */

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



void /*FUNCTION*/ txsw48switch()
{
	static short int sclnk1, sclnk2, sconlc, sconp1, sconp2, srcflg, temp6, temp7, xx;
	static char pgmnam[9] = "TxSW48sw ";
	static short l = 0;
	static short p = 0;
	static short y = 0;
	static short gb = 0;
	static short h1 = 0;
	static short h2 = 0;
	static short om = 0;
	static short s1 = 0;
	static short s2 = 0;
	static short s3 = 0;
	static short s7 = 0;
	static short sc1 = 0;
	static short x1 = 0;
	static short x2 = 0;
	static short ms1 = 0;
	static short ms2 = 0;
	static short x22 = 0;
	static short k3p2 = 0;
	static short n7jim = 0;
	static short phrls1 = 0;
	static short phrls2 = 0;
	static short phrst1 = 0;
	static short phrst2 = 0;
	static short ofl2 = 0;

	k3p2 = vtrfX_.vtrf[vbdataX_.k3+2-One];
	semargX_.k3p3 = vtrfX_.vtrf[vbdataX_.k3+3-One];
	vbdataX_.k3n = vbdataX_.k3 + 4;
	flowckX_.n6jim = im81X_.im81 - k3p2;
	n7jim = im81X_.im81 - semargX_.k3p3;
	phrst1 = sworkX_.phrbeg[flowckX_.n6jim-One];
	phrls1 = sworkX_.phrend[flowckX_.n6jim-One];
	phrst2 = sworkX_.phrbeg[n7jim-One];
	phrls2 = sworkX_.phrend[n7jim-One];

	if( vbdataX_.k3p1 >= 99 && vbdataX_.k3p1 <= 120 ){

		/*     WHEN K3P1 IS IN THE RANGE OF A VC, DO THE FOLLOWING:
		 *       MOVE THE CONTENTS OF THE VC (K3P1) INTO  PHRASE K3P2
		 *       TO THE SAME VC IN PHRBEG K3P3 */

		for( ms1=phrst1; ms1 <= phrls1; ms1++ ){
			if( opadriX_.opadri[ms1-One] == -vbdataX_.k3p1 )
				goto L_8134;
			}
		goto L_7840;
L_8134:
		if( hpdopiX_.hfdopi[ms1-One] != 0 ){

			for( ms2=phrst2; ms2 <= phrls2; ms2++ ){
				if( opadriX_.opadri[ms2-One] == -vbdataX_.k3p1 )
					goto L_8135;
				}
			goto L_7840;
L_8135:
			if( hpdopiX_.hfdopi[ms2-One] == 0 ){

				/*+                                                         *R0977MBS */
				hpdopiX_.hfdopi[ms2-One] = hpdopiX_.hfdopi[ms1-One];
				hpdopiX_.hfdopi[ms1-One] = 0;

				opadriX_.sconpi[ms2-One] = opadriX_.sconpi[ms1-One];
				}
			}
		}
	else if( vbdataX_.k3p1 == 8 ){

		formsaX_.formsv[sconX_.scolnk[sworkX_.swork[flowckX_.n6jim-One][4-One]-
		  One]-One] = semargX_.k3p3;
		}
	else if( vbdataX_.k3p1 == 11 ){

		/*     FUNCTION 11  P.R.1280  5/83
		 *     SET SCON 3 TO THE VALUE HELD BY K3P2 */

		for( p=phrst2; p <= phrls2; p++ ){
			sconX_.scon[opadriX_.sconpi[p-One]-One][3-One] = k3p2;
			}
		}
	else if( vbdataX_.k3p1 == 12 ){

		sconX_.scon[sworkX_.phrhed[n7jim-One]-One][12-One] = k3p2;
		}
	else if( vbdataX_.k3p1 == 13 ){

		sconX_.scon[sworkX_.phrhed[n7jim-One]-One][3-One] = k3p2;
		}
	else if( vbdataX_.k3p1 >= 20 && vbdataX_.k3p1 <= 23 ){

		/*        -48 02X -8X SCON   apply SCON value to all elements
		 *                           concatenated under -8X head element
		 *            020   - apply only to unset (scon=0) and unlocked scons
		 *                    concatenated in the input OPADR
		 *            021   - apply only to unset (scon=0) and unlocked scons
		 *                    concatenated in the output OPADR
		 *            022   - apply only to unlocked scons
		 *                    concatenated in the input OPADR
		 *            023   - apply only to unlocked scons
		 *                    concatenated in the output OPADR */

		txsw48(vbdataX_.k3p1,k3p2,semargX_.k3p3);
		}
	else if( vbdataX_.k3p1 == 43 ){

		/*    FUNCTION 43 - SET SCON 3 OF ELS IN PHRBEG TO K3P3 */

		for( gb=phrst1; gb <= phrls1; gb++ ){
			if( sconX_.scon[opadriX_.sconpi[gb-One]-One][0] >= 0 )
				sconX_.scon[opadriX_.sconpi[gb-One]-One][3-One] = semargX_.k3p3;
			}
		/*    ADD -48SW FUNCTION 044   FOR SETTING SCON5  OGM 12/17/86  R1628+ */
		}
	else if( vbdataX_.k3p1 == 44 ){

		/*    ADD -48SW FUNCTION 044   FOR SETTING SCON5  OGM 12/17/86  R1628+ */

		if( srcflg == 1 ){
			/*        COMPUTE SCON5 FOR ALL NOUNS IN THE PHRBEG BASED ON THE VALUE
			 *        PASSED IN K3P3 AND THE SCON(5) POSITION */
			sconlc = sworkX_.phrhed[flowckX_.n6jim-One];


			for( ms1=phrst1; ms1 <= phrls1; ms1++ ){

				sc1 = sconX_.scon[opadriX_.sconpi[ms1-One]-One][1-
				  One];
				/*     DON'T SET SCON5 IF THE PHRASE MEMBER'S SCON IS LOCKED.
				 *     ONLY SET SCON5 FOR PHRASE MEMBERS WITH WC 1,5,6,7 OR 8 */

				if( sc1 > 0 ){
					if( (((sc1 == 1 || sc1 == 5) || sc1 == 6) || sc1 == 7) || sc1 == 8 ){
						om = nounsX_.gernum[semargX_.k3p3-One][formsaX_.formsv[sconX_.scolnk[opadriX_.sconpi[ms1-One]-
						  One]-One]-One];
						if( om >= 1 && om <= 99 )
							sconX_.scon[opadriX_.sconpi[ms1-One]-One][5-One] = om;
						}
					}
				}
			}
		}
	else if( vbdataX_.k3p1 == 45 ){


		/*                   -48045  COPY HENNUM & HASH CODE OF NN2 TO NN3
		 *                   CHANGES ARE MADE FOR HEAD OF PHRASE  ONLY. */
		temp7 = sconX_.scolnk[sworkX_.swork[n7jim-One][4-One]-One];
		temp6 = sconX_.scolnk[sworkX_.swork[flowckX_.n6jim-One][4-One]-
		  One];
		hensavX_.henum2[temp7-One][0] = hensavX_.henum2[temp6-One][0];
		hensavX_.henum2[temp7-One][2-One] = hensavX_.henum2[temp6-One][2-One];
		hensavX_.root_henum2[temp7-One][0] = hensavX_.root_henum2[temp6-One][0];
		hensavX_.root_henum2[temp7-One][2-One] = hensavX_.root_henum2[temp6-One][2-One];
		hashX_.hashcd[temp7-One][0] = hashX_.hashcd[temp6-One][0];
		hashX_.hashcd[temp7-One][2-One] = hashX_.hashcd[temp6-One][2-One];
		hashX_.root_hashcd[temp7-One][0] = hashX_.root_hashcd[temp6-One][0];
		hashX_.root_hashcd[temp7-One][2-One] = hashX_.root_hashcd[temp6-One][2-One];
		}
	else if( vbdataX_.k3p1 == 46 ){

		/*            -48046  copy target address and SCON60 of NN2 to same of NN3 */

		sconp1 = sworkX_.swork[flowckX_.n6jim-One][4-One];
		sconp2 = sworkX_.swork[n7jim-One][4-One];
		/*             find the opadri for the head */
		for( x1=phrst1; x1 <= phrls1; x1++ ){
			if( opadriX_.sconpi[x1-One] == sconp1 ){
				/*                      find the opadri for the receiving head */
				for( x2=phrst2; x2 <= phrls2; x2++ ){
					if( opadriX_.sconpi[x2-One] == sconp2 )
						goto L_8136;
					}
				}
			}
		goto L_7840;
L_8136:
		opadriX_.opadri[x2-One] = opadriX_.opadri[x1-One];
		sclnk1 = sconX_.scolnk[sconp1-One];
		sclnk2 = sconX_.scolnk[sconp2-One];
		sconX_.scono[sclnk2-One][60-SCONX1-One] = sconX_.scono[sclnk1-One][60-SCONX1-One];
		sconX_.scono[sclnk2-One][59-SCONX1-One] = sconX_.scono[sclnk1-One][59-SCONX1-One];
		}
	else if( vbdataX_.k3p1 == 868 ){


		/*             -48 868  indicate which Black Hole will receive
		 *               subsequent OPADR strings bracketed by -868 on the
		 *               left and -869 on the right. */

		/*                  zero the BH variables if K3P2=0 */
		if( k3p2 == 0 ){
			sw48bhX_.bhpt48 = 0;
			sw48bhX_.bhct48 = 0;
			/*                  K3P2=800 or 900 means directional search for
			 *                      the K3P3'th Black Hole to lft or rt, respectively */
			}
		else if( k3p2 == 900 ){
			sw48bhX_.bhpt48 = BFMVRT;
			sw48bhX_.bhct48 = semargX_.k3p3;
			}
		else if( k3p2 == 800 ){
			sw48bhX_.bhpt48 = BFMVLF;
			sw48bhX_.bhct48 = semargX_.k3p3;
			/*                  K3P2 can be a ptr to a Cell */
			}
		else if( k3p2 > 0 && k3p2 <= 100 ){
			xx = vbdataX_.vbcell[k3p2-One];
			if( xx > 0 && xx <= SCONY ){
				/*                  otherwise K3P2 should be a relative pointer */
				sw48bhX_.bhpt48 = xx;
				sw48bhX_.bhct48 = semargX_.k3p3;
				}
			else if( diagsX_.deepdi == 1 || diagsX_.longdi == 1 ){
				fprintf( _spec_fp, "\nERROR in switch -48 fcn 868 :\n  Cell contains invalid SCON pointer (CELL(%3d)=%3d\n\nERROR in switch -48 fcn 868 :\n  Cell contains invalid SCON pointer (CELL(%3d)=\n", 
				  k3p2, semargX_.k3p3, xx );
				}
			}
		else if( k3p2 < -70 && k3p2 >= -90 ){
			xx = im81X_.im81 - k3p2;
			sw48bhX_.bhpt48 = sworkX_.swork[xx-One][4-One];
			sw48bhX_.bhct48 = semargX_.k3p3;

			}
		else{
			fprintf( _spec_fp, "\nERROR in switch -48 fcn 868 :\n  invalid parameters: %3d %3d\n", 
			  k3p2, semargX_.k3p3 );
			}

		}
	else if( vbdataX_.k3p1 == 1 || vbdataX_.k3p1 == 3 ){



		for( x1=phrst1; x1 <= phrls1; x1++ ){
			if( !(opadriX_.opadri[x1-One] < -115 || 
				  opadriX_.opadri[x1-One] > -110) ){
				if( !(opadriX_.opadri[x1-One] == -111 ||
					  opadriX_.opadri[x1-One] == -114) ){
					if( hpdopiX_.hfdopi[x1-One] != 0 ){

						for( x2=phrst2; x2 <= phrls2; x2++ ){
							if( opadriX_.opadri[x2-One] == opadriX_.opadri[x1-One] )
								goto L_8137;

							}
						continue;
L_8137:
						if( hpdopiX_.hfdopi[x1-One] >= HFPOLO &&
							hpdopiX_.hfdopi[x1-One] <= HFPOHI ){

							h1 = hpdopiX_.hfdopi[x1-One] - HFPOL1;
							hfdoaX_.adct += 1;
							if( opswX_.sw[3-One] == 1 )
								{
								fprintf( _spec_fp, "    NOW %2d HFDOPO VC\\\"S (%2ld-%2ld)\n", 
								  hfdoaX_.adct, HFPOLO, HFPOHI );
								}
							if( hfdoaX_.adct > 5 )
								goto L_8138;
							h2 = hfdoaX_.adct;
							/*+    REDIM. VC-PHRASE ARRAYS                    05/08/86  *B0406DSD */
							y = hfdoaX_.hfpoad[h1-One][HFPADX-One];
							hpdopiX_.hfdopi[x2-One] = hfdoaX_.adct + HFPOL1;
							hfdoaX_.hfpoad[h2-One][HFPADX-One] = y;
							if( y != 0 ){

								for( l=1; l <= y; l++ ){

									/*   PR 1695  DO NOT MOVE OR COPY VC'S WITHIN A VC (GE ONLY) */
									if(tranidX_.tranid != 2 ||
					                   !((srcflgX_.srcflg == 1 && 
									      hfdoaX_.hfpoad[h1-One][0] >= -120) && hfdoaX_.hfpoad[h1-One][0] <= -100) ){

										hfdoaX_.hfpoad[h2-One][l-One] = hfdoaX_.hfpoad[h1-One][l-One];
										om = hfdoaX_.sconhf[h1-One][l-One];
										hfdoaX_.sconhf[h2-One][l-One] = om;
										if( sconX_.scon[om-One][0] > 0 )
											sconX_.scon[om-One][0] = -sconX_.scon[om-One][0];

										for( p=2; p <= 14; p++ ){
											sconX_.scon[om-One][p-One] = sconX_.scon[om-One][p-One];
											}
									}
									}

								if( vbdataX_.k3p1 == 3 )
									hpdopiX_.hfdopi[x1-One] = 0;
								}
							}
						else{
							hpdopiX_.hfdopi[x2-One] = hpdopiX_.hfdopi[x1-One];
							if( vbdataX_.k3p1 == 3 )
								hpdopiX_.hfdopi[x1-One] = 0;

							s1 = opadriX_.sconpi[x1-One];
							s2 = opadriX_.sconpi[x2-One];

							if(tranidX_.tranid == 2){
								for( s3=4; s3 <= 7; s3++ ){
								sconX_.scon[s2-One][s3-One] = sconX_.scon[s1-One][s3-One];
								}
							}
							sconX_.scon[s2-One][7-One] = sconX_.scon[s1-One][7-One];
							if( sconX_.scon[s2-One][0] > 0 )
								sconX_.scon[s2-One][0] = -sconX_.scon[s2-One][0];
							if( vbdataX_.k3p1 == 3 ){

								for( l=1; l <= 14; l++ ){
									sconX_.scon[s1-One][l-One] = 0;
									}
								}
							}
						}
					}
				}
			}

		sconX_.scon[sworkX_.phrhed[n7jim-One]-One][7-One] = sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][7-One];

		goto L_7840;
L_8138:
		if( opswX_.sw[3-One] == 1 )
			{
			fprintf( _spec_fp, " HFDOPO %2ld TO %2ld ALREADY TAKEN - EXIT SW48\n", 
			  HFPOLO, HFPOHI );
			}
		errlog(pgmnam,8025,502,0);
		}
	else if( vbdataX_.k3p1 == 2 ){

		p = 0;
		for( x2=phrst2; x2 <= phrls2; x2++ ){

			/*           ?? VC104 LOADED WITH 171 ?? */
			if( opadriX_.opadri[x2-One] == -104 &&
				hpdopiX_.hfdopi[x2-One] == -171 )
				p += 1;

			/*+                                                         *R1101MBS
			 *          TEST FOR -114 DELETED
			 *-                                                         *R1101MBS */
			s2 = opadriX_.sconpi[x2-One];
			if( sconX_.scon[s2-One][0] >= 0 ){
				if( hpdopiX_.hfdopi[x2-One] >= HFPOLO && hpdopiX_.hfdopi[x2-One] <= 
				  HFPOHI ){

					h2 = hpdopiX_.hfdopi[x2-One] - HFPOL1;
					y = hfdoaX_.hfpoad[h2-One][HFPADX-One];
					for( x22=1; x22 <= y; x22++ ){
						sconX_.scon[hfdoaX_.sconhf[h2-One][x22-One]-
						  One][4-One] = sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][4-One];
						sconX_.scon[hfdoaX_.sconhf[h2-One][x22-One]-
						  One][6-One] = sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][6-One];
						}
					}
				else{
					sconX_.scon[s2-One][4-One] = sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][4-One];
					sconX_.scon[s2-One][6-One] = sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][6-One];
					}
				}
			}

		if( p >= 1 ){
			s7 = sconX_.scon[sworkX_.phrhed[n7jim-One]-One][7-One];
			if( s7 == 2 || s7 == 3 )
				sconX_.scon[sworkX_.phrhed[n7jim-One]-One][7-One] = 1;
			}


		if( tranidX_.tranid == 2 && srcflgX_.srcflg == 1 ){

			/*        THIS SECTION RESOLVES SOURCE/TARGET NUMBER DISCREPTANCIES.
			 *        IT LOOKS AT WHAT THE NUMBER OF THE SUBJECT AND
			 *        IF IT'S NUMBER IS NOT IN AGREEMENT WITH WHAT IT'S SOURCE WAS,
			 *        IT WILL FORCE THE VERB TO AGREE WITH IT. */

			om = 0;
			ofl2 = sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][3-One];
			if( ofl2 >= 7 && ofl2 <= 9 )
				om = 2;
			if( ofl2 >= 4 && ofl2 <= 6 )
				om = 1;
			if( om != 0 ){
				for( x1=phrst2; x1 <= phrls2; x1++ ){
					if( sconX_.scon[opadriX_.sconpi[x1-One]-One][1-One] >= 0 )
						sconX_.scon[opadriX_.sconpi[x1-One]-One][5-One] = om;
					}

				/*   NOW LOCK OUT THE SUBJECT AND MAKE IT'S NUMBER AGREE WITH VERB */

				for( x1=phrst1; x1 <= phrls1; x1++ ){
					if( sconX_.scon[opadriX_.sconpi[x1-One]-One][1-One] >= 0 )
						sconX_.scon[opadriX_.sconpi[x1-One]-One][5-One] = om;
					}
				goto L_7840;
				}
			}



		/*        FORM MUST BE IN FORM48  */
		if(form48_check(formsaX_.formsv[sconX_.scolnk[sworkX_.swork[flowckX_.n6jim-One][4-One]-One]-One]) == 0) goto L_7840;

		if( sconX_.scon[sworkX_.phrhed[n7jim-One]-One][5-One] != 0 ){
			for( x1=phrst1; x1 <= phrls1; x1++ ){
				s1 = opadriX_.sconpi[x1-One];
				if( s1 == sworkX_.phrhed[flowckX_.n6jim-One] )
					goto L_8139;
				}
			goto L_7840;
L_8139:
			if( sconX_.scon[s1-One][0] >= 0 ){
				sconX_.scon[s1-One][0] = -sconX_.scon[s1-One][0];
				sconX_.scon[s1-One][5-One] = sconX_.scon[sworkX_.phrhed[n7jim-One]-One][5-One];
				if( sconX_.scon[s1-1-One][0] == 14 && x1 > phrst1 )
					sconX_.scon[s1-1-One][0] = -sconX_.scon[s1-1-One][0];
				}
			}

		}
	else if( diagsX_.longdi == 1 ){
		fprintf( _spec_fp, " ILLEGAL FIRST PARAMETER,SWITCH 48 %4d\n", 
		  vbdataX_.k3p1 );
		}


L_7840:
	if( diagsX_.deepdi == 1 )
		{
		fprintf( _spec_fp, " AT END SW48 %4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d\n", 
		  vbdataX_.k3p1, k3p2, semargX_.k3p3, phrst1, phrls1, phrst2, 
		  phrls2, flowckX_.n6jim, n7jim, vbdataX_.k3n, s1, s2 );
		}

	return;
} /*end of function*/

