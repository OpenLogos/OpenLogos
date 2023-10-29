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
	/*   ***** BEGINNING OF -49 SWITCH ***** */
	/*      FUNCTION:  SETTING OF VARIABLE CONSTANTS
	 *        WITH THE PARTICULAR ABILITY TO SET MULTIPLE VC'S WITHIN
	 *        AN ALREADY CONCATENATED PHRBEG */
	/*                         ILLEGAL IN TRAN1 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <fcrt.h>
#include "trans.h"
#include "logoslib.h"
#include "projexts.h"

#include <string.h>
#include <jbctrl.h>
#include "parsetrans_ext.h"


void /*FUNCTION*/ t0sw49()
{
	static short int iz1, k3p5, triggr, x;
	static long int _do0, _do1, _do2, _do3, _do4, _do5, _n;

	static char pgmnam[9] = "T0SW49  ";
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

	
	if( tranidX_.tranid == 1 ){
		errlog(pgmnam,11,0,6);
		if( diagsX_.deepdi == 1 || diagsX_.longdi == 1 )
			{
			fprintf( _spec_fp, "\n*** -49 ILLEGAL IN TRAN1, IGNORING THE SWITCHCONTINUING THE VTR ***\n" );
			}
		}
	else{

		vbdataX_.k3n = vbdataX_.k3 + 6;
		vbdataX_.k3p1 = vtrfX_.vtrf[vbdataX_.k3+1-One];
		k3p2 = vtrfX_.vtrf[vbdataX_.k3+2-One];
		semargX_.k3p3 = vtrfX_.vtrf[vbdataX_.k3+3-One];
		k3p4 = vtrfX_.vtrf[vbdataX_.k3+4-One];
		k3p5 = vtrfX_.vtrf[vbdataX_.k3+5-One];

		/*    K3P1 IS A RELATIVE POINTER (SEARCH INCOMING PHRASE FOR V.C. K3P2) */
		xx = 1;
		flowckX_.phrstr = sworkX_.phrbeg[im81X_.im81-vbdataX_.k3p1-One];
		flowckX_.phrlst = sworkX_.phrend[im81X_.im81-vbdataX_.k3p1-One];

		/*     FIND THE FIRST OCCURRENCE OF THE VC WITHIN THE PHRASE */
		for( iz=flowckX_.phrstr, _do0 = flowckX_.phrlst; iz <= _do0; iz++ ){
			if( opadriX_.opadri[iz-One] == -k3p2 )
				goto L_4940;
			/*     IF FIRST OCCURRENCE NOT FOUND, THEN EXIT */
			}
		goto L_6999;

L_4940:
		x = x;
		kw = hpdopiX_.hfdopi[iz-One];
		/* PARM3= 0 IS ERROR */
		if( semargX_.k3p3 != 0 ){
			/* PARM3 > 0 IS HIGH-FREQ CONSTANT. */
			if( semargX_.k3p3 > 120 && semargX_.k3p3 <= 999 ){
				hpdopiX_.hfdopi[iz-One] = -semargX_.k3p3;
				}
			else{
				/* PARM3 < 0 IS RELATIVE POINTER? */
				if( kw < HFPOLO || kw > HFPOHI ){
					x = x;
					flowckX_.n6jim = im81X_.im81 - semargX_.k3p3;
					flowckX_.phrstr = sworkX_.phrbeg[flowckX_.n6jim-One];
					flowckX_.phrlst = sworkX_.phrend[flowckX_.n6jim-One];
					iz2 = flowckX_.phrlst - flowckX_.phrstr + 
					  1;
					if( iz2 != 1 ){
						jz = 1;
						hfdoaX_.adct += 1;
						x = x;
						adr_ = hfdoaX_.adct;
						goto L_5450;
						}
					}
				else{
					jz = 1;
					adr_ = kw - HFPOL1;
					x = x;
					flowckX_.n6jim = im81X_.im81 - semargX_.k3p3;
					flowckX_.phrstr = sworkX_.phrbeg[flowckX_.n6jim-One];
					flowckX_.phrlst = sworkX_.phrend[flowckX_.n6jim-One];
					iz2 = flowckX_.phrlst - flowckX_.phrstr + 
					  1;
					if( iz1 != 1 )
						goto L_5450;
					}

				x = x;
				hpdopiX_.hfdopi[iz-One] = opadriX_.opadri[flowckX_.phrstr-One];
				opadriX_.sconpi[iz-One] = opadriX_.sconpi[flowckX_.phrstr-One];
				goto L_6000;
L_5450:
				x = x;
				for( iz2=flowckX_.phrstr, _do1 = flowckX_.phrlst; iz2 <= _do1; iz2++ ){
					hfdoaX_.hfpoad[adr_-One][jz-One] = opadriX_.opadri[iz2-One];
					if( hpdopiX_.hfdopi[iz2-One] != 0 )
						hfdoaX_.hfpoad[adr_-One][jz-One] = hpdopiX_.hfdopi[iz2-One];
					if( hpdopiX_.hfdopi[iz2-One] < HFPOLO || hpdopiX_.hfdopi[iz2-One] > 
					  HFPOHI ){
						hfdoaX_.sconhf[adr_-One][jz-One] = opadriX_.sconpi[iz2-One];
						jz += 1;
						if( jz > HFPADX )
							goto L_6000;
						}
					else{
						adr2 = hpdopiX_.hfdopi[iz2-One] - HFPOL1;
						jz2 = hfdoaX_.hfpoad[adr2-One][HFPADX-One];
						for( m=1, _do2 = jz2; m <= _do2; m++ ){
							hfdoaX_.hfpoad[adr_-One][jz-One] = hfdoaX_.hfpoad[adr2-One][m-One];
							hfdoaX_.sconhf[adr_-One][jz-One] = hfdoaX_.sconhf[adr2-One][m-One];
							jz += 1;
							if( jz > HFPADX )
								goto L_6000;
							}
						}
					}
				x = x;
				hfdoaX_.hfpoad[adr_-One][HFPADX-One] = jz - 1;
				hpdopiX_.hfdopi[iz-One] = HFPOL1 + adr_;
				}

			/*     FIND ANY ADDITIONAL OCCURRENCE OF THE VC WITHIN THE PHRASE */
L_6000:
			x = x;
			flowckX_.phrstr = sworkX_.phrbeg[im81X_.im81-vbdataX_.k3p1-One];
			flowckX_.phrlst = sworkX_.phrend[im81X_.im81-vbdataX_.k3p1-One];
			if( iz != flowckX_.phrlst ){
				triggr = 0;
				if( k3p4 == 0 )
					k3p4 = semargX_.k3p3;
				for( iz=iz + 1, _do3 = flowckX_.phrlst; iz <= _do3; iz++ ){
					if( opadriX_.opadri[iz-One] == -k3p5 )
						triggr = 1;
					if( triggr == 1 && opadriX_.opadri[iz-One] == 
					  -k3p2 ){
						triggr = 0;
						if( k3p4 > 0 ){
							hpdopiX_.hfdopi[iz-One] = -k3p4;
							}
						else{
							/* PARM4 < 0 IS RELATIVE POINTER? */
							if( kw < HFPOLO || kw > HFPOHI ){
								x = x;
								flowckX_.n6jim = im81X_.im81 - k3p4;
								flowckX_.phrstr = sworkX_.phrbeg[flowckX_.n6jim-One];
								flowckX_.phrlst = sworkX_.phrend[flowckX_.n6jim-One];
								iz2 = flowckX_.phrlst - flowckX_.phrstr + 
								  1;
								if( iz2 != 1 ){
									jz = 1;
									hfdoaX_.adct += 1;
									x = x;
									adr_ = hfdoaX_.adct;
									goto L_7450;
									}
								}
							else{
								jz = 1;
								adr_ = kw - HFPOL1;
								x = x;
								flowckX_.n6jim = im81X_.im81 - k3p4;
								flowckX_.phrstr = sworkX_.phrbeg[flowckX_.n6jim-One];
								flowckX_.phrlst = sworkX_.phrend[flowckX_.n6jim-One];
								iz2 = flowckX_.phrlst - flowckX_.phrstr + 
								  1;
								if( iz1 != 1 )
									goto L_7450;
								}

							x = x;
							hpdopiX_.hfdopi[iz-One] = opadriX_.opadri[flowckX_.phrstr-One];
							opadriX_.sconpi[iz-One] = opadriX_.sconpi[flowckX_.phrstr-One];
							continue;
L_7450:
							x = x;
							for( iz2=flowckX_.phrstr, _do4 = flowckX_.phrlst; iz2 <= _do4; iz2++ ){
								hfdoaX_.hfpoad[adr_-One][jz-One] = opadriX_.opadri[iz2-One];
								if( hpdopiX_.hfdopi[iz2-One] != 0 )
									hfdoaX_.hfpoad[adr_-One][jz-One] = hpdopiX_.hfdopi[iz2-One];
								if( hpdopiX_.hfdopi[iz2-One] < HFPOLO || 
								  hpdopiX_.hfdopi[iz2-One] > HFPOHI ){
									hfdoaX_.sconhf[adr_-One][jz-One] = opadriX_.sconpi[iz2-One];
									jz += 1;
									if( jz > HFPADX )
										goto L_10000;
									}
								else{
									adr2 = hpdopiX_.hfdopi[iz2-One] - 
									  HFPOL1;
									jz2 = hfdoaX_.hfpoad[adr2-One][HFPADX-One];
									for( m=1, _do5 = jz2; m <= _do5; m++ ){
										hfdoaX_.hfpoad[adr_-One][jz-One] = hfdoaX_.hfpoad[adr2-One][m-One];
										hfdoaX_.sconhf[adr_-One][jz-One] = hfdoaX_.sconhf[adr2-One][m-One];
										jz += 1;
										if( jz > HFPADX )
											goto L_10000;
										}
									}
								}
							x = x;
							hfdoaX_.hfpoad[adr_-One][HFPADX-One] = jz - 1;
							hpdopiX_.hfdopi[iz-One] = HFPOL1 + adr_;
							}
						}
L_10000:
					;
					}
				}
			}
		/*     FINISHED WITH PHRASE */

L_6999:
		x = x;
		if( opswX_.sw[20-One] == 1 ){

			fprintf( _spec_fp, " ** END SW49  %4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d", 
			  flowckX_.n6jims, flowckX_.n6jim, flowckX_.phrstr, 
			  flowckX_.phrlst, vbdataX_.k3p1, k3p2, semargX_.k3p3, 
			  k3p4, k3p5, iz, adr_, xx, hpdopiX_.hfdopi[iz-One], opadriX_.opadri[iz-One], 
			  hfdoaX_.adct, n6p1 );
			fprintf( _spec_fp, "\n BUCKET:" );
			for(_n=0L; _n < sizeof(bucket)/sizeof(short); _n++)
				fprintf( _spec_fp, "%4d", ((short*)bucket)[_n] );

			if( adr_ > 0 )
				{
				fprintf( _spec_fp, " HFPOAD \n " );
				for( q=1; q <= HFPADX; q++ ){
					fprintf( _spec_fp, "%6d", hfdoaX_.hfpoad[adr_-One][q-One] );
					}
				fprintf( _spec_fp, "\n" );
				}


			}
		}
	return;
} /*end of function*/

