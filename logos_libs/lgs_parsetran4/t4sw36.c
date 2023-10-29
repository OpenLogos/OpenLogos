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
	/* changes:
	 *     11/23/92 AVK: If german phrasal transfer, use pat of head element
	 *     10/12/92 jal: increase size of imatch to elmmax.
	 *     05/02/87 *R1691RKH*  Deactivate SCONPI for -140 in OPADR
	 *     04/17/87 *R1685RKH*  Change T1-4 SWORK limit from 50 to
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



void /*FUNCTION*/ t4sw36()
{
	static short int addr, adr2, jz2, m, relptr, savadr, sc1, scnrow, sctemp, setscn, subfcn, x, xx;
	static long int _l0, tmpi4;
	int taddress,dictype;
	static char pgmnam[9] = "T4SW36  ";
	static short gz = 0;
	static short iz = 0;
	static short kz = 0;
	static short mm = 0;
	static short ms = 0;
	static short nn = 0;
	static short zz = 0;
	static short iz2 = 0;
	static short gb36 = 0;
	static short k3p2 = 0;
	static short sw36no = 0;

	vbdataX_.k3n = vbdataX_.k3 + 3;

	k3p2 = vtrfX_.vtrf[vbdataX_.k3+2-One];
	if( flag32X_.ad32 == -1 ){

		
		
		
		if( vbdataX_.k3p1 == 9 ){
			/*     FUNCTION 9 - NULLIFY ADDRESSES IN PHRASE */
			flowckX_.n6jim = im81X_.im81 - k3p2;
			flowckX_.phrstr = sworkX_.phrbeg[flowckX_.n6jim-One];
			flowckX_.phrlst = sworkX_.phrend[flowckX_.n6jim-One];
			for( ms=flowckX_.phrstr; ms <= flowckX_.phrlst; ms++ ){
				opadriX_.opadri[ms-One] = -140;
				hpdopiX_.hfdopi[ms-One] = 0;
				/*+ Deactivate SCONPI for -140 in OPADRI         RKH  05/02/87   R1691 */
				if( opadriX_.sconpi[ms-One] != sworkX_.phrhed[flowckX_.n6jim-One] )
					opadriX_.sconpi[ms-One] = 1;
				}
			return;
			}





		else if( vbdataX_.k3p1 == 47 ){

			/*       FUNCTION 47
			 *       PURPOSE: LOAD HEAD OF PHRASE K3P2 INTO OPADRI,
			 *       CREATE A SCON FOR IT WITH SCON(7) = 9 */

			flowckX_.n6jim = im81X_.im81 - k3p2;
			flowckX_.phrstr = sworkX_.phrbeg[flowckX_.n6jim-One];
			flowckX_.phrlst = sworkX_.phrend[flowckX_.n6jim-One];
			for( mm=flowckX_.phrstr; mm <= flowckX_.phrlst; mm++ ){
				if( opadriX_.sconpi[mm-One] == sworkX_.phrhed[flowckX_.n6jim-One] )
					break;
				}
			opadroX_.opo += 1;
			opadroX_.opadro[opadroX_.opo-One] = opadriX_.opadri[mm-One];
			prtscoX_.sct += 1;

			if( prtscoX_.sct > SCONY ){
				prtscoX_.sct = SCONY;
				prtscoX_.scterr += 1;
				if( diagsX_.longdi == 1  )
					{
					fprintf( _spec_fp, " T4SW36, OVERLOADING SCON ARRAY, SCTERR =%4d\n", 
					  prtscoX_.scterr );
					}
				if( prtscoX_.scterr == 1 )
					errlog(pgmnam,6892,500,13);
				opadroX_.opo -= 1;
				}
			else{
				opadroX_.sconpo[opadroX_.opo-One] = prtscoX_.sct;
				for( nn=1; nn <= 14; nn++ ){
					sconX_.scon[prtscoX_.sct-One][nn-One] = 0;
					}
				sconX_.scon[prtscoX_.sct-One][1-One] = 22;
				sconX_.scon[prtscoX_.sct-One][2-One] = opadroX_.opadro[opadroX_.opo-One];
				sconX_.scon[prtscoX_.sct-One][7-One] = 9;
				sconX_.scon[prtscoX_.sct-One][10-One] = sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][10-One];
				}
			return;





			/*   50, 51 AND 66 ARE FLAGS TO THE CHECKER PROGRAMS */
			}
		else if( (vbdataX_.k3p1 == 50 || vbdataX_.k3p1 == 51) || vbdataX_.k3p1 == 66 ){
			return;
			}




		else if( vbdataX_.k3p1 == 46 ){
			/*      FUNCTION 46
			 *        REPLACE FORM FIELD WITH FORMSAV VALUE */
			flowckX_.n6jim = im81X_.im81 - k3p2;
			sworkX_.swork[flowckX_.n6jim-One][3-One] = formsaX_.formsv[sconX_.scolnk[sworkX_.swork[flowckX_.n6jim-One][4-One]-One]-One];
			return;
			}

		
		else if( vbdataX_.k3p1 == 53 ){
			flowckX_.n6jim = im81X_.im81 - k3p2;
			if( sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][1-One] < 0 )
				sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][1-One] *= -1;
			return;
			}




		else if( vbdataX_.k3p1 == 56 ){
			/*           FUNCTION 56 - SAVE RULE ADDRESS TO PREVENT LOOPS */
			/*               FUNCTION -56001 IS HANDLED FIRST: */

			/*       CALL36(1) IS THE ADDRESS RECORDED IN 42 SWITCH OF ORIGINAL
			 *       RULE THAT CALLED A WORD CLASS 10 RULE (USE WITH -36056001)
			 *       CALL36(2) IS 0 UNLESS ORIGINAL CALLING RULE IS FROM MINI (=1)
			 *+ */
			if( k3p2 == 0 ){

				/*                                0GM 10/86  FIX -36056 FOR MINI/MAIN  +
				 *   If IDENSP or IDEN42 are set, then we found identical rules in the
				 *   mini/main files for the non WC10 rules and the WC10 rules respectiv. */

				if( minickX_.minifg == 1 ){
					if( !((spcompX_.idensp == 1 && vtrs42X_.sw42n == 0) ||
						  (spcompX_.iden42 == 1 && vtrs42X_.sw42n == 1)) )
						goto L_540;
					}
				/*                                0GM 10/86  FIX -36056 FOR MINI/MAIN  - */

				/*          FUNCTION 056000  - SAVING ADDRESS OF THIS RULE */

				if( loopckX_.noloop[1-One] == 0 ){
					loopckX_.noloop[1-One] = vwarg2X_.i;
					if( vtrs42X_.sw42n == 1 )
						loopckX_.noloop[1-One] = vwarg1X_.isave;
					}
				for( gb36=2; gb36 <= 21; gb36++ ){
					if( loopckX_.noloop[gb36-One] == 0 )
						goto L_621;
					}
				goto L_500;
L_621:
				loopckX_.noloop[gb36-One] = loopckX_.nptpsv;
				/*                                0GM 10/86  FIX -36056 FOR MINI/MAIN  + */
				if( vtrs42X_.sw42n == 0 && spcompX_.idensp == 1 ){
					loopckX_.noloop[gb36-One] = spcompX_.nptpm;

					}
				else if( vtrs42X_.sw42n == 1 && spcompX_.iden42 == 1 ){
					loopckX_.noloop[gb36-One] = spcompX_.nptp42;
					}
				else{
					if( diagsX_.deepdi == 1 )
						{
						fprintf( _spec_fp, " IN SW36,NOLOOP VAL =  " );
						for( gz=1; gz <= 21; gz++ ){
							fprintf( _spec_fp, "%6d", loopckX_.noloop[gz-One] );
							}
						fprintf( _spec_fp, "\n" );
						}
					return;
					}

				/*         NLOOP2 STORES ADDRESS OF MINI RULE */

L_540:
				if( nloop2X_.nloop2[1-One] == 0 ){
					nloop2X_.nloop2[1-One] = vwarg2X_.i;
					if( vtrs42X_.sw42n == 1 )
						nloop2X_.nloop2[1-One] = vwarg1X_.isave;
					}
				for( gb36=2; gb36 <= 21; gb36++ ){
					if( nloop2X_.nloop2[gb36-One] == 0 )
						goto L_622;
					}
				goto L_500;
L_622:
				nloop2X_.nloop2[gb36-One] = loopckX_.nptpsv;
				if( diagsX_.deepdi == 1 )
					{
					fprintf( _spec_fp, " IN 36 SW,NLOOP2 =  " );
					for( gz=1; gz <= 21; gz++ ){
						fprintf( _spec_fp, "%6d", nloop2X_.nloop2[gz-One] );
						}
					fprintf( _spec_fp, "\n" );
					}
				return;
				}
			else{

				/*                                0GM 10/86  FIX -36056 FOR MINI/MAIN  +
				 *       idensp=1 indicates that both the main WC and mini WC rules
				 *       which called the WC 10 rule are identical. */

				/*       IS CALLING RULE IN CALL36(1) FROM A MINI (CALL36(2) = 1)? */
				if( !(loopckX_.call36[2-One] == 1 && spcompX_.idensp != 1) ){
					/*                                0GM 10/86  FIX -36056 FOR MINI/MAIN  -
					 *      SAVE ORIGINAL CALLING RULE IN NOLOOP ARRAY */

					if( loopckX_.noloop[1-One] == 0 ){
						loopckX_.noloop[1-One] = vwarg2X_.i;
						if( vtrs42X_.sw42n == 1 )
							loopckX_.noloop[1-One] = vwarg1X_.isave;
						}
					for( gb36=2; gb36 <= 21; gb36++ ){
						if( loopckX_.noloop[gb36-One] == 0 )
							goto L_623;

						}
					goto L_500;
L_623:
					loopckX_.noloop[gb36-One] = loopckX_.call36[1-One];
					/*                                0GM 10/86  FIX -36056 FOR MINI/MAIN  + */
					if( spcompX_.idensp != 1 )
						return;
					loopckX_.noloop[gb36-One] = spcompX_.nptpm;
					}

				/*      ORIGINAL CALLING RULE IS FROM A MINI: SAVE IN NLOOP2 ARRAY */

				if( nloop2X_.nloop2[1-One] == 0 ){
					nloop2X_.nloop2[1-One] = vwarg2X_.i;
					if( vtrs42X_.sw42n == 1 )
						nloop2X_.nloop2[1-One] = vwarg1X_.isave;
					}
				for( gb36=2; gb36 <= 21; gb36++ ){
					if( nloop2X_.nloop2[gb36-One] == 0 )
						goto L_624;

					}
				goto L_500;
L_624:
				nloop2X_.nloop2[gb36-One] = loopckX_.call36[1-One];
				return;
				}

			/*    OVERLOADING NOLOOP ARRAY */

L_500:
			if( diagsX_.longdi == 1  )
				{
				fprintf( _spec_fp, " -36056 OVERLOADING NOLOOP ARRAY\n" );
				}
			return;
			}
		else if( !(vbdataX_.k3p1 < 11 || vbdataX_.k3p1 > 14) ){

			if( srcflgX_.srcflg == 1 ){

				/*             WC 1 5 6 7 AND 8 ONLY */
				sw36no = im81X_.im81 - k3p2;
				iz2 = sworkX_.phrhed[sw36no-One];
				iz = sconX_.scon[iz2-One][1-One];
				if( iz > 0 ){
					if( iz <= 8 ){

						if( iz == 1 || (iz >= 5 && iz <= 8) ){



							/*            set source case in SC24 and propigate */

							xx = im81X_.im81 - k3p2;
							scnrow = sworkX_.swork[xx-One][4-One];
							if( sconX_.scon[scnrow-One][1-One] >= 0 ){
								sconX_.scono[sconX_.scolnk[scnrow-One]-One][24-SCONX1-One] = vbdataX_.k3p1 - 10;
								/*                   propigate the changes
								 *                    apply only to unset (scon=0) and unlocked scons
								 *                    concatenated in the input OPADR
								 *                     i.e. subfcn 20 of SW48 */
								subfcn = 20;
								relptr = k3p2;
								setscn = 24;
								txsw48(subfcn,relptr,setscn);
								}

							/*               Only consider setting number of target words that can take
							 *               sing or plural number */
							if( sconX_.scon[iz2-One][3-One] <= 3 ){
								/*             FOR DECLENTION OF NOUNS BY NUMBER */
								iz = nounsX_.gernum[vbdataX_.k3p1-10-One][formsaX_.formsv[sconX_.scolnk[sworkX_.swork[sw36no-One][4-One]-
								  One]-One]-One];

								if( !(iz < 1 || iz > 99) ){

									flowckX_.n6jim = im81X_.im81 - k3p2;
									flowckX_.phrstr = sworkX_.phrbeg[flowckX_.n6jim-One];
									flowckX_.phrlst = sworkX_.phrend[flowckX_.n6jim-One];
									for( zz=flowckX_.phrstr; zz <= flowckX_.phrlst; zz++ ){
										if( sconX_.scon[opadriX_.sconpi[zz-One]-
										  One][1-One] >= 0 ){
											if( sconX_.scon[opadriX_.sconpi[zz-One]-One][1-One] == 1 )
												iz = nounsX_.gernum[vbdataX_.k3p1-10-One][formsaX_.formsv[sconX_.scolnk[opadriX_.sconpi[zz-One]-
												  One]-One]-One];
											sconX_.scon[opadriX_.sconpi[zz-One]-One][5-One] = iz;
											/*                FOR MULTI LOADED VC, SET ALL COMPONENT ELEMENTS */
											if( opadriX_.opadri[zz-One] <= -100 && opadriX_.opadri[zz-One] >= -120 ){
												if( hpdopiX_.hfdopi[zz-One] >= HFPOLO && hpdopiX_.hfdopi[zz-One] <= HFPOHI ){
												iz2 = iz;
												adr2 = hpdopiX_.hfdopi[zz-One] - HFPOL1;
												jz2 = hfdoaX_.hfpoad[adr2-One][HFPADX-One];
												for( m=1; m <= jz2; m++ ){
												sc1 = sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][1-One];
												if( sc1 >= 0 ){
												if( sc1 == 1 ){
												sctemp = sconX_.scolnk[hfdoaX_.sconhf[adr2-One][m-One]-One];
												if( sctemp > 0 && sctemp <= elemctX_.elemct )
												iz2 = nounsX_.gernum[vbdataX_.k3p1-10-One][formsaX_.formsv[sctemp-One]-One];
												}
												sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][5-One] = iz2;
												}
												}
												}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			return;
			}
		else if( vbdataX_.k3p1 == 99 ){
			return;

			}
		else if( vbdataX_.k3p1 <= 100 ){
			return;
			}
		}


	flowckX_.n6jim = im81X_.im81 - k3p2;
	addr = -vbdataX_.k3p1;
	if( flag32X_.ad32 != -1 ){
		addr = vbdataX_.k3p1 + 1000*flag32X_.ad32;
		flag32X_.ad32 = -1;
		}

	if( sconX_.scon[sworkX_.phrhed[flowckX_.n6jim-One]-One][1-One] >= 0 ){

		flowckX_.phrstr = sworkX_.phrbeg[flowckX_.n6jim-One];
		flowckX_.phrlst = sworkX_.phrend[flowckX_.n6jim-One];
		for( kz=flowckX_.phrstr; kz <= flowckX_.phrlst; kz++ ){
			if( opadriX_.sconpi[kz-One] == sworkX_.phrhed[flowckX_.n6jim-One] )
				goto L_625;
			}
		return;
L_625:
		savadr = opadriX_.opadri[kz-One];
		opadriX_.opadri[kz-One] = addr;
		if( addr < 0 && addr > -1000 ){
			sconX_.scono[sconX_.scolnk[opadriX_.sconpi[kz-One]-One]-One][60-SCONX1-One] = -addr;
			taddress = -addr;
			dictype = 2;
		}
		else{
			taddress = addr;
			dictype = 3;
		}

		errvrsX_.err = TARG_CODES(&trgflgX_.trgflg,&dictype,
			       &taddress,
				   LOGCC,
				   (short *)&trgcdsX_,diagsX_.longdi, _spec_fp);
		if( errvrsX_.err == 0 ){
			sconX_.scono[sconX_.scolnk[opadriX_.sconpi[kz-One]-One]-One][59-SCONX1-One] = trgcdsX_.tcpatm[1-One];
			}
		else{
			errlog(pgmnam,5361,taddress,13);
			opadriX_.opadri[kz-One] = savadr;
			}
			
		}
	return;
} /*end of function*/

