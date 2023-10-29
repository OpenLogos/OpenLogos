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
	 *       9/27/93 jal: index into JS goes via SCOLNK now.
	 *      07/12/93 jal: set the black hole count flag for new pos.
	 *      05/26/93 jal: access js directly off scon ptr not indirectly
	 *                    through SCOLNK
	 *      10/12/92 jal: increase size of imatch.
	 *      03/06/91 *JAL*: SET SCON1 FROM NEW WC SELECTED. SET SCON2 TO
	 *           OFL1R OF NEW WC SELECTED
	 *      04/23/87 *R1679RKH*  OFL3B CONVERSION & R1685 SWORK LIMI
	 *      04/01/87 *R1689RKH*  ALTERNATE WC SEARCH
	 *      08/22/86 *R1561DSD: 100 SCONS
	/*        -29 SWITCH - FOR TRAN2, 3, 4 */
	/*        INVOKES A SEARCH OF THE UNMATCHED SWORKS OF AN ELEMENT.
	 *        IF MATCHED THE SWORK PASSED FROM RES WILL BE REPLACED
	 *        AND WE WILL BRANCH TO A WC9 RULE. */
	/*        THERE ARE 5 PARAMETERS:
	 *        1) A RELATIONAL POINTER TO INDICATE THE ELEMENT WHOSE
	 *           SWORKS ARE TO BE SEARCHED
	 *        2) THE WC SOUGHT
	 *        3) THE TYPE SOUGHT (-01 = DON'T CARE)
	 *             FOR WC 2, 860 AND 861 WILL REPRESENT TAGSETS:
	 *              861 - 10,11,12
	 *              860 - 2,3,4,5,6,7,8,9,13,14
	 *        4) THE FORM SOUGHT (-01 = DON'T CARE)
	 *        5) ADDRESS OF WC9 RULE TO BRANCH TO IN CASE OF A MATCH */
	/*        N.B. 'RSWORK' IS THE SWORK PASSED FROM RES - MODIFIED
	 *        (SUBSCRIPTS 1,2,3,7,11 AND 15 ARE UNNECESSARY AND ARE DROPPED) */
	/*        READ THE DICT. TARGET POINTERS, OVERFLOW CODES AND RES SWORK
	 *        WRITTEN TO SW29 DATA FILE IN TRAN1 WRITE */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#define MS_F77
#include <fcrt.h>
#include "logoslib.h"
#include "trans.h"
#include "projexts.h"
#include <jbctrl.h>
#include "parsetrans_ext.h"

#define	NUMDWC	6

void /*FUNCTION*/ txsw29(
short int *k7)
{
	static LOGICAL8 defflg;
	static short int inform, insub, inwc, jj, locflg, yy;
	static long int _l0, jsptr, retflg;
	long target_address;
	int target_type;
	static long i4zero = 0;
	static short val9 = 9;
	static char pgmnam[9] = "TxSW29  ";
	static short iz = 0;
	static short i4 = 0;
	static short ms = 0;
	static short nn = 0;
	static short ss = 0;
	static short wc = 0;
	static short cb9 = 0;
	static short ret = 0;
	static short k3p2 = 0;
	static short k3p4 = 0;
	static short k3p5 = 0;
	static short numr = 0;
	static short typ1 = 0;
	static short typ2 = 0;
	static short typ3 = 0;
	static short jssav = 0;
	static short n7jim = 0;
	static short oldwc = 0;
	static short retsw = 0;
	static short scptr = 0;
	static short defwc[NUMDWC]={3,6,13,11,14,15};
	static short setwc[NUMDWC]={6,3,11,13,15,14};

	vbdataX_.k3n = vbdataX_.k3 + 6;
	vbdataX_.k3p1 = vtrfX_.vtrf[vbdataX_.k3+1-One];
	k3p2 = vtrfX_.vtrf[vbdataX_.k3+2-One];
	semargX_.k3p3 = vtrfX_.vtrf[vbdataX_.k3+3-One];
	k3p4 = vtrfX_.vtrf[vbdataX_.k3+4-One];
	k3p5 = vtrfX_.vtrf[vbdataX_.k3+5-One];
	/*                                IF NOT ORIGINAL ELEMENT NO ALTERNATE
	 *                                SWORKS SO JUST RETURN */
	flowckX_.n6jim = sconX_.scolnk[sworkX_.swork[im81X_.im81-vbdataX_.k3p1-One][4-One]-
	  One];
	if( flowckX_.n6jim <= elemctX_.origct ){

		defflg = TRUE;
		oldwc = sworkX_.swork[im81X_.im81-vbdataX_.k3p1-One][1-One];
		jsptr = sconX_.scolnk[sworkX_.swork[im81X_.im81-vbdataX_.k3p1-One][4-One]-
		  One];
		jssav = prctX_.js[jsptr-One];
		if( jssav > 10 )
			jssav -= 10;

		//  target information used to be read but is now passed from lookup in arrays

		if( diagsX_.deepdi == 1 )
			{
			fprintf( _spec_fp, "***  IN SW27, ALTERNATE VALUES FOR SWORK#%3d", 
			  im81X_.im81 - vbdataX_.k3p1 );
			fprintf( _spec_fp, "\n                    SWORKS                   TARGET ADRESSES       OFL1R              OFL4R\n     " );
			for( yy=1; yy <= 9; yy++ ){
				fprintf( _spec_fp, "%3d", data29X_.rswork[flowckX_.n6jim-One][yy-One] );
				}
			fprintf( _spec_fp, " " );
			for( yy=1; yy <= 3; yy++ ){
				fprintf( _spec_fp, "%3ld", data29X_.targ[flowckX_.n6jim-One][yy-One] );
				}
			fprintf( _spec_fp, " " );
			for( yy=1; yy <= 3; yy++ ){
				fprintf( _spec_fp, "%3d", data29X_.ofl1r[flowckX_.n6jim-One][yy-One] );
				}
			fprintf( _spec_fp, "  " );
			for( yy=1; yy <= 3; yy++ ){
				fprintf( _spec_fp, "%3d", data29X_.ofl4r[flowckX_.n6jim-One][yy-One] );
				}
			fprintf( _spec_fp, " \n" );
			}

		/*                      ALTERNATE WC SEARCH */

		while( TRUE ){
			for( iz=1; iz <= 7; iz += 3 ){

				i4 = (iz + 2)/3;
				if( i4 != jssav ){

					wc = data29X_.rswork[flowckX_.n6jim-One][iz-One];
					if( wc == k3p2 ){

						if( semargX_.k3p3 != 0 ){
							typ1 = data29X_.ofl1r[flowckX_.n6jim-One][i4-One];
							typ2 = data29X_.rswork[flowckX_.n6jim-One][iz+1-One];
							typ3 = data29X_.ofl4r[flowckX_.n6jim-One][i4-One];
							if( (wc == 2 && srcflgX_.srcflg == 2) && 
							  (semargX_.k3p3 == 860 || semargX_.k3p3 == 861) ){
								if( semargX_.k3p3 == 860 ){
									if( !(typ3 >= 2 && typ3 <= 9) ){
										if( !(typ3 == 13 || typ3 == 14) )
											goto L_4529;
										}
									}
								else if( !(typ3 >= 10 && typ3 <= 12)){
									goto L_4529;
									}
								}
							else if( !((semargX_.k3p3 == typ1 || semargX_.k3p3 == 
							  typ2) || semargX_.k3p3 == typ3) ){
								goto L_4529;
								}
							}

						if( k3p4 == 0 )
							goto L_4190;
						if( data29X_.rswork[flowckX_.n6jim-One][iz+2-One] == 
						  k3p4 )
							goto L_4190;

						/*   TEST SUPERFORM */
						ms = 4;
						frmarrX_.frmarr[1-One] = data29X_.rswork[flowckX_.n6jim-One][iz-One];
						frmarrX_.frmarr[2-One] = data29X_.rswork[flowckX_.n6jim-One][iz+2-One];
						formod(2,k3p4,ms,im81X_.im81-vbdataX_.k3p1, &retsw);
						if( retsw == 1 )
							goto L_4190;
						}
					}

L_4529:
				;
				}

			/*+ Alternate WC Search                         RKH  04/01/87   R1689 */

			if( !defflg )
				break;
			for( jj=1; jj <= NUMDWC; jj++ ){
				if( k3p2 == defwc[jj-One] )
					goto L_4530;
				}
			break;
L_4530:
			defflg = FALSE;
			if( diagsX_.deepdi == 1 ){
				fprintf( _spec_fp, " SW29 NO MATCH - N6JIM: %4d\n Trying Alternate WC\n", 
				  flowckX_.n6jim );
				}
			k3p2 = setwc[jj-One];
			}


		return;

		/*        A MATCH - LOAD UP THE CODES FOR THE NEW PART OF SPEECH
		 *        AND TRY TO BRANCH TO WC 9 RULE */

L_4190:
		if( diagsX_.deepdi == 1 ){
			fprintf( _spec_fp, " SW29 MATCHED - N6JIM: %4d\n", flowckX_.n6jim );
			}

		n7jim = im81X_.im81 - vbdataX_.k3p1;
		scptr = sworkX_.phrhed[n7jim-One];
		flowckX_.phrstr = sworkX_.phrbeg[n7jim-One];
		flowckX_.phrlst = sworkX_.phrend[n7jim-One];

		for( ms=flowckX_.phrstr; ms <= flowckX_.phrlst; ms++ ){
			if( opadriX_.sconpi[ms-One] == scptr )
				goto L_4210;
			}
		ms -= 1;

L_4210:
		if( data29X_.targ[flowckX_.n6jim-One][i4-One] > 0 ){

			prctX_.js[jsptr-One] = i4 + 10;
			opadriX_.opadri[ms-One] = flowckX_.n6jim;
			}
		else if( data29X_.targ[flowckX_.n6jim-One][i4-One] != 0 ){
			prctX_.js[jsptr-One] = i4;
			lmove(&opadriX_.opadri[ms-One],1,
				  (short*)&data29X_.targ[flowckX_.n6jim-One][i4-One],3,2);
			}

		sconX_.scon[scptr-One][1-One] = data29X_.rswork[flowckX_.n6jim-One][iz-One];
		sconX_.scon[scptr-One][2-One] = data29X_.ofl1r[flowckX_.n6jim-One][i4-One];

		/*OMDEL */
		sconX_.scon[scptr-One][11-One] = data29X_.rswork[flowckX_.n6jim-One][iz+1-One];
		sconX_.scon[scptr-One][13-One] = data29X_.ofl4r[flowckX_.n6jim-One][i4-One];
		sworkX_.swork[n7jim-One][1-One] = data29X_.rswork[flowckX_.n6jim-One][iz-One];
		sworkX_.swork[n7jim-One][2-One] = data29X_.ofl1r[flowckX_.n6jim-One][i4-One];
		/*+                 SET INPUT SWORK1 IF IN PASS1         *11/06/91*JAL* */
		if( (passesX_.passfl == 1 && passesX_.passct == 1) && sworkiX_.sc2swi[sworkX_.swork[n7jim-One][4-One]-
		  One] > 0 ){
			sworkiX_.sworki[sworkiX_.sc2swi[sworkX_.swork[n7jim-One][4-One]-
			  One]-One][1-One] = data29X_.rswork[flowckX_.n6jim-One][iz-One];
			sworkiX_.sworki[sworkiX_.sc2swi[sworkX_.swork[n7jim-One][4-One]-
			  One]-One][2-One] = data29X_.ofl1r[flowckX_.n6jim-One][i4-One];
			sconinX_.sconin[flowckX_.n6jim-One][1-One] = data29X_.ofl1r[flowckX_.n6jim-One][i4-One];
			sconinX_.sconin[flowckX_.n6jim-One][2-One] = data29X_.rswork[flowckX_.n6jim-One][iz+1-One];
			sconinX_.sconin[flowckX_.n6jim-One][3-One] = data29X_.ofl4r[flowckX_.n6jim-One][i4-One];
			}
		/*-                                                     *11/06/91*JAL* */

		/*   FOR ONE WC2 TO ANOTHER WC2, DON'T CHANGE FORM */
		if( !(k3p2 == 2 && oldwc == 2) ){
			sworkX_.swork[n7jim-One][3-One] = data29X_.rswork[flowckX_.n6jim-One][iz+2-One];
			/*+                                                     *11/06/91*JAL* */
			if( (passesX_.passfl == 1 && passesX_.passct == 1) && 
			  sworkiX_.sc2swi[sworkX_.swork[n7jim-One][4-One]-One] > 
			  0 )
				sworkiX_.sworki[sworkiX_.sc2swi[sworkX_.swork[n7jim-One][4-One]-
				  One]-One][3-One] = data29X_.rswork[flowckX_.n6jim-One][iz+2-One];
			/*-                                                     *11/06/91*JAL* */

			/*   IF CHANGING WC, RESET FORMSV */
			if( k3p2 != oldwc ){
				formsaX_.formsv[sconX_.scolnk[sworkX_.swork[n7jim-One][4-One]-
				  One]-One] = data29X_.rswork[flowckX_.n6jim-One][iz+2-One];
				if( (passesX_.passfl == 1 && passesX_.passct == 1) && 
				  sworkiX_.sc2swi[sworkX_.swork[n7jim-One][4-One]-
				  One] > 0 )
					formsaX_.prsfrm[sconX_.scolnk[sworkX_.swork[n7jim-One][4-One]-
					  One]-One] = data29X_.rswork[flowckX_.n6jim-One][iz+2-One];
				}
			}

	target_address = data29X_.targ[flowckX_.n6jim-One][i4-One];
	if(target_address != 0 ){
		if( target_address < 0 ){
			target_address = target_address;
			target_type=2;
			}
		else{
			target_type=1;
			}
		errvrsX_.err = TARG_CODES(&trgflgX_.trgflg,
						&target_type,
						&target_address,
						cmpsmcX_.cmpcod[flowckX_.n6jim-One][i4-One],
						(short *)&trgcdsX_.tcwdad,diagsX_.longdi, _spec_fp);
		if( errvrsX_.err != 0 ){
			errlog(pgmnam,4580,0,6);
			return;
		}

		sconX_.scon[scptr-One][3-One] = trgcdsX_.tcov2b;
		if( k3p2 == 1 )
			sconX_.scon[scptr-One][4-One] = trgcdsX_.tcov2b;
		sconX_.scon[scptr-One][12-One] = trgcdsX_.tcov3b;
		sconX_.scon[scptr-One][15-One] = trgcdsX_.tcov2a;
		sconX_.scon[scptr-One][16-One] = trgcdsX_.tcov3a;
		targ25X_.targ25[flowckX_.n6jim-One] = trgcdsX_.tcgenm;

		/*     Read the O3B table and reset Scons 12 & 45 & 1 more scon */

		locflg = 2;
		inwc = sworkX_.swork[n7jim-One][1-One];
		insub = sworkX_.swork[n7jim-One][2-One];
		inform = sworkX_.swork[n7jim-One][3-One];
		o3btab(inwc,insub,inform,scptr,&retflg,locflg);
		/*             reset the black hole count flag (SCON100)  jal 7/12/93 */
		bhset(flowckX_.n6jim,&retflg);


	}
	if( diagsX_.deepdi == 1 ){
			fprintf( _spec_fp, " SW29 CODES LOADED: %5d", n7jim );
			for( ss=1; ss <= 3; ss++ ){
				fprintf( _spec_fp, "%5d", sconX_.scon[scptr-One][ss-One] );
				}
			for( ss=11; ss <= 13; ss++ ){
				fprintf( _spec_fp, "%5d", sconX_.scon[scptr-One][ss-One] );
				}
			for( nn=1; nn <= 3; nn++ ){
				fprintf( _spec_fp, "%5d", sworkX_.swork[n7jim-One][nn-One] );
				}
			fprintf( _spec_fp, "%5d\n", targ25X_.targ25[flowckX_.n6jim-One] );
			}

	semargX_.pntr9 = k3p5;
	if( semargX_.pntr9 != 0 ){
//cat-102 status variable introduced
			int status = 0;

			/*        BRANCH TO WC 9 (K3P5) */

			*k7 = 0;
			if( loopckX_.call36[1-One] != 0 ){
				loopckX_.call36[1-One] = loopckX_.nptpsv;
				if( minickX_.minifg == 1 )
					loopckX_.call36[2-One] = 1;
				}

			idxval((short*)ADR(_l0,1),&val9,&semargX_.pntr9,k7,&numr);
			if( numr != 0 )
			{
				status =
					rulein_ptr((short*)ADR(_l0,1),&commdX_.matpos,
						spX_ptr, k7, &retsw);
			}

			if ( status )
				errvrsX_.err = status;

			/*        MINI RULES - IS THERE AN EXPERIMENTAL RULE? */
    
			cb9 = semargX_.pntr9;

			txmini(3,&minickX_.k7m,&loopckX_.nptpx,vwarg2X_.i, &cb9);
			if( errvrsX_.err == 0 ){

				/*       K7M WILL BE ZERO IF NO RULE FOUND */

				if( diagsX_.deepdi == 1 ){
					fprintf( _spec_fp, " SW29 END - K7, K7M: %5d%5d\n", 
					  *k7, minickX_.k7m );
					}

				if( *k7 != 0 || minickX_.k7m != 0 )
					getvtrX_.getvtr = 1;

				}
			}
		}
	else if( diagsX_.longdi == 1 ){
		fprintf( _spec_fp, "\n  *****  WARNING   IN %8.8s  ******\n     SW29 INVALID FOR ELEMENT# %3d\n     ONLY %3dORIGINAL ELEMENTS IN THE SENTENCE\n     IGNORING THIS SWITCH AND CONTINUING.        \n\n", 
		  pgmnam, flowckX_.n6jim, elemctX_.origct );
		}
	return;
} /*end of function*/

