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
	 *      07/12/93 jal: set the black hole count flag for new pos.
	 *      05/26/93 jal: for all but english source set formsv on a match
	 *           regardless of whether or not the new wc= old wc.
	 *      05/11/93 jal: if pass1 of 2, save the new SCON2,11,13 values
	 *            in SCONIN so that pass2 will see the new values.
	 *      03/06/91 *JAL*: SET SCON1 FROM NEW WC SELECTED. SET SCON2 TO
	 *           OFL1R OF NEW WC SELECTED, SET SCON15 TO TCOV2A.
	 *      04/23/87 *R1679RKH*  OFL3B CONVERSION & R1685 SWORK1 LIMI
	 *      04/01/87 *R1689RKH*  ALTERNATE WC SEARCH
	 *      08/17/86 *R1561DSD: 100 SCONS
	 *      12/02/85 */
	/*   ***** BEGINNING OF -29 SWITCH ***** */
	/*        -29 SWITCH - FOR TRAN1 */
	/*        INVOKES A SEARCH OF THE UNMATCHED SWORKS OF AN ELEMENT.
	 *        IF MATCHED THE SWORK1 PASSED FROM RES WILL BE REPLACED
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
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <logos_include_res_pt/fcrt.h>
#include "project.h"
#include "logoslib.h"
#include "trans.h"
#include "projexts.h"
#include "parsetrans.h"
#include <string.h>
#include <logos_include_res_pt/jbctrl.h>

#define NUMDWC 6


EXTERN struct t_getvtrX_ {
	short int getvtr;
	}	getvtrX_;
EXTERN struct t_loopckX_ {
	short int imatch[ELMMAX], noloop[21], nptpsv, nptpx, call36[2];
	}	loopckX_;
EXTERN struct t_commdX_ {
	short int disam, ovrsw, vtrsw, matpos;
	}	commdX_;

void /*FUNCTION*/ t1sw29()
{
	static LOGICAL8 defflg;
	static short int inform, insub, inwc, jj, locflg, x;
	static long int _l0, retflg;
		/*pointer COMMON transls*/
	struct  {
		short int k7, oflad, n3;
		}	*_diacbX_ = (void*)&diacbX_;
	struct  {
		short int i, wc42m, wc50m, iz4, swx, s11prt;
		}	*_vwarg2X_ = (void*)&vwarg2X_;
	struct  {
		short int vtr[26];
		}	*_vtrX_ = (void*)&vtrX_;
	struct  {
		short int im1, i3save, i3str, n6, n6jim, n6jims, phrstr;
		}	*_flowckX_ = (void*)&flowckX_;
	struct  {
		short int k7m, minifg;
		}	*_minickX_ = (void*)&minickX_;
	struct  {
		short int nwrks, index, k3p3, pntr9;
		}	*_semargX_ = (void*)&semargX_;
	int taddress,dictype;
	static short val9 = 9;
	static long i4zero = 0;
	static char pgmnam[9] = "T1SW29  ";
	static short iz = 0;
	static short ms = 0;
	static short wc = 0;
	static short xx = 0;
	static short cb9 = 0;
	static short ret = 0;
	static short k3p2 = 0;
	static short k3p4 = 0;
	static short k3p5 = 0;
	static short numr = 0;
	static short typ1 = 0;
	static short typ2 = 0;
	static short typ3 = 0;
	static short oldjs = 0;
	static short oldwc = 0;
	static short retsw = 0;
	static short defwc[NUMDWC]={3,6,13,11,14,15};
	static short setwc[NUMDWC]={6,3,11,13,15,14};

	vbdataX_.k3n = vbdataX_.k3 + 6;
	vbdataX_.k3p1 = vtrfX_.vtrf[vbdataX_.k3+1-One];
	k3p2 = vtrfX_.vtrf[vbdataX_.k3+2-One];
	_semargX_->k3p3 = vtrfX_.vtrf[vbdataX_.k3+3-One];
	k3p4 = vtrfX_.vtrf[vbdataX_.k3+4-One];
	k3p5 = vtrfX_.vtrf[vbdataX_.k3+5-One];
	/*                                IF NOT ORIGINAL ELEMENT NO ALTERNATE
	 *                                SWORKS SO JUST RETURN
	 *??? INDIRECTION */
	_flowckX_->n6jim = im81X_.im81 - vbdataX_.k3p1;
	if( sconX_.scolnk[_flowckX_->n6jim-One] <= elemctX_.origct ){
		/*                   ALTERNATE WC SEARCH */
		defflg = TRUE;
		oldwc = swork1X_.swork1[_flowckX_->n6jim-One][prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
		  One]*4-One];
		while( TRUE ){

			for( iz=4; iz <= 12; iz += 4 ){

				xx = iz/4;
				if( xx != prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-One] ){

					wc = swork1X_.swork1[_flowckX_->n6jim-One][iz-One];
					if( wc == k3p2 ){

						if( _semargX_->k3p3 != 0 ){
							typ1 = ofltagX_.ofl1r[sconX_.scolnk[_flowckX_->n6jim-One]-
							  One][xx-One];
							typ2 = swork1X_.swork1[_flowckX_->n6jim-One][iz+1-One];
							typ3 = ofltagX_.ofl4r[sconX_.scolnk[_flowckX_->n6jim-One]-
							  One][xx-One];

							/*   860 AND 861 ARE SPECIAL VALUES FOR */
							if( (wc == 2 && srcflgX_.srcflg == 2) && 
							  (_semargX_->k3p3 == 860 || _semargX_->k3p3 == 861) ){

								if( _semargX_->k3p3 == 860 ){

									if( !(typ3 >= 2 && typ3 <= 9) ){
										if( !(typ3 == 13 || typ3 == 14) )
											goto L_9001;
										}
									}
								else if( !(typ3 >= 10 && typ3 <= 12)
								   ){
									goto L_9001;
									}

								}
							else if( !((_semargX_->k3p3 == typ1 || 
							  _semargX_->k3p3 == typ2) || _semargX_->k3p3 ==  typ3) ){
								goto L_9001;
								}
							}

						if( k3p4 == 0 )
							goto L_4540;
						if( swork1X_.swork1[_flowckX_->n6jim-One][iz+2-One] == k3p4 )
							goto L_4540;

						/*   CHECK SUPERFORM TABLE */
						ms = iz + 2;
						formod(2,k3p4,(long)ms,_flowckX_->n6jim,&retsw);
						if( retsw == 1 )
							goto L_4540;
						}
					}

L_9001:
				;
				}

			/*+ Alternate WC Search                         RKH  04/01/87   R1689 */
			if( !defflg )
				break;
			for( jj=1; jj <= NUMDWC; jj++ ){
				if( k3p2 == defwc[jj-One] )
					goto L_9002;
				}
			break;
L_9002:
			defflg = FALSE;
			if( diagsX_.longdi == 1 )
				{
				fprintf( _spec_fp, " SW29 NO MATCH - N6JIM: %4d\n Trying Alternate WC\n", 
				  _flowckX_->n6jim );
				}
			k3p2 = setwc[jj-One];
			}

		return;


		/*        A MATCH - LOAD UP THE CODES FOR THE NEW PART OF SPEECH
		 *        AND TRY TO BRANCH TO WC 9 RULE */

L_4540:
		taddress = targX_.targ[sconX_.scolnk[_flowckX_->n6jim-One]-One][xx-One];

		if( diagsX_.longdi == 1 )
			{
			fprintf( _spec_fp, " SW29 MATCHED - N6JIM: %4d  target address=%d\n",
				_flowckX_->n6jim, taddress);
			}

		// if minus then we have numeric input string like 50
		if( taddress > 0 ){
			dictype = 1;
			errvrsX_.err = TARG_CODES(&trgflgX_.trgflg,&dictype,
			           &taddress,
				       cmpsmcX_.cmpcod[_flowckX_->n6jim-One][xx-One],
					   (short *)&trgcdsX_,diagsX_.longdi, _spec_fp);
			if( errvrsX_.err != 0 ){
				errlog(pgmnam,4550,0,1);
				return;
				}

			swork1X_.swork1[_flowckX_->n6jim-One][iz+3-One] 
				= sconX_.scolnk[_flowckX_->n6jim-One];

			if( passesX_.passfl == 1 && passesX_.passct == 1 )
				swork1X_.swrk1i[_flowckX_->n6jim-One][iz+3-One] 
				= sconX_.scolnk[_flowckX_->n6jim-One];
		}

		oldjs = prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-One];
		prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-One] = xx;
		swork1X_.swork1[_flowckX_->n6jim-One][(oldjs*4)+1-One] = 
			typsvX_.typsav[sconX_.scolnk[_flowckX_->n6jim-One]-One];

		if( passesX_.passfl == 1 && passesX_.passct == 1 )
			swork1X_.swrk1i[_flowckX_->n6jim-One][(oldjs*4)+1-One] = typsvX_.typsav[sconX_.scolnk[_flowckX_->n6jim-One]-
			  One];
		typsvX_.typsav[sconX_.scolnk[_flowckX_->n6jim-One]-One] = swork1X_.swork1[_flowckX_->n6jim-One][(prctX_.js[sconX_.scolnk[_flowckX_->n6jim-One]-
		  One]*4)+1-One];
		sconX_.scon[_flowckX_->n6jim-One][1-One] = swork1X_.swork1[_flowckX_->n6jim-One][iz-One];
		sconX_.scon[_flowckX_->n6jim-One][2-One] = ofltagX_.ofl1r[sconX_.scolnk[_flowckX_->n6jim-One]-One][xx-One];
		sconX_.scon[_flowckX_->n6jim-One][11-One] = swork1X_.swork1[_flowckX_->n6jim-One][iz+1-One];
		sconX_.scon[_flowckX_->n6jim-One][13-One] = ofltagX_.ofl4r[sconX_.scolnk[_flowckX_->n6jim-One]-One][xx-One];
		swork1X_.swork1[_flowckX_->n6jim-One][iz+1-One] = ofltagX_.ofl1r[sconX_.scolnk[_flowckX_->n6jim-One]-One][xx-One];

		if( passesX_.passfl == 1 && passesX_.passct == 1 ){
			swork1X_.swrk1i[_flowckX_->n6jim-One][iz+1-One] = ofltagX_.ofl1r[sconX_.scolnk[_flowckX_->n6jim-One]-One][xx-One];
			sconinX_.sconin[sconX_.scolnk[_flowckX_->n6jim-One]-One][1-One] = sconX_.scon[_flowckX_->n6jim-One][2-One];
			sconinX_.sconin[sconX_.scolnk[_flowckX_->n6jim-One]-One][2-One] = sconX_.scon[_flowckX_->n6jim-One][11-One];
			sconinX_.sconin[sconX_.scolnk[_flowckX_->n6jim-One]-One][3-One] = sconX_.scon[_flowckX_->n6jim-One][13-One];
			}

		if( taddress > 0 ){
			ovc2a3X_.ofl2a[sconX_.scolnk[_flowckX_->n6jim-One]-One] = trgcdsX_.tcov2a;
			sconX_.scon[_flowckX_->n6jim-One][15-One] = trgcdsX_.tcov2a;
			ofl2X_.ofl2i[sconX_.scolnk[_flowckX_->n6jim-One]-One] = trgcdsX_.tcov2b;
			sconX_.scon[_flowckX_->n6jim-One][3-One] = trgcdsX_.tcov2b;
			ovc2a3X_.ofl3a[sconX_.scolnk[_flowckX_->n6jim-One]-One] = trgcdsX_.tcov3a;
			sconX_.scon[_flowckX_->n6jim-One][16-One] = trgcdsX_.tcov3a;
			sconX_.scon[_flowckX_->n6jim-One][12-One] = trgcdsX_.tcov3b;
			targ25X_.targ25[sconX_.scolnk[_flowckX_->n6jim-One]-One] = trgcdsX_.tcgenm;
		}

		if( ofl2a3X_.ofl3r[sconX_.scolnk[_flowckX_->n6jim-One]-One][xx-One] != 0 )
			sconX_.scon[_flowckX_->n6jim-One][12-One] = ofl2a3X_.ofl3r[sconX_.scolnk[_flowckX_->n6jim-One]-One][xx-One];

		/*	if TRAN1 of 2pass , use old form if changing from wc2 to wc2 */
		if( (passesX_.passfl == 1 && passesX_.passct == 2) && (oldwc == 2 && wc == 2) )
			swork1X_.swork1[_flowckX_->n6jim-One][iz+2-One] = swork1X_.swork1[_flowckX_->n6jim-One][(oldjs*4)+2-One];

		/*     Read the O3B table and reset Scons 12 & 45 & 1 more scon */

		locflg = 2;
		inwc = swork1X_.swork1[_flowckX_->n6jim-One][iz-One];
		insub = swork1X_.swork1[_flowckX_->n6jim-One][iz+1-One];
		inform = swork1X_.swork1[_flowckX_->n6jim-One][iz+2-One];
		o3btab(inwc,insub,inform,_flowckX_->n6jim,&retflg,locflg);


		/*   IF CHANGING TO A DIFFERENT WC, RESET FORMSV */

		if( srcflgX_.srcflg != 2 ){
			formsaX_.formsv[sconX_.scolnk[_flowckX_->n6jim-One]-One] = swork1X_.swork1[_flowckX_->n6jim-One][iz+2-One];
			if( passesX_.passfl == 1 && passesX_.passct == 1 )
				formsaX_.prsfrm[sconX_.scolnk[_flowckX_->n6jim-One]-One] = swork1X_.swork1[_flowckX_->n6jim-One][iz+2-One];
			}
		else if( k3p2 != oldwc ){
			formsaX_.formsv[sconX_.scolnk[_flowckX_->n6jim-One]-One] = swork1X_.swork1[_flowckX_->n6jim-One][iz+2-One];
			if( passesX_.passfl == 1 && passesX_.passct == 1 )
				formsaX_.prsfrm[sconX_.scolnk[_flowckX_->n6jim-One]-One] = swork1X_.swork1[_flowckX_->n6jim-One][iz+2-One];
			}
		/*             reset the black hole count flag (SCON100)  jal 7/12/93 */
		bhset(_flowckX_->n6jim,&retflg);

		if( diagsX_.longdi == 1 )
			{
			fprintf( _spec_fp, " SW29 CODES LOADED %4d\n", targ25X_.targ25[sconX_.scolnk[_flowckX_->n6jim-One]-One] );
			}

		_semargX_->pntr9 = k3p5;

		if( _semargX_->pntr9 != 0 ){
			/*        BRANCH TO WC 9 (K3P5) */

			_diacbX_->k7 = 0;
			if( loopckX_.call36[1-One] == 0 ){
				loopckX_.call36[1-One] = loopckX_.nptpsv;
				if( _minickX_->minifg == 1 )
					loopckX_.call36[2-One] = 1;
				}

			idxval((short*)ADR(_l0,1),&val9,&_semargX_->pntr9,&_diacbX_->k7,&numr);
			if( numr != 0 )
			{
				rulein_ptr((short*)ADR(_l0,1), &commdX_.matpos,
					spX_ptr, &_diacbX_->k7, &retsw);
			}

			/*        MINI RULES - IS THERE AN EXPERIMENTAL RULE? */

			cb9 = _semargX_->pntr9;
			txmini(3,&_minickX_->k7m,&loopckX_.nptpx,_vwarg2X_->i,&cb9);
			if( errvrsX_.errlvl == 0 ){

				/*        K7M WILL BE ZERO IF NO RULE FOUND */

				if( diagsX_.longdi == 1 )
					{
					fprintf( _spec_fp, " SW29 END - K7, K7M: %5d%5d\n", 
					  _diacbX_->k7, _minickX_->k7m );
					}

				if( _diacbX_->k7 != 0 || _minickX_->k7m != 0 )
					getvtrX_.getvtr = 1;
				}
			}
		}
	else if( diagsX_.longdi == 1 ){
		fprintf( _spec_fp, "\n  *****  WARNING   IN %8.8s  ******\n     SW29 INVALID FOR ELEMENT# %3d\n     ONLY %3dORIGINAL ELEMENTS IN THE SENTENCE\n     IGNORING THIS SWITCH AND CONTINUING.        \n\n", 
		  pgmnam, _flowckX_->n6jim, elemctX_.origct );
		}
	return;
} /*end of function*/

