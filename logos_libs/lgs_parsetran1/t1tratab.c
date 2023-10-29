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
	 *   11/23/93 AVK If german phrasal transfer, use pat of head element
	 *   10/02/92 JAL print swork1 for sw26 too.
	 *   09/17/91 SET OFL3A FOR PROCESS NOUN FROM GERDEM FLAG #2031BT
	 *   88/09/19 LG002GBA R0GBA: SET SCON 59 = TARGET PAT #
	 *              FOR ALL SOURCES NOT ENGLISH.
	 *   04/23/87 *R1679RKH*  OFL3B CONVERSION & R1685 SWORK1 LIMI
	 *   87/04/08 LG002GBA RB002:  FOR SHOULD, OUGHT, ETC. ALLOW
	 *                                      TYPE OF 16.
	 *   04/01/87 *R1733RKH*  SCON62 TEST TO PUT SOURCE IN UNFOUN
	 *                                          RKH  04/23/87   R1679 */
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


int /*FUNCTION*/ tablet()
{
	static char diagwd[200];
	static short int em1, gb, gblvl, gbws, gg, i, i2, ij, 
	  j, j2, jb, jl, kw, m, ovftst, rcode,  swch, swkad, 
	  trgpt, ws, x, y, z;
	static long int _l0, _l1, fdef, gbnum, m1, m2;
	int taddress,dictype;
	static short wc = 0;
	static short dct[3]={7,11,15};
	static short dctwc[3]={4,8,12};
	static short dctyp[3]={5,9,13};
	static short dctfrm[3]={6,10,14};
	static char pgmnam[9] = "T1TRATAB";

	memcpy(sent_wordsX_.source_word[0],"BOS             ",sizeof(sent_wordsX_.source_word[0]));
	memcpy(sent_wordsX_.source_word[swork1X_.phct-1],"EOS             ",sizeof(sent_wordsX_.source_word[0]));

	em1 = swork1X_.phct - 1;
	ofltabX_.ofl3i[0] = 0;
	ofltabX_.ofl4i[0] = 1;
	ofltabX_.ofl3i[swork1X_.phct-One] = 0;
	ofltabX_.ofl4i[swork1X_.phct-One] = 10;
	ws = 2;

	memset(&targpnX_,'\0',sizeof(targpnX_));
	memset(&targ25X_,'\0',sizeof(targ25X_));
	memset(diagwd,' ',sizeof(diagwd));

	memset(ofl2X_.ofl2i,'\0',sizeof(ofl2X_.ofl2i));
	memset(&ovc2a3X_,'\0',sizeof(ovc2a3X_));


	for( i=2; i <= em1; i++ ){
		jb = prctX_.js[sconX_.scolnk[i-One]-One];
		swkad = swork1X_.swork1[i-One][jb*4+3-One];
		wc = swork1X_.swork1[i-One][jb*4-One];

		taddress = targX_.targ[sconX_.scolnk[i-One]-One][jb-One];
		if( taddress <= 0 ){
			memcpy( diagwd, "-unfound or numeric-", 9 );
			/*winnt			copied following line here so ofl is copyied
			 *winnt		for numerics which look like unfound. */
			ofl2X_.ofl2i[i-One] = ofl2a3X_.ofl2r[i-One][jb-One];
			ofltabX_.ofl3i[i-One] = ofl2a3X_.ofl3r[i-One][jb-One];
			ofltabX_.ofl4i[i-One] = ofltagX_.ofl4r[i-One][jb-One];
			}
		else{

			dictype = 1;
			errvrsX_.err = TARG_CODES(&trgflgX_.trgflg,&dictype,&taddress,
				       cmpsmcX_.cmpcod[sconX_.scolnk[i-One]-One][jb-One],
					   (short *)&trgcdsX_,diagsX_.longdi, _spec_fp);
			if( errvrsX_.err != 0 ){
				errlog(pgmnam,120,taddress,6);
//12-16-99					return -1;
			}

			/*        GENDER INFO FOR NOUNS IS PLACED IN OFL2I */

			ovc2a3X_.ofl2a[i-One] = trgcdsX_.tcov2a;
			ofl2X_.ofl2i[i-One] = trgcdsX_.tcov2b;
			ovc2a3X_.ofl3a[i-One] = trgcdsX_.tcov3a;
			ofltabX_.ofl3i[i-One] = trgcdsX_.tcov3b;
			/*                            OFL3B PROJECT FOR G/F   2/19/87 +
			 *     FOR WC 02 (SEIN) - FLAG OFL2R WITH A 2
			 *     CLUE LIES IN DICT SOURCE RECORD _ A '2' BEGINNING IN POSITIONS
			 *     25 = 1ST POS,   45 = 2CND POS,   65 = 3RD POS */

			/*    IF VERB, AND GERMAN SOURCE, CHECK TO SEE IF A VALUE WAS PASSED
			 *    FROM GERDEM.  IF SO, LOAD IT IN SCON(45) */
			if( wc == 2 && srcflgX_.srcflg == 1){
				sconX_.scono[sconX_.scolnk[i-One]-One][45-SCONX1-One] = ofl2a3X_.auxiliaryCode[i-One][jb-One];
				}

			/*   SET OFL3A FOR PROCESS NOUN FROM GERDEM FLAG           #2031BT */
			if( ofl2a3X_.ofl2r[i-One][jb-One] == 999 ){
				ovc2a3X_.ofl3a[i-One] = 2;
				ofl2a3X_.ofl2r[i-One][jb-One] = 0;
				}

			if( ofl2a3X_.ofl2r[i-One][jb-One] != 0 )
				ofl2X_.ofl2i[i-One] = ofl2a3X_.ofl2r[i-One][jb-One];
			if( ofl2a3X_.ofl3r[i-One][jb-One] != 0 )
				ofltabX_.ofl3i[i-One] = ofl2a3X_.ofl3r[i-One][jb-One];
			ofltabX_.ofl4i[i-One] = ofltagX_.ofl4r[i-One][jb-One];

			/*        CAPTURE TARGET GENDER FROM TARGET CODES FILE */
			targ25X_.targ25[sconX_.scolnk[i-One]-One] = trgcdsX_.tcgenm;

			/*        GERMAN SOURCE -  ENGLISH, FRENCH & ITALIAN TARGETS AT THIS TIME
			 *        CAPTURE TARGET PAT # FROM TARGET CODES FILE
			 *        FILL SCON(58) WITH WC INFO ASSOCIATED WITH THE PAT INFO IN 59 */

			targpnX_.targpn[sconX_.scolnk[i-One]-One] = trgcdsX_.tcpatm[0];

			sconX_.scono[sconX_.scolnk[i-One]-One][59-SCONX1-One] = targpnX_.targpn[sconX_.scolnk[i-One]-One];
			sconX_.scono[sconX_.scolnk[i-One]-One][57-SCONX1-One] = ptdiagX_.patno[sconX_.scolnk[i-One]-One][jb-One];

			
			if( srcflgX_.srcflg == 1 ){
				sconX_.scono[sconX_.scolnk[i-One]-One][58-SCONX1-One] = trgcdsX_.word_class;
				}


			}

		}

	for( ij=1; ij <= swork1X_.phct; ij++ ){
		kw = typsvX_.typsav[ij-One];
		if( !(kw > 16 || kw == 0) ){
			/*+                                                        *RB002*GBA
			 *     FOR SHOULD, OUGHT, ETC. A TYPE 16 IS VALID ‹‹‹
			 *     ALTHOUGH THE 16 IS TECHNICALLY NOT VALID, THE RULES ARE TOO
			 *       NUMEROUS TO CHANGE, SO WE ARE PERMITTING IT HERE */
			if( !((srcflgX_.srcflg == 2 && kw == 16) && ofltabX_.ofl4i[ij-One] == 7) ){
				ofltabX_.ofl4i[ij-One] = kw;
				typsvX_.typsav[ij-One] = 0;
				}
			}
		}


	/*       HENUM VALUES DETERMINED IN RES, JUST NEGATE UNNECESSARY ONES */
	j = 1;
	for( i=1; i <= swork1X_.phct; i++ ){

		/*              ONLY WC 1 3 4 5 6 17 NEED HENUM2 VALUES */

		x = swork1X_.swork1[i-One][dctwc[prctX_.js[sconX_.scolnk[i-One]-One]-One]-One];
		y = swork1X_.swork1[i-One][dctfrm[prctX_.js[sconX_.scolnk[i-One]-One]-One]-One];
		if( x != 17 ){
			if( srcflgX_.srcflg == 1 && x == 19 ){

				if( typsvX_.typsav[i-One] == 76 )
					goto L_840;
				z = swork1X_.swork1[i-One][dctyp[prctX_.js[sconX_.scolnk[i-One]-One]-One]-One];
				if( typsvX_.typsav[i-One] == 49 ){
					if( z == 205 || z == 234 )
						goto L_840;
					}
				else if( typsvX_.typsav[i-One] == 45 ){
					if( z == 218 || z == 221 )
						goto L_840;
					}
				}
			else if( x <= 6 ){
				if( ((x == 1 || x == 3) || x == 5) || x == 6 )
					goto L_840;
				if( x != 2 ){

					if( ofltabX_.ofl4i[i-One] == 13 || ofltabX_.ofl4i[i-One] == 14 )
						goto L_840;
					if( y == 7 )
						goto L_840;
					}
				}
			hensavX_.henum2[sconX_.scolnk[i-One]-One][0] = -1;
			}

L_840:
	;
		}

	return 0;


} /*end of function*/

