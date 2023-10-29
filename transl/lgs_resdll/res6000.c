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

/*    THIS SUBROUTINE IS FOR A SPECIAL SUBSET OF TAGSET MATCHING
 *    WHICH IS USED IN THE RES OVERLAY STRATEGY.  THESE TAGSETS
 *    ARE KEYED BY A VALUE FROM 6000-7000 IN THE 1ST POSITION. */


/*             RETSW = 0  IF THERE IS NO MATCH
 *                   = 1  IF THERE IS A MATCH */

/*             TAGPOS POINTS TO THE 6000 VALUE IN THE TAG
 *                    AND IS PASSED BACK AS THE POSITION OF
 *                    THE NEXT VALUE AFTER THIS 6000 TAG */



#include "rescommon.h"

void   rs6000(retsw, tagpos)
short int *retsw, *tagpos;
{
	static short int accept;
	static char pgmnam[9] = "RES6000 ";
	static short fr55a[13]={1,3,5,7,9,10,12,13,14,15,16,17,80};
	static short fr55b[13]={67,0,0,59,59,59,69,59,63,64,65,66,69};
	static short dctwc[3]={4,8,12};
	static short dctyp[3]={5,9,13};
	static short dctfm[3]={6,10,14};
	static short m = 0;
	static short n = 0;
	static short x = 0;
	static short ms = 0;
	static short m2 = 0;
	static short wc = 0;
	static short xx = 0;
	static short yy = 0;
	static short rec = 0;
	static short wcx = 0;
	static short prm1 = 0;
	static short prm2 = 0;
	static short snum = 0;
	static short tmpp = 0;
	static short val1 = 0;
	static short val2 = 0;
	static short form1 = 0;
	static short funct = 0;
	static short match = 0;
	static short n6jim = 0;
	static short n6nod = 0;
	static short n7jim = 0;
	static short n7nod = 0;
	static short opnod = 0;
	static short typex = 0;
	static short celnum = 0;
	static short locflg = 0;
	static short pthnum = 0;
	static short retflg = 0;
	static short supfrm = 0;
	static short swcpd4 = 0;
	static short tstcnt = 0;
	static short tstnum = 0;

	/*     LAST CHG 01/14/87 13:45 R1245GBA: CHANGE TEST FOR PARENS AND
	 *                                       ADD TEST FOR DOUBLE QUOTES */
	/*+                                                        *R0GBA*GBA */
	/*-                                                        *R0GBA*GBA */
	/*      COMMON /NEGNOD/NO3RES, NEGNOD  --commented out,cwl,2/5/98 */
	swcpd4 = mtcinfX_.swcp/4;
	funct = tagflwX_.tagflw[*tagpos-One] - 6000;
	if( funct == 10 || funct == 12 ){


		/*   FUNCTIONS 10 AND 12  TYPE FIELD TESTS.
		 *      PRM1 = POINTER
		 *      PRM2 TO 9000 (EQUALS END) = TYPE FIELDS
		 *      FOR FUNCTION 10  IF TYPE FIELD MATCHES TAGSET IS A MATCH
		 *      FOR FUNCTION 12  IF TYPE FIELD MATCHES TAGSET IS NOT A MATCH
		 *+                                                         *R1109MBS
		 *          1ST DIGIT OF PRM1 (WHICH IS THE RELATIVE POINTER) IS
		 *          A FLAG :  = 0 MEANS TEST ALL NODES IN THE SWORK
		 *                    = 1 MEANS TEST THE JGUS NODE */
		prm1 = tagflwX_.tagflw[*tagpos+1-One];
		valths(&tstnum,&prm1,prm1);
		n6jim = (ikX_.ik - 81) + prm1;
		/*-                                                         *R1109MBS */
		xx = jbgusX_.jgus[n6jim-One];
		if( xx == 0 ){
			/*+                                                        *R0GBA*GBA
			 *      IF (N6JIM .NE. IKS) GO TO 760 */
			if( tstnum == 1 && n6jim != mtcinfX_.iks )
				goto L_760;
			/*-                                                        *R0GBA*GBA */
			xx = swcpd4;
			}
		ms = *tagpos + 2;

		match = 0;
		for( m=ms; m <= 22; m++ ){
			typex = tagflwX_.tagflw[m-One];
			if( typex == 9000 )
				break;
			/*+                                                        *RFFFF*GBA */
			if( match != 1 ){
				/*-                                                        *RFFFF*GBA
				 *+                                                         *R1109MBS */
				for( n=1; n <= 3; n++ ){
					/*+                                                        *RFFFF*GBA */
					if( match != 1 ){
						/*-                                                        *RFFFF*GBA */

						yy = n;
						if( tstnum == 1 )
							yy = xx;

						if( typex == ofltagX_.ofl4r[n6jim-One][yy-One] )
							match = 1;
						if( typex == ofltagX_.ofl1r[n6jim-One][yy-One] )
							match = 1;
						if( typex == savtyX_.savtyr[n6jim-One][yy-One] )
							match = 1;
						if( typex == swX_.swork[n6jim-One][dctyp[yy-One]-
						  One] )
							match = 1;
						/*+                                                         *R1118MBS */
						if( typex == savtyX_.subchg[n6jim-One][yy-One] )
							match = 1;
						/*-                                                         *R1118MBS */

						if( tstnum == 1 )
							break;
						}
					}
				}
			/*-                                                         *R1109MBS */
			}

		*tagpos = m + 1;
		if( funct == 10 ){
			if( match != 1 )
				goto L_760;

			}
		else if( match == 1 ){
			goto L_760;
			}
		}
	else if( funct == 13 || funct == 14 ){





		/*   FUNCTIONS 13 AND 14  WC TESTS.
		 *      PRM1 = POINTER
		 *      PRM2 TO 9000 (EQUALS END) = WC VALUES
		 *      FOR FUNCTION 13  IF CURRENT NODE = WC  TAGSET IS A MATCH
		 *      FOR FUNCTION 14  IF ANY NODE = WC  TAGSET IS NOT A MATCH */
		n6jim = (ikX_.ik - 81) + tagflwX_.tagflw[*tagpos+1-One];
		ms = *tagpos + 2;

		match = 0;
		for( m=ms; m <= 22; m++ ){
			wcx = tagflwX_.tagflw[m-One];
			if( wcx == 9000 )
				break;

			for( m2=1; m2 <= 3; m2++ ){
				if( swX_.swork[n6jim-One][m2-One] >= 0 ){
					if( !(funct == 13 && swcpd4 != m2) ){
						if( swX_.swork[n6jim-One][dctwc[m2-One]-
						  One] == wcx )
							match = 1;
						}
					}
				}

			}

		*tagpos = m + 1;
		if( funct == 13 ){
			if( match != 1 )
				goto L_760;

			}
		else if( match == 1 ){
			goto L_760;
			}
		}
	else if( funct == 25 ){



		/*   FUNCTION 25 - IF THE SWORK POINTED TO IS UNAMBIGUOUS,
		 *                 I.E. ONLY 1 NODE OPEN, IT IS A MATCH */
		n6jim = (ikX_.ik - 81) + tagflwX_.tagflw[*tagpos+1-One];
		*tagpos += 2;

		opnod = 0;
		for( ms=1; ms <= 3; ms++ ){
			if( swX_.swork[n6jim-One][ms-One] > 0 )
				opnod += 1;
			}
		if( opnod != 1 )
			goto L_760;
		/*+                                                         *R1093MBS */
		}
	else if( funct >= 211 && funct <= 214 ){


		/*   FUNCTION 211 - TEST FOR AN UNAMBIGUOUS VERB.
		 *                  IF ONE IS FOUND, THIS IS A MATCH
		 *   FUNCTION 212 - TEST FOR A COMMA NOT PRECEDED BY AN UNAM. VERB
		 *                  IF ONE IS FOUND, THIS IS A MATCH
		 *   FUNCTION 213 - TEST FOR AN UNAMBIGUOUS VERB.
		 *                  IF ONE IS NOT FOUND, THIS IS A MATCH
		 *   FUNCTION 214 - TEST FOR AN UNAMBIGUOUS VERB OR A POSSIBLE VERB
		 *                  IF ONE IS FOUND, THIS IS A MATCH */
		n6jim = (ikX_.ik - 81) + tagflwX_.tagflw[*tagpos+1-One] + 
		  1;
		*tagpos += 2;

		if( funct == 211 || funct == 213 )
			snum = 1;
		if( funct == 212 )
			snum = 2;
		/*+                                                         *R1093MBS */
		if( funct == 214 )
			snum = 3;
		/*-                                                         *R1093MBS */
		vserch(n6jim,snum,retsw);

		if( funct == 213 ){
			if( *retsw != 0 )
				goto L_760;
			}
		else if( *retsw == 0 ){
			goto L_760;
			}
		}
	else{
		/*-                                                         *R1093MBS */
		if( funct == 50 ){

			/*   FUNCTION 50 IS USED TO TEST THE CELL ARRAY:
			 *      CONSISTS OF A SET OF CELL TESTS. EACH TEST CONSISTS OF 2
			 *      FOUR-DIGIT PARAMETERS.  THE END OF THE TEST WILL BE
			 *      SIGNALLED BY A 9000. */

			/*      TEST PARAMETER 1 IS A CONTROL PARAMETER.  TEST PARAMETER 2
			 *      IS THE VALUE TO TEST FOR. */

			/*      TEST PARAMETER 1 WILL BE BROKEN DOWN:
			 *       THE 1ST DIGIT WILL INDICATE WHICH PATH THE CELL NUMBER
			 *       REFERS TO
			 *              N = 0 MEANS THE RULE WRITER CONTROLS THE PATH
			 *              N = 1 MEANS THE SYSTEM WILL DECIDE WHICH PATH
			 *       THE 2ND DIGIT WILL INDICATE HOW TO TEST
			 *              N = 0 MEANS TEST AS AN EXPLICIT VALUE
			 *              N = 1 MEANS TEST AGAINST THE ERESCELL TABLE
			 *              N = 2 MEANS ANYTHING EXCEPT AN EXPLICIT VALUE
			 *              N = 3 MEANS ANYTHING NOT INCLUDED IN THE ERESCELL TABLE
			 *       THE LAST TWO DIGITS WILL EQUAL WHICH CELL TO TEST */


			ms = *tagpos + 1;

			while( TRUE ){
				for( m=ms; m <= 22; m += 2 ){

					if( tagflwX_.tagflw[m-One] == 9000 )
						goto L_400;
					if( tagflwX_.tagflw[m-One] == 7777 && accept == 
					  1 )
						goto L_8101;

					prm1 = tagflwX_.tagflw[m-One];
					prm2 = tagflwX_.tagflw[m+1-One] - 8000;


					valths(&pthnum,&tmpp,prm1);
					valhun(&tstnum,&celnum,prm1);

					if( pthnum == 1 && cellX_.cell[11-One] != 0 )
						celnum += 10;


					tstnum += 1;
					if( tstnum == 2 ){

						wc = 99;
						rec = prm2 - 19;
						resformod(2,wc,rec,&cellX_.cell[celnum-One],
						  &match,retsw);
						if( errvrsX_.errlvl != 0 )
							goto L_8000;
						/*     IF(MATCH .EQ. 1) GO TO 380 */
						if( match != 1 )
							goto L_8102;
						accept = 1;
						}
					else if( tstnum == 3 ){

						/*340   IF(CELL(CELNUM) .EQ. PRM2) GO TO 390 */
						if( cellX_.cell[celnum-One] == prm2 )
							goto L_8103;
						accept = 1;
						}
					else if( tstnum == 4 ){

						wc = 99;
						rec = prm2 - 19;
						resformod(2,wc,rec,&cellX_.cell[celnum-One],
						  &match,retsw);
						if( errvrsX_.errlvl != 0 )
							goto L_8000;
						/*     IF(MATCH .EQ. 1) GO TO 390 */
						if( match == 1 )
							goto L_8104;
						accept = 1;
						}
					else{

						/*+                                                        *R0GBA*GBA
						 *300   IF (CELL(CELNUM) .EQ. PRM2) GO TO 380 */
						if( cellX_.cell[celnum-One] != prm2 )
							goto L_8105;
						accept = 1;
						}
					/*-                                                        *R0GBA*GBA */

					}
				goto L_390;
L_8102:
				accept = 0;
				goto L_390;
L_8103:
				accept = 0;
				goto L_390;
L_8104:
				accept = 0;
				goto L_390;
				/*     MARKIT AS NOT ACCEPTED */
L_8105:
				accept = 0;

				/*          NO MATCH.  IS THERE AN .OR. TEST (PARAM = 7777) */
L_390:
				if( tagflwX_.tagflw[m+2-One] != 7777 )
					goto L_760;
				ms = m + 3;
				continue;
L_8101:
				ms = m + 3;
				}

L_400:
			*tagpos = m + 1;
			goto L_740;
			}
		else if( funct == 55 ){


			/*   FUNCTION 55 - TEST CERTAIN ELEMENTS THAT HAVE BEEN MATCHED TO
			 *                 SEE IF THEY MATCH EACH OTHER.
			 *    THE FIRST PARAMETER IS A POINTER TO THE CONTROL ELEMENT
			 *    (THE OTHERS MUST MATCH IT).
			 *    THE SECOND IS A CONTROL PARAMETER:
			 *     THE FIRST DIGIT CONTROLS THE TESTING OF THE ELEMENTS
			 *         0 = FORM VALUES MUST MATCH
			 *         1 = SUPERSET VALUES MUST MATCH
			 *         2 = SET VALUES MUST MATCH
			 *         3 = SUBSET VALUES MUST MATCH
			 *     THE SECOND DIGIT IS ??
			 *     THE LAST TWO DIGITS TELL HOW MANY ELEMENTS TO TEST
			 *    THE NEXT X NUMBER OF PARAMETERS ARE POINTERS TO THE ELEMENTS */

			prm1 = tagflwX_.tagflw[*tagpos+1-One];
			prm2 = tagflwX_.tagflw[*tagpos+2-One];
			n6jim = (ikX_.ik - 81) + prm1;
			n6nod = jbgusX_.jgus[n6jim-One];
			/*+                                                        *R0GBA*GBA */
			if( n6nod <= 0 ){
				n6nod = jgus63X_.jgus63[n6jim-One];
				if( n6nod <= 0 )
					n6nod = 1;
				}
			/*-                                                        *R0GBA*GBA */

			valths(&tstnum,&tmpp,prm2);
			valhun(&x,&tstcnt,prm2);

			tstnum += 1;
			supfrm = 0;
			if( tstnum == 2 ){

				val1 = ofltagX_.ofl4r[n6jim-One][n6nod-One];
				}
			else if( tstnum == 3 ){

				val1 = swX_.swork[n6jim-One][dctyp[n6nod-One]-One];
				}
			else if( tstnum == 4 ){

				val1 = ofltagX_.ofl1r[n6jim-One][n6nod-One];
				}
			else{

				val1 = swX_.swork[n6jim-One][dctfm[n6nod-One]-One];

				for( xx=1; xx <= 13; xx++ ){
					if( form1 == fr55a[xx-One] )
						goto L_560;
					}
				goto L_760;

L_560:
				supfrm = fr55b[xx-One];
				}

			yy = *tagpos + 2;
			*tagpos += 3 + tstcnt;

			for( xx=1; xx <= tstcnt; xx++ ){
				yy += 1;

				n7jim = (ikX_.ik - 81) + tagflwX_.tagflw[yy-One];
				n7nod = jbgusX_.jgus[n7jim-One];
				if( n7nod == 0 ){
					if( n7jim != mtcinfX_.iks )
						goto L_760;
					n7nod = swcpd4;
					}

				if( tstnum == 2 ){

					val2 = ofltagX_.ofl4r[n7jim-One][n7nod-One];
					}
				else if( tstnum == 3 ){

					val2 = swX_.swork[n7jim-One][dctyp[n7nod-One]-
					  One];
					}
				else if( tstnum == 4 ){

					val2 = ofltagX_.ofl1r[n7jim-One][n7nod-One];
					}
				else{

					val2 = swX_.swork[n7jim-One][dctfm[n7nod-One]-
					  One];
					}


				if( supfrm != 0 ){

					wc = 2;
					rec = supfrm - 19;
					resformod(2,wc,rec,&val2,&match,retsw);
					if( errvrsX_.errlvl != 0 )
						goto L_8000;
					if( match != 1 )
						goto L_760;

					}
				else if( val2 != val1 ){
					goto L_760;
					}
				}
			goto L_740;
			}
		else{
			goto L_760;
			}



L_8000:
		x = x;
		if( opswX_.sw[3-One] == 1 || opswX_.sw[8-One] == 1 )
			{
			fprintf( _spec_fp, "--ERROR IN RES6000  ERR,LOCFLG,RETSW = %8ld%8d%8d\n", 
			  errvrsX_.err, locflg, *retsw );
			}
		retflg = -1;
		goto L_7000;
		}


L_740:
	*retsw = 1;
	goto L_7000;

L_760:
	*retsw = 0;



L_7000:
	x = x;
	return;

} /*end of function*/



/*-----------------------------------------------------------------
 *----------------------------------------------------------------- */





/*+                                                         *J0289MBS
 *    SUBROUTINE RS6300 IS USED IN PHASE 2 OF THE OVERLAY RULES.
 *               THE PURPOSE OF THE 6300 SERIES TAGSETS IS TO
 *               SEARCH TO THE RIGHT FOR A VERB, WHILE MATCHING
 *               ON SKIP (WC11) AND QUIT (WC10) RULES TO DETERMINE
 *               THE RANGE OF THE SEARCH */

/*    CALLED BY RES1AND2  AFTER ALL OTHER MATCHING IS COMPLETED
 *                        FOR A GIVEN RULE */


/*             RETSW = 0  IF THERE IS NO MATCH
 *                   = 1  IF THERE IS A MATCH */


void /*FUNCTION*/ rs6300(retsw)
short int *retsw;
{
	static short int displm, elem, entry, gba, i, ii, kk, kksav, l2, 
	  locflg, match, mnswsv, number, nwc2, oflad, paren, pt, r65typ, 
	  retflg, rule3, rule4, rultyp, savik, saviks, savk12, savli, 
	  savswc, swc, t63ind, tagser, tagtyp, resnum, val, wc, wc2, x, 
	  xx, yy, zz, zzf, zzl;
	static short dctwc[3]={4,8,12};
	static short dctyp[3]={5,9,13};
	static short dctfm[3]={6,10,14};

	/** LAST CHANGE 87/04/14 LG002GBA RT005: WANG EFFICIENCY -- PUT OFF
	 **                                      READING OVERFLOW TO RESMATCH
	 **      CHANGE 87/04/08 LG002GBA RB001: ADD CHECK OF TYPE FIELD ON RULE
	 **                                      FOR 6300, 6400 WC 10,11
	 *      CHG 03/09/87 13:45 R1711GBA: TEST FOR VERB ADDS WC TO
	 *                                   SUBCHG TO AVOID NONVERB VERBS
	 *      CHG 02/27/87 *RFFFFGBA: EFFICIENCIES.
	 *      CHG 02/11/87 *R1673GBA: AUTOMATIC BACKSPACE FOR WC11 RULES
	 *      CHG 02/09/87 *R1637GBA: PERMIT MULTIPLE RES6300 AND RES6500 */

	
	/*          SAVE RULE MATCHING VALUES */
	savk12 = commkX_.k12;
	savli = commlX_.li;
	savik = ikX_.ik;
	saviks = mtcinfX_.iks;
	savswc = mtcinfX_.swcp;
	for( l2=1; l2 <= swX_.ecount; l2++ ){
		jgus63X_.jgus63[l2-One] = 0;
		}
	t63ind = 1;


	/* THIS IS THE TOP OF LOOP FOR EACH 6300 - 6699 TAGSET */
	while( TRUE ){
		paren = 0;
		if( tag63X_.tag63[t63ind-One] < 6400 ){
			tagser = 63;
			tagtyp = tag63X_.tag63[t63ind-One] - 6300;
			}
		else if( tag63X_.tag63[t63ind-One] < 6500 ){
			tagser = 64;
			tagtyp = tag63X_.tag63[t63ind-One] - 6400;
			}
		else if( tag63X_.tag63[t63ind-One] < 6600 ){
			tagser = 65;
			tagtyp = tag63X_.tag63[t63ind-One] - 6500;
			}
		else if( tag63X_.tag63[t63ind-One] < 6700 ){
			tagser = 66;
			tagtyp = tag63X_.tag63[t63ind-One] - 6600;
			}
		elem = (ikX_.ik - 81) + tag63X_.prm63a[t63ind-One] + 1;
		rultyp = tag63X_.prm63b[t63ind-One] - 8000;
		r65typ = tag63X_.prm63c[t63ind-One] - 8000;

		if( opswX_.sw[8-One] == 1 )
			{
			fprintf( _spec_fp, "  %5d TAGSET STARTING AT ELEMENT %4d\n", 
			  tag63X_.tag63[t63ind-One], elem );
			}


		xx = elem;

		while( xx <= swX_.ecount ){

			/*          LOOP THROUGH TO THE END OF THE SENTENCE. EACH ELEMENT: */

			/*           1) IS MATCHED AGAINST WC 11 (SKIP) RULES.  IF A MATCH
			 *              IS MADE, SKIP OVER THESE ELEMENTS AND CONTINUE SEARCH
			 *           2) IS MATCHED AGAINST WC 10 (QUIT) RULES.  IF A MATCH
			 *              IS MADE, THE SEARCH FAILS
			 *           3) IS TESTED TO SEE IF IT IS A POSSIBLE VERB OR UNAMBIG.
			 *              VERB.  IF IT IS, AND CERTAIN OTHER TESTS MADE, THE
			 *              RULE IS A MATCH. */

			for( ikX_.ik=xx; ikX_.ik <= swX_.ecount; ikX_.ik++ ){

				/*          SKIP OVER ELEMENTS INSIDE PARENTHESES
				 *+                                                         *B0126MBS
				 *+  SWORK(5,IK)=6 WAS LEFT PAREN, NOW LEFT DOUBLE QUOTE    *R1245GBA
				 *     LEAVE AS DOUBLE QUOTE, AND REINSTATE LEFT PAREN TOO  *R1245GBA
				 *     IF (SWORK(4,IK) .EQ. 20 .AND. SWORK(5,IK) .EQ. 6) THEN */
				if( swX_.swork[ikX_.ik-One][4-One] == 20 && (swX_.swork[ikX_.ik-One][5-One] == 
				  6 || swX_.swork[ikX_.ik-One][5-One] == 26) ){
					/*-                                                         *R1245GBA
					 *-                                                         *B0126MBS */
					paren = 1;

					/*+                                                         *B0126MBS
					 *+  SWORK(5,IK)=7 WAS RGT  PAREN, NOW RGT  DOUBLE QUOTE    *R1245GBA
					 *     LEAVE AS DOUBLE QUOTE, AND REINSTATE RGT  PAREN TOO  *R1245GBA
					 *     IF (SWORK(4,IK) .EQ. 20 .AND. SWORK(5,IK) .EQ. 7) THEN */
					}
				else if( swX_.swork[ikX_.ik-One][4-One] == 20 && 
				  (swX_.swork[ikX_.ik-One][5-One] == 7 || swX_.swork[ikX_.ik-One][5-One] == 
				  27) ){
					/*-                                                         *R1245GBA
					 *-                                                         *B0126MBS */
					paren = 0;

					}
				else if( paren != 1 ){


					/*        ZZ = 1 for the WC 11 search
					 *             2 for the WC 10 search
					 *             3 for the WC 07 search */


					/*        for TAGSET series 6300 and 6400 search WC 11 and WC 10 only
					 *        for TAGSET series 6500 and 6600 search 3 word classes:
					 *   WC11 for SKIP rules.  Check rules w/ type=65 and form=Para3-8000
					 *   WC11 for QUIT rules.  Check rules w/ type=65 and form=Para3-8000
					 *            WC07 rules.  Check rules w/ type=Para4-8000 (no form) */


					if( tagser < 65 ){
						zzf = 1;
						zzl = 2;
						}
					else{
						zzf = 1;
						zzl = 3;
						}

					zz = zzf;

					while( zz <= zzl ){

						srch07X_.srch07 = 0;

						if( zz == 1 )
							pt = 11;
						if( zz == 2 )
							pt = 10;
						if( zz == 3 ){
							pt = 7;
							srch07X_.srch07 = 1;
							}

						if( resswX_.ressw == 1 ){
							nwc2 = NENT1(&pt);
							displm = DISSP1(&pt) + nwc2;
							}
						else{
							nwc2 = NENT2(&pt);
							displm = DISSP2(&pt) + nwc2;
							}

						kksav = 1;
						if( nwc2 > 0 ){

							kk = kksav;
							while( TRUE ){

								commkX_.k12 = displm - kk;
								resnum = 2;
								if( resswX_.ressw == 1 )
									resnum = 1;
resrulX_.rule = RULEIN_OCR(resnum, commkX_.k12);
									/*          THE FORM OF THE RULE IS A KEY.  IT MUST BE A -1
									 *          (DON'T CARE) OR IT MUST MATCH TYP63 */

								if(resrulX_.rule){

									errvrsX_.errlvl = 0;

									rule3 = resrulX_.rule[3-One];
									rule4 = resrulX_.rule[4-One];
									commlX_.li = resrulX_.rule[1-One];
									oflad = resrulX_.rule[11-One];
									/*+                                                        *RFFFF*GBA */
									wc2 = resrulX_.rule[5-One];
									if( wc2 > 0 ){
										if( (wc2 != swX_.swork[ikX_.ik-One][4-One] && 
										  wc2 != swX_.swork[ikX_.ik-One][8-One]) && 
										  wc2 != swX_.swork[ikX_.ik-One][12-One] ){
											if( resswX_.ressw == 1 )
												gba = R1RENT(&wc2, &commlX_.li, &pt);
											if( resswX_.ressw == 2 )
												gba = R2RENT(&wc2, &commlX_.li, &pt);
											if( gba > 1 )
												kk += gba - 1;
											goto L_1400;
											}
										}

									/*    In this block, check to see if the Type and Form fields match. */
									if( tagser < 65 ){
										/*+                                                        *RB001*GBA
										 *         IF (RULE4 .NE. -1 .AND. RULE4 .NE. RULTYP) GO TO 1400 */
										if( rule3 != 63 || (rule4 != 
										  -1 && rule4 != rultyp) )
											goto L_1400;
										/*-                                                        *RB001*GBA
										 *           We  have a 6500 or 6600 tag.
										 *   Type and Form check is different for wc7 than wc10 or wc11 */
										}
									else if( zz == 1 || zz == 2 ){
										if( rule3 != 65 || rule4 != rultyp )
											goto L_1400;
										}
									else if( rule3 != r65typ ){
										goto L_1400;
										}
									/*-                                                         *TY0071 */
									if( commlX_.li <= (swX_.ecount - ikX_.ik + 2) ){
										/*                                           QUICK RULE CHECK */
										rulchk(&resrulX_.rule[5-One],
										  &swX_.swork[ikX_.ik-One][1-One],
										  2,retsw);
										if( *retsw != 2 ){

											mtcinfX_.iks = ikX_.ik;
											mtcinfX_.jx = 1;
											for( ii=1; ii <= 3; ii++ ){
												mtcinfX_.swcp = dctwc[ii-One];
												if( swX_.swork[mtcinfX_.iks-One][ii-One] > 
												  0 )
												break;
												}

											mnswsv = resswX_.minisw;
											resswX_.minisw = 0;

											resmtc(retsw, commkX_.k12);

											resswX_.minisw = mnswsv;
											if( *retsw != 1 ){

													if( zz == 1 )
													{
														rsdiag(5,&resnum,&ikX_.ik,&commkX_.k12);
														goto L_2100;
													}
													else if( zz == 2 )
													{
														rsdiag(6,&resnum,&ikX_.ik,&commkX_.k12);
														goto L_2500;
													}
													else if( zz == 3 )
													{
														rsdiag(7,&resnum,&ikX_.ik,&commkX_.k12);
														goto L_2600;
													}

												}
											}
										}
									}


L_1400:
								kk += 1;
								if( kk > nwc2 )
									break;
								}
							}

						zz += 1;
						}



					if( tagser < 65 ){

						/*          NO MATCH ON WC 11 OR 10.  IS IT A PV OR UV? */

						for( yy=1; yy <= 3; yy++ ){
							if( swX_.swork[ikX_.ik-One][yy-One] >= 
							  0 ){
								zz = savtyX_.subchg[ikX_.ik-One][yy-One];
								/*+                                                        *R1711*GBA */
								if( swX_.swork[ikX_.ik-One][dctwc[yy-One]-
								  One] == 2 || swX_.swork[ikX_.ik-One][dctwc[yy-One]-
								  One] == 12 ){
									if( zz >= 844 && zz <= 848 )
										goto L_3000;

									if( zz >= 862 && zz <= 865 )
										goto L_3000;
									}
								}
							}
						}
					}


				}
			goto L_8101;


			/*          A VERB HAS BEEN FOUND.
			 *          DEPENDING ON THE TYPE OF TAGSET, OTHER TESTS ARE MADE
			 *           6355 - NO OTHER TESTS ARE NECESSARY
			 *           6360 - TEST FOR NUMBER AGREEMENT BETWEEN THE NOUN
			 *                  (THIS INFO IS IN CELL 4/14) AND THE VERB
			 *           6353 - TEST IF THE VERB IS SINGULAR
			 *           6357 - TEST IF THE VERB IS PLURAL' */

L_3000:
			if( tagtyp == 55 )
				goto L_3800;
			if( tagtyp != 53 ){
				if( tagtyp != 57 ){

					val = cellX_.cell[4-One];
					if( cellX_.cell[11-One] != 0 )
						val = cellX_.cell[14-One];

					/*+         IF CELL 4/14 = 10, MATCH ON VERB            *03/06/91*JAL*
					 *          SUPERFORM OF 53 OR 57. */
					if( val == 10 ){
						for( i=1; i <= 2; i++ ){
							if( i == 1 ){
								entry = 53 - 19;
								}
							else{
								entry = 57 - 19;
								}
							wc = 2;
							val = swX_.swork[ikX_.ik-One][dctfm[yy-One]-
							  One];
							resformod(2,wc,entry,&val,&match,retsw);
							if( errvrsX_.errlvl != 0 )
								goto L_8000;
							if( match == 1 )
								goto L_3800;
							}
						/*                       NO MATCH */
						xx = ikX_.ik + 1;
						continue;
						}
					else{

						number = 0;
						wc = 99;
						entry = 44 - 19;
						resformod(2,wc,entry,&val,&match,retsw);
						if( errvrsX_.errlvl != 0 )
							goto L_8000;
						if( match == 1 )
							number = 1;
						entry = 66 - 19;
						resformod(2,wc,entry,&val,&match,retsw);
						if( errvrsX_.errlvl != 0 )
							goto L_8000;
						if( match == 1 )
							number = 2;

						if( number == 1 )
							goto L_3060;
						if( number != 2 ){

							xx = ikX_.ik + 1;
							continue;
							}
						}
					}
				entry = 57 - 19;
				goto L_3090;
				}

L_3060:
			entry = 53 - 19;
L_3090:
			wc = 2;
			val = swX_.swork[ikX_.ik-One][dctfm[yy-One]-One];
			resformod(2,wc,entry,&val,&match,retsw);
			if( errvrsX_.errlvl != 0 )
				goto L_8000;
			if( match == 1 )
				goto L_3800;
			xx = ikX_.ik + 1;
			continue;


			/*  match on skip rule go back and try to match again for more skips
			 *+                                                        *R1673*GBA
			 * SUBTRACT 1 FROM LI FOR DUMMY WC11 ENTRY AS FIRST ELEMENT OF RULE
			 * SUBTRACT EXTRA 1 FROM XX FOR AUTOMATIC BACKSPACE
			 *2100  XX = IK + LI - 1 */
L_2100:
			if( commlX_.li > 2 ){
				xx = ikX_.ik + (commlX_.li - 1) - 1;
				}
			else{
				xx = ikX_.ik + (commlX_.li - 1);
				/*-                                                        *R1673*GBA */
				}
			}
		goto L_2500;

		/*  fall thru no match - get out */
L_8101:
		*retsw = 0;
		goto L_4000;

L_3800:
		*retsw = 1;

		if( opswX_.sw[8-One] == 1 )
			{
			fprintf( _spec_fp, "   UV OR PV FOUND AT %3d\n", ikX_.ik );
			}
		goto L_4000;


		/*  match on quit rule - get out (is this right?) */
L_2500:
		*retsw = 0;
		goto L_4000;


		/*  match on hit rule - call VTR subroutine */
L_2600:
		if( tagser == 66 ){
			*retsw = 1;
			goto L_4050;
			}
		else{
			vtr65(&retflg);
			*retsw = 1;
			}




		/*+
		 *          IF THIS IS A 6400 SERIES, SWITCH RETSW */
L_4000:
		if( tagser == 64 ){
			if( *retsw == 1 ){
				*retsw = 0;
				}
			else{
				*retsw = 1;
				}
			}
		/*-
		 *          IF THIS IS A 6600 SERIES, SWITCH RETSW */
L_4050:
		if( tagser == 66 ){
			if( *retsw == 1 ){
				*retsw = 0;
				}
			else{
				*retsw = 1;
				}
			}


		/*                               RESTORE THE IN COMMING RULE INFO. */
		commkX_.k12 = savk12;
		commlX_.li = savli;
		ikX_.ik = savik;
		mtcinfX_.iks = saviks;
		swc = savswc;
		resnum = 2;
		if( resswX_.minisw == 1 )
			resnum = 4;
		/*+                                                        *R0GBA*GBA */
		if( resswX_.ressw == 1 )
			resnum = 1;
		/*-                                                        *R0GBA*GBA
		 *+                                                        *R1637*GBA
		 *  IF IT HAS PASSED, THEN SEE IF OTHER 6300 - 6699 TAGSETS TO CHECK
		 *  IF THERE ARE, THEN GO CHECK THEM.
		 *  IF IT HAS NOT PASSED, FALL THROUGH TO EXIT */
		if( *retsw != 1 )
			break;
		t63ind += 1;
		if( tag63X_.tag63[t63ind-One] == 0 )
			break;
		}
	resrulX_.rule = RULEIN_OCR(resnum, commkX_.k12);
	if(resrulX_.rule){
		errvrsX_.errlvl = 0;
		oflad = resrulX_.rule[11-One];
		if( oflad == 0 )goto L_7000;
		errvrsX_.errlvl = OVRIN(&resnum,resrulX_.overfl, &commkX_.k12, &oflad);

		if( errvrsX_.errlvl == 0 )	goto L_7000;
		}



L_8000:
	fprintf( _spec_fp, "--ERROR IN RES6300  ERR,LOCFLG,RETSW = %8ld%8d%8d\n", 
	  errvrsX_.err, locflg, *retsw );
	retflg = -1;


L_7000:
	return;


} /*end of function*/

