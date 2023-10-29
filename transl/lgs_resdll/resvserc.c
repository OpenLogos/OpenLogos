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

/*   THIS SUBROUTINE IS CALLED TO SEARCH THE SWORKS FOR
 *   AN CERTAIN CONDITION. */

/* LAST CHG 02/03/87 *R0000GBA: CORRECT PAREN SKIPPING LOGIC AND
 *                              ADD SAME FOR DOUBLE QUOTES. */

/*     START =  THE SWORK TO START SEARCHING AT. */

/*     SNUM =  A FLAG TO INDICATE THE CONDITION WE ARE LOOKING FOR
 *               SNUM = 0  SEARCH FOR AN UNAMBIGUOUS VERB
 *                         CALLED FROM THE BEGINNING OF RES2
 *               SNUM = 1  SEARCH FOR AN UNAMBIGUOUS VERB
 *                         CALLED FROM -21001 SWITCH AND TAGSET
 *                         FUNCTIONS 6211 AND 6213
 *                    THE DIFFERENCE BETWEEN 0 AND 1 IS CERTAIN
 *                    QUIT CONDTIONS
 *               SNUM = 2  SEARCH FOR A COMMA NOT PRECEDED
 *                         BY AN UNAMBIGUOUS VERB
 *               SNUM = 3  SEARCH FOR AN UNAMBIGUOUS OR POSSIBLE VERB
 *                         CALLED FROM TAGSET FUNCTION 6214 */

/*     RETFLG =  0 IF THE CONDITION IS NOT MET BEFORE A STOP CONDITION
 *                IS ENCOUNTERED
 *            =  1 IF THE CONDITION IS MET */

#include "rescommon.h"



void  vserch(start, snum, retflg)
long int start, snum;
short int *retflg;
{
	static short ncdwc[3]={4,8,12};
	static short ncdty[3]={5,9,13};
	static short ncdfm[3]={6,10,14};
	static short ncd[3]={7,11,15};
	static short nc[3]={1,2,4};
	static short zero = 0;
	static short x = 0;
	static short ec = 0;
	static short mm = 0;
	static short v1 = 0;
	static short wc = 0;
	static short xx = 0;
	static short ec1 = 0;
	static short set = 0;
	static short sw3 = 0;
	static short sw8 = 0;
	static short wc1 = 0;
	static short yy1 = 0;
	static short form = 0;
	static short form1 = 0;
	static short match = 0;
	static short paren = 0;
	static short retsw = 0;
	static short swtyp = 0;
	static short locflg = 0;
	static short supset = 0;
	static short unamvb = 0;

	paren = 0;
	*retflg = 0;
	unamvb = 0;
	v1 = 0;

	for( ec=(short)start; ec <= swX_.ecount; ec++ ){

		for( xx=1; xx <= 3; xx++ ){

			if( swX_.swork[ec-One][xx-One] >= 0 ){

				wc = swX_.swork[ec-One][ncdwc[xx-One]-One];
				supset = ofltagX_.ofl4r[ec-One][xx-One];
				swtyp = swX_.swork[ec-One][ncdty[xx-One]-One];
				form = swX_.swork[ec-One][ncdfm[xx-One]-One];

				/*   IS IT A VERB? */
				if( wc != 2 && wc != 12 ){


					if( wc == 20 ){
						if( snum != 2 )
							goto L_315;
						if( swtyp != 2 )
							goto L_315;
						goto L_8102;
						}

					/*   IF IT IS INSIDE PARENTHESES, DON'T LOOK AT IT */
					}
				else if( paren != 1 ){

					/*          FIRST TEST FOR QUIT CONDITION
					 *                WC 12 TYPE 8 FOLLOWED BY CERTAIN VERBS */
					if( !(wc != 12 || supset != 8) ){

						ec1 = ec + 1;
						for( yy1=1; yy1 <= 3; yy1++ ){
							if( swX_.swork[ec1-One][yy1-One] >= 
							  0 ){
								wc1 = swX_.swork[ec1-One][ncdwc[yy1-One]-
								  One];
								form1 = swX_.swork[ec1-One][ncdfm[yy1-One]-
								  One];
								if( wc1 == 2 ){
									if( form1 == 1 || (form1 >= 14 && 
									  form1 <= 17) )
										goto L_7000;
									}
								else if( wc1 == 12 && form1 == 11 ){
									goto L_7000;
									}
								}
							}
						}


					/*   IF SUBCHG IS SET TO 844 - 848, IT IS AN UNAMBIGUOUS VERB. */
					if( savtyX_.subchg[ec-One][xx-One] >= 844 && 
					  savtyX_.subchg[ec-One][xx-One] <= 848 )
						goto L_180;
					/*+                                                         *R1093MBS */
					if( snum == 3 && (savtyX_.subchg[ec-One][xx-One] == 
					  862 || savtyX_.subchg[ec-One][xx-One] == 863) )
						goto L_180;
					/*-                                                         *R1093MBS */




					/*   THE CONDITION HAS NOT BEEN MET.  CHECK FOR STOP CONDITIONS. */

					/*   WC02 FORM 05 FOLLOWING BOS */
					if( (wc == 2 && form == 5) && ec == 2 )
						goto L_7000;

					if( form == 5 || form == 12 )
						goto L_8103;

					v1 = 1;

					/*   CERTAIN WC02'S */
					if( wc != 2 )
						goto L_8103;
					/*+                                                         *1156MBS */
					if( savtyX_.subchg[ec-One][xx-One] != 862 && 
					  savtyX_.subchg[ec-One][xx-One] != 863 )
						goto L_8103;

					if( ((supset == 3 || supset == 5) || supset == 
					  8) || supset == 14 )
						goto L_7000;
					set = savtyX_.savtyr[ec-One][xx-One];
					if( (supset == 11 || supset == 12) && ((set == 
					  69 || set == 73) || set == 76) )
						goto L_7000;

					ec1 = ec + 1;
					for( yy1=1; yy1 <= 3; yy1++ ){
						if( swX_.swork[ec1-One][yy1-One] >= 0 ){
							if( swX_.swork[ec1-One][yy1*4-One] == 
							  14 )
								goto L_7000;
							}
						}
					goto L_8103;
					}


				/*   WC 18 TYPE 1 */
				if( wc == 18 && supset == 1 )
					goto L_7000;

				/*   WC 01 FORM 65  IF A VERB HAS BEEN ENCOUNTERED */
				if( wc == 1 ){
					mm = 65 - 19;
					resformod(2,wc,mm,&form,&match,&retsw);
					if( errvrsX_.errlvl != 0 )
						goto L_8000;
					if( match == 0 )
						goto L_8103;
					if( v1 == 1 )
						goto L_7000;
					goto L_8103;
					/*-                                                         *1156MBS */

					/*+                                                         *R1230MBS
					 *   BOS WC19 OR
					 *   WC 19 OR CERTAIN WC 20'S IF AN AMBIGUOUS VERB HAS BEEN ENCOUNTERED */
					}
				else if( wc == 19 ){
					if( ec == 2 )
						goto L_7000;
					if( v1 == 1 )
						goto L_7000;
					if( swtyp == 20 )
						goto L_320;
					goto L_8103;
					}

				/*   WC20 */
L_315:
				if( snum == 0 && wc != 20 )
					goto L_8103;
				if( (swtyp >= 1 && swtyp <= 3) || swtyp == 10 ){
					if( v1 == 1 )
						goto L_7000;
					}
				/*+                                                         *B0110MBS
				 *+                                                        *R0000*GBA
				 *     IF (SWTYP .EQ. 6) PAREN = 1
				 *     IF (SWTYP .EQ. 7) PAREN = 0 */
				if( swtyp == 6 || swtyp == 26 )
					paren = 1;
				if( swtyp == 7 || swtyp == 27 )
					paren = 0;
				/*-                                                        *R0000*GBA
				 *-                                                         *B0110MBS */
				if( ofltagX_.ofl1r[ec-One][xx-One] != 888 )
					goto L_8103;

				/*   WC19 020 OR WC20 888 FOLLOWED BY A VERB */
L_320:
				if( snum != 0 ){
					ec1 = ec + 1;
					for( yy1=1; yy1 <= 3; yy1++ ){
						/*+                                                        *R1711*GBA
						 *     IF (SUBCHG(YY1,EC1) .GE. 844 .AND. SUBCHG(YY1,EC1) .LE. 848)
						 *    *    GO TO 7000
						 *     IF (SNUM .EQ. 3 .AND. (SUBCHG(YY1,EC1) .EQ. 862 .OR.
						 *    *    SUBCHG(YY1,EC1) .EQ. 863)) GO TO 7000 */
						if( swX_.swork[ec1-One][ncdwc[yy1-One]-One] == 
						  2 || swX_.swork[ec1-One][ncdwc[yy1-One]-
						  One] == 12 ){
							if( savtyX_.subchg[ec1-One][yy1-One] >= 
							  844 && savtyX_.subchg[ec1-One][yy1-One] <= 
							  848 )
								goto L_7000;
							if( snum == 3 && (savtyX_.subchg[ec1-One][yy1-One] == 
							  862 || savtyX_.subchg[ec1-One][yy1-One] == 
							  863) )
								goto L_7000;
							}
						}
					}
				}


L_8103:
			;
			}
		goto L_8101;
L_8102:
		if( ec == (unamvb + 1) )
			goto L_8101;
		/*   MAKE SURE IT IS NOT FOLLOWED BY A WC 02 FORM 05 */
		ec1 = ec + 1;
		for( yy1=1; yy1 <= 3; yy1++ ){
			if( swX_.swork[ec1-One][ncdwc[yy1-One]-One] == 2 && 
			  swX_.swork[ec1-One][ncdfm[yy1-One]-One] == 5 )
				goto L_8101;
			}
		goto L_8104;


		/*   UNAMBIGUOUS VERB FOUND. */
L_180:
		if( snum != 2 )
			goto L_8105;
		unamvb = ec;

L_8101:
		;
		}
	goto L_7000;

	/*   THIS SATISFIES SEARCH FUNCTION 2 */
L_8104:
	*retflg = 1;
	goto L_7000;


L_8000:
	if( sw3 == 1 || sw8 == 1 )
		{
		fprintf( _spec_fp, "--ERROR IN RESVSERC ERR,LOCFLG,RETSW = %8ld%8d%8d\n", 
		  errvrsX_.err, locflg, retsw );
		}
	*retflg = -1;
	goto L_7000;

	/*   THIS SATISFIES SEARCH FUNCTION 1 */
L_8105:
	*retflg = 1;




L_7000:
	;

	return;


} /*end of function*/

