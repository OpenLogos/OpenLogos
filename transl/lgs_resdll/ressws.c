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
	 *    01/21/92*JAL:  ADD FCN =3 TO -18 SWITCH. COPIES ONE CELL VALUE INTO
	 *       ANOTHER.
	 *    87/08/26 LG002GBA RCEL7: MOVE CELL 7(17) TO 57(67)
	 **   87/03/24 LG002GBA RT001: DON'T PUT 855 IN SUBCHG
	 **   87/03/17 LG002GBA R1718: FOR WC13 DONT OVERLAY SUGCHG 846
	 *                                       WITH 845.
	 *          CHG 01/14/87 13:30 R....GBA: ADD CSASAV ARRAY FOR SW-17
	 *          CHG 01/14/87 13:30 R1636GBA: WHEN FORM=5 SET CELL4(14)=5
	 *          CHG 04/15/86 14:44 B0393DSD: CHANGE SAVSUB(A,B) WHEN -34SW
	 *                                         ALTERS OFL1R(A,B)
	 *          CHG 04/02/86 R1510BT: DONT ALTER SUBCHG IF IT HAS VALUE
	 *          CHG 03/18/86 11:15 R1473DSD: -18 SW CELL(4/14) MOD */

#include "rescommon.h"


void  ressws(locflg, retflg)
long int locflg;
short int *retflg;
{
	static short int temp, vtrf18[100];
	static char pgmnam[9] = "RESSWS  ";
	static short dct[3]={7,11,15};
	static short dct2[3]={4,8,12};
	static short dct3[3]={6,10,14};
	static short dct4[3]={5,9,13};
	static short dct5[3]={2,4,6};
	static short nosvfr[5]={1,2,5,18,20};
	static short zero = 0;
	static short i = 0;
	static short m = 0;
	static short s = 0;
	static short x = 0;
	static short z = 0;
	static short iz = 0;
	static short mm = 0;
	static short ms = 0;
	static short om = 0;
	static short ty = 0;
	static short wc = 0;
	static short xx = 0;
	static short xy = 0;
	static short yy = 0;
	static short zz = 0;
	//static short iz2 = 0;
	static short iz4 = 0;
	static short lvl = 0;
	static short ms1 = 0;
	static short form = 0;
	static short prm1 = 0;
	static short prm2 = 0;
	static short prm3 = 0;
	static short prm4 = 0;
	static short snum = 0;
	static short vn18 = 0;
	static short frptr = 0;
	static short ikm81 = 0;
	static short n6jim = 0;
	static short retsw = 0;
	static short setwc = 0;
	static short sixty = 0;
	static short sixt1 = 0;
	static short typtr = 0;
	static short wcptr = 0;
	static short celnum = 0;
	static short celval = 0;
	static short frptr2 = 0;
	static short gotosm = 0;
	static short ptrm81 = 0;
	static short resnum = 0;
	static short thirt1 = 0;
	static short twelve = 0;
	static short twent1 = 0;
	static short typtr2 = 0;
	static short vtrcnt = 0;
	static short wcptr2 = 0;
	static short resvtr[26];
	static int _aini = 1;

	int iii;

	if( _aini ){ /* Do 1 TIME INITIALIZATIONS! */
		memset(vtrf18,'\0',sizeof(vtrf18));
	}

	*retflg = 0;

	if( (locflg == 1 || locflg == 2) || locflg == 4 ){


		/************************************************************************
		 *   A POSSIBLE MATCH
		 *    IF THIS RULE CONTAINS A -22 SWITCH, A MATCH MUST BE FOUND
		 *    IN RES22 BEFORE CONSIDERING THIS RULE A MATCH.
		 ************************************************************************ */

		sw22ptX_.sw22pt = 0;
		iz4 = 0;
		memset(vtrfX_.vtrf,'\0',sizeof(vtrfX_.vtrf));
		commvX_.vtrptr = resrulX_.rule[13-One];
		if( locflg == 1 )
			resnum = 1;
		if( locflg == 2 )
			resnum = 2;
		if( locflg == 4 )
			resnum = 4;

		if ( 0 == (iz4 = VTRIN(resnum, vtrfX_.vtrf, resrulX_.rule[13-1])) )
			goto L_2400;

		for ( iii = 0; iii < iz4; iii++ )
		{
			if( -22 == vtrfX_.vtrf[iii] )
				sw22ptX_.sw22pt = iii+1;
		}

		/************************************************************************
		 *     IF THERE WAS NO -22 SWITCH, THIS IS A MATCH.
		 *     IF THERE WAS ANY -46 SWITCHES PRECEDING THE -22, THE -22
		 *     MUST BE PROCESSED BEFORE CONSIDERING THIS MATCH.
		 ************************************************************************ */
		vtrcnt = iz4;
		if( sw22ptX_.sw22pt == 0 ){
			*retflg = 1;
			}
		else{
			if( sw22ptX_.sw22pt != 1 ){

				/*     PROCESS ANY -46 SWITCHES (THE -46 IS THE ONLY SWITCH ALLOWED
				 *     BEFORE THE -22) */

				m = 1;
				while( vtrfX_.vtrf[m-One] == -46 ){

					/************************************************************************
					 *   -46 SWITCH
					 *     FUNCTION: BEFORE THE -22 SWITCH, THE -46 CHANGES
					 *               THE SEMWRK VALUES.
					 *     2ND PARAMETER  =  NEW WC
					 *     3RD PARAMETER  =  NEW TYPE
					 *     4TH PARAMETER  =  NEW FORM
					 ************************************************************************ */
					prm1 = vtrfX_.vtrf[m+1-One];
					prm2 = vtrfX_.vtrf[m+2-One];
					prm3 = vtrfX_.vtrf[m+3-One];
					prm4 = vtrfX_.vtrf[m+4-One];
					ikm81 = ikX_.ik - 81;
					ptrm81 = -prm1 - 80;


					if( prm2 != 0 ){
						if( prm2 > 0 ){

							chgsemX_.semchg[ptrm81-One][1-One] = prm2;
							}
						else{

							xy = ikm81 - prm2;
							wcptr2 = 4*jbgusX_.jgus[xy-One];
							chgsemX_.semchg[ptrm81-One][1-One] = swX_.swork[xy-One][wcptr2-One];
							}
						}


					if( prm3 != 0 ){
						if( prm3 <= 0 ){

							xy = ikm81 - prm3;
							typtr2 = (4*jbgusX_.jgus[xy-One]) + 1;
							chgsemX_.semchg[ptrm81-One][2-One] = swX_.swork[xy-One][typtr2-One];

							}
						else if( prm3 >= 844 && prm3 <= 866 ){

							xy = ikm81 - prm1;
							if( resswX_.ressw == 1 ){
								if( prm3 >= 862 ){
									if( prm3 == 862 ){
										temp = savtyX_.subchg[xy-One][jbgusX_.jgus[xy-One]-
										  One];
										if( (((temp == 850 || temp == 
										  852) || temp == 858) || 
										  temp == 861) || temp == 
										  864 )
											savtyX_.subchg[xy-One][jbgusX_.jgus[xy-One]-
											  One] = prm3;
										}
									/*- 89/12/14                                                 *R1970*GBA */
									if( !(savtyX_.subchg[xy-One][jbgusX_.jgus[xy-One]-
									  One] < 844 || savtyX_.subchg[xy-One][jbgusX_.jgus[xy-One]-
									  One] > 848) ){
										if( prm3 == 862 && savtyX_.subchg[xy-One][jbgusX_.jgus[xy-One]-
										  One] == 844 )
											savtyX_.subchg[xy-One][jbgusX_.jgus[xy-One]-
											  One] = 846;
										goto L_340;
										}
									}
								}
							/*+                                               04/02/86  *R1510BT
							 *  DON'T ALTER SUBCHG IF IT ALREADY HAS A VALUE */
							if( !((savtyX_.subchg[xy-One][jbgusX_.jgus[xy-One]-
							  One] == 852 || savtyX_.subchg[xy-One][jbgusX_.jgus[xy-One]-
							  One] == 858) || savtyX_.subchg[xy-One][jbgusX_.jgus[xy-One]-
							  One] == 861) ){
								if( !((prm3 == 845 && savtyX_.subchg[xy-One][jbgusX_.jgus[xy-One]-
								  One] == 846) && swX_.swork[xy-One][4*jbgusX_.jgus[xy-One]-
								  One] == 13) ){
									if( prm3 != 855 )
										savtyX_.subchg[xy-One][jbgusX_.jgus[xy-One]-
										  One] = prm3;
									}
								}
							}
						else{

							chgsemX_.semchg[ptrm81-One][2-One] = prm3;
							}
					} 

L_340:
					if( prm4 != 0 ){
						if( prm4 > 0 ){

							chgsemX_.semchg[ptrm81-One][3-One] = prm4;
							}
						else{

							xy = ikm81 - prm4;
							frptr2 = (4*jbgusX_.jgus[xy-One]) + 2;
							chgsemX_.semchg[ptrm81-One][3-One] = swX_.swork[xy-One][frptr2-One];
							}
						}


					m += 5;
					if( m == sw22ptX_.sw22pt )
						goto L_460;
					}

				fprintf( _spec_fp, " ILLEGAL SWITCH BEFORE THE -22 IN RES2 \n" );
				}

L_460:
			if( opswX_.sw[8-One] == 1 ){
				fprintf( _spec_fp, " POSSIBLE MATCH GOING TO RESSEM AT %2d RULE NO. %4d", 
				  commiX_.i4, commkX_.k12 );
				print_res_rule(_spec_fp, resnum, commkX_.k12);
				}
			}
		goto L_2380;
		}
	else if( !(((((((((((((locflg == 3 || (locflg >= 5 && locflg <= 
	  12)) || (locflg >= 14 && locflg <= 16)) || locflg == 19) || 
	  locflg == 20) || (locflg >= 23 && locflg <= 27)) || locflg == 
	  29) || locflg == 32) || locflg == 33) || locflg == 35) || (locflg >= 
	  37 && locflg <= 40)) || locflg == 43) || locflg == 44) || (locflg >= 
	  47 && locflg <= 50)) ){
		if( locflg == 13 ){

			/*SW13+
			 ************************************************************************
			 *        -13 SWITCH
			 ************************************************************************ */
			x = x;
			prm1 = vtrfsX_.vtrfs[commvX_.vn+1-One];
			commvX_.vn += 2;

			if( prm1 == 1 ){
				for( m=miniX_.i4sav; m <= miniX_.i4pli; m++ ){
					swX_.scont2[m-One] = 4;
					if( m == miniX_.i4pli && negmtcX_.no2res != 0 )
						swX_.scont2[m-One] = 5;
					}
				}
			else{

				ikm81 = ikX_.ik - 81;
				n6jim = ikm81 - prm1;
				swX_.scont2[n6jim-One] = 4;
				if( n6jim == miniX_.i4pli && negmtcX_.no2res !=  0 )
					swX_.scont2[n6jim-One] = 5;
				}
			goto L_2380;
			}
		else if( locflg == 17 ){
			 /************************************************************************
			 *   THE -17 SWITCH IS USED TO INITIALIZE CELLS
			 ************************************************************************ */
			prm1 = vtrfsX_.vtrfs[commvX_.vn+1-One];
			prm2 = vtrfsX_.vtrfs[commvX_.vn+2-One];
			prm3 = vtrfsX_.vtrfs[commvX_.vn+3-One];
			n6jim = (ikX_.ik - 81) - prm2;
			if( prm2 == 0 )
				n6jim = 0;
			commvX_.vn += 4;

			if( prm1 > 99 ){

				xx = 26;
				yy = prm1 - 179;
				/*+                                                         *R1154MBS
				 *+                                                         *R....GBA
				 *     IF (PRM1 .EQ. 182 .OR. PRM1 .EQ. 186)
				 *    * CALL LMOVE (CELL(51),1,CELL(1),1,98) */
				if( prm1 == 182 || prm1 == 186 ){
					lmove(&cellX_.cell[51-One],1,&cellX_.cell[1-One],
					  1,98);
					if( n6jim != 0 )
						savset(n6jim,&retsw);
					}
				}
			else{
				xx = 1;
				}
			for( zz=1; zz <= xx; zz++ ){

				celnum = prm1;

				if( xx == 26 )
					celnum = cinitX_.cinit[yy-One][zz-One];
				if( celnum == 0 )
					break;

				if( prm3 == 1 && cellX_.cell[11-One] != 0 )
					celnum += 10;

				if( celnum == 1 || celnum == 3 ){

					cellX_.cell[celnum-One] = 1;
					}
				else{
					cellX_.cell[celnum-One] = 0;
					}

				/*   SET THE CELL ARRAY ALSO IF PRM2 IS A POINTER */
				if( n6jim != 0 ){
					csaset(celnum,n6jim,&retsw);
					if( errvrsX_.errlvl != 0 )
						goto L_2400;
					}

				}

			goto L_2380;
			}

		else if( locflg == 18 ){


			/*SW18+
			 ************************************************************************
			 *   THE -18 SWITCH IS USED TO SET CELLS
			 ************************************************************************ */
			prm1 = vtrfsX_.vtrfs[commvX_.vn+1-One];
			prm2 = vtrfsX_.vtrfs[commvX_.vn+2-One];
			prm3 = vtrfsX_.vtrfs[commvX_.vn+3-One];
			prm4 = vtrfsX_.vtrfs[commvX_.vn+4-One];
			n6jim = (ikX_.ik - 81) - prm3;
			if( prm3 == 0 )
				n6jim = 0;
			commvX_.vn += 5;
			gotosm = 0;
			celnum = prm1;
			if( (prm4 == 1 || prm4 == 2) && cellX_.cell[11-One] != 0 )
				celnum += 10;

			/*   WHEN PRM1 = 4, PRM4 = 2 IS A FUNCTION NUMBER WHICH MEANS TEST
			 *   CELLS 3 AND 4 (OR 13 AND 14) BEFORE SETTING CELL 4 */
			if( !(prm1 != 4 || prm4 != 2) ){
				if( cellX_.cell[celnum-One] != 0 )
					goto L_900;
				if( cellX_.cell[celnum-1-One] != 1 && cellX_.cell[celnum-1-One] != 3 )
					goto L_900;
				}

			if( prm2 == 0 ){

				if( !(celnum < 3 || celnum > 5) ){
					ms = celnum - 2;

					}
				else if( celnum < 13 || celnum > 15 ){
					if( celnum == 29 || celnum == 39 ){
						xx = jbgusX_.jgus[n6jim-One];
						cellX_.cell[celnum-One] = ofltagX_.ofl1r[n6jim-One][xx-One];
						}
					goto L_900;
					}
				else{
					ms = celnum - 12;
					}


				xx = jbgusX_.jgus[n6jim-One];
				yy = (xx*4) + 2;

				if( ms == 2 ){

					/*   FOR CELLS 4 AND 14 USE THE FORM (SIGNIFIES NUMBER OF A NOUN) */
					form = swX_.swork[n6jim-One][yy-One];
					celval = form;
					/*+    MORE SPECIAL FORMS: 5, 15, 17              03/18/86  *R1473DSD */
					if( ((form == 13 || form == 50) || form == 60) || 
					  form == 70 )
						celval = 1;
					/*+   ABOVE CHANGED FROM THE FOLLOWING:           14/01/87  *R1636GBA
					 *    *    .OR. FORM .EQ. 70 .OR. FORM .EQ. 05) CELVAL = 1  *R1636GBA
					 *    AND BELOW TEST FOR FORM=5 ADDED             14/01/87  *R1636GBA */
					if( form == 5 )
						celval = 5;
					/*-                                               14/01/87  *R1636GBA */
					if( (form == 12 || form == 14) || form == 15 )
						celval = 2;
					if( form == 17 )
						celval = 10;
					/*-                                               03/18/86  *R1473DSD
					 *+                                                         *R1122MBS */
					gotosm = 1;
					/*-                                                         *R1122MBS */
					}
				else if( ms == 3 ){

					/*   FOR CELLS 5 AND 15 USE THE FORM (SIGNIFIES THE TENSE OF A VERB) */
					form = swX_.swork[n6jim-One][yy-One];
					celval = form;
					/*+                                                         *R1475JGB
					 *   IF CELL5 OR 15 IS NOT 0, TRANSFER VALUES TO CELLS 55/65
					 *   BEFORE SETTING CELL 5/15 SO FORM INFORMATION ISN'T LOST */

					/*     IF (CELNUM .EQ. 5 .OR. CELNUM .EQ. 15) THEN */
					if( ((celnum == 5 || celnum == 15) || celnum == 
					  7) || celnum == 17 ){
						if( cellX_.cell[celnum-One] != 0 )
							cellX_.cell[celnum+50-One] = cellX_.cell[celnum-One];
						}
					}
				else{

					/*   FOR CELLS 3 AND 13, LOAD SET CODE INTO CELL */
					celval = savtyX_.savtyr[n6jim-One][xx-One];
					/*+                                                         *R1122MBS */
					gotosm = 1;
					/*-                                                         *R1122MBS */
					}
				}
			else{

				celval = prm2;
				/*+          IF PRM4 = 3, COPY CELL PRM2 TO CELL PRM1   *01/21/92*JAL* */
				if( prm4 == 3 ){
					celval = cellX_.cell[prm2-One];
					if( prm2 >= 1 && prm2 <= 40 ){
						celval = cellX_.csaray[n6jim-One][prm2-One];
						}
					else if( prm2 >= 51 && prm2 <= 90 ){
						celval = cellX_.csasav[n6jim-One][prm2-50-One];
						/*-                                                     *01/21/92*JAL* */
						}
					}
				}

			cellX_.cell[celnum-One] = celval;
			/*          IF CELL(11) IS SET TO 1 & SET CELL(13) = 1 AND UPDATE THE
			 *          CLUASE STATUS ARRAY, EXCEPT IF CELL(13) NE 0 AND PARM 4=1. */
			if( celnum == 11 && celval == 1 ){
				if( cellX_.cell[13-One] == 0 || prm4 != 1 ){
					celnum = 13;
					cellX_.cell[celnum-One] = 1;
					if( n6jim != 0 )
						csaset(celnum,n6jim,&retsw);
					celnum = 11;
					}
				}

			/*   SET CELL ARRAY ALSO */
			if( n6jim != 0 ){
				csaset(celnum,n6jim,&retsw);
				if( errvrsX_.errlvl != 0 )
					goto L_2400;

				/*             IF CELL(3) OR (13) IS BEING SET, ALSO SET (22) OR (32)
				 *                                                   AND (23) OR (33) */
				if( (prm1 == 3 || prm1 == 13) && prm2 == 0 ){
					celnum += 19;
					cellX_.cell[celnum-One] = ofltagX_.ofl1r[n6jim-One][xx-One];
					csaset(celnum,n6jim,&retsw);

					celnum += 1;
					cellX_.cell[celnum-One] = ofltagX_.ofl4r[n6jim-One][xx-One];
					csaset(celnum,n6jim,&retsw);

					}
				/*+                                                         *R1122MBS */

				/*+                                                         *R1214MBS
				 *          IF CELL(4) OR (14) IS BEING SET, ALSO SET (24) OR (34)
				 *                                                AND (25) OR (35)
				 *                                                AND (26) OR (36) */
				if( (prm1 == 4 || prm1 == 14) && n6jim != 0 ){
					xx = jbgusX_.jgus[n6jim-One];
					celnum += 20;
					cellX_.cell[celnum-One] = ofltagX_.ofl4r[n6jim-One][xx-One];
					csaset(celnum,n6jim,&retsw);

					celnum += 1;
					cellX_.cell[celnum-One] = swX_.swork[n6jim-One][(xx*4)+1-One];
					csaset(celnum,n6jim,&retsw);

					celnum += 1;
					cellX_.cell[celnum-One] = ofltagX_.ofl1r[n6jim-One][xx-One];
					csaset(celnum,n6jim,&retsw);
					}

				/*-                                                         *R1214MBS */

				/*+                                                         *R1436JGB
				 *        SPECIAL TEST TO SET CELL 4 BASED ON CELL 21 VALUE */

				if( ((prm1 == 4 && prm2 == 0) && cellX_.cell[21-One] >= 
				  10) && cellX_.cell[21-One] <= 12 )
					cellX_.cell[4-One] = 10;
				}
			/*-                                                         *R1436JGB */


			/*          ADD A -42 SWITCH SEND TO SEMRES? */
L_900:
			if( !(gotosm == 0 || n6jim == 0) ){
				vtrf18[1-One] = -42;
				vtrf18[2-One] = prm3;
				vtrf18[3-One] = 0;
				vtrf18[4-One] = 0;
				vtrf18[5-One] = 0;
				vtrf18[6-One] = 0;
				vn18 = 1;
				ressem(1,vn18,vtrf18,ikX_.ik,lvl,&sw22chX_.sw22ch,
				  &retsw);
				}

			goto L_2380;
			}
		else if( locflg == 21 ){


			/*SW18- */


			/*SW21+
			 ************************************************************************
			 *   -21 SWITCH
			 *       TEST REMAINING SWORKS FOR AN UNAMBIGUOUS VERB TO SEE IF
			 *       CELL(2) SHOULD BE RESET
			 ************************************************************************ */

			x = x;
			prm1 = vtrfsX_.vtrfs[commvX_.vn+1-One];
			prm2 = vtrfsX_.vtrfs[commvX_.vn+2-One];
			n6jim = ((ikX_.ik - 81) - prm2) + 1;
			commvX_.vn += 3;

			if( prm1 == 1 ){


				/*   FUNCTION 1 - SEARCH FOR AN UNAMBIGUOUS VERB.
				 *                IF FOUND, SET CELL 2 (OR 12) TO 1 */
				celnum = 2;
				if( cellX_.cell[11-One] == 1 )
					celnum = 12;

				snum = 1;
				vserch(n6jim,snum,&retsw);

				cellX_.cell[celnum-One] = retsw;
				csaset(celnum,n6jim,&retsw);
				if( errvrsX_.errlvl != 0 )
					goto L_2400;
				}
			else if( prm1 == 2 ){

				/*   FUNCTION 2 - SEARCH FOR A COMMA NOT PRECEDED BY AN UNAM. VERB
				 *                IF FOUND, SET FORM OF ELEMENT POINTED TO TO 19 */

				snum = 2;
				vserch(n6jim,snum,&retsw);
				if( retsw == 1 )
					swX_.swork[n6jim-One][dct3[jbgusX_.jgus[n6jim-One]-
					  One]-One] = 19;
				}

			goto L_2380;
			}
		else if( locflg == 22 ){
			 /************************************************************************
			 *   START OF -22 SWITCH.
			 ************************************************************************ */
			ressem(2,x,(short*)&x,x,x,&x,&retsw);
			if( errvrsX_.errlvl == 0 ){
				commvX_.vn += 6;
				goto L_2380;
				}
			}
		else if( locflg == 28 ){
			 /************************************************************************
			 *         THE -28 SWITCH SETS THE RESPAS ARRAY, WHICH IS PASSED TO
			 *         TRAN1. THIS ARRAY CONTAINS 3 POSITIONS FOR EACH ELEMENT.
			 *                PRM1 = WHICH CELL TO SET (1,2 OR 3)
			 *                PRM2 = THE VALUE TO SET IT TO
			 *                PRM3 = POINTER TO WHICH ELEMENT
			 *                PRM4 = A FUNCTION TO TELL TRAN WHERE TO PUT THIS
			 *                       VALUES (THIS INFO WILL BE COMMUNICATED
			 *                       THROUGH THE THOUSANDS DIGIT)
			 ************************************************************************ */
			x = x;
			prm1 = vtrfsX_.vtrfs[commvX_.vn+1-One];
			prm2 = vtrfsX_.vtrfs[commvX_.vn+2-One];
			prm3 = vtrfsX_.vtrfs[commvX_.vn+3-One];
			prm4 = vtrfsX_.vtrfs[commvX_.vn+4-One];
			n6jim = (ikX_.ik - 81) - prm3;
			commvX_.vn += 5;

			celval = prm2 + (prm4*1000);

			respasX_.respas[n6jim-One][prm1-One] = celval;

			goto L_2380;
			}
		else if( locflg == 30 ){

			/*SW28-
			 *-                                                         *R1307MBS
			 *SW30+
			 ************************************************************************
			 *   BEGINNING OF -30 SWITCH
			 *    FUNCTION:  TO CORRECT THE RESOLUTION OF THE SWORK PRM1
			 *               TO AN SWORK WITH WORD CLASS PRM2.
			 ************************************************************************ */


			x = x;
			prm1 = vtrfsX_.vtrfs[commvX_.vn+1-One];
			prm2 = vtrfsX_.vtrfs[commvX_.vn+2-One];
			commvX_.vn += 3;

			xx = (ikX_.ik - 81) - prm1;
			wc = prm2;

			for( m=1; m <= 3; m++ ){
				if( jbgusX_.jgus[xx-One] != m ){
					/*+                                                         *R1238MBS */
					if( wc == 21 || wc == 31 ){

						if( swX_.swork[xx-One][4*m-One] != 2 )
							goto L_2422;
						if( swX_.swork[xx-One][(4*m)+1-One] == wc )
							goto L_1120;

						if( ofltagX_.ofl4r[xx-One][m-One] >= 10 && 
						  ofltagX_.ofl4r[xx-One][m-One] <= 12 ){
							if( wc == 31 )
								goto L_1120;
							goto L_2422;
							}
						}
					/*-                                                         *R1238MBS */
					if( swX_.swork[xx-One][4*m-One] == wc )
						goto L_1120;
					}

L_2422:
				;
				}
			goto L_2380;

			/*   CORRECT JGUS */
L_1120:
			swX_.swork[xx-One][jbgusX_.jgus[xx-One]-One] = -1;
			jbgusX_.jgus[xx-One] = m;
			swX_.swork[xx-One][m-One] = 1;
			/*+                                                         *R1241MBS
			 *          FOR RESET TO WC 01, RESET CELL(4) OR (14) IF IT WAS JUST SET */
			if( wc == 1 ){
				celnum = 4;
				if( cellX_.cell[11-One] != 0 )
					celnum = 14;
				if( !(cellX_.csaray[xx-1-One][celnum-One] == 0 || 
				  cellX_.csaray[xx-2-One][celnum-One] != 0) ){
					form = swX_.swork[xx-One][(m*4)+2-One];
					celval = form;
					if( ((form == 13 || form == 50) || form == 60) || 
					  form == 70 )
						celval = 1;
					if( form == 12 || form == 14 )
						celval = 2;
					cellX_.cell[celnum-One] = celval;
					csaset(celnum,xx,&retsw);
					/*-                                                         *R1241MBS */
					}
				}
			goto L_2380;
			}
		else if( locflg == 31 ){

			/*   END OF -30 SWITCH */

			/*SW30- */




			/*SW31+
			 ************************************************************************
			 *   BEGINNING OF -31 SWITCH
			 *    PRM1 = 56.  FUNCTION TO SAVE THE ADDRESS OF
			 *                THIS RULE SO IT WON'T BE MATCHED AGAIN.
			 ************************************************************************ */


			x = x;
			prm1 = vtrfsX_.vtrfs[commvX_.vn+1-One];
			commvX_.vn += 2;

			if( prm1 == 56 ){

				for( mm=1; mm <= 6; mm++ ){
					if( resswX_.minisw == 0 ){
						if( nloopX_.nloop[mm-One] == 0 )
							goto L_2423;
						}
					else if( nloopX_.nloop2[mm-One] == 0 ){
						goto L_2424;
						}

					}
				goto L_2380;
L_2424:
				nloopX_.nloop2[mm-One] = commkX_.k12;
				goto L_2380;
L_2423:
				nloopX_.nloop[mm-One] = commkX_.k12;
				}
			goto L_2380;
			}
		else if( locflg == 34 ){

			/*   END OF -30 SWITCH */

			/*SW30- */



			/*SW34+
			 ************************************************************************
			 *   BEGINNING OF -34 SWITCH.
			 *    FUNCTION:  TO LOAD THE WC, TYPE OR FORM FROM THE 34 SW ARGUMENTS
			 *               INTO THE SWORK. */

			/*     SET THE OUTGOING VALUES FOR TRAN1
			 *     CHECK THE TYPE FIELD RANGES AND SET THE CORRECT FIELDS
			 *            IN TRAN1                     IN RES, PASS TOTRAN1
			 *  WC:       SWORK(4,8 OR 12,N6JIM)       SWORK(4,8 OR 12,N6JIM)
			 *  SUPERSET: SCON(13,ELSCNP(N6JIM))       OFL4R(JGUS(N6JIM),N6JIM)
			 *  SET:      SCON(11,ELSCNP(N6JIM))       SWORK(DCT4(JGUS(N6JIM),N6JIM)
			 *  SUBSET:   SWORK(DCT4(JS(N6JIM)),N6JIM) OFL1R(JGUS(N6JIM),N6JIM)
			 *  FORM:     SWORK(6,10 OR 14,N6JIM)      SWORK(6,10 OR 14,N6JIM)
			 ************************************************************************ */

			/*          FUNCTION - PRM4 POINTS TO HEAD */


			x = x;
			prm1 = vtrfsX_.vtrfs[commvX_.vn+1-One];
			prm2 = vtrfsX_.vtrfs[commvX_.vn+2-One];
			prm3 = vtrfsX_.vtrfs[commvX_.vn+3-One];
			prm4 = vtrfsX_.vtrfs[commvX_.vn+4-One];
			n6jim = (ikX_.ik - 81) - prm4;
			commvX_.vn += 5;

			/*     FIRST DETERMINE WHICH PART OF THE SWORK WE SHOULD BE LOOKING AT. */

			/*     WRITE(6,1111)(JGUS(TY),TY=1,10),N6JIM,IK,PRM4,VN
			 *1111  FORMAT (' -34 SWITCH  JGUS,N6JIM,IK,PRM4,VN = ',10I4,/4I6) */

			om = jbgusX_.jgus[n6jim-One];


			if( prm1 > 0 ){
				swX_.swork[n6jim-One][dct2[om-One]-One] = prm1;
				/*+                                                         *R1132MBS */
				savtyX_.savwc[n6jim-One][om-One] = prm1;
				}
			/*-                                                         *R1132MBS */


			if( prm2 > 0 ){

				if( prm2 <= 16 ){
					ofltagX_.ofl4r[n6jim-One][om-One] = prm2;
					/*+                                                         *R1132MBS */
					savtyX_.savsup[n6jim-One][om-One] = prm2;
					/*-                                                         *R1132MBS
					 *     IN CASE TYPE STORED IN WRONG PLACE - CHECK ALL THREE */
					if( ofltagX_.ofl1r[n6jim-One][om-One] <= 16 )
						ofltagX_.ofl1r[n6jim-One][om-One] = prm2;
					if( swX_.swork[n6jim-One][dct4[om-One]-One] <= 
					  16 )
						swX_.swork[n6jim-One][dct4[om-One]-One] = prm2;

					}
				else if( prm2 > 99 ){
					ofltagX_.ofl1r[n6jim-One][om-One] = prm2;
					/*+                                               04/15/86  *B0393DSD */
					savtyX_.savsub[n6jim-One][om-One] = prm2;
					/*-                                               04/15/86  *B0393DSD
					 *     IN CASE TYPE STORED IN WRONG PLACE - CHECK ALL THREE */
					if( swX_.swork[n6jim-One][dct4[om-One]-One] > 
					  99 )
						swX_.swork[n6jim-One][dct4[om-One]-One] = prm2;
					if( ofltagX_.ofl4r[n6jim-One][om-One] > 99 )
						ofltagX_.ofl4r[n6jim-One][om-One] = prm2;
					}
				else{
					swX_.swork[n6jim-One][dct4[om-One]-One] = prm2;
					/*+                                                         *R1132MBS */
					savtyX_.savtyr[n6jim-One][om-One] = prm2;
					/*-                                                         *R1132MBS
					 *     IN CASE TYPE STORED IN WRONG PLACE - CHECK ALL THREE */
					if( ofltagX_.ofl1r[n6jim-One][om-One] <= 99 )
						ofltagX_.ofl1r[n6jim-One][om-One] = prm2;
					if( ofltagX_.ofl4r[n6jim-One][om-One] > 16 )
						ofltagX_.ofl4r[n6jim-One][om-One] = prm2;
					}
				}


			if( prm3 > 0 ){
				swX_.swork[n6jim-One][dct3[om-One]-One] = prm3;
				savtyX_.savfrm[n6jim-One][om-One] = prm3;
				}

			/*   ***** END OF RES -34 SWITCH ***** */

			goto L_2380;
			}
		else if( locflg == 36 ){




			/*SW36+
			 ************************************************************************
			 *   BEGINNING OF -36 SWITCH - CHANGE FORM.
			 ************************************************************************ */
			x = x;
			xx = (ikX_.ik - 81) - vtrfsX_.vtrfs[commvX_.vn+2-One];
			i = 4*jbgusX_.jgus[xx-One];
			swX_.swork[xx-One][i+2-One] = vtrfsX_.vtrfs[commvX_.vn+1-One];
			ms = swX_.swork[xx-One][i-One];

			/*   NOSVFRM CONTAINS THE WORD CLASSES THAT CAN BE CHANGED BY RES. */
			for( m=1; m <= 5; m++ ){
				if( ms == nosvfr[m-One] )
					goto L_1420;
				}
			goto L_1440;

L_1420:
			savtyX_.savfrm[xx-One][jbgusX_.jgus[xx-One]-One] = vtrfsX_.vtrfs[commvX_.vn+1-One];


L_1440:
			commvX_.vn += 3;
			goto L_2380;
			}
		else if( locflg == 41 ){

			/*   END OF -36 SWITCH
			 *SW36- */




			/*SW41+
			 ************************************************************************
			 *   BEGINNING OF -41 SWITCH.
			 *    IN NORMAL RES PROCESSING, A NEW MATCH WILL BEGIN ON THE
			 *    LAST ELEMENT MATCHED ON IN THE PREVIOUS RULE.
			 *    THE -41 SWITCH CAUSES THE MATCH TO BACK UP MORE
			 *    THAN ONE.
			 ************************************************************************ */
			x = x;
			x = miniX_.lvlgus;
			if( x >= vtrfsX_.vtrfs[commvX_.vn+1-One] )
				commiX_.i4 += -vtrfsX_.vtrfs[commvX_.vn+1-One] + 1;
			miniX_.iksave = commiX_.i4;
			commvX_.vn += 2;
			goto L_2380;
			}
		else if( locflg == 42 ){

			/*   END OF -41 SWITCH. */

			/*SW41- */




			/*SW42+
			 ************************************************************************
			 *   BEGINNING OF -42 SWITCH.
			 *    FUNCTION:  TO SEARCH RES22 FOR A MATCH.
			 ************************************************************************ */
			x = x;
			ressem(1,commvX_.vn,vtrfsX_.vtrfs,ikX_.ik,miniX_.lvlgus,
			  &sw22chX_.sw22ch,&retsw);
			if( errvrsX_.errlvl == 0 ){

				commvX_.vn += 6;
				goto L_2380;
				}
			}
		else if( locflg == 45 ){





			/*SW45+ */
			x = x;
			commvX_.vn += 5;
			goto L_2380;
			}
		else if( locflg == 46 ){
			/*SW45- */



			/*SW46+
			 ************************************************************************
			 *   -46 SWITCH
			 *     FUNCTION: CHANGE WC TYPE AND/OR FORM OF
			 *     ELEMENT POINTED BY FIRST PARAMETER
			 *     2ND PARAMETER = NEW WC
			 *     3RD PARAMETER = NEW TYPE
			 *     4TH PARAMETER = NEW FORM
			 ************************************************************************ */
			x = x;
			prm1 = vtrfsX_.vtrfs[commvX_.vn+1-One];
			prm2 = vtrfsX_.vtrfs[commvX_.vn+2-One];
			prm3 = vtrfsX_.vtrfs[commvX_.vn+3-One];
			prm4 = vtrfsX_.vtrfs[commvX_.vn+4-One];
			/*                                                          *J0195
			 *          -46 SWITCH  TAKE OUT SETTING OF SAVWC
			 *                                                          *J0195 */

			if( prm1 == 31 ){
				/*   FUNCTION 31 INDICATES A CHANGE TO THE LAST ELEMENT OF THE MATCH.
				 *    NNN4 WILL TELL WHAT CHANGES TO MAKE DEPENDING ON IF THE
				 *    LAST ELEMENT OF THE MATCH MATCHED ON A -8 OR A -9.
				 *    NO2RES TELLS WHICH NEGATIVE WC WAS MATCHED. */

				if( !(negmtcX_.no2res != 8 && negmtcX_.no2res != 9)
				   ){

					for( m=1; m <= 3; m++ ){
						if( negmtcX_.negmtc[m-One] != 0 ){

							wcptr = 4*m;
							wc = swX_.swork[miniX_.i4pli-One][wcptr-One];

							if( negmtcX_.no2res == 9 ){

								if( prm4 == 15 ){
									if( wc == 2 )
										setwc = 4;
									if( wc == 12 )
										setwc = 15;
									}
								else if( prm4 == 17 ){
									if( wc == 2 )
										setwc = 10;
									if( wc == 12 )
										setwc = 17;
									}
								else{
									goto L_2425;
									}
								swX_.swork[miniX_.i4pli-One][wcptr-One] = setwc;
								if( wc == 12 )
									swX_.swork[miniX_.i4pli-One][wcptr+2-One] = prm4;
								}
							else{

								setwc = wc;
								if( wc == 14 )
									setwc = 11;
								if( !(wc != 1 && wc != 16) ){
									if( (prm4 == 13 || prm4 == 83) || 
									  prm1 == 84 )
										setwc = 3;
									if( prm4 == 15 )
										setwc = 5;
									if( prm4 == 17 )
										setwc = 7;
									if( prm4 == 18 )
										setwc = 8;
									}
								swX_.swork[miniX_.i4pli-One][wcptr-One] = setwc;
								if( wc == 14 || wc == 18 )
									swX_.swork[miniX_.i4pli-One][wcptr+2-One] = prm4;
								}
							}

L_2425:
						;
						}
					}

				/*      FUNCTION 32 */

				}
			else if( prm1 != 32 ){

				ikm81 = ikX_.ik - 81;
				xx = ikm81 - prm1;
				wcptr = 4*jbgusX_.jgus[xx-One];
				typtr = wcptr + 1;
				frptr = wcptr + 2;


				/*+                                                         *R1248MBS
				 *               BEFORE THE -42 SWITCH, THE -46 CHANGES
				 *               THE SEMWRK VALUES.
				 *                   2ND PARAMETER  =  NEW WC
				 *                   3RD PARAMETER  =  NEW TYPE
				 *                   4TH PARAMETER  =  NEW FORM */


				if( vtrfsX_.vtrfs[commvX_.vn+5-One] == -42 ){

					ptrm81 = -prm1 - 80;


					if( prm2 != 0 ){
						if( prm2 > 0 ){

							chgsemX_.semchg[ptrm81-One][1-One] = prm2;
							}
						else{

							xy = ikm81 - prm2;
							wcptr2 = 4*jbgusX_.jgus[xy-One];
							chgsemX_.semchg[ptrm81-One][1-One] = swX_.swork[xy-One][wcptr2-One];
							}
						}


					if( prm3 != 0 ){
						if( prm3 <= 0 ){

							xy = ikm81 - prm3;
							typtr2 = (4*jbgusX_.jgus[xy-One]) + 1;
							chgsemX_.semchg[ptrm81-One][2-One] = swX_.swork[xy-One][typtr2-One];

							/*+                                                         *R1184MBS */
							}
						else if( prm3 >= 844 && prm3 <= 866 ){
							/*-                                                         *R1184MBS */

							/*+                                                         *R1182MBS */
							xy = ikm81 - prm1;
							if( resswX_.ressw == 1 ){
								if( prm3 >= 862 ){
									/*+ 89/12/14                                                 *R1970*GBA */
									if( prm3 == 862 ){
										temp = savtyX_.subchg[xy-One][jbgusX_.jgus[xy-One]-
										  One];
										if( (((temp == 850 || temp == 
										  852) || temp == 858) || 
										  temp == 861) || temp == 
										  864 )
											savtyX_.subchg[xy-One][jbgusX_.jgus[xy-One]-
											  One] = prm3;
										}
									/*- 89/12/14                                                 *R1970*GBA */
									if( !(savtyX_.subchg[xy-One][jbgusX_.jgus[xy-One]-
									  One] < 844 || savtyX_.subchg[xy-One][jbgusX_.jgus[xy-One]-
									  One] > 848) ){
										if( prm3 == 862 && savtyX_.subchg[xy-One][jbgusX_.jgus[xy-One]-
										  One] == 844 )
											savtyX_.subchg[xy-One][jbgusX_.jgus[xy-One]-
											  One] = 846;
										goto L_1940;
										}
									}
								}
							/*+                                               04/02/86  *R1510BT
							 *  DON'T ALTER SUBCHG IF IT ALREADY HAS A VALUE */
							if( !((savtyX_.subchg[xy-One][jbgusX_.jgus[xy-One]-
							  One] == 852 || savtyX_.subchg[xy-One][jbgusX_.jgus[xy-One]-
							  One] == 858) || savtyX_.subchg[xy-One][jbgusX_.jgus[xy-One]-
							  One] == 861) ){
								/*+                                                        *R1718*GBA */
								if( !((prm3 == 845 && savtyX_.subchg[xy-One][jbgusX_.jgus[xy-One]-
								  One] == 846) && swX_.swork[xy-One][4*jbgusX_.jgus[xy-One]-
								  One] == 13) ){
									/*-                                                        *R1718*GBA
									 *+                                                        *RT001*GBA
									 *-                                                        *RT001*GBA */
									if( prm3 != 855 )
										savtyX_.subchg[xy-One][jbgusX_.jgus[xy-One]-
										  One] = prm3;
									}
								}
							}
						else{

							chgsemX_.semchg[ptrm81-One][2-One] = prm3;
							}
						}
					/*-                                               04/02/86  *R1510BT
					 *-                                                         *R1182MBS */


L_1940:
					if( prm4 != 0 ){
						if( prm4 > 0 ){

							chgsemX_.semchg[ptrm81-One][3-One] = prm4;

							}
						else{

							xy = ikm81 - prm4;
							frptr2 = (4*jbgusX_.jgus[xy-One]) + 2;
							chgsemX_.semchg[ptrm81-One][3-One] = swX_.swork[xy-One][frptr2-One];
							}
						}
					}
				else{

					/*-                                                         *R1248MBS */

					if( prm2 != 0 ){
						if( prm2 <= 0 ){

							xy = ikm81 - prm2;
							wcptr2 = 4*jbgusX_.jgus[xy-One];

							if( xx == miniX_.i4pli && negmtcX_.no2res != 
							  0 ){

								/*   THIS IS THE LAST ELEMENT OF THE MATCH.  CHECK TO SEE
								 *   IF A MATCH ON -9 OR -3 HAS LEFT THE NODES OPEN.  IF
								 *   IT HAS, CHANGE ALL THE OPEN NODES. */


								/*                                                          *J0195 */
								for( m=1; m <= 3; m++ ){
									/*                                                          *J0195 */
									if( negmtcX_.negmtc[m-One] != 
									  0 ){
										z = 4*m;
										swX_.swork[xx-One][z-One] = swX_.swork[xy-One][wcptr2-One];
										}
									}
								}
							else{
								swX_.swork[xx-One][wcptr-One] = swX_.swork[xy-One][wcptr2-One];
								}

							}
						else if( xx == miniX_.i4pli && negmtcX_.no2res != 
						  0 ){


							for( m=1; m <= 3; m++ ){
								if( negmtcX_.negmtc[m-One] != 0 ){
									z = 4*m;
									swX_.swork[xx-One][z-One] = prm2;
									}
								}
							}
						else{

							swX_.swork[xx-One][wcptr-One] = prm2;
							}
						}


					if( prm3 != 0 ){
						if( prm3 <= 0 ){

							xy = ikm81 - prm3;
							typtr2 = (4*jbgusX_.jgus[xy-One]) + 1;
							swX_.swork[xx-One][typtr-One] = swX_.swork[xy-One][typtr2-One];
							/*+                                          *R1366JGB
							 *      TRANSFER SUBCHG AND OFL ARRAYS FROM EL TO EL ALSO */
							savtyX_.subchg[xx-One][jbgusX_.jgus[xx-One]-
							  One] = savtyX_.subchg[xy-One][jbgusX_.jgus[xy-One]-
							  One];
							ofltagX_.ofl1r[xx-One][jbgusX_.jgus[xx-One]-
							  One] = ofltagX_.ofl1r[xy-One][jbgusX_.jgus[xy-One]-
							  One];
							ofltagX_.ofl4r[xx-One][jbgusX_.jgus[xx-One]-
							  One] = ofltagX_.ofl4r[xy-One][jbgusX_.jgus[xy-One]-
							  One];
							/*-                                          *R1366JGB */

							/*                                                          *J0195 */
							}
						else if( prm3 >= 844 && prm3 <= 866 ){

							/*+                                                         *R1182MBS */
							if( resswX_.ressw == 1 ){
								if( prm3 >= 862 ){
									/*+ 89/12/14                                                 *R1970*GBA */
									if( prm3 == 862 ){
										temp = savtyX_.subchg[xx-One][jbgusX_.jgus[xx-One]-
										  One];
										if( (((temp == 850 || temp == 
										  852) || temp == 858) || 
										  temp == 861) || temp == 
										  864 )
											savtyX_.subchg[xx-One][jbgusX_.jgus[xx-One]-
											  One] = prm3;
										}
									/*- 89/12/14                                                 *R1970*GBA */
									if( !(savtyX_.subchg[xx-One][jbgusX_.jgus[xx-One]-
									  One] < 844 || savtyX_.subchg[xx-One][jbgusX_.jgus[xx-One]-
									  One] > 848) ){
										if( prm3 == 862 && savtyX_.subchg[xx-One][jbgusX_.jgus[xx-One]-
										  One] == 844 )
											savtyX_.subchg[xx-One][jbgusX_.jgus[xx-One]-
											  One] = 846;
										goto L_2260;
										}
									}
								}

							/*+                                               04/02/86  *R1510BT
							 *  DON'T ALTER SUBCHG IF IT ALREADY HAS A VALUE */
							if( !((savtyX_.subchg[xx-One][jbgusX_.jgus[xx-One]-
							  One] == 852 || savtyX_.subchg[xx-One][jbgusX_.jgus[xx-One]-
							  One] == 858) || savtyX_.subchg[xx-One][jbgusX_.jgus[xx-One]-
							  One] == 861) ){
								/*+                                                        *R1718*GBA */
								if( !((prm3 == 845 && savtyX_.subchg[xx-One][jbgusX_.jgus[xx-One]-
								  One] == 846) && swX_.swork[xx-One][4*jbgusX_.jgus[xx-One]-
								  One] == 13) ){
									/*-                                                        *R1718*GBA
									 *+                                                        *RT001*GBA
									 *-                                                        *RT001*GBA */
									if( prm3 != 855 )
										savtyX_.subchg[xx-One][jbgusX_.jgus[xx-One]-
										  One] = prm3;
									}
								}
							}
						else{
							/*+                                                      *R1428JGB
							 *        NORMAL -46 SWITCH TYPE CHANGE ALGORITHM
							 *        PRM3 VALUE DICTATES WHICH ARRAY(S) CHANGE */

							if( prm3 >= 100 && prm3 <= 998 )
								ofltagX_.ofl1r[xx-One][jbgusX_.jgus[xx-One]-
								  One] = prm3;
							if( prm3 >= 17 && prm3 <= 99 ){
								swX_.swork[xx-One][typtr-One] = prm3;
								if( ofltagX_.ofl1r[xx-One][jbgusX_.jgus[xx-One]-
								  One] <= 99 )
									ofltagX_.ofl1r[xx-One][jbgusX_.jgus[xx-One]-
									  One] = prm3;
								}
							if( prm3 >= 1 && prm3 <= 16 ){
								ofltagX_.ofl4r[xx-One][jbgusX_.jgus[xx-One]-
								  One] = prm3;
								if( swX_.swork[xx-One][typtr-One] <= 
								  16 )
									swX_.swork[xx-One][typtr-One] = prm3;
								if( ofltagX_.ofl1r[xx-One][jbgusX_.jgus[xx-One]-
								  One] <= 16 )
									ofltagX_.ofl1r[xx-One][jbgusX_.jgus[xx-One]-
									  One] = prm3;
								/*-                                                       *R1428JGB */


								/*                                                          *J0195 */
								}
							}
						}
					/*-                                               04/02/86  *R1510BT
					 *-                                                         *R1182MBS */


L_2260:
					if( prm4 != 0 ){
						if( prm4 > 0 ){

							swX_.swork[xx-One][frptr-One] = prm4;
							}
						else{

							xy = ikm81 - prm4;
							frptr2 = (4*jbgusX_.jgus[xy-One]) + 2;
							swX_.swork[xx-One][frptr-One] = swX_.swork[xy-One][frptr2-One];
							}
						}
					}
				}
			else if( negmtcX_.no2res == 9 ){

				ms1 = 0;
				twelve = 0;
				twent1 = 0;
				thirt1 = 0;
				/*+                                                         *R1038MBS */
				sixty = 0;
				sixt1 = 0;
				/*-                                                         *R1038MBS */

				for( ms=4; ms <= 12; ms += 4 ){
					ms1 += 1;
					wc = swX_.swork[miniX_.i4pli-One][ms-One];
					ty = swX_.swork[miniX_.i4pli-One][ms+1-One];
					if( wc != 0 ){
						if( negmtcX_.negmtc[ms1-One] != 0 ){

							/*+                                                         *R1038MBS */
							if( wc != 2 && wc != 12 )
								goto L_2320;
							if( ty != 60 && ty != 61 ){
								/*-                                                         *R1038MBS */
								if( wc == 2 ){
									if( ty != 21 && ty != 31 )
										goto L_2320;
									if( ty == 21 )
										twent1 = ms1;
									if( ty == 31 )
										thirt1 = ms1;

									}
								else if( wc == 12 ){
									twelve = ms1;
									}
								else{
									goto L_2320;
									}
								}
							else if( sixty == 0 ){
								sixty = ms1;
								}
							else{
								sixt1 = ms1;
								}
							}
						}

					}
				/*+                                               04/02/86  *R1510BT
				 *  DON'T ALTER SUBCHG IF IT ALREADY HAS A VALUE */
				if( !((savtyX_.subchg[miniX_.i4pli-One][ms1-One] == 
				  852 || savtyX_.subchg[miniX_.i4pli-One][ms1-One] == 
				  858) || savtyX_.subchg[miniX_.i4pli-One][ms1-One] == 
				  861) ){
					/*-                                               04/02/86  *R1510BT
					 *+                                                         *R1038MBS */
					if( sixty != 0 )
						savtyX_.subchg[miniX_.i4pli-One][sixty-One] = 847;
					if( sixt1 != 0 )
						savtyX_.subchg[miniX_.i4pli-One][sixt1-One] = 847;
					/*-                                                         *R1038MBS */

					if( twent1 != 0 ){
						savtyX_.subchg[miniX_.i4pli-One][twent1-One] = 844;
						if( thirt1 == 0 && twelve == 0 )
							savtyX_.subchg[miniX_.i4pli-One][twent1-One] = 846;
						}

					if( thirt1 != 0 ){
						savtyX_.subchg[miniX_.i4pli-One][thirt1-One] = 845;
						if( twent1 == 0 && twelve == 0 )
							savtyX_.subchg[miniX_.i4pli-One][thirt1-One] = 847;
						}

					if( twelve != 0 ){
						if( twent1 == 0 && thirt1 == 0 )
							savtyX_.subchg[miniX_.i4pli-One][twelve-One] = 848;
						}
					}
				}
			/*- */

L_2320:
			commvX_.vn += 5;
			goto L_2380;
			}
		}
	/************************************************************************
	 *       ERROR WRITE
	 ************************************************************************ */

L_2400:
	if( opswX_.sw[8-One] == 1 )
		{
		fprintf( _spec_fp, "--ERROR IN RESWS    ERR,LOCFLG,RETSW = %8ld%8ld%8d\n", 
		  errvrsX_.err, locflg, retsw );
		}
	*retflg = -1;


L_2380:
	x = x;
	return;

} /*end of function*/

