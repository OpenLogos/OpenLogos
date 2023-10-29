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
	/** LAST CHANGE 87/04/24 LG002GBA RGA07:  ADD LOGIC FOR 6300 ARRAY */
	/*CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	 *     THIS PROGRAM IS CALLED FROM RES1AND2.
	 *      IT SEARCHES RES2X FOR A MATCH
	 *      IF ONE IS FOUND, THE INFORMATION IS SAVED
	 *      ENTRY WHCRES IS CALLED AFTER THE RES2 MATCH IS MADE TO
	 *      DETERMINE WHICH RES MATCH IS BETTER, RES2 OR RES2X  
	 *      LOADED IN ALL TRANSLATION EXECS, AND TESTRES EXEC */
	/*   THESE COMMONS ARE DUPLICATED IN THE MAIN ROUTINE
	 *    THEY CONTAIN THE INFORMATION WHICH MUST BE SAVED FROM THE
	 *    RULE THAT IS MATCHED */
	/*added to /negmtc/ to fix size,cwl,2/5/98
	 *   MORE COMMONS ADDED FOR MINI ROUTINE */

#include "rescommon.h"


void /*FUNCTION*/ res2x(locflg, retflg)
long int locflg;
short int *retflg;
{
	struct {
		short int no2res, negmtc[3];
	}	negmtxX_;
	static struct t_minixX_ {
		short int k12s, i4pli, iksave, i4sav, lvlgus, jgusav[70];
	}	minixX_;
	static short int xvtrfs[100];
	static char pgmnam[9] = "RES2X   ";
	static short zero = 0;
	static short m = 0;
	static short x = 0;
	static short ii = 0;
	static short iz = 0;
	static short kk = 0;
	static short mm = 0;
	static short wc = 0;
	static short iis = 0;
	static short li2 = 0;
	static short sub = 0;
	static short iist = 0;
	static short nwc2 = 0;
	static short kksav = 0;
	static short oflad = 0;
	static short retsw = 0;
	static short swcps = 0;
	static short resnum = 0;
	static short displm = 0;
	static short origli = 0;
	static short specf2 = 0;
	static short specif = 0;
	static short specix = 0;
	static short xrule[13];
	static short xoverf[21];

	*retflg = 0;

	/*     If Mini option has not been specified then exit */
	if( jbctrlX_.jcmnrs[2-One] != 1 ){
		return;
	}

		if( locflg == 2 ){

			/*                                 WAS THERE A RES2X MATCH ? */
			if( valxX_.match == 0 )
				goto L_7000;
			/*                                 WAS THERE A RES2 MATCH ? */
			if( miniX_.k12s != 0 ){


				resnum = 4;
//				errvrsX_.errlvl = RULEIN(&resnum, resrulX_.rule, &minixX_.k12s);
				resrulX_.rule = RULEIN_OCR(resnum, minixX_.k12s);
				errvrsX_.errlvl = resrulX_.rule ? 0:1;
				if( errvrsX_.errlvl != 0 ){
					goto L_8000;
				}
				commlX_.li = resrulX_.rule[1-One];
				specif = resrulX_.rule[12-One];

				resnum = 2;
//				errvrsX_.errlvl = RULEIN(&resnum, resrulX_.rule, &miniX_.k12s);
				resrulX_.rule = RULEIN_OCR(resnum, miniX_.k12s);
				errvrsX_.errlvl = resrulX_.rule ? 0:1;
				if( errvrsX_.errlvl != 0 ){
					goto L_8000;
				}
				li2 = resrulX_.rule[1-One];
				specf2 = resrulX_.rule[12-One];

				if( li2 <= commlX_.li ){
					if( !(li2 == commlX_.li && specf2 > specif) )
						goto L_2100;
					}


				if( opswX_.sw[8-One] == 1 )
					{
					fprintf( _spec_fp, "   ***   RES2 RULE WINS   ***\n" );
					}

				goto L_7000;
				}



			/*        RES2X RULE WINS */
L_2100:
			memcpy(resrulX_.rule,xrule,sizeof(resrulX_.rule));
			memcpy(resrulX_.overfl,xoverf,sizeof(resrulX_.overfl));
			memcpy(vtrfsX_.vtrfs,xvtrfs,sizeof(vtrfsX_.vtrfs));
			lmove(&miniX_.k12s,1,&minixX_.k12s,1,150);
			lmove(&negmtcX_.no2res,1,&negmtxX_.no2res,1,8);
			resswX_.minisw = 1;

			if( opswX_.sw[8-One] == 1 )
				{
				fprintf( _spec_fp, "   ***  MINI RULE WINS  ***\n" );
				}
			goto L_7000;
			}
		else{

			minixX_.lvlgus = 1;
			iist = 1;
			specif = -1;
			minixX_.k12s = 0;
			ovptrX_.ovptr = 1;

			while( TRUE ){
				ikX_.ik = valxX_.i4x;
				if( ikX_.ik >= swX_.ecount )
					goto L_1750;

				mtcinfX_.iks = ikX_.ik;


				/*     TEST FOR POSITIVE VALUES IN FIRST 3 POSITIONS OF EACH SWORK */

				for( ii=iist; ii <= 3; ii++ ){
					iis = ii;
					mtcinfX_.swcp = 4*ii;
					swcps = mtcinfX_.swcp;
					if( swX_.swork[mtcinfX_.iks-One][ii-One] > 0 )
						goto L_680;
					}
				break;

L_680:
				wc = swX_.swork[mtcinfX_.iks-One][mtcinfX_.swcp-One];


				nwc2 = NENTX(&wc);
				displm = DISSPX(&wc) + nwc2;


				kksav = 1;

				if( nwc2 > 0 ){

					if( kksav <= nwc2 ){


						/*   FIRST OF 3 LOOPS IN THE MATCHING PROCESS.
						 *    LOOPS THROUGH ALL THE RULES IN THE PRESENT WORD
						 *    CLASS, LOOKING FOR THE MOST SPECIFIC MATCH. */

						for( kk=kksav; kk <= nwc2; kk++ ){
							mtcinfX_.jx = 1;
							commkX_.k12 = displm - kk;

							for( mm=1; mm <= 6; mm++ ){
								if( commkX_.k12 == nloopX_.nloop2[mm-One] )
									goto L_8101;
								}

							resnum = 4;
//				errvrsX_.errlvl = RULEIN(&resnum, resrulX_.rule, &commkX_.k12);
				resrulX_.rule = RULEIN_OCR(resnum, commkX_.k12);
				errvrsX_.errlvl = resrulX_.rule ? 0:1;

							if( errvrsX_.errlvl == 0 ){
								commlX_.li = resrulX_.rule[1-One];
								oflad = resrulX_.rule[11-One];
								specix = resrulX_.rule[12-One];
								/*       RES2 IS ALLOWED A LEVEL OF UP TO 40 (ACTUAL LEVEL+10,20..) */
								origli = commlX_.li;
								if( commlX_.li > 10 ){
									sub = ((commlX_.li - 1)/10)*10;
									commlX_.li -= sub;
									}
								if( commlX_.li <= (swX_.ecount - ikX_.ik + 1) ){

									/*   IF THE LEVEL OF THIS RULE IS GREATER THAN A PREVIOUS MATCH,
									 *   IGNORE THE SPECIFICITY TEST
									 *   IF IT IS LESS, WE ARE DONE TRYING FOR A MATCH ON THIS NODE */
									if( origli <= minixX_.lvlgus ){
										if( origli < minixX_.lvlgus )
											goto L_1640;


										/*   IF A PREVIOUS RULE MATCHED ON IS MORE SPECIFIC THAN THIS
										 *   ONE, SKIP THIS ONE. */

										if( specif >= specix )
											continue;
										}

									mtcinfX_.iks = ikX_.ik;
									negnodX_.no3res = 0;
									zapit(negnodX_.negnod,6,(byte)0);


									/*          BRING IN THE OVERFLOW */
									if( oflad != 0 ){
										resnum = 4;
								errvrsX_.errlvl = OVRIN(&resnum, resrulX_.overfl, &commkX_.k12, &oflad);
										if( errvrsX_.errlvl != 0 ){
											errvrsX_.errlvl = 0;
											continue;
											}
										}




									/*   CALL RESMTC TO SEE IF THIS RULE IS A MATCH
									 *     RETSW = 0 MEANS A MATCH ON THIS RULE
									 *     RETSW = 1 MEANS NO MATCH ON THIS RULE */

									memset(tag63X_.tag63,'\0',sizeof(tag63X_.tag63));
									resmtc(&retsw, commkX_.k12);
									if( retsw != 1 ){

										if( tag63X_.tag63[1-One] !=  0 ){
											resnum = 4;
										    rsdiag(2,&resnum,&valxX_.i4x,&commkX_.k12);
											 
											tag63X_.srch63 = 1;
											srch07X_.srch07 = 0;
											rs6300(&retsw);
											tag63X_.srch63 = 0;
											srch07X_.srch07 = 0;
											if( retsw == 0 )
												goto L_1680;
											}

										/*   IS THIS AN OVERRIDE RULE? */
										if( vtrfX_.vtrf[1-One] == 997 ){

											resnum = 4;
										    rsdiag(3,&resnum,&valxX_.i4x,&commkX_.k12);

											memcpy(overidX_.rulovr[ovptrX_.ovptr-One],resrulX_.rule,sizeof(overidX_.rulovr[0]));
											if( commlX_.li > 3 )
												memcpy(overidX_.ovrovr[ovptrX_.ovptr-One],resrulX_.overfl,42);
											ovptrX_.ovptr += 1;
										 
											}
										else{

											/*   THIS IS REALLY A MATCH ‹‹‹‹
											 *   IF NO3RES WAS SET FOR THIS MATCH - LEAVE NEGNOD FLAGS ON
											 *   OTHERWISE, TURN THEM OFF */

											if( negnodX_.no3res == 0 ){
												negmtxX_.no2res = 0;
												zapit(negmtxX_.negmtc,6,(byte)0);
												}

											if( negnodX_.no3res != 0 ){
												negmtxX_.no2res = negnodX_.no3res;
												lmove(negmtxX_.negmtc,1,negnodX_.negnod,1,6);
												zapit(negnodX_.negnod,6,(byte)0);
												}

											/*   SAVE THIS VTR (EXCEPT FOR THE -46 SWITCHES PRECEDING THE -22
											 *   IN VTRFS */
											memcpy(vtrfsX_.vtrfs,vtrfX_.vtrf,sizeof(vtrfsX_.vtrfs));
											iz = sw22ptX_.sw22pt;
											if( sw22ptX_.sw22pt == 0 )
												iz = 1;


											resnum = 4;
										    rsdiag(1,&resnum,&valxX_.i4x,&commkX_.k12);


											minixX_.k12s = commkX_.k12;
											minixX_.i4pli = valxX_.i4x + commlX_.li - 1;
											minixX_.i4sav = valxX_.i4x;
											minixX_.iksave = mtcinfX_.iks - 1;

											for( iz=valxX_.i4x; iz <= minixX_.i4pli; iz++ ){
												minixX_.jgusav[iz-One] = jbgusX_.jgus[iz-One];

												}

											/*   KEEP LOOKING FOR A MORE SPECIFIC RULE. */

											minixX_.lvlgus = origli;
											specif = specix;
											}
										}

L_1680:
									mtcinfX_.swcp = swcps;
									}
								}
							else{
								errvrsX_.errlvl = 0;
								}
L_8101:
							;
							}
						goto L_1720;


L_1640:
						if( iis >= 3 )
							break;
						iist = iis + 1;
						valxX_.i4x = ikX_.ik;
						continue;
						}
					}

L_1720:
				if( iis >= 3 )
					break;
				iist = iis + 1;
				valxX_.i4x = ikX_.ik;
				}


			if( minixX_.k12s != 0 ){

				memcpy(xrule,resrulX_.rule,sizeof(xrule));
				memcpy(xoverf,resrulX_.overfl,sizeof(xoverf));
				memcpy(xvtrfs,vtrfsX_.vtrfs,sizeof(xvtrfs));
				commkX_.k12 = minixX_.k12s;
				valxX_.match = 1;
				goto L_7000;
				}


L_1750:
			valxX_.match = 0;
			miniX_.k12s = 0;
			goto L_7000;
			}


L_8000:
		if( opswX_.sw[3-One] == 1 || opswX_.sw[8-One] == 1 )
			{
			fprintf( _spec_fp, "--ERROR IN RES2X    ERR,LOCFLG,RETSW = %8ld%8ld%8d\n", 
			  errvrsX_.err, locflg, retsw );
			}
		*retflg = -1;

L_7000:
		x = x;
	return;

} /*end of function*/

