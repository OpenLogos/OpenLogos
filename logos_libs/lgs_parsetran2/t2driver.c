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
	 *   03/08/94 jal: TMNP facility. Call NPSLCT() at beginning of each
	 *                 sentence to select NP from input SWORKS.
	 *   03/08/94 jal: link SWORKO to consituents in SWORKI via SWKLNK().
	 *     10/08/92 jal: increase size of IMATCH to ELMMAX.
	 *     01/20/92 JAL: ADD ABILITY TO EXECUTE ONLY 1 OF 2 PASSES WITH
	 *          2 PASS STRATEGY ACTIVE (KEYED OFF PASSKP).  EITHER LOOP
	 *          BACK AFTER 1ST INIT IF PASS2 ONLY OR JUST DONT READ AND
	 *          INIT A 2ND TIME IF PASS1 ONLY. CHANGES TO TXWRITE, TXMOPUP
	 *          AND THE FILE OPEN AND LOAD ROUTINES.
	 *     10/28/91 JAL: IF NO MATCH ON AN SWORK  EVEN IN DEFAULT RULES
	 *          THEN FORCE A LOAD OF THE SWORK AND CONTINUE.
	 *      09/05/91 JAL: CLUDGY, PRECARIOUS CLAUSE FIX.
	 *          DUMMY SWORKS CREATED IN TRAN1 HAVE THE
	 *          SAME SCONPTR AS THE ORIGINAL SWORKS THEY WERE CREATED FROM.
	 *          ENGSRC CREATES A DUMMY SWORK FROM THE ANTECEDENT TO REPRESENT
	 *          A MISSING REL PRONOUN.  THE DUMMY MOVES W/ THE CLAUSE.
	 *          CLSID IS ACCESSED OFF SCONPTR SO ANTECEDENT CLSID = DUMMY
	 *          CLSID = MOVED CLSID EVEN THOUGH ANTECEDENT IS IN THE MAIN
	 *          CLAUSE.  RESULT IS CLBGNS(ANTECEDENT CLAUSE) IS WRONG. THE
	 *          FIX  RECOGNIZES THESE ANTECEDENTS TO BE UNIQUELY SURROUNDED
	 *          BY ELEMENTS WITH THE SAME CLAUSEID. */
	/*  CHANGES:
	 *      09/12/91 JAL: CHANGE PROCESSING OF ENTERING A NEW CLAUSE.
	 *      09/15/89 JAL CLAUSE WORK
	 *      87/06/09 LG002GBA RPHS3:
	 *      04/22/87 *R1685RKH*  CHANGE T1-4 SWORK LIM FROM 50 TO 70
	 *      12/30/86  PR304050: TARGET VTR TABLES
	 *      08/24/86 *R1561DSD: 100 SCONS
	 *      07/24/86 *B0415DSD: COMPRESS "* OUTPUT" FILES
	 *      04/22/86 18:14 *B0395DSD - AFTER VTRPRO, K7M CAN BE ANY
	 *             POSITIVE RULE NO., NOT 1 (THAT VALUE IS ONLY IN WHICSP AND
	 *             PREVTR).  TEST K7M <> 0 IN DECIDING LOOP TEST
	 *      11/26/85 15:25 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <configdatafileinterface/configdatainterfacemain.h>
#define MS_F77
#include <fcrt.h>
#include "project.h"
#include "logoslib.h"
#include "parsetrans.h"
#include "trans.h"
#include "projexts.h"
#include <string.h>
#include <jbctrl.h>
//#include <logos_libs/dbms2restran/gettargetcodes.h>

void addMemoryState(const char* memTag);
void startBlockMemoryLog(void);
void endBlockMemoryLog(void);

#ifdef TRANSTAT
#include <fcntl.h>
int rulestatfile;
int rulestat[20000];
char *rulestatfilename = "e:\\esense\\bin\\rulestat2";
#endif /* TRANSTAT */

int t2driver(void)
{
	static short int bg, endns, jctmnp, junk, locflg, lrgjob, nd, 
	  opobg, opond, retflg, retsw, srcflg, tempid, tmpchl, wc50sv, 
	  wc50to, ww, x, yy, zz;
	static long int _l0, retcod;
	static short zero = 0;
	static char modnam[9] = "TRAN2   ";
	static char pgmnam[9] = "T2DRIVER";
	static short iz = 0;
	static short kz = 0;
	static short xx = 0;
	static short lvl = 0;
	char pgmName[6];
   char lpszBuf[MAX_FILEPATH_LEN]; 

	initializeTrObjects(); // Initialize Database objects
	tranidX_.tranid = 2;
	lrgjob = 1;
	errvrsX_.untcnt = 0;
	sentctX_.sentct = 0;
	strcpy(errvrsX_.mod, "TRAN2   ");

	getProgramName(passesX_.passfl, passesX_.passct, 2, pgmName);
	errvrsX_.err = jcloadc(pgmName);

	if (errvrsX_.err == 0)
   {
		if (passesX_.passfl == 1 && passesX_.passct == 1)
      {
         GetConfigData("tempfile", "parse2_diag", lpszBuf, MAX_FILEPATH_LEN);
         _spec_fp = fopen(lpszBuf, "w");
		}
	   else
      {
         GetConfigData("tempfile", "tran2_diag", lpszBuf, MAX_FILEPATH_LEN);
         _spec_fp = fopen(lpszBuf, "w"); 
      }

#ifdef TRANSTAT
	init_rulestat();
#endif /* TRANSTAT */

		blk_data();


		/*      Open the FORMTRAN file and unload it's content into memory:
		 *          Records 1-3:  The bit maps for FORMOD matching.
		 *          Record 4:     The OVERFLOW 3B Exception Table. */
		formod(1,ww,xx,yy,&zz);
		if( errvrsX_.errlvl == 0 ){

			/*     Read into memory the OFL3B exception table.  It is in the
			 *     fourth record of the FORMTRAN file. */
			o3btab(junk,junk,junk,junk,(long*)&retflg,1);
			if( retflg != 0 ){
				errvrsX_.errlvl = 1;
				}

					/* 			Load in the tran rules */
//cat-102			else if( rule_load((short *)ADR(_l0,2),&miniflX_.minifl) == 0 ){
			else if( rule_load(2, miniflX_.minifl,
						passesX_.passfl, passesX_.passct) == 0 ){

				/*+               set Translation memory NP flag from JBCTRL 3/08/94 jal */
				if( jctmnp == 1 ){
					tmnphrX_.tmnpfl = 1;
					}
				else{
					tmnphrX_.tmnpfl = 0;
					}


				/*                      GER SRC REPLACE DATAED NEGWC2
				 *                      (ENG SRC) WITH GERMAN VERSION */
				if( srcflgX_.srcflg == 1 )
					lmove((short*)neg2X_.negwc2,1,(short*)gneg2X_.gngwc2,1,144);

				/*					open the input data comming from previous component */
				if( tran2_io(ADR(_l0,0)) != 0 ){
					errvrsX_.errlvl = 1;
					/*					open the output written by this component */
					}
				else if( tran2_io(ADR(_l0,10)) == 0 ){
					goto L_5;
					}
				else{
					errvrsX_.errlvl = 1;
					}
				}
			else{
				errvrsX_.err = -1;
				}
			}
		}
	else{
		fprintf( stdout, "Error read job information\n" );
		errvrsX_.errlvl = 1;
		}
	/*cccccccccccccccccccccccccccccccccccccc
	 *				Error out! */
L_9999:
	x = x;
	/*				check if error allow us to continue */
	if( !(errvrsX_.errlvl < 1 || errvrsX_.errlvl > 5) )
		goto L_10000;
	fprintf( _spec_fp, "ERROR: Sentence is being skipped\n" );
	fprintf( _spec_fp, "%5ld %3ld", source_sentX_.sentid, source_sentX_.sentlng );
	fprintf( _spec_fp, " " );
	for( x=1; x <= 300; x++ ){
		fprintf( _spec_fp, "%c", source_sentX_.sentbuf[x-One] );
		}
	fprintf( _spec_fp, "\n" );

L_9905:
	x = x;
	if( diagsX_.anydi == 1 ){
		fprintf( _spec_fp, "*EOS*\n" );
		}







	/* ----------------------- NEXT SENTENCE START ------------------------- */

	/*   COME BACK HERE AFTER EACH SENTENCE HAS BEEN PROCESSED AND READ THE
	 *   INPUT FILES (I.E. RES OUTPUT) FOR THE NEXT SENTENCE AND PUT THEM IN
	 *   COMMON.
	 *   UNTCNT IS SENTENCE COUNT. */

L_5:
	;
	sentctX_.sentct += 1;
	errvrsX_.untcnt = sentctX_.sentct;
	/*                              READ IN NEW SENT */

	retsw = tran2_io(ADR(_l0,1));
	if( retsw != 0 )
		goto L_10001;

	diag_check(3);

/*usefull code for debugging
	printf("%8d %4d\n", source_sentX_.sentid, source_sentX_.sentlng );
	if (source_sentX_.sentid == 19)
	{
		x = x;
	}
*/


	/* ---------------------  DO WE WANT TO BYPASS TRAN2? ------------------ */

	/*        BYPAS ACTIVE IF TRAN1 OUTPUT IS ONLY 3 SWORKS:  BOS  WC1  EOS
	 *        MOVE INCOMING COMMONS TO OUTGOING COMMONS, JUMP TO T2WRITE() */

	/*        (OPADRI,OPI,SCONPI)  TO   (OPADRO,OPO,SCONPO)
	 *        (HFDOPI)            TO   (HFDOPO) */

	if( bypasX_.bypas == 0 ){

		/* ---------------------- INITIALIZATION OF VARIABLES ------------------ */

		/*    RESET ALL THE FLAGS, ARRAYS, ETC. THAT MIGHT HAVE BEEN SET BY THE
		 *    LAST SENTENCE. */

		init();
		/*+               Translation Memory NP selection on input 03/08/94 jal */
		if( tmnphrX_.tmnpfl == 1 )
			npslct(1,sworkX_.swork,sworkX_.phrbeg,sworkX_.phrend,sworkX_.phct,
			  opadriX_.opadri,opadriX_.sconpi,hpdopiX_.hfdopi,&junk);
		/*-                                                         03/08/94 jal */

		while( TRUE ){
			if( sworkoX_.phcto > 0 ){
				sworkoX_.phrndo[sworkoX_.phcto-One] = opadroX_.opo;
				/*+1                link SWORKO to consituents in SWORKI 3/08/94 jal */
				swklnkX_.swklnk[sworkoX_.phcto-One] = flowckX_.i3save - 
				  1;
				}
			sworkoX_.phcto += 1;
			flowckX_.strphr = opadroX_.opo + 1;
			sw25bkX_.sw25n = 0;
			sw21bkX_.sw21n = 0;
			sw25bkX_.tw25 = 0;
			sworkoX_.phrhdo[sworkoX_.phcto-One] = 0;
			sworkoX_.phrbgo[sworkoX_.phcto-One] = opadroX_.opo + 1;

			/*        PHSUP=1 SUPPRESSES NEW PHRBEG */
			while( TRUE ){
				phsupX_.phsup = 0;
				cnX_.vtrdon = 0;
				sw18bkX_.phrnew = 0;
				memset(semtrX_.sem46,'\0',sizeof(semtrX_.sem46));

				/*        FOR SW11 PHRBEG SUPRESSION CONTROL */
				phsupX_.n3sv = opadroX_.opo;

				/*        DIAGNOSTICS */

				if( opswX_.sw[15-One] == 1 ){
					fprintf( _spec_fp, " THE  PHCTO LI I3 IN TRAN2 %4d%4d%4d%4d%4d%4d%4d\n", 
					  sworkoX_.phcto, sploopX_.li, w50valX_.i3, flowckX_.i3save, 
					  vtrs42X_.sw42n, opadroX_.opo, flowckX_.strphr );
				diag_write_shorts("OPADRO\n",
								opadroX_.opadro, opadroX_.opo, 20, 6,
								"");
				diag_write_shorts("SCONPO\n",
								opadroX_.sconpo, opadroX_.opo, 20, 6,
								"");
				diag_write_shorts("HFDOPO\n",
								hpdopoX_.hfdopo, opadroX_.opo, 20, 6,
								"");
					}

				/*         ************* START OF NEW PHRBEG ************ */

				flowckX_.i3str = flowckX_.i3save;
				/*        ISAVE WILL BE USED AT 800 TO CHECK FOR LOOPING OF RULES */
				vwarg1X_.isave = vwarg2X_.i;
				flowckX_.i3save = 0;
				semargX_.pntr9 = 0;
				vtrs42X_.sw42n = 0;
				vwarg2X_.wc42m = 0;
				minickX_.k7m = 0;
				/*+                                                     *09/12/91*JAL*
				 *+                                                     *09/24/91*JAL* */
				swtc68X_.sw68ct = 0;
				/*-                                                     *09/24/91*JAL* */

				/*        BEGINNING TO PARSE A DIFFERENT CLAUSE?
				 *           CHECK BASED ON CLNDNS() - ENDING INPUT SWORK OF CLAUSE
				 *           NOTE, WHILE IN MOVE STATE WE ARE INT THE CHILD CLAUSE
				 *           BEING DELIMITED NOT THE PARENT CLAUSE. */

				if( flowckX_.i3str > clsnfoX_.clndns[clsnfoX_.clcrnt-One] ){
					tempid = clsconX_.clsid[sconX_.scolnk[sworkX_.swork[flowckX_.i3str-One][4-One]-
					  One]-One];

					/*                       CHECK FOR ERROR FIRST */
					if( tempid == clsnfoX_.clcrnt ){
						if( diagsX_.anydi == 1 )
							{
							fprintf( _spec_fp, "\n** ERROR (T2DRIVER): BAD CLAUSE END POINTER\n   I3STR,CLCRNT,CLNDNS(CLCRNT),TEMPID =%3d  %3d  %3d  %3d  \n", 
							  flowckX_.i3str, clsnfoX_.clcrnt, clsnfoX_.clndns[clsnfoX_.clcrnt-One], 
							  tempid );
							}
						errlog(pgmnam,300,0,10);
						clsnfoX_.clndns[clsnfoX_.clcrnt-One] = flowckX_.i3str;

						/*                       IF CLAUSE MOVE STACK NOT EMPTY FORCE A MOVE. */
						}
					else if( clsmovX_.clmcnt == 0 ){
						if( clsnfoX_.clbgns[tempid-One] != flowckX_.i3str ){
							if( diagsX_.anydi == 1 )
								{
								fprintf( _spec_fp, "\n***************  ERROR  T2DRIVER  **************\n  PROBLEM SWITCHING FROM ONE CLAUSE TO NEXT.    \n  I3STR,CLBGNS(TEMPID),CLCRNT,TEMPID =          \n     %4d  %4d  %4d  %4d  \n************************************************\n", 
								  flowckX_.i3str, clsnfoX_.clbgns[tempid-One], 
								  clsnfoX_.clcrnt, tempid );
								}
							errlog(pgmnam,305,0,10);
							}

						/*                       CELLS 11-60 LAST ONE TRAN FOR EACH CLAUSE */
						zapit(&vbdataX_.vbcell[11-One],100,(byte)zero);
						/*                       SET OUTGOING CLAUSE BOUNDARIES */
						clsoutX_.clndno[clsnfoX_.clcrnt-One] = sworkoX_.phcto - 
						  1;
						/*                       SET ANTECEDENT PTRS
						 *                       1ST INIT ALL ANTCDNT SWORK PTRS FOR THIS CLAUSE */
						for( xx=2; xx <= clsnfoX_.cltotl; xx++ ){
							if( clsnfoX_.clprnt[xx-One] == clsnfoX_.clcrnt )
								clsnfoX_.clansw[xx-One] = 0;
							}
						/*                      IF ANT STILL A HEAD IN SWORKO THEN RESET ITS PTRS */
						for( xx=clsoutX_.clbgno[clsnfoX_.clcrnt-One]; xx <= clsoutX_.clndno[clsnfoX_.clcrnt-One]; xx++ ){
							tmpchl = clsconX_.achild[sconX_.scolnk[sworkoX_.sworko[xx-One][4-One]-
							  One]-One];
							if( tmpchl != 0 ){
								clsnfoX_.clansw[tmpchl-One] = xx;
								opobg = sworkoX_.phrbgo[xx-One];
								opond = sworkoX_.phrndo[xx-One];
								clsnfoX_.clanbg[tmpchl-One] = opadroX_.sconpo[opobg-One];
								clsnfoX_.clannd[tmpchl-One] = opadroX_.sconpo[opond-One];
								/*                             SCONPO = 1 MAY BE A DUMMY VALUE */
								if( clsnfoX_.clanbg[tmpchl-One] == 
								  1 && xx != clsnfoX_.clbgns[clsnfoX_.clcrnt-One] ){
									for( bg=opobg; bg <= opond; bg++ ){
										if( opadroX_.sconpo[bg-One] != 
										  1 )
											break;
										}
									opobg = bg;
									clsnfoX_.clanbg[tmpchl-One] = opadroX_.sconpo[opobg-One];
									}
								if( clsnfoX_.clannd[tmpchl-One] == 
								  1 ){
									for( nd=opond; nd >= opobg; nd-- ){
										if( opadroX_.sconpo[nd-One] != 
										  1 )
											break;
										}
									clsnfoX_.clannd[tmpchl-One] = opadroX_.sconpo[nd-One];
									}
								}
							}
						/*                       UPDATE CURRENT CLAUSE PTR AND INIT THE NEW CLAUS */
						clsnfoX_.clcrnt = tempid;
						clsoutX_.clbgno[clsnfoX_.clcrnt-One] = sworkoX_.phcto;
						}
					else{
						/*                        DONT MOVE THE EOS OF THE PARENT IF POSSIBLE. */
						endns = clsnfoX_.clndns[clsnfoX_.clcrnt-One];
						if( (sworkX_.swork[endns-One][1-One] == 20 && 
						  sworkX_.swork[endns-One][2-One] == 10) && 
						  clsnfoX_.clbgns[clsnfoX_.clcrnt-One] < endns )
							clsnfoX_.clndns[clsnfoX_.clcrnt-One] = endns - 
							  1;
						if( diagsX_.anydi == 1 ){
							fprintf( _spec_fp, "\n***************  ERROR  T2DRIVER  **************\n  REACHED END OF PARENT CLAUSE WITHOUT MOVING  \n  THE NESTED CLOSE, I.E. -67 002 IS MISSING.   \n  FORCE A MOVE AND CONTINUE.\n************************************************\n" );
							fprintf( _spec_fp, " SW67 002:   MOVE A CLAUSE.\n\n     *********  BEFORE MOVING THE CLAUSE *********\n\n" );
							diagno(10);
							diagno(9);
							}
						clmove(20,10,1,1);
						if( diagsX_.anydi == 1 ){
							fprintf( _spec_fp, "\n     *********  AFTER MOVING THE CLAUSE ********* \n\n" );
							diagno(10);
							diagno(9);
							}
						errlog(pgmnam,301,0,10);

						/*                       O.K. - MOPUP AND RESET FOR NEW CLAUSEE */
						}
					}
				/*-                                                      *09/12/91*JAL* */

				/* ---------------------------------------------------------------------
				 *     CALL GETSP WILL TAKE CARE OF GETTING THE NEXT SP RULE.
				 *     IT WILL PLACE THE RULE NUMBER IN K7 FOR A MAIN RULE
				 *                               AND IN K7M FOR A MINI RULE */

		getsp_x(&diacb2X_.k7, &diacb2X_.oflad, sp2yX_.ovrflw);
				if( errvrsX_.errlvl != 0 ){
					lvl = (errvrsX_.errlvl + 4)/4;
					errvrsX_.errlvl = 0;
					if( lvl == 1 )
						goto L_9999;
					if( lvl == 2 )
						goto L_9900;
					if( lvl != 3 )
						goto L_9900;
					/*+                                                     *10/28/91*JAL*
					 *              NO MATCH ON THIS SWORK FORCE A LOAD VTR AND CONTINUE */
					}
				else if( diacb2X_.k7 == 0 && minickX_.k7m == 0 ){
					xx = flowckX_.i3str;
					if( vtrs42X_.sw42n == 1 )
						xx = vwarg1X_.isave;
					flowckX_.i3save = xx + 1;
					vtrnX_.vtrn = -1;
					vbdataX_.k3p1 = 0;
					elemld();
					if( errvrsX_.errlvl == 0 ){
						sworkoX_.sworko[sworkoX_.phcto-One][1-One] = sworkX_.swork[xx-One][1-One];
						sworkoX_.sworko[sworkoX_.phcto-One][2-One] = sworkX_.swork[xx-One][2-One];
						sworkoX_.sworko[sworkoX_.phcto-One][3-One] = sworkX_.swork[xx-One][3-One];
						sworkoX_.sworko[sworkoX_.phcto-One][4-One] = sworkX_.swork[xx-One][4-One];
						sworkoX_.phrhdo[sworkoX_.phcto-One] = sworkX_.phrhed[xx-One];
						}
					else{
						lvl = (errvrsX_.errlvl + 4)/4;
						errvrsX_.errlvl = 0;
						if( lvl == 1 )
							goto L_9999;
						if( lvl == 2 )
							goto L_9900;
						if( lvl != 3 )
							goto L_9900;
						}
					}
				else{
					/*-                                                     *10/28/91*JAL* */

					/* ------------------- CHECK FOR SP RULE LOOPING -----------------------
					 *                     DO THIS ONLY FOR THE MAIN SP RULES
					 *                     WC10 RULES START HERE
					 *                     AND NON-MINI WC9 RULES START HERE */

					/*                     NPTP IS UNIQUE VTR POINTER */
					while( TRUE ){
						vtrptrX_.nptp = spX_.sp[12-One];

						/*                     LIMIT MATCHES ON A GIVEN SWORK (I) */
						xx = vwarg2X_.i;
						if( vtrs42X_.sw42n == 1 )
							xx = vwarg1X_.isave;
						loopckX_.imatch[xx-One] += 1;
						if( loopckX_.imatch[xx-One] >= 60 )
							goto L_795;
						/*                     ADD THE TOTAL OF THE POSSIBLE
						 *                     STRETCHES SO THAT WE CAN CHECK TO SEE
						 *                     IF WE ARE LOOPING ON A STRETCH RULE. */
						wc50to = vwarg1X_.chng50[1-One] + (100*vwarg1X_.chng50[2-One]) + 
						  (1000*vwarg1X_.chng50[3-One]);
						/*                     IF VTR ARE DIFFERENT, NO LOOP HERE. */
						if( vtrptrX_.nptp == loopckX_.nptpsv ){
							/*                     IF PREVIOUS RULE WAS FROM MINI, SKIP IT */
							if( minickX_.minifg == 0 ){
								if( minickX_.minilp == 0 ){
									/*                     ISAVE SET BY SW42, DON'T KNOW WHY WE SKIP. */
									if( vwarg2X_.i == vwarg1X_.isave ){
										/*                     SAME RULE, BUT THE STETCHING IS DIFFERENT, SKIP. */
										if( !(vwarg2X_.wc50m != 
										  0 && wc50to != wc50sv) )
											goto L_10002;
										}
									}
								}
							}
						loopckX_.nptpsv = vtrptrX_.nptp;
						wc50sv = wc50to;
						/* --------------------------------------------------------------------- */

						/*    CALL PREVTR WILL:
						 *        DECIDE WETHER TO USE THE MAIN OR MINI RULE AND LOAD THE
						 *        APPROPRIATE END POSITION, STRECTH VARIABLES, ETC...
						 *        MINI WC9 RULE START HERE */

						while( TRUE ){
							vtrpre(&diacb2X_.k7,&diacb2X_.oflad,sp2yX_.ovrflw);
							vwarg2X_.wc50ms = vwarg2X_.wc50m;

							/* --------------------------------------------------------------------- */

							/*        SAVE FORM OF ELEMENT MATCHED FOR SW. -11 099 (VTR INSERT) */
							vwarg1X_.omfrm = sworkX_.swork[vwarg2X_.i-One][3-One];

							/*        'VTRFWR' SUBROUTINE IS THE VTRF WRITE LOOP
							 *        IT CONVERTS THE 'VTR' INPUT LINE(S) FROM THE SPRULE
							 *        INTO ONE 'VTRF' OUTPUT ARRAY */

							vbdataX_.k3n = 1;

							backspX_.bsdflt = 0;

							vtrfwr();

							if( errvrsX_.errlvl != 0 )
								goto L_10003;

							if( vwarg2X_.swx != 0 )
								diagno(6);

							if( opswX_.sw[15-One] == 1 )
								{
								fprintf( _spec_fp, "          %4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d\n", 
								  vtrs42X_.sw42n, flowckX_.i3save, 
								  w50valX_.i3, vwarg2X_.i, vtrptrX_.nptp, 
								  sworkoX_.phcto, phsupX_.phcts, 
								  head26X_.headwc, head26X_.headty, 
								  head26X_.headfr, sw18bkX_.phrnew, 
								  sw36bkX_.rel, sw36bkX_.relcas, sw21bkX_.case_, 
								  sw14bkX_.sw14n, phsupX_.phsup, 
								  sw26nX_.sw26n, sw26bkX_.phr26, minickX_.k7m, 
								  cbsp2X_.i3x, vwarg1X_.isave, vwarg2X_.wc50m );
								}

							/* --------------------------------------------------------------------- */

							/*     N6 IS THE POSITION OF THE LAST ELEMENT MATHCED ON IN THE RULE */
							flowckX_.n6 = flowckX_.im1 + sploopX_.li;
							if( vtrs42X_.sw42n == 1 )
								flowckX_.n6 = sw42bkX_.im1sav + 
								  sploopX_.li;

							/*        CREATE SWORK FOR THIS VTR.  IF PHCTO IS EQUAL TO PHCTS,
							 *        THEN SW26 HAS PREVIOUSLY SET THIS PHRBEG AND
							 *        NO NEW SWORK IS CREATED. */

							if( sworkoX_.phcto != phsupX_.phcts ){
								sworkoX_.sworko[sworkoX_.phcto-One][1-One] = sworkX_.swork[flowckX_.n6-One][1-One];
								sworkoX_.sworko[sworkoX_.phcto-One][2-One] = sworkX_.swork[flowckX_.n6-One][2-One];
								sworkoX_.sworko[sworkoX_.phcto-One][3-One] = sworkX_.swork[flowckX_.n6-One][3-One];
								sworkoX_.sworko[sworkoX_.phcto-One][4-One] = sworkX_.swork[flowckX_.n6-One][4-One];
								sworkoX_.phrhdo[sworkoX_.phcto-One] = sworkX_.phrhed[flowckX_.n6-One];
								}

							if( opswX_.sw[15-One] == 1 ){
								fprintf( _spec_fp, "  AT 1260 %5d%5d%5d%5d%5d%5d%5d%5d%5d%5d\n", 
								  flowckX_.n6, sploopX_.li, vwarg2X_.i, 
								  w50valX_.i3, sworkoX_.phcto, phsupX_.phcts, 
								  flowckX_.i3save, head26X_.headwc, 
								  sw42bkX_.i3keep, sw42bkX_.likeep );
								fprintf( _spec_fp, "\n *** THE SWORK VALUES (BEFORE VTR) \n      " );
								diag_write_sworko();
								scnprt();
								}

							cnX_.vtrdon = 0;
							if( flowckX_.i3save < w50valX_.i3 )
								flowckX_.i3save = w50valX_.i3;
							if( semargX_.pntr9 != 0 || vtrs42X_.sw42n !=  0 )
								flowckX_.i3save = w50valX_.i3;

							/* --------------  PROCESS VTR SUBROUTINE ------------------------------ */

							/*    T1VTRPRO WILL PROCESS THE VTR */

							/*???????????? THE OLD WAY WITH RETSW CONTROLLING ??????????????????????
							 *? CHANGE TO  THE NEW WAY WITH ERRLVL CONTROLLING */

							/*?   RETSW WILL CONTROL WHAT WE DO ONCE WE RETURN FROM THIS ROUTINE
							 *?         RETSW = 1  MEANS GO GET THE NEXT RULE
							 *?         RETSW = 2  MEANS THAT WE WANT TO PROCESS THE VTR OF A WC 9
							 *?                    OR WC 10 RULE (42 SWITCH CALLS T1GETSP DIRECTLY)
							 *?         RETSW = 99 MEANS THAT AN ARRAY OVERFLOWED WHICH MAKES IT
							 *?                    IMPOSSIBLE TO PROCESS THE REST OF THE SENTENCE;
							 *?                    GOTO THE TR2WRT ROUTINE TO SAVE WHAT WE CAN.
							 *? */

							/*+                                                        *Rphs3*GBA
							 *     CALL VTRPRO */
							vtrctl();
							/*-                                                        *Rphs3*GBA */

							if( errvrsX_.errlvl != 0 )
								goto L_10004;

							if( getvtrX_.getvtr != 1 )
								goto L_80;
							getvtrX_.getvtr = 0;
							/*         SKIP LOOP CHECK IF NO MAIN WC RULE WAS FOUND.
							 *              APPLICABLE ONLY FOR WC09 AND WC10
							 *+        K7M IS ONLY 0-OR-1 IN PREVTR, WHICSP   04/22/86  *B0395DSD */
							if( !(minickX_.k7m != 0 && diacb2X_.k7 == 
							  0) )
								break;
							/*-                                               04/22/86  *B0395DSD */
							}
						}
L_10004:
					lvl = (errvrsX_.errlvl + 4)/4;
					errvrsX_.errlvl = 0;
					if( lvl == 1 )
						goto L_9999;
					if( lvl == 2 )
						goto L_9900;
					if( lvl == 3 )
						goto L_80;
					goto L_9900;
L_10003:
					lvl = (errvrsX_.errlvl + 4)/4;
					errvrsX_.errlvl = 0;
					if( lvl == 1 )
						goto L_9999;
					if( lvl == 2 )
						goto L_9900;
					if( lvl == 3 )
						goto L_80;
					goto L_9900;

L_10002:
					loopckX_.nptpsv = 0;

L_795:
					if( opswX_.sw[3-One] == 1 )
						{
						fprintf( _spec_fp, " ******   LOOP‹   ****** \n" );
						}
					flowckX_.i3save = xx + 1;
					vtrnX_.vtrn = -1;
					vbdataX_.k3p1 = 0;
					elemld();
					if( errvrsX_.errlvl != 0 ){
						lvl = (errvrsX_.errlvl + 4)/4;
						errvrsX_.errlvl = 0;
						if( lvl == 1 )
							goto L_9999;
						if( lvl == 2 )
							goto L_9900;
						if( lvl != 3 )
							goto L_9900;
						}
					}

				/* ----------------------  LOOP THROUGH EACH INPUT SWORK --------------- */

				/*        HAVE WE GONE THROUGH ALL OF THE ELEMENTS? */
L_80:
				while( flowckX_.i3save > sworkX_.phct ){

					/* ---------------------------------------------------------------------
					 *     T2MOPUT PUTS THE FINISHING TOUCHES (IF NEEDED) ONCE A SENTENCE
					 *     HAS BEEN PROCESSED.  A LOT OF WRITE STATEMENTS, ETC... */

					mopup(&retcod);
					if( retcod != 99 )
						goto L_9900;
					}

				if( !((sworkoX_.phcto > 0 && sworkoX_.phrbgo[sworkoX_.phcto-One] > 
				  opadroX_.opo) || (phsupX_.phsup == 1)) )
					break;
				}

			/*        BEFORE INCREMENTING PHCTO, SET SWORK WITH SW26 VALUES */
			if( sworkoX_.phcto == sw26bkX_.phr26 )
				set26();
			}
		}
	else{
		lmove(opadroX_.opadro,1,opadriX_.opadri,1,1002);
		lmove(hpdopoX_.hfdopo,1,hpdopiX_.hfdopi,1,500);
		}


L_9900:
	x = x;

	/*winnt	CALL TR2WRT
	 *winnt            IF (ERRLVL .NE. 0) GOTO 9999
	 *							read in sentence data from transl */
	if( tran2_io(ADR(_l0,12)) == 0 )
		goto L_9905;
	errvrsX_.errlvl = 2;
	goto L_9999;
L_10001:
	if( retsw == 99 ){
		/*cccccccccccccccccccccccccccccccccccccc
		 *				Clean out! */
		x = x;

		/*				close the input and output */
		if( tran2_io(ADR(_l0,3)) != 0 ){
			}
		if( tran2_io(ADR(_l0,13)) != 0 ){
			}

//cat-102 line added
        addMemoryState("t2 driver - LSPREAD()");
//        LSPREADCLEAN();
//	Now called from TranslCommonObjects::CleanupCommonObjects()
        addMemoryState("t2 driver - rule_unload()");
		rule_unload();
        addMemoryState("t2 driver - freeTrObjects()");
		freeTrObjects();
        addMemoryState("t2 driver - complete");
      if (_spec_fp)
      {
         fclose(_spec_fp);
      }

#ifdef TRANSTAT
	write_down_rule_stat();
#endif /* TRANSTAT */

		return lexit(0);
		}
	else{
		errvrsX_.errlvl = 2;
		}
	goto L_9999;
	/*				close the input and output */
L_10000:
	fprintf( _spec_fp, "ERROR: Program will terminate.\n" );
	fprintf( _spec_fp, "%5ld %3ld", source_sentX_.sentid, source_sentX_.sentlng );
	fprintf( _spec_fp, " " );
	for( x=1; x <= 300; x++ ){
		fprintf( _spec_fp, "%c", source_sentX_.sentbuf[x-One] );
		}
	fprintf( _spec_fp, "\n" );

	if( tran2_io(ADR(_l0,3)) != 0 ){
		}
	if( tran2_io(ADR(_l0,13)) != 0 ){
		}
	freeTrObjects();
	return labort(3);
} /*end of function*/

