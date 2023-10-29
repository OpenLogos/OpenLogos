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
	/*     DRIVER PROGRAM FOR TRAN1 MODULE */
	/* CHANGES:
	 *      7/12/93 JAL: set the black hole counter flag for each incoming
	 *          element by calling BHSET. Use SCON100 as flag.
	 *      4/30/93 JAL: for 2 pass strategy, save the incoming SCON 2,11,13
	 *          values to protect each pass from changes by the other pass.
	 *     03/05/93 JAL: SET PHRHDO as well as SWORKO(4) at beginning of
	 *          a vtr (just before vtrctl) if not already set by 26 or 34 switch.
	 *     01/20/92 JAL: ADD ABILITY TO EXECUTE ONLY 1 OF 2 PASSES WITH
	 *          2 PASS STRATEGY ACTIVE (KEYED OFF PASSKP).  EITHER LOOP
	 *          BACK AFTER 1ST INIT IF PASS2 ONLY OR JUST DONT READ AND
	 *          INIT A 2ND TIME IF PASS1 ONLY. CHANGES TO TXWRITE, TXMOPUP
	 *          AND THE FILE OPEN AND LOAD ROUTINES.
	 *     10/28/91 JAL: IF NO MATCH ON THIS ELEMENT EVEN IN DEFAULT RULES
	 *          THEN FORCE A LOAD OF THE SWORK AND CONTINUE.
	 *    10/03/91 JAL: SET SCT TO SAVE SCON SPACE FOR ELEMENTS WHICH MAY
	 *             BE ADDED VIA -68 SWITCH.
	 *    87/06/08 LG002GBA RPHS3:  PHASE 3 OF 30.40.50 TBLS
	 *    04/23/87 *R1679RKH*  OFL3B CONVERSION & R1685 SWORK LIMI
	 *    08/17/86 *R1561DSD: 100 SCONS
	 *    07/24/86 *B0415DSD: COMPRESS "* OUTPUT" FILES
	 *    04/22/86 18:23 *B0395DSD - AFTER VTRPRO, K7M CAN BE ANY
	 *          POSITIVE RULE NO., NOT 1 (THAT VALUE IS ONLY IN WHICSP AND
	 *          PREVTR).  TEST K7M <> 0 IN DECIDING LOOP TEST
	 *    04/04/85 09:33 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <configdatafileinterface/configdatainterfacemain.h>
#define MS_F77
#include <logos_include_res_pt/fcrt.h>
#include "project.h"
#include "logoslib.h"
#include "trans.h"
#include "projexts.h"
#include "parsetrans.h"
#include <string.h>
#include <logos_include_res_pt/jbctrl.h>
//#include <logos_libs/dbms2restran/gettargetcodes.h>

#ifdef TRANSTAT
#include <fcntl.h>
int rulestatfile;
int rulestat[20000];
char *rulestatfilename = "e:\\esense\\bin\\rulestat1";
#endif /* TRANSTAT */

void addMemoryState(const char* memTag);
void startBlockMemoryLog(void);
void endBlockMemoryLog(void);

EXTERN struct t_neg1X_ {
	short int negwc1[9][8];
	}	neg1X_;
EXTERN struct t_gneg1X_ {
	short int gngwc1[9][8];
	}	gneg1X_;
EXTERN struct t_form25X_ {
	short int eform2[10][3];
	}	form25X_;
EXTERN struct t_getvtrX_ {
	short int getvtr;
	}	getvtrX_;
EXTERN struct t_head26X_ {
	short int headwc, headty, headfr, headhd, headt2, headt3;
	}	head26X_;
EXTERN struct t_loopckX_ {
	short int imatch[ELMMAX], noloop[21], nptpsv, nptpx, call36[2];
	}	loopckX_;
EXTERN struct t_sploopX_ {
	short int st16, li;
	}	sploopX_;
EXTERN struct t_sw14bkX_ {
	short int gen, num, per;
	}	sw14bkX_;
EXTERN struct t_sw18bkX_ {
	short int phrnew;
	}	sw18bkX_;
EXTERN struct t_sw26nX_ {
	short int sw26n;
	}	sw26nX_;
EXTERN struct t_sw36bkX_ {
	short int det, rel, relcas, relgen, relnum, relper;
	}	sw36bkX_;
EXTERN struct t_sw42bkX_ {
	short int im1sav, i3keep, likeep;
	}	sw42bkX_;
EXTERN struct t_vtrs42X_ {
	short int sw42n;
	}	vtrs42X_;
EXTERN struct t_vwarg1X_ {
	short int look50[3], chng50[3], wc50el[3], isave, omfrm;
	}	vwarg1X_;


int t1driver(void)
{
	static short int form, junk, locflg, lrgjob, retsw, sbsetx, setx, 
	   wc50sv, wc50to, wrdsrc, x, yy, zero, zz;
	static long int _l0, _l1, retflg, tempi4;
		/*pointer COMMON transls*/
	struct  {
		short int k7, oflad, n3;
		}	*_diacbX_ = (void*)&diacbX_;
	struct  {
		short int i, wc42m, wc50m, iz4, swx, s11prt, wc50ms;
		}	*_vwarg2X_ = (void*)&vwarg2X_;
	struct  {
		short int vtr[26];
		}	*_vtrX_ = (void*)&vtrX_;
	struct  {
		short int im1, i3save, i3str, n6, n6jim, n6jims, phrstr;
		}	*_flowckX_ = (void*)&flowckX_;
	struct  {
		short int k7m, minifg, minilp;
		}	*_minickX_ = (void*)&minickX_;
	struct  {
		short int nwrks, index, k3p3, pntr9;
		}	*_semargX_ = (void*)&semargX_;
	struct  {
		short int hedcas, hedgen, hednum, hedper, hedsc3, ngen, nnum, 
		  relngn, relnnm, relnpr, sw25n, tw25;
		}	*_sw25bkX_ = (void*)&sw25bkX_;
		/*end of pointer COMMON transls*/
	static char modnam[9] = "TRAN1   ";
	static char pgmnam[9] = "T1DRIVER";
	static short ij = 0;
	static short iz = 0;
	static short kz = 0;
	static short om = 0;
	static short xp = 0;
	static short xx = 0;
	static short lvl = 0;
	char pgmName[6];
   char lpszBuf[MAX_FILEPATH_LEN]; 

	initializeTrObjects();
	tranidX_.tranid = 1;
	lrgjob = 1;
	errvrsX_.untcnt = 0;
	sentctX_.sentct = 0;
	strcpy(errvrsX_.mod, "TRAN1   ");

	blk_data();

	getProgramName(passesX_.passfl, passesX_.passct, 1, pgmName);
	errvrsX_.err = jcloadc(pgmName);
	if (errvrsX_.err != 0 )
   {
		errvrsX_.errlvl = 1;
		printf("Fatal Error Loading job information.");
		exit(1);
	}		

	if (passesX_.passfl == 1 && passesX_.passct == 1)
   {
      GetConfigData("tempfile", "parse1_diag", lpszBuf, MAX_FILEPATH_LEN);
      _spec_fp = fopen(lpszBuf, "w");
	}
   else
   {
      GetConfigData("tempfile", "tran1_diag", lpszBuf, MAX_FILEPATH_LEN);
      _spec_fp = fopen(lpszBuf, "w");
   }

#ifdef TRANSTAT
	init_rulestat(); 
#endif /* TRANSTAT */

		/*          READ GENERATED GERNUM ARRAY */
   errvrsX_.err = data_load("sourcedata","sw38",nounsX_.gernum,0,200*4);
	if( errvrsX_.err == 0 )
   {
		errvrsX_.errlvl = 1;
		goto L_10000;
	}

	/*                 FORM25 TABLE */
	errvrsX_.err = data_load("sourcedata","form25",form25X_.eform2,0,6*10);
	if( errvrsX_.err == 0 ){
		errvrsX_.errlvl = 1;
		goto L_10000;
		}

	/*     Read into memory the OFL3B exception table.  It is in the
	 *     fourth record of the FORMTRAN file. */
	locflg = 1;
	o3btab(junk,junk,junk,junk,&retflg,locflg);
	if( retflg != 0 ){
		errvrsX_.errlvl = 1;
		goto L_10000;
		}



	/*     Read in the bit tables for the form matching algorithm, FORMOD,
	 *     each of the first three records has one bit map, i.e. Nounform
	 *     Verbform and Otherform, though not nescessarily in that order. */
	formod(1,xx,(long)yy,zz,&retsw);
	if( errvrsX_.errlvl != 0){
		errvrsX_.errlvl = 1;
		goto L_10000;
		}

    errvrsX_.err = data_load("sourcedata","sw36",sw36tableX_.sw36table,0,2*2*9*9);
	if( errvrsX_.err == 0 ){
		errvrsX_.errlvl = 1;
		goto L_10000;
		}



	/* 			Load in the tran rules */
	if( rule_load(1, miniflX_.minifl, passesX_.passfl, passesX_.passct) != 0 ){
		errvrsX_.errlvl = 1;
		goto L_10000;
		}
	/*                   NEGWC1 file.
	 *                   GER SRC REPLACE DATAED NEGWC1
	 *                   (ENG SRC) WITH GERMAN VERSION */
	if( srcflgX_.srcflg == 1 ) memcpy(&neg1X_,&gneg1X_,sizeof(neg1X_));


	/*					open the input data comming from previous component */
	if( tran1_io(ADR(_l0,0)) != 0 ){
		errvrsX_.errlvl = 1;
		goto L_10000;
		}
	/*					open the output written by this component */
	if( tran1_io(ADR(_l0,10)) != 0 ){
		errvrsX_.errlvl = 1;
		goto L_10000;
		}





	/* ----------------------- NEXT SENTENCE START ------------------------- */

	/*   COME BACK HERE AFTER EACH SENTENCE HAS BEEN PROCESSED AND READ THE
	 *   INPUT FILES (I.E. RES OUTPUT) FOR THE NEXT SENTENCE AND PUT THEM IN
	 *   COMMON. */

L_5:
	sentctX_.sentct += 1;
	errvrsX_.untcnt = sentctX_.sentct;

	/*	read in sentence data from transl */
	retsw = tran1_io(ADR(_l0,1));
	if( retsw != 0 )
		goto L_10001;

	diag_check(2);

/*usefull code for debugging
	printf("%8d %4d\n", source_sentX_.sentid, source_sentX_.sentlng );
	if (source_sentX_.sentid == 80)
	{
		x = x;
	}
*/


	/* ---------------------- INITIALIZATION OF VARIABLES ------------------- */

	/*                  RESET ALL THE FLAGS, ARRAYS, ETC... THAT MIGHT
	 *                  HAVE BEEN SET BY THE LAST SENTENCE. */

	init();

	

	/*                  LOAD OFL ARRAYS  */
	if (tablet() != 0)
	{
		goto L_9999;
	}


	diagno(12);
	diag_write_target_info();



	/*                  SET THE SCONS FOR THE SWORKS RIGHT AWAY */

	for( ij=1; ij <= swork1X_.phct; ij++ ){

		prctjX_.j = prctX_.js[sconX_.scolnk[ij-One]-One];
		sconX_.scon[ij-One][1-One] = swork1X_.swork1[ij-One][dct2X_.dct2[prctjX_.j-One]-One];
		sconX_.scon[ij-One][2-One] = swork1X_.swork1[ij-One][dct4X_.dct4[prctjX_.j-One]-One];
		sconX_.scon[ij-One][3-One] = ofl2X_.ofl2i[sconX_.scolnk[ij-One]-One];

		/*   SET THE GENDER OF NOUNS INTO SCON4 RIGHT AWAY */
		if( sconX_.scon[ij-One][1-One] == 1 ){
			if( srcflgX_.srcflg == 2 ){
				if( ofltabX_.ofl4i[sconX_.scolnk[ij-One]-One] < 13 || 
				    ofltabX_.ofl4i[sconX_.scolnk[ij-One]-One] > 16 )
					sconX_.scon[ij-One][4-One] = targ25X_.targ25[sconX_.scolnk[ij-One]-One];
				}
			else{
				sconX_.scon[ij-One][4-One] = targ25X_.targ25[sconX_.scolnk[ij-One]-One];
				}
			}

		sconX_.scon[ij-One][9-One] = respasX_.respas[sconX_.scolnk[ij-One]-One][3-One];

		sconX_.scon[ij-One][10-One] = ij;
		sconX_.scon[ij-One][11-One] = typsvX_.typsav[sconX_.scolnk[ij-One]-One];
		sconX_.scon[ij-One][12-One] = ofltabX_.ofl3i[sconX_.scolnk[ij-One]-One];
		/*         PLACE NOUN GENDER IN SCON 12 */
		sconX_.scon[ij-One][13-One] = ofltabX_.ofl4i[sconX_.scolnk[ij-One]-One];

		/*     Read the O3B table and reset Scons 12 & 45 & 1 more scon */

		locflg = 2;
		form = swork1X_.swork1[ij-One][dct3X_.dct3[prctjX_.j-One]-One];
		o3btab(sconX_.scon[ij-One][1-One],sconX_.scon[ij-One][2-One],
		       form,ij,&retflg,locflg);

		xp = xpatnfX_.xpatfm[sconX_.scolnk[ij-One]-One][prctX_.js[sconX_.scolnk[ij-One]-
		  One]-One];
		if( xp == 101 )
			sconX_.scono[sconX_.scolnk[ij-One]-One][45-SCONX1-One] = 6;
		if( xp == 102 )
			sconX_.scono[sconX_.scolnk[ij-One]-One][45-SCONX1-One] = 7;
		if( xp == 103 )
			sconX_.scono[sconX_.scolnk[ij-One]-One][45-SCONX1-One] = 8;
		/*         PLACE NOUN GENDER IN SCON 12 (Changed to 45 for MS/MT) */
		sconX_.scon[ij-One][15-One] = ovc2a3X_.ofl2a[sconX_.scolnk[ij-One]-One];
		sconX_.scon[ij-One][16-One] = ovc2a3X_.ofl3a[sconX_.scolnk[ij-One]-One];
		sconX_.scon[ij-One][17-One] = respasX_.respas[sconX_.scolnk[ij-One]-One][1-One];
		if( sconX_.scon[ij-One][17-One] > 1000 ){
			xx = sconX_.scon[ij-One][17-One] - 1000;
			swork1X_.swork1[ij-One][dct3X_.dct3[prctjX_.j-One]-One] = xx;
			sconX_.scon[ij-One][17-One] = xx;
			if( opswX_.sw[3-One] == 1 )
				{
				fprintf( _spec_fp, "\nRESPAS VAL%4d CHANGES SWORK1%4d FORM FIELD TO%4d\n", 
				  respasX_.respas[ij-One][1-One], ij, swork1X_.swork1[ij-One][dct3X_.dct3[prctjX_.j-One]-
				  One] );
				}

			}
		/*                    set the black hole counter flag(SCON100) 7/12/93 jal */
		bhset(ij,&retflg);

		}
	/*                    set the sentence break flag based on EOS SAL codes */
	if( sconX_.scon[swork1X_.phct-One][1-One] == 20 &&
		sconX_.scon[swork1X_.phct-One][13-One] == 10 ){
		setx = sconX_.scon[swork1X_.phct-One][11-One];
		if( setx == 51 ){
			sbsetx = sconX_.scon[swork1X_.phct-One][2-One];
			if( (sbsetx >= 185 && sbsetx <= 189) && sbsetx != 187 )
				elemctX_.sntbrk = sbsetx;
			}
		else if( setx == 52 ){
			elemctX_.sntbrk = 52;
			}
		}
	/*                    SAVE SCON SPACE FOR ELEMENTS ADDED VIA -68SWITCH */
	prtscoX_.sct = elemctX_.elemct + AD1MAX;

	/*                    SET UP FOR PARSE: PASS 1 OF 2 PASS STRATEGY
	 *			save the parallel data structures that
	 *			reflect the initial PARSE1 state and that
	 * 			are selectively	modified by explicit switch request */
	if( passesX_.passfl == 1 && passesX_.passct == 1 ){
		/*                    SAVE THE INPUT SWORK FOR PASS2 */
		memcpy(swork1X_.swrk1i, swork1X_.swork1,2*15*ELMMAX);

		/*			init map from SCONS to TRAN1 SWORKS (SWRK1I) */
		memset(sworkiX_.sc2swi,'\0',SCONY*2);
		for( xx=1; xx <= elemctX_.elemct; xx++ ){
			sworkiX_.sc2swi[xx-One] = xx;
			}
		/*                    save SCON 2,11,13 for pass2
		 *                    Only nec. for each SWORK element(ELEMCT), not whole SCON. */
		for( xx=0; xx < elemctX_.elemct; xx++ ){
			sconinX_.sconin[xx][1-One] = sconX_.scon[xx][2-One];
			sconinX_.sconin[xx][2-One] = sconX_.scon[xx][11-One];
			sconinX_.sconin[xx][3-One] = sconX_.scon[xx][13-One];
			}
		/*		     initial PRSFRM from FORMSV */
		memcpy(formsaX_.prsfrm,formsaX_.formsv,2*ELMMAX);
		/*		     initial source swork counter */
		elemctX_.parsct = elemctX_.origct;
		}

	/*                SET UP FOR TRAN: PASS 2 OF 2 PASS STRATEGY
	 *                  Overlay initial res data with PARSE modifications */
	if( passesX_.passfl == 1 && passesX_.passct == 2 ){
		/*	    	   read data from PARSE and overlay the TRAN1 data. */
		getprs(2);
		/*                  PRINT SWORK RECORDS */
		if( diagsX_.anydi == 1 ){
			fprintf( _spec_fp, "\n================================================================================\n               ***    P A R S E    O U T P U T     ***\n================================================================================\n" );
			diagno(12);
			}
		}


	/* ----------------------  LOOP THROUGH EACH INPUT SWORK --------------- */

	/*                         END OF SENTENCE ?? */

	while( _flowckX_->i3save <= swork1X_.phct ){

		/*                          RESET FLAGS AND ARRAYS IF LAST RULE
		 *                          LOADED AND DIDN'T SUPPRESS SWORK CREATION,
		 *                          (NO PHRASE SUPPRESSION,VIA -20 SWITCH). */

		if( supresX_.n3up != _diacbX_->n3 && supresX_.phsup != 1 ){
			/*                          BEFORE BUMPING PHCTO, MAKE THE
			 *                          FINAL SWORKO SETTING FROM SW26 */
			if( sworkoX_.phcto == sw26bkX_.phr26 )
				t1se26();
			/*                          PHCTO, PHCTS, N3UP, N3, PHSUP, PHRNEW
			 *                          ARE ALL INVOLVED IN PHRASE SUPPRESSION. */
			if( sworkoX_.phcto > 0 )
				sworkoX_.phrndo[sworkoX_.phcto-One] = _diacbX_->n3;
			sworkoX_.phcto += 1;
			sworkoX_.phrhdo[sworkoX_.phcto-One] = 0;
			sworkoX_.phrbgo[sworkoX_.phcto-One] = _diacbX_->n3 + 1;
			_flowckX_->phrstr = _diacbX_->n3 + 1;
			supresX_.n3sw25 = 0;
			_sw25bkX_->sw25n = 0;
			sw21bkX_.sw21n = 0;
			sw16bkX_.sw16n = 0;
			sw34bkX_.sw34n = 0;
			_sw25bkX_->tw25 = 0;
			}

		supresX_.phsups = supresX_.phsup;
		supresX_.n3up = _diacbX_->n3;
		sw18bkX_.phrnew = 0;
		supresX_.phsup = 0;
		supresX_.n3sv = _diacbX_->n3;

		if( opswX_.sw[10-One] == 1 ){
			fprintf( _spec_fp, " PHCTO TRAN1 %4d%4d%4d%4d%4d%4d", 
			  sworkoX_.phcto, sploopX_.li, w50valX_.i3, _flowckX_->n6, 
			  _flowckX_->i3save, _flowckX_->phrstr );
			for( xx=131; xx <= 170; xx++ ){
				fprintf( _spec_fp, "%4d", inhbX_.inhb[xx-One] );
				}
			fprintf( _spec_fp, "\n" );
			fprintf( _spec_fp, " OPADRO  " );
			for( kz=1; kz <= _diacbX_->n3; kz++ ){
				fprintf( _spec_fp, "%6d", opadroX_.opadro[kz-One] );
				}
			fprintf( _spec_fp, "\n" );
			fprintf( _spec_fp, " HFDOPO" );
			for( xx=1; xx <= _diacbX_->n3; xx++ ){
				fprintf( _spec_fp, "%6d", hpdopoX_.hfdopo[xx-One] );
				}
			fprintf( _spec_fp, "\n" );
			fprintf( _spec_fp, " SCONPO " );
			for( kz=1; kz <= _diacbX_->n3; kz++ ){
				fprintf( _spec_fp, "%6d", opadroX_.sconpo[kz-One] );
				}
			fprintf( _spec_fp, "\n" );
			}

		/*         ************* START OF NEW PHRASE ************ */

		_flowckX_->i3str = _flowckX_->i3save;
		/*        ISAVE IS USED AT 800 TO CHECK LOOPING OF RULES */
		vwarg1X_.isave = _vwarg2X_->i;
		_flowckX_->i3save = 0;
		vtrs42X_.sw42n = 0;
		_vwarg2X_->wc42m = 0;
		_minickX_->k7m = 0;
		_semargX_->pntr9 = 0;
		/*+                                                     *09/24/91*JAL* */
		swtc68X_.sw68ct = 0;
		/*-                                                     *09/24/91*JAL* */

		/*--------------------------------------------------------------------
		 *     CALL GETSP WILL TAKE CARE OF GETTING THE NEXT SP RULE.
		 *     IT WILL PLACE THE RULE NUMBER IN K7 FOR A MAIN RULE
		 *                               AND IN K7M FOR A MINI RULE */

		getsp_x(&diacbX_.k7, &diacbX_.oflad, sp1yX_.ovrflw);
		if( errvrsX_.errlvl != 0 ) {
			lvl = (errvrsX_.errlvl + 4)/4;
			errvrsX_.errlvl = 0;
			if( lvl == 1 )
				goto L_9999;
			if( lvl == 2 )
				goto L_9900;
			if( lvl != 3 )
				goto L_9900;
			/*+                                                     *10/28/91*JAL*
			 *               NO MATCH‹  FORCE A LOAD RULE AND CONTINUE */
			}
		else if( _diacbX_->k7 == 0 && _minickX_->k7m == 0 ){
			xx = _flowckX_->i3str;
			if( vtrs42X_.sw42n == 1 )
				xx = vwarg1X_.isave;
			_flowckX_->i3save = xx + 1;
			vtrnX_.vtrn = -1;
			vbdataX_.k3p1 = 0;
			elemld();
			if( errvrsX_.errlvl == 0 ){
				sworkoX_.sworko[sworkoX_.phcto-One][1-One] = swork1X_.swork1[_flowckX_->i3str-One][dct2X_.dct2[prctX_.js[sconX_.scolnk[_flowckX_->i3str-One]-
				  One]-One]-One];
				sworkoX_.sworko[sworkoX_.phcto-One][2-One] = swork1X_.swork1[_flowckX_->i3str-One][dct4X_.dct4[prctX_.js[sconX_.scolnk[_flowckX_->i3str-One]-
				  One]-One]-One];
				sworkoX_.sworko[sworkoX_.phcto-One][3-One] = swork1X_.swork1[_flowckX_->i3str-One][dct3X_.dct3[prctX_.js[sconX_.scolnk[_flowckX_->i3str-One]-
				  One]-One]-One];
				sworkoX_.sworko[sworkoX_.phcto-One][4-One] = _flowckX_->i3str;
				sworkoX_.phrhdo[sworkoX_.phcto-One] = _flowckX_->i3str;
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

			while( TRUE ){
				vtrptrX_.nptp = spX_.sp[12-One];


				/*        TO PREVENT LOOPING ON SAME RULE */
				xx = _vwarg2X_->i;
				if( vtrs42X_.sw42n == 1 )
					xx = vwarg1X_.isave;
				loopckX_.imatch[xx-One] += 1;
				/*     Loop logic improvements                    OM fix 4/8/87      + */
				if( loopckX_.imatch[xx-One] >= 20 )
					goto L_795;

				/*  Add the total of the possible stretches so that we can check to see
				 *  if we are looping on a stretch rule. */
				wc50to = vwarg1X_.wc50el[1-One] + vwarg1X_.wc50el[2-One] + 
				  vwarg1X_.wc50el[3-One];

				/*  if vtr are different, no loop here. */
				if( vtrptrX_.nptp == loopckX_.nptpsv ){
					/*  if previous rule was from mini, skip it */
					if( _minickX_->minifg == 0 ){
						if( _minickX_->minilp == 0 ){
							/*  ISAVE set by SW42, don't know why we skip.
							 *  probably to ignore loop if same rule but starts on different swork */
							if( _vwarg2X_->i == vwarg1X_.isave ){
								/*  Same rule, but the stetching is different, skip. */
								if( !(_vwarg2X_->wc50m != 0 && wc50to != 
								  wc50sv) )
									goto L_10002;
								}
							}
						}
					}

				loopckX_.nptpsv = vtrptrX_.nptp;
				/*              Loop logic improvements     OM FIX 4/8/87  (next line) */
				wc50sv = wc50to;

				/* --------------------------------------------------------------------- */

				/*    CALL PREVTR WILL:
				 *        DECIDE WETHER TO USE THE MAIN OR MINI RULE AND LOAD THE
				 *        APPROPRIATE END POSITION, STRECTH VARIABLES, ETC...
				 *        MINI WC9 RULE START HERE */

				while( TRUE ){
					vtrpre(&diacbX_.k7,&diacbX_.oflad,sp1yX_.ovrflw);
					_vwarg2X_->wc50ms = _vwarg2X_->wc50m;

					/*--------------------------------------------------------------------- */

					/*        SAVE FORM OF ELEMENT MATCHED FOR SW. -11 099 (VTR INSERT) */
					vwarg1X_.omfrm = swork1X_.swork1[_vwarg2X_->i-One][dct3X_.dct3[prctX_.js[sconX_.scolnk[_vwarg2X_->i-One]-
					  One]-One]-One];

					/*       'VTRFWR' SUBROUTINE IS THE VTRF WRITE LOOP
					 *        IT CONVERTS THE 'VTR' INPUT LINE(S) FROM THE SPRULE
					 *        INTO ONE 'VTRF' OUTPUT ARRAY */

					vbdataX_.k3n = 1;
					backspX_.bsdflt = 0;
					vtrfwr();
					if( errvrsX_.errlvl != 0 )
						goto L_10003;

					if( opswX_.sw[3-One] == 1 ){
						if( _vwarg2X_->wc50m != 0 || _vwarg2X_->wc42m != 0 )
							{
							fprintf( _spec_fp, "\n VTRF LOADED  \n                    " );
							for( iz=0; iz < _vwarg2X_->iz4; iz++ ){
								fprintf( _spec_fp, "%4d", vtrfX_.vtrf[iz] );
								}
							fprintf( _spec_fp, "\n" );
							}
						}

					if( opswX_.sw[10-One] == 1 )
						{
						fprintf( _spec_fp, "          %4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d\n          %4d%4d\n", 
						  _flowckX_->i3save, w50valX_.i3, _vwarg2X_->i, 
						  vtrptrX_.nptp, sworkoX_.phcto, supresX_.phcts, 
						  supresX_.phsup, supresX_.phsups, head26X_.headwc, 
						  head26X_.headty, head26X_.headfr, sw18bkX_.phrnew, 
						  supresX_.phsup, _diacbX_->k7, _diacbX_->n3, 
						  _flowckX_->n6, _flowckX_->im1, sploopX_.li, 
						  sw36bkX_.rel, sw36bkX_.relcas, sw26nX_.sw26n, 
						  sw26bkX_.phr26, vtrs42X_.sw42n, sw14bkX_.num, 
						  _minickX_->k7m, cbsp2X_.i3x, vwarg1X_.isave, 
						  _vwarg2X_->wc50m );
						}

					/*----------------------------------------------------------------------- */

					/*     N6 IS THE POSITION OF THE LAST ELEMENT MATHCED ON IN THE RULE */
					_flowckX_->n6 = _flowckX_->im1 + sploopX_.li;
					if( vtrs42X_.sw42n == 1 )
						_flowckX_->n6 = sw42bkX_.im1sav + sploopX_.li;

					/*        CREATE SWORKO FOR THIS VTR.  IF PHCTO IS EQUAL TO PHCTS,
					 *        THEN SW26 HAS PREVIOUSLY SET THIS PHRASE AND
					 *        NO SWORKO IS CREATED. */

					if( supresX_.phcts != sworkoX_.phcto && sw34bkX_.sw34n != 1 ){
						om = prctX_.js[sconX_.scolnk[_flowckX_->n6-One]-One]*4;
						sworkoX_.sworko[sworkoX_.phcto-One][1-One] = swork1X_.swork1[_flowckX_->n6-One][om-One];
						sworkoX_.sworko[sworkoX_.phcto-One][2-One] = swork1X_.swork1[_flowckX_->n6-One][om+1-One];
						sworkoX_.sworko[sworkoX_.phcto-One][3-One] = swork1X_.swork1[_flowckX_->n6-One][om+2-One];
						sworkoX_.sworko[sworkoX_.phcto-One][4-One] = _flowckX_->n6;
						sworkoX_.phrhdo[sworkoX_.phcto-One] = _flowckX_->n6;
						}

					if( opswX_.sw[10-One] == 1 ){
						fprintf( _spec_fp,
							"           *** THE SWORKO VALUES,BEFORE VTR %5d%5d%5d%5d%5d%5d%5d%5d\n", 
						  _flowckX_->n6, sploopX_.li, _vwarg2X_->i, 
						  w50valX_.i3, _flowckX_->im1, sworkoX_.phcto, 
						  supresX_.phcts, head26X_.headwc );
						diag_write_sworko();
						scnprt();
						}

					if( _flowckX_->i3save < w50valX_.i3 )
						_flowckX_->i3save = w50valX_.i3;
					if( vtrs42X_.sw42n != 0 || _semargX_->pntr9 != 0)
						_flowckX_->i3save = w50valX_.i3;

					/* --------------  PROCESS VTR SUBROUTINE ------------------------------ */

					/*    T1VTRPRO WILL PROCESS THE VTR
					 *    RETSW WILL CONTROL WHAT WE DO ONCE WE RETURN FROM THIS ROUTINE
					 *          RETSW = 1   MEANS GO GET THE NEXT RULE
					 *          RETSW = 2   MEANS THAT WE WANT TO PROCESS THE VTR OF A  WC 9
					 *                      OR WC 10 RULE (42 SWITCH CALLS T1GETSP DIRECTLY) */

					vtrctl();
					if( errvrsX_.errlvl != 0 )
						goto L_10004;

					if( getvtrX_.getvtr != 1 )
						goto L_10005;
					getvtrX_.getvtr = 0;
					/*         SKIP LOOP CHECK IF NO MAIN WC RULE WAS FOUND.
					 *              APPLICABLE ONLY FOR WC09 AND WC10
					 *+        K7M IS ONLY 0-OR-1 IN PREVTR, WHICSP   04/22/86  *B0395DSD */
					if( !(_minickX_->k7m != 0 && _diacbX_->k7 == 0))
						break;
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
				continue;
			goto L_9900;
L_10003:
			lvl = (errvrsX_.errlvl + 4)/4;
			errvrsX_.errlvl = 0;
			if( lvl == 1 )
				goto L_9999;
			if( lvl == 2 )
				goto L_9900;
			if( lvl == 3 )
				continue;
			goto L_9900;

L_10002:
			loopckX_.nptpsv = 0;

L_795:
			if( opswX_.sw[3-One] == 1 )
				{
				fprintf( _spec_fp, "\n ***** LOOP‹ *****\n" );
				}
			_flowckX_->i3save = xx + 1;
			vtrnX_.vtrn = -1;
			vbdataX_.k3p1 = 0;
			elemld();
			if( errvrsX_.errlvl == 0 ){
				sworkoX_.sworko[sworkoX_.phcto-One][1-One] = swork1X_.swork1[_flowckX_->i3str-One][dct2X_.dct2[prctX_.js[sconX_.scolnk[_flowckX_->i3str-One]-
				  One]-One]-One];
				sworkoX_.sworko[sworkoX_.phcto-One][2-One] = swork1X_.swork1[_flowckX_->i3str-One][dct4X_.dct4[prctX_.js[sconX_.scolnk[_flowckX_->i3str-One]-
				  One]-One]-One];
				sworkoX_.sworko[sworkoX_.phcto-One][3-One] = swork1X_.swork1[_flowckX_->i3str-One][dct3X_.dct3[prctX_.js[sconX_.scolnk[_flowckX_->i3str-One]-
				  One]-One]-One];
				sworkoX_.sworko[sworkoX_.phcto-One][4-One] = _flowckX_->i3str;
				sworkoX_.phrhdo[sworkoX_.phcto-One] = _flowckX_->i3str;
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
L_10005:
		;
		}


	/*     T1MOPUP PUTS THE FINISHING TOUCHES (IF NEEDED) ONCE A SENTENCE
	 *     HAS BEEN FULLY PROCESSED. A LOT OF WRITE STATEMENTS,ETC... */
	mopup();



L_9900:
		/*   WRITE OUT THE ARRAYS TO BE COMMUNICATED TO TRAN2 FOR THIS SENTENCE */
	if( tran1_io(ADR(_l0,12)) == 0 )
		goto L_9905;
	errvrsX_.errlvl = 2;
	goto L_9999;

	/*cccccccccccccccccccccccccccccccccccccc
	 *				Error */
L_9999:
	/*				check if error allow us to continue */
	if( !(errvrsX_.errlvl < 1 || errvrsX_.errlvl > 5) )
		goto L_10000;
	fprintf( _spec_fp, "ERROR: Sentence is being skipped\n" );
	fprintf( _spec_fp, "%5ld %3ld", source_sentX_.sentid, source_sentX_.sentlng );
	fprintf( _spec_fp, " " );
	for( x=0; x < 300; x++ ){
		fprintf( _spec_fp, "%c", source_sentX_.sentbuf[x] );
		}
	fprintf( _spec_fp, "\n" );

L_9905:
	if( diagsX_.anydi == 1 ){
		fprintf( _spec_fp, "*EOS*\n" );
		}
	goto L_5;



L_10001:
	if( retsw == 99 ){
		/* clean out
		/*				close the input and output */
		if( tran1_io(ADR(_l0,3)) != 0 ){
			}
		if( tran1_io(ADR(_l0,13)) != 0 ){
			}

//cat-102 two lines added
        addMemoryState("t1 driver - LSPREADCLEAN()");
//        LSPREADCLEAN(); 
//	Now called from TranslCommonObjects::CleanupCommonObjects()
        addMemoryState("t1 driver - rule_unload()");
		rule_unload();
        addMemoryState("t1 driver - freeTrObjects()");
		freeTrObjects();
        addMemoryState("t1 driver - complete");

#ifdef TRANSTAT
	write_down_rule_stat(); 
#endif /* TRANSTAT */

        if (_spec_fp)
        {
            fclose(_spec_fp);
        }
		return lexit(0);
		}
	else{
		errvrsX_.errlvl = 2;
		}
	goto L_9999;


/*	fatal error out of program */
L_10000:
	if( tran1_io(ADR(_l0,3)) != 0 ){
		}
	if( tran1_io(ADR(_l0,13)) != 0 ){
		}
	freeTrObjects();
	return labort(2);
} /*end of function*/

