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
program. If not, write to Globalware AG, Hospitalstraße 6, D-99817 Eisenach.

Linux port modifications and additions by Bernd Kiefer, Walter Kasper,
Deutsches Forschungszentrum fuer kuenstliche Intelligenz (DFKI)
Stuhlsatzenhausweg 3, D-66123 Saarbruecken
*/

/*     GIVEN AN INPUT SEMWRK ARRAY, SEMSRC CONTROLS THE SEARCH FOR A
 *     SEMTAB MATCH AND EXECUTES THE VTR IF A MATCH IS FOUND.
 *     CALLS SEMTY  TO DO COMPLEX TYPE FIELD MATCHING.
 *     CALLS (*semsw_callback) TO EXECUTE VTR SWITCHES. */

/*     AN INDEX INTO THE RULE BASE IS CREATED FROM THE INCOMING SEMWRK.
 *     THE MATCHING PROCESS LOOPS THRU EVERY RULE WITH THAT INDEX FROM
 *     THE HIGHEST LEVEL AND SPECIFICITY TO THE LOWEST. IF NO MATCHES ARE
 *     MADE ON THAT INDEX THEN THE ROUTINE MAY LOOP THRU 2 MORE TIMES,
 *     EACH TIME CHANGING THE INDEX AS FOLLOWS:
 *       LOOP 1:   INDEX =  WC  SUBSET   SET
 *       LOOP 2:   ONLY IF SET .NE. 0
 *                 INDEX =  WC  SET      SET
 *       LOOP 3:   ONLY IF WC = 1
 *                 INDEX =  WC  SUPERSET SET    (SUPERSET FROM SCON(13)) */


/*     EACH RULE READ WITHIN THE LOOP GOES THRU A 3 STAGE MATCHING PROCES
 *     FIRST, A PREMATCH TEST IS DONE TO SCREEN OUT IMPOSSIBLE MATCHES.
 *     INPUT SEARCH PARAMETERS (DISCUSSED BELOW) ARE IMPLEMENT PRIMARILY
 *     IN THIS STAGE. ONCE PAST THE PREMATCH FILTER, THE RULE IS CHECKED
 *     FOR A MATCH AGAINST THE INPUT SEMWRK.  ANY MATCHES SUPERCEDE ALL
 *     PREVIOUS MATCHES.  THE POST MATCH TEST DECIDES WHETHER TO LEAVE
 *     THE LOOP AND EXECUTE THE RULE IMMEDIATELY OR SAVE IT AND CONTINUE
 *     SEARCHING. */

/*     IN MATCHING, A DISTINCTION IS MADE BETWEEN DEFAULT LOGOS RULES AND
 *     USER RULES.  FOR ANY GIVEN TRANSLATION THE USER SUPPLIES A LIST OF
 *     THE ONLY COMPANY CODES THAT CAN BE MATCHED.  FURTHERMORE, THE COMP
 *     CODES (CC'S) ARE LISTED IN ORDER OF PRIORITY.  THE LIST MAY HAVE
 *     FROM 0-5 CC ENTRIES.  IF EXTENDED SEARCH IS ON THEN ANY CC CAN
 *     MATCH.  IN THIS CASE, ALL UNLISTED CC'S ARE OF EQUAL PRIORITY AND
 *     OF LESSER PRIORITY THAN THOSE LISTED. */

/*     THE BASIC ASSUMPTION OF SEMTAB MATCHING IS THAT THE NATURAL SORT
 *     ORDER OF RULES IN THE DATABASE DETERMINES THEIR RELATIVE PRIORITIE
 *     THUS THE FIRST RULE TO MATCH WINS (READ FROM BOTTOM TO TOP).   A
 *     PROBLEM AROSE WHEN USERS COMPLAINED OF NOT BEING ABLE TO MATCH
 *     THEIR OWN RULES.  AMONG OTHER THINGS, THIS WAS CAUSED BY
 *     LONGER AND/OR MORE SPECIFIC LOGOS RULES MATCHING FIRST.  IN ORDER
 *     TO PLACATE THE CUSTOMER,  WE HAVE IMPLEMENTED A CONDITIONAL,
 *     CONTINUED SEARCH TO GIVE THE USER'S RULESA CHANCE TO PREEMPT
 *     A LOGOS RULE THAT MAY HAVE MATCHED FIRST. */

/*     THE CONTINUED SEARCH PROCESS WORKS AS FOLLOWS:
 *     - IF A LOGOS RULE MATCHES FIRST, THEN CONTINUE SEARCHING FOR
 *       POSSIBLE USER RULES.  IGNORE ALL OTHER LOGOS RULES; NONE
 *       CAN PREEMPT THIS RULE (THIS PRESERVES THE NATURAL SORT ORDER
 *       W/ RESPECT TO LOGOS RULES).
 *     - IF A USER RULE MATCHES FIRST, THEN CONTINUE SEARCHING
 *       FOR THE BEST USER RULE BUT DO NOT MATCH ON A
 *       LOGOS RULE. THE LIST OF COMPANY CODES SUPPLIED IN THE PROFILE
 *       DEFINES THE ORDER OF  PREFERENCE.
 *     - ALL CONTINUED SEARCHES ARE CONTROLLED BY INPUT PARAMETERS. THESE
 *       PARAMETERS GOVERN THE RESOLUTION OF CONFLICTS BETWEEN ANY
 *       TWO RULES THAT COULD OTHERWISE MATCH.  CONFLICT RESOLUTION IS
 *       BASED UPON COMPANY CODE, LEVEL, SPECIFICITY, AND ORIGINAL SORT
 *       ORDER.  IF NO CONFLICT OCCURS THEN THESE PARAMETERS WILL HAVE NO
 *       EFFECT, I.E. THEY ARE CONSIDERED ONLY AFTER THE FIRST MATCH.
 *     - CONTINUED SEARCH STOPS WHEN ALL RULES HAVING THE MATCHED INDEX
 *       HAVE BEEN EXAMINED. IN SOME CIRCUMSTANCES, IF THE APPROPRIATE
 *       INPUT PARAMETERS HAVE BEEN SET, THE SEARCH MAY STOP BEFORE
 *       LOOKING AT ALL THE RULES, BUT THIS IS SOLELY A MATTER OF
 *       MINIMIZING SEARCH TIME.  NOTE, ONCE A RULE MATCHES ANY RULE
 *       THAT PREEMPTS IT IN CONTINUED SEARCH MUST HAVE THE SAME INDEX. */


/*     THE INPUT PARAMETERS ARE EXPLAINED BELOW, BUT FIRST IT IS
 *     NECESSARY TO UNDERSTAND THE STRUCTURE AND NATURAL SORT ORDER OF
 *     A SEMTAB RULE.
 *       - A RULE MAY HAVE TEN ELEMENTS WITH A LEFT TO RIGHT ORDERING.
 *       - EACH ELEMENT IS REPRESENTED BY 3 VALUES OR FIELDS.
 *       - THE FIRST ELEMENT OF THE RULE IS REPRESENTED BY THE FIRST
 *         6 VALUES (THE FIRST 2 ELEMENTS), SO THAT THE TRUE MAXIMUM IS
 *         9 DISTINCT ELEMENTS.
 *       - THE 1ST 3 VALUES OF ELEMENT 1 ARE THE INDEX OR KEY INTO THE
 *         DATABASE FOR THAT RULE.
 *       - THE REST OF THE ELEMENTS, INCLUDING FIELDS 4,5,AND 6 OF THE
 *         INDEX ELEMENT MAKE UP THE "SP LINE".
 *       - A VALUE REPRESENTING THE LEVEL OF THE RULE (NUMBER OF ELEMENTS
 *         PRECEDES THE 2ND REAL ELMENT ON THE SP LINE.
 *       - RULES ARE SORTED BY FIELD VALUE FROM LEFT TO RIGHT AND LOWEST
 *         TO HIGHEST.
 *         EX:
 *           1  2  3   4  5  6   7     7  8  9  10  11  12  13  14  15
 *           --------  -------  -----  -------  ----------  ----------
 *            INDEX    SP LINE  LEVEL  SP LINE    SP LINE     SPLINE  ...
 *            ELM#1     ELM#1           ELM#2      ELM#3       ELM#4 */

/*            INDEX     SP LINE ------------------------------->  ... */

/*   REMEMBER, THE FOLLOWING OPTIONS AFFECT CONTINUED SEARCH ONLY ( I.E.
 *   HOW TO CARRY ON THE SEARCH ONCE A RULE HAS BEEN MATCHED). */

/* OPTION I.  LOGUSR
 * --------   --------
 *      - OPTIONS FOR RESOLVING CONFLICTS BETWEEN LOGOS & USER RULES
 *        BASED ON LEVEL AND THEN SPECIFICITY.
 *      - FAVORING A LOGOS RULE (LOGUSR > 0) WILL HAVE LIMITED EFFECT
 *        SINCE LOGOS RULES NEVER MATCH IN CONTINUED SEARCH (I.E. THEY
 *        ONLY MATCH IF THEY'RE THE FIRST FOUND). */

/*   #      EXPLANATION       (DEFAULT VALUE = 1)
 * ------ ---------------------------------------------------------------
 *        ( FAVOR USER RULE )
 *   99   ONLY USER RULES CAN MATCH
 *   20   USER RULE WINS REGARDLESS OF LEVEL (->SEARCH ALL LEVELS)
 *   1X   USER RULE WINS IF USER LEVEL >= LOGOS LEVEL - X
 *        (WHERE, 0 <= X <= 9)
 *        EX:  10 -  USER WINS IF USER LEVEL >= LOGOS LEVEL
 *        EX:  12 -  USER WINS IF USER LEVEL >= LOGOS LEVEL - 2 */

/*    1   USER RULE WINS IF USER LEVEL >  LOGOS LEVEL
 *        IF LEVELS ARE = THEN USER RULE WINS ONLY IF
 *        USER RULE SPECIFICITY** >= TO THE LOGOS RULE SPECIFICITY** */

/*        ( FAVOR NEITHER )
 *    0   FIRST TO MATCH WINS. THE SORT ORDER DECIDES RULE PRIORITY.
 *        DISREGARD LEVEL. EFFECTIVELY ELIMINATES CONTINUED SEARCH
 *        WHEN A LOGOS RULE IS THE FIRST MATCHED. */

/*        ( FAVOR LOGOS RULE )
 *  -10   LOGOS RULE WINS IF LOGOS LEVEL >= USER LEVEL
 *  -20   LOGOS RULE WINS REGARDLESS OF LEVEL (->SEARCH ALL LEVELS)
 *  -99   ONLY LOGOS RULES CAN MATCH */


/* OPTION II.  USRUSR
 * ----------  ------------
 *      - OPTIONS FOR RESOLVING MATCHING CONFLICTS BETWEEN USER RULES
 *        BASED ON LEVEL AND THEN SPECIFICITY.
 *      - USER SUPPLIES LIST OF VALID RULES BY COMPANY CODE.
 *        EXTENDED SEARCH, IF ON, APPENDS ALL CC'S TO LIST. */

/*   #    EXPLANATION        (DEFAULT VALUE = 2)
 * ------ --------------------------------------------------------------- */

/*   0    1ST USER RULE TO MATCH WINS. THE SEMTAB SORT ORDER DETERMINES
 *        PRIORITY; THE COMPANY CODE LIST JUST GIVES VALID RULES.
 *        EFFECTIVELY ELIMINATES CONTINUED SEARCH ONCE A USER RULE HAS
 *        MATCHED.
 *   1    THE 1ST VALID USER RULE TO MATCH WINS UNLESS ANOTHER HIGHER
 *        PRIORITY USER OF THE SAME OR HIGHER LEVEL AND SPECIFICITY**
 *        MATCHES.
 *   2    THE 1ST VALID USER RULE TO MATCH WINS UNLESS ANOTHER HIGHER
 *        PRIORITY USER OF THE SAME OR HIGHER LEVEL MATCHES. IGNORE
 *        SPECIFICITY**.  SEARCH THE WHOLE LEVEL FIRST MATCHED.
 *   3    USER PRIORITY LIST APPLIES REGARDLESS OF LEVEL (I.E. SEARCH ALL
 *        LEVELS FOR THE HIGHEST PRIORITY USER RULE THAT CAN MATCH). */



/* OPTION III.  EL1LVL
 * -----------  ------------------------
 *      - THIS OPTION ASKS WHICH IS MORE IMPORTANT IN DETERMINING THE
 *        PRIORITY OF RULES: 1) THE SPECIFICITY OF THE 1ST SP ELEMENT
 *        (FIELDS 4,5,6) OR  2) THE LEVEL OF THE RULE (FIELD 7).
 *      - RULES IN THE DATABASE ARE SORTED (AND READ) FIRST BY
 *        SPECIFICITY OF THE INDEX ELEMENT, NEXT BY THE LEVEL OF THE RULE
 *        AND FINALLY BY THE SPECIFICITY OF NON-INDEX ELEMENTS. THEREFORE
 *        ACCORDING TO THE NATURAL SORT ORDER THE SPECIFICITY OF THE 1ST
 *        SP ELEMENT IS MORE IMPORTANT THAN LEVEL. THIS OPTION PROVIDES
 *        THE POSSIBILITY TO EFFECTIVELY REVERSE THE IMPORTANCE OF
 *        THESE TWO SORT CRITERIA. */


/*   #    EXPLANATION        (DEFAULT VALUE = 2)
 * ------ --------------------------------------------------------------- */

/*   1    SPECIFICITY OF ELEMENT #1 CARRIES GREATER WEIGHT THAN LEVEL
 *        OF RULE. WHEN COMPARING TWO RULES, THE RULE WITH THE MORE
 *        SPECIFIC 1ST SP ELEMENT WILL ALWAYS WIN EVEN IF ITS LEVEL
 *        IS LESS THAN THAT OF THE OTHER RULE. */

/*   2    LEVEL OF RULE CARRIES GREATER WEIGHT THAN SPECIFICITY OF
 *        ELEMENT #1.  THE SPECIFICITY OF SP ELEMENT #1 IS
 *        CONSIDERED AFTER LEVEL AND BEFORE THE SPECIFICITY OF OTHER
 *        ELEMENTS. */


/* ** SPECIFICITY
 *    -----------
 * OPTIONS IV AND V.  CMPEL1, CMPELX
 * -----------------  -------------------
 *      - DEFINES THE METHOD FOR COMPARING SPECIFICITIES OF SP ELEMENTS.
 *      - CMPEL1 = COMPARING 1ST SP ELEMENT OF ANY TWO RULES.
 *        CMPELX = COMPARING ALL OTHER SP ELEMENTS OF ANY TWO RULES */

/* * AS OF 3/91 BOTH CMPEL1 AND CMPELX  ARE PRESET = 2, AND THE USER
 *              CANNOT USE AN INPUT PARAMETER TO ALTER THEIR VALUES. */


/*   #      EXPLANATION  (VALUES APPLY TO BOTH CMPEL1 AND CMPELX)
 * ------ --------------------------------------------------------------- */

/*   1     COMPARE VALUES OF THE FIELDS AS THEY APPEAR IN THE DATABASE.
 *         MAKE NO CHANGES.  THUS, WC = 3 IS MORE SPECIFIC THAN WC = 1
 *         EVEN THOUGH THERE IS NO LINGUISTIC JUSTIFICATION. */

/*   2     MEANS SPECIFICITY WHEN SORT ORDER INACCURACIES ARE COMPENSATED
 *         FOR.  DONT ALWAYS USE THE VALUE OF A FIELD AS IS.  SOMETIMES
 *         WE INTERPRET AND ALTER THE VALUES BEFORE COMPARING.
 *         THE FOLLOWING COMPENSATIONS ARE MADE AS OF 1/20/91:
 *             - TYPE FIELD:
 *                 - VALUES ARE COMPARED BY HIERARCHICAL GROUP
 *                   ( 01-16 = SUPERSET, 17-99 = SET, 100-998 = SUBSET)
 *                   NOT BY SPECIFIC VALUE. EX:  2 (SUPERSET) IS OF EQUAL
 *                   SPECIFICITY AS 16, BUT 2 IS MORE SPECIFIC THAN 17
 *                   (SET).
 *                 - SINGLE TYPE ENTRIES CAN BE MORE SPECIFIC THAN A
 *                   LIST OF TYPE VALUES EXPRESSED IN A TAG0 LIST.
 *                 - WHEN COMPARING A SINGLE TYPE VALUE TO A TAG0 LIST
 *                   THE SINGLE VALUE IS MORE SPECIFIC UNLESS ALL TAG0
 *                   VALUES ARE MORE SPECIFIC THAN THE SINGLE VALUE.
 *                   EX:  861 IS MORE SPECIFIC THAN  03,35,861,862
 *                        35  IS LESS SPECIFIC THAN  250,861,862
 *                 - TAG0 ARE CONSIDERED OF EQUAL SPECIFICITY UNLESS
 *                   ALL VALUES IN ONE LIST ARE OF GREATER SPECIFICITY
 *                   THAN ALL VALUES IN THE OTHER LIST.  EX:
 *                      03,04,55,56  =  77,89,911
 *                      03,04,55,56  <  861,862
 *             - GEN0 AND HASH0 TAGSETS ARE CONSIDERED EQUALLY SPECIFIC.
 *             - DUMMY 6000 TAGSET ELEMENTS ARE NOT INCLUDED IN THE
 *               LEVEL.  EACH 6000 TAGSET ELEMENT IS ADDED TO THE
 *               SPECIFICITY IN THE EVENT OF A TIE BETWEEN RULES.
 *             - ACTUAL TAGSET VALUES ARE NOT CURRENTLY CONSIDERED IN
 *               COMPARING SPECIFICITY. */

/*CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC */

/*   VARIABLES:
 *     NWRKS  -  # OF SEMWRK ELEMENTS PASSED.
 *     SIXTAG -  # OF DUMMY 6000 TAGSET ELEMENTS IN THE CURRENT RULE.
 *     TESTLV -  LEVEL OF RULE CURRENTLY BEING TESTED (AS VALUED IN KEY).
 *     sp5rulX_.level -  LEVEL OF CURRENT RULE BEING MATCHED ON.
 *               INCLUDES COMPENSATION FOR DUMMY 6000 TAGSET
 *               ELEMENT, WHICH WILL AFFECT SORT ORDER.
 *     sp5rulX_.company_code -  COMPANY CODE OF RULE CURRENTLY BEING MATCHED ON.
 *     USRUSR -  OPTION FOR USER VS USER CONFLICTS (JCUSUS)
 *     LOGUSR -  OPTION FOR LOGOS VS USER CONFLICTS ( JCLGUS)
 *     LUOPTS -  POSSIBLE LOGOS USER CONFLICT OPTIONS
 *     UUOPTS -  POSSIBLE USER CONFLICT OPTIONS
 *     LUDIFF -  AMOUNT BY WHICH LOGOS RULE LEVEL MUST EXCEED
 *               A USER RULE"S LEVEL IN ORDER TO WIN A CONFLICT.
 *               I.E. IF (LOGOS RULE LEVEL) - LUDIFF >= (USER RULE LEVEL)
 *                    THEN LOGOS RULE WINS.
 *               LUDIFF DERIVED FROM LOGUSR, AND NOT VALID IF LOGUSR = 0.
 *     EL1LVL -  PARM GOVERNING INDEX CONFLICT RESOLUTION.
 *     CMPEL1 -  METHOD FOR COMPARING SP1 ELEMENT SPECIFICITY.
 *     CMPELX -  METHOD FOR COMPARING NON SP1 ELEMENT SPECIFICITIES.
 *     LOADED -  NON-COMMON FLAG:  1=INPUT PARMS HAVE BEEN LOADED,0=NOT Y
 *     CHKLVL -  # OF LEVEL LAST TESTED TO STOP MATCHING.
 *     LOGCC -   SEMTAB,INTERNAL, LOGOS CC REPRESENTATION..
 *     LALIAS _  SIGHT SPECIFIC LOGOS CC ALIAS.
 *     CCLIST -  LIST OF COMP CODES ARRAY
 *               DIMENSION IT W/ SIZ PARM  IN TRPARM
 *               EQUIV IT TO LIST IN JOBCTRL FILE.
 *     jbctrlX_.jcccls_count -  PTR TO LAST CC IN USER PREFERENCE LIST
 *               AT THE BEGINNING OF THIS PROGRAM.
 *     CCPREF -  #   = POINTER TO CCLIST FOR RULE CURRENTLY TESTED.
 *                     IE. THE PREFERENCE LEVEL OF CURRENT USER RULE.
 *               0   = NOT A USER RULE.
 *               999 = USER RULE FOUND WAS NOT IN CCLIST
 *                     BUT OK CUZ EXTENDED SEARCH IS ON.
 *     SVPREF -  0   = SAVED RULE IS NOT A USER RULE.
 *               #   = THE PREFERENCE POSITION OF BEST
 *                     USER RULE FOUND. PTR TO CCLIST()
 *               999 = USER RULE FOUND WAS NOT IN CCLIST
 *                     BUT OK CUZ EXTENDED SEARCH IS ON.
 *     LOGFND -  0 = NO LOGOS RULE YET FOUND.
 *               1 = LOGOS RULE HAS BEEN MATCHED ON.
 *     save_sp5rule_ptr->level -  LEVEL OF THE SAVED(LAST FOUND) RULE,
 *               ADJUSTED FOR ANY DUMMY 6000 TAGSET ELEMENTS
 *               IN THE SPLINE.
 *               0 = NO RULE FOUND.
 *     SAVSIX -  # OF 6000 DUMMY TAGSET ELEMENTS FOUND IN
 *               THE SPLINE OF THE RULE.
 *     SV.... -  INFO ABOUT THE LAST RULE MATCHED AND SAVED. */


	/*         /SEMFLG/  =  semtab matching flags
	 *              VTRINH = 1 => inhibit execution of the VTR
	 *              SEMTCH = 1 = semtab match found */

/************************************************************************ */

/*  CHANGES */

/*     05/26/93 jal:   if error reading VTR5 file, dont abort. Just report
 *               error and return with no match.
 *     10/14/91 JAL:   SET FORMSAV OF PREPS IN A PREP-OBJECTIVE MATCH FOR
 *                     GERMAN SOURCE.
 *     03/13/91*JAL*:  MAJOR CHANGE TO ALLOW FOR SEARCH OPTIONS.
 *                    ADDED: LOGUSR,USRUSR,EL1LVL,CMPEL1,CMPELX OPTIONS.
 *     CHG: 01/09/91*JAL *
 *          ADDED EXTENDED SEARCH THRU USER RULES IF "***" IS LAST
 *          COMPANY CODE LISTED.
 *     CHG: 01/09/91*JAL *
 *          ADDED VARIABLE GUIDELINES FOR RESOLVING CONFLICTS AMONG
 *          LOGOS AND USER RULES IN MATCHING.  SEE OPTIONS LIST ABOVE.
 *          IMPLEMENTED AS PRE AND POST MATCH FILTERS.  ALSO ADDED
 *          PARAMETER CHECKING CODE TO FRONT OF PROGRAM.
 *     CHG: POST MATCH TEST IN GERMAN SOURCE ONLY       *07/12/90*JAL*
 *          IF MATCH ON WC8 OR WC2 INDEX AND AN ELEMENT WITH 01 WC AND
 *          28 FORM APPEARS IN THE RULE THEN SCON 55 OF 01 ELEMENT =94.
 *     CHG: ADDED MULTIPLE COMPANY CODE, CONTINUED SEARCH.   4/89 JAL
 *          IF USER RULE HIT FIRST CONTINUES THRU THAT LEVEL ONLY TILL
 *          THE HIGHEST PRIORITY, NON-LOGOS, COMPANY CODE FOUND
 *          IF LOGOS IS FIRST HIT THEN CONTINUES SEARCH FOR HIGHEST
 *          PRIORITY USER RULE WHICH IS MORE SPECIFIC AFTER
 *          HASH0/GEN0 NORMALIZATION.
 *      CHG: 88/07/28 LG002GBA R0GBA: MATCH COMPANY CODE SUBSET,SET
 *                  OR SUPERSET BEFORE ANY LOGOS DEFAULT RULE
 *      CHG: 04/23/87 *R1679RKH*  OFL3B CONVERSION & R1685 SWORK LIMI
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#define MS_F77
#include <fcrt.h>
#include "logoslib.h"
#include "trans.h"
#include "projexts.h"
#include "parsetrans.h"
#include <jbctrl.h>
#include "parsetrans_ext.h"

EXTERN struct t_f_sp5_rule *curr_sp5rule_ptr = &sp5rulX_;
EXTERN struct t_f_sp5_rule *save_sp5rule_ptr = &save_sp5rulX_;
EXTERN short int curr_sp5rule_level;
EXTERN short int save_sp5rule_level;
EXTERN char curr_sp5rule_company_code[3];
EXTERN char save_sp5rule_company_code[3];

typedef void (*SEMSRC_CALLBACK) (short int swnum, short sw_val1, short int *retflg);

void /*FUNCTION*/ semsrc(SEMSRC_CALLBACK semsw_callback,int *retflg)
{
	static short int chklvl, cmpel1, cmpelx, el1lvl, frmspt, i, indexkey[100], 
	  level, ludiff, nfrm, ptr, read_count, sconpt, sixtag, svgn[21], 
	  svorig[22], svsmwk[21][4], 
	  tmp, tmpswr, typ, vtrn, sw_val1;
	static long int ccpref, cmprng,  logfnd, savsix,  svpref, tempi4, testpo;
	static short neg7[8]={0,23,33,34,44,45,46,85};
	static short nwc19g[9]={115,305,309,325,380,381,382,415,750};
	static short nwc19e[26]={117,231,416,824,826,829,830,846,854,898,
		                     911,912,915,916,917,918,919,920,921,928,
							 959,965,969,973,974,979};
	static char pgmnam[9] = "SEMSRC  ";
	static long cmntct = 0;
	static long tmp4 = 0;
	static short a = 0;
	static short rule_ssu = 0;
	static short j = 0;
	static short sent_ssu = 0;
	static short l = 0;
	static short m = 0;
	static short n = 0;
	static short x = 0;
	static short gb = 0;
	static short g6 = 0;
	static short g8 = 0;
	static short iz = 0;
	static short kg = 0;
	static short ssu_units = 0;
	static short k4 = 0;
	static short k5 = 0;
	static short testlv = 0;
	static short ms = 0;
	static short rs = 0;
	static short xx = 0;
	static short xy = 0;
	static short iz2 = 0;
	static short iz3 = 0;
	static short iz4 = 0;
	static short ms2 = 0;
	static short chwc = 0;
	static short dat1 = 0;
	static short gusn = 0;
	static short newg = 0;
	static short tym1 = 0;
	static short wcm1 = 0;
	static short chfrm = 0;
	static short chtyp = 0;
	static short deact = 0;
	static short gus99 = 0;
	static short match = 0;
	static short newg6 = 0;
	static short newg8 = 0;
	static short rcode = 0;
	static short retsw = 0;
	static short rmode = 0;
	static short point1 = 0;
	static short point2 = 0;
	static short point3 = 0;
	static short postrt = 0;
	static short swapfl = 0;
	static short prepob[2]={0,0};
	static short luopts[17]={0,1,10,20,-10,-20,99,-99,11,12,13,14,15,16,17,18,19};
	static short uuopts[4]={0,1,2,3};
	static short logusr = 0;
	static short usrusr = 0;
	static short loaded = 0;

	*retflg = 0;
	semflgX_.semtch = 0;

	/*                            LOAD INPUT PARAMETERS IF NOT ALREADY DONE. */
	if( loaded == 0 ){

/*
c		set the matching control for the semtab
c		matching  semsrc.f
C I*2  JCLGUS   209  210 SEMTAB MATCHING OPTION FOR RESOLVING CONFLICTS         
C                        BETWEEn LOGOS AND USER RULES (SEE SEMSRC)              
C I*2  JCUSUS   211  212 SEMTAB MATCHING OPTION FOR RESOLVING CONFLICTS         
C                        BETWEEN USER RULES. (see semsrc)                       
C I*2  JCELLV   213  214 SEMTAB MATCHING OPTION FOR REVERSING THE IMPORTANCE OF 
C                        LEVEL AND THE 1ST SP ELEMENT IN RULE PRIORITY.     
*/
/* 11-29-99 hard set the matching parameters. Not sure if they will ever be used*/
	  jbctrlX_.jclgus = 1;                                                      
      jbctrlX_.jcusus = 2;                                                     
      jbctrlX_.jcellv = 1; 

		/*                            GET & CHECK THE MATCHING CONFLICT PARMS */
		logusr = jbctrlX_.jclgus;
		usrusr = jbctrlX_.jcusus;
		el1lvl = jbctrlX_.jcellv;


		for( i=0; i < 17; i++ ){
			if( logusr == luopts[i] )
				goto L_59;
			}
		/*                            REPORT ERROR AND SET DEFAULT */
		if( (diagsX_.longdi == 1) )
			{
			fprintf( _spec_fp, "\nILLEGAL LOGUSR OPTION OF %5d ENTERED.SETTING DEFAULT TO 0 AND CONTINUING.\n\n", 
			  logusr );
			}
		errlog(pgmnam,50,0,6);
		errvrsX_.errlvl = 0;
		logusr = 1;

L_59:
		for( i=0; i < 4; i++ ){
			if( usrusr == uuopts[i] )
				goto L_69;
			}
		/*                            REPORT ERROR AND SET DEFAULT */
		if( (diagsX_.longdi == 1) )
			{
			fprintf( _spec_fp, "\nILLEGAL USRUSR OPTION OF %5d ENTERED.SETTING DEFAULT TO 0 AND CONTINUING.\n\n", 
			  usrusr );
			}
		errlog(pgmnam,60,0,6);
		errvrsX_.errlvl = 0;
		usrusr = 2;

L_69:
		/*                       SET LUDIFF BASED ON LOGUSR */
		ludiff = 0;
		if( logusr == 1 ){
			ludiff = 1;
			}
		else if( logusr >= 10 ){
			ludiff = logusr - 9;
			}
		else if( logusr <= -10 ){
			ludiff = logusr + 10;
			}
		/*                            GET & CHECK THE MATCHING CONFLICT PARMS */
		if( el1lvl != 1 && el1lvl != 2 ){
			/*                            REPORT ERROR AND SET DEFAULT */
			if( (diagsX_.longdi == 1) )
				{
				fprintf( _spec_fp, "\nILLEGAL EL1LVL OPTION OF %5d ENTERED.SETTING DEFAULT TO 2 AND CONTINUING.\n\n", 
				  el1lvl );
				}
			errlog(pgmnam,71,0,6);
			errvrsX_.errlvl = 0;
			el1lvl = 2;
			}
		/*                           SET RANGE FOR SPECIFICITY CHECKING
		 *                           3 = ALL SP ELEMENTS, 2 = ALL BUT SP1 */
		cmprng = 2;
		if( el1lvl == 2 )
			cmprng = 3;
		/*                           GET & CHECK THE SPECIFICITY PARAMETERS
		 *                           DEFAULT SET TO COMPENSATE BEFORE COMPARING */
		cmpel1 = 2;
		cmpelx = 2;

		loaded = 1;	// first time setup done
		}




	/*-----------------------------------------------------------------
	 *                           DIAGNOSTICS */



	if( diagsX_.longdi == 1 )
	{

		fprintf( _spec_fp, "\n\n      ***SEMWRK VALUES\n      " );
		for( i=0; i < semargX_.nwrks; i++ ){
			for( x=0; x < 4; x++ ){
				fprintf( _spec_fp, "%5d", semfrmX_.semwrk[i][x] );
				}
			fprintf( _spec_fp, "       ");
			}
		fprintf( _spec_fp, "\n" );


		fprintf( _spec_fp, "\n          SEMTAB MATCHING PARAMETERS HAVE BEEN LOADED AS FOLLOWS:\n          LOGUSR = %3d USRUSR = %3dEXTENDED SEARCH = %3d LUDIFF = %3d\n          EL1LVL = %3d CMPEL1 = %3d CMPELX = %3d", 
		        logusr, usrusr, jbctrlX_.jcextendedsearch, ludiff, el1lvl, cmpel1, cmpelx );
		fprintf( _spec_fp, "\n          company codes [%2d] ",jbctrlX_.jcccls_count );
		for( i=0; i < jbctrlX_.jcccls_count; i++ ){
			fprintf( _spec_fp, "%3.3s ", jbctrlX_.jcccls[i] );
			}
		fprintf( _spec_fp, "  \n" );
	}



	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */

	/*    SEARCH SP22 RULES FOR A MATCH
	 *    START = BOTTOM RULE WITH TYPE EQUAL TO INDEX
	 *    TOTAL = TOTAL NUMBER OF RULES FOR THIS TYPE */

	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */

	/*                      INIT SAVE AND FOUND FLAGS BEFORE MATCH LOOPING. */
	sixtag = 0;
	save_sp5rule_level = 0;
	svpref = 0;
	logfnd = 0;
	chklvl = 999;

	/*   FIRST STEP IS TO GET THE BOTTOM RULE WITH THE INDEX PASSED.
	 *   TO DO THIS - DO A GENERIC READ OF THE SP FILE. */

	while( TRUE ){
		int bUsePtr=0;
		int bSavedUsePtr=0;

		l = 9;
		rmode = 1;
		indexkey[0] = srcflgX_.srcflg;
		indexkey[1] = trgflgX_.trgflg;
		indexkey[2] = semfrmX_.semwrk[0][0];
		indexkey[3] = semfrmX_.semwrk[0][1];
		indexkey[4] = semfrmX_.semwrk[0][2];

		read_count = 0;
		while( TRUE ){

			read_count += 1;

			bUsePtr=0;
			curr_sp5rule_ptr = LSPREAD_PTR(&rmode, indexkey, 
				&rcode, &sp5rulX_, &bUsePtr);

/*
			LSPREAD(&rmode,indexkey,
				&curr_sp5rule_ptr->spdummy,
				curr_sp5rule_ptr->spdata,
				curr_sp5rule_ptr->sp5vtr,
				&rcode);
*/
  
			errvrsX_.err = rcode;
			/*                        if 1 then end of rules read.
			 *					   if 2 then no rules for this key */
			if( rcode == 1 || rcode == 2 ) {
				if( save_sp5rule_level == 0 )
					break;
			}
			else if( rcode == 0 ){
				/*winnt	set read mode for read next when next read */
				rmode = 2;
				/*                       DIAGNOSTICS */
				if( diagsX_.deepdi == 1 ){
					fprintf( _spec_fp, "\n            READ IN THE FOLLOWING RULE:%7d\n", read_count );
				    sem_diag(curr_sp5rule_ptr->spkey,
						curr_sp5rule_ptr->spdata,
						curr_sp5rule_ptr->sp5vtr);
					}

				//                      IS THE RULE READ THE CORRECT INDEX ??
				if( !(curr_sp5rule_ptr->spkey[0] != semfrmX_.semwrk[0][0] || 
				      curr_sp5rule_ptr->spkey[1] != semfrmX_.semwrk[0][1]) )
					{


				/*                       THE SET FIELD CAN BE EQUAL OR A DON'T CARE ZERO */
					if( curr_sp5rule_ptr->spkey[2] != semfrmX_.semwrk[0][2] && 
					    curr_sp5rule_ptr->spkey[2] != 0 )
						continue;


					testlv = curr_sp5rule_ptr->spkey[7];
					if( testlv == 0 )
						continue;
					/***************************************************************
					 *        COMPENSATE FOR DUMMY ELEMENT 6000 TAGS WHICH ADD TO
					 *        SPRULE LEVEL WITH FAKE ELEMENT.
					 ***************************************************************
					 *                       COUNT THE NUMBER OF DUMMY 6000 TAGSETS
					 *                       IN THIS RULE. NEEDED FOR COMPARING
					 *                       SEMWRK AND RULE LEVELS. */
					if( sixtag != 0 )
						sixtag = 0;

					for( i=3; i <= testlv; i++ ){
						ptr = i*3;
						typ = curr_sp5rule_ptr->spkey[ptr+1-One];
						if( typ >= 6000 && typ < 7000 ){
							if( curr_sp5rule_ptr->spkey[ptr-One] == 0 && 
							    curr_sp5rule_ptr->spkey[ptr+2-One] == 0 )
								sixtag += 1;
							}
						}

					if( testlv > semargX_.nwrks + sixtag )
						continue;


					/*                       ALLSEM = 1 MEANS ALL SEMWRKS MUST
					 *                       MATCH (LEVEL = NWRKS+SIXTAG) */
					if( semargX_.allsem == 1 && testlv < semargX_.nwrks + sixtag )
						continue;


					/***************************************************************
					 *        PREMATCH TEST TO SEE IF RULE COULD BE VALID MATCH.
					 *        FILTER OUT RULES THAT COULD NOT MATCH BASED ON MATCH
					 *        OPTION, LEVEL, COMPANY CODE, ...
					 *        POSSIBLE MATCHES FALL THRU.
					 ***************************************************************
					 *                       GET TEST RULE LEVEL. ACCOUNT FOR 6000 TAGSET */
					curr_sp5rule_level = testlv - sixtag;
					/*                       GET THE COMPANY CODE FOR NEW TEST RULE */
					testpo = 17;
					tempi4 = testlv - 2;
					if( tempi4 > 0 )
						testpo += tempi4*6;
					tempi4 = testlv - 1;
					if( tempi4 > 0 )
						testpo += tempi4*4;
				lmove((short*)curr_sp5rule_company_code,1,
						curr_sp5rule_ptr->spkey,testpo, 3);

					/*------------
					 *            CAN WE ELIMINATE THIS RULE OR STOP SEARCHING
					 *            BY COMPARING THE SPECIFICITY IF ITS INDEX ELEMENT
					 *            SP(5-7) TO THAT OF THE RULE ALREADY MATCHED?
					 *------------ */
					if( (el1lvl == 1) && (save_sp5rule_level != 0) ){
						/*                       SAVED INDEX MUST BE MORE SPECIFIC THAN NEW INDEX */
						specmp(1,cmpel1,cmpelx, curr_sp5rule_ptr->spkey,
							curr_sp5rule_ptr->spdata,
							save_sp5rule_ptr->spkey,save_sp5rule_ptr->spdata,
							&retsw);
						if( retsw == 2 )
							goto L_24404;
						}
					if( diagsX_.deepdi == 1 )
						{
						fprintf( _spec_fp, "               THE RULE PASSES BASED ON INDEX CHECK\n" );
						}

						
						
						
						
						
					/*------------
					 *            CAN WE STOP THE SEARCH BASED ON THE LEVEL OF THE RULE
					 *            ALREADY MATCHED, IF WE CAN ASSUME LEVEL NEVER INCREASES?
					 *            THE GOAL IS ONLY TO SPEED UP THE SEARCH.
					 *             - CHECK A LEVEL ONCE (CHKLVL KEEPS TRACK).
					 *             - RULES ARE SORTED ON INDEX SP(1-3,5-7), THEN LEVEL.
					 *               THUS LEVEL ALWAYS DESCENDING FOR GIVEN INDEX. BUT IF
					 *               EL1LVL = 1  THEN LEVEL OUT OF DBASE
					 *               MAY VARY UP AND DOWN, AND WE MUST SKIP THIS ATTEMPT
					 *               TO STOP SEARCHING.
					 *             - REMEMBER, 6000 TAGSET ELEMENTS MAY CREATE
					 *               AN INFLATED, NON-DESCENDING  LEVEL COUNT.
					 *------------ */
					if( (save_sp5rule_level != 0) && (el1lvl == 1) ){
						/*                          HAVE WE CHECKED THIS LEVEL YET FOR THIS RULE? */
						if( testlv < chklvl ){
							/*                          IS SAVED RULE LOGOS? */
							if( memcmp(save_sp5rule_company_code,LOGCC,
								sizeof(save_sp5rule_company_code)) == 0 ){
								if( save_sp5rule_level - ludiff >= testlv )
									goto L_24404;
								/*                          SAVED RULE IS A USER RULE.
								 *                          IGNORE LOGOS RULES THEY CAN NO LONGER MATCH.
								 *                          CONDITIONS FOR NO MORE PREEMPTIVE USER RULES. */
								}
							else if( usrusr == 0 ){
								goto L_24404;
								}
							else if( save_sp5rule_level > testlv ){
								if( usrusr < 3 )
									goto L_24404;
								/*                          USRUSR = 3, STOP ONLY FOR HIGHEST PRIORITY
								 *                          RULE. THE LEVEL DOESNT MATTER. */
								if( svpref == 1 )
									goto L_24404;
								if( jbctrlX_.jcccls_count == 0 )
									goto L_24404;
								}
							/*                       FLAG THIS LEVEL AS CHECKED ALREADY. */
							chklvl = testlv;
							}
						}
					if( diagsX_.deepdi == 1 )
						{
						fprintf( _spec_fp, "               UNABLE TO STOP SEARCH BASED ON RULE LEVEL\n" );
						}

						
						
						
						
					/*------------
					 *            FILTER BY COMPANY CODE
					 *------------ */

					if( memcmp(curr_sp5rule_company_code,LOGCC,
						sizeof(curr_sp5rule_company_code)) == 0 ){
						/*                       LOGOS RULE CANT MATCH IF A RULE ALREADY MATCHED */
						if( save_sp5rule_level != 0 )
							continue;
						/*                       CAN LOGOS RULES MATCH AT ALL? */
						if( logusr == 99 )
							continue;
						/*                       CAN USER  RULES MATCH? */
						}
					else if( logusr == -99 ){
						continue;
						}
					else{
						/*                       PASS IT IF THIS COMPANY CODE IN THE INPUT LIST. */
						for( ccpref=1; ccpref <= jbctrlX_.jcccls_count; ccpref++ ){
							if( memcmp(jbctrlX_.jcccls[ccpref-One],
								curr_sp5rule_company_code,
								sizeof(curr_sp5rule_company_code)) == 0 )
								goto L_24407;
							}
						/*                       FALL THRU MEANS NOT A LISTED CC. 
												 so if not extended search then no match. */
						if( jbctrlX_.jcextendedsearch != 2 )
							continue;

						ccpref = 99;
						}
L_24407:
					if( diagsX_.deepdi == 1 )
						{
						fprintf( _spec_fp, "               RULE PASSES BASED ON COMPANY CODE. CCPREF = %3ld\n",  ccpref );
						}

						
						
					/*------------
					 *            NOW CHECK FOR CONFLICTS IF A MATCH HAS ALREADY BEEN MADE.
					 *            HERE WE FILTER BASED ON LEVEL,SPECIFICITY, AND COMPANY CODE
					 *            CHECK ALL POSSIBLE COMIBINATIONS OF USER AND LOGOS RULE
					 *            CONFLICTS; THERE ARE 2 CASES.
					 *            NOTE: -IF EL1LVL = 1 THEN (TESTLV) WILL NOT INCREASE IN
					 *                   SUCCESSIVE RULES, OTHERWISE LEVEL MAY VARY.
					 *                  -RULE ORDER MAY NOT DETERMINE SPECIFICITY.
					 *                   SPECMP(...) WILL COMPENSATE IF OPTIONS INDICATE.
					 *------------ */
					if( save_sp5rule_level != 0 ){
						/*1                      CASE 1:  saved = LOGOS , sp5rulX_.company_code = USER */
						if( (memcmp(save_sp5rule_company_code,LOGCC,
								sizeof(save_sp5rule_company_code)) == 0) &&
							(memcmp(curr_sp5rule_company_code,LOGCC,
								sizeof(curr_sp5rule_company_code)) != 0) ){
							if( logusr == 0 )
								continue;
							/*2                      CASE 2:  saved = USER  , sp5rulX_.company_code = USER */
							if( save_sp5rule_level - ludiff >= curr_sp5rule_level )
								continue;
							if( (logusr == 1) && (save_sp5rule_level == curr_sp5rule_level) ){
								specmp(cmprng,cmpel1,cmpelx,curr_sp5rule_ptr->spkey,
								       curr_sp5rule_ptr->spdata,save_sp5rule_ptr->spkey,save_sp5rule_ptr->spdata,&retsw);
								if( retsw == 2 )
									continue;
								}
							}
						else if( (memcmp(save_sp5rule_company_code,LOGCC,
							sizeof(save_sp5rule_company_code)) != 0) &&
							     (memcmp(curr_sp5rule_company_code,LOGCC,
									sizeof(curr_sp5rule_company_code)) != 0) ){
							/*                       USER RULES NEVER READ IN ASCENDING LEVEL ORDER */
							if( usrusr == 0 )
								continue;
							if( usrusr == 1 ){
								if( save_sp5rule_level == curr_sp5rule_level ){
									specmp(cmprng,cmpel1,cmpelx,curr_sp5rule_ptr->spkey,
									       curr_sp5rule_ptr->spdata,save_sp5rule_ptr->spkey,save_sp5rule_ptr->spdata,
										   &retsw);
									if( retsw == 2 )
										continue;
									if( retsw == 0 ){
										if( svpref < ccpref )
											continue;
										if( svpref == ccpref ){
											/*                             PASS NEW RULE ONLY IF ITS MORE SPECIFIC. */
											specmp(cmprng,cmpel1,cmpelx,
											       curr_sp5rule_ptr->spkey,curr_sp5rule_ptr->spdata,
											       save_sp5rule_ptr->spkey,save_sp5rule_ptr->spdata,&retsw);
											if( retsw != 1 )
												continue;
											}
										}
									}
								else if( save_sp5rule_level > curr_sp5rule_level ){
									continue;
									}
								}
							else if( usrusr == 2 ){
								if( save_sp5rule_level > curr_sp5rule_level )
									continue;
								if( save_sp5rule_level == curr_sp5rule_level ){
									if( svpref < ccpref )
										continue;
									if( svpref == ccpref ){
										/*                             PASS NEW RULE ONLY IF ITS MORE SPECIFIC. */
										specmp(cmprng,cmpel1,cmpelx,
										       curr_sp5rule_ptr->spkey,curr_sp5rule_ptr->spdata,
										       save_sp5rule_ptr->spkey,save_sp5rule_ptr->spdata,&retsw);
										if( retsw != 1 )
											continue;
										}
									}
								}
							else if( usrusr == 3 ){
								if( svpref < ccpref )
									continue;
								if( svpref == ccpref ){
									/*                             PASS NEW RULE ONLY IF ITS MORE SPECIFIC. */
									specmp(cmprng,cmpel1,cmpelx,curr_sp5rule_ptr->spkey,
									       curr_sp5rule_ptr->spdata,
										   save_sp5rule_ptr->spkey,
										   save_sp5rule_ptr->spdata,&retsw);
									if( retsw != 1 )
										continue;
									}
								}
							}
						}
					/*                       DIAGNOSTICS */
					if( diagsX_.deepdi == 1 )
						{
						fprintf( _spec_fp, "              THE RULE PASSES CONFLICT CHECK BASED ON LEVEL,SPECIFICITY AND COMPANY CODE.\n" );
						}




		/***************************************************************
		 *            IDENTIFY AND LOCATE PREP OBJ PAIRS FOR MATCHING
		 ***************************************************************
		 *            TYPTR IS THE POINTER TO THE FIRST TYPE FIELD
		 *            (AFTER THE SP ELEMENTS) */
		typtrX_.typtr = ((testlv - 1)*3) + 6;
		gus99 = 0;
		memset(sgnX_.gn,'\0',sizeof(sgnX_.gn));
		sgnX_.gn[0] = 1;

		prepob[0] = 0;
		prepob[1] = 0;
		if( testlv > 2 ){
			point3 = 1;
			/*            CHECK THROUGH THE WORD CLASSES FOR A WC13 FOLLOWED
			 *            BY A WC1. PREPOB (1 OR 2) WILL POINT TO THE PREP OF THE
			 *            PREP-OBJECT PAIR. */
			for( ms=2; ms <= testlv; ms++ ){
				point1 = ms*3;
				if( ms == 2 )
					point1 -= 1;
				point2 = (ms + 1)*3;

				if( curr_sp5rule_ptr->spkey[point2-One] == 1 ){
					chwc  = curr_sp5rule_ptr->spkey[point1-One];
					chtyp = curr_sp5rule_ptr->spkey[point1+1-One];
					chfrm = curr_sp5rule_ptr->spkey[point1+2-One];
					if( chwc != 13 ){
						if( chwc == -7 ){

							for( gb=1; gb <= 8; gb++ ){
								if( chfrm == neg7[gb-One] )
									goto L_1540;
								}
							continue;

							}
						else if( !(srcflgX_.srcflg != 1 || chwc != 19) ){

							for( ms2=1; ms2 <= 9; ms2++ ){
								if( chtyp == nwc19g[ms2-One] )
									goto L_1540;
								}
							continue;

							}
						else if( srcflgX_.srcflg != 2 || chwc != 19 ){
							continue;
							}
						else{
							for( ms2=1; ms2 <= 26; ms2++ ){
								if( chtyp == nwc19e[ms2-One] )
									goto L_1540;
								}
							continue;
							}
					}
L_1540:
					prepob[point3-One] = ms;
					point3 += 1;
					if( point3 > 2 )
						break;
					}
				}
			}





			/*********************************************************************
			 *   LOOP THROUGH RULE AND SEMWRK FOR A MATCH */

			/*     G = SP ELEMENT PTR
			 *     K = SEMWRK PTR
			 *     GN(G) = SEMWRK WHICH MATCHES THE G'TH ELEMENT
			 *     ORIG(GN(G)) = SWORK/NSWORK FROM WHICH MATCHED SEMWRK CREATED */

			/*     A RETURN TO THE TOP OF THE 2340 LOOP GETS A NEW ELEMENT FROM
			 *     THE CURRENT SP LINE.  A FALL THROUGH THIS LOOP MEANS A MATCH ON
			 *     THE CURRENT RULE;  VTR PROCESSING BEGINS. */

			/********************************************************************* */
			a = 2;
L_1560:
					//loop through each element of rule to find match
			for( rule_ssu=a; rule_ssu <= testlv; rule_ssu++ ){
				g8 = (rule_ssu*3) - 1;
				if( rule_ssu == 2 ) g8 -= 1;

				m = 2;
				while( TRUE ){

					for( sent_ssu=m; sent_ssu <= (semargX_.nwrks + sixtag); sent_ssu++ ){

						/*   FOR WC01 AND 02, 2ND RULE ELEMENT MUST MATCH 2ND SEMWRK */
						if( !(semfrmX_.semwrk[0][0] != 1 &&
							  semfrmX_.semwrk[0][0] != 2) ){
							if( rule_ssu == 2 && sent_ssu > 2 ) goto L_TRY_ANOTHER_RULE;
							}

						for( kg=1; kg <= 20; kg++ ){
							if( sgnX_.gn[kg-One] == sent_ssu ) goto L_ELEMENT_FAILED_MATCH;
							}

						k4 = semfrmX_.semwrk[sent_ssu-One][4-One];


			/*     THE SMALLEST LOOP - THE 2260 LOOP - TAKES THE PRESENT SEMWRK
			 *     AND THE PRESENT ELEMENT AND GOES THROUGH 3 TIMES (FOR WC,T&F) */

			/*     A RETURN TO 2260 IS A MATCH.  IN THE CASE OF A MATCH THE NEXT
			 *     RULE ELEMENT IS PROCESSED.  IN THE CASE OF A MATCH ON ALL THE
			 *     ELEMENTS IN A RULE VTR PROCESSING TAKES PLACE. */

			/*     A RETURN TO 2300 IS NO MATCH.  IN THIS CASE THE NEXT SEMWRK IS
			 *     PROCESSED WITH THE SAME RULE ELEMENT. IN THE CASE OF NO
			 *     MATCH WITH ANY OF THE SEMWRKS, A NEW RULE IS PROCESSED. */

	for( ssu_units=1; ssu_units <= 3; ssu_units++ ){
		g6 = g8 + ssu_units;
		gusn = curr_sp5rule_ptr->spkey[g6-One];

		if( gusn != 0 ){

			if( ssu_units == 2 ){		// test the TYPE
				semty(sent_ssu,k4,rule_ssu,&gusn,&match);
				if( match != 1 ) {
					if( diagsX_.deepdi == 1 )
						{
						fprintf( _spec_fp, "              Rule match fails on type field =%d for element %d\n",gusn,rule_ssu );
						}
					goto L_ELEMENT_FAILED_MATCH;
					}
				}

			else{

				if( ssu_units == 1 ){	// test the WORD CLASS

					/*   FIRST TIME AROUND  CHECK FOR WC01. 
					 *   IF CANNOT BE PRECEDED BY A PREP
					 *   (WC13) UNLESS THIS IS A PREP-OBJ RULE. */

					if( semfrmX_.semwrk[sent_ssu-One][0] == 1 ){

						if( !(prepob[0] == (rule_ssu - 1) ||
							  prepob[1] == (rule_ssu - 1)) ){

							wcm1 = semfrmX_.semwrk[sent_ssu-1-One][0];
							if( wcm1 == 13 ) goto L_ELEMENT_FAILED_MATCH;
							if( wcm1 == 19 ){
								tym1 = semfrmX_.semwrk[sent_ssu-1-One][1];

								if( srcflgX_.srcflg == 1 ){
									for( ms2=1; ms2 <= 9; ms2++ ){
										if( tym1 == nwc19g[ms2-One] ) goto L_ELEMENT_FAILED_MATCH;
									}
								}

								if( srcflgX_.srcflg == 2 ){
									for( ms2=1; ms2 <= 26; ms2++ ){
										if( tym1 == nwc19e[ms2-One] ) goto L_ELEMENT_FAILED_MATCH;
									}
								}
								}
							}
					}
				}


				if( gusn > 0 ){

					if( semfrmX_.semwrk[sent_ssu-One][ssu_units-One] != gusn ){

						if( ssu_units != 3 ) goto L_ELEMENT_FAILED_MATCH;

						/*   SUPERFORM MATCHING */
						if( gusn < 20 ) goto L_ELEMENT_FAILED_MATCH;

							/*   FORM 99 CHECKS THROUGH ALL SEMWRKS - FIRST FOR FORM ACCEPTABLE TO
							 *   THEN 76, THEN 93.
							 *    GUS99 = 2 MEANS WE ARE LOOPING THROUGH LOOKING FOR A 76
							 *    GUS99 = 1 MEANS WE ARE LOOPING THROUGH LOOKING FOR A 93
							 *    GUS99 = 0 MEANS THIS IS THE FIRST 99 WE HAVE ENCOUNTERED */
						if( gusn == 99 ){
							if( gus99 == 1 ){
									gusn = 93;
							}
							else{
								if( gus99 != 2 ){
									gus99 = 2;
									dat1 = sent_ssu;
								}
							gusn = 76;
							}
						}

						formod(3,gusn,ssu_units,sent_ssu,&retsw);
						if( retsw == 2 )goto L_ELEMENT_FAILED_MATCH;

						gus99 = 0;
						}

					/*   NEGATIVE FORM
					 *+ OFL3B conversion & R1685 Swork limit Change RKH  04/23/87   R1679 */
					}
				else if( !(ssu_units == 3 && 
		  (  (tranidX_.tranid != 1 && -gusn == sconX_.scono[sconX_.scolnk[sworkX_.phrhed[semtrX_.orig[sent_ssu-One]-One]-One]-One][45-SCONX1-One])
		   ||(tranidX_.tranid == 1 && -gusn == sconX_.scono[sconX_.scolnk[k4-One]-One][45-SCONX1-One]) )
					 ) ){

					/*   NEGATIVE WORD CLASS */
					if( ssu_units != 1 ) goto L_ELEMENT_FAILED_MATCH;
					xy = semfrmX_.semwrk[sent_ssu-One][0];

					/*    NEGWC'S COMMON TO ALL SOURCE LANGUAGES */
					if( gusn == -9 ){
						if( !(xy == 2 || xy == 12) ) goto L_ELEMENT_FAILED_MATCH;
					}
					else if( srcflgX_.srcflg == 1 ){
						if( gusn != -7 ) goto L_ELEMENT_FAILED_MATCH;
						if( !(xy == 13 || xy == 19) ) goto L_ELEMENT_FAILED_MATCH;
						}
					else if( srcflgX_.srcflg != 2 ){
						goto L_ELEMENT_FAILED_MATCH;
						}
					else if( gusn != -3 ){
						goto L_ELEMENT_FAILED_MATCH;
						}
					else if( !(xy == 3 || xy == 13) ){
						goto L_ELEMENT_FAILED_MATCH;
						}
					}
				}
			}

		} // loop through element(WC,TYPE,FORM)





			/*   IF THE ELEMENT MATCHED ON IS THE PREP OF A PREP-OBJECT PAIR
			 *   THE OBJECT MUST BE CHECKED BEFORE CONSIDERING THE PREP A MATCH.
			 *   (THE OBJECT MUST IMMEDIATELY FOLLOW THE PREP).
			 *    NO TEST FOR A FAKE OBJECT (TYPE 995). */
			if( !(prepob[0] == rule_ssu || prepob[1] == rule_ssu) ) goto L_2298;

			scommkX_.k3 = sent_ssu + 1;
			newg = rule_ssu + 1;
			newg8 = (3*newg) - 1;
			k4 = semfrmX_.semwrk[scommkX_.k3-One][4-One];

			for( k5=1; k5 <= 3; k5++ ){
				newg6 = newg8 + k5;
				gusn = curr_sp5rule_ptr->spkey[newg6-One];

				if( gusn != 0 ){
					if( k5 == 2 ){
						semty(scommkX_.k3,k4,newg,&gusn,&match);
						if( match != 1 ) goto L_ELEMENT_FAILED_MATCH;
						}
					else if( gusn > 0 ){

						if( semfrmX_.semwrk[scommkX_.k3-One][k5-One] != gusn ){

							if( k5 != 3 )goto L_ELEMENT_FAILED_MATCH;
							if( gusn < 20 )goto L_ELEMENT_FAILED_MATCH;

							formod(3,gusn,k5,scommkX_.k3,&retsw);
							if( retsw == 2 )goto L_ELEMENT_FAILED_MATCH;
							}

						/*+ OFL3B conversion & R1685 Swork limit Change RKH  04/23/87   R1679 */
						}
					else if( !(k5 == 3 && 
				  (  (tranidX_.tranid != 1 && -gusn == sconX_.scono[sconX_.scolnk[sworkX_.phrhed[semtrX_.orig[scommkX_.k3-One]-One]-One]-One][45-SCONX1-One]) 
				   ||(tranidX_.tranid == 1 && -gusn == sconX_.scono[sconX_.scolnk[k4-One]-One][45-SCONX1-One]) )
						) ){
						goto L_ELEMENT_FAILED_MATCH;
						}
					}

				}
						goto L_24410;


L_ELEMENT_FAILED_MATCH:
						;
					}
					if( gus99 != 2 )
						goto L_TRY_ANOTHER_RULE;
					gus99 = 1;

					m = dat1;
					}

L_2298:
			/*     ALL THREE SEMWRK/SP22 PARAMETERS MATCH  FOR LEVEL K */
			sgnX_.gn[rule_ssu-One] = sent_ssu;
			}
		goto L_2360;

L_24410:
			sgnX_.gn[rule_ssu-One] = sent_ssu;
			sgnX_.gn[rule_ssu+1-One] = sent_ssu + 1;
			postrt = rule_ssu + 2;
			if( postrt <= testlv ){
				a = postrt;
				goto L_1560;
			}




L_2360:
					/***************************************************************
					 *    A MATCH HAS BEEN FOUND 
					 *
					 *            EITHER SAVE RULE  AND CONTINUE SEARCH OR
					 *            EXECUTE THE VTR IMMEDIATELY.
					 *************************************************************** */

					/*                       DUMMY 6000 TAGSET ELEMENTS MEAN RULES MAY NOT
					 *                       ALWAYS SORT IN DECREASING ORDER BY LEVEL. THUS
					 *                       CASES TO STOP SEARCH ARE TOO OBSCURE. JUST SAVE
					 *                       THE RULE AND CONTINUE. */

					/*************************************************************** */

					/*                       DIAGNOSTICS OF SAVED RULE */
					if( diagsX_.longdi == 1 || diagsX_.deepdi == 1 ){
						fprintf( _spec_fp, "*** MATCHED ON THE FOLLOWING RULE ***\nSAVING IT AND CONTINUING THE SEARCH.\n" );
						sem_diag(curr_sp5rule_ptr->spkey,
							curr_sp5rule_ptr->spdata, curr_sp5rule_ptr->sp5vtr);
						}
					/*                       SAVE THE RULE AND CONTINUE SEARCHING */
					if(bUsePtr) {
						save_sp5rule_ptr = curr_sp5rule_ptr;
					} else {
//printf("SAVE MEM\n"); fflush(stdout);
						save_sp5rule_ptr = &save_sp5rulX_;
						memcpy(&save_sp5rulX_,curr_sp5rule_ptr,
							sizeof(sp5rulX_));
					}
					bSavedUsePtr = bUsePtr;

					save_sp5rule_level = curr_sp5rule_level;
					memcpy(save_sp5rule_company_code,
						curr_sp5rule_company_code,3);

					memcpy(svsmwk,semfrmX_.semwrk,sizeof(svsmwk));
					memcpy(svorig,semtrX_.orig,sizeof(svorig));
					memcpy(svgn,sgnX_.gn,sizeof(svgn));
					savsix = sixtag;
					if( memcmp(curr_sp5rule_company_code,LOGCC,
						sizeof(curr_sp5rule_company_code)) == 0 ){
						logfnd = 1;
						svpref = 0;
						}
					else{
						svpref = ccpref;
						}
					/*             NEW RULE SO CHECK THIS LEVEL FOR STOP AGAIN. */
					chklvl = 999;
					continue;
					}
				else if( save_sp5rule_level == 0 ){
					break;
					}
				}
			else{
				goto L_24405;
				}

			/***************************************************************
			 *  RESTOR SAVED RULE AND EXECUTE.
			 *        SEARCHING HALTED CUZ NO BETTER MATCH FOUND IN THE LEVEL.
			 *        CONTROL COMES HERE.
			 *************************************************************** */
L_24404:
			;
			if(bSavedUsePtr) {
				curr_sp5rule_ptr = save_sp5rule_ptr;
			} else {
//printf("RESTORE MEM\n"); fflush(stdout);
				curr_sp5rule_ptr = &sp5rulX_;
				memcpy(curr_sp5rule_ptr,&save_sp5rulX_,
					sizeof(sp5rulX_));
			}
			curr_sp5rule_level = save_sp5rule_level;
			memcpy(curr_sp5rule_company_code,save_sp5rule_company_code,3);

			memcpy(semfrmX_.semwrk,svsmwk,sizeof(semfrmX_.semwrk));
			memcpy(semtrX_.orig,svorig,sizeof(semtrX_.orig));
			memcpy(sgnX_.gn,svgn,sizeof(sgnX_.gn));

			/***************************************************************
			 *  GET THE VTR, PRINT DIAGNOSTICS, AND EXECUTE THE VTR.
			 *************************************************************** */

			semflgX_.semtch = 1;
			if( diagsX_.anydi ){
				fprintf( _spec_fp, " \n" );
				fprintf( _spec_fp, "SEMTAB MATCH. RESTORE & EXECUTE THE FOLLOWING RULE\n" );
				sem_diag(curr_sp5rule_ptr->spkey,curr_sp5rule_ptr->spdata,
					curr_sp5rule_ptr->sp5vtr);
				}


			/***************************************************************
			 *                       ;LOAD VTRF ARRAY WITH VTR22 ELEMENTS
			 *                         IZ INDICATES CURRENT ELEMENT
			 *                         26 IS MAX NUM OF VTR ELEMENTS IN ONE STRING
			 *winnt  vtr is now returned when a rule is read with spread()
			 *winnt all of the vtr is read into the SP5VTR buffer
			 *winnt no 888,888 or 999 are in the vtr. the length count
			 *winnt in the first integer*2 indicates the number of values 
			 *************************************************************** */

			if( semflgX_.vtrinh == 1 )
				goto L_24412;

			iz4 = curr_sp5rule_ptr->sp5vtr[0];
			curr_sp5rule_ptr->sp5vtr[iz4] = 999;
			memcpy(svtrfX_.svtrf,&curr_sp5rule_ptr->sp5vtr[1],iz4*2);
			 /***************************************************************
			 *  POST MATCH & PRE VTR - PROCESSING
			 ***************************************************************
			 *+                                                     *07/12/90*JAL*
			 *               FOR GERMAN SOURCE ONLY.  IF MATCH ON WC8 OR WC2 INDEX
			 *               & AN ELEMENT WITH 01 WC AND28 FORM APPEARS IN THE RULE
			 *               RULE THEN SCON 55 OF 01 ELEMENT = 94. */
			if( srcflgX_.srcflg == 1 &&
				(curr_sp5rule_ptr->spkey[0] == 2 || curr_sp5rule_ptr->spkey[0] == 8) ){
				/*         CHECK FOR WC=1,FM=28,AND TYPE.NE.0 ELEMENT IN RULE */
				level = curr_sp5rule_ptr->spkey[7];
				for( ms=2; ms <= level; ms++ ){
					ptr = ms*3;
					if( ms == 2 )
						ptr -= 1;
					if( ((curr_sp5rule_ptr->spkey[ptr-One] == 1) &&
						 (curr_sp5rule_ptr->spkey[ptr+1-One] != 0)) && 
						 (curr_sp5rule_ptr->spkey[ptr+2-One] == 28) ){
						/*                   CHANGE THE SCON55 OF SEMWRK THAT MATCHED THIS ELEMEN */
						if( tranidX_.tranid != 1){
						  sconpt = sworkX_.phrhed[semtrX_.orig[sgnX_.gn[ms-One]-One]-One];
						}
						else {
						  sconpt = semfrmX_.semwrk[sgnX_.gn[ms-One]-One][4-One];
						}
						sconX_.scono[sconX_.scolnk[sconpt-One]-One][55-SCONX1-One] = 94;
						}
					}
				}



			/*+                                                     *10/14/91*JAL*
			 *               FOR GERMAN SOURCE ONLY - SET FORMSAVE OF PREP IN
			 *               PREP-NOUN MATCHES. */
		if( tranidX_.tranid != 1){
			if( srcflgX_.srcflg == 1 && prepob[0] != 0 ){
				for( tmp=1; tmp <= 2; tmp++ ){
					if( prepob[tmp-One] != 0 ){
						nfrm = curr_sp5rule_ptr->spkey[(prepob[tmp-One]+1)*3+2-One];
						tmpswr = semtrX_.orig[sgnX_.gn[prepob[tmp-One]-One]-One];
						frmspt = sconX_.scolnk[sworkX_.phrhed[tmpswr-One]-One];
						if( (nfrm == 94 || nfrm == 84) || nfrm == 88 ){
							formsaX_.formsv[frmspt-One] = 4;
							}
						else if( (nfrm == 93 || nfrm == 83) || nfrm == 87 ){
							formsaX_.formsv[frmspt-One] = 3;
							}
						else if( (nfrm == 92 || nfrm == 82) || nfrm == 86 ){
							formsaX_.formsv[frmspt-One] = 2;
							}
						else if( (nfrm == 91 || nfrm == 81) || nfrm == 85 ){
							formsaX_.formsv[frmspt-One] = 1;
							}
						}
					}
				}
		}

			/***************************************************************
			 *  VTRF PROCESSING BEGINS
			 *************************************************************** */
			scommkX_.k3n = 1;
			while( scommkX_.k3n <= iz4 ){

				for( scommkX_.k3=scommkX_.k3n; scommkX_.k3 <= iz4; scommkX_.k3 += 2 ){
					vtrn = svtrfX_.svtrf[scommkX_.k3-One];
 					sw_val1 = svtrfX_.svtrf[scommkX_.k3+1-One];

					if( vtrn == 999 ) {  // end of vtr?
						return;
					}

					if( vtrn != -13 ){
					    goto L_24413;
					}

					(*semsw_callback)(-vtrn,sw_val1,&retsw);
	
				}
				goto L_TRY_ANOTHER_RULE;

L_24413:
				(*semsw_callback)(-vtrn,sw_val1,&retsw);
				if( vtrn == -46 && retsw == 1 )
						goto L_24414;
				if( vtrn == -47 ){
					if( errvrsX_.errlvl != 0 )
						break;
					if( retsw == 1 )
						break;
					}
			}

			return;

L_TRY_ANOTHER_RULE:
			;
			}

		//                                IF NO MATCH ON SUBSET, TRY SET
		if( scommsX_.setck == 0 ){
			if( semfrmX_.semwrk[0][2] != 0 ){
				semfrmX_.semwrk[0][1] = semfrmX_.semwrk[0][2];
				scommsX_.setck = 1;
				continue;
				}
			}

		if( semfrmX_.semwrk[0][0] != 1 )
			goto L_8380;
		if( scommsX_.setck == 2 )
			goto L_8380;
		scommsX_.setck = 2;
		if( tranidX_.tranid != 1){
			semfrmX_.semwrk[0][1] = sconX_.scon[sworkX_.phrhed[semargX_.index-One]-One][13-One];
			}
		else{
			semfrmX_.semwrk[0][1] = sconX_.scon[semargX_.index-One][13-One];
		}
		if( semfrmX_.semwrk[0][1] == 0 )
			goto L_8380;
		semfrmX_.semwrk[0][2] = semfrmX_.semwrk[0][1];
	

	}



L_24405:
	if( diagsX_.anydi != 0 ){
		fprintf( _spec_fp, " RCODE FROM LSPREAD = %6d\n", rcode );
		fprintf( stdout, " RCODE FROM LSPREAD = %6d\n", rcode );
		}
	errvrsX_.err = rcode;
	errlog(pgmnam,1500,0,1);
	return;


L_24414:
	*retflg = 1;
	return;

L_24412:
	if( diagsX_.anydi )
		{
		fprintf( _spec_fp, "               *** VTR EXECUTION HAS BEEN INHIBITED ***\n" );
		}
	return;

L_8380:
	if( srch46X_.srch46 != 1 )
		semargX_.pntr9 = 0;
	if( diagsX_.anydi )
		{
		fprintf( _spec_fp, "          SEMTAB: NO MATCH FOUND\n\n" );
		}
	return;
} /*end of function*/





/***************************************************************
 *   SPECMP -  COMPARE 2 SEMTAB RULES TO SEE WHICH IS MORE SPECIFIC.
 *             INTENDED TO COMPENSATE FOR INACCURACIES OF THE
 *             SEMTAB SORT ORDER.  DEPENDENT UPON THE INPUT PARAMETERS
 *             COMPARISON WILL BE BASED ON THE FIRST SP ELEMENT, ALL
 *             SP ELEMENTS EXCEPT THE 1ST, OR ALL SP ELEMENTS.
 *             ALSO, INPUT PARMS DETERMINE WHETHER TO COMPARE THE
 *             VALUES AS IS OR TO FOR INTERPRET THEM TO COMPENSATE
 *             FOR LINGUISTICALLY INSIGNIFICANT DIFFERENCES. */



/*        THE FOLLOWING COMPENSATIONS ARE MADE AS OF 1/20/91:
 *             - TYPE FIELD:
 *                 - VALUES ARE COMPARED BY HIERARCHICAL GROUP
 *                   ( 01-16 = SUPERSET, 17-99 = SET, 100-998 = SUBSET)
 *                   NOT BY SPECIFIC VALUE. EX:  2 (SUPERSET) IS OF EQUAL
 *                   SPECIFICITY AS 16, BUT 2 IS MORE SPECIFIC THAN 17
 *                   (SET).
 *                 - SINGLE TYPE ENTRIES CAN BE MORE SPECIFIC THAN A
 *                   LIST OF TYPE VALUES EXPRESSED IN A TAG0 LIST.
 *                 - WHEN COMPARING A SINGLE TYPE VALUE TO A TAG0 LIST
 *                   THE SINGLE VALUE IS MORE SPECIFIC UNLESS ALL TAG0
 *                   VALUES ARE MORE SPECIFIC THAN THE SINGLE VALUE.
 *                   EX:  861 IS MORE SPECIFIC THAN  03,35,861,862
 *                        35  IS LESS SPECIFIC THAN  250,861,862
 *                 - TAG0 ARE CONSIDERED OF EQUAL SPECIFICITY UNLESS
 *                   ALL VALUES IN ONE LIST ARE OF GREATER SPECIFICITY
 *                   THAN ALL VALUES IN THE OTHER LIST.  EX:
 *                      03,04,55,56  =  77,89,911
 *                      03,04,55,56  <  861,862
 *             - GEN0 AND HASH0 TAGSETS ARE CONSIDERED EQUALLY SPECIFIC.
 *             - DUMMY 6000 TAGSET ELEMENTS ARE NOT INCLUDED IN THE
 *               LEVEL.  EACH 6000 TAGSET ELEMENT IS ADDED TO THE
 *               SPECIFICITY IN THE EVENT OF A TIE BETWEEN RULES.
 *             - ACTUAL TAGSET VALUES ARE NOT CURRENTLY CONSIDERED IN
 *               COMPARING SPECIFICITY. */

/*   POSSIBLE FUTURE CHANGES:
 *         ??  - SHOULD WORD CLASSES AFFECT SPECIFICITY?
 *         ??  - SHOULD CERTAIN FORM FIELD RANGES BE CONSIDERED EQUAL?
 *         ??  - SHOULD CERTAIN TYPE FIELD RANGES BE CONSIDERED EQUAL?
 *         ??  - SHOULD TAGSET VALUES BE CONSIDERED?
 *         ??  - SHOULD TYPE FIELD < 999 BE LESS SPECIFIC THAN A TAG0? */

/*  SPECMP(CMPRNG,SPKEY1,SPDAT1,SPKEY2,SPDAT2,RETSW) */

/*  INPUT:
 *     CMPRNG  =  COMPARE RANGE, WHICH PARTS OF THE RULE TO COMPARE.
 *             = 1, COMPARE INDEX ELMENT ONLY (POSITIONS 4-6).
 *                  AND USE ACTUAL VALUES DONT CONVERT THEM (SEE **).
 *             = 2, COMPARE NON-INDEX ELEMENTS ONLY
 *                  IF THE REAL LEVELS (EXCLUDING 6000 TAGSET ELEMENTS)
 *                  ARE NOT EQUAL THE HIGHEST LEVEL IS MORE SPECIFIC.
 *             = 3, COMPARE ALL ELEMENTS
 *                  IF THE REAL LEVELS (EXCLUDING 6000 TAGSET ELEMENTS)
 *                  ARE NOT EQUAL THE HIGHEST LEVEL IS MORE SPECIFIC. */

/*     CMPEL1  =  COMPARE METHOD
 *             = 1, COMPARE THE VALUES OF EACH FIELD AS IS, WYSIWYG.
 *             = 2, COMPENSATE FOR LINGUISTICALLY INSIGNIFICANT
 *                  DIFFERENCES BEFORE COMPARING. */

/*     SPKEY1,SPKEY2 = THE SEMTAB KEYS OR RULES TO BE COMPARED.
 *     SPDAT1,SPDAT2 = THE VSAM DATA RECORDS FOR THE TWO RULES. */

/*  OUTPUT:
 *     RETSW =    0, RULES OF EQUAL SPECIFICITY
 *                1, RULE1 WAS MORE SPECIFIC OR OF GREATER LEVEL.
 *                2, RULE2 WAS MORE SPECIFIC OR OF GREATER LEVEL.
 *               -1, ERROR ENCOUNTERED AND REPORTED VIA ERRLOG. */

/*  VARIABLES:
 *    TOTLV1,2 - THE LEVEL OF EACH RULE, BEFORE 6000 TAGSETS SUBTRACTED
 *    SIXTG1,2 - THE NUMBER OF 6000 TAGSET ELEMENTS IN EACH RULE.
 *    ELMBEG   - PTR TO THE FIRST FIELD IN SPKEY1 TO COMPARE.
 *    ELMEND   - PTR TO THE LAST FIELD IN SPKEY1  TO COMPARE.
 *    TAGBG1,2 - PTR TO FIRST TAGSET VALUE IN SPKEY1,2. */

/*  CHANGES: */

/*************************************************************** */

void /*FUNCTION*/ specmp(
long cmprng, long cmpel1, long cmpelx,
short spkey1[], short spdat1[], short spkey2[], short spdat2[], short *retsw)
{
	static short int elmbeg, elmcnt, elmend, end1, end2, genhsh, i, 
	  pos, sixtg1, sixtg2, tagbg1, tagbg2, temp, totlv1, totlv2, typ1, 
	  typ2, wc1, wc2;
	static char pgmnam[9] = "SPECMP  ";

	totlv1 = spkey1[8-One];
	totlv2 = spkey2[8-One];
	/*****************
	 *                            NORMALIZE THE RULES BY IDENTIFYING
	 *                            AND SKIPPING DUMMY 6000 TAGSET ELEMENTS.
	 ***************** */
	sixtg1 = 0;
	end1 = 8 + ((totlv1 - 2)*3);
	for( i=9; i <= end1; i += 3 ){
		if( (spkey1[i-One] == 0) && (spkey1[i+2-One] == 0) ){
			if( (spkey1[i+1-One] >= 6000) && (spkey1[i+1-One] <= 6999) )
				sixtg1 += 1;
			}
		}

	sixtg2 = 0;
	end2 = 8 + ((totlv2 - 2)*3);
	for( i=9; i <= end2; i += 3 ){
		if( (spkey2[i-One] == 0) && (spkey2[i+2-One] == 0) ){
			if( (spkey2[i+1-One] >= 6000) && (spkey2[i+1-One] <= 6999) )
				sixtg2 += 1;
			}
		}
	/***************
	 *                            LEVEL COMPARISON
	 *                            IF CHECKING MORE THAN JUST INDEX ELEMENT
	 *                            THEN LEVEL MORE IMPORTANT THAN SPECIFICITY.
	 *************** */
	if( cmprng > 1 ){
		if( (totlv1 - sixtg1) > (totlv2 - sixtg2) )
			goto L_9100;
		if( (totlv1 - sixtg1) < (totlv2 - sixtg2) )
			goto L_9200;
		}


	/***************
	 *                            LOOP THRU AND COMPARE RULE ELEMENTS
	 *************** */
	elmcnt = 0;
	genhsh = 0;
	/*                            INIT FIELD INDICATOR. */
	pos = 0;
	/*                            WHICH ELEMENT STARTS & STOPS COMPARE. */
	if( cmprng == 1 ){
		elmbeg = 5;
		elmend = 7;
		}
	else if( cmprng == 2 ){
		elmbeg = 8;
		temp = (totlv1 - sixtg1) - 2;
		elmend = 8 + temp*3;
		elmcnt = 1;
		}
	else if( cmprng == 3 ){
		elmbeg = 5;
		temp = (totlv1 - sixtg1) - 2;
		elmend = 8 + temp*3;
		}
	/*                            CALCULATE WHERE TAGSETS BEGIN */
	tagbg1 = 9 + (totlv1 - 2)*3;
	tagbg2 = 9 + (totlv2 - 2)*3;

	/*                            LOOP THRU THE RULES. */

	for( i=elmbeg; i <= elmend; i++ ){
		/*                            LEVEL IS ALREADY ACCOUNTED FOR IN ALL CASES */
		if( i != 8 ){
			/*                            UPDATE FIELD POINTER (WC,TY,FM) */
			pos += 1;
			if( pos > 3 )
				pos = 1;
			/*                            THE WC FIELD, A NEW ELEMENT. */
			if( pos == 1 ){
				elmcnt += 1;
				/*                            HOW DO WE COMPARE INDEX ELEMENTS? */
				if( elmcnt == 1 ){
					if( cmpel1 == 1 )
						goto L_10000;
					/*                            HOW DO WE COMPARE NON-INDEX ELEMENTS? */
					}
				else if( cmpelx == 1 ){
					goto L_10000;
					}
				/*               WORD CLASSES ARE DIVIDED INTO THE FOLLOWING GROUPS
				 *               FROM MOST TO LEAST SPECIFIC:  POSITIVE, NEGATIVE, ZERO.
				 *               THEY ARE COMPARED AS GROUPS. EX: WC1 = WC12, WC1 > WC-9 */
				wc1 = 1;
				if( spkey1[i-One] < 1 ){
					wc1 = 0;
					if( spkey1[i-One] < 0 )
						wc1 = -1;
					}
				wc2 = 1;
				if( spkey2[i-One] < 1 ){
					wc2 = 0;
					if( spkey2[i-One] < 0 )
						wc2 = -1;
					}
				if( wc1 > wc2 )
					goto L_9100;
				if( wc2 > wc1 )
					goto L_9200;
				continue;
				}
			else if( pos == 2 ){
				/*                            HOW DO WE COMPARE INDEX ELEMENTS? */
				if( elmcnt == 1 ){
					if( cmpel1 == 1 )
						goto L_10000;
					/*                            HOW DO WE COMPARE NON-INDEX ELEMENTS? */
					}
				else if( cmpelx == 1 ){
					goto L_10000;
					}
				/*                             CHECK FOR TYPE = 0. */
				if( spkey1[i-One] == 0 ){
					if( spkey2[i-One] == 0 )
						continue;
					goto L_9200;
					}
				else if( spkey2[i-One] == 0 ){
					goto L_9100;
					}
				else{

					typ1 = spkey1[i-One]/1000;
					typ2 = spkey2[i-One]/1000;
					/*                            ONE OF THE 2 FIELDS IS A TAGSET > 1999 */
					if( !(typ1 > 1 || typ2 > 1) ){
						/*                            THE FIELDS ARE EITHER TAG0 OR SINGLE
						 *                            TYPE VALUES. */
						tagcmp(elmcnt,spkey1,spdat1,tagbg1,spkey2,
						  spdat2,tagbg2,retsw);
						/*                            IF EQUALLY SPECIFIC AS SEEN BY TAGCMP
						 *                            THEN SINGLE TYPE BEATS A TAG0 LIST. */
						if( typ1 == 0 && typ2 == 1 ){
							if( *retsw == 0 )
								*retsw = 1;
							}
						else if( typ2 == 0 && typ1 == 1 ){
							if( *retsw == 0 )
								*retsw = 2;
							}
						/*                            MORE SPECIFIC TYPE WINS */
						if( *retsw == 1 )
							goto L_9100;
						if( *retsw == 2 )
							goto L_9200;
						continue;
						/*                            GEN0 AND HSH0 ARE EQUALLY SPECIFIC */
						}
					else if( (typ1 == 3 && typ2 == 5) || (typ1 == 5 && typ2 == 3) ){
						continue;
						}
					else if( typ1 > typ2 ){
						goto L_9100;
						}
					else if( typ2 > typ1 ){
						goto L_9200;
						}
					else{
						continue;
						}
					}
				}
			/*                            COMPARE FIELDS, IF EQUAL CONTINUE */
L_10000:
			if( spkey1[i-One] > spkey2[i-One] )
				goto L_9100;
			if( spkey2[i-One] > spkey1[i-One] )
				goto L_9200;
			}
		}
	/*                            RULES ARE EQUAL UNLESS ONE HAS MORE 6000
	 *                            TAGSET ELEMENTS. */
	if( sixtg1 <= sixtg2 ){
		if( sixtg2 > sixtg1 )
			goto L_9200;

		/*                            RULES ARE EQUAL */
		*retsw = 0;
		goto L_9999;
		}
	/*                            RULE1 IS OF HIGHER PRIORITY */
L_9100:
	*retsw = 1;
	goto L_9999;
	/*                            RULE2 IS OF HIGHER PRIORITY */
L_9200:
	*retsw = 2;

L_9999:
	;
	return;
} /*end of function*/
/***************************************************************
 *   TAGCMP -  COMPARE THE TYPE FIELDS IN A SEMTAB RULE (WHETHER A
 *             SINGLE TYPE VALUE OR A TAG0 LIST OF TYPE VALUES)
 *             TO DETERMINE WHICH LIST IS MORE SPECIFIC. */

/*             - TYPE FIELD:
 *                 - VALUES ARE COMPARED BY HIERARCHICAL GROUP
 *                   ( 01-16 = SUPERSET, 17-99 = SET, 100-998 = SUBSET)
 *                   NOT BY SPECIFIC VALUE. EX:  2 (SUPERSET) IS OF EQUAL
 *                   SPECIFICITY AS 16, BUT 2 IS MORE SPECIFIC THAN 17
 *                   (SET).
 *                 - SINGLE TYPE ENTRIES CAN BE MORE SPECIFIC THAN A
 *                   LIST OF TYPE VALUES EXPRESSED IN A TAG0 LIST.
 *                 - WHEN COMPARING A SINGLE TYPE VALUE TO A TAG0 LIST
 *                   THE SINGLE VALUE IS MORE SPECIFIC UNLESS ALL TAG0
 *                   VALUES ARE MORE SPECIFIC THAN THE SINGLE VALUE.
 *                   EX:  861 IS MORE SPECIFIC THAN  03,35,861,862
 *                        35  IS LESS SPECIFIC THAN  250,861,862
 *                 - TAG0 ARE CONSIDERED OF EQUAL SPECIFICITY UNLESS
 *                   ALL VALUES IN ONE LIST ARE OF GREATER SPECIFICITY
 *                   THAN ALL VALUES IN THE OTHER LIST.  EX:
 *                      03,04,55,56  =  77,89,911
 *                      03,04,55,56  <  861,862 */

/*  TAGCMP(ELMCNT,SPKEY1,SPDAT1,SPKEY2,SPDAT2,RETSW) */

/*  INPUT:
 *     ELMCNT  =  NUMBER OF THE ELEMENT IN THE SP RULE BEINGCOMPARED.
 *     SPKEY1,SPKEY2 = THE SEMTAB KEYS OR RULES TO BE COMPARED.
 *     SPDAT1,SPDAT2 = THE VSAM DATA RECORDS FOR THE TWO RULES. */

/*  OUTPUT:
 *     RETSW =    0, TYPE FIELDS OF EQUAL SPECIFICITY
 *                1, TYPE FIELD OF RULE1 WAS MORE SPECIFIC
 *                2, TYPE FIELD OF RULE2 WAS MORE SPECIFIC
 *               -1, ERROR ENCOUNTERED AND REPORTED VIA ERRLOG. */

/*  VARIABLES:
 *    TYPHLN(X,N) -  REPRESENTS THE HIGHEST AND LOWEST VALUE IN THE
 *                   TYPE FIELD LIST FOR THE NTH RULE.
 *                   I.E.
 *                    TAGHLN(1,1) =  HIGH VALUE FOR THE 1ST RULE
 *                    TAGHLN(2,1) =  LOW  VALUE FOR THE 1ST RULE
 *                    TAGHLN(1,2) =  HIGH VALUE FOR THE 2ND RULE
 *                    TAGHLN(2,2) =  LOW  VALUE FOR THE 2ND RULE
 *    FLDPTR(N) -  THE POSITION OF THE TYPE FIELD IN SPKEY FOR THE NTH
 *                 ELEMENT IN THE RULE. */

/*  CHANGES: */

/*************************************************************** */

void /*FUNCTION*/ tagcmp(
long int elmcnt,
short int spkey1[],
short int spdat1[],
long int tagbg1,
short int spkey2[],
short int spdat2[],
long int tagbg2,
short int *retsw)
{
	static short int temp, typhln[2][2], typloc, x, y;
	static short fldptr[10]={6,10,13,16,19,22,25,28,31,33};
	static char pgmnam[9] = "TAGCMP  ";

	/*                       WHERE IS THE TYPE FIELD IN SPKEY */
	if( elmcnt < 1 || elmcnt > 10 ){
		errlog(pgmnam,10,0,6);
		errvrsX_.errlvl = 0;
		*retsw = -1;
		}
	else{
		typloc = fldptr[elmcnt-One];

		/*              FIND HIGHEST AND LOWEST TYPE VALUES FOR RULE1 */

		/*                       WE HAVE A TYPE FIELD VALUE. */
		if( spkey1[typloc-One] < 999 ){
			typhln[0][0] = spkey1[typloc-One];
			typhln[0][1] = spkey1[typloc-One];
			/*                       THE VALUE IS A TAG0. CHECK ALL TAGSET VALUES */
			}
		else{
			/*                       FIRST FROM THE SPKEY TAGSET FIELDS */
			temp = tagbg1 + (elmcnt - 1)*2;
			if( spkey1[temp-One] > spkey1[temp+1-One] ){
				typhln[0][0] = spkey1[temp-One];
				typhln[0][1] = spkey1[temp+1-One];
				}
			else{
				typhln[0][0] = spkey1[temp+1-One];
				typhln[0][1] = spkey1[temp-One];
				}
			/*                       NOW GET ANY TAGSET OVERFLOW IN SPDATA */
			temp = spdat1[16-One] + 15 + elmcnt;
			while( spdat1[temp-One] > 0 ){
				if( spdat1[temp-One] > typhln[0][0] ){
					typhln[0][0] = spdat1[temp-One];
					}
				else if( spdat1[temp-One] < typhln[0][1] ){
					typhln[0][1] = spdat1[temp-One];
					}
				temp += 1;
				}
			}
		/*              FIND HIGHEST AND LOWEST TYPE VALUES FOR RULE 2 */
		if( spkey2[typloc-One] < 999 ){
			typhln[1][0] = spkey2[typloc-One];
			typhln[1][1] = spkey2[typloc-One];
			/*                       THE VALUE IS A TAG0. CHECK ALL TAGSET VALUES */
			}
		else{
			/*                       FIRST FROM THE SPKEY TAGSET FIELDS */
			temp = tagbg2 + (elmcnt - 1)*2;
			if( spkey2[temp-One] > spkey2[temp+1-One] ){
				typhln[1][0] = spkey2[temp-One];
				typhln[1][1] = spkey2[temp+1-One];
				}
			else{
				typhln[1][0] = spkey2[temp+1-One];
				typhln[1][1] = spkey2[temp-One];
				}
			/*                       NOW GET ANY TAGSET OVERFLOW IN SPDATA */
			temp = spdat2[16-One] + 15 + elmcnt;
			while( spdat2[temp-One] > 0 ){
				if( spdat2[temp-One] > typhln[1][0] ){
					typhln[1][0] = spdat2[temp-One];
					}
				else if( spdat2[temp-One] < typhln[1][1] ){
					typhln[1][1] = spdat2[temp-One];
					}
				temp += 1;
				}
			}

		/*            NOW NORMALIZE TYPE VALUES TO ONE OF 3: SUPERSET,SET,SUBSET */

		for( x=1; x <= 2; x++ ){
			for( y=1; y <= 2; y++ ){
				if( typhln[y-One][x-One] > 99 ){
					typhln[y-One][x-One] = 3;
					}
				else if( typhln[y-One][x-One] > 16 ){
					typhln[y-One][x-One] = 2;
					}
				else if( typhln[y-One][x-One] > 0 ){
					typhln[y-One][x-One] = 1;
					}
				else{
					typhln[y-One][x-One] = 0;
					}
				}
			}

		/*            NOW COMPARE THE NORMALIZE TYPE VALUES */

		/*            IF THE LOWEST TYPE VALUE OF RULE X IS GREATER THAN THE
		 *            HIGHEST TYPE VALUE OF RULE Y THEN RULE X WINS SPECIFICITY */
		if( typhln[0][1] > typhln[1][0] ){
			*retsw = 1;
			}
		else if( typhln[1][1] > typhln[0][0] ){
			*retsw = 2;
			}
		else{
			*retsw = 0;

			}
		}
	return;
} /*end of function*/

