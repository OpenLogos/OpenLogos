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
/* CHANGES:
 *      02/06/92*JAL: SET INCOMING FORM=5 FORM HUMAN,INDEF.PRONS
 *             AND DONT CHANGE FORM FOR SING. POSSESSIV, HUM, INDEF PRON
 *      10/28/91 JAL: REMOVE FACILITY TO COMPARE RES OUTPUT TO A CONTROL
 *               OR PREVIOUS OUTPUT VIA SW(12) SEE RES2DIAG FORTRAN.
 *               MAKE SW12 PRODUCT NEW SHORT DIAGS FOR OUTPUT COMPARISON.
 *        02/29/91 JAL: RESTORE WC5 AT THE END OF RES, IF CONVERTED TO
 *             WC1 AT START OF RES, AND IF OFL4 = 05 OR 01.
 *        02/13/91 JAL: INITIAL SWORK NORMALIZATION CHANGE->
 *            ONLY IF WC=5,FORM=4,OFL4=5,OFL1.NE.802 THEN SET FORM=3
 *        89/12/14 LG002GBA R1974: WC2 SUBCHG=864 CHANGED TO
 *                                           SUBCHG=849
 *        87/08/26 LG002GBA RNORM: WC04 SUPSET 16 NORM TYPE = 21
 *        87/03/24 LG002GBA R0000: SET SUBCHG 849 FOR 01.849.33
 *                                       SET SUBCHG 848 FOR 01.849.33
 *        87/03/17 LG002GBA R1635: NOMALIZE TYPE FOR
 *                                       WC18 AND WC14 S.E.S
 *       02/23/87 *R1701GBA: NORMALIZE SCON62 VALUES 858,861 = 852
 *                              SET SCON82(92) FROM CELL57(67)
 *       02/23/87 *R1700GBA: REMOVE TEST FOR SUBCHG 849
 *       01/16/87 *R1682GBA: PASS CELL AND MISC INFO TO TRANS SCONS
 *       07/18/86 *B0415DSD: COMPRESS/EXP MAIN OUTPUT FILES PER SJP
 *       03/18/86 10:50 R1469DSD - ENG WC04 REFL PRON NEW FORM
 *       03/17/86 14:45 R1290DSD - ENG WC01, OFL1 207, OFL4 05: FORM
 *       12/19/86       R    OGM - GER WC4,TY16,OF3B=4: LEAVE ALONE. */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <configdatafileinterface/configdatainterfacemain.h>
#define MS_F77
#define EXTERN 
#include <logos_include_res_pt/fcrt.h>
#include "project.h"
#include <string.h>
#include <logos_include_res_pt/jbctrl.h>



#include "projexts.h"

// These definitions are only there because in the logos sofware obviously no
// one cares about proper modularization, which results in bizarre link
// problems (bk, Sep. 14, 2005)
int prctX_, sconX_, opadroX_, hpdopoX_, sworkX_, hfdoaX_, sworkoX_;

void res_cleanup(void)
{
   res_io_cleanup();
}

int resmain(void)
{
  static short int ci, cx, cy, ix, res1mini, res2mini, ssu_pt, su_pt, 
    swsav[70][3], work, x;
  int ssu_format_code, sent_format_code;
  static long int swork_verb_flag;
  static char pgmnam[9] = "RESMAIN ";
  static short adform[10]={11,12,13,15,16,21,22,23,25,26};
  static short ncdwc[3]={4,8,12};
  static short ncd[3]={7,11,15};
  static short nc[3]={1,2,4};
  static long zero = 0;
  static short f = 0;
  static short i = 0;
  static short k = 0;
  static short m = 0;
  static short n = 0;
  static short p = 0;
  static short w = 0;
  static short y = 0;
  static short z = 0;
  static short k2 = 0;
  static short ms = 0;
  static short ty = 0;
  static short wc = 0;
  static short ms1 = 0;
  static short one = 0;
  static short form = 0;
  static short ofl1 = 0;
  static short savy = 0;
  static short snum = 0;
  static short typ1 = 0;
  static short retsw = 0;
  static short sixty = 0;
  static short sixt1 = 0;
  static short start = 0;
  static short ecntm1 = 0;
  static short locflg = 0;
  static short thirt1 = 0;
  static short first = 0;
  static short two = 0;
  static short twelve = 0;
  static short twent1 = 0;
  static short celind[40]={1,0,3,4,5,6,7,8,22,10,11,0,13,14,15,16,17,18,32,0,51,
                           57,53,54,55,0,0,21,0,0,61,67,63,64,65,0,0,31,0,0};
  static int _aini = 1;
  char lpszBuf[MAX_FILEPATH_LEN];  

  strcpy(errvrsX_.mod, "RES     ");
  errvrsX_.untcnt = 0;

  errvrsX_.err = jcloadc("res");
  if (errvrsX_.err != 0)
    {
      printf("Fatal error loading job information.");
      exit(1);
    }

  if (GetConfigData("tempfile", "res_diag", lpszBuf, MAX_FILEPATH_LEN) != CONFDATA_SUCCESS)
    {
      exit(1);
    }
  _spec_fp = fopen(lpszBuf, "w");

  /*				load the rule bases */
  res1mini = 0;
  res2mini = 0;
  if( jbctrlX_.jcmnrs[1] == 1 )
    res2mini = 1;
  if( res_rule_load(&res1mini,&res2mini, 0) != 0 ){
    errvrsX_.errlvl = 1;
    goto L_8000;
  }

  resformod(1,x,x,&x,&x,&retsw);
  if( errvrsX_.errlvl != 0 ){
    errvrsX_.errlvl = 1;
    goto L_8000;
  }

  blkdata0();

  /*GER SRC REPLACE DATAED NEGWC ARRAYS(ENG SRC) WITH GERMAN VERSIONS */
  if( srcflgX_.srcflg == 1 ){
    memcpy(resnegX_.nwcr1,resne2X_.gnwc1,sizeof(resnegX_.nwcr1));
    memcpy(resnegX_.nwcr2,resne2X_.gnwc2,sizeof(resnegX_.nwcr2));
    memcpy(resnegX_.nwcr22,resne2X_.gnwc22,sizeof(resnegX_.nwcr22));
  }


  memset(cellX_.cell,'\0',sizeof(cellX_.cell));

  /*				open input for data from transl to res */
  if( res_io(0) != 0 ){
    errvrsX_.errlvl = 1;
    goto L_8000;
  }
  /* open output from res */
  if( res_io(10) != 0 ){
    errvrsX_.errlvl = 1;
    goto L_8000;
  }

 L_80:
  errvrsX_.untcnt += 1;
  /*                                        READ IN THE RES FILES
   *				read in sentence data from transl */
  retsw = res_io(1);
  // if error from read
  if( retsw != 0 ){
    // if end of input reached then get out clean
    if( retsw == 99 ){
      if( res_io(3) != 0 ){
      }
      if( res_io(13) != 0 ){
      }

      res_unload();
      if (_spec_fp)
        {
          fclose(_spec_fp);
        }

      return lexit(0);
    }
    else{
      fprintf( _spec_fp, "Error reading input.%hd \n", retsw );
      fprintf( stdout, "Error reading input.%hd \n", retsw );
      errvrsX_.errlvl = 1;
      goto L_8000;
    }		
  }





  for( k=1; k <= 3; k++ ){
    swX_.swork[swX_.ecount-One][k-One] = k;
  }
  memset(respasX_.respas,'\0',sizeof(respasX_.respas));
  memset(cellX_.csasav,'\0',sizeof(cellX_.csasav));
  memcpy(swX_.scont2,swX_.scont1,swX_.ecount*2);
  memset(savtyX_.subchg,'\0',sizeof(savtyX_.subchg));

  diag_check(1);

  res_diag_swork();

  res_diag_hash_hennum();

  /*	case, bold, underline, quotes are now in wstate array
        move to SUBCHG array for res
        we can set only one attribute so the order of checking
        is important.
        If the entire sentence or a word has multiple features 
        the following order gives the priority. Top to bottom in order of preference.
        quotes
        italics
        bold, underline, etc.
        all caps or all initial cap
  */
  sent_format_code = 0;
  if( wstateX_.wstate[0][0] == 1 ){		//all init cap
    sent_format_code = 850;
  }
  else if( wstateX_.wstate[0][0] == 2 ){	//all cap
    sent_format_code = 864;
  }
  else if( wstateX_.wstate[0][3] == 1 ){	//all underline
    sent_format_code = 852;
  }
  else if( wstateX_.wstate[0][4] == 1 ){	//all bold
    sent_format_code = 852;
  }
  else if( wstateX_.wstate[0][5] == 1 ){	//all italics
    sent_format_code = 861;
  }
  else if( wstateX_.wstate[0][2] == 1 ){	//all double quotes
    sent_format_code = 858;
  }
  else if( wstateX_.wstate[0][1] == 1 ){	//all single quotes
    sent_format_code = 858;
  }
  savtyX_.subchg[0][0] = sent_format_code;
  savtyX_.subchg[0][1] = sent_format_code;
  savtyX_.subchg[0][2] = sent_format_code;

  for( su_pt=1; su_pt < swX_.ecount-1; su_pt++ ){
    for( ssu_pt=1; ssu_pt <= 3; ssu_pt++ ){
      /*	must have part of speech in order to set */
      if( swX_.swork[su_pt][ssu_pt*4-One] != 0 ){
        ssu_format_code = 0;
        if( wstateX_.wstate[su_pt][2] != 0 && sent_format_code != 858){	//double  quote
          ssu_format_code = 858;
        }
        else if( wstateX_.wstate[su_pt][1] != 0 && sent_format_code != 858){  //single quote
          ssu_format_code = 858;
        }
        else if( wstateX_.wstate[su_pt][3] != 0 && sent_format_code != 852){	//underline
          ssu_format_code = 852;
        }
        else if( wstateX_.wstate[su_pt][4] != 0 && sent_format_code != 852){	//bold
          ssu_format_code = 852;
        }
        else if( wstateX_.wstate[su_pt][5] != 0 && sent_format_code != 861){	//italic
          ssu_format_code = 861;
        }
        else if( wstateX_.wstate[su_pt][0] != 0 && 
                 (sent_format_code != 850 && sent_format_code != 864)){	//capitalized
          if( wstateX_.wstate[su_pt][0] == 1 ){	//init cap															
            if( su_pt >= 2 ){		/*not initial word in sent */
              ssu_format_code = 850;		
              /*										word class = 2 */
              if( swX_.swork[su_pt][ssu_pt*4-One] == 2 )
                ssu_format_code = 851;
              /*										unfound word */
              if( wstateX_.wstate[su_pt][6] == 0 )
                ssu_format_code = 859;
            }
          }
          else if( wstateX_.wstate[su_pt][0] == 2 ){	//all cap
            ssu_format_code = 864;
            if( swX_.swork[su_pt][ssu_pt*4-One] == 2 )
              ssu_format_code = 849;
            if( wstateX_.wstate[su_pt][6] == 0 )
              ssu_format_code = 228;
          }
        }
        savtyX_.subchg[su_pt][ssu_pt-One] = ssu_format_code;
      }
    }
  }
	
  memcpy(osbchgX_.osbchg,savtyX_.subchg,sizeof(osbchgX_.osbchg));
  memcpy(savtyX_.savsup,ofltagX_.ofl4r,sizeof(savtyX_.savsup));


  /*   SWORK NORMALIZATIONS FOR RES MATCHING PROCESs */
  if( swX_.swork[swX_.ecount-One][6-One] == 9 )
    ofltagX_.ofl1r[0][0] = 909;

  memcpy(savtyX_.savsub,ofltagX_.ofl1r,sizeof(savtyX_.savsub));

  for( k=1; k <= swX_.ecount; k++ ){
    /*		determine if swork only has verbs wc 2 12 */
    if((((swX_.swork[k-One][4-One] == 2 || swX_.swork[k-One][4-One] == 12) ||
         swX_.swork[k-One][4-One] == 0) && ((swX_.swork[k-One][8-One] == 2 ||
                                             swX_.swork[k-One][8-One] == 12) || swX_.swork[k-One][8-One] == 0))
       && ((swX_.swork[k-One][12-One] == 2 || swX_.swork[k-One][12-One] == 12) ||
           swX_.swork[k-One][12-One] == 0) ){
      swork_verb_flag = 1;
    }
    else{
      swork_verb_flag = 0;
    }

    savtyX_.savwc[k-One][0] = swX_.swork[k-One][4-One];
    savtyX_.savwc[k-One][1] = swX_.swork[k-One][8-One];
    savtyX_.savwc[k-One][2] = swX_.swork[k-One][12-One];

    savtyX_.savtyr[k-One][0] = swX_.swork[k-One][5-One];
    savtyX_.savtyr[k-One][1] = swX_.swork[k-One][9-One];
    savtyX_.savtyr[k-One][2] = swX_.swork[k-One][13-One];

    // xpatfm holds the gender of each ssu
    if( (xpatnfX_.xpatfm[k-One][0] != 0) &&	(swX_.swork[k-One][4-One] != 0) )
      swX_.swork[k-One][5-One] = xpatnfX_.xpatfm[k-One][0];
    if( (xpatnfX_.xpatfm[k-One][1] != 0) &&	(swX_.swork[k-One][8-One] != 0) )
      swX_.swork[k-One][9-One] = xpatnfX_.xpatfm[k-One][1];
    if( (xpatnfX_.xpatfm[k-One][2] != 0) && (swX_.swork[k-One][12-One] != 0) )
      swX_.swork[k-One][13-One] = xpatnfX_.xpatfm[k-One][2];

    savtyX_.savfrm[k-One][0] = swX_.swork[k-One][6-One];
    savtyX_.savfrm[k-One][1] = swX_.swork[k-One][10-One];
    savtyX_.savfrm[k-One][2] = swX_.swork[k-One][14-One];


    for( f=0,p=4; p <= 12; p += 4 ){
      f += 1;

      /*   ALL LANGUAGE CODE -
       *   INTRANSITIVE VERBS (SUPERSETS 10,11,12, AND 15) BECOME TYPE 31
       *   TRANSITIVE VERBS (ALL OTHER SUPERSETS) BECOME TYPE 21 */
      if( swX_.swork[k-One][p-One] == 2 ){
        typ1 = ofltagX_.ofl4r[k-One][f-One];
        if( typ1 != 11 ){
          swX_.swork[k-One][p+1-One] = 21;
          if( (typ1 == 10 || typ1 == 12) || typ1 == 15 )
            swX_.swork[k-One][p+1-One] = 31;
          /*   SUPERSET 11 TYPE 60 IS AN EXCEPTION */
        }
        else if( swX_.swork[k-One][p+1-One] != 60 ){
          swX_.swork[k-One][p+1-One] = 31;
        }
      }


      // if source is german then ...
      if( srcflgX_.srcflg == 1 ){
					
        // If German and WC = 01 and Pat = (158,159,160) then store 13 in type field.
        if (swX_.swork[k-One][p-One] == 1 &&
            (ptdiagX_.patno[k-One][f-One] == 158 ||
             ptdiagX_.patno[k-One][f-One] == 159 ||
             ptdiagX_.patno[k-One][f-One] == 160)){
          swX_.swork[k-One][p+1-One] = 13;
        }



        /*   GERMAN-ONLY CODE - WC04 SUPERSET 013 - CERTAIN FORMS ARE CHANGED
         *   AND A FLAG IS SET TO CHANGE OFL3I WHEN IT IS LOADED IN TRATAB. */
        if( swX_.swork[k-One][p-One] == 4 ){
          if( !(ofltagX_.ofl4r[k-One][f-One] < 13 || 
                ofltagX_.ofl4r[k-One][f-One] > 16) ){
            form = swX_.swork[k-One][p+2-One];
            for( ms=1; ms <= 10; ms++ ){
              if( form == adform[ms-One] )
                goto L_280;
              /*  GBA GET RID OF OFL3R = 1 FOR VERBAL ADJECTIVES -- IT SCREWS UP OFL3B
               *     GO TO 310 */
            }
            goto L_9907;
            /*  GBA GET RID OF OFL3R = 1 FOR VERBAL ADJECTIVES -- IT SCREWS UP OFL3B */
          L_280:
            if( ms >= 6 ){
              swX_.swork[k-One][p+2-One] -= 20;
              savtyX_.savfrm[k-One][f-One] = swX_.swork[k-One][p+2-One];
              ofl2a3X_.ofl3r[k-One][f-One] = 9;
            }
            else{
              swX_.swork[k-One][p+2-One] -= 10;
              savtyX_.savfrm[k-One][f-One] = swX_.swork[k-One][p+2-One];
              ofl2a3X_.ofl3r[k-One][f-One] = 8;
            }
          }


        }
        else if( swX_.swork[k-One][p-One] == 1 ){

          if( (savtyX_.savtyr[k-One][f-One] == 81 && 
               ofltagX_.ofl4r[k-One][f-One] == 13) && 
              swX_.swork[k-One][p+2-One] == 9 ){

            /*    CHANGE WC:   01 081 09   TO   04 081 09 */
            swX_.swork[k-One][p-One] = 4;
            savtyX_.savwc[k-One][f-One] = 4;
          }
          else{

            if( swX_.swork[k-One][p+1-One] == 13 && 
                swX_.swork[k-One][p+2-One] == 6 )
              savtyX_.savfrm[k-One][f-One] = 19;
            /*-                                                         *R1065MBS
             *    VERY SPECIAL TEST FOR $IHR - MUST BE CHANGED TO WC 14 */
            if( swX_.swork[k-One][p+1-One] == 6 ){
              if( !(ofltagX_.ofl1r[k-One][f-One] != 123 ||
                    ofltagX_.ofl4r[k-One][f-One] != 6) ){
                swX_.swork[k-One][p-One] = 14;
                savtyX_.savwc[k-One][f-One] = 14;
              }
            }
          }

          /*   05 061 33 BECOMES 01 061 33, FORM BECOMES 21 FOR RES ONLY */
        }
        else if( swX_.swork[k-One][p-One] == 5 ){
          if( swX_.swork[k-One][p+2-One] == 33 ){
            if( savtyX_.savtyr[k-One][f-One] == 61 ){
              swX_.swork[k-One][p-One] = 1;
              savtyX_.savwc[k-One][f-One] = 1;
              swX_.swork[k-One][p+2-One] = 21;
              /*+                                                         *R1216MBS
               *   05 900 33 BECOMES 01 900 33 */
            }
            else if( ofltagX_.ofl1r[k-One][f-One] == 900 ){
              swX_.swork[k-One][p-One] = 1;
              savtyX_.savwc[k-One][f-One] = 1;
            }
          }
        }


        /*   ENGLISH-ONLY CODE -
         *   WC01 TYPE 004 NORMALIZED TO TYPE 007 */

      }
      else if( srcflgX_.srcflg == 2 ){
        if( swX_.swork[k-One][p-One] == 1 ){
          if( ofltagX_.ofl4r[k-One][f-One] == 4 )
            swX_.swork[k-One][p+1-One] = 7;
          if( ofltagX_.ofl4r[k-One][f-One] == 5 &&
              ofltagX_.ofl1r[k-One][f-One] == 207 ){
            if( swX_.swork[k-One][p+2-One] == 1 )
              swX_.swork[k-One][p+2-One] = 5;
            if( swX_.swork[k-One][p+2-One] == 2 )
              swX_.swork[k-One][p+2-One] = 15;

          }

          /*   FOR WC04:
           *  IF FORM OF ADJ = 14,15 : OFL3 = 4,5 : SET FORM FOR TRAN TO 23
           *  IF FORM OF ADJ = 23 : OFL3 = 2,3 : RESET FORM TO 14,15 FOR RES
           *  BECOMES WC 01, TYPES 15,16 NORMALIZED TO 13 */
        }
        else if( swX_.swork[k-One][p-One] == 4 ){
          if( swX_.swork[k-One][p+2-One] == 14 ){
            ofl2a3X_.ofl3r[k-One][f-One] = 4;
            swX_.swork[k-One][p+2-One] = 23;
            savtyX_.savfrm[k-One][f-One] = 23;

          }
          else if( swX_.swork[k-One][p+2-One] == 15 ){
            ofl2a3X_.ofl3r[k-One][f-One] = 5;
            swX_.swork[k-One][p+2-One] = 23;
            savtyX_.savfrm[k-One][f-One] = 23;
          }

          swX_.swork[k-One][p-One] = 1;
          savtyX_.savwc[k-One][f-One] = 1;
          /*+                                                         *R1105MBS
           *          IF THERE IS 1 WC02 FORM 05 IN THE SWORK - SET TYPE TO 31 */
          z = 0;
          for( y=1; y <= 3; y++ ){
            if( swX_.swork[k-One][y*4-One] == 2 ){
              if( swX_.swork[k-One][(y*4)+2-One] == 5 ){
                z += 1;
                savy = y;
              }
            }
          }

          if( z == 1 ){
            if( ofltagX_.ofl4r[k-One][savy-One] >= 10 && 
                ofltagX_.ofl4r[k-One][savy-One] <= 12 )
              swX_.swork[k-One][p+1-One] = 31;
          }
          if( ofltagX_.ofl4r[k-One][f-One] == 16 )
            swX_.swork[k-One][p+1-One] = 21;

          /*   WC05 BECOMES WC01.  FORMS 01, 02 BECOME 11, 12 */
        }
        else if( swX_.swork[k-One][p-One] == 5 ){
          swX_.swork[k-One][p-One] = 1;
          /*+                                               03/17/86  *R1469DSD */
          ofl1 = ofltagX_.ofl1r[k-One][f-One];
          /*     CHANGE FORM OF SOME REFLEXIVE PRONOUNS */
          if( ((ofl1 == 286 || ofl1 == 454) || 
               ofl1 == 581) || ofl1 == 594 ){
            swX_.swork[k-One][p+2-One] = 5;
          }
          else if( ofl1 == 415 || ofl1 == 496 ){
            swX_.swork[k-One][p+2-One] = 15;
          }
          else{
            if( swX_.swork[k-One][p+2-One] == 1 ){
              swX_.swork[k-One][p+2-One] = 11;
              if( ofltagX_.ofl4r[k-One][f-One] == 1 ){
                swX_.swork[k-One][p+2-One] = 5;
                if( ((ofl1 == 115 || ofl1 == 146) || 
                     ofl1 == 151) || ofl1 == 203 )
                  swX_.swork[k-One][p+2-One] = 17;
              }
              /*+                HUMAN INDEF PRONOUNS GET FORM = 5    *02/06/92*JAL* */
              if( ofltagX_.ofl4r[k-One][f-One] == 5 &&
                  savtyX_.savtyr[k-One][f-One] == 42 )
                swX_.swork[k-One][p+2-One] = 5;
              /*-                                                     *02/06/92*JAL* */
              if( ofl1 == 303 )
                swX_.swork[k-One][p+2-One] = 5;
            }

            if( swX_.swork[k-One][p+2-One] == 2 ){
              swX_.swork[k-One][p+2-One] = 12;
              if( ofltagX_.ofl4r[k-One][f-One] == 1){
                swX_.swork[k-One][p+2-One] = 15;
                if( ofl1 == 140 || ofl1 == 148 )
                  swX_.swork[k-One][p+2-One] = 17;
              }
              if( savtyX_.savtyr[k-One][f-One] == 22)
                swX_.swork[k-One][p+2-One] = 15;
            }
            /*-                                                         *R1159MBS */
            if( swX_.swork[k-One][p+2-One] == 4 ){
              if( !(savtyX_.savtyr[k-One][f-One] == 44 && 
                    ofltagX_.ofl1r[k-One][f-One] == 396) ){
                /*+                                                     *02/13/91*JAL* */
                if( !((ofltagX_.ofl4r[k-One][f-One] != 5) ||
                      (ofltagX_.ofl1r[k-One][f-One] == 802)) ){
                  /*-                                                     *02/13/91*JAL*
                   *+         DONT CHG FORM OF SING. POS HUM INDEF PRON   *02/06/92*JAL*
                   *-                                                     *02/06/92*JAL* */
                  if( !(ofltagX_.ofl4r[k-One][f-One] == 5 &&
                        savtyX_.savtyr[k-One][f-One] == 42) )
                    swX_.swork[k-One][p+2-One] = 3;
                }
              }
            }
          }

          /*   WC15 IS NORMALIZED TO WC14 */
        }
        else if( swX_.swork[k-One][p-One] == 15 ){
          swX_.swork[k-One][p-One] = 14;

        }
        else if( !((swX_.swork[k-One][p-One] != 3) && 
                   (swX_.swork[k-One][p-One] != 6)) ){
          swX_.swork[k-One][p-One] = 6;
          /*+           RECOGNIZE COMPARATIVE/SUPERLATIVE ADVERBS *03/14/91*JAL* */
          if( (swX_.swork[k-One][p+2-One] == 14) || 
              (swX_.swork[k-One][p+2-One] == 15) ){
            ofl2a3X_.ofl3r[k-One][f-One] = swX_.swork[k-One][p+2-One] -10;
            swX_.swork[k-One][p+2-One] = 1;
            savtyX_.savfrm[k-One][f-One] = 1;
          }
        }
        else if( swX_.swork[k-One][p-One] == 11 ){
          swX_.swork[k-One][p-One] = 13;
        }
        else if( swX_.swork[k-One][p-One] == 17 ){
          swX_.swork[k-One][p+2-One] = 13;
        }
      }
    L_9907:
      ;
    }




    /*   FOR VERBAL ELEMENTS, A SPECIAL SUBSET VALUE MUST BE CREATED. */

    if( srcflgX_.srcflg == 2 ){
      ms1 = 0;
      twelve = 0;
      twent1 = 0;
      thirt1 = 0;
      sixty = 0;
      sixt1 = 0;

      for( ms=4; ms <= 12; ms += 4 ){
        ms1 += 1;
        wc = swX_.swork[k-One][ms-One];
        ty = swX_.swork[k-One][ms+1-One];
        if( wc != 0 ){
          if( wc == 2 ){
            if( ty != 60 && ty != 61 ){
              if( ty == 21 || ty == 31 ){
                if( ty == 21 )
                  twent1 = ms1;
                if( ty == 31 )
                  thirt1 = ms1;
              }
            }
            else if( sixty == 0 ){
              sixty = ms1;
            }
            else{
              sixt1 = ms1;
            }
          }
          else if( wc == 12 ){
            twelve = ms1;
          }
          else{
            goto L_390;
          }
        }
      }
      /*                                                          *1510BT
       *  DON'T ALTER SUBCHG IF IT ALREADY HAS A VALUE */
      if( !((savtyX_.subchg[k-One][ms1-One] == 852 || 
             savtyX_.subchg[k-One][ms1-One] == 858) ||
            savtyX_.subchg[k-One][ms1-One] == 861) ){
        /*					all verb all cap */
        if(swork_verb_flag == 1 ){
          if( !(savtyX_.subchg[k-One][0] == 851 ||
                savtyX_.subchg[k-One][0] == 849)){
            if( sixty != 0 && first == 0 )
              savtyX_.subchg[k-One][sixty-One] = 847;
            if( sixt1 != 0 && first == 0 )
              savtyX_.subchg[k-One][sixt1-One] = 847;
            if( twent1 != 0 ){
              savtyX_.subchg[k-One][twent1-One] = 844;
              if( thirt1 == 0 && twelve == 0 )
                savtyX_.subchg[k-One][twent1-One] = 846;
            }
            if( thirt1 != 0 ){
              savtyX_.subchg[k-One][thirt1-One] = 845;
              if( twent1 == 0 && twelve == 0 )
                savtyX_.subchg[k-One][thirt1-One] = 847;
            }
            if( !(twelve == 0 || twelve != 1) ){
              if( twent1 == 0 && thirt1 == 0 )
                savtyX_.subchg[k-One][twelve-One] = 848;
            }
          }
          /*					  else if (savtyX_.subchg[k-One][0] == 851){
                                                  savtyX_.subchg[k-One][0] = 848;
                                                  }
          */
        }
      }




      /*   IF THIS IS ONLY A WC01, CREATE ANOTHER SUBSET VALUE */
    L_390:
      ;

      /*   CHECK EACH OF THE THREE NODES FOR THIS SENTENCE ELEMENT.
       *   IF A NODE IS 01.849.33 OF 01.848.33
       *   (IE. AN ARTIFICIALLY GENERATED NODE)
       *   SET ITS SUBCHANGE TO 849
       *   UNLESS, OF COURSE, THE SUBCHANGE IS ALREADY 852, 861, OR 861
       *   (IE. HIGHLIGHT, CITATION, ... )
       *   IN WHICH CASE, THE SUBCHANGE IS NOT TO BE ALTERED. */

      for( f=0,p=4; p <= 12; p += 4 ){
        f += 1;
        work = ofltagX_.ofl1r[k-One][f-One];
        if( work == 849 || work == 848 ){
          if( swX_.swork[k-One][p-One] == 1 &&
              swX_.swork[k-One][p+2-One] == 33 ){
            if( (savtyX_.subchg[k-One][f-One] != 852 &&
                 savtyX_.subchg[k-One][f-One] != 858) &&
                savtyX_.subchg[k-One][f-One] != 861)
              savtyX_.subchg[k-One][f-One] = work;
          }
        }
      }
      /*  IF A SENTENCE ELEMENT HAS BOTH A WC14 NODE AND A WC18 NODE
       *  THEN GIVE EACH THE APPROPRIATE NORMALIZED TYPE
       *  IE. WC18 TYPE 14; WC14 TYPE 18 */
      if( (swX_.swork[k-One][4-One] == 14 ||
           swX_.swork[k-One][8-One] == 14) ||
          swX_.swork[k-One][12-One] == 14 ){
        if( (swX_.swork[k-One][4-One] == 18 ||
             swX_.swork[k-One][8-One] == 18) ||
            swX_.swork[k-One][12-One] == 18 ){
          for( ix=4; ix <= 12; ix += 4 ){
            if( swX_.swork[k-One][ix-One] == 14 )
              swX_.swork[k-One][ix+1-One] = 18;
            if( swX_.swork[k-One][ix-One] == 18 )
              swX_.swork[k-One][ix+1-One] = 14;
          }
        }
      }
    }
  }


  /*       LOOP CONTROLS CYCLE THROUGH SCONTI
   *       (SYNTAX CONTROL ARRAY FOR SENTENCE) */

  for( m=1; m <= swX_.ecount; m++ ){
    for( x=1; x <= 3; x++ ){
      swX_.swork[m-One][x-One] = 1;
      if( swX_.swork[m-One][ncdwc[x-One]-One] == 0 )
        swX_.swork[m-One][x-One] = -1;
    }
  }



  for( m=1; m <= swX_.ecount; m++ ){
    for( x=1; x <= 3; x++ ){
      swsav[m-One][x-One] = swX_.swork[m-One][x-One];
    }
  }

  tag63X_.srch63 = 0;
  srch07X_.srch07 = 0;

  if( opswX_.sw[8-One] == 1 )
    {
      fprintf( _spec_fp, "\n\n*RES1  START*\n\n" );
      res_diag_swork();
    }


  resswX_.ressw = 1;

  reslv2(1,&retsw);
  if( errvrsX_.errlvl >= 1 && errvrsX_.errlvl <= 4 )
    goto L_8000;
  if( errvrsX_.errlvl == 0 ){

    /*   RES1 CHANGES ARE FOR RES1 ONLY UNLESS A -13 SWITCH WAS
     *   IMPLEMENTED FOR THE NODE, IN WHICH CASE SCONT2 WAS SET TO 4 */
    for( m=1; m <= swX_.ecount; m++ ){

      if( swX_.scont2[m-One] >= 4 ){
        swX_.scont2[m-One] = swX_.scont3[m-One];
      }
      else{
        for( x=1; x <= 3; x++ ){
          swX_.swork[m-One][x-One] = swsav[m-One][x-One];
        }
      }

    }


    /*                                                          *J0195
     *   INITIALIZE CELLS
     *      BUT NOT CELLS 40-44 R1491 */

    zapit(cellX_.cell,78,(byte)0);
    zapit(&cellX_.cell[45-One],112,(byte)0);
    cellX_.cell[0] = 1;
    cellX_.cell[2] = 1;
    /*+                                                         *R1127MBS
     *          REMOVE CELL(13) SETTING
     *-                                                         *R1127MBS */

    /*   VSERCH ROUTINE SEARCHES FOR VERB TO DETERMINE CELL(2) SETTING */
    start = 1;
    snum = 0;
    vserch(start,snum,&retsw);
    if( retsw == 1 )
      cellX_.cell[1] = 1;

    for( m=1; m <= swX_.ecount; m++ ){
      lmove(&cellX_.csaray[m-One][0],1,cellX_.cell,1,80);
    }


    resswX_.ressw = 2;
    if( opswX_.sw[8-One] == 1 )
      {
        fprintf( _spec_fp, "\n\n*RES2 START*\n\n" );
        res_diag_swork();
      }

    reslv2(1,&retsw);
    if( errvrsX_.errlvl != 0 )
      goto L_8000;
  }
  else{
    errvrsX_.errlvl = 0;
  }




  memcpy(ofltagX_.ofl4r,savtyX_.savsup,sizeof(ofltagX_.ofl4r));
  memcpy(ofltagX_.ofl1r,savtyX_.savsub,sizeof(ofltagX_.ofl1r));

  for( k=1; k <= swX_.ecount; k++ ){
    /*+   ADD TO PASS TO TRANS SCONS                            *R1682GBA */
    if( srcflgX_.srcflg == 2 ){
      ambfac(savtyX_.savwc[k-One][0],swX_.swork[k-One][5-One],
             savtyX_.savfrm[k-One][0],ofltagX_.ofl4r[k-One][0],
             savtyX_.savwc[k-One][1],swX_.swork[k-One][9-One],
             savtyX_.savfrm[k-One][1],ofltagX_.ofl4r[k-One][1],
             savtyX_.savwc[k-One][2],swX_.swork[k-One][13-One],
             savtyX_.savfrm[k-One][2],ofltagX_.ofl4r[k-One][2],
             &scncelX_.scncel[k-One][12-One]);
    }
    /*-   ADD TO PASS TO TRANS SCONS                            *R1682GBA */

    /*     RESTORE ORIGINAL SWORK VALUES WHERE NECESSARY. */

    /*            RESTORE ORIGINAL WC
     *+           DONT RESTORE WC5 UNLESS OFL4 = 5 OR 1    *02/28/91*JAL* */
    if( savtyX_.savwc[k-One][0] == 5 ){
      if( ofltagX_.ofl4r[k-One][0] != 1 &&
          ofltagX_.ofl4r[k-One][0] != 5 )
        goto L_1026;
    }
    swX_.swork[k-One][4-One] = savtyX_.savwc[k-One][0];

  L_1026:
    ;
    if( savtyX_.savwc[k-One][1] == 5 ){
      if( ofltagX_.ofl4r[k-One][1] != 1 &&
          ofltagX_.ofl4r[k-One][1] != 5 )
        goto L_1027;
    }
    swX_.swork[k-One][8-One] = savtyX_.savwc[k-One][1];

  L_1027:
    ;
    if( savtyX_.savwc[k-One][2] == 5 ){
      if( ofltagX_.ofl4r[k-One][2] != 1 &&
          ofltagX_.ofl4r[k-One][2] != 5 )
        goto L_1028;
    }
    swX_.swork[k-One][12-One] = savtyX_.savwc[k-One][2];
  L_1028:
    ;

    swX_.swork[k-One][5-One] = savtyX_.savtyr[k-One][0];
    swX_.swork[k-One][9-One] = savtyX_.savtyr[k-One][1];
    swX_.swork[k-One][13-One] = savtyX_.savtyr[k-One][2];

    swX_.swork[k-One][6-One] = savtyX_.savfrm[k-One][0];
    swX_.swork[k-One][10-One] = savtyX_.savfrm[k-One][1];
    swX_.swork[k-One][14-One] = savtyX_.savfrm[k-One][2];

    /*+   ADD TO PASS TO TRANS SCONS                            *R1682GBA */
    for( cx=1; cx <= 40; cx++ ){
      if( celind[cx-One] != 0 ){
        ci = celind[cx-One];
        if( ci > 40 ){
          scncelX_.scncel[k-One][cx-One] = cellX_.csasav[k-One][ci-50-One];
        }
        else{
          scncelX_.scncel[k-One][cx-One] = cellX_.csaray[k-One][ci-One];
        }
      }
    }
    cy = 0;
    for( cx=1; cx <= 3; cx++ ){
      if( swX_.swork[k-One][cx-One] == 1 ){
        scncelX_.scncel[k-One][1] = savtyX_.subchg[k-One][cx-One];
        /*+  NORMALIZE SCON62 VALUES 858 AND 861 TO 852            *R1701*GBA */
        if( scncelX_.scncel[k-One][1] == 858 )
          scncelX_.scncel[k-One][1] = 852;
        if( scncelX_.scncel[k-One][1] == 861 )
          scncelX_.scncel[k-One][1] = 852;
      }
      else if( srcflgX_.srcflg == 2 ){
        scncelX_.scncel[k-One][20+cy-One] = swX_.swork[k-One][4+4*(cx-1)-One];
        scncelX_.scncel[k-One][26+cy-One] = ofltagX_.ofl4r[k-One][cx-One];
        scncelX_.scncel[k-One][27+cy-One] = swX_.swork[k-One][5+4*(cx-1)-One];
        scncelX_.scncel[k-One][29+cy-One] = swX_.swork[k-One][6+4*(cx-1)-One];
        cy += 10;
      }
    }
  }



  /* change RESPAS values for hi-light, citation or quotation conditions */
  chgpas();



  if( opswX_.sw[2] == 1 || diagsX_.shrtdi != 0 ){

    fprintf( _spec_fp,"\n*RES END* \n");
    res_diag_swork();

    fprintf( _spec_fp, "\n      RESPAS   FOR ELEMENT # \n" );
    for( x=1; x <= 70; x++ ){
      w = 0;
      for( y=1; y <= 3; y++ ){
        if( respasX_.respas[x-One][y-One] != 0 )
          w = 1;
      }
      if( w != 0 )
        {
          fprintf( _spec_fp, "                        %4d  = ", x );
          for( y=1; y <= 3; y++ ){
            fprintf( _spec_fp, "%6d", respasX_.respas[x-One][y-One] );
          }
          fprintf( _spec_fp, "\n" );
        }
    }

    res_diag_clause_status();


    fprintf( _spec_fp, "\n\n*SCON CELL ARRAY*\n" );
    for( cx=0; cx < swX_.ecount; cx++ ){
      fprintf( _spec_fp, "%2d ", cx+1 );
      for( cy=0; cy < 20; cy++ ){
        fprintf( _spec_fp, "%4d", scncelX_.scncel[cx][cy] );
      }
      fprintf( _spec_fp, "\n   " );
      for( cy=20; cy < 40; cy++ ){
        fprintf( _spec_fp, "%4d", scncelX_.scncel[cx][cy] );
      }
      fprintf( _spec_fp, " \n" );
    }
    fprintf( _spec_fp, " \n" );
  }




  //			Write output for this sentence.
  if( res_io(12) != 0 )
    goto L_8000;

  //				translatability index info. (NOT ACTIVE)
  //winnt    if( opswX_.sw[8-One] == 3 )			tix(&retsw);


 L_9905:
  if(opswX_.sw[2] != 0 || diagsX_.shrtdi != 0 )
    {
      fprintf( _spec_fp, "\n*EOS*\n" );
    }
  // back up for next sentence
  goto L_80;

  ///////////////////////////////////////////////////////////
  /*			ERROR ENCOUNTERED
   */
 L_8000:
  /*				check if error allow us to continue - soft error? */
  if(errvrsX_.errlvl > 5) 
    {
      fprintf( _spec_fp, "ERROR: Sentence is being skipped\n" );
      fprintf( _spec_fp, "%5ld %3ld ", source_sentX_.sentid, source_sentX_.sentlng );
      for( x=1; x <= 300; x++ ){
        fprintf( _spec_fp, "%c", source_sentX_.sentbuf[x-One] );
      }
      fprintf( _spec_fp, "\n" );
      goto L_9905;
    }
  // FATAL ERROR =  GET OUT	
  /*				close the input and output */
  if( res_io(3) != 0 ){
  }
  if( res_io(13) != 0 ){
  }

  return labort(1);

} /*end of function*/
//////////////////////////////////////////////////////////////////////



/*ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 *  DATE:     6/6/86
 *  AUTHOR:   Teresita Young
 *  REMARKS:  This subroutine tests the SUBCHG code for one of three
 *            codes indicating the original sentence element was either
 *            hi-lighted, part of a citation or part of a quotation.
 *            If one of these conditions is found then the third element
 *            of the RESPAS array is set to one of three values
 *            indicating whether the element of the sentence was the
 *            at the beginning, middle, end or the only part of the
 *            sentence contained within the hi-lighting, citation or
 *            quotation. */



void /*FUNCTION*/ chgpas()
{
	static short int count, i, j, k, m, subcnt[3], temp;
	static short zeros = 0;
	static short subcod[3]={852,858,861};
	static short newcod[3][4]={11,12,13,19,21,22,23,29,31,32,33,39};

	memset(subcnt,'\0',sizeof(subcnt));

	/*  for each element test if there is a code match
	 *
	 *    I  points to the element of the sentence
	 *    J  points to the condition codes (hilite, citation, quotation)
	 *    K  points to the condition codes also
	 *
	 * */
	for( i=1; i <= swX_.ecount; i++ ){
		for( j=1; j <= 3; j++ ){
			/*+                                                        *R0GBA*GBA
			 *           IF (SUBCHG(JGUS(I),I) .EQ. SUBCOD(J)) THEN */
			if( (savtyX_.subchg[i-One][0] == subcod[j-One] || 
			  savtyX_.subchg[i-One][1] == subcod[j-One]) || 
			  savtyX_.subchg[i-One][2] == subcod[j-One] ){
				/*-                                                        *R0GBA*GBA */
				subcnt[j-One] += 1;

				/*  test if one of the other conditions have ended
				 * */
				for( k=1; k <= 3; k++ ){
					if( k != j ){
						count = subcnt[k-One];
						if( count > 0 ){
							if( count == 1 )
								temp = 4;
							if( count > 1 )
								temp = 3;
							respasX_.respas[i-1-One][2] = newcod[k-One][temp-One];
							subcnt[k-One] = 0;
							}
						}
					}


				if( respasX_.respas[i-One][2] == 0 ){
					count = subcnt[j-One];
					if( count == 1 )
						temp = 1;
					if( count > 1 )
						temp = 2;
					respasX_.respas[i-One][2] = newcod[j-One][temp-One];
					}

				goto L_100;
				}
			}


		/*  there is no match on the codes
		 *   check if a condition has been concluded
		 * */
		for( m=1; m <= 3; m++ ){
			count = subcnt[m-One];
			if( count > 0 ){
				if( count == 1 )
					temp = 4;
				if( count > 1 )
					temp = 3;
				respasX_.respas[i-1-One][2] = newcod[m-One][temp-One];
				subcnt[m-One] = 0;
				goto L_100;
				}
			}


L_100:
		;
		}

	return;
} /*end of function*/
//////////////////////////////////////////////////////////////////////


/*  DATE:     Jan 16, 1987
 *  AUTHOR:   Glenn Atkinson
 *  REMARKS:  This subroutine tests the Word Class, Type, Form, supset
 *            fields of the three SWORK entires, passed from
 *            RESMAIN.  From these nine fields, AMBFAC determines
 *            a numeric ambiguity factor code and returns the
 *            code in the last passed field. */

/* CHANGES:
 *  08/09/93*JAL*: add supset(ofl4r) to parameter list and include it
 *                 as one of the factors for determing ambiguity pattern.
 *  08/09/93*JAL*: add pattern (RESULT) = 39 .
 *  08/20/91*JAL*: IF WC1=03/06, WC2=13, WC3=2, TY3=21 --> RESULT=38 */



void /*FUNCTION*/ ambfac(wc1, ty1, fm1, sup1, wc2, ty2, fm2, sup2, 
	  wc3, ty3, fm3, sup3, result)
long int wc1, ty1, fm1, sup1, wc2, ty2, fm2, sup2, wc3, ty3, fm3, 
	  sup3;
short int *result;
{

	*result = 0;
	if( wc2 == 0 && wc3 == 0 ){
		*result = 1;
		}
	else if( wc1 == 1 ){
		/* ***
		 * ***  WC1 EXCLUDING FM1 = 23, OR 50, OR 54, OR 60, OR 70
		 * *** */
		if( (((fm1 != 23 && fm1 != 50) && fm1 != 54) && fm1 != 60) && 
		  fm1 != 70 ){
			/* ***
			 * *** WC1 .EQ. 2 -- VERBS
			 * *** */
			if( (wc2 == 2 && ty2 == 21) && wc3 == 0 ){
				/* ***
				 * *** NOW WC1 INCLUDING FM1 = 23
				 * *** NOTE: (IN FIRST POSITION SHOULD NOT HAVE FORMS 50,54,60, OR 70 -
				 *            THESE ARE VERBAL AND SHOULD BE PRECEEDED BY A VERB --
				 *            SEE WC1 = 2 LOGIC)
				 * *** */
				*result = 2;
				}
			else if( (wc2 == 2 && ty2 == 31) && wc3 == 0 ){
				*result = 3;
				}
			else if( ((wc2 == 2 && ty2 == 31) && wc3 == 2) && ty3 == 
			  21 ){
				*result = 4;
				}
			else if( (wc2 == 3 || wc2 == 6) && wc3 == 0 ){
				*result = 5;
				}
			else if( wc2 == 19 && wc3 == 0 ){
				*result = 8;
				}
			else if( (((wc2 == 3 || wc2 == 6) && wc3 == 2) && ty3 == 
			  31) && fm3 != 5 ){
				*result = 21;
				}
			}
		else if( (((fm1 == 23 && wc2 == 19) && wc3 == 2) && ty3 == 
		  21) && fm3 != 5 ){
			*result = 17;
			}
		else if( (fm1 == 23 && (wc2 == 3 || wc2 == 6)) && wc3 == 0 ){
			*result = 25;
			}
		else if( (fm1 == 23 && wc2 == 19) && wc3 == 0 ){
			*result = 26;
			}
		}
	else if( wc1 == 2 ){

		if( ty1 == 21 ){
			/* ***
			 * *** WC1 .EQ. 3
			 * *** */
			if( fm1 == 5 ){

				if( fm1 == 5 ){
					if( wc2 == 1 && wc3 == 0 ){
						if( fm2 == 50 ){
							*result = 12;
							}
						else if( fm2 == 6 && sup2 == 15 ){
							*result = 12;
							}
						}
					}
				}
			else if( wc2 == 0 && wc3 == 0 ){
				*result = 10;
				}
			else if( wc2 == 1 ){
				if( ((fm2 == 54 || fm2 == 60) || fm2 == 70) && wc3 == 
				  0 ){
					*result = 15;
					}
				else if( (fm2 == 6 && sup2 == 16) && wc3 == 0 ){
					*result = 15;
					}
				else if( fm2 == 23 && wc3 == 0 ){
					*result = 19;
					}
				else if( (fm2 == 6 && sup2 == 13) && wc3 == 0 ){
					*result = 19;
					}
				}
			else if( (wc2 == 12 && fm2 != 5) && wc3 == 0 ){
				*result = 22;
				}
			}
		else if( ty1 == 31 ){
			if( fm1 == 5 ){

				if( fm1 == 5 ){
					if( wc2 == 1 && wc3 == 0 ){
						if( fm2 == 50 ){
							*result = 13;
							}
						else if( fm2 == 6 && sup2 == 15 ){
							*result = 13;
							}
						}
					else if( ((wc2 == 2 && ty2 == 21) && fm2 == 5) && 
					  wc3 == 1 ){
						if( fm3 == 50 ){
							*result = 14;
							}
						else if( fm3 == 6 && sup3 == 15 ){
							*result = 14;
							}
						}
					}
				}
			else if( wc2 == 0 && wc3 == 0 ){
				*result = 9;
				}
			else if( (wc2 == 2 && ty2 == 21) && fm2 != 5 ){
				if( wc3 == 0 ){
					*result = 11;
					}
				else if( wc3 == 1 ){
					if( (fm3 == 54 || fm3 == 60) || fm3 == 70 ){
						*result = 16;
						}
					else if( fm3 == 23 ){
						*result = 20;
						}
					else if( fm3 == 6 && sup3 == 13 ){
						*result = 20;
						}
					else if( fm3 == 6 && sup3 == 16 ){
						*result = 16;
						}
					}
				else if( wc3 == 12 ){
					*result = 24;
					}
				}
			else if( wc2 == 1 && wc3 == 0 ){
				if( fm2 == 23 ){
					*result = 18;
					}
				else if( fm2 == 6 && sup2 == 13 ){
					*result = 18;
					}
				}
			else if( (wc2 == 2 && ty2 == 21) && fm2 != 5 ){
				if( wc3 == 1 && fm3 == 23 ){
					*result = 20;
					}
				else if( wc3 == 12 ){
					*result = 24;
					}
				}
			}
		else if( (((ty1 == 60 && fm1 != 5) && wc2 == 12) && fm2 != 
		  5) && wc3 == 0 ){
			*result = 23;

			}
		else if( ty1 == 61 ){
			if( (fm1 != 5 && wc2 == 0) && wc3 == 0 )
				*result = 9;
			}
		}
	else if( wc1 == 3 ){
		if( (wc2 == 11 || wc2 == 13) && wc3 == 0 ){
			/* ***
			 * *** WC1 .EQ. 5
			 * *** */
			*result = 28;
			}
		else if( wc2 == 13 ){
			if( wc3 == 2 && ty3 == 21 ){
				*result = 38;
				}
			else if( wc3 == 1 && (((fm3 != 23 && fm3 != 54) && fm3 != 
			  60) && fm3 != 70) ){
				*result = 39;
				}
			}
		}
	else if( wc1 == 5 ){
		if( wc2 == 15 ){
			/* ***
			 * *** WC1 .EQ. 6
			 * *** */
			if( wc3 == 1 ){
				if( fm3 == 23 ){
					*result = 27;
					}
				else if( fm3 == 6 && sup3 == 13 ){
					*result = 27;
					}
				}
			else if( wc3 == 6 ){
				*result = 29;
				}
			else if( wc3 == 0 ){
				*result = 31;
				}
			else if( wc3 == 19 ){
				*result = 35;
				}
			}
		else if( wc2 == 14 ){
			if( wc3 == 0 ){
				*result = 31;
				}
			else if( wc3 == 19 ){
				*result = 35;
				}
			}
		}
	else if( wc1 == 6 ){
		if( wc2 == 3 && wc3 == 13 ){
			/* ***
			 * *** WC1 .EQ. 12
			 * *** */
			*result = 30;
			}
		else if( (wc2 == 13 && wc3 == 2) && ty3 == 21 ){
			*result = 38;
			}
		}
	else if( wc1 == 12 ){
		if( (((((wc2 == 1 && fm2 != 23) && fm2 != 50) && fm2 != 54) && 
		  fm2 != 60) && fm2 != 70) && wc3 == 0 ){
			/* ***
			 * *** WC1 .EQ. 16
			 * *** */
			*result = 6;
			}
		else if( ((((((wc2 == 2 && ty2 == 21) && wc3 == 1) && fm3 != 
		  23) && fm3 != 50) && fm3 != 54) && fm3 != 60) && fm3 != 
		  70 ){
			*result = 7;
			}
		else if( wc2 == 13 && wc3 == 3 ){
			*result = 32;
			}
		}
	else if( wc1 == 16 ){
		if( wc2 == 14 && wc3 == 1 ){
			/* ***
			 * *** WC1 .EQ. 18
			 * *** */
			*result = 36;
			}
		else if( wc2 == 5 && wc3 == 0 ){
			*result = 37;
			}
		}
	else if( wc1 == 18 ){
		if( (wc2 == 15 || wc2 == 14) && wc3 == 0 )
			*result = 33;
		/* ***
		 * *** WC1 .EQ. 19
		 * *** */
		}
	else if( wc1 == 19 ){
		if( (wc2 == 14 || wc2 == 15) && wc3 == 0 )
			*result = 34;
		}
	return;
} /*end of function*/

/*   * * * * ** * * * **  BLOCK DATA  * * * * * * * * ** * * * * * */


void /*FUNCTION*/ blkdata0()
{
	long int _i, _r;
	static int _aini = 1;
	if( _aini ){ /* Do 1 TIME INITIALIZATIONS! */
		{ static struct{ long rc; short ini; } _rs0[] = {
			8,	0,
			1,	13,
			1,	19,
			6,	0,
			1,	6,
			1,	11,
			1,	12,
			1,	13,
			1,	17,
			1,	19,
			2,	0,
			1,	1,
			1,	2,
			1,	6,
			1,	12,
			1,	13,
			1,	14,
			1,	17,
			1,	18,
			1,	6,
			1,	17,
			1,	19,
			5,	0,
			1,	8,
			1,	19,
			1,	20,
			5,	0,
			1,	1,
			1,	5,
			1,	14,
			1,	16,
			1,	18,
			3,	0,
			1,	1,
			1,	5,
			1,	14,
			1,	16,
			1,	18,
			3,	0,
			1,	2,
			1,	12,
			6,	0,
			0L, 0 };
		for(_i=_r=0L; _i < sizeof(resnegX_.nwcr1)/sizeof(short); _i++)
			((short*)resnegX_.nwcr1)[_i] = RC_INI(_rs0); }
		{ static struct{ long rc; short ini; } _rs1[] = {
			1,	1,
			1,	2,
			1,	6,
			1,	12,
			1,	13,
			1,	14,
			1,	16,
			1,	17,
			1,	6,
			1,	11,
			1,	13,
			1,	19,
			4,	0,
			1,	11,
			1,	13,
			1,	19,
			5,	0,
			8,	0,
			1,	6,
			1,	17,
			1,	19,
			5,	0,
			1,	8,
			1,	19,
			1,	20,
			5,	0,
			1,	1,
			1,	5,
			1,	14,
			1,	16,
			1,	18,
			3,	0,
			1,	1,
			1,	5,
			1,	14,
			1,	16,
			1,	18,
			3,	0,
			1,	2,
			1,	3,
			1,	12,
			5,	0,
			0L, 0 };
		for(_i=_r=0L; _i < sizeof(resnegX_.nwcr2)/sizeof(short); _i++)
			((short*)resnegX_.nwcr2)[_i] = RC_INI(_rs1); }
		{ static struct{ long rc; short ini; } _rs2[] = {
			8,	0,
			1,	6,
			1,	11,
			1,	13,
			1,	19,
			4,	0,
			1,	6,
			1,	11,
			1,	12,
			1,	13,
			1,	19,
			3,	0,
			1,	14,
			1,	18,
			6,	0,
			1,	6,
			1,	13,
			1,	17,
			1,	19,
			4,	0,
			1,	8,
			1,	19,
			1,	20,
			5,	0,
			1,	14,
			7,	0,
			1,	1,
			1,	3,
			1,	5,
			1,	7,
			1,	8,
			1,	11,
			1,	14,
			1,	16,
			1,	2,
			1,	12,
			6,	0,
			0L, 0 };
		for(_i=_r=0L; _i < sizeof(resnegX_.nwcr22)/sizeof(short); _i++)
			((short*)resnegX_.nwcr22)[_i] = RC_INI(_rs2); }
		{ static struct{ long rc; short ini; } _rs3[] = {
			8,	0,
			8,	0,
			8,	0,
			8,	0,
			1,	3,
			1,	6,
			1,	17,
			1,	19,
			4,	0,
			1,	8,
			1,	19,
			1,	20,
			5,	0,
			1,	11,
			1,	13,
			1,	19,
			5,	0,
			1,	1,
			1,	4,
			1,	5,
			1,	14,
			1,	16,
			1,	18,
			2,	0,
			1,	2,
			1,	12,
			6,	0,
			0L, 0 };
		for(_i=_r=0L; _i < sizeof(resne2X_.gnwc1)/sizeof(short); _i++)
			((short*)resne2X_.gnwc1)[_i] = RC_INI(_rs3); }
		{ static struct{ long rc; short ini; } _rs4[] = {
			8,	0,
			1,	3,
			1,	4,
			1,	6,
			1,	13,
			1,	19,
			3,	0,
			8,	0,
			1,	3,
			1,	4,
			1,	6,
			1,	14,
			1,	17,
			3,	0,
			1,	3,
			1,	6,
			1,	17,
			1,	19,
			4,	0,
			1,	8,
			1,	19,
			1,	20,
			5,	0,
			1,	11,
			1,	13,
			1,	19,
			5,	0,
			1,	1,
			1,	4,
			1,	5,
			1,	14,
			1,	16,
			1,	18,
			2,	0,
			1,	2,
			1,	12,
			6,	0,
			0L, 0 };
		for(_i=_r=0L; _i < sizeof(resne2X_.gnwc2)/sizeof(short); _i++)
			((short*)resne2X_.gnwc2)[_i] = RC_INI(_rs4); }
		{ static struct{ long rc; short ini; } _rs5[] = {
			8,	0,
			8,	0,
			8,	0,
			1,	4,
			1,	14,
			6,	0,
			1,	3,
			1,	6,
			1,	17,
			1,	19,
			4,	0,
			1,	8,
			1,	19,
			1,	20,
			5,	0,
			8,	0,
			8,	0,
			8,	0,
			0L, 0 };
		for(_i=_r=0L; _i < sizeof(resne2X_.gnwc22)/sizeof(short); _i++)
			((short*)resne2X_.gnwc22)[_i] = RC_INI(_rs5); }
		_aini = 0;
	}

} /*end of function*/

