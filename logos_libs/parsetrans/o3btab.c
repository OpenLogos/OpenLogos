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
	/*
	 *     LAST CHG: 04/23/87 *R1679RKH*  OFL3B conversion & R1685 Swork limi
	 *
	 *  Program to read in Exception Table for OFL3B Table
	 *
	 *        INPUT                  OUTPUT
	 *        ----------             ----------
	 *        FN: GERSRC             FN: GERSRC
	 *            ENGSRC                 ENGSRC
	 *        FT: O3BTAB             FT: O3BGEN
	 *      UNIT: 02               UNIT: 03
	 *     LRECL: 80              LRECL: 1040
	 *
	 *   Input Record Format:
	 *   COL    FIELDNAME  FIELDTYPE  DESCRIPTION
	 *   ---    ---------  ---------  --------------------------------
	 *    1     RECTYPE     C*1       * means comment card
	 *    2     COMMENT     C*23      inline comments
	 *   25                           blank
	 *   27     TARGET      I1        1 G 2 E 3 F 4 S default all targets
	 *   28                           blank
	 *   30     WC          I2        1-20
	 *   32                           blank
	 *   33     SET         I2        1-
	 *   35                           blank
	 *   36     SUBSET      I2        upto 998
	 *   39                           blank
	 *   40     SUPSET      I2        1-16
	 *   42                           blank
	 *   43     FORM        I2        1-16
	 *   44                           blank
	 *   47     OFL3B       I2        1-9
	 *   49                           blank
	 *   55     SCON12      I2        1-999
	 *   58                           blank
	 *   59     SCON45      I2        1-999
	 *   62                           blank
	 *   64     WILDCRD     I2        1-99
	 *   66                           blank
	 *   67     WILDSCON    I2        1-999
	 *
	 *     FUNC = 1 Read the array,CALL FROM T2OPEN
	 *     FUNC = 2 Set Scons (From SW29 & T1DRIVER)
	 *
	 *                        LOCFLG = 1  CALL FROM T1DRIVER
	 *                        LOCFLG = 2  CALL FROM MAIN AND SWITCHES
	 *                        RETFLG = 0  No Problem
	 *                        RETFLG = 1     Problem
	 *
	 *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *
	 *
	 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <fcrt.h>
#include "trans.h"
#include "logoslib.h"
#include "projexts.h"
#include <jbctrl.h>
#include "parsetrans_ext.h"
#include <string.h>
#include <logos_libs/lgs_tran_rule_io/tran_rule_io.h>

#define	LRECL	1040
#define	O3MAX	47

#pragma pack(2)
struct {
	short  o3rec[O3MAX][11];
	int    junk;
	short int o3siz;
	short int o3tgt[O3MAX], o3wc[O3MAX], o3set[O3MAX], o3sub[O3MAX], 
	          o3sup[O3MAX], o3form[O3MAX], o3ofl3[O3MAX], o3sc45[O3MAX],
			  o3sc12[O3MAX], o3wlds, o3wldv[O3MAX];
	}	o3barrX_;

void /*FUNCTION*/ o3btab(inwc, insub, inform, scptr, retflg, locflg)
long int inwc, insub, inform, scptr, *retflg, locflg;
{
	static short int currec, o3wldn[O3MAX], scnnum;
	static long int  ix;
	static char pgmnam[9] = "O3BTAB  ";
	static short zero = 0;

	*retflg = 1;
	if( locflg == 1 ){


		/*      ROUTINE TO READ IN (INITIALLY) THE BIT MAP OF THE FORM TABLES */

		/*     Initialize all arrays */
		memset(&o3barrX_,'\0',sizeof(o3barrX_));

		//cat-102 instead of the following commented out stuff
		if ( !tran_o3b_load( (short *)o3barrX_.o3rec ) )
		{
			errlog(pgmnam,1000,0,1);
		}

		else{

			/*winnt-
			 *     Move the arrays to wc/type etc. */

			for( ix=1; ix <= o3barrX_.o3siz; ix++ ){
				o3barrX_.o3tgt[ix-One] = o3barrX_.o3rec[ix-One][1-One];
				o3barrX_.o3wc[ix-One] = o3barrX_.o3rec[ix-One][2-One];
				o3barrX_.o3set[ix-One] = o3barrX_.o3rec[ix-One][3-One];
				o3barrX_.o3sub[ix-One] = o3barrX_.o3rec[ix-One][4-One];
				o3barrX_.o3sup[ix-One] = o3barrX_.o3rec[ix-One][5-One];
				o3barrX_.o3form[ix-One] = o3barrX_.o3rec[ix-One][6-One];
				o3barrX_.o3ofl3[ix-One] = o3barrX_.o3rec[ix-One][7-One];
				o3barrX_.o3sc12[ix-One] = o3barrX_.o3rec[ix-One][8-One];
				o3barrX_.o3sc45[ix-One] = o3barrX_.o3rec[ix-One][9-One];
				o3wldn[ix-One] = o3barrX_.o3rec[ix-One][10-One];
				o3barrX_.o3wldv[ix-One] = o3barrX_.o3rec[ix-One][11-One];
				}
			*retflg = 0;
			}
		}
	else if( locflg == 2 ){

		/*     Match the incoming record. */

		currec = 0;
		for( ix=1; ix <= o3barrX_.o3siz; ix++ ){
			currec = ix;
			if( inwc == o3barrX_.o3wc[ix-One] ){
				if( !((o3barrX_.o3sub[ix-One] != 0) && (o3barrX_.o3sub[ix-One] != 
				  insub)) ){
					if( !((o3barrX_.o3set[ix-One] != 0) && (o3barrX_.o3set[ix-One] != 
					  sconX_.scon[scptr-One][11-One])) ){
						if( !((o3barrX_.o3sup[ix-One] != 0) && (o3barrX_.o3sup[ix-One] != 
						  sconX_.scon[scptr-One][13-One])) ){
							if( !((o3barrX_.o3form[ix-One] != 0) && 
							  (o3barrX_.o3form[ix-One] != inform))
							   ){
								if( o3barrX_.o3ofl3[ix-One] == sconX_.scon[scptr-One][12-One] ){
									if( !((o3barrX_.o3tgt[ix-One] != 0) &&
										  (o3barrX_.o3tgt[ix-One] != trgflgX_.trgflg)) )
										goto L_1001;
									}
								}
							}
						}
					}
				}
			}

		/*     None of the records matched */

		*retflg = 0;
		return;

		/*     All parms have matched. Set Scons */

L_1001:
		sconX_.scon[scptr-One][12-One] = o3barrX_.o3sc12[ix-One];
		sconX_.scono[sconX_.scolnk[scptr-One]-One][45-SCONX1-One] = o3barrX_.o3sc45[ix-One];

		/*     Check if Wildcard is non zero */

		if( o3wldn[ix-One] != 0 ){
			scnnum = o3wldn[ix-One];
			if( scnnum <= SCONX1 ){
				sconX_.scon[scptr-One][scnnum-One] = o3barrX_.o3wldv[ix-One];
				}
			else{
				sconX_.scono[sconX_.scolnk[scptr-One]-One][scnnum-
				  SCONX1-One] = o3barrX_.o3wldv[ix-One];
				}
			}

		*retflg = 0;
		}
	else{
		errlog(pgmnam,120,0,21);
		}
	return;
} /*end of function*/

