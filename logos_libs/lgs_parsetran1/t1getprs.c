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
	 *		Read the data that is passed from the last PARSE
	 *		program to TRAN1 and use it to overlay the initial
	 *	        TRAN1 data
	 *	GETPRS must be called twice(i.e. in 2 passes) for each sentence
	 *       and in order (i.e. pass 1 then 2). The input
	 *       argument defines which pass (1 or 2).  Pass 1 loads only
	 *       enough data to set the Parts of Speech selected by PARSE.
	 *       GETPRS(1) called by T1init to set part of speech so that
	 *           t1tratab() will get the correct target codes.
	 *       GETPRS(2) called by t1driver to get remaining PARSE data
	 *           and overlay the initialized RES data.
	 *       The initialization of data is rather complicated in the beginning
	 *       of t1driver.  Initialzation is done by t1init, t1tratab and t1driver.
	 *       It seemed simpler to overlay the RES data with PARSE data after
	 *       initialization in TRAN1, than to start by overlaying the RES data
	 *       and then addressing any affects in the initialization process.
	 *       The 1st pass was added just to cause the initializaiton process
	 *       to load the correct target data.
	 *
	 * CHANGES:
	 *
	 *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *
	 *
	 */

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

#define UNCMP_BUFFER_SIZE 70000		// size of buffer for uncomressed data
extern struct {
	char buf[UNCMP_BUFFER_SIZE];
	char *pt;
	int size;
} uncmp1_buf;
EXTERN struct t_prsdatX_ {
	short int prjs[ELMMAX], prscln[SCONY], praddr[ELMMAX];
	}	prsdatX_;


void /*FUNCTION*/ getprs(long pass)
{
	static short int i, jsxx, sclnxx, swxx, xx;
	long num , pos;


	if( pass == 2 ){

		/*		PASS 2 - load and overlay remainder of PARSE data */


		num = 2*elemctX_.parsct;
		lmove(prctX_.js,1,prsdatX_.prjs,1,num);
		lmove(sconX_.scolnk,1,prsdatX_.prscln,1,num);
		pos = 3 + (num*2);

		/*           COMMON: /PRSFRM/ PRSFRM */
		num = 2*elemctX_.parsct;
		lmove(formsaX_.formsv,1,(short*)uncmp1_buf.buf,pos,num);
		pos += num;

		/*     	    COMMON: /SWORKI/ SWORKI ===(overlays)==> SWORK1
		 *           Prepare SWORK1
		 *           shift SWORK1 up to account for new 68 or 67 swith
		 *           elements inserted, so that each SWORK1 record is in sync. */
		if( elemctX_.parsct > elemctX_.origct ){
			num = SWK1SZ*2;
			for( xx=elemctX_.parsct; xx >= 1; xx-- ){
				/*		if orig positional was different then shift it. */
				sclnxx = sconX_.scolnk[xx-One];
				if( sclnxx <= elemctX_.origct && sclnxx < xx ){
					lmove(&swork1X_.swork1[xx-One][1-One],1,&swork1X_.swork1[sclnxx-One][1-One],
					  1,num);
					/*                zap the record for new 68 67 switch elements */
					}
				else if( sclnxx > elemctX_.origct ){
					zapit(&swork1X_.swork1[xx-One][1-One],num,(byte)0);
					swork1X_.swork1[xx-One][1-One] = 1;
					swork1X_.swork1[xx-One][2-One] = -1;
					swork1X_.swork1[xx-One][3-One] = -1;
					}
				}
			}

		num = 6;
		for( i=1; i <= elemctX_.parsct; i++ ){
			lmove(&swork1X_.swork1[i-One][dct2X_.dct2[prctX_.js[sconX_.scolnk[i-One]-
			  One]-One]-One],1,(short*)uncmp1_buf.buf,pos,num);
			pos += num;
			}


		/*           SWORK addresses for new 67 68 switch elements */
		num = 2*elemctX_.parsct;
		lmove(prsdatX_.praddr,1,(short*)uncmp1_buf.buf,pos,num);
		pos += num;

		/*           COMMON /VBDATA/ ...,VBCELL,...
		 *	     VBCELL(61-100) only */
		num = 80;
		lmove(&vbdataX_.vbcell[61-One],1,(short*)uncmp1_buf.buf,pos,num);
		pos += num;

		/*        COMMON: /NWSTR/ NWSTRG */
		num = sizeof(sent_wordsX_.source_word[0])*elemctX_.parsct;
		lmove((short*)sent_wordsX_.source_word,1,(short*)uncmp1_buf.buf,pos,num);
		pos += num;


		/*        Overlay the SCON array
		 *           COMMON: /SCON/ SCON
		 *      POS = 1 */
		num = 2*SCONX1*elemctX_.parsct;
		lmove((short*)sconX_.scon,1,(short*)uncmp1_buf.buf,pos,num);
		pos += num;

		/*           COMMON: /SCONIN/ SCONIN */
		num = SCINSZ*elemctX_.parsct*2;
		lmove((short*)sconinX_.sconin,1,(short*)uncmp1_buf.buf,pos,num);
		pos += num;


		/*            Overlay SCONO with the PARSE version */

		num = 2*(SCONX - SCONX1)*elemctX_.parsct;
		lmove((short*)sconX_.scono,1,(short*)uncmp1_buf.buf,pos,num);
		pos += num;



		/*	overlay */

		/*                  Restore the saved input SCON 2,11,13 values while
		 *                  saving the SCON 2,11,13 values set by pass1.
		 *                  Only nec. for each SWORK element(ELEMCT), not whole SCON. */
		for( xx=1; xx <= elemctX_.parsct; xx++ ){
			sconX_.scon[xx-One][2-One] = sconinX_.sconin[sconX_.scolnk[xx-One]-
			  One][1-One];
			sconX_.scon[xx-One][11-One] = sconinX_.sconin[sconX_.scolnk[xx-One]-
			  One][2-One];
			sconX_.scon[xx-One][13-One] = sconinX_.sconin[sconX_.scolnk[xx-One]-
			  One][3-One];
			}

		/*		  set SWORK1 POS selection fields (1,2,3) */
		for( xx=1; xx <= elemctX_.parsct; xx++ ){
			jsxx = prctX_.js[sconX_.scolnk[xx-One]-One];
			if( swork1X_.swork1[xx-One][jsxx-One] < 0 ){
				if( jsxx == 1 ){
					swork1X_.swork1[xx-One][1-One] = -swork1X_.swork1[xx-One][1-One];
					if( swork1X_.swork1[xx-One][2-One] > 0 )
						swork1X_.swork1[xx-One][2-One] = -swork1X_.swork1[xx-One][2-One];
					if( swork1X_.swork1[xx-One][3-One] > 0 )
						swork1X_.swork1[xx-One][3-One] = -swork1X_.swork1[xx-One][3-One];
					}
				else if( jsxx == 2 ){
					if( swork1X_.swork1[xx-One][1-One] > 0 )
						swork1X_.swork1[xx-One][1-One] = -swork1X_.swork1[xx-One][1-One];
					swork1X_.swork1[xx-One][2-One] = -swork1X_.swork1[xx-One][2-One];
					if( swork1X_.swork1[xx-One][3-One] > 0 )
						swork1X_.swork1[xx-One][3-One] = -swork1X_.swork1[xx-One][3-One];
					}
				else if( jsxx == 3 ){
					if( swork1X_.swork1[xx-One][1-One] > 0 )
						swork1X_.swork1[xx-One][1-One] = -swork1X_.swork1[xx-One][1-One];
					if( swork1X_.swork1[xx-One][2-One] > 0 )
						swork1X_.swork1[xx-One][2-One] = -swork1X_.swork1[xx-One][2-One];
					swork1X_.swork1[xx-One][3-One] = -swork1X_.swork1[xx-One][3-One];
					}
				}
			}

		/*           SWORK addresses for new 67, 68 switch elements */
		for( xx=1; xx <= elemctX_.parsct; xx++ ){
			if( sconX_.scolnk[xx-One] > elemctX_.origct )
				swork1X_.swork1[xx-One][dctX_.dct[prctX_.js[sconX_.scolnk[xx-One]-
				  One]-One]-One] = prsdatX_.praddr[xx-One];
			}

		elemctX_.elemct = elemctX_.parsct;
		swork1X_.phct = elemctX_.parsct;
		/*                  ALSO RESET SCON COUNTER TO IGNORE LOADS IN PASS1 */
		if( elemctX_.parsct + AD1MAX > ELMMAX ){
			prtscoX_.sct = ELMMAX;
			}
		else{
			prtscoX_.sct = elemctX_.parsct + AD1MAX;



			}
		}
	else if( pass == 1 ){


		/*		PASS 1 - load POS selection and overlay POS selection */

		/*        COMMON: /ELEMCT/ ...,PARSCT,... */
		pos = 1;
		num = 2;
		lmove(&elemctX_.parsct,1,(short*)uncmp1_buf.buf,pos,num);
		pos += num;

		/*        COMMON: /PRSDAT/ PRJS */
		num = 2*elemctX_.parsct;
		lmove(prsdatX_.prjs,1,(short*)uncmp1_buf.buf,pos,num);
		pos += num;

		/*        SCOLNK, SCON LINK LIST - is in SWORK order */
		num = 2*elemctX_.parsct;
		lmove(prsdatX_.prscln,1,(short*)uncmp1_buf.buf,pos,num);
		pos += num;

		/*		  set SWORK1 POS selection fields (1,2,3)
		 *                 for original elements in their original RES positions */
		for( xx=1; xx <= elemctX_.parsct; xx++ ){
			if( prsdatX_.prscln[xx-One] > 0 && prsdatX_.prscln[xx-One] <= 
			  elemctX_.origct ){
				swxx = prsdatX_.prscln[xx-One];
				jsxx = prsdatX_.prjs[prsdatX_.prscln[xx-One]-One];
				if( swork1X_.swork1[swxx-One][jsxx-One] < 0 ){
					if( jsxx == 1 ){
						swork1X_.swork1[swxx-One][1-One] = -swork1X_.swork1[swxx-One][1-One];
						if( swork1X_.swork1[swxx-One][2-One] > 0 )
							swork1X_.swork1[swxx-One][2-One] = -swork1X_.swork1[swxx-One][2-One];
						if( swork1X_.swork1[swxx-One][3-One] > 0 )
							swork1X_.swork1[swxx-One][3-One] = -swork1X_.swork1[swxx-One][3-One];
						}
					else if( jsxx == 2 ){
						if( swork1X_.swork1[swxx-One][1-One] > 0 )
							swork1X_.swork1[swxx-One][1-One] = -swork1X_.swork1[swxx-One][1-One];
						swork1X_.swork1[swxx-One][2-One] = -swork1X_.swork1[swxx-One][2-One];
						if( swork1X_.swork1[swxx-One][3-One] > 0 )
							swork1X_.swork1[swxx-One][3-One] = -swork1X_.swork1[swxx-One][3-One];
						}
					else if( jsxx == 3 ){
						if( swork1X_.swork1[swxx-One][1-One] > 0 )
							swork1X_.swork1[swxx-One][1-One] = -swork1X_.swork1[swxx-One][1-One];
						if( swork1X_.swork1[swxx-One][2-One] > 0 )
							swork1X_.swork1[swxx-One][2-One] = -swork1X_.swork1[swxx-One][2-One];
						swork1X_.swork1[swxx-One][3-One] = -swork1X_.swork1[swxx-One][3-One];
						}
					}
				}
			}
		}
	else{
		}
	return;

} /*end of function*/

