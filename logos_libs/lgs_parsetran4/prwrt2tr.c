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
	/*		Move is passed from the last PARSE
	 *		program to TRAN1.
	 *               All reordering is done here, and written in TRAN1 order. */
	/*       SWORKI is moved as is
	 *       SCON needs to be in SWORKI order (since in TRAN1 there is no
	 *           indirect indexing from SWORK to SCON).
	 *       Any new elements appended to SCONO that will not be passed to
	 *       TRAN1 (i.e. 67sw elements and some 68sw elements), must be
	 *       removed and the remaining new elements shifted down to save
	 *       SCONO space before writing to TRAN1.
	 *       SCOLNK need to be renumbered to reflect changes in SCONO order,
	 *         and it should be written to TRAN1 in SWORKI order. */
	/*       SCO2SC is a mapping from SCONO to SCON and SCOLNK for
	 *       SWORKI elements (i.e. elements passed to TRAN1). It is used
	 *       to renumber SCOLNK and to determine which SCONO elements
	 *       are written to TRAN1.
	 *	SCONO and associated arrays will only be
	 *       reordered if they contain 67sw elements or 68sw elements that
	 *       do not go to TRAN1 */
	/* CHANGES: */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*       SCO2SC =mapping from SCONO to the SCON, used to reorder SCONO arrays
	 *         	and to renumber SCOLNK for SWORKI elements only. */
	/*+   NEW FOR VTRBRNCH SUBROUTINE - THE  FOLLOWING ARE IN COMMON */
	/*- */
	/*    NEW COMMONS FOR VTRBRNCH SUBROUTINE(SEE VTRFWR ALSO) */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#define MS_F77
#include <fcrt.h>
#include <trans.h>
#include "logoslib.h"
#include "projexts.h"
#include <jbctrl.h>


//#include "parsetrans_ext.h"
#define UNCMP_BUFFER_SIZE 70000		// size of buffer for uncomressed data
extern struct {
	char buf[UNCMP_BUFFER_SIZE];
	char *pt;
	int size;
} uncmp_buf;


int /*FUNCTION*/ wrt2tr()
{
	static short int addr, hfppt, opapt, ret, scnpt, sco2sc[ELMMAX], 
	  scopos, swiadr[ELMMAX], xx;

	long num,pos;

	memset(sco2sc,'\0',sizeof(sco2sc));



	/*  	build SCO2SC() mapping only for SWORKI elements that will
	 *	be passed to TRAN. */
	for( xx=1; xx <= elemctX_.parsct; xx++ ){
		sco2sc[sconX_.scolnk[sworkiX_.sworki[xx-One][4-One]-One]-One] = sworkiX_.sworki[xx-One][4-One];
		}

	if( elemctX_.parsct < elemctX_.elemct && elemctX_.parsct > elemctX_.origct ){
		/*             renumber SCOLNK for new 68sw elements in SWORKI so that
		 *             SCOLNK points to where the SCONO value will be. */
		scopos = elemctX_.origct;
		for( xx=elemctX_.origct + 1; xx <= elemctX_.elemct; xx++ ){

			if( sco2sc[xx-One] > 0 ){
				scopos += 1;
				sconX_.scolnk[sco2sc[xx-One]-One] = scopos;
				}
			}
		}
	/*             convert JS values to those expected by TRAN1 */
	for( xx=1; xx <= elemctX_.elemct; xx++ ){
		if( prctX_.js[xx-One] > 10 )
			prctX_.js[xx-One] -= 10;
		}

	/*     	    load addresses of new 68&67 elements in SWORKI order */
	for( xx=1; xx <= elemctX_.parsct; xx++ ){
		if( sconX_.scolnk[sworkiX_.sworki[xx-One][4-One]-One] > elemctX_.origct ){
			opapt = 1;
			hfppt = 0;
			addr = 0;
			getadr(opadroX_.opadro,opadroX_.sconpo,hpdopoX_.hfdopo,
			  opapt,&hfppt,&addr,&scnpt,&ret);
			while( ret != 1 && scnpt != sworkiX_.sworki[xx-One][4-One] ){
				if( hfppt == 0 )
					opapt += 1;
				addr = 0;
				getadr(opadroX_.opadro,opadroX_.sconpo,hpdopoX_.hfdopo,
				  opapt,&hfppt,&addr,&scnpt,&ret);
				}
			swiadr[xx-One] = addr;
			}
		else{
			swiadr[xx-One] = 0;
			}
		}


	/*====  Begin output ==== */

	memset(&uncmp_buf.buf,'\0',sizeof(uncmp_buf.buf));

	/*        COMMON: /ELEMCT/ ...,PARSCT */
	pos = 1;
	num = 2;
	lmove((short*)uncmp_buf.buf,pos,&elemctX_.parsct,1,num);
	pos += num;

	/*        COMMON: /PRCT/ JS */
	if( elemctX_.parsct == elemctX_.elemct || elemctX_.parsct == elemctX_.origct ){
		num = 2*elemctX_.parsct;
		lmove((short*)uncmp_buf.buf,pos,prctX_.js,1,num);
		pos += num;
		}
	else{
		/*		move original elements */
		num = 2*elemctX_.origct;
		lmove((short*)uncmp_buf.buf,pos,prctX_.js,1,num);
		pos += num;
		/*               move only new elements that are passed to TRAN1 */
		num = 2;
		for( xx=elemctX_.origct + 1; xx <= elemctX_.elemct; xx++ ){
			if( sco2sc[xx-One] > 0 ){
				lmove((short*)uncmp_buf.buf,pos,&prctX_.js[xx-One],
				  1,num);
				pos += num;
				}
			}
		}

	/*        SCOLNK
	 *        Write SCOLNK in SWORKI order */
	num = 2;
	for( xx=1; xx <= elemctX_.parsct; xx++ ){
		lmove((short*)uncmp_buf.buf,pos,&sconX_.scolnk[sworkiX_.sworki[xx-One][4-One]-
		  One],1,num);
		pos += num;
		}

	/*           COMMON: /PRSFRM/ PRSFRM */
	if( elemctX_.parsct == elemctX_.elemct || elemctX_.parsct == elemctX_.origct ){
		num = 2*elemctX_.parsct;
		lmove((short*)uncmp_buf.buf,pos,formsaX_.prsfrm,1,num);
		pos += num;
		}
	else{
		/*		move original elements */
		num = 2*elemctX_.origct;
		lmove((short*)uncmp_buf.buf,pos,formsaX_.prsfrm,1,num);
		pos += num;
		/*               move only new elements that are passed to TRAN1 */
		num = 2;
		for( xx=elemctX_.origct + 1; xx <= elemctX_.elemct; xx++ ){
			if( sco2sc[xx-One] > 0 ){
				lmove((short*)uncmp_buf.buf,pos,&formsaX_.prsfrm[xx-One],
				  1,num);
				pos += num;
				}
			}
		}

	/*     	    COMMON: /SWORKI/ SWORKI */
	num = 6;
	for( xx=1; xx <= elemctX_.parsct; xx++ ){
		lmove((short*)uncmp_buf.buf,pos,&sworkiX_.sworki[xx-One][1-One],
		  1,num);
		pos += num;
		}

	/*     	    passing addresses of new 68&67 elements in SWORKI order */
	num = 2*elemctX_.parsct;
	lmove((short*)uncmp_buf.buf,pos,swiadr,1,num);
	pos += num;

	/*           COMMON /VBDATA/ ...,VBCELL,...
	 *	     VBCELL(61-100) only */
	num = 80;
	lmove((short*)uncmp_buf.buf,pos,&vbdataX_.vbcell[61-One],1,num);
	pos += num;


	/*        COMMON: /NWSTR/ NWSTRG */
	if( elemctX_.parsct == elemctX_.elemct || elemctX_.parsct == elemctX_.origct ){
		num = sizeof(sent_wordsX_.source_word[0])*elemctX_.parsct;
		lmove((short*)uncmp_buf.buf,pos,(short*)sent_wordsX_.source_word,1,num);
		pos += num;
		}
	else{
		/*		move original elements */
		num = sizeof(sent_wordsX_.source_word[0])*elemctX_.origct;
		lmove((short*)uncmp_buf.buf,pos,(short*)sent_wordsX_.source_word,1,num);
		pos += num;
		/*               move only new elements that are passed to TRAN1 */
		num = sizeof(sent_wordsX_.source_word[0]);
		for( xx=elemctX_.origct + 1; xx <= elemctX_.elemct; xx++ ){
			if( sco2sc[xx-One] > 0 ){
				lmove((short*)uncmp_buf.buf, pos,
					  (short*)sent_wordsX_.source_word[xx-One] ,1,num);
				pos += num;
				}
			}
		}


	/*           COMMON: /SCON/ SCON */
	num = 2*SCONX1;
	for( xx=1; xx <= elemctX_.parsct; xx++ ){
		lmove((short*)uncmp_buf.buf,pos,&sconX_.scon[sworkiX_.sworki[xx-One][4-One]-
		  One][1-One],1,num);
		pos += num;
		}


	/*           COMMON: /SCONIN/ SCONIN */
	if( elemctX_.parsct == elemctX_.elemct || elemctX_.parsct == elemctX_.origct ){
		num = SCINSZ*2*elemctX_.parsct;
		lmove((short*)uncmp_buf.buf,pos,(short*)sconinX_.sconin,
		  1,num);
		pos += num;
		}
	else{
		/*		move original elements */
		num = SCINSZ*2*elemctX_.origct;
		lmove((short*)uncmp_buf.buf,pos,(short*)sconinX_.sconin,
		  1,num);
		pos += num;
		/*               move only new elements that are passed to TRAN1 */
		num = SCINSZ*2;
		for( xx=elemctX_.origct + 1; xx <= elemctX_.elemct; xx++ ){
			if( sco2sc[xx-One] > 0 ){
				lmove((short*)uncmp_buf.buf,pos,&sconinX_.sconin[xx-One][1-One],
				  1,num);
				pos += num;
				}
			}
		}



	/*     WRITE THE SCON OVERFLOW */
	if( elemctX_.parsct == elemctX_.elemct || elemctX_.parsct == elemctX_.origct ){
		num = 2*(SCONX - SCONX1)*elemctX_.parsct;
		lmove((short*)uncmp_buf.buf,pos,(short*)sconX_.scono,1,num);
		pos += num;
		}
	else{
		/*               move first origct elements */
		num = 2*(SCONX - SCONX1)*elemctX_.origct;
		lmove((short*)uncmp_buf.buf,pos,(short*)sconX_.scono,1,num);
		pos += num;

		/*               move only new elements that are passed to TRAN1 */
		num = 2*(SCONX - SCONX1);
		for( xx=elemctX_.origct + 1; xx <= elemctX_.elemct; xx++ ){
			if( sco2sc[xx-One] > 0 ){
				lmove((short*)uncmp_buf.buf,pos,&sconX_.scono[xx-One][1-One],
				  1,num);
				pos += num;
				}
			}
	}


//	fprintf( _spec_fp, "PRWRT2TR POS size =  %5ld\n", pos );


	return pos;

} /*end of function*/

