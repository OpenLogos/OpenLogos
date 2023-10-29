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

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <fcrt.h>
#include "trans.h"
#include "logoslib.h"
#include "projexts.h"

/****************************************************************
 * TXVCFILL: General purpose function for filling  a vc (the target)
 *           with some addresses (the source). */


/* FUNCTION: VCFILL(FUNC,TARGVC,PHRSTR,PHRLST,FILMOD,OVRFLW,RC)
 *           inputs:
 *      FUNC   = Defines the types of function and the meaning of some of
 *               the passed arguments.
 *         1 = TARGVC is in the output OPADR,
 *             PHRSTR is ptr to beginning of input OPADR to be VC loaded.
 *             PHRLST is ptr to end of input OPADR to be VC loaded.
 *         2 = TARGVC is in the output OPADR,
 *             PHRSTR is address of a constant to be VC loaded (OPADR value).
 *             PHRLST is ptr to the SCON of a constant to be VC loaded (SCONPO).
 *      TARGVC = ptr to OPADRO position where VC is located.
 *      PHRSTR = Depend upon FUNC.  Either first and last OPADR position in
 *      PHRLST   a range of OPADR addresses to load into the VC, or
 *               the address and SCON pointer, respectively, of a constant
 *               to be VC loaded.
 *      FILMOD = method for filling the VC:
 *         0 = APPEND addresses at SRCPTR to end of TARGVC.
 *         1 = OVERLAY anything currently in TARGVC.
 *      OVRFLW = response if vc cannot hold all the new addresses.
 *         0 = load as many addresses into the VC as possible.
 *         1 = quit without loading any addresses.
 *              RC=1 regardless of the value of OVRLFW. */

/*           outputs:
 *      RC     = Return code
 *         0 = ok
 *         1 = Current VC has no more space left.
 *         2 = cannot create a new VC. No more left. */

/* NOTE: This fill algorithm has been designed to be easily modified to
 *       handle inputs from different sources and targets.  Whereever
 *       possible input values (ex: opadri, hfdopo, etc) have been placed
 *       assigned to normalizes variables to satisfy the algorithms.
 *       When adding new fill capabilities, try to conditionally set these
 *       normalized values from the different inputs.
 *       The Normalized variables:
 *          INTEGER*2 SRCOPA,SRCHFD,SRCSCP
 *          INTEGER*2 TRGHFD
 *          INTEGER*2 FILOPA(HFPADX),FILSCP(HFPADX),FPOS */


/* CHANGES: */

/**************************************************************** */
#include <string.h>
#include "parsetrans_ext.h"
#include <jbctrl.h>


void /*FUNCTION*/ vcfill(func, targvc, phrstr, phrlst, filmod, ovrflw, rc)
long int func, targvc, phrstr, phrlst;
short int *filmod;
long int ovrflw, *rc;
{
	static short int adr2, filopa[HFPADX], filscp[HFPADX], fpos, iz2, 
	  jz, jz2, m, srchfd, srcopa, srcscp, trghfd, vcadr, vcfrst, vcleft;
	static long int err;
	static char pgmnam[9] = "VCFILL  ";


	/*                       temp arrays to hold the src addresses */
	/*                       temp to normalize input targ and src arrays. */

	/*-------------------------------------------------------------------
	 *
	 *           Move source values into temp arrays, including src vc's.
	 * */
	if( func == 1 ){
		fpos = 0;
		for( iz2=phrstr; iz2 <= phrlst; iz2++ ){

			srcopa = opadriX_.opadri[iz2-One];
			srchfd = hpdopiX_.hfdopi[iz2-One];
			srcscp = opadriX_.sconpi[iz2-One];

			/*                       IGNORE A -140 UNLESS IT IS A CLAUSE MARKER
			 *                       pointing TO A RELOCATED CLAUSE. */
			if( srcopa == -140 ){
				if( srcscp <= 0 || srcscp > SCONY )
					goto L_500;
				if( sconX_.scolnk[srcscp-One] <= 0 || sconX_.scolnk[srcscp-One] > 
				  ELMMAX )
					goto L_500;
				if( clsconX_.cmchld[sconX_.scolnk[srcscp-One]-One] <= 
				  0 )
					goto L_500;
				}
			/*                       Ignore the empty VC */
			if( (srcopa <= -99 && srcopa >= -120) && (srchfd == 0) )
				goto L_500;

			/*                       This is a good address. Load it. */
			fpos += 1;
			if( fpos >= HFPADX ){
				*rc = 1;
				err = 501;
				goto L_9500;
				}
			filopa[fpos-One] = srcopa;
			filscp[fpos-One] = srcscp;
			if( srchfd != 0 )
				filopa[fpos-One] = srchfd;
			/*                    if THE VC HAS >1 address, then load them now. */
			if( srchfd >= HFPOLO && srchfd <= HFPOHI ){
				fpos -= 1;
				adr2 = srchfd - HFPOL1;
				jz2 = hfdoaX_.hfpoad[adr2-One][HFPADX-One];
				for( m=1; m <= jz2; m++ ){
					fpos += 1;
					if( fpos >= HFPADX ){
						*rc = 1;
						err = 509;
						goto L_9500;
						}
					filopa[fpos-One] = hfdoaX_.hfpoad[adr2-One][m-One];
					filscp[fpos-One] = hfdoaX_.sconhf[adr2-One][m-One];
					}
				}
			/*                                 loop for next src address. */
L_500:
			;
			}

		/*                        FUNC=2, loading a single constant */
		}
	else{
		filopa[1-One] = phrstr;
		filscp[1-One] = phrlst;
		fpos = 1;
		}
	if( fpos <= 0 )
		goto L_9999;

	/*           Calculate space left in target vc. Is it enough?
	 *
	 *                             overlay means whole vc available */
	if( *filmod == 1 ){
		vcleft = HFPADX - 1;
		}
	else{
		vcleft = HFPADX - 1;
		trghfd = hpdopoX_.hfdopo[targvc-One];
		if( trghfd < HFPOLO || trghfd > HFPOHI ){
			vcleft = HFPADX - 2;
			}
		else{
			adr2 = trghfd - HFPOL1;
			jz2 = hfdoaX_.hfpoad[adr2-One][HFPADX-One];
			vcleft = HFPADX - 1 - jz2;
			}
		}
	if( vcleft < fpos ){
		if( ovrflw == 0 ){
			*rc = 1;
			err = 510;
			goto L_9500;
			}
		else{
			if( diagsX_.deepdi == 1 || diagsX_.longdi == 1 ){
				fprintf( _spec_fp, " ********** ERROR IN TXVCFIL   ********\n VC OVERFLOW, LOADING AS MUCH AS POSSIBLE.\nTHE REMAINING ADDRESSES MAY BE LOST.\n **************************************\n" );
				errlog(pgmnam,err,*rc,9);
				}
			fpos = vcleft;
			}
		}

	/*           FILL THE TARGET VC
	 *              The following are significant local variables:
	 *                 VCFRST=HFDOPO(targvc)
	 *                 VCADR= HFPOAD overflow array being used.
	 *                 JZ = current fill postion in overflow array at VCADR.
	 * */
	vcfrst = hpdopoX_.hfdopo[targvc-One];
	if( vcfrst >= HFPOLO && vcfrst <= HFPOHI ){
		vcadr = vcfrst - HFPOL1;
		jz = hfdoaX_.hfpoad[vcadr-One][HFPADX-One];
		}
	/*                       if overlay mode, initialize the vc. */
	if( *filmod == 1 ){
		if( vcfrst >= HFPOLO && vcfrst <= HFPOHI ){
			hfdoaX_.hfpoad[vcadr-One][HFPADX-One] = 0;
			}
		else{
			vcfrst = 0;
			}
		jz = 0;
		*filmod = 0;
		}

	for( iz2=1; iz2 <= fpos; iz2++ ){
		/*                       IF vc is empty then load hfdop array */
		if( vcfrst == 0 ){
			hpdopoX_.hfdopo[targvc-One] = filopa[iz2-One];
			opadroX_.sconpo[targvc-One] = filscp[iz2-One];
			/*                          set mode to append for remaining values. */
			*filmod = 0;
			/*                          reset VCFRST to new value */
			vcfrst = filopa[iz2-One];

			/*                       If no overflow yet then start one */
			}
		else if( vcfrst < HFPOLO || vcfrst > HFPOHI ){
			if( hfdoaX_.adct >= HFPADY ){
				*rc = 2;
				err = 4407;
				goto L_9500;
				}
			hfdoaX_.adct += 1;
			/*                              shift hfdop value down */
			hfdoaX_.hfpoad[hfdoaX_.adct-One][1-One] = hpdopoX_.hfdopo[targvc-One];
			hfdoaX_.sconhf[hfdoaX_.adct-One][1-One] = opadroX_.sconpo[targvc-One];
			hpdopoX_.hfdopo[targvc-One] = HFPOL1 + hfdoaX_.adct;
			/*                              append new address */
			hfdoaX_.hfpoad[hfdoaX_.adct-One][2-One] = filopa[iz2-One];
			hfdoaX_.sconhf[hfdoaX_.adct-One][2-One] = filscp[iz2-One];
			/*                              set loop variables */
			vcfrst = HFPOL1 + hfdoaX_.adct;
			vcadr = hfdoaX_.adct;
			jz = 2;
			/*                       ARRIVE HERE TO LOAD HFPOAD */
			}
		else{
			jz += 1;
			if( jz >= HFPADX ){
				*rc = 1;
				err = 4408;
				goto L_9500;
				}
			hfdoaX_.hfpoad[vcadr-One][jz-One] = filopa[iz2-One];
			hfdoaX_.sconhf[vcadr-One][jz-One] = filscp[iz2-One];
			}
		/*                       loop back for next fill address */
		}

	/*                  set the overflow counter */
	if( vcfrst >= HFPOLO && vcfrst <= HFPOHI ){
		hfdoaX_.hfpoad[vcadr-One][HFPADX-One] = jz;
		}

	goto L_9999;

	/*------                      ERRORS
	 * */
L_9500:
	;
	if( diagsX_.deepdi == 1 || diagsX_.longdi == 1 ){
		fprintf( _spec_fp, " ********** ERROR IN TXVCFIL   ********\n" );

		if( *rc == 1 )
			{
			fprintf( _spec_fp, "  Attempting to overfill VC \n" );
			}
		if( *rc == 2 )
			{
			fprintf( _spec_fp, "  No more VC overflow arrays,limit exceeded. ADCT=%4d\n", 
			  hfdoaX_.adct );
			}

		fprintf( _spec_fp, " *******************************************\n" );
		}
	errlog(pgmnam,err,*rc,9);

	/*------
	 * */
L_9999:
	;
	return;
} /*end of function*/



