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
#include "parsetrans_ext.h"
#include <jbctrl.h>


/* Translation Memory Noun Phrase extraction */

/*  Library of routines used in the TRANs for TM NP
 *   subroutines:
 *      NPINIT() = Initialize /TRTMNP/ data for each sentence.
 *      NPSLCT() = Select Noun phrases at beginning and end of
 *      NPLSAP() = Create a node and Append it to the NP list */

/*   functions: */



/*================================================================= */

/*  NPINIT() - initialize /TRTMNP/ data  */

void /*FUNCTION*/ npinit()
{
				// get out if not word search
	if( jcaddX_.wrdsrc == 0 ||
		(passesX_.passfl == 2 && passesX_.passct == 1) )
		return;

	trtmnpX_.npcnt = 0;
	memset(trtmnpX_.nplsbg, '\0',sizeof(trtmnpX_.nplsbg));
	trtmnpX_.nplssz = 0;
	return;
}  

/*================================================================= */





/*  NPSLCT() = Tranlslation Memory - Noun phrase selection - TRAN2 */

/*                 for each NP in XSWORK
 *   Fill in as much of the /TRTMNP/ common block data as possible.
 *   This means selecting all noun phrases that meet the criteria
 *   and loading all necessary information about them into /TRTMNP/. */

/*  Arguments:
 *    MODE =  which data arrays operating on:
 *            1= input arrays SWORK,OPADRI,etc. Probably called at
 *               the beginning of TRAN2.
 *            2= output arrays SWORKO,OPADRO,etc. Called at
 *               the end of TRAN2.
 *            3= output arrays SWORKO,OPADRO,etc. Called at
 *               the end of TRAN3.
 *       NOTE: MODE is declared as INTEGER to allow constant in call. */

/*    XSWORK
 *    XPHRBG
 *    XPHRND
 *    XPHCT
 *    XSCONP  =  Data needed to select nps.  MODE dependent:
 *          MODE=1 --> SWORK,PHRBEG,PHREND,PHCT,SCONPI
 *          MODE=2 --> SWORKO,PHRBGO,PHRNDO,PHCTO,SCONPO
 *          MODE=3 --> SWORKO,PHRBGO,PHRNDO,PHCTO,SCONPO */

/*    RET  =  return code.  0=ok, 1=Limit of NP/sentence reached,
 *                          else = misc error. */

/*  Input: (External COMMON data, not passed as args but used
 *          by this subroutine)
 *    SWORK -  The input SWORK. Used in MODE=2,3 to determine the
 *             constituents of an SWORKO concatenated in by the
 *             current TRAN step.
 *    SWKLNK - Links output SWORKO to constituents in the input
 *             SWORK array (see SWKLNK include). */

/*  Output:
 *      Only RET. The appropriate TRTMNP arrays are loaded but
 *      this remains hidden from the rest of the TRAN programs.
 *      Declared in "TRTMNP". */

/*  CHANGES: */



void /*FUNCTION*/ npslct(
long int mode,
short  xswork[][4],short xphrbg[],short xphrnd[],
long  xphct,
short  xopadr[],short xsconp[],short xhfdop[],short *ret)
{
	static short int adxx, curtyp, headsc, hfptxx, lstxx, nacnt, newnod, 
	  npxx, nxtxx, okprep, opaptx, optmp, phihed, phixx, phpt, sc1, 
	  sc1abs, sc1x1, sc1x2, sc1x3, sc1xx, scpxx, sctmp, sub, sup, 
	  svlssz, swlnkf, swlnkl, zz;
	static char pgmnam[9] = "NPSLCT  ";

	*ret = 0;
				// get out if not word search
	if( jcaddX_.wrdsrc == 0 ||
		(passesX_.passfl == 2 && passesX_.passct == 1) )
		return;

	/*                 for each NP in XSWORK */

	for( phpt=1; phpt <= xphct; phpt++ ){
		if( sconX_.scon[xswork[phpt-One][4-One]-One][1-One] == 1 ){

			headsc = xswork[phpt-One][4-One];
			/*                      set ptr to first & last input SWORK concatenated
			 *                      with current SWORKO. */
			if( mode > 1 ){
				if( phpt > 1 ){
					swlnkf = swklnkX_.swklnk[phpt-1-One] + 1;
					}
				else{
					swlnkf = 1;
					}
				swlnkl = swklnkX_.swklnk[phpt-One];
				}

			/*                   NP inclusion criteria  -  test order is significant */


			/*              (1)  IF NP  generated from  NP PREP NP from previous TRAN
			 *                       But only if TRAN2 or TRAN3 (i.e. MODE>2). */
			if( mode <= 1 ){

				/*              (2)  IF >1 Noun or ADJ concatenated */

				nacnt = 0;
				for( optmp=xphrbg[phpt-One]; optmp <= xphrnd[phpt-One]; optmp++ ){
					/*                   check all address here, may be a VC */
					hfptxx = 1;
					while( hfptxx != 0 ){
						getadr(xopadr,xsconp,xhfdop,optmp,&hfptxx,&adxx,&scpxx,ret);
						/*                    If valid address & original element, check it. */
						if( *ret == 0 && sconX_.scolnk[scpxx-One] <= elemctX_.origct ){
							sc1 = sconX_.scon[scpxx-One][1-One];
							if( sc1 < 0 ) sc1 = -(sc1);
							if( sc1 == 4 ){
								nacnt += 1; 
								}
							else if( sc1 == 1 ){
								/*                          ignore Unfound words that are likely labels */
								sup = sconX_.scon[scpxx-One][13-One];
								sub = sconX_.scon[scpxx-One][2-One];
								if( !(sup == 1 && (sub == 228 || sub == 989)) )
									nacnt += 1;
								}
							if( nacnt > 1 )
								goto L_10001;
							}
						}
					/*C
					 *C              (3)  IF the head is a multi-word phrasal entry from dictionary.
					 *C                   Only at end of TRAN1 to avoid duplicate NPs selection.
					 *           IF (MODE.EQ.1 .AND. SCONO(54-SCONX1,HEADSC).GT.1) THEN
					 *              CURTYP=4
					 *              GOTO 1000
					 *           ENDIF
					 *                not a valid NP, continue looping thru XSWORK */
					}
				goto L_10000;
				/*                     criteria met, determine type b4 exclusion testing. */
L_10001:
				curtyp = 1;
				if( mode > 1 ){
					if( swlnkl - swlnkf > 0 )
						curtyp = 3;
					}
				}
			else if( swlnkl - swlnkf == 2 ){
				sc1x1 = sconX_.scon[sworkX_.swork[swlnkf-One][4-One]-One][0];
				sc1x2 = sconX_.scon[sworkX_.swork[swlnkf+1-One][4-One]-One][0];
				sc1x3 = sconX_.scon[sworkX_.swork[swlnkl-One][4-One]-One][0];
				if( sc1x1 < 0 )	sc1x1 = -sc1x1;
				if( sc1x2 < 0 )	sc1x2 = -sc1x2;
				if( sc1x3 < 0 )	sc1x3 = -sc1x3;
				if( !((sc1x1 == 1 && (sc1x2 == 11 || sc1x2 == 13)) && sc1x3 == 1) )
					goto L_10000;
				/*                       exclude if both NP1 and NP2 were selected
				 *                       previously. */
				curtyp = 2;
				headsc = sworkX_.swork[swlnkf-One][4-One];
				for( zz=1; zz <= trtmnpX_.npcntx[mode-1-One]; zz++ ){
					if( trtmnpX_.nphdsc[zz-One] == headsc )
						goto L_109;
					/*                          NP1 not previously selected, go with it. */
					}
				goto L_1000;
L_109:
				headsc = sworkX_.swork[swlnkl-One][4-One];
				for( zz=1; zz <= trtmnpX_.npcntx[mode-1-One]; zz++ ){
					if( trtmnpX_.nphdsc[zz-One] == headsc )
						goto L_10000;
					/*                          NP2 not previously selected, go with it. */
					}
				}
			else{
				goto L_10000;
				}

			/*                   NP EXCLUSION criteria */

L_1000:
			;

			/*                  Apply exclusion test to each element by looping
			 *                        thru the OPADR */

			for( optmp=xphrbg[phpt-One]; optmp <= xphrnd[phpt-One]; optmp++ ){
				sctmp = xsconp[optmp-One];
				/*                ignore Constants */
				if( sconX_.scolnk[sctmp-One] <= elemctX_.origct ){

					sc1abs = sconX_.scon[sctmp-One][1-One];
					if( sc1abs < 0 )
						sc1abs = -sc1abs;
					sup = sconX_.scon[sctmp-One][13-One];

					/*                  (1)  None of NP elements is a pronoun */
					if( sc1abs == 5 )
						goto L_10000;
					}

				/*C                  (2)  NP elements cannot be certain possessive adjectives
				 *              IF (SC1ABS.EQ.15) THEN
				 *                 SET=SCON(11,SCTMP)
				 *                 SUB=SCON(2,SCTMP)
				 *C                       exclude its,their,their own,...
				 *                 IF (SUP.EQ.6) THEN
				 *                    IF (SUB.EQ.222 .OR. SUB.EQ.225 .OR. SUB.EQ.226 .OR.
				 *     *                  SUB.EQ.334 .OR. SUB.EQ.336 ) GOTO 5000
				 *                 ENDIF
				 *C                       exclude all,any,some
				 *                 IF (SUP.EQ.3) THEN
				 *                    IF(SET.EQ.30 .OR. SET.EQ. 32) GOTO 5000
				 *                 ENDIF
				 *C                       exclude ???
				 *                 IF (SUP.EQ.8) THEN
				 *                    IF(SET.EQ.40 .OR. SET.EQ. 42) GOTO 5000
				 *                 ENDIF
				 *              ENDIF */

				/*C               (3)  None of NP elements is an unfound word
				 *              IF (SC1ABS.EQ.1 .AND. SUP.EQ.1) GOTO 5000 */

				}

			/*                  Load NP info into /TRTMNP/ */

			/*                  report if NPMAX limit reached, and return */
			if( trtmnpX_.npcnt == NPMAX ){
				if( diagsX_.deepdi == 1 || diagsX_.longdi == 1 )
					{
					fprintf( _spec_fp, "\n\n*** ERROR IN NPSLCT: Noun-Phrase/sentence Limit' reached.  Max = %2ld\n                       NP found at output SWORK #%3d\n                      Continuing execution.\n\n\n", 
					  NPMAX, phpt );
					}
				errlog(pgmnam,4001,NPMAX,10);
				errvrsX_.errlvl = 0;
				}
			else{
				trtmnpX_.npcnt += 1;
				trtmnpX_.nphdsc[trtmnpX_.npcnt-One] = headsc;
				trtmnpX_.nptype[trtmnpX_.npcnt-One] = curtyp;
				/*                load all actual target elements into a linked
				 *                list for this NP.  Skip null addresses. */
				svlssz = trtmnpX_.nplssz;
				for( opaptx=xphrbg[phpt-One]; opaptx <= xphrnd[phpt-One]; opaptx++ ){
					hfptxx = 1;
					while( hfptxx != 0 ){
						getadr(xopadr,xsconp,xhfdop,opaptx,&hfptxx, &adxx,&scpxx,ret);
						/*                    If valid address, append a node to list
						 *                                      and fill it. */
						if( (*ret == 0 && adxx != 0) && adxx != -140 ){
							nplsap(trtmnpX_.npcnt,&newnod,ret);
							/*                           return immed. if no more space in list */
							if( *ret != 0 )
								goto L_10002;
							trtmnpX_.nplssc[newnod-One] = scpxx;
							/*                      lock indicator */
							trtmnpX_.nplslk[newnod-One] = sconX_.scon[scpxx-One][1-One];
							}
						}
					}

				/*                   Exclude initial and final prepositions.
				 *                   They are probably not part of the NP. */
				okprep = 0;
				if( curtyp == 2 )
					okprep = sworkX_.swork[swlnkf+1-One][4-One];
				/*                           check initial address */
				scpxx = trtmnpX_.nplssc[trtmnpX_.nplsbg[trtmnpX_.npcnt-One]-One];
				sc1xx = sconX_.scon[scpxx-One][1-One];
				if( sc1xx < 0 )	sc1xx = -sc1xx;
				if( (sc1xx == 13 || sc1xx == 11) && scpxx != okprep ){
					/*                           extract initial addr from the list */
					nxtxx = trtmnpX_.nplsnx[trtmnpX_.nplsbg[trtmnpX_.npcnt-One]-One];
					if( nxtxx == 0 ){
						trtmnpX_.nphdsc[trtmnpX_.npcnt-One] = 0;
						goto L_10000;
						}
					else{
						trtmnpX_.nplsbg[trtmnpX_.npcnt-One] = nxtxx;
						}
					}
				/*                          check last address */
				scpxx = trtmnpX_.nplssc[trtmnpX_.nplsnd[trtmnpX_.npcnt-One]-One];
				sc1xx = sconX_.scon[scpxx-One][1-One];
				if( sc1xx < 0 )	sc1xx = -sc1xx;
				if( (sc1xx == 13 || sc1xx == 11) && scpxx != okprep ){
					/*                          find the penultimate address & make it the end */
					lstxx = trtmnpX_.nplsbg[trtmnpX_.npcnt-One];
					if( trtmnpX_.nplsnx[lstxx-One] == 0 ){
						trtmnpX_.nphdsc[trtmnpX_.npcnt-One] = 0;
						goto L_10000;
						}
					else{
						while( trtmnpX_.nplsnx[lstxx-One] != trtmnpX_.nplsnd[trtmnpX_.npcnt-One] ){
							lstxx = trtmnpX_.nplsnx[lstxx-One];
							}
						trtmnpX_.nplsnd[trtmnpX_.npcnt-One] = lstxx;
						trtmnpX_.nplsnx[lstxx-One] = 0;
						}
					}



				trtmnpX_.npprnt[trtmnpX_.npcnt-One] = 0;
				/*                 if MODE=2or3, find any children NPs identified from
				 *                 previous TRAN output & set NPPRNT to current NP */
				if( mode > 1 ){
					/*                      end of if(NP) block
					 *                       loop thru each constituent SWORK from prev TRAN */
					for( phixx=swlnkf; phixx <= swlnkl; phixx++ ){
						/*                          if not NP skip it */
						if( sworkX_.swork[phixx-One][1-One] == 1 ){
							phihed = sworkX_.swork[phixx-One][4-One];
							/*                          if this head is in the NPHDSC() array
							 *                          then it is a child. */
							for( npxx=1; npxx <= trtmnpX_.npcntx[mode-1-One]; npxx++ ){
								if( phihed == trtmnpX_.nphdsc[npxx-One] )
									goto L_10003;
								}
							continue;
L_10003:
							if( trtmnpX_.npprnt[npxx-One] == 0 )
								trtmnpX_.npprnt[npxx-One] = trtmnpX_.npcnt;
							}
						}
					}
				}
			}
		/*                      loop for next phrase */
L_10000:
		;
		}
	goto L_9000;
L_10002:
	trtmnpX_.nphdsc[trtmnpX_.npcnt-One] = 0;
	/*                            release sequential list space used by this NP */
	if( trtmnpX_.nplsbg[trtmnpX_.npcnt-One] != 0 )
		trtmnpX_.nplssz = trtmnpX_.nplsbg[trtmnpX_.npcnt-One];
	trtmnpX_.npcnt -= 1;

	/*          save nunber of NP id'ed at end of TRAN1. Used for
	 *          parent-child processing. */
L_9000:
	if( mode < 3 )
		trtmnpX_.npcntx[mode-One] = trtmnpX_.npcnt;


	return;
}  
/*================================================================= */

/*      NPLSAP() = Create a node and Append it to the NP list */

/*       input:
 *         LSTNDX = ptr to the NP list we are appending to (NP#)
 *       output:
 *         NEWNOD = pointer to the new node
 *         RET    = return code
 *              0 = ok
 *              1 = No more space */

void /*FUNCTION*/ nplsap(lstndx, newnod, ret)
long int lstndx;
short int *newnod, *ret;
{
	static char pgmnam[9] = "NPLSAP  ";

	*ret = 0;
	/*               Check if enough space left */
	if( trtmnpX_.nplssz == NPLSMX ){
		if( diagsX_.deepdi == 1 || diagsX_.longdi == 1 )
			{
			fprintf( _spec_fp, "\n\n*** ERROR in NPLSAP(): Cannot append address to  NP\n                      list. Size limit reached. Expand NPLSMX.\n                      Continuing execution.\n\n\n" );
			}
		errlog(pgmnam,33,NPLSMX,10);
		errvrsX_.errlvl = 0;
		*ret = 1;
		}
	else{
		/*               Append another "node" to the list */

		trtmnpX_.nplssz += 1;
		if( trtmnpX_.nplsbg[lstndx-One] == 0 ){
			/*                  list is empty for this NP, start a new one */
			trtmnpX_.nplsbg[lstndx-One] = trtmnpX_.nplssz;
			trtmnpX_.nplsnd[lstndx-One] = trtmnpX_.nplssz;
			}
		else{
			trtmnpX_.nplsnx[trtmnpX_.nplsnd[lstndx-One]-One] = trtmnpX_.nplssz;
			trtmnpX_.nplsnd[lstndx-One] = trtmnpX_.nplssz;
			}
		trtmnpX_.nplsnx[trtmnpX_.nplssz-One] = 0;
		*newnod = trtmnpX_.nplssz;

		}
	return;
} /*end of function*/

