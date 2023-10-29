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

/********************************************************************
 *  CLRTRN - RETURN ALL DEPENDENT CLAUSES TO THEIR ORIGINAL POSITIONS
 *           IN THE MAIN CLAUSE. DONE AT THE VERY END OF TRAN4.
 *           REORDERS: OPADR,SCONPO.  PHRBGO,PHRNDO ARE CHANGED TO
 *           REFLECT NEW OPADR CONFIGURATION. ONLY ONE CLAUSE RESULTS. */

/*    NOTE:  CLRTRN PROCESSING IS BYPASSED IF ONLY 1 CLAUSE IN SENTENCE.
 *           ALL NULL ADDRESSES ARE REMOVED IN THE PROCESS.  ALL VC'S
 *           ARE LOADED INTO OPADRO, WHETHER FROM HFDOPO OR HFPOAD.
 *           IF VC UNLOADING IS NOT DONE HERE, PROBLEMS WILL RESULT IF
 *           A CLAUSE MARKER APPEARS IN AN OVERLOAD VC ARRAY (HFPOAD)
 *           AND THE CLAUSE TO BE MOVED INTO THE VC IS BIGGER THAN THE
 *           HFPOAD ARRAY. */

/*  PROCESS: */

/*    OPADRO,SCONPO,PHRBGO,PHRNDO WILL BE MOVED TO THE INPUT ARRAYS
 *    AND THEN REORDERED WHEN THE DATA IS MOVED BACK INTO THE OUTPUT
 *    ARRAYS. */

/*    FOR EACH CLAUSE MOVE LEFT TO RIGHT THROUGH THE PHRBEG ARRAY
 *    IN A RECURSIVE LOOP.
 *    EACH LOOP LOADS ALL THE OPADR ELEMENTS IT CAN UNTIL IT ENCOUNTERS
 *    A CLAUSE MARKER ELEMENT(PLACE HOLDER FOR A DISLOCATED CLAUSE) OR
 *    IT REACHES THE END OF THE CURRENT CLAUSE.  UPON ENCOUNTERING A
 *    CLAUSE MARKER ELEMENT, CURRENT POSITIONS ARE PUSHED ONTO THE
 *    STACK AND THE NEXT LOOP STARTS WITH THE CHILD CLAUSE.  REACHING
 *    THE END OF A CLAUSE CAUSES THE NEXT LOOP TO CONTINUE WITH THE
 *    SAVED POSITIONS POPPED OFF THE STACK. THE MAIN CLAUSE IS FIRST ON
 *    THE STACK, THEREFORE ALL CLAUSES ARE EFFECTIVELY INSERTED INTO IT.
 *    REACHING THE END OF THE MAIN CLAUSE MEANS PROCESS COMPLETE. */

/*    THE FOLLOWING DATA IS PUSHED WHEN AN IMBEDDED CLAUSE IS ENCOUNTERED
 *      -  THE CURRENT CLAUSE ID IS PUSHED ONTO CLSSTK(I).
 *      -  THE CURRENT SWORK BEING MOVED IS LOADED INTO CLBGNS(CLSSTK(I))
 *      -  THE CURRENT OPADR BEING MOVED IS LOADED INTO PHRBEG. */


/*  INPUT: */

/*  OUTPUT:
 *     PHRBGO, PHRNDO, OPADRO, SCONPO */

/*  LOCAL AND I/O:
 *    CLSNFO - (TRANS MACLIB).  VITAL INFO PER CLAUSE.
 *    CLSCON - (  "     "   ).  CMCHLD() IS USED TO ID CM ELEMENTS.
 *    CLSSTK - LIST OF CLAUSES WHICH ARE IN THE PROCESS OF BEING
 *             RETURNED TO FINAL POSITION BUT NOT FULLY MOVED.
 *             VALUES REPRESENT THE CLAUSE ID(CLSID).
 *    CLBGNS - SWORK FOR A GIVEN CLAUSE TO START RELOCATION AT.
 *             FOR EACH LOOP ON THAT CLAUSE.  REFLECTS PROGRESS OF
 *             RELOCATION FOR EACH CLAUSE.
 *    PHRBEG - IF A CLAUSE MARKER IS FOUND, PHRBEG IS SET TO NEXT OPADRI
 *             POSITION WHERE RELOCATION CONTINUES WHEN RETURNING FROM
 *             MOVING THE CHILD CLAUSE. I.E. REFLECTS CURRENT RELOCATION
 *             POSITION FOR ANY GIVEN PHRASE.
 *    STKPTR - PTR TO LAST ENTRY IN MOVLST, I.E. CLAUSE CURRENTLY BEING
 *             RELOCATED.
 *    OPOPOS - THE NEXT POSITION TO LOAD IN THE OUTPUT OPADR.
 *    OPIPTR - CURRENT OPADRI ELEMENT BEING CONSIDERED FOR LOADING INTO
 *             THE OPADRO.
 *    HFPPTR - POINTER TO CURRENT ELEMENT IN HFPOAD BEING CONSIDERED
 *             FOR LOADING INTO OPADRO. ONE POINTER PER OVERFLOW VC. */

/*  CHANGES: */

/*    05/26/93 jal: fix to incorrect increment of PHRBEG(K) when
 *             a clause marker is found in a multi element VC (HFPOAD).
 *                PHRBEG(K) = OPIPTR, not OPIPTR+1 */


/******************************************************************** */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <fcrt.h>
#include "project.h"
#include "logoslib.h"
#include "trans.h"
#include "projexts.h"
#include <string.h>
#include <jbctrl.h>

void /*FUNCTION*/ clrtrn()
{
	static short int clsstk[CLSMAX], hfpend, hfpptr[HFPADY], hptr, i, k, opiptr, opopos, stkptr, tempi2, ti2;
	static long int opobyt, phobyt;

	
	if( clsnfoX_.cltotl > 1 ){
		if( diagsX_.longdi == 1 )
			{
			fprintf( _spec_fp, "\n     --- T4CLRTRN: %2d CLAUSES.  REPOSITIONING BEGINS ---\n", 
			  clsnfoX_.cltotl );
			}

		/*---------                   INIT FOR LOOP */

		/*                            MOVE THE OUTPUT ARRAYS TO INPUT ARRAYS. */
		phobyt = sworkoX_.phcto*2;
		lmove(sworkX_.phrbeg,1,sworkoX_.phrbgo,1,phobyt);
		lmove(sworkX_.phrend,1,sworkoX_.phrndo,1,phobyt);
		opobyt = opadroX_.opo*2;
		lmove(opadriX_.opadri,1,opadroX_.opadro,1,opobyt);
		lmove(opadriX_.sconpi,1,opadroX_.sconpo,1,opobyt);
		/*                            INIT OUTPUT PHRASE ARRAYS. */
		zapit(sworkoX_.phrbgo,phobyt,(byte)0);
		sworkoX_.phcto = 0;
		/*                            INIT THE MOVE STACK WITH MAIN CLAUSE. */
		clsstk[1-One] = 1;
		stkptr = 1;
		/*                            INIT OUTPUT ADDRESS COUNTER */
		opopos = 1;
		/*                            INIT VC OVERFLOW POINTERS */
		for( i=1; i <= HFPADY; i++ ){
			hfpptr[i-One] = 1;
			}

		/*---------                   LOOP RECURSIVELY THRU ALL CLAUSES
		 *                            SEEKING CLAUSE MARKERS IN THE OPADR. */

		/*                            FINISHED IF NO CLAUSES IN STACK. */
		while( stkptr != 0 ){
			/*                            GET CURRENT CLAUSE ID FROM STACK */
			clsnfoX_.clcrnt = clsstk[stkptr-One];

			/*---------                   LOOP THRU EACH PHRASE IN THE CLAUSE. */

			for( k=clsoutX_.clbgno[clsnfoX_.clcrnt-One]; k <= clsoutX_.clndno[clsnfoX_.clcrnt-One]; k++ ){
				/*                            SET THE OUTPUT PHRBEG AND PHREND VALUES,
				 *                            BUT ONLY FOR MAIN CLAUSE AND ONLY IF
				 *                            THIS PHRBEG HAS NOT BEEN RESTARTED AFTER
				 *                            BEING PUSHED TO INSERT A SUBORDINATE
				 *                            CLAUSE(IN THIS CASE PHRBGO ALREADY SET). */
				if( (clsnfoX_.clcrnt == 1) && (sworkoX_.phrbgo[k-One] == 0) ){
					sworkoX_.phcto += 1;
					sworkoX_.phrbgo[sworkoX_.phcto-One] = opopos;
					if( sworkoX_.phcto > 1 )
						sworkoX_.phrndo[sworkoX_.phcto-1-One] = opopos - 1;
					}


				/*---------                   LOOP THRU EACH OPADRI ELEMENT IN PHRBEG. */

				for( opiptr=sworkX_.phrbeg[k-One]; opiptr <= sworkX_.phrend[k-One]; opiptr++ ){

					/*                    *****   CASE OF A VC ELEMENT */

					if( opadriX_.opadri[opiptr-One] < -99 && opadriX_.opadri[opiptr-One] > -121 ){
						/*                            SKIP EMPTY VC'S */
						if( hpdopoX_.hfdopo[opiptr-One] == 0 )
							goto L_10000;
						/*                            MULIT-ELEMENT VC */
						if( hpdopoX_.hfdopo[opiptr-One] >= HFPOLO && 
						  hpdopoX_.hfdopo[opiptr-One] <= HFPOHI ){

							/*                      ****  CASE OF NON-VC ELEMENT */

							/*                            LOAD NON-NULL OVERFLOW ELEMENTS TIL CM */
							hptr = hpdopoX_.hfdopo[opiptr-One] - HFPOLO + 
							  1;
							hfpend = hfdoaX_.hfpoad[hptr-One][HFPADX-One];
							for( i=hfpptr[hptr-One]; i <= hfpend; i++ ){
								/*                               LOAD ALL NON-NULL OPADR ELEMENTS
								 *                               AND NON-VC ADDRESSES */
								if( hfdoaX_.hfpoad[hptr-One][i-One] !=  -140 &&
									(hfdoaX_.hfpoad[hptr-One][i-One] > -100 || hfdoaX_.hfpoad[hptr-One][i-One] < -120) ){
									opadroX_.opadro[opopos-One] = hfdoaX_.hfpoad[hptr-One][i-One];
									opadroX_.sconpo[opopos-One] = hfdoaX_.sconhf[hptr-One][i-One];
									opopos += 1;
									}
								/*                               IS IT A VALID CLAUSE MARKER ELEMENT ‹‹‹ */
								if( sconX_.scolnk[hfdoaX_.sconhf[hptr-One][i-One]-One] > 0 ){
									ti2 = clsconX_.cmchld[sconX_.scolnk[hfdoaX_.sconhf[hptr-One][i-One]-One]-One];
									if( ti2 > 0 && ti2 != clsnfoX_.clcrnt )
										goto L_10001;
									}
								/*                                 LOOP FOR NEXT OPADRI ELEMENT */
								}
							goto L_10000;
							/*                            LOAD ALL NON-NULL OPADR ELEMENTS */
							}
						else if( hpdopoX_.hfdopo[opiptr-One] != -140 ){
							opadroX_.opadro[opopos-One] = hpdopoX_.hfdopo[opiptr-One];
							opadroX_.sconpo[opopos-One] = opadriX_.sconpi[opiptr-One];
							opopos += 1;
							}
						/*                            LOAD ALL NON-NULL OPADR ELEMENTS
						 *winnt                   IF (OPADRI(OPIPTR).NE.-140) THEN */
						}
					else if( opadriX_.opadri[opiptr-One] != -140 && opadriX_.opadri[opiptr-One] != 0 ){
						opadroX_.opadro[opopos-One] = opadriX_.opadri[opiptr-One];
						opadroX_.sconpo[opopos-One] = opadriX_.sconpi[opiptr-One];
						opopos += 1;

						}
					else if( opadriX_.opadri[opiptr-One] == 0 ){
						if( diagsX_.longdi != 0 )
							{
							fprintf( _spec_fp, "Warning: OPADR was zero!!\n" );
							}
						}
					/*                            IS IT A VALID CLAUSE MARKER ELEMENT ‹‹‹‹‹‹‹ */
					if( sconX_.scolnk[opadriX_.sconpi[opiptr-One]-One] > 0 ){
						tempi2 = clsconX_.cmchld[sconX_.scolnk[opadriX_.sconpi[opiptr-One]-One]-One];
						if( (tempi2 > 0) && (tempi2 != clsnfoX_.clcrnt) )
							goto L_10002;
						}
					/*                              LOOP BACK FOR NEXT OPADRI ELEMENT. */
L_10000:
					;
					}

				}
			goto L_1000;
			/*                              SAVE POSITIONS AND CHILD CLAUSE ID. */
L_10002:
			sworkX_.phrbeg[k-One] = opiptr + 1;
			clsnfoX_.clbgns[clsnfoX_.clcrnt-One] = k;
			stkptr += 1;
			clsstk[stkptr-One] = tempi2;
			/*                             INCR STACK PTR SO JIVES WHEN DECREMENTED
			 *                             AT THE END OF THE LOOP. */
			stkptr += 1;
			goto L_1000;
			/*                                     PUSH STATUS VARIABLES */
L_10001:
			if( i < hfpend ){
				hfpptr[hptr-One] = i + 1;
				sworkX_.phrbeg[k-One] = opiptr;
				}
			else{
				sworkX_.phrbeg[k-One] = opiptr + 1;
				hfpptr[hptr-One] = 1;
				}
			clsnfoX_.clbgns[clsnfoX_.clcrnt-One] = k;
			stkptr += 1;
			clsstk[stkptr-One] = ti2;
			/*                                    INCR STACK PTR SO JIVES WHEN
			 *                                    DECREMENTED AT THE END OF THE LOOP. */
			stkptr += 1;
			/*                                     LEAVE THE LOOP */

			/*----------                  MOVE OPADR STRINGS FROM INPUT TO OUTPUT */

L_1000:
			;

			/*--------                    LOOP BACK TO CONTINUE LOADING. */

			/*                            DECREMENT STACK COUNTER FIRST */
			stkptr -= 1;
			}
		opadroX_.opo = opopos - 1;
		sworkoX_.phrndo[sworkoX_.phcto-One] = opadroX_.opo;


		if( diagsX_.longdi == 1 ){
			fprintf( _spec_fp, "     \n--- T4CLRTRN:   AFTER REPOSITIONING ALL CLAUSES ---\n" );
			fprintf( _spec_fp, "\n     FINAL NUMBER OF PHRASES (PHCTO)      = %3d\n     FINAL NUMBER OF OPADR2 ELEMENTS (OPO)= %3d\n\n", 
			  sworkoX_.phcto, opadroX_.opo );
			if( diagsX_.deepdi == 1 )
				diagno(9);
			}
		}
	else if( diagsX_.longdi == 1 ){
		fprintf( _spec_fp, "\n     --- T4CLRTRN:  ONLY 1 CLAUSE.  NO REPOSITIONING NEEDED  ---\n" );
		}

	
	return;
} /*end of function*/

