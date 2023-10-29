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
#include "rescommon.h"
 

void  vtr65(retsw)
short int *retsw;
{
	static short int ikm81, ikpli, iz4, m, n6jim, no2sav, nodsav[3], 
	  prm1, resnum, resvtr[26], rsnum, s, ty, vs, vtrsav[100], x;
	static long int tempi;
	static char pgmnam[9] = "RESVTR65";

	/* LAST CHG 02/12/87 *B0002GBA: LEAVE MATCHING NODE OPEN FOR NEG WC */
	iz4 = 0;
	commvX_.vtrptr = resrulX_.rule[13-One];
	resnum = 2;
	if( resswX_.ressw == 1 )
		resnum = 1;


	/*  save values from res2 match */
	memcpy(vtrsav,vtrfsX_.vtrfs,sizeof(vtrsav));
	lmove(nodsav,1,negmtcX_.negmtc,1,6);
	no2sav = negmtcX_.no2res;

	/*  load values from wc 07 match */
	lmove(negmtcX_.negmtc,1,srch07X_.n65nod,1,6);
	negmtcX_.no2res = srch07X_.n65res;

	if ( 0 == (iz4 = VTRIN(resnum, vtrfsX_.vtrfs, resrulX_.rule[13-1])) )
		goto L_8000;

	if( opswX_.sw[8-One] == 1 )
		{
		fprintf( _spec_fp, " VTR WC 07 \n       " );
		print_res_rule(_spec_fp, resnum, resrulX_.rule[13-1]);
	    //res_diag_write_rule_vtr(vtrfsX_.vtrfs);
		}

	commvX_.vn = 1;

	while( vtrfsX_.vtrfs[commvX_.vn-One] != 999 ){
		vs = -(vtrfsX_.vtrfs[commvX_.vn-One]) - 10;
		if( vs == 3 ){


			/*   sw -13 will set nodes */

			prm1 = vtrfsX_.vtrfs[commvX_.vn+1-One];
			commvX_.vn += 2;

			/*  1st param = 1 if the nodes of all the elements of match must be set */
			if( prm1 == 1 ){
				ikpli = ikX_.ik + commlX_.li - 2;
				/*+                                                        *R0002*GBA
				 *        DO 500 M = IK,IKPLI */
				for( m=ikX_.ik; m <= (ikpli - 1); m++ ){
					/*-                                                        *R0002*GBA */

					swX_.scont3[m-One] = 1;
					swX_.scont2[m-One] = 4;
					/*        IF (JGUS(M) .EQ. -1) GO TO 500 */

					for( ty=1; ty <= 3; ty++ ){
						if( ty != jbgusX_.jgus[m-One] )
							swX_.swork[m-One][ty-One] = -1;
						}

					}

				/*  if the last element of the match is a neg wc then leave the
				 *   nodes open that belong to that neg wc (N65NOD contains this info) */
				swX_.scont3[ikpli-One] = 0;
				for( ty=1; ty <= 3; ty++ ){
					if( srch07X_.n65nod[ty-One] == 1 || ty == jbgusX_.jgus[ikpli-One] ){
						swX_.scont3[ikpli-One] += 1;
						swX_.scont2[ikpli-One] = 5;
						}
					else{
						swX_.swork[ikpli-One][ty-One] = -1;
						}

					}
				}
			else{

				ikm81 = ikX_.ik - 81;
				n6jim = ikm81 - prm1;
				swX_.scont3[n6jim-One] = 1;
				swX_.scont2[n6jim-One] = 4;
				for( ty=1; ty <= 3; ty++ ){
					if( ty != jbgusX_.jgus[n6jim-One] )
						swX_.swork[n6jim-One][ty-One] = -1;
					}
				}
			}
		else{
			if( vs == 7 ){

				rsnum = 17;
				}
			else if( vs == 24 ){
				rsnum = 34;
				}
			else if( vs == 36 ){
				rsnum = 46;
				}
			else{
				goto L_1800;
				}

			tempi = rsnum;
			ressws(tempi,retsw);
			if( errvrsX_.errlvl != 0 )
				goto L_8000;
			}
		}
	goto L_1935;

L_1800:
	if( opswX_.sw[3-One] == 1 || opswX_.sw[8-One] == 1 )
		errlog(pgmnam,1800,0,9);



	/*   VTR PROCESSING IS COMPLETE. */

L_1935:

	/*  load values from res2 match */
	memcpy(vtrfsX_.vtrfs,vtrsav,sizeof(vtrfsX_.vtrfs));
	lmove(negmtcX_.negmtc,1,nodsav,1,6);
	negmtcX_.no2res = no2sav;



	*retsw = 0;
	return;

L_8000:
	if( opswX_.sw[3-One] == 1 || opswX_.sw[8-One] == 1 )
		{
		fprintf( _spec_fp, " ERR IN RESVTR65  ERRLVL = %6ld\n", errvrsX_.errlvl );
		}
	*retsw = -1;
	return;
} /*end of function*/

