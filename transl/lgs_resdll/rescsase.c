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
	/*+  LASTCHG:  R....GBA 01/2287 10:30
	 *+            SUBROUTINE SAVSET ADDED FOR SW-17
	 *+            ARRAY CSASAV ADDED FOR SW-17 */
	/*   THIS SUBROUTINE IS CALLED TO SET THE CLAUSE STATUS ARRAY
	 *   TO CORRESPOND TO THE CLAUSE STATUS POSITIONS IN
	 *   THE CELL ARRAY */
	/*   CELNUM IS THE POSITION IN THE CELL ARRAY TO USE
	 *   N6JIM IS THE POSITION IN THE SENTENCE TO BEGIN SETTING
	 *   THE CLAUSE STATUS ARRAY */

#include "rescommon.h"

void /*FUNCTION*/ csaset(celnum, n6jim, retflg)
long int celnum, n6jim;
short int *retflg;
{
	long int ms = 0;

	*retflg = 0;

	if( !(celnum < 1 || celnum > 40) ){

		for( ms=n6jim; ms <= swX_.ecount; ms++ ){
			cellX_.csaray[ms-One][celnum-One] = cellX_.cell[celnum-One];

			}
		}
	return;


} /*end of function*/

	/*   THIS SUBROUTINE IS CALLED TO SET THE CLAUSE STATUS ARRAY SAVEAREA
	 *   TO CORRESPOND TO THE CLAUSE STATUS POSITIONS IN
	 *   THE CELL ARRAY FOR CELLS 51-80. */
	/*   N6JIM IS THE POSITION IN THE SENTENCE TO BEGIN SETTING
	 *   THE CLAUSE STATUS ARRAY */

void  savset(n6jim, retflg)
long int n6jim;
short int *retflg;
{
	long int ms = 0;

	*retflg = 0;

	for( ms=n6jim; ms <= swX_.ecount; ms++ ){
		memcpy(cellX_.csasav[ms-One],
			   &cellX_.cell[51-One],
			   sizeof(cellX_.csasav[0]));
		}
	return;


} /*end of function*/

