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
	/*
	 *     LAST CHG: 04/17/87 *R1685RKH*  Change T1-4 SWORK limit from 50 to
	 *      CHG 08/19/86 *R1561DSD: 100 SCONS
	 *      CHG 08/30/85
	 *
	 *   THIS SUBROUTINE SETS THE SWORK TO THE VALUES SET BY THE
	 *   -26 SWITCH.
	 *
	 *   IT IS CALLED BY THE MAIN TRAN PROGRAM IN 5 PLACES:
	 *     MAIN, SW24, SW26, SCON AND BOT, AND MOPUP
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
#include "parsetrans_ext.h"
#include <jbctrl.h>


void /*FUNCTION*/ set26()
{


	if( head26X_.headwc != 0 )
		sworkoX_.sworko[sworkoX_.phcto-One][1-One] = head26X_.headwc;
	if( head26X_.headty != 0 )
		sworkoX_.sworko[sworkoX_.phcto-One][2-One] = head26X_.headty;
	if( head26X_.headfr != 0 )
		sworkoX_.sworko[sworkoX_.phcto-One][3-One] = head26X_.headfr;

	if( head26X_.headhd != 0 )
		sworkoX_.phrhdo[sworkoX_.phcto-One] = sworkX_.phrhed[head26X_.headhd-One];
	if( head26X_.headhd != 0 )
		sworkoX_.sworko[sworkoX_.phcto-One][4-One] = sworkX_.swork[head26X_.headhd-One][4-One];

	if( head26X_.headt2 != 0 )
		sconX_.scon[sworkoX_.phrhdo[sworkoX_.phcto-One]-One][11-One] = head26X_.headt2;
	if( head26X_.headt3 != 0 )
		sconX_.scon[sworkoX_.phrhdo[sworkoX_.phcto-One]-One][13-One] = head26X_.headt3;

	head26X_.headwc = 0;
	head26X_.headty = 0;
	head26X_.headfr = 0;
	head26X_.headhd = 0;
	head26X_.headt2 = 0;
	head26X_.headt3 = 0;
	sw26nX_.sw26n = 0;


	return;
} /*end of function*/

