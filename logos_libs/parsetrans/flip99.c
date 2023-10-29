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
	/*     LAST CHG: 04/23/87 *R1679RKH*  OFL3B conversion & R1685 Swork limi
	 *      CHG 08/19/86 *R1561DSD: 100 SCONS
	 *      CHG 09/29/85 */
	/*     THIS FUNCTION WORKS WITH INIT, SW36-033 AND SW22 IN SAVING THE
	 *     INFORMATION NEEDED TO CREATE A SEMWRK ON A ELEMENT WHICH WAS
	 *     PREVIOUSLY LOADED IN AN EARLIER VTR. ALONG WITH THE SEMWRK IS OTHE
	 *     DATA NEEDED, SUCH AS SCON(12), ETC... */
	/*      LOAD THE SWORK WITH IT'S OLD VALUES AND SAVE THE ACTUAL
	 *     VALUES BY SWITCHING THEM AROUND.
	 *     **** USE THIS SAME BLOCK OF CODE TO RESTORE THE ACTUAL VALUES AFTE
	 *     SEMTRAN RETURN TO SWITCH -22 AND BEFORE THE CALL TO WC9 RULE. */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
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


void /*FUNCTION*/ flip99()
{
	static short om = 0;
	static short n6jim1 = 0;
	static short tmp[4]={0,0,0,0};

	n6jim1 = sav36sX_.sav36s[0];

	/*     FLIP FLOP SWORK(1-4) WITH SAV36S(2-5) */

	lmove(tmp,1,&sworkX_.swork[n6jim1-One][0],1,8);
	lmove(&sworkX_.swork[n6jim1-One][0],1,&sav36sX_.sav36s[1],1,8);
	lmove(&sav36sX_.sav36s[1],1,tmp,1,8);

	/*     FLIP FLOP FORMSV(N6JIM1) WITH SAV36S(6) */
	tmp[0] = formsaX_.formsv[sconX_.scolnk[n6jim1-One]-One];
	formsaX_.formsv[sconX_.scolnk[n6jim1-One]-One] = sav36sX_.sav36s[6-One];
	sav36sX_.sav36s[6-One] = tmp[0];

	/*     FLIP FLOP SCON11, SCON12 AND SCON 13 WITH SAV36S(7-9) */
	om = sworkX_.phrhed[n6jim1-One];
	/*+ OFL3B conversion & R1685 Swork limit Change RKH  04/23/87   R1679 */
	lmove(&tmp[0],1,&sconX_.scon[om-One][11-One],1,2);
	lmove(&tmp[1],1,&sconX_.scono[sconX_.scolnk[om-One]-One][45-SCONX1-One],1,2);
	lmove(&tmp[3-One],1,&sconX_.scon[om-One][13-One],1,2);
	lmove(&sconX_.scon[om-One][11-One],1,&sav36sX_.sav36s[7-One],1,2);
	lmove(&sconX_.scono[sconX_.scolnk[om-One]-One][45-SCONX1-One],1,&sav36sX_.sav36s[8-One],1,2);
	lmove(&sconX_.scon[om-One][13-One],1,&sav36sX_.sav36s[9-One],1,2);
	lmove(&sav36sX_.sav36s[7-One],1,tmp,1,6);

	return;
} /*end of function*/

