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

/*    GETADR() =   get next OPADR address and SCON pointer. */

/*    input:
 *      XOPADR = OPADR array containing desired addresses.
 *      XSCONP = SCONP array containing desired SCON pointers.
 *      XHFDOP = HFDOP array containing desired VC addresses or
 *               overflow pointers.
 *       OPAPT = position in XOPADR to get address from
 *       HFPPT = Location in VC bucket to get address from. */

/*    output:
 *        ADDR =  OPADR address value.
 *        SCNPT = SCON pointer for this address.
 *        HFPPT = Next VC location in VC at OPAPT.
 *                0 = No more VC addresses at OPAPT.
 *               >0 = position of next VC address.
 *        RET  =  return code.
 *           0 =  address was good.
 *           1 =  address was bad. ADDR & SCNPT not set. HFPPT = 0. */



void /*FUNCTION*/ getadr(xopadr, xsconp, xhfdop, opapt, hfppt, addr, 
	  scnpt, ret)
short int xopadr[], xsconp[], xhfdop[];
long int opapt;
short int *hfppt, *addr, *scnpt, *ret;
{
	static short int hfadr, hfdxx, hfpsiz, opaxx;

	*ret = 0;
	if( opapt > OPADRX ){
		*ret = 1;
		*hfppt = 0;
		}
	else{
		opaxx = xopadr[opapt-One];
		/*           case 1:  Non VC address */
		if( opaxx > -VCADLO || opaxx < -VCADHI ){
			*addr = opaxx;
			*scnpt = xsconp[opapt-One];
			*hfppt = 0;
			/*           case 2: VC address */
			}
		else{
			hfdxx = xhfdop[opapt-One];

			/*                     Case 2A: overflow VC */
			if( hfdxx >= HFPOLO && hfdxx <= HFPOHI ){
				hfadr = hfdxx - HFPOL1;
				hfpsiz = hfdoaX_.hfpoad[hfadr-One][HFPADX-One];
				/*                  Initialize VC ptr if 1st time in */
				if( *hfppt <= 0 ){
					*hfppt = 1;
					/*                  Check if a valid HFPPT value */
					}
				else if( *hfppt > hfpsiz ){
					*ret = 1;
					*hfppt = 0;
					return;
					}
				*addr = hfdoaX_.hfpoad[hfadr-One][*hfppt-One];
				*scnpt = hfdoaX_.sconhf[hfadr-One][*hfppt-One];
				*hfppt += 1;
				if( *hfppt > hfpsiz )
					*hfppt = 0;

				/*                     Case 2B: single-address VC */
				}
			else{
				*addr = xhfdop[opapt-One];
				*scnpt = xsconp[opapt-One];
				*hfppt = 0;


				}
			}
		}
	return;
} /*end of function*/

