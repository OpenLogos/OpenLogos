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
/*                                                                            */
/*    Synopsis:   INTIGER*2 LMOVE(target, toffset, source, soffset, length)    */
/*                CHARACTER*1 *target;                                        */
/*                INTEGER*4   *toffset;                                       */
/*                CHARACTER*1 *source;                                        */
/*                INTEGER*4   *soffset;                                       */
/*                INGEGER*4   *length;                                        */
/*                                                                            */
/*    Function:   This function moves bytes from the source array to the      */
/*                target array for a specified length.  Offsets into both     */
/*                the target and source arrays can also be specified.         */
/*                Target[0] and source[0] are considered position 1.          */
/*                                                                            */
/*                Return values are 0 for successful and 4 for not successful.*/
/*                                                                            */
/*                Example:  target = "ZZZZZZZZZZZZZZZZZZZZ"                   */
/*                          source = "ABCDEFGHIJ"                             */
/*                          toffset = 10                                      */
/*                          soffset = 2                                       */
/*                          length  = 5                                       */
/*                The resultant target string would look like:                */
/*                          target = "ZZZZZZZZZBCDEFZZZZZZ"                   */
/*                          return value = 0                                  */
#include <memory.h>


void lmove(target,toffset,source,soffset,length)
 char *target;   /* Address of target array                          */
long   toffset;  /* Offset into target array, 0 offset = target[0]   */
 char *source;   /* Address of source array                          */
long   soffset;  /* Offset into source array, 0 offset = source[0]   */
long   length;   /* Segment length to be moved from source to target */
{




/*----------------------------------------------------------------------*/
/* Check bounds of length.                                              */

// if(length < MIN_ARR_LEN || length > MAX_ARR_LEN) return(1);

/*----------------------------------------------------------------------*/
/* ^^^ Check bounds of toffset and soffset.                             */

// if(toffset < MIN_ARR_LEN || toffset > MAX_ARR_LEN) return(1);
// if(soffset < MIN_ARR_LEN || soffset > MAX_ARR_LEN) return(1);

/*----------------------------------------------------------------------*/
/* ^^^ Check for offset + length overflow of MAX_ARR_DISP.              */

// if(toffset + length > MAX_ARR_DISP) return(FAILURE);
// if(soffset + length > MAX_ARR_DISP) return(FAILURE);

// target = target + toffset - 1;  /* -1 will make relitive to target[0] */
// source = source + soffset - 1;  /* -1 will make relitive to source[0] */
 (void) memcpy((target + toffset - 1), (source + soffset - 1), (unsigned) length);

 return;

}
