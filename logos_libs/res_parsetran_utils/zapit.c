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
/*    Synopsis:   INTEGER*2 zapit(array, arrlen, fillbyte)                    */
/*                CHARACTER*1 *array;                                         */
/*                INTEGER*4   *arrlen;                                        */
/*                CHARACTER*1 *fillbyte;                                      */
/*                                                                            */
/*    Function:   This routine will fill an area of storage (array) with      */
/*                a single character (fillbyte).  The length of the fill      */
/*                is determined by arrlen.  If successful zapit will          */
/*                return 0 else zapit will return 4.                          */
/*                                                                            */
/*                Example:  array = "AAAAAAAAAA"                              */
/*                       fillbyte = "Z"                                       */
/*                         arrlen = 10                                        */
/*                                                                            */
/*                       Zapit will fill array to "ZZZZZZZZZZ" and            */
/*                       return 0.                                            */
/*                                                                            */


#include <memory.h>

void zapit(array, arrlen, fillbyte)
 char *array;      /* Pointer to array.           */
long  arrlen;     /* length of array. */
long  fillbyte;   /* fill byte.       */
{

char *end_array;  /* Pointer to end of array.    */

 end_array = array + arrlen;

/*----------------------------------------------------------------------*/
/* Validate array length.                                               */

// if(arrlen < MIN_ARR_LEN || arrlen > MAX_ARR_LEN) return;

/*----------------------------------------------------------------------*/
/* Fill array with character pointed to by fillbyte.                    */

 (void) memset(array, (int) fillbyte, (unsigned) arrlen);

 return;

}
