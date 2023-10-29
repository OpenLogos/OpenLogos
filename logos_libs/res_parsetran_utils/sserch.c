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
/*    Synopsis:   NOVALUE sserch(array, arrlen, string, strlen, index)        */
/*                CHARACTER*1 *array;                                         */
/*                INTEGER*4   *arrlen;                                        */
/*                CHARACTER*1 *string;                                        */
/*                INTEGER*4   *strlen;                                        */
/*                INTEGER*4   *index;                                         */
/*                                                                            */
/*    Function:   Search array from left to right against a pattern in        */
/*                string.  Return, in index, the starting position of the     */
/*                matched pattern in array as the position from array[1].     */
/*                                                                            */
/*                Example:   array = "My dog likes bones."                    */
/*                          string = "dog"                                    */
/*                          The match will be found and index set to 4.       */
/*                                                                            */
/*                If no pattern match found index is set to 0.                */
/*                                                                            */
/*                This function looks like it could have been coded using     */
/*                some of the standard UNIX subroutines like strtok().        */
/*                The programmer must keep in mind that FORTRAN strings       */
/*                are NOT terminated with a NULL like 'C' strings and the     */
/*                standard UNIX subroutines which handle strings need the     */
/*                NULL to determine the end of the string.  A lot of work     */
/*                would be needed to use the standard UNIX subroutines.       */
/*                                                                            */
/*     WARNING:   There are no bound checks for the string length.            */
/*                                                                            */
/*     Authors:   Richard E. Oswald (REO) for Logos Computers, Inc.           */
/*                                                                            */
/*  Author   Date       Comments                                              */
/*  ------   --------   ----------------------------------------------------  */
/*  REO      08/15/87   Initial conversion from VM Assembler to 'C' routine.  */
/*                      to be called from a fortran program.                  */
/*  REO      09/01/87   Final checks, add comments.                           */
/*  REO      09/28/87   Change function call name to uppercase for Fortran    */
/*                      compiler.                                             */
/*                                                                            */
/*                                                                            */
/*----------------------------------------------------------------------------*/
#define SUCCESS   0
#define FAILURE  -1

#define YES 1
#define NO  0


void sserch(array, arrlen, string, strlen, index)
 register char *array;   /* Pointer to array to be searched.           */
 int   *arrlen;           /* Pointer to the length of array.            */
 register char *string;  /* Pointer to string with the search pattern. */
 int   *strlen;           /* Pointer to the length of string.           */
 int   *index;            /* Pointer to where return value is stored.   */
{
 int match_ctr;           /* First character match counter.             */
 short start_match;             /* Indicates if a match has started.          */
 short full_match;              /* Indicates a full match was found.          */
 char *h_array;          /* Save pointer.                              */
 char *h_string;         /* Save pointer.                              */

/*----------------------------------------------------------------------*/
/* ^^^ Check array length.                                              */
/*
 if(*arrlen < MIN_ARR_LEN || *arrlen > MAX_ARR_LEN)
 {
  *index = 0; 
  return;
 }
*/

/*----------------------------------------------------------------------*/
/* Save array/string pointers and initialize variables.                 */

 h_array  = array;
 h_string = string;
 start_match = NO;
 full_match  = NO;

/*----------------------------------------------------------------------*/
/* One pass through array to find match with string.                    */

while(array < h_array + *arrlen && !full_match)
{
	match_ctr = (start_match) ? match_ctr : array - h_array + 1;

	if(*array == *string)
	{
		start_match = YES;
		++string;
		if(string == h_string + *strlen) full_match = YES;
	}
	else
	{
		if(start_match) array = h_array + match_ctr - 1;
		start_match = NO;
		string = h_string;
	}

	++array;
}

  *index = (full_match) ? match_ctr : 0; /* Put return value in index */


}
