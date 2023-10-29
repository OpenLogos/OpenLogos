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
/*----------------------------------------------------------------------*/
/*									*/
/* NAME		  DSTRIP - return the quotient and the remainder of an  */
/*			   integer value.				*/
/*									*/
/* SYNOPSIS	  DSTRIP (arg1, arg2, arg3)				*/
/*		  short *arg1, *arg2, *arg3;				*/
/*									*/
/* DESCRIPTION	  These functions will place the quotient of the        */
/*		  variable arg3 into variable arg1 and the remainder    */ 
/*		  into variable arg2.                                   */
/*									*/
/*		  Example:						*/
/*		     arg1 = 0						*/
/*		     arg2 = 0						*/
/*		     arg3 = 1234					*/
/*									*/
/*		  Result:						*/
/*		     for VALTHS, arg3 = 1234, arg1 = 1, arg2 = 234      */
/*		     for VALHUN, arg3 = 1234, arg1 = 2, arg2 = 34       */
/*		     for VALTEN, arg3 = 1234, arg1 = 3, arg2 = 4        */
/*									*/
/*									*/
/* ARGUMENTS	  arg1, arg2, arg3.					*/
/*									*/
/* RETURN VALUE	  Two byte integer value.				*/
/*									*/
/* WARNING	  The length of arg3 can not be greater than 4 digits.  */
/*									*/
/*		- If the value of the variable arg3 is a four digits    */
/*		  number, then the function VALHUN will only consider   */
/*		  the last 3 digits.					*/
/*									*/
/*		- If the value of the variable arg3 is more than three  */
/*		  digits, then the function VALTEN will only consider   */
/*		  the last 2 digits.					*/
/*		  
/*									*/
/* AUTHORS	  Hoa N. Phung (Logos Corporation)			*/
/*									*/
/* AUDIT TRAIL :-							*/
/* Author	Date	Comments					*/
/* ------------ ------  ----------------------------------------------- */
/*  HNP        09/30/87 Initial conversion from VM Assembler to 'C'     */
/*		   	routine to be called from a fortran program.    */
/*  HNP        10/15/87 Final check, add comments.                      */
/*  HNP	       10/26/87 Minor modification, add comments.		*/
/*									*/
/*----------------------------------------------------------------------*/

 
void valths(arg1, arg2, arg3)
short *arg1, *arg2;
short  arg3;
{
	*arg1 = 0;

	if (arg3 > 9999)
	return;

	*arg1 = arg3 / 1000;
	*arg2 = arg3 % 1000;
}


void valhun(arg1, arg2, arg3)
short *arg1, *arg2;
short  arg3;

{
	int  	tmpval;

	*arg1 = 0;
	tmpval = arg3;


	if (arg3 > 999) tmpval =  arg3 % 1000;

	*arg1 = tmpval / 100;
	*arg2 = tmpval % 100;
}


void VALTEN(arg1, arg2, arg3)
short *arg1, *arg2;
short  *arg3;

{
	int  tmpval;

	*arg1 = 0;
	tmpval = *arg3;

	if (*arg3 > 99)
	tmpval = *arg3 % 100;

	*arg1 = tmpval / 10;
	*arg2 = tmpval % 10;
}
