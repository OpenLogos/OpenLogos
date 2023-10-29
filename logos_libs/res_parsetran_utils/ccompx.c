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
/*
 *	$Log: ccompx.c,v $
 *	Revision 1.3  2005/11/02 16:14:27  kiefer
 *	Changed the functionality of lexit to simply return a value because the
 *	pthread cleanup functions did not get called when pthread_exit was called
 *	directly (a problem with the dynamic libraries? It remains unclear).
 *	
 *	Removed all CR characters.
 *	
 *	Revision 1.2  2005/10/25 09:10:24  kiefer
 *	Added disclaimer comment in source files.
 *	Made ready for distribution.
 *	
 *	Revision 1.1  2005/08/09 08:56:11  kiefer
 *	
 *	
 *	Initial check in.
 *	
 *      
 *         Rev 1.1   01/19/95 14:12:42   stevez
 *      import771
 *      
 *         Rev 1.0   04/05/94 15:08:06   pvcs
 *      Initial revision.
 */
 
/*----------------------------------------------------------------------*/
/*									*/
/* NAME:	CCOMPX - compare two strings of bytes			*/
/*									*/
/* SYNOPSIS:    CCOMPX (field1, offset1, field2, offset2, count, retsw) */
/*		CHARACTERx1	*field1, *field2;			*/
/*		INTEGERx4	offset1, offset2, *count;		*/
/*		INTEGERx2	*retsw;					*/
/*									*/
/* DESCRIPTION:	This routine is to compare two strings of bytes.  If    */
/*		two strings are identical, then the return switch(retsw)*/
/*		is set to 0.  If the two strings are not identical or   */
/*		the displacement value is not between 0 to 65000, then  */
/*		the return switch is set to 1.				*/
/*									*/
/* ARGUMENTS:	field1, offset1, field2, offset2, count, retsw.         */
/*									*/
/* RETURN VALUE: A two byte integer value.				*/
/*									*/
/* WARNING:								*/
/*									*/
/* AUTHORS:	PETER S. SIMMCOX					*/
/*									*/
/* AUDIT TRAIL :-							*/
/* Author	Date	Comments					*/
/* ------------ ------  ----------------------------------------------- */
/* PSS         11/05/87 Initial conversion from VM Assembler to 'C'     */
/*			routine to be called from fortran program.      */
/* HNP         11/07/87 add comments.					*/
/*									*/
/*----------------------------------------------------------------------*/

void ccompx (field1, offset1, field2, offset2, count, retsw)
char	*field1, *field2;
long int	offset1, offset2, count;
short	*retsw;
{

char	*p_fld1, *p_fld2, *p_endfld1;

*retsw=0;

p_endfld1=field1+(offset1-1)+count;

for (	p_fld1=field1+(offset1-1), p_fld2=field2+(offset2-1);
		p_fld1<p_endfld1;
		p_fld1++, p_fld2++	)
{
	if (*p_fld1 != *p_fld2)
	{
		*retsw=1;
		break;
	}
}

return;

}
