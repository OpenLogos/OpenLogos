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
/*    Synopsis:   NOVALUE match4(element, table, tbllen, retsw)               */
/*                INTEGER*2 element[];                                        */
/*                INTEGER*2 table[];                                          */
/*                INTEGER*4 *tbllen;                                          */
/*                INTEGER*2 *retsw;                                           */
/*                                                                            */
/*    Function:   This function will search for a match on a 2 byte element   */
/*                in a given table.  The table will have a length of tbllen.  */
/*                If a 0 is found in any table match while searching the      */
/*                search is determined to be failed.  If no match between     */
/*                element and table is found the search is determined         */
/*                to be failed.  In the event of a failed match the variable  */
/*                retsw is set to 2.  If a match is found retsw is set to 1.  */
/*                                                                            */


void match4(element, table, tbllen, retsw)
 short element[];	/* Pointer to the 2 x 2 byte match field.      */
 short table[];	/* Pointer to the 2 x 2 match table.           */
 int tbllen;	/* the length of table (2x2 pairs). */
 short *retsw;	/* Pointer to the return switch.               */
{

 short  fndflag;	/* Flag.                                   */
 int i;

/* Loop through table looking for a match on 0 or element.              */

 for (i=0, fndflag=0; i< tbllen*2; i+=2)
 {
	if( (table[i]==0) && (table[i+1]==0) )	{ fndflag = 2; break; }
	if( (table[i]==element[0]) && (table[i+1]==element[1]) )
						{ fndflag = 1; break; }
 }

 *retsw = (fndflag == 0) ? 2 : fndflag;
}
