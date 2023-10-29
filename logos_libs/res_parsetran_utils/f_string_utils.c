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

#include <string.h>


 /* f_strcmp() effectively blank fills the shorter str for the comparison,
		but otherwise functions the same as strcmp() */
int f_strcmp( left, right )	/* F77 str comparison */
char *left, *right;	/* retns <0 if l < r, 0 if equal, or >0 if l > r */
{
	int l, r;

	if( left==NULL || right==NULL )
		return( left ? 1 : (right? -1: 0) );

	while( *left || *right ){
		l = *left ? *left++ : ' ';
		r = *right ? *right++ : ' ';
		if( l != r )
			return( l - r );
			/* NOTE: This function assumes an ASCII character set!
				If not, then the character set should be designed
				so that the values of the characters correspond to
				the position of the letters in the alphabet, (i.e.,
				'a' should be < 'b'), otherwise the function will 
				need to be re-written as appropriate. */
		}

	return( 0 );	/* equal strings */
}
