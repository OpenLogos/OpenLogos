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

/*  NOTE‹‹‹  Because this routine is frequently invoked by one the most
 *  CPU intensive algorithm of the RES module, i.e. the matching routine,
 *  this program was written using GOTOs and single dimensionned arrays
 *  to streamline it's execution.  We will try to explain this below. */
	/*
	 *    The calling routine must point to the first Word Class field
	 *    in the RULE array, as this could be coming from either an SP
	 *    record or an Overflow record. (The WC starts at 2 for the SP0.
	 *
	 *     ARRAY refers to the SWORK array, normally referred to as a double
	 *     dimensioned array.  Using a single dimensioned makes its quicker
	 *     to refer values.  The calling convention is that we point to the
	 *     the first column of the nth row of the SWORK array to be checked.
	 *
	 */

	/*  Word Class:
	 *  We compare the incomeing rule's WC'S to the WC'S in the SWORK array.
	 *  If the rule has a negative value, the compare is inconclusive so the
	 *  the checking continues as still possible.
	 *  If the rule has a 0, we have reached the enbd of WC checking without
	 *  being able to prelcude a match -- Go on to check form.
	 *  If the rule's WC vlue does not match any of the WC'S in the SWORK
	 *  for this word, set RETSW = 2.
	 * FORM
	 *  We compare the incoming rule's forms to the forms in the SWORK array
	 *  once we finish the WC tests.  If the rule has a negative value or a
	 * or a superform value (i.e. GT 19), a compare would be inconclusive
	 *  so the checking continues since a match is still possible.
	 *  If the rule has a 0, we have reached the 3end of form checking
	 *  without being able to preclude a match -- Set RETSW = 1.
	 *  If the rule's form value does not match any of the forms in the
	 *  SWORK for this word, set RETSW = 2.
	 *     START LOOKING IN THE SWORK IN POSITION 4
	 *                   IN THE RULES IN POSITION 1 */

#include "rescommon.h"


void   rulchk(rule, array, count, retsw)
short int rule[], array[];
long int count;
short int *retsw;
{
	static long int rulpos, sppos, sworkp, wcpos, xxx;


	/*     CHECK FIRST THE WC FIELDS.  You can make an early decision there. */
	wcpos = 3;
	rulpos = 0;
	for( xxx=0; xxx < count; xxx++ ){
		sppos = rule[rulpos];

		/*    IF SPPOS < 0, DON'T CARE FIELD, GET NEXT WC
		 *    IF SPPOS = 0, DONE CHECKING WC, GO CHECK THE FORM FIELDS
		 *    IF SPPOS > 0, POSTIVE VALUE, GO CHECK AGAINST THE SWORK WC FIELDS. */

		if( sppos >= 0 ){
			if( sppos == 0 )break;
						// check the wc against the 3 possible parts of speech in the swork
						// exit if non match
			if( sppos != array[wcpos] ){
				if( sppos != array[wcpos+4] ){
					if( sppos != array[wcpos+8] )goto L_999;
					}
				}
			}
		/*                       Set up for the next WC field in ARRAY and RULE.
		 *   BUMP UP THE POINTER TO THE WC FOR THE RULE BY 3, FOR THE SWORK BY 15 */
		rulpos += 3;
		wcpos += 15;
		}

	/*    Check the form positions.  If the form value LE 19, then it must
	 *    be an exact match.  GT 19 is a Superform, we cannot decide here
	 *    if it matches or not. */
	wcpos = 5;
	rulpos = 2;
	for( xxx=0; xxx < count; xxx++ ){
		sppos = rule[rulpos];

		/*    IF SPPOS < 0, DON'T CARE FIELD, GET NEXT FORM
		 *    IF SPPOS = 0, DONE CHECKING Forms, return with possible match flag.
		 *    IF SPPOS > 0, POSTIVE VALUE, Go check against the form fields. */

		if( sppos >= 0 ){
			if( sppos == 0 )break;

			/*               Is the Form in the Superform class??? */
			if( sppos <= 19 ){
				if( sppos != array[wcpos] ){
					if( sppos != array[wcpos+4] ){
						if( sppos != array[wcpos+8] ) goto L_999;
						}
					}
				}
			}
		rulpos += 3;
		wcpos += 15;
		}


	/*       500 -  Possible match, return RETSW = 1 */
	*retsw = 1;
	return;

	/*       999 -  No match, return RETSW = 2 */
L_999:
	*retsw = 2;
	return;
} /*end of function*/



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 *     Entry: RULCHS */

/*      For RESSEM
 *  Routine to check an incoming rule against the sentence SEMworks,
 *  to see if we can eliminate this rule solely on the basis of
 *  a non-match on WC or FORM. */

/*    CALL RULCHS (RULE, SEMWORK, SIZE, RETSW) */

/*   RULE   is the rule to examine.
 *   SEMWORKis the start of the SEMwork.
 *   SIZE   is the max number of elements in this rule
 *   RETSW  is the returned switch,  1 = possible match, 2 = not a match
 *             (I*2)
 * WORD CLASS
 * We compare the incoming rule's WC's to the WC's in the SEMwork array.
 *  If the rule has a negative value a compare would be inconclusive
 *    so the checking continues as still possible.
 *  If the rule has a 0, we have reached the end of WC checking
 *    without being able to preclude a match -- go on to check FORM.
 *  If the rule's WC value does not match the WC in the
 *     SEMWORK for this pos. set RETSW to 2. */

/* FORM
 * We compare the incoming rule's FORMs to the FORMs in the SEMworks.
 *  If the rule has a negative value or a SUPERFORM value (ie GT 19)
 *    a compare would be inconclusive so the checking continues
 *    since a match is still possible.
 *  If the rule has a 0, we have reached the end of FORM checking
 *    without being able to preclude a match -- set RETSW to 1.
 *  If the rule's FORM value does not match the FORM in the
 *     SEMWORK for this pos. set RETSW to 2. */

 	/*
	 *     ARRAY refers to the SWORK array, normally referred to as a double
	 *     dimensioned array.  Using a single dimensioned makes its quicker
	 *     to refer values.  The calling convention is that we point to the
	 *     the first column of the nth row of the SWORK array to be checked.
	 *
	 */
	/*     START LOOKING IN THE SWORK IN POSITION 4
	 *                   IN THE RULES IN POSITION 1 */


void /*FUNCTION*/ rulchs(rule, array, count, retsw)
short int rule[], array[];
long int count;
short int *retsw;
{
	static long int rulpos, sppos, sworkp, wcpos, xxx;

	wcpos = 1;
	rulpos = 1;
	/*     CHECK FIRST THE WC FIELDS.  You can make an early decision there. */
	for( xxx=1; xxx <= count; xxx++ ){
		sppos = rule[rulpos-One];

		/*    IF SPPOS < 0, DON'T CARE FIELD, GET NEXT WC
		 *    IF SPPOS = 0, DONE CHECKING WC, GO CHECK THE FORM FIELDS
		 *    IF SPPOS > 0, POSTIVE VALUE, GO CHECK AGAINST THE SWORK WC FIELDS. */

		if( sppos >= 0 ){
			if( sppos == 0 )break;
			if( sppos != array[wcpos-One] ) goto L_999;
			}
		rulpos += 3;
		wcpos += 7;
		}

	/*    Check the form positions.  If the form value LE 19, then it must
	 *    be an exact match.  GT 19 is a Superform, we cannot decide here
	 *    if it matches or not. */
	wcpos = 3;
	rulpos = 3;
	for( xxx=1; xxx <= count; xxx++ ){
		sppos = rule[rulpos-One];

		/*    IF SPPOS < 0, DON'T CARE FIELD, GET NEXT FORM
		 *    IF SPPOS = 0, DONE CHECKING Forms, return with possible match flag.
		 *    IF SPPOS > 0, POSTIVE VALUE, Go check against the form fields. */

		if( sppos >= 0 ){
			if( sppos == 0 )break;
			/*               Is the Form in the Superform class??? */
			if( sppos <= 19 ){
				if( sppos != array[wcpos-One] )	goto L_999;
				}
			}
		rulpos += 3;
		wcpos += 7;
		}

	/*       500 -  Possible match, return RETSW = 1 */
	*retsw = 1;
	return;

	/*       999 -  No match, return RETSW = 2 */
L_999:
	*retsw = 2;
	return;
} /*end of function*/


