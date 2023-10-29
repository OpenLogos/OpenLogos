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
	/*  Changes:
	 *    06/02/94 jal: fix memory leak. PRTIDX,PRMIDX not checked
	 *          for range limitations.
	 *          AVK: 12/10/93  Added logical expression capability for 8000 tagsets
	 *                         and ability to stretch over scon values
	 *          AVK: 11/04/93  Now able to read a 4-line long 8000 tagset
	 *                         (same as for other tagsets)
	 *          CHG: 04/17/87 *R1685RKH*  Change T1-4 SWORK limit from 50 to
	 *          CHG: 02/26/87 *NORKH*  Diagonostics as per BES
	 *      CHG 08/22/86 *R1561DSD: 100 SCONS
	 *      CHG 03/19/86 */
	/*     CALLED BY P0614 - SPSRC3
	 *     TRAN 3 SP SEARCH/MATCHING PROGRAM */
	/*     ARGUMENTS: */
	/*           RETFLG - INDICATES RETURN (0 = MATCH,   1 = NO MATCH) */
	/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
	/*        PRG. REQ. #1716 - ADD A 'TAGSET' CAPABILITY TO TEST
	 *        FOR WORD CLASS, TYPE AND FORM MATCHING. */
	/*        THAT IS, IF THE TAGSET VALUE IS 8000 THEN THE TAGSET LINE WILL
	 *        CONTAIN UP TO 6 SETS OF WC-TY-FRM TO MATCH AGAINST THE SWORK */
	/*        ZERO IS DON'T CARE VALUE
	 *        A 1 IN THE 'THOUSANDS' (1ST) DIGIT OF ANY OF THE
	 *        (3) 4-DIGIT FIELDS SIGNIFIES EXCLUDE MATCH */

	/*+ AVK Added TAGBLK common and other required variables */
	/*     TAGSET - Tag Data to be retrieved from the Overflow File
	 *     TAGSAV - Tag Data Line to be processed
	 *     TAGPTR - It contains the overflow file pointer for tag data
	 *     NSPTR  - Points to SWORK record for the Tag encountered
	 *     TAGLIN - Points to the current tag line being processed
	 *     TAGCOL - Always points to current tag element in a tagset
	 *     NXTCOL - Points to next tag to be processed in a line
	 *     TAGEND - Points to the last tag element for current tag
	 *     NUMELM - Number of elements in current tag excluding TAG ID. */
	/* STRING- Logical expression string to be evaluated */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#define MS_F77
#include <fcrt.h>
#include "logoslib.h"
#include "trans.h"
#include "projexts.h"
#include "parsetrans.h"
#include <jbctrl.h>
#include "parsetrans_ext.h"



void /*FUNCTION*/ tmatch(locflg, retflg, rule_number)
long int *locflg;
short int *retflg;
short rule_number;
{
	static LOGICAL32 andseen, orseen, rparen;
	short k21,k22,k23;
	static byte string[500];
	static short int i, i3, jj, nsav, strptr, tag1, tagbeg, tagcnt, 
	  tagend, taglin, tagsav[84], tagset[21], tagsets[84];
	static short t = 0;
	static short xx = 0;
	static short key = 0;
	static short igus = 0;
	static short field = 0;
	static short phri3 = 0;
	static short retsw = 0;
	static short tgset = 0;
	static short wctst = 0;
	static short excfrm = 0;
	static short exclud = 0;
	static short exctyp = 0;
	static short expair = 0;
	static short frmtst = 0;
	static short gotest = 0;
	static short parti3 = 0;
	static short startl = 0;
	static short typtst = 0;


	*retflg = 0;


	/*+ Initialize logical expression STRING and STRPTR */
	memset(string,' ',sizeof(string));
	strptr = 0;

	/* Initialize line number to read each tagset line from file */
	xx = tagblkX_.tagptr[1-One];

	/* Initialize the index to copy from each tag line (TAGSET) to TAGSETS */
	tagcnt = 0;

	/* >>>> Begin the "2" loop for a maximum of 4 "8000" tagset lines <<<<<<< */

	for( taglin=1; taglin <= 4; taglin++ ){

		/* Read the line */
		ovrin((short*)locflg,tagset, rule_number, xx);

		/* Put this line into PRMTAG or PRTTAG depending on whether we are
		 * using a mini or complete rulebase for diagnostics output. */
		if( taglin > 1 ){
			if( *locflg == 1 ){
				/*									       <* If complete rulebase *> */
				if( prttagX_.prtidx < 4 ){
					prttagX_.prtidx += 1;
					lmove(&prttagX_.prttag[prttagX_.prtidx-One][1-One],
					  1,tagset,1,42);
					}
				/*	/										* Else if mini rulebase *> */
				}
			else if( prmtagX_.prmidx < 4 ){
				prmtagX_.prmidx += 1;
				lmove(&prmtagX_.prmtag[prmtagX_.prmidx-One][1-One],
				  1,tagset,1,42);
				}
			}

		/* Increment line number to read in file */
		xx += 1;

		/* Copy the current tag line into TAGSETS
		 * Check if there is an 8888 continuation marker at the end */
		for( i=1; i <= 21; i++ ){
			tagcnt += 1;
			if( tagset[tagcnt-One] == 8888 )
				goto L_9001;
			tagsets[tagcnt-One] = tagset[i-One];

			/* If you reach here, it is because there was no continuation marker
			 * Skip the rest of the 2 loop */
			}
		break;
L_9001:
		if( taglin == 4 )
			{
			fprintf( _spec_fp, "8000 tagset exceeds 4 line limit: only first 4 lines evaluated\n" );
			}
		tagcnt -= 1;

		}

	/* Initialize the TAGBEG marker to copy from TAGSETS to TAGSAV */
	tagbeg = 1;

	/* Loop through 6 many times until TAGSETS is completely processed */

	while( tagbeg <= tagcnt ){

		for( nsav=1; nsav <= 84; nsav++ ){
			tagsav[nsav-One] = 0;
			}

		nsav = 0;

		/* Set all markers to false */
		andseen = FALSE;
		orseen = FALSE;
		rparen = FALSE;

		/* Process all 8000 tags in the current line and build up the logical expression string */
		for( jj=tagbeg; jj <= tagcnt; jj++ ){

			tag1 = tagsets[jj-One];

			if( (tag1 == 2222 || tag1 == 5555) || tag1 == 7777 )
				goto L_9002;
			if( tag1 == 1111 ){
				strptr += 1;
				string[strptr-One] = '(';
				}
			else if( tag1 != 8000 ){
				nsav += 1;
				tagsav[nsav-One] = tag1;
				}

			}
		goto L_9;
L_9002:
		if( tag1 == 2222 )
			rparen = TRUE;
		if( tag1 == 5555 )
			orseen = TRUE;
		if( tag1 == 7777 )
			andseen = TRUE;
		tagbeg = jj + 1;

		/* Break out of loop to evaluate the current 8000 tag
		 * Set I3 to the value that was passed in as a parameter before */

L_9:
		i3 = tagblkX_.nsptr[1-One];

		/* Determine the end of the 8000 tag */

		for( i=1; i <= 84; i += 3 ){
			if( (tagsav[i-One] == 0 && tagsav[i+1-One] == 0) && tagsav[i+2-One] == 
			  0 )
				goto L_9003;
			}
		goto L_11;
L_9003:
		tagend = i - 1;

L_11:
		;

		/*     TAGSAV contains the tag to be processed
		 *     Now proceed with the rest of this program as it was before.
		 *+ AVK */

		/*        CALCULATE SUBSCRIPTS FOR 'SWTEST' tran 1 */
		k21 = 4*prctX_.js[sconX_.scolnk[i3-One]-One];
		k22 = k21 + 1;
		k23 = k21 + 2;


		/*        PAIR MATCH - 2 'WC-TYP-FRM' SETS MUST MATCH PAIR OF NSWORKS
		 *        'PAIR' FLAG WILL INDICATE IF THIS IS 1ST OR 2ND PART
		 *        IF TGPAIR IS 0 - NO SPECIAL TEST
		 *        IF TGPAIR IS 1 - 'PAIR MATCH' TEST - LOOK FOR 1ST PART
		 *                         IF NO MATCH, CONTINUE TESTING FOR A 'SINGLE'
		 *                         ON 'NON-PAIR' WC-TYP-FRM SETS OF TAG LINE
		 *        IF TGPAIR IS 2 - 1ST PART MATCHED, TEST FOR 2ND PART -
		 *                         IF NO 'PAIR' CONTINUE TESTING FOR A 'SINGLE'
		 *                         MATCH ON OTHER WC-TYP-FRM SETS OF TAG LINE
		 *        IF TGPAIR IS 9 - 'PAIR-MATCH' WAS SATISFIED - CONTINUE TESTING
		 *                         OTHER SWORK ELEMENTS ON REST OF TAG LINE */

		phri3 = sworkX_.phrhed[i3-One];
		while( TRUE ){
			if( tgpairX_.tgpair == 0 )
				parti3 = 0;

			/*        (START OF LOOP WILL ONLY BE ALTERED FOR 'PAIR MATCH' TEST) */

			startl = 1;
			if( tgpairX_.tgpair == 2 )
				startl = 4;
			if( tgpairX_.tgpair == 9 )
				startl = 7;
			while( TRUE ){
				if( xx == 1 )
					startl = 7;
				xx = 0;

				/*+ AVK Substituted TAGEND for hardcoded value 16 below */
				for( igus=startl; igus <= tagend; igus += 3 ){

					tgset = (igus + 2)/3;

					/*         WE DID NOT FIND 1ST PART OF PAIR, SKIP TO 'SINGLE' SETS */
					if( tgpairX_.tgpair == 1 && tgset == 2 )
						goto L_9004;

					wctst = tagsav[igus-One];
					typtst = tagsav[igus+1-One];
					frmtst = tagsav[igus+2-One];

					/*        ARE WE DONE? */
					if( (wctst == 0 && typtst == 0) && frmtst == 0 )
						break;

					/* Scon test: AVK */

					if( wctst != 4000 ){

						exclud = 0;
						exctyp = 0;
						excfrm = 0;

						/*        WORD CLASS TEST */
						if( wctst != 0 ){

							/*        IS THIS AN 'EXCLUDE' OR 'PAIR MATCH' TEST?
							 *        CODE IS IN 'THOUSANDS' (1ST) DIGIT OF 4-DIGIT FIELD
							 *        1 - EXCLUDE A MATCH ON THIS 'WC-TYP-FRM' SET
							 *        2 - THIS IS A 'PAIR MATCH' TEST
							 *        3 - EXCLUDE A MATCH ON THIS 'PAIR' */
							if( wctst >= 1000 ){
								key = wctst/1000;
								wctst += -key*1000;

								if( key == 1 )
									exclud = 1;
								if( key == 3 )
									expair = 1;
								if( key == 2 || key == 3 )
									tgpairX_.tgpair = 1;
								}

							/*         IS THIS A 'NEGATIVE WORD CLASS' TEST?
							 *         CODE IS A '9' IN 'TENS' (3RD) DIGIT OF 4-DIGIT FIELD */
							if( wctst > 20 ){
								wctst = -(wctst - 90);

								nmatch(wctst,i3,&retsw);
								if( retsw == 1 )
									break;

								}
							else if( tranidX_.tranid != 1 && wctst != sworkX_.swork[i3-One][1-One] ){
								goto L_9005;
								}
							else if( tranidX_.tranid == 1 && wctst != swork1X_.swork1[i3-One][k21-One] ){
								goto L_9005;
								}


							}

						/*        TYPE TEST */
						if( typtst != 0 ){

							/*         IS THIS AN 'EXCLUDE' TEST? */
							if( typtst >= 1000 ){
								typtst -= 1000;
								exctyp = 1;
								}

							//                                            RKH  01/13/87   STOPPER
							if (tranidX_.tranid != 1){
								if( typtst != sworkX_.swork[i3-One][2-One] ){
								if( typtst != sconX_.scon[phri3-One][11-One] ){
									if( typtst != sconX_.scon[phri3-One][13-One] )
										goto L_9005;
									}
								}
							}
							else{
							if( typtst != swork1X_.swork1[i3-One][k22-One] ){
								if( typtst != sconX_.scon[i3-One][11-One] ){
									if( typtst != sconX_.scon[i3-One][13-One] )
										goto L_9005;
									}
								}
							}

							/*        EXCLUDE 'MATCH ON RULE' IF WE MATCH ON THIS 'TYPE' */
							if( exctyp == 1 )
								break;
							}

						/*        FORM TEST */
						if( frmtst == 0 )
							goto L_800;

						/*         IS THIS AN 'EXCLUDE' TEST? */
						if( frmtst >= 1000 ){
							frmtst -= 1000;
							excfrm = 1;
							}

						/*         IS THIS A 'SPECIAL' TEST? */
						if( frmtst >= 100 ){

							/*        THREE TYPES OF SPECIAL TEST FOR FORM FIELD
							 *        CODE IS IN 'HUNDREDS' (2ND) DIGIT OF 4-DIGIT FIELD
							 *        2 - TEST OFLW 2 CODE
							 *        3 - TEST OFLW 3 CODE
							 *        4 - TEST FORMSAV */

							gotest = frmtst/100;
							frmtst += -gotest*100;

							if( gotest == 2 ){

								/*        TEST 'OVFLW 2' */
								if( tranidX_.tranid != 1 && frmtst != sconX_.scon[phri3-One][3-One] ) goto L_9005;
								if( tranidX_.tranid == 1 && frmtst != ofl2X_.ofl2i[sconX_.scolnk[i3-One]-One] )
								goto L_9006;
								}
							else if( gotest == 3 ){

								/*        TEST 'OVFLW 3' */
								if( tranidX_.tranid != 1 && frmtst != sconX_.scon[phri3-One][12-One] ) 	goto L_9005;
								if( tranidX_.tranid == 1 && frmtst != sconX_.scono[sconX_.scolnk[i3-One]-One][45-SCONX1-One] ) goto L_9005;
								goto L_9007;
								}
							else if( gotest == 4 ){

								/*        TEST 'FORM SAVE' */
								if( tranidX_.tranid != 1 && frmtst != formsaX_.formsv[sconX_.scolnk[sworkX_.swork[i3-One][4-One]-One]-One] ) goto L_9005;
								if( tranidX_.tranid == 1 && frmtst != formsaX_.formsv[sconX_.scolnk[i3-One]-One] ) goto L_9005;
								goto L_9008;
								}
							}

						/*        MATCH ON THIS FORM ONLY */
						if( tranidX_.tranid != 1 && frmtst == sworkX_.swork[i3-One][3-One] )
							goto L_9009;
						if( tranidX_.tranid == 1 && frmtst == swork1X_.swork1[i3-One][k23-One] )
							goto L_9009;

						/*        IF NO MATCH ON FORM, TRY SUPER-FORM TABLE */
						field = 3;
						/*---- CALL FORMO3 (FRMTST, FIELD, I3, &160, &300) */
						formod(2,frmtst,field,i3,&retsw);
						if( retsw != 2 )
							goto L_160;
						/*								   <* WCTST = 4000 determines that it is a scon test *> */
						}
					else if( frmtst > 1000 ){
						/*									  <* If FRMTST is of form 1xxx, do exclusion test *> */
						if( !((typtst <= 20 && sconX_.scon[i3-One][typtst-One] != 
						  frmtst - 1000) || (sconX_.scono[i3-One][typtst-20-One] != 
						  frmtst - 1000)) )
							break;
						/*									 <* scon value = FRMTST - 1000 *>
						 *            <* If FRMTST is of form 0xxx, do inclusion test *> */
						}
					else if( (typtst <= 20 && sconX_.scon[i3-One][typtst-One] == 
					  frmtst) || (sconX_.scono[i3-One][typtst-20-One] == 
					  frmtst) ){
						goto L_800;
						}


L_9005:
					;

					}
				goto L_999;
L_9004:
				xx = 1;
				}
L_9006:
			if( excfrm == 0 )
				goto L_800;
			goto L_999;
L_9007:
			if( excfrm == 0 )
				goto L_800;
			goto L_999;
L_9008:
			if( excfrm == 0 )
				goto L_800;
			goto L_999;

			/*         MATCH IN FORM TABLE */
L_160:
			if( excfrm == 0 )
				goto L_800;
			goto L_999;

			/*        EXCLUDE 'MATCH ON RULE' IF WE MATCH ON THIS 'FORM' */
L_9009:
			if( excfrm == 0 )
				goto L_800;

			/*         EXCLUDE MATCH FOR THIS 8000 TEST */


			/*                                             **R1361JGB */
L_999:
			if( exclud != 1 )
				break;
			exclud = 0;


			/*         A MATCH FOR THIS 8000 TEST
			 *                                              **R1361JGB */
L_800:
			if( exclud == 1 ){
				exclud = 0;
				goto L_999;
				}
			else{

				*retflg = 0;

				/*         WAS THIS 'WC-TYP-FRM' SET EXCLUDED? */
				if( exclud == 1 )
					goto L_999;

				/*         WAS THIS A 'PAIR MATCH' TEST?
				 *         (APPLICABLE TO 1ST 2 SETS ONLY) */
				if( tgpairX_.tgpair == 0 || tgset > 2 )
					goto L_2000;

				/*         1ST PART OF A PAIR? */
				if( !(tgpairX_.tgpair != 1 || tgset != 1) )
					goto L_9010;

				/*         2ND PART OF A 'PAIR MATCH' TEST? */
				if( tgpairX_.tgpair != 2 || tgset != 2 )
					goto L_2000;

				/*         ARE NSWORKS OF 'PAIR MATCH' CONTIGUOUS? */
				if( parti3 + 1 == i3 ){

					/*         TAG 'PAIR MATCH' SATISFIED */
					tgpairX_.tgpair = 9;

					/*         WAS THIS 'PAIR MATCH' EXCLUDED? */
					if( expair == 1 )
						goto L_999;

					goto L_2000;
					}
				}

			/*         RESTART LOOP, LOOKING FOR 'NEW' 1ST PART OF 'PAIR MATCH' */
			tgpairX_.tgpair = 0;
			}
		/*                                             **R1361JGB */
		*retflg = 1;
		goto L_2000;

		/*         1ST PART OF TAG 'PAIR MATCH' SATISFIED */
L_9010:
		tgpairX_.tgpair = 2;
		/*         SAVE 'I3' FOR CONTIGUOUS TEST */
		parti3 = i3;

		/* Goto 6 if or, and, or right paren seen to continue processing TAGSETS,
		 * else evaluate the logical expression and return. */

L_2000:
		strptr += 1;
		if( *retflg == 0 ){
			string[strptr-One] = '1';
			}
		else{
			string[strptr-One] = '0';
			}
		if( !((rparen || orseen) || andseen) )
			break;
		strptr += 1;
		if( rparen )
			string[strptr-One] = ')';
		if( orseen )
			string[strptr-One] = '|';
		if( andseen )
			string[strptr-One] = '&';
		}

	*retflg = streval(string);
	if( *retflg == -1 )
		{
		fprintf( _spec_fp, "t1tmatch: Illformed logical expression in 8000 tagset\n" );
		}

	return;
} /*end of function*/

