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
	/*    Changes:
	 *        10/18/93:AVK,  fixed evaluation of "!= tags", also precedence
	 *                       ORs > ANDs, i.e., evaluate ORs first, and
	 *                       parenthetized expressions
	 *        09/03/93:jal,  fix to 6071/70. RELPTx used instead of NSLOCx
	 *        04/29/93:jal,  add 6070/6071 tagsets to compare two scons
	 *                       for equality. */
	/*     Purpose: Processes all tags except 8000. */
	/*     The following is tag description: */
	/*       1000 - Head or Whole Word
	 *       2000 - Whole word only
	 *       4000 - DBL-TYP tagset
	 *       6010 - Test set, subset or superset and pass test if match
	 *       6012 - Test set, subset or superset and fail test if match
	 *       6013 - Test for negative word class and pass test if match
	 *       6014 - Test for negative word class and fail test if match
	 *       6050 - Test one or more supercells  and pass test if match
	 *       6055 - Test form, superset, set or subset and pass if match
	 *       6081 - 6090 : Test one or more SCONs and pass if match */
	/*   Glossary of Variables:
	 *     TAGID  - Tag Name being processed
	 *     TAGSET - Tag Data to be retrieved from the Overflow File
	 *     TAGSAV - Tag Data Line to be processed
	 *     TAGPTR - It contains the overflow file pointer for tag data
	 *     TAGLIN - Points to the current tag line being processed
	 *     NSPTR  - Points to SWORK record for the Tag encountered
	 *     SCNPTR - Points to SCON record for the Tag encountered
	 *     TAGCOL - Always points to current tag element in a tagset
	 *     NXTCOL - Points to next tag to be processed in a line
	 *     TAGEND - Points to the last tag element for current tag
	 *     NUMELM - Number of elements in current tag excluding TAG ID.
	 *     PARM,PARM1,PARM2,TSTPRM - Local variables
	 *     SUPFRM - Superform a tag parm to be tested.
	 *     NSLOC1,NSLOC2 - SWORK elements corresponding to SP element
	 *     ANDFLG = .TRUE. if a 7777  seperator follows
	 *     ORFLAG = .TRUE. if a 5555 tag seperator follows
	 *     RPAREN = .TRUE. if a 2222 tag separator follows (AVK) */
	/* CHANGES:
	 *    02/21/91 *JAL* :  ACCESS HASHCD AND HENUM2 VALUES VIA SCOLNK TO
	 *                    ACCOUNT FOR NEW CLAUSE ELEMENTS ADDED BEYOND THE
	 *                    100TH SCON
	 *    04/17/87 *R1685RKH*  CHANGE T1-4 SWORK LIMIT FROM 50 TO
	 *    04/01/87 *BUGFIXRKH*  FIX BUG IN 608X TAGSET
	 *    02/25/87 *FIXRKH*  6055 TAG FIX
	 *    02/14/87 *FIXRKH*  CURRENT STRETCH */
	/*+ AVK */
	/*+
	 *+ STRING- Logical expression string to be evaluated: AVK */
	/*+
	 *         USED AS PARM TO MATCH4 -- MUST BE I*4 */
	/*+          add 6070/6071 tagset for scon equality        jal 4/29/93 */
	/*- */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <fcrt.h>
#include "trans.h"
#include "logoslib.h"
#include "parsetrans.h"
#include "projexts.h"
#include <string.h>
#include "parsetrans_ext.h"
#include <jbctrl.h>



void /*FUNCTION*/ trtag(
		long int *locflg,
		short int *retflg,
		short int  sp[],
		short int  ovrflw[],
		long int whch50,
		long int  stpprt,
		LOGICAL8 alldon,
		short rule_number
		)
{

	static LOGICAL8 equal, exclud;
	static LOGICAL32 andflg, orflag, rparen;
	static byte string[500];
	static short int err, gl, i, igus, j, jj, k8, nrmcol, nsloc, nsloc1, 
	  nsloc2, nspt, numhun, numths, nxtcol, parm, parm1, parm2, ptr, 
	  relpt1, relpt2, retsw, scnpt, scnval, scon1, scon2, strptr, 
	  supfrm, swkptr, tag1, tag2, tagbeg, tagcol, tagend, tagid, taglin, 
	  tagsav[84], tagset[21], tstprm, tsttyp, ty1, ty2, ty3, x, xx;
	static long int _l0, _l1, nn, numelm;
	int tstval1,tstval2,tstval3,tstval4,tstval12,tstval22,tstval32,tstval42;	
	static short jbnum[10]={2,5,8,1,4,7,10,13,16,19};
	static char pgmnam[9] = "TRTAG   ";

	/*     Following for processing Tagsets in the end in TAG234 Fortran */
	memset(string,' ',sizeof(string));
	strptr = 0;

	/*     All Tags except 8000 are processed in this program.
	 *     >>>> Begin the loop for a maximum of 5 Tags <<<<<<< */

	taglin = 1;
	orflag = FALSE;
	andflg = FALSE;
	rparen = FALSE;
	//     Set SWORK and SCON pointers for this tag line */
	nspt = tagblkX_.nsptr[taglin-One];
	scnpt = tagblkX_.scnptr[taglin-One];

	/*     Initialize the Tag Data line */
	nxtcol = 1;

	xx = tagblkX_.tagptr[taglin-One];
	ovrin((short*)locflg, (char*)tagset, rule_number, xx);

	/*     Initialize TAGSAV line and process a new tag on same SP element
	 *     15 is looped through for each SP element's TAGS for each TAG. */

	while( TRUE ){
		tagbeg = nxtcol;

		/*     If the Tag element is greater than 42, then
		 *     it is end of tag processing for this line */

		if( tagbeg >= 84 )
			goto L_9999;
		nn = 0;
		for( jj=1; jj <= 84; jj++ ){
			tagsav[jj-One] = 0;
			}

		/*     The tagset line will be scanned for tag endings
		 *     5555 7777 or 9000. If one is found then pointers
		 *     will be initialized and this tag processed. If
		 *     the test is no match then we will return with RETFLG=1
		 *     If the test is good we will continue processing till
		 *     all tags for this SP elements are exhausted or 8888
		 *     is encountered in which case we will read Overflow
		 *     file again and process remaining tags */

L_17:
		;

		for( jj=tagbeg; jj <= 21; jj++ ){
			tag1 = tagset[jj-One];
			ptr = jj;
			/*+ Added parenthesis processing: AVK */
			if( tag1 == 1111 ){
				strptr += 1;
				string[strptr-One] = '(';
				}
			else if( tag1 == 2222 && (((
				(tagsav[0] >= 1000 && tagsav[0] < 1111) 
				 && (nn%2) == 1) || 
			     tagsav[0] < 1000) || tagsav[0] > 1111) ){
				goto L_27;
				}
			else if( tag1 == 5555 ){
				goto L_30;
				}
			else if( tag1 == 7777 ){
				goto L_40;
				}
			else if( tag1 == 8888 ){
				goto L_50;
				}
			else{
				nn += 1;
				tagsav[nn-One] = tag1;
				/*+ Added line to recognize end of tagset string: AVK */
				if( tagsav[0] == 0 )
					goto L_9000;
				if( tagsav[0] >= 6000 ){
					if( nn > 3 && tag1 == 0 )
						goto L_25;
					}
				}
			}
		goto L_20;

L_50:
		;

		/*     8888 Tag encountered. Read in the next Overflow
		 *     line and process tags */

		xx += 1;
		ovrin((short*)locflg, (char*)tagset, rule_number, xx);
		if( ((whch50 == 0) || (w50valX_.step50 == 0)) || 
			 (stpprt != 0 && alldon) ){
			if( *locflg == 1 ){
				if( prttagX_.prtidx < 4 ){
					prttagX_.prtidx += 1;
					memcpy(&prttagX_.prttag[prttagX_.prtidx-One],tagset,sizeof(tagset));
					}
				}
			else if( prmtagX_.prmidx < 4 ){
				prmtagX_.prmidx += 1;
				memcpy(&prmtagX_.prmtag[prmtagX_.prmidx-One],tagset,sizeof(tagset));
				}
			}
		tagbeg = 1;
		goto L_17;

		/*     All the elements constitute the last tag to be processed
		 *     NN points to the last element to be processed in TAGSAV line. */

L_20:
		;
		nxtcol = 100;
		andflg = FALSE;
		orflag = FALSE;
		rparen = FALSE;
		goto L_100;

L_27:
		;
		/*     2222 Tag follows */
		rparen = TRUE;
		nxtcol = ptr + 1;
		goto L_100;

L_30:
		;
		/*     5555 Tag follows */
		orflag = TRUE;
		nxtcol = ptr + 1;
		goto L_100;

L_40:
		;
		/*     7777 Tag Follows */
		andflg = TRUE;
		nxtcol = ptr + 1;
		goto L_100;

		/*     3 zeroes were encountered. Determine if they are significant
		 *     For 6000 Tagsets ignore them. For all others allow pairs.
		 *     SO for non 6000 Tags increment NN by 1. */
L_25:
		;
		tagid = tagsav[0];
		nn -= 1;
		ptr -= 1;

		nxtcol = 100;
		andflg = FALSE;
		orflag = FALSE;
		rparen = FALSE;

L_100:
		;
		/*     TAGSAV contains the tag to be processed */
		tagcol = 1;
		tagid = tagsav[tagcol-One];
		tagcol += 1;
		tagend = nn;

		/*     Initialize NN */
		nn = 0;

		/*     Check the tag and go to appropriate section */

		/*+ AVK  If there is no tag test to evaluate */
		if( !(tagid == 0 && ((orflag || andflg) || rparen)) ){

			if( tagid == 6010 || tagid == 6012 || tagid == 6013 || tagid == 6014 ){
				/*     First Parm = Relative SP Element pointed to */
				tag2 = tagsav[tagcol-One] - 80;
				tagcol += 1;

				/*     GETPTR will return the corresponding SWORK/SCON
				 *     pointer for the SP element supplied to it */

				getptr(tag2,&nsloc,*locflg);

				if( tagid == 6013 || tagid == 6014 ){
					if(tranidX_.tranid == 1){
						tstprm = spinX_.nswork[nsloc-One][0];
					}
					else{
						tstprm = sworkX_.swork[nsloc-One][0];
					}

					for( j=tagcol; j <= tagend; j++ ){
						parm = tagsav[j-One];
						if( tagid == 6013 && tstprm == parm )
							goto L_800;
						if( tagid != 6013 && tstprm == parm )
							goto L_824;
						}

					/*     Test failed for 6013 */
					if( tagid != 6013 )
						goto L_800;

					/*     Test was successful for 6014 */
					}

				else{

					/*     Get the test parameter value for Tag 6010 and 6012 */

					if(tranidX_.tranid == 1){
						tstval1 = sconX_.scon[spinX_.nswork[nsloc-One][4-One]-One][13-One];
						tstval2 = sconX_.scon[spinX_.nswork[nsloc-One][4-One]-One][11-One];
						tstval3 = spinX_.nswork[nsloc-One][2-One];
					}
					else{
						tstval1 = sconX_.scon[sworkX_.swork[nsloc-One][4-One]-One][13-One];
						tstval2 = sconX_.scon[sworkX_.swork[nsloc-One][4-One]-One][11-One];
						tstval3 = sworkX_.swork[nsloc-One][2-One];
					}

					for( i=tagcol; i <= tagend; i++ ){
						parm = tagsav[i-One];
						if( parm > 0 && parm <= 16 ){
							tstprm = tstval1;
							}
						else if( parm > 16 && parm <= 99 ){
							tstprm = tstval2;
							}
						else{
							tstprm = tstval3;
							}

						if( tagid == 6010 && tstprm == parm )
							goto L_800;
						if( tagid != 6010 && tstprm == parm )
							goto L_824;
						}

					/*     Test was not successful for 6010 */

					if( tagid != 6010 )
						goto L_800;

					/*     Test was successful for 6012 */

					}
				}

			else if( tagid == 6050 ){
				/*     >>>>>> 6050 Tag Processing <<<<<<<<<< */
				/*     This tag has an implicit and between different parms unlike
				 *     other tags which have an implicit OR. */

				for( i=tagcol; i <= tagend; i += 2 ){
					tsttyp = tagsav[i-One]/1000;
					parm1 = tagsav[i-One] - tsttyp*1000;
					parm2 = tagsav[i+1-One] - 8000;

					if( tsttyp == 0 ){
						if( vbdataX_.vbcell[parm1-One] != parm2 )
							goto L_824;
						}
					else if( tsttyp == 1 ){
						fprintf( _spec_fp, " Not implemnted yet\n" );
						}
					else if( tsttyp != 2 ){
						fprintf( _spec_fp, " Not implemnted yet\n" );
						}
					else if( vbdataX_.vbcell[parm1-One] == parm2 ){
						goto L_824;
						}

					/*     Test for current tag ok. Check the AND/OR conditions */

					}
				goto L_800;
				}

			else if( tagid == 6055 ){
				parm1 = tagsav[tagcol-One] - 80;
				getptr(parm1,&nsloc1,*locflg);
				tagcol += 1;
				parm2 = tagsav[tagcol-One];
				tagcol += 1;
				numths = parm2/1000;
				numhun = (parm2 - numths*1000)/100;


				/*     If superform test required then read it
				 *     before entering the loop */
				if( numths == 0 && numhun == 1 ){
					k8 = jbnum[parm1-One];
					if( parm1 > 3 ){
						supfrm = ovrflw[k8+2-One];
						}
					else{
						supfrm = sp[k8+2-One];
						}
					}


				/*     >>> Loop for 6055 Tag <<<<< */

				for( i=tagcol; i <= tagend; i++ ){
					parm = tagsav[i-One] - 80;
					getptr(parm,&nsloc2,*locflg);

					if(tranidX_.tranid == 1){
						tstval1  = spinX_.nswork[nsloc1-One][3-One];
						tstval12 = spinX_.nswork[nsloc2-One][3-One];
						tstval2  = sconX_.scon[spinX_.nswork[nsloc1-One][4-One]-One][13-One];
						tstval22 = sconX_.scon[spinX_.nswork[nsloc2-One][4-One]-One][13-One];
						tstval3  = sconX_.scon[spinX_.nswork[nsloc1-One][4-One]-One][11-One];
						tstval32 = sconX_.scon[spinX_.nswork[nsloc2-One][4-One]-One][11-One];
						tstval4  = spinX_.nswork[nsloc1-One][2-One];
						tstval42 = spinX_.nswork[nsloc2-One][2-One];
					}
					else{
						tstval1  = sworkX_.swork[nsloc1-One][3-One];
						tstval12 = sworkX_.swork[nsloc2-One][3-One];
						tstval2  = sconX_.scon[sworkX_.swork[nsloc1-One][4-One]-One][13-One];
						tstval22 = sconX_.scon[sworkX_.swork[nsloc2-One][4-One]-One][13-One];
						tstval3  = sconX_.scon[sworkX_.swork[nsloc1-One][4-One]-One][11-One];
						tstval32 = sconX_.scon[sworkX_.swork[nsloc2-One][4-One]-One][11-One];
						tstval4  = sworkX_.swork[nsloc1-One][2-One];
						tstval42 = sworkX_.swork[nsloc2-One][2-One];
					}
													// Check Form/Superform 
					if( numths == 0 ){
						if( numhun == 1 ){
							formod(2,(int)supfrm,3,(int)nsloc2, &retsw);
							if( retsw != 0 ) goto L_824;
							}
						else if( tstval1 != tstval12 ){
								goto L_824;
							}
						else{
							}
						}
													// check superset
					else if( numths == 1 ){
						if( tstval2 != tstval22 )
							goto L_824;
						}
													// check set
					else if( numths == 2 ){
						if( tstval3 != tstval32 )
							goto L_824;
						}
													// check subset
					else if( tstval4 == tstval42 ){
							goto L_800;
						}
					else{
							goto L_824;
						}
					/*     The test performed was good. Repeat 6055 loop */
					/*     6055 Tag Test succsssful. Check other tags on this line */
					}

				goto L_800;
				}
			else{
				if( tagid != 6070 ){
					if( tagid != 6071 ){
						if( tagid >= 6081 && tagid <= 6090 ){
							/*     Tag 608X - SCON testing */

							parm = tagid - 6080;
							getptr(parm,&nsloc,*locflg);
							for( i=tagcol; i <= tagend; i += 2 ){
								parm1 = tagsav[i-One];
								if( parm1 > 2000 ){
									parm1 -= 2000;
									exclud = TRUE;
									}
								else{
									exclud = FALSE;
									}
								parm2 = tagsav[i+1-One] - 8000;

								if(tranidX_.tranid == 1){
									tstval1 = sconX_.scon[spinX_.nswork[nsloc-One][4-One]-One][parm1-One];
									tstval2 = sconX_.scono[sconX_.scolnk[spinX_.nswork[nsloc-One][4-One]-One]-One][parm1-SCONX1-One];
								}
								else{
									tstval1 = sconX_.scon[sworkX_.swork[nsloc-One][4-One]-One][parm1-One];
									tstval2 = sconX_.scono[sconX_.scolnk[sworkX_.swork[nsloc-One][4-One]-One]-One][parm1-SCONX1-One];
								}

								if( exclud ){
									if( parm1 <= SCONX1 ){
										if(	tstval1 == parm2 )
											goto L_824;
										}
									else if(tstval2 == parm2 ){
											 goto L_824;
										}
									}
								else if( parm1 <= SCONX1 ){
									if( tstval1 != parm2 )
										goto L_824;
									}
								else if( tstval2 != parm2 ){
										goto L_824;
									}
								/*     Test passed */

								}
							goto L_800;
							}
						else{

							if( !(tagid < 1000 || tagid > 2100) ){
								/*     Check if it is for any other element */

								if( !(tagid == 1000 || tagid == 2000)  ){
									if( tagid < 2000 ){
										swkptr = tagid - 1080;
										}
									else{
										swkptr = tagid - 2080;
										}

									getptr(swkptr,&nspt,*locflg);
									}

								if(tranidX_.tranid == 1){
									gl = spinX_.nswork[nspt-One][4-One];
								}
								else{
									gl = sworkX_.swork[nspt-One][4-One];
								}

								/*     Find number of elements in the Tag */

								numelm = tagend - tagcol + 1;

								/*        CHECK GENLIT NUMBER */
								if( hensavX_.henum2[sconX_.scolnk[gl-One]-One][0] != -1 ){
									match4(&hensavX_.henum2[sconX_.scolnk[gl-One]-One][0],&tagsav[tagcol-One],numelm,&retsw);
									if( retsw == 1 ) goto L_800;
									// check root form
									match4(&hensavX_.root_henum2[sconX_.scolnk[gl-One]-One][0],&tagsav[tagcol-One],numelm,&retsw);
									if( retsw == 1 ) goto L_800;
								}

								if( tagid >= 2000 ) goto L_824;

								/*        CHECK HASH CODE */
								if( hashX_.hashcd[sconX_.scolnk[gl-One]-One][0] != 0 ){
									match4(&hashX_.hashcd[sconX_.scolnk[gl-One]-One][0],&tagsav[tagcol-One],numelm,&retsw);
									if( retsw == 1 )goto L_800;
									// check root form
									match4(&hashX_.root_hashcd[sconX_.scolnk[gl-One]-One][0],&tagsav[tagcol-One],numelm,&retsw);
									if( retsw == 1 )goto L_800;
								}

								goto L_824; 
								}

							/*     Check if Normal Tag or 4000 Tag */


							/*     Check if TAG is of the form 408X */

							if( tagid > 4000 ){
								swkptr = tagid - 4080;
								getptr(swkptr,&nspt,*locflg);
								if(tranidX_.tranid == 1){
									scnpt = spinX_.nswork[nspt-One][4-One];
								}
								else{
									scnpt = sworkX_.swork[nspt-One][4-One];
								}

							}

							if(tranidX_.tranid == 1){
								ty1 = spinX_.nswork[nspt-One][2-One];
							}
							else{
								ty1 = sworkX_.swork[nspt-One][2-One];
							}
							ty2 = sconX_.scon[scnpt-One][11-One];
							ty3 = sconX_.scon[scnpt-One][13-One];

							if( tagid < 1000 ){

								/*     >>>>>> Normal Tag Processing <<<<<<< */

								/*     The Tag ID itself has to be processed
								 *     Reduce the counter by 1 */

								nrmcol = tagcol - 1;
								for( igus=nrmcol; igus <= tagend; igus++ ){
									tag1 = tagsav[igus-One];
									if( tag1 == 0 )
										break;

									if( (tag1 == ty1 || tag1 == ty2) || tag1 == ty3 )
										goto L_800;

									}
								goto L_824;
								}
							else{

								/*     >>>>>> 4000 Tag Processing <<<<<<< */

								for( igus=tagcol; igus <= tagend; igus += 2 ){
									tag1 = tagsav[igus-One];
									tag2 = tagsav[igus+1-One];

									if( tag1 == 0 )
										break;

									/*         CHECK WHETHER VALID COMBINATIONS EXIST */
									if( tag1 == ty1 && tag2 == ty2 )
										goto L_800;
									if( tag1 == ty1 && tag2 == ty3 )
										goto L_800;
									if( tag1 == ty2 && tag2 == ty1 )
										goto L_800;
									if( tag1 == ty2 && tag2 == ty3 )
										goto L_800;
									if( tag1 == ty3 && tag2 == ty1 )
										goto L_800;
									if( tag1 == ty3 && tag2 == ty2 )
										goto L_800;

									}
								goto L_824;
								}
							}
						}
					}


				/*     Tag 6070/6071 - SCON equality testing */

				/*            loop thru each 607X triplet: SCON,-8X,-8Y */
				for( i=tagcol; i <= tagend; i += 3 ){
					/*             get the parameters */
					scnval = tagsav[i-One];
					relpt1 = tagsav[i+1-One] - 80;
					relpt2 = tagsav[i+2-One] - 80;
					getptr(relpt1,&nsloc1,*locflg);
					getptr(relpt2,&nsloc2,*locflg);

					if(tranidX_.tranid == 1){
						tstval1 = swork1X_.phct;
						/*               get the scon values */
						if( scnval <= SCONX1 ){
							scon1 = sconX_.scon[spinX_.nswork[nsloc1-One][4-One]-One][scnval-One];
							scon2 = sconX_.scon[spinX_.nswork[nsloc2-One][4-One]-One][scnval-One];
							}
						else{
							scon1 = sconX_.scono[sconX_.scolnk[spinX_.nswork[nsloc1-One][4-One]-One]-One][scnval-SCONX1-One];
							scon2 = sconX_.scono[sconX_.scolnk[spinX_.nswork[nsloc2-One][4-One]-One]-One][scnval-SCONX1-One];
							}
					}
					else{
						tstval1 = sworkX_.phct;
						/*               get the scon values */
						if( scnval <= SCONX1 ){
							scon1 = sconX_.scon[sworkX_.swork[nsloc1-One][4-One]-One][scnval-One];
							scon2 = sconX_.scon[sworkX_.swork[nsloc2-One][4-One]-One][scnval-One];
							}
						else{
							scon1 = sconX_.scono[sconX_.scolnk[sworkX_.swork[nsloc1-One][4-One]-One]-One][scnval-SCONX1-One];
							scon2 = sconX_.scono[sconX_.scolnk[sworkX_.swork[nsloc2-One][4-One]-One]-One][scnval-SCONX1-One];
							}
					}


					/*             check the parameters */
					if( scnval < 1 || scnval > SCONX ){
						if( diagsX_.anydi == 1 ){
							fprintf( _spec_fp, "ERROR: illegal parameters to %4d", tagid );
							fprintf( _spec_fp, " tagset in following rule:\n  " );
							for( x=1; x <= 12; x++ ){
								fprintf( _spec_fp, "%3d", sp[x-One] );
								}
							fprintf( _spec_fp, "\n" );
							}
						err = scnval;
						errlog(pgmnam,6071,err,5);
						}
					else if( ((nsloc1 < 1 || nsloc1 > tstval1) || nsloc2 < 1) || nsloc2 > tstval1 ){
						if( diagsX_.anydi == 1 ){
							fprintf( _spec_fp, "ERROR: illegal parameters to %4d", tagid );
							fprintf( _spec_fp, " tagset in following rule:\n  " );
							for( x=1; x <= 12; x++ ){
								fprintf( _spec_fp, "%3d", sp[x-One] );
								}
							fprintf( _spec_fp, "\n" );
							}
						err = relpt1;
						errlog(pgmnam,6072,err,5);
						}
					else{
						/*               compare scon values, break as soon as TRUE found
						 *               6020: match(i.e. tagset true) if scons equal
						 *               6021: match(i.e. tagset true) if scons not equal */
						equal = FALSE;
						if( scon1 == scon2 )
							equal = TRUE;
						if( tagid == 6070 ){
							if( equal )
								goto L_800;
							}
						else if( !equal ){
							goto L_800;
							}
						}

					/*               this triplet tests false, loop for next triplet */

					/*               fell thru, tagset is false, no match */
					}
				}

			/*     Current tag test failed, save result in STRING to be evaluated: AVK */
L_824:
			;
			strptr += 1;
			string[strptr-One] = '0';
			goto L_999;
			//     Current tag test succeeded, save result in STRING to be evaluated: AVK */

L_800:
			;
			strptr += 1;
			string[strptr-One] = '1';
			}

		/*     Now store the AND/OR operation in STRING to be evaluated.
		 *     If AND or OR flag is seen, goto 15 to evaluate next tag,
		 *     otherwise evaluate the string: AVK */

L_999:
		;
		if( andflg ){
			andflg = FALSE;
			strptr += 1;
			string[strptr-One] = '&';
			}
		else if( orflag ){
			orflag = FALSE;
			strptr += 1;
			string[strptr-One] = '|';
			}
		else if( rparen ){
			rparen = FALSE;
			strptr += 1;
			string[strptr-One] = ')';
			}
		else{
			break;
			}
		}

L_9000:
	*retflg = streval(string);

L_9999:
	;
	return;
} /*end of function*/

