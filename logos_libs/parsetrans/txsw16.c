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

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <fcrt.h>
#include "logoslib.h"
#include "trans.h"
#include "projexts.h"
#include <jbctrl.h>
#include "parsetrans_ext.h"


void /*FUNCTION*/ txsw16()
{
	static byte vtfmc, vtfmg, vtfmnu, vtfmp;
	static short int adr2, edscon, iz, jz2, m, scon1;
	static short ms = 0;
	static short xx = 0;
	static short nsct = 0;
	static short vtrc = 0;
	static short vtrg = 0;
	static short vtrp = 0;
	static short vtrnu = 0;
	static short vtrpnt = 0;

	vbdataX_.k3n = vbdataX_.k3 + 6;

	/*        SWITCH IS LEFT ORIENTED IF K3 + 5 IS NEGATIVE */

	vtrg = vtrfX_.vtrf[vbdataX_.k3+1-One];
	vtrnu = vtrfX_.vtrf[vbdataX_.k3+2-One];
	vtrp = vtrfX_.vtrf[vbdataX_.k3+3-One];
	vtrc = vtrfX_.vtrf[vbdataX_.k3+4-One];
	/*+    IS VTRF ELEMENT REL PTR?  BEWARE STRETCH   06/09/86  *R1530DSD */
	vtfmg = vtrfmX_.vtrfm[vbdataX_.k3+1-One];
	vtfmnu = vtrfmX_.vtrfm[vbdataX_.k3+2-One];
	vtfmp = vtrfmX_.vtrfm[vbdataX_.k3+3-One];
	vtfmc = vtrfmX_.vtrfm[vbdataX_.k3+4-One];

	vtrpnt = vtrfX_.vtrf[vbdataX_.k3+5-One];
	if( vtrpnt < 0 ){

		/*     JUMP HERE IF ARG 5 IS REL PTR.  CHANGE ITS WHOLE PHRASE. */
		flowckX_.phrstr = sworkX_.phrbeg[im81X_.im81-vtrpnt-One];
		flowckX_.phrlst = sworkX_.phrend[im81X_.im81-vtrpnt-One];
		nsct = sworkX_.phrhed[im81X_.im81-vtrpnt-One];

		if( sconX_.scon[nsct-One][1-One] > 0 ){

			/*     GENDER */
			if( vtrg != 0 ){
				edscon = 0;
				if( vtrg > 0 ){
					edscon = vtrg;
					}
				else if( vtfmg == vtrfvlX_.vtmrp ){
					edscon = sconX_.scon[sworkX_.phrhed[im81X_.im81-vtrg-One]-
					  One][4-One];
					}
				else if( vtrg == -98 ){
					edscon = sw25bkX_.hedgen;
					}
				else if( vtrg == -99 ){
					if( sw36bkX_.rel == 1 ){
						edscon = sw36bkX_.relgen;
						if(tranidX_.tranid == 2 && trgflgX_.trgflg==1)
						{
							if(sw36bkX_.relcas==5)
								edscon=3;	
							if(sw36bkX_.relcas==6||sw36bkX_.relcas==7)
								edscon=2;
						}
						}
					else{
						if( tranidX_.tranid == 4 )
							edscon = sw14bkX_.case_; 
						else
							edscon = sw21bkX_.case_; 
						if(tranidX_.tranid == 2 && trgflgX_.trgflg==1)
						{
							if(sw21bkX_.case_==5)
								edscon=3;
							if(sw21bkX_.case_==6||sw21bkX_.case_==7)
								edscon=2;
						}
						}
					}
				for( iz=flowckX_.phrstr; iz <= flowckX_.phrlst; iz++ ){
					if( sconX_.scon[opadriX_.sconpi[iz-One]-One][1-
					  One] >= 0 )
						sconX_.scon[opadriX_.sconpi[iz-One]-One][4-
						  One] = edscon;
					/*                   IF THIS IS A MULTI LOADED VC, SET ELEMENTS IN THE VC */
					if( opadriX_.opadri[iz-One] <= -100 && opadriX_.opadri[iz-One] >= 
					  -120 ){
						if( hpdopiX_.hfdopi[iz-One] >= HFPOLO && hpdopiX_.hfdopi[iz-One] <= 
						  HFPOHI ){
							adr2 = hpdopiX_.hfdopi[iz-One] - HFPOL1;
							jz2 = hfdoaX_.hfpoad[adr2-One][HFPADX-One];
							for( m=1; m <= jz2; m++ ){
								if( sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-
								  One][1-One] >= 0 )
									sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-
									  One][4-One] = edscon;
								}
							}
						}
					}
				}


			/*     NUMBER */

			if( vtrnu != 0 ){
				edscon = 0;
				if( vtrnu > 0 ){
					edscon = vtrnu;
					}
				else if( vtfmnu == vtrfvlX_.vtmrp ){
					edscon = sconX_.scon[sworkX_.phrhed[im81X_.im81-vtrnu-One]-
					  One][5-One];
					}
				else if( vtrnu == -98 ){
					edscon = sw25bkX_.hednum;
					}
				else if( vtrnu == -99 ){
					if( sw36bkX_.rel == 1 ){
						edscon = sw36bkX_.relnum;
						}
					else{
						edscon = sw14bkX_.num;
						}
					}
				for( iz=flowckX_.phrstr; iz <= flowckX_.phrlst; iz++ ){
					if( sconX_.scon[opadriX_.sconpi[iz-One]-One][1-
					  One] >= 0 )
						sconX_.scon[opadriX_.sconpi[iz-One]-One][5-
						  One] = edscon;
					/*                   IF THIS IS A MULTI LOADED VC, SET ELEMENTS IN THE VC */
					if( opadriX_.opadri[iz-One] <= -100 && opadriX_.opadri[iz-One] >= 
					  -120 ){
						if( hpdopiX_.hfdopi[iz-One] >= HFPOLO && hpdopiX_.hfdopi[iz-One] <= 
						  HFPOHI ){
							adr2 = hpdopiX_.hfdopi[iz-One] - HFPOL1;
							jz2 = hfdoaX_.hfpoad[adr2-One][HFPADX-One];
							for( m=1; m <= jz2; m++ ){
								if( sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-
								  One][1-One] >= 0 )
									sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-
									  One][5-One] = edscon;
								}
							}
						}
					}
				}



			/*        PERSON -- IGNORE ??? */

			/*     PERSON */
			if( vtrp != 0 ){
				edscon = 0;
				if( vtrp > 0 ){
					edscon = vtrp;
					}
				else if( vtfmp == vtrfvlX_.vtmrp ){
					edscon = sconX_.scon[sworkX_.phrhed[im81X_.im81-vtrp-One]-
					  One][6-One];
					}
				else if( vtrp == -98 ){
					edscon = sw25bkX_.hedper;
					}
				else if( vtrp == -99 ){
					if( sw36bkX_.rel == 1 ){
						edscon = sw36bkX_.relper;
						}
					else{
						edscon = sw14bkX_.per;
						}
					}
				for( iz=flowckX_.phrstr; iz <= flowckX_.phrlst; iz++ ){
					if( sconX_.scon[opadriX_.sconpi[iz-One]-One][1-
					  One] >= 0 )
						sconX_.scon[opadriX_.sconpi[iz-One]-One][6-
						  One] = edscon;
					/*                   IF THIS IS A MULTI LOADED VC, SET ELEMENTS IN THE VC */
					if( opadriX_.opadri[iz-One] <= -100 && opadriX_.opadri[iz-One] >= 
					  -120 ){
						if( hpdopiX_.hfdopi[iz-One] >= HFPOLO && hpdopiX_.hfdopi[iz-One] <= 
						  HFPOHI ){
							adr2 = hpdopiX_.hfdopi[iz-One] - HFPOL1;
							jz2 = hfdoaX_.hfpoad[adr2-One][HFPADX-One];
							for( m=1; m <= jz2; m++ ){
								if( sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-
								  One][1-One] >= 0 )
									sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-
									  One][6-One] = edscon;
								}
							}
						}
					}
				}


			/*     CASE/TENSE - DIFFERENT LOGIC FROM OTHERS */

			if( sconX_.scon[nsct-One][9-One] != 7 ){


				ms = sworkX_.swork[im81X_.im81-vtrpnt-One][1-One];

				if( vtrc != 0 ){
					edscon = 0;
					if( vtrc > 0 ){
						edscon = vtrc;
						}
					else if( vtfmc == vtrfvlX_.vtmrp ){
						edscon = sconX_.scon[sworkX_.phrhed[im81X_.im81-vtrc-One]-
						  One][7-One];
						}
					else if( vtrc == -98 ){
						edscon = sw25bkX_.hedcas;
						}
					else if( vtrc == -99 ){
						if( sw36bkX_.rel == 1 ){
							edscon = sw36bkX_.relcas;
							}
						else{
							edscon = sw14bkX_.case_;
							}
						}
					for( iz=flowckX_.phrstr; iz <= flowckX_.phrlst; iz++ ){
						scon1 = sconX_.scon[opadriX_.sconpi[iz-One]-
						  One][1-One];
						if( scon1 >= 0 ){
							if( ms == 2 ){
								if( scon1 >= 99 && scon1 <= 120 )
									continue;
								}
							sconX_.scon[opadriX_.sconpi[iz-One]-One][7-
							  One] = edscon;
							/*                   IF THIS IS A MULTI LOADED VC, SET ELEMENTS IN THE VC */
							if( opadriX_.opadri[iz-One] <= -100 && 
							  opadriX_.opadri[iz-One] >= -120 ){
								if( hpdopiX_.hfdopi[iz-One] >= HFPOLO && 
								  hpdopiX_.hfdopi[iz-One] <= HFPOHI ){
									adr2 = hpdopiX_.hfdopi[iz-One] - 
									  HFPOL1;
									jz2 = hfdoaX_.hfpoad[adr2-One][HFPADX-One];
									for( m=1; m <= jz2; m++ ){
										scon1 = sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-
										  One][1-One];
										if( scon1 >= 0 ){
											if( ms == 2 ){
												if( scon1 >= 99 && 
												  scon1 <= 120 )
												continue;
												}
											sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-
											  One][7-One] = edscon;
											}
										}
									}
								}
							}
						}
					}
				}
			/*        SCON IS LOCKED.  IGNORE ALL BUT CASE, AND MAYBE CASE TOO */
			}
		else if( vtfmc == vtrfvlX_.vtmrp && sconX_.scon[nsct-One][9-One] != 
		  7 ){
			edscon = sconX_.scon[sworkX_.phrhed[im81X_.im81-vtrc-One]-
			  One][7-One];
			ms = sworkX_.swork[im81X_.im81-vtrpnt-One][1-One];
			for( iz=flowckX_.phrstr; iz <= flowckX_.phrlst; iz++ ){
				scon1 = sconX_.scon[opadriX_.sconpi[iz-One]-One][1-
				  One];
				if( scon1 >= 0 ){
					if( ms == 2 ){
						if( scon1 >= 99 && scon1 <= 120 )
							goto L_2701;
						}
					sconX_.scon[opadriX_.sconpi[iz-One]-One][7-One] = edscon;
					/*                   IF THIS IS A MULTI LOADED VC, SET ELEMENTS IN THE VC */
					if( opadriX_.opadri[iz-One] <= -100 && opadriX_.opadri[iz-One] >= 
					  -120 ){
						if( hpdopiX_.hfdopi[iz-One] >= HFPOLO && hpdopiX_.hfdopi[iz-One] <= 
						  HFPOHI ){
							adr2 = hpdopiX_.hfdopi[iz-One] - HFPOL1;
							jz2 = hfdoaX_.hfpoad[adr2-One][HFPADX-One];
							for( m=1; m <= jz2; m++ ){
								scon1 = sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-
								  One][1-One];
								if( scon1 >= 0 ){
									if( ms == 2 ){
										if( scon1 >= 99 && scon1 <= 
										  120 )
											continue;
										}
									sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-
									  One][7-One] = edscon;
									}
								}
							}
						}
					}
L_2701:
				;
				/*        ALL DONE PROCESSING */
				}
			}
		}
	else{

		/*     A CONSTANT HAS BEEN PROCESSED TO RIGHT OF SW16 */

		nsct = prtscoX_.sct;

		/*     GENDER -- MAYBE IGNORE */

		if( vtrg != 0 ){
			edscon = 0;
			if( vtrg > 0 ){
				edscon = vtrg;
				}
			else if( vtfmg == vtrfvlX_.vtmrp ){
				/*           VTRG IS A RELATIVE POINTER */
				edscon = sconX_.scon[sworkX_.phrhed[im81X_.im81-vtrg-One]-
				  One][4-One];
				}
			else if( vtrg == -98 ){
				edscon = sw25bkX_.hedgen;
				}
			else if( vtrg == -99 ){
				if( sw36bkX_.rel == 1 ){
					edscon = sw36bkX_.relgen;
					}
				else{
					edscon = sw14bkX_.gen;
					}
				}

			sconX_.scon[prtscoX_.sct-One][4-One] = edscon;
			}


		/*     NUMBER -- MAYBE IGNORE */

		if( vtrnu != 0 ){
			edscon = 0;
			if( vtrnu > 0 ){
				edscon = vtrnu;
				}
			else if( vtfmnu == vtrfvlX_.vtmrp ){
				/*           VTRNU IS A RELATIVE POINTER */
				edscon = sconX_.scon[sworkX_.phrhed[im81X_.im81-vtrnu-One]-
				  One][5-One];
				}
			else if( vtrnu == -98 ){
				edscon = sw25bkX_.hednum;
				}
			else if( vtrnu == -99 ){
				if( sw36bkX_.rel == 1 ){
					edscon = sw36bkX_.relnum;
					}
				else{
					edscon = sw14bkX_.num;
					}
				}

			sconX_.scon[prtscoX_.sct-One][5-One] = edscon;
			}


		/*     PERSON -- MAYBE IGNORE */

		if( vtrp != 0 ){
			edscon = 0;
			if( vtrp > 0 ){
				edscon = vtrp;
				}
			else if( vtfmp == vtrfvlX_.vtmrp ){
				/*           VTRP IS A RELATIVE POINTER */
				edscon = sconX_.scon[sworkX_.phrhed[im81X_.im81-vtrp-One]-
				  One][6-One];
				}
			else if( vtrp == -98 ){
				edscon = sw25bkX_.hedper;
				}
			else if( vtrp == -99 ){
				if( sw36bkX_.rel == 1 ){
					edscon = sw36bkX_.relper;
					}
				else{
					edscon = sw14bkX_.per;
					}
				}

			sconX_.scon[prtscoX_.sct-One][6-One] = edscon;
			}


		/*     CASE -- MAYBE IGNORE */

		if( vtrc != 0 ){
			edscon = 0;
			if( vtrc > 0 ){
				edscon = vtrc;
				}
			else if( vtfmc == vtrfvlX_.vtmrp ){
				/*           VTRC IS A RELATIVE POINTER */
				edscon = sconX_.scon[sworkX_.phrhed[im81X_.im81-vtrc-One]-
				  One][7-One];
				}
			else if( vtrc == -98 ){
				edscon = sw25bkX_.hedcas;
				}
			else if( vtrc == -99 ){
				if( sw36bkX_.rel == 1 ){
					edscon = sw36bkX_.relcas;
					if(tranidX_.tranid == 2 && trgflgX_.trgflg==1)
					{
						if(sw36bkX_.relcas==5)
							edscon=3;	
						if(sw36bkX_.relcas==6||sw36bkX_.relcas==7)
							edscon=2;
					}
					}
				else{
					if( tranidX_.tranid == 4 )
						edscon = sw14bkX_.case_; 
					else
						edscon = sw21bkX_.case_; 
						if(tranidX_.tranid == 2 && trgflgX_.trgflg==1)
						{
							if(sw21bkX_.case_==5)
								edscon=3;
							if(sw21bkX_.case_==6||sw21bkX_.case_==7)
								edscon=2;
						}
					}
				}

			sconX_.scon[prtscoX_.sct-One][7-One] = edscon;



			}
		}



	if( diagsX_.deepdi == 1 )
		{
		fprintf( _spec_fp, " SW16 %6d%6d%6d%6d%6d%6d%6d", vbdataX_.k3n, 
		  vtrg, vtrnu, vtrp, vtrc, sw36bkX_.relper, sw36bkX_.rel );
		for( xx=4; xx <= 7; xx++ ){
			fprintf( _spec_fp, "%6d", sconX_.scon[nsct-One][xx-One] );
			}
		fprintf( _spec_fp, "\n" );
		scnprt();
		}

	return;
} /*end of function*/

