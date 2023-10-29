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
	/*   ***** BEGINNING OF -22 SWITCH ***** */
	/*        FUNCTION:  CALLS SUBROUTINE SEMTRN FOR SEARCH OF
	 *        SP22 RULES */
	/*    K3P1 IS NUMBER OF SEMWRKS TO BE CREATED
	 *    K3P2 POINTS TO THE SWORK FROM WHICH TO GET
	 *     TYPE SUBSET AND SET TO USE AS INDEX TO
	 *     SEMTRAN RULES
	 *    K3P3 POINTS TO A PARAMETER OF -47 SWITCH
	 *     WHICH HOLDS NEW ADDRESS
	 *    RELATIONAL POINTERS FOLLOWING K3P3 POINT TO SWORKS
	 *     FROM WHICH TO BUILD SEMWRKS */
/*         /SEMFLG/  =  semtab matching flags
	 *              VTRINH = 1 => inhibit execution of the VTR
	 *              SEMTCH = 1 = semtab match found */

	/*    *K3P1 IS NUMBER OF SEMWRKS TO BE CREATED
	 *    *K3P2 POINTS TO THE SWORK FROM WHICH TO GET
	 *     TYPE SUBSET AND SET TO USE AS INDEX TO
	 *     SEMTRAN RULES
	 *    *K3P3 POINTS TO A PARAMETER OF -47 SWITCH
	 *     WHICH HOLDS NEW ADDRESS
	 *    *RELATIONAL POINTERS FOLLOWING K3P3 POINT TO SWORKS
	 *     FROM WHICH TO BUILD SEMWRKS */
	/* CHANGES:
	 *    8/12/91*JAL:  IF -99 PARAMETER, DONT CREATE A SEMWRK IF SW36S=0. */
/* 
 * *         Rev 1.3   12/01/95 15:36:42   jonathan
 * *      Request Number:	PR000
 * *      Description:
 * *      Fix -22sw flag that inhibits semtab vtr execution.  Add SW20NX to the
 * *      number of elements sent to inhibit (SW20NX=50).
 * *
 * *         Rev 1.2   11/10/95 09:56:42   jonathan
 * *      Request Number:	PR2190
 * *      Description:
 * *      PR2190: inhibit execution of Semtab VTR based on a -22sw parameter.
 * *              Also pass back a flag to the TxSW22 indicating match results
 * *
	 *    08/09/94 jal:  L2107 : remove the -96 function (search to right for
	 *        a verb and make that the index) from TRANs 1,3,4.
 * *      Request Number:	PR000
 * *      Description:
 * *      Fix -22sw flag that inhibits semtab vtr execution.  Add SW20NX to the
 * *      number of elements sent to inhibit (SW20NX=50).
 * *
 * *         Rev 1.3   11/10/95 09:57:26   jonathan
 * *      Request Number:	PR2190
 * *      Description:
 * *      PR2190: inhibit execution of Semtab VTR based on a -22sw parameter.
 * *              Also pass back a flag to the TxSW22 indicating match results
 * *
 * *         Rev 1.2   01/19/95 14:34:04   stevez
 * *      import771
 * *
 * *         Rev 1.1   08/11/94 14:22:30   jonathan
 * *      Request Number:	L2107
 * *      Description:
 * *      # L2107  08/09/94  Steve's request to remove -96 function from
 * *      #      22 switch in TRANs 1,3,4.  -96 in switch syntax ( indicates
 * *      #      search to right for a verb & use it as the index) and
 * *      #      -96 as a true element pointer shifted by a stretch.
 * *      #      The -96 index is only used in TRAN2, according to Steve.
 * *
 * *> */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define MS_F77
#include <fcrt.h>
#include "logoslib.h"
#include "trans.h"
#include "projexts.h"
#include <string.h>
#include <jbctrl.h>
#include "parsetrans.h"
#include "parsetrans_ext.h"

void /*FUNCTION*/ txsw22(short *dia_k7)
{
	static short int   sendct,tmp;
	static long int    _l0;		
	static short   val9=9;
	static short   c=0;
	static short   w=0;
	static short   cb9=0;
	static short   s22=0;
	static short   k3p2=0;
	static short   numr=0;
	static short   retsw=0;
	static short matpos = 0;
	int status;


	k3p2=vtrfX_.vtrf[vbdataX_.k3+2-One];
	semargX_.k3p3=vtrfX_.vtrf[vbdataX_.k3+3-One];
	
	/*   K3P1 = 9XX IS A FLAG TO SEMTRAN THAT ALL SEMWRKS MUST MATCH */
	semargX_.allsem=0;
	
	if(vbdataX_.k3p1>900)
	{
		semargX_.allsem=1;
		vbdataX_.k3p1-=900;
	}
	
	/*         tens digit is a flag to inhibit VTR execution for matches
	 *         SW20NX is added to the number of semwrks in the send */
	if(vbdataX_.k3p1>SW20NX)
	{
		semflgX_.vtrinh=1;
		vbdataX_.k3p1-=SW20NX;
	}
	else
	{
		semflgX_.vtrinh=0;
	}
	vbdataX_.k3n=vbdataX_.k3+4+(vbdataX_.k3p1*2);
	semargX_.index=im81X_.im81-k3p2;
	
	if(tranidX_.tranid == 2)
	{
		if(k3p2== -96)
			semargX_.index= -96;
	}

	semtrX_.i3sem=w50valX_.i3;
	memset(semtrX_.orig,'\0',sizeof(semtrX_.orig));
	semtrX_.orig[0]=1;
	memset(msformX_.forms,'\0',sizeof(msformX_.forms));
	
	sendct=vbdataX_.k3p1+1;
	if(sendct>20)
		sendct=20;

	c=1;
	semargX_.nwrks=1;
	
	for(tmp=2; tmp<=sendct; tmp++)
	{
		if(tranidX_.tranid != 4)
		{
			if(vtrfX_.vtrf[vbdataX_.k3+3+c-One]!= -99)
			{
				semargX_.nwrks+=1;
				semtrX_.orig[semargX_.nwrks-One]=flowckX_.im1-vtrfX_.vtrf[vbdataX_.k3+3+c-One];			
				msformX_.forms[semargX_.nwrks-One]=vtrfX_.vtrf[vbdataX_.k3+3+c+1-One];
				if(tranidX_.tranid == 2)
				{
					if(vtrfX_.vtrf[vbdataX_.k3+3+c-One]== -96)
						semtrX_.orig[semargX_.nwrks-One]= -96;
				}
			}
			else if(sav36sX_.sav36s[0]>0)
			{
				semargX_.nwrks+=1;
				semtrX_.orig[semargX_.nwrks-One]=sav36sX_.sav36s[0];
				msformX_.forms[semargX_.nwrks-One]=vtrfX_.vtrf[vbdataX_.k3+3+c+1-One];
				flip99();
				s22=1;
			}
		}
		else
		{
				semargX_.nwrks+=1;
				semtrX_.orig[semargX_.nwrks-One]=flowckX_.im1-vtrfX_.vtrf[vbdataX_.k3+3+c-One];			
				msformX_.forms[semargX_.nwrks-One]=vtrfX_.vtrf[vbdataX_.k3+3+c+1-One];
		}

		c+=2;
	}
	
	semargX_.tran=(short)tranidX_.tranid;
	scommsX_.switch_ = diagsX_.deepdi;
	if (tranidX_.tranid == 1){
		semtr1();
	}
	else{
		semtr2();
	}

	/*            if Match then  Cell2 = 1, else Cell2 = 0 */
		if(semflgX_.semtch==1)
		{
			vbdataX_.vbcell[1]=1;
		}
		else
		{
			vbdataX_.vbcell[1]=0;
		}
		
		if(tranidX_.tranid == 3)
		{
			if(s22==1)
				flip99();
		}
		s22=0;

		
		if(semargX_.pntr9!=0)
		{
			if(loopckX_.call36[0]==0)
			{
				loopckX_.call36[0]=loopckX_.nptpsv;
				
				if(minickX_.minifg==1)
					loopckX_.call36[1]=1;
			}
			
			/*                          IS THERE A MINI RULE? */
			cb9=semargX_.pntr9;
			txmini(3,&minickX_.k7m,&loopckX_.nptpx,vwarg2X_.i,&cb9);

			idxval((short*)ADR(_l0,1),&val9,&semargX_.pntr9,dia_k7,&numr);
			
			if(!(numr==0&&minickX_.k7m==0))
			{
				matpos = commdX_.matpos;
				if(tranidX_.tranid == 2)
					matpos = 0;
				status = rulein_ptr((short*)ADR(_l0,1),&matpos,
					spX_ptr,dia_k7,&retsw);
				if ( status )
					errvrsX_.errlvl = status;
				if( errvrsX_.errlvl == 0 ) 
				{
					/*                  GETVTR = 1  WILL MAKE US SKIP OUT OF PROCESSING
					 *                              OF THE VTR AND CAUSE THE WC9 VTR TO
					 *                              BE PROCESSED. */
					if(*dia_k7!=0 || minickX_.k7m!=0)
						getvtrX_.getvtr=1;
				}
			}
		}

	return;
}    /*end of function*/
