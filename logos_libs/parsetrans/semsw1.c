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
/* Routine to process the switches from a semtab rule.
   This is called by  by tran1 and not by tran 2,3,4.
   It does the same basic as semsw2
*/       
	/*   CHANGES:
	 *      09/16/94 jal: null black hole counter for new verb  LOG#2128
	 *      11/23/93 AVK: If german phrasal transfer, use pat of head element
	 *      09/03/93 jal: Dont allow target work for pass1 of 2.
	 *      10/29/91 JAL: FOR SW47 SET SCON(14) = 2, SW36 SET SCON(14) = 1
	 *      10/09/91 JAL: FOR SW47, SET SCON59 = TARGET MAIN PAT NUM
	 *      06/04/91 *JAL: PUT /SP2/ INTO INCLUDE & RENAMED NSWRK1 TO SWORKO.
	 *      03/19/90 *JAL: REMOVED SEM46() CODE WHICH DIDNT WORK ANYWAY.
	 *      04/17/87 *R1685RKH*  CHANGE T1-4 SWORK1 LIMIT FROM 50 TO
	 *      04/01/87 *R1703RKH*  BLANK CONSTANT(-3203-36997)
	 *      08/17/86 *R1561DSD: 100 SCONS
	 *      03/11/86 */
	/*      INCLUDE 'SWRK1I.h'
	 *          SWRK1i -  Sentence elements received from RES.  The array has        
	 *                    15 values/element & accounts for all 3 parts of speech     
	 *                    from the dictionary for each sentence element.             
	 *          ECNTI  -  number of sentence elements(# of elements in SWORK1).       */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <fcrt.h>
#include "trans.h"
#include "logoslib.h"
#include "projexts.h"
#include <string.h>
#include "parsetrans_ext.h"
#include "parsetrans.h"
#include <jbctrl.h>

EXTERN char curr_sp5rule_company_code[3];
EXTERN char save_sp5rule_company_code[3];

void /*FUNCTION*/ semsw1(short swnum, short sw_val1, short *retflg)
{
	int                taddress,dictype;
	char               company_code[3];
	static short int   vtrc,vtrg,vtrnu,vtrp,vtrpnt,swork_pos;
	static char    pgmnam[9]="T1SEMSWS";
	static short   x=0;
	static short   gb=0;
	static short   iz=0;
	static short   jz=0;
	static short   n4=0;
	static short   wc=0;
	static short   xx=0;
	static short   x1=0;
	static short   yy=0;
	static short   adr_=0;
	static short   gbx=0;
	static short   jzz=0;
	static short   kgb=0;
	static short   nn4=0;
	static short   rel=0;
	static short   tmp=0;
	static short   zzc=0;
	static short   zzg=0;
	static short   zzp=0;
	static short   addr=0;
	static short   fm46=0;
	static short   k3p1=0;
	static short   k3p2=0;
	static short   k3p3=0;
	static short   k3p4=0;
	static short   prm2=0;
	static short   ty46=0;
	static short   wc46=0;
	static short   zero=0;
	static short   zznu=0;
	static short   addr1=0;
	static short   izbeg=0;
	static short   izend=0;
	static short   n6jim=0;
	static short   n7jim=0;
	static short   type1=0;
	static short   type2=0;
	static short   type3=0;
	static short   zzpnt=0;
	static short   phrbeg=0;
	static short   phrlst=0;
	static short   phrstr=0;
	static short   relcas=0;
	static short   scnpos=0;
	static short   semsct=0;
	static short   supm12=0;
	static short   supset=0;
	static short   switch_=0;
	static short dctwc[3]={4,8,12};
	static short dcttyp[3]={5,9,13};
	static short dctfm[3]={6,10,14};
	static short dctad[3]={7,11,15};

	*retflg=0;
	
	if(swnum==16)
	{
		/*        * * * START OF -16 SWITCH * * * */
		scommkX_.k3n=scommkX_.k3+6;
		
		/*                   No target work if pass 1 of 2. */
		if(!(passesX_.passfl==1&&passesX_.passct==1))
		{
			vtrg=svtrfX_.svtrf[scommkX_.k3+1-One];
			
			if(vtrg<0)
				zzg=semtrX_.orig[sgnX_.gn[-79-vtrg-One]-One];
			vtrnu=svtrfX_.svtrf[scommkX_.k3+2-One];
			
			if(vtrnu<0)
				zznu=semtrX_.orig[sgnX_.gn[-79-vtrnu-One]-One];
			vtrp=svtrfX_.svtrf[scommkX_.k3+3-One];
			
			if(vtrp<0)
				zzp=semtrX_.orig[sgnX_.gn[-79-vtrp-One]-One];
			vtrc=svtrfX_.svtrf[scommkX_.k3+4-One];
			
			if(vtrc<0)
				zzc=semtrX_.orig[sgnX_.gn[-79-vtrc-One]-One];
			vtrpnt=svtrfX_.svtrf[scommkX_.k3+5-One];
			
			if(vtrpnt<=0)
			{
				zzpnt=semtrX_.orig[sgnX_.gn[-79-vtrpnt-One]-One];
				
				/*                            DO NOTHING IF HEAD IS LOCKED */
				if(sconX_.scon[zzpnt-One][1-One]>=0)
				{
					/*                            SET THE SCON FILL VALUES */
					if(vtrg!=0)
					{
						if(vtrg<0)
						{
							sconX_.scon[zzpnt-One][4-One]=sconX_.scon[zzg-One][4-One];
						}
						else
						{
							sconX_.scon[zzpnt-One][4-One]=vtrg;
						}
					}
					
					/*                             NUMBER */
					if(vtrnu!=0)
					{
						if(vtrnu<0)
						{
							sconX_.scon[zzpnt-One][5-One]=sconX_.scon[zznu-One][5-One];
						}
						else
						{
							sconX_.scon[zzpnt-One][5-One]=vtrnu;
						}
					}
					
					/*                             PERSON */
					if(vtrp!=0)
					{
						if(vtrp<0)
						{
							sconX_.scon[zzpnt-One][6-One]=sconX_.scon[zzp-One][6-One];
						}
						else
						{
							sconX_.scon[zzpnt-One][6-One]=vtrp;
						}
					}
					
					/*                             CASE */
					if(vtrc!=0)
					{
						if(vtrc<0)
						{
							sconX_.scon[zzpnt-One][7-One]=sconX_.scon[zzc-One][7-One];
						}
						else
						{
							sconX_.scon[zzpnt-One][7-One]=vtrc;
						}
					}
					
					/*                            SET SEMCAS FOR USE BY SW38 */
					if(swork1X_.swork1[zzpnt-One][dctwc[prctX_.js[sconX_.scolnk[zzpnt-One]-One]-One]-One]==4)
					{
						if(vtrc>0&&vtrg==0)
						{
							if(vtrnu==0&&vtrp==0)
								case_X_.semcas=vtrc;
						}
					}
				}
			}
		}
		
		if(diagsX_.deepdi!=0)
		{
			fprintf(_spec_fp," SEM SW16  %6d%6d%6d%6d",scommkX_.k3n,vtrpnt,phrstr,phrlst);			
			for(xx=4; xx<=7; xx++)
			{
				fprintf(_spec_fp,"%6d",sconX_.scon[semsct-One][xx-One]);
			}
			fprintf(_spec_fp,"\n");
		}
	}




	else if(swnum==32)
	{
		/*        * * * START OF -32 SWITCH * * * */
		/*        -32 SWITCH WILL SET FLAGS FOR THE -36,-44 OR
		 *        -47 SWITCH IMMEDIATELY FOLLOWING IT.
		 *        PURPOSE:  TO ACCESS A CERTAIN RANGE (1000,2000,..)
		 *                  OF THE lo-FREQ CONSTANT DICTIONARY. */
		prm2=svtrfX_.svtrf[scommkX_.k3+2-One];
		scommkX_.k3n=scommkX_.k3+3;
		
		/*                   No target work if pass 1 of 2. */
		if(!(passesX_.passfl==1&&passesX_.passct==1))
		{
			if(sw_val1== -99)
			{
				/*        -99 FUNCTION SETS AD32 TO 1000'S PLACE OF
				 *        HI-FREQ CONSTANT TO BE USED BY -36 OR
				 *        -44 SWITCH */
				flag32X_.ad32=prm2;
				
				/*        FUNCTIONS 1-6 FOR -47 SWITCH */
			}
			else if(!(sw_val1<1||sw_val1>6))
			{
				flag32X_.prm32=sw_val1;
				flag32X_.ad32=prm2;
			}
		}
	}



	else if(swnum==36)
	{
		/*        * * * START OF -36 SWITCH * * * */
		/*        SW36 WILL NOT OVERRIDE A LOCKED SCON */
		prm2=svtrfX_.svtrf[scommkX_.k3+2-One];
		scommkX_.k3n=scommkX_.k3+3;
		
		/*                   No target work if pass 1 of 2. */
		if(!(passesX_.passfl==1&&passesX_.passct==1))
		{
			/*        ;PRM2 IS A REL. PTR. */
			n6jim= -79-prm2;
			xx=semtrX_.orig[sgnX_.gn[n6jim-One]-One];
			x1=xx;
			
			if(flag32X_.ad32== -1)
			{
				if(sw_val1>=11&&sw_val1<=14)
				{
					/*        SWITCH 36 */
					if(srcflgX_.srcflg==1)
					{
						n6jim= -79-prm2;
						xx=semtrX_.orig[sgnX_.gn[n6jim-One]-One];
						
						/*        TRAN1 - VALID WORD CLASSES 1 AND 7 */
						iz=swork1X_.swork1[xx-One][dctwc[prctX_.js[sconX_.scolnk[xx-One]-One]-One]-One];						
						if(!(iz!=1&&iz!=7))
						{
							sw36cX_.sw36no=xx;
							sw36cX_.sw36pa=nounsX_.gernum[sw_val1-10-One][formsaX_.formsv[sconX_.scolnk[xx-One]-One]-One];
						}
					}
					goto L_4160;
				}
			}
			addr= -sw_val1;
			
			//  CHECK FOR E-F ANY -36997. THESE ARE NULL AND SHOULD BE SKIPPED.
			//  test for portugues as it is baseed on spanish which is being tested
			if(!(((srcflgX_.srcflg==2
				 && (trgflgX_.trgflg==3||trgflgX_.trgflg==4||trgflgX_.trgflg==6))
				 && sw_val1==997)
				 && flag32X_.ad32==0))
			{
				// Blank Constant(-3203-36997)                 RKH  04/01/87   R1703
				if(flag32X_.ad32!= -1)
				{
					addr=sw_val1+(1000* flag32X_.ad32);
					flag32X_.ad32= -1;
				}
				swork1X_.swork1[xx-One][dctad[prctX_.js[sconX_.scolnk[xx-One]-One]-One]-One]=addr;
				memcpy(cmpsmcX_.cmpcod[xx-One][prctX_.js[sconX_.scolnk[xx-One]-One]-One],
					   curr_sp5rule_company_code,sizeof(cmpsmcX_.cmpcod[0][0]));
				
				if(passesX_.passfl==1&&passesX_.passct==1)
					swork1X_.swrk1i[xx-One][dctad[prctX_.js[sconX_.scolnk[xx-One]-One]-One]-One]=addr;
				
				sconX_.scon[xx-One][14-One]=1;
				if(addr<0&&addr> -1000)
					sconX_.scono[sconX_.scolnk[xx-One]-One][60-SCONX1-One]= -addr;
				
				if( swork1X_.swork1[xx-One][dctwc[prctX_.js[sconX_.scolnk[xx-One]-One]-One]-One]==3
				  ||swork1X_.swork1[xx-One][dctwc[prctX_.js[sconX_.scolnk[xx-One]-One]-One]-One]==13)
					swork1X_.swork1[xx-One][dctfm[prctX_.js[sconX_.scolnk[xx-One]-One]-One]-One]=53;
				
				/*+                SET SCON59 = TARGET PAT NUM          *10/10/91*JAL* */
				if(addr<0&&addr> -1000)
				{    // HIGH  CONSTANT DICTIONARY 				
					taddress= -addr;
					dictype=2;
					memcpy(company_code,LOGCC,3);
				}
				else
				{    // low constant
					taddress=addr;
					dictype=3;
					memcpy(company_code,curr_sp5rule_company_code,3);
				}
				errvrsX_.err=TARG_CODES(&trgflgX_.trgflg,&dictype,&taddress,company_code,(short*)&trgcdsX_.tcwdad,diagsX_.longdi,_spec_fp);				
				if(errvrsX_.err!=0)
				{
					errlog(pgmnam,3699,taddress,10);
				}
				sconX_.scono[sconX_.scolnk[xx-One]-One][59-SCONX1-One]=trgcdsX_.tcpatm[0];
			}
		}
	L_4160:
		
		if(diagsX_.deepdi!=0)
		{
			fprintf(_spec_fp," SEM SW36\n   %5d%5d%5d%5d%5d\n",n6jim,xx,phrstr,phrlst,sw_val1);
		}
	}
	
	
	
	
	else if(swnum==44)
	{
		/*        * * * START OF -44 SWITCH * * * */
		scommkX_.k3n=scommkX_.k3+5;
		
		/*                   No target work if pass 1 of 2. */
		if(!(passesX_.passfl==1&&passesX_.passct==1))
		{
			k3p1=svtrfX_.svtrf[scommkX_.k3+1-One];
			k3p2=svtrfX_.svtrf[scommkX_.k3+2-One];
			k3p3=svtrfX_.svtrf[scommkX_.k3+3-One];
			k3p4=svtrfX_.svtrf[scommkX_.k3+4-One];
			
			/*+                                                         *R0157CEB */
			/*   HAS THIS VC ALREADY BEEN ADVANCED-SET? */
			if(sw44bkX_.pre44!=0)
			{
				for(gb=1; gb<=sw44bkX_.pre44; gb++)
				{
					if(sw44bkX_.ad44vc[gb-One][1-One]==k3p2)
						goto L_9202;
				}
				
				/*   ARRAY AD44VC CONTAINS PARAMETERS FOR UP TO 5 ADVANCED-SET VC'S
				 *   PRE44 IS THE COUNTER FOR ARRAY AD44VC */
				if(sw44bkX_.pre44==5)
					goto L_5390;
				goto L_5386;
			L_9202:
				iz=sw44bkX_.ad44vc[gb-One][2-One];
				goto L_5392;
			}
		L_5386:
			sw44bkX_.pre44+=1;
			prtscoX_.sct+=1;
			
			/*+                                         *B0305JGB */
			if(prtscoX_.sct>SCONY)
			{
				prtscoX_.sct=SCONY;
				prtscoX_.scterr+=1;
				
				if(diagsX_.shrtdi!=0)
				{
					fprintf(_spec_fp," T1SEMSWS, OVERLOADING SCON ARRAY, SCTERR =%4d\n",prtscoX_.scterr);
				}
				
				if(prtscoX_.scterr==1)
					errlog(pgmnam,5386,500,13);
				return;
			}
			else
			{
				diacbX_.n3+=1;
				iz=diacbX_.n3;
				swork_pos = sconX_.scon[prtscoX_.sct-One][10-One];

				memcpy(cmpsmcX_.cmpcod[swork_pos-One][prctX_.js[swork_pos-One]-One],
					   curr_sp5rule_company_code,sizeof(cmpsmcX_.cmpcod[0][0]));

				opadroX_.opadro[diacbX_.n3-One]= -k3p2;
				opadroX_.sconpo[diacbX_.n3-One]= prtscoX_.sct;

				memset(&sconX_.scon[prtscoX_.sct-One][0],'\0',sizeof(sconX_.scon[0]));
				sconX_.scon[prtscoX_.sct-One][1-One]=k3p2;
				x= -79-k3p1;
				sconX_.scon[prtscoX_.sct-One][2-One]=semtrX_.orig[sgnX_.gn[x-One]-One];
				
				/*   FILL FIRST OPEN POSITION IN AD44VC ARRAY */
				for(kgb=1; kgb<=5; kgb++)
				{
					if(sw44bkX_.ad44vc[kgb-One][1-One]==0)
						goto L_9203;
				}
				goto L_5390;
			L_9203:
				sw44bkX_.ad44vc[kgb-One][1-One]=k3p2;
				sw44bkX_.ad44vc[kgb-One][2-One]=diacbX_.n3;
				goto L_5392;
			}
		L_5390:
			
			if(diagsX_.shrtdi!=0)
			{
				fprintf(_spec_fp," PRE-SET ARRAY OVERLOADED %6d\n",k3p2);
			}
			goto L_6844;
		L_5392:
			
			if(k3p3>=0)
			{
				addr= -k3p3;
				
				if(flag32X_.ad32!= -1)
				{
					addr=k3p3+(1000* flag32X_.ad32);
					flag32X_.ad32= -1;
				}
			}
			
			if(hpdopoX_.hfdopo[iz-One]!=0)
			{
				if(k3p4== -97||k3p4== -96)
				{
					/*   K3P4 EQUALS -97 OR -96; ADD NEW ADDRESSES TO OLD */
					if(hpdopoX_.hfdopo[iz-One]<HFPOLO||hpdopoX_.hfdopo[iz-One]>HFPOHI)
					{
						/*   HFDOPO CONTAINS A CONSTANT; PUT IT IN A NEW HFPOAD */
						jz=1;
						hfdoaX_.adct+=1;
						
						if(hfdoaX_.adct<=HFPADY)
						{
							adr_=hfdoaX_.adct;
							hfdoaX_.hfpoad[adr_-One][jz-One]=hpdopoX_.hfdopo[iz-One];
							hfdoaX_.sconhf[adr_-One][jz-One]=opadroX_.sconpo[iz-One];
							jz+=1;
							
							if(k3p3<0)
								goto L_6760;
						}
						else
						{
							if(switch_==1)
							{
								fprintf(_spec_fp," HFDOPO 65 TO 74 TAKEN - EXIT SW 44\n");
							}
							errlog(pgmnam,6705,502,1);
							goto L_6844;
						}
					}
					else
					{
						/*   GET ADDRESS FOR HFPOAD FROM HFDOPO */
						adr_=hpdopoX_.hfdopo[iz-One]-HFPOL1;
						jz=hfdoaX_.hfpoad[adr_-One][HFPADX-One]+1;
						
						if(k3p3<=0)
							goto L_6760;
					}
					
					/*   K3P3 IS A CONSTANT; PUT IT IN SAME HFPOAD */
					hfdoaX_.hfpoad[adr_-One][jz-One]=addr;
					hfdoaX_.sconhf[adr_-One][jz-One]=opadroX_.sconpo[iz-One];
					hfdoaX_.hfpoad[adr_-One][HFPADX-One]=jz;
					hpdopoX_.hfdopo[iz-One]=HFPOL1+adr_;
					goto L_6780;
				}
			}
			
			/*   K3P4 NOT EQUAL -97 OR -96; NEW ADDRESSES OVERLAY OLD */
			if(k3p3>=0)
			{
				/*   K3P3 IS A CONSTANT; PLACE DIRECTLY IN HFDOPO */
				hpdopoX_.hfdopo[iz-One]=addr;
				goto L_6780;
				
				/*   K3P3 POINTS TO A PHRASE */
			}
			else if(hpdopoX_.hfdopo[iz-One]==0)
			{
				n6jim= -79-k3p3;
				hpdopoX_.hfdopo[iz-One]=swork1X_.swork1[n6jim-One][dctad[prctX_.js[sconX_.scolnk[n6jim-One]-One]-One]-One];
				goto L_6780;
			}
			else if(hpdopoX_.hfdopo[iz-One]<HFPOLO||hpdopoX_.hfdopo[iz-One]>HFPOHI)
			{
				/*   PLACE ELEMENTS OF PHRASE IN NEW HFPOAD */
				jz=1;
				hfdoaX_.adct+=1;
				
				if(hfdoaX_.adct<=HFPADY)
				{
					adr_=hfdoaX_.adct;
				}
				else
				{
					if(switch_==1)
					{
						fprintf(_spec_fp," HFDOPO 65 TO 74 TAKEN - EXIT SW 44\n");
					}
					errlog(pgmnam,6740,502,1);
					goto L_6844;
				}
			}
			else
			{
				/*   USE OLD ADDRESS; BLANK OUT OLD ELEMENTS */
				jz=1;
				adr_=hpdopoX_.hfdopo[iz-One]-HFPOL1;
				
				for(jzz=1; jzz<=20; jzz++)
				{
					hfdoaX_.hfpoad[adr_-One][jzz-One]=0;
				}
			}
			
			/*   K3P3 POINTS TO A PHRASE; ADD ELEMENTS IN PHRASE(N6JIM) TO HFPOAD */
		L_6760:
			n6jim= -79-k3p3;
			hfdoaX_.hfpoad[adr_-One][jz-One]=swork1X_.swork1[n6jim-One][dctad[prctX_.js[sconX_.scolnk[n6jim-One]-One]-One]-One];
			hfdoaX_.sconhf[adr_-One][jz-One]=elscnpX_.elscnp[n6jim-One];
			hfdoaX_.hfpoad[adr_-One][HFPADX-One]=jz;
			hpdopoX_.hfdopo[iz-One]=HFPOL1+adr_;
		L_6780:
			
			if(k3p4>0)
				sconX_.scon[opadroX_.sconpo[iz-One]-One][7-One]=k3p4;
			
			if(k3p4==(-99))
			{
				sconX_.scon[opadroX_.sconpo[iz-One]-One][7-One]=case_X_.case_;
				
				if(rel>0)
					sconX_.scon[opadroX_.sconpo[iz-One]-One][7-One]=relcas;
			}
			else if(k3p4== -13)
			{
				if(sconX_.scon[opadroX_.sconpo[iz-One]-One][1-One]>=0)
					sconX_.scon[opadroX_.sconpo[iz-One]-One][1-One]= -sconX_.scon[opadroX_.sconpo[iz-One]-One][1-One];
			}
		}
	L_6844:
		
		if(diagsX_.deepdi==1)
		{
			fprintf(_spec_fp," SEM SW44 %4d%4d%4d%4d%4d%4d%4d\n",x,k3p1,k3p2,k3p3,k3p4,iz,gb);
		}
		
	}





	else if(swnum==46)
	{
		/*        * * * START OF -46 SWITCH * * * */
		scommkX_.k3n=scommkX_.k3+5;
		k3p2=svtrfX_.svtrf[scommkX_.k3+2-One];
		k3p3=svtrfX_.svtrf[scommkX_.k3+3-One];
		k3p4=svtrfX_.svtrf[scommkX_.k3+4-One];
		
		if(sw_val1==1)
		{
			/*        FUNCTION 1 OF THE -46 SWITCH RESETS SUBSET,SET, AND
			 *        SUPERSET ACCORDING TO THE 3 PARAMETERS, AND STARTS A
			 *        NEW SEARCH. */
			srch46X_.srch46=1;
		}
		else if(sw_val1!=2)
		{
			n6jim= -79-sw_val1;
			yy=semtrX_.orig[sgnX_.gn[n6jim-One]-One];
			n4=yy;
			nn4=sworkoX_.sworko[sworkoX_.phcto-One][4-One];
			wc46=dctwc[prctX_.js[sconX_.scolnk[yy-One]-One]-One];
			ty46=dcttyp[prctX_.js[sconX_.scolnk[yy-One]-One]-One];
			fm46=dctfm[prctX_.js[sconX_.scolnk[yy-One]-One]-One];
			
			if(k3p2!=0)
			{
				if(k3p2>0)
				{
					swork1X_.swork1[yy-One][wc46-One]=k3p2;
				}
				else
				{
					xx=semtrX_.orig[sgnX_.gn[-79-k3p2-One]-One];
					swork1X_.swork1[yy-One][wc46-One]=swork1X_.swork1[xx-One][dctwc[prctX_.js[sconX_.scolnk[xx-One]-One]-One]-One];
				}
				
				/*        IN CASE SWORKO ALREADY CREATED FROM SWORK1 */
				if(n4==nn4&&sw26nX_.sw26n<=0)
					sworkoX_.sworko[sworkoX_.phcto-One][1-One]=swork1X_.swork1[yy-One][wc46-One];
			}
			
			if(k3p3!=0)
			{
				if(k3p3>0)
				{
					/*        TO MAKE SURE SUBSET, SET, AND SUPERSET ARE STILL AVAILABLE */
					type1=swork1X_.swork1[yy-One][ty46-One];
					
					/*OM+ */
					type2=sconX_.scon[yy-One][11-One];
					type3=sconX_.scon[yy-One][13-One];
					
					/*OM- */
					if(k3p3<17)
					{
						if(type1<17)
							swork1X_.swork1[yy-One][ty46-One]=k3p3;
						
						if(type2<17)
							sconX_.scon[yy-One][11-One]=k3p3;
						sconX_.scon[yy-One][13-One]=k3p3;
					}
					else if(k3p3>99)
					{
						swork1X_.swork1[yy-One][ty46-One]=k3p3;
						
						if(type2>99)
							sconX_.scon[yy-One][11-One]=k3p3;
						
						if(type3>99)
							sconX_.scon[yy-One][13-One]=k3p3;
					}
					else
					{
						if(type1>=17&&type1<=99)
							swork1X_.swork1[yy-One][ty46-One]=k3p3;
						sconX_.scon[yy-One][11-One]=k3p3;
						
						if(type3>=17&&type3<=99)
							sconX_.scon[yy-One][13-One]=k3p3;
					}
				}
				else
				{
					xx=semtrX_.orig[sgnX_.gn[-79-k3p3-One]-One];
					swork1X_.swork1[yy-One][ty46-One]=swork1X_.swork1[xx-One][dcttyp[prctX_.js[sconX_.scolnk[xx-One]-One]-One]-One];
					
					sconX_.scon[yy-One][11-One]=sconX_.scon[xx-One][11-One];
					sconX_.scon[yy-One][13-One]=sconX_.scon[xx-One][13-One];
				}
				
				if(n4==nn4&&sw26nX_.sw26n<=0)
					sworkoX_.sworko[sworkoX_.phcto-One][2-One]=swork1X_.swork1[yy-One][ty46-One];
			}
			
			if(k3p4!=0)
			{
				if(k3p4>0)
				{
					swork1X_.swork1[yy-One][fm46-One]=k3p4;
				}
				else
				{
					xx=semtrX_.orig[sgnX_.gn[-79-k3p4-One]-One];
					swork1X_.swork1[yy-One][fm46-One]=swork1X_.swork1[xx-One][dctfm[prctX_.js[sconX_.scolnk[xx-One]-One]-One]-One];
					
					/*        IN CASE SWORKO ALREADY CREATED FROM SWORK1 */
					if(n4==nn4)
						sworkoX_.sworko[sworkoX_.phcto-One][3-One]=swork1X_.swork1[yy-One][fm46-One];
				}
			}
			
			if(diagsX_.deepdi!=0)
			{
				fprintf(_spec_fp," SEM SW46\n   %5d%5d%5d%5d%5d\n",n6jim,yy,type1,type2,type3);
			}
			
			return;
		}
		
		if(k3p2!=0)
			sconX_.scon[semargX_.index-One][11-One]=k3p2;
		
		if(k3p3!=0)
			swork1X_.swork1[semargX_.index-One][dcttyp[prctX_.js[sconX_.scolnk[semargX_.index-One]-One]-One]-One]=k3p3;
		
		if(k3p4!=0)
			sconX_.scon[semargX_.index-One][13-One]=k3p4;
		
		*retflg=1;
	}








	else if(swnum==47)
	{
		/*        * * * START OF -47 SWITCH * * * */
		scommkX_.k3n=scommkX_.k3+8;
		
		/*                   No target work if pass 1 of 2. */
		if(!(passesX_.passfl==1&&passesX_.passct==1))
		{
			if(semargX_.k3p3>10)
			{
				semargX_.pntr9=semargX_.k3p3;
				wc=swork1X_.swork1[semargX_.index-One][dctwc[prctX_.js[sconX_.scolnk[semargX_.index-One]-One]-One]-One];
				
				if(wc!=2&&wc!=14)
				{
					semargX_.k3p3=1;
					
					if(wc!=4)
					{
						supset=sconX_.scon[semargX_.index-One][13-One];
						
						if(supset>=13||supset<=17)
						{
							supm12=supset-12;
							
							if(supm12==2)
							{
								semargX_.k3p3=6;
							}
							else if(supm12==3)
							{
								semargX_.k3p3=3;
							}
							else if(supm12==4)
							{
								semargX_.k3p3=4;
							}
							else
							{
								semargX_.k3p3=5;
							}
						}
						else
						{
							semargX_.k3p3=2;
						}
					}
				}
				else
				{
					semargX_.k3p3=1;
				}
			}
			
			/*        FIRST PARAMETER OF SW47 IS THOUSANDS DIGIT OF ADDRESS
			 *    TEMPORARY CODE FOR ENGLISH FRENCH SYSTEM: 8/1/84   997 = NULL
			 *                       ENGLISH SPANISH    JMB31186 */
			//  test for portugues as it is based on spanish which is being tested
			if((srcflgX_.srcflg!=2&&
			   (trgflgX_.trgflg!=3&&trgflgX_.trgflg!=4&&trgflgX_.trgflg!=6))
			   ||svtrfX_.svtrf[scommkX_.k3+1-One]!=997)
			{
				addr1=svtrfX_.svtrf[scommkX_.k3+1+semargX_.k3p3-One];
				addr=addr1+(1000*(svtrfX_.svtrf[scommkX_.k3+1-One]));
				
				/*        HAS THE FLAG BEEN SET BY THE -32 SWITCH? */
				if(!(flag32X_.prm32==0||flag32X_.ad32== -1))
				{
					if(flag32X_.prm32==semargX_.k3p3)
						addr=addr1+(1000* flag32X_.ad32);
				}
				flag32X_.prm32=0;
				flag32X_.ad32= -1;
				
				if(addr!=0)
				{
					/*        CODE FROM HERE TO 47191 MODIFIES
					 *        SCONS OF NON-PREDICATES ACCORDING TO PNTR
					 *+ 89/08/30                                                 *R0GBA*GBA */
					if(srcflgX_.srcflg==1)
					{
						if((sconX_.scon[semargX_.index-One][1-One]==4&&sconX_.scon[semargX_.index-One][13-One]==16)&&sconX_.scono[sconX_.scolnk[semargX_.index-One]-One][45-SCONX1-One]==4)
							goto L_8080;
					}
					
					if(!(semargX_.k3p3<2||semargX_.k3p3>6))
					{
						if(srcflgX_.srcflg==1&&trgflgX_.trgflg!=2)
						{
							if(!((semargX_.k3p3==1||semargX_.k3p3==5)||semargX_.k3p3==6))
							{
								if(semargX_.k3p3==3)
								{
									sconX_.scon[semargX_.index-One][7-One]=10;
								}
								else if(semargX_.k3p3==4)
								{
									sconX_.scon[semargX_.index-One][7-One]=11;
								}
								else
								{
									sconX_.scon[semargX_.index-One][8-One]=2;
								}
							}
						}
						else if(addr1==svtrfX_.svtrf[scommkX_.k3+2-One])
						{
							if(!((semargX_.k3p3==1||semargX_.k3p3==2)||semargX_.k3p3==5))
							{
								if(semargX_.k3p3==4)
								{
									/*420  SEMCAS = 5 */
									sconX_.scon[semargX_.index-One][7-One]=5;
								}
								else if(semargX_.k3p3==6)
								{
									case_X_.semc14=5;
								}
								else
								{
									/*420  SEMCAS = 4 */
									sconX_.scon[semargX_.index-One][7-One]=4;
								}
							}
						}
					}
					swork1X_.swork1[semargX_.index-One][dctad[prctX_.js[sconX_.scolnk[semargX_.index-One]-One]-One]-One]=addr;
					memcpy(cmpsmcX_.cmpcod[semargX_.index-One][prctX_.js[sconX_.scolnk[semargX_.index-One]-One]-One],
						   curr_sp5rule_company_code,sizeof(cmpsmcX_.cmpcod[0][0]));
					if(passesX_.passfl==1&&passesX_.passct==1)
						swork1X_.swrk1i[semargX_.index-One][dctad[prctX_.js[sconX_.scolnk[semargX_.index-One]-One]-One]-One]=addr;

					sconX_.scon[semargX_.index-One][14-One]=2;
					
					/* GBA CAPTURE GENDER VALUE IN TARG25 FOR ALL LANGS
					 * JAL SET SCON59 TO TARGET MAIN PAT NUMBER */
					taddress=addr;
					dictype=3;
					memcpy(company_code,curr_sp5rule_company_code,3);
					errvrsX_.err=TARG_CODES(& trgflgX_.trgflg,& dictype,&taddress,company_code,(short*)&trgcdsX_,diagsX_.longdi,_spec_fp);
					if(errvrsX_.err==0)
					{
						targ25X_.targ25[sconX_.scolnk[semargX_.index-One]-One]=trgcdsX_.tcgenm;
						ofl2X_.ofl2i[sconX_.scolnk[semargX_.index-One]-One]=trgcdsX_.tcov2b;
						sconX_.scon[semargX_.index-One][3-One]=trgcdsX_.tcov2b;
						sconX_.scono[sconX_.scolnk[semargX_.index-One]-One][59-SCONX1-One]=trgcdsX_.tcpatm[0];
						
						//             null black hole counter for new verb  LOG#2128 jal 9/16/94 */
						sconX_.scono[sconX_.scolnk[semargX_.index-One]-One][100-SCONX1-One]=0;
					}
					else
					{
						errlog(pgmnam,8061,taddress,10);
					}
				}
			}
			else
			{
				addr=0;
			}
		}
	L_8080:
		
		if(diagsX_.longdi!=0)
		{
			fprintf(_spec_fp," SEM SW47\n   %5d%5d%5d%5d%5d%5d%5d\n",semargX_.k3p3,semargX_.pntr9,wc,supset,addr,phrstr,phrlst);
		}
	}









	else if(swnum==48)
	{
		/*        * * * START OF -48 SWITCH * * * */
		scommkX_.k3n=scommkX_.k3+4;
		
		/*                   No target work if pass 1 of 2. */
		if(!(passesX_.passfl==1&&passesX_.passct==1))
		{
			k3p2=svtrfX_.svtrf[scommkX_.k3+2-One];
			k3p3=svtrfX_.svtrf[scommkX_.k3+3-One];
			
			/*   CHECK FOR E-F ANY -48997: THIS IS A NULL VALUE AND SHOULD BE SKIPPED */
			if(!((srcflgX_.srcflg==2&&trgflgX_.trgflg==3)&&sw_val1==997))
			{
				if(!((srcflgX_.srcflg==2&&(trgflgX_.trgflg==4||trgflgX_.trgflg==6))&&sw_val1==997))
				{
					if(sw_val1==8)
					{
						n6jim= -79-k3p2;
						xx=semtrX_.orig[sgnX_.gn[n6jim-One]-One];
						formsaX_.formsv[sconX_.scolnk[xx-One]-One]=k3p3;
					}
					else
					{
						if(sw_val1==12)
						{
							n6jim= -79-k3p3;
							xx=semtrX_.orig[sgnX_.gn[n6jim-One]-One];
							sconX_.scon[xx-One][12-One]=k3p2;
							xpatnfX_.xpatfm[sconX_.scolnk[xx-One]-One][prctX_.js[sconX_.scolnk[xx-One]-One]-One]=0;
						}
						
						if(sw_val1==13)
						{
							n6jim= -79-k3p3;
							xx=semtrX_.orig[sgnX_.gn[n6jim-One]-One];
							ofl2X_.ofl2i[sconX_.scolnk[xx-One]-One]=k3p2;
							sconX_.scon[xx-One][3-One]=k3p2;
						}
						
						/*+         UNLIKE TRAN2,3,4; NO INPUT PHRASE ARRAY   *06/10/91*JAL* */
						if(sw_val1==43)
						{
							n6jim= -79-k3p3;
							xx=semtrX_.orig[sgnX_.gn[n6jim-One]-One];
							
							if(sconX_.scon[xx-One][1-One]>=0)
								sconX_.scon[xx-One][3-One]=k3p2;
						}
					}
				}
			}
		}
		
		if(diagsX_.longdi!=0)
		{
			fprintf(_spec_fp," SEM SW48  %6d%6d%6d%6d%6d\n",scommkX_.k3n,k3p2,k3p3,xx,formsaX_.formsv[sconX_.scolnk[xx-One]-One]);
		}
	}





	else if(swnum==54)
	{
		/*   -54 IS A SCON SETTING SWITCH
		 *       -54  NO.PAIRS  -8X   PAIR 1   PAIR 2 . . . PAIR 3
		 *             K3P1      K3P2   K3P1*2 .... */
		k3p1=svtrfX_.svtrf[scommkX_.k3+1-One];
		k3p2=svtrfX_.svtrf[scommkX_.k3+2-One];
		scommkX_.k3n=scommkX_.k3+3+(k3p1* 2);
		
		/*                   No target work if pass 1 of 2. */
		if(!(passesX_.passfl==1&&passesX_.passct==1))
		{
			izbeg=scommkX_.k3+3;
			izend=izbeg+k3p1*2-1;
			
			/*     WHICH ELEMENT ARE WE LOADING? */
			n6jim=semtrX_.orig[sgnX_.gn[-79-k3p2-One]-One];
			
			for(iz=izbeg; iz<=izend; iz+=2)
			{
				/*     ARE WE LOADING A CONSTANT OR TRANSFERING FROM ANOTHER SCON? */
				scnpos=svtrfX_.svtrf[iz-One];
				
				if(svtrfX_.svtrf[iz+1-One]< -70)
				{
					/*    LOADING THE SAME POSITION FROM ANOTHER ELEMENT */
					n7jim=semtrX_.orig[sgnX_.gn[-79-svtrfX_.svtrf[iz+1-One]-One]-One];
					
					if(scnpos<=SCONX1)
					{
						sconX_.scon[n6jim-One][scnpos-One]=sconX_.scon[n7jim-One][scnpos-One];
					}
					else
					{
						sconX_.scono[sconX_.scolnk[n6jim-One]-One][scnpos-SCONX1-One]=sconX_.scono[sconX_.scolnk[n7jim-One]-One][scnpos-SCONX1-One];
					}
					
					/*     LOADING A CONSTANT */
				}
				else if(scnpos<=SCONX1)
				{
					sconX_.scon[n6jim-One][scnpos-One]=svtrfX_.svtrf[iz+1-One];
				}
				else
				{
					sconX_.scono[sconX_.scolnk[n6jim-One]-One][scnpos-SCONX1-One]=svtrfX_.svtrf[iz+1-One];
				}
			}
		}
	}
	return;
}    /*end of function*/
