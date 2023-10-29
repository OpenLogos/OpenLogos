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
   This is called by tran 2,3,4 and not used by tran1.
   It does the same basic functionality as semsw1
*/       

/*  CHANGES: */
	/*      09/16/94 jal: null black hole counter for new verb  LOG#2128
	 *     02/01/94 jal: If 36sw is changing the address of a Relative ProNoun
	 *              that has been identified by a -67005 as representing
	 *              the antecedent in the parent clause, then when changing
	 *              the address of the antecedent, do not change the Rel PN addr.
	 *     11/23/93 AVK: If german phrasal transfer, use pat of head element.
	 *     11/19/91 JAL: IF A RELATIVE PRONOUN IS ACTED ON BY A -36 OR
	 *              -16 SWITCH THEN CHANGE THE ANTECEDENT, TOO.
	 *     10/09/91 JAL: FOR SW47, SET SCON59 = MAIN PAT # OF TARGET
	 *       03/19/90*JAL :  REMOVED SEM46() CODE WHICH NEVER WORKED ANYWAY.
	 **      CHANGE 89/07/24 LG002GBA R1956: -44 SW DEFAULT VC 106 IF
	 **              SPECIFIED VC IS NOT PHOUND IN PHRBEG
	 **      CHANGE 88/09/06 LG002GBA RSW44:  -32 KEEPS -44997 FROM 'SKIP'
	 **      CHANGE 88/08/02 LG002GBA R0GBA:  SET SCON 60 OF ELEMENT IF
	 *               LOADED FROM THE HIGH FREQ CONSTANTS (SW36)
	 **      CHANGE 87/07/08 LG002GBA RGF47:
	 *          CHG: 06/04/87 *STEVERKH*  OLD BUG FIX IN -36 13
	 *          CHG: 05/02/87 *R1691RKH*  Deactivate SCONPI for -140 in OPADR
	 *          CHG: 04/19/87 *R1685RKH*  Change T1-4 SWORK limit from 50 to
	 *          CHG: 04/01/87 *R1703RKH*  Blank Constant(-3203-36997)
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

void /*FUNCTION*/ semsw2(short swnum, short sw_val1, short *retflg) 
{
	static short
	int   answ,antscn,apos,chksc1,findct,opostp,opostr,relptr,savadr,scnrow,scnval,sctemp,setscn,subfcn,vtrpe,x,xx1;
	long int           taddress,dictype;
	char               company_code[3];
	short	scon_pos,swork_pos;
	static char        pgmnam[9]="T2SEMSW2";
	static short       m=0;
	static short       q=0;
	static short       gb=0;
	static short       hz=0;
	static short       iz=0;
	static short       jz=0;
	static short       kz=0;
	static short       k2=0;
	static short       ms=0;
	static short       n4=0;
	static short       wc=0;
	static short       xx=0;
	static short       xz=0;
	static short       x1=0;
	static short       x3=0;
	static short       yy=0;
	static short       zz=0;
	static short       adr_=0;
	static short       gbx=0;
	static short       gb1=0;
	static short       hz2=0;
	static short       iz2=0;
	static short       iz3=0;
	static short       iz4=0;
	static short       jgb=0;
	static short       jz1=0;
	static short       jz2=0;
	static short       nn4=0;
	static short       sc1=0;
	static short       tmp=0;
	static short       xx2=0;
	static short       zzc=0;
	static short       zzg=0;
	static short       zzp=0;
	static short       addr=0;
	static short       adr2=0;
	static short       gusn=0;
	static short       k3p1=0;
	static short       k3p2=0;
	static short       k3p3=0;
	static short       k3p4=0;
	static short       prm2=0;
	static short       type=0;
	static short       vtrc=0;
	static short       vtrg=0;
	static short       vtrp=0;
	static short       zznu=0;
	static short       addr1=0;
	static short       izbeg=0;
	static short       izend=0;
	static short       n6jim=0;
	static short       n7jim=0;
	static short       retsw=0;
	static short       sconc=0;
	static short       scong=0;
	static short       sconu=0;
	static short       type1=0;
	static short       type2=0;
	static short       type3=0;
	static short       vtrnu=0;
	static short       zzpnt=0;
	static short       elepos=0;
	static short       hfaddr=0;
	static short       n6jim2=0;
	static short       phrlst=0;
	static short       phrstr=0;
	static short       scnpos=0;
	static short       sconpe=0;
	static short       semsct=0;
	static short       subset=0;
	static short       supm12=0;
	static short       supset=0;
	static short       vtrpnt=0;

	*retflg=0;

	if(swnum==13)
	{
		/*        * * * START OF -13 SWITCH * * *  
		 *        FUNCTION: NOP */
	}
	
	
	
	
	
	else if(swnum==16)
	{
		/*        * * * START OF -16 SWITCH * * * */
		scommkX_.k3n=scommkX_.k3+6;
		
		/*		skip this target switch in the PARSE program */
		if(!(passesX_.passfl==1&&passesX_.passct==1))
		{
			/*   CHECK FOR E-F ANY -16997: THIS IS A NULL VALUE AND SHOULD BE SKIPPED */
			if(!((srcflgX_.srcflg==2&&(trgflgX_.trgflg==3||trgflgX_.trgflg==4||trgflgX_.trgflg==6))&&sw_val1==997))
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
					semsct=sworkX_.phrhed[zzpnt-One];
					
					if(sconX_.scon[semsct-One][0]>=0)
					{
						/*                            SET THE SCON FILL VALUES */
						if(vtrg!=0)
						{
							if(vtrg<0)
							{
								scong=sconX_.scon[sworkX_.swork[zzg-One][4-One]-One][4-One];
							}
							else
							{
								scong=vtrg;
							}
						}
						
						/*                             NUMBER */
						if(vtrnu!=0)
						{
							if(vtrnu<0)
							{
								sconu=sconX_.scon[sworkX_.swork[zznu-One][4-One]-One][5-One];
							}
							else
							{
								sconu=vtrnu;
							}
						}
						
						/*                             PERSON */
						if(vtrp!=0)
						{
							if(vtrp<0)
							{
								sconpe=sconX_.scon[sworkX_.swork[zzp-One][4-One]-One][6-One];
							}
							else
							{
								sconpe=vtrp;
							}
						}
						
						/*                             CASE */
						if(vtrc!=0)
						{
							if(vtrc<0)
							{
								sconc=sconX_.scon[sworkX_.swork[zzc-One][4-One]-One][7-One];
							}
							else
							{
								sconc=vtrc;
							}
						}
						
						/*                       IS THERE ANYTHING TO CHANGE? */
						if(!(((vtrg==0&&vtrnu==0)&&vtrpe==0)&&vtrc==0))
						{
							/*                     CHKSC1 = 1, DONT SET CASE FOR A CONSTANT OPADRI */
							chksc1=0;
							ms=sworkX_.swork[zzpnt-One][0];
							
							if(ms==2)
								chksc1=1;
							
							if(srcflgX_.srcflg==1&&(ms==3||ms==8))
								chksc1=1;
							phrstr=sworkX_.phrbeg[zzpnt-One];
							phrlst=sworkX_.phrend[zzpnt-One];
							
							for(iz=phrstr; iz<=phrlst; iz++)
							{
								sc1=sconX_.scon[opadriX_.sconpi[iz-One]-One][0];
								
								if(sc1>=0)
								{
									if(vtrg!=0)
										sconX_.scon[opadriX_.sconpi[iz-One]-One][4-One]=scong;
									
									if(vtrnu!=0)
										sconX_.scon[opadriX_.sconpi[iz-One]-One][5-One]=sconu;
									
									if(vtrp!=0)
										sconX_.scon[opadriX_.sconpi[iz-One]-One][6-One]=sconpe;
									
									if(vtrc!=0)
									{
										if(chksc1!=1||(sc1<99||sc1>120))
											sconX_.scon[opadriX_.sconpi[iz-One]-One][7-One]=sconc;
									}
									
									/*                   IF THIS IS A MULTI LOADED VC, SET ELEMENTS IN THE VC */
									if(opadriX_.opadri[iz-One]<= -100&&opadriX_.opadri[iz-One]>= -120)
									{
										if(hpdopiX_.hfdopi[iz-One]>=HFPOLO&&hpdopiX_.hfdopi[iz-One]<=HFPOHI)
										{
											adr2=hpdopiX_.hfdopi[iz-One]-HFPOL1;
											jz2=hfdoaX_.hfpoad[adr2-One][HFPADX-One];
											
											for(m=1; m<=jz2; m++)
											{
												if(sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][0]>=0)
												{
													if(vtrg!=0)
														sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][4-One]=scong;
													
													if(vtrnu!=0)
														sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][5-One]=sconu;
													
													if(vtrp!=0)
														sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][6-One]=sconpe;
													
													if(vtrc!=0)
													{
														if(chksc1!=1||(sc1<99||sc1>120))
															sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][7-One]=sconc;
													}
												}
											}
										}
									}
								}
							}
							
							/*                   IF THIS IS A RELATIVE PRONOUN THEN CHANGE G,N,P
							 *                   SC4,5,6 OF THE ANTECEDENT AS WELL.  NOT SCON7 */
							if(clsnfoX_.clrelp[clsnfoX_.clcrnt-One]==sworkX_.swork[zzpnt-One][4-One])
							{
								answ=clsnfoX_.clansw[clsnfoX_.clcrnt-One];
								
								/*                     FIND REGION OF OPADRO CONCATENATED W/ ANTECEDENT
								 *                     IS ANTCEDENT STILL A HEAD */
								if(answ==0)
								{
									/*                      WE KNOW SCONPO VALUES FOR 1ST AND LAST ADDRESS
									 *                      CONCATENATED W/ THE ANTCDNT; FIND THE LOCATIONS
									 *                      SEARCH OPADRO OF PARENT CLAUSE ONLY */
									opostr=1;
									opostp=0;
									scnval=clsnfoX_.clanbg[clsnfoX_.clcrnt-One];
									xx1=sworkoX_.phrbgo[clsoutX_.clbgno[clsnfoX_.clprnt[clsnfoX_.clcrnt-One]-One]-One];
									xx2=sworkoX_.phrndo[clsoutX_.clndno[clsnfoX_.clprnt[clsnfoX_.clcrnt-One]-One]-One];
									findct=0;
									
									for(xx=xx1; xx<=xx2; xx++)
									{
										/*                      FOUND FIRST ADDRESS, LOOK FOR LAST ADDRESS */
										if(opadroX_.sconpo[xx-One]==scnval)
										{
											findct+=1;
											
											if(findct!=1)
												goto L_9201;
											opostr=xx;
											scnval=clsnfoX_.clannd[clsnfoX_.clcrnt-One];
										}
									}
									goto L_3504;
								L_9201:
									opostp=xx;
									
									/*                     CHECK THAT ANTECEDENT IS REALY HERE */
								L_3504:;
									
									for(xx=opostr; xx<=opostp; xx++)
									{
										if(opadroX_.sconpo[xx-One]==clsnfoX_.clansc[clsnfoX_.clcrnt-One])
											goto L_3510;
									}
									
									if(diagsX_.anydi==1)
									{
										fprintf(_spec_fp,
												"\n************ ERROR:  SEMSW2 - SW16 ***************\n  ANTECEDENT NOT IN EXPECTED OPADRO RANGE %4d %4d \n DONT CHANGE ANTECEDENT AND CONTINUE.         \n***************************************************\n"
												,opostr,opostp);
									}
									goto L_3740;
								}
								else if(clsconX_.achild[sconX_.scolnk[sworkoX_.sworko[answ-One][4-One]-One]-One]==clsnfoX_.clcrnt)
								{
									opostr=sworkoX_.phrbgo[answ-One];
									opostp=sworkoX_.phrndo[answ-One];
									
									/*                   ANTECEDENT HAS BEEN CONCATENATED */
								}
								else
								{
									if(diagsX_.anydi==1)
									{
										fprintf(_spec_fp,
												"\n************ ERROR:  SEMSW2 - SW16 ***************\n PROBLEM FINDING ANTECEDENT. ANSW,CLCRNT =%4d %4d \n DONT CHANGE ANTECEDENT AND CONTINUE.         \n***************************************************\n"
												,answ,clsnfoX_.clcrnt);
									}
									goto L_3740;
								}
								
								/*                              SET EVERYTHING IN THE ANTECEDENT PHRASE */
							L_3510:;
								
								for(apos=opostr; apos<=opostp; apos++)
								{
									if(vtrg!=0)
										sconX_.scon[opadroX_.sconpo[apos-One]-One][4-One]=scong;
									
									if(vtrnu!=0)
										sconX_.scon[opadroX_.sconpo[apos-One]-One][5-One]=sconu;
									
									if(vtrpe!=0)
										sconX_.scon[opadroX_.sconpo[apos-One]-One][6-One]=sconpe;
									
									if(vtrc!=0)
									{
										if(chksc1!=1||(sc1<99||sc1>120))
											sconX_.scon[opadroX_.sconpo[apos-One]-One][7-One]=sconc;
									}
									
									/*                   IF THIS IS A MULTI LOADED VC, SET ELEMENTS IN THE VC */
									if(opadroX_.opadro[apos-One]<= -100&&opadroX_.opadro[apos-One]>= -120)
									{
										if(hpdopoX_.hfdopo[apos-One]>=HFPOLO
										 &&hpdopoX_.hfdopo[apos-One]<=HFPOHI)
										{
											adr2=hpdopoX_.hfdopo[apos-One]-HFPOL1;
											jz2=hfdoaX_.hfpoad[adr2-One][HFPADX-One];
											
											for(m=1; m<=jz2; m++)
											{
												if(sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][0]>=0)
												{
													if(vtrg!=0)
														sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][4-One]=scong;
													
													if(vtrnu!=0)
														sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][5-One]=sconu;
													
													if(vtrp!=0)
														sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][6-One]=sconpe;
													
													if(vtrc!=0)
													{
														if(chksc1!=1||(sc1<99||sc1>120))
															sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][7-One]=sconc;
													}
												}
											}
										}
									}
									
									/*                     LOOP FOR NEXT OPADR IN ANTECEDENT'S PHRASE */
								}
								
								/*                     MARK THE ANTECEDENT AS CHANGED. */
								sconX_.scon[antscn-One][14-One]=3;
							}
						}
					}
				}
			}
		L_3740:
			
			if(diagsX_.deepdi==1)
			{
				fprintf(_spec_fp," SEM SW16  %6d%6d%6d%6d",scommkX_.k3n,vtrpnt,phrstr,phrlst);
				
				for(xx=4; xx<=7; xx++)
				{
					fprintf(_spec_fp,"%6d",sconX_.scon[semsct-One][xx-One]);
				}
				fprintf(_spec_fp,"\n");
			}
		}
	}
	
	
	
	
	
	else if(swnum==32)
	{
		/*        * * * START OF -32 SWITCH * * * */
		/*        -32 SWITCH WILL SET FLAGS FOR -36, -44  OR -47 SWITCH
		 *            IMMEDIATELY FOLLOWING IT. */
		/*        PURPOSE:  TO ACCESS A CERTAIN RANGE (1000,2000,..)
		 *                   CONSTANT DICTIONARY */
		prm2=svtrfX_.svtrf[scommkX_.k3+2-One];
		scommkX_.k3n=scommkX_.k3+3;
		
		/*		skip this target switch in the PARSE program */
		if(!(passesX_.passfl==1&&passesX_.passct==1))
		{
				/*C       -99 FUNCTION SETS AD32 TO 1000'S PLACE OF
				 *        HI-FREQ CONSTANT TO BE USED BY -36 OR -44 SWITCH */
			if(sw_val1== -99)
			{
				flag32X_.ad32=prm2;
			}
			
			/*        FUNCTION 1-6 FOR -47 SWITCH */
			else if(!(sw_val1<1||sw_val1>6))
			{
				flag32X_.prm32=sw_val1;
				flag32X_.ad32=prm2;
			}
		}
	}
	
	
	
	
	
	
	else if(swnum==36)
	{
		// *        * * * START OF -36 SWITCH * * * */
		//        SW36 WILL NOT OVERRIDE A LOCKED SCON
		k3p1=svtrfX_.svtrf[scommkX_.k3+1-One];
		k3p2=svtrfX_.svtrf[scommkX_.k3+2-One];
		prm2=svtrfX_.svtrf[scommkX_.k3+2-One];
		scommkX_.k3n=scommkX_.k3+3;
		
		/*		skip this target switch in the PARSE program */
		if(!(passesX_.passfl==1&&passesX_.passct==1))
		{
			/*   CHECK FOR E-F ANY -36997: THIS IS A NULL VALUE AND SHOULD BE SKIPPED
			 *+ Blank Constant(-3203-36997)                 RKH  04/01/87   R1703 */
			if(!(((srcflgX_.srcflg==2&&(trgflgX_.trgflg==3||trgflgX_.trgflg==4||trgflgX_.trgflg==6))&&sw_val1==997)&&flag32X_.ad32==0))
			{
				/*        ;PRM2 IS A REL. PTR. */
				n6jim= -79-prm2;
				xx=semtrX_.orig[sgnX_.gn[n6jim-One]-One];
				x1=xx;
				xz=sgnX_.gn[n6jim-One];
				
				if(flag32X_.ad32== -1)
				{
					/*        FUNCTION 9 - NULLIFY ADDRESSES OF PHRASE    PR1235 */
					if(sw_val1==9)
					{
						phrstr=sworkX_.phrbeg[xx-One];
						phrlst=sworkX_.phrend[xx-One];
						
						for(gb=phrstr; gb<=phrlst; gb++)
						{
							opadriX_.opadri[gb-One]= -140;
							hpdopiX_.hfdopi[gb-One]=0;
							
							if(opadriX_.sconpi[gb-One]!=sworkX_.phrhed[xx-One])
								opadriX_.sconpi[gb-One]=1;
						}
						goto L_4160;
					}
					
							/*        FUNCTION 11-14 (GERMAN ONLY)
							 *        FOR DECLENTION OF NOUNS BY NUMBER */
					else if(sw_val1>=11&&sw_val1<=14)
					{
						if(srcflgX_.srcflg==1)
						{
							n6jim= -79-prm2;
							xx=semtrX_.orig[sgnX_.gn[n6jim-One]-One];
									\
									sw36cX_.sw36no=xx;
							iz2=sworkX_.phrhed[sw36cX_.sw36no-One];
							iz=sconX_.scon[iz2-One][0];
							
							if(iz>0)
							{
								if(iz<=7)
								{
									if(!(iz>=2&&iz<=4))
									{
										/*            set source case in SC24 and propigate */
										scnrow=sworkX_.swork[xx-One][4-One];
										
										if(sconX_.scon[scnrow-One][0]>=0)
										{
											sconX_.scono[sconX_.scolnk[scnrow-One]-One][24-SCONX1-One]=sw_val1-10;
											
											/*                   propigate the changes
											 *                    apply only to unset (scon=0) and unlocked scons
											 *                    concatenated in the input OPADR
											 *                     i.e. subfcn 20 of SW48 */
											subfcn=20;
											relptr=im81X_.im81-xx;
											setscn=24;
											txsw48(subfcn,relptr,setscn);
										}
										iz=nounsX_.gernum[k3p1-10-One][formsaX_.formsv[sconX_.scolnk[sworkX_.swork[xx-One][4-One]-One]-One]-One];
										
										if(!(iz<1||iz>99))
										{
											phrstr=sworkX_.phrbeg[xx-One];
											phrlst=sworkX_.phrend[xx-One];
											
											for(zz=phrstr; zz<=phrlst; zz++)
											{
												sc1=sconX_.scon[opadriX_.sconpi[zz-One]-One][0];
												
												if(sc1>=0)
												{
													if(sc1==1)
														iz=nounsX_.gernum[k3p1-10-One][formsaX_.formsv[sconX_.scolnk[opadriX_.sconpi[zz-One]-One]-One]-One];
													sconX_.scon[opadriX_.sconpi[zz-One]-One][5-One]=iz;
													
													/*                FOR MULTI LOADED VC, SET ALL COMPONENT ELEMENTS */
													if( opadriX_.opadri[zz-One]<= -100
													  &&opadriX_.opadri[zz-One]>= -120)
													{
														if(hpdopiX_.hfdopi[zz-One]>=HFPOLO&&hpdopiX_.hfdopi[zz-One]<=HFPOHI)
														{
															iz2=iz;
															adr2=hpdopiX_.hfdopi[zz-One]-HFPOL1;
															jz2=hfdoaX_.hfpoad[adr2-One][HFPADX-One];
															
															for(m=1; m<=jz2; m++)
															{
																sc1=sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][0];
																
																if(sc1>=0)
																{
																	if(sc1==1)
																	{
																		sctemp=sconX_.scolnk[hfdoaX_.sconhf[adr2-One][m-One]-One];
																		
																		if(sctemp>0&&sctemp<=elemctX_.elemct)
																			iz2=nounsX_.gernum[k3p1-10-One][formsaX_.formsv[sctemp-One]-One];
																	}
																	sconX_.scon[hfdoaX_.sconhf[adr2-One][m-One]-One][5-One]=iz2;
																}
															}
														}
													}
												}
											}
										}
									}
								}
							}
						}
						goto L_4160;
					}
				}

				addr= -sw_val1;
				if(flag32X_.ad32!= -1)
				{
					addr=sw_val1+(1000* flag32X_.ad32);
					flag32X_.ad32= -1;
				}
				
				while(sconX_.scon[sworkX_.phrhed[xx-One]-One][0]>=1)
				{
					/*     IF (SCON(14,PHRHED(XX)) .EQ. 1) GOTO 4160 */
					/*        FOR TRAN3
					 *        CHECK TO SEE IF THE ELEMENT BEING TRANFORMED
					 *        IS A WORD CLASS 7.
					 *        (THIS INDICATES THE PRESENCE OF A CHAIN)
					 *        IF IT IS, CHECK THE 'WC7' VARIABLE.
					 *        (THIS VARIABLE WILL POINT TO THE BEGINNING OF THE CHAIN)
					 *        AND TRANSFORM THIS ELEMENT INSTEAD. */
					if(tranidX_.tranid==3)
					{
						if(sworkX_.swork[xx-One][0]==7)
						{
							if(prevbX_.wc7!=0)
							{
								xx=prevbX_.wc7;
								prevbX_.wc7=0;
								
								/*        IF THIS ELEMENT HAS ALREADY BEEN LOADED,
								 *        CHANGE THE OUTGOING OPADRI */
								if(prevbX_.load!=0)
									goto L_9202;
							}
						}
					}
					
					/*              NOW READY TO CHANGE ADDRESS & get target codes.
					 *+                   If this is a rel Pronoun, then only  2/1/94 jal
					 *                    change address of antecedent in parent clause */
					if(clsnfoX_.clrelp[clsnfoX_.clcrnt-One]!=sworkX_.swork[xx-One][4-One])
					{
						phrstr=sworkX_.phrbeg[xx-One];
						phrlst=sworkX_.phrend[xx-One];
						
						for(kz=phrstr; kz<=phrlst; kz++)
						{
							if(opadriX_.sconpi[kz-One]==sworkX_.phrhed[xx-One])
							{
								savadr=opadriX_.opadri[kz-One];
								opadriX_.opadri[kz-One]=addr;
								scon_pos=sworkX_.phrhed[xx-One];
								swork_pos = sconX_.scon[scon_pos-One][10-One];
								memcpy(cmpsmcX_.cmpcod[swork_pos-One][prctX_.js[swork_pos-One]-One],
									   curr_sp5rule_company_code,sizeof(cmpsmcX_.cmpcod[0][0]));

								sconX_.scon[opadriX_.sconpi[kz-One]-One][14-One]=1;
								
								if(addr<0&&addr> -1000)
								{
									taddress= -addr;
									dictype=2;
									memcpy(company_code,LOGCC,3);
								}
								else
								{
									taddress=addr;
									dictype=3;
									memcpy(company_code,curr_sp5rule_company_code,3);
								}
								errvrsX_.err=TARG_CODES(&trgflgX_.trgflg,&dictype,&taddress,company_code,(short*)& trgcdsX_.tcwdad,diagsX_.longdi,_spec_fp);
								if(errvrsX_.err==0)
								{
									sconX_.scono[sconX_.scolnk[opadriX_.sconpi[kz-One]-One]-One][59-SCONX1-One]=trgcdsX_.tcpatm[0];
									sconX_.scono[sconX_.scolnk[opadriX_.sconpi[kz-One]-One]-One][60-SCONX1-One]= -addr;
								}
								else
								{
									errlog(pgmnam,3899,taddress,13);
									opadriX_.opadri[kz-One]=savadr;
								}
								break;    //out of for loop
							}
						}
					}
					
					/*                     IF THE ELEMENT BEING CHANGED IS A REL PRONOUN
					 *                     THEN CHANGE ADDR OF THE ANTECEDENT AS WELL. */
					if(clsnfoX_.clrelp[clsnfoX_.clcrnt-One]==sworkX_.swork[xx-One][4-One])
					{
						antscn=clsnfoX_.clansc[clsnfoX_.clcrnt-One];
						
						/*                     FIND THE ANTECEDENT POSITION IN THE OUTGOING OPADR */
						opostr=sworkoX_.phrbgo[clsoutX_.clbgno[clsnfoX_.clprnt[clsnfoX_.clcrnt-One]-One]-One];
						opostp=sworkoX_.phrndo[clsoutX_.clndno[clsnfoX_.clprnt[clsnfoX_.clcrnt-One]-One]-One];
						
						for(apos=opostr; apos<=opostp; apos++)
						{
							if(opadroX_.sconpo[apos-One]==antscn)
							{
								savadr=opadroX_.opadro[apos-One];
								opadroX_.opadro[apos-One]=addr;
								scon_pos=sworkX_.phrhed[antscn-One];
								swork_pos = sconX_.scon[scon_pos-One][10-One];
								memcpy(cmpsmcX_.cmpcod[swork_pos-One][prctX_.js[swork_pos-One]-One],
									   curr_sp5rule_company_code,sizeof(cmpsmcX_.cmpcod[0][0]));
								sconX_.scon[antscn-One][14-One]=3;
								
								if(addr<0&&addr> -1000)
								{
									taddress= -addr;
									dictype=2;
									memcpy(company_code,LOGCC,3);
								}
								else
								{
									taddress=addr;
									dictype=3;
									memcpy(company_code,curr_sp5rule_company_code,3);
								}
								errvrsX_.err=TARG_CODES(& trgflgX_.trgflg,& dictype,& taddress,company_code,(short*)& trgcdsX_.tcwdad,diagsX_.longdi,_spec_fp);								
								if(errvrsX_.err==0)
								{
									sconX_.scono[sconX_.scolnk[antscn-One]-One][59-SCONX1-One]=trgcdsX_.tcpatm[0];
									sconX_.scono[sconX_.scolnk[antscn-One]-One][60-SCONX1-One]= -addr;
									
									/*                       REINIT ALT WC FLAG OF ANTECEDENT */
									sconX_.scon[antscn-One][7]=0;
								}
								else
								{
									errlog(pgmnam,4003,taddress,13);
									opadroX_.opadro[apos-One]=savadr;
								}
								break;    // out of for loop
							}
						}
					}
					
					/*   IF THIS ELEMENT WAS BROUGHT IN BY THE SAV36 FUNCTION -
					 *    CHANGE THE OUTGOING OPADR */
					if(tranidX_.tranid!=4)
					{
						/*              dont change address of a Rel ProNoun    2/1/94 jal */
						if(clsnfoX_.clrelp[clsnfoX_.clcrnt-One]!=sworkX_.swork[xx-One][4-One])
						{
							if(xx==sav36sX_.sav36s[0]&&sav36sX_.sav36s[11-One]>0)
							{
								opadroX_.opadro[sav36sX_.sav36s[11-One]-One]=addr;
								scon_pos=sconX_.scolnk[opadroX_.sconpo[sav36sX_.sav36s[11-One]-One]-One];
								swork_pos = sconX_.scon[scon_pos-One][10-One];
								memcpy(cmpsmcX_.cmpcod[swork_pos-One][prctX_.js[swork_pos-One]-One],
									curr_sp5rule_company_code,sizeof(cmpsmcX_.cmpcod[0][0]));
								
								if(addr<0&&addr> -1000)
									sconX_.scono[sconX_.scolnk[opadroX_.sconpo[sav36sX_.sav36s[11-One]-One]-One]-One][60-SCONX1-One]= -addr;
							}
						}
					}
					
					/*   IF THIS IS A PREP, LOOP THROUGH THE SEMWRKS LOOKING FOR AN
					 *   IDENTICAL ONE, AND CHANGE IT.
					 *   CHECK THE NSWORKS (SEMWRKS MAY HAVE BEEN CHANGED) */
					/*   IS THIS A PREP?  CHECK THE SEMWRK FOR WC 13 OR WC 19 SUPERFORM 23 */
					wc=semfrmX_.semwrk[xz-One][0];
					
					/*          5/28/85  UNTIL THIS LOGIC CAN BE REFINED, MAKE
					 *                   IT GERMAN SOURCE ONLY */
					if(srcflgX_.srcflg!=1)
						break;
					
					if(wc!=13)
					{
						if(wc!=19)
							break;
						k2=3;
						gusn=23;
						formod(3,gusn,k2,xz,& retsw);
						
						if(retsw==2)
							break;
					}
					xx2=xz+1;
					
					for(m=xx2; m<=semargX_.nwrks; m++)
					{
						x3=semtrX_.orig[m-One];
						wc=sworkX_.swork[x3-One][0];
						subset=sworkX_.swork[x3-One][2-One];
						type=sconX_.scon[sworkX_.phrhed[x3-One]-One][13-One];
						
						/*+ FOLLOWING CODE IS MOOT. FIX IF NEEDED         08/19/86  *R1561DSD */
						if(!(wc!=19||wc!=20))
						{
							if(type>=6&&type<=10)
								break;
							
							if(subset>=885&&subset<=887)
								break;
						}
						
						/*- ABOVE CODE NEVER DONE. FIX IF NEEDED          08/19/86  *R1561DSD */
						if(wc==sworkX_.swork[x1-One][0])
						{
							if(subset==sworkX_.swork[x1-One][2-One])
							{
								if(!(sworkX_.swork[x3-1-One][2-One]!=888&&sconX_.scon[sworkX_.phrhed[x3-1-One]-One][13-One]!=1))
									goto L_9204;
							}
						}
					}
					break;
				L_9204:
					xx=x3;
					xz=m;
				}
				goto L_4160;
			L_9202:
				xx=prevbX_.load;
				
				if(sconX_.scon[sworkoX_.phrhdo[xx-One]-One][0]>=1)
				{
					phrstr=sworkoX_.phrbgo[xx-One];
					phrlst=sworkoX_.phrndo[xx-One];
					
					if(phrlst== -1)
						phrlst=opadroX_.opo;
					prevbX_.load=0;
					
					for(iz=phrstr; iz<=phrlst; iz++)
					{
						if(opadroX_.sconpo[iz-One]==sworkoX_.phrhdo[xx-One])
						{
							savadr=opadroX_.opadro[iz-One];
							opadroX_.opadro[iz-One]=addr;
							scon_pos=opadroX_.sconpo[iz-One];
							swork_pos = sconX_.scon[scon_pos-One][10-One];
							memcpy(cmpsmcX_.cmpcod[swork_pos-One][prctX_.js[swork_pos-One]-One],
								curr_sp5rule_company_code,sizeof(cmpsmcX_.cmpcod[0][0]));
							sconX_.scon[opadroX_.sconpo[iz-One]-One][14-One]=1;
							
							if(addr<0&&addr> -1000)
							{
								sconX_.scono[sconX_.scolnk[opadroX_.sconpo[iz-One]-One]-One][60-SCONX1-One]= -addr;
								taddress= -addr;
								dictype=2;
								memcpy(company_code,LOGCC,3);
							}
							else
							{
								taddress=addr;
								dictype=3;
								memcpy(company_code,curr_sp5rule_company_code,3);
							}
							errvrsX_.err=TARG_CODES(&trgflgX_.trgflg,&dictype,&taddress,company_code,(short*)& trgcdsX_.tcwdad,diagsX_.longdi,_spec_fp);
							
							if(errvrsX_.err==0)
							{
								sconX_.scono[sconX_.scolnk[opadroX_.sconpo[iz-One]-One]-One][59-SCONX1-One]=trgcdsX_.tcpatm[0];
							}
							else
							{
								errlog(pgmnam,3699,taddress,13);
								opadroX_.opadro[iz-One]=savadr;
							}
						}
					}
				}
			}
		L_4160:
			
			if(diagsX_.deepdi==1)
			{
				fprintf(_spec_fp," SEM SW36\n   %5d%5d%5d%5d%5d\n",n6jim,xx,phrstr,phrlst,sw_val1);
			}
		}
	}
	
	
	
	
	
	else if(swnum==44)
	{
		/*        * * * START OF -44 SWITCH * * * */
		scommkX_.k3n=scommkX_.k3+5;
		
		/*		skip this target switch in the PARSE program */
		if(!(passesX_.passfl==1&&passesX_.passct==1))
		{
			k3p1=svtrfX_.svtrf[scommkX_.k3+1-One];
			k3p2=svtrfX_.svtrf[scommkX_.k3+2-One];
			k3p3=svtrfX_.svtrf[scommkX_.k3+3-One];
			k3p4=svtrfX_.svtrf[scommkX_.k3+4-One];
			n6jim=semtrX_.orig[sgnX_.gn[-79-k3p1-One]-One];
			
			//   HAS THIS SWITCH BEEN NULLED FOR ENGLISH-FRENCH?
			if(!(((srcflgX_.srcflg==2&&trgflgX_.trgflg!=1)&&k3p3==997)&&flag32X_.ad32==0))
			{
				addr= -k3p3;
				
				if(flag32X_.ad32!= -1)
				{
					addr=k3p3+(1000* flag32X_.ad32);
					flag32X_.ad32= -1;
				}
				
				/*        CHECK FOR A CHAIN HERE ALSO (SEE SW36) */
				if(tranidX_.tranid==3)
				{
					if(sworkX_.swork[n6jim-One][0]==7)
					{
						if(prevbX_.wc7!=0)
						{
							n6jim=prevbX_.wc7;
							prevbX_.wc7=0;
							
							/*        IF THIS ELEMENT HAS BEEN LOADED, CHANGE THE OUTGOING OPADRI. */
							if(prevbX_.load!=0)
							{
								n6jim=prevbX_.load;
								phrstr=sworkoX_.phrbgo[n6jim-One];
								phrlst=sworkoX_.phrndo[n6jim-One];
								
								if(phrlst== -1)
									phrlst=opadroX_.opo;
								
								for(iz=phrstr; iz<=phrlst; iz++)
								{
									if(opadroX_.opadro[iz-One]== -k3p2)
										goto L_4620;
								}
								
								/*+                                                        *R1956*GBA */
								if(k3p3==140)
									goto L_5360;
								
								for(iz=phrstr; iz<=phrlst; iz++)
								{
									if(opadroX_.opadro[iz-One]== -106)
										goto L_4620;									
								}
								goto L_5360;
								
								/*        THIS CAPABILITY (CHANGING THE OUTGOING OPADRI)
								 *        ONLY FOR CONSTANT-VC. */
							L_4620:
								
								if(k3p3>120)
								{
									hpdopoX_.hfdopo[iz-One]=addr;
 									memcpy(opadroX_.company_code[iz-One],
										   curr_sp5rule_company_code,sizeof(opadroX_.company_code[0]));

									if(k3p4>0)
										sconX_.scon[opadroX_.sconpo[iz-One]-One][7-One]=k3p4;
									
									if(k3p4== -13)
										sconX_.scon[opadroX_.sconpo[iz-One]-One][0]= -sconX_.scon[opadroX_.sconpo[iz-One]-One][0];
								}
								goto L_5360;
							}
						}
					}
				}
				phrstr=sworkX_.phrbeg[n6jim-One];
				phrlst=sworkX_.phrend[n6jim-One];
				
				/*        LOOK FOR VC K3P2 IN THE PHRBEG K3P1 */
				for(iz=phrstr; iz<=phrlst; iz++)
				{
					if(opadriX_.opadri[iz-One]== -k3p2)
						goto L_4680;
				}
				
				if(k3p3==140)
					goto L_5360;
				
				for(iz=phrstr; iz<=phrlst; iz++)
				{
					if(opadriX_.opadri[iz-One]== -106)
						goto L_4680;
				}
				goto L_5360;

			L_4680:			
				if(k3p3>=0)
				{
					/*   IF ADDR HAS BEEN SET BY A PRECEDING -32 SWITCH
					 *   THIS IS A CONSTANT FUNCTION */
					if(addr<1000)
					{
						if(k3p3>=100&&k3p3<=120)
						{
							/*        K3P2 AND K3P3 ARE BOTH VCS.
							 *        IF VC K3P3 IS FOUND IN PHRBEG K3P1
							 *        ADD ELEMENTS FROM VC K3P3 TO VC K3P2 */
							for(iz2=phrstr; iz2<=phrlst; iz2++)
							{
								if(opadriX_.opadri[iz2-One]== -k3p3)
									goto L_4860;
							}
							goto L_5360;
						L_4860:
							hz=hpdopiX_.hfdopi[iz-One];
							hz2=hpdopiX_.hfdopi[iz2-One];
							
							if(hz2==0)
								goto L_5360;
							
							if(hz >= HFPOLO && hz <= HFPOHI)
							{
								/*        VC K3P2 HAS MORE THAN ONE ELEMENT.
								 *        ADD ELEMENT(S) FROM VC K3P3 INTO ITS HFPOAD */
								adr_=hz-HFPOL1;
								jz=hfdoaX_.hfpoad[adr_-One][HFPADX-One];
								
								if(!(hz2>=HFPOLO&&hz2<=HFPOHI))
									goto L_4920;
							}
							else if(hz==0)
							{
								/*        VC K3P2 IS EMPTY.  LOAD ELEMENTS FROM VC K3P3 */
								hpdopiX_.hfdopi[iz-One]=hz2;
								opadriX_.sconpi[iz-One]=opadriX_.sconpi[iz2-One];
								hpdopiX_.hfdopi[iz2-One]=0;
								goto L_5360;
							}
							else
							{
								/*        VC K3P2 HAS ONE ELEMENT.
								 *        PLACE IN NEW HFPOAD WITH ELEMENT(S) FROM VC K3P3 */
								jz=1;
								hfdoaX_.adct+=1;
								
								if(diagsX_.longdi==1)
								{
									fprintf(_spec_fp,"    NOW %2d HFDOPI VC\\\"S (%2ld-%2ld)\n",hfdoaX_.adct,HFPOLO,HFPOHI);
								}
								
								/*     EXPAND HFPOAD, SCONHF INDEX FROM 5         05/08/86  *B0406DSD */
								if(hfdoaX_.adct>HFPADY)
								{
									if(diagsX_.longdi==1)
									{
										fprintf(_spec_fp," IN SMSW44 - HFDOPI %2ld TO %2ld ALREADY TAKEN \n",HFPOLO,HFPOHI);
									}
									errlog(pgmnam,4880,502,0);
									return;
								}
								else
								{
									adr_=hfdoaX_.adct;
									hfdoaX_.hfpoad[adr_-One][jz-One]=hz;
									hfdoaX_.sconhf[adr_-One][jz-One]=opadriX_.sconpi[iz-One];
									
									if(!(hz2>=HFPOLO&&hz2<=HFPOHI))
										goto L_4920;
								}
							}
							
							/*        VC K3P3 HAS MORE THAN ONE ELEMENT TO ADD TO VC K3P2 */
							adr2=hz2-HFPOL1;
							jz1=jz+1;
							jz2=hfdoaX_.hfpoad[adr2-One][HFPADX-One];
							jz+=jz2;
							
							if(jz>=HFPADX)
							{
								if(diagsX_.longdi==1)
								{
									fprintf(_spec_fp," IN SMSW44 - HFPOAD BEING OVERLOADED\n");
								}
								errlog(pgmnam,4940,0,0);
							}
							else
							{
								for(iz3=jz1; iz3<=jz; iz3++)
								{
									for(iz4=1; iz4<=jz2; iz4++)
									{
										hfdoaX_.hfpoad[adr_-One][iz3-One]=hfdoaX_.hfpoad[adr2-One][iz4-One];
										hfdoaX_.sconhf[adr_-One][iz3-One]=hfdoaX_.sconhf[adr2-One][iz4-One];
									}
								}
							}
							goto L_4980;
							
							/*        VC K3P3 HAS ONLY ONE ELEMENT TO ADD TO VC K3P2 */
						L_4920:
							jz+=1;
							
							if(jz>=HFPADX)
							{
								if(diagsX_.longdi==1)
								{
									fprintf(_spec_fp," IN SMSW44 - HFPOAD BEING OVERLOADED\n");
								}
								errlog(pgmnam,4925,0,0);
							}
							else
							{
								hfdoaX_.hfpoad[adr_-One][jz-One]=hz2;
								hfdoaX_.sconhf[adr_-One][jz-One]=opadriX_.sconpi[iz2-One];
							}
							
							/*        SAVE HFPOAD ADDRESSES AND ZERO OUT HFDOPI OF VC K3P3 */
						L_4980:
							hfdoaX_.hfpoad[adr_-One][HFPADX-One]=jz;
							hpdopiX_.hfdopi[iz-One]=adr_+HFPOL1;
							hpdopiX_.hfdopi[iz2-One]=0;
							goto L_5360;
						}
					}
					
					/*        K3P3 IS IN CONSTANT RANGE - LOAD HFDOPI WITH CONSTANT */
					if(k3p4== -97&&hpdopiX_.hfdopi[iz-One]!=0)
					{
						/*        SET FLAG TO ADD CONSTANT TO VC INDEXED BY K3P3.
						 *        CREATE A HFPOAD OR ADD TO THE EXISTING ONE. */
						tmp=1;
					}
					else
					{
						if(k3p3>0){
							hpdopiX_.hfdopi[iz-One]=addr;
							memcpy(opadroX_.company_code[iz-One],curr_sp5rule_company_code,sizeof(opadroX_.company_code[0]));
						}
						if(k3p4== -97)
							goto L_5360;
						goto L_4800;
					}
				}
				
				/*        K3P3 POINTS TO A PHRBEG.  ADD ELEMENTS OF PHRBEG TO HFPOAD */
				if(hpdopiX_.hfdopi[iz-One]<HFPOLO
				 ||hpdopiX_.hfdopi[iz-One]>HFPOHI)
				{
					/*        ENTRY HERE MEANS LOAD A NEW HFPOAD */
					jz=1;
					hfdoaX_.adct+=1;
					
					if(diagsX_.longdi==1)
					{
						fprintf(_spec_fp,"    NOW %2d HFDOPI VC\\\"S (%2ld-%2ld)\n",hfdoaX_.adct,HFPOLO,HFPOHI);
					}
					
					if(hfdoaX_.adct>HFPADY)
					{
						if(diagsX_.longdi==1)
						{
							fprintf(_spec_fp," IN SMSW44 - HFDOPI %2ld TO %2ld ALREADY TAKEN \n",HFPOLO,HFPOHI);
						}
						errlog(pgmnam,4720,502,0);
						return;
					}
					else
					{
						adr_=hfdoaX_.adct;
						
						/*        IF K3P4 = -97,
						 *        THE SINGLE ELEMENT NOW HELD BY THE HFDOPI
						 *        OF THE VC INDEXED BY K3P2 MUST BE INSERTED
						 *        IN THE FIRST POSITION OF HFPOAD. */
						if(k3p4== -97)
						{
							if(hpdopiX_.hfdopi[iz-One]!=0)
							{
								hfdoaX_.hfpoad[adr_-One][jz-One]=hpdopiX_.hfdopi[iz-One];
								hfdoaX_.sconhf[adr_-One][jz-One]=opadriX_.sconpi[iz-One];
								jz+=1;
							}
						}
					}
				}
				else
				{
					/*        FALL THROUGH TO HERE MEANS ADD TO EXISTING HFPOAD */
					jz=1;
					adr_=hpdopiX_.hfdopi[iz-One]-HFPOL1;
					
					/*     CHANGE HFPOAD, SCONHF INDEX FROM (100,5)   05/08/86  *B0406DSD */
					if(k3p4== -97)
						jz=hfdoaX_.hfpoad[adr_-One][HFPADX-One]+1;
				}
				
				if(tmp==1)
				{
					tmp=0;
					hfdoaX_.hfpoad[adr_-One][jz-One]=addr;
					memcpy(opadroX_.company_code[adr_-One],curr_sp5rule_company_code,sizeof(opadroX_.company_code[0]));
					hfdoaX_.sconhf[adr_-One][jz-One]=opadriX_.sconpi[iz-One];
					jz+=1;
				}
				else
				{
					n6jim2=semtrX_.orig[sgnX_.gn[-79-k3p3-One]-One];
					phrstr=sworkX_.phrbeg[n6jim2-One];
					phrlst=sworkX_.phrend[n6jim2-One];
					iz2=phrlst-phrstr+1;
					
					if(iz2==1&&k3p4!= -97)
					{
						/*        PHRBEG K3P3 HOLDS ONLY ONE ELEMENT.  LOAD INTO HFDOPI */
						hpdopiX_.hfdopi[iz-One]=opadriX_.opadri[phrstr-One];
						opadriX_.sconpi[iz-One]=opadriX_.sconpi[phrstr-One];
						goto L_4800;
					}
					else
					{
						/*        PHRBEG K3P3 HOLDS MORE THAN ONE ELEMENT.
						 *        MOVE OPADRI ADDRESSES TO HFPOAD AND
						 *        SCON POSITIONS TO SCONHF */
						for(iz2=phrstr; iz2<=phrlst; iz2++)
						{
							hfdoaX_.hfpoad[adr_-One][jz-One]=opadriX_.opadri[iz2-One];
							
							if(hpdopiX_.hfdopi[iz2-One]!=0)
								hfdoaX_.hfpoad[adr_-One][jz-One]=hpdopiX_.hfdopi[iz2-One];
							
							/*        CHECK TO SEE IF THE ELEMENT CONTAINS A CONTATENATED PHRBEG. */
							if(hpdopiX_.hfdopi[iz2-One]<HFPOLO||hpdopiX_.hfdopi[iz2-One]>HFPOHI)
							{
								hfdoaX_.sconhf[adr_-One][jz-One]=opadriX_.sconpi[iz2-One];
								jz+=1;
								
								if(jz>=HFPADX)
								{
									if(diagsX_.longdi==1)
									{
										fprintf(_spec_fp," IN SMSW44 - HFPOAD BEING OVERLOADED\n");
									}
									errlog(pgmnam,4744,0,0);
								}
							}
							else
							{
								adr2=hpdopiX_.hfdopi[iz2-One]-HFPOL1;
								jz2=hfdoaX_.hfpoad[adr2-One][HFPADX-One];
								
								for(m=1; m<=jz2; m++)
								{
									hfdoaX_.hfpoad[adr_-One][jz-One]=hfdoaX_.hfpoad[adr2-One][m-One];
									hfdoaX_.sconhf[adr_-One][jz-One]=hfdoaX_.sconhf[adr2-One][m-One];
									jz+=1;
									
									if(jz>=HFPADX)
										goto L_9206;
								}
								goto L_9205;
							L_9206:
								
								if(diagsX_.longdi==1)
								{
									fprintf(_spec_fp," IN SMSW44 - HFPOAD BEING OVERLOADED\n");
								}
								errlog(pgmnam,4750,0,0);
							}
						L_9205:;
						}
					}
				}
				hpdopiX_.hfdopi[iz-One]=HFPOL1+adr_;
				hfdoaX_.hfpoad[adr_-One][HFPADX-One]=jz-1;

			L_4800:

				if(k3p4!= -97)
				{
					if(k3p4>0)
						sconX_.scon[opadriX_.sconpi[iz-One]-One][7-One]=k3p4;
					
					if(k3p4== -13)
					{
						if(sconX_.scon[opadriX_.sconpi[iz-One]-One][1-One]>=0)
							sconX_.scon[opadriX_.sconpi[iz-One]-One][1-One]= -sconX_.scon[opadriX_.sconpi[iz-One]-One][0];
					}
				}
			}
		L_5360:
			
			if(diagsX_.deepdi!=0)
			{
				fprintf(_spec_fp," SEM SW44 %4d%4d%4d%4d%4d%4d%4d%4d%4d%4d\n",n6jim,phrstr,phrlst,k3p1,k3p2,k3p3,k3p4,iz,adr_,hfdoaX_.adct);
				fprintf(_spec_fp," %4d%4d\n",hpdopiX_.hfdopi[iz-One],opadriX_.opadri[iz-One]);
				
				if(adr_>0)
				{
					fprintf(_spec_fp," HFPOAD = \n ");
					
					for(q=1; q<=HFPADX; q++)
					{
						fprintf(_spec_fp,"%6d",hfdoaX_.hfpoad[adr_-One][q-One]);
					}
					fprintf(_spec_fp,"\n");
				}
			}
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
			 *        NEW SEARCH.  IT ALSO ZEROS OUT SCON(14) IN CASE OF
			 *        A NEW MATCH. */
			srch46X_.srch46=1;
		}
		else if(sw_val1!=2)
		{
			n6jim= -79-sw_val1;
			yy=semtrX_.orig[sgnX_.gn[n6jim-One]-One];
			
			/*        FOR TRAN SWITCHES, IN CASE SW46 INFO SHOULD BE
			 *        COMMUNICATED TO NSWRK */
			n4=sworkX_.swork[yy-One][4-One];
			nn4=sworkoX_.sworko[sworkoX_.phcto-One][4-One];
			
			if(k3p2!=0)
			{
				if(k3p2>0)
				{
					sworkX_.swork[yy-One][0]=k3p2;
				}
				else
				{
					xx=semtrX_.orig[sgnX_.gn[-79-k3p2-One]-One];
					sworkX_.swork[yy-One][0]=sworkX_.swork[xx-One][0];
				}
				
				/*        IN CASE SWORK ALREADY CREATED */
				if(n4==nn4&&sw26nX_.sw26n<=0)
					sworkoX_.sworko[sworkoX_.phcto-One][0]=sworkX_.swork[yy-One][0];
			}
			
			if(k3p3!=0)
			{
				if(k3p3>0)
				{
					/*        TO MAKE SURE SUBSET, SET, AND SUPERSET ARE STILL AVAILABLE */
					type1=sworkX_.swork[yy-One][2-One];
					type2=sconX_.scon[sworkX_.phrhed[yy-One]-One][11-One];
					type3=sconX_.scon[sworkX_.phrhed[yy-One]-One][13-One];
					
					if(k3p3<17)
					{
						if(type1<17)
							sworkX_.swork[yy-One][2-One]=k3p3;
						
						if(type2<17)
							sconX_.scon[sworkX_.phrhed[yy-One]-One][11-One]=k3p3;
						sconX_.scon[sworkX_.phrhed[yy-One]-One][13-One]=k3p3;
					}
					else if(k3p3>99)
					{
						sworkX_.swork[yy-One][2-One]=k3p3;
						
						if(type2>99)
							sconX_.scon[sworkX_.phrhed[yy-One]-One][11-One]=k3p3;
						
						if(type3>99)
							sconX_.scon[sworkX_.phrhed[yy-One]-One][13-One]=k3p3;
					}
					else
					{
						if(type1>=17&&type1<=99)
							sworkX_.swork[yy-One][2-One]=k3p3;
						sconX_.scon[sworkX_.phrhed[yy-One]-One][11-One]=k3p3;
						
						if(type3>=17&&type3<=99)
							sconX_.scon[sworkX_.phrhed[yy-One]-One][13-One]=k3p3;
					}
				}
				else
				{
					xx=semtrX_.orig[sgnX_.gn[-79-k3p3-One]-One];
					sworkX_.swork[yy-One][2-One]=sworkX_.swork[xx-One][2-One];
					sconX_.scon[sworkX_.phrhed[yy-One]-One][11-One]=sconX_.scon[sworkX_.phrhed[xx-One]-One][11-One];
					sconX_.scon[sworkX_.phrhed[yy-One]-One][13-One]=sconX_.scon[sworkX_.phrhed[xx-One]-One][13-One];
				}
				
				/*        IN CASE SWORK ALREADY CREATED */
				if(n4==nn4&&sw26nX_.sw26n<=0)
					sworkoX_.sworko[sworkoX_.phcto-One][2-One]=sworkX_.swork[yy-One][2-One];
			}
			
			if(k3p4!=0)
			{
				if(k3p4>0)
				{
					sworkX_.swork[yy-One][3-One]=k3p4;
				}
				else
				{
					xx=semtrX_.orig[sgnX_.gn[-79-k3p4-One]-One];
					sworkX_.swork[yy-One][3-One]=sworkX_.swork[xx-One][3-One];
				}
				
				/*        IN CASE SWORK ALREADY CREATED */
				if(n4==nn4)
					sworkoX_.sworko[sworkoX_.phcto-One][3-One]=sworkX_.swork[yy-One][3-One];
			}
			
			if(diagsX_.deepdi==1)
			{
				fprintf(_spec_fp," SEM SW46\n   %5d%5d%5d%5d%5d\n",n6jim,yy,type1,type2,type3);
			}
			return;
		}
		yy=sworkX_.phrhed[semargX_.index-One];
		
		if(k3p2!=0)
			sconX_.scon[yy-One][11-One]=k3p2;
		
		if(k3p3!=0)
			sworkX_.swork[semargX_.index-One][2-One]=k3p3;
		
		if(k3p4!=0)
			sconX_.scon[yy-One][13-One]=k3p4;
		*retflg=1;
	}
	else if(swnum==47)
	{
		/*        * * * START OF -47 SWITCH * * * */
		scommkX_.k3n=scommkX_.k3+8;
		
		/*		skip this target switch in the PARSE program */
		if(!(passesX_.passfl==1&&passesX_.passct==1))
		{
			if(semargX_.k3p3>10)
			{
				semargX_.pntr9=semargX_.k3p3;
				wc=sworkX_.swork[semargX_.index-One][0];
				
				if(wc!=2&&wc!=14)
				{
					semargX_.k3p3=1;
					
					if(wc!=4)
					{
						supset=sconX_.scon[sworkX_.phrhed[semargX_.index-One]-One][13-One];
						
						if(supset>=13&&supset<=17)
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
			 *    TEMPORARY CODE FOR ENGLISH FRENCH SYSTEM: 8/1/84NULL = 997 */
			if((srcflgX_.srcflg!=2||(trgflgX_.trgflg!=3&&trgflgX_.trgflg!=4&&trgflgX_.trgflg!=6))||svtrfX_.svtrf[scommkX_.k3+1-One]!=997)
			{
				addr1=svtrfX_.svtrf[scommkX_.k3+1+semargX_.k3p3-One];
				addr=addr1+(1000*(svtrfX_.svtrf[scommkX_.k3+1-One]));
				
				/*        HAS FLAG BEEN SET BY THE -32? */
				if(!(flag32X_.prm32==0||flag32X_.ad32== -1))
				{
					if(flag32X_.prm32==semargX_.k3p3)
						addr=addr1+(1000* flag32X_.ad32);
				}
				flag32X_.prm32=0;
				flag32X_.ad32= -1;
				
				if(addr!=0)
				{
					/*        CODE FROM HERE TO 47191
					 *        MODIFIES SCONS OF NON-PREDICATES ACCORDING TO PNTR */
					if(srcflgX_.srcflg==1)
					{
						if((sconX_.scon[sworkX_.phrhed[semargX_.index-One]-One][0]==4&&sconX_.scon[sworkX_.phrhed[semargX_.index-One]-One][13-One]==16)&&sconX_.scono[sconX_.scolnk[sworkX_.phrhed[
									semargX_.index-One]-One]-One][45-SCONX1-One]==4)
							goto L_8080;
					}
					
					if(!(semargX_.k3p3<2||semargX_.k3p3>6))
					{
						/*+                                                        *RGF47*GBA
						 *   FOR GERMAN-FRENCH TRANSLATION, THE SEMTAB TRANSFERS DIFFER */
						if(srcflgX_.srcflg==1&&trgflgX_.trgflg!=2)
						{
							if(!((semargX_.k3p3==1||semargX_.k3p3==5)||semargX_.k3p3==6))
							{
								if(semargX_.k3p3==3)
								{
									sconX_.scon[sworkX_.phrhed[semargX_.index-One]-One][7-One]=10;
								}
								else if(semargX_.k3p3==4)
								{
									sconX_.scon[sworkX_.phrhed[semargX_.index-One]-One][7-One]=11;
								}
								else
								{
									sconX_.scon[sworkX_.phrhed[semargX_.index-One]-One][7]=2;
								}
							}
						}
						else if(addr1==svtrfX_.svtrf[scommkX_.k3+2-One])
						{
							if(!(semargX_.k3p3==1||semargX_.k3p3==5))
							{
								if(semargX_.k3p3==3)
								{
									sconX_.scon[sworkX_.phrhed[semargX_.index-One]-One][7-One]=4;
								}
								else if(semargX_.k3p3==4||semargX_.k3p3==6)
								{
									sconX_.scon[sworkX_.phrhed[semargX_.index-One]-One][7-One]=5;
									
									if(semargX_.k3p3==6)
									{
										phrstr=sworkX_.phrbeg[semargX_.index-One];
										phrlst=sworkX_.phrend[semargX_.index-One];
										
										for(ms=phrstr; ms<=phrlst; ms++)
										{
											if(opadriX_.opadri[ms-One]== -115)
												goto L_9207;
										}
										goto L_7920;
									L_9207:
										
										if(hpdopiX_.hfdopi[ms-One]==0)
										{
											hpdopiX_.hfdopi[ms-One]= -175;
										}
										else
										{
											if(hpdopiX_.hfdopi[ms-One]>=HFPOLO&&hpdopiX_.hfdopi[ms-One]<=HFPOHI)
											{
												hfaddr=hpdopiX_.hfdopi[ms-One]-HFPOL1;
												jz=hfdoaX_.hfpoad[hfaddr-One][HFPADX-One]+1;
												hfdoaX_.hfpoad[hfaddr-One][jz-One]=175;
												hfdoaX_.hfpoad[hfaddr-One][HFPADX-One]=jz;
											}
											else
											{
												hfdoaX_.adct+=1;
												
												if(diagsX_.longdi==1)
												{
													fprintf(_spec_fp,"    NOW %2d HFDOPI VC\\\"S (%2ld-%2ld)\n",hfdoaX_.adct,HFPOLO,HFPOHI);
												}
												
												/*     EXPAND HFPOAD, SCONHF INDEX FROM 5         05/08/86  *B0406DSD */
												if(hfdoaX_.adct>HFPADY)
												{
													if(diagsX_.longdi==1)
													{
														fprintf(_spec_fp," IN SMSW44 - HFDOPI %2ld TO %2ld ALREADY TAKEN \n",HFPOLO,HFPOHI);
													}
													errlog(pgmnam,7680,502,0);
													return;
												}
												else
												{
													hfdoaX_.hfpoad[hfdoaX_.adct-One][0]=hpdopiX_.hfdopi[ms-One];
													jz=2;
													hfdoaX_.hfpoad[hfdoaX_.adct-One][2-One]=175;
													hfdoaX_.hfpoad[hfdoaX_.adct-One][HFPADX-One]=jz;
													hpdopiX_.hfdopi[ms-One]=hfdoaX_.adct+HFPOL1;
													hfdoaX_.sconhf[hfdoaX_.adct-One][0]=opadriX_.sconpi[ms-One];
												}
											}
											
											/*        LOAD A SCON FOR NEW CONSTANT */
											prtscoX_.sct+=1;
											
											if(prtscoX_.sct>SCONY)
											{
												prtscoX_.sct=SCONY;
												prtscoX_.scterr+=1;
												
												if(diagsX_.longdi==11)
												{
													fprintf(_spec_fp," SEMSW2, OVERLOADING SCON ARRAY, SCTERR =%4d\n",prtscoX_.scterr);
												}
												
												if(prtscoX_.scterr==1)
													errlog(pgmnam,7880,500,13);
											}
											else
											{
												hfdoaX_.sconhf[hfdoaX_.adct-One][jz-One]=prtscoX_.sct;
												sconX_.scon[prtscoX_.sct-One][0]=21;
												
												for(ms=2; ms<=14; ms++)
												{
													sconX_.scon[prtscoX_.sct-One][ms-One]=0;
												}
											}
										}
									}
								}
								else
								{
									sconX_.scon[sworkX_.phrhed[semargX_.index-One]-One][7]=2;
								}
							}
						}
					}
				L_7920:
					phrstr=sworkX_.phrbeg[semargX_.index-One];
					phrlst=sworkX_.phrend[semargX_.index-One];
					
					/*    PR1709 (GE ONLY) SEARCH FOR VC108 OF INDEX - NULL IT */
					if(srcflgX_.srcflg==1)
					{
						for(gb=phrstr; gb<=phrlst; gb++)
						{
							if(opadriX_.opadri[gb-One]== -108)
							{
								hpdopiX_.hfdopi[gb-One]= -140;
								break;
							}
						}
					}
					
					for(kz=phrstr; kz<=phrlst; kz++)
					{
						if(opadriX_.sconpi[kz-One]==sworkX_.phrhed[semargX_.index-One])
						{
							scon_pos=sworkX_.phrhed[semargX_.index-One];
							swork_pos = sconX_.scon[scon_pos-One][10-One];

							opadriX_.opadri[kz-One]=addr;
							memcpy(cmpsmcX_.cmpcod[swork_pos-One][prctX_.js[swork_pos-One]-One],
								   curr_sp5rule_company_code,sizeof(cmpsmcX_.cmpcod[0][0]));
							
							sconX_.scon[scon_pos-One][14-One]=2;
							if(scommpX_.pntrsv==1)
							{
								if(sconX_.scon[scon_pos-One][7]==2)
								   sconX_.scon[scon_pos-One][7]=1;
							}
							break;
						}
					}
					
					/*  JAL   SET MAIN PAT NUM IN SCON59 FOR ALL LANGS       *10/09/91*JAL*
					 *  GBA   CAPTURE TARG25, TCGENM, TCOV2B FOR LANGUAGE PAIRS */
					taddress=addr;
					dictype=3;			// low constant
					memcpy(company_code,curr_sp5rule_company_code,3);
					errvrsX_.err=TARG_CODES(& trgflgX_.trgflg,& dictype,& taddress,company_code,(short*)& trgcdsX_,diagsX_.longdi,_spec_fp);
					if(errvrsX_.err==0)
					{
						targ25X_.targ25[sconX_.scolnk[sworkX_.swork[semargX_.index-One][4-One]-One]-One]=trgcdsX_.tcgenm;
						
						if(sconX_.scon[sworkX_.phrhed[semargX_.index-One]-One][0]==2)
							sconX_.scon[sworkX_.phrhed[semargX_.index-One]-One][3-One]=trgcdsX_.tcov2b;
						sconX_.scono[sconX_.scolnk[sworkX_.phrhed[semargX_.index-One]-One]-One][59-SCONX1-One]=trgcdsX_.tcpatm[0];
						
						/*+             null black hole counter for new verb  LOG#2128 jal 9/16/94 */
						sconX_.scono[sconX_.scolnk[sworkX_.phrhed[semargX_.index-One]-One]-One][100-SCONX1-One]=0;
					}
					else
					{
						errlog(pgmnam,8070,taddress,10);
					}
				}
			}
			else
			{
				addr=0;
			}
		L_8080:
			
			if(diagsX_.deepdi==1)
			{
				fprintf(_spec_fp," SEM SW47\n   %6d%6d%6d%6d%6d%6d%6d%6ld\n",semargX_.k3p3,semargX_.pntr9,wc,supset,addr,phrstr,phrlst,taddress);
			}
		}
	}

	
	
	
	
	else if(swnum==48)
	{
		/*        * * * START OF -48 SWITCH * * * */
		scommkX_.k3n=scommkX_.k3+4;
		
		/*		skip this target switch in the PARSE program */
		if(!(passesX_.passfl==1&&passesX_.passct==1))
		{
			k3p2=svtrfX_.svtrf[scommkX_.k3+2-One];
			k3p3=svtrfX_.svtrf[scommkX_.k3+3-One];
			
			/*   CHECK FOR E-F ANY -48997: THIS IS A NULL VALUE AND SHOULD BE SKIPPED */
			if(!((srcflgX_.srcflg==2&&(trgflgX_.trgflg==3||trgflgX_.trgflg==4||trgflgX_.trgflg==6))&&sw_val1==997))
			{
				if(sw_val1==43)
				{
					n6jim= -79-k3p3;
					phrstr=sworkX_.phrbeg[n6jim-One];
					phrlst=sworkX_.phrend[n6jim-One];
					
					for(gb=phrstr; gb<=phrlst; gb++)
					{
						gbx=opadriX_.sconpi[gb-One];
						
						if(sconX_.scon[gbx-One][0]>=0)
							sconX_.scon[gbx-One][3-One]=k3p3;
					}
				}
				else if(sw_val1==8)
				{
					n6jim= -79-k3p2;
					xx=sconX_.scolnk[semfrmX_.semwrk[sgnX_.gn[n6jim-One]-One][4-One]-One];
					formsaX_.formsv[xx-One]=k3p3;
				}
				else
				{
					if(sw_val1==12)
					{
						n6jim= -79-k3p3;
						xx=semtrX_.orig[sgnX_.gn[n6jim-One]-One];
						sconX_.scon[sworkX_.phrhed[xx-One]-One][12-One]=k3p2;
					}
					
					if(sw_val1==13)
					{
						n6jim= -79-k3p3;
						xx=semtrX_.orig[sgnX_.gn[n6jim-One]-One];
						sconX_.scon[sworkX_.phrhed[xx-One]-One][3-One]=k3p2;
					}
					
					/*        FUNCTION 14 TEST TYPE FIELD OF N6JIM
					 *        LOAD VC107 OR VC108           PR1087 */
					if(!((sw_val1!=14)||(srcflgX_.srcflg!=1)))
					{
						n6jim= -79-k3p3;
						xx=semtrX_.orig[sgnX_.gn[n6jim-One]-One];
						gb=sconX_.scon[sworkX_.phrhed[xx-One]-One][13-One];
						gb1=sconX_.scon[sworkX_.phrhed[xx-One]-One][11-One];
						
						if((gb!=1)&&(gb1!=81))
						{
							phrstr=sworkX_.phrbeg[semargX_.index-One];
							phrlst=sworkX_.phrend[semargX_.index-One];
							
							for(jgb=phrstr; jgb<=phrlst; jgb++)
							{
								if(opadriX_.opadri[jgb-One]== -108)
									goto L_8279;
							}
							goto L_8280;
						L_8279:
							hpdopiX_.hfdopi[jgb-One]= -k3p2;
						}
						else
						{
							phrstr=sworkX_.phrbeg[xx-One];
							phrlst=sworkX_.phrend[xx-One];
							
							for(jgb=phrstr; jgb<=phrlst; jgb++)
							{
								if(opadriX_.opadri[jgb-One]== -107)
									goto L_8275;
							}
							goto L_8280;
						L_8275:
							hpdopiX_.hfdopi[jgb-One]= -k3p2;
						}
					}
				}
			}
		L_8280:
			
			if(diagsX_.deepdi==1)
			{
				fprintf(_spec_fp," SEM SW48  %6d%6d%6d%6d%6d\n",scommkX_.k3n,k3p2,k3p3,xx,formsaX_.formsv[sconX_.scolnk[xx-One]-One]);
			}
		}
	}

	
	
	else if(swnum==54)
	{
		/*   -54 IS A SCON SETTING SWITCH
		 *       -54  NO.PAIRS  -8X   PAIR 1   PAIR 2 . . . PAIR 3
		 *            K3P1      K3P2   K3P1*2 .... */
		k3p1=svtrfX_.svtrf[scommkX_.k3+1-One];
		k3p2=svtrfX_.svtrf[scommkX_.k3+2-One];
		scommkX_.k3n=scommkX_.k3+3+(k3p1* 2);
		
		/*		skip this target switch in the PARSE program */
		if(!(passesX_.passfl==1&&passesX_.passct==1))
		{
			izbeg=scommkX_.k3+3;
			izend=izbeg+k3p1*2-1;
			
			/*   WHICH ELEMENT ARE WE CHANGING? */
			n6jim=semtrX_.orig[sgnX_.gn[-79-k3p2-One]-One];
			elepos=sworkX_.phrhed[n6jim-One];
			
			for(iz=izbeg; iz<=izend; iz+=2)
			{
				/*   ARE WE LOADING A CONSTANT OR TRANSFERING FROM ANOTHER SCON? */
				scnpos=svtrfX_.svtrf[iz-One];
				
				if(svtrfX_.svtrf[iz+1-One]< -70)
				{
					/*   LOADING THE SAME POSITION FROM ANOTHER ELEMENT */
					n7jim=semtrX_.orig[sgnX_.gn[-79-svtrfX_.svtrf[iz+1-One]-One]-One];
					
					if(scnpos<=20)
						sconX_.scon[elepos-One][scnpos-One]=sconX_.scon[sworkX_.phrhed[n7jim-One]-One][scnpos-One];
					
					if(scnpos>20)
						sconX_.scono[sconX_.scolnk[elepos-One]-One][scnpos-SCONX1-One]=sconX_.scono[sconX_.scolnk[sworkX_.phrhed[n7jim-One]-One]-One][scnpos-SCONX1-One];
				}
				else
				{
					/*   LOADING A CONSTANT */
					if(scnpos<=20)
						sconX_.scon[elepos-One][scnpos-One]=svtrfX_.svtrf[iz+1-One];
					
					if(scnpos>20)
						sconX_.scono[sconX_.scolnk[elepos-One]-One][scnpos-SCONX1-One]=svtrfX_.svtrf[iz+1-One];
				}
			}
		}
	}
	return;
}    /*end of function*/



