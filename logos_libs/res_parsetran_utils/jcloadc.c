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
/*---------------------------------------------------------------------------
	Load the JBCTRL fortran common with job information	
-------------------------------------------------------------------------
*/

#include <stdio.h>
#include <string.h>
#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif

// set EXTERN so jbctrl.h defines structures allocated here
#include <logos_include_res_pt/logoslib.h>
#include <lgs_db_io/jobcntrlarginterface.h>
#define JBEXTERN allocatehere 
#include <logos_include_res_pt/jbctrl.h>

   
int jcloadc(char *pgmmod)
{  

 char buf[300];
 char *jcvalue;
 short int validMode = 1;
 char *pt1;
 int wsoptions;



	srcflgX_.srcflg = sourceLanguage();

	trgflgX_.trgflg = targetLanguage();


//c		load up the company code field.
//c		used by semtab matching and other things
	memset(jbctrlX_.jcccls,' ',sizeof(jbctrlX_.jcccls));
    jcvalue = companyCodes(buf);
//			count the number of company codes
	jbctrlX_.jcccls_count = 0;
	for( pt1=(jcvalue + strlen(jcvalue)); jcvalue < pt1; (jcvalue += sizeof(jbctrlX_.jcccls[0]) + 1) ){
			memcpy(jbctrlX_.jcccls[jbctrlX_.jcccls_count],jcvalue,sizeof(jbctrlX_.jcccls[0]));
			jbctrlX_.jcccls_count++;
	}

	
	//     extended dictionary search yes or no
   jbctrlX_.jcextendedsearch = extendedSearch();

//c			Target form:
//C C*1  JCIMPE     1 161  INFINITIVE('0') / IMPERATIVE('1')  FLAG                
	jbctrlX_.jcimpe = 0;
	if (targetForm() == 1)
	{
	  jbctrlX_.jcimpe = 1;
	}


					// DIAGNOSTICS

	memset(opswX_.sw,'\0',sizeof(opswX_.sw));
	opswX_.diag_line_start = 0;
	opswX_.diag_line_end = 0;
	diagsX_.longdi = 0;
	diagsX_.deepdi = 0;
	diagsX_.shrtdi = 0;
	diagsX_.anydi = 0;
   diagsX_.stats = 0;

	// new diagnostic flag based on levels
	// diagnostics=
	// 0 = no diagnostics
	// 1 = not used
	// 2 = short or low level diagnostics. (In most cases just transl lookup semtab and generate.)
	// 3 = long diagnostics (level 2 diags plus more res and tran diags which used to be switch3 and 8)
	// 4 = deep diagnostics (level 3 diags plus large amounts of diags from res and trans. was 10 15 16 20)

	switch (diagnosticLevel())
	{
		case 1:
			break;
		case 2:                 // short
			diagsX_.shrtdi = 1;
			diagsX_.anydi = 1;
			break;
		case 3:                 // long
			diagsX_.shrtdi = 1;
			diagsX_.longdi = 1;
			diagsX_.anydi = 1;
			opswX_.sw[2] = 1;    // trans
			opswX_.sw[7] = 1;    // res
			break;
		case 4:                 // deep
			diagsX_.shrtdi = 1;
			diagsX_.longdi = 1;
			diagsX_.deepdi = 1;
         diagsX_.anydi = 1;
			opswX_.sw[2] = 1;    // trans
			opswX_.sw[7] = 1;    // res
			opswX_.sw[9] = 1;    // tran1
			opswX_.sw[14] = 1;	// tran2
			opswX_.sw[15] = 1;   // tran3
			opswX_.sw[19] = 1;   // tran4
			break;
		case 5:                 // stats
			diagsX_.stats = 1;
		default:
			break;
	}

   opswX_.diag_line_start = diagnosticStartLine();
   opswX_.diag_line_end = diagnosticEndLine();

	if(opswX_.diag_line_start > 0)
	{
		memcpy(opswX_.savsw, opswX_.sw, sizeof(opswX_.sw));
		memset(opswX_.sw, '\0', sizeof(opswX_.sw));
	}
	
//C           get wrdsearch settings
//c			why are we using the sserch routine? 
//C
    jcaddX_.wrdsrc = 0;
	jcaddX_.ufword=0;
	jcaddX_.ufnphr=0;
	jcaddX_.fndwrd=0;
	jcaddX_.fndnou=0;
	jcaddX_.fndadj=0;
	jcaddX_.fndvrb=0;
	jcaddX_.fndadv=0;

	jcaddX_.tagger = 0;
   jcaddX_.tagsrc = 0;

   if (jobType() == 2)
	{
		jcaddX_.wrdsrc=1;
	}
      
	if (jcaddX_.wrdsrc == 1)
	{
      wsoptions = wordSearchOptions();
      if ((wsoptions & 1) == 1)
		{
		  jcaddX_.ufword=1;
		  jcaddX_.ufnphr=1;
		}
      if ((wsoptions & 2) == 2)
		{
		  jcaddX_.fndnou=1;
		  jcaddX_.fndwrd=1;
		}
      if ((wsoptions & 4) == 4)
		{
		  jcaddX_.fndvrb=1;
		  jcaddX_.fndwrd=1;
		}
      if ((wsoptions & 8) == 8)
		{
		  jcaddX_.fndadj=1;
		  jcaddX_.fndwrd=1;
		}
      if ((wsoptions & 16) == 16)
		{
		  jcaddX_.fndadv=1;
		  jcaddX_.fndwrd=1;
		}
	}



/*
c		Set the mini flags on if requested.
c				checking if keyword in the job control
c				is set to something (name of mini file)
*/
   jcvalue = miniRes2(buf);
   if (strcmp(jcvalue, "") != 0)
    {
		jbctrlX_.jcmnrs[1] = 1;
	}
	else
	{
	    jbctrlX_.jcmnrs[1] = 0;
	}

	miniflX_.minip1 = 0;
	miniflX_.minip2 = 0;
	miniflX_.minifl = 0;


	if (strcmp(pgmmod, "tran1") == 0)
	{
      jcvalue = miniTran1(buf);
		memcpy(passesX_.pssnam+4,"1",1);
	}
	else if (strcmp(pgmmod, "tran2") == 0)
	{
      jcvalue = miniTran2(buf);
		memcpy(passesX_.pssnam+4,"2",1);
	}
	else if( strcmp(pgmmod, "tran3") == 0)
	{
      jcvalue = miniTran3(buf);
		memcpy(passesX_.pssnam+4,"3",1);
	}
	else if( strcmp(pgmmod, "tran4") == 0)
	{
      jcvalue = miniTran4(buf);
		memcpy(passesX_.pssnam+4,"4",1);
	}
	else if (strcmp(pgmmod, "pars1") == 0)
	{
      jcvalue = miniParse1(buf);
		memcpy(passesX_.pssnam+4,"1",1);
	}
	else if (strcmp(pgmmod, "pars2") == 0)
	{
      jcvalue = miniParse2(buf);
		memcpy(passesX_.pssnam+4,"2",1);
	}
	else if( strcmp(pgmmod, "pars3") == 0)
	{
      jcvalue = miniParse3(buf);
		memcpy(passesX_.pssnam+4,"3",1);
	}
	else if( strcmp(pgmmod, "pars4") == 0)
	{
      jcvalue = miniParse4(buf);
		memcpy(passesX_.pssnam+4,"4",1);
	}
	else
	{
		validMode = 0;
	}

	if (validMode)
	{
      if (strcmp(jcvalue, "") != 0)
		{
			miniflX_.minifl=1;
			if(passesX_.passfl != 1 || passesX_.passct != 2)
			{
				miniflX_.minip1 = 1 ;
			}
			else
			{
				miniflX_.minip2 = 1;
			}
		}
	}


 return 0;

}

void getProgramName( int passFlag, int passCount, int parseTranStep, char * pgmName )
{
	if (passFlag == 1 && passCount == 1)
	{
		sprintf(pgmName, "%s%1d", "pars", parseTranStep);
	}
	else
	{
		sprintf(pgmName, "%s%1d", "tran", parseTranStep);
	}
}
