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
//Implementation  File res_stat.cpp
//Created on 03/20/99 by Anand Bosco
//This file contains routines to log the res rule match statistics to a disk
//file
#ifdef _MSC_VER
#include "stdafx.h"
#endif
#include <logos_libs/lgs_tran_rule_io/res_rule_stat.h>
extern "C"
{
	// c interface
	#include <logos_include_res_pt/res_stat_file.h>
}

#include "logos_libs/multithreadlib/lgscritsect.h"

#include <cstring>

const char SEP = '\t'; // a separator symbol

static void   Res_Stat(FILE *diagFile, int nType, int nRuleNumber, int sentID, const char *sentence, int sentLen);
static void Tran1_Stat(FILE *diagFile, int nType, int nRuleNumber, int sentID, const char *sentence, int sentLen);
static void Tran2_Stat(FILE *diagFile, int nType, int nRuleNumber, int sentID, const char *sentence, int sentLen);
static void Tran3_Stat(FILE *diagFile, int nType, int nRuleNumber, int sentID, const char *sentence, int sentLen);
static void Tran4_Stat(FILE *diagFile, int nType, int nRuleNumber, int sentID, const char *sentence, int sentLen);

static void Print_Stat(int nModule, FILE *diagFile, int nType, int nRuleNumber, int sentID, const char *sentence, int sentLen);

static int lastSentID[5] = {-1, -1, -1, -1, -1};

// used for sync
// CRITICAL_SECTION csecResFileIO;
// CRITICAL_SECTION csecTran1FileIO;
// CRITICAL_SECTION csecTran2FileIO;
// CRITICAL_SECTION csecTran3FileIO;
// CRITICAL_SECTION csecTran4FileIO;
LgsCriticalSection csecResFileIO;
LgsCriticalSection csecTran1FileIO;
LgsCriticalSection csecTran2FileIO;
LgsCriticalSection csecTran3FileIO;
LgsCriticalSection csecTran4FileIO;


extern "C"
{
	// c interface (Wrapper function.)
	void res_stat(FILE *diagFile, int nType, int nRuleNumber, int sentID, const char *sentence, int sentLen)
	{
		Res_Stat(diagFile, nType, nRuleNumber, sentID, sentence, sentLen);
	}

	void tran1_stat(FILE *diagFile, int nType, int nRuleNumber, int sentID, const char *sentence, int sentLen)
	{
		Tran1_Stat(diagFile, nType, nRuleNumber, sentID, sentence, sentLen);
	}

	void tran2_stat(FILE *diagFile, int nType, int nRuleNumber, int sentID, const char *sentence, int sentLen)
	{
		Tran2_Stat(diagFile, nType, nRuleNumber, sentID, sentence, sentLen);
	}

	void tran3_stat(FILE *diagFile, int nType, int nRuleNumber, int sentID, const char *sentence, int sentLen)
	{
		Tran3_Stat(diagFile, nType, nRuleNumber, sentID, sentence, sentLen);
	}

	void tran4_stat(FILE *diagFile, int nType, int nRuleNumber, int sentID, const char *sentence, int sentLen)
	{
		Tran4_Stat(diagFile, nType, nRuleNumber, sentID, sentence, sentLen);
	}
}

/*******************************************************************
This funtion initialises the critical section.
*********************************************************************/
bool Stat_File_IO_CriticalSect_init()
{
//    InitializeCriticalSection(&csecResFileIO);
//    InitializeCriticalSection(&csecTran1FileIO);
//    InitializeCriticalSection(&csecTran2FileIO);
//    InitializeCriticalSection(&csecTran3FileIO);
//    InitializeCriticalSection(&csecTran4FileIO);
   csecResFileIO.initialize();
   csecTran1FileIO.initialize();
   csecTran2FileIO.initialize();
   csecTran3FileIO.initialize();
   csecTran4FileIO.initialize();
   return true;
}

//Clean up on dll detach
void Stat_File_IO_Clean()
{
	//Close Critical sections.
// 	DeleteCriticalSection(&csecResFileIO);
// 	DeleteCriticalSection(&csecTran1FileIO);
// 	DeleteCriticalSection(&csecTran2FileIO);
// 	DeleteCriticalSection(&csecTran3FileIO);
// 	DeleteCriticalSection(&csecTran4FileIO);
}


/************************************************************************
This function opens the statistic file for appending the data.
If the file does not exist then it creates the file.
*************************************************************************/
void Res_Stat(FILE *diagFile, int nType, int nRuleNumber, int sentID, const char *sentence, int sentLen)
{
  //EnterCriticalSection(&csecResFileIO);
  csecResFileIO.enter();
  Print_Stat(0, diagFile, nType, nRuleNumber, sentID, sentence, sentLen);
  csecResFileIO.leave();
  //LeaveCriticalSection(&csecResFileIO);
}

void Tran1_Stat(FILE *diagFile, int nType, int nRuleNumber, int sentID, const char *sentence, int sentLen)
{
  //EnterCriticalSection(&csecTran1FileIO);
  csecTran1FileIO.enter();
  Print_Stat(1, diagFile, nType, nRuleNumber, sentID, sentence, sentLen);
  csecTran1FileIO.leave();
  //LeaveCriticalSection(&csecTran1FileIO);
}

void Tran2_Stat(FILE *diagFile, int nType, int nRuleNumber, int sentID, const char *sentence, int sentLen)
{
  //EnterCriticalSection(&csecTran2FileIO);
  csecTran2FileIO.enter();
  Print_Stat(2, diagFile, nType, nRuleNumber, sentID, sentence, sentLen);
  csecTran2FileIO.leave();
  //LeaveCriticalSection(&csecTran2FileIO);
}

void Tran3_Stat(FILE *diagFile, int nType, int nRuleNumber, int sentID, const char *sentence, int sentLen)
{
  //EnterCriticalSection(&csecTran3FileIO);
  csecTran3FileIO.enter();
  Print_Stat(3, diagFile, nType, nRuleNumber, sentID, sentence, sentLen);
  csecTran3FileIO.leave();
  //LeaveCriticalSection(&csecTran3FileIO);
}

void Tran4_Stat(FILE *diagFile, int nType, int nRuleNumber, int sentID, const char *sentence, int sentLen)
{
  //EnterCriticalSection(&csecTran4FileIO);
  csecTran4FileIO.enter();
  Print_Stat(4, diagFile, nType, nRuleNumber, sentID, sentence, sentLen);
  csecTran4FileIO.leave();
  //LeaveCriticalSection(&csecTran4FileIO);
}

void Print_Stat(int nModule, FILE *diagFile, int nType, int nRuleNumber, int sentID, const char *sentence, int sentLen)
{
   if( diagFile )
   {
      fprintf( diagFile, "%d%c%d%c%d%c%d%c%d", nModule, SEP, nType, SEP, nRuleNumber, SEP,
               (nModule==0)? ResRuleIDFromRuleNumber(nType, nRuleNumber): TranRuleIDFromRuleNumber(nType, nRuleNumber),
               SEP, sentID );
      // Print sentence
      if( (sentence != NULL) && (sentLen > 0) )
      {
         fprintf( diagFile, "%c", SEP );
         if( lastSentID[nModule] != sentID )
         {
            lastSentID[nModule] = sentID;
            int i = (strncmp(sentence, "bos ", 4) == 0)? 4: 0;
            for( ; i < sentLen; i++ ) fprintf( diagFile, "%c", sentence[i] );
         }
      }
      fprintf( diagFile, "\n" );
   }
}
