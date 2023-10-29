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
//---------------------------------------------------------------------
// File - PreTransl.cpp
//---------------------------------------------------------------------
#include <logos_include/logoscommon.h>
#include <transl/Filter.h>
#include <transl/Utils.h>
#include <lgs_db_io/lgsdbcommonobjects.h>
#include <configdatafileinterface/configdatainterfacemain.h>
#include <logos_libs/utility/CEnvVariable.h>
#include <logos_libs/utility/stringutil.h>

static ofstream jobInfoFile;

void DisplayJobInfo();
void SetJobInfoFile(int jobID);
bool CheckScratch(int jobID, bool traceOption);

int PreTransl()
{
   int nResult = 0;
   
   int jobID = LgsDBCommonObjects::GetJobControlArguments().JobID();
   LgsString userID = LgsDBCommonObjects::GetJobControlArguments().UserID();
   bool timeOption = LgsDBCommonObjects::GetJobControlArguments().TimeFlag();
   bool traceOption = LgsDBCommonObjects::GetJobControlArguments().TraceFlag();
   
   // setup java runtime and class locations
   CEnvVariable evLGS_ROOT(LGS_ROOT);
   CEnvVariable evCLASSPATH(CLASSPATH);
   evCLASSPATH = evCLASSPATH.GetValue() + PATH_SEP_STR + evLGS_ROOT.GetValue() + DIR_SEP_STR + "bin" + PATH_SEP_STR + evLGS_ROOT.GetValue() + DIR_SEP_STR + "bin" + DIR_SEP_STR + "filter.jar"+PATH_SEP_STR;
   evCLASSPATH.Export();
   
#ifdef _MSC_VER
   char ComputerName[MAX_COMPUTERNAME_LENGTH + 1];
   DWORD nCompNameSize = MAX_COMPUTERNAME_LENGTH + 1;
   GetComputerName(ComputerName, &nCompNameSize);
#else
   char *ComputerName;
   if ((ComputerName = getenv("HOSTNAME")) == NULL) 
     ComputerName = "Unknown";
#endif

   // Display the directories
   CEnvVariable evPATH(PATH);
   cout << " Path\t\t\t= " << evPATH.GetValue() << endl;
   cout << " Class Path\t\t= " << evCLASSPATH.GetValue() << endl;
   cout << " Server\t\t\t= " << ComputerName << endl;
   cout << " Session ID\t\t= " << StringUtil::asStringFromInt(jobID) << endl;
   cout << " User ID\t\t= " << userID << endl;
   cout << " Root Directory\t\t= " << evLGS_ROOT.GetValue() << endl;
   cout << " Scratch Directory\t= " << LgsDBCommonObjects::GetServerProperties().ScratchFileDirectory() << endl;
   cout << " Jobfile Directory\t= " << LgsDBCommonObjects::GetServerProperties().JobFileDirectory() << endl;
   //cout << " Server Interval\t= " << LgsDBCommonObjects::GetServerProperties().TranslationServerInterval() << endl;
#ifdef USE_ODBC
   cout << " ODBC Data Source\t= " << LgsDBCommonObjects::GetServerProperties().ODBCDataSource() << endl;
#else
   cout << " Oracle Data Source\t= " << LgsDBCommonObjects::GetServerProperties().OracleDataSource() << endl;
#endif
   cout << endl;
   
   // indicate the job is running.
   LgsDBCommonObjects::GetJobControlArguments().Status(JobControlArguments::StatusRUNNING);
   
   // The function checks the scrach area and cleans up. It then copies the input files
   // from job file directory to scratch area. It also sets the job status as running.
   if (!CheckScratch(jobID, traceOption))
   {
      return -1;
   }
   
   // Display job information
   DisplayJobInfo();
   
   // Set up the job information file
   SetJobInfoFile(jobID);
   
   // run filter
   LgsString inputFormat = LgsDBCommonObjects::GetJobControlArguments().InputFormat();
   nResult = Filter(inputFormat, true, timeOption, traceOption);
   
   if (traceOption)
   {
      cout << "\n************End Filter Source RetCode = " << nResult << " ***********\n\n";
      cout.flush();
   }
   
   if (nResult != 0)
   {
      ErrorOut("filter Source :" + StringUtil::asStringFromInt(nResult));
   }
   
   return nResult;
}
//---------------------------------------------------------------------
void DisplayJobInfo()
{
   LgsString jobType = (LgsDBCommonObjects::GetJobControlArguments().JobType() == 1)? "translation": "wordSearch";
   
   LgsString sourceLang;
   switch(LgsDBCommonObjects::GetJobControlArguments().SourceLanguage())
   {
      case 1: sourceLang = "german";  break;
      case 2: sourceLang = "english"; break;
   }
   
   LgsString targetLang;
   switch(LgsDBCommonObjects::GetJobControlArguments().TargetLanguage())
   {
      case 1: targetLang = "german";     break;
      case 2: targetLang = "english";    break;
      case 3: targetLang = "french";     break;
      case 4: targetLang = "spanish";    break;
      case 5: targetLang = "italian";    break;
      case 6: targetLang = "portuguese"; break;
   }
   
   LgsString inFile(LgsDBCommonObjects::GetJobControlArguments().ClientInputFile());
   LgsString outFile(LgsDBCommonObjects::GetJobControlArguments().ClientOutputFile());
   LgsString alignedFile(LgsDBCommonObjects::GetJobControlArguments().ClientAlignedFile());
   cout << " Language\t\t= " << sourceLang << "  " << targetLang << endl;
   cout << " Job Type\t\t= " << jobType << endl;
   cout << " Input \t\t\t= " << inFile << endl;
   cout << " Output\t\t\t= " << outFile << endl;
   cout << " Aligned\t\t= " << alignedFile << endl;
   cout.flush();
}
//---------------------------------------------------------------------
void SetJobInfoFile(int jobID)
{
   char jobInfoFileName[MAX_FILEPATH_LEN];
   GetConfigData("tempfile", "job_info", jobInfoFileName, MAX_FILEPATH_LEN);

   jobInfoFile.open(jobInfoFileName);
   if (jobInfoFile.is_open())
   {
      jobInfoFile << "[settings]\n";
      jobInfoFile << "sessionId=" << StringUtil::asStringFromInt(jobID) << '\n';
      
      // Get info from JobControlArguments
      JobControlArguments &jca = LgsDBCommonObjects::GetJobControlArguments();
      // Type of job
      jobInfoFile << "jobTypeName=";
      if (jca.RunMode() == JobControlArguments::translationMode)
      {
         jobInfoFile << "translation";
      }
      else
      {
         jobInfoFile << "wordSearch";
      }
      jobInfoFile << '\n';
      
      // Source language
      jobInfoFile << "sourcelanguage=";
      switch (jca.SourceLanguage())
      {
		  case 1: jobInfoFile << "german";  break;
		  case 2: jobInfoFile << "english"; break;
      }
      jobInfoFile << '\n';
      
      // Target language
      jobInfoFile << "targetlanguage=";
      switch (jca.TargetLanguage())
      {
		  case 1: jobInfoFile << "german";     break;
		  case 2: jobInfoFile << "english";    break;
		  case 3: jobInfoFile << "french";     break;
		  case 4: jobInfoFile << "spanish";    break;
		  case 5: jobInfoFile << "italian";    break;
		  case 6: jobInfoFile << "portuguese"; break;
      }
      jobInfoFile << '\n';
      
      // Company codes
      jobInfoFile << "companyCodes=" << jca.CompanyCodes() << '\n';

      // SubjectMatterCodes
      jobInfoFile << "subjectMatterCodes=" << jca.SubjectMatterCodes() << '\n';

      // ExtendedSearch
      jobInfoFile << "extendedSearch=" << jca.ExtendedSearch() << '\n';

      // ProtectionCharacter
      jobInfoFile << "protectionCharacter=" << jca.ProtectionCharacter() << '\n';

      // TargetForm
      jobInfoFile << "targetForm=" << jca.TargetForm() << '\n';

      // SourceLocale
      jobInfoFile << "sourceLocale=" << jca.SourceLocaleData() << '\n';
      // TargetLocale
      jobInfoFile << "targetLocale=" << jca.TargetLocaleData() << '\n';

      // DiagnosticLevel
      jobInfoFile << "diagnosticLevel=" << jca.DiagnosticLevel() << '\n';
      // DiagnosticStartLine
      jobInfoFile << "diagnosticStartLine=" << jca.DiagnosticStartLine() << '\n';
      // DiagnosticEndLine
      jobInfoFile << "diagnosticEndLine=" << jca.DiagnosticEndLine() << '\n';

      // WordSearchOptions
      jobInfoFile << "wordSearchOptions=";
      if( jca.WordSearchOptions() & 1 ) jobInfoFile << "UNFOUND";
      if( jca.WordSearchOptions() & 2 ) jobInfoFile << " FOUND_NOUN";
      if( jca.WordSearchOptions() & 4 ) jobInfoFile << " FOUND_VERB";
      if( jca.WordSearchOptions() & 8 ) jobInfoFile << " FOUND_ADJ";
      if( jca.WordSearchOptions() & 16 ) jobInfoFile << " FOUND_ADV";
      jobInfoFile << '\n';

      // WordSearchFoundLimits
      jobInfoFile << "wordSearchFoundLimits  =" << jca.WordSearchFoundStart() << ' '
                                                << jca.WordSearchFoundLimit() << '\n';
      // WordSearchUnfoundLimits
      jobInfoFile << "wordSearchUnfoundLimits=" << jca.WordSearchUnfoundStart() << ' '
                                                << jca.WordSearchUnfoundLimit() << '\n';

      // FlagUnfoundWords
      jobInfoFile << "flagUnfoundWords=";
      if( jca.WordSearchOptions() & 0x0100 ) jobInfoFile << "Yes";
      else                                   jobInfoFile << "No";
      jobInfoFile << '\n';

      // Main files
      jobInfoFile << "mainRes1  =" << jca.MainRes1() << '\n';
      jobInfoFile << "mainRes2  =" << jca.MainRes2() << '\n';
      jobInfoFile << "mainRes22 =" << jca.MainRes22() << '\n';
      jobInfoFile << "mainTran1 =" << jca.MainTran1() << '\n';
      jobInfoFile << "mainTran2 =" << jca.MainTran2() << '\n';
      jobInfoFile << "mainTran3 =" << jca.MainTran3() << '\n';
      jobInfoFile << "mainTran4 =" << jca.MainTran4() << '\n';
      jobInfoFile << "mainParse1=" << jca.MainParse1() << '\n';
      jobInfoFile << "mainParse2=" << jca.MainParse2() << '\n';
      jobInfoFile << "mainParse3=" << jca.MainParse3() << '\n';
      jobInfoFile << "mainParse4=" << jca.MainParse4() << '\n';

      // Mini files
      jobInfoFile << "miniRes2  =" << jca.MiniRes2() << '\n';
      jobInfoFile << "miniTran1 =" << jca.MiniTran1() << '\n';
      jobInfoFile << "miniTran2 =" << jca.MiniTran2() << '\n';
      jobInfoFile << "miniTran3 =" << jca.MiniTran3() << '\n';
      jobInfoFile << "miniTran4 =" << jca.MiniTran4() << '\n';
      jobInfoFile << "miniParse1=" << jca.MiniParse1() << '\n';
      jobInfoFile << "miniParse2=" << jca.MiniParse2() << '\n';
      jobInfoFile << "miniParse3=" << jca.MiniParse3() << '\n';
      jobInfoFile << "miniParse4=" << jca.MiniParse4() << '\n';

      // Close file
      jobInfoFile.close();
   }
   else
   {
      cout << " Unable to create job info file " << jobInfoFileName << endl;  
   }
}
//---------------------------------------------------------------------
bool CheckScratch(int jobID, bool traceOption)
{
   // The function checks the scrach area and cleans up. 
   // Then copies the input files from job file directory to scratch area.
   bool returnResult = true;
   LgsString scratchDir = LgsDBCommonObjects::GetServerProperties().ScratchFileDirectory();
   
   if (traceOption)
   {
      cout << "\nRemoving all files with this jobid from scratch area" << endl;
   }
   Delete(scratchDir + DIR_SEP_CHAR + StringUtil::asStringFromInt(jobID) + ".*");
   
   // Copy input file to scratch directory
   LgsString sourceFileName = LgsDBCommonObjects::GetJobControlArguments().InputFile();
   char targetFileName[MAX_FILEPATH_LEN];
   GetConfigData("tempfile", "filter_input", targetFileName, MAX_FILEPATH_LEN);
   bool bRetCode = Copy(sourceFileName, targetFileName);
   if (!bRetCode)
   {
      ErrorOut("Copy of input file to scratch failed.");
      returnResult = false;
   }
   return returnResult;
}
