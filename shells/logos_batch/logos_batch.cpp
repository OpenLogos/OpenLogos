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
// -------------------------------------------------------------------
// logos_batch.cpp
// -------------------------------------------------------------------
#include <logos_include/logoscommon.h>
#include <logos_include/bstr_t.h>
#include <shells/logos_batch/Ls8Jobs.h>
#include <lgs_db_io/serverproperties.h>
#include <lgs_db_io/jobcontrolarguments.h>
#include <logos_libs/sql/sqlconnection.h>
#include <logos_libs/sql/sqlexception.h>
#include <logos_libs/odbcsql/globalsqlconnection.h>
#include <logos_libs/utility/CEnvVariable.h>
#include <logos_libs/utility/stringutil.h>
#include <logos_libs/utility/lgsout.h>
#include <logos_libs/utility/FileUtil.h>
#include <logos_libs/utility/timestamp.h>
#include <engine_api/xlationinterface/xlationinterfacedefs.h>
#include <engine_api/xlationinterface/xlationsession.h>
#include <engine_api/translationserver/xlationconsts.h>
#include <engine_api/translationserver/comparameters.h>
#ifdef _MSC_VER
#include <comdef.h>
#else
#define __declspec(___Dspec)
#endif
#include <time.h>
#include "xlationsessionmanager.h"

//********************************************************************
//  input out redirection - file names
//********************************************************************
#define LOGOS_BATCH_IN  0
#define LOGOS_BATCH_OUT 0
#define LOGOS_BATCH_ERROR 0
//----------------------- Global file buffers ------------------------------
filebuf inbuf;
filebuf outbuf;
filebuf errbuf;
//--------------------------------------------------------------------------

/* 
// redefinition of values imported from xlationconsts
const char *CRTF = "rtf";
const char *CSGML = "sgml";
const char *CHTML = "sgml";
const char *CTMX = "tmx";
const char *CXML = "xml";
const char *CINTERLEAF_ASCII = "ileaf";
const char *CFRAME_MIF = "frame";
const char *CLGS_TXT_EXCHANGE = "lgssgml";
const char *CMS_WORD8 = "word8";
const char *CRTF_WINHELP = "rtfhelp";
*/

//********************************************************
// surrogate for cout 
//********************************************************
CLgsOut &logFile = CLgsOut::singleton();

bool logQueryOption = false;
bool debugOption = false;
bool timeOption = false;
bool highOnlyOption = false;
bool traceOption = false;
bool saveScratchOption = false;
bool processAllJobs = false;
bool processAllUsers = false;
int jobID = -1;
LgsString userID = "";

void PrintUsage();
bool ProcessCmdLine(int argc, char *argv[]);
void DisplayJobInfo(const Ls8Jobs &job);
void InitLgsBatchStreams(char* pszIN, char* pszOUT, char* pszERROR);
void OpenStream(int nStream, const LgsString &FName);
LgsString GetTime();
TimeStamp GetTimeStamp();
bool RunAPI(const Ls8Jobs &job, int &sentCount, int &wordCount);
void SetAPIParameters(XlationSession *pSession, const Ls8Jobs &job);
void SetInputParameter(XlationSession *pSession, const wchar_t *name, const LgsString &value);
void Print(const LgsString &msg, const UString &value);
void ucToSbs(const UString &ucStr, LgsString &sbString);
int ucToInt(const UString &ucStr);
bool Move(const UString &sourceFile, const LgsString &destFile);

int main(int argc, char *argv[])
{
   // Open input output and error streams
   InitLgsBatchStreams(LOGOS_BATCH_IN, LOGOS_BATCH_OUT, LOGOS_BATCH_ERROR);

   // Process the arguments sent to the application
   if( !ProcessCmdLine(argc, argv) )
   {
      return -1;
   }

   // Create ServerProperties object
   ServerProperties server;
#ifdef USE_ODBC
   LgsString connectString = server.ODBCDataSource();
#else
   LgsString connectString = server.OracleDataSource();
#endif
   LgsString username = server.DatabaseUserID();
   LgsString password = server.DatabasePassword();

   // Get the server pause interval when no job are in database. Default to 1000.
   int noJobsInterval = server.TranslationServerInterval() * 1000;
   if( noJobsInterval < 1000 ) noJobsInterval = 1000;

   // Get names of the excluded users
   LgsString excludeUsers = server.ExcludeRunningUserID();
#ifdef _MSC_VER
   //int MAX_COMPUTERNAME_LENGTH = 80;
   char ComputerName[MAX_COMPUTERNAME_LENGTH + 1];
   DWORD nCompNameSize = MAX_COMPUTERNAME_LENGTH + 1;
   GetComputerName(ComputerName, &nCompNameSize);
#endif
   // Display the directories
   CEnvVariable evPATH(PATH);
   CEnvVariable evLGS_ROOT(LGS_ROOT);
#ifndef _MSC_VER
   CEnvVariable evComputerName("HOSTNAME");
#endif
   logFile << "\n";
   logFile << " Path\t\t\t= " << evPATH.GetValue() << "\n";
   //logFile << " Class Path\t\t= " << evCLASSPATH.GetValue() << "\n";
#ifdef _MSC_VER
   logFile << " Server\t\t\t= " << ComputerName << "\n";
#else
   logFile << " Server\t\t\t= " << evComputerName.GetValue() << "\n";
#endif
   logFile << " User ID\t\t= " << userID << "\n";
   if( processAllUsers )
   {
      logFile << " Exclude Users\t\t= " << excludeUsers << "\n";
   }
   logFile << " Root Directory\t\t= " << evLGS_ROOT.GetValue() << "\n";
   //logFile << " Scratch Directory\t= " << server.ScratchFileDirectory() << "\n";
   logFile << " Jobfile Directory\t= " << server.JobFileDirectory() << "\n";
   logFile << " Server Interval\t= " << server.TranslationServerInterval() << "\n";
#ifdef USE_ODBC
   logFile << " ODBC Data Source\t= " << connectString << "\n";
#else
   logFile << " Oracle Data Source\t= " << connectString << "\n";
#endif
   logFile << "\n";
   logFile << " e.Sense Enterprise Server Started: Looking For Jobs" << "\n";
   logFile << "\n";

   // Get SQL connection
   SqlConnection *pConnection = createConnAlways(connectString, username, password);
   if( pConnection == NULL )
   {
      logFile << "SQL Connection Error\n";
      logFile.flush();
      return -1;
   }

   // Create Ls8Jobs object
   Ls8Jobs job(pConnection);

   // Do forever unless only one job has been requested to run.
   do
   {
      if( processAllJobs )
      {
         // Get the next job for all users.
         jobID = job.GetNextJobID(userID, highOnlyOption, excludeUsers);

         // If there are no job currently pending in database, then wait and try again.
         if( jobID == -1 )
         {
            Sleep(noJobsInterval);
            continue;
         }
      }

// Set status to running only if the job
// has not been taken by somebody else.
	  if(!job.UpdateRunningStatusSafely(jobID, JobControlArguments::StatusRUNNING)) {
         logFile << "Job's already been taken by another job runner. Job ID was: " << jobID << "\n";
		 continue;
	  }

      // Get job's info
      LgsString strJobID = StringUtil::asStringFromInt(jobID);

	  if( !job.GetData(jobID) )
      {
         logFile << "Error getting job's data. Job ID was: " << strJobID << "\n";
      }
      else
      {
         // Set the started time 
         job.UpdateDateTimeStarted(jobID, GetTimeStamp());

         // Show Start Time
         logFile << "\n";
         logFile << " e.Sense Job start. Jobid = " << strJobID << "\n";
         logFile << " User ID\t\t= " << job.UserID() << "\n";
         LgsString TimeStr = GetTime();
         logFile << " Start Date and Time\t= " << TimeStr;

         // Display job's settings
         DisplayJobInfo(job);

         // Run API
         int sentCount = 0;
         int wordCount = 0;
         bool result = RunAPI(job, sentCount, wordCount);

         // Set the status
         job.UpdateStatus(jobID, result? JobControlArguments::StatusCOMPLETED:
                                         JobControlArguments::StatusERROR);
         // Set the completed time 
         job.UpdateDateTimeCompleted(jobID, GetTimeStamp());
         // Set input sentence count
         job.UpdateSentInputCount(jobID, sentCount);
         // Set output sentence count
         job.UpdateSentCurrentComplete(jobID, sentCount);
         // Set word count
         job.UpdateWordCount(jobID, wordCount);

         logFile << "\ne.Sense Job Complete. Job ID was: " << strJobID << "\n";
         TimeStr = GetTime();
         logFile << "TIME: " << TimeStr << "\n";
      }
   } while( processAllJobs );

   // Free SQL connection
   delete pConnection;

   // Show End Time
   LgsString TimeStr = GetTime();
   logFile << "\nLogos Batch End Time is "<< TimeStr << "\n";
   logFile.flush();
   return 0;
}
// -------------------------------------------------------------------
void PrintUsage()
{
   LgsString ErrStr = "You must pass at least two arguments JobID, UserID and optional arguments separated by space\n";
   ErrStr += "Usage: logos_batch";
   ErrStr += " jobid=<jobNumber | all> userid=<userID | all> <optional arguments> <path and filenames>\n";
   ErrStr += " optional argument(s) can be one or more of [save debug time highonly trace logquery]\n";
   ErrStr += " You may also pass the paths and file names:\n";
   ErrStr += "infile=<filename> Replaces standard in\n";
   ErrStr += "outfile=<filename> Replaces standard out\n";
   ErrStr += "errfile=<filename> Replaces standard error\n\n";
   ErrStr += "Example:- logos_batch jobid=all userid=ANewman time outfile=/tmp/log.txt\n\n";
   logFile << ErrStr;
   logFile.flush();
}
// -------------------------------------------------------------------
bool ProcessCmdLine(int argc, char *argv[])
{
   // Check the number of command line arguments 
   if( argc < 3 )
   {
      PrintUsage();
      return false;
   }

   LgsString keyWordPhrase;
   LgsString keyWord;
   int startPos;

   bool bJobID = false;
   bool bUserID = false;
   for( int argNo = 1; argNo < 3; argNo++ )
   {
      keyWordPhrase = StringUtil::lower(argv[argNo]);
      startPos = keyWordPhrase.find("=") + 1;
      keyWord = keyWordPhrase.substr(startPos, keyWordPhrase.length() - startPos);

      if (keyWordPhrase.find("jobid=") != -1)
      {
         bJobID = true;
         if (keyWord == "all")
         {
            processAllJobs = true;
         }
         else
         {
            jobID = StringUtil::asInteger(keyWord);
         }
      }
      else if (keyWordPhrase.find("userid=") != -1)
      {
         bUserID = true;
         userID = keyWord;
         if (userID == "all")
         {
            processAllUsers = true;
         }
      }
   }

   if( !bJobID || !bUserID )
   {
      PrintUsage();
      return false;
   }

   if( argc > 3 )
   {
      for( int argNo = 3; argNo < argc; argNo++ )
      {
         keyWordPhrase = StringUtil::lower(argv[argNo]);
         if (keyWordPhrase == "save")
         {
            saveScratchOption = true;
         }
         else if (keyWordPhrase == "debug")
         {
            debugOption = true;
         }
         else if (keyWordPhrase == "time")
         {
            timeOption = true;
         }
         else if (keyWordPhrase == "highonly")
         {
            highOnlyOption = true;
         }
         else if (keyWordPhrase == "trace")
         {
            traceOption = true;
         }
         else if (keyWordPhrase == "logquery")
         {
            logQueryOption = true;
         }
         else  //the command line argument may define the directories
         {
            startPos = keyWordPhrase.find("=") + 1;
            keyWord = keyWordPhrase.substr(startPos, keyWordPhrase.length() - startPos);

            if (keyWordPhrase.find("outfile=") != -1)
            {
               LgsString File = keyWord;
               OpenStream(1, File);
            }
            else if (keyWordPhrase.find("infile=") != -1)
            {
               LgsString File = keyWord;
               OpenStream(0, File);
            }
            else if (keyWordPhrase.find("errorfile=") != -1)
            {
               LgsString File = keyWord;
               OpenStream(2, File);
            }
         }
      }
   }

   // Show command line arguments
   logFile << "*** Logos_batch";
   for( int i = 1; i < argc; i++ ) logFile << " " << argv[i];
   logFile << "\n";
   logFile.flush();

   return true;
}
// -------------------------------------------------------------------
void DisplayJobInfo(const Ls8Jobs &job)
{
   LgsString jobType = (job.JobType() == 1)? "translation": "wordsearch";

   LgsString sourceLang;
   switch( job.SourceLanguage() )
   {
      case 1: sourceLang = "german";  break;
      case 2: sourceLang = "english"; break;
   }

   LgsString targetLang;
   switch( job.TargetLanguage() )
   {
      case 1: targetLang = "german";     break;
      case 2: targetLang = "english";    break;
      case 3: targetLang = "french";     break;
      case 4: targetLang = "spanish";    break;
      case 5: targetLang = "italian";    break;
      case 6: targetLang = "portuguese"; break;
   }

   LgsString inFile(job.ClientInputFile());
   LgsString outFile(job.ClientOutputFile());
   LgsString alignedFile(job.ClientAlignedFile());
   logFile << " Language\t\t= " << sourceLang << "  " << targetLang << "\n";
   logFile << " Job Type\t\t= " << jobType << "\n";
   logFile << " Input \t\t\t= " << inFile << "\n"; 
   logFile << " Output\t\t\t= " << outFile << "\n";
   logFile << " Aligned\t\t= " << alignedFile << "\n";
   logFile.flush();
}
// -------------------------------------------------------------------
// Function :- InitLgsBatchStreams
// Inputs :- arg1 char (file name to replace stdin)
//           arg2 char (file name to replace stdout)
//           arg2 char (file name to replace stderror)
// Description:- The functions opens the files specified by the arguments and
//               redirect the standard i/o if the file open is sucessful. The NULL
//               value for any of the arguments indicate that that standard i/o
//               corresponding that argument should be used.
// -------------------------------------------------------------------
void InitLgsBatchStreams(char *pszIN, char *pszOUT, char *pszERROR)
{
   if (pszIN)
   {
      inbuf.open(pszIN, ios_base::in);
      if (!inbuf.is_open())
      {
         logFile << "Unable to open input stream\n";
      }
      cin.rdbuf(&inbuf);
   }

   if (pszOUT)
   {
      outbuf.open(pszOUT, ios_base::out);
      if (!outbuf.is_open())
      {
         logFile << "Unable to open output stream\n";
      }
      cout.rdbuf(&outbuf);
   }

   if (pszERROR)
   {
      errbuf.open(pszERROR, ios_base::out);
      if (!errbuf.is_open())
      {
         logFile << "Unable to open error stream\n";
      }
      cerr.rdbuf(&errbuf);
   }
}
// -------------------------------------------------------------------
void OpenStream(int nStream, const LgsString &FName)
{
   switch(nStream)
   {
   case 0: // in stream;
      if (inbuf.is_open())
      {
         inbuf.close();
      }
      inbuf.open(FName.c_str(), ios_base::in);
      if (!inbuf.is_open())
      {
         logFile << "Unable to open input stream\n";
      }
      cin.rdbuf(&inbuf);
      break;
   case 1: // out stream;
      logFile.setLogFile(FName);
      break;
   case 2: // errro stream;
      if (errbuf.is_open())
      {
         errbuf.close();
      }
      errbuf.open(FName.c_str(), ios_base::out);
      if (!errbuf.is_open())
      {
         logFile << "Unable to open error stream\n";
      }
      cerr.rdbuf(&errbuf);
      break;
   }
}
// -------------------------------------------------------------------
LgsString GetTime()
{
   struct tm *newtime;
   time_t aclock;
   LgsString TimeStr; 

   // Get time in 
   time(&aclock);
   newtime = localtime(&aclock);
   TimeStr = asctime(newtime);
   return TimeStr;
}
// -------------------------------------------------------------------
TimeStamp GetTimeStamp()
{
   struct tm *newtime;
   time_t aclock;
   // Get system time 
   time(&aclock);
   //convert into local time
   newtime = localtime(&aclock);

   //convert into TimeStamp structure
   TimeStamp endTime;
   endTime.year = newtime->tm_year+1900;
   endTime.month = newtime->tm_mon+1;
   endTime.day= newtime->tm_mday;
   endTime.hour = newtime->tm_hour;
   endTime.minute =newtime->tm_min;
   endTime.second =newtime->tm_sec;

   return endTime;
}
// -------------------------------------------------------------------
bool RunAPI(const Ls8Jobs &job, int &sentCount, int &wordCount)
{
   bool result = false;
   // Get the only instance of XlationSessionManager by calling the static method
   XlationSessionManager &sessionManager = XlationSessionManager::singleton();
   XlationSession *pSession = sessionManager.createXlationSession();
   if( !pSession )
   {
      // Handle error condition and return
      logFile << "*** Error creating XlationSession...\n";
      logFile.flush();
      // Destroy Session Manager
      sessionManager.destroySessionManager();
      return result;
   }

   // Set all input values for this session
   SetAPIParameters(pSession, job);

   // Start the Session and return without waiting for completion
   logFile << "*** Start XlationSession...\n";
   logFile.flush();
   if( !sessionManager.startXlationSession( pSession, 0 ) )
   {
      // Handle error condition and return
      logFile << "*** Error starting XlationSession: ";
      switch( pSession->getErrorCode() )
      {
         case PROXY_ERR_SERVER:        logFile << "PROXY_ERR_SERVER";        break;
         case SESSION_COUNT_EXCEEDED:  logFile << "SESSION_COUNT_EXCEEDED";  break;
         case INVALID_SESSION:         logFile << "INVALID_SESSION";         break;
         case SESSION_ALREADY_STARTED: logFile << "SESSION_ALREADY_STARTED"; break;
         case INVALID_OPERATION:       logFile << "INVALID_OPERATION";       break;
         case PROXY_ERR_INIT:          logFile << "PROXY_ERR_INIT";          break;
         case PROXY_ERR_CREATE:        logFile << "PROXY_ERR_CREATE";        break;
         case PROXY_ERR_LANGUAGE:      logFile << "PROXY_ERR_LANGUAGE";      break;
         case PROXY_ERR_CALL:          logFile << "PROXY_ERR_CALL";          break;
         case PROXY_ERR_FILE:          logFile << "PROXY_ERR_FILE";          break;
      }
      logFile << "\n";
      logFile.flush();
      // Get error description
      UString error = pSession->getOutputValue(WCERROR_DESC);
      if( !error.empty() )
      {
         Print( "Error code description: ", error);
      }
   }
   else
   {
      result = true;
      // No errors -> Get all output parameters
      UString strSessionID = pSession->getOutputValue(WCSESSION_ID);
      Print( "Session ID     = ", strSessionID);
      UString strSentenceCount = pSession->getOutputValue(WCSENTENCE_COUNT);
      Print( "Sentence count = ", strSentenceCount);
      sentCount = ucToInt(strSentenceCount);
      UString strWordCount = pSession->getOutputValue(WCWORD_COUNT);
      Print( "Word count     = ", strWordCount);
      wordCount = ucToInt(strWordCount);
      // Files
      UString outFileName = pSession->getOutputValue(WCOUTPUT_FILE);
      if( !outFileName.empty() )
      {
         logFile << "Copying output file...\n";
         result &= Move(outFileName, job.OutputFile());
      }
      UString diagFileName = pSession->getOutputValue(WCDIAG_FILE);
      if( !diagFileName.empty() )
      {
         logFile << "Copying diagnostic file...\n";
         result &= Move(diagFileName, job.DiagnosticFile());
      }
      UString alignFileName = pSession->getOutputValue(WCALIGNMENT_FILE);
      if( !alignFileName.empty() )
      {
         logFile << "Copying aligned file...\n";
         result &= Move(alignFileName, job.AlignedFile());
      }
      UString logFileName = pSession->getOutputValue(WCLOG_FILE);
      if( !logFileName.empty() )
      {
         logFile << "Getting log file...\n";
         LgsString str;
         ucToSbs(logFileName, str);
         logFile.AppendFile(str);
         DeleteFile(str.c_str());
      }
   }

   // Free the Session
   sessionManager.freeXlationSession(pSession);
   // Destroy Session Manager
   sessionManager.destroySessionManager();

   return result;
}
// -------------------------------------------------------------------
void SetAPIParameters(XlationSession *pSession, const Ls8Jobs &job)
{
   // Operation type
   pSession->setOperationType((job.JobType() == 1)? TRANSLATE_DOC: TERM_SEARCH_DOC);
   // Input file
   SetInputParameter(pSession, WCINPUT_FILE, job.InputFile());
   // Source language
   switch( job.SourceLanguage() )
   {
      case GERMAN:   pSession->setInputParameter(WCSOURCE_LANGUAGE, WCGER); break;
      case ENGLISH:  pSession->setInputParameter(WCSOURCE_LANGUAGE, WCENG); break;
   }
   // Target language
   switch( job.TargetLanguage() )
   {
      case GERMAN:     pSession->setInputParameter(WCTARGET_LANGUAGE, WCGER); break;
      case ENGLISH:    pSession->setInputParameter(WCTARGET_LANGUAGE, WCENG); break;
      case FRENCH:     pSession->setInputParameter(WCTARGET_LANGUAGE, WCFRE); break;
      case SPANISH:    pSession->setInputParameter(WCTARGET_LANGUAGE, WCSPA); break;
      case ITALIAN:    pSession->setInputParameter(WCTARGET_LANGUAGE, WCITA); break;
      case PORTUGUESE: pSession->setInputParameter(WCTARGET_LANGUAGE, WCPOR); break;
   }
   // Input format
   const LgsString &strFormat = job.InputFormat();
   const wchar_t *wcFormat = WCLGS_TXT_EXCHANGE;
   if( strFormat == CRTF )
   {
      wcFormat = WCRTF;
   }
   else if( strFormat == CSGML )
   {
      wcFormat = WCSGML;
   }
   else if( strFormat == CHTML )
   {
      wcFormat = WCHTML;
   }
   else if( strFormat == CTMX )
   {
      wcFormat = WCTMX;
   }
   else if( strFormat == CXML )
   {
      wcFormat = WCXML;
   }
   else if( strFormat == CINTERLEAF_ASCII )
   {
      wcFormat = WCINTERLEAF_ASCII;
   }
   else if( strFormat == CFRAME_MIF )
   {
      wcFormat = WCFRAME_MIF;
   }
   else if( strFormat == CMS_WORD8 )
   {
      wcFormat = WCMS_WORD8;
   }
   else if( strFormat == CRTF_WINHELP )
   {
      wcFormat = WCRTF_WINHELP;
   }
   pSession->setInputParameter(WCINPUT_FORMAT, wcFormat);
   // Protection character
   SetInputParameter(pSession, WCPROTECTION_CHAR, job.ProtectionCharacter());
   // Source locale
   SetInputParameter(pSession, WCSOURCE_LOCALE, job.SourceLocaleData());
   // Target locale
   SetInputParameter(pSession, WCTARGET_LOCALE, job.TargetLocaleData());
   // Pattern matcher rules
   pSession->setInputParameter(WCPM_RULES, job.PatternRules()? WCYES: WCNO);
   // Extended search
   pSession->setInputParameter(WCEXTENDED_SEARCH, (job.ExtendedSearch() == 2)? WCEXT_SEARCH: WCREG_SEARCH);
   // Subject matter codes
   SetInputParameter(pSession, WCSUBJECT_MATTER_CODES, job.SubjectMatterCodes());
   // Company codes
   SetInputParameter(pSession, WCCOMPANY_CODES, job.CompanyCodes());
   // Flag unfound words
   pSession->setInputParameter(WCFLAG_UNFOUND_WORDS, (job.WordSearchOptions() & FLAG_UNFOUND)? WCYES: WCNO);
   // Generate alignment
   pSession->setInputParameter(WCGENERATE_ALIGNMENT, (!job.AlignedFile().empty())? WCYES: WCNO);
   // Diagnostic level
   switch( job.DiagnosticLevel() )
   {
      case SHORT_DIAG: pSession->setInputParameter(WCDIAG_LEVEL, WCSHORT_DIAG); break;
      case LONG_DIAG:  pSession->setInputParameter(WCDIAG_LEVEL, WCLONG_DIAG); break;
      case DEEP_DIAG:  pSession->setInputParameter(WCDIAG_LEVEL, WCDEEP_DIAG); break;
      case STAT_DIAG:  pSession->setInputParameter(WCDIAG_LEVEL, WCSTAT_DIAG); break;
   }
   // Diagnostic line start
   SetInputParameter(pSession, WCDIAG_LINE_START, StringUtil::asStringFromInt(job.DiagnosticStartLine()));
   // Diagnostic line end
   SetInputParameter(pSession, WCDIAG_LINE_END, StringUtil::asStringFromInt(job.DiagnosticEndLine()));
   // Word search options
   if( job.WordSearchOptions() & (UNFOUND|FOUND_NOUN|FOUND_VERB|FOUND_ADJ|FOUND_ADV) )
   {
      int opt = job.WordSearchOptions();
      const UString plus = L"+";
      UString str;
      if( opt & UNFOUND ) str = WCUNFOUND;
      if( opt & FOUND_NOUN )
      {
         if( !str.empty() ) str+= plus;
         str+= WCFOUND_NOUN;
      }
      if( opt & FOUND_VERB )
      {
         if( !str.empty() ) str+= plus;
         str+= WCFOUND_VERB;
      }
      if( opt & FOUND_ADJ )
      {
         if( !str.empty() ) str+= plus;
         str+= WCFOUND_ADJ;
      }
      if( opt & FOUND_ADV )
      {
         if( !str.empty() ) str+= plus;
         str+= WCFOUND_ADV;
      }
      pSession->setInputParameter(WCWORD_SEARCH_OPTIONS, str.c_str());
   }
   // Word search found start
   SetInputParameter(pSession, WCWORD_SEARCH_FOUND_START, StringUtil::asStringFromInt(job.WordSearchFoundStart()));
   // Word search found limit
   SetInputParameter(pSession, WCWORD_SEARCH_FOUND_LIMIT, StringUtil::asStringFromInt(job.WordSearchFoundLimit()));
   // Word search unfound start
   SetInputParameter(pSession, WCWORD_SEARCH_UNFOUND_START, StringUtil::asStringFromInt(job.WordSearchUnfoundStart()));
   // Word search unfound limit
   SetInputParameter(pSession, WCWORD_SEARCH_UNFOUND_LIMIT, StringUtil::asStringFromInt(job.WordSearchUnfoundLimit()));
   // Target form imperative
   // MHON-5QUFGF
   pSession->setInputParameter(WCTARGET_FORM_IMPERATIVE, (job.TargetForm() == 1)? WCYES: WCNO);
   // Statistics flag
   pSession->setInputParameter(WCSTATISTICS_FLAG, timeOption? WCYES: WCNO);
   // Debug flag
   pSession->setInputParameter(WCDEBUG_FLAG, debugOption? WCYES: WCNO);
   // Database mode flag
   pSession->setInputParameter(WCDB_MODE_FLAG, WCYES);
   // Trace flag
   pSession->setInputParameter(WCTRACE_FLAG, traceOption? WCYES: WCNO);
   // Log Query flag
   pSession->setInputParameter(WCLOG_QUERY_FLAG, logQueryOption? WCYES: WCNO);
   // Save Scratch flag
   pSession->setInputParameter(WCSAVE_SCRATCH_FLAG, saveScratchOption? WCYES: WCNO);
   // Generate transl log file
   pSession->setInputParameter(WCGENERATE_LOG, !logFile.getFileName().empty()? WCYES: WCNO);
   // Main Res1 file
   SetInputParameter(pSession, WCMAIN_RES1_FILE, job.MainRes1());
   // Main Res2 file
   SetInputParameter(pSession, WCMAIN_RES2_FILE, job.MainRes2());
   // Main Res22 file
   SetInputParameter(pSession, WCMAIN_RES22_FILE, job.MainRes22());
   // Main Tran1 file
   SetInputParameter(pSession, WCMAIN_TRAN1_FILE, job.MainTran1());
   // Main Tran2 file
   SetInputParameter(pSession, WCMAIN_TRAN2_FILE, job.MainTran2());
   // Main Tran3 file
   SetInputParameter(pSession, WCMAIN_TRAN3_FILE, job.MainTran3());
   // Main Tran4 file
   SetInputParameter(pSession, WCMAIN_TRAN4_FILE, job.MainTran4());
   // Main Parse1 file
   SetInputParameter(pSession, WCMAIN_PARSE1_FILE, job.MainParse1());
   // Main Parse2 file
   SetInputParameter(pSession, WCMAIN_PARSE2_FILE, job.MainParse2());
   // Main Parse3 file
   SetInputParameter(pSession, WCMAIN_PARSE3_FILE, job.MainParse3());
   // Main Parse4 file
   SetInputParameter(pSession, WCMAIN_PARSE4_FILE, job.MainParse4());
   // Mini Res2 file
   SetInputParameter(pSession, WCMINI_RES2_FILE, job.MiniRes2());
   // Mini Tran1 file
   SetInputParameter(pSession, WCMINI_TRAN1_FILE, job.MiniTran1());
   // Mini Tran2 file
   SetInputParameter(pSession, WCMINI_TRAN2_FILE, job.MiniTran2());
   // Mini Tran3 file
   SetInputParameter(pSession, WCMINI_TRAN3_FILE, job.MiniTran3());
   // Mini Tran4 file
   SetInputParameter(pSession, WCMINI_TRAN4_FILE, job.MiniTran4());
   // Mini Parse1 file
   SetInputParameter(pSession, WCMINI_PARSE1_FILE, job.MiniParse1());
   // Mini Parse2 file
   SetInputParameter(pSession, WCMINI_PARSE2_FILE, job.MiniParse2());
   // Mini Parse3 file
   SetInputParameter(pSession, WCMINI_PARSE3_FILE, job.MiniParse3());
   // Mini Parse4 file
   SetInputParameter(pSession, WCMINI_PARSE4_FILE, job.MiniParse4());
   // User ID
   SetInputParameter(pSession, WCUSER_ID, job.UserID());
}
// -------------------------------------------------------------------
void SetInputParameter(XlationSession *pSession, const wchar_t *name, const LgsString &value)
{
   // Convert to _bstr_t
   _bstr_t bstr_value(value.c_str());
   // Set input parameter   
   pSession->setInputParameter(name, bstr_value);
   //pSession->setInputParameter(name, string(value.c_str()));
}
// -------------------------------------------------------------------
void Print(const LgsString &msg, const UString &value)
{
   LgsString str;
   ucToSbs(value, str);
   logFile << msg << str << "\n";
   logFile.flush();
}
// -------------------------------------------------------------------
void ucToSbs(const UString &ucStr, LgsString &sbString)
{
    char *scStr = new char [ucStr.length()+1];
    wcstombs(scStr, ucStr.c_str(), ucStr.length()+1);
    sbString = scStr;
    delete scStr;
}
// -------------------------------------------------------------------
int ucToInt(const UString &ucStr)
{
   LgsString str;
   ucToSbs(ucStr, str);
   return StringUtil::asInteger(str);
}
// -------------------------------------------------------------------
bool Move(const UString &sourceFile, const LgsString &destFile)
{
   LgsString str;
   ucToSbs(sourceFile, str);

   DeleteFile(destFile.c_str());
   if( !FileUtil::moveFile(str.c_str(), destFile.c_str()) )
   {
      logFile << "Error copying file \"" << str
              << "\" to \"" << destFile << "\"\n";
      logFile.flush();
      return false;
   }

   return true;
}
