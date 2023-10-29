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
// APITest.cpp : Defines the entry point for the console application.
//
#ifdef _MSC_VER
#include "StdAfx.h"
#endif
#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#include <logos_libs/utility/FileUtil.h>
#endif
#include <time.h>

#include <engine_api/APITest/APIParameterList.h>
#include <engine_api/APITest/Lgsout.h>
#include "config.h"
#include <cstring>
#include <cstdlib>

#ifdef HAVE_STRCASECMP
#define stricmp strcasecmp
#endif

#include <engine_api/xlationinterface/xlationinterfacedefs.h>
#include <engine_api/xlationinterface/xlationsession.h>
#include <engine_api/xlationinterface/xlationsessionmanager.h>

const char *FILE_EXT_OUT   = ".out";
const char *FILE_EXT_DIAG  = ".diag";
const char *FILE_EXT_ALIGN = ".align";
const char *TEXT_EXT_OUT   = ".text";
const char *TEXT_EXT_DIAG  = ".text_diag";
const char *TEXT_EXT_ALIGN = ".text_align";
const char *TEXT_EXT_LANGP = ".text_langp";
const char *TEXT_EXT_COMPC = ".text_cc";
const char *TEXT_EXT_SMCC  = ".text_smc";

void Usage();
string OutputFileName( unsigned long num, bool bID );
long GenerateAllCombinations( const string &outputDir, bool bID );
bool GenerateRobotFile( const string &robotFileName, const string &inputFileName );
bool Move( const UString &sourceFile, const string &destFile );
bool CopyText( const UString &text, const string &destFile );
void Print( const string &msg, const UString &value );
string GetTime();
void ucToSbs(const UString& ucStr, string& sbString);

CLgsOut logFile;
APIParameterList paramList;

int main(int argc, char* argv[])
{
   bool bID = false;
   int iArg = 0;

   // Get parameters
   if( (argc > 1) && ( stricmp( argv[1], "-ID" ) == 0 ) )
   {
      // ID flag
      bID = true;
      iArg++;
   }
   if( argc < (iArg + 4) )
   {
      // Show usage
      Usage();
      return -1;
   }

   // Table file
   string tableFile( argv[iArg + 1] );
   if( !paramList.Load( tableFile.c_str() ) )
   {
      return -1;
   }

   // Output directory
   string outputDir( argv[iArg + 2] );
   if( !outputDir.empty() && outputDir[outputDir.length()-1] != DIR_SEP_CHAR )
   {
      outputDir+= DIR_SEP_CHAR;
   }

   // Log file
   string logFileName( argv[iArg + 3] );
   logFile.setLogFile( logFileName );

   // Robot file
   string robotFile;
   if( argc > (iArg + 4) )
   {
      robotFile = argv[iArg + 4];
   }

   // Print Start Date and Time
   logFile << "*** Start Date and Time: " << GetTime() << "\n";

   // Print all values of all parameters
   logFile << "*** Input file: " << tableFile << "\n";
   paramList.Print( logFile );

   // Calculate number of combinations
   double totalCombinations = paramList.TotalCombinations();
   logFile << "*** Total Jobs = " << totalCombinations << "\n";

   // Generate all combinations
   long ret = 0; // return code
   if( totalCombinations > 0 )
   {
      // Generate Robot output file
      if( robotFile.length() )
      {
         GenerateRobotFile( robotFile, tableFile );
      }
      // Do all the jobs
      ret = GenerateAllCombinations( outputDir, bID );
      // Check errors
      if( ret > 0 )
      {
         logFile << "\n*** Jobs with errors = " << ret << "\n";
         ret = 0;
      }
   }

   // Print End Date and Time
   logFile << "\n*** End Date and Time: " << GetTime() << "\n";
   logFile.flush();

   return ret;
}
// -------------------------------------------------------------------
void Usage()
{
   cout << "Usage: APITest.exe [-ID] table_file output_dir log_file [robot_file]\n"
           "   -ID        - Uses a job ID in the names of output files\n"
           "   table_file - Specifies the input file with API operation parameters\n"
           "   output_dir - Specifies the directory where create all output files\n"
           "   log_file   - Specifies the name of log file\n"
           "   robot_file - Specifies the name of Robot input file\n";
}
// -------------------------------------------------------------------
bool GenerateRobotFile( const string &robotFileName, const string &inputFileName )
{
   logFile << "Generating Robot file \"" << robotFileName << "\"...\n";

   // Create Robot file
   ofstream stream( robotFileName.c_str() );
   if( !stream.is_open() )
   {
      logFile << "Error generating Robot file \"" << robotFileName << "\".\n";
      logFile.flush();
      return false;
   }

   // Start creating combinations
   paramList.ResetCombination();
   unsigned long num = 0;
   do
   {
      num++;
      stream << inputFileName << "," << num;

      // Print current values of all parameters in one row
      paramList.PrintCurRow( stream );

      stream << "\n";
   } while( paramList.SetNextCombination() ); // try to generate the next combination

   stream.close();
   return true;
}
// -------------------------------------------------------------------
string OutputFileName( unsigned long num, bool bID )
{
   // Input file
   string inputFile = paramList.GetParamValue( "INPUT_FILE" );
   if( !inputFile.empty() )
   {
      char fname[_MAX_FNAME];
#ifdef _MSC_VER
      _splitpath( inputFile.c_str(), NULL, NULL, fname, NULL );
      inputFile = fname;
#else
      strcpy(fname,inputFile.c_str());
      char *base = basename(fname);
      inputFile = base;
#endif
   }
   else
   {
      inputFile = "Text";
   }

   // Source language
   string sourceLanguage = paramList.GetParamValue( "SOURCE_LANGUAGE" );
   if( !sourceLanguage.empty() )
   {
      sourceLanguage = sourceLanguage[0];
   }
   
   // Target language
   string targetLanguage = paramList.GetParamValue( "TARGET_LANGUAGE" );
   if( !targetLanguage.empty() )
   {
      targetLanguage = targetLanguage[0];
   }
   
   // Operation type
   string operationType = paramList.GetParamValue( "OPERATION_TYPE" );
   if( (operationType.compare( "TRANSLATE_DOC" ) == 0) ||
       (operationType.compare( "TRANSLATE_TEXT" ) == 0) )
   {
      operationType = "TR";
   }
   else if( (operationType.compare( "TERM_SEARCH_DOC" ) == 0) ||
            (operationType.compare( "TERM_SEARCH_TEXT" ) == 0) )
   {
      operationType = "TS";
   }
   
   // Generate name
   string name = inputFile + "-" + sourceLanguage + targetLanguage + "-" + operationType;
   if( bID )
   {
      // Add a combination ID
      char buf[30];
      // no ultoa in gcc
      sprintf(buf,"%il",num);
      name+= "." + string( buf );
      //name+= "." + string( ultoa( num, buf, 10 ) );
   }

   return name;
}
// -------------------------------------------------------------------
long GenerateAllCombinations( const string &outputDir, bool bID )
{
   long numErrJobs = 0; // number jobs with errors
   // Start creating combinations
   paramList.ResetCombination();
   unsigned long num = 0;
   do
   {
      num++;
      logFile << "\n*** Job # " << num << "\n";
      string path = outputDir + OutputFileName( num, bID );

      // Print current values of all parameters
      paramList.PrintCur( logFile );

      // Get the only instance of XlationSessionManager by calling the static method
      XlationSessionManager &sessionManager = XlationSessionManager::singleton();
      XlationSession *opSession = sessionManager.createXlationSession();
      if( !opSession )
      {
         // Handle error condition and return
         logFile << "*** Error creating XlationSession...\n";
         logFile.flush();
         // Destroy Session Manager
         sessionManager.destroySessionManager();
         return -1;
      }

      // Add all input values to the session
      paramList.SetInputParameters( opSession );
      // Generate transl log file
      opSession->setInputParameter(WCGENERATE_LOG, !logFile.getFileName().empty()? WCYES: WCNO);

      // Start the Session and return without waiting for completion
      logFile << "*** Start XlationSession...\n";
      logFile.flush();
      if( !sessionManager.startXlationSession( opSession, 0 ) )
      {
         // Handle error condition and return
         logFile << "*** Error starting XlationSession: ";
         switch( opSession->getErrorCode() )
         {
            case PROXY_ERR_SERVER:        logFile << "PROXY_ERR_SERVER"; break;
            case SESSION_COUNT_EXCEEDED:  logFile << "SESSION_COUNT_EXCEEDED"; break;
            case INVALID_SESSION:         logFile << "INVALID_SESSION"; break;
            case SESSION_ALREADY_STARTED: logFile << "SESSION_ALREADY_STARTED"; break;
            case INVALID_OPERATION:       logFile << "INVALID_OPERATION"; break;
            case PROXY_ERR_INIT:          logFile << "PROXY_ERR_INIT"; break;
            case PROXY_ERR_CREATE:        logFile << "PROXY_ERR_CREATE"; break;
            case PROXY_ERR_LANGUAGE:      logFile << "PROXY_ERR_LANGUAGE"; break;
            case PROXY_ERR_CALL:          logFile << "PROXY_ERR_CALL"; break;
            case PROXY_ERR_FILE:          logFile << "PROXY_ERR_FILE"; break;
         }
         logFile << "\n";
         logFile.flush();
         // Get error description
         UString error = opSession->getOutputValue( WCERROR_DESC );
         if( !error.empty() )
         {
            Print( "Error code description: ", error );
         }
         // Increment number jobs with errors
         numErrJobs++;
      }
      else
      {
         // No errors -> Get all output parameters
         UString sessionID = opSession->getOutputValue( WCSESSION_ID );
         Print( "Session ID     = ", sessionID );
         UString sentenceCount = opSession->getOutputValue( WCSENTENCE_COUNT );
         Print( "Sentence count = ", sentenceCount );
         UString wordCount = opSession->getOutputValue( WCWORD_COUNT );
         Print( "Word count     = ", wordCount );
         // Files
         bool bCopyResult = true;
         UString outFileName = opSession->getOutputValue( WCOUTPUT_FILE );
         if( !outFileName.empty() )
         {
            logFile << "Copying output file...\n";
            bCopyResult &= Move( outFileName, path + FILE_EXT_OUT );
         }
         UString diagFileName = opSession->getOutputValue( WCDIAG_FILE );
         if( !diagFileName.empty() )
         {
            logFile << "Copying diagnostic file...\n";
            bCopyResult &= Move( diagFileName, path + FILE_EXT_DIAG );
         }
         UString alignFileName = opSession->getOutputValue( WCALIGNMENT_FILE );
         if( !alignFileName.empty() )
         {
            logFile << "Copying aligned file...\n";
            bCopyResult &= Move( alignFileName, path + FILE_EXT_ALIGN );
         }
         // Texts
         UString outText = opSession->getOutputValue( WCOUTPUT_TEXT );
         if( !outText.empty() )
         {
            logFile << "Copying output text...\n";
            bCopyResult &= CopyText( outText, path + TEXT_EXT_OUT );
         }
         UString diagText = opSession->getOutputValue( WCDIAG_TEXT );
         if( !diagText.empty() )
         {
            logFile << "Copying diagnostic text...\n";
            bCopyResult &= CopyText( diagText, path + TEXT_EXT_DIAG );
         }
         UString alignText = opSession->getOutputValue( WCALIGNED_TEXT );
         if( !alignText.empty() )
         {
            logFile << "Copying aligned text...\n";
            bCopyResult &= CopyText( alignText, path + TEXT_EXT_ALIGN );
         }
         UString languagePairsText = opSession->getOutputValue( WCOLANGUAGE_PAIRS );
         if( !languagePairsText.empty() )
         {
            logFile << "Copying language pairs text...\n";
            bCopyResult &= CopyText( languagePairsText, path + TEXT_EXT_LANGP );
         }
         UString companyCodesText = opSession->getOutputValue( WCOCOMPANY_CODES );
         if( !companyCodesText.empty() )
         {
            logFile << "Copying company codes text...\n";
            bCopyResult &= CopyText( companyCodesText, path + TEXT_EXT_COMPC );
         }
         UString smcCodesText = opSession->getOutputValue( WCOSMC_CODES );
         if( !smcCodesText.empty() )
         {
            logFile << "Copying SMC codes text...\n";
            bCopyResult &= CopyText( smcCodesText, path + TEXT_EXT_SMCC );
         }
         // Log file
         UString logFileName = opSession->getOutputValue(WCLOG_FILE);
         if( !logFileName.empty() )
         {
            logFile << "Getting log file...\n";
            string str;
            ucToSbs(logFileName, str);
            logFile.AppendFile(str);
            DeleteFile(str.c_str());
         }
         // Increment number jobs with errors
         if( !bCopyResult )
         {
            numErrJobs++;
         }
      }

      // Free the Session
      sessionManager.freeXlationSession( opSession );

   } while( paramList.SetNextCombination() ); // try to generate the next combination

   // Destroy Session Manager
   XlationSessionManager::singleton().destroySessionManager();

   // Return number jobs with errors
   return numErrJobs;
}
// -------------------------------------------------------------------
bool Move( const UString &sourceFile, const string &destFile )
{
   string str;
   ucToSbs(sourceFile, str);

   DeleteFile(destFile.c_str());
   if( !MoveFile(str.c_str(), destFile.c_str()) )
   {
      logFile << "Error copying file \"" << str
              << "\" to \"" << destFile << "\"\n";
      logFile.flush();
      return false;
   }

   return true;
}
// -------------------------------------------------------------------
bool CopyText( const UString &text, const string &destFile )
{
   string str;
   ucToSbs( text, str );

   ofstream stream( destFile.c_str() );
   if( !stream.is_open() )
   {
      logFile << "Error copying text\n\"" << str
              << "\"\n to file\"" << destFile << "\"\n";
      logFile.flush();
      return false;
   }

   stream << str;
   stream.close();

   return true;
}
// -------------------------------------------------------------------
void Print( const string &msg, const UString &value )
{
   string str;
   ucToSbs( value, str );
   logFile << msg << str << "\n";
   logFile.flush();
}
// -------------------------------------------------------------------
string GetTime()
{
   struct tm *newtime;
   time_t aclock;

   // Get time in 
   time( &aclock );
   newtime = localtime( &aclock );
   return asctime( newtime );
}
// -------------------------------------------------------------------
void ucToSbs(const UString& ucStr, string& sbString)
{
    char *scStr = new char [ucStr.length()+1];
    wcstombs(scStr, ucStr.c_str(), ucStr.length()+1);
    sbString = scStr;
    delete[] scStr;
}
