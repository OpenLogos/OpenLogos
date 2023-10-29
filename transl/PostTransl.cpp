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
// File - PostTransl.cpp
//---------------------------------------------------------------------
#include <logos_include/logoscommon.h>
#include <transl/AlignOutput.h>
#include <transl/Filter.h>
#include <transl/DiagCombine.h>
#include <transl/Utils.h>
#include <lgs_db_io/lgsdbcommonobjects.h>
#include <logos_libs/utility/stringutil.h>
#include <configdatafileinterface/configdatainterfacemain.h>

bool CopyOutput();
void GoodOut(int jobID, bool saveScratchFlag);

int PostTransl()
{
   int nResult = 0;
   
   int jobID = LgsDBCommonObjects::GetJobControlArguments().JobID();
   bool timeOption = LgsDBCommonObjects::GetJobControlArguments().TimeFlag();
   bool traceOption = LgsDBCommonObjects::GetJobControlArguments().TraceFlag();
   bool saveScratchOption = LgsDBCommonObjects::GetJobControlArguments().SaveScratchFlag();
   
   // Just in case the job has been cancelled, we don't need to complete the process.
   if (LgsDBCommonObjects::GetJobControlArguments().Status() == JobControlArguments::StatusRUNNING)
   {
      if (LgsDBCommonObjects::GetJobControlArguments().RunMode() == JobControlArguments::translationMode)
      {
         int sentInputCount = LgsDBCommonObjects::GetJobControlArguments().SentInputCount();
         int sentOutputCount = LgsDBCommonObjects::GetJobControlArguments().SentCurrentComplete();
         if (sentInputCount != sentOutputCount)
         {
            cout << "Error: The number of input sentences does not equal number of output sentences.\n"; 
         }
         cout << "Input/Output Sentence Count = " << sentInputCount << "  " << sentOutputCount << "\n";
         
         // merge the format information with the translated text
         LgsString inputFormat = LgsDBCommonObjects::GetJobControlArguments().InputFormat();
         nResult = Filter(inputFormat, false, timeOption, traceOption);
         
         if (traceOption)
         {
            cout << "\n************End Filter Target RetCode = " << nResult << " ***********\n\n";
            cout.flush();
         }
         
         if (nResult != 0)
         {
            ErrorOut("filter target: " + StringUtil::asStringFromInt(nResult));
         }
         else
         {
            // if aligned output is requested as well then generate it
            if (LgsDBCommonObjects::GetJobControlArguments().AlignedFile().length())
            {
               AlignOutput();
            }
         }
         cout.flush();
      }
      
      CopyOutput();
      GoodOut(jobID, saveScratchOption);
      LgsDBCommonObjects::GetJobControlArguments().Status(JobControlArguments::StatusCOMPLETED);
   }
   else
   {
      cout << "e.Sense Job Terminated By Client. Job ID was: " << StringUtil::asStringFromInt(jobID) << "\n";
   }
   
   return nResult;
}
//---------------------------------------------------------------------
bool CopyOutput()
{
   bool returnResult = true;
   
   // Place output from job in jobfile directory
   char outputFile[MAX_FILEPATH_LEN];
   
   if (LgsDBCommonObjects::GetJobControlArguments().JobType() == 2) // wordsearch
   {
      GetConfigData("tempfile", "transl_output", outputFile, MAX_FILEPATH_LEN);
      int nWS_LINE_COUNT = GetSentCount(outputFile, false);
      cout << "\nTerm list count = " << nWS_LINE_COUNT << "\n";
   }
   else
   {
      GetConfigData("tempfile", "filter_output", outputFile, MAX_FILEPATH_LEN);
   }
   
   if (!IfExistFile(outputFile))
   {
      ErrorOut(" No output to copy to job file directory. OUTPUT_FILE=" + LgsString(outputFile));
      returnResult = false;
   }
   int jobID = LgsDBCommonObjects::GetJobControlArguments().JobID();
   cout << "Copying " << StringUtil::asStringFromInt(jobID) << " output to Job File directory." "\n";
   bool bRetCode = Copy(outputFile, LgsDBCommonObjects::GetJobControlArguments().OutputFile());
   if (!bRetCode)
   {
      ErrorOut("Error copying output. OUTPUT_FILE=" + LgsString(outputFile));
      returnResult = false;
   }
   
   // copy aligned output file to Job File directory
   if ((LgsDBCommonObjects::GetJobControlArguments().AlignedFile().length()) &&
      (LgsDBCommonObjects::GetJobControlArguments().RunMode() == JobControlArguments::translationMode))
   {
      char alignedOutput[MAX_FILEPATH_LEN];
      GetConfigData("tempfile", "alignedOutput", alignedOutput, MAX_FILEPATH_LEN);
      if (!IfExistFile(alignedOutput))
      {
         ErrorOut("Error copying aligned output to job file directory. FILE=" + LgsString(alignedOutput));
         returnResult = false;
      }
      bRetCode = Copy(alignedOutput, LgsDBCommonObjects::GetJobControlArguments().AlignedFile());
      if (!bRetCode)
      {
         ErrorOut("Error copying aligned output to Job File directory. FILE=" + LgsString(alignedOutput));
         returnResult = false;
      }
   }
   return returnResult;
}
//---------------------------------------------------------------------
void GoodOut(int jobID, bool saveScratchFlag)
{
   int nDiagLVL = LgsDBCommonObjects::GetJobControlArguments().DiagnosticLevel();
   if (nDiagLVL > 1)
   {
      if (DiagCombine(nDiagLVL != 5) == 0)
      {
         // copy diag file if it exists to diagnostic file specified in database
         char allDiagFileName[MAX_FILEPATH_LEN];
         GetConfigData("tempfile", "all_diag", allDiagFileName, MAX_FILEPATH_LEN);
         
         if (IfExistFile(allDiagFileName))
         {
            cout << "Copying " << StringUtil::asStringFromInt(jobID) << " diagnostic file output to job file directory" << "\n" ;
            Copy(allDiagFileName, LgsDBCommonObjects::GetJobControlArguments().DiagnosticFile());
         }
      }
   }
   
   // erase all files for this job from the scratch directory but not if save option is on
   if (!saveScratchFlag)
   {
      Delete(LgsDBCommonObjects::GetServerProperties().ScratchFileDirectory()
             + DIR_SEP_CHAR + StringUtil::asStringFromInt(jobID) + ".*");
   }
}
