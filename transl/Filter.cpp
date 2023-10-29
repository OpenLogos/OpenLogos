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
#include <logos_include/logoscommon.h>
#include <transl/Filter.h>
#include <transl/Utils.h>
#include <configdatafileinterface/configdatainterfacemain.h>
#include <lgs_db_io/lgsdbcommonobjects.h>
#include <logos_libs/utility/CEnvVariable.h>
#include <logos_libs/utility/stringutil.h>
#include <errno.h>

#define _flushall() cout.flush(); fflush(NULL)

//--------------------------------------------------------------------------------------------
int Filter(const LgsString &inputFormat, bool isSource, bool timeCmd, bool traceFlag)
{
   int nRetCode;
   LgsString commandLine;
   LgsString convCmd;
   LgsStringVector javaArgs;
   LgsString fileEncoding = "";
   
   if (isSource)
   {
      cout << "******* filter source" << endl;
      cout << "Converting from " << inputFormat << endl;
   }
   else
   {
      cout << "******* filter target" << endl;
      cout << "Converting to " << inputFormat << endl;
   }
   
   // Initialize the java arguments
   char javaExecutable[MAX_FILEPATH_LEN];
   
   GetConfigData("engine", "java_exe", javaExecutable, MAX_FILEPATH_LEN);
   javaArgs.push_back(javaExecutable);
   javaArgs.push_back("-Xmx128m");
   javaArgs.push_back("-cp");
   CEnvVariable evCLASSPATH(CLASSPATH);
   javaArgs.push_back(evCLASSPATH.GetValue());
   
   // choose preproc task based on format
   if (inputFormat == "lgssgml")
   {
      SetLGSSGML(isSource, commandLine);
   }
   else if ((inputFormat == "sgml") || (inputFormat == "sgmlv4") || (inputFormat == "xml"))
   {
      //For SGML filters we have to determine the File Encoding in order to support Otello text files
      fileEncoding = GetEncoding(javaArgs, traceFlag);
      SetSGML(isSource, javaArgs);
   }
   else if (inputFormat == "ileaf")
   {
      SetILEAF(isSource, javaArgs);
   }
   else if ((inputFormat == "mif") || (inputFormat == "frame"))
   {
      SetMIF(isSource, javaArgs);
   }
   else if ((inputFormat == "rtf") || (inputFormat == "rtfhelp") || (inputFormat == "rtfv4"))
   {
      SetRTF(isSource, javaArgs, inputFormat);
   }
   else if (inputFormat == "word8")
   {
      SetWord8(isSource, javaArgs, convCmd);
   }
   else if (inputFormat == "tmx")
   {
      fileEncoding = GetEncoding(javaArgs, traceFlag);
      SetTmx(isSource, javaArgs);
   }
   else // invalid input format.
   {
      cout << "Logos_filter.exe: Error! Invalid Input format :- \"" <<  inputFormat << "\"" << endl ;
      return -1;
   }
   
   char returnCodeFile[MAX_FILEPATH_LEN];
   GetConfigData("tempfile", "retcode_file", returnCodeFile, MAX_FILEPATH_LEN);
   
   javaArgs.push_back(returnCodeFile);
   if (fileEncoding.length())
   {
      javaArgs.push_back(fileEncoding);
   }
   
   // run the filter
   nRetCode = 0;
   if (isSource)
   {
      if (inputFormat == "word8") // run the pre convert filter in case of MSWord document
      {
         cout << "Converting ms-word to rtf format." << endl;
         
         if (timeCmd)
         {
            nRetCode = ProfExec(convCmd);
         }
         else
         {
            nRetCode = SystemExec(convCmd);
         }
         
         if (nRetCode != 0) 
         {
            cout << "Error converting from word document format. Error Code = " << nRetCode << endl;
            return nRetCode;
         }
         cout << "Converting rtf to lgssgml format." << endl;
      }
      
      _flushall();
      
      if (inputFormat == "lgssgml")
      {
         nRetCode = SystemExec(commandLine);
      }
      else
      {
         nRetCode = java_run(javaArgs, timeCmd, traceFlag);
      }
      
      // check the return code and exit if error
      if (nRetCode != 0)
      {
         cout << "Error executing filter command ... \n Return Code= " << nRetCode << endl;
         return nRetCode;
      }
   }
   else // the target.
   {
      if (inputFormat == "word8")
      {
         cout << "Converting lgssgml to rtf format." << endl;
      }
      
      _flushall();
      
      if (inputFormat == "lgssgml")
      {
         nRetCode = SystemExec(commandLine);
      }
      else
      {
         nRetCode = java_run(javaArgs, timeCmd, traceFlag);
      }
      
      // check the return code and exit if error
      if (nRetCode != 0)
      {
         cout << "Error executing filter command ... \n Return Code= " << nRetCode << endl;
         return nRetCode;
      }
      
      if (inputFormat == "word8") // run the post convert filter
      {
         cout << "Converting rtf to ms-word format. "<< endl;
         
         if (timeCmd && !(inputFormat == "lgssgml"))
         {
            nRetCode = ProfExec(convCmd);
         }
         else
         {
            nRetCode = SystemExec(convCmd);
         }
         
         if (nRetCode != 0) 
         {
            cout << "Error converting from to document format. Error Code = " << nRetCode << endl;
         }
      }
   }
   
   return nRetCode;
}

//--------------------------------------------------------------------------------------------
void SetLGSSGML(bool isSource, LgsString &command)
{
   char fileName1[MAX_FILEPATH_LEN];
   char fileName2[MAX_FILEPATH_LEN];
   
   if (isSource)
   {
      GetConfigData("tempfile", "filter_input", fileName1, MAX_FILEPATH_LEN);
      GetConfigData("tempfile", "transl_input", fileName2, MAX_FILEPATH_LEN);
   }
   else
   {
      GetConfigData("tempfile", "transl_output", fileName1, MAX_FILEPATH_LEN);
      GetConfigData("tempfile", "filter_output", fileName2, MAX_FILEPATH_LEN);
   }
   
   command = 
#ifdef _MSC_VER
     "copy " 
#else
     "cp "
#endif
     + LgsString(fileName1) + " " + LgsString(fileName2);
}

//--------------------------------------------------------------------------------------------
void SetXML(bool isSource, LgsStringVector &args)
{
   char tagFile[MAX_FILEPATH_LEN];
   GetConfigData("filter", "html_tagtbl", tagFile, MAX_FILEPATH_LEN);
   
   if (isSource) 
   {  
      char filterInputFile[MAX_FILEPATH_LEN];
      char translInputFile[MAX_FILEPATH_LEN];
      GetConfigData("tempfile", "filter_input", filterInputFile, MAX_FILEPATH_LEN);
      GetConfigData("tempfile", "transl_input", translInputFile, MAX_FILEPATH_LEN);
      args.push_back("Html2LgsSgml");
      args.push_back(filterInputFile);
      args.push_back(translInputFile);
      args.push_back(tagFile);
   }
   else
   {
      char translOutputFile[MAX_FILEPATH_LEN];
      char filterOutputFile[MAX_FILEPATH_LEN];
      GetConfigData("tempfile", "transl_output", translOutputFile, MAX_FILEPATH_LEN);
      GetConfigData("tempfile", "filter_output", filterOutputFile, MAX_FILEPATH_LEN);
      args.push_back("LgsSgml2Html");
      args.push_back(translOutputFile);
      args.push_back(filterOutputFile);
      args.push_back(tagFile);
   }
}
//--------------------------------------------------------------------------------------------
void SetTmx(bool isSource, LgsStringVector &args)
{

   if (isSource) {
// in, out, tags, target lang, (retcode, encoding - from GetEncoding)
// Encoding should be ALWAYS Unicode, or so it says
// in TMX description, but we disregard this
// according to "be strict in what you generate and forgiving
// when parsing what others generate".
      char filterInputFile[MAX_FILEPATH_LEN];
      char translInputFile[MAX_FILEPATH_LEN];

	  char *targetLang;
      int tl = LgsDBCommonObjects::GetJobControlArguments().TargetLanguage();
	  switch(tl) {
		case 1: targetLang = "DE"; break;
		case 2: targetLang = "EN"; break;
		case 3: targetLang = "FR"; break;
		case 4: targetLang = "ES"; break;
		case 5: targetLang = "IT"; break;
		case 6: targetLang = "PT"; break;
		default: targetLang = "FR"; break;
      }

      GetConfigData("tempfile", "filter_input", filterInputFile, MAX_FILEPATH_LEN);
      GetConfigData("tempfile", "transl_input", translInputFile, MAX_FILEPATH_LEN);
      args.push_back("Tmx2LgsSgml");
      args.push_back(filterInputFile);
      args.push_back(translInputFile);
      args.push_back(targetLang);
   }
   else
   {
      char translOutputFile[MAX_FILEPATH_LEN];
      char filterOutputFile[MAX_FILEPATH_LEN];
      GetConfigData("tempfile", "transl_output", translOutputFile, MAX_FILEPATH_LEN);
      GetConfigData("tempfile", "filter_output", filterOutputFile, MAX_FILEPATH_LEN);
      args.push_back("LgsSgml2Tmx");
      args.push_back(translOutputFile);
      args.push_back(filterOutputFile);
   }
}

//--------------------------------------------------------------------------------------------
void SetSGML(bool isSource, LgsStringVector &args)
{
   char tagFile[MAX_FILEPATH_LEN];
   GetConfigData("filter", "html_tagtbl", tagFile, MAX_FILEPATH_LEN);
   
   if (isSource)
   {  
      char filterInputFile[MAX_FILEPATH_LEN];
      char translInputFile[MAX_FILEPATH_LEN];
      GetConfigData("tempfile", "filter_input", filterInputFile, MAX_FILEPATH_LEN);
      GetConfigData("tempfile", "transl_input", translInputFile, MAX_FILEPATH_LEN);
      args.push_back("Html2LgsSgml");
      args.push_back(filterInputFile);
      args.push_back(translInputFile);
      args.push_back(tagFile);
   }
   else
   {
      char translOutputFile[MAX_FILEPATH_LEN];
      char filterOutputFile[MAX_FILEPATH_LEN];
      GetConfigData("tempfile", "transl_output", translOutputFile, MAX_FILEPATH_LEN);
      GetConfigData("tempfile", "filter_output", filterOutputFile, MAX_FILEPATH_LEN);
      args.push_back("LgsSgml2Html");
      args.push_back(translOutputFile);
      args.push_back(filterOutputFile);
      args.push_back(tagFile);
   }
}

//--------------------------------------------------------------------------------------------
void SetRTF(bool isSource, LgsStringVector &args, const LgsString &inputFormat)
{
   if (isSource) 
   {  
      char tagFile1[MAX_FILEPATH_LEN];
      char tagFile2[MAX_FILEPATH_LEN];
      char filterInputFile[MAX_FILEPATH_LEN];
      char translInputFile[MAX_FILEPATH_LEN];
      GetConfigData("filter", "rtf_tagtbl", tagFile1, MAX_FILEPATH_LEN);
      GetConfigData("filter", "rtf_mac_charset_tagtbl", tagFile2, MAX_FILEPATH_LEN);
      GetConfigData("tempfile", "filter_input", filterInputFile, MAX_FILEPATH_LEN);
      GetConfigData("tempfile", "transl_input", translInputFile, MAX_FILEPATH_LEN);
      
      // check the input format ie win help or rtf.
      LgsString keyWord;
      if (inputFormat == "rtfhelp")
      {
         keyWord = "wh";
      }
      else
      {
         keyWord = "rtf";
      }
      args.push_back("Rtf2LgsSgml");
      args.push_back(filterInputFile);
      args.push_back(translInputFile);
      args.push_back(tagFile1);
      args.push_back(tagFile2);
      args.push_back(keyWord);
   }
   else
   {
      char translOutputFile[MAX_FILEPATH_LEN];
      char filterOutputFile[MAX_FILEPATH_LEN];
      GetConfigData("tempfile", "transl_output", translOutputFile, MAX_FILEPATH_LEN);
      GetConfigData("tempfile", "filter_output", filterOutputFile, MAX_FILEPATH_LEN);
      args.push_back("LgsSgml2Rtf");
      args.push_back(translOutputFile);
      args.push_back(filterOutputFile);
   }
}

//--------------------------------------------------------------------------------------------
void SetWord8(bool isSource, LgsStringVector &args, LgsString &convCmd)
{
   if (isSource) 
   {
      char tagFile1[MAX_FILEPATH_LEN];
      char tagFile2[MAX_FILEPATH_LEN];
      char filterInputFile[MAX_FILEPATH_LEN];
      char translInputFile[MAX_FILEPATH_LEN];
      char wordRtf[MAX_FILEPATH_LEN];
      GetConfigData("filter", "rtf_tagtbl", tagFile1, MAX_FILEPATH_LEN);
      GetConfigData("filter", "rtf_mac_charset_tagtbl", tagFile2, MAX_FILEPATH_LEN);
      GetConfigData("tempfile", "filter_input", filterInputFile, MAX_FILEPATH_LEN);
      GetConfigData("tempfile", "transl_input", translInputFile, MAX_FILEPATH_LEN);
      GetConfigData("tempfile", "pre_word8", wordRtf, MAX_FILEPATH_LEN);
      
      convCmd = "lgs_doc_rtf_convert.exe d2r " + LgsString(filterInputFile) + " " + LgsString(wordRtf);
      args.push_back("Rtf2LgsSgml");
      args.push_back(wordRtf);
      args.push_back(translInputFile);
      args.push_back(tagFile1);
      args.push_back(tagFile2);
      args.push_back("rtf");
   }
   else // Target
   {
      char translOutputFile[MAX_FILEPATH_LEN];
      char filterOutputFile[MAX_FILEPATH_LEN];
      char rtfWord[MAX_FILEPATH_LEN];
      GetConfigData("tempfile", "transl_output", translOutputFile, MAX_FILEPATH_LEN);
      GetConfigData("tempfile", "filter_output", filterOutputFile, MAX_FILEPATH_LEN);
      GetConfigData("tempfile", "post_word8", rtfWord, MAX_FILEPATH_LEN);
      args.push_back("LgsSgml2Rtf");
      args.push_back(translOutputFile);
      args.push_back(rtfWord);
      convCmd = "lgs_doc_rtf_convert.exe r2d " + LgsString(rtfWord) + " " + LgsString(filterOutputFile);    
   }
}

//--------------------------------------------------------------------------------------------
void SetILEAF(bool isSource, LgsStringVector &args)
{
   char tagFile[MAX_FILEPATH_LEN];
   GetConfigData("filter", "ileaf_tagtbl", tagFile, MAX_FILEPATH_LEN);
   
   if (isSource) 
   {  
      char filterInputFile[MAX_FILEPATH_LEN];
      char translInputFile[MAX_FILEPATH_LEN];
      GetConfigData("tempfile", "filter_input", filterInputFile, MAX_FILEPATH_LEN);
      GetConfigData("tempfile", "transl_input", translInputFile, MAX_FILEPATH_LEN);
      args.push_back("Ileaf2LgsSgml");
      args.push_back(filterInputFile);
      args.push_back(translInputFile);
      args.push_back(tagFile);
   }
   else
   {
      char translOutputFile[MAX_FILEPATH_LEN];
      char filterOutputFile[MAX_FILEPATH_LEN];
      GetConfigData("tempfile", "transl_output", translOutputFile, MAX_FILEPATH_LEN);
      GetConfigData("tempfile", "filter_output", filterOutputFile, MAX_FILEPATH_LEN);
      args.push_back("LgsSgml2Ileaf");
      args.push_back(translOutputFile);
      args.push_back(filterOutputFile);
      args.push_back(tagFile);
   }
}

//--------------------------------------------------------------------------------------------
void SetMIF(bool isSource, LgsStringVector &args)
{
   char tagFile[MAX_FILEPATH_LEN];
   char hashTable[MAX_FILEPATH_LEN];
   GetConfigData("filter", "mif_tagtbl", tagFile, MAX_FILEPATH_LEN);
   GetConfigData("tempfile", "hash_table", hashTable, MAX_FILEPATH_LEN);
   
   if (isSource) 
   {  
      char filterInputFile[MAX_FILEPATH_LEN];
      char translInputFile[MAX_FILEPATH_LEN];
      GetConfigData("tempfile", "filter_input", filterInputFile, MAX_FILEPATH_LEN);
      GetConfigData("tempfile", "transl_input", translInputFile, MAX_FILEPATH_LEN);
      args.push_back("Mif2LgsSgml");
      args.push_back(filterInputFile);
      args.push_back(translInputFile);
      args.push_back(tagFile);
      args.push_back(hashTable);
   }
   else
   {
      char translOutputFile[MAX_FILEPATH_LEN];
      char filterOutputFile[MAX_FILEPATH_LEN];
      GetConfigData("tempfile", "transl_output", translOutputFile, MAX_FILEPATH_LEN);
      GetConfigData("tempfile", "filter_output", filterOutputFile, MAX_FILEPATH_LEN);
      args.push_back("LgsSgml2Mif");
      args.push_back(translOutputFile);
      args.push_back(filterOutputFile);
      args.push_back(tagFile);
      args.push_back(hashTable);
   }
}

//--------------------------------------------------------------------------------------------
int SystemExec(LgsString command)
{
   StringUtil::rightTrim(command);
   StringUtil::leftTrim(command);
   command = ConvertPath(command);
   int RetCode = system(command.c_str());
   return RetCode;
}

//--------------------------------------------------------------------------------------------
LgsString GetEncoding(LgsStringVector &args, bool traceFlag) 
{
   char fileEncoding[MAX_FILEPATH_LEN];
   char filterInputFile[MAX_FILEPATH_LEN];
   char encodingFile[MAX_FILEPATH_LEN];
   char returnCodeFile[MAX_FILEPATH_LEN];
   GetConfigData("tempfile", "filter_input", filterInputFile, MAX_FILEPATH_LEN);
   GetConfigData("tempfile", "sgml_encoding", encodingFile, MAX_FILEPATH_LEN);
   GetConfigData("tempfile", "retcode_file", returnCodeFile, MAX_FILEPATH_LEN);
   args.push_back("FileEncoding");
   args.push_back(filterInputFile);
   args.push_back(encodingFile);
   args.push_back(returnCodeFile);
   
   _flushall();
   
   // check the return code and exit if error
   int nRetCode = java_run(args, false, traceFlag);
   if (nRetCode != 0)
   {
      cout << "Error executing command in GetEncoding... \n Return Code= " << nRetCode << endl;
   }
   else
   {
      //get the encoding scheme and set the environmental variable LGS_FILE_ENCODING
      FILE *fp;
      fp = fopen(encodingFile, "r");
      if (fp == NULL)
      {
         cout << "Error: Unable to open EncodingScheme Code File " << encodingFile << endl;
      }
      else
      {
         // Read and return the return code from the file.
         int nResult;
         char enCoding[25];
         nResult = fscanf(fp, "%s", enCoding);
         if ((nResult == 0) || (nResult == EOF)) 
         {
            cout << "Error: Unable to read the return code from file " << encodingFile << endl;
         }
         else
         {
            strcpy(fileEncoding, enCoding);
         }
         fclose(fp);
      }
   }
   
   // Remove the arguments put in the java vector.
   for (int iter = 1; iter <= 4; iter++)
   {
      args.pop_back();
   }
   return fileEncoding;
}

//--------------------------------------------------------------------------------------------
int java_run(LgsStringVector &argVec, bool timeCmd, bool traceFlag)
{
   LgsStringIterator iter;
   int cntr;

   if (traceFlag)
   {
      cout << " ----------Java run invoked---------------" << endl;
      cout << "Number of arguments is " << argVec.size() << endl;
      cout << "program Name is " << argVec.front() << endl;
      cout.flush();
   }
   
   if (traceFlag)
   {
      for (iter = argVec.begin(), cntr = 0; iter != argVec.end(); iter++, cntr++)
      {
         cout << "arg " << cntr << "=" << *iter << endl;
      }
      cout << "-----Java Run is spawning java run time \" java \"------" << endl << flush;
      cout.flush();
   }
   
   LgsString Cmd = "";
   bool firstArg = true;
   for (iter = argVec.begin(); iter != argVec.end(); iter++)
   {
      if (firstArg)
      {
         Cmd = *iter;
         firstArg = false;
      }
      else
      {
         Cmd = Cmd + " \"" + *iter + "\"";
      }
   }
   
   if (traceFlag)
   {
      cout << "Command = " << Cmd.c_str() << endl;
   }
   
   int nRetCode;
   if (timeCmd)
   {
      nRetCode = ProfExec(Cmd);
   }
   else
   {
      nRetCode = SystemExec(Cmd);
   }
   
   if (nRetCode == -1)
   {
      cout <<"Error spawning java runtime. System Error Code errno = " << errno << endl;
      cout << strerror(errno) << endl;
      cout << "Debug: dumping the arguments to java..." << endl;
      for (iter = argVec.begin(), cntr = 0; iter != argVec.end(); iter++, cntr++)
      {
         cout << "arg " << cntr << "=" << *iter << endl;
      }
      cout << "Debug: dumping java command line ..." << endl;
      cout << "Command = " << Cmd << endl;
      cout.flush();
      return -1;
   }
   
   char returnCodeFile[MAX_FILEPATH_LEN];
   GetConfigData("tempfile", "retcode_file", returnCodeFile, MAX_FILEPATH_LEN);
   
   // open the ret code  file.
   FILE *fp;
   fp = fopen(returnCodeFile, "r");
   if (fp == NULL) 
   {
      cout << "Error: Unable to open Return Code File " << returnCodeFile << endl;
      cout << "Debug: Dumping the arguments to java...\n" ;
      for (iter = argVec.begin(), cntr = 0; iter != argVec.end(); iter++, cntr++)
      {
         cout << "arg " << cntr << "=" << *iter << endl;
      }
      cout.flush();
      return 1;
   }
   
   // Read and return the return code from the file.
   int nRetValue;
   int nResult;
   nResult = fscanf(fp, "%d", &nRetValue);
   if ((nResult == 0) || (nResult == EOF)) 
   {
      cout << "Error: Unable to read the return code from file " << returnCodeFile << endl;
      nRetValue = 1;
   }
   fclose(fp);
   return nRetValue;
}
