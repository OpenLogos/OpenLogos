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
#include <transl/Utils.h>
#include <lgs_db_io/lgsdbcommonobjects.h>
#include <configdatafileinterface/configdatainterfacemain.h>
#include <logos_libs/utility/stringutil.h>
#include <transl/lgstraninterface.h>
#include <time.h>

#ifndef _MSC_VER
#include <sys/stat.h>
#include <sys/wait.h>
#endif

void ErrorOut(const LgsString &ErrMsg)
{
   // copy diag file if it exists to diagnostic file specified in database
   char allDiagFileName[MAX_FILEPATH_LEN];
   GetConfigData("tempfile", "all_diag", allDiagFileName, MAX_FILEPATH_LEN);
   
   if (IfExistFile(allDiagFileName))
   {
      Copy(allDiagFileName, LgsDBCommonObjects::GetJobControlArguments().DiagnosticFile());
   }
   
   // Set the job status to error.
   LgsDBCommonObjects::GetJobControlArguments().Status(JobControlArguments::StatusERROR);
   
   cout << ErrMsg << "\n" ;
}

#ifndef _MSC_VER
int ProfExec(const LgsString &strCmd)
{
  pid_t newpid = fork();
  clock_t dwStart = clock();
  switch (newpid) {
  case -1:
    cout << "Error: Process Fork failed....\n" << strCmd << endl;
    return -1;
  case 0:
    {
      // this is the child: separate the program name from the args
      const char *progname = strCmd.c_str();
      const char *args = "";
      // _fix_me_ do it!
      // This has to be fixed if there command string really contains args
      if (execlp(progname, progname, args, NULL) < 0) {
        cout << "Error: Process Creation failed....\n" << strCmd << "\n";
        return -1;
      }
    }
    break;
  default:
    {
      // this is the parent process
      int status;
      // Wait for child to terminate
      pid_t termpid = waitpid(newpid, &status, 0);
      clock_t dwEnd = clock();
      if (termpid < 0 || ! WIFEXITED(status)) {
        cout << "Process error: Unable to get return code of\n '" 
             << strCmd << endl;
        return -1;
      }
      int retcode = WEXITSTATUS(status);

      long nElapsedTime = ((dwEnd - dwStart) * 1000) / CLOCKS_PER_SEC;
      int nMilliSec = nElapsedTime % 1000;
      nElapsedTime = nElapsedTime / 1000; // in seconds
      int nSec = nElapsedTime % 60; 
      nElapsedTime = nElapsedTime / 60; // in minutes
      int nMin = nElapsedTime % 60; 
      int nHrs = nElapsedTime / 60; // in hours
      cout << "CPU time elapsed (Hours:Minutes:Seconds) = "
           << nHrs <<" : " << nMin << " : " << nSec << "." << nMilliSec 
           << endl;
      return retcode;
    }
  }
}
#else
//--------------------------------------------------------------------------
int ProfExec(const LgsString &strCmd)
{
   DWORD nRetCode;
   STARTUPINFO si;
   PROCESS_INFORMATION pi;
   char cCmdBuf[2048];
   
   LgsString Cmd(strCmd);
   StringUtil::rightTrim(Cmd);
   StringUtil::leftTrim(Cmd); 
   Cmd = ConvertPath(Cmd);
   
   ZeroMemory(&si, sizeof(si));
   si.cb = sizeof(si);
   strcpy(cCmdBuf, Cmd.c_str());
   
   DWORD dwStart = GetTickCount();
   // Start the child process. 
   if (!CreateProcess(NULL,    // No module name (use command line). 
      cCmdBuf, // Command line. 
      NULL,    // Process handle not inheritable. 
      NULL,    // Thread handle not inheritable. 
      FALSE,   // Set handle inheritance to FALSE. 
      0,       // No creation flags. 
      NULL,    // Use parentís environment block. 
      NULL,    // Use parentís starting directory. 
      &si,     // Pointer to STARTUPINFO structure.
      &pi))    // Pointer to PROCESS_INFORMATION structure.
   {
      cout << "Error: Process Creation failed....\n" << Cmd << "\n";
      return -1;
   }
   
   // Wait until child process exits.
   WaitForSingleObject(pi.hProcess, INFINITE);
   DWORD dwEnd = GetTickCount();
   
   if (!GetExitCodeProcess(pi.hProcess, &nRetCode))
   {
      cout << "Process error: Unable to get return code of\n '" << Cmd << "'\n";
      return -1;
   }
   
   DWORD nElapsedTime = dwEnd - dwStart;
   int nMilliSec = nElapsedTime % 1000;
   nElapsedTime = nElapsedTime / 1000; // in seconds
   int nSec = nElapsedTime % 60; 
   nElapsedTime = nElapsedTime / 60; // in minutes
   int nMin = nElapsedTime % 60; 
   int nHrs = nElapsedTime / 60; // in hours
   cout << "CPU time elapsed (Hours:Minutes:Seconds) = "<< nHrs <<" : " << nMin << " : " << nSec << "." << nMilliSec << "\n";
   
   // Close process and thread handles. 
   CloseHandle(pi.hProcess);
   CloseHandle(pi.hThread);
   
   return (int)nRetCode;
}
#endif
//--------------------------------------------------------------------------
LgsString ConvertPath(const LgsString &FName)
{
#if (DIR_SEP_CHAR == '\\')
   int nPos;
   LgsString Temp = FName;
   nPos = Temp.find("/");
   while (nPos != -1)
   {
      Temp.at(nPos) = '\\';
      nPos = Temp.find("/");
   }
   return Temp;
#else
   return FName;
#endif
}

#ifdef _MSC_VER
//***************************************************************************
// checks the existance of a file non zero if file exist and openable.
//***************************************************************************
bool IfExistFile(const LgsString &FName)
{
   DWORD nResult = GetFileAttributes(FName.c_str());
   if( nResult != 0xFFFFFFFF )
   {
      return nResult & FILE_ATTRIBUTE_ARCHIVE;
   }
   return false;
}
//***************************************************************************
// checks the existance of a directory non zero if  exist 
//***************************************************************************
bool IfExistDir(const LgsString &FName)
{
   DWORD nResult = GetFileAttributes(FName.c_str());
   if( nResult != 0xFFFFFFFF )
   {
      return nResult & FILE_ATTRIBUTE_DIRECTORY;
   }
   return false;
}
//********************************************************************************
// Function :- Delete
// Inputs :- arg1 LgsString object (Name of the file to be deleted)
// Return Value :- Return value from the shell invoked indicates the sucess or failure
//                 0 = sucess
// Description:-  This function invokes a shell with the command 'del' and the filename
//********************************************************************************
int Delete(const LgsString &FName)
{
   WIN32_FIND_DATA FindFileData; 
   HANDLE hFind;
   int nRetCode;
   int nFindNext;
   
   char drive[_MAX_DRIVE];
   char dir[_MAX_DIR];
   char fname[_MAX_FNAME];
   char ext[_MAX_EXT];
   LgsString Temp = ConvertPath(FName);
   _splitpath(FName.c_str(), drive, dir, fname, ext);
   
   
   if ((Temp.find('*') != -1) || (Temp.find('?') != -1)) // wild card specified
   {
      hFind = FindFirstFile(Temp.c_str(), &FindFileData);
      if (hFind ==  INVALID_HANDLE_VALUE)
         return -1;
      LgsString Path = drive;
      Path =  Path + dir;
      
      
      LgsString fileName = Path + FindFileData.cFileName;
      DWORD fileAttrib = GetFileAttributes(fileName.c_str());
      fileAttrib &= (~FILE_ATTRIBUTE_READONLY);
      SetFileAttributes(fileName.c_str(), fileAttrib);
      DeleteFile(fileName.c_str());
      nFindNext = 1;
      while(nFindNext)
      {
         nFindNext = FindNextFile(hFind,&FindFileData);
         if (nFindNext)
         {
            fileName = Path + FindFileData.cFileName;
            fileAttrib = GetFileAttributes(fileName.c_str());
            fileAttrib &= (~FILE_ATTRIBUTE_READONLY);
            SetFileAttributes(fileName.c_str(), fileAttrib);
            
            int ntemp = DeleteFile(fileName.c_str());
         }
      }
      FindClose(hFind);
      return 0;
   }
   else if (IfExistFile(Temp))
   {
      nRetCode = DeleteFile(Temp.c_str());
      if (nRetCode)
         return 0;
      else
         return -1;
   }
   else
      return -1;
}
//********************************************************************************
// Function :- Copy
// Inputs :- arg1 LgsString (Source file name)
//           arg2 LgsString (target file name)
// Return Value :- Return value indicates the sucess or failure
//                 TRUE = Sucess.
// Description:- The function copies the contents of source file to target file.
//               Overwrites the target file if exists.
//********************************************************************************
bool Copy(const LgsString &SourceFile, const LgsString &DestFile)
{
   return CopyFile(SourceFile.c_str(), DestFile.c_str(), false);
}
#else
/*************************************************************************
  Checks the existence of a file.
  \returns true if file exist and openable.
**************************************************************************/
bool IfExistFile(const LgsString &FName)
{
  struct stat res;
  if( stat(FName.c_str(), &res) == 0 ) {
    return S_ISREG(res.st_mode) && (res.st_mode & S_IRUSR);
  }
  return false;
}
/***************************************************************************
 checks the existence of a directory.
 \returns true if it exists
 **************************************************************************/
bool IfExistDir(const LgsString &FName)
{
  struct stat res;
  if( stat(FName.c_str(), &res) == 0 ) {
    return S_ISDIR(res.st_mode);
  }
  return false;
}

//*****************************************************************************
// Delete one or more files.
// Invokes a shell with the command 'rm' and the filename
// \param FName LgsString object (Name of the file to be deleted)
// \returns value from the shell invoked indicates the sucess or failure
//*****************************************************************************
int Delete(const LgsString &FName) {
  LgsString command = LgsString("rm ") + FName; 
  pid_t newpid = fork();
  switch (newpid) {
  case -1:
    return -1;
  case 0:
   { // this is the child: Call shell with command and args
     if (execlp("sh", "sh", "-c", command.c_str(), NULL) < 0) {
       return (-1 == 0);
     }
   }
   // this break can never be reached
   break;
  default:
    {
      // this is the parent process
      int status;
      // Wait for child to terminate
      return (((waitpid(newpid, &status, 0) < 0 || ! WIFEXITED(status))
               ? -1
               : WEXITSTATUS(status)) == 0);
    }
  }
}

//*****************************************************************************
// Copy a file.
// Invokes a shell with the command 'cp' and the filenames
// \param FName LgsString object (Name of the file to be deleted)
// \returns value from the shell invoked indicates the sucess or failure
//*****************************************************************************
bool Copy(const LgsString &SourceFile, const LgsString &DestFile)
{
  pid_t newpid = fork();
  switch (newpid) {
  case -1:
    return -1;
  case 0:
    { // this is the child: Call shell with command and args
      string command = (string)"cp -f " + SourceFile.c_str() 
        + " " + DestFile.c_str();
      if (execlp("sh", "sh", "-c", command.c_str(), NULL) < 0) {
        return (-1 == 0);
      }
    }
    // this break can never be reached
    break;
  default:
    {
      // this is the parent process
      int status;
      // Wait for child to terminate
      return (((waitpid(newpid, &status, 0) < 0 || ! WIFEXITED(status))
               ? -1
               : WEXITSTATUS(status)) == 0);
    }
  }
}
#endif
//--------------------------------------------------------------------------
int GetSentCount(const LgsString &fileName, bool openBinary)
{
   LgsString FName(fileName);
   StringUtil::rightTrim(FName);
   StringUtil::leftTrim(FName);
   FName = ConvertPath(FName);
   ifstream fs;
   
   if (openBinary)
   {
#if defined(_MSC_VER)
      fs.open(FName.c_str(), ios::in | ios::binary);
#else
      fs.open(FName.c_str(), ios::in);
#endif
   }
   else
   {
      fs.open(FName.c_str());
   }
   if (!fs)
      return -1;
   
   // count number of sentences (new line).
   int nSentCount = 0;
   
   if (openBinary)
   {
      ILgsSentInfo sentInfo;
      ILgsWordMarkup wordInfo;
      
      while (!fs.eof())
      {
         if (fs.read((char *)&sentInfo, sizeof(ILgsSentInfo)))
         {
            if (sentInfo._totWords > 0)
            {
               char wordBuffer[256];
               wordBuffer[255] = '\0';
               for (int cntr = 0; cntr < sentInfo._totWords; cntr++)
               {
                  fs.read((char *)&wordInfo, sizeof(ILgsWordMarkup));
                  fs.read(wordBuffer, wordInfo._sizeOfWord);
               }
            }
            nSentCount++;
         }
      }
   }
   else
   {
      char c;
      while (fs)
      {
         fs.get(c);
         if ((fs) && (c =='\n'))
         {
            nSentCount++;
         }
      }
   }
   fs.close();
   return nSentCount;
}
