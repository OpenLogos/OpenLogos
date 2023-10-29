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
//-------------------------------------------------------------------
// File - JobControlArguments.cpp
//
// Class - JobControlArguments (implementation)
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <lgs_db_io/jobcontrolarguments.h>
#include <logos_libs/utility/stringutil.h>

#ifdef HAVE_MMAP
#include "logos_include/memory_mapper.h"
#endif
#include <errno.h>
#define TrimStore(source,target) TrimAndStore(source, target, sizeof(target))

//-------------------------------------------------------------------
// Construction/Destruction
//-------------------------------------------------------------------

const char *FILE_MAP_NAME = "TransL_JobControlArguments";
//const int   FILE_MAP_SIZE = sizeof(JobControlArguments);

bool JobControlArguments::m_bCreateSharedMemory = false;
JobControlArguments *JobControlArguments::m_pJobControlArguments = NULL;
#ifdef HAVE_MMAP
static memory_mapper JCAmapper;
SHM_HANDLE JobControlArguments::m_hFileMap = &JCAmapper;
#else
SHM_HANDLE JobControlArguments::m_hFileMap = NULL;
#endif

//-------------------------------------------------------------------
JobControlArguments::JobControlArguments()
{
}
//-------------------------------------------------------------------
JobControlArguments::~JobControlArguments()
{
   DestroyObject();
}
//-------------------------------------------------------------------
JobControlArguments *JobControlArguments::CreateObject(bool createSharedMemory)
{
   if (m_pJobControlArguments == NULL)
   {
      m_bCreateSharedMemory = createSharedMemory;
      m_pJobControlArguments = new(std::nothrow) JobControlArguments();
      // overloaded operator new
      if(createSharedMemory)
      {
         m_pJobControlArguments->m_bDatabaseMode = true;
      }
   }

   return m_pJobControlArguments;
}
//-------------------------------------------------------------------
JobControlArguments *JobControlArguments::GetObject()
{
   return m_pJobControlArguments;
}
//-------------------------------------------------------------------
#ifdef _MSC_VER
void JobControlArguments::DestroyObject()
{
   if (m_pJobControlArguments != NULL)
   {
      UnmapViewOfFile(m_pJobControlArguments);
      CloseHandle(m_hFileMap);

      m_hFileMap = NULL;
      m_pJobControlArguments = NULL;
   }
}
//-------------------------------------------------------------------
void *JobControlArguments::operator new(size_t sz)
{
   if (m_bCreateSharedMemory)
   {
      // Create shared memory
      m_hFileMap = CreateFileMapping((HANDLE)0xFFFFFFFF, // handle to file to map
                                      NULL,               // optional security attributes
                                      PAGE_READWRITE,     // protection for mapping object
                                      0,                  // high-order 32 bits of object size
                                      sz,                 // low-order 32 bits of object size
                                      FILE_MAP_NAME       // name of file-mapping object
                                   );
      if (m_hFileMap == NULL)
      {
         fprintf(stderr, "JobControlArguments: CreateFileMapping() error=%d\n", 
                  GetLastError());
         return NULL;
      }
   }
   else
   {
      // Open shared memory
      m_hFileMap = OpenFileMapping(FILE_MAP_WRITE, // access mode
                                    FALSE,          // inherit flag
                                    FILE_MAP_NAME   // pointer to name of file-mapping object
                                 );
      if (m_hFileMap == NULL)
      {
         fprintf(stderr, "JobControlArguments: OpenFileMapping() error=%d\n", 
                  GetLastError());
         return NULL;
      }
   }

   // Map it
   void *ptr = MapViewOfFile(m_hFileMap,       // file-mapping object to map into address space
                              FILE_MAP_WRITE,   // access mode
                              0,                // high-order 32 bits of file offset
                              0,                // low-order 32 bits of file offset
                              sz                // number of bytes to map
                           );
   if (ptr == NULL)
   {
      fprintf(stderr, "JobControlArguments: MapViewOfFile() error=%d\n", 
               GetLastError());
      CloseHandle(m_hFileMap);
   }

   return ptr;
}
#endif

#ifdef HAVE_MMAP
void JobControlArguments::DestroyObject()
{
   if (m_pJobControlArguments != NULL)
   {
     m_hFileMap->close();
     m_pJobControlArguments = NULL;
   }
}
//-------------------------------------------------------------------
void *JobControlArguments::operator new(size_t sz, const std::nothrow_t &n)
  throw()
{
  void *ptr;
  // Create or open shared memory
  ptr = m_hFileMap->open(FILE_MAP_NAME, sz, 
                         (m_bCreateSharedMemory 
                          ? memory_mapper::RDWR_CREATE 
                          : memory_mapper::RDWR_OPEN));
  if (ptr == NULL) {
    fprintf(stderr, "JobControlArguments: %s error=%d\n", 
            (m_bCreateSharedMemory 
             ? "CreateFileMapping()" : "OpenFileMapping()"),
            errno);
    return NULL;
  }

   return ptr;
}
#endif

//-------------------------------------------------------------------
void JobControlArguments::operator delete(void *ptr)
{
}
//-------------------------------------------------------------------
void JobControlArguments::Initialize(
      int jobType, int jobID, const LgsString &userID,
      int sourceLanguage, int targetLanguage,
      int targetForm,
      int wordSearchOptions,
      int wordSearchFoundStart, int wordSearchFoundLimit,
      int wordSearchUnfoundStart, int wordSearchUnfoundLimit,
      int tranPasses,
      int diagnosticLevel, int diagnosticStartLine, int diagnosticEndLine,
      int extendedSearch,
      const LgsString &companyCodes, const LgsString &subjectMatterCodes,
      const LgsString &sourceLocaleData, const LgsString &targetLocaleData,
      const LgsString &inputFormat,
      const LgsString &protectionCharacter,
      const LgsString &mainRes1, const LgsString &mainRes2, const LgsString &mainRes22,
      const LgsString &mainTran1, const LgsString &mainTran2,
      const LgsString &mainTran3, const LgsString &mainTran4,
      const LgsString &mainParse1, const LgsString &mainParse2,
      const LgsString &mainParse3, const LgsString &mainParse4,
      const LgsString &miniRes2,
      const LgsString &miniTran1, const LgsString &miniTran2,
      const LgsString &miniTran3, const LgsString &miniTran4,
      const LgsString &miniParse1, const LgsString &miniParse2,
      const LgsString &miniParse3, const LgsString &miniParse4,
      const LgsString &inputFile,
      const LgsString &outputFile,
      const LgsString &diagnosticFile,
      const LgsString &alignedFile,
      bool patternRulesFlag,
      bool timeFlag,
      bool traceFlag, bool logQueryFlag, bool saveScratchFlag
   )
{
//printf("JobControlArguments.Initialize w/ a lot of parms"); fflush(stdout);
   if (m_bCreateSharedMemory)
   {
      // Set all args
      m_nJobID = jobID;
      m_nJobType = jobType;
      m_nPriority = 1;
      m_nSourceLanguage = sourceLanguage;
      m_nTargetLanguage = targetLanguage;
      m_nTargetForm = targetForm;
      m_nWordSearchOptions = wordSearchOptions;
      m_nWordSearchFoundStart = wordSearchFoundStart;
      m_nWordSearchFoundLimit = wordSearchFoundLimit;
      m_nWordSearchUnfoundStart = wordSearchUnfoundStart;
      m_nWordSearchUnfoundLimit = wordSearchUnfoundLimit;
      m_nWordCount = 0;
      m_nSentInputCount = 0;
      m_nSentCurrentComplete = 0;
      m_nTranPasses = tranPasses;
      m_nDiagnosticLevel = diagnosticLevel;
      m_nDiagnosticStartLine = diagnosticStartLine;
      m_nDiagnosticEndLine = diagnosticEndLine;
      m_nStatus = StatusENQUEUED;

      TrimStore(userID, m_szUserID);
      TrimStore(companyCodes, m_szCompanyCodes);
      TrimStore(subjectMatterCodes, m_szSubjectMatterCodes);

      m_nExtendedSearch = extendedSearch;

      TrimStore(sourceLocaleData, m_szSourceLocaleData);
      TrimStore(targetLocaleData, m_szTargetLocaleData);
      TrimStore(inputFormat, m_szInputFormat);
      TrimStore(protectionCharacter, m_szProtectionCharacter);
      
      // Main Files
      TrimStore(mainRes1, m_szMainRes1);
      TrimStore(mainRes2, m_szMainRes2);
      TrimStore(mainRes22, m_szMainRes22);
      TrimStore(mainTran1, m_szMainTran1);
      TrimStore(mainTran2, m_szMainTran2);
      TrimStore(mainTran3, m_szMainTran3);
      TrimStore(mainTran4, m_szMainTran4);
      TrimStore(mainParse1, m_szMainParse1);
      TrimStore(mainParse2, m_szMainParse2);
      TrimStore(mainParse3, m_szMainParse3);
      TrimStore(mainParse4, m_szMainParse4);

      // Mini Files
      TrimStore(miniRes2, m_szMiniRes2);
      TrimStore(miniTran1, m_szMiniTran1);
      TrimStore(miniTran2, m_szMiniTran2);
      TrimStore(miniTran3, m_szMiniTran3);
      TrimStore(miniTran4, m_szMiniTran4);
      TrimStore(miniParse1, m_szMiniParse1);
      TrimStore(miniParse2, m_szMiniParse2);
      TrimStore(miniParse3, m_szMiniParse3);
      TrimStore(miniParse4, m_szMiniParse4);

      // Client Files
      TrimStore(inputFile, m_szClientInputFile);
      TrimStore(outputFile, m_szClientOutputFile);
      TrimStore(diagnosticFile, m_szClientDiagnosticFile);
      TrimStore(alignedFile, m_szClientAlignedFile);

      // Job Files
      TrimStore(inputFile, m_szInputFile);
      TrimStore(outputFile, m_szOutputFile);
      TrimStore(diagnosticFile, m_szDiagnosticFile);
      TrimStore(alignedFile, m_szAlignedFile);

      m_bPatternRulesFlag = patternRulesFlag;

      // Additional job options
      m_bTimeFlag = timeFlag;
      m_bTraceFlag = traceFlag;
      m_bLogQueryFlag = logQueryFlag;
      m_bSaveScratchFlag = saveScratchFlag;
   }
}
//-------------------------------------------------------------------
void JobControlArguments::TrimAndStore(const LgsString &str, char arr[], int arrSize)
{
   // Trim
   LgsString s = str;
   StringUtil::rightTrim(s);

   // Store
   strncpy(arr, s.c_str(), arrSize);
   int len = strlen(s.c_str());
   if (len >= arrSize)
   {
      fprintf(stderr, "JobControlArguments Warning: String is too long len=%d (%d)\n"
                       "\"%s\"\n", len, arrSize-1, s.c_str());
      arr[arrSize-1] = '\0';
   }
}
//-------------------------------------------------------------------
LgsString JobControlArguments::GetLanguageName(int langCode)
{
   switch(langCode)
   {
   case 1:
      return LgsString("german");
      break;
   case 2:
      return LgsString("english");
      break;
   case 3:
      return LgsString("french");
      break;
   case 4:
      return LgsString("spanish");
      break;
   case 5:
      return LgsString("italian");
      break;
   case 6:
      return LgsString("portuguese");
      break;
   default:
      return LgsString("");
   }
}
//-------------------------------------------------------------------
bool JobControlArguments::isDatabaseMode()
{
//   return m_pJobControlArguments->m_bDatabaseMode && (m_pConnection != NULL);
   if(m_pJobControlArguments)
      return m_pJobControlArguments->m_bDatabaseMode;
   else
      return false;
}
//-------------------------------------------------------------------
void JobControlArguments::setDatabaseMode(bool nm)
{
   m_pJobControlArguments->m_bDatabaseMode = nm;
}
//-------------------------------------------------------------------
// Getting functions
//-------------------------------------------------------------------
JobControlArguments::RunMODE JobControlArguments::RunMode()
{ 
   return (JobType() == 1)? translationMode: searchMode;
}
//-------------------------------------------------------------------
int JobControlArguments::JobID()
{
   return m_nJobID;
}
//-------------------------------------------------------------------
int JobControlArguments::JobType()
{
   return m_nJobType;
}
//-------------------------------------------------------------------
int JobControlArguments::Priority()
{
   return m_nPriority;
}
//-------------------------------------------------------------------
int JobControlArguments::SourceLanguage()
{
   return m_nSourceLanguage;
}
//-------------------------------------------------------------------
LgsString JobControlArguments::SourceLanguageStr()
{
   return GetLanguageName(m_nSourceLanguage);
}
//-------------------------------------------------------------------
int JobControlArguments::TargetLanguage()
{
   return m_nTargetLanguage;
}
//-------------------------------------------------------------------
LgsString JobControlArguments::TargetLanguageStr()
{
   return GetLanguageName(m_nTargetLanguage);
}
//-------------------------------------------------------------------
int JobControlArguments::TargetForm()
{
   return m_nTargetForm;
}
//-------------------------------------------------------------------
int JobControlArguments::WordSearchOptions()
{
   return m_nWordSearchOptions;
}
//-------------------------------------------------------------------
int JobControlArguments::WordSearchFoundStart()
{
   return m_nWordSearchFoundStart;
}
//-------------------------------------------------------------------
int JobControlArguments::WordSearchFoundLimit()
{
   return m_nWordSearchFoundLimit;
}
//-------------------------------------------------------------------
int JobControlArguments::WordSearchUnfoundStart()
{
   return m_nWordSearchUnfoundStart;
}
//-------------------------------------------------------------------
int JobControlArguments::WordSearchUnfoundLimit()
{
   return m_nWordSearchUnfoundLimit;
}
//-------------------------------------------------------------------
int JobControlArguments::WordCount()
{
   return m_nWordCount;
}
//-------------------------------------------------------------------
int JobControlArguments::SentInputCount()
{
   return m_nSentInputCount;
}
//-------------------------------------------------------------------
int JobControlArguments::SentCurrentComplete()
{
   return m_nSentCurrentComplete;
}
//-------------------------------------------------------------------
int JobControlArguments::Tranpasses()
{
   return m_nTranPasses;
}
//-------------------------------------------------------------------
int JobControlArguments::DiagnosticLevel()
{
   return m_nDiagnosticLevel;
}
//-------------------------------------------------------------------
int JobControlArguments::DiagnosticStartLine()
{
   return m_nDiagnosticStartLine;
}
//-------------------------------------------------------------------
int JobControlArguments::DiagnosticEndLine()
{
   return m_nDiagnosticEndLine;
}
//-------------------------------------------------------------------
int JobControlArguments::Status()
{
   return m_nStatus;
}
//-------------------------------------------------------------------
LgsString JobControlArguments::UserID()
{
   return LgsString(m_szUserID);
}
//-------------------------------------------------------------------
LgsString JobControlArguments::CompanyCodes()
{
   return LgsString(m_szCompanyCodes);
}
//-------------------------------------------------------------------
LgsString JobControlArguments::SubjectMatterCodes()
{
   return LgsString(m_szSubjectMatterCodes);
}
//-------------------------------------------------------------------
int JobControlArguments::ExtendedSearch()
{
   return m_nExtendedSearch;
}
//-------------------------------------------------------------------
LgsString JobControlArguments::SourceLocaleData()
{
   return LgsString(m_szSourceLocaleData);
}
//-------------------------------------------------------------------
LgsString JobControlArguments::TargetLocaleData()
{
   return LgsString(m_szTargetLocaleData);
}
//-------------------------------------------------------------------
LgsString JobControlArguments::InputFormat()
{
   return LgsString(m_szInputFormat);
}
//-------------------------------------------------------------------
LgsString JobControlArguments::ProtectionCharacter()
{
   return LgsString(m_szProtectionCharacter);
}
//-------------------------------------------------------------------
LgsString JobControlArguments::InputFile()
{
   return LgsString(m_szInputFile);
}
//-------------------------------------------------------------------
LgsString JobControlArguments::OutputFile()
{
   return LgsString(m_szOutputFile);
}
//-------------------------------------------------------------------
LgsString JobControlArguments::AlignedFile()
{
   return LgsString(m_szAlignedFile);
}
//-------------------------------------------------------------------
LgsString JobControlArguments::DiagnosticFile()
{
   return LgsString(m_szDiagnosticFile);
}
//-------------------------------------------------------------------
LgsString JobControlArguments::MainRes1()
{
   return LgsString(m_szMainRes1);
}
//-------------------------------------------------------------------
LgsString JobControlArguments::MainRes2()
{
   return LgsString(m_szMainRes2);
}
//-------------------------------------------------------------------
LgsString JobControlArguments::MainRes22()
{
   return LgsString(m_szMainRes22);
}
//-------------------------------------------------------------------
LgsString JobControlArguments::MainTran1()
{
   return LgsString(m_szMainTran1);
}
//-------------------------------------------------------------------
LgsString JobControlArguments::MainTran2()
{
   return LgsString(m_szMainTran2);
}
//-------------------------------------------------------------------
LgsString JobControlArguments::MainTran3()
{
   return LgsString(m_szMainTran3);
}
//-------------------------------------------------------------------
LgsString JobControlArguments::MainTran4()
{
   return LgsString(m_szMainTran4);
}
//-------------------------------------------------------------------
LgsString JobControlArguments::MainParse1()
{
   return LgsString(m_szMainParse1);
}
//-------------------------------------------------------------------
LgsString JobControlArguments::MainParse2()
{
   return LgsString(m_szMainParse2);
}
//-------------------------------------------------------------------
LgsString JobControlArguments::MainParse3()
{
   return LgsString(m_szMainParse3);
}
//-------------------------------------------------------------------
LgsString JobControlArguments::MainParse4()
{
   return LgsString(m_szMainParse4);
}
//-------------------------------------------------------------------
LgsString JobControlArguments::MiniRes2()
{
   return LgsString(m_szMiniRes2);
}
//-------------------------------------------------------------------
LgsString JobControlArguments::MiniTran1()
{
   return LgsString(m_szMiniTran1);
}
//-------------------------------------------------------------------
LgsString JobControlArguments::MiniTran2()
{
   return LgsString(m_szMiniTran2);
}
//-------------------------------------------------------------------
LgsString JobControlArguments::MiniTran3()
{
   return LgsString(m_szMiniTran3);
}
//-------------------------------------------------------------------
LgsString JobControlArguments::MiniTran4()
{
   return LgsString(m_szMiniTran4);
}
//-------------------------------------------------------------------
LgsString JobControlArguments::MiniParse1()
{
   return LgsString(m_szMiniParse1);
}
//-------------------------------------------------------------------
LgsString JobControlArguments::MiniParse2()
{
   return LgsString(m_szMiniParse2);
}
//-------------------------------------------------------------------
LgsString JobControlArguments::MiniParse3()
{
   return LgsString(m_szMiniParse3);
}
//-------------------------------------------------------------------
LgsString JobControlArguments::MiniParse4()
{
   return LgsString(m_szMiniParse1);
}
//-------------------------------------------------------------------
LgsString JobControlArguments::ClientInputFile()
{
   return LgsString(m_szClientInputFile);
}
//-------------------------------------------------------------------
LgsString JobControlArguments::ClientOutputFile()
{
   return LgsString(m_szClientOutputFile);
}
//-------------------------------------------------------------------
LgsString JobControlArguments::ClientAlignedFile()
{
   return LgsString(m_szClientAlignedFile);
}
//-------------------------------------------------------------------
LgsString JobControlArguments::ClientDiagnosticFile()
{
   return LgsString(m_szClientDiagnosticFile);
}
//-------------------------------------------------------------------
bool JobControlArguments::PatternRulesFlag()
{
   return m_bPatternRulesFlag;
}
//-------------------------------------------------------------------
bool JobControlArguments::TimeFlag()
{
   return m_bTimeFlag;
}
//-------------------------------------------------------------------
bool JobControlArguments::TraceFlag()
{
   return m_bTraceFlag;
}
//-------------------------------------------------------------------
bool JobControlArguments::LogQueryFlag()
{
   return m_bLogQueryFlag;
}
//-------------------------------------------------------------------
bool JobControlArguments::SaveScratchFlag()
{
   return m_bSaveScratchFlag;
}
//-------------------------------------------------------------------
// Setting functions
//-------------------------------------------------------------------
void JobControlArguments::Status(int newStatus)
{
   m_nStatus = newStatus;
}
//-------------------------------------------------------------------
void JobControlArguments::WordCount(int count)
{
   m_nWordCount = count;
}
//-------------------------------------------------------------------
void JobControlArguments::SentCurrentComplete(int count)
{
   m_nSentCurrentComplete = count;
}
//-------------------------------------------------------------------
void JobControlArguments::SentInputCount(int count)
{
   m_nSentInputCount = count;
}
