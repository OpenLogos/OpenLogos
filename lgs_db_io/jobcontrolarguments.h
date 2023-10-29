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
#ifndef __JobControlArguments_h__
#define __JobControlArguments_h__

#ifdef _MSC_VER
#define SHM_HANDLE HANDLE
#endif

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_MMAP
#define SHM_HANDLE class memory_mapper *
#endif

//---------------------------------------------------------------------
// File - JobControlArguments.h
//
// Class - JobControlArguments (interface)
//---------------------------------------------------------------------
// Note - The object of this class is created in shared memory.
//        So, this class can have members variables of simple type ONLY:
//        int, char[size], bool.
//        Do NOT use strings and other STL objects!
//---------------------------------------------------------------------

class JobControlArguments
{
public:
   enum RunMODE
   {
      translationMode,
      resolveMode,
      searchMode,
      transferMode
   };

   enum JobSTATUS
   {
      StatusENQUEUED = 1,
      StatusRUNNING,
      StatusABORTED,
      StatusERROR,
      StatusCOMPLETED,
      StatusRETRIEVED,
      StatusCLEARED
   };

   static JobControlArguments *CreateObject(bool createSharedMemory);
   static JobControlArguments *GetObject();
   static void DestroyObject();
   static bool isDatabaseMode();

   ~JobControlArguments();
   void operator delete(void *ptr); // overloaded operator delete

   // Initilize by using API
   void Initialize(
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
  );

   // Getting functions
   RunMODE RunMode();

   int JobID();
   int JobType();
   int Priority();
   int SourceLanguage();
   LgsString SourceLanguageStr();
   int TargetLanguage();
   LgsString TargetLanguageStr();
   int TargetForm();
   int WordSearchOptions();
   int WordSearchFoundStart();
   int WordSearchFoundLimit();
   int WordSearchUnfoundStart();
   int WordSearchUnfoundLimit();
   int WordCount();
   int SentInputCount();
   int SentCurrentComplete();
   int Tranpasses();
   int DiagnosticLevel();
   int DiagnosticStartLine();
   int DiagnosticEndLine();
   int Status();

   LgsString UserID();
   LgsString CompanyCodes();
   LgsString SubjectMatterCodes();

   int ExtendedSearch();

   LgsString SourceLocaleData();
   LgsString TargetLocaleData();
   LgsString InputFormat();
   LgsString ProtectionCharacter();

   LgsString MainRes1();
   LgsString MainRes2();
   LgsString MainRes22();
   LgsString MainTran1();
   LgsString MainTran2();
   LgsString MainTran3();
   LgsString MainTran4();
   LgsString MainParse1();
   LgsString MainParse2();
   LgsString MainParse3();
   LgsString MainParse4();

   LgsString MiniRes2();
   LgsString MiniTran1();
   LgsString MiniTran2();
   LgsString MiniTran3();
   LgsString MiniTran4();
   LgsString MiniParse1();
   LgsString MiniParse2();
   LgsString MiniParse3();
   LgsString MiniParse4();

   LgsString ClientInputFile();
   LgsString ClientOutputFile();
   LgsString ClientAlignedFile();
   LgsString ClientDiagnosticFile();

   LgsString InputFile();
   LgsString OutputFile();
   LgsString AlignedFile();
   LgsString DiagnosticFile();

   bool PatternRulesFlag();
   bool TimeFlag();
   bool TraceFlag();
   bool LogQueryFlag();
   bool SaveScratchFlag();

   // Setting functions
   void Status(int newStatus);
   void WordCount(int count);
   void SentCurrentComplete(int count);
   void SentInputCount(int count);
   void setDatabaseMode(bool nmode);

private:
   JobControlArguments();
   // overloaded operator new
   void *operator new(size_t sz, const std::nothrow_t &) throw();

   static void TrimAndStore(const LgsString &str, char arr[], int arrSize);
   static LgsString GetLanguageName(int langCode);

   // Static members
   static JobControlArguments *m_pJobControlArguments;
   static SHM_HANDLE m_hFileMap;          // handle to file to map
   static bool m_bCreateSharedMemory; // create(true) or open(false) shared memory

   bool m_bDatabaseMode; // are we in a database mode?

protected:

   // All job control arguments
   // Sizes of char args as they are in ls8jobs table +1
   int m_nJobID;
   int m_nJobType;
   int m_nPriority;
   int m_nSourceLanguage;
   int m_nTargetLanguage;
   int m_nTargetForm;
   int m_nWordSearchOptions;
   int m_nWordSearchFoundStart;
   int m_nWordSearchFoundLimit;
   int m_nWordSearchUnfoundStart;
   int m_nWordSearchUnfoundLimit;
   int m_nWordCount;
   int m_nSentInputCount;
   int m_nSentCurrentComplete;
   int m_nTranPasses;
   int m_nDiagnosticLevel;
   int m_nDiagnosticStartLine;
   int m_nDiagnosticEndLine;
   int m_nStatus;

   char m_szUserID[16];
   char m_szCompanyCodes[101];
   char m_szSubjectMatterCodes[401];

   int m_nExtendedSearch;

   char m_szSourceLocaleData[51];
   char m_szTargetLocaleData[51];
   char m_szInputFormat[21];
   char m_szProtectionCharacter[2];

   char m_szInputFile[101];
   char m_szOutputFile[101];
   char m_szAlignedFile[101];
   char m_szDiagnosticFile[101];

   char m_szMainRes1[51];
   char m_szMainRes2[51];
   char m_szMainRes22[51];
   char m_szMainTran1[51];
   char m_szMainTran2[51];
   char m_szMainTran3[51];
   char m_szMainTran4[51];

   char m_szMainParse1[51];
   char m_szMainParse2[51];
   char m_szMainParse3[51];
   char m_szMainParse4[51];

   char m_szMiniRes2[51];
   char m_szMiniTran1[51];
   char m_szMiniTran2[51];
   char m_szMiniTran3[51];
   char m_szMiniTran4[51];
   char m_szMiniParse1[51];
   char m_szMiniParse2[51];
   char m_szMiniParse3[51];
   char m_szMiniParse4[51];

   char m_szClientInputFile[256];
   char m_szClientOutputFile[256];
   char m_szClientDiagnosticFile[201];
   char m_szClientAlignedFile[201];

   bool m_bPatternRulesFlag;

   // Additional job options
   bool m_bTimeFlag;
   bool m_bTraceFlag;
   bool m_bLogQueryFlag;
   bool m_bSaveScratchFlag;
};

#endif // __JobControlArguments_h__
