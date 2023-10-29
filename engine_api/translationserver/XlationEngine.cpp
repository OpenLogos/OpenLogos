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
// XlationEngine.cpp : Implementation of CXlationEngine
#ifdef _MSC_VER
#include "stdafx.h"
#include "Translationserver.h"
#endif
#include <logos_include/logoscommon.h>
#include "XlationEngine.h"
#include "xlationconsts.h"
#include <engine_api/translationserver/comparameters.h>
#include <lgs_db_io/jobcontrolarguments.h>
#include <lgs_db_io/serverproperties.h>
#include <logos_libs/dbcache/CacheCompanyData.h>
#include <logos_libs/dbcache/CacheCompanyQuery.h>
#include <logos_libs/sql/sqlconnection.h>
#include <logos_libs/sql/sqlexception.h>
#include <logos_libs/utility/CEnvVariable.h>

#ifndef _MSC_VER
#include <sys/types.h>
#include <unistd.h>
#endif

// object class - derive all classes which are to be inherited from, from Object
// the virtual destructor is then inherited, so that no further empty destructors need be defined
// class Object
// {
// public:
//    virtual ~Object() {}
// };

#include <logos_libs/SubjectMatterCodes/SubjectMatterCodeTree.h>

#ifdef _MSC_VER
#include <comdef.h>
#endif

SqlConnection *CXlationEngine::p_connection = 0;

int RunTransl(bool bDebug, bool bGenerateLog, const LgsString &logFileName);
int ProfExec(const LgsString &Cmd);
int SpawnCmd(const LgsString &Cmd, const LgsString &logFileName);


void ucToSbs(const BSTR ucStr, LgsString &sbString)
{
   _bstr_t bStr(const_cast<BSTR>(ucStr), true);
   char *scStr = new char [bStr.length()+1];
   wcstombs(scStr, bStr, bStr.length()+1);
   sbString = scStr;
   delete[] scStr;
}

void setInputFormat(short nFormat, LgsString &fileFormat)
{
   switch (nFormat)
   {
   case RTF:
      fileFormat = CRTF;
      break;
   case SGML:
      fileFormat = CSGML;
      break;
   case HTML:
      fileFormat = CHTML;
      break;
   case TMX:
      fileFormat = CTMX;
      break;
   case XML:
      fileFormat = CXML;
      break;
   case INTERLEAF_ASCII:
      fileFormat = CINTERLEAF_ASCII;
      break;
   case FRAME_MIF:
      fileFormat = CFRAME_MIF;
      break;
   case MS_WORD8:
      fileFormat = CMS_WORD8;
      break;
   case RTF_WINHELP:
      fileFormat = CRTF_WINHELP;
      break;
   default:
      fileFormat = CLGS_TXT_EXCHANGE;
      break;
   }
}

int getJobId()
{
#ifdef _MSC_VER
   DWORD idValue = 0;
   DWORD dataSize = sizeof(idValue);
   DWORD dataType = 0;
   DWORD disposition;
   HKEY regKey = NULL;
   RegCreateKeyEx(HKEY_LOCAL_MACHINE, CXLATION_ENGINE, 0, NULL, REG_OPTION_NON_VOLATILE, 
                  KEY_ALL_ACCESS, NULL, &regKey, &disposition);
   if( REG_CREATED_NEW_KEY == disposition )
   {
      idValue = 1;
      RegSetValueEx(regKey, CJOB_ID, 0, REG_DWORD, (const unsigned char*)&idValue, sizeof(idValue));
   }
   else
   {
      RegQueryValueEx(regKey, CJOB_ID, 0, &dataType, (unsigned char *)&idValue, &dataSize);
      DWORD nextJobId = idValue + 1;
      if( 32767 == nextJobId )
      {
         nextJobId = 1;
      }
      RegSetValueEx(regKey, CJOB_ID, 0, REG_DWORD, (const unsigned char *)&nextJobId, sizeof(nextJobId));  
   }
   RegCloseKey(regKey);
#else
   pid_t idValue = getpid();
#endif
   
   return idValue;
}

// Store the text string in a file
bool createTempFile(const LgsString &text, const LgsString &fileName)
{
   FILE* fp = fopen(fileName.c_str(), "w");
   if( NULL == fp )
   {
      return false;
   }
   if( fwrite(text.c_str(), 1, text.length(), fp) < text.length() )
   {
      return false;
   }
   fclose(fp);
   return true;
}

bool readFromFile(LgsString &text, const LgsString &fileName)
{
   char cBuffer[401];
   text.reserve(401);
   FILE* fp = fopen(fileName.c_str(), "r");
   if( NULL == fp )
   {
      return false;
   }
   size_t sz = 0;
   if( (sz = fread(cBuffer, 1, 400, fp)) != 0 )
   {
      cBuffer[sz] = '\0';
      text += cBuffer;
   }
   fclose(fp);
   return true;
}

LgsString getFilePrefix(int jobId)
{
   char cJobId[35];
   sprintf(cJobId, "%d", jobId);
   ServerProperties server;
   LgsString filePrefix = server.JobFileDirectory();
   filePrefix+= COUT_FILE_PREFIX;
   filePrefix+= cJobId;
   return filePrefix;
}

static SqlConnection *p_connection = 0;

//--------------------------------------------------------------------------
// This is a DBMS query that does not use a SqlQuery object. It is only performed once at the top 
// of the program. It fetches all of company records and converts the data into objects.
//--------------------------------------------------------------------------
void completeCompanyList(LgsString &companyCodes)
{
   
   // try cache in shared memory first
   CacheCompanyData *ccd = new CacheCompanyData(NULL, 0, false, false, false);
   if(ccd->isValid()) {
      char cc[4], desc[201], rs[2];
      CacheCompanyQuery *q = new CacheCompanyQuery(ccd);
      if(q->query()) {
         while(q->fetch(cc, desc, rs)) {
            companyCodes += cc;
            companyCodes += ':';
            companyCodes += desc;
            companyCodes += ';';
         }
         delete q;
         delete ccd;
         return;
      }
   }
   delete ccd;
   
   try
   {
      if( !p_connection )
      {
         p_connection = getSqlConnection();
      }
      SqlStatement* pStatement = p_connection->CreateStatement();
      LgsString s = "select Company_Code, Description, Restrict_Switch "
         "from Company order by Search_Sequence";
      
      pStatement->AddToCommandString( s );
      pStatement->ParseDeferred();
      SqlColumn* pCompanyCode = pStatement->BindOutputColumn(1, SqlColumn::StringType);
      SqlColumn* pDescription = pStatement->BindOutputColumn(2, SqlColumn::StringType);
      SqlColumn* pRestrict = pStatement->BindOutputColumn(3, SqlColumn::StringType);
      pStatement->Execute();
      while (pStatement->Fetch())
      {
         companyCodes += pCompanyCode->AsString();
         companyCodes += ':';
         companyCodes += pDescription->AsString();
         companyCodes += ';';
      }
      
      delete pStatement;
   }
   catch(SqlException& x)
   {
      companyCodes = "Company Code Error: " + x.Message();
   }
}

/////////////////////////////////////////////////////////////////////////////
// CXlationEngine


STDMETHODIMP CXlationEngine::translateText(const BSTR usInputText, short nSourceLanguage,
                                           short nTargetLanguage, short nInputFormat,
                                           unsigned short unProtectChar, const BSTR usSourceLocale,
                                           const BSTR usTargetLocale, unsigned char ucPmRulesFlag,
                                           unsigned char ucExtendedSearchFlag, const BSTR usSmcCodes,
                                           const BSTR usCompanyCodes, unsigned char ucFlagUnfoundWords,
                                           unsigned char ucGenerateAlignment, unsigned char ucGenerateLog, unsigned char ucTargetImperativeForm,
                                           unsigned char ucDbModeFlag, unsigned char ucDebugFlag, unsigned char ucStatisticsFlag,
                                           unsigned char ucTraceFlag, unsigned char ucLogQueryFlag, unsigned char ucSaveScratchFlag,
                                           unsigned char ucDiagLevel, long lDiagStartLine, long lDiagEndLine,
                                           const BSTR usRuleOverrideFile,
                                           const BSTR usMainRes1, const BSTR usMainRes2, const BSTR usMainRes22,
                                           const BSTR usMainTran1, const BSTR usMainTran2,
                                           const BSTR usMainTran3, const BSTR usMainTran4,
                                           const BSTR usMainParse1, const BSTR usMainParse2,
                                           const BSTR usMainParse3, const BSTR usMainParse4,
                                           const BSTR usMiniRes2,
                                           const BSTR usMiniTran1, const BSTR usMiniTran2,
                                           const BSTR usMiniTran3, const BSTR usMiniTran4,
                                           const BSTR usMiniParse1, const BSTR usMiniParse2,
                                           const BSTR usMiniParse3, const BSTR usMiniParse4,
                                           const BSTR usUserId,
                                           BSTR *uspXlatedText, BSTR *uspAlignedText, BSTR *uspDiagText,
                                           long *lpSentenceCount, long *lpWordCount,
                                           long *lpSessionId, BSTR *uspLogFile, BSTR *uspErrorDesc)
{
   // TODO: Add your implementation code here
   int jobId = getJobId();
   LgsString filePrefix = getFilePrefix(jobId);

   int iExtSearch = (FLAG_ON == ucExtendedSearchFlag)? 2 : 1;

   LgsString companyCodes;
   ucToSbs(usCompanyCodes, companyCodes);
   LgsString smc;
   ucToSbs(usSmcCodes, smc);
   LgsString sourceLocale;
   ucToSbs(usSourceLocale, sourceLocale);
   LgsString targetLocale;
   ucToSbs(usTargetLocale, targetLocale);
   
   LgsString inputFormat;
   setInputFormat(nInputFormat, inputFormat);
   char protChar[2];
   protChar[0] = unProtectChar;
   protChar[1] = '\0';
   LgsString protectionChar(protChar);
   LgsString inputFile(filePrefix+CINP_EXT);
   LgsString inputText;
   ucToSbs(usInputText, inputText);
   _bstr_t wEmptyOut(L"");
   if( !createTempFile(inputText, inputFile) )
   {
      _bstr_t wErrText(WCERR_INPUT_FILE);
      *uspErrorDesc = wErrText.copy();
      *lpSessionId = jobId;
      *uspXlatedText = wEmptyOut.copy();
      *uspAlignedText = wEmptyOut.copy();
      *uspDiagText = wEmptyOut.copy();
      *lpSentenceCount = 0;
      *lpWordCount = 0;
      return S_OK;
   }
   
   LgsString outFile(filePrefix+COUT_EXT);
   LgsString diagFile(filePrefix+CDIAG_EXT);
   LgsString alignFile;
   
   if( FLAG_ON == ucGenerateAlignment )
   {
      alignFile = filePrefix+CALIGN_EXT;
   }
   
   bool pmRuleFlag = (FLAG_ON == ucPmRulesFlag);
   
   LgsString mainRes1;
   ucToSbs(usMainRes1, mainRes1);
   LgsString mainRes2;
   ucToSbs(usMainRes2, mainRes2);
   LgsString mainRes22;
   ucToSbs(usMainRes22, mainRes22);
   LgsString mainTran1;
   ucToSbs(usMainTran1, mainTran1);
   LgsString mainTran2;
   ucToSbs(usMainTran2, mainTran2);
   LgsString mainTran3;
   ucToSbs(usMainTran3, mainTran3);
   LgsString mainTran4;
   ucToSbs(usMainTran4, mainTran4);
   LgsString mainParse1;
   ucToSbs(usMainParse1, mainParse1);
   LgsString mainParse2;
   ucToSbs(usMainParse2, mainParse2);
   LgsString mainParse3;
   ucToSbs(usMainParse3, mainParse3);
   LgsString mainParse4;
   ucToSbs(usMainParse4, mainParse4);
   LgsString miniRes2;
   ucToSbs(usMiniRes2, miniRes2);
   LgsString miniTran1;
   ucToSbs(usMiniTran1, miniTran1);
   LgsString miniTran2;
   ucToSbs(usMiniTran2, miniTran2);
   LgsString miniTran3;
   ucToSbs(usMiniTran3, miniTran3);
   LgsString miniTran4;
   ucToSbs(usMiniTran4, miniTran4);
   LgsString miniParse1;
   ucToSbs(usMiniParse1, miniParse1);
   LgsString miniParse2;
   ucToSbs(usMiniParse2, miniParse2);
   LgsString miniParse3;
   ucToSbs(usMiniParse3, miniParse3);
   LgsString miniParse4;
   ucToSbs(usMiniParse4, miniParse4);
   LgsString userId;
   ucToSbs(usUserId, userId);

   // ApiInitialize
   JobControlArguments *m_jcArgs = JobControlArguments::CreateObject(true);
   m_jcArgs->Initialize(
      TRANSLATE, jobId, userId,
      nSourceLanguage, nTargetLanguage, (FLAG_ON == ucTargetImperativeForm)? 1: 0,
      (FLAG_ON == ucFlagUnfoundWords)? FLAG_UNFOUND: 0, 1, 1,
      1, 1, 1, ucDiagLevel, lDiagStartLine, lDiagEndLine, iExtSearch,
      companyCodes, smc, sourceLocale, targetLocale, inputFormat, protectionChar,
      mainRes1, mainRes2, mainRes22,
      mainTran1,  mainTran2, mainTran3,  mainTran4,
      mainParse1,  mainParse2, mainParse3,  mainParse4,
      miniRes2,
      miniTran1,  miniTran2, miniTran3,  miniTran4,
      miniParse1,  miniParse2, miniParse3,  miniParse4,
      inputFile, outFile, diagFile, alignFile, pmRuleFlag,
      (FLAG_ON == ucStatisticsFlag)? true: false,
      (FLAG_ON == ucTraceFlag)? true: false,
      (FLAG_ON == ucLogQueryFlag)? true: false,
      (FLAG_ON == ucSaveScratchFlag)? true: false
   );
   m_jcArgs->setDatabaseMode((FLAG_ON == ucDbModeFlag)? true : false);
   
   // Run Transl
   LgsString logFileName;
   bool bGenerateLog = (FLAG_ON == ucGenerateLog);
   if( bGenerateLog )
   {
      logFileName = filePrefix + CLOG_EXT;
   }
   RunTransl((FLAG_ON == ucDebugFlag), bGenerateLog, logFileName);
   
   // Set output values
   *lpSentenceCount = m_jcArgs->SentCurrentComplete();
   *lpWordCount = m_jcArgs->WordCount();
   *lpSessionId = jobId;

   delete m_jcArgs;

   _bstr_t wLogFile(logFileName.c_str());
   *uspLogFile = bGenerateLog? wLogFile.copy(): wEmptyOut.copy();

   LgsString outText;
   if( !readFromFile(outText, outFile) )
   {
      _bstr_t wErrText(WCERR_READ_OUTPUT);
      *uspErrorDesc = wErrText.copy();
      *uspXlatedText = wEmptyOut.copy();
      *uspAlignedText = wEmptyOut.copy();
      *uspDiagText = wEmptyOut.copy();
      return S_OK;
   }
   _bstr_t wOutText(outText.c_str());
   *uspXlatedText = wOutText.copy();
   if( FLAG_ON == ucGenerateAlignment )
   {
      LgsString alignText;
      if( !readFromFile(alignText, alignFile) )
      {
         _bstr_t wErrText(WCERR_READ_ALIGN);
         *uspErrorDesc = wErrText.copy();
         *uspXlatedText = wEmptyOut.copy();
         *uspAlignedText = wEmptyOut.copy();
         *uspDiagText = wEmptyOut.copy();
         return S_OK;
      }
      _bstr_t wAlignText(alignText.c_str());
      *uspAlignedText = wAlignText.copy();
   }
   else
   {
      *uspAlignedText = wEmptyOut.copy();
   }
   
   if( NO_DIAG == ucDiagLevel)
   {
      *uspDiagText = wEmptyOut.copy();   
   }
   else
   {
      LgsString diagText;
      if( !readFromFile(diagText, diagFile) )
      {
         _bstr_t wErrText(WCERR_READ_DIAG);
         *uspErrorDesc = wErrText.copy();
         return S_OK;
      }
      _bstr_t wDiagText(diagText.c_str());
      *uspDiagText = wDiagText.copy();
   }
   *uspErrorDesc = wEmptyOut.copy();
   
   return S_OK;
}

STDMETHODIMP CXlationEngine::translateDoc(const BSTR usInputFile, short nSourceLanguage,
                                          short nTargetLanguage, short nInputFormat,
                                          unsigned short unProtectChar, const BSTR usSourceLocale,
                                          const BSTR usTargetLocale, unsigned char ucPmRulesFlag,
                                          unsigned char ucExtendedSearchFlag, const BSTR usSmcCodes,
                                          const BSTR usCompanyCodes, unsigned char ucFlagUnfoundWords,
                                          unsigned char ucGenerateAlignment, unsigned char ucGenerateLog, unsigned char ucTargetImperativeForm,
                                          unsigned char ucDbModeFlag, unsigned char ucDebugFlag, unsigned char ucStatisticsFlag,
                                          unsigned char ucTraceFlag, unsigned char ucLogQueryFlag, unsigned char ucSaveScratchFlag,
                                          unsigned char ucDiagLevel, long lDiagStartLine, long lDiagEndLine,
                                          const BSTR usRuleOverrideFile,
                                          const BSTR usMainRes1, const BSTR usMainRes2, const BSTR usMainRes22,
                                          const BSTR usMainTran1, const BSTR usMainTran2,
                                          const BSTR usMainTran3, const BSTR usMainTran4,
                                          const BSTR usMainParse1, const BSTR usMainParse2,
                                          const BSTR usMainParse3, const BSTR usMainParse4,
                                          const BSTR usMiniRes2,
                                          const BSTR usMiniTran1, const BSTR usMiniTran2,
                                          const BSTR usMiniTran3, const BSTR usMiniTran4,
                                          const BSTR usMiniParse1, const BSTR usMiniParse2,
                                          const BSTR usMiniParse3, const BSTR usMiniParse4,
                                          const BSTR usUserId,
                                          BSTR *uspXlatedTextFile, BSTR *uspAlignedFile, BSTR *uspDiagFile,
                                          long *lpSentenceCount, long *lpWordCount,
                                          long *lpSessionId, BSTR *uspLogFile, BSTR *uspErrorDesc)
{
   // TODO: Add your implementation code here
   int jobId = getJobId();
   LgsString filePrefix = getFilePrefix(jobId);

   int iExtSearch = (FLAG_ON == ucExtendedSearchFlag)? 2 : 1;

   LgsString companyCodes;
   ucToSbs(usCompanyCodes, companyCodes);
   LgsString smc;
   ucToSbs(usSmcCodes, smc);
   LgsString sourceLocale;
   ucToSbs(usSourceLocale, sourceLocale);
   LgsString targetLocale;
   ucToSbs(usTargetLocale, targetLocale);
   
   LgsString inputFormat;
   setInputFormat(nInputFormat, inputFormat);
   char protChar[2];
   protChar[0] = unProtectChar;
   protChar[1] = '\0';
   LgsString protectionChar(protChar);
   LgsString inputFile;
   ucToSbs(usInputFile, inputFile);
   LgsString outFile(filePrefix+COUT_EXT);
   LgsString diagFile(filePrefix+CDIAG_EXT);
   LgsString alignFile;
   if( FLAG_ON == ucGenerateAlignment )
   {
      alignFile = filePrefix+CALIGN_EXT;
   }
   bool pmRuleFlag = (FLAG_ON == ucPmRulesFlag);
   
   LgsString mainRes1;
   ucToSbs(usMainRes1, mainRes1);
   LgsString mainRes2;
   ucToSbs(usMainRes2, mainRes2);
   LgsString mainRes22;
   ucToSbs(usMainRes22, mainRes22);
   LgsString mainTran1;
   ucToSbs(usMainTran1, mainTran1);
   LgsString mainTran2;
   ucToSbs(usMainTran2, mainTran2);
   LgsString mainTran3;
   ucToSbs(usMainTran3, mainTran3);
   LgsString mainTran4;
   ucToSbs(usMainTran4, mainTran4);
   LgsString mainParse1;
   ucToSbs(usMainParse1, mainParse1);
   LgsString mainParse2;
   ucToSbs(usMainParse2, mainParse2);
   LgsString mainParse3;
   ucToSbs(usMainParse3, mainParse3);
   LgsString mainParse4;
   ucToSbs(usMainParse4, mainParse4);
   LgsString miniRes2;
   ucToSbs(usMiniRes2, miniRes2);
   LgsString miniTran1;
   ucToSbs(usMiniTran1, miniTran1);
   LgsString miniTran2;
   ucToSbs(usMiniTran2, miniTran2);
   LgsString miniTran3;
   ucToSbs(usMiniTran3, miniTran3);
   LgsString miniTran4;
   ucToSbs(usMiniTran4, miniTran4);
   LgsString miniParse1;
   ucToSbs(usMiniParse1, miniParse1);
   LgsString miniParse2;
   ucToSbs(usMiniParse2, miniParse2);
   LgsString miniParse3;
   ucToSbs(usMiniParse3, miniParse3);
   LgsString miniParse4;
   ucToSbs(usMiniParse4, miniParse4);
   LgsString userId;
   ucToSbs(usUserId, userId);

   // ApiInitialize
   JobControlArguments *m_jcArgs = JobControlArguments::CreateObject(true);
   m_jcArgs->Initialize(
      TRANSLATE, jobId, userId,
      nSourceLanguage, nTargetLanguage, (FLAG_ON == ucTargetImperativeForm)? 1: 0,
      (FLAG_ON == ucFlagUnfoundWords)? FLAG_UNFOUND: 0, 1, 1,
      1, 1, 1, ucDiagLevel, lDiagStartLine, lDiagEndLine, iExtSearch,
      companyCodes, smc, sourceLocale, targetLocale, inputFormat, protectionChar,
      mainRes1, mainRes2, mainRes22,
      mainTran1,  mainTran2, mainTran3,  mainTran4,
      mainParse1,  mainParse2, mainParse3,  mainParse4,
      miniRes2,
      miniTran1,  miniTran2, miniTran3,  miniTran4,
      miniParse1,  miniParse2, miniParse3,  miniParse4,
      inputFile, outFile, diagFile, alignFile, pmRuleFlag,
      (FLAG_ON == ucStatisticsFlag)? true: false,
      (FLAG_ON == ucTraceFlag)? true: false,
      (FLAG_ON == ucLogQueryFlag)? true: false,
      (FLAG_ON == ucSaveScratchFlag)? true: false
   );
   m_jcArgs->setDatabaseMode((FLAG_ON == ucDbModeFlag)? true : false);
   
   // Run Transl
   LgsString logFileName;
   bool bGenerateLog = (FLAG_ON == ucGenerateLog);
   if( bGenerateLog )
   {
      logFileName = filePrefix + CLOG_EXT;
   }
   RunTransl((FLAG_ON == ucDebugFlag), bGenerateLog, logFileName);
   
   // Set output values
   _bstr_t wEmptyOut(L"");
   _bstr_t wLogFile(logFileName.c_str());
   *uspLogFile = bGenerateLog? wLogFile.copy(): wEmptyOut.copy();
   _bstr_t wOutFile(outFile.c_str());
   _bstr_t wDiagFile(diagFile.c_str());
   _bstr_t wAlignFile(alignFile.c_str());
   *uspXlatedTextFile = wOutFile.copy();
   *uspAlignedFile = (FLAG_ON == ucGenerateAlignment)? wAlignFile.copy(): wEmptyOut.copy();
   *uspDiagFile = (NO_DIAG == ucDiagLevel)? wEmptyOut.copy(): wDiagFile.copy();
   *lpSentenceCount = m_jcArgs->SentCurrentComplete();
   *lpWordCount = m_jcArgs->WordCount();
   *lpSessionId = jobId;
   *uspErrorDesc = wEmptyOut.copy();
   
   delete m_jcArgs;

   return S_OK;
}

STDMETHODIMP CXlationEngine::termSearchText(const BSTR usInputText, short nSourceLanguage,
                                            short nTargetLanguage, short nInputFormat,
                                            unsigned short unProtectChar, const BSTR usSourceLocale,
                                            const BSTR usTargetLocale, unsigned char ucPmRulesFlag,
                                            unsigned char ucExtendedSearchFlag, const BSTR usSmcCodes,
                                            const BSTR usCompanyCodes, unsigned char ucFlagUnfoundWords,
                                            unsigned char ucLogosDefaultFlag, unsigned short unWordSearchOptions,
                                            long lWordSearchFoundStart, long lWordSearchFoundLimit,
                                            long lWordSearchUnfoundStart, long lWordSearchUnfoundLimit,
                                            unsigned char ucGenerateAlignment, unsigned char ucGenerateLog, unsigned char ucTargetImperativeForm,
                                            unsigned char ucDbModeFlag, unsigned char ucDebugFlag, unsigned char ucStatisticsFlag,
                                            unsigned char ucTraceFlag, unsigned char ucLogQueryFlag, unsigned char ucSaveScratchFlag,
                                            unsigned char ucDiagLevel, long lDiagStartLine, long lDiagEndLine,
                                            const BSTR usRuleOverrideFile,
                                            const BSTR usMainRes1, const BSTR usMainRes2, const BSTR usMainRes22,
                                            const BSTR usMainTran1, const BSTR usMainTran2,
                                            const BSTR usMainTran3, const BSTR usMainTran4,
                                            const BSTR usMainParse1, const BSTR usMainParse2,
                                            const BSTR usMainParse3, const BSTR usMainParse4,
                                            const BSTR usMiniRes2,
                                            const BSTR usMiniTran1, const BSTR usMiniTran2,
                                            const BSTR usMiniTran3, const BSTR usMiniTran4,
                                            const BSTR usMiniParse1, const BSTR usMiniParse2,
                                            const BSTR usMiniParse3, const BSTR usMiniParse4,
                                            const BSTR usUserId,
                                            BSTR *uspTermSearchText, BSTR *uspAlignedText, BSTR *uspDiagText,
                                            long *lpSentenceCount, long *lpWordCount,
                                            long *lpSessionId, BSTR *uspLogFile, BSTR *uspErrorDesc)
{
   // TODO: Add your implementation code here
   int jobId = getJobId();
   LgsString filePrefix = getFilePrefix(jobId);

   int iExtSearch = (FLAG_ON == ucExtendedSearchFlag)? 2 : 1;

   LgsString companyCodes;
   ucToSbs(usCompanyCodes, companyCodes);
   LgsString smc;
   ucToSbs(usSmcCodes, smc);
   LgsString sourceLocale;
   ucToSbs(usSourceLocale, sourceLocale);
   LgsString targetLocale;
   ucToSbs(usTargetLocale, targetLocale);
   
   LgsString inputFormat;
   setInputFormat(nInputFormat, inputFormat);
   char protChar[2];
   protChar[0] = unProtectChar;
   protChar[1] = '\0';
   LgsString protectionChar(protChar);
   LgsString inputFile(filePrefix+CINP_EXT);
   LgsString inputText;
   ucToSbs(usInputText, inputText);
   _bstr_t wEmptyOut(L"");
   if( !createTempFile(inputText, inputFile) )
   {
      _bstr_t wErrText(WCERR_INPUT_FILE);
      *uspErrorDesc = wErrText.copy();
      *lpSessionId = jobId;
      *uspTermSearchText = wEmptyOut.copy();
      *uspAlignedText = wEmptyOut.copy();
      *uspDiagText = wEmptyOut.copy();
      *lpSentenceCount = 0;
      *lpWordCount = 0;
      return S_OK;
   }
   
   LgsString outFile(filePrefix+COUT_EXT);
   LgsString diagFile(filePrefix+CDIAG_EXT);
   LgsString alignFile;
   ucGenerateAlignment = FLAG_OFF; // do not generate align file for termsearch job
   if( FLAG_ON == ucGenerateAlignment )
   {
      alignFile = filePrefix+CALIGN_EXT;
   }
   
   bool pmRuleFlag = (FLAG_ON == ucPmRulesFlag);
   
   LgsString mainRes1;
   ucToSbs(usMainRes1, mainRes1);
   LgsString mainRes2;
   ucToSbs(usMainRes2, mainRes2);
   LgsString mainRes22;
   ucToSbs(usMainRes22, mainRes22);
   LgsString mainTran1;
   ucToSbs(usMainTran1, mainTran1);
   LgsString mainTran2;
   ucToSbs(usMainTran2, mainTran2);
   LgsString mainTran3;
   ucToSbs(usMainTran3, mainTran3);
   LgsString mainTran4;
   ucToSbs(usMainTran4, mainTran4);
   LgsString mainParse1;
   ucToSbs(usMainParse1, mainParse1);
   LgsString mainParse2;
   ucToSbs(usMainParse2, mainParse2);
   LgsString mainParse3;
   ucToSbs(usMainParse3, mainParse3);
   LgsString mainParse4;
   ucToSbs(usMainParse4, mainParse4);
   LgsString miniRes2;
   ucToSbs(usMiniRes2, miniRes2);
   LgsString miniTran1;
   ucToSbs(usMiniTran1, miniTran1);
   LgsString miniTran2;
   ucToSbs(usMiniTran2, miniTran2);
   LgsString miniTran3;
   ucToSbs(usMiniTran3, miniTran3);
   LgsString miniTran4;
   ucToSbs(usMiniTran4, miniTran4);
   LgsString miniParse1;
   ucToSbs(usMiniParse1, miniParse1);
   LgsString miniParse2;
   ucToSbs(usMiniParse2, miniParse2);
   LgsString miniParse3;
   ucToSbs(usMiniParse3, miniParse3);
   LgsString miniParse4;
   ucToSbs(usMiniParse4, miniParse4);
   LgsString userId;
   ucToSbs(usUserId, userId);

   // ApiInitialize
   JobControlArguments *m_jcArgs = JobControlArguments::CreateObject(true);
   m_jcArgs->Initialize(
      TERM_SEARCH, jobId, userId,
      nSourceLanguage, nTargetLanguage, (FLAG_ON == ucTargetImperativeForm)? 1: 0,
      unWordSearchOptions, 
      lWordSearchFoundStart, lWordSearchFoundLimit, lWordSearchUnfoundStart, 
      lWordSearchUnfoundLimit, 1, ucDiagLevel, lDiagStartLine, lDiagEndLine, iExtSearch,
      companyCodes, smc, sourceLocale, targetLocale, inputFormat, protectionChar,
      mainRes1, mainRes2, mainRes22,
      mainTran1,  mainTran2, mainTran3,  mainTran4,
      mainParse1,  mainParse2, mainParse3,  mainParse4,
      miniRes2,
      miniTran1,  miniTran2, miniTran3,  miniTran4,
      miniParse1,  miniParse2, miniParse3,  miniParse4,
      inputFile, outFile, diagFile, alignFile, pmRuleFlag,
      (FLAG_ON == ucStatisticsFlag)? true: false,
      (FLAG_ON == ucTraceFlag)? true: false,
      (FLAG_ON == ucLogQueryFlag)? true: false,
      (FLAG_ON == ucSaveScratchFlag)? true: false
   );
   m_jcArgs->setDatabaseMode((FLAG_ON == ucDbModeFlag)? true : false);
   
   // Run Transl
   LgsString logFileName;
   bool bGenerateLog = (FLAG_ON == ucGenerateLog);
   if( bGenerateLog )
   {
      logFileName = filePrefix + CLOG_EXT;
   }
   RunTransl((FLAG_ON == ucDebugFlag), bGenerateLog, logFileName);
   
   // Set output values
   *lpSentenceCount = m_jcArgs->SentCurrentComplete();
   *lpWordCount = m_jcArgs->WordCount();
   *lpSessionId = jobId;

   delete m_jcArgs;

   _bstr_t wLogFile(logFileName.c_str());
   *uspLogFile = bGenerateLog? wLogFile.copy(): wEmptyOut.copy();

   LgsString outText;
   if( !readFromFile(outText, outFile))
   {
      _bstr_t wErrText(WCERR_READ_OUTPUT);
      *uspErrorDesc = wErrText.copy();
      *uspTermSearchText = wEmptyOut.copy();
      *uspAlignedText = wEmptyOut.copy();
      *uspDiagText = wEmptyOut.copy();
      return S_OK;
   }
   _bstr_t wOutText(outText.c_str());
   *uspTermSearchText = wOutText.copy();
   if( FLAG_ON == ucGenerateAlignment )
   {
      LgsString alignText;
      if( !readFromFile(alignText, alignFile) )
      {
         _bstr_t wErrText(WCERR_READ_ALIGN);
         *uspErrorDesc = wErrText.copy();
         *uspTermSearchText = wEmptyOut.copy();
         *uspAlignedText = wEmptyOut.copy();
         *uspDiagText = wEmptyOut.copy();
         return S_OK;
      }
      _bstr_t wAlignText(alignText.c_str());
      *uspAlignedText = wAlignText.copy();
   }
   else
   {
      *uspAlignedText = wEmptyOut.copy();
   }
   
   if( NO_DIAG == ucDiagLevel )
   {
      *uspDiagText = wEmptyOut.copy();   
   }
   else
   {
      LgsString diagText;
      if( !readFromFile(diagText, diagFile) )
      {
         _bstr_t wErrText(WCERR_READ_DIAG);
         *uspErrorDesc = wErrText.copy();
         *uspTermSearchText = wEmptyOut.copy();
         *uspAlignedText = wEmptyOut.copy();
         *uspDiagText = wEmptyOut.copy();
         return S_OK;
      }
      _bstr_t wDiagText(diagText.c_str());
      *uspDiagText = wDiagText.copy();
      
   }
   *uspErrorDesc = wEmptyOut.copy();
   
   return S_OK;
}

STDMETHODIMP CXlationEngine::termSearchDoc(const BSTR usInputFile, short nSourceLanguage,
                                           short nTargetLanguage, short nInputFormat,
                                           unsigned short unProtectChar, const BSTR usSourceLocale,
                                           const BSTR usTargetLocale, unsigned char ucPmRulesFlag,
                                           unsigned char ucExtendedSearchFlag, const BSTR usSmcCodes,
                                           const BSTR usCompanyCodes, unsigned char ucFlagUnfoundWords,
                                           unsigned char ucLogosDefaultFlag, unsigned short unWordSearchOptions,
                                           long lWordSearchFoundStart, long lWordSearchFoundLimit,
                                           long lWordSearchUnfoundStart, long lWordSearchUnfoundLimit,
                                           unsigned char ucGenerateAlignment, unsigned char ucGenerateLog, unsigned char ucTargetImperativeForm,
                                           unsigned char ucDbModeFlag, unsigned char ucDebugFlag, unsigned char ucStatisticsFlag,
                                           unsigned char ucTraceFlag, unsigned char ucLogQueryFlag, unsigned char ucSaveScratchFlag,
                                           unsigned char ucDiagLevel, long lDiagStartLine, long lDiagEndLine,
                                           const BSTR usRuleOverrideFile,
                                           const BSTR usMainRes1, const BSTR usMainRes2, const BSTR usMainRes22,
                                           const BSTR usMainTran1, const BSTR usMainTran2,
                                           const BSTR usMainTran3, const BSTR usMainTran4,
                                           const BSTR usMainParse1, const BSTR usMainParse2,
                                           const BSTR usMainParse3, const BSTR usMainParse4,
                                           const BSTR usMiniRes2,
                                           const BSTR usMiniTran1, const BSTR usMiniTran2,
                                           const BSTR usMiniTran3, const BSTR usMiniTran4,
                                           const BSTR usMiniParse1, const BSTR usMiniParse2,
                                           const BSTR usMiniParse3, const BSTR usMiniParse4,
                                           const BSTR usUserId,
                                           BSTR *uspTermSearchFile, BSTR *uspAlignedFile, BSTR *uspDiagFile,
                                           long *lpSentenceCount, long *lpWordCount,
                                           long *lpSessionId, BSTR *uspLogFile, BSTR *uspErrorDesc)
{
   // TODO: Add your implementation code here
   int jobId = getJobId();
   LgsString filePrefix = getFilePrefix(jobId);

   int iExtSearch = (FLAG_ON == ucExtendedSearchFlag)? 2 : 1;

   LgsString companyCodes;
   ucToSbs(usCompanyCodes, companyCodes);
   LgsString smc;
   ucToSbs(usSmcCodes, smc);
   LgsString sourceLocale;
   ucToSbs(usSourceLocale, sourceLocale);
   LgsString targetLocale;
   ucToSbs(usTargetLocale, targetLocale);
   
   LgsString inputFormat;
   setInputFormat(nInputFormat, inputFormat);
   char protChar[2];
   protChar[0] = unProtectChar;
   protChar[1] = '\0';
   LgsString protectionChar(protChar);
   LgsString inputFile;
   ucToSbs(usInputFile, inputFile);
   LgsString outFile(filePrefix+COUT_EXT);
   LgsString diagFile(filePrefix+CDIAG_EXT);
   LgsString alignFile;
   ucGenerateAlignment = FLAG_OFF; // do not generate align file for termsearch job
   if( FLAG_ON == ucGenerateAlignment )
   {
      alignFile = filePrefix+CALIGN_EXT;
   }
   bool pmRuleFlag = (FLAG_ON == ucPmRulesFlag);
   
   LgsString mainRes1;
   ucToSbs(usMainRes1, mainRes1);
   LgsString mainRes2;
   ucToSbs(usMainRes2, mainRes2);
   LgsString mainRes22;
   ucToSbs(usMainRes22, mainRes22);
   LgsString mainTran1;
   ucToSbs(usMainTran1, mainTran1);
   LgsString mainTran2;
   ucToSbs(usMainTran2, mainTran2);
   LgsString mainTran3;
   ucToSbs(usMainTran3, mainTran3);
   LgsString mainTran4;
   ucToSbs(usMainTran4, mainTran4);
   LgsString mainParse1;
   ucToSbs(usMainParse1, mainParse1);
   LgsString mainParse2;
   ucToSbs(usMainParse2, mainParse2);
   LgsString mainParse3;
   ucToSbs(usMainParse3, mainParse3);
   LgsString mainParse4;
   ucToSbs(usMainParse4, mainParse4);
   LgsString miniRes2;
   ucToSbs(usMiniRes2, miniRes2);
   LgsString miniTran1;
   ucToSbs(usMiniTran1, miniTran1);
   LgsString miniTran2;
   ucToSbs(usMiniTran2, miniTran2);
   LgsString miniTran3;
   ucToSbs(usMiniTran3, miniTran3);
   LgsString miniTran4;
   ucToSbs(usMiniTran4, miniTran4);
   LgsString miniParse1;
   ucToSbs(usMiniParse1, miniParse1);
   LgsString miniParse2;
   ucToSbs(usMiniParse2, miniParse2);
   LgsString miniParse3;
   ucToSbs(usMiniParse3, miniParse3);
   LgsString miniParse4;
   ucToSbs(usMiniParse4, miniParse4);
   LgsString userId;
   ucToSbs(usUserId, userId);

   // ApiInitialize
   JobControlArguments *m_jcArgs = JobControlArguments::CreateObject(true);
   m_jcArgs->Initialize(
      TERM_SEARCH, jobId, userId,
      nSourceLanguage, nTargetLanguage, (FLAG_ON == ucTargetImperativeForm)? 1: 0,
      unWordSearchOptions, 
      lWordSearchFoundStart, lWordSearchFoundLimit, lWordSearchUnfoundStart, 
      lWordSearchUnfoundLimit, 1, ucDiagLevel, lDiagStartLine, lDiagEndLine, iExtSearch,
      companyCodes, smc, sourceLocale, targetLocale, inputFormat, protectionChar,
      mainRes1, mainRes2, mainRes22,
      mainTran1,  mainTran2, mainTran3,  mainTran4,
      mainParse1,  mainParse2, mainParse3,  mainParse4,
      miniRes2,
      miniTran1,  miniTran2, miniTran3,  miniTran4,
      miniParse1,  miniParse2, miniParse3,  miniParse4,
      inputFile, outFile, diagFile, alignFile, pmRuleFlag,
      (FLAG_ON == ucStatisticsFlag)? true: false,
      (FLAG_ON == ucTraceFlag)? true: false,
      (FLAG_ON == ucLogQueryFlag)? true: false,
      (FLAG_ON == ucSaveScratchFlag)? true: false
   );
   m_jcArgs->setDatabaseMode((FLAG_ON == ucDbModeFlag)? true : false);
   
   // Run Transl
   LgsString logFileName;
   bool bGenerateLog = (FLAG_ON == ucGenerateLog);
   if( bGenerateLog )
   {
      logFileName = filePrefix + CLOG_EXT;
   }
   RunTransl((FLAG_ON == ucDebugFlag), bGenerateLog, logFileName);
   
   // Set output values
   _bstr_t wEmptyOut(L"");
   _bstr_t wLogFile(logFileName.c_str());
   *uspLogFile = bGenerateLog? wLogFile.copy(): wEmptyOut.copy();
   _bstr_t wOutFile(outFile.c_str());
   _bstr_t wDiagFile(diagFile.c_str());
   _bstr_t wAlignFile(alignFile.c_str());
   *uspTermSearchFile = wOutFile.copy();
   *uspAlignedFile = (FLAG_ON == ucGenerateAlignment)? wAlignFile.copy(): wEmptyOut.copy();
   *uspDiagFile = (NO_DIAG == ucDiagLevel)? wEmptyOut.copy(): wDiagFile.copy();
   *lpSentenceCount = m_jcArgs->SentCurrentComplete();
   *lpWordCount = m_jcArgs->WordCount();
   *lpSessionId = jobId;
   *uspErrorDesc = wEmptyOut.copy();
   
   delete m_jcArgs;

   return S_OK;
}

STDMETHODIMP CXlationEngine::qryConfiguration(const BSTR usSmcTreeName, BSTR *uspLanguagePairs, 
                                              BSTR *uspCompanyCodes, BSTR *uspSmcCodes)
{
   // TODO: Add your implementation code here
   LgsString smcTreeName;
   LgsString smcTree;
   ucToSbs(usSmcTreeName, smcTreeName);
   SubjectMatterCodeTree oSmcTree;
   oSmcTree.setAsCompleteTree(smcTreeName);
   oSmcTree.getTreeAsString(smcTree);
   _bstr_t bSmcTree(smcTree.c_str());
   *uspSmcCodes = bSmcTree.copy();
   
   _bstr_t bLanguagePairs(languagePairs);
   *uspLanguagePairs = bLanguagePairs.copy();
   LgsString companyCodes;
   completeCompanyList(companyCodes);
   _bstr_t bCompanyCodes(companyCodes.c_str());
   *uspCompanyCodes = bCompanyCodes.copy();
   
   return S_OK;
}

STDMETHODIMP CXlationEngine::cleanup()
{
   // TODO: Add your implementation code here
   if( p_connection )
   {
      freeSqlConnection(p_connection);
   }
   return S_OK;
}

int RunTransl(bool bDebug, bool bGenerateLog, const LgsString &logFileName)
{
   int ret = -1;

   // Generate command
   LgsString cmd = "transl";

   // Create an absolute path with LGS_ROOT because i want to obviate the need
   // of putting the path to transl into the PATH variable
   cmd = LgsString(getenv(CLGS_ROOT)) + DIR_SEP_STR + "bin" + DIR_SEP_STR + cmd;

   if( bDebug )
   {
      // Get name of debugger
      CEnvVariable lgsDebugger(LGS_DEBUGGER);
      if( lgsDebugger.IsDefined() )
      {
         cmd = lgsDebugger.GetValue();
      }
      else
      {
         cmd = "msdev.exe"; // default debugger
      }
      // Get name of Transl workspace file
      ServerProperties server;
      LgsString translDSW = server.TranslDSWFile();
      if( !translDSW.empty() )
      {
         cmd+= ' ' + translDSW;
      }
   }

   if( bDebug || !bGenerateLog )
   {
      ret = ProfExec(cmd);
   }
   else
   {
#ifdef _MSC_VER
      if( AllocConsole() )
      {
         SetConsoleTitle("TranslationServer");
      }
#endif
      ret = SpawnCmd(cmd, logFileName);
   }

   return ret;
}

#ifdef _MSC_VER
int ProfExec(const LgsString &Cmd)
{
   DWORD nRetCode;
   STARTUPINFO si;
   PROCESS_INFORMATION pi;
   char cCmdBuf[512];

   ZeroMemory(&pi, sizeof(pi));
   ZeroMemory(&si, sizeof(si));
   si.cb = sizeof(si);
   strcpy(cCmdBuf, Cmd.c_str());

   // Start the child process. 
   if( !CreateProcess(NULL,    // No module name (use command line). 
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
      //cout << "Error: Process Creation failed....\n" << Cmd << "\n";
      return -1;
   }

   // Wait until child process exits.
   WaitForSingleObject(pi.hProcess, INFINITE);

   if( !GetExitCodeProcess(pi.hProcess, &nRetCode))
   {
      //cout << "Process error: Unable to get return code of\n '" << Cmd << "'\n";
      return -1;
   }

   // Close process and thread handles. 
   CloseHandle(pi.hProcess);
   CloseHandle(pi.hThread);

   return (int)nRetCode;
}


/* define USESTDHANDLES to use the new technique of passing the 
standard handles to the child process via the STARTUPINFO structure. 
This technique must be used for "console-less" parents such as GUI 
applications or detached applications. */ 
#define USESTDHANDLES 

int SpawnCmd(const LgsString &Cmd, const LgsString &logFileName)
{
   char chReadBuffer[128];  /* pipe read buffer */ 
   BOOL bSuccess;  /* BOOL return code for APIs */ 
   /* handles to the anonymous pipe */ 
   HANDLE hReadPipe, hWritePipe, hWritePipe2; 
   DWORD cchReadBuffer;  /* number of bytes read or to be written */ 
   STARTUPINFO si;  /* for CreateProcess call */ 
   PROCESS_INFORMATION pi;  /* for CreateProcess call */ 
   SECURITY_ATTRIBUTES saPipe;  /* security for anonymous pipe */ 

   /* create the log file where we will save all output from child */ 
   HANDLE hOutFile = CreateFile(logFileName.c_str(),  /* file to open */ 
      GENERIC_WRITE,  /* access mode */ 
      FILE_SHARE_READ,  /* share mode */ 
      NULL,  /* security attributes */ 
      CREATE_ALWAYS,  /* creation flags - trash existing file */ 
      FILE_ATTRIBUTE_NORMAL,  /* file attributes */ 
      NULL); 
   if( hOutFile == INVALID_HANDLE_VALUE )
   {
      return -1; // CreateFile
   }
   
   /* set up the security attributes for the anonymous pipe */ 
   saPipe.nLength = sizeof(SECURITY_ATTRIBUTES); 
   saPipe.lpSecurityDescriptor = NULL; 
   /* In order for the child to be able to write to the pipe, the handle */ 
   /* must be marked as inheritable by setting this flag: */ 
   saPipe.bInheritHandle = TRUE; 
   
   /* create the anonymous pipe */ 
   bSuccess = CreatePipe(&hReadPipe,  /* read handle */ 
      &hWritePipe,  /* write handle, used as stdout by child */ 
      &saPipe,  /* security descriptor */ 
      0);  /* pipe buffer size */ 
   if( !bSuccess )
   {
      return -1; // CreatePipe
   }
   
   /* Now we need to change the inheritable property for the readable 
   end of the pipe so that the child will not inherit that handle as 
   a "garbage" handle. This will keep us from having extra, 
   unclosable handles to the pipe. Alternatively, we could have 
   opened the pipe with saPipe.bInheritHandle = FALSE and changed the 
   inherit property on the *write* handle of the pipe to TRUE. */ 
   
   bSuccess = DuplicateHandle(GetCurrentProcess(), /* source process */ 
      hReadPipe, /* handle to duplicate */ 
      GetCurrentProcess(), /* destination process */ 
      NULL, /* new handle - don't want one, change original handle */ 
      0, /* new access flags - ignored since DUPLICATE_SAME_ACCESS */ 
      FALSE, /* make it *not* inheritable */ 
      DUPLICATE_SAME_ACCESS); 
   if( !bSuccess )
   {
      return -1; // DuplicateHandle
   }
   
   /* I most cases you can get away with using the same anonymous 
   pipe write handle for both the child's standard output and 
   standard error, but this may cause problems if the child app 
   explicitly closes one of its standard output or error handles. If 
   that happens, the anonymous pipe will close, since the child's 
   standard output and error handles are really the same handle. The 
   child won't be able to write to the other write handle since the 
   pipe is now gone, and parent reads from the pipe will return 
   ERROR_BROKEN_PIPE and child output will be lost. To solve this 
   problem, simply duplicate the write end of the pipe to create 
   another distinct, separate handle to the write end of the pipe. 
   One pipe write handle will serve as standard out, the other as 
   standard error. Now *both* write handles must be closed before the 
   write end of the pipe actually closes. */ 
   
   bSuccess = DuplicateHandle(GetCurrentProcess(), /* source process */ 
      hWritePipe, /* handle to duplicate */ 
      GetCurrentProcess(), /* destination process */ 
      &hWritePipe2, /* new handle, used as stderr by child */ 
      0, /* new access flags - ignored since DUPLICATE_SAME_ACCESS */ 
      TRUE, /* it's inheritable */ 
      DUPLICATE_SAME_ACCESS); 
   if( !bSuccess )
   {
      return -1; // DuplicateHandle
   }
   
   /* Set up the STARTUPINFO structure for the CreateProcess() call */ 
   memset(&si, 0, sizeof(si)); 
   si.cb = sizeof(si); 
   
#ifdef USESTDHANDLES 
   /* If using the STARTUPINFO STARTF_USESTDHANDLES flag, be sure to 
   set the CreateProcess fInheritHandles parameter too TRUE so that 
   the file handles specified in the STARTUPINFO structure will be 
   inheritied by the child. Note that we don't specify a standard 
   input handle; the child will not inherit a valid input handle, so 
   if it reads from stdin, it will encounter errors. */ 
   
   si.hStdInput = hWritePipe2; /* hStdInput needs a valid handle in case it is checked by the child */ 
   si.hStdOutput = hWritePipe; /* write end of the pipe */ 
   si.hStdError = hWritePipe2; /* duplicate of write end of the pipe */ 
   si.dwFlags = STARTF_USESTDHANDLES; 
#else 
   /* If we're not using the STARTF_USESTDHANDLES flag, set the 
   standard output and error handles to the end of the pipe we want 
   the child to inherit with SetStdHandle(). For this program, we 
   don't want standard input inherited so we'll also change the 
   handle inheritance property of standard input so that it is not 
   inherited */ 
   bSuccess = SetStdHandle(STD_INPUT_HANDLE, hWritePipe2); 
   if( !bSuccess )
   {
      return -1; // SetStdHandle
   }
   bSuccess = SetStdHandle(STD_OUTPUT_HANDLE, hWritePipe); 
   if( !bSuccess )
   {
      return -1; // SetStdHandle
   }
   bSuccess = SetStdHandle(STD_ERROR_HANDLE, hWritePipe2); 
   if( !bSuccess )
   {
      return -1; // SetStdHandle
   }
   bSuccess = DuplicateHandle(GetCurrentProcess(), /* source process */ 
      GetStdHandle(STD_INPUT_HANDLE), /* handle to duplicate */ 
      GetCurrentProcess(), /* destination process */ 
      NULL, /* new handle - don't want one, change original handle */ 
      0, /* new access flags - ignored since DUPLICATE_SAME_ACCESS */ 
      FALSE, /* it's *not* inheritable */ 
      DUPLICATE_SAME_ACCESS); 
   if( !bSuccess )
   {
      return -1; // DuplicateHandle
   }
#endif 
   
   /* Now create the child process, inheriting handles */ 
   char cCmdBuf[512];
   strcpy(cCmdBuf, Cmd.c_str());

   bSuccess = CreateProcess(NULL,  /* filename */ 
      cCmdBuf,  /* full command line for child */ 
      NULL,  /* process security descriptor */ 
      NULL,  /* thread security descriptor */ 
      TRUE,  /* inherit handles? Also use if STARTF_USESTDHANDLES */ 
      0,  /* creation flags */ 
      NULL,  /* inherited environment address */ 
      NULL,  /* startup dir; NULL = start in current */ 
      &si,  /* pointer to startup info (input) */ 
      &pi);  /* pointer to process info (output) */ 
   if( !bSuccess )
   {
      return -1; // CreateProcess
   }
   
   /* We need to close our instances of the inheritable pipe write 
   handle now that it's been inherited so that all open handles to 
   the pipe are closed when the child process ends and closes its 
   handles to the pipe. */ 
   
   bSuccess = CloseHandle(hWritePipe2); 
   if( !bSuccess )
   {
      return -1; // CloseHandle
   }
   bSuccess = CloseHandle(hWritePipe); 
   if( !bSuccess )
   {
      return -1; // CloseHandle
   }
   
   /* read from the pipe until we get an ERROR_BROKEN_PIPE */ 
   HANDLE hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
   for (;;) 
   { 
      bSuccess = ReadFile(hReadPipe,  /* read handle */ 
         chReadBuffer,  /* buffer for incoming data */ 
         sizeof(chReadBuffer),  /* number of bytes to read */ 
         &cchReadBuffer,  /* number of bytes actually read */ 
         NULL);  /* no overlapped reading */ 
      if (!bSuccess && (GetLastError() == ERROR_BROKEN_PIPE)) 
         break;  /* child has died */ 
      
      if (bSuccess && cchReadBuffer) 
      { 
         /* write the data from the child to the file */ 
         bSuccess = WriteFile(hOutFile,  /* write handle */ 
            chReadBuffer,  /* buffer to write */ 
            cchReadBuffer,  /* number of bytes to write */ 
            &cchReadBuffer,  /* number of bytes actually written */ 
            NULL);  /* no overlapped writing */ 
         
         /* write buffer (of specified length) to console */ 
         bSuccess = WriteFile(hStdout,  /* write handle */
            chReadBuffer,  /* buffer to write */
            cchReadBuffer,  /* number of bytes to write */
            &cchReadBuffer,  /* number of bytes actually written */
            NULL);  /* no overlapped writing */
      } 
   } 

   /* close the trace file, pipe handles */ 
   CloseHandle(hOutFile); 
   CloseHandle(hReadPipe); 

   DWORD nRetCode;
   // Wait until child process exits.
   WaitForSingleObject(pi.hProcess, INFINITE);

   if( !GetExitCodeProcess(pi.hProcess, &nRetCode))
   {
      //cout << "Process error: Unable to get return code of\n '" << Cmd << "'\n";
      return -1;
   }

   // Close process and thread handles. 
   CloseHandle(pi.hProcess);
   CloseHandle(pi.hThread);

   return (int)nRetCode;
}

#else
#include <sys/wait.h>

int ProfExec(const LgsString &strCmd)
{
  pid_t newpid = fork();
  switch (newpid) {
  case -1:
    // cout << "Error: Process Fork failed....\n" << strCmd << endl;
    return -1;
  case 0: 
    {
      // this is the child: separate the program name from the args
      // _fix_me_ right now, we don't need it because there are no args
      const char *progname = strCmd.c_str();
      //printf("Doing execlp with %s\n", progname);
      if (execlp(progname, progname, NULL) < 0) {
        // cout << "Error: Process Creation failed....\n" << strCmd << "\n";
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
      if (termpid < 0 || ! WIFEXITED(status)) {
        //cout << "Process error: Unable to get return code of\n '" 
        //     << strCmd << endl;
        return -1;
      }
      int retcode = WEXITSTATUS(status);
      
      return retcode;
    }
  }
}

/* define USESTDHANDLES to use the new technique of passing the 
standard handles to the child process via the STARTUPINFO structure. 
This technique must be used for "console-less" parents such as GUI 
applications or detached applications. */ 
#define USESTDHANDLES 

int SpawnCmd(const LgsString &strCmd, const LgsString &logFileName)
{
  char chReadBuffer[128];  /* pipe read buffer */ 
  int cchReadBuffer = 1;   /* number of bytes read or to be written */ 

  int pipe_fd[2];          /* handles to the anonymous pipe */ 
   
  /* Open logfile */
  FILE *hOutFile = fopen(logFileName.c_str(), "w");
  if (hOutFile == NULL) return -1;
      
  /* create the anonymous pipe */ 
  if (pipe(pipe_fd) < 0) return -1;
   
  pid_t newpid = fork();   /* pid of current process after fork */  

  switch (newpid) {
  case -1:
    // cout << "Error: Process Fork failed....\n" << strCmd << endl;
    return -1;
  case 0: 
    {
      // this is the child: separate the program name from the args
      // _fix_me_ right now, we don't need it because there are no args
      const char *progname = strCmd.c_str();

      close(pipe_fd[0]);	/* close read end */
      if (pipe_fd[1] != STDOUT_FILENO) {
        if (dup2(pipe_fd[1], STDOUT_FILENO) != STDOUT_FILENO)
          return -1;
        close(pipe_fd[1]);	/* don't need this after dup2 */
      }

      //printf("Now doing execlp with %s\n", progname);
      if (execlp(progname, progname, NULL) < 0) {
        printf("Execlp failed: %s\n",progname);
        // cout << "Error: Process Creation failed....\n" << strCmd << "\n";
        return -1;
      }
    }
    break;
  default:
    {
      // this is the parent process
      close(pipe_fd[1]);  /* close write end */

      // Read from pipe until we get an error (broken pipe)

      while (cchReadBuffer > 0) {
        cchReadBuffer = read(pipe_fd[0], chReadBuffer, 128);
        if (cchReadBuffer > 0) {
          // log to file
          fwrite(chReadBuffer, 1, cchReadBuffer, hOutFile);

          // log to stdout
          write(STDOUT_FILENO, chReadBuffer, cchReadBuffer);
        }
      }

      fclose(hOutFile);
      close(pipe_fd[0]);

      int status;
      // Wait for child to terminate
      pid_t termpid = waitpid(newpid, &status, 0);
      if (termpid < 0 || ! WIFEXITED(status)) {
        //cout << "Process error: Unable to get return code of\n '" 
        //     << strCmd << endl;
        return -1;
      }
      int retcode = WEXITSTATUS(status);

      return retcode;
    }
  }
}

#endif
