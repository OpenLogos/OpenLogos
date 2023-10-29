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
// lgscominterface.cpp: implementation of the LgsComInterface class.
//
//////////////////////////////////////////////////////////////////////

#define _WIN32_DCOM

#include "lgscominterface.h"
#include <stdlib.h>
#include <engine_api/translationserver/comparameters.h>
#include <engine_api/xlationinterface/xlationinterfacedefs.h>
#include <engine_api/xlationinterface/xlationsession.h>
#include <engine_api/proxylayer/lgstokenizer.h>

#ifdef _MSC_VER
#include <objbase.h>

const IID IID_IXlationEngine = {0x1B750F45,0x3B23,0x11D4,{0x96,0x8D,0x00,0xA0,0xCC,0x51,0xB8,0x4B}};
const CLSID CLSID_XlationEngine = {0x1B750F46,0x3B23,0x11D4,{0x96,0x8D,0x00,0xA0,0xCC,0x51,0xB8,0x4B}};
#else
FILE *_wfopen(const wchar_t *wname, const wchar_t *wmode) {
  FILE *fdesc;
  int len = char_traits<wchar_t>::length(wname);
  char *name = new char[len * sizeof(wchar_t)];
  wcstombs(name, wname, len);
  len = char_traits<wchar_t>::length(wmode);
  char *mode = new char[len * sizeof(wchar_t)];
  wcstombs(mode, wmode, len);
  fdesc = fopen(name, mode);
  delete[] name;
  delete[] mode;
  return fdesc;
}

#endif

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

bool LgsComInterface::m_bInitialized = false;
ofstream LgsComInterface::m_LogStream("proxy.log", ios_base::out|ios_base::app);

LgsComInterface::LgsComInterface()
{
#ifdef _MSC_VER
   HRESULT hr;
   if (!m_bInitialized)
   {
      hr = CoInitializeEx(NULL, COINIT_MULTITHREADED);
      if (!SUCCEEDED(hr))
      {
         // Log error here
         errorCode(PROXY_ERR_INIT);
         m_LogStream << "Initialization Failure (CoInitializeEx())" << endl;
         m_LogStream << "HRESULT : " << hr << endl << flush;
         return;
      }
      m_bInitialized = true;
   }
   LPVOID qi = 0;
   hr = CoCreateInstance(CLSID_XlationEngine, NULL, CLSCTX_LOCAL_SERVER, IID_IXlationEngine, &qi);
   
   if (!SUCCEEDED(hr))
   {
      // Log Error
      errorCode(PROXY_ERR_CREATE);
      m_LogStream << "Interface Creation Failure (CoCreateInstance())" << endl;
      m_LogStream << "HRESULT : " << hr << endl << flush;
      return;
   }
   m_pXlationEngine = (IXlationEngine *) qi;
#else
   m_pXlationEngine = new CXlationEngine();
#endif
}

LgsComInterface::~LgsComInterface()
{
   if (m_pXlationEngine)
   {
      m_pXlationEngine->cleanup();
#ifdef _MSC_VER
      m_pXlationEngine->Release();
#else
      delete m_pXlationEngine;
#endif
      m_pXlationEngine = 0;
   }
}

bool LgsComInterface::translateText(XlationSession *pSession, unsigned long timeOut)
{
   // Extract parameters and process request
   errorCode(SUCCESS);
   // Output values from the translation server
   long sentenceCount = 0;
   long wordCount = 0;
   long sessionId = 0;
   BSTR logFile = 0;
   BSTR errorDesc = 0;
   BSTR translatedText = 0;
   BSTR diagText = 0;
   BSTR alignedText = 0;
   
   short sLang = sourceLanguage(pSession);
   short tLang = targetLanguage(pSession);
   if (INVALID_LANGUAGE == sLang || INVALID_LANGUAGE == tLang)
   {
      errorCode(PROXY_ERR_LANGUAGE);
      return false;
   }
   
   HRESULT hr;
   hr = m_pXlationEngine->translateText(
      inputText(pSession), sLang, tLang, inputFormat(pSession),
      protectionChar(pSession), sourceLocale(pSession),
      targetLocale(pSession), pmRulesFlag(pSession),
      extendedSearch(pSession), subjectMatterCodes(pSession),
      companyCodes(pSession), flagUnfoundWords(pSession),
      generateAlignment(pSession), generateLog(pSession), targetImperativeForm(pSession),
      dbModeFlag(pSession), debugFlag(pSession), statisticsFlag(pSession),
      traceFlag(pSession), logQueryFlag(pSession), saveScratchFlag(pSession),
      diagLevel(pSession), diagLineStart(pSession), diagLineEnd(pSession),
      ruleOverrideFile(pSession),
      mainRes1File(pSession), mainRes2File(pSession), mainRes22File(pSession),
      mainTran1File(pSession),  mainTran2File(pSession), mainTran3File(pSession),  mainTran4File(pSession),
      mainParse1File(pSession),  mainParse2File(pSession), mainParse3File(pSession),  mainParse4File(pSession),
      miniRes2File(pSession),
      miniTran1File(pSession),  miniTran2File(pSession), miniTran3File(pSession),  miniTran4File(pSession),
      miniParse1File(pSession),  miniParse2File(pSession), miniParse3File(pSession),  miniParse4File(pSession),
      userID(pSession),
      &translatedText, &alignedText, &diagText,
      &sentenceCount, &wordCount, &sessionId, &logFile, &errorDesc);
   if (!SUCCEEDED(hr))
   {
      // Log Error
      errorCode(PROXY_ERR_CALL);
      m_LogStream << "Interface Call Failure (translateDoc())" << endl;
      m_LogStream << "HRESULT : " << hr << endl << flush;
      return false;
   }
   clearOutputValues(pSession);
   UString returnedValue;
   _bstr_t bsErrorDesc(errorDesc, false);
   returnedValue = bsErrorDesc;
   if (!returnedValue.empty())
   {
      setOutputValue(pSession, WCERROR_DESC, returnedValue);
      errorCode(PROXY_ERR_SERVER);
      return false;
   }
   _bstr_t bsLogFile(logFile, false);
   returnedValue = bsLogFile;
   if (!returnedValue.empty())
   {
      setOutputValue(pSession, WCLOG_FILE, returnedValue);
   }
   _bstr_t bsTranslatedText(translatedText, false);
   returnedValue = bsTranslatedText;
   setOutputValue(pSession, WCOUTPUT_TEXT, returnedValue);
   _bstr_t bsAlignedText(alignedText, false);
   returnedValue = bsAlignedText;
   if (!returnedValue.empty())
   {
      setOutputValue(pSession, WCALIGNED_TEXT, returnedValue);
   }
   _bstr_t bsDiagText(diagText, false);
   returnedValue = bsDiagText;
   if (!returnedValue.empty())
   {
      setOutputValue(pSession, WCDIAG_TEXT, returnedValue);
   }
   wchar_t wcarray[40];
   swprintf( wcarray, 40, L"%d", sentenceCount);
   setOutputValue(pSession, WCSENTENCE_COUNT, wcarray);
   swprintf( wcarray, 40, L"%d", wordCount);
   setOutputValue(pSession, WCWORD_COUNT, wcarray);
   swprintf( wcarray, 40, L"%d", sessionId);
   setOutputValue(pSession, WCSESSION_ID, wcarray);
   
   return true;
}

bool LgsComInterface::translateDoc(XlationSession *pSession, unsigned long timeOut)
{
   errorCode(SUCCESS);
   // Output values from the translation server
   long sentenceCount = 0;
   long wordCount = 0;
   long sessionId = 0;
   BSTR logFile = 0;
   BSTR errorDesc = 0;
   BSTR translatedFile = 0;
   BSTR diagFile = 0;
   BSTR alignedFile = 0;
   
   short sLang = sourceLanguage(pSession);
   short tLang = targetLanguage(pSession);
   if (INVALID_LANGUAGE == sLang || INVALID_LANGUAGE == tLang)
   {
      errorCode(PROXY_ERR_LANGUAGE);
      return false;
   }
   
   FILE *fdesc;
   if ((fdesc = _wfopen(pSession->getInputParameter(WCINPUT_FILE), L"r")) == NULL)
   {
      errorCode(PROXY_ERR_FILE);
      return false;
   }
   fclose(fdesc);
   
   
   HRESULT hr;
   hr = m_pXlationEngine->translateDoc(
      inputFile(pSession), sLang, tLang, inputFormat(pSession),
      protectionChar(pSession), sourceLocale(pSession),
      targetLocale(pSession), pmRulesFlag(pSession),
      extendedSearch(pSession), subjectMatterCodes(pSession),
      companyCodes(pSession), flagUnfoundWords(pSession),
      generateAlignment(pSession), generateLog(pSession), targetImperativeForm(pSession),
      dbModeFlag(pSession), debugFlag(pSession), statisticsFlag(pSession),
      traceFlag(pSession), logQueryFlag(pSession), saveScratchFlag(pSession),
      diagLevel(pSession), diagLineStart(pSession), diagLineEnd(pSession),
      ruleOverrideFile(pSession),
      mainRes1File(pSession), mainRes2File(pSession), mainRes22File(pSession),
      mainTran1File(pSession),  mainTran2File(pSession), mainTran3File(pSession),  mainTran4File(pSession),
      mainParse1File(pSession),  mainParse2File(pSession), mainParse3File(pSession),  mainParse4File(pSession),
      miniRes2File(pSession),
      miniTran1File(pSession),  miniTran2File(pSession), miniTran3File(pSession),  miniTran4File(pSession),
      miniParse1File(pSession),  miniParse2File(pSession), miniParse3File(pSession),  miniParse4File(pSession),
      userID(pSession),
      &translatedFile, &alignedFile, &diagFile,
      &sentenceCount, &wordCount, &sessionId, &logFile, &errorDesc);
   if (!SUCCEEDED(hr))
   {
      // Log Error
      errorCode(PROXY_ERR_CALL);
      m_LogStream << "Interface Call Failure (translateDoc())" << endl;
      m_LogStream << "HRESULT : " << hr << endl << flush;
      return false;
   }
   clearOutputValues(pSession);
   UString returnedValue;
   _bstr_t bsErrorDesc(errorDesc, false);
   returnedValue = bsErrorDesc;
   if (!returnedValue.empty())
   {
      setOutputValue(pSession, WCERROR_DESC, returnedValue);
      errorCode(PROXY_ERR_SERVER);
      return false;
   }
   _bstr_t bsLogFile(logFile, false);
   returnedValue = bsLogFile;
   if (!returnedValue.empty())
   {
      setOutputValue(pSession, WCLOG_FILE, returnedValue);
   }
   _bstr_t bsTranslatedFile(translatedFile, false);
   returnedValue = bsTranslatedFile;
   setOutputValue(pSession, WCOUTPUT_FILE, returnedValue);
   _bstr_t bsAlignedFile(alignedFile, false);
   returnedValue = bsAlignedFile;
   if (!returnedValue.empty())
   {
      setOutputValue(pSession, WCALIGNMENT_FILE, returnedValue);
   }
   _bstr_t bsDiagFile(diagFile, false);
   returnedValue = bsDiagFile;
   if (!returnedValue.empty())
   {
      setOutputValue(pSession, WCDIAG_FILE, returnedValue);
   }
   wchar_t wcarray[40];
   swprintf( wcarray, 40, L"%d", sentenceCount);
   setOutputValue(pSession, WCSENTENCE_COUNT, wcarray);
   swprintf( wcarray, 40, L"%d", wordCount);
   setOutputValue(pSession, WCWORD_COUNT, wcarray);
   swprintf( wcarray, 40, L"%d", sessionId);
   setOutputValue(pSession, WCSESSION_ID, wcarray);
   
   return true;
}

bool LgsComInterface::termSearchText(XlationSession *pSession, unsigned long timeOut)
{
   // Extract parameters and process request
   errorCode(SUCCESS);
   // Output values from the translation server
   long sentenceCount = 0;
   long wordCount = 0;
   long sessionId = 0;
   BSTR logFile = 0;
   BSTR errorDesc = 0;
   BSTR outputText = 0;
   BSTR diagText = 0;
   BSTR alignedText = 0;
   
   short sLang = sourceLanguage(pSession);
   short tLang = targetLanguage(pSession);
   if (INVALID_LANGUAGE == sLang || INVALID_LANGUAGE == tLang)
   {
      errorCode(PROXY_ERR_LANGUAGE);
      return false;
   }
   
   HRESULT hr;
   hr = m_pXlationEngine->termSearchText(
      inputText(pSession), sLang, tLang, inputFormat(pSession),
      protectionChar(pSession), sourceLocale(pSession),
      targetLocale(pSession), pmRulesFlag(pSession),
      extendedSearch(pSession), subjectMatterCodes(pSession),
      companyCodes(pSession), flagUnfoundWords(pSession),
      searchDefaultFlag(pSession), wordSearchOption(pSession),
      wordSearchFoundStart(pSession), wordSearchFoundLimit(pSession),
      wordSearchUnfoundStart(pSession), wordSearchUnfoundLimit(pSession),
      generateAlignment(pSession), generateLog(pSession), targetImperativeForm(pSession),
      dbModeFlag(pSession), debugFlag(pSession), statisticsFlag(pSession),
      traceFlag(pSession), logQueryFlag(pSession), saveScratchFlag(pSession),
      diagLevel(pSession), diagLineStart(pSession), diagLineEnd(pSession),
      ruleOverrideFile(pSession),
      mainRes1File(pSession), mainRes2File(pSession), mainRes22File(pSession),
      mainTran1File(pSession),  mainTran2File(pSession), mainTran3File(pSession),  mainTran4File(pSession),
      mainParse1File(pSession),  mainParse2File(pSession), mainParse3File(pSession),  mainParse4File(pSession),
      miniRes2File(pSession),
      miniTran1File(pSession),  miniTran2File(pSession), miniTran3File(pSession),  miniTran4File(pSession),
      miniParse1File(pSession),  miniParse2File(pSession), miniParse3File(pSession),  miniParse4File(pSession),
      userID(pSession),
      &outputText, &alignedText, &diagText,
      &sentenceCount, &wordCount, &sessionId, &logFile, &errorDesc);
   if (!SUCCEEDED(hr))
   {
      // Log Error
      errorCode(PROXY_ERR_CALL);
      m_LogStream << "Interface Call Failure (translateDoc())" << endl;
      m_LogStream << "HRESULT : " << hr << endl << flush;
      return false;
   }
   clearOutputValues(pSession);
   UString returnedValue;
   _bstr_t bsErrorDesc(errorDesc, false);
   returnedValue = bsErrorDesc;
   if (!returnedValue.empty())
   {
      setOutputValue(pSession, WCERROR_DESC, returnedValue);
      errorCode(PROXY_ERR_SERVER);
      return false;
   }
   _bstr_t bsLogFile(logFile, false);
   returnedValue = bsLogFile;
   if (!returnedValue.empty())
   {
      setOutputValue(pSession, WCLOG_FILE, returnedValue);
   }
   _bstr_t bsOutputText(outputText, false);
   returnedValue = bsOutputText;
   setOutputValue(pSession, WCOUTPUT_TEXT, returnedValue);
   _bstr_t bsAlignedText(alignedText, false);
   returnedValue = bsAlignedText;
   if (!returnedValue.empty())
   {
      setOutputValue(pSession, WCALIGNED_TEXT, returnedValue);
   }
   _bstr_t bsDiagText(diagText, false);
   returnedValue = bsDiagText;
   if (!returnedValue.empty())
   {
      setOutputValue(pSession, WCDIAG_TEXT, returnedValue);
   }
   wchar_t wcarray[40];
   swprintf( wcarray, 40, L"%d", sentenceCount);
   setOutputValue(pSession, WCSENTENCE_COUNT, wcarray);
   swprintf( wcarray, 40, L"%d", wordCount);
   setOutputValue(pSession, WCWORD_COUNT, wcarray);
   swprintf( wcarray, 40, L"%d", sessionId);
   setOutputValue(pSession, WCSESSION_ID, wcarray);
   
   return true;
}

bool LgsComInterface::termSearchDoc(XlationSession *pSession, unsigned long timeOut)
{
   errorCode(SUCCESS);
   // Output values from the translation server
   long sentenceCount = 0;
   long wordCount = 0;
   long sessionId = 0;
   BSTR logFile = 0;
   BSTR errorDesc = 0;
   BSTR outputFile = 0;
   BSTR diagFile = 0;
   BSTR alignedFile = 0;
   
   short sLang = sourceLanguage(pSession);
   short tLang = targetLanguage(pSession);
   if (INVALID_LANGUAGE == sLang || INVALID_LANGUAGE == tLang)
   {
      errorCode(PROXY_ERR_LANGUAGE);
      return false;
   }
   
   FILE *fdesc;
   if ((fdesc = _wfopen(pSession->getInputParameter(WCINPUT_FILE), L"r")) == NULL)
   {
      errorCode(PROXY_ERR_FILE);
      return false;
   }
   fclose(fdesc);
   
   
   HRESULT hr;
   hr = m_pXlationEngine->termSearchDoc(
      inputFile(pSession), sLang, tLang, inputFormat(pSession),
      protectionChar(pSession), sourceLocale(pSession),
      targetLocale(pSession), pmRulesFlag(pSession),
      extendedSearch(pSession), subjectMatterCodes(pSession),
      companyCodes(pSession), flagUnfoundWords(pSession),
      searchDefaultFlag(pSession), wordSearchOption(pSession),
      wordSearchFoundStart(pSession), wordSearchFoundLimit(pSession),
      wordSearchUnfoundStart(pSession), wordSearchUnfoundLimit(pSession),
      generateAlignment(pSession), generateLog(pSession), targetImperativeForm(pSession),
      dbModeFlag(pSession), debugFlag(pSession), statisticsFlag(pSession),
      traceFlag(pSession), logQueryFlag(pSession), saveScratchFlag(pSession),
      diagLevel(pSession), diagLineStart(pSession), diagLineEnd(pSession),
      ruleOverrideFile(pSession),
      mainRes1File(pSession), mainRes2File(pSession), mainRes22File(pSession),
      mainTran1File(pSession),  mainTran2File(pSession), mainTran3File(pSession),  mainTran4File(pSession),
      mainParse1File(pSession),  mainParse2File(pSession), mainParse3File(pSession),  mainParse4File(pSession),
      miniRes2File(pSession),
      miniTran1File(pSession),  miniTran2File(pSession), miniTran3File(pSession),  miniTran4File(pSession),
      miniParse1File(pSession),  miniParse2File(pSession), miniParse3File(pSession),  miniParse4File(pSession),
      userID(pSession),
      &outputFile, &alignedFile, &diagFile,
      &sentenceCount, &wordCount, &sessionId, &logFile, &errorDesc);
   if (!SUCCEEDED(hr))
   {
      // Log Error
      errorCode(PROXY_ERR_CALL);
      m_LogStream << "Interface Call Failure (translateDoc())" << endl;
      m_LogStream << "HRESULT : " << hr << endl << flush;
      return false;
   }
   clearOutputValues(pSession);
   UString returnedValue;
   _bstr_t bsErrorDesc(errorDesc, false);
   returnedValue = bsErrorDesc;
   if (!returnedValue.empty())
   {
      setOutputValue(pSession, WCERROR_DESC, returnedValue);
      errorCode(PROXY_ERR_SERVER);
      return false;
   }
   _bstr_t bsLogFile(logFile, false);
   returnedValue = bsLogFile;
   if (!returnedValue.empty())
   {
      setOutputValue(pSession, WCLOG_FILE, returnedValue);
   }
   _bstr_t bsOutputFile(outputFile, false);
   returnedValue = bsOutputFile;
   setOutputValue(pSession, WCOUTPUT_FILE, returnedValue);
   _bstr_t bsAlignedFile(alignedFile, false);
   returnedValue = bsAlignedFile;
   if (!returnedValue.empty())
   {
      setOutputValue(pSession, WCALIGNMENT_FILE, returnedValue);
   }
   _bstr_t bsDiagFile(diagFile, false);
   returnedValue = bsDiagFile;
   if (!returnedValue.empty())
   {
      setOutputValue(pSession, WCDIAG_FILE, returnedValue);
   }
   wchar_t wcarray[40];
   swprintf( wcarray, 40, L"%d", sentenceCount);
   setOutputValue(pSession, WCSENTENCE_COUNT, wcarray);
   swprintf( wcarray, 40, L"%d", wordCount);
   setOutputValue(pSession, WCWORD_COUNT, wcarray);
   swprintf( wcarray, 40, L"%d", sessionId);
   setOutputValue(pSession, WCSESSION_ID, wcarray);
   
   return true;
}

bool LgsComInterface::qryConfiguration(XlationSession *pSession, unsigned long timeOut)
{
   BSTR languagePairs = 0;
   BSTR companyCodes = 0;
   BSTR smcCodes = 0;
   const wchar_t* wcSmcTreeName = pSession->getInputParameter(WCSMC_TREE_NAME);
   _bstr_t smcTreeName(wcSmcTreeName);
   if (0 == smcTreeName.length())
   {
      smcTreeName = WCSMC_TREE_DEFAULT;
   }
   
   HRESULT hr;
   hr = m_pXlationEngine->qryConfiguration(smcTreeName, &languagePairs, &companyCodes, &smcCodes);
   if (!SUCCEEDED(hr))
   {
      // Log Error
      errorCode(PROXY_ERR_CALL);
      m_LogStream << "Interface Call Failure (translateDoc())" << endl;
      m_LogStream << "HRESULT : " << hr << endl << flush;
      return false;
   }
   
   clearOutputValues(pSession);
   UString returnedValue;
   // If an empty string is returned for language pairs,
   // it is an error condition 
   // and the error string is returned with the parameter 
   // companyCodes
   // If possible, change later to add an extra out parameter to the 
   // COM interface and return the error value through that parameter.
   _bstr_t bsLanguagePairs(languagePairs, false);
   returnedValue = bsLanguagePairs;
   _bstr_t bsCompanyCodes(companyCodes, false);
   
   if (returnedValue.empty())
   {
      returnedValue = bsCompanyCodes;
      setOutputValue(pSession, WCERROR_DESC, returnedValue);
      errorCode(PROXY_ERR_SERVER);
      return false;
   }
   
   setOutputValue(pSession, WCOLANGUAGE_PAIRS, returnedValue);
   returnedValue = bsCompanyCodes;
   setOutputValue(pSession, WCOCOMPANY_CODES, returnedValue);
   _bstr_t bsSmcCodes(smcCodes, false);
   returnedValue = bsSmcCodes;
   setOutputValue(pSession, WCOSMC_CODES, returnedValue);
   return true;
}

_bstr_t LgsComInterface::inputText(XlationSession* pSession) const
{
   const wchar_t* wcText = pSession->getInputParameter(WCINPUT_TEXT);
   _bstr_t retText(wcText);
   return retText;
}

_bstr_t LgsComInterface::inputFile(XlationSession* pSession) const
{
   const wchar_t* wcText = pSession->getInputParameter(WCINPUT_FILE);
   _bstr_t retText(wcText);
   return retText;
}

short LgsComInterface::sourceLanguage(XlationSession* pSession) const
{
   return languageCode(pSession->getInputParameter(WCSOURCE_LANGUAGE));
}

short LgsComInterface::targetLanguage(XlationSession* pSession) const
{
   return languageCode(pSession->getInputParameter(WCTARGET_LANGUAGE));
}

short LgsComInterface::languageCode(const wchar_t *usText) const
{
   short retVal = INVALID_LANGUAGE;
   if (!wcscmp(usText, WCENG))
   {
      retVal = ENGLISH;    
   }
   else if (!wcscmp(usText, WCGER))
   {
      retVal = GERMAN;
   }
   else if (!wcscmp(usText, WCSPA))
   {
      retVal = SPANISH;
   }
   else if (!wcscmp(usText, WCITA))
   {
      retVal = ITALIAN;
   }
   else if(!wcscmp(usText, WCPOR))
   {
      retVal = PORTUGUESE;
   }
   else if (!wcscmp(usText, WCFRE))
   {
      retVal = FRENCH;
   }
   return retVal;
}

short LgsComInterface::inputFormat(XlationSession* pSession) const
{
   short retVal = LGS_TXT_EXCHANGE;
   const wchar_t *usText = pSession->getInputParameter(WCINPUT_FORMAT);
   if (!wcscmp(usText, WCRTF))
   {
      retVal = RTF;    
   }
   else if (!wcscmp(usText, WCSGML))
   {
      retVal = SGML;
   }
   else if (!wcscmp(usText, WCHTML))
   {
      retVal = HTML;
   }
   else if (!wcscmp(usText, WCTMX))
   {
      retVal = TMX;
   }
   else if (!wcscmp(usText, WCXML))
   {
      retVal = XML;
   }
   else if(!wcscmp(usText, WCINTERLEAF_ASCII))
   {
      retVal = INTERLEAF_ASCII;
   }
   else if (!wcscmp(usText, WCFRAME_MIF))
   {
      retVal = FRAME_MIF;
   }
   else if (!wcscmp(usText, WCMS_WORD8))
   {
      retVal = MS_WORD8;
   }
   else if (!wcscmp(usText, WCRTF_WINHELP))
   {
      retVal = RTF_WINHELP;
   }
   return retVal;
}

unsigned short LgsComInterface::protectionChar(XlationSession* pSession) const
{
   unsigned short retVal = L'^';
   const wchar_t *usText = pSession->getInputParameter(WCPROTECTION_CHAR);
   if (usText && L'\0' != usText[0])
   {
      retVal = usText[0];
   }
   return retVal;
}

_bstr_t LgsComInterface::sourceLocale(XlationSession* pSession) const
{
   const wchar_t* wcText = pSession->getInputParameter(WCSOURCE_LOCALE);
   _bstr_t retText(wcText);
   return retText;
}

_bstr_t LgsComInterface::targetLocale(XlationSession* pSession) const
{
   const wchar_t* wcText = pSession->getInputParameter(WCTARGET_LOCALE);
   _bstr_t retText(wcText);
   return retText;
}

unsigned char LgsComInterface::pmRulesFlag(XlationSession* pSession) const
{
   unsigned char retVal = FLAG_OFF;
   if (!wcscmp(WCYES, pSession->getInputParameter(WCPM_RULES)))
   {
      retVal = FLAG_ON;
   }
   return retVal;   
}

unsigned char LgsComInterface::dbModeFlag(XlationSession* pSession) const
{
   unsigned char retVal = FLAG_OFF;
   if (!wcscmp(WCYES, pSession->getInputParameter(WCDB_MODE_FLAG)))
   {
      retVal = FLAG_ON;
   }
   return retVal;   
}
unsigned char LgsComInterface::extendedSearch(XlationSession* pSession) const
{
   unsigned char retVal = FLAG_OFF;
   if (!wcscmp(WCEXT_SEARCH, pSession->getInputParameter(WCEXTENDED_SEARCH)))
   {
      retVal = FLAG_ON;
   }
   return retVal;
}

_bstr_t LgsComInterface::subjectMatterCodes(XlationSession* pSession) const
{
   const wchar_t* wcText = pSession->getInputParameter(WCSUBJECT_MATTER_CODES);
   _bstr_t retText(wcText);
   return retText;
}

_bstr_t LgsComInterface::companyCodes(XlationSession* pSession) const
{
   const wchar_t* wcText = pSession->getInputParameter(WCCOMPANY_CODES);
   _bstr_t retText(wcText);
   return retText;
}

unsigned char LgsComInterface::flagUnfoundWords(XlationSession* pSession) const
{
   unsigned char retVal = FLAG_OFF;
   if (!wcscmp(WCYES, pSession->getInputParameter(WCFLAG_UNFOUND_WORDS)))
   {
      retVal = FLAG_ON;
   }
   return retVal;    
}

unsigned char LgsComInterface::generateAlignment(XlationSession* pSession) const
{
   unsigned char retVal = FLAG_OFF;
   if (!wcscmp(WCYES, pSession->getInputParameter(WCGENERATE_ALIGNMENT)))
   {
      retVal = FLAG_ON;
   }
   return retVal;    
}

unsigned char LgsComInterface::targetImperativeForm(XlationSession* pSession) const
{
   unsigned char retVal = FLAG_ON;
   if (wcscmp(WCYES, pSession->getInputParameter(WCTARGET_FORM_IMPERATIVE)))
   {
      retVal = FLAG_OFF;
   }
   return retVal;    
}

unsigned char LgsComInterface::statisticsFlag(XlationSession* pSession) const
{
   unsigned char retVal = FLAG_OFF;
   if (!wcscmp(WCYES, pSession->getInputParameter(WCSTATISTICS_FLAG)))
   {
      retVal = FLAG_ON;
   }
   return retVal;    
}

unsigned char LgsComInterface::debugFlag(XlationSession* pSession) const
{
   unsigned char retVal = FLAG_OFF;
   if (!wcscmp(WCYES, pSession->getInputParameter(WCDEBUG_FLAG)))
   {
      retVal = FLAG_ON;
   }
   return retVal;    
}


unsigned char LgsComInterface::diagLevel(XlationSession* pSession) const
{
   unsigned char retVal = NO_DIAG;
   const wchar_t *usText = pSession->getInputParameter(WCDIAG_LEVEL);
   if (!wcscmp(WCDEEP_DIAG, usText))
   {
      retVal = DEEP_DIAG;
   }
   else if (!wcscmp(WCLONG_DIAG, usText))
   {
      retVal = LONG_DIAG;
   }
   else if (!wcscmp(WCSHORT_DIAG, usText))
   {
      retVal = SHORT_DIAG;
   }
   else if (!wcscmp(WCSTAT_DIAG, usText))
   {
      retVal = STAT_DIAG;
   }
   return retVal;    
}

long LgsComInterface::diagLineStart(XlationSession* pSession) const
{
   long retVal = 1;
   const wchar_t *usText = pSession->getInputParameter(WCDIAG_LINE_START);
   if (!usText && L'\0' != usText[0])
   {
      wchar_t *stopPos = 0;
      retVal = wcstol(usText, &stopPos, 10);
   }
   return retVal;
}

long LgsComInterface::diagLineEnd(XlationSession* pSession) const
{
   long retVal = 2147483647;
   const wchar_t *usText = pSession->getInputParameter(WCDIAG_LINE_END);
   if (!usText && L'\0' != usText[0])
   {
      wchar_t *stopPos = 0;
      retVal = wcstol(usText, &stopPos, 10);
   }
   return retVal;
}

_bstr_t LgsComInterface::ruleOverrideFile(XlationSession* pSession) const
{
   const wchar_t* wcText = pSession->getInputParameter(WCRULE_OVERRIDE_FILE_NAME);
   _bstr_t retText(wcText);
   return retText;
}

unsigned char LgsComInterface::searchDefaultFlag(XlationSession* pSession) const
{
   unsigned char retVal = FLAG_OFF;
   if (!wcscmp(WCYES, pSession->getInputParameter(WCSEARCH_DEFAULT_FLAG)))
   {
      retVal = FLAG_ON;
   }
   return retVal;    
}

unsigned short LgsComInterface::wordSearchOption(XlationSession* pSession) const
{
   const wchar_t *usText = pSession->getInputParameter(WCWORD_SEARCH_OPTIONS);
   UStringVector options;
   LgsTokenizer::Tokenize(usText, options, L"+");
   unsigned short retVal = 0;
   for (UStringIterator curr = options.begin(); curr != options.end(); curr++)
   {
      if ((*curr) == WCUNFOUND)
      {
         retVal |= UNFOUND;
      }
      else if ((*curr) == WCFOUND_NOUN)
      {
         retVal |= FOUND_NOUN;
      }
      else if ((*curr) == WCFOUND_VERB)
      {
         retVal |= FOUND_VERB;
      }
      else if ((*curr) == WCFOUND_ADJ)
      {
         retVal |= FOUND_ADJ;
      }
      else if ((*curr) == WCFOUND_ADV)
      {
         retVal |= FOUND_ADV;
      }
   }
   if (!retVal)
   {
      retVal = UNFOUND|FOUND_NOUN|FOUND_VERB|FOUND_ADJ|FOUND_ADV;
   }
   return retVal;
}

long LgsComInterface::wordSearchFoundStart(XlationSession* pSession) const
{
   long retVal = 1;
   const wchar_t *usText = pSession->getInputParameter(WCWORD_SEARCH_FOUND_START);
   //if (!usText && L'\0' != usText[0])  // Bugfix ANe 29.07.2003
   if ((usText != NULL) && (L'\0' != usText[0]))
   {
      wchar_t *stopPos = 0;
      retVal = wcstol(usText, &stopPos, 10);
   }
   return retVal;
}

long LgsComInterface::wordSearchFoundLimit(XlationSession* pSession) const
{
   long retVal = 1;
   const wchar_t *usText = pSession->getInputParameter(WCWORD_SEARCH_FOUND_LIMIT);
   //if (!usText && L'\0' != usText[0])  // Bugfix ANe 29.07.2003
   if ((usText != NULL) && (L'\0' != usText[0]))
   {
      wchar_t *stopPos = 0;
      retVal = wcstol(usText, &stopPos, 10);
   }
   return retVal;
}

long LgsComInterface::wordSearchUnfoundStart(XlationSession* pSession) const
{
   long retVal = 1;
   const wchar_t *usText = pSession->getInputParameter(WCWORD_SEARCH_UNFOUND_START);
   //if (!usText && L'\0' != usText[0])  // Bugfix ANe 29.07.2003
   if ((usText != NULL) && (L'\0' != usText[0]))
   {
      wchar_t *stopPos = 0;
      retVal = wcstol(usText, &stopPos, 10);
   }
   return retVal;
}

long LgsComInterface::wordSearchUnfoundLimit(XlationSession* pSession) const
{
   long retVal = 1;
   const wchar_t *usText = pSession->getInputParameter(WCWORD_SEARCH_UNFOUND_LIMIT);
   //if (!usText && L'\0' != usText[0])  // Bugfix ANe 29.07.2003
   if ((usText != NULL) && (L'\0' != usText[0]))
   {
      wchar_t *stopPos = 0;
      retVal = wcstol(usText, &stopPos, 10);
   }
   return retVal;
}

unsigned char LgsComInterface::traceFlag(XlationSession* pSession) const
{
   return (wcscmp(WCYES, pSession->getInputParameter(WCTRACE_FLAG)) == 0)? FLAG_ON: FLAG_OFF;
}

unsigned char LgsComInterface::logQueryFlag(XlationSession* pSession) const
{
   return (wcscmp(WCYES, pSession->getInputParameter(WCLOG_QUERY_FLAG)) == 0)? FLAG_ON: FLAG_OFF;
}

unsigned char LgsComInterface::saveScratchFlag(XlationSession* pSession) const
{
   return (wcscmp(WCYES, pSession->getInputParameter(WCSAVE_SCRATCH_FLAG)) == 0)? FLAG_ON: FLAG_OFF;
}

unsigned char LgsComInterface::generateLog(XlationSession* pSession) const
{
   return (wcscmp(WCYES, pSession->getInputParameter(WCGENERATE_LOG)) == 0)? FLAG_ON: FLAG_OFF;
}

_bstr_t LgsComInterface::mainRes1File(XlationSession* pSession) const
{
   const wchar_t *wcText = pSession->getInputParameter(WCMAIN_RES1_FILE);
   _bstr_t retText(wcText);
   return retText;
}

_bstr_t LgsComInterface::mainRes2File(XlationSession* pSession) const
{
   const wchar_t *wcText = pSession->getInputParameter(WCMAIN_RES2_FILE);
   _bstr_t retText(wcText);
   return retText;
}

_bstr_t LgsComInterface::mainRes22File(XlationSession* pSession) const
{
   const wchar_t *wcText = pSession->getInputParameter(WCMAIN_RES22_FILE);
   _bstr_t retText(wcText);
   return retText;
}

_bstr_t LgsComInterface::mainTran1File(XlationSession* pSession) const
{
   const wchar_t *wcText = pSession->getInputParameter(WCMAIN_TRAN1_FILE);
   _bstr_t retText(wcText);
   return retText;
}

_bstr_t LgsComInterface::mainTran2File(XlationSession* pSession) const
{
   const wchar_t *wcText = pSession->getInputParameter(WCMAIN_TRAN2_FILE);
   _bstr_t retText(wcText);
   return retText;
}

_bstr_t LgsComInterface::mainTran3File(XlationSession* pSession) const
{
   const wchar_t *wcText = pSession->getInputParameter(WCMAIN_TRAN3_FILE);
   _bstr_t retText(wcText);
   return retText;
}

_bstr_t LgsComInterface::mainTran4File(XlationSession* pSession) const
{
   const wchar_t *wcText = pSession->getInputParameter(WCMAIN_TRAN4_FILE);
   _bstr_t retText(wcText);
   return retText;
}

_bstr_t LgsComInterface::mainParse1File(XlationSession* pSession) const
{
   const wchar_t *wcText = pSession->getInputParameter(WCMAIN_PARSE1_FILE);
   _bstr_t retText(wcText);
   return retText;
}

_bstr_t LgsComInterface::mainParse2File(XlationSession* pSession) const
{
   const wchar_t *wcText = pSession->getInputParameter(WCMAIN_PARSE2_FILE);
   _bstr_t retText(wcText);
   return retText;
}

_bstr_t LgsComInterface::mainParse3File(XlationSession* pSession) const
{
   const wchar_t *wcText = pSession->getInputParameter(WCMAIN_PARSE3_FILE);
   _bstr_t retText(wcText);
   return retText;
}

_bstr_t LgsComInterface::mainParse4File(XlationSession* pSession) const
{
   const wchar_t *wcText = pSession->getInputParameter(WCMAIN_PARSE4_FILE);
   _bstr_t retText(wcText);
   return retText;
}

_bstr_t LgsComInterface::miniRes2File(XlationSession* pSession) const
{
   const wchar_t *wcText = pSession->getInputParameter(WCMINI_RES2_FILE);
   _bstr_t retText(wcText);
   return retText;
}

_bstr_t LgsComInterface::miniTran1File(XlationSession* pSession) const
{
   const wchar_t *wcText = pSession->getInputParameter(WCMINI_TRAN1_FILE);
   _bstr_t retText(wcText);
   return retText;
}

_bstr_t LgsComInterface::miniTran2File(XlationSession* pSession) const
{
   const wchar_t *wcText = pSession->getInputParameter(WCMINI_TRAN2_FILE);
   _bstr_t retText(wcText);
   return retText;
}

_bstr_t LgsComInterface::miniTran3File(XlationSession* pSession) const
{
   const wchar_t *wcText = pSession->getInputParameter(WCMINI_TRAN3_FILE);
   _bstr_t retText(wcText);
   return retText;
}

_bstr_t LgsComInterface::miniTran4File(XlationSession* pSession) const
{
   const wchar_t *wcText = pSession->getInputParameter(WCMINI_TRAN4_FILE);
   _bstr_t retText(wcText);
   return retText;
}

_bstr_t LgsComInterface::miniParse1File(XlationSession* pSession) const
{
   const wchar_t *wcText = pSession->getInputParameter(WCMINI_PARSE1_FILE);
   _bstr_t retText(wcText);
   return retText;
}

_bstr_t LgsComInterface::miniParse2File(XlationSession* pSession) const
{
   const wchar_t *wcText = pSession->getInputParameter(WCMINI_PARSE2_FILE);
   _bstr_t retText(wcText);
   return retText;
}

_bstr_t LgsComInterface::miniParse3File(XlationSession* pSession) const
{
   const wchar_t *wcText = pSession->getInputParameter(WCMINI_PARSE3_FILE);
   _bstr_t retText(wcText);
   return retText;
}

_bstr_t LgsComInterface::miniParse4File(XlationSession* pSession) const
{
   const wchar_t *wcText = pSession->getInputParameter(WCMINI_PARSE4_FILE);
   _bstr_t retText(wcText);
   return retText;
}

_bstr_t LgsComInterface::userID(XlationSession* pSession) const
{
   const wchar_t *wcText = pSession->getInputParameter(WCUSER_ID);
   if (!wcText || L'\0' == wcText[0])
   {
      wcText = L"API";
   }
   _bstr_t retText(wcText);
   return retText;
}
