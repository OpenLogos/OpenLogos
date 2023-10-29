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
// XlationEngine.h : Declaration of the CXlationEngine

#ifndef __XLATIONENGINE_H_
#define __XLATIONENGINE_H_

#include "resource.h"       // main symbols
#include "xlationconsts.h"
#include <logos_libs/odbcsql/globalsqlconnection.h>

#ifndef _MSC_VER
#define STDMETHOD(__CALL) int __CALL
#define STDMETHODIMP int
#define S_OK 0
#define ATL_NO_VTABLE

#include "logos_include/bstr_t.h"
#endif

/////////////////////////////////////////////////////////////////////////////
// CXlationEngine
class ATL_NO_VTABLE CXlationEngine 
#ifdef _MSC_VER
   : 
   public CComObjectRootEx<CComMultiThreadModel>,
   public CComCoClass<CXlationEngine, &CLSID_XlationEngine>,
   public IConnectionPointContainerImpl<CXlationEngine>,
   public IDispatchImpl<IXlationEngine, &IID_IXlationEngine, &LIBID_TRANSLATIONSERVERLib>
#endif
{
public:
   CXlationEngine()
   {
   }

#ifdef _MSC_VER
DECLARE_REGISTRY_RESOURCEID(IDR_XLATIONENGINE)

DECLARE_PROTECT_FINAL_CONSTRUCT()

BEGIN_COM_MAP(CXlationEngine)
   COM_INTERFACE_ENTRY(IXlationEngine)
   COM_INTERFACE_ENTRY(IDispatch)
   COM_INTERFACE_ENTRY(IConnectionPointContainer)
END_COM_MAP()
BEGIN_CONNECTION_POINT_MAP(CXlationEngine)
END_CONNECTION_POINT_MAP()
#endif

// IXlationEngine
public:
   STDMETHOD(cleanup)();

   STDMETHOD(qryConfiguration)(
      /*[in]*/ const BSTR usSmcTreeName,
      /*[out]*/ BSTR *uspLanguagePairs,
      /*[out]*/ BSTR *uspCompanyCodes,
      /*[out]*/ BSTR *uspSmcCodes);

   STDMETHOD(termSearchDoc)(
      /*[in]*/ const BSTR usInputFile,
      /*[in]*/ short nSourceLanguage,
      /*[in]*/ short nTargetLanguage,
      /*[in]*/ short nInputFormat,
      /*[in]*/ unsigned short unProtectChar,
      /*[in]*/ const BSTR usSourceLocale,
      /*[in]*/ const BSTR usTargetLocale,
      /*[in]*/ unsigned char ucPmRulesFlag,
      /*[in]*/ unsigned char ucExtendedSearchFlag,
      /*[in]*/ const BSTR usSmcCodes,
      /*[in]*/ const BSTR usCompanyCodes,
      /*[in]*/ unsigned char ucFlagUnfoundWords,
      /*[in]*/ unsigned char ucLogosDefaultFlag,
      /*[in]*/ unsigned short unWordSearchOptions,
      /*[in]*/ long lWordSearchFoundStart,
      /*[in]*/ long lWordSearchFoundLimit,
      /*[in]*/ long lWordSearchUnfoundStart,
      /*[in]*/ long lWordSearchUnfoundLimit,
      /*[in]*/ unsigned char ucGenerateAlignment,
      /*[in]*/ unsigned char ucGenerateLog,
      /*[in]*/ unsigned char ucTargetImperativeForm,
      /*[in]*/ unsigned char ucDbModeFlag,
      /*[in]*/ unsigned char ucDebugFlag,
      /*[in]*/ unsigned char ucStatisticsFlag,
      /*[in]*/ unsigned char ucTraceFlag,
      /*[in]*/ unsigned char ucLogQueryFlag,
      /*[in]*/ unsigned char ucSaveScratchFlag,
      /*[in]*/ unsigned char ucDiagLevel,
      /*[in]*/ long lDiagStartLine,
      /*[in]*/ long lDiagEndLine,
      /*[in]*/ const BSTR usRuleOverrideFile,
		/*[in]*/ const BSTR usMainRes1,
		/*[in]*/ const BSTR usMainRes2,
		/*[in]*/ const BSTR usMainRes22,
		/*[in]*/ const BSTR usMainTran1,
		/*[in]*/ const BSTR usMainTran2,
		/*[in]*/ const BSTR usMainTran3,
		/*[in]*/ const BSTR usMainTran4,
		/*[in]*/ const BSTR usMainParse1,
		/*[in]*/ const BSTR usMainParse2,
		/*[in]*/ const BSTR usMainParse3,
		/*[in]*/ const BSTR usMainParse4,
		/*[in]*/ const BSTR usMiniRes2,
		/*[in]*/ const BSTR usMiniTran1,
		/*[in]*/ const BSTR usMiniTran2,
		/*[in]*/ const BSTR usMiniTran3,
		/*[in]*/ const BSTR usMiniTran4,
		/*[in]*/ const BSTR usMiniParse1,
		/*[in]*/ const BSTR usMiniParse2,
		/*[in]*/ const BSTR usMiniParse3,
		/*[in]*/ const BSTR usMiniParse4,
		/*[in]*/ const BSTR usUserId,
      /*[out]*/ BSTR *uspTermSearchFile,
      /*[out]*/ BSTR *uspAlignedFile,
      /*[out]*/ BSTR *uspDiagFile,
      /*[out]*/ long *lpSentenceCount,
      /*[out]*/ long *lpWordCount,
      /*[out]*/ long *lpSessionId,
      /*[out]*/ BSTR *uspLogFile,
      /*[out]*/ BSTR *uspErrorDesc);

   STDMETHOD(termSearchText)(
      /*[in]*/ const BSTR usInputText,
      /*[in]*/ short nSourceLanguage,
      /*[in]*/ short nTargetLanguage,
      /*[in]*/ short nInputFormat,
      /*[in]*/ unsigned short unProtectChar,
      /*[in]*/ const BSTR usSourceLocale,
      /*[in]*/ const BSTR usTargetLocale,
      /*[in]*/ unsigned char ucPmRulesFlag,
      /*[in]*/ unsigned char ucExtendedSearchFlag,
      /*[in]*/ const BSTR usSmcCodes,
      /*[in]*/ const BSTR usCompanyCodes,
      /*[in]*/ unsigned char ucFlagUnfoundWords,
      /*[in]*/ unsigned char ucLogosDefaultFlag,
      /*[in]*/ unsigned short unWordSearchOptions,
      /*[in]*/ long lWordSearchFoundStart,
      /*[in]*/ long lWordSearchFoundLimit,
      /*[in]*/ long lWordSearchUnfoundStart,
      /*[in]*/ long lWordSearchUnfoundLimit,
      /*[in]*/ unsigned char ucGenerateAlignment,
      /*[in]*/ unsigned char ucGenerateLog,
      /*[in]*/ unsigned char ucTargetImperativeForm,
      /*[in]*/ unsigned char ucDbModeFlag,
      /*[in]*/ unsigned char ucDebugFlag,
      /*[in]*/ unsigned char ucStatisticsFlag,
      /*[in]*/ unsigned char ucTraceFlag,
      /*[in]*/ unsigned char ucLogQueryFlag,
      /*[in]*/ unsigned char ucSaveScratchFlag,
      /*[in]*/ unsigned char ucDiagLevel,
      /*[in]*/ long lDiagStartLine,
      /*[in]*/ long lDiagEndLine,
      /*[in]*/ const BSTR usRuleOverrideFile,
		/*[in]*/ const BSTR usMainRes1,
		/*[in]*/ const BSTR usMainRes2,
		/*[in]*/ const BSTR usMainRes22,
		/*[in]*/ const BSTR usMainTran1,
		/*[in]*/ const BSTR usMainTran2,
		/*[in]*/ const BSTR usMainTran3,
		/*[in]*/ const BSTR usMainTran4,
		/*[in]*/ const BSTR usMainParse1,
		/*[in]*/ const BSTR usMainParse2,
		/*[in]*/ const BSTR usMainParse3,
		/*[in]*/ const BSTR usMainParse4,
		/*[in]*/ const BSTR usMiniRes2,
		/*[in]*/ const BSTR usMiniTran1,
		/*[in]*/ const BSTR usMiniTran2,
		/*[in]*/ const BSTR usMiniTran3,
		/*[in]*/ const BSTR usMiniTran4,
		/*[in]*/ const BSTR usMiniParse1,
		/*[in]*/ const BSTR usMiniParse2,
		/*[in]*/ const BSTR usMiniParse3,
		/*[in]*/ const BSTR usMiniParse4,
		/*[in]*/ const BSTR usUserId,
      /*[out]*/ BSTR *uspTermSearchText,
      /*[out]*/ BSTR *uspAlignedText,
      /*[out]*/ BSTR *uspDiagText,
      /*[out]*/ long *lpSentenceCount,
      /*[out]*/ long *lpWordCount,
      /*[out]*/ long *lpSessionId,
      /*[out]*/ BSTR *uspLogFile,
      /*[out]*/ BSTR *uspErrorDesc);

   STDMETHOD(translateDoc)(
      /*[in]*/ const BSTR usInputFile,
      /*[in]*/ short nSourceLanguage,
      /*[in]*/ short nTargetLanguage,
      /*[in]*/ short nInputFormat,
      /*[in]*/ unsigned short unProtectChar,
      /*[in]*/ const BSTR usSourceLocale,
      /*[in]*/ const BSTR usTargetLocale,
      /*[in]*/ unsigned char ucPmRulesFlag,
      /*[in]*/ unsigned char ucExtendedSearchFlag,
      /*[in]*/ const BSTR usSmcCodes,
      /*[in]*/ const BSTR usCompanyCodes,
      /*[in]*/ unsigned char ucFlagUnfoundWords,
      /*[in]*/ unsigned char ucGenerateAlignment,
      /*[in]*/ unsigned char ucGenerateLog,
      /*[in]*/ unsigned char ucTargetImperativeForm,
      /*[in]*/ unsigned char ucDbModeFlag,
      /*[in]*/ unsigned char ucDebugFlag,
      /*[in]*/ unsigned char ucStatisticsFlag,
      /*[in]*/ unsigned char ucTraceFlag,
      /*[in]*/ unsigned char ucLogQueryFlag,
      /*[in]*/ unsigned char ucSaveScratchFlag,
      /*[in]*/ unsigned char ucDiagLevel,
      /*[in]*/ long lDiagStartLine,
      /*[in]*/ long lDiagEndLine,
      /*[in]*/ const BSTR usRuleOverrideFile,
		/*[in]*/ const BSTR usMainRes1,
		/*[in]*/ const BSTR usMainRes2,
		/*[in]*/ const BSTR usMainRes22,
		/*[in]*/ const BSTR usMainTran1,
		/*[in]*/ const BSTR usMainTran2,
		/*[in]*/ const BSTR usMainTran3,
		/*[in]*/ const BSTR usMainTran4,
		/*[in]*/ const BSTR usMainParse1,
		/*[in]*/ const BSTR usMainParse2,
		/*[in]*/ const BSTR usMainParse3,
		/*[in]*/ const BSTR usMainParse4,
		/*[in]*/ const BSTR usMiniRes2,
		/*[in]*/ const BSTR usMiniTran1,
		/*[in]*/ const BSTR usMiniTran2,
		/*[in]*/ const BSTR usMiniTran3,
		/*[in]*/ const BSTR usMiniTran4,
		/*[in]*/ const BSTR usMiniParse1,
		/*[in]*/ const BSTR usMiniParse2,
		/*[in]*/ const BSTR usMiniParse3,
		/*[in]*/ const BSTR usMiniParse4,
		/*[in]*/ const BSTR usUserId,
      /*[out]*/ BSTR *uspXlatedTextFile,
      /*[out]*/ BSTR *uspAlignedFile,
      /*[out]*/ BSTR *uspDiagFile,
      /*[out]*/ long *lpSentenceCount,
      /*[out]*/ long *lpWordCount,
      /*[out]*/ long *lpSessionId,
      /*[out]*/ BSTR *uspLogFile,
      /*[out]*/ BSTR *uspErrorDesc);

   STDMETHOD(translateText)(
      /*[in]*/ const BSTR usInputText,
      /*[in]*/ short nSourceLanguage,
      /*[in]*/ short nTargetLanguage,
      /*[in]*/ short nInputFormat,
      /*[in]*/ unsigned short unProtectChar,
      /*[in]*/ const BSTR usSourceLocale,
      /*[in]*/ const BSTR usTargetLocale,
      /*[in]*/ unsigned char ucPmRulesFlag,
      /*[in]*/ unsigned char ucExtendedSearchFlag,
      /*[in]*/ const BSTR usSmcCodes,
      /*[in]*/ const BSTR usCompanyCodes,
      /*[in]*/ unsigned char ucFlagUnfoundWords,
      /*[in]*/ unsigned char ucGenerateAlignment,
      /*[in]*/ unsigned char ucGenerateLog,
      /*[in]*/ unsigned char ucTargetImperativeForm,
      /*[in]*/ unsigned char ucDbModeFlag,
      /*[in]*/ unsigned char ucDebugFlag,
      /*[in]*/ unsigned char ucStatisticsFlag,
      /*[in]*/ unsigned char ucTraceFlag,
      /*[in]*/ unsigned char ucLogQueryFlag,
      /*[in]*/ unsigned char ucSaveScratchFlag,
      /*[in]*/ unsigned char ucDiagLevel,
      /*[in]*/ long lDiagStartLine,
      /*[in]*/ long lDiagEndLine,
      /*[in]*/ const BSTR usRuleOverrideFile,
		/*[in]*/ const BSTR usMainRes1,
		/*[in]*/ const BSTR usMainRes2,
		/*[in]*/ const BSTR usMainRes22,
		/*[in]*/ const BSTR usMainTran1,
		/*[in]*/ const BSTR usMainTran2,
		/*[in]*/ const BSTR usMainTran3,
		/*[in]*/ const BSTR usMainTran4,
		/*[in]*/ const BSTR usMainParse1,
		/*[in]*/ const BSTR usMainParse2,
		/*[in]*/ const BSTR usMainParse3,
		/*[in]*/ const BSTR usMainParse4,
		/*[in]*/ const BSTR usMiniRes2,
		/*[in]*/ const BSTR usMiniTran1,
		/*[in]*/ const BSTR usMiniTran2,
		/*[in]*/ const BSTR usMiniTran3,
		/*[in]*/ const BSTR usMiniTran4,
		/*[in]*/ const BSTR usMiniParse1,
		/*[in]*/ const BSTR usMiniParse2,
		/*[in]*/ const BSTR usMiniParse3,
		/*[in]*/ const BSTR usMiniParse4,
		/*[in]*/ const BSTR usUserId,
      /*[out]*/ BSTR *uspXlatedText,
      /*[out]*/ BSTR *uspAlignedText,
      /*[out]*/ BSTR *uspDiagText,
      /*[out]*/ long *lpSentenceCount,
      /*[out]*/ long *lpWordCount,
      /*[out]*/ long *lpSessionId,
      /*[out]*/ BSTR *uspLogFile,
      /*[out]*/ BSTR *uspErrorDesc);

private:
   static SqlConnection *p_connection;
};

#endif //__XLATIONENGINE_H_
