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
program. If not, write to Globalware AG, Hospitalstraße 6, D-99817 Eisenach.

Linux port modifications and additions by Bernd Kiefer, Walter Kasper,
Deutsches Forschungszentrum fuer kuenstliche Intelligenz (DFKI)
Stuhlsatzenhausweg 3, D-66123 Saarbruecken
*/
// lgscominterface.h: interface for the LgsComInterface class.
//
//////////////////////////////////////////////////////////////////////

#ifndef _LGSCOMINTERFACE_H_
#define _LGSCOMINTERFACE_H_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <engine_api/proxylayer/proxyinterface.h>
#include <fstream>
using namespace std;

#ifdef _MSC_VER
#include <comdef.h>
#include <engine_api/translationserver/translationserver.h>
#else
#include "logos_include/bstr_t.h"
#include "engine_api/translationserver/XlationEngine.h"
#define HRESULT int
#define S_OK 0
#define SUCCEEDED(__arg) (__arg == S_OK)
#endif

class LgsComInterface : public ProxyInterface  
{
public:
   LgsComInterface();
   virtual ~LgsComInterface();
   virtual bool translateText(XlationSession *pSession, unsigned long timeOut);
   virtual bool translateDoc(XlationSession *pSession, unsigned long timeOut);
   virtual bool termSearchText(XlationSession *pSession, unsigned long timeOut);
   virtual bool termSearchDoc(XlationSession *pSession, unsigned long timeOut);
   virtual bool qryConfiguration(XlationSession *pSession, unsigned long timeOut);

protected:
   // Utility functions to convert parameters to the correct COM format
   virtual _bstr_t inputText(XlationSession* pSession) const;
   virtual _bstr_t inputFile(XlationSession* pSession) const;
   virtual short sourceLanguage(XlationSession* pSession) const;
   virtual short targetLanguage(XlationSession* pSession) const;
   short languageCode(const wchar_t *usText) const;
   virtual short inputFormat(XlationSession* pSession) const;
   virtual unsigned short protectionChar(XlationSession* pSession) const;
   virtual _bstr_t sourceLocale(XlationSession* pSession) const;
   virtual _bstr_t targetLocale(XlationSession* pSession) const;
   virtual unsigned char pmRulesFlag(XlationSession* pSession) const;
   virtual unsigned char extendedSearch(XlationSession* pSession) const;
   virtual _bstr_t subjectMatterCodes(XlationSession* pSession) const;
   virtual _bstr_t companyCodes(XlationSession* pSession) const;
   virtual unsigned char flagUnfoundWords(XlationSession* pSession) const;
   virtual unsigned char generateAlignment(XlationSession* pSession) const;
   virtual unsigned char targetImperativeForm(XlationSession* pSession) const;
   virtual unsigned char dbModeFlag(XlationSession* pSession) const;
   virtual unsigned char debugFlag(XlationSession* pSession) const;
   virtual unsigned char statisticsFlag(XlationSession* pSession) const;
   virtual unsigned char diagLevel(XlationSession* pSession) const;
   virtual long diagLineStart(XlationSession* pSession) const;
   virtual long diagLineEnd(XlationSession* pSession) const;
   virtual _bstr_t ruleOverrideFile(XlationSession* pSession) const;
   virtual unsigned short wordSearchOption(XlationSession* pSession) const;
   virtual long wordSearchFoundStart(XlationSession* pSession) const;
   virtual long wordSearchFoundLimit(XlationSession* pSession) const;
   virtual long wordSearchUnfoundStart(XlationSession* pSession) const;
   virtual long wordSearchUnfoundLimit(XlationSession* pSession) const;
   virtual unsigned char searchDefaultFlag(XlationSession* pSession) const;
   virtual unsigned char traceFlag(XlationSession* pSession) const;
   virtual unsigned char logQueryFlag(XlationSession* pSession) const;
   virtual unsigned char saveScratchFlag(XlationSession* pSession) const;
   virtual unsigned char generateLog(XlationSession* pSession) const;
   virtual _bstr_t mainRes1File(XlationSession* pSession) const;
   virtual _bstr_t mainRes2File(XlationSession* pSession) const;
   virtual _bstr_t mainRes22File(XlationSession* pSession) const;
   virtual _bstr_t mainTran1File(XlationSession* pSession) const;
   virtual _bstr_t mainTran2File(XlationSession* pSession) const;
   virtual _bstr_t mainTran3File(XlationSession* pSession) const;
   virtual _bstr_t mainTran4File(XlationSession* pSession) const;
   virtual _bstr_t mainParse1File(XlationSession* pSession) const;
   virtual _bstr_t mainParse2File(XlationSession* pSession) const;
   virtual _bstr_t mainParse3File(XlationSession* pSession) const;
   virtual _bstr_t mainParse4File(XlationSession* pSession) const;
   virtual _bstr_t miniRes2File(XlationSession* pSession) const;
   virtual _bstr_t miniTran1File(XlationSession* pSession) const;
   virtual _bstr_t miniTran2File(XlationSession* pSession) const;
   virtual _bstr_t miniTran3File(XlationSession* pSession) const;
   virtual _bstr_t miniTran4File(XlationSession* pSession) const;
   virtual _bstr_t miniParse1File(XlationSession* pSession) const;
   virtual _bstr_t miniParse2File(XlationSession* pSession) const;
   virtual _bstr_t miniParse3File(XlationSession* pSession) const;
   virtual _bstr_t miniParse4File(XlationSession* pSession) const;
   virtual _bstr_t userID(XlationSession* pSession) const;

private:
   static bool m_bInitialized;
   static ofstream m_LogStream;
#ifdef _MSC_VER
   IXlationEngine *m_pXlationEngine;
#else
   CXlationEngine *m_pXlationEngine;
#endif
};

#endif // _LGSCOMINTERFACE_H_
