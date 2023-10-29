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
// xlationsession.cpp: implementation of the XlationSession class.
//
//////////////////////////////////////////////////////////////////////

#include <engine_api/xlationinterface/xlationsession.h>
#include <engine_api/proxylayer/proxyinterface.h>

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////


XlationSession::XlationSession(const wchar_t *sessionType) 
                    : m_sessionType(sessionType), m_eOperation(TRANSLATE_TEXT),
                      m_callBack(0)
{
  m_proxyInterface = ProxyInterface::makeProxyInterface();
}

XlationSession::~XlationSession()
{
}

void XlationSession::setOperationType(OperationType eOperation)
{
    m_eOperation = eOperation;    
}

OperationType XlationSession::getOperationType(void) const
{
    return m_eOperation;    
}

void XlationSession::setInputParameter(const wchar_t *name, const wchar_t *value)
{
    m_inParameters.setPropertyValue(name, value);
}

void XlationSession::clearInputParameters(void)
{
    m_inParameters.clearAll();
}

void XlationSession::removeInputParameter(const wchar_t *name)
{
    m_inParameters.removeProperty(name);
}

const wchar_t* XlationSession::getInputParameter(const wchar_t *name) const
{
    return m_inParameters.getPropertyValue(name);
}

void XlationSession::registerCallback(CALL_BACK_FP fp)
{
    m_callBack = fp;
}

const wchar_t* XlationSession::getOutputValue(const wchar_t *name) const
{
    return m_outParameters.getPropertyValue(name);
}

unsigned long XlationSession::getErrorCode(void)
{
    return m_errorCode;
}

bool XlationSession::start(unsigned long timeOut)
{
    bool execStatus = true;
    switch (m_eOperation)
    {
    case TRANSLATE_TEXT:
        execStatus = m_proxyInterface->translateText(this, timeOut);
        break;
    case TRANSLATE_DOC:
        execStatus = m_proxyInterface->translateDoc(this, timeOut);
        break;
    case TERM_SEARCH_TEXT:
        execStatus = m_proxyInterface->termSearchText(this, timeOut);
        break;
    case TERM_SEARCH_DOC:
        execStatus = m_proxyInterface->termSearchDoc(this, timeOut);
        break;
    case QRY_CONFIGURATION:
        execStatus = m_proxyInterface->qryConfiguration(this, timeOut);
        break;
    default:
        m_errorCode = INVALID_OPERATION;
        return false;
    }
    if (!execStatus)
    {
        m_errorCode = m_proxyInterface->getErrorCode();
        return false;
    }
    m_errorCode = SUCCESS;
    return true;
}

void XlationSession::clearOutputValues(void)
{
    m_outParameters.clearAll();
}

void XlationSession::setOutputValue(const UString& name, const UString& value)
{
    m_outParameters.setPropertyValue(name, value);
}

bool XlationSession::asynchronousMode(void) const
{
    return (0 != m_callBack);
}
