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
// xlationsessionmanager.cpp: implementation of the XlationSessionManager class.
//
//////////////////////////////////////////////////////////////////////

#include <engine_api/xlationinterface/xlationsessionmanager.h>
#include <engine_api/xlationinterface/xlationsession.h>
#include <engine_api/proxylayer/proxyinterface.h>

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

XlationSessionManager *XlationSessionManager::m_sessionManager = 0;

XlationSessionManager::XlationSessionManager() : m_maxSessionCount(1)
{
  //InitializeCriticalSection(&m_synchronize);
  m_synchronize.initialize();
}

XlationSessionManager::~XlationSessionManager()
{
    SessionIterator curr = m_idleSessions.begin(); 
    while (curr != m_idleSessions.end())
    {
        XlationSession * temp = (*curr);
        curr = m_idleSessions.erase(curr);
        delete temp;
    }
    curr = m_startedSessions.begin(); 
    while (curr != m_startedSessions.end())
    {
        XlationSession * temp = (*curr);
        curr = m_startedSessions.erase(curr);
        delete temp;
    } 
    //DeleteCriticalSection(&m_synchronize);
}

__declspec(dllexport) XlationSessionManager& XlationSessionManager::singleton(void)
{
    if (!m_sessionManager)
    {
        m_sessionManager = new XlationSessionManager;
    }
    return *m_sessionManager;
}

__declspec(dllexport) void XlationSessionManager::destroySessionManager(void)
{
    if (m_sessionManager)
    {
        delete m_sessionManager;
        m_sessionManager = 0;
    }
    // ProxyInterface::destroyProxyInterface();
}

XlationSession* XlationSessionManager::createXlationSession(const wchar_t *sessionType)
{
    m_errorCode = SUCCESS;
    XlationSession *pSession = new XlationSession(sessionType);
    //EnterCriticalSection(&m_synchronize);
    m_synchronize.enter();
    m_idleSessions.push_back(pSession);
    m_synchronize.leave();
    //LeaveCriticalSection(&m_synchronize);
    return pSession;
}

SessionIterator XlationSessionManager::findSession(SessionVector &sessionList, XlationSession *pSession)
{
    SessionIterator curr = sessionList.begin();
    for (; curr != sessionList.end() && (*curr) != pSession; curr++);
    return curr;
}

bool XlationSessionManager::freeXlationSession(XlationSession* pSession)
{
    m_errorCode = SUCCESS;
    SessionIterator foundIterator;
    //EnterCriticalSection(&m_synchronize);
    m_synchronize.enter();
    if ((foundIterator = findSession(m_idleSessions, pSession)) != m_idleSessions.end())
    {
        m_idleSessions.erase(foundIterator);
    }
    else if ((foundIterator = findSession(m_startedSessions, pSession)) != m_startedSessions.end())
    {
        m_startedSessions.erase(foundIterator);
    }
    else
    {
        m_errorCode = INVALID_SESSION;
        m_synchronize.leave();
        //LeaveCriticalSection(&m_synchronize);
        return false;
    }
    m_synchronize.leave();
    //LeaveCriticalSection(&m_synchronize);
    delete pSession;
    return true;
}

bool XlationSessionManager::startXlationSession(XlationSession* pSession, unsigned long timeOut)
{
    m_errorCode = SUCCESS;
    //EnterCriticalSection(&m_synchronize);
    m_synchronize.enter();
    if (m_startedSessions.size() >= m_maxSessionCount)
    {
        m_synchronize.leave();
        //LeaveCriticalSection(&m_synchronize);
        m_errorCode = SESSION_COUNT_EXCEEDED;
        return false;
    }
    SessionIterator foundIterator;
    if ((foundIterator = findSession(m_startedSessions, pSession)) != m_startedSessions.end())
    {
        m_synchronize.leave();
        //LeaveCriticalSection(&m_synchronize);
        m_errorCode = SESSION_ALREADY_STARTED;
        return false;
    }
    if ((foundIterator = findSession(m_idleSessions, pSession)) == m_idleSessions.end())    
    {
        m_synchronize.leave();
        //LeaveCriticalSection(&m_synchronize);
        m_errorCode = INVALID_SESSION;
        return false;
    }
    m_idleSessions.erase(foundIterator);
    m_startedSessions.push_back(pSession);
    m_synchronize.leave();
    //LeaveCriticalSection(&m_synchronize);

    if (!pSession->start(timeOut))
    {
        m_errorCode = pSession->getErrorCode();
        //EnterCriticalSection(&m_synchronize);
        m_synchronize.enter();
        foundIterator = findSession(m_startedSessions, pSession);
        m_startedSessions.erase(foundIterator);
        m_idleSessions.push_back(pSession);
        m_synchronize.leave();
        //LeaveCriticalSection(&m_synchronize);
        return false;
    }
    //EnterCriticalSection(&m_synchronize);
    m_synchronize.enter();
    foundIterator = findSession(m_startedSessions, pSession);
    m_startedSessions.erase(foundIterator);
    m_idleSessions.push_back(pSession);
    m_synchronize.leave();
    //LeaveCriticalSection(&m_synchronize);

    return true;
}
 
unsigned long XlationSessionManager::getErrorCode(void)
{
    return m_errorCode;
}

void XlationSessionManager::invokeCallback(XlationSession *pSession)
{
    if (pSession->asynchronousMode())
    {
        (*pSession->m_callBack)(pSession);
    }
}
