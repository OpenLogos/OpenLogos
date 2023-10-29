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
// xlationsessionmanager.h: interface for the XlationSessionManager class.
//
// XlationSessionManager is responsible for the creation of XlationSession. The policy of 
// how to invoke the XlationSession is encapsulated in this class. This class also limits 
// the number of XlationSession objects that can be active at any time.
//
// Copyright (C) 2000 Logos Corporation
//////////////////////////////////////////////////////////////////////

#ifndef _XLATIONSESSIONMANAGER_H_
#define _XLATIONSESSIONMANAGER_H_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <vector>
#include <engine_api/xlationinterface/xlationinterfacedefs.h>
#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif
#include "logos_libs/multithreadlib/lgscritsect.h"

using namespace std;

class XlationSession;
class ProxyInterface;

typedef vector<XlationSession*> SessionVector;
typedef SessionVector::iterator SessionIterator;
typedef SessionVector::const_iterator ConstSessionIterator;

class XlationSessionManager  
{
   friend class ProxyInterface;

public:
   // This returns the only instance of XlationSessionManager. An instance of 
   // XlationSessionManager can be obtained only by calling this method.
   __declspec(dllexport) static XlationSessionManager& singleton(void);
   // Method to destroy the XlationSessionManager
   __declspec(dllexport) static void destroySessionManager(void);
   // Creates a XlationSession object if allowed. The new XlationSession object 
   // created is stored in the idle list of XlationSession(s)
   virtual XlationSession* createXlationSession(const wchar_t *sessionType = L"");
   // Frees a XlationSession object and releases its associated resources. 
   virtual bool freeXlationSession(XlationSession* pSession);
   // Initiates the operation associated with the Session object passed as a parameter. 
   // This method sends the request to the Translation Server by calling the start()
   // method of the XlationSession. The started XlatedSession object is moved from the 
   // idle list to the started list of XlationSession objects.
   virtual bool startXlationSession(XlationSession* pSession, unsigned long timeOut);
   // Returns the error code corresponding to the last operation performed on this object.
   virtual unsigned long getErrorCode(void);
   
private:
   // This is the constructor and is declared private. This class is created only in 
   // the singleton method of this class.
   XlationSessionManager();
   // Destructor 
   virtual ~XlationSessionManager();
   // Calls the Callback function of the XlationSession object passed as a parameter. 
   // This method is called from the Proxy Layer.
   virtual void invokeCallback(XlationSession *pSession);
   // This is a utility function to find the iterator that corresponds to the XlationSession
   // pointer passed as a parameter
   virtual SessionIterator findSession(SessionVector &sessionList, XlationSession *pSession);
   
   // This is a pointer to the Session Manager which is initialized by the singleton()
   // method
   static XlationSessionManager *m_sessionManager;
   unsigned short m_maxSessionCount; // Maximum number of sessions allowed
   SessionVector m_idleSessions; // Vector of sessions that are idle and not started
   SessionVector m_startedSessions; // Vector of sessions that have been started 
   unsigned long m_errorCode; // Error Code associated with the last operation performed
   //CRITICAL_SECTION m_synchronize; // Critical section to synchronize between threads
   LgsCriticalSection m_synchronize; // Critical section to synchronize between threads
};

#endif // _XLATIONSESSIONMANAGER_H_
