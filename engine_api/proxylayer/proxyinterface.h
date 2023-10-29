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
// proxyinterface.h: interface for the ProxyInterface class.
//
//////////////////////////////////////////////////////////////////////

#ifndef _PROXYINTERFACE_H_
#define _PROXYINTERFACE_H_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <engine_api/xlationinterface/xlationinterfacedefs.h>

class XlationSession;

class ProxyInterface  
{
public:
   ProxyInterface(void);
   virtual ~ProxyInterface(void);
   static ProxyInterface* makeProxyInterface(void);
   static void destroyProxyInterface(void);
   virtual unsigned long getErrorCode(void) const;
   // All operations below have to be over-ridden in the Derived class
   virtual bool translateText(XlationSession *pSession, unsigned long timeOut) = 0;
   virtual bool translateDoc(XlationSession *pSession, unsigned long timeOut) = 0;
   virtual bool termSearchText(XlationSession *pSession, unsigned long timeOut) = 0;
   virtual bool termSearchDoc(XlationSession *pSession, unsigned long timeOut) = 0;
   virtual bool qryConfiguration(XlationSession *pSession, unsigned long timeOut) = 0;

protected:
   // Clears all elements in the output Parameter list of the XlationSession. Output parameter 
   // list will be empty after this operation.
   virtual void clearOutputValues(XlationSession *pSession);
   // Calls setOutputValue of the XlationSession object with the name and value passed as 
   // parameters.
   virtual void setOutputValue(XlationSession *pSession, const UString& name, const UString& value);
   // Checks if call back function is set up (asynchronous mode) in the XlationSession object
   virtual bool asynchronousMode(XlationSession *pSession) const;
   // Invoke the call back function registered with the XlationSession through the
   // XlationSessionManager
   virtual void invokeCallback(XlationSession *pSession);
   virtual void errorCode(unsigned long errCode);
   static ProxyInterface *m_proxyInterface;
   unsigned long m_errorCode;
};

#endif // _PROXYINTERFACE_H_
