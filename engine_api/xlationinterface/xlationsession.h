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
// xlationsession.h: interface for the XlationSession class.
// Encapsulates the details of a particular request. This class interacts with the 
// Proxy Classes layer to send the request to the Translation Server. This contains all 
// the properties that are needed to carry out the operation.
//
// Copyright (C) 2000 Logos Corporation
//////////////////////////////////////////////////////////////////////

#ifndef _XLATIONSESSION_H_
#define _XLATIONSESSION_H_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <engine_api/xlationinterface/xlationinterfacedefs.h>
#include <engine_api/xlationinterface/propertylist.h>

class ProxyInterface;
class XlationSession;
typedef void (*CALL_BACK_FP)(XlationSession*);

class XlationSession  
{
   friend class XlationSessionManager;
   friend class ProxyInterface;

public:
   // Sets the operation associated with this Session object.
   virtual void setOperationType(OperationType eOperation);
   // Calls setPropertyValue method of the PropertyList representing the input 
   // Parameter List with the name and value passed as parameters. The method
   // is used to set the parameters needed for the operation.
   virtual void setInputParameter(const wchar_t *name, const wchar_t *value);
   // Clears all elements in the input Parameter list. Input parameter list will 
   // be empty after this operation.
   virtual void clearInputParameters(void);
   // Removes the input parameter from the input parameter list.
   virtual void removeInputParameter(const wchar_t *name);
   // Registers a callback function with this object. When the operation associated 
   // with this object completes, control is transferred to this Callback function 
   // registered.
   virtual void registerCallback(CALL_BACK_FP fp);
   // Returns the value corresponding to the name passed as a parameter. The output 
   // values sent by the Translation Server are stored in a PropertyList.
   virtual const wchar_t* getOutputValue(const wchar_t *name) const;
   // Get the operation associated with this session
   virtual OperationType getOperationType(void) const;
   // Returns the value corresponding to the name passed as a parameter.
   virtual const wchar_t* getInputParameter(const wchar_t *name) const;
   virtual unsigned long getErrorCode(void);

protected:
   // Constructor
   XlationSession(const wchar_t *sessionType);
   // Destructor
   ~XlationSession();
   // Starts the operation associated with this Session. This sends the request down 
   // to the Translation Server through the Proxy Classes layer. All properties required 
   // for the operation should have been populated prior to calling this method. If a 
   // Callback function is registered, this method returns immediately else this method 
   // blocks until the return values are received. The time for which the call blocks is 
   // determined by the  timeOut parameter specified in milliseconds. If 0 is specified 
   // for the timeOut parameter, then the call blocks for ever.
   virtual bool start(unsigned long timeOut);
   // Clears all elements in the output Parameter list. Output parameter list will be empty 
   // after this operation.
   virtual void clearOutputValues(void);
   // Calls setPropertyValue of the PropertyList object with the name and value passed as 
   // parameters (PropertyList object mentioned here is the object which holds returned 
   // values from the Translation Server).
   virtual void setOutputValue(const UString& name, const UString& value);
   // Checks if call back function is set up (asynchronous mode)
   virtual bool asynchronousMode(void) const;

private:
   UString m_sessionType; // Type of session
   PropertyList m_inParameters; // List of Input Parameters
   PropertyList m_outParameters; // List of Output Parameters
   OperationType m_eOperation; // Operation requested
   CALL_BACK_FP m_callBack; // Pointer to a call back function
   ProxyInterface *m_proxyInterface; // Pointer to Proxy Interface which is responsible 
                                     // sending the request down to the Translation Server
                                     // using an appropriate Communication scheme
   unsigned long m_errorCode; // Error Code associated with the last operation performed
};

#endif // _XLATIONSESSION_H_
