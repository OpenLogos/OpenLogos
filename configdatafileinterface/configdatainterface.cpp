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
// ConfigDataInterface.cpp: implementation of the CConfigDataInterface class.
//
//////////////////////////////////////////////////////////////////////
#include <logos_include/logoscommon.h>
#include <configdatafileinterface/configdataerror.h>
#include <configdatafileinterface/configdatafile.h>
#include <configdatafileinterface/mswindowsinifile.h>
#include <configdatafileinterface/resdata.h>
#include <configdatafileinterface/configdatainterface.h>

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CConfigDataInterface::CConfigDataInterface(bool isTwoPass, int jobID, const LgsString& iniFileName,
                                           const LgsString& srcLang, const LgsString& trgLang,
                                           const LgsString& res1Main, const LgsString& res2Main,
                                           const LgsString& res22Main, const LgsString& tran1Main,
                                           const LgsString& tran2Main, const LgsString& tran3Main,
                                           const LgsString& tran4Main, const LgsString& parse1Main,
                                           const LgsString& parse2Main, const LgsString& parse3Main,
                                           const LgsString& parse4Main, const LgsString& res2Mini,
                                           const LgsString& tran1Mini, const LgsString& tran2Mini,
                                           const LgsString& tran3Mini, const LgsString& tran4Mini,
                                           const LgsString& parse1Mini, const LgsString& parse2Mini,
                                           const LgsString& parse3Mini, const LgsString& parse4Mini,
                                           const char* scratchDir)
{
   m_pDataFile = NULL;
   m_pResData = NULL;
   m_pSourceData = NULL;
//   m_pTargetData = NULL;
   m_pEngineData = NULL;
   m_pTranData = NULL;

   try
   {
      m_pDataFile = InitConfigDataFile(iniFileName);

      // construct the res, tran and other sections.
      m_pSourceData = new CSourceData(srcLang, trgLang, isTwoPass, m_pDataFile);
//      m_pTargetData = new CSourceData(trgLang, m_pDataFile);
      m_pResData = new CResData(srcLang, trgLang, isTwoPass, m_pDataFile, res1Main, res2Main,
                                res22Main, res2Mini);
      m_pEngineData = new CEngineData(jobID, m_pDataFile, scratchDir);
      m_pTranData = new CTranData(srcLang, trgLang, isTwoPass, m_pDataFile, tran1Main, tran2Main,
                                  tran3Main, tran4Main, parse1Main, parse2Main, parse3Main, parse4Main,
                                  tran1Mini, tran2Mini, tran3Mini, tran4Mini, parse1Mini, parse2Mini,
                                  parse3Mini, parse4Mini);
   }
   catch(CConfigDataError * pError)
   {
      // clean up the objects created and throw again
      // Destructer cleans of the memory with valid  objects pointers.
      this->~CConfigDataInterface();
      CConfigDataError* pNewError = new CConfigDataError();
      pNewError->nErrorCode =  pError->nErrorCode;
      pNewError->strErrMsg = pError->strErrMsg;
      delete pError;
      throw pNewError;
   }

//   if (!m_pDataFile || !m_pResData || !m_pSourceData || !m_pTargetData || !m_pEngineData || !m_pTranData)
   if (!m_pDataFile || !m_pResData || !m_pSourceData || !m_pEngineData || !m_pTranData)
   {
      CConfigDataError* pError = new CConfigDataError();
      pError->nErrorCode = MEMORY_ERROR;
      pError->strErrMsg = "Memory allocation Error.";
      throw pError;
   }
}

CConfigDataInterface::~CConfigDataInterface()
{
   if (m_pDataFile) 
      delete  m_pDataFile;
   if (m_pResData)  
      delete m_pResData;
   if (m_pSourceData)  
      delete m_pSourceData;
//   if (m_pTargetData)  
//      delete m_pTargetData;
   if (m_pEngineData)  
      delete m_pEngineData;
   if (m_pTranData)
      delete m_pTranData;
}

int CConfigDataInterface::GetConfigData(LgsString& sectionKey, LgsString& nameKey, LgsString& DataBuf)
{
   if (sectionKey == "resdata")
      return m_pResData->GetData(nameKey, DataBuf);
   else if (sectionKey == "sourcedata")
      return m_pSourceData->GetData(nameKey, DataBuf);
//   else if (sectionKey == "targetdata")
//      return m_pTargetData->GetData(nameKey, DataBuf); 
   else if (sectionKey == "trandata")
      return m_pTranData->GetData(nameKey, DataBuf); 
   else
      return m_pEngineData->GetData(sectionKey, nameKey, DataBuf); 
}

///////////////////////////////////////////////////////////////////////////////////////////
// Function InitDataFile(LgsString FName)                                                //
//                                                                                       //
//   This fuction returns a interface pointer to the  file containing data. This creats  //
//   a object wrapping the datafile. This file can be modified to create the required    //
//   derived type.                                                                       //
///////////////////////////////////////////////////////////////////////////////////////////
CConfigDataFile * CConfigDataInterface::InitConfigDataFile(LgsString iniFileName)
{
   CConfigDataFile* pDataFile = NULL;
   pDataFile = new CMSWindowsINIFile(iniFileName);
   if (pDataFile == NULL)
   {
      CConfigDataError* pError = new CConfigDataError();
      pError->nErrorCode = MEMORY_ERROR;
      pError->strErrMsg = "Memory allocation Error.";
      throw pError;
   }
   return pDataFile;
}
