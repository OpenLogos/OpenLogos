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
// MSWindowsINIFile.cpp: implementation of the CMSWindowsINIFile class.
//
//////////////////////////////////////////////////////////////////////
#include <logos_include/logoscommon.h>
#include <configdatafileinterface/configdataerror.h>
#include <configdatafileinterface/configdatafile.h>
#include <configdatafileinterface/mswindowsinifile.h>

#ifndef _MSC_VER
#include <sys/stat.h>
#endif

#include "logos_include/writeprivateprofilestring.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CMSWindowsINIFile::CMSWindowsINIFile(LgsString strFName) 
{
    //open the file throw exection if fails.
    // this done in order to check the existance of the file
    m_pFName = 0;
    m_pFName = new char[strFName.length() + 1];
    if (m_pFName == NULL)
    {
        CConfigDataError* pError = new CConfigDataError();
        pError->nErrorCode = MEMORY_ERROR;
        pError->strErrMsg = "Memory allocation Error.";
        throw pError;
    }
    
    ::strcpy(m_pFName,strFName.c_str());
    //Check the file existence
#ifdef _MSC_VER
    WIN32_FIND_DATA FindFileData;
    HANDLE hFind;
    hFind = FindFirstFile(m_pFName, &FindFileData);
    bool err = (hFind == INVALID_HANDLE_VALUE);
#else
    // _fix_me_
    // this doesn't do the job if this function relies on the recursive search
    // of FindFirstFile
    struct stat res;
    bool err = (stat(m_pFName, &res) != 0 ) || ! S_ISREG(res.st_mode);
#endif

    if (err)
    {
        CConfigDataError* pError = new CConfigDataError();
        pError->nErrorCode = CONFIG_DATA_FILE_ERROR;
        pError->strErrMsg = "Config data file " + strFName + " not found.";
        throw pError;
    }
#ifdef _MSC_VER
    FindClose(hFind);   
#endif
}

CMSWindowsINIFile::~CMSWindowsINIFile()
{
    if (m_pFName)
    {
       delete[] m_pFName;
    }
}

/////////////////////////////////////////////////////////////////////////////////////////////
/// Implementatin of the pure  virtual function defined in th base class. See base class   //
//  for description                                                                        //
///////////////////////////////////////////////////////////////////////////////////////////// 

bool CMSWindowsINIFile::GetValue(LgsString& strSection, LgsString& strName, LgsString& strValue)
{
   char* pSection = new char[strSection.length() + 1];
   char* pName = new char[strName.length() + 1];
   char* pValue = new char[1024];
   char* pDefault = "not found";
   ::strcpy(pSection, strSection.c_str());
   ::strcpy(pName, strName.c_str());
   int nRet = ::GetPrivateProfileString(pSection, pName, pDefault, pValue, 1024, m_pFName);
   strValue = pValue;
   delete[] pSection;
   delete[] pName;
   delete[] pValue;

   if ((strValue == "not found") || (nRet == 0))
   {
      strValue = "";
      return false;
   }
   return true;
}

bool CMSWindowsINIFile::GetKeyValues(LgsString& strSection, LgsString& strValue)
{
   char* pSection = new char[strSection.length() + 1];
   char* pValue = new char[1024];
   char* pDefault = "not found";
   ::strcpy(pSection,strSection.c_str());
   int nRet = ::GetPrivateProfileString(pSection, NULL, pDefault, pValue, 1024, m_pFName);

   if (nRet == 0)
   {
      strValue = "";
      return false;
   }

   char* nextKey = pValue;
   strValue = "";
   while (nextKey[0] != '\0')
   {
      strValue += nextKey;
      strValue += " ";
      nextKey += strlen(nextKey) + 1;
   }
   delete[] pSection;
   delete[] pValue;
   return true;
}
