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
#include <iostream>
#include <fstream>
#include <logos_include/logoscommon.h>
#include <configdatafileinterface/configdataerror.h>
#include <configdatafileinterface/configdatainterface.h>
#include <logos_libs/utility/stringutil.h>
extern "C"
{
  #include <configdatafileinterface/configdatainterfacemain.h>
}

static int C_GetConfigData(const char *lpszSecKey, const char *lpszNameKey, char *lpszBuf, int *pnBufSize);
static int C_InitConfigDataInterface(int numPass, int jobID, const char* srcLang, const char* trgLang,
                                     const char* res1Main, const char* res2Main, const char* res22Main,
                                     const char* tran1Main, const char* tran2Main, const char* tran3Main,
                                     const char* tran4Main, const char* parse1Main, const char* parse2Main,
                                     const char* parse3Main, const char* parse4Main, const char* res2Mini,
                                     const char* tran1Mini, const char* tran2Mini, const char* tran3Mini,
                                     const char* tran4Mini, const char* parse1Mini, const char* parse2Mini,
                                     const char* parse3Mini, const char* parse4Mini, const char* scratchDir);
static CConfigDataInterface *pConfigInterface = NULL;
static LgsString LgsRoot = "";
extern "C"
{
	// c interface (Wrapper function.)
   int GetConfigData(const char *lpszSecKey, const char *lpszNameKey, char *lpszBuf, int nBufSize)
   {
      int bufsize = nBufSize;
      int nRetCode = C_GetConfigData(lpszSecKey, lpszNameKey, lpszBuf, &bufsize);
      if (nRetCode != CONFDATA_SUCCESS)
      {
         cout << "ConfigDataError: " << lpszSecKey  << " : " << lpszNameKey << " : ";           
         switch (nRetCode)
         {
         case CONFDATA_INIT_INTERFACE_ERROR :
            cout  << "Interface not initialized: Unable to get  File name.\n";                 
            break;
         case CONFDATA_RETRIVE_ERROR :
            cout  << " Retrive error: Unable to get  File name.\n";
            break;
         case CONFDATA_BUFFER_ERROR :
            cout  << " Buffersize small: Unable to get  File.\n";
            break;
         default:
            cout << " Unknown error : Unable to get  File name.\n";
            break;
         }
      } 
      return nRetCode;
   }

   int GetConfigValue(const char *secKey, const char *nameKey, LgsString& value)
   {
      char lpszBuf[MAX_FILEPATH_LEN];
      value = "";
      int result;
      if ((result = GetConfigData(secKey, nameKey, lpszBuf, MAX_FILEPATH_LEN)) == CONFDATA_SUCCESS)
      {
         value = lpszBuf;
      }
      return result;
   }

   int InitConfigDataInterface(int numPass, int jobID, const char* srcLang, const char* trgLang,
                               const char* res1Main, const char* res2Main, const char* res22Main,
                               const char* tran1Main, const char* tran2Main, const char* tran3Main,
                               const char* tran4Main, const char* parse1Main, const char* parse2Main,
                               const char* parse3Main, const char* parse4Main, const char* res2Mini,
                               const char* tran1Mini, const char* tran2Mini, const char* tran3Mini,
                               const char* tran4Mini, const char* parse1Mini, const char* parse2Mini,
                               const char* parse3Mini, const char* parse4Mini, const char* scratchDir)
   {
      return C_InitConfigDataInterface(numPass, jobID, srcLang, trgLang, res1Main, res2Main, res22Main,
                                       tran1Main, tran2Main, tran3Main, tran4Main, parse1Main, parse2Main,
                                       parse3Main, parse4Main, res2Mini, tran1Mini, tran2Mini, tran3Mini,
                                       tran4Mini, parse1Mini, parse2Mini, parse3Mini, parse4Mini, scratchDir);
   }
}

int C_InitConfigDataInterface(int numPass, int jobID, const char* srcLang, const char* trgLang,
                              const char* res1Main, const char* res2Main, const char* res22Main,
                              const char* tran1Main, const char* tran2Main, const char* tran3Main,
                              const char* tran4Main, const char* parse1Main, const char* parse2Main,
                              const char* parse3Main, const char* parse4Main, const char* res2Mini,
                              const char* tran1Mini, const char* tran2Mini, const char* tran3Mini,
                              const char* tran4Mini, const char* parse1Mini, const char* parse2Mini,
                              const char* parse3Mini, const char* parse4Mini, const char* scratchDir)
{
   LgsString iniFileName;

   //get the root
   LgsRoot = ::getenv("LGS_ROOT");
   iniFileName = LgsRoot + DIR_SEP_STR + "bin"+ DIR_SEP_STR
     + "filenames.ini";
   try
   {
      if (pConfigInterface)
      {
         delete pConfigInterface;
      }
      pConfigInterface = NULL;

      bool isTwoPass = (numPass == 2);

      pConfigInterface = new CConfigDataInterface(isTwoPass, jobID, iniFileName, srcLang, trgLang,
                                                  res1Main, res2Main, res22Main, tran1Main, tran2Main,
                                                  tran3Main, tran4Main, parse1Main, parse2Main,
                                                  parse3Main, parse4Main, res2Mini, tran1Mini,
                                                  tran2Mini, tran3Mini, tran4Mini, parse1Mini,
                                                  parse2Mini, parse3Mini, parse4Mini, scratchDir);
   }
   catch (CConfigDataError * pError)
   {
      cout << "Configration file initialization failed. error code =" << pError->nErrorCode << " : " << pError->strErrMsg << endl;
      delete pError;
      return false;
   }
   if (pConfigInterface == NULL)
   {
      return CONFDATA_MOMERY_ERROR;
   }
   return CONFDATA_SUCCESS;
}

int C_GetConfigData(const char *lpszSecKey, const char *lpszNameKey, char *lpszBuf, int *pnBufSize)
{
   LgsString SecKey = lpszSecKey;
   LgsString NameKey = lpszNameKey;
   LgsString DataBuf;

   if (pConfigInterface == NULL)
   {
      return CONFDATA_INIT_INTERFACE_ERROR;
   }

   int nRet = pConfigInterface->GetConfigData(SecKey, NameKey, DataBuf);
   if (!nRet)
   {
      return CONFDATA_RETRIVE_ERROR;
   }

   if ((StringUtil::lower(DataBuf).find(StringUtil::lower(LgsRoot)) == NPOS) &&
#ifdef _MSC_VER
       (DataBuf[1] != ':')
#else
       (DataBuf[0] != '/')
#endif
       )
   {
      DataBuf = LgsRoot + DIR_SEP_STR + DataBuf;
   }
   if (DataBuf.length() < (*pnBufSize))
   {   //copy 
      ::strcpy(lpszBuf,DataBuf.c_str());
      *pnBufSize = DataBuf.length() + 1;
      return CONFDATA_SUCCESS;
   }
   else
   {
      *pnBufSize =  DataBuf.length() +1;
      return CONFDATA_BUFFER_ERROR;
   }
}
