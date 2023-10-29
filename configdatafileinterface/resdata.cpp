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
// ResData.cpp: implementation of the CResData class.
//
//////////////////////////////////////////////////////////////////////
#include <logos_include/logoscommon.h>
#include <configdatafileinterface/configdataerror.h>
#include <configdatafileinterface/configdatafile.h>
#include <configdatafileinterface/resdata.h>

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CResData::CResData(const LgsString& srcLang, const LgsString& trgLang, bool isTwoPass,
                   CConfigDataFile* pConfigDataFile, const LgsString& res1Main,
                   const LgsString& res2Main, const LgsString& res22Main, const LgsString& res2Mini)
         :CSourceData(srcLang, trgLang, isTwoPass, pConfigDataFile),
          res1MainFileName(res1Main),
          res2MainFileName(res2Main),
          res22MainFileName(res22Main),
          res2MiniFileName(res2Mini)
{
   if (m_strSourceLang == "english")
   {
      m_strSectionName = "ENGLISH-SOURCE";
   }
   else if (m_strSourceLang == "german")
   {
      m_strSectionName = "GERMAN-SOURCE";
   }
   else 
   {
      CConfigDataError* pError = new CConfigDataError();
      pError->nErrorCode = NOT_SUPPORTED_SRC_LANG;
      pError->strErrMsg = "Source language not supported error: " + m_strSourceLang;
      throw pError;
   }
}

CResData::~CResData()
{
}

bool CResData::GetData(LgsString& key, LgsString& DataBuf)
{
   LgsString resDataFile = "";
   LgsString reskey;
   reskey = "resdata_" + key;

   if (key == "res2mini")
   {
      DataBuf = res2MiniFileName;
      return true;
   }

   if (key == "res1")
   {
      resDataFile = res1MainFileName;
   }
   else if (key == "res2")
   {
      resDataFile = res2MainFileName;
   }
   else if (key == "res22")
   {
      resDataFile = res22MainFileName;
   }

   if (resDataFile.size())
   {
      DataBuf = resDataFile;
      return true;
   }
   else
   {
      return m_pDataFile->GetValue(m_strSectionName, reskey, DataBuf);
   }
}
