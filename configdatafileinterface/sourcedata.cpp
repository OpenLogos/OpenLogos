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
// SourceData.cpp: implementation of the CSourceData class.
//
//////////////////////////////////////////////////////////////////////
#include <logos_include/logoscommon.h>
#include <configdatafileinterface/configdataerror.h>
#include <configdatafileinterface/configdatafile.h>
#include <configdatafileinterface/sourcedata.h>

//-------------------------------------------------------------------
CSourceData::CSourceData(const LgsString& srcLang, const LgsString& trgLang, bool isTwoPass,
                         CConfigDataFile* pConfigDataFile)
            :m_strSourceLang(srcLang),
             m_strTargetLang(trgLang),
             m_b2Pass(isTwoPass),
             m_pDataFile(pConfigDataFile)
{
   if (m_strSourceLang == "english")  
      m_strSectionName = "ENGLISH-SOURCE";
   else if (m_strSourceLang == "german")
      m_strSectionName = "GERMAN-SOURCE";
   else if (m_strSourceLang == "french") 
      m_strSectionName = "FRENCH-SOURCE";
   else if (m_strSourceLang == "spanish")
      m_strSectionName = "SPANISH-SOURCE";
   else if (m_strSourceLang == "italian")  
      m_strSectionName = "ITALIAN-SOURCE";
   else if (m_strSourceLang == "portuguese")
      m_strSectionName = "PORTUGUESE-SOURCE";
   else 
   {
      CConfigDataError* pError = new CConfigDataError();
      pError->nErrorCode = NOT_SUPPORTED_LANG;
      pError->strErrMsg = "language not supported error :" + m_strSourceLang;
      throw pError;
   }
}
//-------------------------------------------------------------------
CSourceData::~CSourceData()
{
}
//-------------------------------------------------------------------
bool CSourceData::GetData(LgsString &key, LgsString& dataBuf)
{
   return m_pDataFile->GetValue(m_strSectionName, key, dataBuf);
}
//-------------------------------------------------------------------
void CSourceData::GetParseTranNames(LgsString& mainDir, LgsString& languageName, LgsString& fullName,
                                    LgsString& mainName, const LgsString& origFileName)
{
   int index1;
   int index2;
   LgsString dirName;
   LgsString langName;
   LgsString fileName;
   LgsString mName;
   LgsString fullPath = origFileName;

   // Extract the directory name and file name
   index1 = fullPath.rfind(DIR_SEP_CHAR);
   dirName = fullPath.substr(0, index1);
   fileName = fullPath.substr((index1 + 1), (fullPath.length() - index1 - 1));
   index1 = fileName.find('.');
   langName = fileName.substr(0, index1);
   index2 = fileName.find('.', (index1 + 1));
   fileName = fileName.substr((index1 + 1), (index2 - index1 - 1));
   index1 =  fileName.find_first_of("0123456789");
   mName = fileName.substr(0, index1);

   // return the values
   mainDir = dirName;
   languageName = langName;
   fullName = fileName;
   mainName = mName;
}
//-------------------------------------------------------------------
void CSourceData::GetParseTranMiniNames(LgsString& miniDir, LgsString& fullName,
                                        LgsString& miniName, const LgsString& origFileName)
{
   int nIndex;
   LgsString dirName;
   LgsString fileName;
   LgsString mName;
   LgsString fullPath = origFileName;

   // Extract the directory name and file name
   nIndex = fullPath.rfind(DIR_SEP_CHAR);
   dirName = fullPath.substr(0, nIndex);
   fileName = fullPath.substr(nIndex + 1, fullPath.length() - nIndex - 1);
   nIndex = fileName.find('.');
   fileName = fileName.substr(0, nIndex);
   nIndex=  fileName.find_first_of("0123456789");
   mName = fileName.substr(0, nIndex);

   // return the values
   miniDir = dirName;
   fullName = fileName;
   miniName = mName;
}
//-------------------------------------------------------------------
LgsString CSourceData::LanguageShortName(const LgsString& origLang)
{
   if (origLang == "portuguese")
   {
      return "ptg";
   }
   else
   {
      return origLang.substr(0, 3);
   }
}
//-------------------------------------------------------------------
void CSourceData::CreateMainTableName(const LgsString& origFileName, LgsString& tableName,
                                      TableTYPE tableType)
{
   LgsString directory;
   LgsString language;
   LgsString fullName;
   LgsString baseName;

   tableName = "";
   if (origFileName.size())
   {
      GetParseTranNames(directory, language, fullName, baseName, origFileName);
      if (tableType == TABLE_50)
      {
         tableName = directory + DIR_SEP_STR + m_strTargetLang + "." + baseName + ".lgr50";
      }
      else if (m_b2Pass)
      {
         if (tableType == TABLE_30)
         {
            tableName = directory + DIR_SEP_STR + language + "." + fullName + ".lgr30";
         }
         else
         {
            tableName = directory + DIR_SEP_STR + language + "." + baseName + ".lgr40";
         }
      }
      else
      {
         LgsString shortSrcName = LanguageShortName(m_strSourceLang);
         LgsString shortTrgName = LanguageShortName(m_strTargetLang);

         if (tableType == TABLE_30)
         {
            tableName = directory + DIR_SEP_STR + shortSrcName + shortTrgName + "." + fullName + ".lgr30";
         }
         else
         {
            tableName = directory + DIR_SEP_STR + shortSrcName + shortTrgName + "." + baseName + ".lgr40";
         }
      }
   }
}
//-------------------------------------------------------------------
void CSourceData::CreateMiniTableName(const LgsString& origFileName, LgsString& tableName,
                                      TableTYPE tableType, bool isParse)
{
   LgsString directory;
   LgsString fullName;
   LgsString baseName;

   tableName = "";
   if (origFileName.size())
   {
      GetParseTranMiniNames(directory, fullName, baseName, origFileName);
      if (tableType == TABLE_30)
      {
         tableName = directory + DIR_SEP_STR + fullName + ".lgr30";
      }
      else if (isParse)
      {
         tableName = directory + DIR_SEP_STR + baseName + ".lgr40";
      }
      else
      {
         LgsString tranPassStr = "";
         if (m_b2Pass)
         {
            tranPassStr = "2";
         }

         if (tableType == TABLE_40)
         {
            tableName = directory + DIR_SEP_STR + baseName + tranPassStr + ".lgr40";
         }
         else
         {
            tableName = directory + DIR_SEP_STR + baseName + tranPassStr + ".lgr50";
         }
      }
   }
}
