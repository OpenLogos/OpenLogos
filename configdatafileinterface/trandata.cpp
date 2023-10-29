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
// TranData.cpp: implementation of the CTranData class.
//
//////////////////////////////////////////////////////////////////////
#include <logos_include/logoscommon.h>
#include <configdatafileinterface/configdataerror.h>
#include <configdatafileinterface/configdatafile.h>
#include <configdatafileinterface/sourcedata.h>
#include <configdatafileinterface/trandata.h>
#define npos NPOS

//-------------------------------------------------------------------
CTranData::CTranData(const LgsString& srcLang, const LgsString& trgLang, bool isTwoPass,
                     CConfigDataFile* pConfigDataFile, const LgsString& tran1Main,
                     const LgsString& tran2Main, const LgsString& tran3Main,
                     const LgsString& tran4Main, const LgsString& parse1Main,
                     const LgsString& parse2Main, const LgsString& parse3Main,
                     const LgsString& parse4Main, const LgsString& tran1Mini,
                     const LgsString& tran2Mini, const LgsString& tran3Mini,
                     const LgsString& tran4Mini, const LgsString& parse1Mini,
                     const LgsString& parse2Mini, const LgsString& parse3Mini,
                     const LgsString& parse4Mini)
          :CSourceData(srcLang, trgLang, isTwoPass, pConfigDataFile),
           tran1MainFileName(tran1Main),
           tran2MainFileName(tran2Main),
           tran3MainFileName(tran3Main),
           tran4MainFileName(tran4Main),
           parse1MainFileName(parse1Main),
           parse2MainFileName(parse2Main),
           parse3MainFileName(parse3Main),
           parse4MainFileName(parse4Main),
           tran1MiniFileName(tran1Mini),
           tran2MiniFileName(tran2Mini),
           tran3MiniFileName(tran3Mini),
           tran4MiniFileName(tran4Mini),
           parse1MiniFileName(parse1Mini),
           parse2MiniFileName(parse2Mini),
           parse3MiniFileName(parse3Mini),
           parse4MiniFileName(parse4Mini)
{
   LgsString strSrc;
   LgsString strTrg;
   if (m_strSourceLang == "english") 
      strSrc = "ENGLISH";
   else if (m_strSourceLang == "german")
      strSrc = "GERMAN";
   else 
   {
      CConfigDataError* pError = new CConfigDataError();
      pError->nErrorCode = NOT_SUPPORTED_SRC_LANG;
      pError->strErrMsg = "Source language not supported error : " + strSrc;
      throw pError;
   }

   //Check the target lang
   if (m_strTargetLang == "english")  
      strTrg = "ENGLISH";
   else if (m_strTargetLang == "german")
      strTrg = "GERMAN";
   else if (m_strTargetLang == "french") 
      strTrg = "FRENCH";
   else if (m_strTargetLang == "spanish")
      strTrg = "SPANISH";
   else if (m_strTargetLang == "italian")  
      strTrg = "ITALIAN";
   else if (m_strTargetLang == "portuguese")
      strTrg = "PORTUGUESE";
   else 
   {
      CConfigDataError* pError = new CConfigDataError();
      pError->nErrorCode = NOT_SUPPORTED_TARG_LANG;
      pError->strErrMsg = "Target language not supported error :" + strTrg;
      throw pError;
   }
   if (strSrc == strTrg)
   {
      CConfigDataError* pError = new CConfigDataError();
      pError->nErrorCode = BAD_LANGUAGE_PAIR;
      pError->strErrMsg = "The source and target languages are the same : src=" + strSrc + " trg=" + strTrg;
      throw pError;
   }

   m_strLangPairSecName = strSrc + "-" + strTrg;
}
//-------------------------------------------------------------------
CTranData::~CTranData() 
{
}
//-------------------------------------------------------------------
bool CTranData::GetData(LgsString &key, LgsString& dataBuf)
{
   LgsString trankey;
   trankey = "trandata_" + key;
   if (key.find("mini") != npos)
   {
      return GetMiniFiles(key, dataBuf);
   }
   else
   {
      return GetMainFiles(key, dataBuf);
   }
}
//-------------------------------------------------------------------
bool CTranData::GetMiniFiles(LgsString& key, LgsString& dataBuf)
{
   LgsString miniFileName = "";

   // Parse files
   if (key.find("parse") != npos)
   {
      // If no user designated mini files, return blank string
      if (!parse1MiniFileName.size() && !parse2MiniFileName.size() &&
          !parse3MiniFileName.size() && !parse4MiniFileName.size())
      {
         dataBuf = "";
      }
      else if (key.find("table40") != npos)
      {
         // See if one of the 4 mini file names have been set
         if (parse1MiniFileName.size())
         {
            miniFileName = parse1MiniFileName;
         }
         else if (parse2MiniFileName.size())
         {
            miniFileName = parse2MiniFileName;
         }
         else if (parse3MiniFileName.size())
         {
            miniFileName = parse3MiniFileName;
         }
         else if (parse4MiniFileName.size())
         {
            miniFileName = parse4MiniFileName;
         }
         CreateMiniTableName(miniFileName, dataBuf, TABLE_40, true);
      }
      else
      {
         // Find the correct mini that has been set
         if (key.find("parse1") != npos)
         {
            miniFileName = parse1MiniFileName;
         }
         else if (key.find("parse2") != npos)
         {
            miniFileName = parse2MiniFileName;
         }
         else if (key.find("parse3") != npos)
         {
            miniFileName = parse3MiniFileName;
         }
         else if (key.find("parse4") != npos)
         {
            miniFileName = parse4MiniFileName;
         }

         if (key.find("table30") != npos)
         {
            CreateMiniTableName(miniFileName, dataBuf, TABLE_30, true);
         }
         else
         {
            dataBuf = miniFileName;
         }
      }
   }
   else  // Trans files
   {
      // If no tran minis were set then return a blank string
      if (!tran1MiniFileName.size() && !tran2MiniFileName.size() &&
          !tran3MiniFileName.size() && !tran4MiniFileName.size())
      {
         dataBuf = "";
         return true;
      }
      else if ((key.find("table40") != npos) || (key.find("table50") != npos))
      {
         // See if one of the 4 mini file names have been set
         if (tran1MiniFileName.size())
         {
            miniFileName = tran1MiniFileName;
         }
         else if (tran2MiniFileName.size())
         {
            miniFileName = tran2MiniFileName;
         }
         else if (tran3MiniFileName.size())
         {
            miniFileName = tran3MiniFileName;
         }
         else if (tran4MiniFileName.size())
         {
            miniFileName = tran4MiniFileName;
         }
         if (key.find("table40") != npos)
         {
            CreateMiniTableName(miniFileName, dataBuf, TABLE_40, false);
         }
         else
         {
            CreateMiniTableName(miniFileName, dataBuf, TABLE_50, false);
         }
      }
      else
      {
         // Find the correct mini that has been set
         if (key.find("tran1") != npos)
         {
            miniFileName = tran1MiniFileName;
         }
         else if (key.find("tran2") != npos)
         {
            miniFileName = tran2MiniFileName;
         }
         else if (key.find("tran3") != npos)
         {
            miniFileName = tran3MiniFileName;
         }
         else if (key.find("tran4") != npos)
         {
            miniFileName = tran4MiniFileName;
         }

         if (key.find("table30") != npos)
         {
            CreateMiniTableName(miniFileName, dataBuf, TABLE_30, false);
         }
         else
         {
            dataBuf = miniFileName;
         }
      }
   }
   return true;
}
//-------------------------------------------------------------------
bool CTranData::GetMainFiles(LgsString& key, LgsString& dataBuf)
{
   LgsString tranKey = "trandata_" + key;
   LgsString tran2PassKey = "trandata_2pass_" + key;
   LgsString mainFileName = "";

   if (key.find("parse") != npos) // parse tables
   {
      if (key.find("table40") != npos)
      {
         if (parse1MainFileName.size())
         {
            mainFileName = parse1MainFileName;
         }
         else if (parse2MainFileName.size())
         {
            mainFileName = parse2MainFileName;
         }
         else if (parse3MainFileName.size())
         {
            mainFileName = parse3MainFileName;
         }
         else if (parse4MainFileName.size())
         {
            mainFileName = parse4MainFileName;
         }

         if (mainFileName.size())
         {
            CreateMainTableName(mainFileName, dataBuf, TABLE_40);
         }
         else
         {
            return m_pDataFile->GetValue(m_strSectionName, tranKey, dataBuf);
         }
      }
      else
      {
         if (key.find("parse1") != npos)
         {
            mainFileName = parse1MainFileName;
         }
         else if (key.find("parse2") != npos)
         {
            mainFileName = parse2MainFileName;
         }
         else if (key.find("parse3") != npos)
         {
            mainFileName = parse3MainFileName;
         }
         else if (key.find("parse4") != npos)
         {
            mainFileName = parse4MainFileName;
         }

         if (mainFileName.size())
         {
            if (key.find("table30") != npos)
            {
               CreateMainTableName(mainFileName, dataBuf, TABLE_30);
            }
            else
            {
               dataBuf = mainFileName;
            }
         }
         else
         {
            return m_pDataFile->GetValue(m_strSectionName, tranKey, dataBuf);
         }
      }
   }
   else   // tran
   {
      if ((key.find("table40") != npos) || (key.find("table50") != npos))
      {
         if (parse1MainFileName.size())
         {
            mainFileName = tran1MainFileName;
         }
         else if (parse1MainFileName.size())
         {
            mainFileName = tran2MainFileName;
         }
         else if (parse1MainFileName.size())
         {
            mainFileName = tran3MainFileName;
         }
         else if (parse1MainFileName.size())
         {
            mainFileName = tran4MainFileName;
         }
         if (key.find("table40") != npos)
         {
            if (mainFileName.size())
            {
               CreateMainTableName(mainFileName, dataBuf, TABLE_40);
            }
            else if (m_b2Pass)
            {
               return m_pDataFile->GetValue(m_strLangPairSecName, tran2PassKey, dataBuf);
            }
            else
            {
               return m_pDataFile->GetValue(m_strLangPairSecName, tranKey, dataBuf);
            }
         }
         else
         {
            if (mainFileName.size())
            {
               CreateMainTableName(mainFileName, dataBuf, TABLE_50);
            }
            else if (m_b2Pass)
            {
               tranKey = "trandata_2pass_" + m_strTargetLang + "_" + key;
               return m_pDataFile->GetValue(m_strSectionName, tranKey, dataBuf); 
            }
            else
            {
               tranKey = "trandata_" + m_strTargetLang + "_" + key;
               return m_pDataFile->GetValue(m_strSectionName, tranKey, dataBuf); 
            }
         }
      }
      else
      {
         if (key.find("tran1") != npos)
         {
            mainFileName = tran1MainFileName;
         }
         else if (key.find("tran2") != npos)
         {
            mainFileName = tran2MainFileName;
         }
         else if (key.find("tran3") != npos)
         {
            mainFileName = tran3MainFileName;
         }
         else if (key.find("tran4") != npos)
         {
            mainFileName = tran4MainFileName;
         }

         if (key.find("table30") != npos)
         {
            if (mainFileName.size())
            {
               CreateMainTableName(mainFileName, dataBuf, TABLE_30);
            }
            else if (m_b2Pass)
            {
               return m_pDataFile->GetValue(m_strLangPairSecName, tran2PassKey, dataBuf);
            }
            else
            {
               return m_pDataFile->GetValue(m_strLangPairSecName, tranKey, dataBuf);
            }
         }
         else  // Main tran
         {
            if (mainFileName.size())
            {
               dataBuf = mainFileName;
            }
            else if (m_b2Pass)
            {
               return m_pDataFile->GetValue(m_strLangPairSecName, tranKey, dataBuf);
            }
            else
            {
               return m_pDataFile->GetValue(m_strSectionName, tranKey, dataBuf);
            }
         }
      }
   }
   return true;
}
