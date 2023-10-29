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
// EngineData.cpp: implementation of the CEngineData class.
//
//////////////////////////////////////////////////////////////////////
#include <logos_include/logoscommon.h>
#include <configdatafileinterface/configdataerror.h>
#include <configdatafileinterface/configdatafile.h>
#include <configdatafileinterface/enginedata.h>

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CEngineData::CEngineData(int jobID, CConfigDataFile* pConfigDataFile, const char* scratchDir)
            :m_nJobID(jobID),
             m_pDataFile(pConfigDataFile),
             scratchDirectory(scratchDir)
{
   LgsString keyValues;
   LgsString sectionName = "TEMP_FILES";
   m_pDataFile->GetKeyValues(sectionName, keyValues);
}

CEngineData::~CEngineData()
{
}

bool CEngineData::GetData(LgsString& Section, LgsString& key, LgsString& DataBuf)
{
   LgsString sectionName;
   if (Section == "tempfile")
   {
      LgsString tempData;
      sectionName = "TEMP_FILES";
      if (m_pDataFile->GetValue(sectionName, key, tempData))
      {
         char cBuf[MAX_TEMP_FILE_LENGTH];
         ::sprintf(cBuf, "%d", m_nJobID);
         LgsString strJobID = cBuf;
         // NT to Unix check
         DataBuf = scratchDirectory + DIR_SEP_STR + strJobID + "." + tempData;
         return true;
      }
      else
         return false;
   }
   else if (Section  == "filter")
      sectionName = "FILTER";
   else if (Section  == "engine")
      sectionName = "LOGOS_ENGINE";
   else
      return false;

   return m_pDataFile->GetValue(sectionName, key, DataBuf);
}
