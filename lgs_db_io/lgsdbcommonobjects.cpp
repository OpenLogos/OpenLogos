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
#include <logos_include/logoscommon.h>
#include <lgs_db_io/lgsdbcommonobjects.h>
#include <logos_libs/sql/sqlconnection.h>
#include <logos_libs/odbcsql/globalsqlconnection.h>
#include <configdatafileinterface/configdatainterfacemain.h>

//-------------------------------------------------------------------
JobControlArguments* LgsDBCommonObjects::jobCntrlArgs = 0;
ServerProperties* LgsDBCommonObjects::serverProps = 0;
InteractiveTranslStatus* LgsDBCommonObjects::interTranslStatus = 0;
SqlConnection* LgsDBCommonObjects::sqlConnection = 0;

//-------------------------------------------------------------------
bool LgsDBCommonObjects::InitializeCommonObjects()
{
   serverProps = new ServerProperties();
   jobCntrlArgs = JobControlArguments::CreateObject(false);
//printf("LgsDBCommonObjects.InitializeCommonObjects \"%s\" create=%d\n", 
//    userID.c_str(), createJCA);
//XXX lines swapped
   sqlConnection = getSqlConnection();

   if( !jobCntrlArgs )
   {
      return false;
   }

   if( jobCntrlArgs->JobID() != -1 )
   {
      if( InitConfigDataInterface(jobCntrlArgs->Tranpasses(), jobCntrlArgs->JobID(), jobCntrlArgs->SourceLanguageStr().c_str(),
                                 jobCntrlArgs->TargetLanguageStr().c_str(), jobCntrlArgs->MainRes1().c_str(),
                                 jobCntrlArgs->MainRes2().c_str(), jobCntrlArgs->MainRes22().c_str(),
                                 jobCntrlArgs->MainTran1().c_str(), jobCntrlArgs->MainTran2().c_str(),
                                 jobCntrlArgs->MainTran3().c_str(), jobCntrlArgs->MainTran4().c_str(),
                                 jobCntrlArgs->MainParse1().c_str(), jobCntrlArgs->MainParse2().c_str(),
                                 jobCntrlArgs->MainParse3().c_str(), jobCntrlArgs->MainParse4().c_str(),
                                 jobCntrlArgs->MiniRes2().c_str(), jobCntrlArgs->MiniTran1().c_str(),
                                 jobCntrlArgs->MiniTran2().c_str(), jobCntrlArgs->MiniTran3().c_str(),
                                 jobCntrlArgs->MiniTran4().c_str(), jobCntrlArgs->MiniParse1().c_str(),
                                 jobCntrlArgs->MiniParse2().c_str(), jobCntrlArgs->MiniParse3().c_str(),
                                 jobCntrlArgs->MiniParse4().c_str(), serverProps->ScratchFileDirectory().c_str())
          == CONFDATA_SUCCESS )
      {
         interTranslStatus = new InteractiveTranslStatus(serverProps->ScratchFileDirectory(),
                                                         jobCntrlArgs->JobID());
      }
      else
      {
         cout << "Config data file initialization error" << endl;
         return false;
      }
   }
   return true;
}

//-------------------------------------------------------------------
void LgsDBCommonObjects::CleanupCommonObjects()
{
   if (interTranslStatus)
   {
        delete interTranslStatus;
   }
   if (jobCntrlArgs)
   {
        delete jobCntrlArgs;
   }
   if (sqlConnection)
   {
        sqlConnection->Close();
        freeSqlConnection(sqlConnection);
   }
   if (serverProps)
   {
        delete serverProps;
   }
}
//-------------------------------------------------------------------
JobControlArguments &LgsDBCommonObjects::GetJobControlArguments()
{
   return *jobCntrlArgs;
}
//-------------------------------------------------------------------
ServerProperties &LgsDBCommonObjects::GetServerProperties()
{
   return *serverProps;
}
//-------------------------------------------------------------------
InteractiveTranslStatus &LgsDBCommonObjects::GetInteractiveTranslStatus()
{
   return *interTranslStatus;
}
//-------------------------------------------------------------------
SqlConnection *LgsDBCommonObjects::GetSqlConnection()
{
   return sqlConnection;
}
