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
#include <lgs_db_io/serverproperties.h>
#include <logos_libs/utility/ascii.h>
#include <logos_libs/utility/stringutil.h>
//#include <logos_libs/utility/argumentexception.h>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif

//-------------------------------------------------------------------
//	checks the existance of a directory
//-------------------------------------------------------------------
bool IsDirectory(const char *dir)
{
#ifdef _MSC_VER
   DWORD nResult = GetFileAttributes(dir);
   if( nResult != 0xFFFFFFFF )
   {
      return nResult & FILE_ATTRIBUTE_DIRECTORY;
   }
#endif
#ifdef HAVE_DIRENT_H
   DIR *thedir = opendir(dir);
   if (thedir != NULL) {
     closedir(thedir);
     return true;
   }
#endif
   return false;
}
//-------------------------------------------------------------------
ServerProperties::ServerProperties()
{
   // Check LGS_ROOT environment variable
   char *envLgsRoot = ::getenv("LGS_ROOT");
   if( !envLgsRoot )
   {
      cout << endl << "ERROR: LGS_ROOT environment variable is not set!" << endl;
      exit(-1);
   }
   // Check LGS_ROOT directory
   if( !IsDirectory(envLgsRoot) )
   {
      cout << endl << "ERROR: LGS_ROOT directory \"" << envLgsRoot << "\" is not exist!" << endl;
      exit(-1);
   }

   // Check Server Properties file
   serverPropertiesFileName = LgsString(envLgsRoot) + DIR_SEP_STR 
     + "bin" + DIR_SEP_STR + "server.properties";
   #if _MSC_VER >= 1100
      ifstream serverPropertiesStream(serverPropertiesFileName.c_str());
   #else
      ifstream serverPropertiesStream(serverPropertiesFileName.c_str());
   #endif
   if (!serverPropertiesStream.good())
   {
      cout << endl << "ERROR: Unable to open Logos Server Properties file!" << endl;
      exit(-1);
   }
   serverPropertiesStream.close();
   propertiesSection = "settings";
}
//-------------------------------------------------------------------
ServerProperties::~ServerProperties()
{
}
//-------------------------------------------------------------------
LgsString ServerProperties::GetPropertiesProfileString(const LgsString& keyword)
{
	return Ascii::getPrivateProfileString(serverPropertiesFileName, propertiesSection, keyword);
}
//-------------------------------------------------------------------
void ServerProperties::WritePropertiesProfileString(const LgsString& keyword, const LgsString& value)
{
	Ascii::writePrivateProfileString(serverPropertiesFileName, propertiesSection, keyword, value);
}
//-------------------------------------------------------------------
void ServerProperties::WritePropertiesProfileInt(const LgsString& keyword, int value)
{
	char valueAsString[21];
	sprintf(valueAsString, "%d", value);
   WritePropertiesProfileString(keyword, valueAsString);
}
//-------------------------------------------------------------------
int ServerProperties::gatewayPort()
{
   return StringUtil::asInteger(GetPropertiesProfileString("Gateway_Port"));
}
//-------------------------------------------------------------------
LgsString ServerProperties::JDBCDataSource()
{
   return GetPropertiesProfileString("JDBCDataSource");
}
//-------------------------------------------------------------------
LgsString ServerProperties::ODBCDataSource()
{
   return GetPropertiesProfileString("ODBCDataSource");
}
//-------------------------------------------------------------------
LgsString ServerProperties::OracleDataSource()
{
   return GetPropertiesProfileString("OracleDataSource");
}
//-------------------------------------------------------------------
LgsString ServerProperties::DatabaseDriver()
{
   return GetPropertiesProfileString("Database_Driver");
}
//-------------------------------------------------------------------
LgsString ServerProperties::DatabaseProtocol()
{
   return GetPropertiesProfileString("Database_Protocol");
}
//-------------------------------------------------------------------
LgsString ServerProperties::DatabaseUserID()
{
   return GetPropertiesProfileString("Database_UserID");
}
//-------------------------------------------------------------------
LgsString ServerProperties::DatabasePassword()
{
   return GetPropertiesProfileString("Database_Password");
}
//-------------------------------------------------------------------
LgsString ServerProperties::JobFileDirectory()
{
   return GetPropertiesProfileString("Jobfile_Dir");
}
//-------------------------------------------------------------------
LgsString ServerProperties::ScratchFileDirectory()
{
   // Try Scratch dir first
   LgsString sDir(GetPropertiesProfileString("Scratchfile_Dir"));
   if( IsDirectory(sDir.c_str()) )
   {
      return sDir;
   }
   // Try all TMP dirs
   char *envTmpDir = ::getenv("TMP");
   if( envTmpDir && IsDirectory(envTmpDir) )
   {
      return envTmpDir;
   }
   envTmpDir = ::getenv("TEMP");
   if( envTmpDir && IsDirectory(envTmpDir) )
   {
      return envTmpDir;
   }
   envTmpDir = ::getenv("TMPDIR");
   if( envTmpDir && IsDirectory(envTmpDir) )
   {
      return envTmpDir;
   }

   cout << endl << "ERROR: Unable to use Logos Scratch directory!" << endl;
   exit(-1);
}
//-------------------------------------------------------------------
LgsString ServerProperties::TranslDSWFile()
{
   return GetPropertiesProfileString("TranslDSW");
}
//-------------------------------------------------------------------
int ServerProperties::TranslationServerInterval()
{
   return StringUtil::asInteger(GetPropertiesProfileString("Translation_Server_Interval"));
}
//-------------------------------------------------------------------
LgsString ServerProperties::ExcludeRunningUserID()
{
   return GetPropertiesProfileString("Exclude_Running_UserID");
}
