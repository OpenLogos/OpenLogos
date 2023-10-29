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
#include <stdio.h>
#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif
#include <logos_libs/halfnoun/germandictionary.h>
#include <logos_libs/halfnoun/germansearch.h>
#include <logos_libs/halfnoundll/germancache.h>
#include <logos_java/GermanHalfNoun/GermanHalfNoun.h>
#include <logos_libs/utility/stringutil.h>

// _fix_me_ The code needs this, which was extracted from
// dev/filters/libraries/mif/winlib.c
#include "logos_include/getprivateprofilestring.h"

GermanCache* theGermanCache = 0;
LgsVector(LgsString)* companies = 0;

const char* GetRidOfUTFChars(const char*);
bool GetEnvVariable(LgsString envVarName, LgsString& envVarValue);
bool SetEnvVariable(LgsString envVarName, LgsString& envVarValue);
bool GetServerProperty(LgsString key, LgsString& value);
void SetupCompanyCodes(LgsVector(LgsString)* companies, LgsString& companyList);

bool GetEnvVariable(LgsString envVarName, LgsString& envVarValue)
{
   envVarValue.erase();
   char* envVar = getenv(envVarName.c_str());

   if (envVar)
   {
      envVarValue = envVar;
      return true;
   }
   else
      return false;
}

bool SetEnvVariable(LgsString envVarName, LgsString& envVarValue)
{
   LgsString newEnvVar = envVarName + "=" + envVarValue;
   char *temp = new char[newEnvVar.size() + 1]; 
   strcpy(temp, newEnvVar.c_str());
   bool res = (putenv(temp) == 0);
   delete[] temp;
   return res;
}

bool GetServerProperty(LgsString key, LgsString& value)
{
   char buffer[256];
   buffer[0] = '\0';
   LgsString envLGSROOT;

   // Get the environment variable for the logos root directory path
   if (!GetEnvVariable("LGS_ROOT", envLGSROOT))
   {
      return false;
   }
   LgsString SvrPropFileName = envLGSROOT + DIR_SEP_STR + "bin" 
     + DIR_SEP_STR + "server.properties";
   if (GetPrivateProfileString("settings", key.c_str(), "", buffer, 256, SvrPropFileName.c_str()) == 0)
   {
      return false;
   }
   else
   {
      value = buffer;
      return true;
   }
}

void SetupCompanyCodes(LgsVector(LgsString)* companies, LgsString& companyList)
{
   int pos = 0;
   LgsString company = "LOG";
   companies->push_back(company);

   // Get all the companies from the list and add them to the vector of companies.
   while (pos < companyList.length())
   {
      company = companyList.substr(pos, 3);
      companies->push_back(company);
      pos += 3;
   }
}

JNIEXPORT jboolean JNICALL Java_logos_1java_GermanHalfNoun_GermanHalfNoun_InitializeHalfNounEnvironment
(JNIEnv* javaEnv, jobject halfNoundObj, jstring companyCodes)
{
	const char* companyCodesPtr = javaEnv->GetStringUTFChars(companyCodes, 0);
	companyCodesPtr = GetRidOfUTFChars(companyCodesPtr);
   LgsString companyCodesStr = companyCodesPtr;

   companies = new LgsVector(LgsString);
   SetupCompanyCodes(companies, companyCodesStr);
	javaEnv->ReleaseStringUTFChars(companyCodes, companyCodesPtr);

   LgsString envLGS_ROOT;
   LgsString envLGS_SCRATCH;

   if (!GetEnvVariable("LGS_ROOT", envLGS_ROOT))
      return JNI_FALSE;

   if (!GetServerProperty("Scratchfile_Dir", envLGS_SCRATCH))
      return JNI_FALSE;

   LgsString envLGS_DATA = envLGS_ROOT + "/bin_data/german";
   LgsString halfNounLogFileName = envLGS_SCRATCH + "/java.halfnoun.log";
   LgsString halfNounWordListFileName = envLGS_DATA + "/halfnoun.wordlist";
   LgsString halfNounPlaceNamesFileName = envLGS_DATA + "/german.place_names";
   LgsString halfNounProperNamesFileName = envLGS_DATA + "/german.proper_names";
   LgsString halfNounControlFileName = envLGS_ROOT + "/bin/halfnoun.ini";

   theGermanCache = &(GermanCache::singleton(halfNounControlFileName,
                                             halfNounWordListFileName,
                                             halfNounLogFileName,
                                             halfNounPlaceNamesFileName,
                                             halfNounProperNamesFileName,
                                             *companies));
   if (theGermanCache)
      return JNI_TRUE;
   else
      return JNI_FALSE;
}

JNIEXPORT jint JNICALL Java_logos_1java_GermanHalfNoun_GermanHalfNoun_FindHeadWordPosition
(JNIEnv* javaEnv, jobject halfNoundObj, jstring compoundWord, jint headWordCase)
{
	const char* compoundWordPtr = javaEnv->GetStringUTFChars(compoundWord, 0);
	compoundWordPtr = GetRidOfUTFChars(compoundWordPtr);
   LgsString compoundWordStr = compoundWordPtr;
	javaEnv->ReleaseStringUTFChars(compoundWord, compoundWordPtr);

   GermanTokenList decomposition;
   GermanHeadWordInfo headWordInfo;

   switch (headWordCase)
   {
   case 1:
      headWordInfo = nounHeadWord;
      break;
   case 2:
      headWordInfo = adjHeadWord;
      break;
   default:
      headWordInfo = nounAdjHeadWord;
      break;
   }

   if (theGermanCache->germanSearch().decompose(compoundWordStr, decomposition, headWordInfo,
                                                StringUtil::isAllUpperCase(compoundWordStr)))
   {
	  char *normalizedCompound = new char [compoundWordStr.length()*2+1];
	  GermanUtil::normalize(compoundWordStr.c_str(),normalizedCompound);
	  LgsString normalizedVal(normalizedCompound);

      LgsString headWord = decomposition.getHeadWord()->text_;
      int headPos = normalizedVal.rfind(headWord);
	  int retPos = 0;
	  for (LgsString::const_iterator charIter = compoundWordStr.begin();
		   headPos; ((*charIter) == 'ﬂ')?headPos-=2:headPos--, retPos++, charIter++);
	  delete [] normalizedCompound;

	  return retPos;		
   }
   else
      return -1;
}

JNIEXPORT void JNICALL Java_logos_1java_GermanHalfNoun_GermanHalfNoun_CloseHalfNounEnvironment
(JNIEnv* javaEnv, jobject halfNoundObj)
{
   delete companies;
   theGermanCache->destroySingleton();
}

const char* GetRidOfUTFChars(const char* cString)
{
	char tempStr[255];
	static char newStr[255];
	int a=0;
	int b=0;
	int templen;

	strcpy(tempStr, cString);

	templen = strlen(tempStr); 

	for(a=0; a < templen ; a++)
	{
		if (tempStr[a] & 0x80)  //found an extended character
		{
//We know bit 7 is set to get this far, so check bit 6 (UTF high order char)
			if (tempStr[a] & 1)
				newStr[b] = (tempStr[a+1] | 0x40);  //this byte looks like: 11??????
			else
				newStr[b] = tempStr[a+1];			//this byte looks like: 10??????
			b++;
			a++;
		}
		else
		{
			newStr[b] = tempStr[a];
			b++;
		}
	}
	newStr[b] = '\0';  //null terminate the LgsString
	return newStr;
}
