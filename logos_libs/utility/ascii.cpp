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
//---------------------------------------------------------------------
// File - ascii.cpp
//
// Class - Ascii (implementation)
//
//---------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/utility/stringutil.h>
#include <logos_libs/utility/ascii.h>
#ifdef _MSC_VER
#include <windows.h>
#endif
//Constant required for WriteWinProfileString and ReadWinProfileString
#define MAXFILENAME 256
#define MAX_LINE_LENGTH MAXFILENAME

#ifdef _MSC_VER
#define strncasecmp _strnicmp
#define strcasecmp _stricmp
#endif
#define NO_ERROR 0
#define ER_UNDEFINED -1
//-------------------------------------------------------------------
LgsString Ascii::getEnvironment(const LgsString& s)
{
   char buffer[MaximumArraySize];

   strcpy(buffer, s.c_str());

   char* value = ::getenv(buffer);

   LgsString v;

   if (value)
   {
      return v = value;
   }
   return v;
}
//-------------------------------------------------------------------
void Ascii::putEnvironment(const LgsString& name, const LgsString& value)
{
   LgsString envString = name + "=" + value;
   // changed bk Jul 19 2005 because of non-constness of putenv argument
   char *temp = new char[envString.size() + 1];
   strcpy(temp, envString.c_str());
   putenv(temp);
   delete[] temp;
}
//-------------------------------------------------------------------
LgsString Ascii::getPrivateProfileString(const LgsString& fileName,
                                         const LgsString& sectionName,
                                         const LgsString& keyword)
{
    assert(!fileName.empty());
    assert(!sectionName.empty());

    char name[MaximumArraySize];
    char section[MaximumArraySize];
    char key[MaximumArraySize];
    char result[MaximumArraySize];

    strcpy (name, fileName.c_str());
    strcpy (section, sectionName.c_str());
    strcpy (key, keyword.c_str());

    char defaultIni[] = "LOGOS DEFAULT INI VALUE";
#ifdef _MSC_VER//Call Windows Native API
    ::GetPrivateProfileString(section, key, defaultIni, result, 1023, name);
#else //Call ReadWinProfileString
	ReadWinProfileString(section, key, defaultIni, result, 1023, name);
#endif
    LgsString resultString;
    if (strcmp(defaultIni,result) != 0)
    {
        resultString = result;
    }

    return resultString;
}

//-------------------------------------------------------------------
void Ascii::writePrivateProfileString(const LgsString& fileName,
                                      const LgsString& sectionName,
                                      const LgsString& keyword,
                                      const LgsString& value)
{
    assert(!fileName.empty());
    assert(!sectionName.empty());

    char name[MaximumArraySize];
    char section[MaximumArraySize];
    char key[MaximumArraySize];
    char v[MaximumArraySize];

    strcpy (name, fileName.c_str());
    strcpy (section, sectionName.c_str());
    strcpy (key, keyword.c_str());
    strcpy (v, value.c_str());

    char defaultIni[] = "LOGOS DEFAULT INI VALUE";
#ifdef _MSC_VER //Windows - call the native API
    ::WritePrivateProfileString(section, key, v, name);
#else //Unix - call WriteWinProfileString
    Ascii::WriteWinProfileString(section, key, v, name);
#endif
}
//WritePrivateProfileString() and ReadPrivateProfileString() are not available on Unix
//The following methods substitute their functionality on UNIX, thus making this class
//portable across Windows and Unix platforms
/***************************************************************************
 * Function:    WriteWinProfileString()
 * Arguments:   <char*> section - the name of the section to search for
 *              <char*> entry - the name of the entry to find the value of
 *              <char*> buffer - pointer to the buffer that holds the LgsString
 *              <char*> file_name - the name of the .ini file to read from
 * Returns:     NO_ERROR if successful, ER_UNDEFINED otherwise 
 ***************************************************************************/
struct SectionEntry
{
   LgsString Key;
   LgsString Value;
};

struct Section
{
   LgsString Name;
   LgsVector(SectionEntry) EntryList;
};

int Ascii::WriteWinProfileString(char* section, char* entry, char* buffer, char* file_name)
{   
   LgsVector(Section) lSection;
	fstream iniFile;
   char buff[MAX_LINE_LENGTH];
   LgsString sectionname = LgsString("[") + LgsString(section) + LgsString("]");
   LgsString key = entry;
   LgsString value = buffer;

   //Open the ini file for read
   iniFile.open(file_name, ios::in);
   if (!iniFile.good()) //File does not exist
   {
      iniFile.close();
      iniFile.open(file_name, ios::out);
		if (!iniFile.good())
			return ER_UNDEFINED;
		iniFile << sectionname << endl;
		iniFile << key << "=" << value << endl;
		iniFile.close();
		return NO_ERROR;
   }
   
   //Load the file into memory
   iniFile.getline(buff, MAX_LINE_LENGTH);
   buff[iniFile.gcount()] = 0; //String termination
   LgsString readLine = buff;
   Section* nextSection = 0;
   SectionEntry* nextEntry = 0;
   while (!iniFile.eof() && iniFile.good())
   {
      StringUtil::leftTrim(readLine);
      StringUtil::leftTrim(readLine, '\t');
      if (readLine[0] == '[')
      {
         if (nextSection)
         {
            lSection.push_back(*nextSection);
            delete nextSection;
            nextSection = 0;
         }
         nextSection = new Section;
         nextSection->Name = readLine;
      }
      else if (readLine.find("=") != NPOS)
      {
         nextEntry = new SectionEntry;
         int nPos = readLine.find("=");
         nextEntry->Key = readLine.substr(0, nPos);
         nextEntry->Value = readLine.substr(nPos+1, readLine.length() - nPos -1);
         if (nextSection)
            nextSection->EntryList.push_back(*nextEntry);
         delete nextEntry;
         nextEntry = 0;
      }
      else 
      {
         nextEntry = new SectionEntry;
         nextEntry->Value = readLine;
         if (!nextSection)
            nextSection = new Section;
         nextSection->EntryList.push_back(*nextEntry);
         delete nextEntry;
         nextEntry = 0;
      }
      iniFile.getline(buff, MAX_LINE_LENGTH);
      buff[iniFile.gcount()] = 0; //String termination
      readLine = buff;
   }

   iniFile.close();

   if (nextSection)
   {
      lSection.push_back(*nextSection);
      delete nextSection;
      nextSection = 0;
    }
   //File has been loaded into the memory
   
   //Search for the section in the list of sections
   LgsVector(Section)::iterator lit;
   for (lit = lSection.begin(); lit != lSection.end();lit++) 
   {
      if (lit->Name == sectionname)
         break;
   }

   if (lit == lSection.end()) //Section not found
   {
      if (!nextSection)
         nextSection = new Section;
      nextSection->Name = sectionname;
      SectionEntry* nextEntry = new SectionEntry;
      nextEntry->Key = key;
      nextEntry->Value = value;
      nextSection->EntryList.push_back(*nextEntry);
      delete nextEntry;
      nextEntry = 0;
      lSection.push_back(*nextSection);
      delete nextSection;
      nextSection = 0;
   }
   else
   {
      SectionEntry* nextEntry;
      LgsVector(SectionEntry)::iterator leit;
      for (leit = lit->EntryList.begin(); leit != lit->EntryList.end(); leit++)
      {  
          if (leit->Key == key)
             break;
      }
      if (leit != lit->EntryList.end())
         leit->Value = value;
      else //Key not found
      {
         nextEntry = new SectionEntry;
         nextEntry->Value = value;
         nextEntry->Key = key;
         lit->EntryList.push_back(*nextEntry);
         delete nextEntry;
         nextEntry = 0;
      }
   }

   //Write back into the file
   fstream outFile(file_name, ios::out);
   if (!outFile.good())
      return ER_UNDEFINED;
   for (LgsVector(Section)::iterator liter = lSection.begin(); liter != lSection.end(); liter++)
   {
      if (!liter->Name.empty())
         outFile << liter->Name << endl;
      for (LgsVector(SectionEntry)::iterator iter = liter->EntryList.begin();
          iter != liter->EntryList.end(); iter++)
      {
          if (!iter->Key.empty())
             outFile << iter->Key << "=";
          if (!iter->Value.empty())
          outFile << iter->Value << endl;
      }
   }
   iniFile.close();
   return NO_ERROR;
}


/**************************************************************************
* Function:     ReadWinProfileString()
* Arguments:    <char *> section - the name of the section to search for
*               <char *> entry - the name of the entry to find the value of
*               <char *> def - default LgsString in the event of a failed read
*               <char *> buffer - a pointer to the buffer to copy into
*               <int> buffer_len - the max number of characters to copy
*               <char *> file_name - the name of the .ini file to read from
* Returns:      the number of characters copied into the supplied buffer
***************************************************************************/
int Ascii::ReadWinProfileString(char *section, char *entry, char *def, 
                                char *buffer, int buffer_len, char *file_name)
{ 
   fstream iniFile;
   char buff[MAX_LINE_LENGTH];
   strncpy(buffer, def, buffer_len);
   LgsString key = entry;
   StringUtil::toLower(key);
   LgsString sectionname = section;
   StringUtil::toLower(sectionname);
   sectionname = LgsString("[") + sectionname + LgsString("]");
   iniFile.open(file_name, ios::in);
   if (!iniFile.good())   
      return ER_UNDEFINED;

   iniFile.getline(buff,MAX_LINE_LENGTH);
   buff[iniFile.gcount()] = 0;
   LgsString readLine = buff;
   while (!iniFile.eof() && iniFile.good() && !readLine.empty())
   {
      StringUtil::leftTrim(readLine);
      StringUtil::leftTrim(readLine,'\t');
      StringUtil::rightTrim(readLine);
      StringUtil::toLower(readLine);
      if (readLine[0] = '[') 
      {
         if (readLine == sectionname)
            break; //section found
      }
      iniFile.getline(buff,MAX_LINE_LENGTH);
      buff[iniFile.gcount()] = 0;
      readLine = buff;
   }

   if (iniFile.eof())
   {
      iniFile.close();
      return ER_UNDEFINED;
   }
   else
   {
      iniFile.getline(buff,MAX_LINE_LENGTH);
      buff[iniFile.gcount()] = 0;
      readLine = buff;
      while (!iniFile.eof() && iniFile.good() && !readLine.empty())
      {
         StringUtil::leftTrim(readLine);
         StringUtil::leftTrim(readLine,'\t');
         StringUtil::rightTrim(readLine);
         if (readLine[0] == '[') 
         {
            iniFile.close();
            return ER_UNDEFINED; //Key was not found
         }
         if (readLine.find("=") != NPOS)
         {
            int nPos = readLine.find("=");
            LgsString keyword = readLine.substr(0, nPos);
            StringUtil::toLower(keyword);
            if (keyword == key)
               break;
         }
         iniFile.getline(buff,MAX_LINE_LENGTH);
         buff[iniFile.gcount()] = 0;
         readLine = buff;
      }
      if (iniFile.eof())
         return ER_UNDEFINED;
      int nPos = readLine.find("=") + 1;
      LgsString value = readLine.substr(nPos, readLine.length() - nPos);
      strncpy(buffer, value.c_str(), value.length());
      buffer[value.length()] = 0;
      iniFile.close();
      return NO_ERROR;
   }
}




