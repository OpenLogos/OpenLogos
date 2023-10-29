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
#include <transl/AsciiAlignTool.h>
#include <time.h>

//--------------------------------------------------------------------------------------------
AsciiAlignTool::AsciiAlignTool(const LgsString &nameOrigFile, const LgsString &nameTransFile,
                               const LgsString &nameOutputFile, const LgsString &nameRealOrigFile,
                               const LgsString &nameRealTransFile)
               :origFileName(nameOrigFile),
               transFileName(nameTransFile),
               outputFileName(nameOutputFile),
               realOrigFileName(nameRealOrigFile),
               realTransFileName(nameRealTransFile),
               origLineCount(0),
               transLineCount(0),
               hasRealOrigFileName(false),
               hasRealTransFileName(false)
{
   // Check to see if the real file names were provided
   if (realOrigFileName.length())
   {
      hasRealOrigFileName = true;
   }
   if (realTransFileName.length())
   {
      hasRealTransFileName = true;
   }
   
   // Open the original & translation input files and the output file.
#ifdef _MSC_VER
   origFile.open(getOrigFileName().c_str(), ios::binary);
   transFile.open(getTransFileName().c_str(), ios::binary);
   outputFile.open(getOutputFileName().c_str(), ios::binary);
#else
   origFile.open(getOrigFileName().c_str());
   transFile.open(getTransFileName().c_str());
   outputFile.open(getOutputFileName().c_str());
#endif
}

//--------------------------------------------------------------------------------------------
AsciiAlignTool::~AsciiAlignTool()
{
   origFile.close();
   transFile.close();
   outputFile.close();
}

//--------------------------------------------------------------------------------------------
void AsciiAlignTool::alignText()
{  
   LgsString errMsg = "Could not open input file: ";
   
   // Check to see if the original input file opened successfully.
   if (!origFile.good())
   {
      errMsg += getOrigFileName();
      cout << errMsg << endl;
      return;
   }
   
   // Check to see if the translation input file opened successfully.
   if (!transFile.good())
   {
      errMsg += getTransFileName();
      cout << errMsg << endl;
      return;
   }
   
   // Check to see if the output file opened successfully.
   if (!outputFile.good())
   {
      errMsg += getOutputFileName();
      cout << errMsg << endl;
      return;
   }
   
   LgsString currLine;
   
   // Get and format the current time and date to be printed out later.
   char currDateStr[10];
   char currTimeStr[10];
   
   time_t locTime;
   time(&locTime);
   struct tm* currTime = localtime(&locTime);
   
   sprintf(currDateStr, "%.2d-%.2d-%.2d", currTime->tm_mon + 1, currTime->tm_mday, currTime->tm_year);
   sprintf(currTimeStr, "%.2d:%.2d:%.2d", currTime->tm_hour + ((currTime->tm_isdst) ? -1 : 0), currTime->tm_min, currTime->tm_sec);
   
   // Count the lines in the files.
   origLineCount = countLinesInFile(origFile);
   transLineCount = countLinesInFile(transFile);
   
   // Output the header.
   outputFile << "<HTML>\n";
   outputFile << "<HEAD>\n\n";
   outputFile << "<CENTER>\n";
   outputFile << "<TABLE BORDER BGCOLOR=\"#87CEFA\">\n";
   outputFile << "<TR>\n";
   outputFile << "<TH ALIGN=CENTER>\n";
   outputFile << "<FONT SIZE=+2><B>Logos Aligned Text</B></FONT>\n";
   outputFile << "</TH>\n";
   outputFile << "</TR>\n";
   outputFile << "</TABLE>\n";
   outputFile << "</CENTER>\n";
   outputFile << "</HEAD>\n";
   outputFile << "<BODY BACKGROUND=\"/bin/logos.gif\">\n";
   outputFile << "<CENTER>\n";
   
   outputFile << "<TABLE WIDTH=100%>\n";
   outputFile << "<TR>\n";
   outputFile << "\t<TD ALIGN=CENTER>\n";
   outputFile << "\t<TABLE BORDER WIDTH=90% BGCOLOR=\"#EEEEEE\">\n";
   outputFile << "\t<TR>\n";
   outputFile << "\t\t<TH WIDTH=15%>Attributes</TH>\n";
   outputFile << "\t\t<TH WIDTH=15%>Sentences</TH>\n";
   outputFile << "\t\t<TH WIDTH=70%>File Names</TH>\n";
   outputFile << "\t</TR>\n";
   outputFile << "\t<TR>\n";
   outputFile << "\t\t<TD ALIGN=LEFT>Source</TD>\n";
   outputFile << "\t\t<TD ALIGN=CENTER><B>" << origLineCount << "</B></TD>\n";
   outputFile << "\t\t<TD ALIGN=LEFT>" << getOrigFileName(true) << "</TD>\n";
   outputFile << "\t</TR>\n";
   outputFile << "\t<TR>\n";
   outputFile << "\t\t<TD ALIGN=LEFT>Target</TD>\n";
   outputFile << "\t\t<TD ALIGN=CENTER><B>" << transLineCount << "</B></TD>\n";
   outputFile << "\t\t<TD ALIGN=LEFT>" << getTransFileName(true) << "</TD>\n";
   outputFile << "\t</TR>\n";
   outputFile << "\t<TR>\n";
   outputFile << "\t\t<TD ALIGN=LEFT>Date</TD>\n";
   outputFile << "\t\t<TD ALIGN=CENTER WIDTH=30%>" << currDateStr << "</TD>\n";
   outputFile << "\t\t<TD ALIGN=CENTER WIDTH=40%>" << currTimeStr << "</TD>\n";
   outputFile << "\t</TR>\n";
   outputFile << "\t</TABLE>\n";
   outputFile << "\t</TD>\n";
   outputFile << "</TR>\n";
   outputFile << "</TABLE>\n";
   outputFile << "</CENTER>\n";
   outputFile << "<BR>\n";
   outputFile << "<CENTER>\n";
   outputFile << "<TABLE BORDER WIDTH=100% BGCOLOR=\"#EEEEEE\">\n";
   outputFile << "<TR>\n";
   outputFile << "\t<TH ALIGN=CENTER WIDTH=4%>#</TH>\n";
   outputFile << "\t<TH ALIGN=LEFT WIDTH=48%>Source Line</TH>\n";
   outputFile << "\t<TH ALIGN=LEFT WIDTH=48%>Target Line</TH>\n";
   outputFile << "</TR>\n";
   
   // Prepare to get the lines of text from the files.
   origFile.clear();
   origFile.seekg(0, ios::beg);
   transFile.clear();
   transFile.seekg(0, ios::beg);
   
   LgsString origLine;
   LgsString transLine;
   LgsString countLine;
   int lineCntr = 0;
   char lineCntrStr[10];
   
   // Output the lines of text.
   while (getoneline(origFile, origLine, '\n') && origFile.good() &&
      getoneline(transFile, transLine, '\n') && transFile.good())
   {
      lineCntr++;
      sprintf(lineCntrStr, "%d", lineCntr);
      countLine = lineCntrStr;
      
      outputFile << "<TR>\n";
      outputFile << "\t<TD ALIGN=CENTER WIDTH=4%>" << countLine << "</TD>\n";
      outputFile << "\t<TD ALIGN=LEFT WIDTH=48%>" << origLine << "</TD>\n";
      outputFile << "\t<TD ALIGN=LEFT WIDTH=48%>" << transLine << "</TD>\n";
      outputFile << "</TR>\n";
   }
   
   outputFile << "</CENTER>\n";
   outputFile << "</TABLE>\n";
   outputFile << "</BODY>\n";
   outputFile << "</HTML>\n";
}

//--------------------------------------------------------------------------------------------
int AsciiAlignTool::countLinesInFile(ifstream& inputFile)
{
   int count = 0;
   LgsString currLine;
   
   // Make sure the file is set to begin reading at the beginning of the file.
   inputFile.clear();
   inputFile.seekg(0, ios::beg);
   
   while (getoneline(inputFile, currLine, '\n') && inputFile.good())
   {
      count++;
   }
   
   return count;
}

//--------------------------------------------------------------------------------------------
LgsString::iterator AsciiAlignTool::getNextWord(LgsString &str, LgsString::iterator currChar,
                                                LgsString &nextWord, bool skipLeadingSpaces)
{
   nextWord = "";
   
   // If requested, skip all leading spaces now.
   if (skipLeadingSpaces)
   {
      while ((currChar != str.end()) && (*currChar == ' '))
      {
         currChar++;
      }
   }
   else
   {
      if ((currChar != str.end()) && (*currChar == ' '))
      {
         currChar++;
         return currChar;
      }
   }
   
   // Now get the next word.
   while ((currChar != str.end()) && (*currChar != ' '))
   {
      nextWord.append(currChar, (currChar + 1));
      currChar++;
   }
   return currChar;
}

//--------------------------------------------------------------------------------------------
ifstream& AsciiAlignTool::getoneline(ifstream& inFile, LgsString &strVar, char delimeter)
{
   // Object space does not provide this version of getline function
   strVar.erase(strVar.begin(), strVar.end());
   if (inFile.good())
   {
      char inBuf[4096];
      int nChar;
      inFile.getline(inBuf, 4095, delimeter);
      nChar = inFile.gcount();
      inBuf[nChar] = '\0';
      int length = strlen(inBuf);
      if (((length - 1) > 0) && (inBuf[length-1] == 0xd))
      {
         inBuf[length - 1] = '\0';
      }
      
      // The characters '<' and '>' are interpreted as tags by browsers used to
      // view the aligned output HTML files so we have to convert them into entity
      LgsString strBuf(inBuf);
      Char2Entity(strBuf);
      strVar = strBuf;
   }
   return inFile;
}

//--------------------------------------------------------------------------------------------
void AsciiAlignTool::Char2Entity(LgsString &str)
{
   // Find the character and insert the entity if found.
   const char *pchar = str.c_str();
   int ncount = str.length();
   char ch;
   LgsString strTemp;
   LgsString strBuf;
   for (int i = 0; i < ncount; i++)
   {
      ch = pchar[i];
      switch(ch)
      {
      case '<':
         strTemp = "&lt;";
         break;
         
      case  '>':
         strTemp = "&gt;";
         break;
         
      default:
         strTemp = ch;
         break;
      }
      strBuf = strBuf + strTemp;
   }
   
   str = strBuf;
}
