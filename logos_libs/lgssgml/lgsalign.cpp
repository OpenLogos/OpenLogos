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
//////////////////////////////////////////////////////////////////////
//
// LgsAlign.cpp: implementation of the CLgsAlign class.
//
//////////////////////////////////////////////////////////////////////
#include <logos_include/logoscommon.h>
#include <logos_libs/lgssgml/lgsutil.h>
#include <logos_libs/lgssgml/lgsalign.h>
#include <lgs_db_io/lgsdbcommonobjects.h>
#include <configdatafileinterface/configdatainterfacemain.h>

// -------------------------------------------------------------------
CLgsAlign::CLgsAlign(bool split, int bufsize)
          :m_bSplit(split)
{
   if (m_bSplit)
   {
      char sourceTempAlignedFileName[MAX_FILEPATH_LEN];

      GetConfigData("tempfile", "sourceTempAligned", sourceTempAlignedFileName, MAX_FILEPATH_LEN);
      m_alignedFile = new CLgsAlignTarget(sourceTempAlignedFileName, bufsize);
      CLgsUtil::Assert(m_alignedFile, "Source Temporary Aligned File failure");
   }
   else
   {
      char targetTempAlignedFileName[MAX_FILEPATH_LEN];

      GetConfigData("tempfile", "targetTempAligned", targetTempAlignedFileName, MAX_FILEPATH_LEN);
      m_alignedFile = new CLgsAlignTarget(targetTempAlignedFileName, bufsize);
      CLgsUtil::Assert(m_alignedFile, "Target Temporary Aligned File failure");
   }
}
// -------------------------------------------------------------------
CLgsAlign::~CLgsAlign()
{
   if (m_alignedFile)
   {
      delete m_alignedFile;
   }
}
// -------------------------------------------------------------------
void CLgsAlign::GetFormatSentence(const char* formatSent, int sentID)
{
   m_sentID = sentID;
   m_sent.GetFormatSentence(formatSent);
}
// -------------------------------------------------------------------
void CLgsAlign::GetTransSourceWords(WV& words, int start, int count)
{
   v_tranWords.clear();

   // Vector Iterators
   WVIT vIndex;
   WVIT vStartIndex = words.begin() + start;
   WVIT vEndIndex = words.begin() + start + count;

   for (vIndex = vStartIndex; vIndex != vEndIndex; vIndex++)
   {
      if (vIndex->GetTranslate())
      {
         CLgsWord word = *vIndex;

         // Add trailing spaces if they exist.
         if (((vIndex + 1) != vEndIndex) && ((*(vIndex + 1)).TotalSpaces() > 0))
         {
            LgsString spaceStr((*(vIndex + 1)).TotalSpaces(), ' ');
            word = word.toString() + spaceStr;
         }
         v_tranWords.push_back(word);
      }
   }
}
// -------------------------------------------------------------------
void CLgsAlign::GetTransTargetWords(WV& words)
{
   v_tranWords.clear();

   // Vector Iterators
   WVIT vIndex;
   WVIT vStartIndex = words.begin();
   WVIT vEndIndex = words.end();

   for (vIndex = vStartIndex; vIndex != vEndIndex; vIndex++)
   {
      CLgsWord word = *vIndex;
      v_tranWords.push_back(word);
   }
}
// -------------------------------------------------------------------
void CLgsAlign::AlignSentence()
{
   LgsString doubleQuote = "\"";
   LgsString singleQuote = "\'";
   LgsString endOfLine = "\n";

   bool bSingleQuoted = false;
   bool bDoubleQuoted = false;
   for (WVIT vindex = v_tranWords.begin(); vindex != v_tranWords.end(); vindex++)
   {
      LgsString tranString = vindex->toString();
      CLgsWord thisWord;
      bool ret = m_sent.GetWord(vindex->Id(), thisWord);
      thisWord = *vindex;

      //Opening quotes
      if (thisWord.IsSglQuoted() && !bSingleQuoted)
      {
         bSingleQuoted = true;
         m_alignedFile->write(singleQuote.c_str(), singleQuote.length());
      }

      if (thisWord.IsDblQuoted() && !bDoubleQuoted)
      {
         bDoubleQuoted = true;
         m_alignedFile->write(doubleQuote.c_str(), doubleQuote.length());
      }

      if (vindex->IsProtected()) 
      {
         CLgsWord lgsWord;
         bool stat = m_sent.GetWord(vindex->Id(), lgsWord);
         LgsString origStr;
         if (stat)
         {
            origStr = lgsWord.toString();
         }
         else
         {
            cerr << "Protected word was not found in the format file" << endl;
         }
         vindex->SetTranString(tranString, origStr);
      }

      //Remove the trailing space
      LgsString copyString = tranString;
      LgsString::iterator rit = tranString.end();
      if (rit != tranString.begin())
      {
         rit--;
      }
      while ((rit != tranString.begin()) && (*rit == 0x20))
      {
         tranString.erase(rit--);
      }
      m_alignedFile->write(tranString.c_str(), tranString.length());

      //Closing Quotes
      if (vindex + 1 != v_tranWords.end())
      {
         ret = m_sent.GetWord((vindex+1)->Id(), thisWord);
      }
      else
      {
         ret = false;
      }

      if (ret && ((vindex + 1) != v_tranWords.end()))
      {
         thisWord = *(vindex+1);
      }

      if ((!ret || !thisWord.IsSglQuoted()) && bSingleQuoted) 
      {
         bSingleQuoted = false;
         m_alignedFile->write(singleQuote.c_str(), singleQuote.length());
      }

      if ((!ret || !thisWord.IsDblQuoted()) && bDoubleQuoted)
      {
         bDoubleQuoted = false;
         m_alignedFile->write(doubleQuote.c_str(), doubleQuote.length());
      }

      //Print the trailing space that was stripped from the word
      if (copyString.length() > tranString.length())
      {
         m_alignedFile->write(copyString.c_str() + tranString.length(), copyString.length() - tranString.length());
      }
   }
   m_alignedFile->write(endOfLine.c_str(), endOfLine.length());
}


