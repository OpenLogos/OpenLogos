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
#include <logos_libs/lgssgml/lgstranio.h>
#include <logos_libs/lgssgml/lgsutil.h>
#include <logos_libs/lgssgml/lgssentence.h>

#include <stdio.h>
#include <transl/interface.h>
#include <transl/translthrman.h>
#include <logos_libs/multithreadlib/comminterface.h>

LgsMessage* generateMsg = 0;

//Constructor
LgsTranIO::LgsTranIO(bool loadOption)
          :loadOnly(loadOption)
{
   if (loadOnly)
   {
      generateMsg = new LgsMessage;
   }
}
// -------------------------------------------------------------------
//Destructor
LgsTranIO::~LgsTranIO()
{
   if (loadOnly)
   {
      delete generateMsg;
      generateMsg = 0;
   }
}
// -------------------------------------------------------------------
//Extraction Operator
bool LgsTranIO::Load(WV& wordList, int& sentenceID)
{
   TranslThreadManager & thrManager = TranslThreadManager::singleton();
   const LgsSgmlMerger* mergerThread = dynamic_cast<const LgsSgmlMerger*>(thrManager.GetThread(TranslThreadManager::LGSSGML_MERGE.c_str()));
	const QHandle* qHandle = mergerThread->generateOutHandle();
	CommunicationInterface & commInterface = CommunicationInterface::singleton();
	commInterface.receiveMsg(qHandle, *generateMsg);
   wordList.clear();

	// If End Of Document; return
	if ((generateMsg->msgType() == EOD) || (generateMsg->msgType() == CLOSEFILE))
	{
		return false;
	}

	char* dataPtr = generateMsg->dataPtr();

	ILgsSentInfo* sentInfo = reinterpret_cast<ILgsSentInfo *>(dataPtr);

   sentenceID = -1;

   if (sentInfo->_totWords > 0)
   {
      sentenceID = sentInfo->_id;
      ILgsWordMarkup* wordInfo = reinterpret_cast<ILgsWordMarkup *>(dataPtr + sizeof(ILgsSentInfo));
      for (int cntr = 0; cntr < sentInfo->_totWords; cntr++)
      {
	      char* theWord = (reinterpret_cast<char *>(wordInfo)) + sizeof(ILgsWordMarkup);
         char* wordBuffer = new char[wordInfo->_sizeOfWord + 1];
         memcpy(wordBuffer, theWord, wordInfo->_sizeOfWord);
         wordBuffer[wordInfo->_sizeOfWord] = '\0';
         LgsString actualWord(wordBuffer);
         delete[] wordBuffer;
         CLgsWord pLgsWord;
         fillLgsWord(pLgsWord, *wordInfo, actualWord);
         wordList.push_back(pLgsWord);
         wordInfo = reinterpret_cast<ILgsWordMarkup *>(theWord + wordInfo->_sizeOfWord);
      }
      return true;
   }
   return false;
}
// -------------------------------------------------------------------
void LgsTranIO::fillLgsWord(CLgsWord& lgsWord, ILgsWordMarkup& wordInfo, LgsString& theWord)
{
   LgsString aSpace = " ";

   lgsWord.Id(wordInfo._id);
   lgsWord.SetProtected(wordInfo._isProtected);
   lgsWord.SetTranslate(true);

   if (wordInfo._trailingSpaces > 0)
   {
      lgsWord = (theWord + aSpace).c_str();
   }
   else
   {
      lgsWord = theWord.c_str();
   }

   if (wordInfo._isBold)
   {
      lgsWord.SetBold();
   }

   if (wordInfo._isItalic)
   {
      lgsWord.SetItalic();
   }

   if (wordInfo._isUnderlined)
   {
      lgsWord.SetUnderline();
   }

   if (wordInfo._isSingleQuoted)
   {
      lgsWord.SetSglQuoted();
   }

   if (wordInfo._isDoubleQuoted)
   {
      lgsWord.SetDblQuoted();
   }
}
// -------------------------------------------------------------------
// Insertion Operator
void LgsTranIO::Store(int sentId, WV& rhs, int start, int count) 
{
   LgsString ProtectedWord = "^p";

   // Vector Iterator
   WVIT vIndex;
   WVIT vStartIndex = rhs.begin() + start;
   WVIT vEndIndex = rhs.begin() + start + count;
   
	//  +-------------------------------------------------------+
	// -| Calculate size of message to be sent to LGSSGML Merge |
	//  +-------------------------------------------------------+
	int msgSize = 0;

   msgSize += sizeof(ILgsSentInfo); // Sentence ID & number of words in sentence
   // Determine how many LgsWords are going to be sent
   int wordCount = 0;

   for (vIndex = vStartIndex; vIndex != vEndIndex; vIndex++)
   {
      if (vIndex->GetTranslate())
      {
         wordCount++;

         if (vIndex->IsProtected())
         {
            msgSize += ProtectedWord.length();
         }
         else
         {
            msgSize += vIndex->toString().length();
         }
      }
   }
   msgSize += wordCount * sizeof(ILgsWordMarkup);

	//  +-------------------------------+
	// -| construct the message to send |
	//  +-------------------------------+
   TranslThreadManager & thrManager = TranslThreadManager::singleton();
   short lookupId = getThreadId(thrManager.threadList(), TranslThreadManager::LOOKUP.c_str());
   LgsMessage outMsg(SplitterMsg, msgSize, 0, lookupId);
   char* dataPtr = outMsg.dataPtr();    // the message content

   ILgsSentInfo* sentInfo = reinterpret_cast<ILgsSentInfo *>(dataPtr);
   sentInfo->_id = sentId;
   sentInfo->_totWords = wordCount;
   ILgsWordMarkup* wordInfo = reinterpret_cast<ILgsWordMarkup *>(dataPtr + sizeof(ILgsSentInfo));
   char* theWord = 0;
   // QuoteWords(rhs, start, count);

   // Now write out the individual words
   for (vIndex = vStartIndex; vIndex != vEndIndex; vIndex++) 
   {
      if (vIndex->GetTranslate())
      {
         fillWordInfo(*wordInfo, vIndex, vStartIndex, vEndIndex);
	      char* theWord = (reinterpret_cast<char *>(wordInfo)) + sizeof(ILgsWordMarkup);

         if (vIndex->IsProtected())
         {
            memcpy(theWord, ProtectedWord.c_str(), ProtectedWord.length());
            wordInfo = reinterpret_cast<ILgsWordMarkup *>(theWord + ProtectedWord.length());
         }
         else
         {
            memcpy(theWord, vIndex->toString().c_str(), vIndex->toString().length());
            wordInfo = reinterpret_cast<ILgsWordMarkup *>(theWord + vIndex->toString().length());
         }
      }
   }
	//  +------------------+
	// -| send the message |
	//  +------------------+
   CommunicationInterface::singleton().sendMsg(lookupId, outMsg);
}
// -------------------------------------------------------------------
void LgsTranIO::SendEndOfDocument()
{
   TranslThreadManager & thrManager = TranslThreadManager::singleton();
   short lookupId = getThreadId(thrManager.threadList(), TranslThreadManager::LOOKUP.c_str());
	LgsMessage outMsg(EOD, 0, 0, lookupId);
	CommunicationInterface::singleton().sendMsg(lookupId, outMsg);
}
// -------------------------------------------------------------------
void LgsTranIO::fillWordInfo(ILgsWordMarkup& wordInfo, WVIT lgsWord, WVIT firstWord, WVIT wordEnd) 
{
   LgsString ProtectedWord = "^p";
   LgsString spaceInfo;
   wordInfo._id = lgsWord->Id();

   wordInfo._precedingSpaces = 0;
   wordInfo._trailingSpaces = 0;

   if ((lgsWord != firstWord) && (lgsWord->SpaceInfo(spaceInfo) > 0))
   {
      wordInfo._precedingSpaces = 1;
   }

   if (((lgsWord + 1) != wordEnd) && ((lgsWord + 1)->SpaceInfo(spaceInfo) > 0))
   {
      wordInfo._trailingSpaces = 1;
   }

   wordInfo._isProtected = lgsWord->IsProtected();

   if (lgsWord->IsProtected())
   {
      wordInfo._sizeOfWord = ProtectedWord.length();
   }
   else
   {
      wordInfo._sizeOfWord = lgsWord->toString().length();
   }

   wordInfo._isBold = lgsWord->IsBold();
   wordInfo._isItalic = lgsWord->IsItalic();
   wordInfo._isUnderlined = lgsWord->IsUnderline();
   wordInfo._isSingleQuoted = lgsWord->IsSglQuoted();
   wordInfo._isDoubleQuoted = lgsWord->IsDblQuoted();
}
// -------------------------------------------------------------------
void LgsTranIO::QuoteWords(WV& rhs, int start, int count)
{
   bool SingleQuoted = false;
   bool DoubleQuoted = false;
   LgsString detachedSpaces;
   LgsVector(int) attrs;
   LgsVector(LgsString) attrSps;

   WVIT wit = rhs.begin() + start;
   while (wit != (rhs.begin() + start + count))
   {
      LgsString word = wit->toString();
      if (word.length() == 1)
      {
         switch(word[0])
         {
         case '\"':
            DoubleQuoted = !DoubleQuoted;
            wit->SetTranslate(false);
            wit->SpaceInfo(detachedSpaces);
            if (attrs.size() == 0)
            {
               wit->Attributes(attrs, attrSps);
            }
            else
            {
               LgsVector(int) lattr;
               LgsVector(LgsString) lattrSps;
               wit->Attributes(lattr, lattrSps);
               for (LgsVector(int)::iterator vit = lattr.begin(); vit != lattr.end(); vit++)
               {
                  attrs.push_back(*vit);
               }
               for (LgsVector(LgsString)::iterator vsit = lattrSps.begin(); vsit != lattrSps.end(); vsit++)
               {
                  attrSps.push_back(*vsit);
               }
            }
            break;

         case '\'':
            SingleQuoted = !SingleQuoted;
            wit->SetTranslate(false);
            wit->SpaceInfo(detachedSpaces);
            if (attrs.size() == 0)
            {
               wit->Attributes( attrs, attrSps);
            }
            else
            {
               LgsVector(int) lattr;
               LgsVector(LgsString) lattrSps;
               wit->Attributes(lattr, lattrSps);
               for (LgsVector(int)::iterator vit = lattr.begin(); vit != lattr.end(); vit++)
               {
                  attrs.push_back(*vit);
               }
               for (LgsVector(LgsString)::iterator vsit = lattrSps.begin(); vsit != lattrSps.end(); vsit++)
               {
                  attrSps.push_back(*vsit);
               }
            }
            break;
         }
      }

      if ((DoubleQuoted || SingleQuoted) && wit->GetTranslate())
      {
         LgsString spaces;
         if (DoubleQuoted)
         {
            wit->SetFlag(CLgsWord::dblquote);
         }
         else if (SingleQuoted)
         {
            wit->SetFlag(CLgsWord::sglquote);
         }
         wit->SpaceInfo(spaces);
         detachedSpaces += spaces;
         wit->SetSpaceInfo(detachedSpaces.length(), detachedSpaces);
         detachedSpaces.erase(detachedSpaces.begin(), detachedSpaces.end());
         LgsVector(int) lattr;
         LgsVector(LgsString) lattrSps;
         wit->Attributes(lattr, lattrSps);
         for (LgsVector(int)::iterator vit = lattr.begin(); vit != lattr.end(); vit++)
         {
            attrs.push_back(*vit);
         }
         for (LgsVector(LgsString)::iterator vsit = lattrSps.begin(); vsit != lattrSps.end(); vsit++)
         {
            attrSps.push_back(*vsit);
         }
         wit->SetAttributes(attrs, attrSps);
         attrs.erase(attrs.begin(), attrs.end());
         attrSps.erase(attrSps.begin(), attrSps.end());
      }
      wit++;
   }
}
