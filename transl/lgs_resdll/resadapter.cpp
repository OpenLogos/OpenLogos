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
//-----------------------------------------------------------------
// File - ResAdapter.cpp
//
// Class - ResAdaptor
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include "resadapter.h"
#include <transl/interface.h>
#include <logos_libs/linguistic/lword.h>
#include <logos_libs/multithreadlib/comminterface.h>

static LgsMessage* lkupMsg = 0;
extern CommunicationInterface *commInterface;
extern QHandle *qResReadHandle;
extern thread_id_t targetIdRes;

extern "C"
{
#include <logos_include_res_pt/jbctrl.h>
}   //extern c 

//-------------------------------------------------------------------
ResAdapter::ResAdapter(void)
{
}
//-------------------------------------------------------------------
ResAdapter::~ResAdapter()
{
}
//-------------------------------------------------------------------
ResAdapter* ResAdapter::Create(LgsMessage* msg)
{
   lkupMsg = msg;
   ResAdapter* p_adapter = new ResAdapter();
   return p_adapter;
}
//-------------------------------------------------------------------
SWorkBaseVector* ResAdapter::NextSentence()
{
   SWorkBaseVector* p_vector = 0;

   while (!p_vector)
   {
      commInterface->receiveMsg(qResReadHandle, *lkupMsg);

      if (lkupMsg->msgType() == EOD)
      {
         commInterface->sendMsg(targetIdRes, *lkupMsg);
         return 0;
      } 
      else if (lkupMsg->msgType() == UntranslatedLookupMsg)
      {
         fprintf(_spec_fp, "\nNo Translatable Units for Res");
			fprintf(_spec_fp, "\n*EOS*\n");
         commInterface->sendMsg(targetIdRes, *lkupMsg);
         continue;
      }
      else if (lkupMsg->msgType() == CLOSEFILE)
      {
         commInterface->sendMsg(targetIdRes, *lkupMsg);
         continue;
      }

      char * dataPtr = lkupMsg->dataPtr();
      ISentenceInfo * sentInfo = reinterpret_cast<ISentenceInfo *>(dataPtr);

      bool isObject;
      isObject = sentInfo->_isObject;
      if (!isObject )
      {
         return 0;
      }
      sentencePosition_ = sentInfo->_position;

      int size = sentInfo->_ssuListSize;
      int partOf = sentInfo->_partOf;
      int numberOfParts = sentInfo->_numberOfParts;

      isTranslateable_ = sentInfo->_translationState;
      if (ResAdapter::DoTranslate == isTranslateable_)
      {
         p_vector = new SWorkBaseVector;
      }
      caseState_ = sentInfo->_caseState;
      bold_ = sentInfo->_bold;
      italic_ = sentInfo->_italic;
      underlined_ = sentInfo->_underlined;
      singleQuoted_ = sentInfo->_singleQuoted;
      doubleQuoted_ = sentInfo->_doubleQuoted;

      char * wordSize = dataPtr + sizeof(ISentenceInfo);
      for (int w = 0; w < size; w++)
      {
         int wsize;
         bool excludeFromTranslation = false;

         memcpy((char *)&wsize, wordSize, sizeof(wsize));
         IWordMarkup * wrdMrkup = reinterpret_cast<IWordMarkup *>(wordSize + sizeof(int));

         // Calculate SWORK pointer
         IWordMarkup * tempWrdMarkup = wrdMrkup;
         char * sworkPtr = reinterpret_cast<char *>(wrdMrkup);
         for (int wordNo = 0; wordNo < wsize; wordNo++)
         {	
            if (tempWrdMarkup->_isExcludedFromTranslation)
               excludeFromTranslation = true;
            sworkPtr += sizeof(IWordMarkup) + tempWrdMarkup->_sizeOfWord;
            tempWrdMarkup = reinterpret_cast<IWordMarkup *>(sworkPtr);
         }

         SWorkBase swork;
         wordSize = streamInSWorkInfo(sworkPtr, swork);
         if (p_vector && !excludeFromTranslation)
         {
            p_vector->push_back(swork);
         }
      }
   }
   return p_vector;
}
//---------------------------------------------------------------------
void ResAdapter::TestPersistence()
{
}
