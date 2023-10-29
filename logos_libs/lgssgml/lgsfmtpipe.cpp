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
// LgsFmtPipe.cpp: implementation of the CLgsFmtPipe class.
//
//////////////////////////////////////////////////////////////////////

#include <transl/translthrman.h>
#include <logos_libs/multithreadlib/comminterface.h>
LgsMessage* formatMsg = 0;

#include <logos_libs/lgssgml/lgsfmtpipe.h>

// -------------------------------------------------------------------
CLgsFmtPipe::CLgsFmtPipe(bool bSplit)
            :m_bSplit(bSplit)
{
   if (!m_bSplit)
   {
      formatMsg = new LgsMessage;
   }
}
// -------------------------------------------------------------------
CLgsFmtPipe::~CLgsFmtPipe()
{
   if (!m_bSplit)
   {
      delete formatMsg;
      formatMsg = 0;
   }
}
// -------------------------------------------------------------------
unsigned int CLgsFmtPipe::receiveMsg(char *msg, int& msgSize)
{
   if (!m_bSplit)
   {
      TranslThreadManager & thrManager = TranslThreadManager::singleton();
      const LgsSgmlMerger* mergerThread = dynamic_cast<const LgsSgmlMerger*>(thrManager.GetThread(TranslThreadManager::LGSSGML_MERGE.c_str()));
	   const QHandle* qHandle = mergerThread->splitterOutHandle();
	   CommunicationInterface::singleton().receiveMsg(qHandle, *formatMsg);

	   // If End Of Document; return
	   if ((formatMsg->msgType() == EOD) || (formatMsg->msgType() == CLOSEFILE))
	   {
		   return (unsigned int)CLgsFmtPipe::TRANSEND;
	   }

	   char* info = formatMsg->dataPtr();

      unsigned int msgType;
	   memcpy((char *)&msgType, info, sizeof(unsigned int));
      info += sizeof(unsigned int);

      memcpy((char *)&msgSize, info, sizeof(int));
      info += sizeof(int);

      memcpy(msg, info, msgSize);
      return msgType;
   }

   return (unsigned int)CLgsFmtPipe::TRANSEND;
}
// -------------------------------------------------------------------
int CLgsFmtPipe::sendMsg(unsigned int msgType, const char* msg, int msgSize)
{
   if (m_bSplit)
   {
      // Get the handle to the receive pipe owned by Merger thread.
      TranslThreadManager& thrManager = TranslThreadManager::singleton();
      const LgsSgmlMerger* pMerger = dynamic_cast<const LgsSgmlMerger *>(thrManager.GetThread(TranslThreadManager::LGSSGML_MERGE.c_str()));

      //Create the message structure
      LgsMessage outMsg(LgsSgmlFormat, (msgSize + sizeof(unsigned int) + sizeof(int)), 0, pMerger->threadId());
      char* dataPtr = outMsg.dataPtr();
      memcpy(dataPtr, (const char*)&msgType, sizeof(unsigned int));
      dataPtr += sizeof(unsigned int);
      memcpy(dataPtr, (const char*)&msgSize, sizeof(int));
      dataPtr += sizeof(int);
      memcpy(dataPtr, msg, msgSize);
      CommunicationInterface::singleton().sendMsg(pMerger->splitterOutHandle(), outMsg);
      return msgSize;
   }
   return 0;
}
// -------------------------------------------------------------------
void CLgsFmtPipe::sendEndOfDocument()
{
   TranslThreadManager& thrManager = TranslThreadManager::singleton();
   const LgsSgmlMerger* pMerger = dynamic_cast<const LgsSgmlMerger *>(thrManager.GetThread(TranslThreadManager::LGSSGML_MERGE.c_str()));

   //Create the message structure
   LgsMessage outMsg(EOD, 0, 0, pMerger->threadId());
   CommunicationInterface::singleton().sendMsg(pMerger->splitterOutHandle(), outMsg);
}
// -------------------------------------------------------------------
int CLgsFmtPipe::write(const char *msg, int msgSize)
{
   return sendMsg((unsigned int)CLgsFmtPipe::RAWDATA, msg, msgSize);
}
