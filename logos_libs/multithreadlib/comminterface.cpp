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
//////////////////////////////////////////////////////////////////////////////
//	Member function definitions for the CommunicationInterface class, MsgData
//	LgsMessage and QHandle classes.
//
//	Author: Sundar Ramaswamy; Date: 5/98
//
//      Modified: Bernd Kiefer, DFKI, 08/2005


#include <logos_include/logoscommon.h>
//#include <lgs_base/multiheap.h>
#include <logos_libs/multithreadlib/comminterface.h>
#include <logos_libs/multithreadlib/lgscomminterface.h>
#include <lgs_db_io/lgsdbcommonobjects.h>

#ifdef MEMORYCHECK
extern void IncThreadMemCount(THREAD_ID &id, unsigned int size);
extern void DecThreadMemCount(THREAD_ID &id, unsigned int size);
char validString[5] = "!~~!";
#endif

CommunicationInterface  *CommunicationInterface::_commInterface = 0;
// --------------------------------------------------------------------------
CommunicationInterface & CommunicationInterface::singleton(void)
{
   if (!_commInterface)
   {
      LgsCommInterface::makeCommInterface();
   }
   return *commInterface();
}
// --------------------------------------------------------------------------
void CommunicationInterface::destroySingleton(void)
{
   if (_commInterface)
   {
      delete _commInterface;
      _commInterface = 0;
   }
}
// --------------------------------------------------------------------------
LgsMessage::MsgData::MsgData(short iType, int iLength,
                             thread_id_t iSource, thread_id_t iTarget)
{
   _dataCritSect.initialize();
   _refCount = 1;
   _sourceThreadId = GetCurrentThreadId();
   _dataPtr = (char *) (malloc(iLength + sizeof(MsgHeader)));
   
#ifdef MEMORYCHECK
   IncThreadMemCount(_sourceThreadId, (iLength + sizeof(MsgHeader)));
#endif

   MsgHeader *headerPtr = reinterpret_cast<MsgHeader *>(_dataPtr);
   headerPtr->_msgType = iType;
   headerPtr->_msgLength = iLength;
   headerPtr->_sourceId = _sourceThreadId;
   headerPtr->_targetId = iTarget;
}
// --------------------------------------------------------------------------
LgsMessage::MsgData::~MsgData(void)
{
   if (_dataPtr)
   {
#ifdef MEMORYCHECK
      MsgHeader *headerPtr = reinterpret_cast<MsgHeader *>(_dataPtr);
      DecThreadMemCount(headerPtr->_sourceId, (headerPtr->_msgLength + sizeof(MsgHeader)));
#endif
      free(_dataPtr);
      _dataPtr = 0;
   }
}
// --------------------------------------------------------------------------
void* LgsMessage::MsgData::operator new (size_t dataSize)
{
   void* memPtr;
   unsigned int newDataSize = dataSize;

#ifdef MEMORYCHECK
   newDataSize += sizeof(MemoryHeader);
#endif

   memPtr = malloc(newDataSize);

#ifdef MEMORYCHECK
   THREAD_ID id = GetCurrentThreadId();
   MemoryHeader* memHead = reinterpret_cast<MemoryHeader *>(memPtr);
   memcpy(memHead->valid, validString, 4);
   memHead->threadId = id;
   memHead->memSize = dataSize;
   memPtr = (void *)((MemoryHeader *)memPtr + 1);
   IncThreadMemCount(id, dataSize);
#endif

   return memPtr;
}
// --------------------------------------------------------------------------
void* LgsMessage::MsgData::operator new[] (size_t dataSize)
{
   void* memPtr;
   unsigned int newDataSize = dataSize;

#ifdef MEMORYCHECK
   newDataSize += sizeof(MemoryHeader);
#endif

   memPtr = malloc(newDataSize);

#ifdef MEMORYCHECK
   THREAD_ID id = GetCurrentThreadId();
   MemoryHeader* memHead = reinterpret_cast<MemoryHeader *>(memPtr);
   memcpy(memHead->valid, validString, 4);
   memHead->threadId = id;
   memHead->memSize = dataSize;
   memPtr = (void *)((MemoryHeader *)memPtr + 1);
   IncThreadMemCount(id, dataSize);
#endif

   return memPtr;
}
// --------------------------------------------------------------------------
void LgsMessage::MsgData::operator delete (void *pData)
{
   void* memPtr = pData;

#ifdef MEMORYCHECK
   MemoryHeader* memHead = reinterpret_cast<MemoryHeader *>((MemoryHeader *)memPtr - 1);
   if (memcmp(memHead->valid, validString, 4) == 0)
   {
      DecThreadMemCount(memHead->threadId, memHead->memSize);
      memPtr = (void *)((MemoryHeader *)memPtr - 1);
   }
#endif

   free(memPtr);
}
// --------------------------------------------------------------------------
void LgsMessage::MsgData::operator delete[] (void *pData)
{
   void* memPtr = pData;

#ifdef MEMORYCHECK
   MemoryHeader* memHead = reinterpret_cast<MemoryHeader *>((MemoryHeader *)memPtr - 1);
   if (memcmp(memHead->valid, validString, 4) == 0)
   {
      DecThreadMemCount(memHead->threadId, memHead->memSize);
      memPtr = (void *)((MemoryHeader *)memPtr - 1);
   }
#endif

   free(memPtr);
}
// --------------------------------------------------------------------------
void* LgsMessage::operator new (size_t dataSize)
{
   void* memPtr;
   unsigned int newDataSize = dataSize;

#ifdef MEMORYCHECK
   newDataSize += sizeof(MemoryHeader);
#endif

   memPtr = malloc(newDataSize);

#ifdef MEMORYCHECK
   THREAD_ID id = GetCurrentThreadId();
   MemoryHeader* memHead = reinterpret_cast<MemoryHeader *>(memPtr);
   memcpy(memHead->valid, validString, 4);
   memHead->threadId = id;
   memHead->memSize = dataSize;
   memPtr = (void *)((MemoryHeader *)memPtr + 1);
   IncThreadMemCount(id, dataSize);
#endif

   return memPtr;
}
// --------------------------------------------------------------------------
void* LgsMessage::operator new[] (size_t dataSize)
{
   void* memPtr;
   unsigned int newDataSize = dataSize;

#ifdef MEMORYCHECK
   newDataSize += sizeof(MemoryHeader);
#endif

   memPtr = malloc(newDataSize);

#ifdef MEMORYCHECK
   THREAD_ID id = GetCurrentThreadId();
   MemoryHeader* memHead = reinterpret_cast<MemoryHeader *>(memPtr);
   memcpy(memHead->valid, validString, 4);
   memHead->threadId = id;
   memHead->memSize = dataSize;
   memPtr = (void *)((MemoryHeader *)memPtr + 1);
   IncThreadMemCount(id, dataSize);
#endif

   return memPtr;
}
// --------------------------------------------------------------------------
void LgsMessage::operator delete (void *pData)
{
   void* memPtr = pData;

#ifdef MEMORYCHECK
   MemoryHeader* memHead = reinterpret_cast<MemoryHeader *>((MemoryHeader *)memPtr - 1);
   if (memcmp(memHead->valid, validString, 4) == 0)
   {
      DecThreadMemCount(memHead->threadId, memHead->memSize);
      memPtr = (void *)((MemoryHeader *)memPtr - 1);
   }
#endif

   free(memPtr);
}
// --------------------------------------------------------------------------
void LgsMessage::operator delete[] (void *pData)
{
   void* memPtr = pData;

#ifdef MEMORYCHECK
   MemoryHeader* memHead = reinterpret_cast<MemoryHeader *>((MemoryHeader *)memPtr - 1);
   if (memcmp(memHead->valid, validString, 4) == 0)
   {
      DecThreadMemCount(memHead->threadId, memHead->memSize);
      memPtr = (void *)((MemoryHeader *)memPtr - 1);
   }
#endif

   free(memPtr);
}

