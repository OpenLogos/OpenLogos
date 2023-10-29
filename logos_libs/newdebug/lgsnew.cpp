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
#ifdef _MSC_VER
#include <lgs_base/multiheap.h>
#include <lgs_db_io/lgsdbcommonobjects.h>

LgsMultiHeap *opMultiHeap = 0;

//This is MS-Windows  specific call and has to be replaced to get the module
// file handle for transl.exe
HMODULE translHandle = 0;
short handleInitialized = 0;
static int nprocs = -1;

#ifdef MEMORYCHECK

char validStr[5] = "!~~!";

// --------------------------------------------------------------------------
void IncThreadMemCount(DWORD id, unsigned int size)
{
   LgsMultiHeap *multiHeap = &LgsMultiHeap::singleton();
   for (int index = 0; index < multiHeap->MAXTHREAD; index++)
   {
      if (multiHeap->memCount[index].threadId == 0)
      {
         multiHeap->memCount[index].threadId = id;
         multiHeap->memCount[index].currMemSize = size;
         multiHeap->memCount[index].peakMemSize = size;
         break;
      }
      else if (multiHeap->memCount[index].threadId == id)
      {
         bool found = false;
         if (index == 2)
         {
            found = true;
         }
         multiHeap->memCount[index].currMemSize += size;
         if (multiHeap->memCount[index].currMemSize > multiHeap->memCount[index].peakMemSize)
         {
            multiHeap->memCount[index].peakMemSize = multiHeap->memCount[index].currMemSize;
         }
         break;
      }
   }
}

// --------------------------------------------------------------------------
void DecThreadMemCount(DWORD id, unsigned int size)
{
   LgsMultiHeap *multiHeap = &LgsMultiHeap::singleton();
   for (int index = 0; index < multiHeap->MAXTHREAD; index++)
   {
      if (multiHeap->memCount[index].threadId == id)
      {
         bool found = false;
         if (index == 2)
         {
            found = true;
         }
         if (multiHeap->memCount[index].currMemSize < size)
         {
            multiHeap->memCount[index].currMemSize = 0;
         }
         else
         {
            multiHeap->memCount[index].currMemSize -= size;
         }
         break;
      }
   }
}

// --------------------------------------------------------------------------
void NameThreadMem(DWORD id, const LgsString& threadName)
{
   LgsMultiHeap *multiHeap = &LgsMultiHeap::singleton();
   for (int index = 0; index < multiHeap->MAXTHREAD; index++)
   {
      if (multiHeap->memCount[index].threadId == id)
      {
         strcpy(multiHeap->memCount[index].threadName, threadName.c_str());
         break;
      }
   }
}

// --------------------------------------------------------------------------
void OutputMemoryInfo(unsigned int totPeakMem, char* threadName, unsigned int peakMem,
                      unsigned int currMem, bool printPercent)
{
	unsigned int billions;
	unsigned int millions;
	unsigned int thousands;
	unsigned int hundreds;
	long double dPeakMem = peakMem;
	long double percentMem = (dPeakMem / totPeakMem) * 100;
   unsigned int value = peakMem;

   billions = value / 1000000000;
   value -= (billions * 1000000000);
   millions = value / 1000000;
   value -= (millions * 1000000);
   thousands = value / 1000;
   hundreds = value - (thousands * 1000);

	char memInfo[90];
   sprintf(memInfo, "%25s --- ", threadName);
   cout << memInfo;
   if (billions > 0)
   {
      sprintf(memInfo, "%3u,%03u,%03u,%03u", billions, millions, thousands, hundreds);
   }
   else if (millions > 0)
   {
      sprintf(memInfo, "    %3u,%03u,%03u", millions, thousands, hundreds);
   }
   else if (thousands > 0)
   {
      sprintf(memInfo, "        %3u,%03u", thousands, hundreds);
   }
   else
   {
      sprintf(memInfo, "            %3u", hundreds);
   }
   cout << memInfo;
	if (printPercent)
	{
		sprintf(memInfo, " --- %05.02f%% --- ", percentMem);
   }
   else
   {
      sprintf(memInfo, " ---        --- ");
   }
   cout << memInfo;

   value = currMem;

   billions = value / 1000000000;
   value -= (billions * 1000000000);
   millions = value / 1000000;
   value -= (millions * 1000000);
   thousands = value / 1000;
   hundreds = value - (thousands * 1000);

   if (billions > 0)
   {
      sprintf(memInfo, "%3u,%03u,%03u,%03u", billions, millions, thousands, hundreds);
   }
   else if (millions > 0)
   {
      sprintf(memInfo, "    %3u,%03u,%03u", millions, thousands, hundreds);
   }
   else if (thousands > 0)
   {
      sprintf(memInfo, "        %3u,%03u", thousands, hundreds);
   }
   else
   {
      sprintf(memInfo, "            %3u", hundreds);
   }
   cout << memInfo;

	cout << endl;
}

// --------------------------------------------------------------------------
void OutputAllMemoryInfo()
{
   LgsMultiHeap *multiHeap = &LgsMultiHeap::singleton();
   unsigned int totalPeakMem = 0;
   unsigned int totalCurrMem = 0;
   int index;

   // Total up the values for all the threads.
   for (index = 0; index < multiHeap->MAXTHREAD; index++)
   {
      if (multiHeap->memCount[index].threadId != 0)
      {
         totalPeakMem += multiHeap->memCount[index].peakMemSize;
         totalCurrMem += multiHeap->memCount[index].currMemSize;
         if (strlen(multiHeap->memCount[index].threadName) == 0)
         {
            strcpy(multiHeap->memCount[index].threadName, "Main Thread");
         }
      }
   }

   // Output the header
   cout << endl << endl;
   cout << "PEAK MEMORY INFORMATION FOR ALL THREADS" << endl;
   cout << "(Thread Name --- Memory Peak In Bytes --- Percentage Of Total --- Memory Still Allocated)" << endl;
   for (index = 0; index < multiHeap->MAXTHREAD; index++)
   {
      if (multiHeap->memCount[index].threadId != 0)
      {
         OutputMemoryInfo(totalPeakMem, multiHeap->memCount[index].threadName,
                          multiHeap->memCount[index].peakMemSize,
                          multiHeap->memCount[index].currMemSize, true);
      }
   }
   OutputMemoryInfo(totalPeakMem, "Total Peak Memory", totalPeakMem, totalCurrMem, false);
}
#endif

// --------------------------------------------------------------------------
inline void InitHandle()
{
	if (!handleInitialized)
	{
		translHandle = GetModuleHandle("Transl.exe");
		handleInitialized = 1;
	}
}

// --------------------------------------------------------------------------
inline void InitMultiHeap(void)
{
	if (!opMultiHeap)
	{
		opMultiHeap = &LgsMultiHeap::singleton();
		SYSTEM_INFO sysinfo;
		GetSystemInfo(&sysinfo);
		nprocs = sysinfo.dwNumberOfProcessors;
	}
}
// --------------------------------------------------------------------------
void* LgsAlloc(unsigned int szRequested)
{
  unsigned int newSzRequested = szRequested;
  void* memPtr;
  InitHandle();

  if (NULL != translHandle)
    {
#ifdef MEMORYCHECK
      newSzRequested += sizeof(MemoryHeader);
#endif
      InitMultiHeap();
      if (nprocs > 1)
        memPtr = opMultiHeap->mhAlloc(newSzRequested);
      else
        memPtr = malloc(newSzRequested);
    }
  else
    {
      memPtr = malloc(newSzRequested);
    }	

#ifdef MEMORYCHECK
  if (NULL != translHandle)
    {
      DWORD id = GetCurrentThreadId();
      MemoryHeader* memHead = reinterpret_cast<MemoryHeader *>(memPtr);
      memcpy(memHead->valid, validStr, 4);
      memHead->threadId = id;
      memHead->memSize = szRequested;
      memPtr = (void *)((MemoryHeader *)memPtr + 1);
      IncThreadMemCount(id, szRequested);
    }
#endif

  return memPtr;
}

// --------------------------------------------------------------------------
void LgsFree(void *pMemory)
{
   void* memPtr = pMemory;
	InitHandle();
	if (!pMemory)
	{
		return;
	}

#ifdef MEMORYCHECK
	if (NULL != translHandle)
	{
      MemoryHeader* memHead = reinterpret_cast<MemoryHeader *>((MemoryHeader *)memPtr - 1);
      if (memcmp(memHead->valid, validStr, 4) == 0)
      {
         DecThreadMemCount(memHead->threadId, memHead->memSize);
         memPtr = (void *)((MemoryHeader *)memPtr - 1);
      }
   }
#endif

	if (NULL != translHandle)
	{
	   InitMultiHeap();
	   if (nprocs > 1)
         opMultiHeap->mhFree(memPtr);
		else
         free(memPtr);
	}
	else
	{
      free(memPtr);
	}	
}

// --------------------------------------------------------------------------
void* LgsRealloc(void *pMemory, unsigned int reallocSize)
{
   void* memPtr = pMemory;
   unsigned int newReallocSize = reallocSize;
	InitHandle();

#ifdef MEMORYCHECK
   MemoryHeader* memHead;
	if (NULL != translHandle)
	{
      memHead = reinterpret_cast<MemoryHeader *>((MemoryHeader *)memPtr - 1);
      if (memcmp(memHead->valid, validStr, 4) == 0)
      {
         DecThreadMemCount(memHead->threadId, memHead->memSize);
         memPtr = (void *)((MemoryHeader *)memPtr - 1);
      }
      newReallocSize += sizeof(MemoryHeader);
   }
#endif

	if (NULL != translHandle)
	{
		InitMultiHeap();
		if (nprocs > 1)
			memPtr = opMultiHeap->mhRealloc(memPtr, newReallocSize);
		else 
			memPtr = realloc(memPtr, newReallocSize);
	}
	else
	{
		memPtr = realloc(memPtr, newReallocSize);
	}

#ifdef MEMORYCHECK
	if (NULL != translHandle)
	{
      DWORD id = GetCurrentThreadId();
      memHead = reinterpret_cast<MemoryHeader *>(memPtr);
      memcpy(memHead->valid, validStr, 4);
      memHead->threadId = id;
      memHead->memSize = reallocSize;
      memPtr = (void *)((MemoryHeader *)memPtr + 1);
      IncThreadMemCount(id, reallocSize);
   }
#endif

   return memPtr;
}

// --------------------------------------------------------------------------
void* __cdecl operator new( unsigned int stAllocateBlock )
{
	return LgsAlloc(stAllocateBlock);
}

// --------------------------------------------------------------------------
void __cdecl operator delete( void *pvMem )
{
	LgsFree(pvMem);
}

// --------------------------------------------------------------------------
void* __cdecl operator new[](size_t size)
{
	return LgsAlloc(size);
}

// --------------------------------------------------------------------------
void __cdecl operator delete[](void* p)
{
	LgsFree(p);
}

#else

#include <cstdlib>

void *LgsAlloc(size_t size) { return malloc(size); }
void *LgsRealloc(void *ptr, size_t size) { return realloc(ptr, size); }
void LgsFree(void *ptr) { free(ptr); }

#endif

// --------------------------------------------------------------------------
extern "C"
{

void* c_LgsAlloc(unsigned int szRequested)
{
	return LgsAlloc(szRequested);
}

// --------------------------------------------------------------------------
void c_LgsFree(void *pMemory)
{
	LgsFree(pMemory);
}

}
