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
#ifndef _MULTIHEAP_H_
#define _MULTIHEAP_H_
#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif
extern "C"
{
#include <tmpheap/mpheap.h>
}

struct ThreadMemoryCount
{
   DWORD threadId;
   unsigned int currMemSize;
   unsigned int peakMemSize;
   char threadName[21];
};

struct MemoryHeader
{
   char valid[4];    // If valid header then the string "!~~!" resides here
   DWORD threadId;
   unsigned int memSize;
};

class LgsMultiHeap
{
public:
   enum { MAXTHREAD = 14 };
	static LgsMultiHeap& singleton(void);
	static void setHeapProperties(DWORD flOptions, DWORD dwInitialSize, DWORD dwParallelism);
	~LgsMultiHeap(void);
	void* mhAlloc(unsigned int szRequested);
	void mhFree(void *pMemory);
	void* mhRealloc(void *pMemory, unsigned int newSize);
   ThreadMemoryCount memCount[MAXTHREAD];

private:
	LgsMultiHeap(void);
	static LgsMultiHeap m_heap;
	bool m_bInitialized;
	HANDLE m_heapHandle;
	static DWORD m_flOptions;
	static DWORD m_dwInitialSize;
	static DWORD m_dwParallelism;
};

inline void* LgsMultiHeap::mhAlloc(unsigned int szRequested)
{
	return(MpHeapAlloc(m_heapHandle, 0, szRequested));
}

inline void LgsMultiHeap::mhFree(void *pMemory)
{
	MpHeapFree(m_heapHandle, pMemory);
}

inline void* LgsMultiHeap::mhRealloc(void *pMemory, unsigned int newSize)
{
	return MpHeapReAlloc(m_heapHandle, pMemory, newSize);
}

#endif
