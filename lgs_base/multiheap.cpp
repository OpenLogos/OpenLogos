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
#ifdef _MSC_VER
#include <lgs_base/multiheap.h>
#include <iostream>
using namespace std;

LgsMultiHeap LgsMultiHeap::m_heap;
DWORD LgsMultiHeap::m_flOptions = 0;
DWORD LgsMultiHeap::m_dwInitialSize = 0;
DWORD LgsMultiHeap::m_dwParallelism = 8;

LgsMultiHeap::LgsMultiHeap(void): m_bInitialized(false), m_heapHandle(NULL)
{
   for (int index = 0; index < MAXTHREAD; index++)
   {
      memCount[index].currMemSize = 0;
      memCount[index].peakMemSize = 0;
      memCount[index].threadId = 0;
      memset(memCount[index].threadName, '\0', 21);
   }
}

void LgsMultiHeap::setHeapProperties(DWORD flOptions, DWORD dwInitialSize, DWORD dwParallelism)
{
	m_flOptions = flOptions;
	m_dwInitialSize = dwInitialSize;
	m_dwParallelism = dwParallelism;
}

LgsMultiHeap::~LgsMultiHeap(void)
{
	if (NULL != m_heapHandle)
	{
		MpHeapDestroy(m_heapHandle);
	}
}

LgsMultiHeap& LgsMultiHeap::singleton(void)
{
	if (!m_heap.m_bInitialized)
	{
		m_heap.m_heapHandle = MpHeapCreate(m_flOptions, m_dwInitialSize, m_dwParallelism);
		m_heap.m_bInitialized = true;
	}
	return m_heap;
}

extern "C"
{

void* mh_alloc(unsigned int szRequested)
{
	return LgsMultiHeap::singleton().mhAlloc(szRequested);
}

void* mh_realloc(void *pMemory, unsigned int newSize)
{
	return LgsMultiHeap::singleton().mhRealloc(pMemory, newSize);
}

void mh_free(void *pMemory)
{
	LgsMultiHeap::singleton().mhFree(pMemory);	
}

}
#else

#include <malloc.h>

extern "C"
{

void* mh_alloc(unsigned int szRequested)
{
	return malloc(szRequested);
}

void* mh_realloc(void *pMemory, unsigned int newSize)
{
	return realloc(pMemory, newSize);
}

void mh_free(void *pMemory)
{
	free(pMemory);	
}

}
#endif
