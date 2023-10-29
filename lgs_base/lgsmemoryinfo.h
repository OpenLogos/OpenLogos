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
program. If not, write to Globalware AG, Hospitalstraße 6, D-99817 Eisenach.

Linux port modifications and additions by Bernd Kiefer, Walter Kasper,
Deutsches Forschungszentrum fuer kuenstliche Intelligenz (DFKI)
Stuhlsatzenhausweg 3, D-66123 Saarbruecken
*/
// lgsmemoryinfo.h: interface for the LgsMemoryInfo class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_LGSMEMORYINFO_H__2B0D24A6_4DC7_11D4_9692_00A0CC51B84B__INCLUDED_)
#define AFX_LGSMEMORYINFO_H__2B0D24A6_4DC7_11D4_9692_00A0CC51B84B__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif
#ifdef _MSC_VER
#include <psapi.h>
#endif
#include <logos_include/lgsstring.h>
#include <logos_include/lgscontainers.h>
#include <iostream>
#include <cstdio>
#include <logos_libs/multithreadlib/lgscritsect.h>

using namespace std;

#ifdef _MSC_VER
#define PROC_HANDLE HANDLE
#else
#include <sys/types.h>
#include <unistd.h>

#define PROC_HANDLE pid_t
#define GetCurrentProcess getpid
#endif

struct MemoryState
{
    LgsString m_tag;
#ifdef _MSC_VER
    PROCESS_MEMORY_COUNTERS m_counters;
#endif
    MemoryState(const char* memTag) : m_tag(memTag)
    {

    }
};

typedef LgsVector(MemoryState) MemoryStateVector;
typedef MemoryStateVector::iterator MemoryStateIterator;
typedef MemoryStateVector::const_iterator MemoryStateConstIterator;


class LgsMemoryInfo  
{
public:
    static LgsMemoryInfo& singleton(void);
    static void setMemoryFlag(bool flagValue);
    virtual ~LgsMemoryInfo();

    void addMemoryState(const char* memTag);
    void dumpMemoryStates(ostream &outStream);

private:
	LgsMemoryInfo(PROC_HANDLE procHandle);
    static LgsMemoryInfo m_memInfo;
    static bool m_memFlag;
    PROC_HANDLE m_procHandle;
    MemoryStateVector m_memStates;
    LgsCriticalSection m_critSection;
};

#endif // !defined(AFX_LGSMEMORYINFO_H__2B0D24A6_4DC7_11D4_9692_00A0CC51B84B__INCLUDED_)
