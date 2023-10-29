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
// lgsmemoryinfo.cpp: implementation of the LgsMemoryInfo class.
//
//////////////////////////////////////////////////////////////////////

#include "lgsmemoryinfo.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

extern "C"
{

void addMemoryState(const char* memTag)
{
    LgsMemoryInfo::singleton().addMemoryState(memTag);
}

}


LgsMemoryInfo LgsMemoryInfo::m_memInfo(GetCurrentProcess());
bool LgsMemoryInfo::m_memFlag = true;

LgsMemoryInfo::LgsMemoryInfo(PROC_HANDLE procHandle) : m_procHandle(procHandle)
{
    m_critSection.initialize();
}

LgsMemoryInfo& LgsMemoryInfo::singleton(void)
{
    return m_memInfo;   
}

void LgsMemoryInfo::setMemoryFlag(bool flagValue)
{
    m_memFlag = flagValue;
}


LgsMemoryInfo::~LgsMemoryInfo()
{
    m_memStates.clear();
}

void LgsMemoryInfo::addMemoryState(const char* memTag)
{
    if (m_memFlag)
    {
        m_critSection.enter();
        MemoryState memState(memTag);
#ifdef _MSC_VER
        GetProcessMemoryInfo(m_procHandle, &memState.m_counters, sizeof(memState.m_counters));
#endif
        m_memStates.push_back(memState);
        m_critSection.leave();
    }
}

void LgsMemoryInfo::dumpMemoryStates(ostream &outStream)
{
    if (m_memFlag)
    {
        unsigned long startMem;
        unsigned long endMem;
        unsigned long maxMem;
        outStream << "TRANSL PROCESS MEMORY INFORMATION\n\n";
        outStream << "PWSS   -  Peak Working Set Size\n";
        outStream << "WSS    -  Working Set Size\n";
        outStream << "QPPPU  -  Quota Peak Paged Pool Usage\n";
        outStream << "QPPU   -  Quota Paged Pool Usage\n";
        outStream << "QPNPPU -  Quota Peak Non Paged Pool Usage\n";
        outStream << "QNPPU  -  Quota Non Paged Pool Usage\n";
        outStream << "PFU    -  Page File Usage\n";
        outStream << "PPFU   -  Peak Page File Usage\n";
        outStream << "Memory Size specified in Kilo Bytes\n";
        outStream << '\n';

        outStream << "    PWSS       WSS     QPPPU      QPPU    QPNPPU     QNPPU       PFU      PPFU\n";

        MemoryStateIterator endIter = m_memStates.end();
        for (MemoryStateIterator memState = m_memStates.begin(); memState != endIter; memState++)
        {
            char memInfo[100];
#ifdef _MSC_VER
            sprintf(memInfo, "%8u  %8u  %8u  %8u  %8u  %8u  %8u  %8u ", 
                         (*memState).m_counters.PeakWorkingSetSize/1024, 
                         (*memState).m_counters.WorkingSetSize/1024,
                         (*memState).m_counters.QuotaPeakPagedPoolUsage/1024,
                         (*memState).m_counters.QuotaPagedPoolUsage/1024,
                         (*memState).m_counters.QuotaPeakNonPagedPoolUsage/1024,
                         (*memState).m_counters.QuotaNonPagedPoolUsage/1024,
                         (*memState).m_counters.PagefileUsage/1024,
                         (*memState).m_counters.PeakPagefileUsage/1024);
#else
            memInfo[0] = '\0';
#endif
            outStream << memInfo;
            outStream << (*memState).m_tag.c_str();
            outStream << '\n';
#ifdef _MSC_VER
            if (memState == m_memStates.begin())
            {
                startMem = (*memState).m_counters.WorkingSetSize+(*memState).m_counters.PagefileUsage;
            }
            if (memState == endIter-1)
            {
                endMem = (*memState).m_counters.WorkingSetSize+(*memState).m_counters.PagefileUsage;
                maxMem = (*memState).m_counters.PeakWorkingSetSize+(*memState).m_counters.PeakPagefileUsage;
            }
#endif
        }
        char msgBuffer[100];
        outStream << '\n';
        outStream << '\n';
        sprintf(msgBuffer, "%s%9u\n", "Memory at start of program (KB):  ", startMem/1024);
        outStream << msgBuffer;
        sprintf(msgBuffer, "%s%9u\n", "Memory at end of program (KB):    ", endMem/1024);
        outStream << msgBuffer;
        sprintf(msgBuffer, "%s%9u\n", "Max Memory Usage (KB):            ", maxMem/1024);
        outStream << msgBuffer;
        outStream << '\n';
        outStream << '\n';

    }

}

