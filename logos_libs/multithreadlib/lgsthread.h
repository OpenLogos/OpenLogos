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
/////////////////////////////////////////////////////////////////////////////
//	Platform specific typedefs for platform specific Thread.
//
//	Author: Sundar Ramaswamy; Date: 5/98
//

#ifndef _LGSTHREAD_H_
#define _LGSTHREAD_H_

#include <logos_include/lgscontainers.h>

#define LGSTHREADLISTIMPL LgsVector

// To wait infinitely long for a thread to terminate
extern const long Infinite;

#ifdef _MSC_VER

#include <logos_libs/multithreadlib/win32thread.h>
typedef Win32Thread LgsThread;

#else

#include <logos_libs/multithreadlib/unixthread.h>
typedef UnixThread LgsThread;

#endif

//////////////////////////////////////////////////////////////////////////////
// API call Interface
// waitForThread(LgsThreadList & threadList, long milliSeconds, 
//				 LgsThread::ExitStatus & status)
// Description: waits for any thread in the threadList to terminate and upon
//	        termination returns the thread that terminated with the status
//		value for the Thread termination
// Returns:     LgsThreadList::iterator for the thread that terminated and
//              status is filled up with the termination status of the thread
//              (NormalExit/Error) 
//
//		

#define waitForThread LgsThread::WaitForThread
#define exitThread LgsThread::ExitThread

// this must be defined in the implementation specific headers using 
// LGSTHREADLISTIMPL
// typedef LgsVector(LgsThread*) LgsThreadList;

inline short getThreadId(const LgsThreadList & iList, const char * iName)
{
  short threadId = 0xFF;
  LgsThreadList::const_iterator endIter = iList.end();
  for (LgsThreadList::const_iterator currThread = iList.begin(); 
       currThread != endIter && threadId == 0xFF;
       currThread++)
    {
      if ((*currThread)->threadName()==iName)
        {
          threadId = (*currThread)->threadId();
        }
    }
  return threadId;
}

#endif
