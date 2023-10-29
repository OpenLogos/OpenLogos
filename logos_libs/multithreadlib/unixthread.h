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
/* -*- Mode: C++ -*- */

#ifndef _UNIXTHREAD_H
#define _UNIXTHREAD_H

using namespace std;
#include <logos_libs/multithreadlib/basethread.h>

#include <pthread.h>
#include <queue>

typedef LGSTHREADLISTIMPL(class UnixThread *) LgsThreadList;

class UnixThread : public Thread
{
private:
  /**  A queue of terminated (by exit, cancellation, or return) threads */
  static queue<pthread_t> __zombies;
  /** to avoid two threads storing into the queue simultaneously */
  static pthread_mutex_t __zombies_mutex;
 
  /** mutex for __terminated_condition */
  static pthread_mutex_t __term_cond_mutex; 
  /** signal the controlling thread a thread exit */
  static pthread_cond_t __terminated_condition; 
  
  /** A wrapper around the run function to install a cleanup function that
   *  singals the termination of the thread an other things.
   */
  static void * entryProc(void *objPtr);

  /** _threadHandle is only used internally in the thread lib. IDs given to
   *  the outside world (also to the communication interface are always the
   *   home-brewn _threadId 
   */
  pthread_t _threadHandle;

  static void unixThreadTerminates(void *ignored);

public:
  UnixThread(const LgsString & iThreadName, bool timeFlag) 
    : Thread(iThreadName, timeFlag) {}
  ~UnixThread(void) { }
  void start(void);

  static LgsThreadList::iterator 
  WaitForThread(LgsThreadList & threadList, long milliSeconds,
                ExitStatus & status);
  static void ExitThread(int retval);

  static void initialize() {
    pthread_mutex_init(& __zombies_mutex, NULL);
    pthread_mutex_init(& __term_cond_mutex, NULL);
    pthread_cond_init(& __terminated_condition, NULL);
  }
};

#endif
