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
/* file: unixthread.cpp 
 * Author: Bernd Kiefer, DFKI 08/2005
 */

#include "lgsthread.h"
#include <lgs_db_io/lgsdbcommonobjects.h>

#include <time.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <errno.h>

#include <cassert>

// All globals used for multithreading are defined in this file. Therefore,
// this library must reside in the library of the controlling thread. This is
// necessary because otherwise, all dynamic libraries create their own copy of
// this data, although the linker will only use one of them. This becomes
// critical when the global data structures are destroyed, since destruction
// will then be carried out multiple times on an already destroyed object
// Alternatively, a dynamic library can be used that is also loaded only once.
queue<pthread_t> UnixThread::__zombies;
pthread_mutex_t UnixThread::__zombies_mutex;
pthread_mutex_t UnixThread::__term_cond_mutex; 
pthread_cond_t UnixThread::__terminated_condition; 

const long Infinite = -1;

/** This function must be called from EVERY terminating thread.
  For threads that call pthread_exit() or are cancelled, this is guaranteed by
  adding it to the cleanup functions.
  Threads whose function simply returns will return to entryProc, which will 
  then call unixThreadTerminates()
*/
void UnixThread::unixThreadTerminates(void *ignored) {
  //fprintf(stderr, "Cleaning up thread %u\n", pthread_self()); fflush(stderr);
  pthread_mutex_lock(&__zombies_mutex);
  __zombies.push(pthread_self());
  pthread_cond_broadcast(&__terminated_condition);
  pthread_mutex_unlock(&__zombies_mutex);
}


void *UnixThread::entryProc(void *arg) {
  UnixThread *thisthread = (UnixThread *) arg;
  /*
  fprintf(stderr, "thread entry %s (%u,%u)\n",
          thisthread->threadName().c_str(),
          thisthread->_threadHandle, thisthread->_threadId);
  fflush(stderr);
  */
  pthread_cleanup_push(UnixThread::unixThreadTerminates, NULL);
  if (//LgsDBCommonObjects::GetJobControlArguments().TimeFlag()
      thisthread->_timeFlag) { }
    // _fix_me_ start timer
  thisthread->run();
  if (//LgsDBCommonObjects::GetJobControlArguments().TimeFlag()
      thisthread->_timeFlag) { }
  // _fix_me_ stop timer

  // This also executes unixThreadTerminates()
  pthread_cleanup_pop(1);
  return NULL;
}


/** This implementation of WaitForThread relies on the condition that EVERY
 thread the function waits for will signal its exit (also in the case of a
 regular return) with unixThreadTerminates(), which pushes the thread id
 onto the __zombies stack and signals termination with the
 __terminaded_condition variable
*/
LgsThreadList::iterator 
UnixThread::WaitForThread(LgsThreadList & threadList, long milliSeconds,
                          ExitStatus & status)
{
  LgsThreadList::iterator retval = threadList.end();
  int retcode;

  if (__zombies.empty()) {
    pthread_mutex_lock(&__term_cond_mutex);
    if (Infinite == milliSeconds) {
      //fprintf(stderr,"Now waiting for terminating threads\n");fflush(stderr);
      retcode = pthread_cond_wait(&__terminated_condition,
                                  &__term_cond_mutex);
      //fprintf(stderr, "... a thread terminated\n");fflush(stderr);
    }
    else {
      // compute timeout to be milliSeconds milliseconds from now
      struct timeval now;
      struct timespec timeout;
      gettimeofday(&now, NULL);
      timeout.tv_sec = now.tv_sec + (int)(milliSeconds / 1000);
      long usec = now.tv_usec + (1000 * (int)(milliSeconds % 1000));
      if (usec > 1000000) {
        timeout.tv_sec++;
        usec -= 1000000;
      }
      timeout.tv_nsec = 1000 * usec;
      retcode 
        = pthread_cond_timedwait(&__terminated_condition,
                                 &__term_cond_mutex,
                                 &timeout);
    }
    pthread_mutex_unlock(&__term_cond_mutex);
  }
  if (retcode == ETIMEDOUT) {
    // no thread terminated in time
    assert(__zombies.empty());
    return retval;
  }

  pthread_mutex_lock(&__zombies_mutex);
  pthread_t term_thread = __zombies.front();
  __zombies.pop();
  pthread_mutex_unlock(&__zombies_mutex);

  void *thread_retval;
  pthread_join(term_thread, &thread_retval);

  if (((int) thread_retval) != 0)
    status = LgsThread::Error;
  else
    status = LgsThread::NormalExit;

  LgsThreadList::iterator currThread = threadList.begin();
  while (currThread != threadList.end()
         && (*currThread)->_threadHandle != term_thread)
    currThread++;

  // Abuse the __zombies_mutex to synchronize output to stderr
  /*
  pthread_mutex_lock(&__zombies_mutex);
  fprintf(stderr, "Thread %s (%u) terminated returning %u\n",
          (*currThread)->threadName().c_str(), term_thread,
          (int) thread_retval);
  pthread_mutex_unlock(&__zombies_mutex);
  */
  assert(currThread != threadList.end());
  return currThread;
}

void UnixThread::start(void) {
  // Do we want to create the threads joinable (PTHREAD_CREATE_JOINABLE)??
  // It's the default anyway
  //fprintf(stderr, "Create Thread %s\n", this->threadName().c_str());
  int status = pthread_create( &_threadHandle, NULL, entryProc, this);
  //fprintf(stderr, "Thread %s Created\n", this->threadName().c_str());
  // _fix_me_ we should throw an exception here in case status is non-zero
}

void UnixThread::ExitThread(int retval) {
  /*fprintf(stderr, "Thread %u calls pthread_exit with retval %u\n"
    , pthread_self(), retval); fflush(stderr);*/
  pthread_exit((void *) retval);
}
