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
/////////////////////////////////////////////////////////////////////////
// Member function definitions for Unix specific classes (UnixQHandle &
// UnixUPipes).
//
// Author: Bernd Kiefer
//
// Use anonymous pipes for thread communication
//
// CommunicationInterface works on the following assumptions:
// 
// 1. Q handles for all threads is created by the process/thread which will
//    spawn the threads. This makes sure that no threads will call the read/
//    write functions when the Q handles are being created and the vector of 
//    Q handles is initialized.
//    No thread can read/write to the Qs before the Q handles for all the
//    threads have been created.

// 2. When a thread is terminated due to some reason, the Q handle of that
//    thread should not be deleted. Any operation that will result in the
//    modification of the vector of Q handles (thru new Q handles creation/Q
//    handle deletions) should happen only after all threads have been
//    terminated and all Q handles are no longer used. 
//
// IMPORTANT
// *********
// Modifications need to be done to solve the above issues by adding
// synchronization blocks.
//	

#include <logos_libs/multithreadlib/unixupipes.h>
#include <algorithm>

CommunicationInterface * UnixUPipes::makeCommInterface()
{
  if (!commInterface())
    {
      commInterface(new UnixUPipes);
    }
  return commInterface();
}


UnixUPipes::~UnixUPipes(void)
{
  QHandleVector & qVector = qHandleVector();
  for (QHandleVector::iterator currHandle = qVector.begin();
       currHandle != qVector.end();
       )
    {
      QHandle * handleP = *currHandle;
      // Clear All messages in the Q ; Call the virtual function to cleanup and
      // allow the virtual function to delete the pointer since it was created
      // in the Derived Class's scope. deleteQHandle() has to remove the
      // QHandle from the Q by calling removeQHandle().
      currHandle = qVector.erase(currHandle);
      //flushQMessages(handleP);
      delete handleP;
    }

}

void UnixUPipes::freeQHandle(const QHandle * iQHandle)
{
  if (iQHandle)
    {
      QHandleVector & qVector = qHandleVector();
		
      QHandleVector::iterator qIterator = 
        find(qVector.begin(), qVector.end(), iQHandle);
      QHandle * handle = *qIterator;
      qVector.erase(qIterator);
      flushQMessages(handle);
      delete handle;
    }
}

void UnixUPipes::flushQMessages(const QHandle * iQHandle)
{
  if (iQHandle) {
    UnixQHandle *unixReadHandle 
      = static_cast<UnixQHandle *>(const_cast<QHandle *>(iQHandle));
    LgsMessage *pipeMsg = 0;
    int bytesRead;

    while (unixReadHandle->qMsgCount()) {
      unixReadHandle->enterCSection();
      bytesRead = 
        read(unixReadHandle->readHandle(), &pipeMsg, sizeof(LgsMessage *));
      unixReadHandle->decMsgCount();
      unixReadHandle->leaveCSection();
      delete pipeMsg;
    }
  }
}

const QHandle * UnixUPipes::createQHandle(thread_id_t iTaskId)
{
  UnixQHandle * retVal = new UnixQHandle(iTaskId);
  QHandleVector & qVector = qHandleVector();
  qVector.push_back(retVal);
  return retVal;
}

int UnixUPipes::sendMsg(thread_id_t iTarget, LgsMessage & iMsg)
{
  return sendMsg(findQHandle(iTarget), iMsg);
}

//+-----------------lgssgml2ltx------------------------
int UnixUPipes::sendMsg(const QHandle* iWriteHandle, LgsMessage & iMsg)
{
    UnixQHandle *iUnixWriteHandle 
      = static_cast<UnixQHandle *>(const_cast<QHandle*>(iWriteHandle));
    LgsMessage *pipeMsg = new LgsMessage(iMsg);

    iUnixWriteHandle->enterCSection();
    int bytesWritten = 
      write(iUnixWriteHandle->writeHandle(), &pipeMsg, sizeof(LgsMessage *));
    if (bytesWritten == sizeof(LgsMessage *)) {  // or errno == 0??
      iUnixWriteHandle->incMsgCount();
    }

    /*
    fprintf(stderr, "Sent Message %d->%d (%d:%d|%d) over %d\n",
            iUnixWriteHandle->taskId(), iMsg.targetId(),
            iUnixWriteHandle->qMsgCount(),
            iMsg.msgType(), iMsg.msgLength(),
            (long) iUnixWriteHandle->writeHandle()); fflush(stderr);
    */        

    iUnixWriteHandle->leaveCSection();
    return bytesWritten;
}

//+-----------------lgssgml2ltx------------------------
int UnixUPipes::receiveMsg(const QHandle * iReadHandle, LgsMessage & oMsg)
{
  UnixQHandle *unixReadHandle 
    = static_cast<UnixQHandle *>(const_cast<QHandle *>(iReadHandle));
  LgsMessage *pipeMsg = 0;
  //unixReadHandle->enterCSection();
  int bytesRead = 
    read(unixReadHandle->readHandle(), &pipeMsg, sizeof(LgsMessage *));
  if (bytesRead == sizeof(LgsMessage *)) {  // or errno = 0 ??
    /*
    fprintf(stderr, "Received Message %d->%d (%d:%d|%d) over %d\n",
            unixReadHandle->taskId(), pipeMsg->targetId(),
            unixReadHandle->qMsgCount(),
            pipeMsg->msgType(), pipeMsg->msgLength(),
            (long) unixReadHandle->readHandle()); fflush(stderr);
    */
    unixReadHandle->decMsgCount();
    oMsg = (*pipeMsg);
    delete pipeMsg;
  }
  //unixReadHandle->leaveCSection();
  return bytesRead;
}

  
