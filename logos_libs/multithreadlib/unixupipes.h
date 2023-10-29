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

#ifndef _UNIXUPIPES_H
#define _UNIXUPIPES_H

#include <logos_libs/multithreadlib/comminterface.h>
#include <logos_libs/multithreadlib/lgscritsect.h>

#include <unistd.h>

// Unnamed pipes of ipc
class UnixUPipes : public CommunicationInterface
{
  UnixUPipes(void) {}
  class UnixQHandle : public QHandle
  {
    int _filedes[2];
    short _qMsgCount;
    LgsCriticalSection _critSection;
  public:
    UnixQHandle(thread_id_t iTask): QHandle(iTask) , _qMsgCount(0) {
      _critSection.initialize();
      // open anonymous pipe
      if (pipe(_filedes) == -1) {
        _filedes[0] = _filedes[1] = -1;
      }
    }
    ~UnixQHandle(void) {
      // close the pipe and reset file descriptors
      close(_filedes[0]) ;
      close(_filedes[1]) ;
      _filedes[0] = _filedes[1] = -1;
    }
    int readHandle(void) const { return _filedes[0]; }
    int writeHandle(void) const { return _filedes[1]; }
    short qMsgCount(void) const { return _qMsgCount; }
    void incMsgCount(void) { _qMsgCount++; }
    void decMsgCount(void) { _qMsgCount--; }
    void enterCSection(void) { _critSection.enter(); }
    void leaveCSection(void) { _critSection.leave(); }
  };

public:
  virtual ~UnixUPipes(void);
  static CommunicationInterface * makeCommInterface();
  const QHandle * createQHandle(thread_id_t iTaskId);
  int sendMsg(thread_id_t iTarget, LgsMessage & iMsg);
  //------------------------lgssgml2ltx------------------------
  int sendMsg(const QHandle* iWriteHandle, LgsMessage & iMsg);
  //------------------------lgssgml2ltx------------------------
  int receiveMsg(const QHandle * iReadHandle, LgsMessage & oMsg);
  void freeQHandle(const QHandle * iQHandle);
  void flushQMessages(const QHandle * iQHandle);

};
#endif
