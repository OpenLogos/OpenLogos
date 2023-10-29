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

#ifndef _UNIXCRITSECT_H
#define _UNIXCRITSECT_H

//#include <logos_libs/multithreadlib/critsection.h>
#include <pthread.h>

class UnixCriticalSection {
private:
  pthread_mutex_t _mutex;

public:
  UnixCriticalSection(void) { initialize() ; }

  ~UnixCriticalSection(void) {
    pthread_mutex_destroy(& _mutex);
  }

  void initialize(void){
    //pthread_mutex_t new_mut = PTHREAD_MUTEX_INITIALIZER;
    //_mutex = new_mut;

    // This comes from 
    // http://www.codeguru.com/forum/archive/index.php/t-229072.html
    // The default linux mutexes are of the "fast" kind, which means that a
    // thread that tries to lock a mutex it itself owns will be suspended.  see
    // man pthread_mutexattr_init(2) for more information.
    // Seemingly Win mutexes have the "recursive" behaviour.

    pthread_mutexattr_t attr;
    pthread_mutexattr_init( &attr );
    pthread_mutexattr_settype( &attr, PTHREAD_MUTEX_RECURSIVE );
    
    pthread_mutex_init( &_mutex, &attr );
    
    pthread_mutexattr_destroy( &attr );
  }

  void enter(void) {
    pthread_mutex_lock(& _mutex);
  }

  void leave(void) {
    pthread_mutex_unlock(& _mutex);
  }
};

#endif
