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
#ifndef _BASETHREAD_H_
#define _BASETHREAD_H_

#include <logos_include/lgsstring.h>
using namespace std;

typedef short thread_id_t;
#define INVALID_THREAD_ID -1

class Thread
{
protected:
	Thread(void);
	static short _threadCounter;
	LgsString _threadName;
	thread_id_t _threadId;
	short _restartCount;
        bool _timeFlag;
	unsigned long _execTime;

public:
	enum ExitStatus
	{
		StillActive,
		NormalExit,
		Error
	};
	static short threadCount(void);
	Thread(const LgsString & iThreadName, bool timeFlag);
	virtual ~Thread(void);
	const LgsString & threadName(void) const;
	unsigned long executionTime(void) const;
	thread_id_t threadId(void) const;
	void incRestartCount(void);
	short restartCount(void) const;
	virtual void start(void) = 0;
	virtual void run(void) = 0;
	virtual void cleanup(void) = 0;
};
// -------------------------------------------------------------------
inline Thread::~Thread(void)
{
}
// -------------------------------------------------------------------
inline short Thread::threadCount(void)
{
	return _threadCounter;	
}
// -------------------------------------------------------------------
inline const LgsString & Thread::threadName(void) const
{
	return _threadName;
}
// -------------------------------------------------------------------
inline unsigned long Thread::executionTime(void) const
{
	return _execTime;
}
// -------------------------------------------------------------------
inline thread_id_t Thread::threadId(void) const
{
	return _threadId;
}
// -------------------------------------------------------------------
inline short Thread::restartCount(void) const
{
	return _restartCount;
}
// -------------------------------------------------------------------
inline void Thread::incRestartCount(void) 
{
	_restartCount++;
}

#endif
