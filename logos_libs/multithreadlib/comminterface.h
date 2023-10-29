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
///////////////////////////////////////////////////////////////////////////////
//	Interface specification for CommunicationInterface which forms the basis
//	for communication between various tasks. Pure virtual functions have to be
//	defined in the derived classes to actually send a message over any medium
//	Also defines the LgsMessage class which is used to carry information between 
//	tasks. Communication interface is a singleton class.
//
//	Author: Sundar Ramaswamy; Date: 5/98
//
//	   
#ifndef _COMMINTERFACE_H_
#define _COMMINTERFACE_H_

#include <logos_libs/multithreadlib/lgscritsect.h>
#include <logos_libs/multithreadlib/lgsthread.h>

#ifdef _MSC_VER
#include <windows.h>
typedef DWORD THREAD_ID;
#else
#include <pthread.h>
typedef pthread_t THREAD_ID;
#define GetCurrentThreadId pthread_self
#endif

enum MsgType 
{
	EOD = 0x9F,
   SplitterMsg,
	LookupMsg,
	ResMsg,
	Parse1Msg,
	Parse2Msg,
	Parse3Msg,
	Parse4Msg,
	Tran1Msg,
	Tran2Msg,
	Tran3Msg,
	Tran4Msg,
   GenerateMsg,
	UntranslatedLookupMsg,
   LgsSgmlFormat,
	CLOSEFILE
};


class LgsMessage
{
public:
   class MsgData
   {
   private:
      struct MsgHeader
      {
         /** The _sourceId is not really needed, except for the memory check.
             Under Linux, it is set to the pthread_t id of the sending thread.
          */
         THREAD_ID _sourceId;
         thread_id_t _targetId;
         short _msgType;
         int _msgLength;
      };
      unsigned char _refCount;
      LgsCriticalSection _dataCritSect;
      char *_dataPtr;
      /** The _sourceThreadId is not really needed, except for the memory
          check.
          Under Linux, it is set to the pthread_t id of the sending thread.
      */
      THREAD_ID _sourceThreadId;
      MsgData(void);

   public:
      MsgData(short iType, int iLength,
              thread_id_t iSource, thread_id_t iTarget);
      ~MsgData(void);

      void* operator new (size_t dataSize);
      void operator delete (void* pData);
      void* operator new[] (size_t dataSize);
      void operator delete[] (void* pData);

      void enterCriticalSection(void);
      void leaveCriticalSection(void);
      void incRefCount(void);
      void decRefCount(void);
      unsigned char refCount(void) const;
      char * dataPtr(void) const;
      char * startOfMsg(void) const;
      THREAD_ID sourceId(void) const;
      thread_id_t targetId(void) const;
      short msgType(void) const;
      int msgLength(void) const;
   };
   const short MultiTarget;
   MsgData *_commData;
   thread_id_t _sourceThreadId;

   void linkData(MsgData * iLnkData);

public:
   void* operator new (size_t dataSize);
   void operator delete (void* pData);
   void* operator new[] (size_t dataSize);
   void operator delete[] (void* pData);

   char * dataPtr(void) const;
   char * startOfMsg(void) const;
   const LgsMessage & operator=(const LgsMessage & rhs);
   LgsMessage(const LgsMessage & iCopy);
   LgsMessage(short iType, int iLength, thread_id_t iSource, thread_id_t iTarget);
   LgsMessage(void);
   ~LgsMessage(void);
   virtual THREAD_ID sourceId(void) const;
   virtual thread_id_t targetId(void) const;
   virtual short msgType(void) const;
   virtual int msgLength(void) const;
};
// --------------------------------------------------------------------------
inline char * LgsMessage::MsgData::dataPtr(void) const
{
	return (_dataPtr+sizeof(MsgHeader));
}
// --------------------------------------------------------------------------
inline char * LgsMessage::MsgData::startOfMsg(void) const
{
	return _dataPtr;
}
// --------------------------------------------------------------------------
inline void LgsMessage::MsgData::incRefCount(void)
{
   _refCount++;
}
// --------------------------------------------------------------------------
inline void LgsMessage::MsgData::decRefCount(void)
{
   _refCount--;
}
// --------------------------------------------------------------------------
inline unsigned char LgsMessage::MsgData::refCount(void) const
{
   return _refCount;
}
// --------------------------------------------------------------------------
inline THREAD_ID LgsMessage::MsgData::sourceId(void) const
{
  MsgHeader *headerPtr = reinterpret_cast<MsgHeader *>(_dataPtr);
  if (headerPtr)
    {
      return headerPtr->_sourceId;
    }
  else
    {
      return _sourceThreadId;
    }
}
// --------------------------------------------------------------------------
inline thread_id_t LgsMessage::MsgData::targetId(void) const
{
	MsgHeader *headerPtr = reinterpret_cast<MsgHeader *>(_dataPtr);
	return headerPtr->_targetId;
}
// --------------------------------------------------------------------------
inline short LgsMessage::MsgData::msgType(void) const
{
	MsgHeader *headerPtr = reinterpret_cast<MsgHeader *>(_dataPtr);
   return headerPtr->_msgType;
}
// --------------------------------------------------------------------------
inline int LgsMessage::MsgData::msgLength(void) const
{
	MsgHeader *headerPtr = reinterpret_cast<MsgHeader *>(_dataPtr);
	return headerPtr->_msgLength;
}
// --------------------------------------------------------------------------
inline void LgsMessage::MsgData::enterCriticalSection(void)
{
	_dataCritSect.enter();
}
// --------------------------------------------------------------------------
inline void LgsMessage::MsgData::leaveCriticalSection(void)
{
	_dataCritSect.leave();
}
// --------------------------------------------------------------------------
inline char * LgsMessage::dataPtr(void) const
{
	return (_commData->dataPtr());
}

inline char * LgsMessage::startOfMsg(void) const
{
	return (_commData->startOfMsg());
}
// --------------------------------------------------------------------------
inline const LgsMessage & LgsMessage::operator=(const LgsMessage & rhs)
{
	if (this == &rhs)
	{
		return *this;
	}
	linkData(rhs._commData);
	return *this;
}
// --------------------------------------------------------------------------
inline void LgsMessage::linkData(MsgData * iLnkData)
{
	if (_commData)
	{
		_commData->enterCriticalSection();
		_commData->decRefCount();
		if (!_commData->refCount())
		{
			_commData->leaveCriticalSection();
			delete _commData;
			_commData = 0;
		}
		else 
		{
			_commData->leaveCriticalSection();
		}
	}
	iLnkData->enterCriticalSection();
   iLnkData->incRefCount();
	_commData = iLnkData;
   _sourceThreadId = _commData->sourceId();
	iLnkData->leaveCriticalSection();
}
// --------------------------------------------------------------------------
inline LgsMessage::LgsMessage(const LgsMessage & iCopy)
                  :MultiTarget(0xFF)
{
	_commData = 0;
	linkData(iCopy._commData);
}
// --------------------------------------------------------------------------
inline LgsMessage::LgsMessage(short iType, int iLength,
                              thread_id_t iSource, thread_id_t iTarget)
  : MultiTarget(0xFF)
{
  _commData = new MsgData(iType, iLength, iSource, iTarget);
  _sourceThreadId = _commData->sourceId();
}
// --------------------------------------------------------------------------
inline LgsMessage::LgsMessage(void)
                  :_commData(0),
                   MultiTarget(0xFF)
{
}
// --------------------------------------------------------------------------
inline LgsMessage::~LgsMessage(void)
{
	if (_commData)
	{
		_commData->enterCriticalSection();
		_commData->decRefCount();
		if (!_commData->refCount())
		{
			_commData->leaveCriticalSection();
			delete _commData;
			_commData = 0;
		}
		else
		{
			_commData->leaveCriticalSection();
		}
	}
}
// --------------------------------------------------------------------------
inline THREAD_ID LgsMessage::sourceId(void) const
{
  thread_id_t retVal = INVALID_THREAD_ID;
  if (_commData)
    {
      retVal = _commData->sourceId();
    }
  else
    {
      retVal = _sourceThreadId;
    }
  return retVal;
}
// --------------------------------------------------------------------------
inline thread_id_t LgsMessage::targetId(void) const
{
  thread_id_t retVal = INVALID_THREAD_ID;
  if (_commData)
    {
      retVal = _commData->targetId();
    }
  return retVal;
}
// --------------------------------------------------------------------------
inline short LgsMessage::msgType(void) const
{
	short retVal = -1;
	if (_commData)
	{
		retVal = _commData->msgType();
	}
	return retVal;
}
// --------------------------------------------------------------------------
inline int LgsMessage::msgLength(void) const
{
	short retVal = -1;
	if (_commData)
	{
		retVal = _commData->msgLength();
	}
	return retVal;
}
// --------------------------------------------------------------------------
class QHandle
{
	thread_id_t _taskId;

	// Assignment operator purposely made private to disallow
	// assignments of this class
	const QHandle & operator=(const QHandle & rhs);
	QHandle(void);
	QHandle(const QHandle & iCopy);

public:
	QHandle(thread_id_t iTask);
	virtual ~QHandle(void);
	thread_id_t taskId(void) const;
	bool operator==(const QHandle & rhs) const;
};
// --------------------------------------------------------------------------
inline QHandle::QHandle(thread_id_t iTask) : _taskId(iTask)
{
}
// --------------------------------------------------------------------------
inline QHandle::QHandle(void)
{
}
// --------------------------------------------------------------------------
inline QHandle::QHandle(const QHandle & iCopy)
{
}
// --------------------------------------------------------------------------
inline QHandle::~QHandle(void)
{
}
// --------------------------------------------------------------------------
inline thread_id_t QHandle::taskId(void) const
{
	return _taskId;
}
// --------------------------------------------------------------------------
inline bool QHandle::operator==(const QHandle & rhs) const
{
  return (_taskId == rhs._taskId);
}
// --------------------------------------------------------------------------
inline const QHandle & QHandle::operator=(const QHandle & rhs)
{
  return (*this);
}
// --------------------------------------------------------------------------
typedef LgsVector(QHandle *) QHandleVector;
// --------------------------------------------------------------------------
class CommunicationInterface
{
 protected:
  CommunicationInterface(void);
  static void commInterface(CommunicationInterface * iInterface);
  static CommunicationInterface * commInterface(void);
  QHandle * findQHandle(thread_id_t iTask);
  QHandleVector & qHandleVector(void);
 private:
  static CommunicationInterface *_commInterface;
  QHandleVector _qHandles;

 public:
  virtual ~CommunicationInterface(void);
  static CommunicationInterface & singleton(void);
  static void destroySingleton(void);

  // returns -1, if invalid / not created
  // else returns number of bytes sent
  virtual int sendMsg(thread_id_t iTarget, LgsMessage & iMsg) = 0;
  virtual int sendMsg(const QHandle* iWriteHandle, LgsMessage & iMsg) = 0;
  virtual int receiveMsg(const QHandle * iReadHandle, LgsMessage & oMsg) = 0;
  virtual void freeQHandle(const QHandle * iQHandle) = 0;
  virtual void flushQMessages(const QHandle * iQHandle) = 0;
  virtual const QHandle * createQHandle(short iTaskId) = 0;
};
// --------------------------------------------------------------------------
inline void CommunicationInterface::commInterface(CommunicationInterface * iInterface)
{
	_commInterface = iInterface;
}
// --------------------------------------------------------------------------
inline CommunicationInterface * CommunicationInterface::commInterface(void)
{
	return _commInterface;
}
// --------------------------------------------------------------------------
inline CommunicationInterface::CommunicationInterface(void)
{
}
// --------------------------------------------------------------------------
inline CommunicationInterface::~CommunicationInterface(void)
{
}
// --------------------------------------------------------------------------
inline QHandleVector & CommunicationInterface::qHandleVector(void)
{
	return _qHandles;
}
// --------------------------------------------------------------------------
inline QHandle * CommunicationInterface::findQHandle(thread_id_t iTask)
{
  QHandle * retVal = 0;

  QHandleVector::iterator endIter = _qHandles.end();
  for (QHandleVector::iterator currIter = _qHandles.begin();
       (currIter != endIter) && (NULL == retVal); currIter++)
    {
      if ((*currIter)->taskId() == iTask)
        {
          retVal = *currIter;
        }
    }
  return retVal;
}

#endif
      
