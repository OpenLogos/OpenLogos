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
#include <logos_include/logoscommon.h>
#include <time.h>
#include <logos_libs/gerdem/targetunitconcretebuilder.h>
#include <logos_libs/gerdem/gfactory.h>
#include <logos_libs/utility/argumentexception.h>
#include <logos_libs/linguistic/ldocument.h>
#include <logos_libs/regex/regex.h>
#include <transl/translthrman.h>
#include <logos_libs/multithreadlib/lgsthread.h>
#include <algorithm>
#include <logos_libs/dbms2restran/gettargetcodes.h>
#include <logos_libs/translutility/translcommonobjects.h>
#include <lgs_db_io/lgsdbcommonobjects.h>
#include <configdatafileinterface/configdatainterfacemain.h>
#include <logos_libs/lgssgml/lgsmerger.h>
#include <logos_libs/lgssgml/lgssplitter.h>

bool TranslThreadManager::_initialized = false;
TranslThreadManager TranslThreadManager::_thrManager;
JobControlArguments::RunMODE TranslThreadManager::_runMode;

extern const long Infinite;
extern void DoLookup(void);
extern void DoGenerate(void);
extern int lgsparsetran1(const CommunicationInterface * iCommInterface, const QHandle * iQHandle, short iTargetId, bool pass2flag);
extern void tran1cleanup(void);
extern int lgsparsetran2(const CommunicationInterface * iCommInterface, const QHandle * iQHandle, short iTargetId, bool pass2flag);
extern void tran2cleanup(void);
extern int lgsparsetran3(const CommunicationInterface * iCommInterface, const QHandle * iQHandle, short iTargetId, bool pass2flag);
extern void tran3cleanup(void);
extern int lgsparsetran4(const CommunicationInterface * iCommInterface, const QHandle * iQHandle, short iTargetId, bool pass2flag);
extern void tran4cleanup(void);
extern int lgsresmain(const CommunicationInterface * iCommInterface, const QHandle * iQHandle, short iTargetId);
extern void rescleanup(void);
extern void lgstran1pass(const CommunicationInterface * iCommInterface, const QHandle * iQHandle, short iTargetId);
extern void tran1passcleanup(void);
extern void lgstran2pass(const CommunicationInterface * iCommInterface, const QHandle * iQHandle, short iTargetId);
extern void tran2passcleanup(void);
extern void lgstran3pass(const CommunicationInterface * iCommInterface, const QHandle * iQHandle, short iTargetId);
extern void tran3passcleanup(void);
extern void lgstran4pass(const CommunicationInterface * iCommInterface, const QHandle * iQHandle, short iTargetId);
extern void tran4passcleanup(void);

#ifdef MEMORYCHECK
extern void NameThreadMem(DWORD id, const LgsString& threadName);
#endif

//typedef void (*DLLENTRYFN)(const CommunicationInterface * iCommInterface, const QHandle * iQHandle, short iTargetId);
//typedef void (*DLLCLEANUPFN)(void);
void DoLookup();
void DoTermSearch();
void DoGenerate();

const LgsString TranslThreadManager::LOOKUP("Lookup");
const LgsString TranslThreadManager::RES("LgsRes");
const LgsString TranslThreadManager::TRAN1("LgsParseTran1");
const LgsString TranslThreadManager::TRAN2("LgsParseTran2");
const LgsString TranslThreadManager::TRAN3("LgsParseTran3");
const LgsString TranslThreadManager::TRAN4("LgsParseTran4");
const LgsString TranslThreadManager::TRAN1_2PASS("LgsTran1Pass");
const LgsString TranslThreadManager::TRAN2_2PASS("LgsTran2Pass");
const LgsString TranslThreadManager::TRAN3_2PASS("LgsTran3Pass");
const LgsString TranslThreadManager::TRAN4_2PASS("LgsTran4Pass");
const LgsString TranslThreadManager::TERM_SEARCH("LgsTermSearch");
const LgsString TranslThreadManager::GENERATE("LgsGenerate");
const LgsString TranslThreadManager::LGSSGML_SPLIT("LgsSgmlSplitter");
const LgsString TranslThreadManager::LGSSGML_MERGE("LgsSgmlMerger");

// --------------------------------------------------------------------------
//					TransL thread manager
// --------------------------------------------------------------------------
TranslThreadManager::TranslThreadManager(void)
                    :splitterTime(0),
                     lookupTime(0),
                     resTime(0),
                     tran1Time(0),
                     tran2Time(0),
                     tran3Time(0),
                     tran4Time(0),
                     tran1Pass2Time(0),
                     tran2Pass2Time(0),
                     tran3Pass2Time(0),
                     tran4Pass2Time(0),
                     generateTime(0),
                     mergerTime(0),
                     termSearchTime(0),
                     totalExecTime(0)
{
}
// --------------------------------------------------------------------------
void TranslThreadManager::destroyCommInterface() {
	CommunicationInterface::destroySingleton();
}
// --------------------------------------------------------------------------
TranslThreadManager::~TranslThreadManager(void)
{
	CommunicationInterface::destroySingleton();
}
// --------------------------------------------------------------------------
TranslThreadManager & TranslThreadManager::singleton(void)
{
  _runMode = LgsDBCommonObjects::GetJobControlArguments().RunMode();
  if (!_initialized)
    {
      // Initialize global thread management data structures
      LgsThread::initialize();
      switch (_runMode)
        {
        case JobControlArguments::translationMode:
          _thrManager.initializeTranslationThreads();
          break;
        case JobControlArguments::searchMode:
          _thrManager.initializeTermSearchThreads();
          break;
        default:
          break;
        }
      _initialized = true;
    }
  return _thrManager;
}
// --------------------------------------------------------------------------
void TranslThreadManager::initializeTranslationThreads(void)
{
	// Create Thread objects
	// DO NOT start the threads, before all threads have been constructed
	// This ensures that the QHandles for communication is initialized 
	// before any thread is run.
   LgsThread *thrInst = new LgsSgmlSplitter;
   _threadList.push_back(thrInst);
	thrInst = new LgsLookup;
	_threadList.push_back(thrInst);
	thrInst = new LgsRes;
	_threadList.push_back(thrInst);
	thrInst = new LgsParseTran1;
	_threadList.push_back(thrInst);
	thrInst = new LgsParseTran2;
	_threadList.push_back(thrInst);
	thrInst = new LgsParseTran3;
	_threadList.push_back(thrInst);
	thrInst = new LgsParseTran4;
	_threadList.push_back(thrInst);
	thrInst = new LgsGenerate;
	_threadList.push_back(thrInst);
   thrInst = new LgsSgmlMerger;
	_threadList.push_back(thrInst);

	if (LgsDBCommonObjects::GetJobControlArguments().Tranpasses() == 2)
	{
		// Create all Tran threads
		thrInst = new LgsTran1Pass;
		_threadList.push_back(thrInst);
		thrInst = new LgsTran2Pass;
		_threadList.push_back(thrInst);
		thrInst = new LgsTran3Pass;
		_threadList.push_back(thrInst);
		thrInst = new LgsTran4Pass;
		_threadList.push_back(thrInst);
	}
}
// --------------------------------------------------------------------------
void TranslThreadManager::initializeTermSearchThreads(void)
{
	// Create Thread objects
	// DO NOT start the threads, before all threads have been constructed
	// This ensures that the QHandles for communication is initialized 
	// before any thread is run.
   LgsThread *thrInst = new LgsSgmlSplitter;
   _threadList.push_back(thrInst); 
	thrInst = new LgsLookup;
	_threadList.push_back(thrInst);
	thrInst = new LgsRes;
	_threadList.push_back(thrInst);
	thrInst = new LgsParseTran1;
	_threadList.push_back(thrInst);
	thrInst = new LgsTermSearch;
	_threadList.push_back(thrInst);

	if (LgsDBCommonObjects::GetJobControlArguments().Tranpasses() == 2)
	{
		// Create all Tran threads
		thrInst = new LgsParseTran2;
		_threadList.push_back(thrInst);
		thrInst = new LgsParseTran3;
		_threadList.push_back(thrInst);
		thrInst = new LgsParseTran4;
		_threadList.push_back(thrInst);
		thrInst = new LgsTran1Pass;
		_threadList.push_back(thrInst);
	}
}
// --------------------------------------------------------------------------
void TranslThreadManager::UpdateTimingInfo(LgsThread* opThread)
{
	totalExecTime += opThread->executionTime();
   if (opThread->threadName() == LGSSGML_SPLIT)
   {
      splitterTime = opThread->executionTime();
   }
	else if (opThread->threadName() == LOOKUP)
	{
		lookupTime = opThread->executionTime();
	}
	else if (opThread->threadName() == RES)
	{
		resTime = opThread->executionTime();
	}
	else if (opThread->threadName() == TRAN1)
	{
		tran1Time = opThread->executionTime();
	}
	else if (opThread->threadName() == TRAN2)
	{
		tran2Time = opThread->executionTime();
	}
	else if (opThread->threadName() == TRAN3)
	{
		tran3Time = opThread->executionTime();
	}
	else if (opThread->threadName() == TRAN4)
	{
		tran4Time = opThread->executionTime();
	}
	else if (opThread->threadName() == TRAN1_2PASS)
	{
		tran1Pass2Time = opThread->executionTime();
	}
	else if (opThread->threadName() == TRAN2_2PASS)
	{
		tran2Pass2Time = opThread->executionTime();
	}
	else if (opThread->threadName() == TRAN3_2PASS)
	{
		tran3Pass2Time = opThread->executionTime();
	}
	else if (opThread->threadName() == TRAN4_2PASS)
	{
		tran4Pass2Time= opThread->executionTime();
	}
	else if (opThread->threadName() == GENERATE)
	{
		generateTime = opThread->executionTime();
	}
	else if (opThread->threadName() == LGSSGML_MERGE)
	{
		mergerTime = opThread->executionTime();
	}
	else if (opThread->threadName() == TERM_SEARCH)
	{
		termSearchTime = opThread->executionTime();
	}
}
// --------------------------------------------------------------------------
void TranslThreadManager::OutputAllTimingInfo()
{
   cout << endl << endl;
   cout << "TIMING INFORMATION FOR ALL THREADS" << endl;
   cout << "(Thread Name --- HH:MM:SS:mmm --- PP%) HH - hours MM - minutes SS - seconds mmm - milliseconds PP - percentage of total time" << endl;
   OutputTimingInfo(LGSSGML_SPLIT, splitterTime);
   OutputTimingInfo(LOOKUP, lookupTime);
   OutputTimingInfo(RES, resTime);
   OutputTimingInfo(TRAN1, tran1Time);
   if (_runMode != JobControlArguments::searchMode)
   {
	   OutputTimingInfo(TRAN2, tran2Time);
	   OutputTimingInfo(TRAN3, tran3Time);
	   OutputTimingInfo(TRAN4, tran4Time);
   }

   if (LgsDBCommonObjects::GetJobControlArguments().Tranpasses() == 2)
   {
	   OutputTimingInfo(TRAN1_2PASS, tran1Pass2Time);
	   if (_runMode != JobControlArguments::searchMode)
	   {
		   OutputTimingInfo(TRAN2_2PASS, tran2Pass2Time);
		   OutputTimingInfo(TRAN3_2PASS, tran3Pass2Time);
		   OutputTimingInfo(TRAN4_2PASS, tran4Pass2Time);
	   }
   }
   if (termSearchTime)
	   OutputTimingInfo(TERM_SEARCH, termSearchTime);
   if (generateTime)
	   OutputTimingInfo(GENERATE, generateTime);
   if (mergerTime)
      OutputTimingInfo(LGSSGML_MERGE, mergerTime);
   if (LgsDBCommonObjects::GetSqlConnection())
   {
        unsigned long dbt = LgsDBCommonObjects::GetSqlConnection()->getTotalDbTime();
        OutputTimingInfo("Total Database Time", dbt);
   }
   long unsigned int i = totalExecTime;
   OutputTimingInfo("Total Execution Time", i, false);
}
// --------------------------------------------------------------------------
void TranslThreadManager::OutputTimingInfo(const LgsString & threadName, unsigned long execTime,
                                           bool printPercent) const
{
	unsigned short msRun = execTime % 1000;
	unsigned short secRun = (execTime / 1000) % 60;
	unsigned short minRun = (execTime / 60000) % 60;
	unsigned short hrsRun = execTime / 3600000;
	long double dExecTime = execTime;
	long double percentTime = (dExecTime / totalExecTime) * 100;
	char timeInfo[90];
	if (printPercent)
	{
		sprintf(timeInfo, "%25s --- %02u:%02u:%02u:%03u --- %05.02f%%", threadName.c_str(), hrsRun, minRun, secRun, msRun, percentTime);
	}
	else
	{
		sprintf(timeInfo, "%25s --- %02u:%02u:%02u:%03u", threadName.c_str(), hrsRun, minRun, secRun, msRun);
	}
	cout << timeInfo << endl;
}
// --------------------------------------------------------------------------
int TranslThreadManager::start(void)
{
  //Start all threads
  LgsThreadList threadListCopy;
  LgsThreadList::iterator endIter = _threadList.end();
  LgsThreadList::iterator currThread;

  for (currThread = _threadList.begin(); currThread != endIter; currThread++)
    {
      (*currThread)->start();
    }

  LgsThread::ExitStatus status = LgsThread::NormalExit;
  while (!_threadList.empty())
    {
      LgsThreadList::iterator currThread = waitForThread(_threadList, Infinite, status);
      if (currThread != _threadList.end())
        {
          if (status == LgsThread::NormalExit)
            {
              LgsThread* threadPtr = *currThread;
              _threadList.erase(currThread);
#ifdef MEMORYCHECK
              NameThreadMem(threadPtr->win32ThreadId(), threadPtr->threadName());
#endif
              if (LgsDBCommonObjects::GetJobControlArguments().TimeFlag())
                {
                  UpdateTimingInfo(threadPtr);
                }
              threadListCopy.push_back(threadPtr);
            }
          else 
            {
              if ((*currThread)->restartCount() < 5)
                {
                  (*currThread)->start();
                  (*currThread)->incRestartCount();
                  printf("RESTARTING THREAD (%s)\n", (*currThread)->threadName().c_str());
                }
            }
        }
    }

  // Delete all the threads at the same time
  for (LgsThreadList::iterator nextThread = threadListCopy.begin(); nextThread != threadListCopy.end(); nextThread++)
    {
      LgsThread* threadToDelete = *nextThread;
      threadToDelete->cleanup();
      delete threadToDelete;
    }
  threadListCopy.erase(threadListCopy.begin(), threadListCopy.end());

  // Output the timing information for the threads.
  if (LgsDBCommonObjects::GetJobControlArguments().TimeFlag())
    {
      OutputAllTimingInfo();
    }

  return 0;
}
// --------------------------------------------------------------------------
const LgsThreadList & TranslThreadManager::threadList(void) const
{
	return _threadList;
}
// --------------------------------------------------------------------------
const LgsThread* TranslThreadManager::GetThread(const LgsString& threadName) const
{
	LgsThreadList::const_iterator endIter = _threadList.end();
   LgsThreadList::const_iterator currThread = _threadList.begin();

	for (; (currThread != endIter) && (threadName != (*currThread)->threadName()); currThread++);
   if (currThread == endIter)
   {
      return 0;
   }
   return (*currThread);
}
// --------------------------------------------------------------------------
//					LgsLookup thread
// --------------------------------------------------------------------------
LgsLookup::LgsLookup(void)
          :LgsThread(TranslThreadManager::LOOKUP,
                     LgsDBCommonObjects::GetJobControlArguments().TimeFlag()), 
           _lookupHandle(CommunicationInterface::singleton().createQHandle(threadId()))
{
}
// --------------------------------------------------------------------------
LgsLookup::~LgsLookup(void)
{
}
// --------------------------------------------------------------------------
const QHandle* LgsLookup::lookupHandle(void) const
{
   return _lookupHandle;
}
// --------------------------------------------------------------------------
void LgsLookup::run(void)
{
   DoLookup();
}
// --------------------------------------------------------------------------
void LgsLookup::cleanup(void)
{
}

// --------------------------------------------------------------------------
//					LgsSgmlSplitter thread
// --------------------------------------------------------------------------
LgsSgmlSplitter::LgsSgmlSplitter(void) 
                :LgsThread(TranslThreadManager::LGSSGML_SPLIT,
                           LgsDBCommonObjects::GetJobControlArguments().TimeFlag())
{
}
// --------------------------------------------------------------------------
LgsSgmlSplitter::~LgsSgmlSplitter(void) 
{
   delete splitterApp;
}
// --------------------------------------------------------------------------
void LgsSgmlSplitter::run(void) 
{

// Create and Initialize the CSplitter object.
   bool result = false; 
   splitterApp = new CLgsSplitter;
   if (result = splitterApp->Init())
   {
      result = splitterApp->Run();
   }
   else // Error terminate all threads
   {
      cout << "Error unable to start the splitter thread" << endl;
      abort();
   } 
}
// --------------------------------------------------------------------------
void LgsSgmlSplitter::cleanup(void)
{
}

// --------------------------------------------------------------------------
//					LgsSgmlMerger thread
// --------------------------------------------------------------------------
LgsSgmlMerger::LgsSgmlMerger(void) 
              :LgsThread(TranslThreadManager::LGSSGML_MERGE,
                         LgsDBCommonObjects::GetJobControlArguments().TimeFlag()),
			      _generateOutHandle(CommunicationInterface::singleton().createQHandle(threadId())),
               _splitterOutHandle(CommunicationInterface::singleton().createQHandle(threadId()))
{
}
// --------------------------------------------------------------------------
LgsSgmlMerger::~LgsSgmlMerger(void) 
{
   delete mergerApp;
}
// --------------------------------------------------------------------------
void LgsSgmlMerger::run(void) 
{
// Create and Initialize the CMerger object.
   bool result = false; 
   mergerApp = new CLgsMerger;
   if (result = mergerApp->Init())
   {
      result = mergerApp->Run();
   }
   else // Error terminate all threads
   {
      cout << "Error unable to start the merger thread" << endl;
   }
}
// --------------------------------------------------------------------------
const QHandle* LgsSgmlMerger::generateOutHandle(void) const
{
    return _generateOutHandle;
}
// --------------------------------------------------------------------------
const QHandle* LgsSgmlMerger::splitterOutHandle(void) const
{
   return _splitterOutHandle;
}
// --------------------------------------------------------------------------
void LgsSgmlMerger::cleanup(void)
{
}

// --------------------------------------------------------------------------
//					LgsGenerate thread
// --------------------------------------------------------------------------
LgsGenerate::LgsGenerate(void) 
            :LgsThread(TranslThreadManager::GENERATE,
                       LgsDBCommonObjects::GetJobControlArguments().TimeFlag()),
             _generateHandle(CommunicationInterface::singleton().createQHandle(threadId()))
{
}
// --------------------------------------------------------------------------
LgsGenerate::~LgsGenerate(void)
{
}
// --------------------------------------------------------------------------
void LgsGenerate::run(void)
{
	DoGenerate();
}
// --------------------------------------------------------------------------
const QHandle* LgsGenerate::generateHandle(void) const
{
   return _generateHandle;
}
// --------------------------------------------------------------------------
void LgsGenerate::cleanup(void)
{
}

// --------------------------------------------------------------------------
//					LgsTermSearch thread
// --------------------------------------------------------------------------
LgsTermSearch::LgsTermSearch(void) 
              :LgsThread(TranslThreadManager::TERM_SEARCH,
                         LgsDBCommonObjects::GetJobControlArguments().TimeFlag()),
               _termSearchHandle(CommunicationInterface::singleton().createQHandle(threadId()))
{
}
// --------------------------------------------------------------------------
LgsTermSearch::~LgsTermSearch(void)
{
}
// --------------------------------------------------------------------------
const QHandle* LgsTermSearch::termSearchHandle(void) const
{
   return _termSearchHandle;
}
// --------------------------------------------------------------------------
void LgsTermSearch::run(void)
{
	DoTermSearch();
}
// --------------------------------------------------------------------------
void LgsTermSearch::cleanup(void)
{
}

// --------------------------------------------------------------------------
//					LgsRes thread
// --------------------------------------------------------------------------
LgsRes::LgsRes(void) 
       :LgsThread(TranslThreadManager::RES,
                  LgsDBCommonObjects::GetJobControlArguments().TimeFlag()), 
        _qHandle(CommunicationInterface::singleton().createQHandle(threadId()))
{
}
// --------------------------------------------------------------------------
LgsRes::~LgsRes(void)
{
}
// --------------------------------------------------------------------------
void LgsRes::run(void)
{
	TranslThreadManager & thrManager = TranslThreadManager::singleton();
	short tran1Id = getThreadId(thrManager.threadList(), TranslThreadManager::TRAN1.c_str());
	lgsresmain(&CommunicationInterface::singleton(), _qHandle, tran1Id);
}
// --------------------------------------------------------------------------
void LgsRes::cleanup(void)
{
	rescleanup();
}
// --------------------------------------------------------------------------
LgsParseTran1::LgsParseTran1(void)
              :LgsThread(TranslThreadManager::TRAN1,
                         LgsDBCommonObjects::GetJobControlArguments().TimeFlag()),
               _qHandle(CommunicationInterface::singleton().createQHandle(threadId()))
{

}

// --------------------------------------------------------------------------
//					LgsTran 1 threads
// --------------------------------------------------------------------------
LgsParseTran1::~LgsParseTran1(void)
{
}
// --------------------------------------------------------------------------
void LgsParseTran1::run(void)
{
	TranslThreadManager & thrManager = TranslThreadManager::singleton();

	short targetId;
   bool twoPassMode = LgsDBCommonObjects::GetJobControlArguments().Tranpasses() == 2;

   if ((JobControlArguments::searchMode == LgsDBCommonObjects::GetJobControlArguments().RunMode()) &&
       !twoPassMode)
	{
		targetId = getThreadId(thrManager.threadList(), TranslThreadManager::TERM_SEARCH.c_str());
	} 
	else
	{
		targetId = getThreadId(thrManager.threadList(), TranslThreadManager::TRAN2.c_str());
	}

	lgsparsetran1(&CommunicationInterface::singleton(), _qHandle, targetId, twoPassMode);
}
// --------------------------------------------------------------------------
void LgsParseTran1::cleanup(void)
{
	tran1cleanup();
}
// --------------------------------------------------------------------------
LgsTran1Pass::LgsTran1Pass(void)
             :LgsThread(TranslThreadManager::TRAN1_2PASS,
                        LgsDBCommonObjects::GetJobControlArguments().TimeFlag()),
              _qHandle(CommunicationInterface::singleton().createQHandle(threadId()))
{
}
// --------------------------------------------------------------------------
LgsTran1Pass::~LgsTran1Pass(void)
{
}
// --------------------------------------------------------------------------
void LgsTran1Pass::run(void)
{
	TranslThreadManager & thrManager = TranslThreadManager::singleton();
	short targetId;
   if (JobControlArguments::searchMode == LgsDBCommonObjects::GetJobControlArguments().RunMode())
	{
		targetId = getThreadId(thrManager.threadList(), TranslThreadManager::TERM_SEARCH.c_str());
	}
	else
	{
		targetId = getThreadId(thrManager.threadList(), TranslThreadManager::TRAN2_2PASS.c_str());
	}

	lgstran1pass(&CommunicationInterface::singleton(), _qHandle, targetId);
}
// --------------------------------------------------------------------------
void LgsTran1Pass::cleanup(void)
{
	tran1passcleanup();
}

// --------------------------------------------------------------------------
//					LgsTran 2 threads
// --------------------------------------------------------------------------
LgsParseTran2::LgsParseTran2(void)
              :LgsThread(TranslThreadManager::TRAN2,
                         LgsDBCommonObjects::GetJobControlArguments().TimeFlag()),
               _qHandle(CommunicationInterface::singleton().createQHandle(threadId()))
{
}
// --------------------------------------------------------------------------
LgsParseTran2::~LgsParseTran2(void)
{
}
// --------------------------------------------------------------------------
void LgsParseTran2::run(void)
{
   bool twoPassMode = LgsDBCommonObjects::GetJobControlArguments().Tranpasses() == 2;
	TranslThreadManager & thrManager = TranslThreadManager::singleton();
	short tran3Id = getThreadId(thrManager.threadList(), TranslThreadManager::TRAN3.c_str());
	lgsparsetran2(&CommunicationInterface::singleton(), _qHandle, tran3Id, twoPassMode);
}
// --------------------------------------------------------------------------
void LgsParseTran2::cleanup(void)
{
	tran2cleanup();
}
// --------------------------------------------------------------------------
LgsTran2Pass::LgsTran2Pass(void)
             :LgsThread(TranslThreadManager::TRAN2_2PASS,
                        LgsDBCommonObjects::GetJobControlArguments().TimeFlag()),
              _qHandle(CommunicationInterface::singleton().createQHandle(threadId()))
{
}
// --------------------------------------------------------------------------
LgsTran2Pass::~LgsTran2Pass(void)
{
}
// --------------------------------------------------------------------------
void LgsTran2Pass::run(void)
{
	TranslThreadManager & thrManager = TranslThreadManager::singleton();
	short targetId = getThreadId(thrManager.threadList(), TranslThreadManager::TRAN3_2PASS.c_str());

	lgstran2pass(&CommunicationInterface::singleton(), _qHandle, targetId);
}
// --------------------------------------------------------------------------
void LgsTran2Pass::cleanup(void)
{
	tran2passcleanup();
}

// --------------------------------------------------------------------------
//					LgsTran 3 threads
// --------------------------------------------------------------------------
LgsParseTran3::LgsParseTran3(void)
              :LgsThread(TranslThreadManager::TRAN3,
                         LgsDBCommonObjects::GetJobControlArguments().TimeFlag()),
               _qHandle(CommunicationInterface::singleton().createQHandle(threadId()))
{
}
// --------------------------------------------------------------------------
LgsParseTran3::~LgsParseTran3(void)
{
}
// --------------------------------------------------------------------------
void LgsParseTran3::run(void)
{
   bool twoPassMode = LgsDBCommonObjects::GetJobControlArguments().Tranpasses() == 2;
	TranslThreadManager & thrManager = TranslThreadManager::singleton();
	short tran4Id = getThreadId(thrManager.threadList(), TranslThreadManager::TRAN4.c_str());
	lgsparsetran3(&CommunicationInterface::singleton(), _qHandle, tran4Id, twoPassMode);
}
// --------------------------------------------------------------------------
void LgsParseTran3::cleanup(void)
{
	tran3cleanup();
}
// --------------------------------------------------------------------------
LgsTran3Pass::LgsTran3Pass(void)
             :LgsThread(TranslThreadManager::TRAN3_2PASS,
                        LgsDBCommonObjects::GetJobControlArguments().TimeFlag()),
              _qHandle(CommunicationInterface::singleton().createQHandle(threadId()))
{
}
// --------------------------------------------------------------------------
LgsTran3Pass::~LgsTran3Pass(void)
{
}
// --------------------------------------------------------------------------
void LgsTran3Pass::run(void)
{
	TranslThreadManager & thrManager = TranslThreadManager::singleton();
	short targetId = getThreadId(thrManager.threadList(), TranslThreadManager::TRAN4_2PASS.c_str());

	lgstran3pass(&CommunicationInterface::singleton(), _qHandle, targetId);
}
// --------------------------------------------------------------------------
void LgsTran3Pass::cleanup(void)
{
	tran3passcleanup();
}

// --------------------------------------------------------------------------
//					LgsTran 4 threads
// --------------------------------------------------------------------------
LgsParseTran4::LgsParseTran4(void)
              :LgsThread(TranslThreadManager::TRAN4,
                         LgsDBCommonObjects::GetJobControlArguments().TimeFlag()),
               _qHandle(CommunicationInterface::singleton().createQHandle(threadId()))
{
}
// --------------------------------------------------------------------------
LgsParseTran4::~LgsParseTran4(void)
{
}
// --------------------------------------------------------------------------
void LgsParseTran4::run(void)
{
	TranslThreadManager & thrManager = TranslThreadManager::singleton();
	short targetId;
   bool twoPassMode = LgsDBCommonObjects::GetJobControlArguments().Tranpasses() == 2;

	if (twoPassMode)
	{
		targetId = getThreadId(thrManager.threadList(), TranslThreadManager::TRAN1_2PASS.c_str());
	}
	else
	{
		targetId = getThreadId(thrManager.threadList(), TranslThreadManager::GENERATE.c_str());
	}
	lgsparsetran4(&CommunicationInterface::singleton(), _qHandle, targetId, twoPassMode);
}
// --------------------------------------------------------------------------
void LgsParseTran4::cleanup(void)
{
	tran4cleanup();
}
// --------------------------------------------------------------------------
LgsTran4Pass::LgsTran4Pass(void)
             :LgsThread(TranslThreadManager::TRAN4_2PASS,
                        LgsDBCommonObjects::GetJobControlArguments().TimeFlag()),
              _qHandle(CommunicationInterface::singleton().createQHandle(threadId()))
{
}
// --------------------------------------------------------------------------
LgsTran4Pass::~LgsTran4Pass(void)
{
}
// --------------------------------------------------------------------------
void LgsTran4Pass::run(void)
{
	TranslThreadManager & thrManager = TranslThreadManager::singleton();
	short targetId = getThreadId(thrManager.threadList(), TranslThreadManager::GENERATE.c_str());

	lgstran4pass(&CommunicationInterface::singleton(), _qHandle, targetId);
}
// --------------------------------------------------------------------------
void LgsTran4Pass::cleanup(void)
{
	tran4passcleanup();
}

// --------------------------------------------------------------------------
//					DoLookup
// --------------------------------------------------------------------------
void DoLookup()
{
	// Get the job control arguments object
	TranslCommonObjects::initializeLookupDiag();

	//setCompBuf();
   LDocument document(TranslCommonObjects::GetDictionary());
	document.lookup();

	LgsDBCommonObjects::GetJobControlArguments().WordCount(document.translatedWordCount());
	TranslCommonObjects::freeDiag();
   //cleanupCompBuf();
}

// --------------------------------------------------------------------------
//					DoGenerate
// --------------------------------------------------------------------------
void DoGenerate()
{
	// set up the diagnostic file for this process
	TranslCommonObjects::initializeGenerateDiag();
//	setCompBuf();
	// create an output document object for this job
	LDocument document(TranslCommonObjects::GetDictionary());
	// retrieve the vector of target sentence units
	TargetUnitConcreteBuilder targetBuilder(TranslCommonObjects::GetPersistDataFactory());
	TranslCommonObjects::GetDictionary().setTargetBuilder(&targetBuilder);
	// generate the actual translation (in final surface form) in filterOutStrm stream
	document.generateTranslation();
	TranslCommonObjects::freeDiag();
 //  cleanupCompBuf();
}

// --------------------------------------------------------------------------
//					DoTermSearch
// --------------------------------------------------------------------------
void DoTermSearch()
{
	// set up the diagnostic file for this process
	TranslCommonObjects::initializeSearchDiag();
//	setCompBuf();
	
   // set in which stream (output file) to generate the term search report
   char outputFile[MAX_FILEPATH_LEN];
   GetConfigData("tempfile", "transl_output", outputFile, MAX_FILEPATH_LEN);
    
   ofstream *termSearchStrm = new ofstream(outputFile, ios::binary);
	if (!termSearchStrm->good())
   {
		TranslCommonObjects::freeDiag();
		exitThread(2);
	}
	// create an output document object for this job
	LDocument document(TranslCommonObjects::GetDictionary());
	// retrieve the vector of target sentence units
	TargetUnitConcreteBuilder targetBuilder(TranslCommonObjects::GetPersistDataFactory());
	TranslCommonObjects::GetDictionary().setTargetBuilder(&targetBuilder);
	// set the stream where to save the report for this document
	document.termSearchStream(termSearchStrm);
	// capture all terms in this document and generate report in stream
	document.termSearch();
   termSearchStrm->close();
   delete termSearchStrm;
	TranslCommonObjects::freeDiag();
//   cleanupCompBuf();
}
