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
#ifndef _TRANSLTHRMAN_H_
#define _TRANSLTHRMAN_H_

#include <logos_libs/multithreadlib/lgsthread.h>
#include <lgs_db_io/jobcontrolarguments.h>
#include <logos_libs/multithreadlib/comminterface.h>

class CLgsSplitter;
class CLgsMerger;

#define HINSTANCE void *

// -------------------------------------------------------------------
class LgsLookup: public LgsThread
{
private:
   const QHandle* _lookupHandle;

public:
   LgsLookup(void);
   virtual ~LgsLookup(void);
   const QHandle* lookupHandle(void) const;
   void run(void);
   void cleanup(void);
};
// -------------------------------------------------------------------
class LgsSgmlSplitter: public LgsThread
{
public:
   LgsSgmlSplitter();
   virtual ~LgsSgmlSplitter();
   void run(void);
   void cleanup(void);

private:
   CLgsSplitter* splitterApp;
};
// -------------------------------------------------------------------
class LgsSgmlMerger: public LgsThread
{
public:
   LgsSgmlMerger();
   virtual ~LgsSgmlMerger();
   void run(void);
   void cleanup(void);
   const QHandle* generateOutHandle(void) const;
   const QHandle* splitterOutHandle(void) const;

private:
   const QHandle* _generateOutHandle;
   const QHandle* _splitterOutHandle;
   HINSTANCE _dllInstance;
   CLgsMerger* mergerApp;
};
// -------------------------------------------------------------------
class LgsGenerate: public LgsThread
{
private:
   const QHandle* _generateHandle;

public:
   LgsGenerate(void);
   virtual ~LgsGenerate(void);
   const QHandle* generateHandle(void) const;
   void run(void);
   void cleanup(void);
};
// -------------------------------------------------------------------
class LgsTermSearch: public LgsThread
{
private:
   const QHandle* _termSearchHandle;

public:
   LgsTermSearch(void);
   virtual ~LgsTermSearch(void);
   const QHandle* termSearchHandle(void) const;
   void run(void);
   void cleanup(void);
};
// -------------------------------------------------------------------
class LgsRes: public LgsThread
{
private:
   const QHandle* _qHandle;

public:
   LgsRes(void);
   virtual ~LgsRes(void);
   void run(void);
   void cleanup(void);
};
// -------------------------------------------------------------------
class LgsParseTran1: public LgsThread
{
private:
   const QHandle* _qHandle;

public:
   LgsParseTran1(void);
   virtual ~LgsParseTran1(void);
   void run(void);
   void cleanup(void);
};
// -------------------------------------------------------------------
class LgsParseTran2: public LgsThread
{
private:
   const QHandle* _qHandle;

public:
   LgsParseTran2(void);
   virtual ~LgsParseTran2(void);
   void run(void);
   void cleanup(void);
};
// -------------------------------------------------------------------
class LgsParseTran3: public LgsThread
{
private:
   const QHandle* _qHandle;

public:
   LgsParseTran3(void);
   virtual ~LgsParseTran3(void);
   void run(void);
   void cleanup(void);
};
// -------------------------------------------------------------------
class LgsParseTran4: public LgsThread
{
private:
   const QHandle* _qHandle;

public:
   LgsParseTran4(void);
   virtual ~LgsParseTran4(void);
   void run(void);
   void cleanup(void);
};
// -------------------------------------------------------------------
class LgsTran1Pass: public LgsThread
{
private:
   const QHandle* _qHandle;
   HINSTANCE _dllInstance;

public:
   LgsTran1Pass(void);
   virtual ~LgsTran1Pass(void);
   void run(void);
   void cleanup(void);
};
// -------------------------------------------------------------------
class LgsTran2Pass: public LgsThread
{
private:
   const QHandle* _qHandle;
   HINSTANCE _dllInstance;

public:
   LgsTran2Pass(void);
   virtual ~LgsTran2Pass(void);
   void run(void);
   void cleanup(void);
};
// -------------------------------------------------------------------
class LgsTran3Pass: public LgsThread
{
private:
   const QHandle* _qHandle;
   HINSTANCE _dllInstance;

public:
   LgsTran3Pass(void);
   virtual ~LgsTran3Pass(void);
   void run(void);
   void cleanup(void);
};
// -------------------------------------------------------------------
class LgsTran4Pass: public LgsThread
{
private:
   const QHandle* _qHandle;
   HINSTANCE _dllInstance;

public:
   LgsTran4Pass(void);
   virtual ~LgsTran4Pass(void);
   void run(void);
   void cleanup(void);
};
// -------------------------------------------------------------------
class TranslThreadManager
{
private:
   LgsThreadList _threadList;
   static bool _initialized;
   static TranslThreadManager _thrManager;
   static JobControlArguments::RunMODE _runMode;
   unsigned long splitterTime;
   unsigned long lookupTime;
   unsigned long resTime;
   unsigned long tran1Time;
   unsigned long tran2Time;
   unsigned long tran3Time;
   unsigned long tran4Time;
   unsigned long tran1Pass2Time;
   unsigned long tran2Pass2Time;
   unsigned long tran3Pass2Time;
   unsigned long tran4Pass2Time;
   unsigned long generateTime;
   unsigned long mergerTime;
   unsigned long termSearchTime;
   long double totalExecTime;

   TranslThreadManager(void);
   void UpdateTimingInfo(LgsThread* opThread);
   void OutputAllTimingInfo();
   void OutputTimingInfo(const LgsString & threadName, unsigned long execTime,
                         bool printPercent = true) const;

public:
   static const LgsString LOOKUP;
   static const LgsString RES;
   static const LgsString TRAN1;
   static const LgsString TRAN2;
   static const LgsString TRAN3;
   static const LgsString TRAN4;
   static const LgsString TRAN1_2PASS;
   static const LgsString TRAN2_2PASS;
   static const LgsString TRAN3_2PASS;
   static const LgsString TRAN4_2PASS;
   static const LgsString TERM_SEARCH;
   static const LgsString GENERATE;
   static const LgsString LGSSGML_SPLIT;
   static const LgsString LGSSGML_MERGE;
   static TranslThreadManager & singleton();
   static void destroyCommInterface();
   ~TranslThreadManager(void);

   const LgsThreadList & threadList() const;

   void initializeTranslationThreads(void);
   void initializeTermSearchThreads(void);
   int start(void);
   const LgsThread* GetThread(const LgsString& threadName) const;
};


#endif
