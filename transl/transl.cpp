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
//---------------------------------------------------------------------
// File - TransL.cpp
//
// Class - none (main function implementation)
//
// Description - Like any good main function the code in this file
//      does as little as possible. All real behavior occurs in the
//      classes that have been defined elsewhere. It asks the arg
//      parser to parse the command line arguements, creates some
//      polymorphic objects, stacrts a new document and has the document
//      "gerdem" itself. In has a final catch for exceptions that have
//      been thrown throughout the application but have not been
//      caught.
//
//---------------------------------------------------------------------
//#include <crtdbg.h>
#include <logos_include/logoscommon.h>
#include <time.h>
#include <logos_libs/gerdem/dictionaryentrybuilder.h>
#include <logos_libs/gerdem/targetunitconcretebuilder.h>
#include <logos_libs/sql/sqlexception.h>
#include <logos_libs/utility/argumentexception.h>
#include <logos_libs/translutility/translcommonobjects.h>
#include <logos_libs/linguistic/lsentence.h>
#include <lgs_db_io/lgsdbcommonobjects.h>
#include <configdatafileinterface/configdatainterfacemain.h>
#include <transl/translthrman.h>
#include <lgs_base/lgsmemoryinfo.h>
#include <malloc.h>

void GerdemReport(ostream&);
extern void DoTermSearch(void);
#ifdef MEMORYCHECK
extern void OutputAllMemoryInfo();
#endif

unsigned long getCacheSize(void);

int PreTransl();
int PostTransl();

void getHeapStatistics(unsigned long& totalRegionCommittedSize, unsigned long& totalBusyBlockSize,
                       unsigned long& totalFreeBlockSize)
{
/*    totalRegionCommittedSize = 0;
    totalBusyBlockSize = 0;
    totalFreeBlockSize = 0;


   _HEAPINFO hinfo;
   int heapstatus;
   hinfo._pentry = NULL;
   while( ( heapstatus = _heapwalk( &hinfo ) ) == _HEAPOK )
   { 
        if (hinfo._useflag == _USEDENTRY)
        {
            totalBusyBlockSize += hinfo._size;
        }
        else
        {
            totalFreeBlockSize += hinfo._size;
        }
   }
   totalRegionCommittedSize = totalBusyBlockSize + totalFreeBlockSize;

*/

#ifdef _MSC_VER
    HANDLE processHeaps[30];
    unsigned long heapSize = 0;
    unsigned long noHeapsToCheck = 0;
    if ((noHeapsToCheck = GetProcessHeaps(30, processHeaps)) > 30)
    {
        noHeapsToCheck = 30;
    }
    PROCESS_HEAP_ENTRY heapEntry;
    while (noHeapsToCheck--)
    {
        heapEntry.lpData = NULL;
        while (HeapWalk(processHeaps[noHeapsToCheck], &heapEntry))
        {
            if (PROCESS_HEAP_REGION & heapEntry.wFlags)
            {
                totalRegionCommittedSize += heapEntry.Region.dwCommittedSize;
            }
            else if (PROCESS_HEAP_ENTRY_BUSY & heapEntry.wFlags)
            {
                totalBusyBlockSize += heapEntry.cbData+heapEntry.cbOverhead;
            }
            else if (!(PROCESS_HEAP_UNCOMMITTED_RANGE & heapEntry.wFlags))
            {
                totalFreeBlockSize += heapEntry.cbData+heapEntry.cbOverhead;
            }
        }
    }
#endif
    
}

//-------------------------------------------------------------------
int main(int argc, char* argv[])
{
#ifdef _MSC_VER
   // Get the current state of the flag
   // and store it in a temporary variable
   int tmpFlag = _CrtSetDbgFlag(_CRTDBG_REPORT_FLAG);

   // Turn Off - Keep freed memory blocks in the
   // heap's linked list and mark them as freed
   tmpFlag &= ~_CRTDBG_DELAY_FREE_MEM_DF;
   tmpFlag &= ~_CRTDBG_ALLOC_MEM_DF;

   // Turn Off (AND) - prevent _CrtCheckMemory from
   // being called at every allocation request
   tmpFlag &= ~_CRTDBG_CHECK_ALWAYS_DF;
   // Turn off Leak Checking at program exit
   tmpFlag &= ~_CRTDBG_LEAK_CHECK_DF;

   // Set the new state for the flag
   _CrtSetDbgFlag(tmpFlag);

   // Get start time value
   DWORD dwStart = GetTickCount();
#else
   clock_t dwStart = clock();
#endif

   int totalWords = 0;

   // Display Transl has started
   cout << "\n*** Transl\n\n";

   unsigned long iTotalRegionHeapSize = 0;
   unsigned long iTotalBusyHeapSize = 0;
   unsigned long iTotalFreeHeapSize = 0;
   unsigned long eTotalRegionHeapSize = 0;
   unsigned long eTotalBusyHeapSize = 0;
   unsigned long eTotalFreeHeapSize = 0;
   try
   {
      // Initialize the global objects
      LgsDBCommonObjects::InitializeCommonObjects();
      bool timeFlag = LgsDBCommonObjects::GetJobControlArguments().TimeFlag();
      LgsMemoryInfo::setMemoryFlag(timeFlag);
      LgsMemoryInfo::singleton().addMemoryState("transl main - start");
      if (timeFlag)
      {
        getHeapStatistics(iTotalRegionHeapSize, iTotalBusyHeapSize, iTotalFreeHeapSize);
      }

      if( PreTransl() == 0 )
      {
         TranslCommonObjects::InitializeCommonObjects();
         switch (LgsDBCommonObjects::GetJobControlArguments().RunMode())
         {
            case JobControlArguments::transferMode:
            case JobControlArguments::resolveMode:
               break;

            case JobControlArguments::translationMode:
               LSentence::createMessaging();
               TranslThreadManager::singleton().start();
               //GerdemReport(cout);
               break;

            case JobControlArguments::searchMode:
               cout << "******* TermSearch" << endl;
               LSentence::createMessaging();
               TranslThreadManager::singleton().start();
               break;
         }
         // Has to be accomplished this way otherwise job control arguments object will
         // be invalid by the time we check the time flag directly in the object.
         LgsMemoryInfo::singleton().addMemoryState("transl main - msg cleanup");
         LSentence::cleanupMessaging();
         LgsMemoryInfo::singleton().addMemoryState("transl main - transl common objects cleanup");
         TranslCommonObjects::CleanupCommonObjects();
      }
      PostTransl();

      totalWords = LgsDBCommonObjects::GetJobControlArguments().WordCount();

      LgsMemoryInfo::singleton().addMemoryState("transl main - lgsdb common objects cleanup");
      LgsDBCommonObjects::CleanupCommonObjects();
      LgsMemoryInfo::singleton().addMemoryState("transl main - complete");
      cout << '\n';
      LgsMemoryInfo::singleton().dumpMemoryStates(cout);
      char msgBuffer[100];
      if (timeFlag)
      {
        sprintf(msgBuffer, "%s%9u\n", "Semtab rules cache size (KB):     ", getCacheSize()/1024);
        cout << msgBuffer;
      }

      if (timeFlag)
      {
        getHeapStatistics(eTotalRegionHeapSize, eTotalBusyHeapSize, eTotalFreeHeapSize);
        cout << '\n';
        cout << "HEAP INFORMATION\n";
        sprintf(msgBuffer, "%s%9u\n", "Start Busy Heap Size (KB):        ", iTotalBusyHeapSize/1024);
        cout << msgBuffer;
        sprintf(msgBuffer, "%s%9u\n", "End Busy Heap Size (KB):          ", eTotalBusyHeapSize/1024);
        cout << msgBuffer;
      }

#ifdef MEMORYCHECK
      if (timeFlag)
      {
         OutputAllMemoryInfo();
      }
#endif
	}
	catch(ArgumentException& ax)
	{
		cout << endl << "EXCEPTION: " << ax.Message() << endl;
		return -1;
	}
	catch(SqlException& x)
	{
		cout << endl << "EXCEPTION: " << x.Message() << endl;
		return -1;
	}
	catch(LgsString& exceptString)
	{
		cout << endl << "EXCEPTION: " << exceptString << endl;
		return -1;
	}
	catch(...)
	{
		cout << endl << "EXCEPTION: " << "Unrecognized exception" << endl;
		return -1;
	}

#ifdef _MSC_VER
   // Get finish time value
   DWORD dwEnd = GetTickCount();
   
   // Display speed and time
   DWORD nElapsedTime = dwEnd - dwStart; // in milliseconds
#else
   clock_t dwEnd = clock();
   long nElapsedTime = (dwEnd - dwStart) * 1000 / CLOCKS_PER_SEC;
#endif
   double wordsPerSec = ((double)totalWords * 1000) / ((nElapsedTime > 0)? nElapsedTime: 1);
   int nMilliSec = nElapsedTime % 1000;
   nElapsedTime = nElapsedTime / 1000; // in seconds
   int nSec = nElapsedTime % 60; 
   nElapsedTime = nElapsedTime / 60; // in minutes
   int nMin = nElapsedTime % 60; 
   int nHrs = nElapsedTime / 60; // in hours
   cout << '\n';
   cout << "Total Words     = " << totalWords << '\n';
   cout << "Words / Hour    = " << (wordsPerSec * 3600) << '\n';
   cout << "Words / Second  = " << (wordsPerSec) << '\n'; 
   cout << "Total CPU time elapsed (Hours:Minutes:Seconds) = "<< nHrs <<" : " << nMin << " : " << nSec << "." << nMilliSec << "\n";

   return 0;
}
//-------------------------------------------------------------------
void GerdemReport(ostream& stream)
{
   DictionaryEntryBuilder* pBuilder = TranslCommonObjects::GetDictionaryEntryBuilder();
	stream << endl;
	stream << "LOOKUP: Total Words     = " << pBuilder->wordCount() << endl;
	stream << "LOOKUP: Word Matches    = " << pBuilder->entryCache().Count() << endl;
	stream << "LOOKUP: Unmatched words = " << pBuilder->unfoundWordCachePre().Count() << endl;
	stream << endl;
}
