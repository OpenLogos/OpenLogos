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
#ifdef _MSC_VER
#include "stdafx.h"
#include <io.h>
#endif
#include <logos_libs/GrammarRules/LgsGrammarRules.h>
#include <configdatafileinterface/configdatainterfacemain.h>
#include "logos_libs/multithreadlib/lgscritsect.h"
#include "logos_libs/multithreadlib/comminterface.h"

#define _TRAN_RULE_IO_IMPL_
#include "tran_rule_io.h"
#include "res_rule_stat.h"

#include "util_io.h"
#include "util.h"

#ifdef HAVE_MMAP
#include "logos_include/memory_mapper.h"
static memmap_manager TranMManager;
#endif

// these two headers are for dumping rules and indexes and
// can be safely removed along with fwrite_typos, fwrite_noftyp,
// write_comp_rule_set and fwrite_comp_rule_set helpers.
#include <fcntl.h>
// #include<io.h>

#ifndef _MSC_VER
#define __try try
#include <sys/io.h>
#define FillMemory(__Dest,__Len,__Fill) memset(__Dest,__Fill,__Len)
#define CopyMemory(__Dest,__Src,__Len) memcpy(__Dest,__Src,__Len)

#define GetCurrentThreadId pthread_self
typedef pthread_t THREAD_ID;
#define ILLEGAL_THREAD_ID 0

#include <cstring>

pthread_mutex_t interlock_mutex = PTHREAD_MUTEX_INITIALIZER;
#else
#define ILLEGAL_THREAD_ID -1
#endif

#define RULE_TABLES 2 // 0 - main, 1 - mini
#define IDX1_NUMS 1024
#define IDX2_NUMS 1024
#define RULE_NUMS 12
#define OVR_NUMS 21
#define COM_CHARS 80

// do not change these values
#define TAB30_MINI_IX	0
#define TAB40_MINI_IX	1
#define TAB50_MINI_IX	2

// old system legacy defs
struct neg_type_idx3 { short lstng[20], numng[20]; };

// instead of physical record numbers as overflow
// extention record pointers (which were used in the old system)
// some artificial values (or handles) are used now
// (see rulein[1st] / ovrin[1st] / vtrin[1st] code for details)

// cannot be less than 3,
// because negated version of this is used in type slots of rule/ovr
#define TAGSET_PART_BASE		3
#define OVERFLOW_RECORD			0x7fff


// the following functions are called only during initialization
// to make compacted versions of the rules
static int rulein1st(short* rules, short* rule_buffer, short* rule_number, int* ruleId);

static int ovrin1st(
	short* rules, short* rule_buffer, short* rule_number, short* part_number);

static int comin1st(short* rules, char* rule_buffer, short* rule_number);

static int vtrin1st(
	short* rules, short* rule_buffer, short* rule_number, short* part_number);

static int rulein1st(int tix, short* rules, short* rule_buffer, short* rule_number, int* ruleId);
static int ovrin1st(int tix, short* rules, short* rule_buffer, short* rule_number, short* part_number);
static int comin1st(int tix, short* rules, char* rule_buffer, short* rule_number);
static int vtrin1st(int tix, short* rules, short* rule_buffer, short* rule_number, short* part_number);

extern "C" int rule_load_from_cache(const char *base, char *ruleset_filename,
									int tran_number, bool isParse); 
extern "C" int rule_load_original(const char *base, char *ruleset_filename,
								  int tran_number, int miniflag, bool isParse);
extern "C" int rule_unload();
void unmap(int i);

// compacted rule
typedef struct _compact_rule_hdr
{
	short len;	// length of the structure
	int ruleId;
	short tOff; // offset of the first tagset record
	short vOff; // offset of the first vtr record
	char comment[COM_CHARS];
	short rule[RULE_NUMS];
	// optional OVR record: short[OVR_NUMS]
	// one or more TAGSET records: short[numTagsetRecs][OVR_NUMS]
	// optional VTR records: short[numVtrRecs][VTR_NUMS]
} CompRuleHdr;

// compacted rules set
typedef struct _comp_rule_set
{
	int numRules;
	CompRuleHdr** r;
} CompRuleSet;

static bool tab30exist(	int tabNo );
static bool BuildIndexes( int tranIx, int mini);
static CompRuleSet* CompactRuleSet(int tranIx, int which, CLgsRuleSet* rs);
static void FreeCompRuleSet(CompRuleSet* crs);

// per tran data (or per thread)
typedef short typos_type[RULE_TABLES][20][IDX1_NUMS];
typedef short noftyp_type[RULE_TABLES][20][IDX2_NUMS];
struct tran_data_struct {
//	short typos[RULE_TABLES][20][IDX1_NUMS];
//	short noftyp[RULE_TABLES][20][IDX2_NUMS];
//	struct neg_type_idx3 negt[RULE_TABLES];
	typos_type *typos;
	noftyp_type *noftyp;
	struct neg_type_idx3 *negt;
	
	CLgsTableSet* m_pTab30;

	CLgsRuleSet* m_pTran;
	CLgsRuleSet* m_pTranMini;

	// compacted versions of the above two
	int rulesetLen;
	CompRuleSet* m_pCTran;
	CompRuleSet* m_pCTranMini;

	// these are kept uncompacted, and replicated (40/50) because they are small
	CLgsTableSet* m_pTabMini[3];
// indicator - whether m_pCTran,typos,noftyp,negt have been
//	shared memory mapped or allocated with new.
	bool original;
};

// used for sync
//CRITICAL_SECTION __csec;
LgsCriticalSection __csec;

static long __refcnt40[2]; // one for pass, one for tran
static long __refcnt50[2]; // same
//static CRITICAL_SECTION __csec40[2]; // same
//static CRITICAL_SECTION __csec50[2]; // same
static LgsCriticalSection __csec40[2]; // same
static LgsCriticalSection __csec50[2]; // same

// per pass data, one instance is for passX, one for tranX, where X = 1,2,3,4
static CLgsTableSet* m_pTab40[2];
static CLgsTableSet* m_pTab50[2];

// This causes problem in 2pass mode: save file is loaded for both pass and tran
// so far these are kept uncompacted
//static CLgsTableSet* m_pTab40 = NULL;
//static CLgsTableSet* m_pTab50 = NULL;

// only this #of tran modules is allowed in one process space
// All modules should run in different modes
// Possible senarios:
// 1. tran1, tran2, tran3, tran4
// 1. parse1, parse2, parse3, parse4, tran1, tran2, tran3, tran4
//
#define MAX_TRAN_MODULES 8
static THREAD_ID tranTid[MAX_TRAN_MODULES];
static tran_data_struct* tranData[MAX_TRAN_MODULES];

// this function is called once in ON_PROCESS_ATTATCH handler
extern bool tran_rule_io_init()
{
// 	InitializeCriticalSection(&__csec);
// 	InitializeCriticalSection(&__csec40[0]);
// 	InitializeCriticalSection(&__csec50[0]);
// 	InitializeCriticalSection(&__csec40[1]);
// 	InitializeCriticalSection(&__csec50[1]);
	__csec.initialize();
	__csec40[0].initialize();
	__csec50[0].initialize();
	__csec40[1].initialize();
	__csec50[1].initialize();
	__refcnt40[0] = 0;
	__refcnt40[1] = 0;
	__refcnt50[0] = 0;
	__refcnt50[1] = 0;
	for ( int i = 0; i < MAX_TRAN_MODULES; i++ )
	{
		tranData[i] = new struct tran_data_struct();
		ZeroMemory(tranData[i], sizeof(struct tran_data_struct));
		tranTid[i] = ILLEGAL_THREAD_ID;
	}
	return true;
}

void tran_rule_io_terminate()
{
// 	DeleteCriticalSection(&__csec);
// 	DeleteCriticalSection(&__csec40[0]);
// 	DeleteCriticalSection(&__csec40[1]);
// 	DeleteCriticalSection(&__csec50[0]);
// 	DeleteCriticalSection(&__csec50[1]);
	for (int i = 0; i < MAX_TRAN_MODULES; i++)
	{
      if (tranData[i])
		   delete tranData[i];
	}
}

// convenience functions used internally by this module
static bool SetupTranData(int ix, THREAD_ID tid)
{
	tranTid[ix] = tid;
	return true;
}

static inline int GetTranIndexByThreadId(THREAD_ID tid)
{
	for ( int i = 0; i < MAX_TRAN_MODULES; i++ )
		if ( tranTid[i] == tid ) return i;
	throw("");
	return -1;
}

// Exported Functions
// ==================

// functions to access former common blocks negty1X_, and negtx1X_
extern "C" int lstng1(int ix)
{
	register int tranIx = GetTranIndexByThreadId(GetCurrentThreadId());
	return tranData[tranIx]->negt[0].lstng[ix];
}

extern "C" int numng1(int ix)
{
	register int tranIx = GetTranIndexByThreadId(GetCurrentThreadId());
	return tranData[tranIx]->negt[0].numng[ix];
}

extern "C" int lstnx1(int ix)
{
	register int tranIx = GetTranIndexByThreadId(GetCurrentThreadId());
	return tranData[tranIx]->negt[1].lstng[ix];
}

extern "C" int numnx1(int ix)
{
	register int tranIx = GetTranIndexByThreadId(GetCurrentThreadId());
	return tranData[tranIx]->negt[1].numng[ix];
}

static int max_rule_len=0, nrules=0;

/** _FIX_ME_
    The next three functions are duplicated in res_rule_io.cpp. They should
    really be FACTORED OUT! 
 */
static int getFlatCompRuleSetLength(CompRuleSet *p) {
	int compRuleSetSize = 0;
	char **ttt = (char **)((char *)p + sizeof(int));
	*ttt = (char *)p + sizeof(int)*2;
	char *raddr = (char *)p + sizeof(int)*2 + sizeof(CompRuleHdr *) * p->numRules;
	for(int i=0;i<p->numRules;i++) {
		CompRuleHdr *h = (CompRuleHdr *)raddr;
		raddr += h->len;
	}
	compRuleSetSize = raddr - (char *)p;
	return compRuleSetSize;
}

static int getCompRuleSetLength(CompRuleSet *p) {
	int compRuleSetSize = 0;
	compRuleSetSize += sizeof(int) * 2; // numRules
	compRuleSetSize += sizeof(CompRuleHdr *) * p->numRules;
	for(int i=0;i<p->numRules;i++) {
		CompRuleHdr *h = p->r[i];
		compRuleSetSize += h->len;
		nrules++;
		if(max_rule_len < h->len) {
			max_rule_len = h->len;
		}
	}
	return compRuleSetSize;
}

#ifdef _MSC_VER
static char *
mapChunk(char *mem_name, int sz) {

	HANDLE hShmem = CreateFileMapping(
		(HANDLE)0xFFFFFFFF,		// file to be mapped is in paging
		NULL,					// no security attribs
		PAGE_READWRITE,
		0,						// size high 32 bits
		sz,						// size low
		mem_name
	);

	if(hShmem == NULL) {
		int errCode = GetLastError();
		if(errCode==ERROR_ALREADY_EXISTS) {
//			printf("\"%s\" already exists\n", mem_name);
		} else {
			printf("getMappingChunk \"%s\" (sz=%d) mapping error: %d\n", 
				mem_name, sz, errCode);
			fflush(stdout);
			return NULL;
		}
	}

	char *ret = (char *)MapViewOfFile(hShmem, FILE_MAP_WRITE, 0, 0, 0);
	if(ret==NULL) {
		printf("MapViewOfFile %s: errCode = %d\n",
			mem_name, GetLastError());
	}

//printf("mapChunk: ret=%x \"%s\" %d\n", ret, mem_name, sz);
//fflush(stdout);
//printf("\"%s\", max_rule_len=%d, nrules=%d, size=%10.2f / %10.2f\n", 
//	mem_name, max_rule_len, nrules, max_rule_len*nrules/1024., sz/1024.);
//fflush(stdout);
//max_rule_len = nrules = 0;
	
	return ret;
}
#endif

#ifdef HAVE_MMAP
static char *mapChunk(char *mem_name, int sz) {
  return TranMManager.mapChunk(mem_name, sz);
}

static bool isMapped(char *mem_name) {
  return TranMManager.isMapped(mem_name);
}

static char *getMappedChunk(char *mem_name) {
  return TranMManager.getMappedChunk(mem_name);
}

static void UnmapViewOfFile(void *address) {
  TranMManager.unloadChunk(address);
}
#endif

/** _fix_me_ The next two are almost the same fn as in res_rule_io.cpp.
    FACTOR OUT! */
static CompRuleSet *
compactRuleSetInMemory(CompRuleSet *p, char *mem_name)
{
// here we just pack CompRuleSet to be contiguous
// in memory (to allow memory-mapping for use
// across processes).

	int len = getCompRuleSetLength(p);
	char *cp = mapChunk(mem_name, len);
//printf("CompRuleSet size = %d\n", len);
//fflush(stdout);

	CompRuleSet *newSet = (CompRuleSet *)cp;
	newSet->numRules = p->numRules;
	newSet->r = (CompRuleHdr **)(cp + sizeof(int) + sizeof(CompRuleHdr **));
	char *raddr = cp + sizeof(int)*2 + sizeof(CompRuleHdr *) * p->numRules;
	for(int i=0;i<p->numRules;i++)
   {
		CompRuleHdr *h = p->r[i];
		CopyMemory(raddr, h, h->len);
		newSet->r[i] = (CompRuleHdr *)raddr;
		raddr += h->len;
	}
	FreeCompRuleSet(p);
//printf("CompRuleSet done # rules = %d\n", newSet->numRules);
//fflush(stdout);
	return newSet;
}

static void
adjustPointers(CompRuleSet *p) {
// from logos_batch we're getting memory mapped
// packed (contiguous in memory) CompRuleSets which unfortunately are of
// complex structure - w/ pointers to the rules
// inside.

	p->r = (CompRuleHdr **)((char *)p + sizeof(int) + sizeof(CompRuleHdr **));

	char *raddr = (char *)p + sizeof(int)*2 + sizeof(CompRuleHdr *) * p->numRules;
	for(int i=0;i<p->numRules;i++) {
		CompRuleHdr *h = (CompRuleHdr *)raddr;
		p->r[i] = h;
		raddr += h->len;
	}
}

static void
write_comp_rule_set(CompRuleSet *p, char *name) {
	int fd = _open(name, O_RDWR | O_CREAT | O_TRUNC, 0644);
	for(int i=0;i<p->numRules;i++) {
		_write(fd, p->r[i], p->r[i]->len);
	}
	_close(fd);
}

static void
fwrite_comp_rule_set(CompRuleSet *p, char *name) {
	FILE *fp = fopen(name, "w");
//	char *cp = (char *)p + sizeof(int)*2 + sizeof(CompRuleHdr *) * p->numRules;
	for(int i=0;i<p->numRules;i++) {
/*
		CompRuleHdr *ip = (CompRuleHdr *)cp;
		fprintf(fp, "%d %d\n", i, ip->len);
		cp += ip->len;
*/
		fprintf(fp, "%d %d %d %d %d \"%s\"\n\t", i, p->r[i]->len, p->r[i]->ruleId,
			p->r[i]->tOff, p->r[i]->vOff, p->r[i]->comment);
		for(int j=0;j<RULE_NUMS;j++)
			fprintf(fp, "%d ", p->r[i]->rule[j]);
		fprintf(fp, "\n");
	}
	fclose(fp);
}

static void
fwrite_typos(typos_type *p, char *name) {
	FILE *fp = fopen(name, "w");
	for(int i=0;i<RULE_TABLES;i++) {
		for(int j=0;j<20;j++) {
			for(int k=0;k<IDX1_NUMS;k++) {
				fprintf(fp, "%d %d %d: %d\n", i, j, k, (*p)[i][j][k]);
			}
		}
	}
	fclose(fp);
}
static void
fwrite_noftyp(noftyp_type *p, char *name) {
	FILE *fp = fopen(name, "w");
	for(int i=0;i<RULE_TABLES;i++) {
		for(int j=0;j<20;j++) {
			for(int k=0;k<IDX2_NUMS;k++) {
				fprintf(fp, "%d %d %d: %d\n", i, j, k, (*p)[i][j][k]);
			}
		}
	}
	fclose(fp);
}

static void
make_good_name(const char *filename, char *chunk_name, char *result) {
	if(filename)
		(void) strcpy(result, filename);
	else
		result[0] = '\0';

	char *cp = result;
	while(1) {
		cp = strchr(cp, DIR_SEP_CHAR);
		if(!cp) break;
		*cp = '.';
		cp++;
	}
	(void) strcat(result, chunk_name);
}

#ifdef _MSC_VER
/** __fix_me_ The next two fn's are identical to those in res_rule_io.cpp
    FACTOR OUT!
 */
static bool
isMapped(char *mem_name) {
	HANDLE hShmem = OpenFileMapping(FILE_MAP_READ, 0, mem_name);
	bool ret = (hShmem!=NULL);
	if(ret)
		CloseHandle(hShmem);
//printf("\"%s\" isMapped: %d\n", mem_name, ret);
//fflush(stdout);
	return ret;
}

static char *
getMappedChunk(char *mem_name) {
	HANDLE hShmem = OpenFileMapping(FILE_MAP_WRITE, 0, mem_name);
	if(hShmem==NULL) {
		printf("\"%s\" getMappedChunk: %d\n", mem_name, GetLastError());
		fflush(stdout);
		return NULL;
	}
	char *ret = (char *)MapViewOfFile(hShmem, FILE_MAP_WRITE, 0, 0, 0);
	if(ret==NULL) {
		printf("MapViewOfFile %s: errCode = %d\n",
			mem_name, GetLastError());
	}
//printf("\"%s\" getMappedChunk: %x\n", mem_name, ret);
//fflush(stdout);
	CloseHandle(hShmem);
	return ret;
}
#endif

extern "C" int
rule_load(int tran_number, int miniflg, int twoPasses, int pass_number)
{
  int ret;

  // logic is:
  //	  - use memory-mapped cache (held by CacheMgr process) if
  //	    mini's are not used and, obviously, if CacheMgr is running
  //	  - otherwise load whole thing as earlier
  char ruleSetFileName[MAX_FILEPATH_LEN];
  char envVar[256];
  bool isParse = twoPasses && 1 == pass_number;
  //char* base = isParse ? "LGS_PARSE" : "LGS_TRAN";
  const char* base = isParse ? "parse" : "tran";
  int tranIx = tran_number -1 + (isParse? 4 : 0);

  if ( tranIx < 0 || tranIx > MAX_TRAN_MODULES-1 )
    return 1;

  SetupTranData(tranIx, GetCurrentThreadId());

  envVar[0] = 0;
  strcat(envVar, base);
  char bbb[256];
  strcat(envVar, itoa(tran_number, bbb, 10));

  if (GetConfigData("trandata", envVar, ruleSetFileName, MAX_FILEPATH_LEN) != CONFDATA_SUCCESS)
    {
      return 1;
    }

  char mem_tran_name[1024];
  make_good_name(ruleSetFileName, "-CompRuleSet", mem_tran_name);

  bool bMapped = isMapped(mem_tran_name);

  //   printf("miniflg = %d, bMapped = %d\n", miniflg, bMapped);
  if (miniflg || !bMapped)
    ret = rule_load_original(base, ruleSetFileName, tran_number, miniflg, isParse);
  else
    ret = rule_load_from_cache(base, ruleSetFileName, tran_number, isParse);

  if( ret != 0 )
    {
      printf("Error loading tran%i ruleset file %s\n", tran_number, ruleSetFileName);
      fflush(stdout);
    }

  return ret;
}

extern "C" int
rule_load_original(const char *base, char *ruleset_filename, int tran_number, int miniflg, bool isParse) 
{
  char tble30FileName[MAX_FILEPATH_LEN];
  char fileName[MAX_FILEPATH_LEN];
  int result = 0; // success
  char envVar[256];
  char bbb[256];
  int tranIx = tran_number -1 + (isParse? 4 : 0);

  //printf("tran.load_rule_original.ruleset_filename \"%s\"\n", ruleset_filename);
  //fflush(stdout);
  // serialize initializations of t1-t4 modules
  // (because memory usage is high during initialization)
  //EnterCriticalSection(&__csec);
  __csec.enter();

  try
    {
      //t0 = GetTickCount();

      if (!(tranData[tranIx]->m_pTran = LoadRuleSet(ruleset_filename)))
        {
          result = 1;
        }
      else
        {
          int typos_size = RULE_TABLES*20*IDX1_NUMS;
          int noftyp_size = RULE_TABLES*20*IDX2_NUMS;
          // XXX ???
          // If we allocate sizeof(short)*typos_size bytes - it results
          // in wrong indexes somehow.
          tranData[tranIx]->typos = (typos_type *) new short[typos_size*8];
          tranData[tranIx]->noftyp = (noftyp_type *) new short[noftyp_size*8];
          tranData[tranIx]->negt = (struct neg_type_idx3 *) new char[sizeof(struct neg_type_idx3)*RULE_TABLES];
        }

      envVar[0] = 0;
      strcat(envVar, base);
      strcat(envVar, itoa(tran_number, bbb, 10));
      strcat(envVar, "_table30");

      if (GetConfigData("trandata", envVar, tble30FileName, MAX_FILEPATH_LEN) != CONFDATA_SUCCESS)
        {
          return 1;
        }

      if (!(tranData[tranIx]->m_pTab30 = LoadTableSet(tble30FileName, true)))
        {
          result = 1;
        }

      // tran mini rules, and tables
      if (miniflg)
        {
          // rules
          envVar[0] = 0;
          strcat(envVar, base);
          strcat(envVar, itoa(tran_number, bbb, 10));
          strcat(envVar, "_mini");
          if (GetConfigData("trandata", envVar, fileName, MAX_FILEPATH_LEN) != CONFDATA_SUCCESS)
            {
              result = 1;
            }
          else
            {
              if (!(tranData[tranIx]->m_pTranMini = LoadRuleSet(fileName)))
                {
                  result = 1;
                }
              else
                {
                  SortRuleSet(tranData[tranIx]->m_pTranMini, RST_TRAN);

                  char temp[256];
                  temp[0] = 0;
                  strcat(temp, isParse? "Parse" : "Tran");
                  strcat(temp, itoa(tran_number, bbb, 10));
                  strcat(temp, " Mini");
                  OutputMsg(temp, fileName);
                }
            }

          // 30, 40, 50 tab minis
          // WARNING:
          //   LGS_TRAN_TBL40_MINI, LGS_TRAN_TBL50_MINI
          //   LGS_PARSE_TBL40_MINI
          //
          //   The same env var is used for all trans
          //   This could result in confusion
          for (int k = 0; k < 3; k ++)
            {
              envVar[0] = 0;
              if ((10*k+3) != 50)
                {
                  strcat(envVar, base);
                }
              if (0 == k)
                {
                  strcat(envVar, itoa(tran_number, bbb, 10));
                }
              strcat(envVar, "_table");
              strcat(envVar, itoa(10*(k+3), bbb, 10));
              strcat(envVar, "_mini");
              if (GetConfigData("trandata", envVar, fileName, MAX_FILEPATH_LEN) == CONFDATA_SUCCESS)
                {
                  tranData[tranIx]->m_pTabMini[k] = LoadTableSet(fileName, false);
                }
            }
        }

      // build all tran indexes, including mini
      if (!BuildIndexes(tranIx, miniflg))
        result = 1;
      /*
        char fff[256];
        sprintf(fff, "comp_rule_set.%-d.1", tranIx);
        if(tranData[tranIx] && tranData[tranIx]->m_pCTran)
	fwrite_comp_rule_set(tranData[tranIx]->m_pCTran, fff);
        sprintf(fff, "typos.%-d.1", tranIx);
        if(tranData[tranIx] && tranData[tranIx]->typos)
	fwrite_typos(tranData[tranIx]->typos, fff);
        sprintf(fff, "noftyp.%-d.1", tranIx);
        if(tranData[tranIx] && tranData[tranIx]->noftyp)
	fwrite_noftyp(tranData[tranIx]->noftyp, fff);
      */
      // 40 tables
      if (!m_pTab40[(tranIx>3?1:0)])
        {
          envVar[0] = 0;
          strcat(envVar, base);
          strcat(envVar, "_table40");
          if (GetConfigData("trandata", envVar, fileName, MAX_FILEPATH_LEN) != CONFDATA_SUCCESS)
            {
              result = 1;
            }
          else if (!(m_pTab40[(tranIx>3?1:0)] = LoadTableSet(fileName, true)))
            {
              result = 1;
            }

          if (m_pTab40[(tranIx>3?1:0)])
            {
              __refcnt40[(tranIx>3?1:0)]++;
            }
        }
      else
        {
          __refcnt40[(tranIx>3?1:0)]++;
        }

      // 50 tables
      if (!isParse && !m_pTab50[(tranIx>3?1:0)])
        {
          envVar[0] = 0;
          if (GetConfigData("trandata", "table50", fileName, MAX_FILEPATH_LEN) != CONFDATA_SUCCESS)
            {
              result = 1;
            }
          else if (!(m_pTab50[(tranIx>3?1:0)] = LoadTableSet(fileName, true)))
            {
              result = 1;
            }

          if (m_pTab50[(tranIx>3?1:0)])
            {
              __refcnt50[(tranIx>3?1:0)]++;
            }
        }
      else
        {
          __refcnt50[(tranIx>3?1:0)]++;
        }

      // compact and clean up
      if (tranData[tranIx]->m_pTran)
        {
          tranData[tranIx]->m_pCTran = CompactRuleSet(tranIx, 1, tranData[tranIx]->m_pTran);
          delete ( tranData[tranIx]->m_pTran);
          tranData[tranIx]->m_pTran = NULL;
        }

      if (tranData[tranIx]->m_pTranMini)
        {
          tranData[tranIx]->m_pCTranMini = CompactRuleSet(tranIx, 2, tranData[tranIx]->m_pTranMini);
          delete ( tranData[tranIx]->m_pTranMini );
          tranData[tranIx]->m_pTranMini = NULL;
        }

      tranData[tranIx]->original = true;
    }
  catch(...)
    {
      //t1 = GetTickCount();
      //LeaveCriticalSection(&__csec);
      __csec.leave();
    }
  //t1 = GetTickCount();
  //LeaveCriticalSection(&__csec);
  __csec.leave();

  //tRuleLoad = t1 - t0;
  //printf("tran %d load time %f\n", tranIx, tRuleLoad/1000.);
  //fflush(stdout);
    
  return result;
}

// ---------------------------------------------------
// This is supposed to be called from client-side only
// and only in case w/o mini's.
// There is no checks in the code whether CacheMgr is running.
// Caller has the responsibility to check it out.
extern "C" int
rule_load_from_cache(const char *base, char *ruleset_filename, int tran_number, bool isParse) 
{
	char mem_typos_name[1024];
   char mem_noftyp_name[1024];
   char mem_negt_name[1024];
	char mem_tran_name[1024];
   char fileName[MAX_FILEPATH_LEN];
   char tbl30FileName[MAX_FILEPATH_LEN];
	int result = 0; // success
	char envVar[256];
	char bbb[256];
	int tranIx = tran_number -1 + (isParse? 4 : 0);

//printf("tran.load_rule_cache.ruleset_filename \"%s\"\n", ruleset_filename);
//fflush(stdout);

	unmap(tranIx);

	// serialize initializations of t1-t4 modules
	// (because memory usage is high during initialization)
	//EnterCriticalSection(&__csec);
        __csec.enter();

	try
          {
//t0 = GetTickCount();

		// tran main rules, and 30 tables
		make_good_name(ruleset_filename, "-CompRuleSet", mem_tran_name);
		make_good_name(ruleset_filename, "-typos", mem_typos_name);
		make_good_name(ruleset_filename, "-noftyp", mem_noftyp_name);
		make_good_name(ruleset_filename, "-negt", mem_negt_name);

		char *cp = getMappedChunk(mem_tran_name);
		CompRuleSet *tmp = (CompRuleSet *)cp;
		int lll = getFlatCompRuleSetLength(tmp);
		tranData[tranIx]->m_pCTran = (CompRuleSet *) new char[lll];
		memcpy(tranData[tranIx]->m_pCTran, tmp, lll);
		adjustPointers(tranData[tranIx]->m_pCTran);
		UnmapViewOfFile(cp);

		tranData[tranIx]->typos = (typos_type *)getMappedChunk(mem_typos_name);
		tranData[tranIx]->noftyp = (noftyp_type *)getMappedChunk(mem_noftyp_name);
		tranData[tranIx]->negt = (struct neg_type_idx3 *)getMappedChunk(mem_negt_name);

		envVar[0] = 0;
		strcat(envVar, base);
		strcat(envVar, itoa(tran_number, bbb, 10));
		strcat(envVar, "_table30");
        
      if (GetConfigData("trandata", envVar, tbl30FileName, MAX_FILEPATH_LEN) != CONFDATA_SUCCESS)
      {
         result = 1;
      }
      else if (!(tranData[tranIx]->m_pTab30 = LoadTableSet(tbl30FileName, true)))
      {
         result = 1;
      }

		// 40 tables
		if (!m_pTab40[(tranIx>3?1:0)])
		{
         envVar[0] = 0;
         strcat(envVar, base);
         strcat(envVar, "_table40");
         if (GetConfigData("trandata", envVar, fileName, MAX_FILEPATH_LEN) != CONFDATA_SUCCESS)
         {
            result = 1;
         }
         else if (!(m_pTab40[(tranIx>3?1:0)] = LoadTableSet(fileName, true)))
         {
            result = 1;
         }

         if (m_pTab40[(tranIx>3?1:0)])
         {
            __refcnt40[(tranIx>3?1:0)]++;
         }
		}
		else
      {
			__refcnt40[(tranIx>3?1:0)]++;
      }

		// 50 tables
		if (!isParse && !m_pTab50[(tranIx>3?1:0)])
		{
         envVar[0] = 0;
         if (GetConfigData("trandata", "table50", fileName, MAX_FILEPATH_LEN) != CONFDATA_SUCCESS)
         {
            result = 1;
         }
         else if (!(m_pTab50[(tranIx>3?1:0)] = LoadTableSet(fileName, true)))
         {
            result = 1;
         }

         if (m_pTab50[(tranIx>3?1:0)])
         {
            __refcnt50[(tranIx>3?1:0)]++;
         }
      }
      else
      {
         __refcnt50[(tranIx>3?1:0)]++;
      }

		// compact rulesets

		if (tranData[tranIx]->m_pTran)
		{
			delete (tranData[tranIx]->m_pTran);
			tranData[tranIx]->m_pTran = NULL;
		}
		tranData[tranIx]->original = false;
	}
        catch(...)
	{
//t1 = GetTickCount();
          //LeaveCriticalSection(&__csec);
          __csec.leave();
	}
//t1 = GetTickCount();
        //LeaveCriticalSection(&__csec);
        __csec.leave();

//tRuleLoad = t1 - t0;
//printf("tran %d load time %f\n", tranIx, tRuleLoad/1000.);
//fflush(stdout);
	return result;
}

// this is supposed to be called from CacheMgr
// to initialize rulesets (but not tables) and build indexes.

extern "C" DLLEXPORT int
rule_load_into_cache(char *ruleset_filename, int tran_number)
{
	char mem_typos_name[1024], mem_noftyp_name[1024], mem_negt_name[1024];
	char mem_tran_name[1024];
	int result = 0; // success
	int tranIx = tran_number -1;

	if ( tranIx < 0 || tranIx > MAX_TRAN_MODULES-1 )
		return 1;
	if ( !ruleset_filename || !ruleset_filename[0] ) {
		OutputMsg(ruleset_filename, "bad ruleset_filename");
		return 1;
	} 

	make_good_name(ruleset_filename, "-CompRuleSet", mem_tran_name);
	make_good_name(ruleset_filename, "-typos", mem_typos_name);
	make_good_name(ruleset_filename, "-noftyp", mem_noftyp_name);
	make_good_name(ruleset_filename, "-negt", mem_negt_name);

	if (!(tranData[tranIx]->m_pTran = LoadRuleSet(ruleset_filename))) {
			return 1;
	}
/*ocr*/
#ifdef TRANSTAT
/* 
	to print (and possibly delete) rules that are not being hit.
*/
char fname[256];
sprintf(fname, "c:/ocr/misc/transtat/merge/merged%d.de", tran_number);
printf("======= %s ====== \n", fname);
int fd = _open(fname, O_RDONLY|O_BINARY);
if(fd==-1) { perror(fname);}
int rulestat[20000];
_read(fd, rulestat, 80000);
CLgsRuleSet* t = tranData[tranIx]->m_pTran;
int nr = t->NumberOfRules();
int ndel = 0;
printf("Source Language = %s\n", t->Source());
printf("Target Language = %s\n", t->Target());
printf("# of rules = %d\n", nr);
for(int i=nr;i>0;i--) {
	if(rulestat[i]==0) {
		ndel++;
		CLgsRule *r = t->GetItem(i-1);
// Extended print of not hit rules
		printf("Rule ID = %d, Comment = \"%s\"\n", r->id(), 
			r->CommentDisplayString(false));
//		printf("VTR=\"%s\"\n", r->VtrsDisplayString(false, false));
//		t->Delete(r);
	}
}
_close(fd);
if(ndel)
printf("%d rules deleted according to \"%s\"\n", ndel, fname);
#endif /* TRANSTAT */
	
	tranData[tranIx]->m_pCTran =
		CompactRuleSet(tranIx, 1, tranData[tranIx]->m_pTran);

	tranData[tranIx]->m_pCTran =
		compactRuleSetInMemory(tranData[tranIx]->m_pCTran, mem_tran_name);

	tranData[tranIx]->typos = 
		(typos_type *)mapChunk(mem_typos_name, sizeof(typos_type));
	tranData[tranIx]->noftyp = 
		(noftyp_type *)mapChunk(mem_noftyp_name,sizeof(noftyp_type));
	tranData[tranIx]->negt = 
		(struct neg_type_idx3 *)mapChunk(mem_negt_name, 
			sizeof(struct neg_type_idx3)*RULE_TABLES);
	if ( !BuildIndexes(tranIx, 0) )
		result = 1;

	if ( tranData[tranIx]->m_pTran ) {
		delete ( tranData[tranIx]->m_pTran );
		tranData[tranIx]->m_pTran = NULL;
	}
	return result;
}

extern "C" int
rule_unload()
{
  int result = 0; // success

  register int tranIx = GetTranIndexByThreadId(GetCurrentThreadId());
  if ( tranData[tranIx] )
    {
      // tab 30
      if ( tranData[tranIx]->m_pTab30 )
        delete ( tranData[tranIx]->m_pTab30 );

      // main and mini rules
      if ( tranData[tranIx]->m_pTran )
        delete ( tranData[tranIx]->m_pTran );
      if ( tranData[tranIx]->m_pTranMini )
        delete ( tranData[tranIx]->m_pTranMini );

      // compacted mini rules
      if ( tranData[tranIx]->m_pCTranMini )
        FreeCompRuleSet(tranData[tranIx]->m_pCTranMini);

      // mini 30 40 50 tabs
      for ( int k = 0; k < 3; k++ )
        {
          if ( tranData[tranIx]->m_pTabMini[k] )
            delete ( tranData[tranIx]->m_pTabMini[k] );
        }

      // main record
      //		delete ( tranData[tranIx] ), tranData[tranIx] = NULL;
    }

  // free 40 tables only if reference count reaches zero
  int i;
#ifdef _MSC_VER
  i = InterlockedDecrement(&__refcnt40[(tranIx>3?1:0)]);
#else
  pthread_mutex_lock(&interlock_mutex);
  i = --__refcnt40[(tranIx>3?1:0)];
  pthread_mutex_unlock(&interlock_mutex);
#endif
  if ( 0 == i )
    {
      //EnterCriticalSection(&__csec40[(tranIx>3?1:0)]);
      __csec40[(tranIx>3?1:0)].enter();
      try
        {
          if ( m_pTab40[(tranIx>3?1:0)] ) delete ( m_pTab40[(tranIx>3?1:0)] );
          m_pTab40[(tranIx>3?1:0)] = NULL;
        }
      catch(...)
        {
          //LeaveCriticalSection(&__csec40[(tranIx>3?1:0)]);
          __csec40[(tranIx>3?1:0)].leave();
        }
      //LeaveCriticalSection(&__csec40[(tranIx>3?1:0)]);
      __csec40[(tranIx>3?1:0)].leave();
    }

#ifdef _MSC_VER
  i = InterlockedDecrement(&__refcnt40[(tranIx>3?1:0)]);
#else
  pthread_mutex_lock(&interlock_mutex);
  i = --__refcnt50[(tranIx>3?1:0)];
  pthread_mutex_unlock(&interlock_mutex);
#endif
  // free 50 tables only if reference count reaches zero
  if ( 0 == i )
    {
      //EnterCriticalSection(&__csec50[(tranIx>3?1:0)]);
      __csec50[(tranIx>3?1:0)].enter();
      try
        {
          if ( m_pTab50[(tranIx>3?1:0)] ) delete ( m_pTab50[(tranIx>3?1:0)] );
          m_pTab50[(tranIx>3?1:0)] = NULL;
        }
      catch(...)
        {
          //LeaveCriticalSection(&__csec50[(tranIx>3?1:0)]);
          __csec50[(tranIx>3?1:0)].leave();
        }
      //LeaveCriticalSection(&__csec50[(tranIx>3?1:0)]);
      __csec50[(tranIx>3?1:0)].leave();
    }
	
  if(tranData[tranIx]->original) {
    FreeCompRuleSet(tranData[tranIx]->m_pCTran);
    delete [] (tranData[tranIx]->typos);
    delete [] (tranData[tranIx]->noftyp);
    delete [] (tranData[tranIx]->negt);
  } else
    unmap(tranIx);

  return result;
}

void unmap(int tranIx) {
//	if ( tranData[tranIx]->m_pCTran )
//		UnmapViewOfFile(tranData[tranIx]->m_pCTran);
	delete [] tranData[tranIx]->m_pCTran;

	if(tranData[tranIx]->typos) UnmapViewOfFile(tranData[tranIx]->typos);
	if(tranData[tranIx]->noftyp) UnmapViewOfFile(tranData[tranIx]->noftyp);
	if(tranData[tranIx]->negt) UnmapViewOfFile(tranData[tranIx]->negt);
}

extern "C" int
rulein(short* ruleset, short* matpos,
	   short rule_buffer[], short* rule_number, short* ret_flag)
{
	register int tranIx = GetTranIndexByThreadId(GetCurrentThreadId());
	if ( !tranData[tranIx] ) return 1;
	CompRuleSet* crs = 1 == *ruleset ? tranData[tranIx]->m_pCTran :
					 2 == *ruleset ? tranData[tranIx]->m_pCTranMini : NULL;
	if ( !crs ) return 1;

	int rulesize = RULE_NUMS*sizeof(short);
	short *prev_rule_buffer;

	*ret_flag = 0;

	// get the rule
	int ruleIx = *rule_number - 1;
	if ( ruleIx < 0 || ruleIx >= crs->numRules )
	{
//		ZeroMemory(rule_buffer, RULE_NUMS*sizeof(short));
		return 1;
	}
	CompRuleHdr* hdr = crs->r[ruleIx];
	CopyMemory(rule_buffer, hdr->rule, rulesize);

	// quick check of rule only if previous rule had been searched
	if ( 0 != *matpos )
	{
		// do not test wc 9 and 10 rules
		if ( 9 == rule_buffer[1] || 10 == rule_buffer[1] )
		{
			*matpos = 0;
		}
		else
		{				
			// get the previous rule
			int prevRuleIx = *rule_number;
			if ( prevRuleIx < 0 || prevRuleIx >= crs->numRules )
			{
				*matpos = 0;
			}
			else
			{
				CompRuleHdr* hdr = crs->r[prevRuleIx];
				prev_rule_buffer = hdr->rule;

				for ( int pt = 1; pt <= *matpos; pt++ )
				{
					// if wc is 50 of higher then end becuase of
					// stretch zone is starting.
					if ( pt == 4 || pt == 7 )
					{
						if ( rule_buffer[pt] > 50 )
						{
							*matpos = pt;
							break;
						}
					}

					// force type elements referring to tagsets to be different
					if ( (2 == pt || 5 == pt || 8 == pt) &&
						 ( rule_buffer[pt] < -2 || prev_rule_buffer[pt] < -2)
						)
					{
						*matpos = pt - 1;
						break;
					}

					// force overflow indicators to be different
					// (although literally they are the same)
					if ( 10 == pt )
					{
						*matpos = pt - 1;
						break;
					}

					// does current value = value in previous rule?
					if ( rule_buffer[pt] != prev_rule_buffer[pt] )
					{
						*matpos = pt - 1;
						break;
					}

					// if matched all up to position then set flag
					// back to caller to indicate rule will fail as well
					if ( pt == *matpos )
					{
						*ret_flag = 99;
						break;
					}
				}
			}
		}
	}

	return 0;
}


struct t_spX_ {
	short int *sp;
};

extern "C" DLLEXPORT int
rulein_ptr(short* ruleset, short* matpos,
	   struct t_spX_ *spp, short* rule_number, short* ret_flag)
{
	register int tranIx = GetTranIndexByThreadId(GetCurrentThreadId());
	if ( !tranData[tranIx] ) return 1;
	CompRuleSet* crs = 1 == *ruleset ? tranData[tranIx]->m_pCTran :
					 2 == *ruleset ? tranData[tranIx]->m_pCTranMini : NULL;
	if ( !crs ) return 1;

	int rulesize = RULE_NUMS*sizeof(short);
	short *prev_rule_buffer;

	*ret_flag = 0;

	// get the rule
	int ruleIx = *rule_number - 1;
	if ( ruleIx < 0 || ruleIx >= crs->numRules ) {
		return 1;
	}

	CompRuleHdr* hdr = crs->r[ruleIx];
	spp->sp = hdr->rule;

	// quick check of rule only if previous rule had been searched
	if ( 0 != *matpos ) {
		// do not test wc 9 and 10 rules
		if ( 9 == spp->sp[1] || 10 == spp->sp[1] ) {
			*matpos = 0;
		} else {				
			// get the previous rule
			int prevRuleIx = *rule_number;
			if ( prevRuleIx < 0 || prevRuleIx >= crs->numRules ) {
				*matpos = 0;
			} else {

				CompRuleHdr* hdr = crs->r[prevRuleIx];
				prev_rule_buffer = hdr->rule;

				for ( int pt = 1; pt <= *matpos; pt++ ) {
					// if wc is 50 of higher then end becuase of
					// stretch zone is starting.
					if ( pt == 4 || pt == 7 ) {
						if ( spp->sp[pt] > 50 ) {
							*matpos = pt;
							break;
						}
					}

					// force type elements referring to tagsets to be different
					if ( (2 == pt || 5 == pt || 8 == pt) &&
						 ( spp->sp[pt] < -2 || prev_rule_buffer[pt] < -2)
						) {
						*matpos = pt - 1;
						break;
					}

					// force overflow indicators to be different
					// (although literally they are the same)
					if ( 10 == pt ) {
						*matpos = pt - 1;
						break;
					}

					// does current value = value in previous rule?
					if ( spp->sp[pt] != prev_rule_buffer[pt] ) {
						*matpos = pt - 1;
						break;
					}

					// if matched all up to position then set flag
					// back to caller to indicate rule will fail as well
					if ( pt == *matpos ) {
						*ret_flag = 99;
						break;
					}
				}
			}
		}
	}

	return 0;
}

extern "C" int
ovrin(short* ruleset, char* rule_buffer, short rule_number, short part_number)
{
	register int tranIx = GetTranIndexByThreadId(GetCurrentThreadId());
	if ( !tranData[tranIx] ) return 1;
	CompRuleSet* crs = 1 == *ruleset ? tranData[tranIx]->m_pCTran :
					 2 == *ruleset ? tranData[tranIx]->m_pCTranMini : NULL;
	if ( !crs ) return 1;

	int result = 1; // error
	ZeroMemory(rule_buffer, OVR_NUMS*sizeof(short));

	int ruleIx = rule_number - 1;
	if ( ruleIx < 0 || ruleIx >= crs->numRules ) return result;
	CompRuleHdr* hdr = crs->r[ruleIx];

	if ( OVERFLOW_RECORD == part_number )
	{
		// overflow record request
		if ( hdr->tOff != sizeof(CompRuleHdr) )
		{
			CopyMemory(rule_buffer,
				(char*)hdr + sizeof(CompRuleHdr), OVR_NUMS*sizeof(short));
			result = 0; // success
		}
	}
	else
	{
		int reqPartIx = part_number - TAGSET_PART_BASE;
		short off = hdr->tOff + reqPartIx*OVR_NUMS*sizeof(short);
		if ( off < hdr->vOff )
		{
			CopyMemory(rule_buffer, (char*)hdr + off, OVR_NUMS*sizeof(short));
			result = 0; // success
		}
	}

	return result;
}

extern "C" int
vtrin(short* ruleset, short* rule_buffer, short* rule_number, short part_number)
{
	register int tranIx = GetTranIndexByThreadId(GetCurrentThreadId());
	if ( !tranData[tranIx] ) return 1;
	CompRuleSet* crs = 1 == *ruleset ? tranData[tranIx]->m_pCTran :
					 2 == *ruleset ? tranData[tranIx]->m_pCTranMini : NULL;
	if ( !crs ) return 1;

	int result = 1; // error
	//ZeroMemory(rule_buffer, VTR_NUMS*sizeof(short));

	int ruleIx = *rule_number - 1;
	if ( ruleIx < 0 || ruleIx >= crs->numRules ) return result;
	CompRuleHdr* hdr = crs->r[ruleIx];

	int reqPartIx = part_number - *rule_number;
	short off = hdr->vOff + reqPartIx*VTR_NUMS*sizeof(short);
	if ( off < hdr->len )
	{
		CopyMemory(rule_buffer, (char*)hdr + off, VTR_NUMS*sizeof(short));
		result = 0; // success
	}

	return result;
}

extern "C" int
comin(short* ruleset, char* rule_buffer, short rule_number)
{
	register int tranIx = GetTranIndexByThreadId(GetCurrentThreadId());
	if ( !tranData[tranIx] ) return 1;
	CompRuleSet* crs = 1 == *ruleset ? tranData[tranIx]->m_pCTran :
					 2 == *ruleset ? tranData[tranIx]->m_pCTranMini : NULL;
	if ( !crs ) return 1;

	int ruleIx = rule_number - 1;
	if ( ruleIx < 0 || ruleIx >= crs->numRules ) return 1;
	CompRuleHdr* hdr = crs->r[ruleIx];

	CopyMemory(rule_buffer, hdr->comment, COM_CHARS);
	return 0;
}

extern "C" void
idxval(short* ruleset, short* swc, short* type, short* start, short* number)
{
	register int tranIx = GetTranIndexByThreadId(GetCurrentThreadId());
	*start = 0;
	*number = 0;
	if ( *type > 0 && *type < 1000)
	{
		*start  = (*(tranData[tranIx]->typos))[*ruleset-1][*swc-1][*type-1];
		if (*start > 0 )
		{
		   *number = (*(tranData[tranIx]->noftyp))[*ruleset-1][*swc-1][*type-1];
		}
	}
}

extern "C" void
vtrld(
	  int* ruleset,				// 1 - main, 2 - mini
	  short* rule_number,		// rule# now (was rec#)
	  short* retsw,				// 0 - ok, non-0 - bad
	  short* vtrs,		// output buffer with vtrs (was common blk)
	  struct diag_vtr_struct* vtrsDiag	// diag copy of vtrs (can be NULL)
	  )
{
	*retsw = 1; // error
	if ( vtrsDiag )
	{
		vtrsDiag->dgtag  = 0;
		vtrsDiag->dgtagx = 0;
	}

	if ( !vtrs ) return;
	//ZeroMemory( vtrs, VTRS_TAB_NUMS * sizeof(short) );

	register int tranIx = GetTranIndexByThreadId(GetCurrentThreadId());
	if ( !tranData[tranIx] ) return;
	CompRuleSet* crs = 1 == *ruleset ? tranData[tranIx]->m_pCTran :
					 2 == *ruleset ? tranData[tranIx]->m_pCTranMini : NULL;
	if ( !crs ) return;

	int ruleIx = *rule_number - 1;
	if ( ruleIx < 0 || ruleIx >= crs->numRules ) return;
	CompRuleHdr* hdr = crs->r[ruleIx];

	// copy vtr numbers to output buffer
	int vtrBytes = ((hdr->len - hdr->vOff) / VTR_NUMS ) * VTR_NUMS;
	if ( vtrBytes > VTRS_TAB_NUMS*sizeof(short) )
		throw("");
	short* vtrs_temp = (short*)((char*)hdr + hdr->vOff);

        int i, k;
	for ( i = 0, k = 0; 999 != vtrs_temp[i]; i++ )
	{
		if ( 888 == vtrs_temp[i] && 888 == vtrs_temp[i+1] )
		{
			i++;
			continue;
		}

		vtrs[k++] = vtrs_temp[i];
	}
	vtrs[k] = 999;

	// copy vtrs to diag buffer
	if ( vtrsDiag )
	{
		if ( 1 == *ruleset )
		{
			CopyMemory((char*)vtrsDiag->dgvtr, (char*)hdr + hdr->vOff, vtrBytes);
			vtrsDiag->dgtag = vtrBytes / sizeof(vtrsDiag->dgvtr[0]);
		}
		else
		{
			CopyMemory((char*)vtrsDiag->dgvtrx, (char*)hdr + hdr->vOff, vtrBytes);
			vtrsDiag->dgtagx = vtrBytes / sizeof(vtrsDiag->dgvtrx[0]);
		}
	}

	// check for 30 tables
	for ( short* p = vtrs; *p != 999; p++ )
	{
		if( -63 == *p )
		{
			short qnum = *(p + 3);
			if ( qnum == 0 || qnum == 1 || qnum == 4 || qnum == 5 || qnum == 6 )
			{
				int tabNo = 1000 * *(p+1) + *(p+2);
				if ( !tab30exist(tabNo) )
					return;
			}
		}
	}

	*retsw = 0; // success
	return;
}

extern "C" void
vtr_t_ld(
	short* tblid,			// in: 1 - 30, 2 - 40, 3 - 50
	short* rule_number,		// in: tab#, not phys rec# as comment in old file says
	short* ret_vtrcnt,		// out: #of numbers returned
	short* ret_vtrtbl,		// out: array of vtr numbers
	short* ret_table_num,	// out: 1 - main, 2 - mini
	short* retsw			// out:
	)
{
	register int tranIx = GetTranIndexByThreadId(GetCurrentThreadId());
	if ( !tranData[tranIx] )
		throw ("");

	int whichTabset = *tblid - 1;
	if ( whichTabset < 0 || whichTabset > 2 )
		throw ("");

	*retsw = 0;
	*ret_vtrcnt = 0;
	*ret_table_num = 1; // mini

	// minis are of higher priority than mains
	CLgsTableSet* tabset = NULL;
	tabset = tranData[tranIx]->m_pTabMini[whichTabset];
	CLgsTable* tab = NULL;

	if ( tabset )
		tab = tabset->GetTable( *rule_number );

	if ( !tab )
	{
		// set main tabsets
		tabset = TAB30_MINI_IX == whichTabset ? tranData[tranIx]->m_pTab30 :
				 TAB40_MINI_IX == whichTabset ? m_pTab40[(tranIx>3?1:0)] : m_pTab50[(tranIx>3?1:0)];
		*ret_table_num = 0; // main
		if ( tabset )
			tab = tabset->GetTable( *rule_number );
	}

	if ( !tab )
	{
		// no such tab
		*retsw = 1;
		return;
	}

	// copy vtr nums to output buffer
	CLgsVtrStatement** vtrs;
	int numVtrs = tab->VtrStatements(&vtrs);

	if ( numVtrs < 1 )
	{
		ret_vtrtbl[0] = 999;
		*ret_vtrcnt = 1;
	}
	else
	{
		short* nums;
		int numNums = vtrs[0]->GetNumbers( &nums );
		if ( numNums +1 > VTRS_TAB_NUMS )
			throw("");
		CopyMemory(ret_vtrtbl, nums, numNums*sizeof(short));
		ret_vtrtbl[numNums] = 999;
		*ret_vtrcnt = numNums + 1;
	}
}

extern "C" void
print_tran_rule(FILE* file, int type, int rule_number)
{
	register int tranIx = GetTranIndexByThreadId(GetCurrentThreadId());
	if ( !tranData[tranIx] )
		return;
	CompRuleSet* crs = 1 == type ? tranData[tranIx]->m_pCTran :
					 2 == type ? tranData[tranIx]->m_pCTranMini : NULL;
	if ( !crs )
	{
		fprintf(file, "ERROR: print_tran_rule: invalid argument: type: %d\n", type);
		return;
	}

	int ruleIx = rule_number - 1;
	if ( ruleIx < 0 || ruleIx >= crs->numRules )
	{
		fprintf(file, "ERROR: print_tran_rule: rule number is out of range: %d\n", rule_number);
		return;
	}

	CompRuleHdr* hdr = crs->r[ruleIx];

	// print header
	fprintf(file, "\n%s rule #%d, ID: %d\n",
		1 == type ? "Tran" :
		 2 == type ? "Mini Tran" : "<unknown type>", rule_number, hdr->ruleId);

	// print comment
	fprintf(file, "%80.80s\n", hdr->comment);

	// print level and 1st 3 sps (don't print an sp if it is 0)
	fprintf(file, "%2d", hdr->rule[0]);
	fprintf(file, "  (%d %d %d)", hdr->rule[1],
			hdr->rule[2] < -2 ? -2 : hdr->rule[2], hdr->rule[3]);
	if ( hdr->rule[4] && hdr->rule[5] && hdr->rule[6] )
		fprintf(file, "  (%d %d %d)", hdr->rule[4],
			hdr->rule[5] < -2 ? -2 : hdr->rule[5], hdr->rule[6]);
	if ( hdr->rule[7] && hdr->rule[8] && hdr->rule[9] )
		fprintf(file, "  (%d %d %d)", hdr->rule[7],
			hdr->rule[8] < -2 ? -2 : hdr->rule[8], hdr->rule[9]);

	fprintf(file, "\n");

	// if there is an overflow sp record, print it
	if ( OVERFLOW_RECORD == hdr->rule[10] )
	{
		short* o = (short*)((char*)hdr + sizeof(CompRuleHdr));
		for ( int i = 0; i < 7; i++ )
		{
			if ( o[3*i] && o[3*i+1] && o[3*i+2] )
				fprintf(file, "  (%d %d %d)", o[3*i],
					o[3*i+1] < -2 ? -2 : o[3*i+1], o[3*i+2]);
		}
		fprintf(file, "\n");
	}

	// print tagsets if any
        short off;
	for ( off = hdr->tOff; off < hdr->vOff; off += OVR_NUMS*sizeof(short) )
	{
		fprintf(file, "  {");
		short* t = (short*)((char*)hdr + off);
		for ( int i = 0; i < OVR_NUMS; i++ )
			fprintf(file, " %4d", t[i]);
		fprintf(file, " }\n");
	}

	// print vtrs
	int inc = VTR_NUMS*sizeof(short);
	for ( off = hdr->vOff; off < hdr->len; off += inc )
	{
		fprintf(file, "  ");
		short* t = (short*)((char*)hdr + off);
		for ( int i = 0; i < VTR_NUMS; i++ )
		{
// 02.07.02 ocr
// the following 2 lines were put in because we're getting
// exceptions when printing some rules from tran4.
// E.g. rule # 1662 has length 172, offset is 116 and
// "inc" above is 52. Thus after 1-st time this loop
// would start printing from offset 168 and went
// beyond rule boundary which supposedly sometimes
// caused access violations.
			if( (char *)(&t[i]) >= (char *)hdr + hdr->len)
				break;

			if ( i == VTR_NUMS - 2 && 888 == t[i] && 888 == t[i+1] )
				break;
			if ( i > 0 &&
				 (-63 == t[i] || -56 == t[i] || -22 == t[i] ||
				  -66 == t[i] || -57 == t[i]) )
				fprintf(file, "\n  ");
			fprintf(file, " %3d", t[i]);
			if ( 999 == t[i] )
				break;
		}
		//fprintf(file, "\n");
	}
	fprintf(file, "\n");
}

extern "C" void
print_table(FILE* file, int type, int table_number, int numParms, short* parms)
{
	register int tranIx = GetTranIndexByThreadId(GetCurrentThreadId());
	if ( !tranData[tranIx] )
		throw ("");

	int whichTabset = type - 1;
	if ( whichTabset < 0 || whichTabset > 2 )
		throw ("");

	bool bParmSubstError = false;

	// minis are of higher priority than mains
	bool bMini = true;
	CLgsTableSet* tabset = NULL;
	tabset = tranData[tranIx]->m_pTabMini[whichTabset];
	CLgsTable* tab = NULL;

	if ( tabset )
		tab = tabset->GetTable( table_number );

	if ( !tab )
	{
		// set main tabsets
		tabset = TAB30_MINI_IX == whichTabset ? tranData[tranIx]->m_pTab30 :
				 TAB40_MINI_IX == whichTabset ? m_pTab40[(tranIx>3?1:0)] : m_pTab50[(tranIx>3?1:0)];
		bMini = false;
		if ( tabset )
			tab = tabset->GetTable( table_number );
	}

	if ( !tab )
	{
		// no such tab
		fprintf(file, "ERROR: print_table: no such table: %d\n", table_number);
		return;
	}

	// print header
	fprintf(file, "\n%s %s table #%d\n",
		bMini ? "Mini" : "Main",
		1 == type ? "30" : 2 == type ? "40" : "50", table_number);

	// print out vtr numbers
	CLgsVtrStatement** vtrs;
	int numVtrs = tab->VtrStatements(&vtrs);

	if ( numVtrs > 0 )
	{
		short* t;
		int numNums = vtrs[0]->GetNumbers( &t );

		fprintf(file, "  ");
		for ( int i = 0; i < numNums; i++ )
		{
			if ( i > 0 &&
				  (-63 == t[i] || -56 == t[i] || -22 == t[i] ||
					-66 == t[i] || -57 == t[i]) )
				fprintf(file, "\n  ");
			if ( t[i] < -1000 )
			{
				// fetch the parameter from the command line
				int parmIx = - t[i] - 1000 - 1;
				if ( parmIx < numParms )
					fprintf(file, " %3d", parms[parmIx]);
				else
				{
					bParmSubstError = true;
					fprintf(file, " %3d", t[i]); // leave as it is
				}
			}
			else
				fprintf(file, " %3d", t[i]);
		}
	}
	fprintf(file, " 999\n");

	if ( bParmSubstError )
	{
		fprintf(file,
			"At least one parameter substitution "
			"error occurred while printing out the above rule\n\n");
	}

	fprintf(file, "\n\n");
}


// local function, used only by this module
// ========================================

static bool
tab30exist(	int tabNo )
{
	register int tranIx = GetTranIndexByThreadId(GetCurrentThreadId());
	if ( !tranData[tranIx] )
		throw ("");

	if ( tranData[tranIx]->m_pTabMini[TAB30_MINI_IX] &&
		 tranData[tranIx]->m_pTabMini[TAB30_MINI_IX]->GetTable( tabNo ) )
		return true;

	if ( tranData[tranIx]->m_pTab30 &&
		 tranData[tranIx]->m_pTab30->GetTable( tabNo ) )
		return true;

	return false;
}

static bool BuildIndexes( int tranIx, int mini )
{
	int* startIndex;
	int* countIndex;
	int* startNegIndex;
	int* countNegIndex;
// FOLLOWING BLOCK IS ADDED TO FIX "RANDOM OUTPUTS" BUG
{
int abc, def, ghi;
for(abc=0;abc<2;abc++)
        {
          for(def=0;def<20;def++)
        {
          for(ghi=0;ghi<1024;ghi++)
          (*(tranData[tranIx]->typos))[abc][def][ghi]=0;
        }
        } 
}

	for ( int which = 0; which < (mini? 2 : 1); which++ )
	{
		CLgsRuleSet* pRset = which ? tranData[tranIx]->m_pTranMini :
									 tranData[tranIx]->m_pTran;

		if ( !pRset )
			return false;

		if ( !pRset->BuildIndex(
				&startIndex, &countIndex, &startNegIndex, &countNegIndex, false) )
			return false;

		int wcMin, wcMax, tyMin, tyMax;
		pRset->QueryBounds(&wcMin, &wcMax, &tyMin, &tyMax);

		int wcNum = wcMax - wcMin + 1;
		int tyNum = tyMax - tyMin + 1;

		if ( wcNum > 20 )
			throw("too many different word classes");
		if ( tyNum > 1024 )
			throw("too many different word types");

                int i;
		for ( i = 0; i < wcNum; i++ )
		{
			for ( int j = 0; j < tyNum; j++ )
			{
				int start = startIndex[tyNum*i + j];
				int count = countIndex[tyNum*i + j];
				if ( -1 != start && 0 != count )
				{
					if ( i + wcMin > 0 && j + tyMin > 0 )
					{
						(*(tranData[tranIx]->typos))
							[which][i + wcMin - 1][j + tyMin - 1] =
								start + count - 1 + 1;
						(*(tranData[tranIx]->noftyp))
							[which][i + wcMin - 1][j + tyMin - 1] = count;
					}
				}
			}
		}

		// to make memory dumps of these arrays in the old and
		// new system the same (see spgen/spgen.f)
		for ( i = 0; i < 20; i++ )
			(*(tranData[tranIx]->typos))[which][i][IDX1_NUMS - 1] = 1;

		// negative type rule index
		for ( i = 0; i < wcNum; i++ )
		{
			int start = startNegIndex[i];
			int count = countNegIndex[i];
			if ( -1 != start && 0 != count )
			{
				tranData[tranIx] ->
					negt[which].lstng[i + wcMin - 1] = start + count - 1 + 1;
				tranData[tranIx] ->
					negt[which].numng[i + wcMin - 1] = count;
			}
		}

		delete [] ( startIndex );
		delete [] ( countIndex );
		delete [] ( startNegIndex );
		delete []( countNegIndex );
	}

	return true;
}

static CompRuleSet* 
CompactRuleSet(int tranIx, int which, CLgsRuleSet* rs) {
	CompRuleSet* result = new CompRuleSet();
	result->numRules = rs->NumberOfRules();
	result->r = new CompRuleHdr*[result->numRules];
	ZeroMemory(result->r, sizeof(CompRuleHdr*) * result->numRules);

	char comment[COM_CHARS];
	short rule[RULE_NUMS];

	bool bO = false;
	short o[OVR_NUMS];

	int ruleId = 0;
	short numT, numV;
	short t[OVR_NUMS*100];
	short v[VTR_NUMS*100];

//printf("CompactRuleSet %d: numRules = %d\n", tranIx, rs->NumberOfRules());
//fflush(stdout);
	for ( int i = 0; i < result->numRules; i++ )
	{
		// bring the rule in temp stack arrays
		short rules = which;
		short rule_number = i+1;
		short part_number = OVERFLOW_RECORD;
		FillMemory(comment, sizeof(comment), ' ');
		comin1st(tranIx, &rules, comment, &rule_number);
		rulein1st(tranIx, &rules, rule, &rule_number, &ruleId);
		bO = 0 == ovrin1st(tranIx, &rules, o, &rule_number, &part_number);

		for ( numT = 0, part_number = TAGSET_PART_BASE;
			   !ovrin1st(tranIx, &rules, t + OVR_NUMS*numT, &rule_number, 
				   &part_number);
				numT++, part_number++ );

		for ( numV = 0, part_number = rule_number;
			   ;
				numV++, part_number++ )
		{
			vtrin1st(tranIx, &rules, v + VTR_NUMS*numV, &rule_number, &part_number);
                        int j;
			for ( j = VTR_NUMS-1; j >= 0; j-- )
			{
				if ( v[VTR_NUMS*numV + j] == 999 )
					break;
			}

			if ( j >= 0)
			{
				numV++;
				break;
			}
		}
		
		// alloc the compacted rule record, and fill it in
		int pcrLen = sizeof(CompRuleHdr) +
							(bO ? OVR_NUMS*sizeof(short) : 0) +	// overflow
							numT*OVR_NUMS*sizeof(short) +		// tagsets
							numV*VTR_NUMS*sizeof(short);		// vtrs
		pcrLen += sizeof(int) - (pcrLen % sizeof(int));

		CompRuleHdr* pcr = (CompRuleHdr*) new char[pcrLen];
		pcr->len = pcrLen;
		pcr->ruleId = ruleId;
		pcr->tOff = sizeof(CompRuleHdr) + (bO ? OVR_NUMS*sizeof(short) : 0);
		pcr->vOff = pcr->tOff + numT*OVR_NUMS*sizeof(short);
		CopyMemory(pcr->comment, comment, COM_CHARS);
		CopyMemory(pcr->rule, rule, RULE_NUMS*sizeof(short));
		if ( bO )
			CopyMemory((char*)pcr + sizeof(CompRuleHdr), o, OVR_NUMS*sizeof(short));
                int j;
		for ( j = 0; j < numT; j++ )
			CopyMemory((char*)pcr + pcr->tOff + j*OVR_NUMS*sizeof(short),
				t + OVR_NUMS*j, OVR_NUMS*sizeof(short));
		for ( j = 0; j < numV; j++ )
			CopyMemory((char*)pcr + pcr->vOff + j*VTR_NUMS*sizeof(short),
						v + VTR_NUMS*j, VTR_NUMS*sizeof(short));

		result->r[i] = pcr;
	}

	return result;
}

static void FreeCompRuleSet(CompRuleSet* crs)
{
	if ( crs )
	{
		for ( int i = 0; i < crs->numRules; i++ )
		{
			if ( crs->r[i] )
				delete [] ( crs->r[i] );
		}
		delete [] ( crs->r );
		delete ( crs );
	}
}

static int rulein1st(short* rules, short* rule_buffer, short* rule_number, 
					 int* ruleId) {
	register int tranIx = GetTranIndexByThreadId(GetCurrentThreadId());
	return rulein1st(tranIx, rules, rule_buffer, rule_number, ruleId);
}

static int rulein1st(int tranIx, short* rules, short* rule_buffer, 
					 short* rule_number, int* ruleId) {
//printf("\t\trulein1st: tranIx = %d\n", tranIx);
//fflush(stdout);
	if ( !tranData[tranIx] ) return 1;

	ZeroMemory(rule_buffer, RULE_NUMS*sizeof(short));
	CLgsRuleSet* rs = 1 == *rules ? tranData[tranIx]->m_pTran :
					 2 == *rules ? tranData[tranIx]->m_pTranMini : NULL;
	if ( !rs ) return 1;

	int ruleIx = *rule_number - 1;
	if ( ruleIx < 0 || ruleIx >= rs->NumberOfRules() ) return 1;

	CLgsRule* r = rs->GetItem(ruleIx);
	if ( !r ) return 1;

	int numTsps = 0;

	// copy level and first 3 sp elements to the output buffer
	*ruleId = r->id();
	rule_buffer[0] = r->Level();
	CLgsSpElement** sps;
	int numSps = r->SpStatements( &sps );

	for ( int i = 0; i < numSps; i++ )
	{
		short* nums;
		int numNums = sps[i]->TypeStatement()->GetNumbers(&nums);

		if ( i < 3 )
		{
			if ( 1 == numNums && !sps[i]->TypeStatement()->GetHints() )
				rule_buffer[1 + i*3 + 1] = nums[0];
			else
			{
				char* hints = sps[i]->TypeStatement()->GetHints();
				rule_buffer[1 + i*3 + 1] = - TAGSET_PART_BASE - numTsps;
				numTsps += hints ? strlen(hints) : 1;
			}

			rule_buffer[1 + i*3 + 0] = sps[i]->WordClass();
			rule_buffer[1 + i*3 + 2] = sps[i]->FormCode();
		}
	}

	// this is just an indication that there is an overflow record
	rule_buffer[10] = numSps > 3 ? OVERFLOW_RECORD : 0;
	rule_buffer[11] = *rule_number; // vtrs are always there

	return 0;
}

static int ovrin1st(short* rules, short* rule_buffer, short* rule_number, 
					short* part_number) {
	register int tranIx = GetTranIndexByThreadId(GetCurrentThreadId());
	return ovrin1st(tranIx, rules, rule_buffer, rule_number, part_number);
}
static int ovrin1st(int tranIx, short* rules, short* rule_buffer, 
					short* rule_number, short* part_number) {
//printf("\t\tovrin1st: tranIx = %d\n", tranIx);
//fflush(stdout);
	if ( !tranData[tranIx] ) return 1;

	ZeroMemory(rule_buffer, OVR_NUMS*sizeof(short));
	int result = 1; // error
	CLgsRuleSet* rs = 1 == *rules ? tranData[tranIx]->m_pTran :
					 2 == *rules ? tranData[tranIx]->m_pTranMini : NULL;
	if ( !rs ) return result;

	int ruleIx = *rule_number - 1;
	if ( ruleIx < 0 || ruleIx >= rs->NumberOfRules() ) return result;

	CLgsRule* r = rs->GetItem(ruleIx);
	if ( !r ) return result;


	if ( OVERFLOW_RECORD == *part_number )
	{
		// overflow record request
		int numTsps = 0;
		CLgsSpElement** sps;
		int numSps = r->SpStatements( &sps );

		for ( int i = 0; i < numSps && i < 10; i++ )
		{
			if ( i > 2 )
			{
				rule_buffer[(i-3)*3 + 0] = sps[i]->WordClass();
				rule_buffer[(i-3)*3 + 2] = sps[i]->FormCode();
				result = 0; // success
			}

			short* nums;
			int numNums = sps[i]->TypeStatement()->GetNumbers(&nums);

			if ( 1 == numNums && !sps[i]->TypeStatement()->GetHints() )
			{
				if ( i > 2 )
					rule_buffer[(i-3)*3 + 1] = nums[0];
			}
			else
			{
				char* hints = sps[i]->TypeStatement()->GetHints();
				if ( i > 2 )
					rule_buffer[(i-3)*3 + 1] = - TAGSET_PART_BASE - numTsps;
				numTsps += hints ? strlen(hints) : 1;
			}
		}
	}
	else
	{
		// get requested tagset portion of the rule
		int reqPartIx = *part_number - TAGSET_PART_BASE;
		CLgsSpElement** sps;
		int numSps = r->SpStatements( &sps );
		int tagsetPartsCount = 0;

		for ( int i = 0; i < numSps; i++ )
		{
			short* nums;
			int numNums = sps[i]->TypeStatement()->GetNumbers(&nums);

			if ( 1 == numNums && !sps[i]->TypeStatement()->GetHints() )
				continue;

			char* hints = sps[i]->TypeStatement()->GetHints();
			int additionalTagsetParts =
					hints ? strlen(hints) :
						numNums/20 + (numNums%20 > 0 ? 1 : 0);

			int numsInThisPart = 0;
			for ( int j = 0, z = 0; j < additionalTagsetParts;
					z += (hints ? hints[j] : 20), j++ )
			{
				if ( reqPartIx != tagsetPartsCount + j )
					continue;

				result = 0; // success

				int numsInThisPart = hints ? hints[j] : 20;
				if ( numsInThisPart > 20 )
				{
					cerr << "Tagset line is too long, truncated to 20 numbers\n";
					cerr << " (rule set: " << *rules <<
						", rule: " << *rule_number << ")\n";
				}
				for ( int k = 0;
						k < 20 && k < numsInThisPart && z + k < numNums;
						 k++ )
					rule_buffer[k] = nums[z + k];
			}

			tagsetPartsCount += additionalTagsetParts;
		}
	}

	return result;
}

static int comin1st(short* rules, char* rule_buffer, short* rule_number) {
	int tranIx = GetTranIndexByThreadId(GetCurrentThreadId());
	return comin1st(tranIx, rules, rule_buffer, rule_number);
}
static int comin1st(int tranIx, short* rules, char* rule_buffer, 
					short* rule_number) {
//printf("\t\tcomin1st: tranIx = %d\n", tranIx);
//fflush(stdout);
	if ( !tranData[tranIx] ) return 1;

	int result = 1; // error
	CLgsRuleSet* rs = 1 == *rules ? tranData[tranIx]->m_pTran :
					 2 == *rules ? tranData[tranIx]->m_pTranMini : NULL;
	if ( !rs ) return result;

	int ruleIx = *rule_number - 1;
	if ( ruleIx < 0 || ruleIx >= rs->NumberOfRules() ) return result;

	CLgsRule* r = rs->GetItem(ruleIx);
	if ( !r ) return result;

	// use Description() function, because it doesn't add level to the string
	const char* com = r->Description();
	if ( com )
	{
		int len = strlen(com);
		if ( len > 0 )
		{
			int lenlen = len < COM_CHARS ? len : COM_CHARS-1;
			strncpy(rule_buffer, com, lenlen);
			rule_buffer[lenlen] = ' ';
		}
	}

	return result;
}

static int vtrin1st(short* rules, short* rule_buffer, short* rule_number, 
					short* part_number) {
	int tranIx = GetTranIndexByThreadId(GetCurrentThreadId());
	return vtrin1st(tranIx, rules, rule_buffer, rule_number, part_number);
}
static int vtrin1st(int tranIx, short* rules, short* rule_buffer, 
					short* rule_number, short* part_number) {
//printf("\t\tvtrin1st: tranIx = %d\n", tranIx);
//fflush(stdout);
	if ( !tranData[tranIx] ) return 1;

	ZeroMemory(rule_buffer, VTR_NUMS*sizeof(short));
	int result = 1; // error
	CLgsRuleSet* rs = 1 == *rules ? tranData[tranIx]->m_pTran :
					 2 == *rules ? tranData[tranIx]->m_pTranMini : NULL;
	if ( !rs ) return result;

	int ruleIx = *rule_number - 1;
	if ( ruleIx < 0 || ruleIx >= rs->NumberOfRules() ) return result;

	CLgsRule* r = rs->GetItem(ruleIx);
	if ( !r ) return result;

	int reqPartIx = *part_number - *rule_number;

	CLgsVtrStatement** vtrs;
	int numVtrs = r->VtrStatements(&vtrs);

	if ( numVtrs < 1 )
	{
		result = 0;
		rule_buffer[0] = 999;
		return result;
	}

	short* nums;
    int numNums = vtrs[0]->GetNumbers( &nums );

	if ( numNums > 0 )
	{
		int numVtrParts = numNums/24 + (numNums%24 > 0 ? 1 : 0);
		for ( int j = 0; j < numVtrParts; j++ )
		{
			if ( reqPartIx != j )
				continue;

			result = 0; // success

                        int k;
			for ( k = 0; k < 24 && j*24 + k < numNums; k++ )
				rule_buffer[k] = nums[j*24 + k];

			if ( j == numVtrParts -1 )
			{
				rule_buffer[k] = 999;
			}
			else
			{
				rule_buffer[k++] = 888;
				rule_buffer[k] = 888;
			}
		}
	}
	else
	{
		result = 0;
		rule_buffer[0] = 999;
	}

	return result;
}



CompRuleSet* TranRuleSetFromRuleNumber(int nType)
{
	
	register int tranIx = GetTranIndexByThreadId(GetCurrentThreadId());
	if ( !tranData[tranIx] )
		return  NULL;

	CompRuleSet* crs = 1 == nType ? tranData[tranIx]->m_pCTran :
					 2 == nType ? tranData[tranIx]->m_pCTranMini : NULL;
	
	
	return  crs;
	
}

/*****************************************************************************
This function returns the ruleID of the rule object corresponding to RuleNumber and Type
from the rule the rule set loaded.
******************************************************************************/
int TranRuleIDFromRuleNumber(int nType, int nRuleNumber)
{
	CompRuleSet* pRuleSet = TranRuleSetFromRuleNumber(nType);
	if(pRuleSet)
	{
		int ruleIx = nRuleNumber - 1;
		//Check the range
		if ( ruleIx < 0 || ruleIx >= pRuleSet->numRules ) return -1;

		CompRuleHdr* hdr= pRuleSet->r[ruleIx];
		if(hdr) return hdr->ruleId;
		else return -1;
	}
	else
		return -1;
}
