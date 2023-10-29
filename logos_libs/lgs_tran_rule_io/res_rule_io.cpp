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
#endif
#include <logos_libs/GrammarRules/LgsGrammarRules.h>

#define _RES_RULE_IO_IMPL_
#include "res_rule_io.h"
#include "res_rule_stat.h"

#include "util_io.h"
#include "util.h"

#ifdef _MSC_VER
#include <wtypes.h>
#include <winbase.h>
#include <winerror.h>
#include <windows.h>
#include <io.h>
#else
#include <sys/io.h>
#define FillMemory(__Dest,__Len,__Fill) memset(__Dest,__Fill,__Len)
#define CopyMemory(__Dest,__Src,__Len) memcpy(__Dest,__Src,__Len)
#define UNREFERENCED_PARAMETER(__Arg) 
#endif
#include <fcntl.h>
#include <configdatafileinterface/configdatainterfacemain.h>
#include "logos_libs/multithreadlib/lgscritsect.h"

#include <cstring>

// defined in tran
//extern CRITICAL_SECTION __csec;
extern LgsCriticalSection __csec;

#ifdef HAVE_MMAP
#include "logos_include/memory_mapper.h"
static memmap_manager MManager;
#endif

#define TAGSET_PART_BASE		3	// cannot be less than 3
#define OVERFLOW_RECORD			0x7fff

static int VTRIN1ST(
	short* rules, short* rule_buffer, short* rule_number, short* part_number);

static int RULEIN1ST(short* rules, short* rule_buffer, short* rule_number, int* ruleId);

static int OVRIN1ST(
	short* rules, short* rule_buffer, short* rule_number, short* part_number);

static int COMIN1ST(short* rules, char* rule_buffer, short* rule_number);

static int calledFromCacheMgr = false;
static int allocatedByNew = false;

// private data
static CLgsRuleSet* m_pRes1 = NULL;
static CLgsRuleSet* m_pRes2 = NULL;
static CLgsRuleSet* m_pRes2Mini = NULL;
static CLgsRuleSet* m_pRes1Mini = NULL;
static CLgsRuleSet* m_pRes22 = NULL;

// compacted rule
//  (keep len of this struct to be multiple of 4 bytes)
typedef struct _compact_rule_hdr
{
	short len;	// length of the structure
	int ruleId;
	short tOff; // offset of the first tagset record
	short vOff; // offset of the first vtr record
	char comment[80];
	short rule[13];
	// optional OVR record: short[21]
	// one or more TAGSET records: short[numTagsetRecs][21]
	// optional VTR records: short[numVtrRecs][26]
} CompRuleHdr;

typedef struct _comp_rule_set
{
	int numRules;
	CompRuleHdr** r;
} CompRuleSet;

// compacted rule sets
static CompRuleSet* m_pCRes1 = NULL;
static CompRuleSet* m_pCRes2 = NULL;
static CompRuleSet* m_pCRes2Mini = NULL;
static CompRuleSet* m_pCRes1Mini = NULL;
static CompRuleSet* m_pCRes22 = NULL;

static void FreeCompRuleSet(CompRuleSet* crs) {
	if ( crs )
	{
		for ( int i = 0; i < crs->numRules; i++ )
		{
			if (crs->r[i])
				delete[] crs->r[i];
		}
		delete[] crs->r;
		delete crs;
	}
}

static int max_rule_len = 0;
static int nrules = 0;

#ifdef _MSC_VER
char *
mapChunk(char *mem_name, int sz) {

  HANDLE hShmem 
    = CreateFileMapping((HANDLE)0xFFFFFFFF,   // file to be mapped is in paging
                        NULL,		      // no security attribs
                        PAGE_READWRITE,
                        0,		      // size high 32 bits
                        sz,		      // size low
                        mem_name
                        );

  int errCode = GetLastError();
  if(hShmem == NULL) {
    /* this code is complete bullshit. ERROR_ALREADY_EXISTS only comes with a
       non-NULL handle*/
    //if(errCode==ERROR_ALREADY_EXISTS) {
    //			printf("\"%s\" already exists\n", mem_name);
    //} else {
    printf("getMappingChunk \"%s\" (sz=%d) mapping error: %d\n", 
           mem_name, sz, errCode);
    fflush(stdout);
    return NULL;
  }

  // Want to make sure that is is only called for non-existing mappings ??
  /*
  if(errCode==ERROR_ALREADY_EXISTS) {
    printf("\"%s\" already exists\n", mem_name); fflush(stdout);
    CloseHandle(hShmem);
    return NULL
  }
  */

  char *ret = (char *)MapViewOfFile(hShmem, FILE_MAP_WRITE, 0, 0, 0);
  if(ret==NULL) {
    printf("MapViewOfFile %s: errCode = %d\n",
           mem_name, GetLastError());
  }

  //printf("res_rule_io.MapChunk: ret=%x \"%s\" %d\n", ret, mem_name, sz);
  //fflush(stdout);
  //printf("\"%s\", max_rule_len=%d, nrules=%d, size=%10.2f / %10.2f\n", 
  //	mem_name, max_rule_len, nrules, max_rule_len*nrules/1024., sz/1024.);
  //fflush(stdout);
  //max_rule_len = nrules = 0;
  return ret;
}
#endif

#ifdef HAVE_MMAP
char *mapChunk(char *mem_name, int sz) {
  return MManager.mapChunk(mem_name, sz);
}
#endif


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

int getCompRuleSetLength(CompRuleSet *p) {
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


/* This is the only other function calling mapChunk except for the init
   functions, which, themselves, are the only functions calling compactMemory. 
   As a consequence, the init functions are the only ones calling mapChunk,
   isMapped, and getMappedChunk.
   So, mapChunk is truly getNewMappedChunk, while getMappedChunk always
   accesses an existing mapping.
   Leave it at this!
*/
CompRuleSet *
compactMemory(CompRuleSet *p, char *mem_name) {
  // here we just pack CompRuleSet to be continious
  // in memory (to allow memory-mapping for use
  // across processes).

  int len = getCompRuleSetLength(p);
  char *cp;
  if(calledFromCacheMgr) {
    cp = mapChunk(mem_name, len);
  } else {
    cp = new char[len];
    allocatedByNew = true;
  }
  //printf("CompRuleSet size = %d\n", len);
  //fflush(stdout);

  CompRuleSet *newSet = (CompRuleSet *)cp;
  newSet->numRules = p->numRules;
  newSet->r = (CompRuleHdr **)(cp + sizeof(int)*2);
  char *raddr = cp + sizeof(int)*2 + sizeof(CompRuleHdr *) * p->numRules;
  for(int i=0;i<p->numRules;i++) {
    CompRuleHdr *h = p->r[i];
    CopyMemory(raddr, h, h->len);
    newSet->r[i] = (CompRuleHdr *)raddr;
    raddr += h->len;
  }
  FreeCompRuleSet(p);
  return newSet;
}

void
adjustPointers(CompRuleSet *p) {
// from CacheMgr we're getting memory mapped
// packed (continuous in memory) CompRuleSet's which unfortunately are of
// complex structure - w/ pointers to the rules
// inside.

	char **ttt = (char **)((char *)p + sizeof(int));
	*ttt = (char *)p + sizeof(int)*2;
	char *raddr = (char *)p + sizeof(int)*2 + sizeof(CompRuleHdr *) * p->numRules;
	for(int i=0;i<p->numRules;i++) {
		CompRuleHdr *h = (CompRuleHdr *)raddr;
		p->r[i] = h;
		raddr += h->len;
	}
}

// RES1C, RES2H, RES2HX arrays are private to this module now,
// the values should be accessed by calling functions
//		NENT1, DISSP1 for RES1C
//		NENT2, DISSP2 for RES2H
//		NENTX, DISSPX for RES2HX
// To enable comment out all RES1C, RES2H, RES2HX common block, and
//   declarations of all vars named as one of the functions below
struct RES_LIMITS {
	short NENT[20];
	short DISST[20];
};
static struct RES_LIMITS *RES1C;
static struct RES_LIMITS *RES2H;
static struct RES_LIMITS *RES2HX; 

// functions to access the arrays (from fortran only)
// (former common blocks)
extern "C" int NENT1(short* ix) { return RES1C->NENT[*ix-1]; }
extern "C" int DISSP1(short* ix) { return RES1C->DISST[*ix-1]; }
extern "C" int NENT2(short* ix) { return RES2H->NENT[*ix-1]; }
extern "C" int DISSP2(short* ix) { return RES2H->DISST[*ix-1]; }
extern "C" int NENTX(short* ix) { return RES2HX->NENT[*ix-1]; }
extern "C" int DISSPX(short* ix) { return RES2HX->DISST[*ix-1]; }

// Get the RuleSet
static CompRuleSet* ResRuleSetFromRuleNumber(int nType);

// semres array which hold starting position, and count of rules
// for a given word class and type
typedef short start_count_type[20][1024];

static start_count_type *noftyp;
static start_count_type *typos;

// RxR arrays, now static
struct RESRENT { short RENT[20][40][21]; };
static struct RESRENT *R1R;
static struct RESRENT *R2R;

// functions to access these arrays
extern "C" int R1RENT(short* z, short* lev, short* x)
{
	return R1R->RENT[*x-1][*lev-1][*z-1];
}

extern "C" int R2RENT(short* z, short* lev, short* x)
{
	return R2R->RENT[*x-1][*lev-1][*z-1];
}

static bool init_wcix( CLgsRuleSet* rs, struct RES_LIMITS* lims)
{
	int* startIndex;
	int* countIndex;
	if ( !rs->BuildIndex(&startIndex, &countIndex, NULL, NULL, true) )
		return false;

	int wcMin, wcMax, tyMin, tyMax;
	rs->QueryBounds(&wcMin, &wcMax, &tyMin, &tyMax);

	int wcNum = wcMax - wcMin + 1;

	if ( wcMin <= 0 || wcMax <= 0 )
		throw("Error: zero or negative WC value");
	if ( wcNum > 20 )
		throw("Error: #of word classes > 20");
	if ( wcNum <= 0 )
		throw("Error: #of word classes is zero");

        int i;
	for ( i = 0; i < wcNum; i++ )
	{
		if ( 0 != countIndex[i] )
		{
			lims->NENT[i] = countIndex[i];
			lims->DISST[i] = startIndex[i] + 1;
			//  (added 1 for compatibility with fortran code)
		}
	}

	// if count is zero the corresponding disst entry
	// should be set to the value from the nearest (up) entry with non-zero count
	int x = lims->DISST[wcNum - 1];
	for ( i = wcNum - 1; i >= 0; i-- )
	{
		if ( 0 == countIndex[i] )
			lims->DISST[i] = x;
		else
			x = lims->DISST[i];
	}

	delete[] startIndex;
	delete[] countIndex;
	return true;
}

// part_number:
//		OVERFLOW_RECORD					- to get overflow record
//		TAGSET_PART_BASE + n			- to get n-th tagset portion of the rule
extern "C" int
OVRIN(short* rules, short* rule_buffer, short* rule_number, short* part_number)
{
	int result = 1; // error
	ZeroMemory(rule_buffer, 21*sizeof(short));
	CompRuleSet* crs = 1 == *rules ? m_pCRes1 :
					 2 == *rules ? m_pCRes2 :
					 3 == *rules ? m_pCRes22 :
					 4 == *rules ? m_pCRes2Mini :
					 5 == *rules ? m_pCRes1Mini : NULL;
	if ( !crs ) return result;

	int ruleIx = *rule_number - 1;
	if ( ruleIx < 0 || ruleIx >= crs -> numRules ) return result;
	CompRuleHdr* hdr = crs -> r[ruleIx];

	if ( OVERFLOW_RECORD == *part_number )
	{
		// overflow record request
		if ( hdr -> tOff != sizeof(CompRuleHdr) )
		{
			CopyMemory(rule_buffer, (char*)hdr + sizeof(CompRuleHdr), 21*sizeof(short));
			result = 0; // success
		}
	}
	else
	{
		int reqPartIx = *part_number - TAGSET_PART_BASE;
		short off = hdr -> tOff + reqPartIx*21*sizeof(short);
		if ( off < hdr -> vOff )
		{
			CopyMemory(rule_buffer, (char*)hdr + off, 21*sizeof(short));
			result = 0; // success
		}
	}

	return result;
}

static int
OVRIN1ST(short* rules, short* rule_buffer, short* rule_number, short* part_number)
{
  ZeroMemory(rule_buffer, 21*sizeof(short));
  int result = 1; // error
  CLgsRuleSet* rs = 1 == *rules ? m_pRes1 :
    2 == *rules ? m_pRes2 :
    3 == *rules ? m_pRes22 :
    4 == *rules ? m_pRes2Mini :
    5 == *rules ? m_pRes1Mini : NULL;
  if ( !rs ) return result;

  int ruleIx = *rule_number - 1;
  if ( ruleIx < 0 || ruleIx >= rs -> NumberOfRules() ) return result;

  CLgsRule* r = rs -> GetItem(ruleIx);
  if ( !r ) return result;


  if ( OVERFLOW_RECORD == *part_number )
    {
      // overflow record request
      int numTsps = 0;
      CLgsSpElement** sps;
      int numSps = r -> SpStatements( &sps );

      for ( int i = 0; i < numSps && i < 10; i++ )
        {
          if ( i > 2 )
            {
              rule_buffer[(i-3)*3 + 0] = sps[i] -> WordClass();
              rule_buffer[(i-3)*3 + 2] = sps[i] -> FormCode();
              result = 0; // success
            }

          short* nums;
          int numNums = sps[i] -> TypeStatement() -> GetNumbers(&nums);

          if ( 1 == numNums && !sps[i] -> TypeStatement() -> GetHints())
            {
              if ( i > 2 )
                rule_buffer[(i-3)*3 + 1] = nums[0];
            }
          else
            {
              char* hints = sps[i] -> TypeStatement() -> GetHints();
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
      int numSps = r -> SpStatements( &sps );
      int tagsetPartsCount = 0;

      for ( int i = 0; i < numSps; i++ )
        {
          short* nums;
          int numNums = sps[i] -> TypeStatement() -> GetNumbers(&nums);

          if ( 1 == numNums && !sps[i] -> TypeStatement() -> GetHints())
            continue;

          char* hints = sps[i] -> TypeStatement() -> GetHints();
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

extern "C" int
RULEIN(short* rules, short* rule_buffer, short* rule_number)
{
	ZeroMemory(rule_buffer, (3 == *rules ? 12 : 13)*sizeof(short));
	CompRuleSet* crs = 1 == *rules ? m_pCRes1 :
					 2 == *rules ? m_pCRes2 :
					 3 == *rules ? m_pCRes22 :
					 4 == *rules ? m_pCRes2Mini :
					 5 == *rules ? m_pCRes1Mini : NULL;
	if ( !crs ) return 1;

	int ruleIx = *rule_number - 1;
	if ( ruleIx < 0 || ruleIx >= crs -> numRules ) return 1;
	CompRuleHdr* hdr = crs -> r[ruleIx];
	CopyMemory(rule_buffer, hdr->rule, (3 == *rules ? 12 : 13)*sizeof(short));
	return 0;
}

extern "C" short *
RULEIN_OCR(short rules, short rule_number) {

	register CompRuleSet* crs = 2 == rules ? m_pCRes2 :
					 1 == rules ? m_pCRes1 :
					 3 == rules ? m_pCRes22 :
					 4 == rules ? m_pCRes2Mini :
					 5 == rules ? m_pCRes1Mini : NULL;
	if ( !crs )
		return NULL;

	int ruleIx = rule_number - 1;
	if ( ruleIx < 0 || ruleIx >= crs -> numRules ) return NULL;
	return crs->r[ruleIx]->rule;

}

static int
RULEIN1ST(short* rules, short* rule_buffer, short* rule_number, int* ruleId)
{
	ZeroMemory(rule_buffer, (3 == *rules ? 12 : 13)*sizeof(short));
	CLgsRuleSet* rs = 1 == *rules ? m_pRes1 :
					 2 == *rules ? m_pRes2 :
					 3 == *rules ? m_pRes22 :
					 4 == *rules ? m_pRes2Mini :
					 5 == *rules ? m_pRes1Mini : NULL;
	if ( !rs ) return 1;

	int ruleIx = *rule_number - 1;
	if ( ruleIx < 0 || ruleIx >= rs -> NumberOfRules() ) return 1;

	CLgsRule* r = rs -> GetItem(ruleIx);
	if ( !r ) return 1;

	int numTsps = 0;

	// copy level and first 3 sp elements to the output buffer
	*ruleId = r -> id();
	rule_buffer[0] = r -> Level();
	CLgsSpElement** sps;
	int numSps = r -> SpStatements( &sps );

	for ( int i = 0; i < numSps; i++ )
	{
		short* nums;
		int numNums = sps[i] -> TypeStatement() -> GetNumbers(&nums);

		if ( i < 3 )
		{
			if ( 1 == numNums && !sps[i] -> TypeStatement() -> GetHints() )
				rule_buffer[1 + i*3 + 1] = nums[0];
			else
			{
				char* hints = sps[i] -> TypeStatement() -> GetHints();
				rule_buffer[1 + i*3 + 1] = - TAGSET_PART_BASE - numTsps;
				numTsps += hints ? strlen(hints) : 1;
			}

			rule_buffer[1 + i*3 + 0] = sps[i] -> WordClass();
			rule_buffer[1 + i*3 + 2] = sps[i] -> FormCode();
		}
	}

	// this is just an indication that there is an overflow record
	rule_buffer[10] = numSps > 3 ? OVERFLOW_RECORD : 0;

	if ( 3 != *rules )
	{
		// other than res22
		rule_buffer[11] = r -> Specificity();
		rule_buffer[12] = *rule_number; // vtrs are always there
	}
	else
	{
		// res22
		rule_buffer[11] = *rule_number; // vtrs are always there
	}

	return 0;
}

static CompRuleSet* CompactRuleSet(int which, CLgsRuleSet* rs)
{
  CompRuleSet* result = new CompRuleSet();
  result -> numRules = rs->NumberOfRules();
  result -> r = new CompRuleHdr*[result -> numRules];
  ZeroMemory(result -> r, sizeof(CompRuleHdr*) * result -> numRules);

  int ruleId = 0;
  char comment[80];
  short rule[13];

  bool bO = false;
  short o[21];

  short numT, numV;
  short t[21*100];
  short v[26*100];

  for ( int i = 0; i < result -> numRules; i++ )
    {
      // bring the rule in temp stack arrays
      short rules = which;
      short rule_number = i+1;
      short part_number = OVERFLOW_RECORD;
      FillMemory(comment, sizeof(comment), ' ');
      COMIN1ST(&rules, comment, &rule_number);
      RULEIN1ST(&rules, rule, &rule_number, &ruleId);
      bO = 0 == OVRIN1ST(&rules, o, &rule_number, &part_number);

      for ( numT = 0, part_number = TAGSET_PART_BASE;
            !OVRIN1ST(&rules, t + 21*numT, &rule_number, &part_number);
            numT++, part_number++ );

      for ( numV = 0, part_number = rule_number;
            ;
            numV++, part_number++ )
        {
          VTRIN1ST(&rules, v + 26*numV, &rule_number, &part_number);
          int j;
          for ( j = 26-1; j >= 0; j-- )
            {
              if ( v[26*numV + j] == 999 )
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
        (bO ? 21*sizeof(short) : 0) +	// overflow
        numT*21*sizeof(short) +		// tagsets
        numV*26*sizeof(short);		// vtrs
      //if ( pcrLen % sizeof(int) )
      //{
      //	pcrLen += sizeof(int) - (pcrLen % sizeof(int));
      //}

      CompRuleHdr* pcr = (CompRuleHdr*) new char[pcrLen];
      pcr -> len = pcrLen;
      pcr -> ruleId = ruleId;
      pcr -> tOff = sizeof(CompRuleHdr) + (bO ? 21*sizeof(short) : 0);
      pcr -> vOff = pcr -> tOff + numT*21*sizeof(short);
      CopyMemory(pcr->comment, comment, 80);
      CopyMemory(pcr->rule, rule, 13*sizeof(short));
      if ( bO )
        CopyMemory((char*)pcr + sizeof(CompRuleHdr), o, 21*sizeof(short));
      int j;
      for ( j = 0; j < numT; j++ )
        CopyMemory((char*)pcr + pcr->tOff + j*21*sizeof(short),
                   t + 21*j, 21*sizeof(short));
      for ( j = 0; j < numV; j++ )
        CopyMemory((char*)pcr + pcr->vOff + j*26*sizeof(short),
                   v + 26*j, 26*sizeof(short));

      result -> r[i] = pcr;
    }

  return result;
}

#ifdef _MSC_VER
bool isMapped(char *mem_name)
{
  HANDLE hShmem = OpenFileMapping(FILE_MAP_READ, 0, mem_name);
  bool ret = (hShmem!=NULL);
  if (ret)
    CloseHandle(hShmem);
  //printf("\"%s\" isMapped: %d\n", mem_name, ret);
  //fflush(stdout);
  return ret;
}

char * getMappedChunk(char *mem_name)
{
  HANDLE hShmem = OpenFileMapping(FILE_MAP_WRITE, 0, mem_name);
  if (hShmem == NULL)
    {
      printf("\"%s\" getMappedChunk: %d\n", mem_name, GetLastError());
      fflush(stdout);
      return NULL;
    }
  char *ret = (char *)MapViewOfFile(hShmem, FILE_MAP_WRITE, 0, 0, 0);
  if (ret == NULL)
    {
      printf("MapViewOfFile %s: errCode = %d\n",
             mem_name, GetLastError());
    }
  CloseHandle(hShmem);

  //printf("\"%s\" getMappedChunk: %x\n", mem_name, ret);
  //fflush(stdout);
  return ret;
}
#endif

#ifdef HAVE_MMAP
bool isMapped(char *mem_name) {
  return MManager.isMapped(mem_name);
}

char *getMappedChunk(char *mem_name) {
  return MManager.getMappedChunk(mem_name);
}

void UnmapViewOfFile(void *address) {
  MManager.unloadChunk(address);
}  
#endif


// load/unload rule sets
extern "C" void
res_unload()
{
  if(R1R) UnmapViewOfFile(R1R);
  if(R2R) UnmapViewOfFile(R2R);
  if(noftyp) UnmapViewOfFile(noftyp);
  if(typos) UnmapViewOfFile(typos);
  if(RES1C) UnmapViewOfFile(RES1C);
  if(RES2H) UnmapViewOfFile(RES2H);
  if(RES2HX) UnmapViewOfFile(RES2HX);

  if(m_pCRes1) UnmapViewOfFile(m_pCRes1);
  if(m_pCRes2) UnmapViewOfFile(m_pCRes2);
  if(m_pCRes22) UnmapViewOfFile(m_pCRes22);

  if(allocatedByNew) {
    delete [] m_pCRes1;
    delete [] m_pCRes2;
    delete [] m_pCRes22;
  }

  if(m_pCRes2Mini) UnmapViewOfFile(m_pCRes2Mini);
  if(m_pCRes1Mini) UnmapViewOfFile(m_pCRes1Mini);

}


void 
make_good_name(const char *filename, const char *chunk_name, char *result) {
  (void) strcpy(result, filename);
  char *cp = result;
  while(1)
    {
      cp = strchr(cp, DIR_SEP_CHAR);
      if (!cp)
        break;
      *cp = '.';
      cp++;
    }
  (void) strcat(result, chunk_name);
}

int init_res(short rsType, const char *rname,
             const char *resXYname, const char *rXrname,
             CLgsRuleSet * &m_pRes, CompRuleSet * &m_pCRes,
             struct RES_LIMITS * &p_RESXY, struct RESRENT * &p_RXR)
{
  char mem_resX_name[1024];
  char mem_resXY_name[1024];
  char mem_rXr_name[1024];
  char fileName[MAX_FILEPATH_LEN];

  if (GetConfigData("resdata", rname, fileName, MAX_FILEPATH_LEN)
      != CONFDATA_SUCCESS) {
    return 1;
  }

  make_good_name(fileName, "-CompRuleSet", mem_resX_name);
  make_good_name(fileName, resXYname, mem_resXY_name);
  make_good_name(fileName, rXrname, mem_rXr_name);
    
  bool bAlreadyMapped = isMapped(mem_resX_name);
  if (bAlreadyMapped)
    {
      char *cp = getMappedChunk(mem_resX_name);
      CompRuleSet *tmp = (CompRuleSet *)cp;
      int lll = getFlatCompRuleSetLength(tmp);
      m_pCRes = (CompRuleSet *) new char[lll];
      memcpy(m_pCRes, tmp, lll);
      adjustPointers(m_pCRes);
      UnmapViewOfFile(cp);
      p_RESXY = (struct RES_LIMITS *)getMappedChunk(mem_resXY_name);
      p_RXR = (struct RESRENT *)getMappedChunk(mem_rXr_name);
      return 0;
    }

  if (!(m_pRes = LoadRuleSet(fileName)))
    {
      return 1;
    }

  // build RES index

  p_RESXY = (RES_LIMITS *)mapChunk(mem_resXY_name, sizeof(*p_RESXY));
  ZeroMemory(p_RESXY, sizeof(*p_RESXY));
  if (!init_wcix(m_pRes, p_RESXY))
    {
      OutputMsg(rname, "wc-index cannot be built");
      delete m_pRes;
      m_pRes = NULL;
      return 1;
    }

  // _fix_me_
  // CompactRuleSet needs the m_pResX pointer that belongs to rsType correctly
  // set! This is a global dependency that is dangerous and could easily be
  // removed, which would also allow to make m_pRes completely local to this
  // function.
  m_pCRes = CompactRuleSet(rsType, m_pRes);
  m_pCRes = compactMemory(m_pCRes, mem_resX_name);
  delete m_pRes;
  m_pRes = NULL;

  p_RXR = (RESRENT *)mapChunk(mem_rXr_name, sizeof(*p_RXR));
  ZeroMemory(p_RXR, sizeof(*p_RXR));
  short rBuf[13];
  short ruleNo;
  for (ruleNo = 1; !RULEIN(&rsType, rBuf, &ruleNo); ruleNo++ )
    {
      int lev = rBuf[0];
      int x = rBuf[1];
      int z = rBuf[4];
      if (z < 1)
        z = 21;
      p_RXR->RENT[x-1][lev-1][z-1]++;
    }
  return 0;
}

int init_res2mini()
{
  char mem_res2mini_name[1024];
  char mem_res2hx_name[1024];
  char fileName[MAX_FILEPATH_LEN];

  if (GetConfigData("resdata", "res2mini", fileName, MAX_FILEPATH_LEN) != CONFDATA_SUCCESS)
    {
      return 1;
    }

  make_good_name(fileName, "-CompRuleSet", mem_res2mini_name);
  make_good_name(fileName, "-RES2HX", mem_res2hx_name);

  bool bAlreadyMapped = isMapped(mem_res2mini_name);
  if (bAlreadyMapped)
    {
      m_pCRes2Mini = (CompRuleSet *)getMappedChunk(mem_res2mini_name);
      adjustPointers(m_pCRes2Mini);
      RES2HX = (struct RES_LIMITS *)getMappedChunk(mem_res2hx_name);
      return 0;
    }

  if (!(m_pRes2Mini = LoadRuleSet(fileName)))
    {
      return 1;
    }

  SortRuleSet(m_pRes2Mini, RST_RES2);
  OutputMsg("Res Mini", fileName);

  RES2HX = (RES_LIMITS *)mapChunk(mem_res2hx_name, sizeof(*RES2HX));
  ZeroMemory(RES2HX, sizeof(*RES2HX));
  if (!init_wcix(m_pRes2Mini, RES2HX))
    {
      OutputMsg("Res2Mini", "wc-index cannot be built");
      return 1;
    }

  m_pCRes2Mini = CompactRuleSet(4, m_pRes2Mini);
  m_pCRes2Mini = compactMemory(m_pCRes2Mini, mem_res2mini_name);
  delete m_pRes2Mini, m_pRes2Mini = NULL;
  return 0;
}

int init_res22()
{
  char mem_res22_name[1024];
  char mem_typos_name[1024];
  char mem_noftyp_name[1024];
  char fileName[MAX_FILEPATH_LEN];

  if (GetConfigData("resdata", "res22", fileName, MAX_FILEPATH_LEN) != CONFDATA_SUCCESS)
    {
      return 1;
    }

  make_good_name(fileName, "-CompRuleSet", mem_res22_name);
  make_good_name(fileName, "-noftyp", mem_noftyp_name);
  make_good_name(fileName, "-typos", mem_typos_name);

  bool bAlreadyMapped = isMapped(mem_res22_name);
  if(bAlreadyMapped)
    {
      char *cp = getMappedChunk(mem_res22_name);
      CompRuleSet *tmp = (CompRuleSet *)cp;
      int lll = getFlatCompRuleSetLength(tmp);
      m_pCRes22 = (CompRuleSet *) new char[lll];
      memcpy(m_pCRes22, tmp, lll);
      adjustPointers(m_pCRes22);
      UnmapViewOfFile(cp);
      typos = (start_count_type *)getMappedChunk(mem_typos_name);
      noftyp = (start_count_type *)getMappedChunk(mem_noftyp_name);
      return 0;
    }
	
  if (!(m_pRes22 = LoadRuleSet(fileName)))
    {
      return 1;
    }

  typos = (start_count_type *)mapChunk(mem_typos_name, sizeof(start_count_type));
  noftyp = (start_count_type *)mapChunk(mem_noftyp_name, sizeof(start_count_type));
  // build noftyp, and typos arrays (RES22)
  ZeroMemory(typos, sizeof(start_count_type));
  ZeroMemory(noftyp, sizeof(start_count_type));
  int* startIndex;
  int* countIndex;
  if (m_pRes22->BuildIndex(&startIndex, &countIndex, NULL, NULL, false))
    {
      int wcMin, wcMax, tyMin, tyMax;
      m_pRes22->QueryBounds(&wcMin, &wcMax, &tyMin, &tyMax);

      int wcNum = wcMax - wcMin + 1;
      int tyNum = tyMax - tyMin + 1;

      for (int i = 0; i < wcNum; i++)
        {
          for (int j = 0; j < tyNum; j++)
            {
              int start = startIndex[tyNum*i + j];
              int count = countIndex[tyNum*i + j];
              if ( -1 != start && 0 != count )
                {
                  if ( i + wcMin > 0 && j + tyMin > 0 )
                    {
                      (*typos)[i + wcMin - 1][j + tyMin - 1] = start + count - 1 + 1;
                      (*noftyp)[i + wcMin - 1][j + tyMin - 1] = count;
                    }
                }
            }
        }

      delete[] startIndex;
      delete[] countIndex;
    }

  if ( m_pRes22 )
    {
      m_pCRes22 = CompactRuleSet(3, m_pRes22);
      m_pCRes22 = compactMemory(m_pCRes22, mem_res22_name);
      delete m_pRes22;
      m_pRes22 = NULL;
    }
  return 0;
}

int init_res1mini()
{
  /*
    char mem_res1mini_name[1024];
    char *filename = "dummy_res1mini";
    make_good_name(filename, "-CompRuleSet", mem_res1mini_name);
    if (m_pRes1Mini)
      {
        m_pCRes1Mini = CompactRuleSet(5, m_pRes1Mini);
        m_pCRes1Mini = compactMemory(m_pCRes1Mini, mem_res1mini_name);
        delete m_pRes1Mini, m_pRes1Mini = NULL;
      }
  */
  return 0;
}

extern "C" int
res_rule_load(int *res1_mini, int *res2_mini, int fromwhere)
{
  UNREFERENCED_PARAMETER(res1_mini);
  int result = 0; // success
  calledFromCacheMgr = fromwhere;

  // serialize initializations of res and t1-t4 modules
  // (because memory usage is high during initialization)
  //EnterCriticalSection(&__csec);
  __csec.enter();

  res_unload(); // unmap all file view. Shared memory still there if exists.

  result = init_res(1, "res1", "-RES1C", "-R1R"
                     , m_pRes1, m_pCRes1, RES1C, R1R);
  result |= init_res(2, "res2", "-RES2H", "-R2R"
                     , m_pRes2, m_pCRes2, RES2H, R2R);
  if (*res2_mini)
    result |= init_res2mini();
  result |= init_res22();
  result |= init_res1mini();

  if (result)
    res_unload();

  //LeaveCriticalSection(&__csec);
  __csec.leave();

  return result;
}

extern "C" int
VTRIN(int rules, short* rule_buffer, int rule_number)
{
  CompRuleSet* crs = 1 == rules ? m_pCRes1 :
    2 == rules ? m_pCRes2 :
    3 == rules ? m_pCRes22 :
    4 == rules ? m_pCRes2Mini :
    5 == rules ? m_pCRes1Mini : NULL;
  if (!crs)
    return 0; // error

  int ruleIx = rule_number - 1;
  if (ruleIx < 0 || ruleIx >= crs -> numRules)
    return 0;
  CompRuleHdr* hdr = crs -> r[ruleIx];

  short* p = rule_buffer;
  short* q = (short*)((char*)hdr + hdr->vOff);
	
  int i;
  for (i = 1; ; p++, q++, i++)
    {
      p[0] = q[0];
      if (999 == *p)
        break;
    }

  return i;
}

static int
VTRIN1ST(short* rules, short* rule_buffer, short* rule_number, short* part_number)
{
	ZeroMemory(rule_buffer, 26*sizeof(short));
	int result = 1; // error
	CLgsRuleSet* rs = 1 == *rules ? m_pRes1 :
					 2 == *rules ? m_pRes2 :
					 3 == *rules ? m_pRes22 :
					 4 == *rules ? m_pRes2Mini :
					 5 == *rules ? m_pRes1Mini : NULL;
	if ( !rs ) return result;

	int ruleIx = *rule_number - 1;
	if ( ruleIx < 0 || ruleIx >= rs -> NumberOfRules() ) return result;

	CLgsRule* r = rs -> GetItem(ruleIx);
	if ( !r ) return result;

	int reqPartIx = *part_number - *rule_number;

	CLgsVtrStatement** vtrs;
	int numVtrs = r -> VtrStatements(&vtrs);

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
		//int numVtrParts = numNums/26 + (numNums%26 > 0 ? 1 : 0);
		int numVtrParts = numNums/26 + 1;
		for ( int j = 0; j < numVtrParts; j++ )
		{
			if ( reqPartIx != j )
				continue;

			result = 0; // success

                        int k;
			for ( k = 0; k < 26 && j*26 + k < numNums; k++ )
				rule_buffer[k] = nums[j*26 + k];

			if ( j == numVtrParts -1 && k < 26 )
				rule_buffer[k] = 999;
		}
	}
	else
	{
		result = 0;
		rule_buffer[0] = 999;
	}

	return result;
}

extern "C" int
COMIN(short* rules, char* rule_buffer, short* rule_number)
{
	CompRuleSet* crs = 1 == *rules ? m_pCRes1 :
					 2 == *rules ? m_pCRes2 :
					 3 == *rules ? m_pCRes22 :
					 4 == *rules ? m_pCRes2Mini :
					 5 == *rules ? m_pCRes1Mini : NULL;
	if ( !crs ) return 1;

	int ruleIx = *rule_number - 1;
	if ( ruleIx < 0 || ruleIx >= crs -> numRules ) return 1;
	CompRuleHdr* hdr = crs -> r[ruleIx];

	CopyMemory(rule_buffer, hdr -> comment, 80);
	return 0;
}

static int
COMIN1ST(short* rules, char* rule_buffer, short* rule_number)
{
	int result = 1; // error
	CLgsRuleSet* rs = 1 == *rules ? m_pRes1 :
					 2 == *rules ? m_pRes2 :
					 3 == *rules ? m_pRes22 :
					 4 == *rules ? m_pRes2Mini :
					 5 == *rules ? m_pRes1Mini : NULL;
	if ( !rs ) return result;

	int ruleIx = *rule_number - 1;
	if ( ruleIx < 0 || ruleIx >= rs -> NumberOfRules() ) return result;

	CLgsRule* r = rs -> GetItem(ruleIx);
	if ( !r ) return result;

	// Use Description() function instead CommentDisplayString()
	// because the last one adds level value which we don't want
	const char* com = r -> Description();
	if ( com )
	{
		int len = strlen(com);
		if ( len > 0 )
		{
			int lenlen = len < 80 ? len : 80-1;
			strncpy(rule_buffer, com, lenlen);
			rule_buffer[lenlen] = ' ';
		}
	}
	return result;
}

// this routine has been taken as it was from the old res_rule_io.cpp file
extern "C" void
IDXVAL(short* swc, short* type, short *start, short* number)
{
	*start = 0;
	*number = 0;

	if ( *type > 0 && *type < 1000)
	{
		*start  = (*typos)[*swc-1][*type-1];
		if (*start > 0 ) 
		{
		   *number = (*noftyp)[*swc-1][*type-1];
		}
	}

	return;
}

extern "C" void
print_res_rule(FILE* file, int type, int rule_number)
{
	CompRuleSet* crs = 1 == type ? m_pCRes1 :
					 2 == type ? m_pCRes2 :
					 3 == type ? m_pCRes22 :
					 4 == type ? m_pCRes2Mini :
					 5 == type ? m_pCRes1Mini : NULL;
	if ( !crs )
	{
		fprintf(file, "ERROR: print_res_rule: invalid argument: type: %d\n", type);
		return;
	}

	int ruleIx = rule_number - 1;
	if ( ruleIx < 0 || ruleIx >= crs -> numRules )
	{
		fprintf(file, "ERROR: print_res_rule: rule number is out of range: %d\n", rule_number);
		return;
	}

	CompRuleHdr* hdr = crs -> r[ruleIx];

	// print header
	fprintf(file, "\n%s rule #%d, ID: %d\n",
		 1 == type ? "Res1" :
		 2 == type ? "Res2" :
		 3 == type ? "Res22" :
		 4 == type ? "Res2 Mini" :
		 5 == type ? "Res1 Mini" : "<unknown type>", rule_number, hdr -> ruleId);

	// print comment
	fprintf(file, "%80.80s\n", hdr -> comment);

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

	if ( 3 != type )
	{
		// print specificity for rules other that res22
		fprintf(file, "  spec: %d", hdr->rule[11]);
	}

	fprintf(file, "\n");

	// if there is an overflow sp record, print it
	if ( OVERFLOW_RECORD == hdr->rule[10] )
	{
		short* o = (short*)((char*)hdr + sizeof(CompRuleHdr));
		fprintf(file, "  ");
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
	for ( off = hdr -> tOff; off < hdr -> vOff; off += 21*sizeof(short) )
	{
		fprintf(file, "  {");
		short* t = (short*)((char*)hdr + off);
		for ( int i = 0; i < 21; i++ )
			fprintf(file, " %4d", t[i]);
		fprintf(file, " }\n");
	}

	// print vtrs
	for ( off = hdr -> vOff; off < hdr -> len; off += 26*sizeof(short) )
	{
		fprintf(file, "  ");
		short* t = (short*)((char*)hdr + off);
		for ( int i = 0; i < 26; i++ )
		{
			if ( i == 26 - 2 && 888 == t[i] && 888 == t[i+1] )
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


CompRuleSet* ResRuleSetFromRuleNumber(int nType)
{
	CompRuleSet* crs = 1 == nType ? m_pCRes1 :
					 2 == nType ? m_pCRes2 :
					 3 == nType ? m_pCRes22 :
					 4 == nType ? m_pCRes2Mini :
					 5 == nType ? m_pCRes1Mini : NULL;
	
	return  crs;
	
}

/*****************************************************************************
This function returns the ruleID of the rule object corresponding to RuleNumber and Type
from the rule the rule set loaded.
******************************************************************************/
int ResRuleIDFromRuleNumber(int nType, int nRuleNumber)
{
	CompRuleSet* pRuleSet = ResRuleSetFromRuleNumber(nType);
	if(pRuleSet)
	{
		int ruleIx = nRuleNumber - 1;
		//Check the range
		if ( ruleIx < 0 || ruleIx >= pRuleSet -> numRules )
         return -1;

		CompRuleHdr* hdr= pRuleSet -> r[ruleIx];
		if(hdr)
         return hdr->ruleId;
		else
         return -1;
	}
	else
		return -1;
}
