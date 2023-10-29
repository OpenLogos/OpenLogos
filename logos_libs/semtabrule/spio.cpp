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
//***************************************************************
// This module is C interface to C++ classes to retrieve
// semtab rules stored in relational database.
// This module is written to replace old SPIO fortran calls
// which retrieved the rules for C-Tree.
//
// The interface provided by this module will be used by
// the translation engine which is still in fortran.
//
// This module should be discarded when tranlation engine
// is rewritten/ported to C++.
//
// Author: Manoj Agarwala
// History:  10/23/96 Originally concieved
//***************************************************************

#include <logos_include/logoscommon.h>
#include /**/ <stdio.h>
#include <logos_libs/semtabrule/spio.h>
#include <logos_libs/sql/sqlexception.h>
#include <logos_libs/semtabrule/semtabrulebuilder.h>
// #include <logos_libs/odbcsql/odbcconnection.h>
#include <lgs_db_io/dbinstances.h>
#include <logos_libs/utility/argumentexception.h>
#include <logos_libs/multithreadlib/lgsthrlocalstore.h>

#define SMR_SIZE  (sizeof(short)*\
	(SP5KEY_ARRAY_SIZE + \
	 SP5DATA_ARRAY_SIZE + \
	 SP5VTR_ARRAY_SIZE)) + 4

extern GlobalDBInstances *globalObjects;

//~90M, 21k rules max for German source
const unsigned long MAX_CACHE_SIZE = 21000*SMR_SIZE;
unsigned long cacheSize = 0;

//Private function used by spread
short _populateSP5Key(CSemtabRule &sr, short sp5key[SP5KEY_ARRAY_SIZE]);
short _populateSP5Data(CSemtabRule &sr, short sp5data[SP5DATA_ARRAY_SIZE]);
short _populateSP5Vtr(CSemtabRule &sr, short sp5vtr[SP5VTR_ARRAY_SIZE]);

#define SMR_CACHE
#include "SMRCache.h"

inline void * ZeroMemory(void *mem, size_t n) {
  return memset(mem, 0, n);
}

inline void * CopyMemory(void *dest, void *src, size_t n) {
  return memcpy(dest, src, n);
}


bool CompareMemory(char* loc1, char* loc2, int len)
{
	for ( int i = 0; i < len; i++ )
	{
		if ( loc1[i] != loc2[i] )
			return false;
	}
	return true;
}

static SMRCache smrCache;
static bool smrNeedsCleaning = true;

void LSPREADCLEAN(void)
{
	if(smrNeedsCleaning) {
		smrNeedsCleaning = false;
		smrCache.cleanup();
	}
}

// The documentation for spread parameters is in spread.h file
void LSPREAD(short *rmode, short indexKey[5], short sp5key[SP5KEY_ARRAY_SIZE],
             short sp5data[SP5DATA_ARRAY_SIZE], short sp5vtr[SP5VTR_ARRAY_SIZE],
             short *retCode)
{
   *retCode = 0;
   LSPStaticData & staticData = globalObjects->srbStaticData();

   CSemtabRuleBuilder *p_srb = staticData.p_srb;
   CSemtabRuleVector & semtabRules = staticData.semtabRules;
   bool bCacheRule = true;
   try
   {
      int & numItemsFetched = staticData.numItemsFetched;
      int & numOfItemsUsed = staticData.numOfItemsUsed;
      short* rules = smrCache.get(indexKey[2], indexKey[3], indexKey[4]);

      //Query the database
      if (*rmode == 1)
      {
         if (!rules)
         {
            char buffer[8];

            sprintf(buffer, "%02d", indexKey[0]);
            LgsString sourceLangCd = buffer;

            sprintf(buffer, "%02d", indexKey[1]);
            LgsString targetLangCd = buffer;

            int wordClass = indexKey[2];
            int subset = indexKey[3];
            int set = indexKey[4];

            LgsString companyCd = "%";
            LgsString deactivationSwitch = "0"; //0 means active

            semtabRules.erase(semtabRules.begin(), semtabRules.end());

//DWORD dwbefore = ::GetTickCount();

            p_srb->QueryAndFetch(sourceLangCd, targetLangCd, wordClass, subset, set,
                                 companyCd, deactivationSwitch, semtabRules);

            numItemsFetched = semtabRules.size();
            numOfItemsUsed = 0;

//DWORD dwafter = ::GetTickCount();
//char tmp[256];
//sprintf(tmp, "%10ld: (%03d %03d %03d) - %5d", dwafter - dwbefore, wordClass, subset, set, numItemsFetched);
//cout << endl << tmp;

#if defined(SMR_CACHE)
				// first int in the buffer is the #of rules fetched,
				//  followed by SMR-blocks. numRulesFetched is restored
				//   each time this function is called with rmode == 1,
				//    and rules != null.
            if (numItemsFetched > 0)
            {
               int ruleSize = sizeof(short) + numItemsFetched*SMR_SIZE;
               if (cacheSize < MAX_CACHE_SIZE)
               {
                  cacheSize += ruleSize;
               }
               else
               {
                  bCacheRule = false;
               }
               rules = (short*)new char[ruleSize];
               ZeroMemory(rules, sizeof(short) + numItemsFetched*SMR_SIZE);
               *rules = numItemsFetched;
            }
            else
               rules = (short*)-1;

            for (int i = 0; i < numItemsFetched; i++)
            {
               char* rule_base = ((char*)rules) + sizeof(short);
               *retCode = _populateSP5Key(semtabRules[i], (short*)
               (rule_base + i*SMR_SIZE));

               if (*retCode == 0)
               {
                  *retCode = _populateSP5Data(semtabRules[i], (short*)
					(rule_base + i*SMR_SIZE + SP5KEY_ARRAY_SIZE*sizeof(short)));
               }

               if (*retCode == 0)
               {
                  *retCode = _populateSP5Vtr(semtabRules[i], (short*)
                     (rule_base + i*SMR_SIZE + SP5KEY_ARRAY_SIZE*sizeof(short) +
                                             SP5DATA_ARRAY_SIZE*sizeof(short)));
               }
            }
            if (bCacheRule)
            {
               smrCache.put(indexKey[2], indexKey[3], indexKey[4], rules);
            }
#endif // SMR_CACHE

         }
         else
         {
            numItemsFetched = rules == ((short*)-1) ? 0 : *rules;
            numOfItemsUsed = 0;
         }
      }

      if (*retCode != 0)
         ; // buffer overflow during populating rules buffer
      else if (rules == (short*)-1)
         *retCode = 2; // no matching rules
      else if (numOfItemsUsed == numItemsFetched)
         *retCode = 1; // end of rules
      else
      {
         if (rules)
         {
            char* entry = ((char*)rules + sizeof(short)) + numOfItemsUsed*SMR_SIZE;
            CopyMemory(sp5key, entry, SP5KEY_ARRAY_SIZE*sizeof(short));
            CopyMemory(sp5data, entry + SP5KEY_ARRAY_SIZE*sizeof(short), SP5DATA_ARRAY_SIZE*sizeof(short));
            CopyMemory(sp5vtr, entry + SP5KEY_ARRAY_SIZE*sizeof(short) + SP5DATA_ARRAY_SIZE*sizeof(short),
                       SP5VTR_ARRAY_SIZE*sizeof(short));
/*
bool diff = false;
if ( !CompareMemory((char*)xkey, (char*)sp5key, SP5KEY_ARRAY_SIZE*sizeof(short)) )
	diff = true;
if ( !CompareMemory((char*)xdata, (char*)sp5data, SP5DATA_ARRAY_SIZE*sizeof(short)) )
	diff = true;
if ( !CompareMemory((char*)xvtr, (char*)sp5vtr, SP5VTR_ARRAY_SIZE*sizeof(short)) )
	diff = true;

if ( diff )
{
	int iii = 5;
	cout << endl << indexKey[2] << " " << indexKey[3] << " " << indexKey[4] << " " << rules;
}
*/
			}
			else
			{
				//cout << endl << endl << "UNEXPECTED CODE EXECUTED";
				*retCode = _populateSP5Key(semtabRules[numOfItemsUsed], sp5key);

				if (*retCode == 0)
					*retCode = _populateSP5Data(semtabRules[numOfItemsUsed], sp5data);

				if (*retCode == 0)
					*retCode = _populateSP5Vtr(semtabRules[numOfItemsUsed], sp5vtr);
			}
			numOfItemsUsed++;
		}
		if (!bCacheRule)
		{
			delete[] rules;
		}
   }
   catch( SqlException& x)
   {
      cerr << "LSPREAD:" << x.Message() << endl;
      *retCode = 6; //Database Error
   }
   catch ( ArgumentException &ae)
   {
      cerr << "LSPREAD:" << ae.Message() << endl;
      *retCode = 7; //INI file realted error
   }
}

DLLEXPORT void * LSPREAD_PTR(short *rmode, short indexKey[5], 
				short *retCode, char *static_buffer, int *bReturningPtr) {

	*bReturningPtr = 1;

   *retCode = 0;
   char *retRulePtr = NULL;

   LSPStaticData & staticData = globalObjects->srbStaticData();

   bool bCacheRule = false;
   bool bDeleteNeeded = false;

   CSemtabRuleBuilder *p_srb = staticData.p_srb;
   CSemtabRuleVector & semtabRules = staticData.semtabRules;
   try
   {
      int & numItemsFetched = staticData.numItemsFetched;
      int & numOfItemsUsed = staticData.numOfItemsUsed;
      short* rules = smrCache.get(indexKey[2], indexKey[3], indexKey[4]);
	  bCacheRule = (rules!=0);

      //Query the database
      if (*rmode == 1) {
         if (!rules) {
            char buffer[8];

            sprintf(buffer, "%02d", indexKey[0]);
            LgsString sourceLangCd = buffer;

            sprintf(buffer, "%02d", indexKey[1]);
            LgsString targetLangCd = buffer;

            int wordClass = indexKey[2];
            int subset = indexKey[3];
            int set = indexKey[4];

            LgsString companyCd = "%";
            LgsString deactivationSwitch = "0"; //0 means active

            semtabRules.erase(semtabRules.begin(), semtabRules.end());

            p_srb->QueryAndFetch(sourceLangCd, targetLangCd, wordClass, 
				subset, set, companyCd, deactivationSwitch, semtabRules);

            numItemsFetched = semtabRules.size();
            numOfItemsUsed = 0;

				// first int in the buffer is the #of rules fetched,
				//  followed by SMR-blocks. numRulesFetched is restored
				//   each time this function is called with rmode == 1,
				//    and rules != null.
            if (numItemsFetched > 0) {
				int ruleSize = 2*sizeof(short) + numItemsFetched*SMR_SIZE;
				bCacheRule = (cacheSize < MAX_CACHE_SIZE);
				if(bCacheRule)
					cacheSize += ruleSize;
				else	// rules will not be put into the cache but copied
						// into caller provided buffer and deleted afterwards
					bDeleteNeeded = true;

				rules = (short*)new char[ruleSize];
				ZeroMemory(rules, ruleSize);

               *rules = numItemsFetched;
            } else
               rules = (short*)-1;

			if((rules != (short*)-1) && rules) {
	            char* rule_base = ((char*)rules) + sizeof(short)*2;
	            for (int i = 0; i < numItemsFetched; i++) {
					*retCode = _populateSP5Key(semtabRules[i], (short*)
						(rule_base + i*SMR_SIZE));

					if (*retCode == 0) {
						*retCode = _populateSP5Data(semtabRules[i], (short*)
							(rule_base + i*SMR_SIZE + 
							SP5KEY_ARRAY_SIZE*sizeof(short)));
					}

					if (*retCode == 0) {
						*retCode = _populateSP5Vtr(semtabRules[i], (short*)
							(rule_base + i*SMR_SIZE + SP5KEY_ARRAY_SIZE*sizeof(short) +
							SP5DATA_ARRAY_SIZE*sizeof(short)));
					}
				}

				if(bCacheRule)
					smrCache.put(indexKey[2], indexKey[3], indexKey[4], rules);

			}
         } else {	// cache hit
            numItemsFetched = rules == ((short*)-1) ? 0 : *rules;
            numOfItemsUsed = 0;
			bCacheRule = true;
         }
      }

      if (*retCode != 0) {
         ; // buffer overflow during populating rules buffer
      } else if (rules == (short*)-1)
         *retCode = 2; // no matching rules
      else if (numOfItemsUsed >= numItemsFetched)
         *retCode = 1; // end of rules
      else {
         if ((rules!=(short*)-1) && rules) {
			 if(bCacheRule) {
				retRulePtr = ((char*)rules + sizeof(short))
					+ numOfItemsUsed*SMR_SIZE;
				*bReturningPtr = 1;
			 } else {
//printf("CACHE TOO SMALL sz = %8.2fK, max = %8.2fK\n", cacheSize/1024.,
//	   MAX_CACHE_SIZE/1024.);
				 char* entry = ((char*)rules + 2*sizeof(short)) 
					+ numOfItemsUsed*SMR_SIZE;
				CopyMemory(static_buffer+sizeof(short), entry, SMR_SIZE);
				retRulePtr = static_buffer;
				*bReturningPtr = 0;

				if(bDeleteNeeded)
					delete[] rules;
			 }
		 } else {
//printf("NOT IN CACHE AND RMODE!=1\n");
			short *shft = (short *)static_buffer + 1;
			*retCode = _populateSP5Key(semtabRules[numOfItemsUsed], shft);

			shft += SP5KEY_ARRAY_SIZE;
			if (*retCode == 0)
				*retCode = _populateSP5Data(semtabRules[numOfItemsUsed], shft);

			shft += SP5DATA_ARRAY_SIZE;
			if (*retCode == 0)
				*retCode = _populateSP5Vtr(semtabRules[numOfItemsUsed], shft);

			*bReturningPtr = 0;
			retRulePtr = static_buffer;
		 }

		  numOfItemsUsed++;

	  }
		   
	}
   catch( SqlException& x)
   {
      cerr << "LSPREAD:" << x.Message() << endl;
      *retCode = 6; //Database Error
   }
   catch ( ArgumentException &ae)
   {
      cerr << "LSPREAD:" << ae.Message() << endl;
      *retCode = 7; //INI file realted error
   }

	return retRulePtr; 

}

short _populateSP5Key(CSemtabRule &sr, short sp5key[SP5KEY_ARRAY_SIZE])
{
        short expectedLengthOfKey = 8 + sr.getSPElements().size() * 5;

        if(expectedLengthOfKey >  SP5KEY_ARRAY_SIZE )
        {
                sp5key[0] = 1; //1 becuase only sp5key[0] is valid
                return 3;
        }

        sp5key[0] = expectedLengthOfKey ;

        sp5key[1] = sr.getWordClassCode();
        sp5key[2] = sr.getSubsetId();
        sp5key[3] = sr.getSetId();
        sp5key[4] = sr.getSpecificity();
        sp5key[5] = sr.getSPElements()[0].getWordClass();
        sp5key[6] = sr.getSPElements()[0].getType();
        sp5key[7] = sr.getSPElements()[0].getForm();
        sp5key[8] = sr.getRuleLevel();

        int nextEntryIndex = 9;

        int i;
        for(i=1; i < sr.getSPElements().size(); i++)
        {
                sp5key[9  + (i-1)*3] = sr.getSPElements()[i].getWordClass();
                sp5key[10 + (i-1)*3] = sr.getSPElements()[i].getType();
                sp5key[11 + (i-1)*3] = sr.getSPElements()[i].getForm();

                nextEntryIndex += 3;
        }

        for(i=0; i < sr.getSPElements().size(); i++)
        {
                sp5key[nextEntryIndex ] = sr.getSPElements()[i].getTags()[0];
                sp5key[nextEntryIndex + 1] = sr.getSPElements()[i].getTags()[1];

                nextEntryIndex += 2;
        }


        char *tmp = (char *)sp5key + (nextEntryIndex*2);

        //3 letter company code and terminating null
        strncpy(tmp, sr.getCompanyCode().c_str(), 4);

        return 0;
}


short _populateSP5Data(CSemtabRule &sr, short sp5data[SP5DATA_ARRAY_SIZE])
{

                memset(sp5data, 0, (int)(SP5DATA_ARRAY_SIZE*sizeof(short)) );

                //Ignore first 30 bytes

        //Make sure we take care of odd length comments
        int numOfShortsInComment = sr.getCommentLine().length()/2 + sr.getCommentLine().length()%2;

        sp5data[15] = numOfShortsInComment ;//Number of shorts in comment

        //Check whether sp5data has enough buffer size
        //16 for space upto comment length, +9 for displacements
        //for 9 tagssets, another +9 for terminating -1 for overflow tags
        if( SP5DATA_ARRAY_SIZE < 16+numOfShortsInComment+9+9 )
        {
                sp5data[0] = 1; //1 becuase only sp5key[0] is valid
                return 4; //data will not fit in sp5data buffer
        }

        char *tmp = (char *)sp5data;
        strncpy(tmp+32, sr.getCommentLine().c_str(), numOfShortsInComment*2 );

        short tagIIndex = 16 + numOfShortsInComment ;

        short overflowTagIndex = tagIIndex+9; //+9 is space for tagset displacements

        for(int i =0; i<9; i++)
        {
            sp5data[tagIIndex + i] =  overflowTagIndex +1;//+1 to make it 1 based

            //If there is a corresponding tagset
            if( i < sr.getSPElements().size() )
            {
                    if( overflowTagIndex + sr.getSPElements()[i].getTags().size()-2
                            >= SP5DATA_ARRAY_SIZE )
                    {
                            sp5data[0] = 1; //1 becuase only sp5key[0] is valid
                            return 4; //data will not fit in sp5data buffer
                    }

                    //Overflow tags start at index 2, tag 0 and 1 are part of sp5key
                    for(int j=2; j< sr.getSPElements()[i].getTags().size(); j++)
                    {
                            sp5data[overflowTagIndex] = sr.getSPElements()[i].getTags()[j];

                            overflowTagIndex++;
                    }
            }

            if( overflowTagIndex >= SP5DATA_ARRAY_SIZE )
            {
                    sp5data[0] = 1; //1 becuase only sp5key[0] is valid
                    return 4; //data will not fit in sp5data buffer
            }

            sp5data[overflowTagIndex] = -1; // To indicate the end of overflow tags
            overflowTagIndex++;
        }

        //We don't worry about the last increment of overflowTagIndex
        //as that makes it one based and sp5data[0] contains the size
        //of relevant data in sp5data and not index
        //Store the length of relevant data in sp5data[0]
        sp5data[0] = overflowTagIndex;

        return 0;
}

short _populateSP5Vtr(CSemtabRule &sr, short sp5vtr[SP5VTR_ARRAY_SIZE])
{
        if( sr.getVtrs().size() >= SP5VTR_ARRAY_SIZE )
        {
                sp5vtr[0] = 0 ; //Number of VTR numbers in the array
                return 5; // Data will not fit into buffer passed for sp5vtr
        }

        //Number of VTR numbers in the array, +1 to count itself
        sp5vtr[0] = sr.getVtrs().size() +1;

        for(int i=0; i < sr.getVtrs().size(); i++)
        {
                //i+1 because 0 is already used for the number of vtrs
                sp5vtr[i+1] = sr.getVtrs()[i];
        }

        return 0;
}

unsigned long getCacheSize(void)
{
    return cacheSize;
}
