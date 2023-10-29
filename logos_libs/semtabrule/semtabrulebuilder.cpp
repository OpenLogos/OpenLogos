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
#if defined(_WINDOWS) || defined(WIN32)
        #include /**/ <winsock.h> //for ntohs - on Unix it will be different,
                                                 //we will have to enclose it in ifdef
#else //Else part is for UNIX and is not tested
        #include /**/ <sys/types.h>
        #include /**/ <netinet/in.h>
#endif

#include <logos_libs/semtabrule/semtabrulebuilder.h>

#include <logos_libs/sql/logossql.h>
#include /**/ <memory.h>
#include /**/ <time.h>

#include <logos_libs/dbcache/CacheSemtabRuleData.h>
#include <logos_libs/dbcache/CacheSemtabRuleQuery.h>

CSemtabRuleBuilder::CSemtabRuleBuilder(SqlConnection *pConnection)
                   :m_pSqlConnection(pConnection),
                    m_pSqlStatement(NULL),
                    fetchAll(false)
{
   for (int i = 1; i < 7; i++)
   {
      for (int j = 1; j < 7; j++)
      {
         fdata[i][j]= new CacheSemtabRuleData(NULL, i, j, false, false, false);
         if (fdata[i][j]->isValid())
         {
            fquery[i][j] = new CacheSemtabRuleQuery(fdata[i][j], 0, 0, 0, 0, NULL);
         }
         else
         {
            delete fdata[i][j];
            fdata[i][j] = NULL;
            fquery[i][j] = NULL;
         }
      }
   }
   srclStr = trglStr = NULL;
}

CSemtabRuleBuilder::~CSemtabRuleBuilder()
{

	if (m_pSqlStatement)
   {
      delete m_pSqlStatement;
   }

   for (int i = 1; i < 7; i++)
   {
      for (int j = 1; j < 7; j++)
      {
         if (fdata[i][j])
         {
            delete fdata[i][j];
            fdata[i][j] = NULL;
         }
         if (fquery[i][j])
         {
            delete fquery[i][j];
            fquery[i][j] = NULL;
         }
      }
   }
   if(srclStr) delete srclStr;
   if(trglStr) delete trglStr;
}


void CSemtabRuleBuilder::QueryAndFetchAll(const LgsString& srcLangCd,
                                          const LgsString& trgtLangCd,
                                          const LgsString& deactivationSwitch,
                                          CSemtabRuleVector& semtabRules)
{
   fetchAll = true;

   ConnectAndCreateSqlStatement();

   time_t startTime = time(NULL);

   getStatement()->BindInputString(":aSrcLangCd", srcLangCd.c_str());
   getStatement()->BindInputString(":aTrgtLangCd", trgtLangCd.c_str());
   getStatement()->BindInputString(":aDeactivationSwitch", deactivationSwitch.c_str());

   getStatement()->Execute();

   Fetch(semtabRules);

   cout << "Time to Execute and Fetch in seconds = " << time(NULL) - startTime << endl;

   //cerr << "Stopped after populating the vector, hit return to continue..." << flush;
   //getchar();

   time_t startSortTime = time(NULL);

   sort(semtabRules.begin(), semtabRules.end());

   cout << "Time to sort in seconds = " << time(NULL) - startSortTime << endl;
}


void CSemtabRuleBuilder::QueryAndFetch(const LgsString& srcLangCd, const LgsString& trgtLangCd,
                                       int wordClass, int subset, int set, const LgsString& companyCd,
                                       const LgsString& deactivationSwitch, CSemtabRuleVector& semtabRules)
{
   fetchAll = false;

   int previousNumOfItems = 0;

   //We don't need these rules as the translation engine
   //automatically requests these rules if does not find a
   //matching rule
   //
   //We also want rules with subsetid equal to setid
   /*Query(srcLangCd, trgtLangCd, wordClass, set, set, companyCd, deactivationSwitch);
   Fetch(semtabRules);
   sort(semtabRules.begin(), semtabRules.end());

   previousNumOfItems = semtabRules.size();*/

   //We also want rules with setid 0
   Query(srcLangCd, trgtLangCd, wordClass, subset, 0, companyCd, deactivationSwitch);
   Fetch(semtabRules);

   sort(semtabRules.begin()+previousNumOfItems, semtabRules.end());

   previousNumOfItems = semtabRules.size();

   Query(srcLangCd, trgtLangCd, wordClass, subset, set, companyCd, deactivationSwitch);
   Fetch(semtabRules);

   sort(semtabRules.begin()+previousNumOfItems, semtabRules.end());

   //Reverse the vector so that it sorted in the descending order
   reverse( semtabRules.begin(), semtabRules.end());
}

void CSemtabRuleBuilder::Query(const LgsString& srcLangCd, const LgsString& trgtLangCd,
                               int wordClass, int subset, int set, const LgsString& companyCd,
                               const LgsString& deactivationSwitch)
{
   fetchAll = false;
   //printf("SemtabruleBuilder.Query\n");
   //fflush(stdout);

   srcl = atoi(srcLangCd.c_str());
   trgl = atoi(trgtLangCd.c_str());

   if(srclStr) delete srclStr;
   if(trglStr) delete trglStr;
   srclStr = new LgsString(srcLangCd.c_str());
   trglStr = new LgsString(trgtLangCd.c_str());

   ds = deactivationSwitch.c_str()[0];
   wcc = wordClass;
   subsid = subset;
   setid = set;
   strcpy(cc, companyCd.c_str());

   //printf("CacheSemtabRuleQuery.query %c %d %d %d\n", ds, wcc, subsid, setid);
   //fflush(stdout);
   if ((srcl > 0) && (srcl < 3) && (trgl > 0) && (trgl < 7))
   {
      if (fdata[srcl][trgl])
      {
         //XXX - " ... company_code like :cc" should be changed to
         //	"... company_code=:cc"
         strcpy(cc, "LOG");
         int ret = fquery[srcl][trgl]->query(ds, wcc, subsid, setid, cc);
         //printf("CacheSemtabRuleQuery.query ret %d\n", ret);
         //fflush(stdout);
         return;
      }
   }

   ConnectAndCreateSqlStatement();

   getStatement()->BindInputString(":aSrcLangCd", srcLangCd.c_str());
   getStatement()->BindInputString(":aTrgtLangCd", trgtLangCd.c_str());
   getStatement()->BindInputInteger(":aWordClassCd", &wordClass);
   getStatement()->BindInputInteger(":aSubsetId", &subset);
   getStatement()->BindInputInteger(":aSetId", &set);
   getStatement()->BindInputString(":aCompanyCd", companyCd.c_str());
   getStatement()->BindInputString(":aDeactivationSwitch", deactivationSwitch.c_str());

   getStatement()->Execute();
}

static void push_spelem(CSPElement & spelem, CSemtabRule & sr, int w, int t, int f)
{
   spelem.setWordClassCode(w);
   spelem.setType(t);
   spelem.setForm(f);
   sr.getSPElements().push_back(spelem);
}

void CSemtabRuleBuilder::Fetch(CSemtabRuleVector& semtabRules)
{
   //printf("SemtabruleBuilder.Fetch\n");
   //fflush(stdout);
	if ((srcl > 0) && (srcl < 3) && (trgl > 0) && (trgl < 7))
   {
      if (fdata[srcl][trgl])
      {
         while (1)
         {
            int ret = fquery[srcl][trgl]->fetch(&semid, &wc1, &ty1, &fm1, &wc2, &ty2, &fm2,
                                                &wc3, &ty3, &fm3, &wc4, &ty4, &fm4, &wc5, &ty5, &fm5,
                                                &wc6, &ty6, &fm6, &wc7, &ty7, &fm7, &wc8, &ty8, &fm8,
                                                &wc9, &ty9, &fm9, &tag11, &tag12, &tag21, &tag22,
                                                &tag31, &tag32, &tag41, &tag42, &tag51, &tag52,
                                                &tag61, &tag62, &tag71, &tag72, &tag81, &tag82,
                                                &tag91, &tag92, &rl, &spec, &nb, blob);
            if (ret==0)
               break;

            //printf("\nCacheSemtabRuleQuery(%d %d %d %d \"%s\"): %d %d %d\n", 
            //	   ds, wcc, subsid, setid, cc, rl, spec, nb);
            //fflush(stdout);
            CSemtabRule tmp;
            semtabRules.push_back(tmp);

            //We obtain a reference back to the element just inserted
            CSemtabRule& sr = semtabRules[semtabRules.size()-1];

            sr.setSemtabId(semid);
            sr.setCompanyCode(cc);
            sr.setSourceLanguageCode(*srclStr);
            sr.setTargetLanguageCode(*trglStr);
            sr.setWordClassCode(wcc);
            sr.setSubsetId(subsid);
            sr.setSetId(setid);
            sr.setRuleLevel(rl);
            sr.setSpecificity(spec);
            sr.setDeactivationSwitch(ds);

            CSPElement sPElement;

            if (rl > 1)
               push_spelem(sPElement, sr, wc1, ty1, fm1);
            if (rl > 2)
               push_spelem(sPElement, sr, wc2, ty2, fm2);
            if (rl > 3)
               push_spelem(sPElement, sr, wc3, ty3, fm3);
            if (rl > 4)
               push_spelem(sPElement, sr, wc4, ty4, fm4);
            if (rl > 5)
               push_spelem(sPElement, sr, wc5, ty5, fm5);
            if (rl > 6)
               push_spelem(sPElement, sr, wc6, ty6, fm6);
            if (rl > 7)
               push_spelem(sPElement, sr, wc7, ty7, fm7);
            if (rl > 8)
               push_spelem(sPElement, sr, wc8, ty8, fm8);
            if (rl > 9)
               push_spelem(sPElement, sr, wc9, ty9, fm9);

            if (rl > 1)
               sr.getSPElements()[0].getTags().push_back(tag11);
            if (rl > 1)
               sr.getSPElements()[0].getTags().push_back(tag12);
            if (rl > 2)
               sr.getSPElements()[1].getTags().push_back(tag21);
            if (rl > 2)
               sr.getSPElements()[1].getTags().push_back(tag22);
            if (rl > 3)
               sr.getSPElements()[2].getTags().push_back(tag31);
            if (rl > 3)
               sr.getSPElements()[2].getTags().push_back(tag32);
            if (rl > 4)
               sr.getSPElements()[3].getTags().push_back(tag41);
            if (rl > 4)
               sr.getSPElements()[3].getTags().push_back(tag42);
            if (rl > 5)
               sr.getSPElements()[4].getTags().push_back(tag51);
            if (rl > 5)
               sr.getSPElements()[4].getTags().push_back(tag52);
            if (rl > 6)
               sr.getSPElements()[5].getTags().push_back(tag61);
            if (rl > 6)
               sr.getSPElements()[5].getTags().push_back(tag62);
            if (rl > 7)
               sr.getSPElements()[6].getTags().push_back(tag71);
            if (rl > 7)
               sr.getSPElements()[6].getTags().push_back(tag72);
            if (rl > 8)
               sr.getSPElements()[7].getTags().push_back(tag81);
            if (rl > 8)
               sr.getSPElements()[7].getTags().push_back(tag82);
            if (rl > 9)
               sr.getSPElements()[8].getTags().push_back(tag91);
            if (rl > 9)
               sr.getSPElements()[8].getTags().push_back(tag92);

            ExtractFromBlob(sr, nb, blob);
            //cout << "\nCacheSemtabRuleQuery: " << sr;
         }
         return;
      }
   }

   //Fetch all the rows
   while(getStatement()->Fetch())
   {
      //CSemtabRule added to the vector before it is
      //populated becuase it is more efficient to copy
      //unpopulated CSemtabRule to the vector than to
      //copy a populated CSemtabRule to the vector
      CSemtabRule tmp;
      semtabRules.push_back(tmp);

      //We obtain a reference back to the element just inserted
      CSemtabRule& sr = semtabRules[semtabRules.size()-1];

      sr.setSemtabId(getColumnValueAsInteger(SEMTAB_ID));
      sr.setCompanyCode(getColumnValueAsString(COMPANY_CODE));

      sr.setSourceLanguageCode(getColumnValueAsString(SOURCE_LANGUAGE_CODE));
      sr.setTargetLanguageCode(getColumnValueAsString(TARGET_LANGUAGE_CODE));

      sr.setCommentLine(getColumnValueAsString(COMMENTLINE));

      sr.setWordClassCode(getColumnValueAsInteger(WORD_CLASS_CODE));
      sr.setSubsetId(getColumnValueAsInteger(SUBSET_ID));
      sr.setSetId(getColumnValueAsInteger(SET_ID));

      sr.setRuleLevel(getColumnValueAsInteger(RULE_LEVEL));
      sr.setSpecificity(getColumnValueAsInteger(SPECIFICITY));

      sr.setDeactivationSwitch(getColumnValueAsString(DEACTIVATION_SWITCH)[0]);

      //WC1 is starting point in SqlColumnEnum for the relevant columns below
      int i, j = WC1;
      for (i = 0; i < sr.getRuleLevel() - 1 ; i++)
      {
         CSPElement sPElement;

         sPElement.setWordClassCode(getColumnValueAsInteger(j+i));
         sPElement.setType(getColumnValueAsInteger(j+i+1));
         sPElement.setForm(getColumnValueAsInteger(j+i+2));

         sr.getSPElements().push_back(sPElement);

         //j is increment only by 2 not 3 because next iteration
         //of for loop, i is increment by 1, hence i+j gets increment by 3
         j += 2;
      }

      j = TAG11;
      for (i=0; i < sr.getRuleLevel()-1; i++)
      {
         sr.getSPElements()[i].getTags().push_back(getColumnValueAsInteger(j+i));
         sr.getSPElements()[i].getTags().push_back(getColumnValueAsInteger(j+i+1));

         //j is increment only by 1 not 2 because next iteration
         //of for loop, i is increment by 1, hence i+j gets increment by 2
         j += 1;
      }

      int blobLength = getColumnValueAsInteger(NUMBYTESINDATABLOB);

      //printf("\nCacheSemtabRuleQuery(%d %d %d %d \"%s\"): %d %d %d\n", 
      //	   ds, wcc, subsid, setid, cc, sr.getRuleLevel(), 
      //	   sr.getSpecificity(), blobLength);
      //fflush(stdout);

      unsigned char *blob = getColumnValueAsLongRaw(DATA_BLOB);

      ExtractFromBlob(sr, blobLength, blob);
      //cout << "\nCacheSemtabRuleQuery: " << sr;

   } //End of while loop
}

void CSemtabRuleBuilder::ExtractFromBlob(CSemtabRule& sr, int blobLength, const unsigned char* blob)
{
   //Current byte offset in the blob from where data should be extracted
   int byteOffset = 0;
   
   int i;
   for (i = 0; i < sr.getRuleLevel() - 1; i++)
   {
      short tagSetILength = GetShortFromBlob(byteOffset, blob);

      //-2 because at least 2 more bytes will be neede for the VTR length
      if (byteOffset + 2 * tagSetILength > blobLength - 2)
      {
         LgsString msg = "Either the blob is corrupted or rule level is wrong.";
         throw msg;
      }

      for (int j = 0; j < tagSetILength ; j++)
      {
         sr.getSPElements()[i].getTags().push_back(GetShortFromBlob(byteOffset, blob));
      }
   }

   short vtrLength = GetShortFromBlob(byteOffset, blob);

   if (byteOffset+2*vtrLength > blobLength)
   {
      LgsString msg = "Either the blob is corrupted or rule level is wrong.";
      throw msg;
   }

   for (i=0; i< vtrLength; i++)
   {
      sr.getVtrs().push_back(GetShortFromBlob(byteOffset, blob));
   }
}

short CSemtabRuleBuilder::GetShortFromBlob(int& byteOffset, const unsigned char* blob) const
{
   const unsigned char* beginOfShort  = blob + byteOffset;
   short* p_NetworkShort = (short *) beginOfShort;
   short retval = ntohs(*p_NetworkShort);

   byteOffset += 2;
   return retval;
}

  
void CSemtabRuleBuilder::ConnectAndCreateSqlStatement()
{
   if (!m_pSqlStatement)
   {
      m_pSqlStatement = m_pSqlConnection->CreateStatement();

      LgsString s = "select                          "
                    "  Company_Code, "
                    "  Source_Language_Code, "
                    "  Target_Language_Code, "
                    "  Deactivation_Switch, "
                    "  Commentline, "
                    "  Semtab_ID, "
                    "  Word_Class_Code, "
                    "  Subset_ID, "
                    "  Set_ID, "
                    "  WC1, "
                    "  TY1, "
                    "  FM1, "
                    "  WC2, "
                    "  TY2, "
                    "  FM2, "
                    "  WC3, "
                    "  TY3, "
                    "  FM3, "
                    "  WC4, "
                    "  TY4, "
                    "  FM4, "
                    "  WC5, "
                    "  TY5, "
                    "  FM5, "
                    "  WC6, "
                    "  TY6, "
                    "  FM6, "
                    "  WC7, "
                    "  TY7, "
                    "  FM7, "
                    "  WC8, "
                    "  TY8, "
                    "  FM8, "
                    "  WC9, "
                    "  TY9, "
                    "  FM9, "
                    "  TAG11, "
                    "  TAG12, "
                    "  TAG21, "
                    "  TAG22, "
                    "  TAG31, "
                    "  TAG32, "
                    "  TAG41, "
                    "  TAG42, "
                    "  TAG51, "
                    "  TAG52, "
                    "  TAG61, "
                    "  TAG62, "
                    "  TAG71, "
                    "  TAG72, "
                    "  TAG81, "
                    "  TAG82, "
                    "  TAG91, "
                    "  TAG92, "
                    "  Rule_Level, "
                    "  Specificity, "
                    "  NumBytesInDataBlob, "
                    "  Data_Blob "
                    " from  Semtab_Data "
                    " where "
                    " Source_Language_Code = :aSrcLangCd "
                    " and Target_Language_Code = :aTrgtLangCd "
                    " and Deactivation_Switch = :aDeactivationSwitch ";

      if (!fetchAll)
      {
         s += LgsString (" and   Word_Class_Code = :aWordClassCd "
                         " and   Subset_ID = :aSubsetId "
                         " and   Set_ID = :aSetId "
                         " and   Company_Code like :aCompanyCd ");

                         //The reason order by cannot be used for sorting is
                         //becuase Microsoft SQL Server allows only upto
                         //16 fields in order by clause
                         /*" order by "
                         "  WORD_CLASS_CODE DESC, "
                         "  SUBSET_ID DESC, "
                         "  SET_ID DESC, "
                         "  WC1 DESC, "
                         "  TY1 DESC, "
                         "  FM1 DESC, "
                         "  RULE_LEVEL DESC, "
                         "  WC2 DESC, "
                         "  TY2 DESC, "
                         "  FM2 DESC, "
                         "  WC3 DESC, "
                         "  TY3 DESC, "
                         "  FM3 DESC, "
                         "  WC4 DESC, "
                         "  TY4 DESC, "
                         "  FM4 DESC, "
                         "  WC5 DESC, "
                         "  TY5 DESC, "
                         "  FM5 DESC, "
                         "  WC6 DESC, "
                         "  TY6 DESC, "
                         "  FM6 DESC, "
                         "  WC7 DESC, "
                         "  TY7 DESC, "
                         "  FM7 DESC, "
                         "  WC8 DESC, "
                         "  TY8 DESC, "
                         "  FM8 DESC, "
                         "  WC9 DESC, "
                         "  TY9 DESC, "
                         "  FM9 DESC, "
                         "  TAG11 DESC, "
                         "  TAG12 DESC, "
                         "  TAG21 DESC, "
                         "  TAG22 DESC, "
                         "  TAG31 DESC, "
                         "  TAG32 DESC, "
                         "  TAG41 DESC, "
                         "  TAG42 DESC, "
                         "  TAG51 DESC, "
                         "  TAG52 DESC, "
                         "  TAG61 DESC, "
                         "  TAG62 DESC, "
                         "  TAG71 DESC, "
                         "  TAG72 DESC, "
                         "  TAG81 DESC, "
                         "  TAG82 DESC, "
                         "  TAG91 DESC, "
                         "  TAG92 DESC, "
                         "  COMPANY_CODE DESC "*/
      }

      m_pSqlStatement->AddToCommandString(s);
      m_pSqlStatement->Parse();

      int i;
      for (i = 0; i< EndOfStringColumns; i++)
      {
         m_pSqlColumns[i] = m_pSqlStatement->BindOutputColumn(i + 1, SqlColumn::StringType);
      }

      for (i = EndOfStringColumns + 1; i< EndOfIntegerColumns; i++)
      {
         //Even though the first parameter to BindOutputColumn is 1 based
         //index, BindOutputColumn has i instead of i+1 since one entry
         //in the enumerated type is wasted by EndOfStringColumns
         m_pSqlColumns[i] = m_pSqlStatement->BindOutputColumn(i, SqlColumn::Integer);
      }

      //Even though the first parameter to BindOutputColumn is 1 based
      //index, BindOutputColumn has DATA_BLOB-1 instead of DATA_BLOB+1 since two entries
      //in the enumerated type is wasted by EndOfStringColumns and
      //EndOfIntegerColumns
      m_pSqlColumns[DATA_BLOB] = m_pSqlStatement->BindOutputColumn(DATA_BLOB - 1, SqlColumn::Long_Row);
   }//End Of If
}
