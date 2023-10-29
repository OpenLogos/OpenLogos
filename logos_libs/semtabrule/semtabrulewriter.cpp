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
#include /**/ <stdio.h>

#if defined(_WINDOWS) || defined(WIN32)
        #include /**/ <winsock.h> //for htons - on Unix it will be different,
                                                 //we will have to enclose it in ifdef
#else //Else part is for UNIX and is not tested
        #include /**/ <sys/types.h>
        #include /**/ <netinet/in.h>
#endif

#include <logos_libs/semtabrule/semtabrulewriter.h>
#include <logos_libs/sql/logossql.h>
#include <logos_libs/odbcsql/odbcsequencegenerator.h>

CSemtabRuleWriter::CSemtabRuleWriter(SqlConnection *pConnection)
                  :m_pSqlConnection(pConnection),
                   m_pSqlStatement(NULL),
                   m_pSequenceGenerator(NULL)
{
}

CSemtabRuleWriter::~CSemtabRuleWriter()
{
   if (m_pSqlStatement)
      delete m_pSqlStatement;
   if (m_pSequenceGenerator)
      delete m_pSequenceGenerator;
}

void CSemtabRuleWriter::Insert(CSemtabRule &sr)
{
   CreateSqlStatement();

   if (sr.getSemtabId() < 0)
   {
      int semtabId = m_pSequenceGenerator->incrementAndGetNextSequence("Semtab_Data");
      sr.setSemtabId(semtabId);
   }

   getStatement()->BindInputInteger(":si", & sr.getSemtabId());
   getStatement()->BindInputString(":cc", sr.getCompanyCode().c_str());
   getStatement()->BindInputString(":slc", sr.getSourceLanguageCode().c_str());
   getStatement()->BindInputString(":tlc", sr.getTargetLanguageCode().c_str());

   getStatement()->BindInputInteger(":wcc", &sr.getWordClassCode());
   getStatement()->BindInputInteger(":ssi", &sr.getSubsetId());
   getStatement()->BindInputInteger(":sti", &sr.getSetId());

   getStatement()->BindInputString(":smc", sr.getSubjectMatterCode().c_str());
   getStatement()->BindInputInteger(":rl", &sr.getRuleLevel());
   getStatement()->BindInputInteger(":sy", &sr.getSpecificity());

   getStatement()->BindInputString(":cl", sr.getCommentLine().c_str());

   LgsString ds;
   ds = sr.getDeactivationSwitch();
   getStatement()->BindInputString(":ds", ds.c_str());

   int wc[9];
   int type[9];
   int form[9];
   int tag[9][2];

   for (int i = 0; i < 9; i++)
   {
      if (i < sr.getSPElements().size())
      {
         wc[i] = sr.getSPElements()[i].getWordClass();
         type[i] = sr.getSPElements()[i].getType();
         form[i] = sr.getSPElements()[i].getForm();
         tag[i][0] = sr.getSPElements()[i].getTags()[0];
         tag[i][1] = sr.getSPElements()[i].getTags()[1];
      }
      else
      {
         wc[i] = 0;
         type[i] = 0;
         form[i] = 0;
         tag[i][0] = 0;
         tag[i][1] = 0;
      }
      char wc_pn[8];       // WordClass parameter name
      char type_pn[8];     // Type parameter name
      char form_pn[8];     // Form parameter name
      char tag_pn1[8];     // Tag parameter name
      char tag_pn2[8];     // Tag parameter name

      sprintf(wc_pn,":w%d", i+1);
      sprintf(type_pn,":t%d", i+1);
      sprintf(form_pn,":f%d", i+1);
      sprintf(tag_pn1,":tag%d1", i+1);
      sprintf(tag_pn2,":tag%d2", i+1);

      getStatement()->BindInputInteger(wc_pn, &wc[i]);
      getStatement()->BindInputInteger(type_pn, &type[i]);
      getStatement()->BindInputInteger(form_pn, &form[i]);
      getStatement()->BindInputInteger(tag_pn1, &tag[i][0]);
      getStatement()->BindInputInteger(tag_pn2, &tag[i][1]);
   }

   //Create the blob
   char blob[2000];
   int bloblen = createBlob(blob, sr);

   getStatement()->BindInputInteger(":dbl", &bloblen);

   getStatement()->BindInputBlob(":db", blob, bloblen);

   getStatement()->BindInputTimeStamp(":dc", & sr.getDateCreated());

   getStatement()->BindInputString(":cb", sr.getCreatedBy().c_str());

   getStatement()->Execute();
}

int CSemtabRuleWriter::createBlob(char *blob, const CSemtabRule &sr)
{
   int bloblen = 0;

   int i;
   for (i = 0; i < sr.getSPElements().size(); i++)
   {
      //First 2 tags are outputed separately, hence - 2
      short tagSetILength = sr.getSPElements()[i].getTags().size() - 2;

      appendToBlob(blob, bloblen, tagSetILength);

      for (int j = 0; j < tagSetILength; j++)
      {
         short tag = sr.getSPElements()[i].getTags()[j+2];
         appendToBlob(blob, bloblen, tag);
      }
   }

   short vtrSize = sr.getVtrs().size() ;

   appendToBlob(blob, bloblen, vtrSize);

   for(i=0; i < vtrSize; i++)
   {
      short vtr = sr.getVtrs()[i];
      appendToBlob(blob, bloblen, vtr);
   }

   return bloblen;
}

void CSemtabRuleWriter::appendToBlob(char *blob, int &blobIndex, short numToBeAdded)
{
   short bigEndian = htons(numToBeAdded); //Blob is written in network byte order
   char *tmp = (char *)&bigEndian;

   blob[blobIndex] = tmp[0];
   blob[blobIndex+1] = tmp[1];
   blobIndex += 2;
}


void CSemtabRuleWriter::CreateSqlStatement()
{
   if (!m_pSqlStatement)
   {
      m_pSqlStatement = m_pSqlConnection->CreateStatement();

      LgsString s =      " insert into Semtab_Data "
                         " ( "
                         "  SEMTAB_ID, "
                         "  COMPANY_CODE, "
                         "  SOURCE_LANGUAGE_CODE, "
                         "  TARGET_LANGUAGE_CODE, "
                         "  WORD_CLASS_CODE, "
                         "  SUBSET_ID, "
                         "  SET_ID, "
                         "  SUBJECT_MATTER_CODE, "
                         "  DEACTIVATION_SWITCH, "
                         "  RULE_LEVEL, "
                         "  SPECIFICITY, "
                         "  COMMENTLINE, "
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
                         "  NUMBYTESINDATABLOB, "
                         "  DATA_BLOB, "
                         "  DATE_CREATED, "
                         "  CREATED_BY "
                         " ) "
                         " values ( "
                         " :si,:cc,:slc,:tlc,:wcc,:ssi,:sti,:smc,:ds, "
                         " :rl,:sy, :cl,"
                         " :w1,:t1,:f1,:w2,:t2,:f2,:w3,:t3, :f3, "
                         " :w4,:t4,:f4,:w5,:t5,:f5,:w6,:t6, :f6, "
                         " :w7,:t7,:f7,:w8,:t8,:f8,:w9,:t9, :f9, "
                         " :tag11,:tag12,:tag21,:tag22,:tag31,:tag32, "
                         " :tag41,:tag42,:tag51,:tag52,:tag61,:tag62, "
                         " :tag71,:tag72,:tag81,:tag82,:tag91,:tag92, "
                         " :dbl, :db, :dc, :cb "
                         " ) " ;

      m_pSqlStatement->AddToCommandString(s);
      m_pSqlStatement->Parse();
      m_pSequenceGenerator = new ODBCSequenceGenerator(m_pSqlConnection);
   }
}
