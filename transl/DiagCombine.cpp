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
/*
Program will read in the diagnostic files from 
Translation processes and combine them into one.
Each sentence in each diag file is separated by
the string '*EOS*' 
*/
#include <logos_include/logoscommon.h>
#include <transl/DiagCombine.h>
#include <configdatafileinterface/configdatainterfacemain.h>

#define INPUT_FILE_COUNT 11
/* For preventing openlogos to go in infinite loop when run with deep diagnostic choice. */
#define TEMPBUF_SIZE 5000

int DiagCombine(bool addJobInfo)
{
   int fcnt;
   int not_eof;
   int not_eos;
   
   char *ifileName[] =
   {
      "transl_in.diag", "res.diag", "parse1.diag", "parse2.diag",
         "parse3.diag", "parse4.diag", "tran1.diag", "tran2.diag",
         "tran3.diag", "tran4.diag", "transl_out.diag"
   };
   char *ifileNameKey[] =
   {
      "transl_in_diag", "res_diag", "parse1_diag", "parse2_diag",
         "parse3_diag", "parse4_diag", "tran1_diag", "tran2_diag",
         "tran3_diag", "tran4_diag", "transl_out_diag"
   };
   
   ifstream inputFiles[INPUT_FILE_COUNT];
   char fileName[MAX_FILEPATH_LEN];
   
   // open all the input files
   for (fcnt = 0; fcnt < INPUT_FILE_COUNT; fcnt++)
   { 
      GetConfigData("tempfile", ifileNameKey[fcnt], fileName, MAX_FILEPATH_LEN);
      inputFiles[fcnt].open(fileName);
      if (!inputFiles[fcnt].good())
      {
         cerr << "Unable to open input file " << ifileName[fcnt] << endl; 
      }
   }
   
   // open the output file
   GetConfigData("tempfile", "all_diag", fileName, MAX_FILEPATH_LEN);
   ofstream outputFile(fileName, ios::binary);
   if (!outputFile.good())
   {
      cerr << "Unable to open output file " << "all.diag" << endl;
      return 1;
   }
   
   // define buffer for lines to be read
   char tempbuf[TEMPBUF_SIZE];
   // write job information file to diagnostic file.
   if( addJobInfo )
   {
      GetConfigData("tempfile", "job_info", fileName, MAX_FILEPATH_LEN);
      ifstream jobInfoFile;
      jobInfoFile.open(fileName);
      if (!jobInfoFile.good())
      {
         cerr << "Unable to open input file " << "jobinfo" << endl; 
      }
      else
      {
         while (!jobInfoFile.eof())
         {
            jobInfoFile.getline(tempbuf, TEMPBUF_SIZE);
            if (jobInfoFile.good())
            {
               outputFile << tempbuf << endl; 
               if (!outputFile.good())
               {
                  cerr << "Error writing all.diag file." << endl;
                  return 1;
               }
            }
            else
            {
               break;
            }
         }
         jobInfoFile.close();
      }
   }
   
   // loop through the input files reading a sentence from each
   // and writing the sentence information to the output.
   not_eof = 0;
   while (not_eof < INPUT_FILE_COUNT)
   {
      not_eof = 0;
      for (fcnt = 0; fcnt < INPUT_FILE_COUNT; fcnt++)
      { 
         if (!inputFiles[fcnt].is_open())
         {
            not_eof++;
            continue;
         }
         not_eos = 1;
         while (not_eos)
         {
            inputFiles[fcnt].getline(tempbuf, TEMPBUF_SIZE);
            if (inputFiles[fcnt].eof())
            {
               not_eos = 0;
               not_eof++;
               continue;
            }
            if (!inputFiles[fcnt].good())
            {
               cerr << "Error reading file " << ifileName[fcnt] << endl;
               not_eos = 0;
               continue;
            }
            outputFile << tempbuf << endl; 
            if (!outputFile.good())
            {
               cerr << "Error writing file." << endl;
               return 1;
            }
            if ((memcmp(tempbuf, "*EOS*", 4) == 0) || (memcmp(tempbuf," *EOS*", 5) == 0))
            {
               not_eos = 0;
               continue;
            }
         }
      }
   }
   
   for (fcnt = 0; fcnt < INPUT_FILE_COUNT; fcnt++)
   { 
      if (inputFiles[fcnt].is_open())
      {
         inputFiles[fcnt].close();
      }
   }
   outputFile.close();
   return 0;
}
