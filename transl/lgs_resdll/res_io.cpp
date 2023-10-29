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
//   function to read write, or close the io data passed between
//   the RES program
//              arguments:
//                              operation = open_input, open_output read, write, close_input, close_output
//

#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/sworkbase.h>
#include "resadapter.h"
#include <stdio.h>
#include <errno.h>
#include <logos_libs/multithreadlib/comminterface.h>

extern CommunicationInterface * commInterface;
extern short targetIdRes;
static LgsMessage* lookupMsg = 0;

extern "C"
{
#include <logos_include_res_pt/jbctrl.h>
#include "projexts.h"
}   //extern c 
                            // following structure is used to list the commons which
                            // should be written out. We write the entire common area
                            // in block mode so all we need is the address and number
                            // of bytes. To write additional commons just add to the
                            // list in this structure.
struct fort_commons fort_commons_out[] =
{
   (char *) &swX_,          sizeof(swX_),
   (char *) &sent_wordsX_,  sizeof(sent_wordsX_),
   (char *) &ofltagX_,      sizeof(ofltagX_),
   (char *) &xpatnfX_,      sizeof(xpatnfX_),
   (char *) &hensavX_,      sizeof(hensavX_),
   (char *) &ofl2a3X_,      sizeof(ofl2a3X_),
   (char *) &hashX_,        sizeof(hashX_),
   (char *) &respasX_,      sizeof(respasX_),
   (char *) &scncelX_,      sizeof(scncelX_),
   (char *) &targX_,        sizeof(targX_),
   (char *) &cmpsmcX_,      sizeof(cmpsmcX_),
   (char *) &ptdiagX_,      sizeof(ptdiagX_),
   (char *) &source_sentX_, sizeof(source_sentX_),
};

int Res_IO_Interface(int operation);
void Res_IO_Cleanup(void);

/////////////////////////////////////////////////////////////////
//       fortran calls this c function which in turn calls
//       c++ function that does actual io
//                      return
//                                      0 no errors
//                                  99 end of file on a read
//                                      -1 error of some type.
extern "C"
{
   int res_io(int operation)
   {
       return (Res_IO_Interface(operation));
   }

   void res_io_cleanup(void)
   {
      Res_IO_Cleanup();
   }

} // end of extern c


void Res_IO_Cleanup(void)
{
   delete lookupMsg;
   lookupMsg = 0;
}

/////////////////////////////////////////////////////////////////////
//                      c++ code function to read and write the data comming to
//                      res and going out of res
//
int Res_IO_Interface(int operation)
{
   int  cnt,su_cnt;
   static ResAdapter* pResAdapt;
   SWorkBaseVector* SWVector = 0;


   try
   {
      switch (operation)
      {
      case 0: // open input file (for res this is objects)
      {
         lookupMsg = new LgsMessage;
         pResAdapt = ResAdapter::Create(lookupMsg);
         break;
      }

      case 1:                 // read input file
      {
         // clear the fortran commons first
         memset(wstateX_.wstate, '\0',  sizeof(wstateX_.wstate));
         for (cnt = 0; cnt < sizeof(fort_commons_out) / sizeof(fort_commons_out[0]); cnt++)
         {
            memset(fort_commons_out[cnt].address, '\0',  fort_commons_out[cnt].size);
         }
         memset(sent_wordsX_.source_word, ' ', sizeof(sent_wordsX_.source_word));
         memset(source_sentX_.sentbuf, ' ', sizeof(source_sentX_.sentbuf));
         memset(&cmpsmcX_, ' ', sizeof(cmpsmcX_));

         // point to the next sentence in the objects
         if (!(SWVector = pResAdapt->NextSentence()))
         {
            // return code set when end of sentences read.
            return(99);				
         }

         // unique ID for sentence
         source_sentX_.sentid = pResAdapt->sentencePosition();

         // Get the sentence markup values
         wstateX_.wstate[0][0] = 0;
         if (ResAdapter::AllUpperCase == pResAdapt->caseState())
         {
            wstateX_.wstate[0][0] = 2;
         }
         else if(ResAdapter::BeginsUpperCase == pResAdapt->caseState())
         {
            wstateX_.wstate[0][0] = 1;
         }
         // Get value for indicating if sentence is all bold.
         if (pResAdapt->isBold())
         {
            wstateX_.wstate[0][4] = 1;
         }

         // Get value for indicating if sentence is all italic.
         if (pResAdapt->isItalic())
         {
            wstateX_.wstate[0][5] = 1;
         }

         // Get value for indicating if sentence is all underlined.
         if (pResAdapt->isUnderlined())
         {
            wstateX_.wstate[0][3] = 1;
         }

         // Get value for indicating if sentence is all single quoted.
         if (pResAdapt->isSingleQuoted())
         {
            wstateX_.wstate[0][1] = 1;
         }

         // Get value for indicating if sentence is all double quoted.
         if (pResAdapt->isDoubleQuoted())
         {
            wstateX_.wstate[0][2] = 1;
         }

         // count of sworks or sentence units.
         swX_.ecount = SWVector->size();

         // quick check on reading the data by checking
         if (swX_.ecount <= 0 )
         {
            return(5); // return indicating no sentence
         }

         // loop through vector of sentence units in sentence.
         su_cnt = 0;
         for (SWorkBaseVector::iterator su_vpt = SWVector->begin(); su_vpt != SWVector->end();
              su_cnt++, su_vpt++)
         {
            // swork control table for each ssu tracking used units in each ssu
            swX_.swork[su_cnt][0] = -1;
            swX_.swork[su_cnt][1] = -1;
            swX_.swork[su_cnt][2] = -1;

            // number of SSUs (or parts of speech) this swork has.
            swX_.scont1[su_cnt] = su_vpt->SsuCount();
            sent_wordsX_.source_word_size[su_cnt]  = su_vpt->WordLength(); // length of words
            sent_wordsX_.source_word_count[su_cnt] = su_vpt->WordCount();  // number of words in dictionary match (not number of pos)

            if (su_vpt->WordLength() > 0)
            {
               int movlng;
               // get phrase as seen in sentence
               movlng = su_vpt->WordLength() > sizeof(sent_wordsX_.source_word[0])
                        ? sizeof(sent_wordsX_.source_word[0])
                        : su_vpt->WordLength();
               memmove(sent_wordsX_.source_word[su_cnt], su_vpt->Word().c_str(), movlng);

               // put words in a sentence buffer for diagnotics only!
               movlng = source_sentX_.sentlng + su_vpt->WordLength() < sizeof(source_sentX_.sentbuf)
                        ? su_vpt->WordLength()
                        : sizeof(source_sentX_.sentbuf) - source_sentX_.sentlng;
               memmove(&source_sentX_.sentbuf[source_sentX_.sentlng], su_vpt->Word().c_str(), movlng);
               source_sentX_.sentlng += movlng;
               if (source_sentX_.sentlng < sizeof(source_sentX_.sentbuf))
               {
                  source_sentX_.sentbuf[source_sentX_.sentlng] = ' ';
                  source_sentX_.sentlng++;
               }
            }
            else
            {
            }

            // hash code must come from dictionary and is based on head word
            hashX_.hashcd[su_cnt][0] = su_vpt->HashCode1(0);
            hashX_.hashcd[su_cnt][1] = su_vpt->HashCode2(0);
            hashX_.root_hashcd[su_cnt][0] = su_vpt->rootHashCode1(0);			
            hashX_.root_hashcd[su_cnt][1] = su_vpt->rootHashCode2(0);			
            // henum = hash of entire phrase in root form
            hensavX_.hennum[su_cnt][0] = su_vpt->Henum1();
            hensavX_.hennum[su_cnt][1] = su_vpt->Henum2();
            hensavX_.root_hennum[su_cnt][0] = su_vpt->rootHenum1();
            hensavX_.root_hennum[su_cnt][1] = su_vpt->rootHenum2();

            // if the first ssu then it is the bos word format code should not be changed
            if (su_cnt > 0)
            {
               // we will capture all the states of the word. Like bold quote case
               wstateX_.wstate[su_cnt][0] = su_vpt->capitalizationState();
               if (su_vpt->isSingleQuoted())
                  wstateX_.wstate[su_cnt][1] = 1;
               if (su_vpt->isDoubleQuoted())
                  wstateX_.wstate[su_cnt][2] = 1;
               if (su_vpt->isUnderlined())
                  wstateX_.wstate[su_cnt][3] = 1;
               if (su_vpt->isBold())
                  wstateX_.wstate[su_cnt][4] = 1;
               if (su_vpt->isItalic())
                  wstateX_.wstate[su_cnt][5] = 1;
               if (su_vpt->IsFound())
                  wstateX_.wstate[su_cnt][6] = 1;
            }


            // loop for number of parts of speech (ssu) for this sentence unit.
            for (int pos_cnt = 0; pos_cnt < su_vpt->SsuCount(); pos_cnt++)
            {
               // set flag indicating this word of this swork is active.
               swX_.swork[su_cnt][pos_cnt] = 1;

               ofl2a3X_.auxiliaryCode[su_cnt][pos_cnt] = su_vpt->AuxiliaryCode(pos_cnt);

               // set the WC, SET, Form, etc for each ssu or part of speech
               swX_.swork[su_cnt][(pos_cnt*4)+3] = su_vpt->WordClass(pos_cnt);
               swX_.swork[su_cnt][(pos_cnt*4)+4] = su_vpt->TypeID(pos_cnt);
               swX_.swork[su_cnt][(pos_cnt*4)+5] = su_vpt->FormCode(pos_cnt);
               swX_.swork[su_cnt][(pos_cnt*4)+6] = su_vpt->SourcePosition();    // position of unit in sentence (1 based.) this also used to be set to the target address for high freq dictionary.
               /* not sure if position should be changed like the following
               if (su_cnt == 0)
               {
                  swX_.swork[su_cnt][(pos_cnt*4)+6] = -1;     //  if first element which is BOS then set to -1
               }
               if (su_cnt == (swX_.ecount - 1))
               {
                  swX_.swork[su_cnt][(pos_cnt*4)+6] = -11;     //  last element which is eos
               }
               */
               ofltagX_.ofl4r[su_cnt][pos_cnt] = su_vpt->SupersetID(pos_cnt);
               ofltagX_.ofl1r[su_cnt][pos_cnt] = su_vpt->SubsetID(pos_cnt);

               xpatnfX_.xpatfm[su_cnt][pos_cnt] = su_vpt->Gender(pos_cnt);
               if (xpatnfX_.xpatfm[su_cnt][pos_cnt] != 0)
                  xpatnfX_.xpatfm[su_cnt][pos_cnt] = xpatnfX_.xpatfm[su_cnt][pos_cnt] + 100;

               ptdiagX_.patno[su_cnt][pos_cnt] = (short) su_vpt->PatNumber(pos_cnt);
               ptdiagX_.stemno[su_cnt][pos_cnt] = (short) su_vpt->SourceStemNumber(pos_cnt);

               // If sentence element is punctuation then set overflow 2a and 2b
               // check logic int restrat.f
               ofl2a3X_.ofl2r[su_cnt][pos_cnt] = su_vpt->Overflow2b(pos_cnt);
               ofl2a3X_.ofl3r[su_cnt][pos_cnt] = su_vpt->Overflow3b(pos_cnt);
               targX_.targ[su_cnt][pos_cnt] = su_vpt->MeaningID(pos_cnt);

               // if the word is an unfound word then
               // set the meaning id to a negative counter
               if (su_vpt->IsFound() == 0 )
               {
                  targX_.targ[su_cnt][pos_cnt] = su_vpt->SourcePosition();
               }

               memmove( cmpsmcX_.cmpcod[su_cnt][pos_cnt],
               su_vpt->CompanyCode(pos_cnt).c_str(), 3);
               sprintf(cmpsmcX_.smccod[su_cnt][pos_cnt], "%3.3d", su_vpt->AtomicCode(pos_cnt));
               sprintf(cmpsmcX_.gencod[su_cnt][pos_cnt], "%2.2d", su_vpt->GenericCode(pos_cnt));
            }  // loop through ssu in this setence unit
         }  // loop through sentence units

         delete SWVector;

         // quick check on reading the data by checking the length of the sentence.
         if (source_sentX_.sentlng <= 0)
         {
            return(5);  // return indicating lenght of words is not legal
         }
         break;
      }

      case 3: // close input file
         delete pResAdapt;
         break;

      case 10: // open output file
         break;

      case 12:                // write output file
      {
         // data read from list of commons defined above
         int msgSize = 0;
         for (cnt = 0; cnt < sizeof(fort_commons_out) / sizeof(fort_commons); cnt++)
         {
            msgSize += fort_commons_out[cnt].size;
         }
         LgsMessage outMsg(ResMsg, msgSize, 0, targetIdRes);

         char * dataPtr = outMsg.dataPtr();
         for (cnt=0; cnt < sizeof(fort_commons_out) / sizeof(fort_commons); cnt++)
         {
            memmove(dataPtr, fort_commons_out[cnt].address, fort_commons_out[cnt].size);
            dataPtr += fort_commons_out[cnt].size;
         }

         if (commInterface->sendMsg(targetIdRes, outMsg) == -1)
         {
            throw ("Write Error. ");
         }

         commInterface->sendMsg(targetIdRes, *lookupMsg);

         break;
      }

      case 13:
         break;

      default:
         break;
      }

      return (0);
   }

   catch (char *s)
   {
      cerr << "RESIO - " << s << "errno message = " << strerror(errno) << endl;
      return (-1);
   }

   catch (...)
   {
      cerr << "RESIO - " "error exception thrown. Operation= " << operation  << endl;
      return (-1);
   }

   return (0);
}

