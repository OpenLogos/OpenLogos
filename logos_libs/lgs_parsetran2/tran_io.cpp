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
//   the tran2
//		arguments:
//				operation = open_input, open_output read, write, close_input, close_output
//
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <iostream>
#include <fstream>
#include <logos_include/lgsstring.h>
#include <stdlib.h>
#include <memory>
#include <logos_libs/multithreadlib/comminterface.h>

static LgsMessage* lookupMsg = 0;
static LgsMessage* resMsg = 0;
static LgsMessage* parse1Msg = 0;
static LgsMessage* tran1Msg = 0;

static QHandle * qReadHandle;
static CommunicationInterface * commInterface;
static thread_id_t targetId;

using namespace std;

extern "C" 
{ 
///////////////////////////////////////////////////
//			fortran common areas are defined here.

#include <jbctrl.h>
#include "projexts.h"


					// list of commons to read in
static struct fort_commons fort_commons_in[] =
{
   (char *) &formsaX_,        sizeof(formsaX_),
   (char *) &prctX_,          sizeof(prctX_),
   (char *) &sworkX_,         sizeof(sworkX_),
   (char *) &hensavX_,        sizeof(hensavX_),
   (char *) &nounsX_,         sizeof(nounsX_),
   (char *) &prtscoX_,        sizeof(prtscoX_),
   (char *) &bypasX_,         sizeof(bypasX_),
   (char *) &rmorstX_,        sizeof(rmorstX_),
   (char *) &hashX_,          sizeof(hashX_),
   (char *) &sent_wordsX_,    sizeof(sent_wordsX_),
   (char *) &targ25X_,        sizeof(targ25X_),
   (char *) &vbdataX_.vbcell, sizeof(vbdataX_.vbcell),
   (char *) &hfdoaX_,         sizeof(hfdoaX_),
   (char *) &hpdopiX_,        sizeof(hpdopiX_),
   (char *) &opadriX_,        sizeof(opadriX_),
   (char *) &sconX_,          sizeof(sconX_),
   (char *) &elemctX_,        sizeof(elemctX_),
   (char *) &data29X_,        sizeof(data29X_),
   (char *) &cmpsmcX_,        sizeof(cmpsmcX_),
   (char *) &sconinX_,	      sizeof(sconinX_),
   (char *) &sworkiX_,	      sizeof(sworkiX_),
   (char *) &source_sentX_,   sizeof(source_sentX_),
};
					// list of commons to write out
struct fort_commons fort_commons_out[] =
{
   (char *) &formsaX_,        sizeof(formsaX_),
   (char *) &prctX_,          sizeof(prctX_),
   (char *) &sworkoX_,        sizeof(sworkoX_),
   (char *) &hensavX_,        sizeof(hensavX_),
   (char *) &nounsX_,         sizeof(nounsX_),
   (char *) &prtscoX_,        sizeof(prtscoX_),
   (char *) &bypasX_,         sizeof(bypasX_),
   (char *) &rmorstX_,        sizeof(rmorstX_),
   (char *) &hashX_,          sizeof(hashX_),
   (char *) &sent_wordsX_,    sizeof(sent_wordsX_),
   (char *) &targ25X_,        sizeof(targ25X_),
   (char *) &vbdataX_.vbcell, sizeof(vbdataX_.vbcell),
   (char *) &hfdoaX_,         sizeof(hfdoaX_),
   (char *) &hpdopoX_,        sizeof(hpdopoX_),
   (char *) &opadroX_,        sizeof(opadroX_),
   (char *) &sconX_,          sizeof(sconX_),
   (char *) &elemctX_,        sizeof(elemctX_),
   (char *) &clsoutX_,        sizeof(clsoutX_),
   (char *) &clsnfoX_.clprnt, (sizeof(clsnfoX_) - sizeof(clsoutX_)),
   (char *) &clsconX_,        sizeof(clsconX_),
   (char *) &data29X_,        sizeof(data29X_),
   (char *) &cmpsmcX_,	      sizeof(cmpsmcX_),
   (char *) &sconinX_,	      sizeof(sconinX_),
   (char *) &sworkiX_,	      sizeof(sworkiX_),
   (char *) &source_sentX_,   sizeof(source_sentX_),
};

#pragma pack()

///////		end of fortran common definitions //////////////
} // extern c

int Tran2_IO_Interface(int *operation);
void Tran2_IO_Cleanup(void);

/////////////////////////////////////////////////////////////////
//			c function called by fortran to read and write
//			data for tran. Calls c++ to do the work
//			return  
//					0 no errors
//				    99 end of file on a read
//					-1 error of some type.
extern "C"
{
   int tran2_io(int *operation)
   {
	   return Tran2_IO_Interface( operation);
   }

  int t2driver();
}

int tran2run(CommunicationInterface * iCommInterface,
              QHandle * iQHandle,
              short iTargetId) {
  commInterface = iCommInterface;
  qReadHandle = iQHandle;
  targetId = iTargetId;
  t2driver();
}

void Tran2_IO_Cleanup(void)
{
   delete lookupMsg;
   lookupMsg = 0;
   delete resMsg;
   resMsg = 0;
   delete parse1Msg;
   parse1Msg = 0;
   delete tran1Msg;
   tran1Msg = 0;
}

//--------------------------------------------------------
//   c++ function to actualy do the io for tran
//
int Tran2_IO_Interface(int *operation)
{
   //char *fname;
   int  cnt;
   static ifstream inp_stream;
   static ofstream out_stream;

   try
   {
      switch (*operation)
      {
      case 0:				// open input file 
         lookupMsg = new LgsMessage;
         resMsg = new LgsMessage;
         parse1Msg = new LgsMessage;
         tran1Msg = new LgsMessage;
         break;

      case 1:			// read input file
         {
            // data read from list of commons defined above
            bool lookupRead = false;
            bool resRead = false;
            bool parse1Read = false;
            bool tran1Read = false;
            LgsMessage inpMsg;
            for (; !((passesX_.passct == 2) || (passesX_.passfl == 0) || resRead) || !lookupRead || !(parse1Read || tran1Read) ; )
            {
               commInterface->receiveMsg(qReadHandle, inpMsg);

               switch (inpMsg.msgType())
               {
               case EOD:
                  commInterface->sendMsg(targetId, inpMsg);
                  return (99);
               case UntranslatedLookupMsg:
                  fprintf(_spec_fp, "\nNo Translatable Units for Tran 2");
			         fprintf(_spec_fp, "\n*EOS*\n");
                  commInterface->sendMsg(targetId, inpMsg);
                  break;
               case CLOSEFILE:
                  commInterface->sendMsg(targetId, inpMsg);
                  break;
               case ResMsg:
                  resRead = true;
                  *resMsg = inpMsg;
                  break;
               case LookupMsg:
                  lookupRead = true;
                  *lookupMsg = inpMsg;
                  break;
               case Parse1Msg:
                  parse1Read = true;
                  *parse1Msg = inpMsg;
                  break;
               case Tran1Msg:
                  tran1Read = true;
                  *tran1Msg = inpMsg;
                  break;
               }
            }
            char * dataPtr = 0;
            if (tran1Read)
            {
               dataPtr = tran1Msg->dataPtr();
            }
            else
            {
               dataPtr = parse1Msg->dataPtr();
            }

            for (cnt = 0; cnt < sizeof(fort_commons_in) / sizeof(fort_commons_in[0]); cnt++)
            {
               memmove(fort_commons_in[cnt].address, dataPtr, fort_commons_in[cnt].size);
               dataPtr += fort_commons_in[cnt].size;
            }
         }
         break;

      case 3:			// close input file
         break;

      case 10:		// open output file
         break;

      case 12:		// write output file
         {
            // Calculate msg size
            int msgSize = 0;
            for (cnt = 0; cnt < sizeof(fort_commons_out) / sizeof(fort_commons); cnt++)
            {
               msgSize += fort_commons_out[cnt].size;
            }

            // data read from list of commons defined above
            short msgType;
            if (passesX_.passct == 2)
            {
               msgType = Tran2Msg;	
            }
            else
            {
               msgType = Parse2Msg;
            }
            LgsMessage outMsg(msgType, msgSize, 0, targetId);
            char * outData = outMsg.dataPtr();
            for (cnt = 0; cnt < sizeof(fort_commons_out) / sizeof(fort_commons); cnt++)
            {
               memmove(outData, fort_commons_out[cnt].address, fort_commons_out[cnt].size);
               outData += fort_commons_out[cnt].size;
            }
            commInterface->sendMsg(targetId, outMsg);
            commInterface->sendMsg(targetId, *lookupMsg);
            if (passesX_.passfl && msgType == Parse2Msg)
            {
               commInterface->sendMsg(targetId, *resMsg);
            }
         }
         break;

      case 13:
         break;

      default:
         break;
      }

      return (0);
   }	

   catch (char *s)
   {
      cerr << "TRAN2_IO: " << s << " errno message = " << strerror(errno) << endl;
      return (-1);
   }

   catch (...)
   {
      return (-1);
   }

   return (0);
}
