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
// --------------------------------------------------------------------------
// File: Tran_IO.cpp (Tran4)
// --------------------------------------------------------------------------
// Purpose: Function to read the TransL messages sent by RES and Lookup
//                   to write the message TranMsg after Tran1 process
// --------------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <memory>
#include <logos_libs/parsetrans/targetmapper.h>
#include <logos_libs/multithreadlib/comminterface.h>

// --------------------------------------------------------------------------
// All types of messages that can be read/written by Tran4
static LgsMessage* lookupMsg = 0;		// message from Lookup
static LgsMessage* resMsg = 0;			// message from RES
static LgsMessage* parse3Msg = 0;		// in case Tran is two-pass mode (parse + tran)
static LgsMessage* tran3Msg = 0;
static QHandle* qReadHandle;
static CommunicationInterface* commInterface;
static thread_id_t targetId;

// --------------------------------------------------------------------------
int Tran4_IO_Interface(int*);		// main function
void Tran4_IO_Cleanup(void);
int readAllMessagesToTran4();		// read all messages to Tran4 process
void writeMessageFromTran4();		// write the TranMsg after the Tran4 process

// --------------------------------------------------------------------------
// FORTRAN common definitions
// --------------------------------------------------------------------------
extern "C"
{ 
   #include <jbctrl.h>
   #include "projexts.h"

   int wrt2tr();

   #define UNCMP_BUFFER_SIZE 70000		// size of buffer for uncomressed data
   struct
   {
      char buf[UNCMP_BUFFER_SIZE];
      char *pt;
      int size;
   } uncmp_buf;

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
      (char *) &targ25X_,			sizeof(targ25X_),
      (char *) &vbdataX_.vbcell, sizeof(vbdataX_.vbcell),
      (char *) &hfdoaX_,         sizeof(hfdoaX_),
      (char *) &hpdopiX_,        sizeof(hpdopiX_),
      (char *) &opadriX_,        sizeof(opadriX_),
      (char *) &sconX_,          sizeof(sconX_),
      (char *) &elemctX_,        sizeof(elemctX_),
      (char *) &clsnfoX_,        sizeof(clsnfoX_),
      (char *) &clsconX_,        sizeof(clsconX_),
      (char *) &data29X_,        sizeof(data29X_),
      (char *) &sconinX_,        sizeof(sconinX_),
      (char *) &sworkiX_,	      sizeof(sworkiX_),
      (char *) &cmpsmcX_,	      sizeof(cmpsmcX_),					
      (char *) &source_sentX_,   sizeof(source_sentX_),
   };
}

// ---------------------------------------------------------------------------------
// C function called by FORTRAN to read and write data for tran.
// Calls the following C++ function to do the actual work.
// Returns:	0 no errors
//			99 end of file on a read
//			-1 error of some type
// ---------------------------------------------------------------------------------
extern "C"
{
  int tran4_io(int* operation)
  {
    return Tran4_IO_Interface(operation);
  }

  int t4driver();
}

int tran4run(CommunicationInterface * iCommInterface,
              QHandle * iQHandle,
              short iTargetId) {
  commInterface = iCommInterface;
  qReadHandle = iQHandle;
  targetId = iTargetId;
  return t4driver();
}

void Tran4_IO_Cleanup(void)
{
   delete lookupMsg;
   lookupMsg = 0;
   delete resMsg;
   resMsg = 0;
   delete parse3Msg;
   parse3Msg = 0;
   delete tran3Msg;
   tran3Msg = 0;
}

// ---------------------------------------------------------------------------------
// Interface to get and write TransL messages to/from Tran4 process
// ---------------------------------------------------------------------------------
int Tran4_IO_Interface(int* operation)
{
	int returnValue = 0;
	
	try
   {
      switch (*operation)
      {
      case 0:
         lookupMsg = new LgsMessage;
         resMsg = new LgsMessage;
         parse3Msg = new LgsMessage;
         tran3Msg = new LgsMessage;
         break;
      case 1:
         returnValue = readAllMessagesToTran4();
         break;
      case 12:
         writeMessageFromTran4();
         break;
      default:
         break;
      }
	}
	catch (char *s)
   {
		cerr << "TRAN_IO: " << s << " errno message = " << strerror(errno) << endl;
		returnValue = -1;
	}
	catch (...)
   {
		returnValue = -1;
	}
	return(returnValue);
}


// --------------------------------------------------------------------------
// Read all messages as input to the Tran4 process
// --------------------------------------------------------------------------
int readAllMessagesToTran4()
{
	int cnt;
	bool lookupRead=false;
	bool resRead=false;
	bool parse3Read=false;
	bool tran3Read = false;
	LgsMessage inpMsg;

	// -< retrieve all messages for Tran4
	for (; !((passesX_.passct == 2) || (passesX_.passfl == 0) || resRead) || !lookupRead || !(parse3Read || tran3Read) ; )
   {
		commInterface->receiveMsg(qReadHandle, inpMsg);
		switch (inpMsg.msgType())
      {
		case EOD:
			commInterface->sendMsg(targetId, inpMsg);
			return (99);
		case UntranslatedLookupMsg:
         fprintf(_spec_fp, "\nNo Translatable Units for Tran 4");
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
		case Parse3Msg:
			parse3Read = true;
			*parse3Msg = inpMsg;
			break;
		case Tran3Msg:
			tran3Read = true;
			*tran3Msg = inpMsg;
			break;
		}
	}

	// -< read in message from Tran3
	char* dataPtr=0;
	if (tran3Read)
      dataPtr = tran3Msg->dataPtr();
	else
      dataPtr = parse3Msg->dataPtr();
	for (cnt = 0; cnt < sizeof(fort_commons_in) / sizeof(fort_commons_in[0]); cnt++)
   {
		memmove(fort_commons_in[cnt].address, dataPtr, fort_commons_in[cnt].size);
		dataPtr += fort_commons_in[cnt].size;
	}

	return(0);
}


// --------------------------------------------------------------------------
// Write the TranMsg after the Tran4 process for the following processes
// (either generate)
// --------------------------------------------------------------------------
void writeMessageFromTran4()
{
	if (passesX_.passfl!=1 || passesX_.passct==2)
   {
		//  +----------------------------------------------------+
		// -| write a message for generate process (target info) |
		//  +----------------------------------------------------+
		// -< get the target information for this sentence (as a vector of targetmapper objects)
		TargetMapperTable targetMapperTable(opadroX_.opo,		// number of target semantico-syntactic units
                                          opadroX_.opadro,	// OPADR array
                                          opadroX_.sconpo,	// array of SCON positions
                                          prctX_.js,			// array of primary semantico-syntactic units
                                          sconX_.scon,		// SCON table
                                          sconX_.scolnk,		// overflow SCON position
                                          sconX_.scono,     // overflow SCON array (used to store SCONs in case they don't fit in SCON table)
                                          cmpsmcX_.cmpcod);	// array of company codes for SSUs. there are 3 for each, of which only one is to be used.

		// -< compute the total size of the message to send (TranMsg)
		int msgSize = sizeof(int);									         // bytes required for sentid info
		// offset (hold the total size required for target info) - not used here but required in the 
		// message for consistency (the same method reads the message either from Tran1 for term search 
		// or Tran4)
		msgSize += sizeof(short int);									      // bytes required to hold size of target info
		short int sizeTargetInfo=targetMapperTable.streamOutSize();	// bytes required for target info
		msgSize += sizeTargetInfo;

		// -< define the type of message to send
		LgsMessage outMsg(Tran4Msg, msgSize, 0, targetId);

		// -< write the message
		char *outData=outMsg.dataPtr();										// message to send
		memcpy(outData, (char*)&source_sentX_.sentid, sizeof(int));	// write sentence id info
		outData += sizeof(int);												   // move pointer in stream to write next piece of info
		memcpy(outData, (char*)&sizeTargetInfo, sizeof(short int));	// write size of targetMappertable object
		outData += sizeof(short int);											// move pointer in stream to write next piece of info
		streamOutTargetMapperTable(outData,targetMapperTable);		// add target info to message
		outData += targetMapperTable.streamOutSize();					// move pointer in stream to write next piece of info

		// -< send all the messages for the following process
		commInterface->sendMsg(targetId, outMsg);				// send TranMsg for generate process
		commInterface->sendMsg(targetId, *lookupMsg);			// forward message from Lookup
	}
	else
   {
		//  +-------------------------------------------------------+
		// -| write a message for end of parse for tran1 reading... |
		//  +-------------------------------------------------------+
		// -< compute the total size of the message to send (TranMsg)
		int msgSize = wrt2tr();			// move data into one buffer

		// -< define the type of message to send
		LgsMessage outMsg(Parse4Msg, msgSize, 0, targetId);

		// -< write the message
		char * outData = outMsg.dataPtr();

		// -< write the message
		memmove(outData, uncmp_buf.buf, msgSize);

		// -< send all the messages for the following process
		commInterface->sendMsg(targetId, outMsg);				// send TranMsg
		commInterface->sendMsg(targetId, *lookupMsg);		// forward message from Lookup
		commInterface->sendMsg(targetId, *resMsg);			// forward message from RES
	}
}
