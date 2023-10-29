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
// File: Tran_IO.cpp (Tran1)
// --------------------------------------------------------------------------
// Purpose: Function to read the TransL messages sent by RES and Lookup
//                   to write the message TranMsg after Tran1 process
// --------------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
//#include <memory>
#include <logos_libs/parsetrans/targetmapper.h>
#include <logos_libs/multithreadlib/comminterface.h>
#include <TermMiner/Tran1Output.h>

// --------------------------------------------------------------------------
// All types of messages that can be read/written by Tran1
static LgsMessage* lookupMsg = 0;		// message from Lookup
static LgsMessage* resMsg = 0;			// message from RES
static LgsMessage* parse4Msg = 0;		// in case Tran is two-pass mode (parse + tran)
static QHandle* qReadHandle;
static CommunicationInterface* commInterface;
static thread_id_t targetId;

// --------------------------------------------------------------------------
int Tran1_IO_Interface(int*);										// main function
void Tran1_IO_Cleanup(void);
int readAllMessagesToTran1();										// read all messages to Tran process
void writeMessageFromTran1();										// write the TranMsg after the Tran1 process
void captureTran1OutputsForTermSearch(Tran1OutputSequence&);

// --------------------------------------------------------------------------
// FORTRAN common definitions
// --------------------------------------------------------------------------
extern "C"
{ 
	#define UNCMP_BUFFER_SIZE 70000		// size of buffer for uncomressed data
	struct
   {
		char buf[UNCMP_BUFFER_SIZE];
		char *pt;
		int size;
	} uncmp1_buf;

	#include <logos_include_res_pt/jbctrl.h>
	#include "projexts.h"

	void rswork_build();

	#define MAX_ELEMENTS_RES 70


	// list of commons to read in SCON and SCNCEL commons are different name in res and tran1 and the 
	// elemnts are defined differently we will use the scncel arrray in tran1 to temporarily
	// read in the data from res, then quickly move the data to the SCON common/structure.
	struct
   {
		short scncel[MAX_ELEMENTS_RES][40];
		//char s[5600];
	} SCNCEL;

  static struct fort_commons fort_commons_in[] =
   {
      (char *) &swork1X_.swork1,        sizeof(swork1X_.swork1[0])*MAX_ELEMENTS_RES,
      (char *) &swork1X_.scont1,        sizeof(swork1X_.scont1[0])*MAX_ELEMENTS_RES,
      (char *) &swork1X_.scont2,        sizeof(swork1X_.scont2[0])*MAX_ELEMENTS_RES,
      (char *) &swork1X_.phct,          sizeof(swork1X_.phct),
      (char *) &swork1X_.scont3,        sizeof(swork1X_.scont3[0])*MAX_ELEMENTS_RES,
      (char *) &sent_wordsX_,           sizeof(sent_wordsX_),
      (char *) &ofltagX_,               sizeof(ofltagX_),
      (char *) &xpatnfX_.xpatfm,        sizeof(xpatnfX_.xpatfm[0])*MAX_ELEMENTS_RES,
      (char *) &hensavX_.henum2,        sizeof(hensavX_.henum2[0])*MAX_ELEMENTS_RES,
      (char *) &hensavX_.root_henum2,   sizeof(hensavX_.root_henum2[0])*MAX_ELEMENTS_RES,
      (char *) &ofl2a3X_.ofl2r,         sizeof(ofl2a3X_.ofl2r[0])*MAX_ELEMENTS_RES,
      (char *) &ofl2a3X_.ofl3r,         sizeof(ofl2a3X_.ofl3r[0])*MAX_ELEMENTS_RES,
      (char *) &ofl2a3X_.auxiliaryCode, sizeof(ofl2a3X_.auxiliaryCode[0])*MAX_ELEMENTS_RES,
      (char *) &hashX_.hashcd,          sizeof(hashX_.hashcd[0])*MAX_ELEMENTS_RES,
      (char *) &hashX_.root_hashcd,     sizeof(hashX_.root_hashcd[0])*MAX_ELEMENTS_RES,
      (char *) &respasX_.respas,        sizeof(respasX_.respas[0])*MAX_ELEMENTS_RES,
      (char *) &SCNCEL.scncel,          sizeof(SCNCEL.scncel[0])*MAX_ELEMENTS_RES,
      (char *) &targX_,                 sizeof(targX_),
      (char *) &cmpsmcX_,               sizeof(cmpsmcX_),
      (char *) &ptdiagX_,               sizeof(ptdiagX_),
      (char *) &source_sentX_,          sizeof(source_sentX_),
   };
	
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
      (char *) &targX_,          sizeof(targX_),
      (char *) &ofltagX_,        sizeof(ofltagX_),
      (char *) &rsworkX_,        sizeof(rsworkX_),
      (char *) &cmpsmcX_,	      sizeof(cmpsmcX_),
      (char *) &sconinX_,	      sizeof(sconinX_),
      (char *) &sworkiX_,	      sizeof(sworkiX_),
      (char *) &source_sentX_,   sizeof(source_sentX_),
   };

	#pragma pack()
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
  int tran1_io(int *operation)
  {
    return Tran1_IO_Interface(operation);
  }

  int t1driver();
}

int tran1run(CommunicationInterface * iCommInterface,
              QHandle * iQHandle,
              short iTargetId) {
  commInterface = iCommInterface;
  qReadHandle = iQHandle;
  targetId = iTargetId;
  t1driver();
}

void Tran1_IO_Cleanup(void)
{
   delete lookupMsg;
   lookupMsg = 0;
   delete resMsg;
   resMsg = 0;
   delete parse4Msg;
   parse4Msg = 0;
}

// ---------------------------------------------------------------------------------
// Interface to get and write TransL messages to/from Tran1 process
// ---------------------------------------------------------------------------------
int Tran1_IO_Interface(int *operation)
{
	//static char *compress_buffer = new char[CMP_BUFFER_SIZE];
	int returnValue = 0;
	
	try
   {
      switch (*operation)
      {
      case 0:
         lookupMsg = new LgsMessage;
         resMsg = new LgsMessage;
         parse4Msg = new LgsMessage;
         break;
      case 1:
         returnValue = readAllMessagesToTran1();
         break;
      case 12:
         writeMessageFromTran1();
         break;
      default:
         break;
      }
	}
	catch (char *s)
   {
		cerr << "TRAN1_IO: " << s << " errno message = " << strerror(errno) << endl;
		returnValue = -1;
	}
	catch (...)
   {
		returnValue = -1;
	}
	return(returnValue);
}


// --------------------------------------------------------------------------
// Read all messages as input to the Tran1 process
// --------------------------------------------------------------------------
int readAllMessagesToTran1()
{
  int cnt;
  int ct1;
  bool lookupRead = false;
  bool resRead = false;
  bool parse4Read = false;
  LgsMessage inpMsg;

  // -< retrieve all messages for Tran1
  for (; (!resRead || !lookupRead) ;)
    {
      commInterface->receiveMsg(qReadHandle, inpMsg);
      switch (inpMsg.msgType())
        {
        case EOD:
          commInterface->sendMsg(targetId, inpMsg);
          return (99);
        case UntranslatedLookupMsg:
          fprintf(_spec_fp, "\nNo Translatable Units for Tran 1");
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
        case Parse4Msg:
          parse4Read = true;
          *parse4Msg = inpMsg;
          break;
        }
    }

  // -< read in message from RES
  // data read from list of commons defined above
  char *dataPtr = resMsg->dataPtr();
  for (cnt = 0; cnt < sizeof(fort_commons_in) / sizeof(fort_commons_in[0]); cnt++)
    {
      memmove(fort_commons_in[cnt].address, dataPtr, fort_commons_in[cnt].size);
      dataPtr += fort_commons_in[cnt].size;
    }
  // adjust for some realignment of commons. yuck!
  // scncel common in res was written. we have read this in so we must now move it to the scon 
  // common used by tran the scncel common here is only temp in the io routine.
  // setup the new scon array with SCNCEL and lvl from res
  memset(&sconX_, '\0',  sizeof(sconX_));			// first clear common			
  for (ct1 = 0; ct1 < swork1X_.phct; ct1++)
    {
      sconX_.scono[ct1][54-SCONX1-1] = sent_wordsX_.source_word_count[ct1];
      memcpy(&sconX_.scono[ct1][61-SCONX1-1], &SCNCEL.scncel[ct1], sizeof(SCNCEL.scncel[ct1]));
    }
  // if running 2 pass and no in the tran phase, read in the parse information from tran4
  if (passesX_.passfl == 1 && passesX_.passct == 2)
    {
      if (!parse4Read)
        commInterface->receiveMsg(qReadHandle, *parse4Msg);
      char * parse4Data = parse4Msg->dataPtr();
      memmove(uncmp1_buf.buf, parse4Data, parse4Msg->msgLength());
      // call to unpack block into separate structures is done by the getprs routine.
      // It will look at uncmp1_buf
    }

  return(0);
}


// --------------------------------------------------------------------------
// Write the TranMsg after the Tran1 process for the following processes
// (either Tran2 or TermSearch)
// --------------------------------------------------------------------------
void writeMessageFromTran1()
{
	int cnt;

	// if writing out tran (not parse)
	if (passesX_.passfl != 1 || passesX_.passct == 1)
   {
		rswork_build();			// call to setup the sw29 data rswork common
	}

	// setup compression buffer variables
	uncmp1_buf.size=0;
	uncmp1_buf.pt=uncmp1_buf.buf;

	// if not term search, then writes out the commons.
	// else term search only writes out the opadr and NPID data.
	if (jcaddX_.wrdsrc != 1 || (passesX_.passfl && passesX_.passct == 1))
   {
		//  +-------------------------------------------------------------+
		// -| write a message for next process (Tran2) in translation job |
		//  +-------------------------------------------------------------+
		// -< compute the total size of the message to send (TranMsg)
		int msgSize = 0;

		// data read from list of commons defined above
		for (cnt = 0; cnt < sizeof(fort_commons_out) / sizeof(fort_commons); cnt++)
      {
			msgSize += fort_commons_out[cnt].size;
		}

		// -< define the type of message to send
		short msgType;
		if (passesX_.passct == 2)
         msgType = Tran1Msg;	
		else
         msgType = Parse1Msg;
		LgsMessage outMsg(msgType, msgSize, 0, targetId);

		// -< write the message
		char* outData = outMsg.dataPtr();

		// write into message the info pertaining to translations
		for (cnt=0; cnt<sizeof(fort_commons_out)/sizeof(fort_commons); cnt++)
      {
			memmove(outData, fort_commons_out[cnt].address, fort_commons_out[cnt].size);
			outData += fort_commons_out[cnt].size;
		}
		
		// -< send all the messages for the following process (Tran2)
		commInterface->sendMsg(targetId, outMsg);
		commInterface->sendMsg(targetId, *lookupMsg);
		if (passesX_.passfl && msgType == Parse1Msg)
         commInterface->sendMsg(targetId, *resMsg);
	}
	else
   {
		//  +---------------------------------------------------------------+
		// -| write a message for a term search job (target info + NP info) |
		//  +---------------------------------------------------------------+
		// -< capture information about the phrases generated by Tran1 (to be used by term search)
		Tran1OutputSequence outputSequence;
		captureTran1OutputsForTermSearch(outputSequence);
		//outputSequence.display();		// debug

		// -< get the target information for this sentence (as a vector of targetmapper objects)
		TargetMapperTable sentenceInfo(opadroX_.opo,		 // number of target semantico-syntactic units
                                     opadroX_.opadro,	 // OPADR array
                                     opadroX_.sconpo,	 // array of SCON positions
                                     prctX_.js,			 // array of primary semantico-syntactic units
                                     sconX_.scon, 		 // SCON table
                                     sconX_.scolnk,	 // overflow SCON position
                                     sconX_.scono,		 // overflow SCON array (used to store SCONs in case they don't fit in SCON table)
                                     cmpsmcX_.cmpcod); // array of company codes for SSUs. there are 3 for each, of which only one is to be used.

		// -< compute the total size of the message to send (TranMsg)
		int msgSize = sizeof(int);									// bytes required for id info
		msgSize += sizeof(short int);									// offset (hold the total size required for target info)
		short int sizeTargetInfo = sentenceInfo.streamOutSize();		// bytes required for target info
		msgSize += sizeTargetInfo;
		msgSize += outputSequence.streamOutSize();

		// -< define the type of message to send
		LgsMessage outMsg(Tran4Msg, msgSize, 0, targetId);

		// -< write the message
		char *outData = outMsg.dataPtr();									// message to send
		memcpy(outData, (char*)&source_sentX_.sentid, sizeof(int));		// write sentence sentid info
		outData += sizeof(int);											// move pointer in stream to write next piece of info
		memcpy(outData, (char*)&sizeTargetInfo, sizeof(short int));		// write size of targetMappertable object
		outData += sizeof(short int);										// move pointer in stream to write next piece of info
		streamOutTargetMapperTable(outData,sentenceInfo);				// add target info to message
		outData += sentenceInfo.streamOutSize();							// move pointer in stream to write next piece of info
		outputSequence.streamOut(outData);

		// -< send all the messages for the following process (term search)
		commInterface->sendMsg(targetId, outMsg);				// send TranMsg
		commInterface->sendMsg(targetId, *lookupMsg);			// forward message from Lookup
	}
}


// --------------------------------------------------------------------------
// Capture all SWORKs created by Tran1 that will be used by term search
// --------------------------------------------------------------------------
void captureTran1OutputsForTermSearch(Tran1OutputSequence& sequence)
{
	// constant that identifies a term as being a switch68
	LgsString switch68Identifier = "***SWITCH68***";

	int totalSworkElements = tran1Units_.totalSworkElements;

	// get the number of the source unit starting the sentence (sentence unit number for BOS)
	// and the one that finishes the sentence (sentence unit number for EOS).
	// Get this info from the last SWORK formed by Tran1
	int sourceUnitNumberFirstElement = 1;
	int sourceUnitNumberLastElement = sworkoX_.sworko[totalSworkElements-One][3];

	// save the sentence elements
	for (int i = sourceUnitNumberFirstElement; i <= sourceUnitNumberLastElement; i++)
	{
		LgsString nextWord = sent_wordsX_.source_word[i-One];
		nextWord = nextWord.substr(0,nextWord.find_first_of(" "));
		sequence.appendNextSourceWord(nextWord);
	}

	// get info on each phrase formed after Tran1 (each is an swork)
	for (int unit = 1; unit <= totalSworkElements; unit++)		// for each Swork element
	{
		// capture head word information from the C structures
		LgsString headWord = sent_wordsX_.source_word[sconX_.scolnk[sworkoX_.sworko[unit-One][4-One]-One]-One];
		headWord = headWord.substr(0,headWord.find_first_of(" "));
		if (headWord == "*")			// this is a switch 68 added to the original sequence of source words
		{
			headWord = switch68Identifier;
		}
		int headWordPOS = tran1Units_.units[unit-1].headWordPOS;			// word class
		int headWordSSSet = tran1Units_.units[unit-1].headWordSSSet;		// superset or set or subset
		int headWordForm = tran1Units_.units[unit-1].headWordForm;			// form code
		int headWordSourceUnitNumber = tran1Units_.units[unit-1].headWordSconPointer;
		int scon1 = sconX_.scon[headWordSourceUnitNumber-1][0];

		// create a new Tran1 unit object
		Tran1Output aTran1Output(headWord,headWordPOS,headWordSSSet,headWordForm,headWordSourceUnitNumber,scon1);

		// capture all source units grouped under that head word (phrase).
		// the word order corresponds to target word order
        int element = 0;
		// for each scon pointer, write it in the message
		for (int sconptr = sworkoX_.phrbgo[unit-One]; sconptr <= sworkoX_.phrndo[unit - One]; sconptr++)
		{
			aTran1Output.appendSconPointerElement(tran1Units_.units[unit - 1].elements[element]);
			aTran1Output.appendOpadrElement(tran1Units_.units[unit - 1].opadro[element]);
			element++;
		}
		//aTran1Output.display();		// debug

		// append to the sequence of Tran1 phrases
		sequence.append(aTran1Output);
	}

	// account in the sequence of source words for any switch68 that have been added
	sequence.accountForSwitch68(switch68Identifier);
}
