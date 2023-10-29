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
// Tran1Unit.cpp: implementation of the Tran1Unit class.
//
//////////////////////////////////////////////////////////////////////

#include <logos_include/logoscommon.h>
#include <TermMiner/Tran1Unit.h>
#include <logos_libs/multithreadlib/comminterface.h>

extern LgsMessage* tranMsg;

// --------------------------------------------------------------------------
// Construct a default Tran1Unit object
// --------------------------------------------------------------------------
Tran1Unit::Tran1Unit() :
tran1TargetArraysSize_(0)
{
	resetArrayIndex();
}

// --------------------------------------------------------------------------
// Set this unit from the given Tran1 message
// --------------------------------------------------------------------------
Tran1Unit::Tran1Unit(char*& dataIn)
{
	// get headWordSurfaceForm_
	int totalCharacters = 0;
	memcpy((char*)&totalCharacters, dataIn, sizeof(totalCharacters));
	dataIn += sizeof(totalCharacters);
	headWordSurfaceForm_ = "";
	for (int i=0; i<totalCharacters; i++)
	{
		char c = ' ';
		memcpy((char*)&c, dataIn, sizeof(c));
		dataIn += sizeof(c);
		headWordSurfaceForm_ += c;
	}

	// get headWordPOS_
	memcpy((char*)&headWordPOS_, dataIn, sizeof(headWordPOS_));
	dataIn += sizeof(headWordPOS_);

	// get headWordSSSet_
	memcpy((char*)&headWordSSSet_, dataIn, sizeof(headWordSSSet_));
	dataIn += sizeof(headWordSSSet_);

	// get headWordForm_
	memcpy((char*)&headWordForm_, dataIn, sizeof(headWordForm_));
	dataIn += sizeof(headWordForm_);

	// get headWordSourceUnitNumber_
	memcpy((char*)&headWordSourceUnitNumber_, dataIn, sizeof(headWordSourceUnitNumber_));
	dataIn += sizeof(headWordSourceUnitNumber_);

	// get the total number of SCONPO elements in the sequence in target order
	int totalTargets = 0;
	memcpy((char*)&totalTargets, dataIn, sizeof(totalTargets));
	dataIn += sizeof(totalTargets);

	// get the sequence of SCONPO elements in target order
	LgsList(int) elementsInSourceSequenceOrderTmp;
	elementsInTargetSequenceOrder_.clear();
        int e;
	for (e=0; e<totalTargets; e++)
	{
		int targetElement = 0;
		memcpy((char*)&targetElement, dataIn, sizeof(targetElement));
		dataIn += sizeof(targetElement);
		elementsInTargetSequenceOrder_.push_back(targetElement);
		elementsInSourceSequenceOrderTmp.push_back(targetElement);
	}

	// get the total number of OPADRO elements in the sequence in target order
	int totalOpadroElements = 0;
	memcpy((char*)&totalOpadroElements, dataIn, sizeof(totalOpadroElements));
	dataIn += sizeof(totalOpadroElements);

	// get the sequence of OPADRO elements in target order
	opadro_.clear();
	for (e=0; e<totalOpadroElements; e++)
	{
		int opadr = 0;
		memcpy((char*)&opadr, dataIn, sizeof(opadr));
		dataIn += sizeof(opadr);
		opadro_.push_back(opadr);
	}

	constructTargetArraysTable(totalOpadroElements,totalTargets);

	// reorder the list of scon pointers in ascending order (corresponding to the source word order)
	elementsInSourceSequenceOrderTmp.sort();
	elementsInSourceSequenceOrder_.clear();
	// remove any inserted elements that do not concern the source sequence
	for (LgsList(int)::iterator src=elementsInSourceSequenceOrderTmp.begin(); src!=elementsInSourceSequenceOrderTmp.end(); src++)
	{
		if (*src>0)
		{
			elementsInSourceSequenceOrder_.push_back(*src);
		}
	}

	// get the value of scon1
	memcpy((char*)&scon1_, dataIn, sizeof(scon1_));
	dataIn += sizeof(scon1_);
}

// --------------------------------------------------------------------------
// 
// --------------------------------------------------------------------------
void Tran1Unit::constructTargetArraysTable(int totalOpadroElements, int totalSconpoElements)
{
	if (totalOpadroElements == totalSconpoElements && totalSconpoElements < TARGET_ARRAYS_MAX_SIZE)
	{
		tran1TargetArraysSize_ = 0;
		LgsList(int)::iterator opadro = opadro_.begin();
		LgsList(int)::iterator sconpo = elementsInTargetSequenceOrder_.begin();

		while (opadro!=opadro_.end() && sconpo!=elementsInTargetSequenceOrder_.end())
		{
			tran1TargetArrays_[tran1TargetArraysSize_][OPADRO] = *opadro;
			tran1TargetArrays_[tran1TargetArraysSize_][SCONPO] = *sconpo;
			opadro++;
			sconpo++;
			tran1TargetArraysSize_++;
		}
	}
	else
	{
		tran1TargetArraysSize_ = 0;
	}
}

// --------------------------------------------------------------------------
// Destructor
// --------------------------------------------------------------------------
Tran1Unit::~Tran1Unit()
{
}

// --------------------------------------------------------------------------
void Tran1Unit::report(ostream& out)
{
	// head word information (from SWORKO structure)
	out << "\thead word: " << headWordSurfaceForm_;
	out << " [POS=" << headWordPOS_ << "]";
	out << " [SSSet=" << headWordSSSet_ << "]";
	out << " [Form=" << headWordForm_ << "]";
	out << " [SSU#=" << headWordSourceUnitNumber_ << "]";
	out << endl;

	// Elements (scon pointers) of this unit (in target sequence order)
	out << "\t" << numberElements() << " elements (scon pointers) in target sequence order:";
	LgsList(int)::iterator e;
	for (e=elementsInTargetSequenceOrder_.begin(); e!=elementsInTargetSequenceOrder_.end(); e++)
	{
		out << " " << (*e);
	}
	out << endl;

	out << "\t" << numberElements() << " elements (scon pointers) in source sequence order:";
	for (e=elementsInSourceSequenceOrder_.begin(); e!=elementsInSourceSequenceOrder_.end(); e++)
	{
		out << " " << (*e);
	}
	out << endl;

	out << "\ttarget arrays" << endl;
	out << "\tOPADRO";
	for (int opadro=0; opadro<tran1TargetArraysSize_; opadro++)
	{
		out << "\t" << tran1TargetArrays_[opadro][OPADRO];
	}
	out << endl;
	out << "\tSCONPO";
	for (int sconpo=0; sconpo<tran1TargetArraysSize_; sconpo++)
	{
		out << "\t" << tran1TargetArrays_[sconpo][SCONPO];
	}
	out << endl;
}

// --------------------------------------------------------------------------
// Construct a sequence of Tran1Unit by reading the message sent by Tran1.
// --------------------------------------------------------------------------
Tran1UnitSequence::Tran1UnitSequence() :
numberSW68_(0)
{
	readInTran1Message();
	countNumberOfSwitch68();
}

// --------------------------------------------------------------------------
// Destructor
// --------------------------------------------------------------------------
Tran1UnitSequence::~Tran1UnitSequence()
{
}

// --------------------------------------------------------------------------
// Set the features of this object from the information contained in the given 
// message (sequence of characters)
// --------------------------------------------------------------------------
void Tran1UnitSequence::readInTran1Message()
{
	char* dataIn = tranMsg->dataPtr();		// pointer to the message sent by Tran1

	// move pointer to beginning of phrases info in stream
	dataIn += sizeof(int);														// move pointer to read size required for target info (ie, skip sentence id info)
	short int sizeTargetInfo = 0;												// get size required for target info
	memcpy((char*)&sizeTargetInfo,dataIn,sizeof(sizeTargetInfo));
	dataIn += sizeof(sizeTargetInfo);										// skip this offset
	dataIn += sizeTargetInfo;													// skip target info (ie, move pointer to start of NP info)

	// get the total number of elements in the original source sentence
	int totalElements = 0;
	memcpy((char*)&totalElements, dataIn, sizeof(totalElements));
	dataIn+=sizeof(totalElements);

	// get each of these elements (words)
	for (int j=0; j<totalElements; j++)
	{
		// get the number of characters in the next word
		int totalCharacters = 0;
		memcpy((char*)&totalCharacters, dataIn, sizeof(totalCharacters));
		dataIn+=sizeof(totalCharacters);
		// get the next word
		LgsString word = "";
		for (int i=0; i<totalCharacters; i++)
		{
			char c = ' ';
			memcpy((char*)&c, dataIn, sizeof(c));
			dataIn+=sizeof(c);
			word += c;
		}
		// add it to the sequence of elements
		sourceWords_.push_back(word);
	}

	// get the total number of units
	int totalUnits = 0;
	memcpy((char*)&totalUnits, dataIn, sizeof(totalUnits));
	dataIn+=sizeof(totalUnits);

	// get each unit
	for (int p=0; p<totalUnits; p++)
	{
		Tran1Unit aUnit(dataIn);
		append(aUnit);
	}
}

// --------------------------------------------------------------------------
void Tran1UnitSequence::report(ostream& out)
{
	/*
	out << "* Tran1 unit sequence received by Tran1:" << endl;
	
	out << "Original source sentence elements" << endl;
	int n=1;
	for (LgsStringIterator s=sourceWords_.begin(); s!=sourceWords_.end(); s++)
	{
		out << n++ << "\t" << (*s) << endl;
	}
	*/

	out << endl << "* Tran1 units at the end of Tran1:" << endl;
	int cnt=1;
	for (Tran1UnitIterator aUnit=begin(); aUnit!=end(); aUnit++)
	{
		out << "(" << cnt++ << ")" << endl;
		aUnit->report(out);
	}
}

// --------------------------------------------------------------------------
bool Tran1Unit::isValidForTermSearch()
{
	return     headWordSurfaceForm_ != "***SWITCH68***"	// do not consider inserted units
			&& headWordSurfaceForm_ != "BOS"				// discard sentence markers
			&& headWordSurfaceForm_ != "EOS";
}

// --------------------------------------------------------------------------
// Count number of SWITCH68 in the sequence of units.
// --------------------------------------------------------------------------
void Tran1UnitSequence::countNumberOfSwitch68()
{
	numberSW68_ = 0;

	for (Tran1UnitIterator unit=begin(); unit!=end(); unit++)
	{
		if (unit->headWordSurfaceForm() == "***SWITCH68***")
		{
			numberSW68_++;
		}
	}
}

// --------------------------------------------------------------------------
// return the number of instances of this scon value in the sequence of scons.
// --------------------------------------------------------------------------
int Tran1Unit::numberInstancesOfSconValue(int sconValue)
{
	int n = 0;

	for (int sconpo=0; sconpo<tran1TargetArraysSize_; sconpo++)
	{
		if (tran1TargetArrays_[sconpo][SCONPO] == sconValue)
		{
			n++;
		}
	}

	return n;
}

