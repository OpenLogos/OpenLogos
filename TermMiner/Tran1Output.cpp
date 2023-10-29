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
// Tran1Output.cpp: implementation of the Tran1Output class.
//
//////////////////////////////////////////////////////////////////////

#include <logos_include/logoscommon.h>
#include <TermMiner/Tran1Output.h>

// --------------------------------------------------------------------------
// Default construtor - set with default values
// This constructor is required to use STL
// --------------------------------------------------------------------------
Tran1Output::Tran1Output()
{
	headWordSurfaceForm_ = "";
	headWordPOS_ = 0;
	headWordSSSet_ = 0;
	headWordForm_ = 0;
	headWordSourceUnitNumber_ = 0;
	elementsInTargetSequenceOrder_.clear();
	scon1_ = 0;

}


// --------------------------------------------------------------------------
// Constructor for Tran1_io.cpp to capture result from Tran1 process
// --------------------------------------------------------------------------
Tran1Output::Tran1Output(LgsString aHeadWordSurfaceForm,
								 int aHeadWordPOS,
								 int aHeadWordSSSet,
								 int aHeadWordForm,
								 int aHeadWordSourceUnitNumber,
								 int aScon1)
{
	headWordSurfaceForm_ = aHeadWordSurfaceForm;
	headWordPOS_ = aHeadWordPOS;
	headWordSSSet_ = aHeadWordSSSet;
	headWordForm_ = aHeadWordForm;
	headWordSourceUnitNumber_ = aHeadWordSourceUnitNumber;
	elementsInTargetSequenceOrder_.clear();
	scon1_ = aScon1;
}


// --------------------------------------------------------------------------
// Destructor
// --------------------------------------------------------------------------
Tran1Output::~Tran1Output()
{
}

// --------------------------------------------------------------------------
int Tran1Output::numberElements()
{
	int n = 0;
	if (!elementsInTargetSequenceOrder_.empty())
	{
		n = elementsInTargetSequenceOrder_.size();
	}
	return n;
}

// --------------------------------------------------------------------------
Tran1OutputSequence::Tran1OutputSequence()
{
}

// --------------------------------------------------------------------------
// Destructor
// --------------------------------------------------------------------------
Tran1OutputSequence::~Tran1OutputSequence()
{
}

// --------------------------------------------------------------------------
// Display content of this object (debug)
// --------------------------------------------------------------------------
void Tran1OutputSequence::display()
{
	cout << endl << "*** Output Units constructed by Tran1 ***" << endl;
	
	cout << "Source sentence" << endl;
	int n=1;
	for (LgsStringIterator s=sourceWords_.begin(); s!=sourceWords_.end(); s++)
	{
		cout << "\tElement " << n++ << "\t" << (*s) << endl;
	}

	cout << "Output Units:" << endl;
	int cnt = 1;
	Tran1OutputIterator aUnit;
	for (aUnit=begin(); aUnit!=end(); aUnit++)
	{
		cout << "(" << cnt++ << ")\t";
		aUnit->display();
	}
}
// --------------------------------------------------------------------------
void Tran1Output::display()
{
	// info about the head word
	cout << "TRAN1 output unit:" << endl;
	cout << "\thead word: " << headWordSurfaceForm_;
	cout << " [POS=" << headWordPOS_ << "]";
	cout << " [SSSet=" << headWordSSSet_ << "]";
	cout << " [Form=" << headWordForm_ << "]";
	cout << " [SSU#=" << headWordSourceUnitNumber_ << "]";
	cout << endl;

	// info about the elements of this unit
	cout << "\tunit elements (in target sequence order):";
	LgsList(int)::iterator e;
	for (e=elementsInTargetSequenceOrder_.begin(); e!=elementsInTargetSequenceOrder_.end(); e++)
	{
		cout << " " << (*e);
	}
	cout << endl;
}


// --------------------------------------------------------------------------
// Return the number of bytes necessary to store the selected content of this 
// object in memory (used for message passing strategy)
// --------------------------------------------------------------------------
int Tran1OutputSequence::streamOutSize()
{
	int numberBytes = 0;

	// bytes required for the number of original source sentence elements (words)
	numberBytes += sizeof(int);

	// bytes required for all the source words
	LgsStringIterator s;
	for (s=sourceWords_.begin(); s!=sourceWords_.end(); s++)
	{
		numberBytes += (*s).length() * sizeof(char);
	}

	// bytes required for all the total number of characters for each element
	numberBytes += sourceWords_.size() * sizeof(int);

	// bytes required for the number of phrases in this object
	numberBytes += sizeof(int);

	// bytes required for each element
	Tran1OutputIterator aUnit;
	for (aUnit=begin(); aUnit!=end(); aUnit++)
	{
		numberBytes += aUnit->streamOutSize();
	}

	return numberBytes;
}
// --------------------------------------------------------------------------
int Tran1Output::streamOutSize()
{
	int numberBytes = 0;

	// bytes required for the total number of characters in headWordSurfaceForm_
	numberBytes += sizeof(int);

	// bytes required for each character of this word
	numberBytes += headWordSurfaceForm_.length() * sizeof(char);
	
	// bytes required for headWordPOS_
	numberBytes += sizeof(int);
	
	// bytes required for headWordSSSet_
	numberBytes += sizeof(int);
	
	// bytes required for headWordForm_
	numberBytes += sizeof(int);
	
	// bytes required for headWordSourceUnitNumber_
	numberBytes += sizeof(int);
	
	// bytes required for the number of SCONPO elements in this phrase (target sequence)
	numberBytes += sizeof(int);
	
	// all these SCONPO elements
	numberBytes += elementsInTargetSequenceOrder_.size() * sizeof(int);

	// bytes required for the number of OPADRO elements in this phrase (target sequence)
	numberBytes += sizeof(int);
	
	// all these OPADRO elements
	numberBytes += opadro_.size() * sizeof(int);

	// bytes required for the value of scon1
	numberBytes += sizeof(int);
	
	return numberBytes;
}


// --------------------------------------------------------------------------
// Return the selected content of this object as a sequence of characters
// (used for message passing strategy)
// --------------------------------------------------------------------------
void Tran1OutputSequence::streamOut(char*& outData)
{
	// save the number of elements (words) in the original source sentence
	int totalElements = sourceWords_.size();
	memcpy(outData, (char*)&totalElements, sizeof(totalElements));
	outData+=sizeof(totalElements);

	// save each source sentence element
	for (LgsStringIterator s=sourceWords_.begin(); s!=sourceWords_.end(); s++)
	{
		// save the total number of characters in this word
		int totalCharacters = (*s).length();
		memcpy(outData, (char*)&totalCharacters, sizeof(totalCharacters));
		outData+=sizeof(totalCharacters);
		// save each character
		for (int i=0; i<totalCharacters; i++)
		{
			char c = (*s)[i];
			memcpy(outData, (char*)&c, sizeof(c));
			outData+=sizeof(c);
		}
	}

	// save the number of phrases in this object
	int totalPhrases = size();
	memcpy(outData, (char*)&totalPhrases, sizeof(totalPhrases));
	outData+=sizeof(totalPhrases);

	// for each phrase
	Tran1OutputIterator aUnit;
	for (aUnit=begin(); aUnit!=end(); aUnit++)
	{
		aUnit->streamOut(outData);
	}
}
// --------------------------------------------------------------------------
void Tran1Output::streamOut(char*& outData)
{
	// save the total number of characters in headWordSurfaceForm_
	int totalCharacters = headWordSurfaceForm_.length();
	memcpy(outData, (char*)&totalCharacters, sizeof(totalCharacters));
	outData+=sizeof(totalCharacters);

	// save each character of word headWordSurfaceForm_
	for (int i=0; i<totalCharacters; i++)
	{
		char c = headWordSurfaceForm_[i];
		memcpy(outData, (char*)&c, sizeof(c));
		outData+=sizeof(c);
	}
	
	// save headWordPOS_
	memcpy(outData, (char*)&headWordPOS_, sizeof(headWordPOS_));
	outData+=sizeof(headWordPOS_);

	// save headWordSSSet_
	memcpy(outData, (char*)&headWordSSSet_, sizeof(headWordSSSet_));
	outData+=sizeof(headWordSSSet_);

	// save headWordForm_
	memcpy(outData, (char*)&headWordForm_, sizeof(headWordForm_));
	outData+=sizeof(headWordForm_);

	// save headWordSourceUnitNumber_
	memcpy(outData, (char*)&headWordSourceUnitNumber_, sizeof(headWordSourceUnitNumber_));
	outData+=sizeof(headWordSourceUnitNumber_);

	// save the total number of SCONPO elements in the target sequence order
	int totalTargetElements = elementsInTargetSequenceOrder_.size();
	memcpy(outData, (char*)&totalTargetElements, sizeof(totalTargetElements));
	outData+=sizeof(totalTargetElements);

	// save the target sequence of SCONPO elements
	LgsList(int)::iterator e;
	for (e=elementsInTargetSequenceOrder_.begin(); e!=elementsInTargetSequenceOrder_.end(); e++)
	{
		int targetElement = (*e);
		memcpy(outData, (char*)&targetElement, sizeof(targetElement));
		outData+=sizeof(targetElement);
	}

	// save the total number of OPADRO elements in the target sequence order
	int totalOpadroElements = opadro_.size();
	memcpy(outData, (char*)&totalOpadroElements, sizeof(totalOpadroElements));
	outData+=sizeof(totalOpadroElements);

	// save the target sequence of OPADRO elements
	for (e=opadro_.begin(); e!=opadro_.end(); e++)
	{
		int opadr = (*e);
		memcpy(outData, (char*)&opadr, sizeof(opadr));
		outData+=sizeof(opadr);
	}

	// save the value of scon1
	memcpy(outData, (char*)&scon1_, sizeof(scon1_));
	outData+=sizeof(scon1_);
}


// --------------------------------------------------------------------------
// Modify the sequence of source words to account for any switch 68 that has been added
// to the original sentence. The goal is to have a correct correspondence between the
// scon pointers and the elements of the source sentence. When Tran1 adds a new SWORK to
// the source sentence (switch68), it renumbers the sequence of scon pointers. Term search
// needs to do that too.
// --------------------------------------------------------------------------
void Tran1OutputSequence::accountForSwitch68(LgsString switch68Identifier)
{
	// find those units that are switch 68
	for (Tran1OutputIterator aUnit=begin(); aUnit!=end(); aUnit++)
	{
		// this unit is a switch 68 - then add an element in the sequence of source words at the
		// location of the switch
		if (aUnit->headWordSurfaceForm() == switch68Identifier)
		{
			sourceWords_.insert(sourceWords_.begin()+aUnit->headWordSourceUnitNumber()-1,switch68Identifier);
		}
	}
}

