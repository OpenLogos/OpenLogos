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
// File: ProperNameRecognizer.h (interface)
// Purpose: define the strategy to recognize proper names.
// --------------------------------------------------------------------------

#ifndef _ProperNameRecognizer_h
#define _ProperNameRecognizer_h

#include <logos_libs/PatternRecognition/ProperNamePattern.h>
#include <logos_libs/PatternRecognition/ProperName.h>
#include <logos_libs/PatternRecognition/ChangedName.h>

class SourceUnitVector;

// --------------------------------------------------------------------------
// Interface for class ProperNameRecognizer
// --------------------------------------------------------------------------
class ProperNameRecognizer 
{
public:
	ProperNameRecognizer(LgsString);									// constructor - give the file where the proper name patterns are located
	ProperNameRecognizer();											// constructor - from the database
	virtual ~ProperNameRecognizer();								// destructor
	void recognizePatternsIn(SourceUnitVector*);					// recognize proper names in the given sequence (modify elements to reflect discovery)
	bool disabled() {return !patternsExist_;}

private:
	bool patternsExist_;											// whether the patterns are available for recognition
	ProperNamePatternVector patterns_;								// the list of patterns to recognize proper names
	LgsStringVector knownLastNames_;									// list of last names already recognized
	ChangedNameVector changedNames_;									// list of original info of names changed by PNR in the document

	void clear();

	void restoreOriginalInformation(SourceUnitVector*);

	void readPatterns(LgsString);										// read proper name patterns - set whether this was successful
	void readPatternsFromDatabase();

	bool isTitle13(LgsString,int,int,int,int,int);						// return whether the SSU can be the stated proper name constituent
	bool isTitle(LgsString,int,int,int,int,int);
	bool canBeFirstName(LgsString,int,int,int,int,int,bool);
	bool isFirstName(LgsString,int,int,int,int);
	bool isLastName(LgsString,int,int,int,int,int);
	bool isKnownLastName(LgsString);
	bool canBeLastName(LgsString,int,int,int,int,int);
	bool couldBeLastName(LgsString,int,int,int,int,int);
	bool isInitial(int,int,int,int);
	bool isPostPosedTitle(int,int,int,int);
	bool isRomanNumeral(int,int,int,int);
	bool isEndedByApostrophe(LgsString);
	bool hasCorrectForm(LgsString);
	LgsString clean(LgsString);

	LgsStringVector getPossibleConstituents(LgsString,int,int,int,int,int,bool);	// get the possible proper name elements for this token
	void displayConstituents(LgsStringVector);

	ProperNameVector restrictAgenda(ProperNameVector&,LgsStringVector&,int,LgsString);		// restrict content of agenda
	void expandAgendaWithNewPatterns(ProperNameVector&,LgsStringVector&,int,LgsString);		// augment content of agenda
	void checkMatchedPatternsInAgenda(ProperNameVector&,ProperName&);

	void finalizeMatchedProperName(ProperName,SourceUnitVector*);				// change SSU's involved in recognized proper names
	void finalizeFirstName(SourceSentenceUnit*);
	void finalizeLastName(SourceSentenceUnit*);
};

#endif
