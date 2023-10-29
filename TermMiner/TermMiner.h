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
program. If not, write to Globalware AG, Hospitalstraße 6, D-99817 Eisenach.

Linux port modifications and additions by Bernd Kiefer, Walter Kasper,
Deutsches Forschungszentrum fuer kuenstliche Intelligenz (DFKI)
Stuhlsatzenhausweg 3, D-66123 Saarbruecken
*/
// TermMiner.h: interface for the TermMiner class.
//
//////////////////////////////////////////////////////////////////////

#ifndef _TermMiner_h_
#define _TermMiner_h_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <TermMiner/Term.h>
#include <TermMiner/BinaryTree.h>
#include <TermMiner/TermSearchSentenceUnit.h>
#include <TermMiner/TermSearchStatistic.h>

class LSentence;
class Diagnostic;
class CompanyCodeVector;
class SubjectMatterCodeTree;

// --------------------------------------------------------------------------
class TermMiner : public Object
{
public:
	TermMiner();
	virtual ~TermMiner();
	void captureTermsIn(LSentence*);			// split source sentence into term units
	void categorizeTerms();			// after generate, construct terms
	void generateDiagnostic();
	void generateTermSearchReport(ostream&);

private:
	LSentence* currentSentence_;		// info from Lookup, RES, and Generate
	TermSearchSentenceUnitVector sentenceUnits_;
	BinaryTree foundTerms_;			// pointer to the appropriate set of found terms (one of the above)
	BinaryTree unfoundTerms_;			// pointer to the appropriate set of unfound terms (one of the above)
	TermSearchStatistic stat_;

	// for diagnostic purposes
	TermVector dictionaryFoundTerms_;
	TermVector dictionaryUnfoundTerms_;
	TermVector tran1NounPhraseTerms_;
	TermVector tran1GermanCompoundTerms_;
	Diagnostic* diagnostic_;
   CompanyCodeVector* userSelectedCCs;
   SubjectMatterCodeTree* userSelectedSMCs;
	LgsString yes_no(bool);
	void diagnoseTermSearchSentenceUnits();
	LgsStringVector settingsForTransferInCanonicalForm_;
	void diagnoseSettingsForTransferInCanonicalForm();
	void diagnoseSentenceUnitsInSequenceOrder();
	void diagnoseSelectedTerms();
	void diagnoseSelectedTerms(TermVector&,LgsString);

	// job control arguments related to term search
	void getTermSearchJobArguments();
	int minFound_;
	int maxFound_;
	int minUnfound_;
	int maxUnfound_;
   bool extendedSearch_;
	bool reportUnfoundTerms_;
	bool reportNouns_;
	bool reportVerbs_;
	bool reportAdverbs_;
	bool reportAdjectives_;
	bool reportFoundTerms_;

	// split source sentence into term search units (NPs, compounds, words)
	void constructTermSearchSentenceUnits();

	// set all scons in transfer to have transfer in canonical form
	void setTransferForCanonicalForm();

	bool keepForTermSearchReport(TermSearchSentenceUnit &);
	bool filterUndesirableTerms(TermSearchSentenceUnit &);
	bool isURLaddress(LgsString);
	bool isOnlyPunctuationAndDigits(LgsString);
	LgsString key(Term*);
	void setAsDictionaryTerm(TermSearchSentenceUnit &);
	void setAsGermanNounPhraseTerm(TermSearchSentenceUnit &);
	void setAsEnglishNounPhraseTerm(TermSearchSentenceUnit &);
	void setAsGermanCompoundTerm(TermSearchSentenceUnit &);
	bool reportThisWordClassCode(int);
        bool isValidCC_SMC(TermSearchSentenceUnit &);

	void generateStatReport();
};

#endif // _TermMiner_h_
