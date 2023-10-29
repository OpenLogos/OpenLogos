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
// Term.h: interface for the Term class.
//
//////////////////////////////////////////////////////////////////////

#ifndef _Term_h_
#define _Term_h_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <logos_libs/linguistic/companycode.h>
#include <logos_libs/SubjectMatterCodes/SubjectMatterCode.h>
#include "TermElement.h"

class SourceSentenceUnit;
class SconTable;
//class TermElement;
class TermSearchSentenceUnit;

// --------------------------------------------------------------------------
class Term : public Object
{
public:
	Term();
	virtual ~Term();

	// constants that should be used to identify the search status
	enum SEARCH_STATUS {UNFOUND_TERM=1,FOUND_TERM};

	// getter methods
	LgsString sourceLanguage();
	LgsString targetLanguage();
	int searchStatus();
	LgsString sourceSurfaceForm();
	LgsString sourceCanonicalForm();
	LgsString sourceHeadWord();
	LgsString sourceHeadTerm();
	LgsString sourceHeadInCanonicalForm();
	LgsString sourceTermInCanonicalForm();
	int sourcePOS();
	int sourceGender();
	LgsString targetSurfaceForm();
	int targetStatus();
	int targetGender();
	LgsString companyCode();
	LgsString subjectMatterCode();
	int sourceLocation();
	int sentenceNumber();
	LgsString context();

	// formatting of features
	LgsString formattedSearchStatus();
	LgsString formattedGender(int);
	LgsString formattedSourceGender();
	LgsString formattedTargetGender();
	LgsString formattedSourcePOS();
	LgsString formattedTargetStatus();

	bool setCorrectly();

	enum GENDER_CODE {MASC = 1, FEM = 2, NEUT = 3};

protected:
	//  +-------------------------------------------------+
	// -| information required for the term search report |
	//  +-------------------------------------------------+
	LgsString sourceLanguage_;				// source language two-letter code
	LgsString targetLanguage_;				// target language two-letter code
	int searchStatus_;					// status of this term: known or unknown to the dictionary
	LgsString sourceSurfaceForm_;			// source word(s) that form this term
	LgsString sourceCanonicalForm_;		// canonical form of the main word of the source form of this term
	LgsString sourceHeadTerm_;				// the whole main term that contains sourceHeadWord_
	LgsString sourceHeadInCanonicalForm_;	// the source head in canonical form
	LgsString sourceTermInCanonicalForm_;	// the whole source head in canonical form
	int sourcePOS_;						// part-of-speech of source term
	int sourceGender_;					// gender of source term
	LgsString targetSurfaceForm_;			// target form - transfer as found in lexicon
	int targetStatus_;					// status of the transfer
	int targetGender_;					// gender of corresponding target term
	CompanyCode companyCode_;			// dictionary in which this term was found
	SubjectMatterCode smc_;				// subject matter code
	int sourceLocation_;					// position of the term in that sentence
	int sentenceNumber_;					// number of the sentence in source text in which this occurrence has been found
	LgsString context_;						// context (sequence of words) around the term (whole sentence or part of the sentence/fixed window)

	//  +------------------------+
	// -| additional information |
	//  +------------------------+
	TermSearchSentenceUnit* sentenceUnit_;
	void reset(TermSearchSentenceUnit*);
	void doGeneralSettings();
	LgsString filterCanonicalForm(LgsString,SourceSentenceUnit*);
	bool setCorrectly_;					// whether this term is set properly and can be used
	LgsString sourceHeadWord_;

	// information about the head word of this term
	LgsString computeSourceHeadWord(SourceSentenceUnit*,LgsString);
	LgsString computeHeadSourceCanonicalForm();							// return the canonical form of the head word of the given tran1 unit
	int queryDatabaseForTargetUsageID(int,SourceSentenceUnit*);			// query table transfer for the specified field
	int queryDatabaseForDegree(SourceSentenceUnit*);					// query table word_class_subtype for the specified field
	void queryTableMorphology(SourceSentenceUnit*,LgsString&,int&,int&);	// query table morphology for several fields
	int queryDatabaseForWordID(int,SourceSentenceUnit*);				// query table morphology for wordID
	LgsString queryDatabaseForWord(int,SourceSentenceUnit*);				// query table word_phrase for word
	int queryDatabaseForMeaningID(SourceSentenceUnit*,int);				// query table meaning for meaningID
	
	void reconstructHeadTerm();
	LgsString computeTermSourceCanonicalForm();							// return the canonical form of the whole term
	bool usedHeadSourceCanonicalForm_;									// whether the head form is the canonical form or not
	LgsString checkCase(LgsString);
	LgsString computePreceedingSpaces(const TermElementIterator &);
	void computeSourceSurfaceForm_GermanSource();	// update sourceSurfaceForm_

	// removing undesirable elements at the beginning of a phrase
	bool removeFirstElementInSource();
	bool removeFirstElementInTarget();
	bool isDeterminer(int);
	bool isNumber(int,int,const SconTable&);
	bool isCoordination(int,int);

	// construct the transfer of the term in canonical form
	void computeTargetCanonicalForm();		// set targetSurfaceForm_
	virtual LgsString nextTargetWord(const TermElementIterator &);
	void removeHyphensAtEndOfTarget();

	void setTransferGender();
	int queryDatabaseForTransferGender();

	bool hasMissingFields();

	void cleanBeginningOfTerm();
};

// --------------------------------------------------------------------------
// Definition of a vector of Term objects
// --------------------------------------------------------------------------
typedef LgsVector(Term) TermVector;
typedef TermVector::iterator TermIterator;

#endif // _Term_h_
