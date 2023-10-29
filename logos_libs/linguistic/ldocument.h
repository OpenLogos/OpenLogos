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
// File: LDocument.h (interface)
// Purpose: Definition of the concept of a document - a series of sentences.
//          A document has a source language, one or more target languages,
//          and a set of streams to contain the translation, the term search
//          report, etc.
// --------------------------------------------------------------------------

#ifndef __LDocument_h__
#define __LDocument_h__

#include <logos_libs/linguistic/translationobject.h>
#include <logos_libs/linguistic/lsentence.h>
#include <logos_libs/linguistic/ldocument.h>
#include <logos_libs/patternmatcher/factory.h>
#include <logos_libs/startrules/factory.h>

typedef RE_Engine<PM_Variable> RuleEngine;


// --------------------------------------------------------------------------
// Definition of class LDocument
// --------------------------------------------------------------------------
class LDocument: public TranslationObject
{
public:
	LDocument();                                 // default constructor
	LDocument(LDictionary&);                     // create a document for lookup / output process
	LDocument(const LDocument&);                 // create this document from a given document
	virtual ~LDocument();                        // destructor

	virtual void lookup();              // match each source element with dictionary
	void generateTranslation();         // generate the translation in stream (output file) - for translation jobs
	void termSearch();                  // capture all terms in this document, generate report in stream - for term search jobs
	void cleanUpSentence(LSentence*);   // free memory from info about current sentence

	const LSentenceVector& sentences() const {return v_sentences;}		// sentences in this document
	virtual int translatedWordCount() const;

	void termSearchStream(ostream*);
	ostream& termSearchStream() const;

protected:
	void completeLookup(LSentence*);					      	// performs the part of Lookup that must occur after a long sentence has been split
	void completeGenerate(LSentence*);						   // performs the part of Generate that must occur after a long sentence has been re-built from the sub-sentences that it was split into
	void addSentence(LSentence*);
	void incrementTranslatedWordCount(const LSentence&);	// update total words billable by Logos
	bool doLookup();
	bool doGenerate();

private:
	LgsString v_header;
	LSentenceVector v_sentences;					// list of sentences contained in this document
	ostream* p_termSearchStream;					// stream containing the term search report
	int translatedWordCount_;
	static int st_nextSentencePosition_;		// sentence number in the document
};

// --------------------------------------------------------------------------
// Add a setence object to this document
// --------------------------------------------------------------------------
inline void LDocument::addSentence(LSentence* sentence)
{
   sentence->position(++st_nextSentencePosition_);
}

// --------------------------------------------------------------------------
inline void LDocument::termSearchStream(ostream* s)
{
	p_termSearchStream = s;
}

// --------------------------------------------------------------------------
inline ostream& LDocument::termSearchStream() const 
{
	return *p_termSearchStream;
}

#endif


