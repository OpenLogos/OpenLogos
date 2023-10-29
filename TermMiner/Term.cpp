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
// Term.cpp: implementation of the Term class.
//
//////////////////////////////////////////////////////////////////////

#include <logos_include/logoscommon.h>
#include <TermMiner/Term.h>
#include <TermMiner/TermSearchSentenceUnit.h>
#include <TermMiner/TermElement.h>
#include <TermMiner/DegreeQuery.h>
#include <TermMiner/MeaningIDQuery.h>
#include <TermMiner/MorphologyTableQuery.h>
#include <TermMiner/TransferGenderQuery.h>
#include <TermMiner/TargetUsageIDQuery.h>
#include <TermMiner/WordIDQuery.h>
#include <TermMiner/WordQuery.h>
#include <logos_libs/linguistic/llanguage.h>
#include <logos_libs/linguistic/lsentence.h>
#include <logos_libs/linguistic/sourcesentenceunit.h>
#include <logos_libs/translutility/translcommonobjects.h>
#include <logos_libs/sql/sqlexception.h>
#include <logos_libs/utility/stringutil.h>

// --------------------------------------------------------------------------
Term::Term()
{
	reset(0);
}

// --------------------------------------------------------------------------
void Term::reset(TermSearchSentenceUnit* sentenceUnit)
{
	// info for term search report
	sourceLanguage_ = "";
	targetLanguage_ = "";
	searchStatus_ = 0;
	sourceSurfaceForm_ = "";
	sourceCanonicalForm_ = "";
	sourceHeadTerm_ = "";
	sourceHeadInCanonicalForm_ = "";
	sourceTermInCanonicalForm_ = "";
	sourcePOS_ = 0;
	sourceGender_ = 9;
	targetSurfaceForm_ = "";
	targetStatus_ = 9;
	targetGender_ = 9;
	companyCode_.set("");
	sourceLocation_ = 0;
	sentenceNumber_ = 0;
	context_ = "";

	// additional info
	sourceHeadWord_ = "";
	setCorrectly_ = false;
	sentenceUnit_ = sentenceUnit;
}

// --------------------------------------------------------------------------
Term::~Term()
{
}

// --------------------------------------------------------------------------
// Define what to write in term search report for the gender
// --------------------------------------------------------------------------
LgsString Term::formattedGender(int aGender) 
{
	LgsString gender=" ";
	switch(aGender) 
	{
	case MASC: 
		gender="Masc"; 
		break;
	case FEM: 
		gender="Fem"; 
		break;
	case NEUT: 
		gender="Neut";
	}
	return gender;
}

// --------------------------------------------------------------------------
// Define what to write in term search report for the part-of-speech
// --------------------------------------------------------------------------
LgsString Term::formattedSourcePOS() 
{
	LgsString pos=" ";
	switch(sourcePOS_) 
	{
	case LLanguage::NOUN: 
		pos="Noun"; 
		break;
	case LLanguage::VERB: 
		pos="Verb"; 
		break;
	case LLanguage::ADVERB_LOCATIVE:
	case LLanguage::ADVERB: 
		pos="Adv"; 
		break;
	case LLanguage::ADJECTIVE: 
		pos="Adj"; 
	}
	return pos;
}

// --------------------------------------------------------------------------
// Return search status as a LgsString
// --------------------------------------------------------------------------
LgsString Term::formattedSearchStatus() 
{
	LgsString ss=" ";
	switch(searchStatus_) 
	{
	case UNFOUND_TERM: 
		ss="Unfound"; 
		break;
	case FOUND_TERM: 
		ss="Found"; 
	}
	return ss;
}

// --------------------------------------------------------------------------
// Return the transfer status as a LgsString
// --------------------------------------------------------------------------
LgsString Term::formattedTargetStatus()
{
	LgsString transferStatus=" ";
	switch(targetStatus_) 
	{
	case 1:		// autocoded
		transferStatus="A"; 
		break;
	case 2:		// unvetted
		transferStatus="U"; 
		break;
	case 3:		// vetted
		transferStatus="V"; 
	}
	return transferStatus;
}

// --------------------------------------------------------------------------
// The goal is to filter the given expression (main word of the dictionary match value)
// to obtain the root form if it is defined in the dictionary (database).
// If the decision is to use the root form, then it returns it, else it returns the
// expression unchanged.
// Arguments:
// expression -> the main word of the dictionary match of the given SSU (computed somewhere else)
// sourceUnit -> all info pertaining the expression
// --------------------------------------------------------------------------
LgsString Term::filterCanonicalForm(LgsString expression, SourceSentenceUnit* sourceUnit)
{
	LgsString finalForm = expression;
	int nWordClassCode = sourceUnit->primarySsu().wordClassCode();
	int degree = queryDatabaseForDegree(sourceUnit);
	LgsString inflectedFormSwitch = "";
	int rootUsageID = 0;
	int numericConstraint = 0;
	queryTableMorphology(sourceUnit,inflectedFormSwitch,rootUsageID,numericConstraint);
	int PATnumber = sourceUnit->PATNumber();
	int nonCanonicalTargetUsageID = queryDatabaseForTargetUsageID(sourceUnit->meaningID(),sourceUnit);
	int canonicalTargetUsageID = 0;
	int canonicalWordID = queryDatabaseForWordID(rootUsageID,sourceUnit);
	int canonicalMeaningID = queryDatabaseForMeaningID(sourceUnit,rootUsageID);
	canonicalTargetUsageID = queryDatabaseForTargetUsageID(canonicalMeaningID,sourceUnit);
	bool useFilteredForm = false;
   
	// decide whether to use the filtered form
	// -< case of German as the source language
	if (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::GermanID) 
	{
		if (nWordClassCode == 1 && inflectedFormSwitch == "Y") 
		{
			useFilteredForm = true;
		}
		if (   nWordClassCode == 1 
			&& numericConstraint == 2 
			&& rootUsageID != 0 
			&& inflectedFormSwitch == "N"
			&& canonicalTargetUsageID == nonCanonicalTargetUsageID) 
		{
			useFilteredForm = true;
		}
		if (   nWordClassCode == 2 
			&& inflectedFormSwitch == "Y") 
		{
			useFilteredForm = true;
		}
		if (   (nWordClassCode == 3 || nWordClassCode == 6) 
			&& (degree == 2 || degree == 3) 
			&& PATnumber != 489 
			&& PATnumber != 490 
			&& PATnumber != 491 
			&& PATnumber != 492 
			&& PATnumber != 494 
			&& PATnumber != 495) 
		{
			useFilteredForm = true;
		}
		if (   nWordClassCode == 4 
			&& inflectedFormSwitch == "Y") 
		{
			useFilteredForm = true;
		}
		if (   nWordClassCode == 4 
			&& (degree == 2 || degree == 3) 
			&& inflectedFormSwitch == "N" 
			&& PATnumber != 437 
			&& PATnumber != 446) 
		{
			useFilteredForm = true;
		}
	}
	// -< case of English as the source language
	else if (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::EnglishID) 
	{
		if (   nWordClassCode == 1
			&& numericConstraint == 2
			&& rootUsageID != 0
			&& canonicalTargetUsageID == nonCanonicalTargetUsageID)
		{
			useFilteredForm = true;
		}
		if (  nWordClassCode == 2 
			&& inflectedFormSwitch == "Y") 
		{
			useFilteredForm = true;
		}
		if (   (nWordClassCode == 3 || nWordClassCode == 6) 
			&& (degree == 2 || degree == 3) 
			&& PATnumber != 57 
			&& PATnumber != 193
			&& PATnumber != 194 
			&& PATnumber != 195 
			&& PATnumber != 290) 
		{
			useFilteredForm = true;
		}
		if (   nWordClassCode == 4 
			&& (degree == 2 || degree == 3) 
			&& PATnumber != 35 
			&& PATnumber != 202 
			&& PATnumber != 203
			&& PATnumber != 204 
			&& PATnumber != 287) 
		{
			useFilteredForm = true;
		}
	}
		
	if (useFilteredForm)
	{
		finalForm = queryDatabaseForWord(canonicalWordID,sourceUnit);
	}

	// last check: if the final form is empty for any reason (meaning there was a data problem, etc.)
	// then output the surface form because one cannot output nothing!
	// This is an arbitrary decision to avoid empty fields in term search report
	if (finalForm == "")
	{
		finalForm = expression;
	}
		
	return finalForm;
}

// -------------------------------------------------------------------
// Query table word_class_subtype from database for the specified field
// -------------------------------------------------------------------
int Term::queryDatabaseForDegree(SourceSentenceUnit* ssu) 
{
	int value = 0;
	SqlConnection* databaseConnection = TranslCommonObjects::GetSqlConnection();
	DegreeQuery databaseQuery;

	try 
	{
		// establish database connection
		databaseQuery.open(databaseConnection);
	}
	catch (SqlException& x) 
	{
		// catch any connection problems
		throw(x);
	}
	// query the database
	databaseQuery.execute(ssu);
	// extract info from query result
	databaseQuery.fetch(value);
	// clean up
	databaseQuery.close();

	return value;
}

// -------------------------------------------------------------------
// Query table morphology for several fields
// -------------------------------------------------------------------
void Term::queryTableMorphology(SourceSentenceUnit* ssu,
												 LgsString& inflectedFormSwitch,
												 int& rootUsageID,
												 int& numericConstraint)
{
	SqlConnection* databaseConnection = TranslCommonObjects::GetSqlConnection();
	MorphologyTableQuery databaseQuery;

	try 
	{
		// establish database connection
		databaseQuery.open(databaseConnection);
	}
	catch (SqlException& x) 
	{
		// catch any connection problems
		throw(x);
	}
	// query the database
	databaseQuery.execute(ssu);
	// extract info from query result
	databaseQuery.fetch(inflectedFormSwitch,rootUsageID,numericConstraint);
	// clean up
	databaseQuery.close();
}

// -------------------------------------------------------------------
// Query table transfer from database to retrieve the target usage ID
// -------------------------------------------------------------------
int Term::queryDatabaseForTargetUsageID(int meaningID, SourceSentenceUnit* ssu) 
{
	int value = 0;
	SqlConnection* databaseConnection = TranslCommonObjects::GetSqlConnection();
	TargetUsageIDQuery databaseQuery;

	try 
	{
		// establish database connection
		databaseQuery.open(databaseConnection);
	}
	catch (SqlException& x) 
	{
		// catch any connection problems
		throw(x);
	}
	// query the database
	databaseQuery.execute(ssu->companyCode(),meaningID,ssu->language());
	// extract info from query result
	databaseQuery.fetch(value);
	// clean up
	databaseQuery.close();

	return value;
}

// -------------------------------------------------------------------
// Query table morphology for wordID
// -------------------------------------------------------------------
int Term::queryDatabaseForWordID(int nonCanonicalRootUsageID, SourceSentenceUnit* ssu) 
{
	int value = 0;
	SqlConnection* databaseConnection = TranslCommonObjects::GetSqlConnection();
	WordIDQuery databaseQuery;

	try 
	{
		// establish database connection
		databaseQuery.open(databaseConnection);
	}
	catch (SqlException& x) 
	{
		// catch any connection problems
		throw(x);
	}
	// query the database
	databaseQuery.execute(nonCanonicalRootUsageID,ssu->companyCode());
	// extract info from query result
	databaseQuery.fetch(value);
	// clean up
	databaseQuery.close();

	return value;
}

// -------------------------------------------------------------------
// Query table word_phrase for word
// -------------------------------------------------------------------
LgsString Term::queryDatabaseForWord(int canonicalWordID, SourceSentenceUnit* ssu) 
{
	LgsString value = "";
	SqlConnection* databaseConnection = TranslCommonObjects::GetSqlConnection();
	WordQuery databaseQuery;

	try 
	{
		// establish database connection
		databaseQuery.open(databaseConnection);
	}
	catch (SqlException& x) 
	{
		// catch any connection problems
		throw(x);
	}
	// query the database
	databaseQuery.execute(canonicalWordID,ssu->companyCode());
	// extract info from query result
	databaseQuery.fetch(value);
	// clean up
	databaseQuery.close();

	return value;
}

// -------------------------------------------------------------------
// Query table meaning for meaningID
// -------------------------------------------------------------------
int Term::queryDatabaseForMeaningID(SourceSentenceUnit* ssu, int usageID)
{
	int meaningID = 0;

	SqlConnection* databaseConnection = TranslCommonObjects::GetSqlConnection();
	MeaningIDQuery databaseQuery;

	try 
	{
		// establish database connection
		databaseQuery.open(databaseConnection);
	}
	catch (SqlException& x) 
	{
		// catch any connection problems
		throw(x);
	}
	// query the database
	databaseQuery.execute(ssu,ssu->language(),usageID);
	// extract info from query result
	databaseQuery.fetch(ssu,meaningID);
	// clean up
	databaseQuery.close();

	return meaningID;
}

// -------------------------------------------------------------------
// Compute the correct form of the head word.
// Find whether to use the canonical form, whether to
// lowercase the first capital letter, recognize the head word is the source head word is a
// compound word, etc.
// NOTE: anything else to change the form of the head should be put here, since the result of this
// method will be used to compute the surface form of the whole phrase.
// -------------------------------------------------------------------
LgsString Term::computeSourceHeadWord(SourceSentenceUnit* sourceUnit, LgsString wordSequence)
{
	// extract the main word from the head term
	LgsString mainWord = "";			// the main word to extract
	int headWordNumber = sourceUnit->headLocation();		// word number of the word containing the main word
	int firstCharacter = sourceUnit->hashLocation();		// first character where head word start in this word
	
	// some range checking
	if (headWordNumber<1) headWordNumber=1;			// head word is at least the first word of the sequence
	if (firstCharacter<1) firstCharacter=1;			// head word starts at least at the first character of the word that contains it
	
	// extract the word containing the main word
	int pos = 0;										// position of the next word separator (blank space)
	for (int i=1; i<headWordNumber; i++) 
	{
		pos = wordSequence.find(' ',0);
		if (pos!=LgsString::npos) 
		{
			wordSequence = wordSequence.substr(pos+1,wordSequence.size());
		}
	}

	// position of the last character of the main word (either the next blank space or the end of the sequence)
	int lastCharacter = wordSequence.find(' ',0);		

	// extract the main word - it is at the beginning of the string and has lastCharacter length
	if (lastCharacter != LgsString::npos) 
	{
		//mainWord = wordSequence.substr(pos,lastCharacter);
		mainWord = wordSequence.substr(0,lastCharacter);
	}
	else 
	{
		mainWord = wordSequence;
	}

	// extract the main word from the word that contains it
	//mainWord = mainWord.substr(firstCharacter-1,mainWord.size());

	// compute the case of the source head term. Keep the case as it is in the surface form. For the first
	// word, change to lowercase if it is the first word of the context_.
	LgsString dictionaryMatchOfTerm = sourceUnit->primarySsu().word();
	LgsString dictionaryMatchOfTermCaseInsensitive = StringUtil::lower(dictionaryMatchOfTerm);

	// extract the dictionary match of the main word from the dictionary match of the whole term (case
	// there are several words in the term)
	LgsString dictionaryMatchOfMainWord = "";
	LgsString mainWordCaseInsensitive = StringUtil::lower(mainWord);
	int posWord = dictionaryMatchOfTermCaseInsensitive.find(mainWordCaseInsensitive,0);
	if (posWord != LgsString::npos)
	{
		dictionaryMatchOfMainWord = dictionaryMatchOfTerm.substr(posWord,mainWord.length());
	}
	else
	{
		// string is not in the dictionary match
		dictionaryMatchOfMainWord = mainWord;
	}

	// if the word is not all upper case and the dictionary match is not initially capped, then change to lowercase
	if (!StringUtil::isAllUpperCase(mainWord) && !StringUtil::beginsUpperCase(dictionaryMatchOfMainWord))
	{
		StringUtil::toLower(mainWord);
	}

	return mainWord;
}

// --------------------------------------------------------------------------
LgsString Term::formattedSourceGender()
{
	return formattedGender(sourceGender_);
}

// --------------------------------------------------------------------------
LgsString Term::formattedTargetGender()
{
	return formattedGender(targetGender_);
}


// --------------------------------------------------------------------------
LgsString Term::sourceLanguage()
{
	return sourceLanguage_;
}

// --------------------------------------------------------------------------
LgsString Term::targetLanguage() 
{
	return targetLanguage_;
}

// --------------------------------------------------------------------------
int Term::searchStatus() 
{
	return searchStatus_;
}

// --------------------------------------------------------------------------
LgsString Term::sourceSurfaceForm() 
{
	return sourceSurfaceForm_;
}

// --------------------------------------------------------------------------
LgsString Term::sourceCanonicalForm() 
{
	return sourceCanonicalForm_;
}

// --------------------------------------------------------------------------
LgsString Term::sourceHeadWord() 
{
	return sourceHeadWord_;
}

// --------------------------------------------------------------------------
LgsString Term::sourceHeadTerm() 
{
	return sourceHeadTerm_;
}

// --------------------------------------------------------------------------
LgsString Term::sourceHeadInCanonicalForm() 
{
	return sourceHeadInCanonicalForm_;
}

// --------------------------------------------------------------------------
LgsString Term::sourceTermInCanonicalForm() 
{
	return sourceTermInCanonicalForm_;
}

// --------------------------------------------------------------------------
int Term::sourcePOS() 
{
	return sourcePOS_;
}

// --------------------------------------------------------------------------
int Term::sourceGender() 
{
	return sourceGender_;
}

// --------------------------------------------------------------------------
LgsString Term::targetSurfaceForm() 
{
	return targetSurfaceForm_;
}

// --------------------------------------------------------------------------
int Term::targetStatus() 
{
	return targetStatus_;
}

// --------------------------------------------------------------------------
int Term::targetGender() 
{
	return targetGender_;
}

// --------------------------------------------------------------------------
LgsString Term::companyCode() 
{
	return companyCode_.content();
}

// --------------------------------------------------------------------------
LgsString Term::subjectMatterCode() 
{
	return smc_.content();
}

// --------------------------------------------------------------------------
int Term::sourceLocation() 
{
	return sourceLocation_;
}

// --------------------------------------------------------------------------
int Term::sentenceNumber() 
{
	return sentenceNumber_;
}

// --------------------------------------------------------------------------
LgsString Term::context() 
{
	return context_;
}

// --------------------------------------------------------------------------
bool Term::setCorrectly() 
{
	return setCorrectly_;
}

// --------------------------------------------------------------------------
// Return whether there are missing fields in the definition of this term
// --------------------------------------------------------------------------
bool Term::hasMissingFields()
{
	bool result = false;		// by default: there are no missing fields
	if (   sourceSurfaceForm_ == ""
		|| sourceCanonicalForm_ == ""
		//|| sourceHeadTerm_ == ""
		//|| sourceHeadInCanonicalForm_ == ""
		//|| sourceTermInCanonicalForm_ == ""
		//|| targetSurfaceForm_ == ""
		//|| sourceLanguage_ == ""
		//|| targetLanguage_ == ""
		)
	{
		result = true;
	}
	return result;
}

// --------------------------------------------------------------------------
void Term::computeSourceSurfaceForm_GermanSource()
{
	sourceSurfaceForm_ = "";

	if (sentenceUnit_ != 0)
	{
		bool bCompoundWithoutHead = false;
		bool bFirstElementToOutput = true;
		bool bHyphen = false;
		bool bWasHyphen=false;
		LgsString Hyphen("-");
		LgsString Period(".");
		for (TermElementIterator element=sentenceUnit_->elementsInSourceSequenceOrder().begin(); element!=sentenceUnit_->elementsInSourceSequenceOrder().end(); element++)
		{
			if (bCompoundWithoutHead && (*element).isGermanCompoundAndNotHead())
			{
				continue;
			}
			bCompoundWithoutHead = false;

			if ((*element).sourceSentenceUnit() != 0)
			{
				// the next word in the sequence
				LgsString nextWord = (*element).sourceSentenceUnit()->surfaceExpressionAsString();

				// its dictionary match as found during the Dictionary Lookup phase
				LgsString dictionaryMatch = (*element).sourceSentenceUnit()->primarySsu().word();

				// if the word is not all upper case and the dictionary match is not initially capped
				// then change to lowercase
				if (!StringUtil::isAllUpperCase(nextWord) && !StringUtil::beginsUpperCase(dictionaryMatch))
				{
					StringUtil::toLower(nextWord);
				}
            
				// compute the space between this word and the previous one
				LgsString preceedingSpaces = computePreceedingSpaces(element);

				if( bHyphen && !preceedingSpaces.empty() )
				{
					preceedingSpaces.erase(0, 1);
				}
				bWasHyphen = bHyphen; // in case we have German compound non-head unit
				bHyphen = false;

				// check if have to add a period or a hyphen (inc#3226,3275)
				if( (element->sourceSentenceUnit()->formCode() == 9) && (element->sourceSentenceUnit()->superSetID() == 13) )
				{
					int iSubSet = element->sourceSentenceUnit()->subSetID();
					if( iSubSet == 455 )
					{
						nextWord += Period; // add a period right now
					}
					else if( (iSubSet == 123) || (iSubSet == 222) ||
								(iSubSet == 234) || (iSubSet == 567) || (iSubSet == 789) )
					{
						nextWord += Hyphen; // add a hyphen right now
						bHyphen = true; // and remove space later
					}
				}

				// Check if is a compund without a head.
				// add the word to the sequence
				// is this element part of a compound
				if ((*element).isGermanCompoundAndNotHead())
				{
					//Check if is a compund without a head.
					int nSize = nextWord.size();
					if( ( nSize >0) && (nextWord[nSize-1] == '-') )
					{
						bCompoundWithoutHead = true;
						if(bFirstElementToOutput )
						{   
							//Supress the leading space for the  first element to be outputed
							sourceSurfaceForm_ += nextWord;
							bFirstElementToOutput = false;
						}
						else 
						{
							sourceSurfaceForm_ +=  preceedingSpaces + nextWord;
						}
					}
					else
					{
						// German compound non-head unit is not outputed => restore bHyphen
						if( bWasHyphen )
						{
							bHyphen = true;
						}
					}
				}
				else
				{
					if(bFirstElementToOutput )
					{   
						//Supress the leading space for the  first element to be outputed
						sourceSurfaceForm_ += nextWord;
						bFirstElementToOutput = false;
					}
					else 
					{
						sourceSurfaceForm_ +=  preceedingSpaces + nextWord;
					}
			   }
			}
		}
	}
}

// --------------------------------------------------------------------------
// Remove the first article from the phrase (in both source elements and target
// elements).
// Note: this method alters the content of the noun phrase (number of elements) and
//       thus will not match the original content of the associated Tran1 unit.
// Return whether the first element of the source term has been removed.
// --------------------------------------------------------------------------
bool Term::removeFirstElementInSource()
{
	bool removedAnElement = false;

	if (sentenceUnit_ != 0)
	{
		// decide whether to remove the first source element in the current sequence
		TermElementIterator firstSourceElement = sentenceUnit_->elementsInSourceSequenceOrder().begin();
		if (firstSourceElement != sentenceUnit_->elementsInSourceSequenceOrder().end())
		{
			// is this element an article (either definite or indefinite)?
			if((*firstSourceElement).sourceSentenceUnit() != 0) 
			{
				int wordClass = firstSourceElement->sourceSentenceUnit()->primarySsu().wordClassCode();
				int formField = firstSourceElement->sourceSentenceUnit()->primarySsu().formCode();
				const SconTable& sconTable = firstSourceElement->targetSentenceUnit()->getSconTable();
				int set = firstSourceElement->sourceSentenceUnit()->primarySsu().setID();
				if (   isDeterminer(wordClass) 
					|| isNumber(wordClass,formField,sconTable)
					|| isCoordination(wordClass,set)
				   )
				{
					// discard this element
					sentenceUnit_->elementsInSourceSequenceOrder().erase(firstSourceElement);
					removedAnElement = true;
				}
			}
		}
	}

	return removedAnElement;
}

// --------------------------------------------------------------------------
// Return whether the first element of the target term has been removed.
// --------------------------------------------------------------------------
bool Term::removeFirstElementInTarget()
{
	bool removedAnElement = false;

	if (sentenceUnit_ != 0)
	{
		// decide whether to remove the first target element in the current sequence
		TermElementIterator firstTargetElement = sentenceUnit_->elementsInTargetSequenceOrder().begin();
		if (firstTargetElement != sentenceUnit_->elementsInTargetSequenceOrder().end())
		{
			int targetWordClass = firstTargetElement->targetSentenceUnit()->wordClassCode();
			int sourceWordClass = 0;
			int sourceFormField = 0;
			int sourceSetField = 0;
			if (firstTargetElement->sourceSentenceUnit() != 0)
			{
				sourceWordClass = firstTargetElement->sourceSentenceUnit()->primarySsu().wordClassCode();
				sourceFormField = firstTargetElement->sourceSentenceUnit()->primarySsu().formCode();
				sourceSetField = firstTargetElement->sourceSentenceUnit()->primarySsu().setID();
			}
			const SconTable& sconTable = firstTargetElement->targetSentenceUnit()->getSconTable();

			if (   isDeterminer(targetWordClass) 
				|| isNumber(sourceWordClass,sourceFormField,sconTable)
				|| isCoordination(sourceWordClass,sourceSetField)
			   )
			{
				// discard this element
				sentenceUnit_->elementsInTargetSequenceOrder().erase(firstTargetElement);
				removedAnElement = true;
			}
		}
	}

	return removedAnElement;
}

// --------------------------------------------------------------------------
// Decide whether to remove this element based on it being a determiner.
// --------------------------------------------------------------------------
bool Term::isDeterminer(int wordClass)
{
	bool remove = false;

	if (   wordClass == LLanguage::ARTICLE_DEFINITE			// WC14
		|| wordClass == LLanguage::ARTICLE_INDEFINITE		// WC15
		|| wordClass == LLanguage::ARITHMATE				// WC16
	   )
	{
		remove = true;
	}

	return remove;
}

// --------------------------------------------------------------------------
// Decide whether this element is a number - see incident 3190 + 3191
// --------------------------------------------------------------------------
bool Term::isNumber(int wordClass, int formField, const SconTable& sconTable)
{
	bool remove = false;

	if (wordClass == LLanguage::ADJECTIVE)
	{
		if (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::EnglishID)
		{
			remove = formField == 9;
		}
		else if (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::GermanID)
		{
			short scon45 = sconTable.scon45();
			remove = formField == 8 && scon45 == 5;
		}
	}

	return remove;
}

// --------------------------------------------------------------------------
// Remove the leading coordination from the phrase.
// These specs are valid for both English and German source languages - see incident 3218.
// --------------------------------------------------------------------------
bool Term::isCoordination(int wordClassCode, int set)
{
	bool remove = false;

	if (   TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::EnglishID
		|| TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::GermanID)
	{
		remove = wordClassCode == LLanguage::CONJUNCTION && set == 20;
	}

	return remove;
}

// --------------------------------------------------------------------------
// Reconstruct the whole head term (sourceTermInCanonicalForm_) based on 
// sourceHeadTerm_ in which sourceHeadWord_ is replaced by sourceHeadInCanonicalForm_.
// --------------------------------------------------------------------------
void Term::reconstructHeadTerm()
{
    sourceTermInCanonicalForm_ = "";

	if (sourcePOS_ == LLanguage::VERB)
	{
		sourceTermInCanonicalForm_ = sourceHeadInCanonicalForm_;
	}
	else
	{
		if (sourceHeadWord_ == sourceHeadTerm_)
		{
			sourceTermInCanonicalForm_ = sourceHeadInCanonicalForm_;
		}
		else
		{
			// replace sourceHeadWord_ by sourceHeadInCanonicalForm_ in sourceHeadTerm_
			int x = sourceHeadTerm_.find(sourceHeadWord_);
			if (x != NPOS)
			{
				sourceTermInCanonicalForm_ = sourceHeadTerm_;
				sourceTermInCanonicalForm_.replace(x,sourceHeadWord_.length(),sourceHeadWord_);
			}
			else
			{
				sourceTermInCanonicalForm_ = sourceHeadInCanonicalForm_;
			}
		}
	}
}

// --------------------------------------------------------------------------
// Compute the space between this word and the previous one.
// --------------------------------------------------------------------------
LgsString Term::computePreceedingSpaces(const TermElementIterator & element)
{
	LgsString preceedingSpaces = "";

	if (element->sourceSentenceUnit() != 0 && sentenceUnit_ != 0)
	{
		int nSpace = element->sourceSentenceUnit()->precedingSpaces();

		if (   element != sentenceUnit_->elementsInSourceSequenceOrder().begin() 
			&& element != sentenceUnit_->elementsInSourceSequenceOrder().end())
		{
			LgsString strSpace(nSpace, ' ');
			preceedingSpaces = strSpace;
		}
	}

	return preceedingSpaces;
}

// --------------------------------------------------------------------------
// Return the correct case of the given word.
// --------------------------------------------------------------------------
LgsString Term::checkCase(LgsString wordWithCorrectCase)
{
	if (   !StringUtil::beginsUpperCase(wordWithCorrectCase)
		&& !StringUtil::isAllUpperCase(wordWithCorrectCase))
	{
		// case of word which dictionary match is neither all upper case nor initially capped
		StringUtil::toLower(wordWithCorrectCase);
	}

	return wordWithCorrectCase;
}

// -------------------------------------------------------------------
// Query table morphology from database for the specified field
// -------------------------------------------------------------------
int Term::queryDatabaseForTransferGender() 
{
	int value = 0;

	if (sentenceUnit_ != 0)
	{
		SqlConnection* databaseConnection = TranslCommonObjects::GetSqlConnection();
		TransferGenderQuery databaseQuery;

		try 
		{
			// establish database connection
			databaseQuery.open(databaseConnection, sentenceUnit_->headElement().sourceSentenceUnit()->dictionary().targetLanguage());
		}
		catch (SqlException& x) 
		{
			// catch any connection problems
			throw(x);
		}

		// query the database
		databaseQuery.execute(sentenceUnit_->headElement().sourceSentenceUnit()->primarySsu());

		// extract info from query result
		databaseQuery.fetch(value);

		// clean up
		databaseQuery.close();
	}

	return value;
}

// -------------------------------------------------------------------
// Do not set the target gender if the target language is English.
// -------------------------------------------------------------------
void Term::setTransferGender()
{
	if (sentenceUnit_ != 0)
	{
		if (TranslCommonObjects::GetTargetLanguage()->id() != LLanguage::EnglishID)
		{
			// first, access the transfer gender from the database match (see incident 2606 - 2000/02/11)
			targetGender_ = queryDatabaseForTransferGender();

			// if the transfer gender was not masculine (1), feminine (2), or neutral (3) in the databse
			// match, then attempt to get it from what has been set in the target sentence unit of the head
			// element
			if (targetGender_ < 1 || targetGender_ > 3)
			{
				targetGender_ = sentenceUnit_->headElement().targetSentenceUnit()->gender();
			}
		}
		else
		{
			targetGender_ = 9;		// default (out of range value)
		}
	}
}

// --------------------------------------------------------------------------
// Construct the transfer of the term in canonical form.
// --------------------------------------------------------------------------
void Term::computeTargetCanonicalForm()
{
	targetSurfaceForm_ = "";

	if (sentenceUnit_ != 0)
	{
          LgsString strDuplicate = "";
          bool bDuplicate = false;
          TermElementIterator prevElement
            = sentenceUnit_->elementsInTargetSequenceOrder().end();
          // remove redundant elements - incident 2770

          for (TermElementIterator element = sentenceUnit_->elementsInTargetSequenceOrder().begin()
                 ; element != sentenceUnit_->elementsInTargetSequenceOrder().end()
                 ; element++)
            {
              // Compare the current element with the previous one. 
              // Remove redundant elements - incident 2770.
              if (   element != sentenceUnit_->elementsInTargetSequenceOrder().begin()
                     && prevElement != sentenceUnit_->elementsInTargetSequenceOrder().end()
                     && prevElement->targetSentenceUnit() == element->targetSentenceUnit()
                     )
                {
                  continue; // pointing to same target unit.
                }
			 
              LgsString nextWord = nextTargetWord(element);		// the next word in the sequence
			 
              // Use the trailing spaces instead of preceding spaces like generate.
              int nSpace = element->targetSentenceUnit()->trailingSpaces();
              LgsString trailingSpaces(nSpace,' ');

              // add the word and its spaces to the sequence
              targetSurfaceForm_ += nextWord + trailingSpaces;
			 
              prevElement = element;		// Remove redundant - incident 2770
            }

          removeHyphensAtEndOfTarget();
	}
}

// -------------------------------------------------------------------
// Removing undesirable elements at the end of a term.
// -------------------------------------------------------------------
void Term::removeHyphensAtEndOfTarget()
{
	int nSize = targetSurfaceForm_.size();
	while (( nSize >0) && (targetSurfaceForm_[nSize-1] == '-'))
	{
		targetSurfaceForm_.resize(nSize-1);
		nSize = targetSurfaceForm_.size();
	}
}

// -------------------------------------------------------------------
// Return the correct form of the next word in the transfer sequence
// -------------------------------------------------------------------
LgsString Term::nextTargetWord(const TermElementIterator &element)
{
	LgsString nextWord = "";

	// is it from a German compound and unfound word?
	if (element->isUnfoundGermanCompound())
	{
		nextWord = element->targetSentenceUnit()->word();
	}
	// all other cases
	else
	{
		nextWord = element->targetSentenceUnit()->surfaceExpressionAsString();
	}

	return nextWord;
}

// --------------------------------------------------------------------------
// NP clean up: remove all the first consecutive articles from the beginning of the 
// term in both source and target
// --------------------------------------------------------------------------
void Term::cleanBeginningOfTerm()
{
	if (sentenceUnit_ != 0)
	{
		while (removeFirstElementInSource());		// modify number of source elements
		while (removeFirstElementInTarget());		// modify number of target elements
	}
}

// --------------------------------------------------------------------------
void Term::doGeneralSettings()
{
	if (sentenceUnit_ != 0)
	{
		sourceLanguage_ = TranslCommonObjects::GetSourceLanguage()->shortDescription();
		targetLanguage_ = TranslCommonObjects::GetTargetLanguage()->shortDescription();
		sourceLocation_ = sentenceUnit_->sourceLocationInSentence();
		sentenceNumber_ = sentenceUnit_->sentence()->position();
		context_ = sentenceUnit_->context();
	}
}

