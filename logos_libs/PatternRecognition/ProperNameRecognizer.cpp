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
// File: ProperNameRecognizer.cpp (implementation)
// --------------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/PatternRecognition/ProperNameRecognizer.h>
#include <logos_libs/linguistic/sourcesentenceunit.h>
#include <logos_libs/translutility/translcommonobjects.h>

// --------------------------------------------------------------------------
// Create a new proper name recognizer.
// The patterns to recognize proper names for English and German are read from
// the specified file.
// --------------------------------------------------------------------------
ProperNameRecognizer::ProperNameRecognizer(LgsString filename) 
{
	clear();
	readPatterns(filename);				// read the patterns from a file
}


// --------------------------------------------------------------------------
// Create a new proper name recognizer.
// The patterns to recognize proper names for English and German are read from
// the database.
// --------------------------------------------------------------------------
ProperNameRecognizer::ProperNameRecognizer() 
{
	clear();
	readPatternsFromDatabase();		// read patterns from the database (TO BE DEFINED)
}


// --------------------------------------------------------------------------
// Reset this object
// --------------------------------------------------------------------------
void ProperNameRecognizer::clear()
{
	knownLastNames_.clear();			// start with an empty list
	patterns_.clear();					// no patterns loaded yet
	patternsExist_=false;				// whether patterns are loaded
	changedNames_.clear();				// reset list of changed names
}


// --------------------------------------------------------------------------
// Destructor
// --------------------------------------------------------------------------
ProperNameRecognizer::~ProperNameRecognizer() 
{
}


// --------------------------------------------------------------------------
// Read in the patterns to recognize proper names, from the given filename.
// Set whether this operation has been successful.
// --------------------------------------------------------------------------
void ProperNameRecognizer::readPatterns(LgsString filename) 
{
	ifstream infile(filename.c_str(),ios::in);
	int patternNumber = 0;

	if (!infile.good()) 
	{
		patternsExist_ = false;
	}
	else 
	{
		LgsString token;
		ProperNamePattern pattern;
		while (infile) 
		{
			infile >> token;
			// token identifies the beginning of a new pattern
			if (token == "[") 
			{
				pattern.clear();							// start a new pattern object
				patternNumber++;
				pattern.setPatternNumber(patternNumber);
			}
			// token identifies the end of the current pattern
			else if (token == "]") 
			{
				patterns_.append(pattern);		// add pattern to list of patterns
			}
			// token identifies a pattern constituent
			else 
			{
				pattern.append(token);						// add constituent to current pattern
			}
		}
		patternsExist_ = patterns_.size()>0;
	}
}


// --------------------------------------------------------------------------
// Read in the patterns to recognize proper names, from the database.
// Set whether this operation has been successful.
// --------------------------------------------------------------------------
void ProperNameRecognizer::readPatternsFromDatabase() 
{
	// TO BE DEFINED WHEN MOVE TO DATABASE IS DECIDED
	patternsExist_=false;			// set whether this operation has been successful
}


// --------------------------------------------------------------------------
// Return whether the given SSU could be a title.
// Assumption: the Logos dictionary contains the common titles. There is currently no attempt
// to define whether the SSU could be a title if it is not already defined as such.
// However, there are additional constraints on what can be a title in order to eliminate
// special cases.
// --------------------------------------------------------------------------
bool ProperNameRecognizer::isTitle(LgsString surfaceForm,
											  int wordClassCode,
											  int superSetId,
											  int setId,
											  int subsetId,
											  int formCode) 
{
	bool correctFormCode = false;
	if (TranslCommonObjects::GetSourceLanguage()->id()==LLanguage::EnglishID)
	{
		correctFormCode = !(formCode == 4 || formCode == 7);
	}
	else if (TranslCommonObjects::GetSourceLanguage()->id()==LLanguage::GermanID)
	{
		correctFormCode = !(formCode == 2 || formCode == 12);
	}

	return    wordClassCode == 1
		    && superSetId == 5 
		    && setId == 91
		    && subsetId == 807
		    && correctFormCode
		    && !StringUtil::isAllUpperCase(surfaceForm)
		    && StringUtil::beginsUpperCase(surfaceForm);
}


// --------------------------------------------------------------------------
// A title that has form 13
// --------------------------------------------------------------------------
bool ProperNameRecognizer::isTitle13(LgsString surfaceForm,
												 int wordClassCode,
												 int superSetId,
												 int setId,
												 int subsetId,
												 int formCode) 
{
	return    isTitle(surfaceForm,wordClassCode,superSetId,setId,subsetId,formCode)
		    && formCode == 13;
}


// --------------------------------------------------------------------------
// Return whether the given source form is terminated by an ' (eg, Stevens')
// or by an 's (eg, John's).
// --------------------------------------------------------------------------
bool ProperNameRecognizer::isEndedByApostrophe(LgsString surfaceForm)
{
	return    surfaceForm[surfaceForm.size()-1] == '\''
			 || (surfaceForm[surfaceForm.size()-2] == '\'' && surfaceForm[surfaceForm.size()-1] == 's');
}


// --------------------------------------------------------------------------
// Clean the surface form from undesirable characters for recognition.
// --------------------------------------------------------------------------
LgsString ProperNameRecognizer::clean(LgsString originalSurfaceForm)
{
	LgsString newSurfaceForm = originalSurfaceForm;

	if (originalSurfaceForm[originalSurfaceForm.size()-1] == '\'')		// case terminated by '
	{
		newSurfaceForm = originalSurfaceForm.substr(0,originalSurfaceForm.size()-1);
	}
	else if (   originalSurfaceForm[originalSurfaceForm.size()-2]=='\'' 
		      && originalSurfaceForm[originalSurfaceForm.size()-1]=='s')		// case terminated by 's
	{
		newSurfaceForm = originalSurfaceForm.substr(0,originalSurfaceForm.size()-2);
	}

	return newSurfaceForm;
}


// --------------------------------------------------------------------------
// Return whether the given source form contains a correct sequence of characters
// for being a first name or a last name. Basically: initially capped and only alpha
// characters or terminated by ' or 's.
// --------------------------------------------------------------------------
bool ProperNameRecognizer::hasCorrectForm(LgsString surfaceForm)
{
	return    StringUtil::isAllAlphaNumeric(surfaceForm)
			 && StringUtil::beginsUpperCase(surfaceForm)
			 ;
}


// --------------------------------------------------------------------------
// Return whether the given SSU is a first name constituent.
// --------------------------------------------------------------------------
bool ProperNameRecognizer::isFirstName(LgsString surfaceForm,
													int wordClassCode,
													int superSetId,
													int setId,
													int subsetId)
{
	return ((wordClassCode == 1) && (superSetId == 5) && ((setId == 5) || (setId == 91)) &&
           (subsetId == 207) && hasCorrectForm(surfaceForm));
}


// --------------------------------------------------------------------------
// Return whether the given SSU could be a first name.
// A unit could be a first name if it is initially capped and it is not a title 
// nor an initial, nor anyhting else.
// --------------------------------------------------------------------------
bool ProperNameRecognizer::canBeFirstName(LgsString surfaceForm,
														int wordClassCode,
														int supersetId,
														int setId,
														int subsetId,
														int formCode,
														bool unfoundWord) 
{
	return    !isTitle(surfaceForm,wordClassCode,supersetId,setId,subsetId,formCode) 
		    && !isInitial(wordClassCode,supersetId,setId,subsetId)
		    && !isPostPosedTitle(wordClassCode,supersetId,setId,subsetId)
		    && !isRomanNumeral(wordClassCode,supersetId,setId,subsetId)
		    && hasCorrectForm(surfaceForm);

}


// --------------------------------------------------------------------------
// Return whether the given SSU is a known last name constituent.
// Search in the list of known last names (for this document) whether this SSU
// has already been recognized as a last name.
// --------------------------------------------------------------------------
bool ProperNameRecognizer::isKnownLastName(LgsString surfaceForm) 
{
	bool alreadylastName=false;
	bool found=false;
	LgsStringIterator lastName=knownLastNames_.begin();

	while (!found && lastName!=knownLastNames_.end())
	{
		if (*lastName == surfaceForm) 
		{
			alreadylastName=true;
			found=true;
		}
		lastName++;
	}

	return alreadylastName;
}


// --------------------------------------------------------------------------
// Return whether the given SSU is a last name constituent (whether found
// or unfound).
// --------------------------------------------------------------------------
bool ProperNameRecognizer::isLastName(LgsString surfaceForm,
												  int wordClassCode,
												  int supersetId,
												  int setId,
												  int subsetId,
												  int formCode)
{
	/*
	bool hasCorrectFormCode = false;
	if (TranslCommonObjects::GetSourceLanguage()->id()==LLanguage::EnglishID)
	{
		hasCorrectFormCode = formCode == 1;
	}
	else if (TranslCommonObjects::GetSourceLanguage()->id()==LLanguage::GermanID)
	{
		hasCorrectFormCode = formCode == 9;
	}
	*/

   // subsetId should be 827 when tran rules can handle 827)
	return ((wordClassCode == 1) && (supersetId == 5) && ((setId == 5) || (setId == 91)) &&
           (subsetId == 207) && hasCorrectForm(surfaceForm));
}


// --------------------------------------------------------------------------
// Return whether the given SSU could be a last name constituent.
// --------------------------------------------------------------------------
bool ProperNameRecognizer::canBeLastName(LgsString surfaceForm,
													  int wordClassCode,
													  int supersetId,
													  int setId,
													  int subsetId,
													  int formCode) 
{
		return (   isLastName(surfaceForm,wordClassCode,supersetId,setId,subsetId,formCode)
				  || couldBeLastName(surfaceForm,wordClassCode,supersetId,setId,subsetId,formCode));
}


// --------------------------------------------------------------------------
// Return whether the given SSU could be a last name constituent.
// --------------------------------------------------------------------------
bool ProperNameRecognizer::couldBeLastName(LgsString surfaceForm,
														 int wordClassCode,
														 int supersetId,
														 int setId,
														 int subsetId,
														 int formCode) 
{
	return (   !isTitle(surfaceForm,wordClassCode,supersetId,setId,subsetId,formCode) 
			  && !isInitial(wordClassCode,supersetId,setId,subsetId)
			  && !isPostPosedTitle(wordClassCode,supersetId,setId,subsetId)
			  && !isRomanNumeral(wordClassCode,supersetId,setId,subsetId)
			  && hasCorrectForm(surfaceForm)
			);
}


// --------------------------------------------------------------------------
// Return whether the given SSU could be an initial.
// Assumption: the Logos dictionary contains all initials. Therefore, there is no
// attempt to define whether the SSU could be an initial if it is not already defined
// as one.
// --------------------------------------------------------------------------
bool ProperNameRecognizer::isInitial(int wordClassCode,
												 int supersetId,
												 int setId,
												 int subsetId) 
{
	return    wordClassCode == 1
		    && supersetId == 1
		    && setId == 1
		    && subsetId == 900;
}


// --------------------------------------------------------------------------
// Return whether the given SSU could be a post-posed title constituent.
// Assumption: the Logos dictionary contains all post-posed titles. Therefore, there is no
// attempt to define whether the SSU could be a post-posed title if it is not already defined
// as one.
// --------------------------------------------------------------------------
bool ProperNameRecognizer::isPostPosedTitle(int wordClassCode,
														  int supersetId,
														  int setId,
														  int subsetId) 
{
	return    wordClassCode == 1
		    && supersetId == 5
		    && setId == 91
		    && subsetId == 195;
}


// --------------------------------------------------------------------------
// Return whether the given SSU could be a roman numeral constituent.
// Assumption: the Logos dictionary contains all roman numerals. Therefore, there is no
// attempt to define whether the SSU could be a roman numeral if it is not already defined
// as one.
// --------------------------------------------------------------------------
bool ProperNameRecognizer::isRomanNumeral(int wordClassCode,
														int supersetId,
														int setId,
														int subsetId) 
{
	return    wordClassCode == 16
		    && supersetId == 4
		    && setId == 92
		    && subsetId == 92;
}


// --------------------------------------------------------------------------
// Return all the possible proper name elements that this token could be.
// The token is defined by the features corresponding to the arguments of this method.
// --------------------------------------------------------------------------
LgsStringVector ProperNameRecognizer::getPossibleConstituents(LgsString surfaceForm,
																				 int wordClassCode,
																				 int supersetId,
																				 int setId,
																				 int subsetId,
																				 int formCode,
																				 bool unfoundWord)
{
	LgsStringVector possibleElements;

	// if this SSU has already been recognized as a last name, mark it as a last name
	if (isKnownLastName(surfaceForm)) 
	{
		possibleElements.push_back("KnownLastName");
		possibleElements.push_back("LastName");
	}

	// else find all possible constituents for this token
	else 
	{
		// could it be a title with form 13?
		if (isTitle13(surfaceForm,wordClassCode,supersetId,setId,subsetId,formCode)) 
		{
			possibleElements.push_back("Title13");
		}
		// could it be a title (all other titles)?
		else if (isTitle(surfaceForm,wordClassCode,supersetId,setId,subsetId,formCode)) 
		{
			possibleElements.push_back("Title");
		}
		// could it be already defined as a found first name? (exists in dictionary as a first name)
		if (   isFirstName(surfaceForm,wordClassCode,supersetId,setId,subsetId) 
		    && !unfoundWord)
		{
			possibleElements.push_back("FoundFirstName");
		}
		// could it be then a first name?
		if (   isFirstName(surfaceForm,wordClassCode,supersetId,setId,subsetId)
			|| canBeFirstName(surfaceForm,wordClassCode,supersetId,setId,subsetId,formCode,unfoundWord)) 
		{
			possibleElements.push_back("FirstName");
		}
		// could it be an initial?
		if (isInitial(wordClassCode,supersetId,setId,subsetId)) 
		{
			possibleElements.push_back("Initial");
		}
		// could it be a last name already defined in the dictionary?
		if (   isLastName(surfaceForm,wordClassCode,supersetId,setId,subsetId,formCode)
			&& !unfoundWord
			&& wordClassCode == LLanguage::NOUN) 
		{
			possibleElements.push_back("FoundLastName");
		}
		// could it be an unfound last name?
		if (   canBeLastName(surfaceForm,wordClassCode,supersetId,setId,subsetId,formCode)
			&& unfoundWord) 
		{
			possibleElements.push_back("UnfoundLastName");
		}
		// could it be a restricted form of a last name?
		if (   (   wordClassCode == LLanguage::NOUN 
			    || wordClassCode == LLanguage::ADJECTIVE)
			&& couldBeLastName(surfaceForm,wordClassCode,supersetId,setId,subsetId,formCode))
		{
			possibleElements.push_back("RestrictedLastName");
		}
		// could it be a last name?
		if (canBeLastName(surfaceForm,wordClassCode,supersetId,setId,subsetId,formCode)) 
		{
			possibleElements.push_back("LastName");
		}
		// could it be a post-posed title?
		if (isPostPosedTitle(wordClassCode,supersetId,setId,subsetId)) 
		{
			possibleElements.push_back("PostPosedTitle");
		}
		// could it be a roman numeral?
		if (isRomanNumeral(wordClassCode,supersetId,setId,subsetId)) 
		{
			possibleElements.push_back("RomanNumeral");
		}
	}

	return possibleElements;
}

// --------------------------------------------------------------------------
void ProperNameRecognizer::displayConstituents(LgsStringVector possibleElements)
{
	cout << "constituents:";
	LgsStringIterator constituent;
	for (constituent=possibleElements.begin(); constituent!=possibleElements.end(); constituent++)
	{
		cout << " " << *constituent;
	}
	cout << endl;
}


// --------------------------------------------------------------------------
// Recognize proper names in the given sequence of source sentence units.
// This is the main behavior of the ProperNameRecognizer.
// Approach:
// (1) determine which set of possible proper name element each SSU could be.
// (2) then find patterns of possible elements that match the patterns to recognize proper names.
// --------------------------------------------------------------------------
void ProperNameRecognizer::recognizePatternsIn(SourceUnitVector* sourceUnits) 
{
	// report activity in diagnostic file
	TranslCommonObjects::GetDiagnostic()->writeLine("\n*Proper Name Recognition*");

	if (patternsExist_) 
	{
		// restore original SSU information for first and last names that have been recognized
		// in a previous sentence
		restoreOriginalInformation(sourceUnits);

		ProperNameVector agenda;		// start with an empty agenda
		ProperName bestMatch;			// best current match for current agenda
		SourceUnitIterator sourceUnit;
		int ssuNumber=0;

		for (sourceUnit=sourceUnits->begin(); sourceUnit!=sourceUnits->end(); sourceUnit++) 
		{
			LgsString surfaceForm = clean((*sourceUnit)->surfaceExpressionAsString());
			int wordClassCode = (*sourceUnit)->wordClassCode();
			int superSetId = (*sourceUnit)->superSetID();
			int setId = (*sourceUnit)->setID();
			int subsetId = (*sourceUnit)->subSetID();
			int formCode = (*sourceUnit)->formCode();
			bool unfoundWord = (*sourceUnit)->isUnfoundWord();
			LgsStringVector possibleConstituents = getPossibleConstituents(surfaceForm,
																							  wordClassCode,
																							  superSetId,
																							  setId,
																							  subsetId,
																							  formCode,
																							  unfoundWord);

			if (!possibleConstituents.empty())
			{
				// - remove from agenda any patterns that do not match any of these constituents
				agenda=restrictAgenda(agenda,possibleConstituents,ssuNumber,surfaceForm);
				// - add to the agenda any pattern that starts with any of these constituents
				expandAgendaWithNewPatterns(agenda,possibleConstituents,ssuNumber,surfaceForm);
				// - are there patterns that fired in agenda? If so, keep the longest match.
				checkMatchedPatternsInAgenda(agenda,bestMatch);
			}
			else
			{
				// no possible constituents for this SSU. This is then the end of a consecutive
				// sequence of SSU and therefore whatever best proper name was found is final.
				agenda.clear();
			}
			// if agenda is empty, then the current best match is final
			if (agenda.empty()) 
			{
				finalizeMatchedProperName(bestMatch,sourceUnits);
				bestMatch.clear();
			}
			ssuNumber++;
		}
	}
	else
	{
		// report in diagnostic file that there are no proper name patterns to use
		TranslCommonObjects::GetDiagnostic()->write("Proper name recognizer disabled (no proper names patterns were loaded)\n");
	}
}


// --------------------------------------------------------------------------
// Update the content of the agenda (currently selected patterns) based on the
// set of possible elements for the current token.
// Remove from agenda any patterns that do not match any of these constituents
// at the current position in the pattern. That means that the patterns will not
// match whatever the next tokens are. All patterns removed are put back in the 
// set of unselected patterns.
// --------------------------------------------------------------------------
ProperNameVector ProperNameRecognizer::restrictAgenda(ProperNameVector& currentAgenda,
																		LgsStringVector& possibleConstituents,
																		int ssuNumber,
																		LgsString surfaceForm) 
{
	ProperNameVector newAgenda;
	LgsStringIterator constituent;

	// for each constituent, check each pattern in agenda whether it still matches the sequence
	for (constituent=possibleConstituents.begin(); constituent!=possibleConstituents.end(); constituent++) 
	{
		ProperNameVectorIterator properName;
		for (properName=currentAgenda.begin(); properName!=currentAgenda.end(); properName++) 
		{
			// does constituent match again the current element in the pattern?
			// (only if the current element has a +, meaning it can have one or more instances)
			if (   (*properName).currentElement()==(*constituent)
				 && (*properName).currentElementIsOneOrMore())
			{
				ProperName pn=(*properName);
				pn.update(*constituent,ssuNumber,surfaceForm);
				newAgenda.push_back(pn);
			}

			// else, does constituent match the next element in the pattern?
			else 
			{
				if ((*properName).nextElement()==(*constituent)) 
				{
					ProperName pn=(*properName);
					pn.update(*constituent,ssuNumber,surfaceForm);
					pn.moveToNextElement();
					newAgenda.push_back(pn);
				}
			}
		}
	}

	return newAgenda;
}


// --------------------------------------------------------------------------
// Add to the agenda any pattern that starts with any of these constituents.
// We want to identify any new pattern that may start with the current token.
// --------------------------------------------------------------------------
void ProperNameRecognizer::expandAgendaWithNewPatterns(ProperNameVector& agenda,
													   LgsStringVector& possibleConstituents,
													   int ssuNumber,
													   LgsString surfaceForm) 
{
	LgsStringIterator constituent;
	// for each constituent, add in agenda all patterns that start with that constituent
	for (constituent=possibleConstituents.begin(); constituent!=possibleConstituents.end(); constituent++) 
	{
		ProperNamePatternVectorIterator pattern;
		// for each pattern in the set of original patterns recognizing proper names
		for (pattern=patterns_.begin(); pattern!=patterns_.end(); pattern++) 
		{
			// if this pattern starts with the current constituent, put it in agenda. It might be the
			// beginning of a proper name.
			if ((*pattern).element(1)==(*constituent)) 
			{
				ProperName newProperName(&(*pattern),*constituent,ssuNumber,surfaceForm);
				agenda.append(newProperName);
			}
		}
	}
}


// --------------------------------------------------------------------------
// Check the agenda for any patterns that are completed (ie, all elements have been
// matched). In case of conflicts, keep the longest match (the pattern with the most
// elements). Compare to the current match, and update accordingly.
// --------------------------------------------------------------------------
void ProperNameRecognizer::checkMatchedPatternsInAgenda(ProperNameVector& agenda,
														ProperName& currentBestMatch)
{
	ProperNameVectorIterator properName;
	for (properName=agenda.begin(); properName!=agenda.end(); properName++)
	{
		if (   (*properName).completed()													// fired pattern
			&& (*properName).numberOfElements() > currentBestMatch.numberOfElements())		// longer than current match
		{
			currentBestMatch.setTo(*properName);		// new best match (longer match)
		}
	}
}


// --------------------------------------------------------------------------
// Finalize the best match. This is the best match for this sequence of SSU.
// (1) mark the SSU elements accordingly (as appropriate proper name constituents)
// (2) store the last name in the cache (knownLastNames_)
// --------------------------------------------------------------------------
void ProperNameRecognizer::finalizeMatchedProperName(ProperName bestMatch,
													 SourceUnitVector* sourceUnits)
{
   if (!bestMatch.empty())
   {
      // get the constituents of the matched pattern
      LgsStringVector properNameConstituents = bestMatch.properNameConstituents();
      LgsStringIterator constituent;

      // get the corresponding SSU numbers
      LgsVector(int) sourceSentenceUnitNumbers = bestMatch.sourceSentenceUnitNumbers();
      LgsVector(int)::iterator ssuNumber = sourceSentenceUnitNumbers.begin();

      // find the first SSU number in the SSU vector
      SourceUnitIterator sourceUnit = sourceUnits->begin();
      for (int i = 0; i < *ssuNumber; i++)
		{
			sourceUnit++;
		}

      for (constituent = properNameConstituents.begin(); ssuNumber != sourceSentenceUnitNumbers.end(), constituent != properNameConstituents.end(); ssuNumber++, constituent++)
      {
         if (*constituent == "Title") 
         {
            (*sourceUnit)->setWordClassCode(1);
            (*sourceUnit)->setSuperSetID(5);
            (*sourceUnit)->setSetID(91);
            (*sourceUnit)->setSubSetID(807);
            (*sourceUnit)->setIsProperName(true);
         }
         else if (*constituent == "FirstName") 
         {
            finalizeFirstName(*sourceUnit);
         }
         else if (*constituent == "Initial") 
         {
            (*sourceUnit)->setWordClassCode(1);
            (*sourceUnit)->setSuperSetID(1);
            (*sourceUnit)->setSetID(1);
            (*sourceUnit)->setSubSetID(900);
            (*sourceUnit)->setIsProperName(true);
         }
         else if (*constituent == "KnownLastName") 
         {
            finalizeLastName(*sourceUnit);
         }
         else if ((*constituent == "LastName") || (*constituent == "RestrictedLastName") ||
                  (*constituent == "FoundLastName") || (*constituent == "UnfoundLastName"))
         {
            finalizeLastName(*sourceUnit);
            // keep this last name in memory for the rest of the current document
            knownLastNames_.push_back((*sourceUnit)->surfaceExpressionAsString());
         }
         else if (*constituent == "PostPosedTitle") 
         {
            (*sourceUnit)->setWordClassCode(1);
            (*sourceUnit)->setSuperSetID(5);
            (*sourceUnit)->setSetID(91);
            (*sourceUnit)->setSubSetID(195);
            (*sourceUnit)->setIsProperName(true);
         }
         else if (*constituent == "RomanNumeral") 
         {
            (*sourceUnit)->setWordClassCode(16);
            (*sourceUnit)->setSuperSetID(4);
            (*sourceUnit)->setSetID(92);
            (*sourceUnit)->setSubSetID(92);
            (*sourceUnit)->setIsProperName(true);
         }
         sourceUnit++;
      }

      // write diagnostic info
      TranslCommonObjects::GetDiagnostic()->writeLine(bestMatch.prettyPrint());
   }
}


// --------------------------------------------------------------------------
// Finalize this ssu as a first name.
// --------------------------------------------------------------------------
void ProperNameRecognizer::finalizeFirstName(SourceSentenceUnit* sourceUnit)
{
	LgsString surfaceForm = sourceUnit->surfaceExpressionAsString();

   // - a recognized first name should not be translated unless its transfer is already
   //   defined in the dictionary
   if (isFirstName(surfaceForm,
						 sourceUnit->wordClassCode(), 
						 sourceUnit->superSetID(), 
						 sourceUnit->setID(),
                   sourceUnit->subSetID()))
   {
      sourceUnit->setKeepSourceExpression(false);			// do not use transfer in generate phase
   }
   else
   {
      sourceUnit->setKeepSourceExpression(true);			// use transfer
   }

	// - keep original values of this SSU for later sentences in the same document
	changedNames_.append(sourceUnit);

   // - set the SSU SAL code as a first name
   sourceUnit->setWordClassCode(1);
   sourceUnit->setSuperSetID(5);
   sourceUnit->setSetID(5);
   sourceUnit->setSubSetID(207);
	// set the form code
	if (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::EnglishID)
	{
		if (isEndedByApostrophe(surfaceForm))
		{
			sourceUnit->setFormCode(4);
		}
		else if (sourceUnit->isUnfoundWord())
		{
			sourceUnit->setFormCode(33);
		}
		else
		{
			sourceUnit->setFormCode(1);
		}
	}
	else if (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::GermanID)
	{
		if (sourceUnit->isUnfoundWord())
		{
			sourceUnit->setFormCode(33);
		}
		else
		{
			sourceUnit->setFormCode(9);
		}
	}
   
   sourceUnit->setIsProperName(true);
}


// --------------------------------------------------------------------------
// Finalize this ssu as a last name.
// NOTE: CHANGE 207 with 827 WHEN RES+TRAN RULES CAN HANDLE SUBSET 827
// --------------------------------------------------------------------------
void ProperNameRecognizer::finalizeLastName(SourceSentenceUnit* sourceUnit)
{
	LgsString surfaceForm = sourceUnit->surfaceExpressionAsString();

   // - a recognized last name should not be translated unless its transfer is already
   //   defined in the dictionary
   if (isLastName(surfaceForm,sourceUnit->wordClassCode(),sourceUnit->superSetID(),sourceUnit->setID(),sourceUnit->subSetID(),sourceUnit->formCode()))
   {
      sourceUnit->setKeepSourceExpression(false);			// use transfer in generate phase
   }
   else
   {
      sourceUnit->setKeepSourceExpression(true);			// do not use transfer
   }

	if (isKnownLastName(surfaceForm))
	{
		sourceUnit->setKeepSourceExpression(true);
	}

	// - keep original values of this SSU for later sentences in the same document
	changedNames_.append(sourceUnit);

   // - set the SSU SAL code as a last name
   sourceUnit->setWordClassCode(1);
   sourceUnit->setSuperSetID(5);
   sourceUnit->setSetID(5);
   sourceUnit->setSubSetID(207);		// SHOULD BE 827 WHEN TRAN RULES CAN HANDLE 827!
	// set the form code
	if (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::EnglishID)
	{
		if (isEndedByApostrophe(surfaceForm))
		{
			sourceUnit->setFormCode(4);
		}
		else if (sourceUnit->isUnfoundWord())
		{
			sourceUnit->setFormCode(33);
		}
		else
		{
			sourceUnit->setFormCode(1);
		}
	}
	else if (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::GermanID)
	{
		if (sourceUnit->isUnfoundWord())
		{
			sourceUnit->setFormCode(33);
		}
		else
		{
			sourceUnit->setFormCode(9);
		}
	}

   sourceUnit->setIsProperName(true);
}


// --------------------------------------------------------------------------
// The goal is to restore information about the SSUs that has been recognized as
// constituent of a proper name and thus has been changed in a previous sentence.
// If the original information (superset/set/subset/form) is not restored, a 
// previously unknown name to the dictionary but recognized and now changed as a
// known name will be translated - this is not desirable.
// --------------------------------------------------------------------------
void ProperNameRecognizer::restoreOriginalInformation(SourceUnitVector* sourceUnits)
{
	SourceUnitIterator sourceUnit;

	for (sourceUnit=sourceUnits->begin(); sourceUnit!=sourceUnits->end(); sourceUnit++) 
	{
		// restore this unit if it is a member of the changed names list
		changedNames_.restore(*sourceUnit);
	}
}

