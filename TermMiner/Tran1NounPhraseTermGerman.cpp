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
// Tran1NounPhraseTermGerman.cpp: implementation of the Tran1NounPhraseTermGerman class.
//
//////////////////////////////////////////////////////////////////////

#include <logos_include/logoscommon.h>
#include <TermMiner/Tran1NounPhraseTermGerman.h>
#include <TermMiner/TermSearchSentenceUnit.h>
#include <TermMiner/GermanSourceCompoundWord.h>
#include <logos_libs/linguistic/llanguage.h>
#include <logos_libs/linguistic/sourcesentenceunit.h>
#include <logos_libs/linguistic/targetsentenceunit.h>
#include <logos_libs/translutility/translcommonobjects.h>

// --------------------------------------------------------------------------
Tran1NounPhraseTermGerman::Tran1NounPhraseTermGerman(TermSearchSentenceUnit* sentenceUnit)
{
	resetNP(sentenceUnit);
	if (headIsNoun() && TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::GermanID)
	{
		cleanBeginningOfTerm();
		if (isNounPhrase())
		{
			doGeneralSettings();
			setSourceAndTargetGenders();
			computeSourceSurfaceForm_GermanSource();
			computeSourceCanonicalForm();
			setCorrectly_ = !hasMissingFields();
		}
		else
		{
			setAsNotANounPhrase();
		}
	}
	else
	{
		setCorrectly_ = false;
	}
}

// --------------------------------------------------------------------------
Tran1NounPhraseTermGerman::~Tran1NounPhraseTermGerman()
{
}

// --------------------------------------------------------------------------
// Set the genders.
// If the head noun is an unfound word, then the source and target gender are set to
// masculine by default.
// --------------------------------------------------------------------------
void Tran1NounPhraseTermGerman::setSourceAndTargetGenders()
{
	if (sentenceUnit_->headElement().sourceSentenceUnit()->isUnfoundWord())
	{
		// use default value to masculine for unfound
		sourceGender_ = MASC;

		// do not set the target gender if the target language is English
		if (TranslCommonObjects::GetTargetLanguage()->id() != LLanguage::EnglishID)
		{
			targetGender_ = MASC;
		}
	}
	else
	{
		// gender of unfound NP is gender of the head
		// do not set the source gender if the source language is English
		sourceGender_ = sentenceUnit_->headElement().sourceSentenceUnit()->primarySsu().genderCode();
		setTransferGender();		// set targetGender_
	}
}

// --------------------------------------------------------------------------
// Compute the source canonical form of the whole NP for German as the source language.
// This updates the field: sourceCanonicalForm_ with sourceHeadInCanonicalForm_ for the head element.
// All elements have surface form but the head noun, which canonical form is sourceHeadInCanonicalForm_.
// However, if an element is an adjective, it has to agree with the head noun.
// --------------------------------------------------------------------------
void Tran1NounPhraseTermGerman::computeSourceCanonicalForm() {
  sourceCanonicalForm_ = "";

  if (!sentenceUnit_->elementsInSourceSequenceOrder().empty())
    {
      GermanSourceCompoundWord compoundWord;
      LgsString currentCompoundSurfaceForm = "";
      LgsString nextWord = "";
      bool bHyphen = false;
      LgsString Hyphen("-");
      LgsString Period(".");
      for (TermElementIterator element=sentenceUnit_->elementsInSourceSequenceOrder().begin()
             ; element!=sentenceUnit_->elementsInSourceSequenceOrder().end()
             ; element++)
        {
          if (element->sourceSentenceUnit() != 0) 
            {
              // is this element part of a german compound? if so, is it the first element?
              if (element->isGermanCompound())
                {
                  // This element is part of a German compound.
                  // We need to collect all elements before processing the compound.
                  // Is this element part of the current compound or a new compound?
                  if (element->sourceSentenceUnit()->surfaceExpressionAsString() != currentCompoundSurfaceForm)
                    {
                      // this is a different compound - need to process the previous one (if any)
                      if (compoundWord.hasElements())
                        {
                          LgsString preceedingSpaces = compoundWord.precedingSpaces();
                          if( bHyphen && !preceedingSpaces.empty() )
                            {
                              preceedingSpaces.erase(0, 1);
                            }
                          bHyphen = false;
                          sourceCanonicalForm_ += preceedingSpaces + germanCompoundCanonicalForm(compoundWord);
                        }

                      // reset for new compound
                      currentCompoundSurfaceForm = element->sourceSentenceUnit()->surfaceExpressionAsString();
                      compoundWord.reset();
                    }
                  compoundWord.push_back(element->sourceSentenceUnit());
                }
              else
                {
                  // if a German compound was being defined, then we got all its elements at this point.
                  // It now needs to be defined, before processing the current element (which is not 
                  // part of the compound).
                  if (compoundWord.hasElements())
                    {
                      // Process the German compound that has been defined from the previous
                      // source sentence units
                      LgsString preceedingSpaces = compoundWord.precedingSpaces();
                      if( bHyphen && !preceedingSpaces.empty() )
                        {
                          preceedingSpaces.erase(0, 1);
                        }
                      bHyphen = false;
                      sourceCanonicalForm_ += preceedingSpaces + germanCompoundCanonicalForm(compoundWord);
                      compoundWord.reset();
                    }

                  // now, process the current element (not part of the German compound)
                  LgsString preceedingSpaces = computePreceedingSpaces(element);
                  nextWord = computeFormOfDictionaryMatch(*element);
                  nextWord = checkCase(nextWord);
                  if( bHyphen && !preceedingSpaces.empty() )
                    {
                      preceedingSpaces.erase(0, 1);
                    }
                  bHyphen = false;
                  // add this word to the sequence
                  sourceCanonicalForm_ += preceedingSpaces + nextWord;

                  // check if have to add a period or a hyphen (inc#3226,3275)
                  if( (element->sourceSentenceUnit()->formCode() == 9) && (element->sourceSentenceUnit()->superSetID() == 13) )
                    {
                      int iSubSet = element->sourceSentenceUnit()->subSetID();
                      if( iSubSet == 455 )
                        {
                          sourceCanonicalForm_ += Period; // add a period right now
                        }
                      else if( (iSubSet == 123) || (iSubSet == 222) ||
                               (iSubSet == 234) || (iSubSet == 567) || (iSubSet == 789) )
                        {
                          sourceCanonicalForm_ += Hyphen; // add a hyphen right now
                          bHyphen = true; // and remove a space later
                        }
                    }
                }
            }
        }

      // process the compound word if there was a compound as the last word of the sequence
      if (compoundWord.hasElements())
        {
          // Process the German compound that has been defined from the previous
          // source sentence units
          LgsString preceedingSpaces = compoundWord.precedingSpaces();
          if( bHyphen && !preceedingSpaces.empty() )
            {
              preceedingSpaces.erase(0, 1);
            }
          bHyphen = false;
          sourceCanonicalForm_ += preceedingSpaces + germanCompoundCanonicalForm(compoundWord);
        }

      cleanSourceCanonicelForm();
    }
}

// --------------------------------------------------------------------------
void Tran1NounPhraseTermGerman::cleanSourceCanonicelForm()
{
	// remove any undesirable leading spaces
	int x = sourceCanonicalForm_.find_first_of(" ");
	while (x<string::npos && x==0)
	{
		sourceCanonicalForm_ = sourceCanonicalForm_.substr(1,sourceCanonicalForm_.length());
		x = sourceCanonicalForm_.find_first_of(" ");
	}
}

// --------------------------------------------------------------------------
LgsString Tran1NounPhraseTermGerman::germanCompoundCanonicalForm(GermanSourceCompoundWord& compoundWord)
{
	LgsString canonicalForm = "";
	LgsString compoundHeadInCanonicalForm = "";	// not defined by default
	int genderNounPhraseHeadNoun = 0;			// no gender defined by default

	if (compoundWord.headElement() != 0)
	{
		compoundHeadInCanonicalForm = filterCanonicalForm(compoundWord.headElement()->word(),compoundWord.headElement());
		genderNounPhraseHeadNoun = sentenceUnit_->headElement().sourceSentenceUnit()->genderCode();
	}
	canonicalForm = compoundWord.canonicalForm(compoundHeadInCanonicalForm,genderNounPhraseHeadNoun);

	return canonicalForm;
}

// --------------------------------------------------------------------------
// Compute the form of the next word in the sequence.
// Use the dictionary match by default (canonical form). If no changes are required,
// then use the surface form.
// --------------------------------------------------------------------------
LgsString Tran1NounPhraseTermGerman::computeFormOfDictionaryMatch(TermElement &element) {
  LgsString word = "";
  LgsString surfaceForm = element.sourceSentenceUnit()->surfaceExpressionAsString();
  LgsString dictionaryMatch = element.sourceSentenceUnit()->primarySsu().word();
  const SconTable& sconTable = element.targetSentenceUnit()->getSconTable();

  // if the word is the head, then use its computed canonical form
  if (element.sourceSentenceUnit() == sentenceUnit_->headElement().sourceSentenceUnit())
    {
      word = sourceTermInCanonicalForm_;
    }
  // for the non head elements, check if the element is an adjective that needs to agree
  // with the head noun
  else if (   element.sourceSentenceUnit()->primarySsu().wordClassCode() == LLanguage::ADJECTIVE
              && element.sourceSentenceUnit()->primarySsu().formCode() != 9
              && !(sconTable.scon45()==5 && sconTable.getScon(1)!=455)	// see specs in incident 3191
              )
    {
      // this is an adjective that needs to agree with the noun of the phrase
      word = agreementADJtoN(element);
    }
  else
    {
      // do not use the canonical form if the word surface expression terminates by an hyphen
      if (surfaceForm.substr(surfaceForm.length()-1,1) == "-")
        {
          word = surfaceForm;
        }
      // use the canonical form given by the dictionary match
      else
        {
          word = dictionaryMatch;
        }
    }

  return word;
}

// --------------------------------------------------------------------------
// Return the form of the given adjective that agrees with the noun of the phrase.
// --------------------------------------------------------------------------
LgsString Tran1NounPhraseTermGerman::agreementADJtoN(TermElement &adjective) {
  LgsString adjectiveDictForm = adjective.sourceSentenceUnit()->primarySsu().word();
  LgsString adjectiveFormInAgreementWithNoun = adjectiveDictForm;		// default value (no changes)
  LgsString adjectiveDictFormLastCharacter = "";
  if (adjectiveDictForm.length() >= 1)
    {
      adjectiveDictFormLastCharacter = adjectiveDictForm.substr(adjectiveDictForm.length()-1,1);
    }
  LgsString adjectiveDictFormLast2Characters = "";
  if (adjectiveDictForm.length() >= 2)
    {
      adjectiveDictFormLast2Characters = adjectiveDictForm.substr(adjectiveDictForm.length()-2,2);
    }
  int genderHeadNoun = sentenceUnit_->headElement().sourceSentenceUnit()->genderCode();
  int adjectivePat = adjective.sourceSentenceUnit()->primarySsu().patNumber();
  int adjectiveStem = adjective.sourceSentenceUnit()->primarySsu().sourceStemNumber();

  // if the adjective if of PAT 486 (this defines non-inflecting adjectives)
  if (adjectivePat == 486)
    {
      // render the adjective as it is (ie, as given by dictionary match from lookup)
      adjectiveFormInAgreementWithNoun = adjectiveDictForm;
    }
  // if the adjective stem is not 1 and the adjective does end by any of (e, em, en, er, es)
  else if (adjectiveStem!=1 && adjectiveStem != 2 && adjectiveDictFormLastCharacter=="e")
    {
      LgsString adjectiveDictFormLessLastCharacter = adjectiveDictForm.substr(0,adjectiveDictForm.length()-1);
      adjectiveFormInAgreementWithNoun = adjectiveFinalForm(adjectiveDictFormLessLastCharacter,genderHeadNoun);
    }
  else if (   adjectiveStem != 1
              && adjectiveStem != 2
              && (   adjectiveDictFormLast2Characters == "em"
                     || adjectiveDictFormLast2Characters == "en"
                     || adjectiveDictFormLast2Characters == "er"
                     || adjectiveDictFormLast2Characters == "es"
                     ))
    {
      LgsString adjectiveDictFormLessLast2Characters = adjectiveDictForm.substr(0,adjectiveDictForm.length()-2);
      adjectiveFormInAgreementWithNoun = adjectiveFinalForm(adjectiveDictFormLessLast2Characters,genderHeadNoun);
    }
  // if the adjective ends with the character 'e'
  else if (adjectiveDictFormLastCharacter == "e")
    {
      // if the gender of the noun is masculine
      if (genderHeadNoun == MASC)
        {
          // then append 'r' to the canonical form of the adjective
          adjectiveFormInAgreementWithNoun = adjectiveDictForm + "r";
        }
      // else if the gender of the noun is neuter
      else if (genderHeadNoun == NEUT)
        {
          // then append 's' to the canonical form of the adjective
          adjectiveFormInAgreementWithNoun = adjectiveDictForm + "s";
        }
    }
  else
    {
      adjectiveFormInAgreementWithNoun = adjectiveFinalForm(adjectiveDictForm,genderHeadNoun);
    }

  return adjectiveFormInAgreementWithNoun;
}

// --------------------------------------------------------------------------
LgsString Tran1NounPhraseTermGerman::adjectiveFinalForm(LgsString adjectiveRootForm, int genderHeadNoun)
{
	LgsString adjectiveFinalForm = adjectiveRootForm;

	// if the gender of the noun is masculine
	if (genderHeadNoun == MASC)
	{
		// then append 'er' to the canonical form of the adjective
		adjectiveFinalForm = adjectiveRootForm + "er";
	}
	// else if the gender of the noun is neuter
	else if (genderHeadNoun == NEUT)
	{
		// then append 'es' to the canonical form of the adjective
		adjectiveFinalForm = adjectiveRootForm + "es";
	}
	// else the gender of the noun is feminine
	else
	{
		// then append 'e' to the canonical form of the adjective
		adjectiveFinalForm = adjectiveRootForm + "e";
	}

	return adjectiveFinalForm;
}

