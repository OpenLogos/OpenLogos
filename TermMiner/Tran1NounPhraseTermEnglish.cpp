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
// Tran1NounPhraseTermEnglish.cpp: implementation of the Tran1NounPhraseTermEnglish class.
//
//////////////////////////////////////////////////////////////////////

#include <logos_include/logoscommon.h>
#include <TermMiner/Tran1NounPhraseTermEnglish.h>
#include <TermMiner/TermSearchSentenceUnit.h>
#include <logos_libs/linguistic/llanguage.h>
#include <logos_libs/linguistic/sourcesentenceunit.h>
#include <logos_libs/translutility/translcommonobjects.h>
#include <logos_libs/utility/stringutil.h>

// --------------------------------------------------------------------------
Tran1NounPhraseTermEnglish::Tran1NounPhraseTermEnglish(TermSearchSentenceUnit* sentenceUnit)
{
	resetNP(sentenceUnit);
	if (headIsNoun() && TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::EnglishID)
	{
		cleanBeginningOfTerm();
		if (isNounPhrase())
		{
			doGeneralSettings();
			setSourceAndTargetGenders();
			computeSourceSurfaceForm();
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
Tran1NounPhraseTermEnglish::~Tran1NounPhraseTermEnglish()
{
}

// --------------------------------------------------------------------------
// Set the genders.
// There is no source gender for English noun phrases, since the gender of the phrase is the
// gender of the head noun.
// For the transfer gender, it is set to a default masculine if the head is unfound, else it
// is the one of the head.
// --------------------------------------------------------------------------
void Tran1NounPhraseTermEnglish::setSourceAndTargetGenders()
{
	if (sentenceUnit_->headElement().sourceSentenceUnit()->isUnfoundWord())
	{
		// use default value to masculine for unfound
		// do not set the target gender if the target language is English
		if (TranslCommonObjects::GetTargetLanguage()->id() != LLanguage::EnglishID)
		{
			targetGender_ = MASC;
		}
	}
	else
	{
		// gender of unfound NP is gender of the head
		setTransferGender();		// set targetGender_
	}
}

// --------------------------------------------------------------------------
// Compute the source canonical form of the whole NP.
// This updates the field: sourceCanonicalForm_ with sourceHeadInCanonicalForm_ for the head element.
// All elements have surface form but the head noun, which canonical form is sourceHeadInCanonicalForm_.
// --------------------------------------------------------------------------
void Tran1NounPhraseTermEnglish::computeSourceCanonicalForm()
{
	sourceCanonicalForm_ = "";
    LgsString Hyphen("-");
    bool bHyphen=false;
    bool bFirstElementToOutput=true;

	if (!sentenceUnit_->elementsInSourceSequenceOrder().empty())
	{
		for (TermElementIterator element=sentenceUnit_->elementsInSourceSequenceOrder().begin(); element!=sentenceUnit_->elementsInSourceSequenceOrder().end(); element++)
		{
			if ((*element).sourceSentenceUnit() != 0) 
			{
				// the next word in the sequence
				LgsString nextWord = (*element).sourceSentenceUnit()->surfaceExpressionAsString();

				// its dictionary match as found during the Dictionary Lookup phase
				LgsString dictionaryMatch = (*element).sourceSentenceUnit()->primarySsu().word();

				// if the word is the head, then use its computed canonical form
				if ((*element).sourceSentenceUnit() == sentenceUnit_->headElement().sourceSentenceUnit())
				{
					nextWord = sourceTermInCanonicalForm_;
				}

				// if the word is not all upper case and the dictionary match is not initially capped
				// then change to lowercase
				if (!StringUtil::isAllUpperCase(nextWord) && !StringUtil::beginsUpperCase(dictionaryMatch))
				{
					StringUtil::toLower(nextWord);
				}

			    LgsString precedingSpaces = "";
                int nSpace = 0;
                if(!bFirstElementToOutput )
				{
                    nSpace = (*element).sourceSentenceUnit()->precedingSpaces();
				}
                else
				{
                     bFirstElementToOutput = false; // Reset the flag
                }
                if(bHyphen && nSpace >0)
                {
                    nSpace--; //Adjust the preceding space previous word ends with an hyphen
                }

                if (element!=sentenceUnit_->elementsInSourceSequenceOrder().begin() && element!=sentenceUnit_->elementsInSourceSequenceOrder().end())
			    {
				    LgsString strSpace(nSpace, ' ');
				    precedingSpaces = strSpace;
			    }

                if(bHyphen) 
				{
                    nextWord = Hyphen+precedingSpaces + nextWord;
				}
                else
				{
                     nextWord = precedingSpaces + nextWord;
				}
                bHyphen = false;

                if(( (*element).sourceSentenceUnit()->isHyphenated() ) ||( (*element).sourceSentenceUnit()->formCode() == 6 ) )//add a hyphen.
			    {
                     bHyphen = true;
			    }

				// add the element to the NP
				sourceCanonicalForm_ += nextWord;
			}
		}
	}
}

// --------------------------------------------------------------------------
void Tran1NounPhraseTermEnglish::computeSourceSurfaceForm()
{
	sourceSurfaceForm_ = "";
    LgsString Hyphen("-");
    bool bHyphen=false;
    bool bFirstElementToOutput = true;

	for (TermElementIterator element=sentenceUnit_->elementsInSourceSequenceOrder().begin(); element!=sentenceUnit_->elementsInSourceSequenceOrder().end(); element++)
	{
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
			LgsString precedingSpaces = "";
			int nSpace = (*element).sourceSentenceUnit()->precedingSpaces();
			if(bHyphen && nSpace >0)
			{
				nSpace--; //Adjust the preceding space previous word ends with an hyphen
			}

            if (   element!=sentenceUnit_->elementsInSourceSequenceOrder().begin() 
				&& element!=sentenceUnit_->elementsInSourceSequenceOrder().end())
			{
				LgsString strSpace(nSpace, ' ');
				precedingSpaces = strSpace;
			}
           
            
			
			// add the word to the sequence
			// is this element part of a compound
			if (   (*element).sourceSentenceUnit()->compoundUnit() 
				&& !(*element).sourceSentenceUnit()->headUnit())
			{
				// do nothing
			}
			else
			{
                 // Check weather the previous outputted word is hyphenated or not
			    if (bHyphen) 
			    {
                    precedingSpaces  = Hyphen + precedingSpaces;
			    }

                //Reset the flag.
                bHyphen = false;
                
                // Check the courrent word is hyphenated or not
                if(( (*element).sourceSentenceUnit()->isHyphenated() ) ||( (*element).sourceSentenceUnit()->formCode() == 6 ) )//add a hyphen.
			    {
				    bHyphen = true; // set the flag
			    }
                if(bFirstElementToOutput )
                {   
					//Supress the leading space for the  first element to be outputed
                     sourceSurfaceForm_ += nextWord;
                     bFirstElementToOutput = false; // Reset the flag
                }
                else sourceSurfaceForm_ +=  precedingSpaces + nextWord;
				
			}
		}
	}
}

