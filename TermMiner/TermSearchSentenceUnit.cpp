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
// TermSearchSentenceUnit.cpp: implementation of the TermSearchSentenceUnit class.
//
//////////////////////////////////////////////////////////////////////

#include <logos_include/logoscommon.h>
#include <TermMiner/TermSearchSentenceUnit.h>
#include <logos_libs/linguistic/llanguage.h>
#include <logos_libs/linguistic/lsentence.h>
#include <logos_libs/linguistic/sourcesentenceunit.h>
#include <TermMiner/TransferStatusQuery.h>
#include <logos_libs/sql/sqlexception.h>
#include <logos_libs/utility/stringutil.h>
#include <logos_libs/translutility/translcommonobjects.h>

// --------------------------------------------------------------------------
// A term search sentence unit groups information gathered from the whole process - from
// the dictionary lookup to generate. Mainly from LSentence info and from the tran1 analysis.
// Both sources of information need to be link together. Tran1 unit uses only scon pointers, but
// identifies the head and its elements, if any. These need to be linked to the corresponding
// source sentence units (SSU) and target sentence units (TSU) from the sentence info.
// The source words are from the Tran1 analysis and are needed to know whether new sworks (switch 68)
// have been added to the original sequence of scon pointers.
// --------------------------------------------------------------------------
TermSearchSentenceUnit::TermSearchSentenceUnit(Tran1Unit tran1Unit, LSentence* sentence, int numberSW68):
tran1Unit_(tran1Unit),
sentence_(sentence),
numberSW68_(numberSW68)
{
	reset();
}

// --------------------------------------------------------------------------
TermSearchSentenceUnit::~TermSearchSentenceUnit()
{
}

// --------------------------------------------------------------------------
void TermSearchSentenceUnit::reset()
{
	// initial settings
	setCorrectly_ = false;
	elementsSourceSeqOrder_.clear();
	sourceSurfaceForm_ = "";
	elementsTargetSeqOrder_.clear();
	targetSurfaceForm_ = "";

	// define the head element
	definesHeadElement();			// set headElement_ and setCorrectly_

	// define all concatenated elements (if any) only if the head is well defined
	if (setCorrectly_)
	{
		definesAllElements();
	}
}

// --------------------------------------------------------------------------
// Defines the head element from gathered information (eg, find its associated SSU and TSU).
// --------------------------------------------------------------------------
void TermSearchSentenceUnit::definesHeadElement()
{
	headElement_.reset(tran1Unit_.headWordSourceUnitNumber(),sentence_,true);
	setCorrectly_ = headElement_.isSetCorrectly();
}

// --------------------------------------------------------------------------
// Defines all elements of the term from gathered information.
// --------------------------------------------------------------------------
void TermSearchSentenceUnit::definesAllElements()
{
	definesAllElementsInTargetSequenceOrder();
	computeTargetSurfaceForm();					// for diagnostic purposes

	definesAllElementsInSourceSequenceOrder();
	computeSourceSurfaceForm();					// for diagnostic purposes
}

// --------------------------------------------------------------------------
// Same approach as in method definesHeadElement() but for all elements of the
// term. The elements are ordered according to the source sequence order.
// The approach is to use source ordering and to search for all SSU/TSU pairs
// in the same order.
// Uses the list of valid target OPADRs to construct the list of valid source OPADRs.
// Then, from each, find the corresponding source sentence unit.
// --------------------------------------------------------------------------
void TermSearchSentenceUnit::definesAllElementsInSourceSequenceOrder()
{
	// determine the valid source scons
	if (!targetScons_.empty())
	{
		sourceScons_.clear();
		int maxElements = numberSW68_ + sentence_->sourceSentenceUnits().size();
		for (LgsList(int)::iterator scon=targetScons_.begin(); scon!=targetScons_.end(); scon++)
		{
			if (*scon <= maxElements)
			{
				sourceScons_.push_back(*scon);
			}
		}
		// reorder them into source sequence order (simply ascending order)
		sourceScons_.sort();
        
        //Remove redundant scons Incident 2770
        sourceScons_.unique();
	}

	// find all source sentence units associated with the list of valid source scons
	elementsSourceSeqOrder_.clear();
	for (LgsList(int)::iterator scon=sourceScons_.begin(); scon!=sourceScons_.end(); scon++)
	{
		bool isHeadElement = (*scon == tran1Unit_.headWordSourceUnitNumber());
		TermElement element(*scon,sentence_,isHeadElement);

		if (element.isSetCorrectly() && element.hasSourceSetCorrectly())
		{                                                                   // Filter BOS units  Incident 3150
			elementsSourceSeqOrder_.push_back(element);
		}
	}
}

// --------------------------------------------------------------------------
// Same approach as in method definesHeadElement() but for all elements of the
// term. The elements are ordered according to the target sequence order.
// The approach is to use target ordering and to search for all SSU/TSU pairs
// in the same order.
// Construct the list of valid target OPADRs based on the information provided by Tran1
// (set the list of valid OPADRs for the target units - targetOPADRs_).
// --------------------------------------------------------------------------
void TermSearchSentenceUnit::definesAllElementsInTargetSequenceOrder()
{
	elementsTargetSeqOrder_.clear();						// structure to hold the elements TSU/SSU in target sequence order
	targetScons_.clear();									// list of valid target scon pointers
	TargetUnitVector alreadySelectedTargetSentenceUnits;	// maintain a list of already selected units to avoid duplicates in a term

	tran1Unit_.resetArrayIndex();			// read each opadro/sconpo pair from the end of Tran1
	while (tran1Unit_.isValidArrayIndex())
	{
		int opadr = tran1Unit_.currentOpadro();
		int scon = tran1Unit_.currentSconpo();

		// decide if this element is the head of the sequence
		bool isHeadElement = (scon == tran1Unit_.headWordSourceUnitNumber());

		// check if this scon value if present more than one time in th esequence of scons. This
		// will imply to use the opadr info in order to distinguish which TSU to match with.
		bool thereAreMultipleCopiesOfThisScon = tran1Unit_.numberInstancesOfSconValue(scon) > 1;

		// create an element corresponding to this scon (TSU and SSU)
		TermElement element(scon,thereAreMultipleCopiesOfThisScon,opadr,sentence_,isHeadElement,alreadySelectedTargetSentenceUnits);

		// if this is a valid element, then keep it, else discard
		if (element.isSetCorrectly())
		{
			elementsTargetSeqOrder_.push_back(element);
			targetScons_.push_back(scon);
		}

		tran1Unit_.nextArrayIndex();
	}
}

// --------------------------------------------------------------------------
void TermSearchSentenceUnit::report(ostream& out)
{
	out << "-------------term search sentence element-------------" << endl;
	out << "info received from Tran1" << endl;
	tran1Unit_.report(out);

	out << "source: " << sourceSurfaceForm_;
	out << " ---> head: ";
	if (headElement_.hasSourceSetCorrectly()) 
	{
		out << "*" << headElement_.sourceSentenceUnit()->surfaceExpressionAsString();
		out << "*" << headElement_.sourceSentenceUnit()->primarySsu().word() << "*";
	}
	else 
	{
		out << " ***not defined!***";
	}
	out << endl;

	out << "target: " << targetSurfaceForm_;
	out << " ---> head: ";
	if (headElement_.hasTargetSetCorrectly()) 
	{
		out << "*" << headElement_.targetSentenceUnit()->surfaceExpressionAsString();
		if (headElement_.hasSourceSetCorrectly()) 
		{
			out << "*" << headElement_.sourceSentenceUnit()->primarySsu().targetWord();
		}
		out << "*";
	}
	else
	{
		out << " ***not defined!***";
	}
	out << endl;

	reportTargetScons(out);
	reportSourceScons(out);
	reportTargetElements(out);
	reportSourceElements(out);
}

// --------------------------------------------------------------------------
void TermSearchSentenceUnit::reportSourceElements(ostream& out)
{
	out << "source elements";
	if (elementsSourceSeqOrder_.empty())
	{
		out << ": ***empty!***" << endl;
	}
	else
	{
		out << "(" << elementsSourceSeqOrder_.size() << " elements): ";
		for (TermElementIterator element = elementsSourceSeqOrder_.begin(); element!=elementsSourceSeqOrder_.end(); element++)
		{
			if (element->hasSourceSetCorrectly())
			{
				out << "*" << element->sourceSentenceUnit()->surfaceExpressionAsString();
			}
		}
		out << "*" << endl;
	}
}

// --------------------------------------------------------------------------
void TermSearchSentenceUnit::reportTargetElements(ostream& out)
{
	out << "target elements:";
	if (elementsTargetSeqOrder_.empty())
	{
		out << " ***empty!***" << endl;
	}
	else
	{
		out << "(" << elementsTargetSeqOrder_.size() << " elements): "; 
		for (TermElementIterator element = elementsTargetSeqOrder_.begin(); element!=elementsTargetSeqOrder_.end(); element++)
		{
			out << "*" << (*element).targetSentenceUnit()->surfaceExpressionAsString();
		}
		out << "*" << endl;
	}
}

// --------------------------------------------------------------------------
void TermSearchSentenceUnit::reportSourceScons(ostream& out)
{
	out << "source scons:";
	if (sourceScons_.empty())
	{
		out << " ***empty!***" << endl;
	}
	else
	{
		out << "(" << sourceScons_.size() << " elements): "; 
		for (LgsList(int)::iterator scon = sourceScons_.begin(); scon!=sourceScons_.end(); scon++)
		{
			out << "*" << *scon;
		}
		out << "*" << endl;
	}
}

// --------------------------------------------------------------------------
void TermSearchSentenceUnit::reportTargetScons(ostream& out)
{
	out << "target scons:";
	if (targetScons_.empty())
	{
		out << " ***empty!***" << endl;
	}
	else
	{
		out << "(" << targetScons_.size() << " elements): "; 
		for (LgsList(int)::iterator scon = targetScons_.begin(); scon!=targetScons_.end(); scon++)
		{
			out << "*" << *scon;
		}
		out << "*" << endl;
	}
}

// --------------------------------------------------------------------------
// Return whether this unit is a noun phrase that has been generated by Tran1.
// A NP is formed of more than one word and the form field of the head noun must
// have one of the following values.
// Note: sourceElements_ should be populated (ie, method findAssociatedSourceSentenceUnits()
// must have been called prior to this method for this one to be accurate).
// --------------------------------------------------------------------------
bool TermSearchSentenceUnit::isTran1NounPhrase()
{
	bool result = false;		// by default, this unit is not a noun phrase

	if (   tran1Unit_.headWordPOS() == LLanguage::NOUN	// the head word must be a noun
		 && elementsSourceSeqOrder_.size() > 1)							// and there must be more than one elements in this unit
	{
		// case of English as the source language
		if (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::EnglishID)
		{
			// check if the head has the right form field value
			result =    tran1Unit_.headWordForm() == 1
						|| tran1Unit_.headWordForm() == 2
						|| tran1Unit_.headWordForm() == 3
						|| tran1Unit_.headWordForm() == 5
						|| tran1Unit_.headWordForm() == 6
						|| tran1Unit_.headWordForm() == 8
						|| tran1Unit_.headWordForm() == 9
						|| tran1Unit_.headWordForm() == 10
						|| (tran1Unit_.headWordForm() >= 12 && tran1Unit_.headWordForm() <= 22)
						|| tran1Unit_.headWordForm() == 25
						|| tran1Unit_.headWordForm() == 26
						|| tran1Unit_.headWordForm() == 27
						|| (tran1Unit_.headWordForm() >= 29 && tran1Unit_.headWordForm() <= 34)
						|| (tran1Unit_.headWordForm() >= 37 && tran1Unit_.headWordForm() <= 41)
						|| tran1Unit_.headWordForm() == 57
						|| tran1Unit_.headWordForm() == 58
						|| tran1Unit_.headWordForm() == 66
						|| tran1Unit_.headWordForm() == 68
						|| tran1Unit_.headWordForm() == 71
						|| tran1Unit_.headWordForm() == 72
						|| tran1Unit_.headWordForm() == 74
						|| tran1Unit_.headWordForm() == 76
						|| tran1Unit_.headWordForm() == 78
						|| tran1Unit_.headWordForm() == 83
						|| tran1Unit_.headWordForm() == 96
						|| tran1Unit_.headWordForm() == 98
						;
		}

		// case of German as the source language
		else if (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::GermanID)
		{
			// check if scon1 of the head has the right value
			result = tran1Unit_.scon1() == 1;
		}
	}

	return result;
}

// --------------------------------------------------------------------------
// Return whether this term is an unfound noun.
// --------------------------------------------------------------------------
bool TermSearchSentenceUnit::isUnfoundNoun()
{
	bool result = false;

	if (headElement_.hasSourceSetCorrectly())
	{
		result =    headElement_.sourceSentenceUnit()->wordClassCode() == LLanguage::NOUN 
		         && headElement_.sourceSentenceUnit()->isUnfoundWord();
	}

	return result;
}

// -------------------------------------------------------------------
// Return whether any element of this term is a protected word.
// -------------------------------------------------------------------
bool TermSearchSentenceUnit::isProtected()
{
	bool result = false;
	TermElementIterator element = elementsSourceSeqOrder_.begin();

	while (!result && element!=elementsSourceSeqOrder_.end())
	{
		if ((*element).sourceSentenceUnit() != 0)
		{
			if (   (*element).sourceSentenceUnit()->isProtectedWord()
			    || (*element).sourceSentenceUnit()->isProtectedFromTranslation())
			{
				result = true;
			}
		}
		element++;
	}

	return result;
}

// --------------------------------------------------------------------------
// Compute the position (0 based index) of the first character of the first 
// occurrence of the first word of the term.
/// --------------------------------------------------------------------------
int TermSearchSentenceUnit::sourceLocationInSentence()
{
	int position = 0;
    	bool bSingleQuoted = false;
	bool bDoubleQuoted = false;
	bool bFrenchSingleQuoted = false;
	bool bFrenchDoubleQuoted = false;
    bool bCompoundWithoutHead = false;
    bool bFirstElementToOutput = true;

	// Check if the target is French
	bool bFrenchTarget = (TranslCommonObjects::GetTargetLanguage()->id()==LLanguage::FrenchID);
    //check for bos units!
    SourceSentenceUnit *pSourceUnitFirst =0;
    if(!elementsSourceSeqOrder_.empty())
    {
	    
        pSourceUnitFirst = elementsSourceSeqOrder_.begin()->sourceSentenceUnit();

        #ifdef _DEBUG
            LgsString SourceStr =  pSourceUnitFirst->surfaceExpressionAsString();
        #endif

        if (pSourceUnitFirst->surfaceExpressionAsString() == "bos" ) //bos unit!
        {
            pSourceUnitFirst = ( elementsSourceSeqOrder_.begin() +1)->sourceSentenceUnit();
        } 
         
        #ifdef _DEBUG
            SourceStr =  pSourceUnitFirst->surfaceExpressionAsString();
        #endif
     }   

	for (SourceUnitIterator ssu=sentence_->sourceSentenceUnits().begin(); ssu!=sentence_->sourceSentenceUnits().end(); ++ssu) 
	{
		
        //Incident 3150 German Source German Compund without a head is repeated
        if ( bCompoundWithoutHead && (*ssu)->compoundUnit() && !(*ssu)->headUnit())
		{
            continue;
        }
        bCompoundWithoutHead = false;
        //End Incident 3150 German Source German Compund without a head is repeated

        LgsString sourceWord = (*ssu)->surfaceExpressionAsString();
        
		LWordMarkup* pWordMarkup = (*ssu)->wordMarkup();

		if (sourceWord !="bos")
		{
			 // check for closing quote.
            if (bSingleQuoted && (!pWordMarkup->isSingleQuoted()) ) //add a closing  single quote
            {
               position++;
               bSingleQuoted = false;
            }
            if (bDoubleQuoted && (!pWordMarkup->isDoubleQuoted()) ) //add a closing  Double quote
            {
               position++;
               bDoubleQuoted = false;
            }

            if (bFrenchTarget)
	        {
               if (bFrenchSingleQuoted && (!pWordMarkup->isFrenchSingleQuoted()) ) //add a closing  single quote
               {
                  position++;
                  bFrenchSingleQuoted = false;
               }
 
               if (bFrenchDoubleQuoted && (!pWordMarkup->isFrenchDoubleQuoted()) ) //add a closing  Double quote
               {
                  position++;
                  bFrenchDoubleQuoted = false;
               }

            }// End if(bFrenchTarget) block

           //position+=(*ssu)->precedingSpaces();
           //Incident 3150 German Source German Compund without a head is repeated
    
            if (   TranslCommonObjects::GetSourceLanguage()->id()==LLanguage::GermanID)
            {  
                // check if have to add a period(inc#3226,3275)
                if( ((*ssu)->formCode() == 9) && ((*ssu)->superSetID() == 13) && ((*ssu)->subSetID() == 455) )
                {
                    position++;
                }

                if( (*ssu)->compoundUnit()   && (!(*ssu)->headUnit()) )
			    {
				    int nSize = sourceWord.size();
                    if( ( nSize >0) && (sourceWord[nSize-1] == '-') )
                    {
                        bCompoundWithoutHead = true;
                        if(bFirstElementToOutput )
                        {   //Supress the leading space for the  first element to be outputed
                            bFirstElementToOutput = false;
                        }
                        else position+=(*ssu)->precedingSpaces();

                     }
                     else // German compound with a head.
                     {
                        if (elementsSourceSeqOrder_.empty() && (*ssu == headElement_.sourceSentenceUnit()) )
	                    {
		                    position+=(*ssu)->precedingSpaces();
                            return position; 
	                    }
                        else if (*ssu == pSourceUnitFirst ) 
			            {    
				            position+=(*ssu)->precedingSpaces();
                            return position;
		                }
                        else // No further processing is neccessary 
                            continue;
                    }
			    }
			    else
			    {
				    if(bFirstElementToOutput )
                    {   //Supress the leading space for the  first element to be outputed
                     
                      bFirstElementToOutput = false;
                    }
                    else position+=(*ssu)->precedingSpaces();
                }
          }
          else  // Non German.
            position+=(*ssu)->precedingSpaces();

            // end Incident 3150 German Source German Compund without a head is repeated

                        
            if ( (!bSingleQuoted) && pWordMarkup->isSingleQuoted() ) //add a opening  single quote
            {
               position++;
               bSingleQuoted = true;
            }

                 
            if ( (!bDoubleQuoted) && pWordMarkup->isDoubleQuoted() ) //add a opening  Double quote
            {
                position++;
                bDoubleQuoted = true;
            }

            if (bFrenchTarget)
	        {
				if ( (!bFrenchSingleQuoted) && pWordMarkup->isFrenchSingleQuoted() ) //add a opening  single quote
               {
                  position++;
                  bFrenchSingleQuoted = true;
               }

               if ( (!bFrenchDoubleQuoted) && pWordMarkup->isFrenchDoubleQuoted() ) //add a opening  Double quote
               {
                  position++;
                  bFrenchDoubleQuoted = true;
               }
            }// End if(bFrenchTarget) block

            // The quotes are added. Check if the current ssu is the term.
            if (elementsSourceSeqOrder_.empty() && (*ssu == headElement_.sourceSentenceUnit()) )
	        {
		       return position; 
	        }
            else if (*ssu == pSourceUnitFirst ) 
			{
				return position;
			}
			else
			{
				
                
                position += sourceWord.size();
			}
            
            //check weather the word is hyphenated.
            if ((*ssu)->isHyphenated() ) //add a hyphen.
            {
               position++;
            }
		}//end if (SentenceUnitSourceForm !="bos") block
	}//Endfor (for (ssu=sourceUnits->begin(); ssu!=sourceUnits->end(); ++ssu) block.

	return position;
}

// -------------------------------------------------------------------
// Query the database to obtain the transfer status.
// The query uses a combination of keys that uniquely identify a record.
// -------------------------------------------------------------------
int TermSearchSentenceUnit::transferStatus()
{
	int targetStatus = 0;

	SqlConnection* databaseConnection = TranslCommonObjects::GetSqlConnection();
	TransferStatusQuery databaseQuery;

	try 
	{
		// establish database connection
		databaseQuery.open(databaseConnection,headElement_.targetSentenceUnit()->language());
	}
	catch (SqlException& x) 
	{
		// catch any connection problems
		throw(x);
	}
	// query the database
	databaseQuery.execute(headElement_.sourceSentenceUnit()->companyCode(),headElement_.sourceSentenceUnit()->meaningID());
	// extract info from query result
	int ts;
	while (databaseQuery.fetch(ts))
	{
		if (ts != 0)
		{
			targetStatus = ts;
		}
	}
	// clean up
	databaseQuery.close();

	return targetStatus;
}

// -------------------------------------------------------------------
// Whether this unit is a German compound that has been decomposed by Lookup
// and grouped together in Tran1 under the compound head N or ADJ.
// -------------------------------------------------------------------
bool TermSearchSentenceUnit::isTran1GermanCompound()
{
	bool isGermanCompound = false;

	if (    (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::GermanID)		// the source language must be German
		 && (headElement_.sourceSentenceUnit()->compoundUnit())		// it is a German compound
		 && (   tran1Unit_.headWordPOS() == LLanguage::NOUN			// the head word must be a N or an ADJ
		     || tran1Unit_.headWordPOS() == LLanguage::ADJECTIVE)
		 && elementsSourceSeqOrder_.size() > 1						// and there must be more than one elements in this unit
		)
	{
		// check that all elements have the same surface form
		bool allElementsHaveSameSurfaceForm = true;
		TermElementIterator element = elementsSourceSeqOrder_.begin();
		LgsString surfaceFrom = element->sourceSentenceUnit()->surfaceExpressionAsString();
		element++;
		while (allElementsHaveSameSurfaceForm && element!=elementsSourceSeqOrder_.end())
		{
			if (element->sourceSentenceUnit()->surfaceExpressionAsString() != surfaceFrom)
			{
				allElementsHaveSameSurfaceForm = false;
			}
			element++;
		}

		if (allElementsHaveSameSurfaceForm)
		{
			isGermanCompound = true;
		}
	}

	return isGermanCompound;
}

// -------------------------------------------------------------------
// Whether this unit is an element that has been inserted by Tran1 (switch 68)
// -------------------------------------------------------------------
bool TermSearchSentenceUnit::isTran1InsertedElement()
{
	return tran1Unit_.headWordSurfaceForm() == "***SWITCH68***";
}

// --------------------------------------------------------------------------
void TermSearchSentenceUnit::computeSourceSurfaceForm()
{
	sourceSurfaceForm_ = "";

	if (elementsSourceSeqOrder_.empty())
	{
		sourceSurfaceForm_ = headElement_.sourceSentenceUnit()->surfaceExpressionAsString();
	}
	else
	{
		for (TermElementIterator element = elementsSourceSeqOrder_.begin(); element!=elementsSourceSeqOrder_.end(); element++)
		{
			// the next word in the sequence
            if((*element).sourceSentenceUnit() == 0) continue;
			LgsString nextWord = (*element).sourceSentenceUnit()->surfaceExpressionAsString();

			// its dictionary match as found during the Dictionary Lookup phase
			LgsString dictionaryMatch = (*element).sourceSentenceUnit()->primarySsu().word();

			// compute the space between this word and the previous one
			LgsString precedingSpaces = "";
			int nSpace = (*element).sourceSentenceUnit()->precedingSpaces();
			if (element!=elementsSourceSeqOrder_.begin() && element!=elementsSourceSeqOrder_.end())
			{
				/*if ((nSpace < 1) && (!((*element).sourceSentenceUnit()->surfaceExpressionAsString() == "-")))
				{
					nSpace = 1;
				}*/
				LgsString strSpace(nSpace, ' ');
				precedingSpaces = strSpace;
			}

			// if the word is not all upper case and the dictionary match is not initially capped
			// then change to lowercase
			if (!StringUtil::isAllUpperCase(nextWord) && !StringUtil::beginsUpperCase(dictionaryMatch))
			{
				StringUtil::toLower(nextWord);
			}

			// add the word to the sequence
			if (   (TranslCommonObjects::GetSourceLanguage()->id()==LLanguage::GermanID) 
				&& (*element).sourceSentenceUnit()->compoundUnit())
			{
				// case of a german compound - include only once (eg, if it is the head of the compound)
				if ((*element).sourceSentenceUnit()->headUnit())
				{
					sourceSurfaceForm_ += precedingSpaces + nextWord;
				}
			}
			else
			{
				// any other case
				sourceSurfaceForm_ += precedingSpaces + nextWord;
			}
		}
	}
}

// --------------------------------------------------------------------------
// Return the whole term in target surface form.
// Use the canonical form for the head.
// --------------------------------------------------------------------------
void TermSearchSentenceUnit::computeTargetSurfaceForm()
{
	targetSurfaceForm_ = "";

	if (elementsTargetSeqOrder_.empty())
	{
		// there are no elements associated to the head, therefore the term is a single word
		targetSurfaceForm_ = headElement_.sourceSentenceUnit()->primarySsu().targetWord();
	}
	else
	{
		for (TermElementIterator element = elementsTargetSeqOrder_.begin(); element!=elementsTargetSeqOrder_.end(); element++)
		{
			// the next word in the sequence
			LgsString nextWord = "";
			if (   element->targetSentenceUnit() == headElement_.targetSentenceUnit()
				&& headElement_.hasSourceSetCorrectly())
			{
				// this is the head element - use the form from the dictionary match
                
				nextWord = headElement_.sourceSentenceUnit()->primarySsu().targetWord();
			}
			else
			{
				// this is not the head - use the surface form
				nextWord = element->targetSentenceUnit()->surfaceExpressionAsString();
			}

            // Use the trailing spaces instead of preceding spaces like generate.
            int nSpace = element->targetSentenceUnit()->trailingSpaces();
            LgsString TrailingSpaces(nSpace,' ');


			// if the word is not all upper case then change to lowercase
			if (!StringUtil::isAllUpperCase(nextWord))
			{
				StringUtil::toLower(nextWord);
			}

			// add the word to the sequence
			targetSurfaceForm_ += nextWord;
            targetSurfaceForm_ += TrailingSpaces;
		}
	}
}

// --------------------------------------------------------------------------
LgsString TermSearchSentenceUnit::context()
{
	return sentence_->sourceSentenceUnits().generateSentenceSurfaceForm();
}

// --------------------------------------------------------------------------
LgsString TermSearchSentenceUnit::sourceSconsAsString()
{
	LgsString s = "[SRC_SEQ";
	for (LgsList(int)::iterator src=sourceScons_.begin(); src!=sourceScons_.end(); src++)
	{
		s += " " + StringUtil::asStringFromInt(*src);
	}
	s += "]";
	return s;
}

// --------------------------------------------------------------------------
LgsString TermSearchSentenceUnit::targetSconsAsString()
{
	LgsString s = "[TGT_SEQ";
	for (LgsList(int)::iterator tgt=targetScons_.begin(); tgt!=targetScons_.end(); tgt++)
	{
		s += " " + StringUtil::asStringFromInt(*tgt);
	}
	s += "]";
	return s;
}
