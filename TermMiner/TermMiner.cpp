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
// TermMiner.cpp: implementation of the TermMiner class.
//
//////////////////////////////////////////////////////////////////////

#include <logos_include/logoscommon.h>
#include <TermMiner/TermMiner.h>
#include <TermMiner/DictionaryUnfoundTerm.h>
#include <TermMiner/DictionaryFoundTerm.h>
#include <TermMiner/Tran1NounPhraseTerm.h>
#include <TermMiner/Tran1NounPhraseTermGerman.h>
#include <TermMiner/Tran1NounPhraseTermEnglish.h>
#include <TermMiner/Tran1GermanCompoundTerm.h>
#include <logos_libs/linguistic/llanguage.h>
#include <logos_libs/linguistic/lsentence.h>
#include <logos_libs/linguistic/sourcesentenceunit.h>
#include <logos_libs/linguistic/targetsentenceunit.h>
#include <lgs_db_io/lgsdbcommonobjects.h>
#include <logos_libs/translutility/translcommonobjects.h>
#include <logos_libs/utility/stringutil.h>

// --------------------------------------------------------------------------
// Default constructor
// --------------------------------------------------------------------------
TermMiner::TermMiner() :
currentSentence_(0),
maxFound_(0),
maxUnfound_(0),
extendedSearch_(false),
reportUnfoundTerms_(false),
reportNouns_(false),
reportVerbs_(false),
reportAdjectives_(false),
reportAdverbs_(false),
reportFoundTerms_(false),
diagnostic_(TranslCommonObjects::GetDiagnostic()),
userSelectedCCs(TranslCommonObjects::GetContext()->userSelectedCompanyCodes()),
userSelectedSMCs(TranslCommonObjects::GetContext()->userSelectedSMCs())
{
	// read term search properties
	getTermSearchJobArguments();
}

// --------------------------------------------------------------------------
TermMiner::~TermMiner()
{
}

// --------------------------------------------------------------------------
void TermMiner::getTermSearchJobArguments()
{
	JobControlArguments &jobCntrlArgs = LgsDBCommonObjects::GetJobControlArguments();

	// determine the range of the occurrences to report (from user's preferences)
	minFound_ = jobCntrlArgs.WordSearchFoundStart();
	maxFound_ = jobCntrlArgs.WordSearchFoundLimit();
	foundTerms_.setMaxOccurrences(maxFound_);
	minUnfound_ = jobCntrlArgs.WordSearchUnfoundStart();
	maxUnfound_ = jobCntrlArgs.WordSearchUnfoundLimit();
	unfoundTerms_.setMaxOccurrences(maxUnfound_);
   extendedSearch_ = jobCntrlArgs.ExtendedSearch() == 1 ? true : false;

	// retrieve term search options from job control info
	int termSearchOptions = jobCntrlArgs.WordSearchOptions();
	reportUnfoundTerms_ = (termSearchOptions & 1) == 1;
	reportNouns_ = (termSearchOptions & 2) == 2;
	reportVerbs_ = (termSearchOptions & 4) == 4;
	reportAdjectives_ = (termSearchOptions & 8) == 8;
	reportAdverbs_ = (termSearchOptions & 16) == 16;
	reportFoundTerms_ = reportNouns_ || reportVerbs_ || reportAdjectives_ || reportAdverbs_;
}

// --------------------------------------------------------------------------
// Select the terms from the current sentence.
// Collect information from:
// - the end of Tran 1 -> from tran message object
// - lookup (dictionary matches), res, generate (before scons are changed to get 
//   transfer in canonical form) -> from given LSentence object
// --------------------------------------------------------------------------
void TermMiner::captureTermsIn(LSentence* sentence)
{
	currentSentence_ = sentence;

	// construct the term search sentence units - this is the set of all terms that can be
	// candidate for the term search report but that will be filtered
	constructTermSearchSentenceUnits();		// set sentenceUnits_

	// set scons for canonical form of transfer - this must be done before completing Generate process
	// (see term search in LDocument)
	setTransferForCanonicalForm();
}

// --------------------------------------------------------------------------
// Define the key that identifies uniquely the node where to store similar term objects.
// --------------------------------------------------------------------------
LgsString TermMiner::key(Term* term)
{
	LgsString theKey = term->sourceCanonicalForm() + term->formattedSourcePOS();
	StringUtil::toLower(theKey);
	return theKey;
}

// --------------------------------------------------------------------------
// Construct the elements from which term miner will extract terms for the report.
// These elements are called TermSearchSentenceUnit and are based on information
// gathered from various part of the process from lookup to generate (from the current
// LSentence being analyzed and from Tran1 analysis).
// Set sentenceUnits_.
// --------------------------------------------------------------------------
void TermMiner::constructTermSearchSentenceUnits()
{
	sentenceUnits_.clear();

	// retrieve information (units) about the source sentence analysis from the message sent by Tran1
	Tran1UnitSequence tran1Units;

	// for each unit, create a candidate term search sentence unit, given the current sentence info
	for (Tran1UnitIterator unit=tran1Units.begin(); unit!=tran1Units.end(); unit++)
	{
		if (unit->isValidForTermSearch())
		{
			TermSearchSentenceUnit sentenceUnit(*unit,currentSentence_,tran1Units.numberOfSwitch68());
			if (sentenceUnit.isSetCorrectly())
			{
				sentenceUnits_.push_back(sentenceUnit);
			}
		}
	}
}

// --------------------------------------------------------------------------
// currentSentence_ has been updated from finishing Generate process.
// Now, the task is to categorize each sentence unit into an appropriate term
// object.
// --------------------------------------------------------------------------
void TermMiner::categorizeTerms()
{
  // reset the structures holding local terms to that sentence
  dictionaryFoundTerms_.clear();
  dictionaryUnfoundTerms_.clear();
  tran1NounPhraseTerms_.clear();
  tran1GermanCompoundTerms_.clear();

  // Partition the units into Tran1 generated noun phrases/german compounds and non-Tran1 
  // generated NPs.
  // A Tran1 NP has more than 1 element.
  // The non tran1 NPs are found dictionary terms if they are N, V, Adj, or Adv 
  // and unfound dictionary terms if they are unfound.
  for (TermSearchSentenceUnitIterator sentenceUnit=sentenceUnits_.begin(); sentenceUnit!=sentenceUnits_.end(); sentenceUnit++)
    {
      // reset the unit to take into account the information from finishing Generate process
      sentenceUnit->reset();

      // classify each unit in the appropriate term (if selected for the report)
      if (keepForTermSearchReport(*sentenceUnit))
        {
          // German source terms
          if (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::GermanID)
            {
              // is it a German compound that has been decomposed?
              if (sentenceUnit->isTran1GermanCompound())
                {
                  setAsGermanCompoundTerm(*sentenceUnit);
                }
              // is this unit a NP generated by Tran1?
              else if (sentenceUnit->isTran1NounPhrase())
                {
                  setAsGermanNounPhraseTerm(*sentenceUnit);
                }
              // is it a dictionary term? (if it is a single unit - no elements)
              else if (sentenceUnit->numberSourceElements()<2 && !sentenceUnit->isTran1InsertedElement())
                {
                  setAsDictionaryTerm(*sentenceUnit);
                }
            }

          // English source terms
          else if (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::EnglishID)
            {
              // is this unit a NP generated by Tran1?
              if (sentenceUnit->isTran1NounPhrase())
                {
                  setAsEnglishNounPhraseTerm(*sentenceUnit);
                }
              // is it a dictionary term? (if it is a single unit - no elements)
              else if (sentenceUnit->numberSourceElements()<2 
                       && !sentenceUnit->isTran1InsertedElement())
                {
                  setAsDictionaryTerm(*sentenceUnit);
                }
            }
        }
    }
}

// --------------------------------------------------------------------------
// Return whether the given unit should be included in the term search report.
// This depends on its part of speech and the user selection.
// --------------------------------------------------------------------------
bool TermMiner::keepForTermSearchReport(TermSearchSentenceUnit &unit){
  bool keepIt = false;

  if (filterUndesirableTerms(unit))
    {
      // if the user has selected to report unfound terms, report only the noun phrases formed by
      // Tran1 and the dictionary unfound nouns
      if (unit.isTran1GermanCompound())
        {
          if (reportUnfoundTerms_) 
            {
              keepIt = true;
            }
          else 
            {
              keepIt = false;
            }
        }
      else if (unit.isTran1NounPhrase())
        {
          if (reportUnfoundTerms_) 
            {
              keepIt = true;
            }
          else 
            {
              keepIt = false;
            }
        }
      else if (unit.numberSourceElements()<2)
        {
          if (unit.isUnfoundNoun())
            {
              if (reportUnfoundTerms_) 
                {
                  keepIt = true;
                }
              else 
                {
                  keepIt = false;
                }
            }

          // case of a found term (not a Tran1 noun phrase nor an unfound term)
          // keep in report only if it is an appropriate POS and the user wants to report it
          else
            {
              keepIt = reportThisWordClassCode(unit.headElement().sourceSentenceUnit()->primarySsu().wordClassCode());
            }
        }

      // Now one last check, make certain the user has designated the output to be seen
      // based on the company codes and SMC's they have chosen
      if (keepIt)
        {
          keepIt = extendedSearch_ || isValidCC_SMC(unit);
        }
    }

  return keepIt;
}

// --------------------------------------------------------------------------
// Indicates whether the term is one to be reported based on the user's
// preference for company codes and SMCs
// --------------------------------------------------------------------------
bool TermMiner::isValidCC_SMC(TermSearchSentenceUnit &unit) {
  // search for the unit's company code in the list of company codes
  // selected by the user
  bool found = false;
  CompanyCodeIterator ccIter = userSelectedCCs->begin();

  while (!found && ccIter != userSelectedCCs->end())
    {
      if (unit.headElement().sourceSentenceUnit()->companyCode() == ccIter->content())
        {
          found = true;
        }
      else
        {
          ccIter++;
        }
    }

  SubjectMatterCode smc =
    unit.headElement().sourceSentenceUnit()->subjectMatterCode();
  // search for the candidate subject matter code in the selected structure
  if (found && 
      (userSelectedSMCs->nodeNumber(smc) != 0))
    {
      return true;
    }
  else
    {
      return false;
    }
}

// --------------------------------------------------------------------------
// Return whether the given word class code is one to be reported according to
// the job argument settings.
// --------------------------------------------------------------------------
bool TermMiner::reportThisWordClassCode(int wordClassCode)
{
	bool keepIt = false;		// default

	switch(wordClassCode)
	{
	case LLanguage::NOUN: 
		if (reportNouns_) 
		{
			keepIt = true;
		}
		else 
		{
			keepIt = false;
		}
		break;
	case LLanguage::VERB: 
		if (reportVerbs_) 
		{
			keepIt = true;
		}
		else 
		{
			keepIt = false;
		}
		break;			
	case LLanguage::ADJECTIVE:
		if (reportAdjectives_) 
		{
			keepIt = true;
		}
		else 
		{
			keepIt = false;
		}
		break;
	case LLanguage::ADVERB_LOCATIVE:
	case LLanguage::ADVERB:	
		if (reportAdverbs_) 
		{
			keepIt = true;
		}
		else 
		{
			keepIt = false;
		}
		break;
	default: 
		keepIt = false;
	}

	return keepIt;
}

// --------------------------------------------------------------------------
// Clean selection from undesirable valid terms.
// --------------------------------------------------------------------------
bool TermMiner::filterUndesirableTerms(TermSearchSentenceUnit &unit)
{
	bool keepIt = true;
	LgsString sourceSurfaceForm = unit.sourceSurfaceForm();

	if (sourceSurfaceForm == "^p")
	{
		keepIt = false;
	}
	else if (unit.isProtected())
	{
		keepIt = false;
	}
	else if (StringUtil::isAllNumeric(sourceSurfaceForm))
	{
		keepIt = false;
	}
	else if (isURLaddress(sourceSurfaceForm))
	{
		keepIt = false;
	}
	else if (isOnlyPunctuationAndDigits(sourceSurfaceForm))
	{
		keepIt = false;
	}

	return keepIt;
}

// --------------------------------------------------------------------------
bool TermMiner::isURLaddress(LgsString expression)
{
	bool result = false;
	if (expression.length()>4)
	{
		result = expression.substr(0,5) == "http:" || expression.substr(0,5) == "ftp:";
	}
	return result;
}

// --------------------------------------------------------------------------
bool TermMiner::isOnlyPunctuationAndDigits(LgsString expression)
{
	bool result = true;
	int i=0;
	while (result && i<expression.length())
	{
		if (   !StringUtil::containsPunctuation(expression.substr(i,1))
			&& !StringUtil::containsDigit(expression.substr(i,1)))
		{
			result = false;
		}
		i++;
	}
	return result;
}

// --------------------------------------------------------------------------
// Generate the term search report following the current sort order.
// Save the report into the specified output stream.
// Report found and unfound terms only if they have occurred a minimum number of 
// times in the document (as set by user options - job control info).
// --------------------------------------------------------------------------
void TermMiner::generateTermSearchReport(ostream& out)
{
	// report content of each list in the required format to the required output stream
	// report a term only if it has at least min occurrences
	unfoundTerms_.report(out,minUnfound_);
	foundTerms_.report(out,minFound_);

#ifdef __TERM_SEARCH_STATS__
	generateStatReport();
#endif
}

// --------------------------------------------------------------------------
// Given the sentence unit, set it as a dictionary term (either a found or unfound term).
// Return the classification result.
// --------------------------------------------------------------------------
void TermMiner::setAsDictionaryTerm(TermSearchSentenceUnit &sentenceUnit) {
  if (sentenceUnit.isUnfoundNoun())
    {
      DictionaryUnfoundTerm* unfoundTerm 
        = new DictionaryUnfoundTerm(&sentenceUnit);

#ifdef __TERM_SEARCH_STATS__
      stat_.incrementNumberDictionaryUnfoundTermObjectsCreated();
#endif

      if (unfoundTerm->setCorrectly()) 
        {
          dictionaryUnfoundTerms_.push_back(*unfoundTerm);
          unfoundTerms_.append(unfoundTerm,key(unfoundTerm),stat_);
        }

      delete unfoundTerm;
    }
  else
    {
      DictionaryFoundTerm* foundTerm = new DictionaryFoundTerm(&sentenceUnit);

#ifdef __TERM_SEARCH_STATS__
      stat_.incrementNumberDictionaryFoundTermObjectsCreated();
#endif

      if (foundTerm->setCorrectly()) 
        {
          dictionaryFoundTerms_.push_back(*foundTerm);
          foundTerms_.append(foundTerm,key(foundTerm),stat_);
        }

      delete foundTerm;
    }
}

// --------------------------------------------------------------------------
// Given the sentence unit, set it as a German compound term.
// --------------------------------------------------------------------------
void TermMiner::setAsGermanCompoundTerm(TermSearchSentenceUnit &sentenceUnit)
{
	Tran1GermanCompoundTerm* tran1GermanCompoundTerm
          = new Tran1GermanCompoundTerm(& sentenceUnit);

#ifdef __TERM_SEARCH_STATS__
	stat_.incrementNumberGermanCompoundTermObjectsCreated();
#endif

	if (tran1GermanCompoundTerm->setCorrectly()) 
	{
		tran1GermanCompoundTerms_.push_back(*tran1GermanCompoundTerm);
		unfoundTerms_.append(tran1GermanCompoundTerm,key(tran1GermanCompoundTerm),stat_);
	}

	delete tran1GermanCompoundTerm;
}

// --------------------------------------------------------------------------
// Given the sentence unit, set it as a noun phrase term.
// If the NP has had elements removed in its front (determiners, numbers, etc.)
// it might be degraded as a dictionary term. We do so only if such a term has
// to be reported (settings from job arguments).
// --------------------------------------------------------------------------
void TermMiner::setAsGermanNounPhraseTerm(TermSearchSentenceUnit &sentenceUnit)
{
	Tran1NounPhraseTerm* tran1NounPhraseTerm
          = new Tran1NounPhraseTermGerman(& sentenceUnit);

#ifdef __TERM_SEARCH_STATS__
	stat_.incrementNumberGermanNounPhraseTermObjectsCreated();
#endif

	if (tran1NounPhraseTerm->isSingleElement())
	{
		if (sentenceUnit.isTran1GermanCompound())
		{
			setAsGermanCompoundTerm(sentenceUnit);
		}
		else if (reportThisWordClassCode(sentenceUnit.headElement().sourceSentenceUnit()->primarySsu().wordClassCode()))
		{
			setAsDictionaryTerm(sentenceUnit);
		}
	}
	else if (tran1NounPhraseTerm->setCorrectly()) 
	{
		tran1NounPhraseTerms_.push_back(*tran1NounPhraseTerm);
		unfoundTerms_.append(tran1NounPhraseTerm,key(tran1NounPhraseTerm),stat_);
	}

	delete tran1NounPhraseTerm;
}
// --------------------------------------------------------------------------
void TermMiner::setAsEnglishNounPhraseTerm(TermSearchSentenceUnit &sentenceUnit)
{
	Tran1NounPhraseTerm* tran1NounPhraseTerm 
          = new Tran1NounPhraseTermEnglish(& sentenceUnit);

#ifdef __TERM_SEARCH_STATS__
	stat_.incrementNumberEnglishNounPhraseTermObjectsCreated();
#endif

	if (   tran1NounPhraseTerm->isSingleElement()
		&& reportThisWordClassCode(sentenceUnit.headElement().sourceSentenceUnit()->primarySsu().wordClassCode()))
	{
		setAsDictionaryTerm(sentenceUnit);
	}
	else if (tran1NounPhraseTerm->setCorrectly()) 
	{
		tran1NounPhraseTerms_.push_back(*tran1NounPhraseTerm);
		unfoundTerms_.append(tran1NounPhraseTerm,key(tran1NounPhraseTerm),stat_);
	}

	delete tran1NounPhraseTerm;
}

// --------------------------------------------------------------------------
// Set all the scons in appropriate units to have the transfer in canonical form.
// When this is done, Generate can be finished (see term search in LDocument).
// The units modified are the heads of NPs and all concatenated adjectives.
// The approach is the following - only for English source NPs
//   If the unit is an English source NP
//   (1) For the head: set scons 5 and 7 to 1
//   (2) For all concatenated ADJ (WC=4):
//       (a) set scons 5 and 7 to 1
//       (b) for German target, also set scon 3 to 9
// New Specs Incident 3026
//   (3) For any concatenated Romance target verbal adjective(WC02)
//        Reset scon 5 to 1
//   (4) For any article(WC14) appearing before the head of the target NP only  
//        Set scons 5 and 7 to 1.          
// --------------------------------------------------------------------------
void TermMiner::setTransferForCanonicalForm()
{
	settingsForTransferInCanonicalForm_.clear();

	for (TermSearchSentenceUnitIterator sentenceUnit=sentenceUnits_.begin(); sentenceUnit!=sentenceUnits_.end(); sentenceUnit++)
	{
		LgsString diagnosticInfo = sentenceUnit->sourceSurfaceForm();	// for diagnostic purposes

		if (sentenceUnit->isTran1NounPhrase())
		{
		    // set scon 5 of head to 1
			sentenceUnit->headElement().targetSentenceUnit()->setNumberInSconTable(1);

			// set scon 7 to 1
			sentenceUnit->headElement().targetSentenceUnit()->setCaseInSconTable(1);

			diagnosticInfo += "\t[HEAD: scon5=1 scon7=1]";

			// for all concatenated adjectives
			for (TermElementIterator element=sentenceUnit->elementsInTargetSequenceOrder().begin(); element!=sentenceUnit->elementsInTargetSequenceOrder().end(); element++)
			{
				if (element->targetSentenceUnit() != 0)
				{
					switch (element->targetSentenceUnit()->wordClassCode())
                    { 
						case LLanguage::ADJECTIVE: // Adjective
                               // set scon 5 to 1
						    element->targetSentenceUnit()->setNumberInSconTable(1);

						    // set scon 7 to 1
						    element->targetSentenceUnit()->setCaseInSconTable(1);

						    diagnosticInfo += "\t[ADJ: scon5=1 scon7=1";
                             // if target language is German, then set scon 3 to 9
						    if (TranslCommonObjects::GetTargetLanguage()->id() == LLanguage::GermanID)
						    {
							    element->targetSentenceUnit()->setDeclensionInSconTable(9);
							    diagnosticInfo += " scon3=9";
						    }
                            diagnosticInfo += "]";
                         break;

                         case LLanguage::VERB: // WC02 -verbal adjective Romance only. 
                            if( (TranslCommonObjects::GetTargetLanguage()->id() != LLanguage::GermanID)
                                &&(TranslCommonObjects::GetTargetLanguage()->id() != LLanguage::EnglishID) )
    					    {
							    diagnosticInfo += "\t[Romance-Verbal ADJ:scon5=1";
                                 // set scon 5 to 1
						        element->targetSentenceUnit()->setNumberInSconTable(1);
                                diagnosticInfo += "]";
								    
						    }
                         break;
        
                         case LLanguage::ARTICLE_DEFINITE: //WC14 - Article appearing before head only
                             if((element+1)->isHeadElement())
                             {
                                   diagnosticInfo += "\t[Article before head:scon5=1 scon7=1";
                                   // set scon 5 to 1
						          element->targetSentenceUnit()->setNumberInSconTable(1);
                                    // set scon 7 to 1
						          element->targetSentenceUnit()->setCaseInSconTable(1);
                                  diagnosticInfo += "]";
                             }     
                         break;
                                                  			
					} // End Switch
				}// End if (element->targetSentenceUnit() != 0)
			}//End for (TermElementIterator element=sentenceUnit->elementsInTargetSequenceOrder()->begin(); element!=sentenceUnit->elementsInTargetSequenceOrder()->end(); element++)
		}//End if (sentenceUnit->isTran1NounPhrase())
		else
		{
				diagnosticInfo += "\t[no settings]";
		}

		settingsForTransferInCanonicalForm_.push_back(diagnosticInfo);
	} //Endfor (TermSearchSentenceUnitIterator sentenceUnit=sentenceUnits_->begin();

}

// --------------------------------------------------------------------------
// Add term search diagnostic info collected during the whole process to the
// diagnostic file (if selected in job arguments).
// --------------------------------------------------------------------------
void TermMiner::generateDiagnostic()
{
	if (diagnostic_ != 0)
	{
		diagnostic_->writeLine("\n*** Term Search ***");

		// diagnose term search options
		diagnostic_->writeLine("\n* term search options *");

		diagnostic_->writeLine("FOR UNFOUND TERMS");
		diagnostic_->write("report unfound terms: ");
		if (reportUnfoundTerms_) 
		{
			diagnostic_->writeLine("yes");
			diagnostic_->writeLine("report each unfound term if seen at least " + StringUtil::asStringFromInt(minUnfound_) + " times");
			diagnostic_->writeLine("report no more than " + StringUtil::asStringFromInt(maxUnfound_) + " occurrences of each unfound term");
		}
		else 
		{
			diagnostic_->writeLine("no");
		}

		diagnostic_->writeLine("FOR FOUND TERMS");
		diagnostic_->writeLine("report found nouns: " + yes_no(reportNouns_));
		diagnostic_->writeLine("report found verbs: " + yes_no(reportVerbs_));
		diagnostic_->writeLine("report found adjectives: " + yes_no(reportAdjectives_));
		diagnostic_->writeLine("report found adverbs: " + yes_no(reportAdverbs_));
		if (reportFoundTerms_)
		{
			diagnostic_->writeLine("report each found term if seen at least " + StringUtil::asStringFromInt(minFound_) + " times");
			diagnostic_->writeLine("report no more than " + StringUtil::asStringFromInt(maxFound_) + " occurrences of each found term");
		}

		// generate a detailled diagnostic
		diagnoseTermSearchSentenceUnits();
		diagnoseSentenceUnitsInSequenceOrder();
		diagnoseSettingsForTransferInCanonicalForm();
		diagnoseSelectedTerms();
	}
}

// --------------------------------------------------------------------------
// Return the LgsString yes or no depending on the value of the given boolean.
// --------------------------------------------------------------------------
LgsString TermMiner::yes_no(bool isTrue)
{
	LgsString result = "no";
	if (isTrue)
	{
		result = "yes";
	}
	return result;
}

// --------------------------------------------------------------------------
// Diagnose all elements of the sentence that are considered by term search before
// any selection, etc. (i.e., basis at the beginning of the process).
// --------------------------------------------------------------------------
void TermMiner::diagnoseTermSearchSentenceUnits()
{
	diagnostic_->writeLine("\n* term search sentence units (all elements considered by term search) *");
	if (sentenceUnits_.empty())
	{
		diagnostic_->writeLine("no elements in list");
	}
	else
	{
		int elementNb = 0;
		for (TermSearchSentenceUnitIterator unit=sentenceUnits_.begin(); unit!=sentenceUnits_.end(); unit++)
		{
			elementNb++;
			diagnostic_->write(StringUtil::asStringFromInt(elementNb) + "\t" + unit->sourceSurfaceForm());
			if (unit->isTran1GermanCompound()) 
			{
				diagnostic_->write("\t(Tran1 German compound)");
			}
			if (unit->isTran1NounPhrase()) 
			{
				diagnostic_->write("\t(Tran1 NP)");
			}
			diagnostic_->writeLine("");
		}
	}
}

// --------------------------------------------------------------------------
// Diagnose all settings that were made in order to have the transfer of a term in
// canonical form.
// --------------------------------------------------------------------------
void TermMiner::diagnoseSettingsForTransferInCanonicalForm()
{
	diagnostic_->writeLine("\n* list of settings made to have the transfers in canonical forms *");
	if (settingsForTransferInCanonicalForm_.empty())
	{
		diagnostic_->writeLine("no settings were made");
	}
	else
	{
		int elementNb = 0;
		for (LgsStringIterator info=settingsForTransferInCanonicalForm_.begin(); info!=settingsForTransferInCanonicalForm_.end(); info++)
		{
			elementNb++;
			diagnostic_->writeLine(StringUtil::asStringFromInt(elementNb) + "\t" + *info);
		}
	}
}

// --------------------------------------------------------------------------
// Diagnose all terms that have been selected for the report, categorized by
// their type (found word, noun phrase, etc.).
// --------------------------------------------------------------------------
void TermMiner::diagnoseSelectedTerms()
{
	diagnostic_->writeLine("\n* all selected terms for the report by category *");
	diagnoseSelectedTerms(dictionaryFoundTerms_,"TERMS FOUND IN THE DICTIONARY:");
	diagnoseSelectedTerms(dictionaryUnfoundTerms_,"TERMS NOT FOUND IN THE DICTIONARY:");
	diagnoseSelectedTerms(tran1NounPhraseTerms_,"NOUN PHRASES GENERATED BY TRAN1:");
	if (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::GermanID)
	{
		diagnoseSelectedTerms(tran1GermanCompoundTerms_,"GERMAN COMPOUNDS GENERATED BY LOOKUP+TRAN1:");
	}
}

// --------------------------------------------------------------------------
// Diagnose all elements that have been selected by term miner in the given category.
// --------------------------------------------------------------------------
void TermMiner::diagnoseSelectedTerms(TermVector& terms, LgsString title)
{
	diagnostic_->writeLine("SELECTED " + StringUtil::asStringFromInt(terms.size()) + " " + title);
	int elementNb = 0;
	for (TermIterator term=terms.begin(); term!=terms.end(); term++)
	{
		elementNb++;
		diagnostic_->write(StringUtil::asStringFromInt(elementNb));
		diagnostic_->write("\t" + term->sourceSurfaceForm());
		diagnostic_->write("\t(" + term->sourceCanonicalForm() + ")");
		diagnostic_->write("\t(" + term->targetSurfaceForm() + ")");
		diagnostic_->writeLine("");
	}
}

// --------------------------------------------------------------------------
void TermMiner::diagnoseSentenceUnitsInSequenceOrder()
{
	diagnostic_->writeLine("\n* all TS sentence units in sequence order as constructed by term miner*");
	if (sentenceUnits_.empty())
	{
		diagnostic_->writeLine("***empty!***");
	}
	else
	{
		int elementNb = 0;
		for (TermSearchSentenceUnitIterator unit=sentenceUnits_.begin(); unit!=sentenceUnits_.end(); unit++)
		{
			elementNb++;
			diagnostic_->write(StringUtil::asStringFromInt(elementNb));
			diagnostic_->write("\t" + unit->sourceSurfaceForm());
			diagnostic_->write("\t" + unit->sourceSconsAsString());
			diagnostic_->write("\t" + unit->targetSconsAsString());
			diagnostic_->writeLine("");
		}
	}
}

// --------------------------------------------------------------------------
// Report collected statistics. Tthe statistics are collected only with the 
// mean to study the data structure scheme used to insert and search Terms.
// Stats are saved only for specific user ids.
// --------------------------------------------------------------------------
void TermMiner::generateStatReport()
{
#ifdef __TERM_SEARCH_STATS__
	JobControlArguments &jobCntrlArgs = LgsDBCommonObjects::GetJobControlArguments();

	// get the user id
	LgsString userID = jobCntrlArgs.UserID();

	// check the user id. If ok, then save the stats in a local file.
	// *** NOTE: THIS IS ONLY TEMPORARY FOR TESTING TERM SEARCH AND NOT MEANT FOR PRODUCTION ***
	if (userID == "tsmlcc" || userID == "patrick" || userID == "tsstress")
	{
		cout << endl << "saving statistics" << endl;
		ofstream out("g:/esense/stats/TermSearch.stats");
		if (!out.good())
		{
			cout << "\tcannot open the file" << endl;
		}
		else
		{
			int totalWords = jobCntrlArgs.WordCount();
			out << endl;
			out << "--- TermSearch Statistics -----------------------------------------------------------" << endl;
			out << "Job ID: " << jobCntrlArgs.JobID() << endl;
			out << "Total words: " << totalWords << endl;
			stat_.report(out);
		}
		out.close();
	}
#endif
}

