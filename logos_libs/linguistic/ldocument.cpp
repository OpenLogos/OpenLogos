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
// File: LDocument.cpp
// --------------------------------------------------------------------------
// Purpose: Implementation of class LDocument
// --------------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/ldocument.h>
#include <logos_libs/linguistic/luntranslatedsentence.h>
#include <logos_libs/linguistic/sentenceexception.h>
#include <logos_libs/translutility/translcommonobjects.h>
#include <transl/translthrman.h>
#include <logos_libs/multithreadlib/comminterface.h>
#include <lgs_db_io/lgsdbcommonobjects.h>
#include <logos_libs/PatternRecognition/ProperNameRecognizer.h>
#include <TermMiner/TermMiner.h>
#include <configdatafileinterface/configdatainterfacemain.h>

int LDocument::st_nextSentencePosition_ = 0;


// --------------------------------------------------------------------------
// Default constructor
// --------------------------------------------------------------------------
LDocument::LDocument()
          :TranslationObject(),
           p_termSearchStream(0),
           translatedWordCount_(0)
{
}


// --------------------------------------------------------------------------
// Construct this document based on the given document object
// --------------------------------------------------------------------------
LDocument::LDocument(const LDocument& aDocument)
          :TranslationObject(aDocument),
           p_termSearchStream(aDocument.p_termSearchStream),
           translatedWordCount_(aDocument.translatedWordCount_)
{
}


// --------------------------------------------------------------------------
// Construct this document for the dictionary lookup && output process
// --------------------------------------------------------------------------
LDocument::LDocument(LDictionary& dictionary)
          :TranslationObject(dictionary),
           translatedWordCount_(0)
{
}


// --------------------------------------------------------------------------
// Destructor
// --------------------------------------------------------------------------
LDocument::~LDocument()
{
}


// --------------------------------------------------------------------------
// This method causes each sentence to have its elements matched in the database.
// At the end of this method each sentence is an ordered list of source sentence
// units. Each sentence unit is made up of 1 or more SemantoSyntacticUnits
// (interpretations).
// Strategy: 
// After parsing the head of the input stream the document iterates through each 
// Sentence of the document.
// The LTX message parses the sentence into its component parts.
// --------------------------------------------------------------------------
bool nonInteractiveLookup = true;
bool nonInteractiveGenerate = true;
bool firstTimeGenerate = true;
bool firstTimeLookup = true;

void LDocument::lookup() 
{
	// create pattern matcher rule engine
	PM_Factory patternMatcherFactory("pattern matcher", TranslCommonObjects::GetSqlConnection(), true);
	RE_Engine<PM_Variable>* patternMatcherEngine = (RE_Engine<PM_Variable>*)patternMatcherFactory.createRuleEngine();

	TranslCommonObjects::GetDiagnostic()->writeAlways("*TRANSL IN*\n\n");

	TranslThreadManager & thrManager = TranslThreadManager::singleton();
	short resId = getThreadId(thrManager.threadList(), TranslThreadManager::RES.c_str());
	CommunicationInterface & commInterface = CommunicationInterface::singleton();

	// set the proper name pattern recognizer
   char properNamePatternsFilename[MAX_FILEPATH_LEN];

   GetConfigData("engine", "propernames_pattern", properNamePatternsFilename, MAX_FILEPATH_LEN);
	ProperNameRecognizer properNameRecognizer(properNamePatternsFilename);		// read patterns from a file

	while (doLookup())
	{
      bool documentComplete = false;
		for (int i = 1; !documentComplete; i++)
		{
			LSentence sentence(dictionary());

         try
			{
				if (sentence.makeSourceWords())
            {
			      TranslCommonObjects::GetDiagnostic()->setCurrLineNumber(i);
               char header[100];

				   sprintf(header, "\n*Original Input: Line #%d*", i);
				   TranslCommonObjects::GetDiagnostic()->writeLine(header);
				   TranslCommonObjects::GetDiagnostic()->wordsAsString(sentence.sourceWords());
				   TranslCommonObjects::GetDiagnostic()->writeLine("\n*Pattern Matcher*");
				   if (LgsDBCommonObjects::GetJobControlArguments().PatternRulesFlag() && patternMatcherEngine->hasRules())
				   {
					   sentence.patternMatch(patternMatcherEngine, true);
					   TranslCommonObjects::GetDiagnostic()->wordsAsString(sentence.sourceWords());
				   }
				   else
				   {
					   TranslCommonObjects::GetDiagnostic()->writeAlways("\nPattern matcher has no rules\n");
				   }
				   sentence.recognizeHyphenatedWords();

				   TranslCommonObjects::GetDiagnostic()->writeLine("\n*Start Rules*");
				   sentence.doStartRules();
				   TranslCommonObjects::GetDiagnostic()->wordsAsString(sentence.sourceWords(), true);

				   sentence.preLookupSetTranslationState ();
				   sentence.lookup();							// search for sworks in dictionary
				   sentence.postLookupSetTranslationState();

				   sentence.recognizeProperNames(properNameRecognizer);	// recognize any proper names in this sentence

				   incrementTranslatedWordCount(sentence);

				   LSentenceVector extraSentences; // holds sentences after breaking
                                               // sentence at Invalid Proper Name
                                               // positions
               LSentenceVector overSizedSentences; // holds sentences that resulted 
                                                   // from a split of the oversized 
                                                   // sentence.

               // Break at positions where the filter ASSUMED a Proper Name 
               // BUT it is NOT a Proper Name.
               // Since the filter does not do any Dictionary Lookup
               // , only Lookup can analyse the sentence and split it
               // if it is really NOT  a proper name
               sentence.splitSentence(&extraSentences);
               extraSentences.insert(extraSentences.begin(), &sentence);
               int pos = 0;
               LSentenceVector::iterator sv ;
               for( sv = extraSentences.begin(); sv != extraSentences.end(); )
               {
	               if ((*sv)->isSourceOverSized())
	               {
		               (*sv)->splitOverSized(&overSizedSentences);
	               }
                  overSizedSentences.insert(overSizedSentences.begin()+pos, *sv);
                  pos = overSizedSentences.size();
                  sv = extraSentences.erase(sv);
               }
               // Renumber parts of sentence
               int numberOfParts = overSizedSentences.size();
               int partOf = 0;
               for( sv = overSizedSentences.begin(); sv != overSizedSentences.end(); sv++ )
               {
                  if( numberOfParts > 1 )
                  {
   	               (*sv)->setNumberOfParts(numberOfParts);
   	               (*sv)->setPartOf(++partOf);
                  }
                  completeLookup(*sv);
               }
               overSizedSentences.erase(overSizedSentences.begin());
               overSizedSentences.removeAndDeleteAll();
               extraSentences.removeAndDeleteAll();
            }
            else
            {
               documentComplete = true;
            }
			}
			catch(SentenceException& sx)
			{
				TranslCommonObjects::GetDiagnostic()->writeLine("\n*SENTENCE EXCEPTION THROWN*");
				TranslCommonObjects::GetDiagnostic()->writeLine (sx.what() + LgsString(" For Sentence #") +
										(const_cast<SentenceMarkup&>(sentence.markup())).idAsString());
				TranslCommonObjects::GetDiagnostic()->writeLine("*Sentence Not Translated*\n");
				sentence.deleteEntries();
				sentence.deleteSourceUnits();
			}
			catch(...)
			{
				throw;
			}

		}
		if (LgsDBCommonObjects::GetJobControlArguments().TimeFlag())
		{
			sourceLanguage().logStartRuleStatistics();
		}
		if (!nonInteractiveLookup)
		{
			LgsMessage outMsg(CLOSEFILE, 0, 0, resId);
			commInterface.sendMsg(resId, outMsg);
		}
   }

   if (patternMatcherEngine)
   {
      delete patternMatcherEngine;
   }
   TranslCommonObjects::CleanupLookupObjects();
	TranslCommonObjects::GetDiagnostic()->flush();
}


// --------------------------------------------------------------------------
// This is the part of the Lookup process that is performed individually for each sentence 
// or sub-sentence. In the case of a sentence that exceeds 70 units, the sentence is split
// into sub-sentences of 70 units or less. Each of these sub-sentences is processed through 
// this method as an individual.
// --------------------------------------------------------------------------
void LDocument::completeLookup(LSentence* sentence)
{
   addSentence(sentence);

   sentence->persistOut();
   TranslCommonObjects::GetDiagnostic()->lookupSentence(*sentence);
   sentence->deleteEntries();
   sentence->deleteSourceUnits();
   TranslCommonObjects::GetDiagnostic()->writeLine("*EOS*\n");
}


// --------------------------------------------------------------------------
// --------------------------------------------------------------------------
bool LDocument::doLookup(void)
{
   bool waitMessage = false;

	if (!nonInteractiveLookup)
	{
		while (1)
		{
			if (LgsDBCommonObjects::GetInteractiveTranslStatus().InteractiveStatus() == "enqueued")
			{
				LgsDBCommonObjects::GetInteractiveTranslStatus().InteractiveStatus("running");
				return true;
			}
			else if (LgsDBCommonObjects::GetInteractiveTranslStatus().InteractiveStatus() == "kill")
			{
   			cout << "\nIn Lookup: Interactive complete.\n" << flush;
				return false;
			}
         if (!waitMessage)
         {
   			cout << "\nIn Lookup: Waiting for interactive signal ...\n" << flush;
            waitMessage = true;
         }
			Sleep(1000);
		}
	}
	else if (LgsDBCommonObjects::GetInteractiveTranslStatus().InteractiveTranslMode())
	{
		nonInteractiveLookup = false;
		while (1)
		{
			if (LgsDBCommonObjects::GetInteractiveTranslStatus().InteractiveStatus() == "enqueued")
			{
				LgsDBCommonObjects::GetInteractiveTranslStatus().InteractiveStatus("running");
				return true;
			}
         if (!waitMessage)
         {
   			cout << "\nIn Lookup: Waiting for interactive signal ...\n" << flush;
            waitMessage = true;
         }
			Sleep(1000);
		}
	}
	else
	{
		if (firstTimeLookup)
		{
			firstTimeLookup = false;
			return true;
		}
		else
		{
			return false;
		}
	}
}


// --------------------------------------------------------------------------
// Update the total number of words that are billable by Logos. This is not a
// count of the matches that were made (matches sometimes count several words
// as one phrase match).
// --------------------------------------------------------------------------
void LDocument::incrementTranslatedWordCount(const LSentence& sentence) {
   translatedWordCount_ += sentence.translatedWordCount();
}

// --------------------------------------------------------------------------
int LDocument::translatedWordCount() const
{
   return translatedWordCount_;
}


// --------------------------------------------------------------------------
// Generate the translation in defined stream (output file).
// This method takes all the sentences processes by the transfer modules and turns them into 
// the target text.
// --------------------------------------------------------------------------
void LDocument::generateTranslation()
{
	PM_Factory patternMatcherFactory("pattern matcher", TranslCommonObjects::GetSqlConnection(), false);
	RE_Engine<PM_Variable>* patternMatcherEngine = (RE_Engine<PM_Variable>*)patternMatcherFactory.createRuleEngine();
	LSentence* sentence = 0;
	LSentence* superSentence = 0;

   while(doGenerate())
   {
		int lineCount = 0;
		while(1)
		{
			try
			{
				sentence = new LSentence(dictionary());
				if (!sentence->persistIn())
				{
	   			delete sentence;
					break;
				}
				TranslCommonObjects::GetDiagnostic()->writeAlways("*TRANSL OUT*\n");

				lineCount++;
				TranslCommonObjects::GetDiagnostic()->setCurrLineNumber(lineCount);

				if (!(sentence->isTranslated() && dictionary().isSentencePositionCorrect(sentence->position())))
				{
					LUntranslatedSentence *untranslatedSentence = new LUntranslatedSentence(*sentence);
					delete sentence;
					sentence = untranslatedSentence;
				}
				sentence->finalizeTransfer(patternMatcherEngine);
			}
			catch(SentenceException& sx)
			{
				TranslCommonObjects::GetDiagnostic()->writeLine ("\n*SENTENCE EXCEPTION THROWN*");
				TranslCommonObjects::GetDiagnostic()->writeLine (sx.what() + LgsString(" For Sentence #") +
										(const_cast<SentenceMarkup&>(sentence->markup())).idAsString());
				TranslCommonObjects::GetDiagnostic()->generateSentence(*sentence);
				TranslCommonObjects::GetDiagnostic()->writeLine ("*Sentence Not Translated*\n");
			}
			catch(...)
			{
				TranslCommonObjects::GetDiagnostic()->writeLine ("\n*UNKNOWN EXCEPTION THROWN*");
				TranslCommonObjects::GetDiagnostic()->writeLine ("*Sentence Not Translated*\n");
				TranslCommonObjects::GetDiagnostic()->generateSentence(*sentence);
			}
			if (sentence->numberOfParts())
			{
				if (1 == sentence->partOf())
				{
					superSentence = sentence;
				}
				else
				{
					superSentence->concatenate(sentence);
					if (sentence->partOf() == sentence->numberOfParts())
					{
						completeGenerate(superSentence);
					}
					delete sentence;
				}
			}
			else
			{
				completeGenerate(sentence);
			}

			// Write out to the screen when every sentence has completed.
			if (lineCount % 10 == 0)
         {
				cout << "\r" << "Sentence Line Count\t= " << lineCount << flush;
         }
		}
      if (patternMatcherEngine)
      {
         delete patternMatcherEngine;
      }
		if (LgsDBCommonObjects::GetJobControlArguments().TimeFlag())
		{
			targetLanguage().logElisionStatistics();
		}
   }
}


//---------------------------------------------------------------------
bool LDocument::doGenerate(void)
{
	if (!nonInteractiveGenerate)
	{
		LgsDBCommonObjects::GetInteractiveTranslStatus().InteractiveStatus("complete");
		while (1)
		{
			if (LgsDBCommonObjects::GetInteractiveTranslStatus().InteractiveStatus() == "running")
			{
				return true;
			}
			else if (LgsDBCommonObjects::GetInteractiveTranslStatus().InteractiveStatus() == "kill")
			{
				return false;
			}
			Sleep(1000);
		}
	}
	else if (LgsDBCommonObjects::GetInteractiveTranslStatus().InteractiveTranslMode())
	{
		nonInteractiveGenerate = false;
		while (1)
		{
			if (LgsDBCommonObjects::GetInteractiveTranslStatus().InteractiveStatus() == "running")
			{
				return true;
			}
			Sleep(1000);
		}
	}
	else
	{
		if (firstTimeGenerate)
		{
			firstTimeGenerate = false;
			return true;
		}
		else
		{
			return false;
		}
	}
}


//---------------------------------------------------------------------
// Performs the part of Generate that must occur after a long sentence has been 
// re-built from the sub-sentences that it was split into.
//---------------------------------------------------------------------
void LDocument::completeGenerate(LSentence* sentence)
{
   sentence->completeGenerate();

   TranslCommonObjects::GetDiagnostic()->write("\n*After Complete Generate*\n");
   TranslCommonObjects::GetDiagnostic()->generateSentence(*sentence);

	sentence->reconcileMarkup();

   sentence->outputTarget();

   TranslCommonObjects::GetDiagnostic()->write("\n*Output*\n");
   TranslCommonObjects::GetDiagnostic()->generateSentence(*sentence);
  
   TranslCommonObjects::GetDiagnostic()->writeLine("\n*EOS*\n");

   cleanUpSentence(sentence);
}


//---------------------------------------------------------------------
// Clean up this object - free memory from the information about the current sentence
//---------------------------------------------------------------------
void LDocument::cleanUpSentence(LSentence* sentence)
{
   sentence->deleteEntries();
   sentence->deleteTargetUnits();
   sentence->deleteSourceUnits();
   delete sentence;
}


//---------------------------------------------------------------------
// Capture all terms from the document, whether known or not
// Exit the function with value 0 if cannot open the term search diagnostic file
//---------------------------------------------------------------------
void LDocument::termSearch()
{
	TermMiner termMiner;

	int lineCount = 0;
	while (true)
	{
		try
		{
			LSentence *sentence = new LSentence(dictionary());	// next sentence in the document
			
			if (!sentence->persistIn())
			{
				delete sentence;
				break;
			}

			lineCount++;

			if (sentence->isTranslated() && dictionary().isSentencePositionCorrect(sentence->position()))
			{
				sentence->beginFinalizeTransferForTermSearch();
				termMiner.captureTermsIn(sentence);
				sentence->Finish_beginFinalizeTransferForTermSearch();
				PM_Factory patternMatcherFactory("pattern matcher",TranslCommonObjects::GetSqlConnection(),false);
				RE_Engine<PM_Variable>* patternMatcherEngine = (RE_Engine<PM_Variable>*)patternMatcherFactory.createRuleEngine();
				sentence->finishFinalizeTransferForTermSearch(patternMatcherEngine);
				termMiner.categorizeTerms();
				termMiner.generateDiagnostic();
            if (patternMatcherEngine)
            {
               delete patternMatcherEngine;
            }
			}
			cleanUpSentence(sentence);		// free memory from that sentence
		}
		catch(SentenceException& sx)
		{
			TranslCommonObjects::GetDiagnostic()->writeLine("\n*SENTENCE EXCEPTION THROWN.*\n");
			TranslCommonObjects::GetDiagnostic()->writeLine(sx.what());
			TranslCommonObjects::GetDiagnostic()->writeLine("\n*Sentence Not Translated.*\n");
		}
		catch(...)
		{
			TranslCommonObjects::GetDiagnostic()->writeLine("\n*UNKNOWN EXCEPTION THROWN.*\n");
			TranslCommonObjects::GetDiagnostic()->writeLine("\n*Sentence Not Translated.*\n");
		}
			
		// Write out to the screen when every sentence has completed.
		cout << "\r" << "Sentence Line Count\t= " << lineCount << flush;
	}

	termMiner.generateTermSearchReport(*p_termSearchStream);
}
