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
#ifndef __LSentence_h__
#define __LSentence_h__

// --------------------------------------------------------------------------
// File - LSentence.h
//
// Class - LSentence (interface)
//
// Description - the primary object of the Gerdem subsystem. An object
// of this class is created using the "Create()" static member
// function. The result of the "Create()" function is a LSentence
// object that is complete with all its GDictionaryEntry objects and
// their respective GSemantoSyntacticUnits. In fact, the LSentence
// object is ready for the RES subsystem.
//
// --------------------------------------------------------------------------

#include <logos_libs/linguistic/translationobject.h>
#include <logos_libs/linguistic/targetsentenceunit.h>
#include <logos_libs/linguistic/sourcesentenceunit.h>
#include <logos_libs/linguistic/lword.h>
#include <logos_libs/linguistic/sentencemarkup.h>
#include <logos_libs/linguistic/sentenceexception.h>
#include <logos_libs/patternmatcher/engine.h>
#include <logos_libs/patternmatcher/variable.h>

class LDictionary;
class BlackHole;
class ProperNameRecognizer;
class SentenceException;

class LSentence;
typedef LgsVector(LSentence*) LSentencePVector;
typedef LSentencePVector::iterator LSentencePIterator;
typedef RE_Engine<PM_Variable> RuleEngine;


// --------------------------------------------------------------------------
// Definition of class LSentence
// The DictionaryEntryVector is built by this object and cannot be set by any outside object.
// --------------------------------------------------------------------------
class LSentence: public TranslationObject 
{
public:
   DummyLess(LSentence);
   DummyEqual(LSentence);
   LSentence();
   LSentence(LDictionary& dictionary);
   LSentence(const LSentence&);
   virtual ~LSentence();

   SourceUnitVector& sourceSentenceUnits();
   int numberOfSourceUnits() const;                // units that have been isolated by dictionary matches, etc.
   TargetUnitVector& targetSentenceUnits();

   virtual int translatedWordCount() const;        // number of words or phrases, matched or unmatched within this sentence

   bool makeSourceWords();
   LWordVector& sourceWords();                     // returns a vector of the actual words used in sentence
   LWordVector& origSourceWords();

   bool isQuestion() const;                        // true if the sentence is a question

   virtual void lookup() throw (SentenceException);   // dictionary matching of the source words
   virtual void doStartRules();                    // permorms start rules (dates, times, numbers etc) on the sentence
   virtual void elide();                           // performs the finishing rules on this sentence
   void generateDictionaryEntries();
   void recognizeHyphenatedWords();

   //---------------------------------------------------------------
   // adjustFinalSpaces() - sets the trailing spaces for each target sentence
   //                       unit to the preceding spaces of the preceding target
   //                       sentence unit.
   // assignMarkupToWords() - each target word gets the markup of owning target
   //                         sentence unit (which in turn received it from its
   //                         corresponding source sentence unit which received it
   //                         from its source word.
   // associateTargetToSource() - have each target sentence unit identify its
   //                             corresponding source unit (if it has one).
   // capitalize() -
   //---------------------------------------------------------------
   // finalize the surface form of the target units
   // - for translation jobs: call beginFinalizeTransfer() and finishFinalizeTransfer() without 
   //   interuption -> see LDocument::generateTranslation()
   // - for term search jobs: call beginFinalizeTransfer() and finishFinalizeTransfer() with 
   //   interuption for processing -> see LDocument::termSearch()
   void finalizeTransfer(RuleEngine*);
   void beginFinalizeTransfer();
   void finishFinalizeTransfer(RuleEngine*);
   virtual void generate() throw (SentenceException);		// builds an ordered set of Target Sentence Units
   void adjustFinalSpaces();
   void assignMarkupToWords();
   void associateTargetToSource();
   virtual void capitalize();
   void completeGenerate();

   //-----------------------------------------------------------
   // For Termsearch job the begin finalize tranfer is done in two steps
   //The Generate stems is done after the termsearch resets the scons.
   void beginFinalizeTransferForTermSearch();
   void Finish_beginFinalizeTransferForTermSearch();
   void finishFinalizeTransferForTermSearch(RuleEngine* patternMatcherEngine);

   //---------------------------------------------------------------
   // generateStems() - has each target unit look itself up in the database's pat
   //                   tables and convert its text to the appropriate form based
   //                   on gender, peson, etc.,.
   // inhibitCapConstant() -
   // isLastOfOversizedTarget() -
   // isSourceOverSized() - returns true if this sentence has over 70 source units.
   // isTranslated() -
   //---------------------------------------------------------------
   virtual void generateStems();
   bool isLastOfOversizedTarget();
   bool isSourceOverSized() const;
   virtual bool isTranslated() const;

   //---------------------------------------------------------------
   // outputTarget() - 
   // patternMatch() -
   // processBlackHoles() -
   // reconcileMarkup() -
   //---------------------------------------------------------------
   virtual void makeAspirations();
   void outputTarget();
   void patternMatch(RE_Engine<PM_Variable>*, bool source);
   virtual void processBlackHoles();
   virtual void reconcileMarkup();

   //---------------------------------------------------------------
   // reconcilePrecedingSpaces() -
   // removeExtraneousTargetUnits() -
   // setSurfaceExpressionFromDictionary() -
   // splitOverSized() - creates a vector of sub-sentences, leaving this sentence
   //                    with 70 units. It is assumed that the vector of sub-sentences
   //                    will be processed following this sentence.
   // updateSourcePrimarySsu() - requests that each of the target units go to its
   //                            corresponding source unit (if it has one) and set the
   //                            index of the primary Ssu according to the information
   //                            coming from Tran 4.
   //---------------------------------------------------------------
   void reconcilePrecedingSpaces();
   virtual void removeExtraneousTargetUnits();
   void setSurfaceExpressionFromDictionary();
   void splitOverSized(LSentencePVector*);
   // Break at positions where the filter ASSUMED a Proper Name 
   // BUT it is NOT a Proper Name.
   // Since the filter does not do any Dictionary Lookup
   // , only Lookup can analyse the sentence and split it
   // if it is really NOT  a proper name
   void splitSentence(LSentencePVector *);
   LSentence* breakSentence(bool bFirstTime);
   bool sentenceCanBeSplit(SourceUnitIterator currentUnit) const;
   void updateSourcePrimarySsu();

   //---------------------------------------------------------------
   // Each sentence object has a translation state. If it is a no translation sentence,
   // the source sentence units are just transfered to the target side.
   //---------------------------------------------------------------
   enum TranslationState{ DoTranslate = 0, DoNotTranslate = 1 };
   TranslationState translationState() const;
   void setTranslationState(TranslationState);
   void preLookupSetTranslationState();
   void postLookupSetTranslationState();

   //---------------------------------------------------------------
   void deleteEntries();
   void deleteSourceUnits();
   void deleteTargetUnits();
   void mergeTargetUnits();

   //---------------------------------------------------------------
   enum { maximumInputLength = 1024 };
   enum { minimumSourceAddress =  1 };
   enum { maximumSourceAddress = 70 };

   //---------------------------------------------------------------
   // markup() - access methods for the markup information that is specific to the
   //            sentence as a whole.
   //---------------------------------------------------------------
   const SentenceMarkup& markup() const;
   void setMarkup(const SentenceMarkup&);

   //---------------------------------------------------------------
   // Persistence stuff.
   //---------------------------------------------------------------
   virtual void persistOut(void);
   virtual bool persistIn(void);
   void deleteEmptyUnits(bool termSearchOnly = false);

   //---------------------------------------------------------------
   // The following members are used only to adapt extra long
   // sentences (over 70 units).
   //
   // partOf() - If this sentence is a part of a larger sentence, this value
   //            indicates its position in the super-sentence.
   // numberOfParts() - If this sentence is a part of a larger sentence, this value
   //                   indicates how many other sentences make up the super-sentence.
   // concatenate() - This method recombines sub-sentences into the original long
   //                 sentence. This is used in the generate end of the processing
   //                 after all Fortran processing is over (this sentences were broke
   //                 up into sub-sentences because Fortran couldn't handle more than 70 units.
   //---------------------------------------------------------------
   int partOf() const;
   void setPartOf(int);
   int numberOfParts() const;
   void setNumberOfParts(int);
   void concatenate(LSentence*);

   //---------------------------------------------------------------
   // isSourceAllUpperCase() -
   // isSourceBeginsUpperCase() -
   // isTargetAllUpperCase() -
   // isTargetBeginsUpperCase() -
   // caseState() -
   //---------------------------------------------------------------
   bool isSourceAllUpperCase() const;
   bool isSourceBeginsUpperCase() const;
   bool isTargetAllUpperCase() const;
   bool isTargetBeginsUpperCase() const;
   SentenceUnit::CaseState caseState();
   void writeSourceWords(ostream&);
   bool isBold() const;
   bool isItalic() const;
   bool isUnderlined() const;
   bool isSingleQuoted() const;
   bool isDoubleQuoted() const;

   // pattern recognition activities
   void recognizeProperNames(ProperNameRecognizer&);		// recognize proper names
   void filterLastNames();									// used during generation phase to keep the source form of last names
   void findAssociatedSourceSentenceUnit(TargetSentenceUnit*,SourceUnitIterator&);
   void checkForPhraseModification();
#ifdef TRANSLAPP
   static void createMessaging();
   static void cleanupMessaging();
#endif

protected:
   //---------------------------------------------------------------
   // makeAllSourceUnitsProtected() -
   // makeSourceSentenceUnits() -
   // makeTargetSentenceUnits() -
   // makeAllTargetUnitsProtected() -
   //---------------------------------------------------------------
   void makeAllSourceUnitsProtected();
   void makeSourceSentenceUnits();
   virtual void makeTargetSentenceUnits();
   void makeAllTargetUnitsProtected();

   //---------------------------------------------------------------
   // findNextBlackHoleEnd() -
   // createBlackHole() -
   // insertBlackHole() -
   //---------------------------------------------------------------
   TargetUnitIterator findNextBlackHoleEnd(TargetUnitIterator start, TargetUnitIterator end) const;
   BlackHole* createBlackHole(TargetUnitIterator start, TargetUnitIterator end);
   void insertBlackHole(const BlackHole*);

   //---------------------------------------------------------------
   // removeDividingUnits() -
   // renumberFoundUnits() -
   // renumberUnFoundUnits() -
   // replaceWithUnresolvedUnit() -
   //---------------------------------------------------------------
   void removeDividingUnits();
   void renumberFoundUnits();
   void assignOrigSentencePosition();
   void renumberUnFoundUnits();
   void replaceWithUnresolvedUnit(TargetUnitIterator*);
   SourceUnitIterator findLastSourceSurfaceExpression(SourceUnitIterator begin,
                                                      SourceUnitIterator end,
                                                      const LgsString& s);
   SourceUnitIterator findLastSourceSurfaceExpression(SourceUnitIterator begin,
                                                      SourceUnitIterator end,
                                                      LgsVector(LgsString) & s);
   void determineSentenceMarkup();
   void determineIfBracketed();
   bool isBracketedBy(LgsString beginBracket, LgsString endBracket);
   void markUnfoundWords(); // mark unfound words by adding a question mark in front of ones
   //---------------------------------------------------------------
   // findSentenceBreak() - If the sentence needs to be broken up into 70 units or less,
   //                       there is a very intricate criteria for determining where to
   //                       break the sentence. This method finds the best breaking point.
   // reduceToValidSize() - This method breaks a oversized sentence up into sub-sentences.
   //---------------------------------------------------------------
   SourceUnitIterator findSentenceBreak(SourceUnitIterator begin, SourceUnitIterator end,
                                        bool* isBreakPositionInNew);
   LSentence* reduceToValidSize();
   SourceUnitVector v_sourceSentenceUnits;
   LWordVector v_sourceWords;
   LWordVector v_origSourceWords;

private:
   TargetUnitVector v_targetSentenceUnits;
   SentenceMarkup v_markup;
   static int st_baseUnfoundWordAddress;
   TranslationState translationState_;
   int partOf_;
   int numberOfParts_;
   SentenceUnit::CaseState caseState_;
   bool bold_;
   bool italic_;
   bool underlined_;
   bool singleQuoted_;
   bool doubleQuoted_;
   bool bDifferentSentence_;
};


// --------------------------------------------------------------------------
// Definition of a vector of LSentence objects
// --------------------------------------------------------------------------
class LSentenceVector: public LgsVector(LSentence*)
{
public:
   virtual ~LSentenceVector();
   void removeAndDelete(LSentenceVector::iterator start, LSentenceVector::iterator end);
   void removeAndDeleteAll();
};

typedef LSentenceVector::iterator LSentenceIterator;

//-------------------------------------------------------------------
inline LWordVector& LSentence::sourceWords()
{
   return v_sourceWords;
}
//-------------------------------------------------------------------
inline LWordVector& LSentence::origSourceWords()
{
   return v_origSourceWords;
}
//-------------------------------------------------------------------
inline int LSentence::partOf() const
{
   return partOf_;
}
//-------------------------------------------------------------------
inline void LSentence::setPartOf(int n)
{
   partOf_ = n;
}
//-------------------------------------------------------------------
inline int LSentence::numberOfParts() const
{
   return numberOfParts_;
}
//-------------------------------------------------------------------
inline void LSentence::setNumberOfParts(int n)
{
   numberOfParts_ = n;
}
//-------------------------------------------------------------------
inline const SentenceMarkup& LSentence::markup() const
{
   return v_markup;
}
//-------------------------------------------------------------------
inline void LSentence::setMarkup(const SentenceMarkup& rhs)
{
   v_markup = rhs;
}
//---------------------------------------------------------------------
inline SourceUnitVector& LSentence::sourceSentenceUnits()
{
   // Simply a "Getter" member function for the vector of GDictionaryEntry objects.
   return v_sourceSentenceUnits;
}
//---------------------------------------------------------------------
inline TargetUnitVector& LSentence::targetSentenceUnits()
{
   // Simply a "Getter" member function for the vector of GDictionaryEntry objects.
   return v_targetSentenceUnits;
}


//---------------------------------------------------------------------
inline int LSentence::numberOfSourceUnits() const
{
   return v_sourceSentenceUnits.size();
}
//---------------------------------------------------------------------
inline LSentence::TranslationState LSentence::translationState() const
{
   return translationState_;
}
//-------------------------------------------------------------------
inline void LSentence::setTranslationState(TranslationState t)
{
   translationState_ = t;
}
//---------------------------------------------------------------------
inline bool LSentence::isTargetAllUpperCase() const
{
   return caseState_ == SentenceUnit::AllUpperCase;
}
//---------------------------------------------------------------------
inline bool LSentence::isTargetBeginsUpperCase() const
{
   return caseState_ == SentenceUnit::BeginsUpperCase;
}
//---------------------------------------------------------------------
inline bool LSentence::isBold() const
{
   return bold_;
}
//---------------------------------------------------------------------
inline bool LSentence::isItalic() const
{
   return italic_;
}
//---------------------------------------------------------------------
inline bool LSentence::isUnderlined() const
{
   return underlined_;
}
//---------------------------------------------------------------------
inline bool LSentence::isSingleQuoted() const
{
   return singleQuoted_;
}
//---------------------------------------------------------------------
inline bool LSentence::isDoubleQuoted() const
{
   return doubleQuoted_;
}

#endif // __LSentence_h__


