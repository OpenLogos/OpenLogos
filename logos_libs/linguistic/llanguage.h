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
#ifndef __LLanguage_h__
#define __LLanguage_h__

//-------------------------------------------------------------------
// File - LLanguage.h
//
// Class - LLanguage
//
// Description - The primary purpose of this class is to make the
//      rest of the translation engine independent of the specific
//      languages that are being processed. This class is an abstract
//      class that provides the interface for a "strategy pattern"
//      of subclasses that provide language specific algorithms.
//
//-------------------------------------------------------------------

#include <logos_libs/linguistic/linflection.h>
#include <logos_libs/linguistic/context.h>
#include <logos_libs/linguistic/lroot.h>
#include <logos_libs/linguistic/lword.h>
#include <logos_libs/entity/dderivedform.h>
#include <logos_libs/ruleengine/engine.h>
#include <logos_libs/elision/variable.h>
#include <logos_libs/regex/charutil.h>
#include <logos_libs/startrules/variable.h>

class TargetSentenceUnit;
class TargetUnitVector;
class LSemantoSyntacticUnit;
class ST_Locale;

class LLanguage: public Object
{
    DisableCopyAssign(LLanguage);

public:
   // constants that should be used to identify a particular language within Logos.
   enum ID {GermanID = 1, 
			EnglishID, 
			FrenchID, 
			SpanishID, 
			ItalianID, 
			PortugueseID
   };

   // constants that identify the parts of speech.
   enum WordClassCode
   {
      NOUN                 =  1,
      VERB                 =  2,
      ADVERB_LOCATIVE      =  3,
      ADJECTIVE            =  4,
      PRONOUN              =  5,
      ADVERB               =  6,
      PREPOSITION          = 11,
      AUXILIARY            = 12,
      PREPOSITION_LOCATIVE = 13,
      ARTICLE_DEFINITE     = 14,
      ARTICLE_INDEFINITE   = 15,
      ARITHMATE            = 16,
      NEGATIVE             = 17,
      PRONOUN_RELATIVE     = 18,
      CONJUNCTION          = 19,
      PUNCTUATION          = 20
   };

   LLanguage(ID id, const LgsString& description);
   virtual ~LLanguage();

   ID id() const;
   LgsString idString() const;
   const LgsString& description() const;
   LgsString shortDescription() const;		// return a two-letter description
   const Context& context() const;
   bool isRomanceLanguage() const;

   //---------------------------------------------------------------
   // inflections() - A collection of all the inflections that are
   //     valid with the given language. For example, the English
   //     language will contain "s", "es", "ing", etc.
   //     This list is used throughout the matching process to find
   //     non-exact matches for source words.
   //---------------------------------------------------------------
   const LInflectionVector* inflections() const;
   void inflections(LInflectionVector*);
   static const LInflection* nullInflection();

//   void derivedForms(DDerivedFormVector*);
   void derivedForms(DDerivedFormMap *);
   void context(const Context*);

   //---------------------------------------------------------------
   // CreateRootsFromWord() - allows a user to create a list of all
   //                         the possible roots of a given word.
   // CreateWordsFromRoot() - alows a user to create a list of words
   //                         that are possible from a given root.
   //---------------------------------------------------------------
   LRootVector* createRootsFromWord(const LWord&) const;
   LWordVector* createWordsFromRoot(const LRoot&) const;

   int findFormCode(const LSemantoSyntacticUnit&, const LgsString& ending) const;

   const DDerivedForm* findDerivedForm(int patNumber, int stemNumber,
                                       const LInflection* ending) const;

   bool isFormDeriveable(const LSemantoSyntacticUnit&, const LgsString& ending) const;

   bool isFormDeriveable(int patNumber, int stemNumber, const LgsString& ending) const;

   //---------------------------------------------------------------
   // isOrdinalSuffix() - this pure virtual function simply answers whether a
   //                     given LgsString is a valid suffix for an ordinal number
   //                     in the specific language.
   //---------------------------------------------------------------
   virtual bool isOrdinalSuffix(const LgsString&) const = 0;

   void loadStartRulesEngine(const LgsString& startRulesFileName);
   void doStartRules(LWordVector& words) const;

   void setElisionEngine(RE_Engine<EL_Variable>* engine);
   void elide(TargetUnitVector&) const;

   virtual const LgsString* combiningForm(int);
   virtual LgsString applyCombiningFormCode(int, LgsString&);

   //---------------------------------------------------------------
   // The following methods determine the catagories that a
   // character belongs to. These categories are language specific.
   // The methods are not virtual because they rely on data
   // structures that are filled in by the constructors of the
   // derived classes (e.g. French fills in the French vowels).
   //---------------------------------------------------------------
   bool isVowel(const char& ch) const;
   bool isConsonant(const char& ch) const;
   bool isDiacritic(const char& ch) const;

   //---------------------------------------------------------------
   // inPhraseCache( 1 ) - Should phrases begininning with first + " " be cached
   // inPhraseCache( 2 ) - Should phrases begininning with first + " " + second be cached
   //---------------------------------------------------------------
   virtual bool inPhraseCache(const LgsString& first) const;
   virtual bool inPhraseCache(const LgsString& first, const LgsString& second) const;

   //---------------------------------------------------------------
   // findAspirePoints() - This virtual method has a default implementation in this
   //                      class. The default returns the beginning of the LgsString as
   //                      the aspire point if and only if the "isAspired" parameter is true.
   //---------------------------------------------------------------
   virtual void findAspirePoints(TargetSentenceUnit* prev, TargetSentenceUnit* curr, LgsVector(int)* positions) const;
   void logStartRuleStatistics(void) const;
   void logElisionStatistics(void) const;

protected:
    bool isAlphabeticalCategory(const char& ch, const LgsString& category) const;
    void setVowels(const LgsString&);
    void setConsonants(const LgsString&);
    void setDiacritics(const LgsString&);

private:
    ID id_;
    LgsString description_;

    const Context* p_context;
    LInflectionVector* p_inflections;
//    DDerivedFormVector* p_derivedForms;
    DDerivedFormMap* p_derivedFormsMap;

    static LInflection v_nullInflection;

    // rule-engines
    RE_Engine<EL_Variable>* p_elisionEngine;
    RE_Engine<ST_Variable>* p_startRulesEngine;

    LgsString m_vowels;
    LgsString m_consonants;
    LgsString m_diacritics;
};

//-------------------------------------------------------------------
inline LLanguage::ID LLanguage::id() const
{
   return id_;
}
//-------------------------------------------------------------------
inline LgsString LLanguage::idString() const
{
   char s[10];
   sprintf(s, "%02d", id_);
   return LgsString(s);
}
//-------------------------------------------------------------------
inline const LgsString& LLanguage::description() const
{
   return description_;
}
//-------------------------------------------------------------------
inline const LInflectionVector* LLanguage::inflections() const
{
   return p_inflections;
}
//-------------------------------------------------------------------
inline const Context& LLanguage::context() const
{
   assert(p_context);
   return *p_context;
}
//-------------------------------------------------------------------
/* replacing vector w/ map
inline void LLanguage::derivedForms(DDerivedFormVector* aVector)
{
   assert(!p_derivedForms);
   p_derivedForms = aVector;
}
*/
inline void LLanguage::derivedForms(DDerivedFormMap* aMap)
{
   assert(!p_derivedFormsMap);
   p_derivedFormsMap = aMap;
}
//-------------------------------------------------------------------
inline void LLanguage::inflections(LInflectionVector* aVector)
{
   assert(!p_inflections);
   p_inflections = aVector;
}
//-------------------------------------------------------------------
inline void LLanguage::context(const Context* aContext)
{
   p_context = aContext;
}
//-------------------------------------------------------------------
inline const LInflection* LLanguage::nullInflection()
{
   return &v_nullInflection;
}
//---------------------------------------------------------------------
inline bool LLanguage::isAlphabeticalCategory(const char& ch, const LgsString& s) const
{
   return (LgsString::npos == s.find(CharUtil::lower(ch))) ? false : true;
}
//---------------------------------------------------------------------
inline bool LLanguage::isVowel(const char& ch) const
{
   return isAlphabeticalCategory(ch, m_vowels);
}
//---------------------------------------------------------------------
inline void LLanguage::setVowels(const LgsString& s)
{
   m_vowels = s;
}
//---------------------------------------------------------------------
inline bool LLanguage::isConsonant(const char& ch) const
{
   return isAlphabeticalCategory (ch, m_consonants);
}
//---------------------------------------------------------------------
inline void LLanguage::setConsonants(const LgsString& s)
{
    m_consonants = s;
}
//---------------------------------------------------------------------
inline bool LLanguage::isDiacritic(const char& ch) const
{
   return isAlphabeticalCategory(ch, m_diacritics);
}
//---------------------------------------------------------------------
inline void LLanguage::setDiacritics(const LgsString& s)
{
   m_diacritics = s;
}
//---------------------------------------------------------------------
inline void LLanguage::setElisionEngine(RE_Engine<EL_Variable>* engine)
{
   p_elisionEngine = engine;
}
//---------------------------------------------------------------------
inline bool LLanguage::inPhraseCache(const LgsString& first) const
{
   return false;
}
//---------------------------------------------------------------------
inline bool LLanguage::inPhraseCache(const LgsString& first, const LgsString& second) const
{
   return false;
}

#endif



