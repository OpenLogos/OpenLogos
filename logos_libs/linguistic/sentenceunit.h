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
#ifndef __sentenceunit_h__
#define __sentenceunit_h__

//-------------------------------------------------------------------
// File - sentenceunit.h
//
// Class - SentenceUnit
//
// Description - This is the abstract "root" class for Sentence Units
//      that exist on either the source or target side.
//
//-------------------------------------------------------------------

#include <logos_libs/linguistic/lsyntaxunit.h>
#include <logos_libs/linguistic/compositeword.h>

class LDictionary;
class LDictionaryEntry;
class LSentence;

class SentenceUnit: public LSyntaxUnit
{
public:
   SentenceUnit();
   SentenceUnit(LDictionary&);
   SentenceUnit(LDictionary&, LDictionaryEntry*);
   SentenceUnit(const SentenceUnit&);
   virtual ~SentenceUnit();

   //---------------------------------------------------------------
   // The following functions are based upon the external text that
   // is represented by the object. This text is either the source
   // or the target text, depending upon the use of this sentence unit.
   //---------------------------------------------------------------
   const CompositeWord& surfaceExpression() const;
   void setSurfaceExpression(const CompositeWord&);
   const LgsString& surfaceExpressionAsString() const;
   LWordVector& surfaceWords();
   void clear();
   void addSurfaceWords(const LWordIterator& from, const LWordIterator& upTo);
   void addSurfaceWord(const LWord & inpWord);
   void surfaceExpressionFromString (const LgsString&);
   void insertString(int position, const LgsString&);
   void insertWord(int position, const LWord&);

   //---------------------------------------------------------------
   // The following enumeration and methods represent the case
   // or capitalization state of the whole unit. This is set on
   // the source side based on the incoming text and read by the
   // corresponding target unit. It is further updated based on
   // language rules at the target end.
   //
   // determineCaseState() - primarily used at source side -- sets
   //      the state according to what it finds in the actual text.
   //---------------------------------------------------------------
   enum CaseState
   {
      UndeterminedCase = -2,
      IrrelevantCase   = -1,
      LowerCase        =  0,
      BeginsUpperCase  =  1,
      AllUpperCase     =  2
   };

   //---------------------------------------------------------------
   CaseState caseState() const;
   void setCaseState(CaseState);
   virtual int precedingSpaces() const = 0;
   virtual int trailingSpaces() const = 0;

   //---------------------------------------------------------------
   enum
   {
      LowestUnfoundWordIdentifier  =  -35,
      HighestUnfoundWordIdentifier =  -11,
      BosIdentifier                =   -1,
      HighestTransferIdentifier    =   70,
      ProperNameIdentifier         = -880
   };

   //---------------------------------------------------------------
   virtual int wordClassCode() const = 0;
   virtual bool isPunctuation() const;
   virtual bool isPunctuationConstant() const;
   virtual bool isQuestionMark() const { return true; }
   virtual bool isComma() const;
   bool isEndOfSentence() const;
   void setEndOfSentence(bool);
   bool isMultiWord();
   virtual bool isValidFirstWord();
   const LWordMarkup& markup() const;
   LWordMarkup* wordMarkup();
   void setSentence(LSentence* p);
   int sentenceAddress() const;
   void setSentenceAddress(int);

   //---------------------------------------------------------------
   // language() - A sentence only has one language -- the following
   // is a virtual function. The units at the source end return the
   // source language while the units at the target end return
   // target language -- accesses the dictionary to get these
   // languages.
   //---------------------------------------------------------------
   virtual const LLanguage& language() const = 0;
   virtual int translatedWordCount() const;

   //---------------------------------------------------------------
   // The following is machinery for representing the object on
   // non-volatile medium.
   //---------------------------------------------------------------
   virtual void persistOut(ostream&);
   virtual void persistIn(istream&);

   //---------------------------------------------------------------
   virtual const LgsString& word() const = 0;

protected:
   LSentence& sentence();
   virtual void determineCaseState();

private:
   int v_sentenceAddress;
   CompositeWord v_surfaceExpression;
   LSentence* p_sentence;
   CaseState v_caseState;
   bool v_isEndOfSentence;
   LWordMarkup v_wordMarkup;
};

//---------------------------------------------------------------------
inline const CompositeWord& SentenceUnit::surfaceExpression() const
{
   return v_surfaceExpression;
}
//-------------------------------------------------------------------
inline const LgsString& SentenceUnit::surfaceExpressionAsString() const
{
   return v_surfaceExpression.toString();
}
//-------------------------------------------------------------------
inline void SentenceUnit::addSurfaceWord(const LWord & inpWord)
{
   v_surfaceExpression = inpWord;
}
//--------------------------------------------------------------------
inline void SentenceUnit::addSurfaceWords(const LWordIterator& from,
                                          const LWordIterator& upTo)
{
   if (from != upTo)
   {
      v_surfaceExpression = *from;
      for (LWordIterator i = (from + 1); i != upTo; i++)
      {
         v_surfaceExpression += *i;
      }
   }
}
//-------------------------------------------------------------------
inline void SentenceUnit::surfaceExpressionFromString(const LgsString& s)
{
   v_surfaceExpression.fromString(s, &(language()));
}
//-------------------------------------------------------------------
inline LWordVector& SentenceUnit::surfaceWords()
{
   return const_cast<LWordVector&>(surfaceExpression().words());
}
//-------------------------------------------------------------------
inline bool SentenceUnit::isEndOfSentence() const
{
   return v_isEndOfSentence;
}
//---------------------------------------------------------------------
inline void SentenceUnit::setEndOfSentence(bool b)
{
   v_isEndOfSentence = b;
}
//-------------------------------------------------------------------
inline const LWordMarkup& SentenceUnit::markup() const
{
   return v_wordMarkup;
}
//---------------------------------------------------------------------
inline LWordMarkup* SentenceUnit::wordMarkup()
{
   return &v_wordMarkup;
}
//-------------------------------------------------------------------
inline void SentenceUnit::setCaseState(SentenceUnit::CaseState s)
{
   v_caseState = s;
}
//---------------------------------------------------------------------
inline LSentence& SentenceUnit::sentence()
{
   return *p_sentence;
}
//---------------------------------------------------------------------
inline void SentenceUnit::setSentence(LSentence* p)
{
   p_sentence = p;
}
//---------------------------------------------------------------------
inline int SentenceUnit::sentenceAddress() const
{
   return v_sentenceAddress;
}
//---------------------------------------------------------------------
inline void SentenceUnit::setSentenceAddress(int n)
{
   v_sentenceAddress = n;
}
//---------------------------------------------------------------------
inline void SentenceUnit::setSurfaceExpression(const CompositeWord& rhs)
{
   v_surfaceExpression = rhs;
}
//---------------------------------------------------------------------
inline void SentenceUnit::insertString(int position, const LgsString& s)
{
   v_surfaceExpression.insertString(position, s);
}
//---------------------------------------------------------------------
inline void SentenceUnit::insertWord(int position, const LWord& s)
{
   v_surfaceExpression.insertWord(position, s);
}
//---------------------------------------------------------------------
inline bool SentenceUnit::isMultiWord()
{
   return 1 < surfaceWords().size();
}
//---------------------------------------------------------------------
inline void SentenceUnit::clear()
{
   v_surfaceExpression.clear();
}
//---------------------------------------------------------------------
inline bool SentenceUnit::isPunctuationConstant() const
{
   return false;
}

#endif // __sentenceunit_h__

