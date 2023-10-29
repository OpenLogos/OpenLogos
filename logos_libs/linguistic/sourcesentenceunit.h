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
// File - sourcesentenceunit.h
//
// Class - SourceSentenceUnit
//
// --------------------------------------------------------------------------

#ifndef __sourcesentenceunit_h__
#define __sourcesentenceunit_h__

#include <logos_libs/linguistic/sentenceunit.h>
#include <logos_libs/linguistic/ldictionary.h>
#include <logos_libs/linguistic/ldictionaryentry.h>

class LLanguage;
class LSemantoSyntacticUnit;
class SWork;
class Henum;
class SubjectMatterCode;
class LDictionary;
class LDictionaryEntry;

// --------------------------------------------------------------------------
// Definition of the class of SourceSentenceUnit object
// --------------------------------------------------------------------------
class SourceSentenceUnit: public SentenceUnit 
{
public:
   DummyLess(SourceSentenceUnit);
   DummyEqual(SourceSentenceUnit);

   //---------------------------------------------------------------
   SourceSentenceUnit();
   SourceSentenceUnit(const SourceSentenceUnit&);
   SourceSentenceUnit(LDictionary& dictionary);
   SourceSentenceUnit(LDictionary& dictionary, LDictionaryEntry* entry);
   SourceSentenceUnit(LDictionary& dictionary, const SWork& swork);
   SourceSentenceUnit(const SWork&);
   virtual ~SourceSentenceUnit();

   //---------------------------------------------------------------
   virtual bool isCompound(void) const;
   virtual const LLanguage& language() const;
   LDictionaryEntry& dictionaryEntry() const;
   void dictionaryEntry(LDictionaryEntry*);

   void deleteEntry();
   const LSemantoSyntacticUnit& primarySsu() const;

   const LgsString& companyCode() const;
   virtual const LgsString& word() const;
   virtual int wordTypeCode() const;
   virtual int wordClassCode() const;
   virtual int wordCount() const;
   virtual int wordID() const;
   virtual int wordLength() const;
   virtual int blackHoleLocation() const;
   virtual int protectionCode() const;
   virtual int hashCode1() const;
   virtual int hashCode2() const;
   virtual int rootHashCode1() const;
   virtual int rootHashCode2() const;
   virtual int hashLocation() const;
   virtual int headLocation() const;
   virtual int henum1() const;
   virtual int henum2() const;
   virtual int rootHenum1() const;
   virtual int rootHenum2() const;
   virtual int meaningID() const;
   virtual int usageID()const;
   virtual int genderCode() const;
   virtual bool matchedOnFullyCapped() const;
   int numericConstraint() const;
   int rootUsageID() const;
   int alternateSequence() const;
   int PATNumber() const;
   int superSetID() const;
   int setID() const;
   int subSetID() const;
   int formCode() const;
   bool isCompoundTerm() const;

   int genericCode() const;
   int atomicCode() const;
   SubjectMatterCode subjectMatterCode() const;

   void setWordClassCode(int);
   void setSuperSetID(int);
   void setSetID(int);
   void setSubSetID(int);
   void setFormCode(int);

   int formCode(int ssuIndex);
   void setUnitFormCode(int);
   void reconcilePrecedingSpaces();
   void setPrecedingSpaces(int value);
   void setTrailingSpaces(int value);

   bool isHyphenated() const;
   void setHyphenated(bool);
   bool isHyphenatedHyphen() const;
   void setHyphenatedHyphen(bool value = true);

   bool isProtectedWord() const;
   void setProtectedWord(bool);
   bool isProtected() const;
   void setProtected(bool);

   virtual int precedingSpaces() const;
   virtual int trailingSpaces() const;

   bool isProtectedFromTranslation() const;

   bool isTranslateable() const;

   virtual bool isQuestionMark();

   virtual bool isUnfoundWord() const;
   void setIsUnfound(bool);

   int ssuCount();
   const LSemantoSyntacticUnit& ssuAt(int) const;

   void makeBeginningOfQuestion();

   void setPrimarySsuPosition(int);

   virtual int translatedWordCount() const;

   void setKeepSourceExpression(bool);		// whether to use the source or transfer expression for this unit at generate (eg, for proper names)
   bool keepSourceExpression();

   void setIsProperName(bool value = true);
   bool isProperName();

   virtual void persistOut(ostream&);
   virtual void persistIn (istream&);
   virtual void getMarkupFromWords();
   void updateSetID(int);

   void display();
   int originalSentencePosition();
   void setOriginalSentencePosition(int position);
   bool headUnit(void) const;
   void setAsHead(void);
   bool nonHeadUnit(void) const;
   void setAsNonHead(void);
   bool compoundUnit(void) const;
   short compoundInfo(void) const;
   void compoundInfo(short sCompoundInfo);

protected:
   virtual int primarySsuPosition() const;

private:
   void propagateOrigSentPosition();

	LDictionaryEntry* p_dictionaryEntry;
	int v_primarySsuPosition;
	Henum* p_henum;
	bool isHyphenated_;
	bool isHyphenatedHyphen_;
	bool isProtectedWord_;
   bool isProtected_;
	int unitFormCode_;
	int precedingSpaces_;
   int trailingSpaces_;
   int origSentPosition_;
};


// --------------------------------------------------------------------------
// Definition of a vector of SourceSentenceUnit objects
// --------------------------------------------------------------------------
class SourceUnitVector: public LgsVector(SourceSentenceUnit*)
{
public:
	virtual ~SourceUnitVector();
	void removeAndDelete(SourceUnitVector::iterator start, 
						 SourceUnitVector::iterator end);
	void removeAndDeleteAll();
	SourceSentenceUnit* at(int);				// return the SSU at the given position in the sentence
	void display(LgsString);								// display on console content of this vector (for testing)
	LgsString generateSentenceSurfaceForm();		// reconstruct the surface form of the source sentence
};


// --------------------------------------------------------------------------
typedef SourceUnitVector::iterator SourceUnitIterator;


// --------------------------------------------------------------------------
inline int SourceSentenceUnit::formCode(int ssuIndex)
{
   if (unitFormCode_)
   {
      return unitFormCode_;
   }
   return dictionaryEntry().derivedFormCodeOf(ssuIndex);
}
//---------------------------------------------------------------------
inline bool SourceSentenceUnit::isCompound(void) const
{
   return dictionaryEntry().isCompound();		// always return false!
}
//---------------------------------------------------------------------
inline void SourceSentenceUnit::setUnitFormCode(int code)
{
   unitFormCode_ = code;
}
//-------------------------------------------------------------------
inline bool SourceSentenceUnit::isProtectedFromTranslation() const
{
   // The unit is protected if either the markup or the word
   // itself indicates that it is protected.
   return markup().isProtected() || isProtectedWord();
}
//---------------------------------------------------------------------
inline int SourceSentenceUnit::meaningID() const
{
   return primarySsu().meaningID();
}
//---------------------------------------------------------------------
inline void SourceSentenceUnit::updateSetID(int n)
{
   LSemantoSyntacticUnit& ssu = const_cast<LSemantoSyntacticUnit&>(primarySsu());
   ssu.updateSetID(n);
}
//---------------------------------------------------------------------
inline int SourceSentenceUnit::wordTypeCode() const
{
   return primarySsu().wordTypeCode();
}
//---------------------------------------------------------------------
inline int SourceSentenceUnit::protectionCode() const
{
   return primarySsu().protectionCode();
}
//---------------------------------------------------------------------
inline int SourceSentenceUnit::hashCode1() const
{
   return primarySsu().hashCode1();
}
//---------------------------------------------------------------------
inline int SourceSentenceUnit::hashCode2() const
{
   return primarySsu().hashCode2();
}
//---------------------------------------------------------------------
inline int SourceSentenceUnit::rootHashCode1() const
{
   return primarySsu().rootHashCode1();
}
//---------------------------------------------------------------------
inline int SourceSentenceUnit::rootHashCode2() const
{
   return primarySsu().rootHashCode2();
}
//---------------------------------------------------------------------
inline int SourceSentenceUnit::rootHenum1() const
{
   return 0;
}
//---------------------------------------------------------------------
inline int SourceSentenceUnit::rootHenum2() const
{
   return 0;
}
//---------------------------------------------------------------------
inline int SourceSentenceUnit::blackHoleLocation() const
{
   return primarySsu().blackHoleLocation();
}
//---------------------------------------------------------------------
inline int SourceSentenceUnit::hashLocation() const
{
   return primarySsu().hashLocation();
}
//---------------------------------------------------------------------
inline int SourceSentenceUnit::headLocation() const
{
   return primarySsu().headWord();
}
//---------------------------------------------------------------------
inline bool SourceSentenceUnit::isQuestionMark()
{
   return primarySsu().isQuestionMark();
}
//---------------------------------------------------------------------
inline void SourceSentenceUnit::makeBeginningOfQuestion()
{
   dictionaryEntry().makeBeginningOfQuestion();
}
//---------------------------------------------------------------------
inline int SourceSentenceUnit::ssuCount()
{
   return dictionaryEntry().countOfSemantoSyntacticUnits();
}
//-------------------------------------------------------------------
inline const LLanguage& SourceSentenceUnit::language() const
{
   return dictionary().sourceLanguage();
}
//---------------------------------------------------------------------
inline void SourceSentenceUnit::setPrimarySsuPosition(int n)
{
   v_primarySsuPosition = n;
}
//---------------------------------------------------------------------
inline const LgsString& SourceSentenceUnit::companyCode() const
{
   return primarySsu().companyCode();
}
//-------------------------------------------------------------------
inline LDictionaryEntry& SourceSentenceUnit::dictionaryEntry() const
{
   return *p_dictionaryEntry;
}
//-------------------------------------------------------------------
inline void SourceSentenceUnit::dictionaryEntry(LDictionaryEntry* entry)
{
   assert(0 == p_dictionaryEntry);
   p_dictionaryEntry = entry;
}
//-------------------------------------------------------------------
inline bool SourceSentenceUnit::isHyphenated() const
{
   return isHyphenated_;
}
//---------------------------------------------------------------------
inline void SourceSentenceUnit::setHyphenated(bool b)
{
   isHyphenated_ = b;
}
//-------------------------------------------------------------------
inline bool SourceSentenceUnit::isHyphenatedHyphen() const
{
   return isHyphenatedHyphen_;
}
//---------------------------------------------------------------------
inline void SourceSentenceUnit::setHyphenatedHyphen(bool b)
{
   isHyphenatedHyphen_ = b;
}
//-------------------------------------------------------------------
inline bool SourceSentenceUnit::isProtectedWord() const
{
   return isProtectedWord_;
}
//---------------------------------------------------------------------
inline void SourceSentenceUnit::setProtectedWord(bool b)
{
   isProtectedWord_ = b;
}
//-------------------------------------------------------------------
inline bool SourceSentenceUnit::isProtected() const
{
   return isProtected_;
}
//---------------------------------------------------------------------
inline void SourceSentenceUnit::setProtected(bool b)
{
   isProtected_ = b;
}
//---------------------------------------------------------------------
inline int SourceSentenceUnit::precedingSpaces() const
{
   return precedingSpaces_;
}
//---------------------------------------------------------------------
inline int SourceSentenceUnit::trailingSpaces() const
{
   return trailingSpaces_;
}
//---------------------------------------------------------------------
inline void SourceSentenceUnit::setPrecedingSpaces(int value)
{
   precedingSpaces_ = value;
   (surfaceWords().begin())->setPrecedingSpaces(value);
}
//---------------------------------------------------------------------
inline void SourceSentenceUnit::setTrailingSpaces(int value)
{
   trailingSpaces_ = value;
   (surfaceWords().end() - 1)->setTrailingSpaces(value);
}
//-------------------------------------------------------------------
inline const LSemantoSyntacticUnit& SourceSentenceUnit::primarySsu() const
{
   return ssuAt(primarySsuPosition());
}
//-------------------------------------------------------------------
inline int SourceSentenceUnit::usageID() const
{
   return primarySsu().usageID();
}
//-------------------------------------------------------------------
inline int SourceSentenceUnit::genderCode() const
{
   return primarySsu().genderCode();
}
//-------------------------------------------------------------------
inline bool SourceSentenceUnit::matchedOnFullyCapped() const
{
   return primarySsu().matchedOnFullyCapped();
}
//---------------------------------------------------------------------
inline int SourceSentenceUnit::wordClassCode() const
{
   return primarySsu().wordClassCode();
}
//---------------------------------------------------------------------
inline int SourceSentenceUnit::numericConstraint() const
{
   return primarySsu().numericConstraint();
}
//---------------------------------------------------------------------
inline int SourceSentenceUnit::rootUsageID() const
{
   return primarySsu().rootUsageID();
}
//---------------------------------------------------------------------
inline int SourceSentenceUnit::alternateSequence() const
{
   return 0;
}
//---------------------------------------------------------------------
inline int SourceSentenceUnit::PATNumber() const
{
   return primarySsu().patNumber();
}
//---------------------------------------------------------------------
inline int SourceSentenceUnit::superSetID() const
{
   return primarySsu().superSetID();
}
//---------------------------------------------------------------------
inline int SourceSentenceUnit::setID() const
{
   return primarySsu().setID();
}
//---------------------------------------------------------------------
inline int SourceSentenceUnit::subSetID() const
{
   return primarySsu().subSetID();
}
//---------------------------------------------------------------------
inline int SourceSentenceUnit::formCode() const
{
   return primarySsu().formCode();
}
//---------------------------------------------------------------------
inline bool SourceSentenceUnit::isCompoundTerm() const
{
   return ((headLocation() > 1) || (hashLocation() > 1));
}
//---------------------------------------------------------------------
// TO BE REMOVED WHEN NEW SMC IS TESTED
inline int SourceSentenceUnit::genericCode() const
{
   return primarySsu().genericCode();
}
//---------------------------------------------------------------------
// TO BE REMOVED WHEN NEW SMC IS TESTED
inline int SourceSentenceUnit::atomicCode() const
{
   return primarySsu().atomicCode();
}
//---------------------------------------------------------------------
inline SubjectMatterCode SourceSentenceUnit::subjectMatterCode() const
{
   return primarySsu().subjectMatterCode();
}
//---------------------------------------------------------------------
inline int SourceSentenceUnit::originalSentencePosition()
{
   return origSentPosition_;
}
//---------------------------------------------------------------------
inline void SourceSentenceUnit::setOriginalSentencePosition(int position)
{
   origSentPosition_ = position;
   propagateOrigSentPosition();
}

inline void SourceSentenceUnit::setAsHead(void)
{
	dictionaryEntry().setAsHead();
}

inline bool SourceSentenceUnit::headUnit(void) const
{
	return dictionaryEntry().headUnit();
}

inline void SourceSentenceUnit::setAsNonHead(void)
{
	dictionaryEntry().setAsNonHead();
}

inline bool SourceSentenceUnit::nonHeadUnit(void) const
{
	return dictionaryEntry().nonHeadUnit();
}

inline bool SourceSentenceUnit::compoundUnit(void) const
{
	return dictionaryEntry().compoundUnit();
}

inline short SourceSentenceUnit::compoundInfo(void) const
{
	return dictionaryEntry().compoundInfo();
}

inline void SourceSentenceUnit::compoundInfo(short sCompoundInfo)
{
	dictionaryEntry().compoundInfo(sCompoundInfo);
}


#endif // __sourcesentenceunit_h__

