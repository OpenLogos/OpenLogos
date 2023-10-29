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
// -------------------------------------------------------------------------------
// File - LSemantoSyntacticUnit.h
//
// Class - LSemantoSyntacticUnit (interface)
// -------------------------------------------------------------------------------

#ifndef __LSemantoSyntacticUnit_h__
#define __LSemantoSyntacticUnit_h__

#include <logos_libs/linguistic/context.h>
#include <logos_libs/linguistic/llanguage.h>
#include <logos_libs/SubjectMatterCodes/SubjectMatterCode.h>

class LLanguage;
class LInflection;

// -------------------------------------------------------------------------------
// Definition of a semanto-syntactic unit
// -------------------------------------------------------------------------------
class LSemantoSyntacticUnit
{
	friend class ContextCompare;
public:
   DummyLess(LSemantoSyntacticUnit);
   DummyNotEqual(LSemantoSyntacticUnit);

   // ----------------------------------------------------------------------------
   // This is a static ("Meta Class") member function. It is the only way that a
   // GSentence object can be created. The GSentence object that is returned is
   // complete with all the GDictionaryEntry objects. The GDictionaryEntry objects
   // are complete with their LSemantoSyntacticUnit objects.
   // The returned object is ready to return an RSentence object which should be
   // readable by the RES subsystem.
   // ----------------------------------------------------------------------------
   static LSemantoSyntacticUnit* create();
   static int unfoundWordID();

   // ----------------------------------------------------------------------------
   // Public constructors and desctructor. Assures proper copying and default
   // states for all SSU objects.
   // ----------------------------------------------------------------------------
   LSemantoSyntacticUnit();
   LSemantoSyntacticUnit(const LSemantoSyntacticUnit&);
   virtual ~LSemantoSyntacticUnit();

   // ----------------------------------------------------------------------------
   // oid() - an arbitrary identification number given to this unit that uniquely
   //      identifies from among other SSU objects.
   // ----------------------------------------------------------------------------
   int oid() const;

   // ----------------------------------------------------------------------------
   // contextValue() - This is a weight that is given the unit based upon its
   //     company and SMC codes and related SMC codes.
   // ----------------------------------------------------------------------------
   int contextValue(const LLanguage&);

   // ----------------------------------------------------------------------------
   // language() - an SSU object has only 1 language. It carries the source or
   //     target language depending upon where it is used.
   // ----------------------------------------------------------------------------
   const LLanguage& language() const;
   void setLanguage(const LLanguage*);

   // ----------------------------------------------------------------------------
   // isSourceTransferred() - reflects whether this SSU is actually transferred
   //     directly into the target language. Contrary, to the original intent of
   //     Bud Scott, the source information is actually dependent upon the target
   //     language and this is the point where that dependency reflects itself.
   // ----------------------------------------------------------------------------
   bool isSourceTransferred() const;
   void setIsSourceTransferred(bool isTransferred = true);

   // ----------------------------------------------------------------------------
   // derivedFormCode() - This generates a derived form code to be used in place
   //     of the regular form code. The derived form code is based upon the
   //     inflection used and the pat and stem of the unit.
   // isFormDeriveable() - Either a matching derived form exists or the word has
   //     a pat of 0 and no inflection.
   // ----------------------------------------------------------------------------
   void derivedFormCode(const LInflection* inflection, bool germanNonHead = false);
   int derivedFormCode(void) const;
   bool isFormDeriveable(const LInflection* inflection) const;

   // ----------------------------------------------------------------------------
   // isQuestionMark() - true if this unit represents a question mark.
   // makeBeginningOfQuestion() - simply sets a flag in this SSU that indicates
   //     that this SSU is at the beginning of a question.
   // ----------------------------------------------------------------------------
   bool isQuestionMark() const;
   void makeBeginningOfQuestion();

   // ----------------------------------------------------------------------------
   // The following methods are the getters for the database data. These need to
   // be public because this data accessed by legacy code that is not object-oriented.
   // ----------------------------------------------------------------------------
   const LgsString& companyCode() const;
   int wordID() const;
   int wordCount() const;
   const LgsString& word() const;
   const LgsString& targetWord() const;
   int wordTypeCode() const;
   int protectionCode() const;
   int hashCode1() const;
   int hashCode2() const;
   int rootHashCode1() const;
   int rootHashCode2() const;
   int henNum1() const;
   int henNum2() const;
   int rootHenNum1() const;
   int rootHenNum2() const;
   int hashLocation() const;
   int headWord() const;
   int blackHoleLocation() const;
   int wildcardPosition() const;
   bool isAspire() const;
   bool isMonth() const;
   bool isPrefix() const;
   bool isNeverEos() const;
   bool isOrdinal() const;

   int wordClassCode() const;
   int genderCode() const;
   int numericConstraint() const;
   int auxiliaryCode() const;
   int inflectionPosition() const;
   int verbPrefixInseparable() const;
   int verbPrefixSeparable() const;
   int verbPrefixInsepLength() const;
   int verbPrefixSepLength() const;
   int patNumber() const;
   int sourceStemNumber() const;
   int rootUsageID() const;
   bool sourceAnalysisCode() const;

   int meaningID() const;
   int usageID() const;
   int priorityOrder() const;
   int atomicCode() const;          // TO BE REMOVED WHEN NEW SMC IS TESTED
   int genericCode() const;         // TO BE REMOVED WHEN NEW SMC IS TESTED
   SubjectMatterCode subjectMatterCode() const;
   int superSetID() const;
   int setWordClassCode() const;
   int setID() const;
   int superWordClassCode() const;
   int subSetID() const;
   int formCode() const;
   int overflow2b() const;
   int overflow3b() const;

   bool transitivity() const;
   bool isHomograph() const;
   bool isPrimary() const;
   bool isDeactivated() const;
   bool isMiscoded() const;
   bool matchedOnFullyCapped() const;

   // ----------------------------------------------------------------------------
   // The following methods are the setters for the database data. These need to
   // be public because this data accessed by legacy code that is not object-oriented.
   // ----------------------------------------------------------------------------
   void setCompanyCode(const LgsString&);
   void setWordID(int);
   void setWordCount(int);
   void setWord(const LgsString&);
   void setTargetWord(const LgsString&);
   void setWordTypeCode(int);
   void setProtectionCode(int);
   void setHashCode1(int);
   void setHashCode2(int);
   void setRootHashCode1(int);
   void setRootHashCode2(int);
   void setHenNum1(int);
   void setHenNum2(int);
   void setRootHenNum1(int);
   void setRootHenNum2(int);
   void setHashLocation(int);
   void setHeadWord(int);
   void setBlackHoleLocation(int);
   void setWildcardPosition(int);
   void setIsAspire(bool);
   void setIsMonth(bool);
   void setIsPrefix(bool);
   void setIsNeverEos(bool);
   void setIsOrdinal(bool);

   void setWordClassCode(int);
   void setGenderCode(int);
   void setNumericConstraint(int);
   void setAuxiliaryCode(int);
   void setInflectionPosition(int);
   void setVerbPrefixInseparable(int);
   void setVerbPrefixSeparable(int);
   void setVerbPrefixInsepLength(int);
   void setVerbPrefixSepLength(int);
   void setPatNumber(int);
   void setSourceStemNumber(int);
   void setRootUsageID(int);
   void setSourceAnalysisCode(bool);

   void setMeaningID(int);
   void setUsageID(int);
   void setPriorityOrder(int);
   void setAtomicCode(int);      // TO BE REMOVED WHEN NEW SMC IS TESTED
   void setGenericCode(int);     // TO BE REMOVED WHEN NEW SMC IS TESTED
   void setSubjectMatterCode(SubjectMatterCode aSMC);
   void setSuperSetID(int);
   void setSetWordClassCode(int);
   void setSetID(int);
   void setSuperWordClassCode(int);
   void setSubSetID(int);
   void setFormCode(int);
   void setOverflow2b(int);
   void setOverflow3b(int);
   void setTransitivity(bool);
   void setIsHomograph(bool);
   void setIsPrimary(bool);
   void setIsDeactivated(bool);
   void setIsMiscoded(bool);
   void setMatchedOnFullyCapped(bool);
   bool useHashForHenum(void) const;
 
   // ----------------------------------------------------------------------------
   // The following are some typical operator overloads.
   // ----------------------------------------------------------------------------
   const LSemantoSyntacticUnit& operator=(const LSemantoSyntacticUnit&);
   bool operator==(const LSemantoSyntacticUnit&) const;
 
   // ----------------------------------------------------------------------------
   // updateSetID() - This method is really only used when a unit is discovered
   //     to be the EOS unit. The SetID is dynamically set, overriding the DB
   //     information.
   // ----------------------------------------------------------------------------
   void updateSetID(int);
 
protected: 
   // ----------------------------------------------------------------------------
   // copyComponents() - simply copies that database information from another SSU. 
   // ----------------------------------------------------------------------------
   void copyComponents(const LSemantoSyntacticUnit&);

   // ----------------------------------------------------------------------------
   // findContextValue() - This method goes to the language object to determine
   // the context value of the object. This is only called when the SSU's context
   // value is unset.
   // ----------------------------------------------------------------------------
   void setOid();
   void findContextValue(const LLanguage&);

private:
   // ----------------------------------------------------------------------------
   // p_language - a pointer to the language object that this unit belongs to
   // (could be source or target).
   // ----------------------------------------------------------------------------
   const LLanguage* p_language;
   int v_contextValue;
   bool v_isSourceTransferred;
   int v_oid;
   static int st_nextOid;
   static int st_unfoundWordID;

   LgsString companyCode_;
   int wordID_;
   int wordCount_;
   LgsString word_;
   LgsString targetWord_;
   int wordTypeCode_;
   int protectionCode_;
   int hashCode1_;
   int hashCode2_;
   int rootHashCode1_;
   int rootHashCode2_;
   int henNum1_;
   int henNum2_;
   int rootHenNum1_;
   int rootHenNum2_;
   int hashLocation_;
   int headWord_;
   int blackHoleLocation_;
   int wildcardPosition_;
   bool isAspire_;
   bool isMonth_;
   bool isPrefix_;
   bool isNeverEos_;
   bool isOrdinal_;

   int wordClassCode_;
   int genderCode_;
   int numericConstraint_;
   int auxiliaryCode_;
   int inflectionPosition_;
   int verbPrefixInseparable_;
   int verbPrefixSeparable_;
   int verbPrefixInsepLength_;
   int verbPrefixSepLength_;
   int patNumber_;
   int sourceStemNumber_;
   int rootUsageID_;
   bool sourceAnalysisCode_;

   int meaningID_;
   int usageID_;
   int priorityOrder_;
   int atomicCode_;        // TO BE REMOVED WHEN NEW SMC IS TESTED
   int genericCode_;       // TO BE REMOVED WHEN NEW SMC IS TESTED
   SubjectMatterCode smc_;
   int superSetID_;
   int setWordClassCode_;
   int setID_;
   int superWordClassCode_;
   int subSetID_;
   int formCode_;
   int overflow2b_;
   int overflow3b_;

   bool isHomograph_;
   bool isPrimary_;
   bool isDeactivated_;
   bool isMiscoded_;
   bool transitivity_;
   bool matchedOnFullyCapped_;
};

typedef LgsVector(LSemantoSyntacticUnit) LSsuVector;
typedef LgsVector(LSemantoSyntacticUnit)::iterator LSsuIterator;

class ContextCompare
{
public:
	bool operator()(const LSemantoSyntacticUnit & lhs, const LSemantoSyntacticUnit & rhs)
	{
		return lhs.v_contextValue > rhs.v_contextValue;
	}
};

// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::oid() const
{
   return v_oid;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setOid()
{
   v_oid = st_nextOid++;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::contextValue(const LLanguage& sourceLanguage)
{
   if (Context::InvalidValue == v_contextValue)
   {
      findContextValue(sourceLanguage);
   }
   return v_contextValue;
}
// ----------------------------------------------------------------------------
inline bool LSemantoSyntacticUnit::isQuestionMark() const
{
   return (LLanguage::PUNCTUATION == wordClassCode()) && (9 == formCode());
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::makeBeginningOfQuestion()
{
   subSetID_ = 909;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::updateSetID(int n)
{
   setID_ = n;
}
// ----------------------------------------------------------------------------
inline bool LSemantoSyntacticUnit::isSourceTransferred() const
{
   return v_isSourceTransferred;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setIsSourceTransferred(bool isTransferred)
{
   v_isSourceTransferred = isTransferred;
}
// ----------------------------------------------------------------------------
inline const LLanguage& LSemantoSyntacticUnit::language() const
{
   return *p_language;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setLanguage(const LLanguage* aLanguage)
{
   p_language = aLanguage;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::priorityOrder() const
{
   return priorityOrder_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::blackHoleLocation() const
{
   return blackHoleLocation_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::hashLocation() const
{
   return hashLocation_;
}
// ----------------------------------------------------------------------------
inline const LgsString& LSemantoSyntacticUnit::companyCode() const
{
   return companyCode_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::formCode() const
{
   return formCode_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::overflow2b() const
{
   return overflow2b_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::overflow3b() const
{
   return overflow3b_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::hashCode1() const
{
   return hashCode1_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::hashCode2() const
{
   return hashCode2_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::rootHashCode1() const
{
   return rootHashCode1_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::rootHashCode2() const
{
   return rootHashCode2_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::henNum1() const
{
   return henNum1_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::henNum2() const
{
   return henNum2_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::rootHenNum1() const
{
   return rootHenNum1_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::rootHenNum2() const
{
   return rootHenNum2_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::wordTypeCode() const
{
   return wordTypeCode_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::protectionCode() const
{
   return protectionCode_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::wildcardPosition() const
{
   return wildcardPosition_;
}
// ----------------------------------------------------------------------------
inline bool LSemantoSyntacticUnit::isAspire() const
{
   return isAspire_;
}
// ----------------------------------------------------------------------------
inline bool LSemantoSyntacticUnit::isMonth() const
{
   return isMonth_;
}
// ----------------------------------------------------------------------------
inline bool LSemantoSyntacticUnit::isPrefix() const
{
   return isPrefix_;
}
// ----------------------------------------------------------------------------
inline bool LSemantoSyntacticUnit::isNeverEos() const
{
   return isNeverEos_;
}
// ----------------------------------------------------------------------------
inline bool LSemantoSyntacticUnit::isOrdinal() const
{
   return isOrdinal_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::genderCode() const
{
   return genderCode_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::numericConstraint() const
{
   return numericConstraint_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::auxiliaryCode() const
{
   return auxiliaryCode_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::inflectionPosition() const
{
   return inflectionPosition() - 1;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::verbPrefixInseparable() const
{
   if (verbPrefixInseparable_)
   {
      return verbPrefixInseparable_ - 1;
   }
   return 0;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::verbPrefixSeparable() const
{
   if (verbPrefixSeparable_)
   {
      return verbPrefixSeparable_ - 1;
   }
   return 0;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::verbPrefixInsepLength() const
{
   return verbPrefixInsepLength_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::verbPrefixSepLength() const
{
   return verbPrefixSepLength_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::rootUsageID() const
{
   return rootUsageID_;
}
// ----------------------------------------------------------------------------
inline bool LSemantoSyntacticUnit::sourceAnalysisCode() const
{
   return sourceAnalysisCode_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::superSetID() const
{
   return superSetID_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::setWordClassCode() const
{
   return setWordClassCode_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::setID() const
{
   return setID_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::superWordClassCode() const
{
   return superWordClassCode_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::subSetID() const
{
   return subSetID_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::patNumber() const
{
   return patNumber_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::sourceStemNumber() const
{
   return sourceStemNumber_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::wordID() const
{
   return wordID_;
}
// ----------------------------------------------------------------------------
inline const LgsString& LSemantoSyntacticUnit::word() const
{
   return word_;
}
// ----------------------------------------------------------------------------
inline const LgsString& LSemantoSyntacticUnit::targetWord() const
{
   return targetWord_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::wordCount() const
{
   return wordCount_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::wordClassCode() const
{
   return wordClassCode_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::meaningID() const
{
   return meaningID_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::usageID() const
{
   return usageID_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::atomicCode() const     // TO BE REMOVED WHEN NEW SMC IS TESTED
{
   return atomicCode_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::genericCode() const    // TO BE REMOVED WHEN NEW SMC IS TESTED
{
   return genericCode_;
}
// ----------------------------------------------------------------------------
inline bool LSemantoSyntacticUnit::transitivity() const
{
   return transitivity_;
}
// ----------------------------------------------------------------------------
inline bool LSemantoSyntacticUnit::matchedOnFullyCapped() const
{
   return matchedOnFullyCapped_;
}
// ----------------------------------------------------------------------------
inline bool LSemantoSyntacticUnit::isHomograph() const
{
   return isHomograph_;
}
// ----------------------------------------------------------------------------
inline bool LSemantoSyntacticUnit::isPrimary() const
{
   return isPrimary_;
}
// ----------------------------------------------------------------------------
inline bool LSemantoSyntacticUnit::isDeactivated() const
{
   return isDeactivated_;
}
// ----------------------------------------------------------------------------
inline bool LSemantoSyntacticUnit::isMiscoded() const
{
   return isMiscoded_;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::unfoundWordID()
{
   return st_unfoundWordID;
}
// ----------------------------------------------------------------------------
inline int LSemantoSyntacticUnit::headWord() const
{
   return headWord_;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setCompanyCode(const LgsString& rhs)
{
   companyCode_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setWordID(int rhs)
{
   wordID_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setWordCount(int rhs)
{
   wordCount_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setWord(const LgsString& rhs)
{
   word_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setTargetWord(const LgsString& rhs)
{
   targetWord_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setWordTypeCode(int rhs)
{
   wordTypeCode_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setProtectionCode(int rhs)
{
   protectionCode_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setHashCode1(int rhs)
{
   hashCode1_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setHashCode2(int rhs)
{
   hashCode2_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setRootHashCode1(int rhs)
{
	rootHashCode1_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setRootHashCode2(int rhs)
{
	rootHashCode2_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setHenNum1(int rhs)
{
	henNum1_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setHenNum2(int rhs)
{
	henNum2_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setRootHenNum1(int rhs)
{
	rootHenNum1_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setRootHenNum2(int rhs)
{
	rootHenNum2_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setHashLocation(int rhs)
{
   hashLocation_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setHeadWord(int rhs)
{
   headWord_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setBlackHoleLocation(int rhs)
{
   blackHoleLocation_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setWildcardPosition(int rhs)
{
   wildcardPosition_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setIsAspire(bool rhs)
{
   isAspire_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setIsMonth(bool rhs)
{
   isMonth_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setIsPrefix(bool rhs)
{
   isPrefix_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setIsNeverEos(bool rhs)
{
   isNeverEos_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setIsOrdinal(bool rhs)
{
   isOrdinal_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setWordClassCode(int rhs)
{
   wordClassCode_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setGenderCode(int rhs)
{
   genderCode_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setNumericConstraint(int rhs)
{
   numericConstraint_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setAuxiliaryCode(int rhs)
{
   auxiliaryCode_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setInflectionPosition(int rhs)
{
   inflectionPosition_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setVerbPrefixInseparable(int rhs)
{
   verbPrefixInseparable_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setVerbPrefixSeparable(int rhs)
{
   verbPrefixSeparable_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setVerbPrefixInsepLength(int rhs)
{
   verbPrefixInsepLength_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setVerbPrefixSepLength(int rhs)
{
   verbPrefixSepLength_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setPatNumber(int rhs)
{
   patNumber_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setSourceStemNumber(int rhs)
{
   sourceStemNumber_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setRootUsageID(int rhs)
{
   rootUsageID_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setSourceAnalysisCode(bool rhs)
{
   sourceAnalysisCode_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setMeaningID(int rhs)
{
   meaningID_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setUsageID(int rhs)
{
   usageID_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setPriorityOrder(int rhs)
{
   priorityOrder_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setAtomicCode(int rhs)      // TO BE REMOVED WHEN NEW SMC IS TESTED
{
   atomicCode_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setGenericCode(int rhs)     // TO BE REMOVED WHEN NEW SMC IS TESTED
{
   genericCode_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setSuperSetID(int rhs)
{
   superSetID_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setSetWordClassCode(int rhs)
{
   setWordClassCode_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setSetID(int rhs)
{
   setID_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setSuperWordClassCode(int rhs)
{
   superWordClassCode_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setSubSetID(int rhs)
{
   subSetID_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setFormCode(int rhs)
{
   formCode_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setOverflow2b(int rhs)
{
   overflow2b_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setOverflow3b(int rhs)
{
   overflow3b_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setTransitivity(bool rhs)
{
   transitivity_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setIsHomograph(bool rhs)
{
   isHomograph_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setIsPrimary(bool rhs)
{
   isPrimary_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setIsDeactivated(bool rhs)
{
   isDeactivated_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setIsMiscoded(bool rhs)
{
   isMiscoded_ = rhs;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setMatchedOnFullyCapped(bool rhs)
{
   matchedOnFullyCapped_ = rhs;
}
// ----------------------------------------------------------------------------
inline SubjectMatterCode LSemantoSyntacticUnit::subjectMatterCode() const
{
   return smc_;
}
// ----------------------------------------------------------------------------
inline void LSemantoSyntacticUnit::setSubjectMatterCode(SubjectMatterCode aSMC)
{
   smc_=aSMC;
}

#endif // __LSemantoSyntacticUnit_h__

