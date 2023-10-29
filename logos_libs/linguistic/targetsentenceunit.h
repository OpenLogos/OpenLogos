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
#ifndef __targetsentenceunit_h__
#define __targetsentenceunit_h__

//-------------------------------------------------------------------
// File - targetsentenceunit.h
//
// Class - TargetSentenceUnit
//
// Description - This is the abstract "root" class for Sentence Units
//      that exist on either the source or target side.
//
//-------------------------------------------------------------------

#include <logos_libs/linguistic/sentenceunit.h>
//#include <logos_libs/linguistic/ldictionary.h>
#include <logos_libs/gerdem/scontable.h>

class BlackHole;
class SourceSentenceUnit;
class SourceUnitVector;
class TargetSentenceUnit;

class TargetUnitVector: public LgsVector(TargetSentenceUnit*)
{
public:
   virtual ~TargetUnitVector();

   void removeAndDelete(TargetUnitVector::iterator,TargetUnitVector::iterator);
   void removeAndDeleteAll();
   void display();							// display on console content of this vector (for testing)
};

typedef TargetUnitVector::iterator TargetUnitIterator;

class TargetSentenceUnit: public SentenceUnit
{
public:
   TargetSentenceUnit();
   TargetSentenceUnit(class LDictionary&);
   TargetSentenceUnit(const TargetSentenceUnit&);
   virtual ~TargetSentenceUnit();

   virtual bool isValidFirstWord();
   virtual bool isConstantSentenceUnit();
   virtual const LLanguage& language() const;
   virtual void adjustAsEos();
   virtual void findAssociatedSourceUnit(SourceUnitVector&);
   virtual void reconcileMarkup();
   virtual void updateSourcePrimarySsu();
   virtual void generateDictionaryEntry();
   virtual void generateDictionaryEntry(LgsString&);
   virtual void setSurfaceExpressionFromDictionary();
   virtual void generateStem();
   virtual void assignMarkupToWords();
   virtual void makeAspiration(TargetSentenceUnit* prev);
   virtual int precedingSpaces() const;
   virtual void capitalize (int offset);
   virtual SourceSentenceUnit* sourceSentenceUnit();
   int transferSwitch() const;
   void setTransferSwitch(int);
   virtual bool isBlackHoleStart() const;
   virtual bool isBlackHoleEnd() const;
   virtual int blackHoleSentenceAddress() const;
   virtual void insertBlackHole(const BlackHole*);
   virtual bool isFunctionalConstant() const;
   virtual bool isCloseSpaceFunctionalConstant(TargetUnitIterator prev) const;
   virtual bool isInhibitCapConstant() const;
   virtual const LgsString& word() const;
   virtual bool isQuestionMark() const;
   virtual int usageID() const;
   virtual bool allowsPrecedingSpaces() const;
   virtual bool allowsTrailingSpaces() const;
   virtual bool isOpenBracketPunctuation() const;
   virtual int trailingSpaces() const;
   virtual void completeGenerate();
   void setTrailingSpaces(int);
   void mergeMarkupAndSpaces(const TargetSentenceUnit& unit);
   int blackHoleTargetAddress() const;
   int caseOf() const;
   virtual int declension() const;
   virtual int degree() const;
   int gender() const;
   int number() const;
   int person() const;
   int subSetOf() const;
   int sourceUnitPosition() const;
   int tense() const;
   void setTense(short sValue);
   virtual int wordClassCode() const;
   virtual int patNumber() const;
   void setDeclension(int);
   void setSourceUnitPosition(int);
   void setWordClassCode(int);
   void setInitialUpperCaseFlag(bool);
   void setAllUpperCaseFlag(bool);
   void setHadSpaceClosed(bool);
   bool hadSpaceClosed();
   void zeroSconTable();
   virtual bool isTargetDictionaryUnit();
   virtual bool isUntranslatedSentenceUnit();
   bool isProperName();
   void setIsProperName(bool value = true);
   bool isProperNameTitle();
   bool isProtected();
   void setIsProtected(bool value = true);
	bool sconPointerFound(int sconPointer);
   void setPhraseModifier(short value);
   short phraseModifier();
   virtual int headWord() const;

   //---------------------------------------------------------------
   // The following is machinery for representing the object on non-volatile medium.
   //---------------------------------------------------------------
   virtual void persistOut(ostream&);
   virtual void persistIn (istream&);

   //---------------------------------------------------------------
   void setOpadr(int);
   void setSourcePrimarySsuPosition(int);
   int opadr() const;
   int sourcePrimarySsuPosition();
   void setSconTable(const SconTable& newSconTable);
   const SconTable& getSconTable() const;
   virtual void checkSconValues();
   virtual void findOrigSourceWord(LWordVector origList);

	// set/get particular scons
	// scon 3
	void setDeclensionInSconTable(short newValue) {sconTable.setDeclension(newValue);}
	short getDeclensionInSconTable() {return sconTable.getScon(SconTable::TARGET_DECLENSION_INDEX);}
	// scon 5
	void setNumberInSconTable(short newValue) {sconTable.setNumber(newValue);}
	short getNumberInSconTable() {return sconTable.getScon(SconTable::TARGET_NUMBER_INDEX);}
	// scon 7
	void setCaseInSconTable(short newValue) {sconTable.setCaseOf(newValue);}
	short getCaseInSconTable() {return sconTable.getScon(SconTable::TARGET_CASE_INDEX);}

   void sconPointer(int x, bool appendBack = true);
   void sconPointerVector(LgsVector(int)& x, bool appendBack = true);
   LgsVector(int)& sconPointerVector();
   int sconPointer();
   virtual bool isAspired() const;
   virtual const LgsString& companyCode();
   void setCompanyCode(LgsString& newCC);
   void setCompanyCode(char* newCC);
   void resolveSpaces();
   bool isMerged();
   void setMerged(bool val = true);
   TargetSentenceUnit* prevMergedUnit();
   void setPrevMergedUnit(TargetSentenceUnit* prevUnit);
   TargetSentenceUnit* nextMergedUnit();
   void setNextMergedUnit(TargetSentenceUnit* nextUnit);
   virtual bool isLinkedTargetUnit();
   bool isFlagUnfoundWord();
   void acceptTransfer(bool value);
   bool acceptTransfer() const;

private:
   SconTable sconTable;
   int v_opadr;
   int v_sourcePrimarySsuPosition;
   bool v_initialUpperCaseFlag;
   bool v_allUpperCaseFlag;
   bool v_hadSpaceClosed;
   bool v_acceptTransfer;
   int v_trailingSpaces;
   typedef LgsVector(int) sconVector;
   sconVector sconPointerVector_;
   bool v_isProperName;
   bool v_isProtected;
   short v_phraseModifier;
   LgsString v_companyCode;
   bool v_merged;
   TargetSentenceUnit* v_prevMergeUnit;
   TargetSentenceUnit* v_nextMergeUnit;
};

//---------------------------------------------------------------------
inline int TargetSentenceUnit::headWord() const
{
   return -1;
}
//-------------------------------------------------------------------
inline const LgsString& TargetSentenceUnit::companyCode()
{
   return v_companyCode;
}
//-------------------------------------------------------------------
inline int TargetSentenceUnit::sconPointer()
{
   return *(sconPointerVector_.begin());
}
//-------------------------------------------------------------------
inline LgsVector(int)& TargetSentenceUnit::sconPointerVector()
{
   return sconPointerVector_;
}
//-------------------------------------------------------------------
inline bool TargetSentenceUnit::isConstantSentenceUnit()
{
   return false;
}
//-------------------------------------------------------------------
inline int TargetSentenceUnit::opadr() const
{
   return v_opadr;
}
//-------------------------------------------------------------------
inline void TargetSentenceUnit::setOpadr(int n)
{
   v_opadr = n;
}
//-------------------------------------------------------------------
inline bool TargetSentenceUnit::isQuestionMark() const
{
   return false;
}
//-------------------------------------------------------------------
inline void TargetSentenceUnit::setInitialUpperCaseFlag(bool b)
{
   v_initialUpperCaseFlag = b;
}
//-------------------------------------------------------------------
inline void TargetSentenceUnit::setAllUpperCaseFlag(bool b)
{
   v_allUpperCaseFlag = b;
}
//-------------------------------------------------------------------
inline void TargetSentenceUnit::setSourcePrimarySsuPosition(int n)
{
   v_sourcePrimarySsuPosition = n;
}
//-------------------------------------------------------------------
inline int TargetSentenceUnit::sourcePrimarySsuPosition()
{
   return v_sourcePrimarySsuPosition;
}
//-------------------------------------------------------------------
inline int TargetSentenceUnit::transferSwitch() const
{
   return sconTable.transferSwitch();
}
//-------------------------------------------------------------------
inline void TargetSentenceUnit::setTransferSwitch(int value)
{
   sconTable.setTransferSwitch(value);
}
//-------------------------------------------------------------------
inline void TargetSentenceUnit::setTrailingSpaces (int n)
{
   v_trailingSpaces = n;
}
//-------------------------------------------------------------------
inline int TargetSentenceUnit::blackHoleTargetAddress() const
{
   return sconTable.blackHoleTargetAddress();
}
//-------------------------------------------------------------------
inline int TargetSentenceUnit::caseOf() const
{
   return sconTable.caseOf();
}
//-------------------------------------------------------------------
inline int TargetSentenceUnit::declension() const
{
   return sconTable.declension();
}
//-------------------------------------------------------------------
inline void TargetSentenceUnit::setDeclension(int value)
{
   sconTable.setDeclension(value);
}
//-------------------------------------------------------------------
inline int TargetSentenceUnit::degree() const
{
   return sconTable.degree();
}
//-------------------------------------------------------------------
inline int TargetSentenceUnit::gender() const
{
   return sconTable.gender();
}
//-------------------------------------------------------------------
inline int TargetSentenceUnit::number() const
{
   return sconTable.number();
}
//-------------------------------------------------------------------
inline int TargetSentenceUnit::person() const
{
   return sconTable.person();
}
//-------------------------------------------------------------------
inline int TargetSentenceUnit::subSetOf() const
{
   return sconTable.subSetOf();
}
//-------------------------------------------------------------------
inline int TargetSentenceUnit::sourceUnitPosition() const
{
   return sconTable.sourceSentenceUnitPosition();
}
//-------------------------------------------------------------------
inline void TargetSentenceUnit::setSourceUnitPosition(int value)
{
   sconTable.setSourceSentenceUnitPosition(value);
}
//-------------------------------------------------------------------
inline int TargetSentenceUnit::tense() const
{
   return sconTable.tense();
}
//-------------------------------------------------------------------
inline void TargetSentenceUnit::setTense(short sValue)
{
   sconTable.setTense(sValue);
}
//-------------------------------------------------------------------
inline void TargetSentenceUnit::setHadSpaceClosed(bool b)
{
   v_hadSpaceClosed = b;
}
//-------------------------------------------------------------------
inline bool TargetSentenceUnit::hadSpaceClosed()
{
   return v_hadSpaceClosed;
}
//-------------------------------------------------------------------
inline void TargetSentenceUnit::zeroSconTable()
{
   sconTable.zeroSconTable();
}
//-------------------------------------------------------------------
inline void TargetSentenceUnit::checkSconValues()
{
   sconTable.checkSconValues();
}
//-------------------------------------------------------------------
inline void TargetSentenceUnit::completeGenerate()
{
}
//-------------------------------------------------------------------
inline bool TargetSentenceUnit::isTargetDictionaryUnit()
{
   return false;
}
//-------------------------------------------------------------------
inline void TargetSentenceUnit::findOrigSourceWord(LWordVector origList)
{
}
//-------------------------------------------------------------------
inline bool TargetSentenceUnit::isUntranslatedSentenceUnit()
{
   return false;
}
//-------------------------------------------------------------------
inline bool TargetSentenceUnit::isProperName()
{
   return v_isProperName;
}
//-------------------------------------------------------------------
inline void TargetSentenceUnit::setIsProperName(bool value)
{
   v_isProperName = value;
}
//-------------------------------------------------------------------
inline bool TargetSentenceUnit::isProperNameTitle()
{
   if (v_isProperName && (subSetOf() == 807))
   {
      return true;
   }
   return false;
}
//-------------------------------------------------------------------
inline bool TargetSentenceUnit::isProtected()
{
   return v_isProtected;
}
//-------------------------------------------------------------------
inline void TargetSentenceUnit::setIsProtected(bool value)
{
   v_isProtected = value;
}
//-------------------------------------------------------------------
inline SourceSentenceUnit* TargetSentenceUnit::sourceSentenceUnit()
{
   return 0;
}
//-------------------------------------------------------------------
inline void TargetSentenceUnit::setPhraseModifier(short value)
{
   v_phraseModifier = value;
}
//-------------------------------------------------------------------
inline short TargetSentenceUnit::phraseModifier()
{
   return v_phraseModifier;
}
//---------------------------------------------------------------------
inline bool TargetSentenceUnit::isAspired() const
{
   return false;
}
//---------------------------------------------------------------------
inline bool TargetSentenceUnit::isMerged()
{
   return v_merged;
}
//---------------------------------------------------------------------
inline void TargetSentenceUnit::setMerged(bool val)
{
   v_merged = val;
}
//---------------------------------------------------------------------
inline TargetSentenceUnit* TargetSentenceUnit::prevMergedUnit()
{
   return v_prevMergeUnit;
}
//---------------------------------------------------------------------
inline void TargetSentenceUnit::setPrevMergedUnit(TargetSentenceUnit* prevUnit)
{
   v_prevMergeUnit = prevUnit;
}
//---------------------------------------------------------------------
inline TargetSentenceUnit* TargetSentenceUnit::nextMergedUnit()
{
   return v_nextMergeUnit;
}
//---------------------------------------------------------------------
inline void TargetSentenceUnit::setNextMergedUnit(TargetSentenceUnit* nextUnit)
{
   v_nextMergeUnit = nextUnit;
}
//---------------------------------------------------------------------
inline const LgsString& TargetSentenceUnit::word() const
{
   return surfaceExpressionAsString();
}
//-------------------------------------------------------------------
inline bool TargetSentenceUnit::isLinkedTargetUnit()
{
   return true;
}
//-------------------------------------------------------------------
inline void TargetSentenceUnit::acceptTransfer(bool value)
{
   v_acceptTransfer = value;
}
//-------------------------------------------------------------------
inline bool TargetSentenceUnit::acceptTransfer() const
{
   return v_acceptTransfer;
}

#endif // __targetsentenceunit_h__

