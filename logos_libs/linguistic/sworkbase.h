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
// File - SWorkBase.h (interface)
//
// Description - abase.
// --------------------------------------------------------------------------

#ifndef __SWorkBase_h__
#define __SWorkBase_h__


#include <logos_libs/SubjectMatterCodes/SubjectMatterCode.h>


// --------------------------------------------------------------------------
// Class - SWork (interface)
// --------------------------------------------------------------------------
class SWorkBase : public Object
{
public:
   DummyLess(SWorkBase);
   DummyEqual(SWorkBase);
   SWorkBase();
   virtual ~SWorkBase();

   static bool IsValidSetID(int);
   static bool IsValidSubsetID(int);
   static bool IsValidSupersetID(int);

   int BlackHoleLocation() const;
   void BlackHoleLocation(int);
   const LgsString& HeadWord();
   const LgsString HeadWordInUpperCase();
   void HeadWord(const LgsString&);
   int HeadWordLocation() const;
   void HeadWordLocation(int);
   int HashLocation() const;
   void HashLocation(int);
   int Henum1() const;
   void Henum1(int);
   int Henum2() const;
   void Henum2(int);
   int rootHenum1() const;
   void rootHenum1(int);
   int rootHenum2() const;
   void rootHenum2(int);
   int IsFound() const;
   void IsFound(int);
   int IsProtected() const;
   void IsProtected(int);
   int PrimarySsu() const;
   void PrimarySsu(int);
   int SourcePosition() const;
   void SourcePosition(int);
   int SsuCount() const;
   void SsuCount(int);
   const LgsString& Word() const;
   const LgsString WordInUpperCase() const;
   void Word(const LgsString&);
   int WordCount() const;
   void WordCount(int);
   int WordLength() const;
   int capitalizationState() const;
   void setCapitalizationState(int);
   bool isEndOfSentence() const;
   void setEndOfSentence(bool);
   int precedingSpaces() const;
   void setPrecedingSpaces(int);

   bool isBold() const;
   bool isItalic() const;
   bool isUnderlined() const;
   bool isSingleQuoted() const;
   bool isDoubleQuoted() const;
   const LgsString& CompanyCode(size_t) const;
   void CompanyCode(size_t, const LgsString&);
   int AuxiliaryCode(size_t) const;
   void AuxiliaryCode(size_t, int);
   int FormCode(size_t) const;
   void FormCode(size_t, int);
   int Overflow2b(size_t) const;
   void Overflow2b(size_t, int);
   int Overflow3b(size_t) const;
   void Overflow3b(size_t, int);
   int Gender(size_t) const;
   void Gender(size_t, int);
   int HashCode1(size_t) const;
   void HashCode1(size_t, int);
   int rootHashCode1(size_t) const;
   void rootHashCode1(size_t, int);
   int HashCode2(size_t) const;
   void HashCode2(size_t, int);
   int rootHashCode2(size_t) const;
   void rootHashCode2(size_t, int);
   int IsTransferred(size_t) const;
   void IsTransferred(size_t, int);
   int MatchedOnFullyCapped(size_t) const;
   void MatchedOnFullyCapped(size_t, int);
   int MeaningID(size_t) const;
   void MeaningID(size_t, int);
   int PatNumber(size_t) const;
   void PatNumber(size_t, int);
   int ProtectionCode(size_t) const;
   void ProtectionCode(size_t, int);
   int SetID(size_t) const;
   void SetID(size_t, int);
   int SourceStemNumber(size_t) const;
   void SourceStemNumber(size_t, int);
   int SubsetID(size_t) const;
   void SubsetID(size_t, int);
   int SupersetID(size_t) const;
   void SupersetID(size_t, int);
   const LgsString& CanonicalWord(size_t) const;
   void CanonicalWord(size_t, const LgsString&);
   const LgsString& TargetWord(size_t) const;
   void TargetWord(size_t, const LgsString&);
   int TypeID(size_t) const;
   int UsageID(size_t) const;
   void UsageID(size_t, int);
   int WordClass(size_t) const;
   void WordClass(size_t, int);
   int WordID(size_t) const;
   void WordID(size_t, int);
   int WordTypeCode(size_t) const;
   void WordTypeCode(size_t, int);

   void WriteOutEmptySsu();

   int GenericCode(size_t i) const {return v_genericCode[i];}		// TO BE REMOVED WHEN NEW SMC IS TESTED
   void GenericCode(size_t i, int x) {v_genericCode[i]=x;}			// TO BE REMOVED WHEN NEW SMC IS TESTED
   int AtomicCode(size_t i) const {return v_atomicCode[i];}			// TO BE REMOVED WHEN NEW SMC IS TESTED
   void AtomicCode(size_t i, int x) {v_atomicCode[i]=x;}			// TO BE REMOVED WHEN NEW SMC IS TESTED

   void setSubjectMatterCode(short int, SubjectMatterCode);		// set the i-th SMC
   SubjectMatterCode subjectMatterCode(short int) const;		// return the i-th SMC
   void displaySMCs();

   friend char* streamOutSWorkInfo(char * dest, SWorkBase& object);
   friend char* streamInSWorkInfo(char * source, SWorkBase& object);
   void display(ostream&);
   void position(int);
   int position() const;
   void CompoundInfo(short sCompoundInfo);
   short CompoundInfo(void) const;

protected:
   void InitializeHeadWord();

   LgsString v_buffer;

   int v_blackHoleLocation;
   LgsString v_headWord;
   int v_headWordLocation;
   int v_hashLocation;
   int v_henum1;
   int v_henum2;
   int v_rootHenum1;
   int v_rootHenum2;
   int v_isFound;
   int v_isProtected;
   int v_primarySsu;
   int v_sourcePosition;
   int v_ssuCount;
   LgsString v_word;
   int v_wordCount;
   bool v_isHeadWordBuilt;

   bool v_isBold;
   bool v_isItalic;
   bool v_isUnderlined;
   bool v_isSingleQuoted;
   bool v_isDoubleQuoted;

   LgsString v_companyCode[3];
   LgsString v_canonicalWord[3]; 
   LgsString v_targetWord[3];
   int v_formCode[3];
   int v_overflow2b[3];
   int v_overflow3b[3];
   int v_gender[3];
   int v_hashCode1[3];
   int v_rootHashCode1[3];
   int v_hashCode2[3];
   int v_rootHashCode2[3];
   int v_isTransferred[3];
   int v_matchedOnFullyCapped[3];
   int v_meaningID[3];
   int v_patNumber[3];
   int v_protectionCode[3];
   int v_setID[3];
   int v_sourceStemNumber[3];
   int v_subsetID[3];
   int v_supersetID[3];
   int v_usageID[3];
   int v_wordClass[3];
   int v_wordID[3];
   int v_wordTypeCode[3];
   int v_auxiliaryCode[3];
   int v_atomicCode[3];							// TO BE REMOVED WHEN NEW SMC IS TESTED
   int v_genericCode[3];						// TO BE REMOVED WHEN NEW SMC IS TESTED
   SubjectMatterCode subjectMatterCode_[3];

   int v_position;
   int v_capitalizationState;
   int v_precedingSpaces;
   bool v_isEof;
   short v_compoundInfo;

   static int st_lowSetID;
   static int st_highSetID;
   static int st_lowSubsetID;
   static int st_highSubsetID;
   static int st_lowSupersetID;
   static int st_highSupersetID;
};

//-------------------------------------------------------------------
typedef LgsVector(SWorkBase) SWorkBaseVector;

//-------------------------------------------------------------------
inline int SWorkBase::capitalizationState() const
{
   return v_capitalizationState;
}

inline void SWorkBase::setCapitalizationState(int s)
{
   v_capitalizationState = s;
}
//-------------------------------------------------------------------
inline bool SWorkBase::isEndOfSentence() const
{
   return v_isEof;
}

inline void SWorkBase::setEndOfSentence(bool b)
{
   v_isEof = b;
}
//---------------------------------------------------------------------
inline int SWorkBase::BlackHoleLocation() const
{
   return v_blackHoleLocation;
}

inline void SWorkBase::BlackHoleLocation(int rhs)
{
   v_blackHoleLocation = rhs;
}
//---------------------------------------------------------------------
inline const LgsString& SWorkBase::CompanyCode(size_t i) const
{
   return v_companyCode[i];
}

inline void SWorkBase::CompanyCode(size_t i, const LgsString& rhs)
{
   v_companyCode[i] = rhs;
}
//---------------------------------------------------------------------
inline const LgsString& SWorkBase::CanonicalWord(size_t i) const
{
   return v_canonicalWord[i];
}

inline void SWorkBase::CanonicalWord(size_t i, const LgsString& rhs)
{
   v_canonicalWord[i] = rhs;
}
//---------------------------------------------------------------------
inline const LgsString& SWorkBase::TargetWord(size_t i) const
{
   return v_targetWord[i];
}

inline void SWorkBase::TargetWord(size_t i, const LgsString& rhs)
{
   v_targetWord[i] = rhs;
}
//---------------------------------------------------------------------
inline int SWorkBase::AuxiliaryCode(size_t i) const
{
   return v_auxiliaryCode[i];
}

inline void SWorkBase::AuxiliaryCode(size_t i, int rhs)
{
   v_auxiliaryCode[i] = rhs;
}
//---------------------------------------------------------------------
inline int SWorkBase::FormCode(size_t i) const
{
   return v_formCode[i];
}

inline void SWorkBase::FormCode(size_t i, int rhs)
{
   v_formCode[i] = rhs;
}
//---------------------------------------------------------------------
inline int SWorkBase::Overflow2b(size_t i) const
{
   return v_overflow2b[i];
}

inline void SWorkBase::Overflow2b(size_t i, int rhs)
{
   v_overflow2b[i] = rhs;
}
//---------------------------------------------------------------------
inline int SWorkBase::Overflow3b(size_t i) const
{
   return v_overflow3b[i];
}

inline void SWorkBase::Overflow3b(size_t i, int rhs)
{
   v_overflow3b[i] = rhs;
}
//---------------------------------------------------------------------
inline int SWorkBase::Gender(size_t i) const
{
   return v_gender[i];
}

inline void SWorkBase::Gender(size_t i, int rhs)
{
   v_gender[i] = rhs;
}
//---------------------------------------------------------------------
inline int SWorkBase::HashCode1(size_t i) const
{
   return v_hashCode1[i];
}

inline void SWorkBase::HashCode1(size_t i, int rhs)
{
	v_hashCode1[i] = rhs;
}
//---------------------------------------------------------------------
inline int SWorkBase::HashCode2(size_t i) const
{
   return v_hashCode2[i];
}

inline void SWorkBase::HashCode2(size_t i, int rhs)
{
	v_hashCode2[i] = rhs;
}
//---------------------------------------------------------------------
inline int SWorkBase::rootHashCode1(size_t i) const
{
   return v_rootHashCode1[i];
}

inline void SWorkBase::rootHashCode1(size_t i, int rhs)
{
	v_rootHashCode1[i] = rhs;
}
//---------------------------------------------------------------------
inline int SWorkBase::rootHashCode2(size_t i) const
{
   return v_rootHashCode2[i];
}

inline void SWorkBase::rootHashCode2(size_t i, int rhs)
{
	v_rootHashCode2[i] = rhs;
}
//---------------------------------------------------------------------
inline int SWorkBase::HeadWordLocation() const
{
   return v_headWordLocation;
}

inline void SWorkBase::HeadWordLocation(int rhs)
{
   v_headWordLocation = rhs;
}
//---------------------------------------------------------------------
inline int SWorkBase::HashLocation() const
{
   return v_hashLocation;
}

inline void SWorkBase::HashLocation(int x)
{
   v_hashLocation = x;
}
//---------------------------------------------------------------------
inline int SWorkBase::Henum1() const
{
   return v_henum1;
}

inline void SWorkBase::Henum1(int rhs)
{
   v_henum1 = rhs;
}
//---------------------------------------------------------------------
inline int SWorkBase::Henum2() const
{
   return v_henum2;
}

inline void SWorkBase::Henum2(int rhs)
{
   v_henum2 = rhs;
}
//---------------------------------------------------------------------
inline int SWorkBase::rootHenum1() const
{
   return v_rootHenum1;
}

inline void SWorkBase::rootHenum1(int rhs)
{
   v_rootHenum1 = rhs;
}
//---------------------------------------------------------------------
inline int SWorkBase::rootHenum2() const
{
   return v_rootHenum2;
}

inline void SWorkBase::rootHenum2(int rhs)
{
   v_rootHenum2 = rhs;
}
//---------------------------------------------------------------------
inline int SWorkBase::IsFound() const
{
   return v_isFound;
}

inline void SWorkBase::IsFound(int rhs)
{
   v_isFound = rhs;
}
//---------------------------------------------------------------------
inline int SWorkBase::IsProtected() const
{
   return v_isProtected;
}

inline void SWorkBase::IsProtected(int rhs)
{
   v_isProtected = rhs;
}
//---------------------------------------------------------------------
inline int SWorkBase::IsTransferred(size_t i) const
{
   return v_isTransferred[i];
}

inline void SWorkBase::IsTransferred(size_t i, int b)
{
   v_isTransferred[i] = b;
}
//---------------------------------------------------------------------
inline int SWorkBase::MatchedOnFullyCapped(size_t i) const
{
   return v_matchedOnFullyCapped[i];
}

inline void SWorkBase::MatchedOnFullyCapped(size_t i, int b)
{
   v_matchedOnFullyCapped[i] = b;
}
//---------------------------------------------------------------------
inline int SWorkBase::MeaningID(size_t i) const
{
   return v_meaningID[i];
}

inline void SWorkBase::MeaningID(size_t i, int rhs)
{
   v_meaningID[i] = rhs;
}
//---------------------------------------------------------------------
inline int SWorkBase::precedingSpaces() const
{
   return v_precedingSpaces;
}

inline void SWorkBase::setPrecedingSpaces(int rhs)
{
   v_precedingSpaces = rhs;
}
//---------------------------------------------------------------------
inline int SWorkBase::position() const
{
   return v_position;
}

inline void SWorkBase::position(int rhs)
{
   v_position = rhs;
}
//---------------------------------------------------------------------
inline int SWorkBase::PrimarySsu() const
{
   return v_primarySsu;
}

inline void SWorkBase::PrimarySsu(int rhs)
{
   v_primarySsu = rhs;
}
//---------------------------------------------------------------------
inline int SWorkBase::PatNumber(size_t i) const
{
   return v_patNumber[i];
}

inline void SWorkBase::PatNumber(size_t i, int rhs)
{
   v_patNumber[i] = rhs;
}
//---------------------------------------------------------------------
inline int SWorkBase::ProtectionCode(size_t i) const
{
   return v_protectionCode[i];
}

inline void SWorkBase::ProtectionCode(size_t i, int rhs)
{
   v_protectionCode[i] = rhs;
}
//---------------------------------------------------------------------
inline int SWorkBase::SetID(size_t i) const
{
   return v_setID[i];
}

inline void SWorkBase::SetID(size_t i, int rhs)
{
   v_setID[i] = rhs;
}
//---------------------------------------------------------------------
inline int SWorkBase::SourceStemNumber(size_t i) const
{
   return v_sourceStemNumber[i];
}

inline void SWorkBase::SourceStemNumber(size_t i, int rhs)
{
   v_sourceStemNumber[i] = rhs;
}
//---------------------------------------------------------------------
inline int SWorkBase::SourcePosition() const
{
   return v_sourcePosition;
}

inline void SWorkBase::SourcePosition(int rhs)
{
   v_sourcePosition = rhs;
}
//---------------------------------------------------------------------
inline int SWorkBase::SsuCount() const
{
   return v_ssuCount;
}

inline void SWorkBase::SsuCount(int rhs)
{
   v_ssuCount = rhs;
}
//---------------------------------------------------------------------
inline int SWorkBase::SubsetID(size_t i) const
{
   return v_subsetID[i];
}

inline void SWorkBase::SubsetID(size_t i, int rhs)
{
   v_subsetID[i] = rhs;
}
//---------------------------------------------------------------------
inline int SWorkBase::SupersetID(size_t i) const
{
   return v_supersetID[i];
}

inline void SWorkBase::SupersetID(size_t i, int rhs)
{
   v_supersetID[i] = rhs;
}
//---------------------------------------------------------------------
inline int SWorkBase::UsageID(size_t i) const
{
   return v_usageID[i];
}

inline void SWorkBase::UsageID(size_t i, int rhs)
{
   v_usageID[i] = rhs;
}
//---------------------------------------------------------------------
inline const LgsString& SWorkBase::Word() const
{
   return v_word;
}

inline void SWorkBase::Word(const LgsString& rhs)
{
   v_word = rhs;
}
//---------------------------------------------------------------------
inline int SWorkBase::WordClass(size_t i) const
{
   return v_wordClass[i];
}

inline void SWorkBase::WordClass(size_t i, int rhs)
{
   v_wordClass[i] = rhs;
}
//---------------------------------------------------------------------
inline int SWorkBase::WordCount() const
{
   return v_wordCount;
}

inline void SWorkBase::WordCount(int rhs)
{
   v_wordCount = rhs;
}
//---------------------------------------------------------------------
inline int SWorkBase::WordID(size_t i) const
{
   return v_wordID[i];
}

inline void SWorkBase::WordID(size_t i, int rhs)
{
   v_wordID[i] = rhs;
}
//---------------------------------------------------------------------
inline int SWorkBase::WordTypeCode(size_t i) const
{
   return v_wordTypeCode[i];
}

inline void SWorkBase::WordTypeCode(size_t i, int rhs)
{
   v_wordTypeCode[i] = rhs;
}
//---------------------------------------------------------------------
inline int SWorkBase::WordLength() const
{
   return v_word.length();
}
//---------------------------------------------------------------------
inline bool SWorkBase::isBold() const
{
   return v_isBold;
}
//---------------------------------------------------------------------
inline bool SWorkBase::isItalic() const
{
   return v_isItalic;
}
//---------------------------------------------------------------------
inline bool SWorkBase::isUnderlined() const
{
   return v_isUnderlined;
}
//---------------------------------------------------------------------
inline bool SWorkBase::isSingleQuoted() const
{
   return v_isSingleQuoted;
}
//---------------------------------------------------------------------
inline bool SWorkBase::isDoubleQuoted() const
{
   return v_isDoubleQuoted;
}


// --------------------------------------------------------------------------
// Set the i-th SMC
// --------------------------------------------------------------------------
inline void SWorkBase::setSubjectMatterCode(short int i, 
											SubjectMatterCode theSMC) 
{
	subjectMatterCode_[i]=theSMC;
}

// --------------------------------------------------------------------------
// Return the i-th SMC
// --------------------------------------------------------------------------
inline SubjectMatterCode SWorkBase::subjectMatterCode(short int i) const 
{
	return subjectMatterCode_[i];
}

inline short SWorkBase::CompoundInfo(void) const
{
	return v_compoundInfo;
}

inline void SWorkBase::CompoundInfo(short sCompoundInfo)
{
	v_compoundInfo = sCompoundInfo;
}

#endif // __SworkBase_h__


