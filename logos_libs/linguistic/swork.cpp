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
// File: SWork.cpp (implementation)
// --------------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/swork.h>
#include <stdio.h>
#include <logos_libs/linguistic/sourcesentenceunit.h>
#include <logos_libs/linguistic/ltextualcomponent.h>


// --------------------------------------------------------------------------
// Default constructor
// --------------------------------------------------------------------------
SWork::SWork()
{
}


// --------------------------------------------------------------------------
// Default destructor
// --------------------------------------------------------------------------
SWork::~SWork()
{
}


// --------------------------------------------------------------------------
// --------------------------------------------------------------------------
SWork::SWork(const SourceSentenceUnit& su)
{
   Initialize(su);
}


// --------------------------------------------------------------------------
// Initialize this object from the given source sentence unit
// --------------------------------------------------------------------------
void SWork::Initialize(const SourceSentenceUnit& unit)
{
   SourceSentenceUnit& su = const_cast<SourceSentenceUnit&>(unit);
   PrimarySsu(0);
   SourcePosition(su.sentenceAddress());
   position(su.position());
   setCapitalizationState(su.caseState());
   setPrecedingSpaces(su.precedingSpaces());
   setEndOfSentence(su.isEndOfSentence());
   IsProtected(su.isProtectedFromTranslation());
   CompoundInfo(su.compoundInfo());

   v_isBold = unit.markup().isBold();
   v_isItalic = unit.markup().isItalic();
   v_isUnderlined = unit.markup().isUnderlined();
   v_isSingleQuoted = unit.markup().isSingleQuoted();
   v_isDoubleQuoted = unit.markup().isDoubleQuoted();

   if (su.isUnfoundWord())
   {
      Word(su.surfaceExpressionAsString());
      IsFound(0);
      WordCount(1);
      HeadWordLocation(1);
      HashLocation(1);
      BlackHoleLocation(0);
   }
   else
   {
      Word(su.word());
      IsFound(1);
      WordCount(su.wordCount());
      HeadWordLocation(su.headLocation());
      HashLocation(su.hashLocation());
      BlackHoleLocation(su.blackHoleLocation());
   }
   Henum1((const_cast<SourceSentenceUnit&>(su)).henum1());
   Henum2((const_cast<SourceSentenceUnit&>(su)).henum2());
   rootHenum1((const_cast<SourceSentenceUnit&>(su)).rootHenum1());
   rootHenum2((const_cast<SourceSentenceUnit&>(su)).rootHenum2());
   v_isHeadWordBuilt = false;

   if (su.ssuCount() <= 3)
   {
      SsuCount(su.ssuCount());
   }
   else
   {
      SsuCount(3);			// maximum number of semanto-syntactic units allowed per sentence unit
   }

   // set the semanto-syntactic units as defined by the given source sentence unit
   for (int i = 0; i < SsuCount(); i++)
   {
	   setSubjectMatterCode(i, su.ssuAt(i).subjectMatterCode());
      CompanyCode(i, su.ssuAt(i).companyCode());
      AuxiliaryCode(i, su.ssuAt(i).auxiliaryCode());
      FormCode(i, su.formCode(i));
      Overflow2b(i, su.ssuAt(i).overflow2b());
      Overflow3b(i, su.ssuAt(i).overflow3b());
      Gender(i, su.ssuAt(i).genderCode());
      HashCode1(i, su.ssuAt(i).hashCode1());
      HashCode2(i, su.ssuAt(i).hashCode2());
	  rootHashCode1(i, su.ssuAt(i).rootHashCode1());
	  rootHashCode2(i, su.ssuAt(i).rootHashCode2());
      IsTransferred(i, su.ssuAt(i).isSourceTransferred());
      MatchedOnFullyCapped(i, su.ssuAt(i).matchedOnFullyCapped());
      MeaningID(i, su.ssuAt(i).meaningID());
      PatNumber(i, su.ssuAt(i).patNumber());
      ProtectionCode(i, su.ssuAt(i).protectionCode());
      SetID(i, su.ssuAt(i).setID());
      SourceStemNumber(i, su.ssuAt(i).sourceStemNumber());
      SubsetID(i, su.ssuAt(i).subSetID());
      SupersetID(i, su.ssuAt(i).superSetID());
      UsageID(i, su.ssuAt(i).usageID());
      WordClass(i, su.ssuAt(i).wordClassCode());
      WordID(i, su.ssuAt(i).wordID());
      WordTypeCode(i, su.ssuAt(i).wordTypeCode());
      CanonicalWord(i, su.ssuAt(i).word());
      TargetWord(i, su.ssuAt(i).targetWord());
   }

   // set the remaining semanto-syntactic units to default
   SubjectMatterCode aDefaultSMC;
   for (int j = SsuCount(); j < 3; j++ )
   {
	  setSubjectMatterCode(j,aDefaultSMC);
      CompanyCode(j, "   ");
      AuxiliaryCode(j, 0);
      FormCode(j, 0);
      Overflow2b(j, 0);
      Overflow3b(j, 0);
      Gender(j, 0);
      HashCode1(j, 0);
      HashCode2(j, 0);
	  rootHashCode1(j, 0);
	  rootHashCode2(j, 0);
      IsTransferred(j, 0);
      MatchedOnFullyCapped(j, 0);
      MeaningID(j, 0);
      PatNumber(j, 0);
      ProtectionCode(j, 0);
      SetID(j, 0);
      SourceStemNumber(j, 0);
      SubsetID(j, 0);
      SupersetID(j, 0);
      UsageID(j, 0);
      WordClass(j, 0);
      WordID(j, 0);
      WordTypeCode(j, 0);
      CanonicalWord(j, "");
      TargetWord(j, "");
   }
}

