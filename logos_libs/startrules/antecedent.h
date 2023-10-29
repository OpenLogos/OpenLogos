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
#ifndef __StartAntecedent_h__
#define __StartAntecedent_h__

//-------------------------------------------------------------------
// File - Antecedent.h
//
// Class - ST_Antecedent
//
// Description - Antecedent used to match a pattern
//
//-------------------------------------------------------------------

#include <logos_libs/ruleengine/rule.h>
#include <logos_libs/ruleengine/antecedent.h>
#include <logos_libs/startrules/action.h>
#include <logos_libs/startrules/variable.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace Start {

class ST_Antecedent: public RE_Antecedent<ST_Variable>
{
   DisableCopyAssign(ST_Antecedent);

public:
   static const LgsString PairedFloatingSingleHyphens1ID;
   static const LgsString PairedFloatingSingleHyphens2ID;
   static const LgsString PairedFloatingMultipleHyphens1ID;
   static const LgsString PairedFloatingMultipleHyphens2ID;
   static const LgsString PairedEmbeddedHyphensID;
   static const LgsString HyphenSequenceID;
   static const LgsString OutlineRomanBosID;
   static const LgsString OutlineRomanCapBosID;
   static const LgsString MultiHyphenatedID;
   static const LgsString OutlineBosID;
   static const LgsString OrdinalID;
   static const LgsString GermanOrdinalID;
   static const LgsString TwoDigitDateID;
   static const LgsString DateID;
   static const LgsString EnglishHourID;
   static const LgsString TimeID;
   static const LgsString ShortYearID;
   static const LgsString YearSID;
   static const LgsString DigitSID;
   static const LgsString AdjNumberID;
   static const LgsString NumericID;
   static const LgsString YearRangeID;
   static const LgsString NumericRangeID;
   static const LgsString MathTimesMinusID;
   static const LgsString DottedAlphanumID;
   static const LgsString FractionID;
   static const LgsString DigitTimesDigitID;
   static const LgsString AngledBracketsID;
   static const LgsString NumericHyphenAlphanumID;
   static const LgsString AlphanumHyphenNumericID;
   static const LgsString MultiHyphenAlphanumID;
   static const LgsString WordPlusWordID;
   static const LgsString MathExpressionID;
   static const LgsString EllipsisID;
   static const LgsString MiscSymbolID;
   static const LgsString CatchAllID;
   static const LgsString SlashAlpanumID;
   static const LgsString MiscDigitsID;
   static const LgsString LookupNeededID;
   static const LgsString OutlineMidID;
   static const LgsString OutlineRomanCapMidID;
   static const LgsString OutlineRomanMidID;

   enum ST_Type
   {
      PairedFloatingSingleHyphens1TYPE,
      PairedFloatingSingleHyphens2TYPE,
      PairedFloatingMultipleHyphens1TYPE,
      PairedFloatingMultipleHyphens2TYPE,
      PairedEmbeddedHyphensTYPE,
      HyphenSequenceTYPE,
      OutlineRomanBosTYPE,
      OutlineRomanCapBosTYPE,
      MultiHyphenatedTYPE,
      OutlineBosTYPE,
      OrdinalTYPE,
      GermanOrdinalTYPE,
      TwoDigitDateTYPE,
      DateTYPE,
      EnglishHourTYPE,
      TimeTYPE,
      ShortYearTYPE,
      YearSTYPE,
      DigitSTYPE,
      AdjNumberTYPE,
      NumericTYPE,
      YearRangeTYPE,
      NumericRangeTYPE,
      MathTimesMinusTYPE,
      DottedAlphanumTYPE,
      FractionTYPE,
      DigitTimesDigitTYPE,
      AngledBracketsTYPE,
      NumericHyphenAlphanumTYPE,
      AlphanumHyphenNumericTYPE,
      MultiHyphenAlphanumTYPE,
      WordPlusWordTYPE,
      MathExpressionTYPE,
      EllipsisTYPE,
      MiscSymbolTYPE,
      CatchAllTYPE,
      SlashAlpanumTYPE,
      MiscDigitsTYPE,
      LookupNeededTYPE,
      OutlineMidTYPE,
      OutlineRomanCapMidTYPE,
      OutlineRomanMidTYPE,
      InvalidTYPE = 999
   };

   ST_Antecedent(const LgsString& pattern, RegularExpression::Borders borders,
                 ST_Type antecedentType);
   virtual bool evaluate(ST_Variable& variable);
   void incMatchCount(void);
   void incTotalCount(void);
   void decTotalCount(void);
   virtual bool isViableMatch(ST_Variable& variable);
   static ST_Type antecedentType(const LgsString& idString);

private:
   bool hasFloatingSingleHyphens(LgsString& str, int numToFind);
   bool hasFloatingMultipleHyphens(LgsString& str, int numToFind);
   bool isHyphenString(LgsString& str);
   bool hasAtLeastTwoEmbeddedMulitpleHyphens(LgsString& str);
   bool hasHyphenSequence(LgsString& str);
   bool hasAtLeastOneHyphen(LgsString& str);
   bool hasAtLeastTwoSingleHyphens(LgsString& str);
   bool hasLowerCaseRomanNumeralBOS(LgsString& str);
   bool hasUpperCaseRomanNumeralBOS(LgsString& str);
   bool hasBOSEnding(LgsString& str);
   bool hasDigit(LgsString& str);
   bool hasNumberPeriod(LgsString& str);
   bool hasAngleBrackets(LgsString& str);
   bool hasShortYear(LgsString& str);
   bool hasYearPlural(LgsString& str);
   bool hasNumberPlural(LgsString& str);
   bool hasDottedAlphaNumeric(LgsString& str);
   bool hasYearRange(LgsString& str);
   bool hasNumberRange(LgsString& str);
   bool hasMathTimesMinus(LgsString& str);
   bool hasFraction(LgsString& str);
   bool hasDigitTimesDigit(LgsString& str);
   bool hasNumberHyphen(LgsString& str);
   bool hasAlphaNumericHyphenNumeric(LgsString& str);
   bool hasAlphaNumbericHyphenAlphaNumeric(LgsString& str);
   bool hasAlphaPlusSignAlpha(LgsString& str);
   bool hasMathExpression(LgsString& str);
   bool hasAlphaNumericSlashAlphaNumeric(LgsString& str);
   bool hasMiscDigits(LgsString& str);
   bool isExcludedMathCharacter(char& ch);
   bool hasEllipsis(LgsString& str);
   bool hasCatchAllSymbol(LgsString& str);
   bool hasMiscSymbol(LgsString& str);

private:
   RegularExpression pattern_;
   const RE_Consequent<class ST_Variable>* getAction();
   ST_Type antecedentType_;
};

//}

// --------------------------------------------------------------------------
inline const RE_Consequent<class ST_Variable>* ST_Antecedent::getAction()
{
   return rule_->getConsequent();
}

// --------------------------------------------------------------------------
inline void ST_Antecedent::incMatchCount(void)
{
   rule_->incMatchCount();
}

// --------------------------------------------------------------------------
inline void ST_Antecedent::incTotalCount(void)
{
   rule_->incTotalCount();
}

// --------------------------------------------------------------------------
inline void ST_Antecedent::decTotalCount(void)
{
   rule_->decTotalCount();
}

#endif



