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
//-------------------------------------------------------------------
// File - Antecedent.cpp
//
// Class - ST_Antecedent
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/startrules/antecedent.h>
#include <logos_libs/translutility/translcommonobjects.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace Start {

const LgsString ST_Antecedent::PairedFloatingSingleHyphens1ID   = "pairedfloatingsinglehyphens1";
const LgsString ST_Antecedent::PairedFloatingSingleHyphens2ID   = "pairedfloatingsinglehyphens2";
const LgsString ST_Antecedent::PairedFloatingMultipleHyphens1ID = "pairedfloatingmultiplehyphens1";
const LgsString ST_Antecedent::PairedFloatingMultipleHyphens2ID = "pairedfloatingmultiplehyphens2";
const LgsString ST_Antecedent::PairedEmbeddedHyphensID          = "pairedembeddedhyphens";
const LgsString ST_Antecedent::HyphenSequenceID                 = "hyphensequence";
const LgsString ST_Antecedent::OutlineRomanBosID                = "outline_roman_BOS";
const LgsString ST_Antecedent::OutlineRomanCapBosID             = "outline_roman_cap_BOS";
const LgsString ST_Antecedent::MultiHyphenatedID                = "multi_hyphenated";
const LgsString ST_Antecedent::OutlineBosID                     = "outline_BOS";
const LgsString ST_Antecedent::OrdinalID                        = "ordinal";
const LgsString ST_Antecedent::GermanOrdinalID                  = "german_ordinal";
const LgsString ST_Antecedent::TwoDigitDateID                   = "two_digit_date";
const LgsString ST_Antecedent::DateID                           = "date";
const LgsString ST_Antecedent::EnglishHourID                    = "english_hour";
const LgsString ST_Antecedent::TimeID                           = "time";
const LgsString ST_Antecedent::ShortYearID                      = "short_year";
const LgsString ST_Antecedent::YearSID                          = "year_s";
const LgsString ST_Antecedent::DigitSID                         = "digit_s";
const LgsString ST_Antecedent::AdjNumberID                      = "adj_number";
const LgsString ST_Antecedent::NumericID                        = "numeric";
const LgsString ST_Antecedent::YearRangeID                      = "year_range";
const LgsString ST_Antecedent::NumericRangeID                   = "numeric_range";
const LgsString ST_Antecedent::MathTimesMinusID                 = "math_times_minus";
const LgsString ST_Antecedent::DottedAlphanumID                 = "dotted_alphanum";
const LgsString ST_Antecedent::FractionID                       = "fraction";
const LgsString ST_Antecedent::DigitTimesDigitID                = "digit_times_digit";
const LgsString ST_Antecedent::AngledBracketsID                 = "angled_brackets";
const LgsString ST_Antecedent::NumericHyphenAlphanumID          = "numeric_hyphen_alphanum";
const LgsString ST_Antecedent::AlphanumHyphenNumericID          = "alphanum_hyphen_numeric";
const LgsString ST_Antecedent::MultiHyphenAlphanumID            = "multi_hyphen_alphanum";
const LgsString ST_Antecedent::WordPlusWordID                   = "word_plus_word";
const LgsString ST_Antecedent::MathExpressionID                 = "math_expression";
const LgsString ST_Antecedent::EllipsisID                       = "ellipsis";
const LgsString ST_Antecedent::MiscSymbolID                     = "misc_symbol";
const LgsString ST_Antecedent::CatchAllID                       = "catch_all";
const LgsString ST_Antecedent::SlashAlpanumID                   = "slashalpnum";
const LgsString ST_Antecedent::MiscDigitsID                     = "miscdigits";
const LgsString ST_Antecedent::LookupNeededID                   = "lookup_needed";
const LgsString ST_Antecedent::OutlineMidID                     = "outline_mid";
const LgsString ST_Antecedent::OutlineRomanCapMidID             = "outline_roman_cap_mid";
const LgsString ST_Antecedent::OutlineRomanMidID                = "outline_roman_mid";

// --------------------------------------------------------------------------
ST_Antecedent::ST_Antecedent(const LgsString& pattern, RegularExpression::Borders borders,
                             ST_Type antecedentType)
              :pattern_(pattern, borders),
               antecedentType_(antecedentType)
{
}

// --------------------------------------------------------------------------
bool ST_Antecedent::evaluate(ST_Variable& variable)
{
   // set the start postion to the beginning of the sentence
   variable.resetStartPos();

   for (;;)
   {
      // exit condition - no more matches
      if (variable.pastEndPos())
      {
         decTotalCount();
         break;
      }
      if (!variable.matches(pattern_) || variable.getMatch(0).length == 0)
         break;

      // increment start position so we don't find the same match
      variable.incrementStartPos();

      incTotalCount();
      // check that the text is not protected, and has not already been
      // assigned a token type; if not call the action object
      if (!variable.protected_or_tokenized())
      {
         getAction()->fire(variable);
         incMatchCount();
      }

      // continue searching from beyond the current pattern
   }

    return false;  // done with this rule - we did the loop internally, so don't need to return true
}

// --------------------------------------------------------------------------
ST_Antecedent::ST_Type ST_Antecedent::antecedentType(const LgsString& idString)
{
   if (idString == PairedFloatingSingleHyphens1ID)
      return PairedFloatingSingleHyphens1TYPE;
   else if (idString == PairedFloatingSingleHyphens2ID)
      return PairedFloatingSingleHyphens2TYPE;
   else if (idString == PairedFloatingMultipleHyphens1ID)
      return PairedFloatingMultipleHyphens1TYPE;
   else if (idString == PairedFloatingMultipleHyphens2ID)
      return PairedFloatingMultipleHyphens2TYPE;
   else if (idString == PairedEmbeddedHyphensID)
      return PairedEmbeddedHyphensTYPE;
   else if (idString == HyphenSequenceID)
      return HyphenSequenceTYPE;
   else if (idString == OutlineRomanBosID)
      return OutlineRomanBosTYPE;
   else if (idString == OutlineRomanCapBosID)
      return OutlineRomanCapBosTYPE;
   else if (idString == MultiHyphenatedID)
      return MultiHyphenatedTYPE;
   else if (idString == OutlineBosID)
      return OutlineBosTYPE;
   else if (idString == OrdinalID)
      return OrdinalTYPE;
   else if (idString == GermanOrdinalID)
      return GermanOrdinalTYPE;
   else if (idString == TwoDigitDateID)
      return TwoDigitDateTYPE;
   else if (idString == DateID)
      return DateTYPE;
   else if (idString == EnglishHourID)
      return EnglishHourTYPE;
   else if (idString == TimeID)
      return TimeTYPE;
   else if (idString == ShortYearID)
      return ShortYearTYPE;
   else if (idString == YearSID)
      return YearSTYPE;
   else if (idString == DigitSID)
      return DigitSTYPE;
   else if (idString == AdjNumberID)
      return AdjNumberTYPE;
   else if (idString == NumericID)
      return NumericTYPE;
   else if (idString == YearRangeID)
      return YearRangeTYPE;
   else if (idString == NumericRangeID)
      return NumericRangeTYPE;
   else if (idString == MathTimesMinusID)
      return MathTimesMinusTYPE;
   else if (idString == DottedAlphanumID)
      return DottedAlphanumTYPE;
   else if (idString == FractionID)
      return FractionTYPE;
   else if (idString == DigitTimesDigitID)
      return DigitTimesDigitTYPE;
   else if (idString == AngledBracketsID)
      return AngledBracketsTYPE;
   else if (idString == NumericHyphenAlphanumID)
      return NumericHyphenAlphanumTYPE;
   else if (idString == AlphanumHyphenNumericID)
      return AlphanumHyphenNumericTYPE;
   else if (idString == MultiHyphenAlphanumID)
      return MultiHyphenAlphanumTYPE;
   else if (idString == WordPlusWordID)
      return WordPlusWordTYPE;
   else if (idString == MathExpressionID)
      return MathExpressionTYPE;
   else if (idString == EllipsisID)
      return EllipsisTYPE;
   else if (idString == MiscSymbolID)
      return MiscSymbolTYPE;
   else if (idString == CatchAllID)
      return CatchAllTYPE;
   else if (idString == SlashAlpanumID)
      return SlashAlpanumTYPE;
   else if (idString == MiscDigitsID)
      return MiscDigitsTYPE;
   else if (idString == LookupNeededID)
      return LookupNeededTYPE;
   else if (idString == OutlineMidID)
      return OutlineMidTYPE;
   else if (idString == OutlineRomanCapMidID)
      return OutlineRomanCapMidTYPE;
   else if (idString == OutlineRomanMidID)
      return OutlineRomanMidTYPE;
   else
      return InvalidTYPE;
}

// --------------------------------------------------------------------------
bool ST_Antecedent::isViableMatch(ST_Variable& variable)
{
   LgsString str = variable.getPhraseManager()->getText();

   if (str.length() == 0)
   {
      return false;
   }

   switch (antecedentType_)
   {
   case PairedFloatingSingleHyphens1TYPE:
      return hasFloatingSingleHyphens(str, 3);

   case PairedFloatingSingleHyphens2TYPE:
      return hasFloatingSingleHyphens(str, 2);

   case PairedFloatingMultipleHyphens1TYPE:
      return hasFloatingMultipleHyphens(str, 3);

   case PairedFloatingMultipleHyphens2TYPE:
      return hasFloatingMultipleHyphens(str, 2);

   case PairedEmbeddedHyphensTYPE:
      return hasAtLeastTwoEmbeddedMulitpleHyphens(str);

   case HyphenSequenceTYPE:
      return hasHyphenSequence(str);

   case MultiHyphenatedTYPE:
      return (hasAtLeastTwoSingleHyphens(str) && hasDigit(str));

   case OutlineRomanBosTYPE:
      return hasLowerCaseRomanNumeralBOS(str);

   case OutlineRomanCapBosTYPE:
      return hasUpperCaseRomanNumeralBOS(str);

   case OutlineBosTYPE:
      return hasBOSEnding(str);

   case OrdinalTYPE:
   case DateTYPE:
   case EnglishHourTYPE:
   case TimeTYPE:
   case NumericTYPE:
      return hasDigit(str);

   case GermanOrdinalTYPE:
   case TwoDigitDateTYPE:
      if (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::GermanID)
      {
         return hasNumberPeriod(str);
      }
      return false;

   case ShortYearTYPE:
      return hasShortYear(str);

   case YearSTYPE:
      return hasYearPlural(str);

   case DigitSTYPE:
      return hasNumberPlural(str);

   case AdjNumberTYPE:
      return hasNumberPeriod(str);

   case YearRangeTYPE:
      return hasYearRange(str);

   case NumericRangeTYPE:
      return hasNumberRange(str);

   case MathTimesMinusTYPE:
      return hasMathTimesMinus(str);

   case DottedAlphanumTYPE:
      return hasDottedAlphaNumeric(str);

   case FractionTYPE:
      return hasFraction(str);

   case DigitTimesDigitTYPE:
      return hasDigitTimesDigit(str);

   case AngledBracketsTYPE:
      return hasAngleBrackets(str);

   case NumericHyphenAlphanumTYPE:
      return hasNumberHyphen(str);

   case AlphanumHyphenNumericTYPE:
      return hasAlphaNumericHyphenNumeric(str);

   case MultiHyphenAlphanumTYPE:
      return hasAlphaNumbericHyphenAlphaNumeric(str);

   case WordPlusWordTYPE:
      return hasAlphaPlusSignAlpha(str);

   case MathExpressionTYPE:
      return hasMathExpression(str);

   case EllipsisTYPE:
      return hasEllipsis(str);

   case MiscSymbolTYPE:
      return hasMiscSymbol(str);

   case CatchAllTYPE:
      return (hasDigit(str) || hasCatchAllSymbol(str));

   case SlashAlpanumTYPE:
      return (hasAlphaNumericHyphenNumeric(str) || hasAlphaNumericSlashAlphaNumeric(str));

   case MiscDigitsTYPE:
      return hasMiscDigits(str);

   case LookupNeededTYPE:
   case OutlineMidTYPE:
   case OutlineRomanCapMidTYPE:
   case OutlineRomanMidTYPE:
   default:
      return true;
   }
}

// --------------------------------------------------------------------------
bool ST_Antecedent::hasFloatingSingleHyphens(LgsString& str, int numToFind)
{
   int pos = 0;
   int numFound = 0;

   while ((pos != LgsString::npos) && (pos < str.length()))
   {
      if ((pos = str.find(" - ", pos)) != LgsString::npos)
      {
         numFound++;
         if (numFound == numToFind)
         {
            return true;
         }
         pos += 2;
      }
   }
   return false;
}

// --------------------------------------------------------------------------
bool ST_Antecedent::hasFloatingMultipleHyphens(LgsString& str, int numToFind)
{
   int pos1 = 0;
   int pos2 = 0;
   int numFound = 0;

   while ((pos1 < str.length()) && (pos2 < str.length()) &&
          (pos1 != LgsString::npos) && (pos2 != LgsString::npos))
   {
      pos1 = str.find(" --", pos1);
      pos2 = pos1 + 2;

      if (pos1 != LgsString::npos)
      {
         if ((pos2 = str.find("- ", pos2)) != LgsString::npos)
         {
            LgsString hyphenStr = str.substr((pos1 + 1), (pos2 - pos1));

            if (isHyphenString(hyphenStr))
            {
               numFound++;
               if (numFound == numToFind)
               {
                  return true;
               }
            }
            pos1 = pos2 + 1;
         }
      }
   }
   return false;
}

// --------------------------------------------------------------------------
bool ST_Antecedent::isHyphenString(LgsString& str)
{
   for (LgsString::iterator iter = str.begin(); iter != str.end(); iter++)
   {
      if (*iter != '-')
      {
         return false;
      }
   }
   return true;
}

// --------------------------------------------------------------------------
bool ST_Antecedent::hasAtLeastTwoEmbeddedMulitpleHyphens(LgsString& str)
{
   int pos = 0;
   int numFound = 0;

   while ((pos < str.length()) && (pos != LgsString::npos))
   {
      if ((pos = str.find("--", pos)) != LgsString::npos)
      {
         if (numFound == 0)
         {
            if ((pos == 0) || (str[pos - 1] != ' '))
            {
               numFound++;
            }
            pos = str.find_first_not_of('-', pos);
         }
         else
         {
            if (((pos = str.find_first_not_of('-', pos)) == LgsString::npos) || (str[pos] != ' '))
            {
               return true;
            }
         }
      }
   }
   return false;
}

// --------------------------------------------------------------------------
bool ST_Antecedent::hasHyphenSequence(LgsString& str)
{
   if (str.find(" -") != LgsString::npos)
   {
      return true;
   }
   return (str.find("--") != LgsString::npos);
}

// --------------------------------------------------------------------------
bool ST_Antecedent::hasAtLeastOneHyphen(LgsString& str)
{
   return (str.find('-') != LgsString::npos);
}

// --------------------------------------------------------------------------
bool ST_Antecedent::hasAtLeastTwoSingleHyphens(LgsString& str)
{
   int pos = 0;
   int numFound = 0;

   while ((pos != LgsString::npos) && (pos < str.length()))
   {
      if ((pos = str.find('-', pos)) != LgsString::npos)
      {
         pos++;

         if (pos < str.length())
         {
            if (str[pos] != '-')
            {
               numFound++;
               if (numFound > 1)
               {
                  return true;
               }
            }
            pos = str.find_first_not_of('-', pos);
         }
      }
   }
   return false;
}

// --------------------------------------------------------------------------
bool ST_Antecedent::hasLowerCaseRomanNumeralBOS(LgsString& str)
{
   int pos1 = str.find_first_of("ixv");
   
   if ((pos1 == 0) || (pos1 == 1))
   {
      int pos2 = str.find_first_not_of("ixv", pos1);
      
      if ((pos2 != LgsString::npos) && ((str[pos2] == '.') || (str[pos2] == ')')))
      {
         return true;
      }
   }
   return false;
}

// --------------------------------------------------------------------------
bool ST_Antecedent::hasUpperCaseRomanNumeralBOS(LgsString& str)
{
   int pos1 = str.find_first_of("IXV");
   
   if ((pos1 == 0) || (pos1 == 1))
   {
      int pos2 = str.find_first_not_of("IXV", pos1);
      
      if ((pos2 != LgsString::npos) && ((str[pos2] == '.') || (str[pos2] == ')')))
      {
         return true;
      }
   }
   return false;
}

// --------------------------------------------------------------------------
bool ST_Antecedent::hasBOSEnding(LgsString& str)
{
   int pos = str.find_first_of(".)");

   if (((pos == 1) || (pos == 2)) && CharUtil::isAlphaNumeric(str[pos - 1]))
   {
      return true;
   }
   return false;
}

// --------------------------------------------------------------------------
bool ST_Antecedent::hasDigit(LgsString& str)
{
   return (str.find_first_of("0123456789") != LgsString::npos);
}

// --------------------------------------------------------------------------
bool ST_Antecedent::hasNumberPeriod(LgsString& str)
{
   int pos = 0;
    
   while ((pos != LgsString::npos) && (pos < str.length()))
   {
      if ((pos = str.find('.', pos)) != LgsString::npos)
      {
         if ((pos > 0) && CharUtil::isNumeric(str[pos - 1]))
         {
            return true;
         }
         pos++;
      }
   }
   return false;
}

// --------------------------------------------------------------------------
bool ST_Antecedent::hasAngleBrackets(LgsString& str)
{
   int pos = str.find('<');
   
   if (pos != LgsString::npos)
   {
      return (str.find('>', pos + 1) != LgsString::npos);
   }
   return false;
}

// --------------------------------------------------------------------------
bool ST_Antecedent::hasShortYear(LgsString& str)
{
   int pos = 0;
   
   while ((pos != LgsString::npos) && (pos < str.length()))
   {
      if ((pos = str.find('\'', pos)) != LgsString::npos)
      {
         if (((pos + 2) < str.length()) && CharUtil::isNumeric(str[pos + 1]) &&
             CharUtil::isNumeric(str[pos + 2]))
         {
            return true;
         }
         pos++;
      }
   }
   return false;
}
         
// --------------------------------------------------------------------------
bool ST_Antecedent::hasYearPlural(LgsString& str)
{
   if (str.find("0s") != LgsString::npos)
   {
      return true;
   }
   
   return (str.find("0's") != LgsString::npos);
}

// --------------------------------------------------------------------------
bool ST_Antecedent::hasNumberPlural(LgsString& str)
{
   int pos = 0;

   while ((pos != LgsString::npos) && (pos < str.length()))
   {
      if ((pos = str.find('s', pos)) != LgsString::npos)
      {
         if (((pos > 0) && CharUtil::isNumeric(str[pos - 1])) ||
             ((pos > 1) && (str[pos - 1] == '\'') && CharUtil::isNumeric(str[pos - 2])))
         {
            return true;
         }
         pos++;
      }
   }
   return false;
}

// --------------------------------------------------------------------------
bool ST_Antecedent::hasDottedAlphaNumeric(LgsString& str)
{
   int pos = 0;

   while ((pos != LgsString::npos) && (pos < str.length()))
   {
      if ((pos = str.find('.', pos)) != LgsString::npos)
      {
         if ((pos > 0) && ((pos + 1) < str.length()) && CharUtil::isAlphaNumeric(str[pos - 1]) &&
             CharUtil::isAlphaNumeric(str[pos + 1]))
         {
            return true;
         }
         pos++;
      }
   }
   return false;
}

// --------------------------------------------------------------------------
bool ST_Antecedent::hasYearRange(LgsString& str)
{
   int pos = 0;

   while ((pos != LgsString::npos) && (pos < str.length()))
   {
      if ((pos = str.find_first_of("/-", pos)) != LgsString::npos)
      {
         if ((pos > 3) && ((pos + 1) < str.length()) && CharUtil::isNumeric(str[pos - 1]) &&
             CharUtil::isNumeric(str[pos + 1]))
         {
            return true;
         }
         pos++;
      }
   }
   return false;
}

// --------------------------------------------------------------------------
bool ST_Antecedent::hasNumberRange(LgsString& str)
{
   int pos = 0;

   while ((pos != LgsString::npos) && (pos < str.length()))
   {
      if ((pos = str.find('-', pos)) != LgsString::npos)
      {
         if ((pos > 0) && ((pos + 1) < str.length()) && CharUtil::isNumeric(str[pos - 1]) &&
             CharUtil::isNumeric(str[pos + 1]))
         {
            return true;
         }
         pos++;
      }
   }
   return false;
}

// --------------------------------------------------------------------------
bool ST_Antecedent::hasMathTimesMinus(LgsString& str)
{
   int pos = 0;

   while ((pos != LgsString::npos) && (pos < str.length()))
   {
      if ((pos = str.find_first_of("*◊-", pos)) != LgsString::npos)
      {
         if ((pos > 0) && CharUtil::isNumeric(str[pos - 1]) &&
             ((((pos + 1) < str.length()) && CharUtil::isNumeric(str[pos + 1])) ||
              (((pos + 2) < str.length()) && (str.find_first_of("+±-") == (pos + 1)) &&
               CharUtil::isNumeric(str[pos + 2]))))
         {
            return true;
         }
         pos++;
      }
   }
   return false;
}

// --------------------------------------------------------------------------
bool ST_Antecedent::hasFraction(LgsString& str)
{
   int pos = 0;

   while ((pos != LgsString::npos) && (pos < str.length()))
   {
      if ((pos = str.find('/', pos)) != LgsString::npos)
      {
         if ((pos > 0) && ((pos + 1) < str.length()) && CharUtil::isNumeric(str[pos - 1]) &&
             CharUtil::isNumeric(str[pos + 1]))
         {
            return true;
         }
         pos++;
      }
   }
   return false;
}

// --------------------------------------------------------------------------
bool ST_Antecedent::hasDigitTimesDigit(LgsString& str)
{
   int pos = 0;

   while ((pos != LgsString::npos) && (pos < str.length()))
   {
      if ((pos = str.find_first_of("x◊", pos)) != LgsString::npos)
      {
         if ((pos > 0) && ((pos + 1) < str.length()) && CharUtil::isNumeric(str[pos - 1]) &&
             CharUtil::isNumeric(str[pos + 1]))
         {
            return true;
         }
         pos++;
      }
   }
   return false;
}

// --------------------------------------------------------------------------
bool ST_Antecedent::hasNumberHyphen(LgsString& str)
{
   int pos = 0;

   while ((pos != LgsString::npos) && (pos < str.length()))
   {
      if ((pos = str.find('-', pos)) != LgsString::npos)
      {
         if ((pos > 0) && CharUtil::isNumeric(str[pos - 1]))
         {
            return true;
         }
         pos++;
      }
   }
   return false;
}

// --------------------------------------------------------------------------
bool ST_Antecedent::hasAlphaNumericHyphenNumeric(LgsString& str)
{
   int pos = 0;

   while ((pos != LgsString::npos) && (pos < str.length()))
   {
      if ((pos = str.find('-', pos)) != LgsString::npos)
      {
         if ((pos > 0) && ((pos + 1) < str.length()) && CharUtil::isAlphaNumeric(str[pos - 1]) &&
             CharUtil::isNumeric(str[pos + 1]))
         {
            return true;
         }
         pos++;
      }
   }
   return false;
}

// --------------------------------------------------------------------------
bool ST_Antecedent::hasAlphaNumbericHyphenAlphaNumeric(LgsString& str)
{
   int pos = 0;

   while ((pos != LgsString::npos) && (pos < str.length()))
   {
      if ((pos = str.find('-', pos)) != LgsString::npos)
      {
         if ((pos > 0) && ((pos + 1) < str.length()) && CharUtil::isAlphaNumeric(str[pos - 1]) &&
             CharUtil::isAlphaNumeric(str[pos + 1]))
         {
            return true;
         }
         pos++;
      }
   }
   return false;
}

// --------------------------------------------------------------------------
bool ST_Antecedent::hasAlphaPlusSignAlpha(LgsString& str)
{
   int pos = 0;

   while ((pos != LgsString::npos) && (pos < str.length()))
   {
      if ((pos = str.find('+', pos)) != LgsString::npos)
      {
         if ((pos > 0) && ((pos + 1) < str.length()) && CharUtil::isAlphabetic(str[pos - 1]) &&
             CharUtil::isAlphabetic(str[pos + 1]))
         {
            return true;
         }
         pos++;
      }
   }
   return false;
}

// --------------------------------------------------------------------------
bool ST_Antecedent::hasMathExpression(LgsString& str)
{
   int pos = 0;

   while ((pos != LgsString::npos) && (pos < str.length()))
   {
      if ((pos = str.find_first_of("=+˜±", pos)) != LgsString::npos)
      {
         if ((pos > 0) && !isExcludedMathCharacter(str[pos - 1]))
         {
            return true;
         }
         pos++;
      }
   }
   return false;
}

// --------------------------------------------------------------------------
bool ST_Antecedent::isExcludedMathCharacter(char& ch)
{
   switch (ch)
   {
   case '(':
   case ')':
   case '{':
   case '}':
   case '[':
   case ']':
      return true;

   default:
      if (CharUtil::isBlank(ch))
      {
         return true;
      }
      else
      {
         return false;
      }
   }
}

// --------------------------------------------------------------------------
bool ST_Antecedent::hasAlphaNumericSlashAlphaNumeric(LgsString& str)
{
   int pos = 0;

   while ((pos != LgsString::npos) && (pos < str.length()))
   {
      if ((pos = str.find('/', pos)) != LgsString::npos)
      {
         if ((pos > 0) && ((pos + 1) < str.length()) && CharUtil::isAlphaNumeric(str[pos - 1]) &&
             CharUtil::isAlphaNumeric(str[pos + 1]))
         {
            return true;
         }
         pos++;
      }
   }
   return false;
}

// --------------------------------------------------------------------------
bool ST_Antecedent::hasMiscDigits(LgsString& str)
{
   int pos = 0;

   while ((pos != LgsString::npos) && (pos < str.length()))
   {
      if ((pos = str.find_first_of("/-", pos)) != LgsString::npos)
      {
         if ((pos > 0) && CharUtil::isNumeric(str[pos - 1]))
         {
            return true;
         }
         pos++;
      }
   }
   return false;
}

// --------------------------------------------------------------------------
bool ST_Antecedent::hasEllipsis(LgsString& str)
{
   return (str.find("...") != LgsString::npos);
}

// --------------------------------------------------------------------------
bool ST_Antecedent::hasCatchAllSymbol(LgsString& str)
{
   return (str.find_first_of("@#~$%^&*+_=|£¢:?.!ø°") != LgsString::npos);
}

// --------------------------------------------------------------------------
bool ST_Antecedent::hasMiscSymbol(LgsString& str)
{
   for (LgsString::iterator cntr = str.begin(); cntr != str.end(); cntr++)
   {
      if (CharUtil::isSymbol(*cntr))
      {
         return true;
      }
   }
   return false;
}

//}

