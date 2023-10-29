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
//----------------------------------------------------------------------------
// File - Factory.cpp
//
// Class - ST_Factory
//
//----------------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/ruleengine/document.h>
#include <logos_libs/startrules/factory.h>
#include <logos_libs/startrules/antecedent.h>
#include <logos_libs/startrules/consequent.h>
#include <logos_libs/startrules/adjnumberaction.h>
#include <logos_libs/startrules/angledbracketsaction.h>
#include <logos_libs/startrules/catchallaction.h>
#include <logos_libs/startrules/lookupaction.h>
#include <logos_libs/startrules/dateaction.h>
#include <logos_libs/startrules/digittimesdigitaction.h>
#include <logos_libs/startrules/dottedalphanumaction.h>
#include <logos_libs/startrules/ellipsisaction.h>
#include <logos_libs/startrules/fractionaction.h>
#include <logos_libs/startrules/houraction.h>
#include <logos_libs/startrules/mathexpressionaction.h>
#include <logos_libs/startrules/mathtimesminusaction.h>
#include <logos_libs/startrules/miscsymbolaction.h>
#include <logos_libs/startrules/numericaction.h>
#include <logos_libs/startrules/numerichyphenaction.h>
#include <logos_libs/startrules/numericrangeaction.h>
#include <logos_libs/startrules/ordinalaction.h>
#include <logos_libs/startrules/outlinebosaction.h>
#include <logos_libs/startrules/outlinemidaction.h>
#include <logos_libs/startrules/outlineromanbosaction.h>
#include <logos_libs/startrules/outlineromancapbosaction.h>
#include <logos_libs/startrules/outlineromancapmidaction.h>
#include <logos_libs/startrules/outlineromanmidaction.h>
#include <logos_libs/startrules/shortyearaction.h>
#include <logos_libs/startrules/timeaction.h>
#include <logos_libs/startrules/wordpluswordaction.h>
#include <logos_libs/startrules/yearaction.h>
#include <logos_libs/startrules/yearrangeaction.h>
#include <logos_libs/startrules/multihyphenaction.h>
#include <logos_libs/startrules/slashalpnumactn.h>
#include <logos_libs/startrules/pairedfloatingsinglehyphens1action.h>
#include <logos_libs/startrules/pairedfloatingsinglehyphens2action.h>
#include <logos_libs/startrules/pairedfloatingmultiplehyphens1action.h>
#include <logos_libs/startrules/pairedfloatingmultiplehyphens2action.h>
#include <logos_libs/startrules/pairedembeddedhyphensaction.h>
#include <logos_libs/startrules/hyphensequenceaction.h>
#include <logos_libs/startrules/digits.h>
#include <logos_libs/startrules/twodigitdate.h>
#include <logos_libs/startrules/germanordinal.h>
#include <logos_libs/startrules/miscdigitsaction.h>
#include <logos_libs/startrules/alpnumhyphenaction.h>
#include <logos_libs/startrules/multihyphenalpnumaction.h>




#define EXIT_START_RULES throw "Fatal error in start rules"

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace Start {

ST_Factory::ST_Factory(const LgsString& description, istream* input,
                       const ST_Locale* sourceLocale, const ST_Locale* targetLocale,
                       char quote)
           :RE_StreamFactory<ST_Variable, ST_Engine, RE_SequentialRuleBase<ST_Variable> >(description, input),
            sourceLocale_(sourceLocale),
            targetLocale_(targetLocale),
            quote_(quote)
{
   LgsString quoteStr = LgsString(1, quote);
   LgsString period = LgsString(".");
   LgsString lp = LgsString("(");
   LgsString rp = LgsString(")");

   macros_[quoteStr + period + quoteStr] = RegularExpression::escapeForm(period);

   macros_[quoteStr + lp + quoteStr] = RegularExpression::escapeForm(lp);

   macros_[quoteStr + rp + quoteStr] = RegularExpression::escapeForm(rp);

   macros_[quoteStr + LgsString("dec_sep") + quoteStr] =
                                RegularExpression::escapeForm(sourceLocale_->dec_sep_);

   macros_[quoteStr + LgsString("thou_sep") + quoteStr] =
                                RegularExpression::escapeForm(sourceLocale_->thou_sep_);

   macros_[quoteStr + LgsString("date_sep") + quoteStr] =
                                RegularExpression::escapeForm(sourceLocale_->date_sep_);

   macros_[quoteStr + LgsString("time_sep") + quoteStr] =
                                RegularExpression::escapeForm(sourceLocale_->time_sep_);

   macros_[quoteStr + LgsString("time_dec_sep") + quoteStr] =
                                RegularExpression::escapeForm(sourceLocale_->time_dec_sep_);

   macros_[quoteStr + LgsString("ord_suffix") + quoteStr] = sourceLocale_->ord_suffix_;

   macros_[quoteStr + LgsString("time_suffix") + quoteStr] = sourceLocale_->time_suffix_;
}

RE_Rule<ST_Variable>* ST_Factory::getNextRule()
{
   static LgsString ruleKey = "rule> ";
   static LgsString searchKey = "search ";
   static LgsString noBorderKey = "no_border ";
   static LgsString leftBorderKey = "left_border ";
   static LgsString rightBorderKey = "right_border ";

   // note: when this is called the line_ member is already filled with the last readLine()
   // before exiting or continue - readLine() needs to be called again to refresh the buffer

   int ruleNo = 0; // current rule number

   while (moreData())
   {
      // check keywords at start of rule to determine rule type
      if (isBlankLine() || isCommentLine())
      {
         readLine();
         continue;
      }

      // find ruleKey
      if (!search(ruleKey))
      {
         displayError("'rule>' expected");
         EXIT_START_RULES;
      }
      skipWhiteSpace();

      // create a document for the rule
      // parse for a pattern - the documentation in angled braces
      RE_Document* document = new RE_Document(++ruleNo,
      parsePattern(false, true) ? getPattern() : LgsString());

      readLine();

      // parse the search antecedent for the rule

      // find searchKey
      if (!search(searchKey))
      {
         displayError("'search' expected");
         EXIT_START_RULES;
      }
      skipWhiteSpace();

      bool leftBorder = false, rightBorder = false;
      if (search(noBorderKey))
      {
      }
      else if (search(leftBorderKey))
      {
         leftBorder = true;
      }
      else if (search(rightBorderKey))
      {
         rightBorder = true;
      }
      else
      {
         leftBorder = true;
         rightBorder = true;
      }

      RegularExpression::Borders borders;
      if (leftBorder && rightBorder)
         borders = RegularExpression::Both;
      else if (leftBorder)
         borders = RegularExpression::Left;
      else if (rightBorder)
         borders = RegularExpression::Right;
      else
         borders = RegularExpression::None;

      // find search-id
      if (!parsePattern())
      {
         displayError("search-id expected");
         EXIT_START_RULES;
      }

      // construct a suitable action
      LgsString ruleName = getPattern();
      RE_Consequent<class ST_Variable>* action = makeAction(ruleName);
      skipWhiteSpace();

      // construct an antecedent
      ST_Antecedent* antecedent = parseAntecedent(borders, ST_Antecedent::antecedentType(ruleName));

      // now build the rule and return it
      RE_Rule<ST_Variable>* rule = new RE_Rule<ST_Variable>(antecedent, action, document, ruleName);
      return rule;
   }

   return 0;
}

RE_Consequent<class ST_Variable>* ST_Factory::makeAction(const LgsString& id)
{
   switch (ST_Antecedent::antecedentType(id))
   {
   case ST_Antecedent::PairedFloatingSingleHyphens1TYPE:
      return new ST_PairedFloatingSingleHyphens1Action(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::PairedFloatingSingleHyphens2TYPE:
      return new ST_PairedFloatingSingleHyphens2Action(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::PairedFloatingMultipleHyphens1TYPE:
      return new ST_PairedFloatingMultipleHyphens1Action(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::PairedFloatingMultipleHyphens2TYPE:
      return new ST_PairedFloatingMultipleHyphens2Action(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::PairedEmbeddedHyphensTYPE:
      return new ST_PairedEmbeddedHyphensAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::HyphenSequenceTYPE:
      return new ST_HyphenSequenceAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::OutlineRomanBosTYPE:
      return new ST_OutlineRomanBOSAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::OutlineRomanCapBosTYPE:
      return new ST_OutlineRomanCapBOSAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::MultiHyphenatedTYPE:
      return new ST_MultiHyphenAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::OutlineBosTYPE:
      return new ST_OutlineBOSAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::OrdinalTYPE:
      return new ST_OrdinalAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::GermanOrdinalTYPE:
      return new ST_GermanOrdinalAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::TwoDigitDateTYPE:
      return new ST_TwoDigitDateAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::DateTYPE:
      return new ST_DateAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::EnglishHourTYPE:
      return new ST_HourAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::TimeTYPE:
      return new ST_TimeAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::ShortYearTYPE:
      return new ST_ShortYearAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::YearSTYPE:
      return new ST_YearAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::DigitSTYPE:
      return new ST_DigitAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::AdjNumberTYPE:
      return new ST_AdjNumberAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::NumericTYPE:
      return new ST_NumericAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::YearRangeTYPE:
      return new ST_YearRangeAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::NumericRangeTYPE:
      return new ST_NumericRangeAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::MathTimesMinusTYPE:
      return new ST_MathTimesMinusAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::DottedAlphanumTYPE:
      return new ST_DottedAlphanumAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::FractionTYPE:
      return new ST_FractionAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::DigitTimesDigitTYPE:
      return new ST_DigitTimesDigitAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::AngledBracketsTYPE:
      return new ST_AngledBracketsAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::NumericHyphenAlphanumTYPE:
      return new ST_NumericHyphenAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::AlphanumHyphenNumericTYPE:
      return new ST_AlpNumHyphenAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::MultiHyphenAlphanumTYPE:
      return new ST_MultiHyphenAlpNumAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::WordPlusWordTYPE:
      return new ST_WordPlusWordAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::MathExpressionTYPE:
      return new ST_MathExpressionAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::EllipsisTYPE:
      return new ST_EllipsisAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::MiscSymbolTYPE:
      return new ST_MiscSymbolAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::CatchAllTYPE:
      return new ST_CatchAllAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::SlashAlpanumTYPE:
      return new ST_SlashAlpNumAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::MiscDigitsTYPE:
      return new ST_MiscDigitsAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::LookupNeededTYPE:
      return new ST_LookupAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::OutlineMidTYPE:
      return new ST_OutlineMidAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::OutlineRomanCapMidTYPE:
      return new ST_OutlineRomanCapMidAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::OutlineRomanMidTYPE:
      return new ST_OutlineRomanMidAction(*sourceLocale_, *targetLocale_);
   case ST_Antecedent::InvalidTYPE:
   default:
      displayError("Invalid search id");
      EXIT_START_RULES;
      return 0;         // keep compiler happy
   }
}

ST_Antecedent* ST_Factory::parseAntecedent(RegularExpression::Borders borders,
                                           ST_Antecedent::ST_Type antecedentType)
{
   static LgsString default_key = "default";

   // find search pattern - and adjust for macros between '`' characters
   LgsString pattern;

   if (!parsePattern())
   {
      displayError("Search pattern expected");
      EXIT_START_RULES;
   }

   pattern = getPattern();
   if (!checkEOL())
   {
      EXIT_START_RULES;
   }
   readLine();
   replaceMacros(pattern);

   return new ST_Antecedent(pattern, borders, antecedentType);
}

void ST_Factory::replaceMacros(LgsString& pattern)
{
   for (LgsMap(LgsString, LgsString)::const_iterator iter = macros_.begin(); iter != macros_.end(); iter++)
   {
      LgsString::size_type pos;
      do
      {
         if ((pos = pattern.find(iter->first)) != LgsString::npos)
         {
            int length = iter->first.length();
            pattern.replace(pos, length, iter->second);
         }
      }
      while (pos != LgsString::npos);
   }
}

//}

