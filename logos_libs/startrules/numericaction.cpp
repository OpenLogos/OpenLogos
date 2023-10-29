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
// File - NumericAction.cpp
//
// Class - ST_NumericAction
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/startrules/numericaction.h>
#include <logos_libs/regex/charutil.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace Start {

void ST_NumericAction::fire(ST_Variable& variable) const
{
   bool sign = variable.getSubString(1).length() != 0;
   dump(sign);
   bool currency = variable.getSubString(2).length() != 0;
   dump(currency);
   LgsString digits = variable.getSubString(6);
   dump(digits);
   LgsString decimals;
   if (digits.length() != 0)
   {
      decimals = variable.getSubString(9);
   }
   else
   {
      decimals = variable.getSubString(11);
   }
   dump(decimals);
   bool fraction = variable.getSubString(13).length() != 0;
   dump(fraction);
   LgsString units = variable.getSubString(14);
   dump(units);
 

//   if (0 != decimals.length())
//   {
//      if (digits.length() != 0)
//      {
//         variable.convertGroup(9, sourceLocale_.dec_sep_, targetLocale_.dec_sep_, false);
//      }
//      else
//      {
//         variable.convertGroup(11, sourceLocale_.dec_sep_, targetLocale_.dec_sep_, false);
//      }
//   }
//   if (digits.length() != 0)
//   {
//      variable.convertGroup(6, sourceLocale_.thou_sep_, targetLocale_.thou_sep_, true);
//   }

   // check for 'mal' unit - language must be german
   if (units == LgsString("mal"))
   {
      if (sourceLocale_.language_ != LLanguage::GermanID)
         return;
   }

   // check at least one of the numeric parts is not empty
   if (digits.length() == 0 && decimals.length() <= 1 && !fraction)
      return;

   // can't have both currency and units
   // note: quotes do not come through so " and ' are inches and feet resp.
   if (currency && units.length() != 0)
   {
      variable.setTokenType(LookupTokenType::tok_Unfound_Alpha_Num, true);
      return;
   }

   // can't have both decimals and a fraction
   if (decimals.length() != 0 && fraction)
   {
      variable.setTokenType(LookupTokenType::tok_Unfound_Alpha_Num, true);
      return;
   }

   // write currency token type
   if (currency)
   {
      if (sign)
         // combine sign and currency sybmol together
         variable.setTokenType(LookupTokenType::tok_Monetary_Sym, "12", true);
      else
         variable.setTokenType(LookupTokenType::tok_Monetary_Sym, 2, true);
   }

   // write units token types
   bool unitsMonetary = false, unitsMal = false;
   if (units.length() != 0)
   {
      switch (units[0])
      {
      case '¢':
         unitsMonetary = true;
         variable.setTokenType(LookupTokenType::tok_Monetary_Sym, 14, true);
         break;
      case '\'':
         variable.setTokenType(LookupTokenType::tok_FT_Sym, 14, true);
         break;
      case '"':
         variable.setTokenType(LookupTokenType::tok_IN_Sym, 14, true);
         break;
      case '%':
         variable.setTokenType(LookupTokenType::tok_PerC_Sym, 14, true);
         break;
      case '∞':
         if (units.length() == 1)
            variable.setTokenType(LookupTokenType::tok_Deg_Sym, 14, true);
         else
         {
            PhraseManager::Key startKey, endKey;
            variable.getKeys(startKey, endKey, 14);
            LWordIterator start = variable.getStartWord() + startKey;
            LWordIterator end = variable.getStartWord() + endKey;

            variable.setTokenType(LookupTokenType::tok_Deg_Sym, start, start + 1, true);
            variable.setTokenType((units[1]=='C')? LookupTokenType::tok_TempC_Scale: LookupTokenType::tok_TempF_Scale,
                                  start + 1, end, true);
         }
         break;
      case 'C':
      case 'F':
         variable.setTokenType(LookupTokenType::tok_Temp_Scale, 14, true);
         break;
      case 'm':           // German mal
         unitsMal = true;
         variable.setTokenType(LookupTokenType::tok_Adv_mal, 14, true);
         break;
      default:
         assert(("invalid value", 0));
      }
   }

   // write numeric token types
   // if we have digits only, check ranges
   if (!sign && decimals.length() == 0 && !fraction)
   {
      bool separators;
      int val = parseNumber(digits, separators);
      variable.setTokenType(calculateRange(val, separators), 6, true);
      return;
   }
   // calculate token types for expressions that contain a number as well as currency or units
   LookupTokenType::Type tokenType = LookupTokenType::tok_none;
   //if (unitsMal)
   //    tokenType = LookupTokenType::tok_No_04;
   //else if (unitsMonetary)
   //    tokenType = LookupTokenType::tok_No_Curr_14;
   //else if (units.length() != 0)
   //    tokenType = LookupTokenType::tok_No_Non_Curr_Unit;

   if (tokenType == LookupTokenType::tok_none)
   {
      // assign token to whole expression - there are no units or currency
      if (sign)
      {
         // signed number
         variable.setTokenType(LookupTokenType::tok_No_16_14, "13", true);
      }
      // Block below has been commented out since handling  one or
      // or more decimals have to be handled in the same way
      //else if (decimals.length() == 1 && !fraction)
      //{
      //    if ((sourceLocale_.dec_sep_ == '.' && variable.pastEndPos()) ||
      //        sourceLocale_.dec_sep_ == ',')
      //    {
      //        // set the range of the numeric portion
      //        bool separators;
      //        int val = parseNumber(digits, separators);
      //        variable.setTokenType(calculateRange(val, separators), 3, true);
      //    }
      //    else
      //        variable.setTokenType(LookupTokenType::tok_Unfound_Alpha_Num, true);
      //}
      else if (decimals.length() >= 1 && digits.length() == 0)
         variable.setTokenType(LookupTokenType::tok_No_Decimal, 3, true);
      else if (fraction && digits.length() == 0)
         variable.setTokenType(LookupTokenType::tok_No_Fraction, 3, true);
      else if (fraction || decimals.length() >= 1)
         variable.setTokenType(LookupTokenType::tok_No_Mixed, 3, true);
   }
   else
   {
      // assign only to parts of the expression where the token has not yet been assigned
      LWordIterator start, end;
      variable.getBlankTokenRange(start, end);
      variable.setTokenType(tokenType, start, end, true);

      // unset the final comma or period if it does not belong to the expression
      if (decimals.length() == 1 && !fraction && units.length() == 0 &&
          ((sourceLocale_.dec_sep_ == '.' && variable.pastEndPos()) || sourceLocale_.dec_sep_ == ','))
      {  
         if (digits.length() != 0)
         {
            variable.setTokenType(LookupTokenType::tok_none, 10, true);
         }
         else
         {
            variable.setTokenType(LookupTokenType::tok_none, 12, true);
         }
      }
   }
}

int ST_NumericAction::parseNumber(const LgsString& digits, bool& separators)
{
   int ret = 0;
   separators = false;
   for (LgsString::const_iterator iter = digits.begin(); iter != digits.end(); iter++)
   {
      if (CharUtil::isNumeric(*iter))
      {
         ret *= 10;
         ret += *iter - '0';
         if (ret > 10000)
            return ret;
      }
      else
         separators = true;
   }
   return ret;
}

LookupTokenType::Type ST_NumericAction::calculateRange(int val, bool separators)
{
   if (val == 0)
      return LookupTokenType::tok_No_Range_0;
   else if (val == 1)
      return LookupTokenType::tok_No_Range_1;
   else if (val == 2)
      return LookupTokenType::tok_No_Range_2;
   else if (val < 5)
      return LookupTokenType::tok_No_Range_3_4;
   else if (val < 32)
      return LookupTokenType::tok_No_Range_5_31;
   else if (val < 100)
      return LookupTokenType::tok_No_Range_32_99;
   else if (val < 1000)
      return LookupTokenType::tok_No_Range_100_999;
   else if (val < 1400)
      return LookupTokenType::tok_No_Range_1000_1399;
   else if (val <= 2100)
      return separators ? LookupTokenType::tok_No_Range_1400_2100 : LookupTokenType::tok_Poss_Date;
   else
      return LookupTokenType::tok_No_Range_2101_;
}

//}

