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
// File - NumericHyphenAction.cpp
//
// Class - ST_NumericHyphenAction
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/startrules/numerichyphenaction.h>
#include <logos_libs/translutility/translcommonobjects.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace Start {

void ST_NumericHyphenAction::fire(ST_Variable& variable) const
{
   bool separators;
   LgsString digits;
   int val;
   if (variable.getSubString(1).length() != 0)
   {
      if (variable.getSubString(11).length() != 0)
      {
         variable.convertGroup(12, sourceLocale_.thou_sep_, targetLocale_.thou_sep_, false);
         digits = variable.getSubString(12);
         val = parseNumber(digits, separators);
         variable.setTokenType(calculateRange(val), 12, false);
      }
      variable.convertGroup(2, sourceLocale_.thou_sep_, targetLocale_.thou_sep_, false);
      digits = variable.getSubString(2);
      val = parseNumber(digits, separators);
      variable.setTokenType(calculateRange(val), 2, true);

      if (variable.getSubString(7).length() != 0)
      {
         variable.remove(5);
      }
      else
      {
         variable.convertGroup(5, '-', ' ', true);
      }
      if (variable.getSubString(10).length() != 0 && (TranslCommonObjects::GetSourceLanguage()->id() != LLanguage::GermanID))
      {
         variable.convertGroup(10, '-', ' ', true);
      }
      if (variable.getSubString(15).length() != 0)
      {
         variable.convertGroup(15, '-', ' ', true);
      }
      if (variable.getSubString(17).length() != 0 && (TranslCommonObjects::GetSourceLanguage()->id() != LLanguage::GermanID))
      {
         variable.convertGroup(17, '-', ' ', true);
      }
   }
   else
   {
      variable.convertGroup(20, sourceLocale_.thou_sep_, targetLocale_.thou_sep_, false);
      digits = variable.getSubString(20);
      val = parseNumber(digits, separators);
      variable.setTokenType(calculateRange(val), 20, true);
      if (variable.getSubString(23).length() != 0)
      {
         variable.convertGroup(23, '-', ' ', true);
      }
   }
   
//   variable.convertGroup(0, '-', ' ', true);
}

int ST_NumericHyphenAction::parseNumber(const LgsString& digits, bool& separators)
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

LookupTokenType::Type ST_NumericHyphenAction::calculateRange(int val)
{
   if (val == 0)
      return LookupTokenType::tok_No_Hyphen_0;
   else if (val == 1)
      return LookupTokenType::tok_No_Hyphen_1;
   else if (val == 2)
      return LookupTokenType::tok_No_Hyphen_2;
   else if (val < 5)
      return LookupTokenType::tok_No_Hyphen_3_4;
   else if (val < 32)
      return LookupTokenType::tok_No_Hyphen_5_31;
   else if (val < 100)
      return LookupTokenType::tok_No_Hyphen_32_99;
   else if (val < 1000)
      return LookupTokenType::tok_No_Hyphen_100_999;
   else if (val < 1400)
      return LookupTokenType::tok_No_Hyphen_1000_1399;
   else if (val <= 2100)
      return LookupTokenType::tok_No_Hyphen_1400_2100;
   else
      return LookupTokenType::tok_No_Hyphen_2101_;
}

//}

