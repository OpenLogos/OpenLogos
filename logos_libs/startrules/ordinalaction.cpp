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
// File - OrdinalAction.cpp
//
// Class - ST_OrdinalAction
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/startrules/ordinalaction.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace Start {

void ST_OrdinalAction::fire(ST_Variable& variable) const
{
   LgsString digits = variable.getSubString(2);
   dump(digits);
   LgsString suffix = variable.getSubString(4);
   dump(suffix);
   bool hyphenated = false;
   int hyphenGroupNumber = 6; // Group number for hyphen if language is English.

   // If language is German, change the hyphen group number.
   if (sourceLocale_.language_ == LLanguage::GermanID)
   {
      hyphenGroupNumber = 8;
   }

   if (!variable.getSubString(hyphenGroupNumber).empty())
   {
      hyphenated = true;
   }

   switch (sourceLocale_.language_)
   {
   case LLanguage::GermanID:
      if (!fireGerman(variable, suffix))
         return;
      break;

   case LLanguage::EnglishID:
      if (fireEnglish(variable, calcSuffix(digits), suffix, hyphenated))
      {
         // remove the hyphen and suffix
         if (hyphenated)
         {
            variable.convertGroup(6, '-', ' ', true);
         }
         // variable.remove(4);
      }
      else
         return;
   break;

   default:
      assert(("invalid language", 0));
   }

   // replace thousand separator to target value
   variable.convertGroup(2, sourceLocale_.thou_sep_, targetLocale_.thou_sep_);
}

bool ST_OrdinalAction::fireEnglish(ST_Variable& variable, SuffixType suffixType, const LgsString& suffix, bool hyphenated) const
{
   char first = suffix[0];

   bool valid = false;
   switch (suffixType)
   {
   case st1:
      if (first == 's')
      {
         if (hyphenated)
         {
            variable.setTokenType(LookupTokenType::tok_AdjAdv_Num_1st_E_Hyphen, 1, true);
         }
         else
         {
            variable.setTokenType(LookupTokenType::tok_AdjAdv_Num_1st_E, 1, true);
         }
         valid = true;
      }
      break;
   case st:
      if (first == 's')
      {
         if (hyphenated)
         {
            variable.setTokenType(LookupTokenType::tok_AdjAdv_Num_st_E_Hyphen, 1, true);
         }
         else
         {
            variable.setTokenType(LookupTokenType::tok_AdjAdv_Num_st_E, 1, true);
         }
         valid = true;
      }
      break;
   case nd:
      if (first == 'n')
      {
         if (hyphenated)
         {
            variable.setTokenType(LookupTokenType::tok_AdjAdv_Num_nd_E_Hyphen, 1, true);
         }
         else
         {
            variable.setTokenType(LookupTokenType::tok_AdjAdv_Num_nd_E, 1, true);
         }
         valid = true;
      }
      break;
   case rd:
      if (first == 'r')
      {
         if (hyphenated)
         {
            variable.setTokenType(LookupTokenType::tok_AdjAdv_Num_rd_E_Hyphen, 1, true);
         }
         else
         {
            variable.setTokenType(LookupTokenType::tok_AdjAdv_Num_rd_E, 1, true);
         }
         valid = true;
      }
      break;
   case th:
      if (first == 't')
      {
         if (hyphenated)
         {
            variable.setTokenType(LookupTokenType::tok_AdjAdv_Num_th_E_Hyphen, 1, true);
         }
         else
         {
            variable.setTokenType(LookupTokenType::tok_AdjAdv_Num_th_E, 1, true);
         }
         valid = true;
      }
      break;
   }
   return valid;
}

bool ST_OrdinalAction::fireGerman(ST_Variable& variable, const LgsString& suffix) const
{
   variable.setTokenType(LookupTokenType::tok_No_04, 2, true);

   if (suffix == LgsString("er"))
      variable.setTokenType(LookupTokenType::tok_Noun_er_G, 4, true);
   else if (suffix == LgsString("ers"))
      variable.setTokenType(LookupTokenType::tok_Noun_ers_G, 4, true);
   else if (suffix == LgsString("ern"))
      variable.setTokenType(LookupTokenType::tok_Noun_ern_G, 4, true);
   else if (suffix == LgsString("erin"))
      variable.setTokenType(LookupTokenType::tok_Noun_erin_G, 4, true);
   else if (suffix == LgsString("erinnen"))
      variable.setTokenType(LookupTokenType::tok_Noun_erinnen_G, 4, true);
   else if (suffix == LgsString("stel"))
      variable.setTokenType(LookupTokenType::tok_Noun_stel_G, 4, true);
   else if (suffix == LgsString("stels"))
      variable.setTokenType(LookupTokenType::tok_Noun_stels_G, 4, true);
   else if (suffix == LgsString("steln"))
      variable.setTokenType(LookupTokenType::tok_Noun_steln_G, 4, true);
   else if (suffix == LgsString("fach"))
      variable.setTokenType(LookupTokenType::tok_Adj_fach, 4, true);
   else if (suffix == LgsString("facher"))
      variable.setTokenType(LookupTokenType::tok_AdjNoun_facher, 4, true);
   else if (suffix == LgsString("faches"))
      variable.setTokenType(LookupTokenType::tok_AdjNoun_faches, 4, true);
   else if (suffix == LgsString("fachem"))
      variable.setTokenType(LookupTokenType::tok_AdjNoun_fachem, 4, true);
   else if (suffix == LgsString("fachen"))
      variable.setTokenType(LookupTokenType::tok_AdjNoun_fachen, 4, true);
   else if (suffix[0] == '%')
   {
      PhraseManager::Key startKey, endKey;
      variable.getKeys(startKey, endKey, 4);
      LWordIterator start = variable.getStartWord() + startKey;
      LWordIterator end = variable.getStartWord() + endKey;

      variable.setTokenType(LookupTokenType::tok_PerC_Sym, start, start + 1, true);
      if (suffix == LgsString("%ig"))
         variable.setTokenType(LookupTokenType::tok_Adj_ig,   start + 1, end, true);
      else if (suffix == LgsString("%ige"))
         variable.setTokenType(LookupTokenType::tok_Adj_ige,  start + 1, end, true);
      else if (suffix == LgsString("%igen"))
         variable.setTokenType(LookupTokenType::tok_Adj_igen, start + 1, end, true);
      else if (suffix == LgsString("%iger"))
         variable.setTokenType(LookupTokenType::tok_Adj_iger, start + 1, end, true);
      else if (suffix == LgsString("%igen"))
         variable.setTokenType(LookupTokenType::tok_Adj_igen, start + 1, end, true);
      else if (suffix == LgsString("%iges"))
         variable.setTokenType(LookupTokenType::tok_Adj_iges, start + 1, end, true);
   }
   else
   {
      // do nothing - so that it will be looked up in the dictionary
      return false;
   }

   return true;
}

ST_OrdinalAction::SuffixType ST_OrdinalAction::calcSuffix(const LgsString& digits)
{
   assert(digits.length() != 0);

   char last = digits[digits.length() - 1];
   if (last == '0' || last >= '4')
      return th;

   if (digits.length() == 2 && digits[0] == '1')
      return th;

   switch (last)
   {
   default:
      assert(("shouldn't get here", 0));
   case '1':
      return digits.length() == 1 ? st1 : st;
   case '2':
      return nd;
   case '3':
      return rd;
   }
}
//}

