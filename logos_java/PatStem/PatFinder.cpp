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
/*******************************************************************
 *
 *    DESCRIPTION:   Picks up pat number based on the best ending match.
 *
 *    AUTHOR:        Vlad Yakimetz
 *
 *    HISTORY:       Created 05/08/98
 *
 *******************************************************************/

#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/llanguage.h>
#include "PatFinder.h"


PatFinder
::PatFinder(const LLanguage& language_, const LgsString& word_) 
   : _language(language_), _word(word_), _matchingPat(0)
{
}   


PatFinder
::~PatFinder()
{
}   


/******************************************************************************                        
 ** isBestMatch
 *
 *  FILENAME: \logos_dev\lgs_stemgen\test\stemgen.cpp
 *
 *  PARAMETERS:   int patNumber_
 *                const string& ending_
 *
 *  DESCRIPTION:  tries to find the best matching ending for the word
 *                according to the following rules:
 *
 *                   $ending  -  'ending' must match the word exactly
 *
 *                   all else being equal the longest ending is 
 *                   considered a better match
 *
 *                   a character takes precedence over any symbol
 *
 *                   symbol precedence is as follows (highest to lowest):
 *                      ^, |  - any one char that [can have, has] a diacritical mark
 *                      ?, #  - any one [consonant, vowel]
 *                      *     - any one or several consonants
 *                      %     - any one or several characters
 *
 *  RETURNS:      true if unconditionally best match is found, 
 *                otherwise records the best known match and returns false
 *
 */
bool
PatFinder
::isBestMatch(int patNumber_, const LgsString& ending_)
{
   if (ending_.find_first_of("^|") == LgsString::npos 
                           && ending_.size() < _ending.size() 
      || ending_[0] == '$' && ending_.size() - 1 != _word.size()
      || ending_[0] != '$' && ending_.size() > _word.size())
   {
      return false;
   }
   
   if (ending_[0] == '$' && !_compareNoCase(_word, ending_.substr(1)))
   {
      _ending = ending_;
      _matchingPat = patNumber_;
      return true;
   }

   bool match = true;
   LgsString::const_reverse_iterator e_crit = ending_.rbegin();   
   LgsString::const_reverse_iterator w_crit = _word.rbegin();
   
   while (match && e_crit != ending_.rend() && w_crit != _word.rend())
   {
      if (!_isSymbol(*e_crit))
      {
         if (CharUtil::lower(*e_crit) != CharUtil::lower(*w_crit)) 
            match = false;
         else 
            ++e_crit, ++w_crit;
      }
      else if (!_isCompatible(*e_crit, *w_crit))
      {
         match = false;
      }
      else if (*e_crit != '*' && *e_crit != '%')
      {
         ++e_crit, ++w_crit;
      }
      else 
      {
         char sym = *e_crit++;
         char nxt = e_crit != ending_.rend() ? *e_crit : 0;
         
         if (nxt && _isSymbol(nxt)) match = false;

         while (++w_crit != _word.rend() && _isCompatible(sym, *w_crit))
         {
            if (nxt && _isCompatible(nxt, *w_crit))
            {
               match = true;
               break;
            }
         }
      }
   }
   
   if (match)
   {
      if (ending_.size() > _ending.size())
      {
         _ending = ending_;
         _matchingPat = patNumber_;
         return false;
      }
   
      e_crit = ending_.rbegin();
      
      LgsString::reverse_iterator e_rit = _ending.rbegin();   
      
      while (e_crit != ending_.rend())
      {
         switch (_precedes(*e_crit++, *e_rit++))
         {
         case 1:
            _ending = ending_;
            _matchingPat = patNumber_;
            return false;
         case 0:
            break;
         case -1:
            return false;
            break;
         }
      }
   }

   return false;
}   


int  
PatFinder
::_compareNoCase(const LgsString& str1_, const LgsString& str2_)
{
   if (str1_.size() != str2_.size())
   {
      return str1_.size() > str2_.size() ? 1 : -1;
   }

   LgsString::const_iterator it1, it2;
  
   for (it1 = str1_.begin(), it2 = str2_.begin(); 
      it1 != str1_.end() && CharUtil::lower(*it1) == CharUtil::lower(*it2); 
      ++it1, ++it2)
   {
   }
   
   return it1 == str1_.end() ? 0 
      : (CharUtil::lower(*it1) > CharUtil::lower(*it2) ? 1 : -1);
}   


bool
PatFinder
::_isCompatible(char ch1_, char ch2_)
{
   if (!_isSymbol(ch1_) || _isSymbol(ch2_)) 
   {
      return false;
   }

   bool rc = false;
   
   switch (ch1_)
   {
   case '^':
      if (_language.isDiacritic(ch2_)) rc = true;
      break;
   case '|':
      if (_language.isDiacritic(ch2_) && ch2_ & 0x80) rc = true;
      break;
   case '?':
   case '*':
      if (_language.isConsonant(ch2_)) rc = true;
      break;
   case '#':
      if (_language.isVowel(ch2_)) rc = true;
      break;
   case '%':
      rc = true;
      break;
   default:
      cout << "Unknown symbol: " << ch1_ << endl;
      break;
   }
   
   return rc;
}  
 

bool
PatFinder
::_isSymbol(char ch_)
{
   bool rc = false;

   switch (ch_)
   {
   case '?':
   case '#':
   case '^':
   case '|':
   case '*':
   case '%':
      rc = true;
      break;
   default:
      break;
   }   
   
   return rc;
}  


/******************************************************************************                        
 ** _precedes
 *
 *  FILENAME: \logos_dev\lgs_stemgen\test\stemgen.cpp
 *
 *  PARAMETERS: char ch1_, 
 *              char ch2_
 *
 *  DESCRIPTION:  compares precedence of two string elements
 *
 *  RETURNS:      1  if ch1_ has higher precedence than ch2_
 *                0  if ch1_ has the same precedence as ch2_
 *               -1  if ch1_ has lower precedence than ch2_
 *
 */
int  
PatFinder
::_precedes(char ch1_, char ch2_)
{
   int pr = 0;
   
   if (!_isSymbol(ch1_) && _isSymbol(ch2_))
   {
      pr = 1;
   }
   else if (_isSymbol(ch1_) && !_isSymbol(ch2_))
   {
      pr = -1;
   }
   else if (_isSymbol(ch1_) && _isSymbol(ch2_) && ch1_ != ch2_)
   {
      switch (ch1_)
      {
      case '^':
      case '|':
         pr = 1;
         break;
      
      case '?':
      case '#':
         switch (ch2_)
         {
         case '^':
         case '|':
            pr = -1;
            break;
         case '?':
         case '#':
            pr = 0;
            break;
         case '*':
         case '%':
            pr = 1;
            break;
         default:
            cout << "Unknown symbol: " << ch2_ << endl;
            pr = 1;
            break;
         }
         break;
      
      case '*':
         switch (ch2_)
         {
         case '^':
         case '|':
         case '?':
         case '#':
            pr = -1;
            break;
         case '%':
            pr = 1;
            break;
         default:
            cout << "Unknown symbol: " << ch2_ << endl;
            pr = 1;
            break;
         }
         break;
      case '%':
         switch (ch2_)
         {
         case '^':
         case '|':
         case '?':
         case '#':
         case '*':
            pr = -1;
            break;
         default:
            cout << "Unknown symbol: " << ch2_ << endl;
            pr = 1;
            break;
         }
         break;
      
      default:
         cout << "Unknown symbol: " << ch1_ << endl;
         pr = -1;
         break;
      }
   }
   
   return pr;
}   
