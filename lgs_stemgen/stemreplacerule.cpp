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
 *    DESCRIPTION:
 *
 *    AUTHOR:
 *
 *    HISTORY:    
 *
 *******************************************************************/

#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/llanguage.h>
#include <lgs_stemgen/stemreplacerule.h>

const int
StemReplaceRule
::_xRefTable[] =
{
    0,  0,  0,  0,  0, 26, 26,  2,  4,  4,  4,  4,  8,  8,  8,  8, 
    3, 13, 14, 14, 14, 14, 14, 26, 26, 20, 20, 20, 20, 24, 15, 26,  
   32, 32, 32, 32, 32, 26, 26, 34, 36, 36, 36, 36, 40, 40, 40, 40, 
   35, 45, 46, 46, 46, 46, 46, 26, 26, 52, 52, 52, 52, 56, 47, 56
};   

const char
StemReplaceRule
::_accentedUnaccentedMap[_vowelCount*2][3] =
{
   {'¿', 'ƒ', 'A'},
   {'‡', '‰', 'a'},
   {'»', 'À', 'E'},
   {'Ë', 'Î', 'e'},
   {'Ã', 'œ', 'I'},
   {'Ï', 'Ô', 'i'},
   {'“', '÷', 'O'},
   {'Ú', 'ˆ', 'o'},
   {'Ÿ', '‹', 'U'},
   {'˘', '¸', 'u'}
};

const char 
StemReplaceRule
::_accentTable[_numAccentRows][10] =
{  // 0    1    2    3    4    5    6    7    8    9
   { 'A', '¡', '¿', '¬', 'ƒ', 'A', '√', 'A', 'A', 'A' }, //0
   { 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B' }, //1
   { 'C', 'C', 'C', 'C', 'C', '«', 'C', 'C', 'C', 'C' }, //2
   { 'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D' }, //3
   { 'E', '…', '»', ' ', 'À', 'E', 'E', 'E', 'E', 'E' }, //4
   { 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F' }, //5
   { 'G', 'G', 'G', 'G', 'G', 'G', 'G', 'G', 'G', 'G' }, //6
   { 'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H' }, //7
   { 'I', 'Õ', 'Ã', 'Œ', 'œ', 'I', 'I', 'I', 'I', 'I' }, //8
   { 'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J' }, //9
   { 'K', 'K', 'K', 'K', 'K', 'K', 'K', 'K', 'K', 'K' }, //10
   { 'L', 'L', 'L', 'L', 'L', 'L', 'L', 'L', 'L', 'L' }, //11
   { 'M', 'M', 'M', 'M', 'M', 'M', 'M', 'M', 'M', 'M' }, //12
   { 'N', 'N', 'N', 'N', 'N', 'N', '—', 'N', 'N', 'N' }, //13
   { 'O', '”', '“', '‘', '÷', 'O', '’', 'O', 'O', 'O' }, //14
   { 'P', 'P', 'P', 'P', 'P', 'P', 'P', 'P', 'P', 'P' }, //15
   { 'Q', 'Q', 'Q', 'Q', 'Q', 'Q', 'Q', 'Q', 'Q', 'Q' }, //16
   { 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R' }, //17
   { 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S' }, //18
   { 'T', 'T', 'T', 'T', 'T', 'T', 'T', 'T', 'T', 'T' }, //19
   { 'U', '⁄', 'Ÿ', '€', '‹', 'U', 'U', 'U', 'U', 'U' }, //20
   { 'V', 'V', 'V', 'V', 'V', 'V', 'V', 'V', 'V', 'V' }, //21
   { 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W' }, //22
   { 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X' }, //23
   { 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y' }, //24
   { 'Z', 'Z', 'Z', 'Z', 'Z', 'Z', 'Z', 'Z', 'Z', 'Z' }, //25
   {'\0','\0','\0','\0','\0','\0','\0','\0','\0','\0' }, //26
   {'\0','\0','\0','\0','\0','\0','\0','\0','\0','\0' }, //27
   {'\0','\0','\0','\0','\0','\0','\0','\0','\0','\0' }, //28
   {'\0','\0','\0','\0','\0','\0','\0','\0','\0','\0' }, //29
   {'\0','\0','\0','\0','\0','\0','\0','\0','\0','\0' }, //30
   {'\0','\0','\0','\0','\0','\0','\0','\0','\0','\0' }, //31
   { 'a', '·', '‡', '‚', '‰', 'a', '„', 'a', 'a', 'a' }, //32
   { 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b' }, //33
   { 'c', 'c', 'c', 'c', 'c', 'Á', 'c', 'c', 'c', 'c' }, //34
   { 'd', 'd', 'd', 'd', 'd', 'd', 'd', 'd', 'd', 'd' }, //35
   { 'e', 'È', 'Ë', 'Í', 'Î', 'e', 'e', 'e', 'e', 'e' }, //36
   { 'f', 'f', 'f', 'f', 'f', 'f', 'f', 'f', 'f', 'f' }, //37
   { 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g' }, //38
   { 'h', 'h', 'h', 'h', 'h', 'h', 'h', 'h', 'h', 'h' }, //39
   { 'i', 'Ì', 'Ï', 'Ó', 'Ô', 'i', 'i', 'i', 'i', 'i' }, //40
   { 'j', 'j', 'j', 'j', 'j', 'j', 'j', 'j', 'j', 'j' }, //41
   { 'k', 'k', 'k', 'k', 'k', 'k', 'k', 'k', 'k', 'k' }, //42
   { 'l', 'l', 'l', 'l', 'l', 'l', 'l', 'l', 'l', 'l' }, //43
   { 'm', 'm', 'm', 'm', 'm', 'm', 'm', 'm', 'm', 'm' }, //44
   { 'n', 'n', 'n', 'n', 'n', 'n', 'Ò', 'n', 'n', 'n' }, //45
   { 'o', 'Û', 'Ú', 'Ù', 'ˆ', 'o', 'ı', 'o', 'o', 'o' }, //46
   { 'p', 'p', 'p', 'p', 'p', 'p', 'p', 'p', 'p', 'p' }, //47
   { 'q', 'q', 'q', 'q', 'q', 'q', 'q', 'q', 'q', 'q' }, //48
   { 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r' }, //49
   { 's', 's', 's', 's', 's', 's', 's', 's', 's', 's' }, //50
   { 't', 't', 't', 't', 't', 't', 't', 't', 't', 't' }, //51
   { 'u', '˙', '˘', '˚', '¸', 'u', 'u', 'u', 'u', 'u' }, //52
   { 'v', 'v', 'v', 'v', 'v', 'v', 'v', 'v', 'v', 'v' }, //53
   { 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w' }, //54
   { 'x', 'x', 'x', 'x', 'x', 'x', 'x', 'x', 'x', 'x' }, //55
   { 'y', 'y', 'y', 'y', 'y', 'y', 'y', 'y', 'y', 'y' }, //56
   { 'z', 'z', 'z', 'z', 'z', 'z', 'z', 'z', 'z', 'z' }  //57
};


StemReplaceRule
::StemReplaceRule(const LgsString& s_, const LLanguage& l_)
   : _count(1),
     _code(0),
     _language(l_),
     _startingPosition(LgsString::npos),
     _replaceLength(0),
     _rowPosition(0)
{
   if (!s_.empty())
   {
      _initialize (s_);
   }
}

StemReplaceRule
::~StemReplaceRule()
{
}


void 
StemReplaceRule
::_initialize(const LgsString& s_)
{
   int cursor = 0;

   if (isdigit(s_[cursor]) && !(_count = s_[cursor++] - '0'))
   {
      ++_count;
   }
    
   int delimPos = s_.find ('-', cursor);
   int codePos  = s_.find_first_of(LgsString("0123456789"), delimPos);

   _lhs.append (s_, cursor, delimPos - cursor);

   if (codePos == LgsString::npos)
   {
      _rhs.append (s_, delimPos + 1, s_.size());
   }
   else
   {
      _code = s_[codePos] - '0';
      _rhs.append (s_, delimPos + 1, codePos - delimPos - 1);
      _rhs.append (s_, codePos + 1, s_.size());
   }
}


void
StemReplaceRule::fire(LgsString* s_)
{
   while (_lhs.size())
   {
      _makeNewAndOld();
      _match(s_);
      _replace(s_);
   }
}


void
StemReplaceRule
::_match(LgsString* s_)
{
   switch (_old[0])
   {
   case '*':
      _matchAnyConsonant(s_);
      break;
   case '#':
      _matchAnyVowel(s_);
      break;
   case '^':
      _matchAnyDiacritical(s_);
      break;
   case '%':
      _matchAnyCharacter(s_);
      break;
   case '~':
      _matchAnyAccent(s_);
      break;
   case '&':
      switch (_old[1])
      {
      case '+':
         _matchInSeparablePrefix(s_);
         break;
      case '-':
         _matchSeparablePrefix(s_);
         break;
      default:
         _matchLiteral(s_);
         break;
      }
      break;
   default:
      _matchLiteral(s_);
      break;
   }
}


void
StemReplaceRule
::_makeNewAndOld()
{
   LgsString::iterator lhsIt, rhsIt;

   _old.resize(0);
   _new.resize(0);

   switch (_lhs[0])
   {
   case '*':
   case '#':
   case '^':
   case '%':
   case '~':
   case '&':
      for (lhsIt = _lhs.begin(); 
         _isSymbol(*lhsIt) && lhsIt != _lhs.end(); ++lhsIt)
      {
         _old.append(lhsIt, lhsIt + 1);
      }
      if (lhsIt == _lhs.end())
      {
         _lhs.resize(0);
      }
      else
      {
         LgsString str;
         str.append(lhsIt, _lhs.end());
         _lhs = str;
      }
      break;

   default:
      _old = _lhs;
      _lhs.resize(0);
      break;
   }

   if (_rhs.size())
   {
      switch (_rhs[0])
      {
      case '*':
      case '#':
      case '^':
      case '%':
      case '~':
      case '&':
         for (rhsIt = _rhs.begin(); 
            _isSymbol(*rhsIt) && rhsIt != _rhs.end(); ++rhsIt)
         {
            _new.append(rhsIt, rhsIt + 1);
         }
         if (rhsIt == _rhs.end())
         {
            _rhs.resize(0);
         }
         else
         {
            LgsString str;
            str.append(rhsIt, _rhs.end());
            _rhs = str;
         }
         break;
      
      default:
         _new = _rhs;
         _rhs.resize(0);
         break;
      }
   }
}


bool
StemReplaceRule
::_isSymbol(const char c_)
{
   return c_ == '*' || 
          c_ == '#' || 
          c_ == '^' || 
          c_ == '&' || 
          c_ == '%' ||
          c_ == '~';
}


void
StemReplaceRule
::_matchAnyConsonant(LgsString* s_)
{
   int n = 0;
   int i = s_->size();
   
   while (--i >= 0)
   {
      if (_language.isConsonant((*s_)[i]) && ++n == _count)
      {
         _startingPosition = i;
         _replaceLength = 1;
         return;
      }
   }
   
   _startingPosition = LgsString::npos;
}


void
StemReplaceRule
::_matchAnyVowel(LgsString* s_)
{
   int n = 0;
   int i = s_->size();
   
   while (--i >= 0)
   {
      if (_language.isVowel((*s_)[i]) && ++n == _count)
      {
         _startingPosition = i;
         _replaceLength = 1;
         _adjustDiphthongPos(*s_, i);
         return;
      }
   }
   
   _startingPosition = LgsString::npos;
}


void
StemReplaceRule
::_matchAnyDiacritical(LgsString* s_)
{
   int n = 0;
   int i = s_->size();
   
   while (--i >= 0)
   {
      if (_language.isDiacritic((*s_)[i]) 
         && ++n == _count)
      {
         _startingPosition = i;
         _replaceLength = 1;
         _adjustDiphthongPos(*s_, i);
         return;
      }
   }
   
   _startingPosition = LgsString::npos;
}


void
StemReplaceRule
::_matchAnyAccent(LgsString* s_)
{
   int i = s_->size();
   
   while (--i >= 0)
   {
      for (_rowPosition = 0; _rowPosition < (_vowelCount*2) && ((*s_)[i] < _accentedUnaccentedMap[_rowPosition][0] ||
           (*s_)[i] > _accentedUnaccentedMap[_rowPosition][1]); _rowPosition++);
      if (_rowPosition < (_vowelCount*2))
      {
         _startingPosition = i;
         _replaceLength = 1;
         return;
      }
   }
   
   _startingPosition = LgsString::npos;
}


void 
StemReplaceRule
::_adjustDiphthongPos(const LgsString& s_, int i_)
{
   if (i_ <= 0) return;

   // If language is German and if a letter combination is
   // AU or au, it's the 'a' that gets the umlaut not the 'u'.
   if (_language.id() == LLanguage::GermanID 
      && (s_[i_] == 'u' || s_[i_] == 'U') 
      && (s_[i_ - 1] == 'a' || s_[i_ - 1] == 'A'))
   {
       --_startingPosition;
   }
}


void
StemReplaceRule
::_matchAnyCharacter(LgsString* s_)
{
   int n = 0;
   int i = s_->size();
   
   while (--i >= 0)
   {
      if (++n == _count)
      {
         _startingPosition = i;
         _replaceLength = 1;
         return;
      }
   }
   
   _startingPosition = LgsString::npos;
}


void
StemReplaceRule
::_matchSeparablePrefix(LgsString* s_)
{
}


void
StemReplaceRule
::_matchInSeparablePrefix(LgsString* s_)
{
}


void
StemReplaceRule
::_matchLiteral(LgsString* s_)
{
   int n = 0;
   int i = s_->size();
   
   while (--i >= 0)
   {
      if (s_->substr(i, _old.length()) == _old && ++n == _count)
      {
         _startingPosition = i;
         _replaceLength = _old.length();
         return;
      }
   }
   
   _startingPosition = LgsString::npos;
}


void
StemReplaceRule
::_replace(LgsString* s_)
{
   switch (_new[0])
   {
   case '*':
   case '#':
   case '^':
   case '%':
      _replaceAnyWildCard(s_);
      break;
   case '~':
      _replaceAccentedChar(s_);
      break;
   case '&':
      switch (_new[1])
      {
      case '-':
         _replaceSeparablePrefix(s_);
         break;
      case '+':
         _replaceInSeparablePrefix(s_);
         break;
      default:
         _replaceLiteral(s_);
         break;
      }
      break;
   default:
      _replaceLiteral(s_);
      break;
   }
}

void
StemReplaceRule
::_replaceAnyWildCard(LgsString* s_)
{
   if (_startingPosition == LgsString::npos || _code < 0 || _code > 9) 
   {
      return;
   }

   int i = static_cast<unsigned char>((*s_)[_startingPosition]) - 'A';
   
   if (i > _numAccentRows)
   {
      i = _xRefTable[i - _xRefOrigin];
   }

   s_->replace(_startingPosition, 1, _new.size(), _accentTable[i][_code]);
}

void
StemReplaceRule
::_replaceAccentedChar(LgsString* s_)
{
   if (_startingPosition == LgsString::npos) 
   {
      return;
   }

   s_->replace(_startingPosition, 1, _new.size(), _accentedUnaccentedMap[_rowPosition][2]);
}


void
StemReplaceRule
::_replaceSeparablePrefix(LgsString* s_)
{
}


void
StemReplaceRule
::_replaceInSeparablePrefix(LgsString* s_)
{
}


void
StemReplaceRule
::_replaceLiteral(LgsString* s_)
{
   if (_startingPosition != LgsString::npos)
   {
      s_->replace(_startingPosition, _replaceLength, _new);
   }
}
