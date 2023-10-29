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
// File - LWord.cpp
//
// Class - LWord (implementation)
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/lword.h>
#include <logos_libs/linguistic/llanguage.h>
#include <transl/lgstraninterface.h>
#include <transl/interface.h>

const short LWord::NO_REMOVE = 0x00;
const short LWord::REMOVE_HEAD = 0x01;
const short LWord::REMOVE_NON_HEAD = 0x02;

static const unsigned char wordEnds[]={
      // Admissible word ends (<= 0xff)
      0x28,    // '('
      0x29,    // ')'
      0x2b,    // '+'
      0x2c,    // ','
      0x2d,    // '-'
      0x2E,    // '.' RAM
      0x2f,    // '/'
      0x3a,    // ':'
      0x3b,    // ';'
      0x3c,    // '<'
      0x3d,    // '='
      0x3e,    // '>'
      0x3F,    // '?' RAM
      0x5b,    // '['
      0x5c,    // '\\'
      0x5d,    // ']'
      0x7b,    // '{'
      0x7c,    // '|'
      0x7d,    // '}'
      0
};
static const bool protectionMap[]={
//       0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
/* 0 */  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
/* 1 */  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
//          !  "  #  $  %  &  '  (  )  *  +  ,  -  .  /
/* 2 */  0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0,
//       0  1  2  3  4  5  6  7  8  9  :  ;  <  =  >  ?
/* 3 */  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
//       @  A  B  C  D  E  F  G  H  I  J  K  L  M  N  O
/* 4 */  1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
//       P  Q  R  S  T  U  V  W  X  Y  Z  [  \  ]  ^  _
/* 5 */  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1,
//       `  a  b  c  d  e  f  g  h  i  j  k  l  m  n  o
/* 6 */  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
//       p  q  r  s  t  u  v  w  x  y  z  {  |  }  ~  
/* 7 */  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
//
/* 8 */  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
//
/* 9 */  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
//          !  "  #  $  %  &  '  (  )  *  +  ,  -  .  /
/* a */  1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1,
//       0  1  2  3  4  5  6  7  8  9  :  ;  <  =  >  ?
/* b */  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0,
//       @  A  B  C  D  E  F  G  H  I  J  K  L  M  N  O
/* c */  0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
//       P  Q  R  S  T  U  V  W  X  Y  Z  [  \  ]  ^  _
/* d */  1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0,
//          a  b  c  d  e  f  g  h  i  j  k  l  m  n  o
/* e */  0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
//       p  q  r  s  t  u  v  w  x  y  z  {  |  }  ~  
/* f */  1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1,
};

LWord::LWord()
      :isHyphenated_(false),
       isProtectedWord_(false),
       precedingSpaces_(1),
       trailingSpaces_(0),
       tokenType_(LookupTokenType::tok_none),
       isExcludedFromTranslation_(false),
       keepSourceExpression_(false),
       isProperName_(false),
       origSentPosition_(0),
       modificationType_(NO_REMOVE)

{
}

LWord::LWord(ILgsWordMarkup& wordInfo, LgsString& newWord, const LLanguage* aLanguage, bool parseWord)
      :LTextualComponent(newWord, aLanguage),
       v_markup(wordInfo),
       isHyphenated_(false),
       isProtectedWord_(wordInfo._isProtected),
       precedingSpaces_(wordInfo._precedingSpaces),
       trailingSpaces_(wordInfo._trailingSpaces),
       tokenType_(LookupTokenType::tok_none),
       isExcludedFromTranslation_(false),
       keepSourceExpression_(false),
       isProperName_(false),
       origSentPosition_(wordInfo._id),
       modificationType_(NO_REMOVE)
{
   if (parseWord)
   {
      int charCount = 0;

      if (isProtectedWord_ && (newWord == "^p"))
      {
         charCount = 2;
      }
      else
      {
         while (charCount < newWord.length())
         {
            if (isWordEnd(newWord[charCount]) || isProtectionChar(newWord[charCount]))
            {
               if (charCount == 0)
               {
                  charCount++;
                  break;
               }
               break;
            }
            charCount++;
         }
      }

      if (charCount != newWord.length())
      {
         trailingSpaces_ = 0;
         wordInfo._precedingSpaces = 0;
         LgsString::operator=(newWord.substr(0, charCount));
      }
      newWord.erase(0, charCount);
   }
}

LWord::LWord(const LgsString aString, const LLanguage* aLanguage, int precedingSpaces)
      :LTextualComponent(aString, aLanguage),
       isHyphenated_(false),
       isProtectedWord_(false),
       precedingSpaces_(precedingSpaces),
       trailingSpaces_(0),
       tokenType_(LookupTokenType::tok_none),
       isExcludedFromTranslation_(false),
       keepSourceExpression_(false),
       isProperName_(false),
       origSentPosition_(0),
       modificationType_(NO_REMOVE)
{
}

LWord::LWord(const LWord& aWord)
      :LTextualComponent(aWord),
       v_markup(aWord.v_markup),
       isHyphenated_(aWord.isHyphenated_),
       isProtectedWord_ (aWord.isProtectedWord_),
       precedingSpaces_(aWord.precedingSpaces_),
       trailingSpaces_(aWord.trailingSpaces_),
       tokenType_(aWord.tokenType_),
       isExcludedFromTranslation_(aWord.isExcludedFromTranslation_),
       keepSourceExpression_(aWord.keepSourceExpression_),
       isProperName_(aWord.isProperName_),
       origSentPosition_(aWord.origSentPosition_),
       modificationType_(aWord.modificationType_)
{
        // This is the copy constructor.
}

LWord::~LWord()
{
}

void LWord::fillWordInfo(ILgsWordMarkup& wordInfo) const
{
   v_markup.fillWordInfo(wordInfo);
   wordInfo._precedingSpaces = precedingSpaces_;
   wordInfo._trailingSpaces = trailingSpaces_;
   wordInfo._sizeOfWord = length();

   if (*this == "^p")
   {
      wordInfo._isProtected = true;
   }
   else
   {
      wordInfo._isProtected = false;
   }
}

const LWord& LWord::operator=(const LWord& rhs)
{
   if (&rhs != this)
   {
      LTextualComponent::operator=(rhs);
      v_markup = rhs.markup();
      isHyphenated_ = rhs.isHyphenated_;
      isProtectedWord_ = rhs.isProtectedWord_;
      precedingSpaces_ = rhs.precedingSpaces_;
      trailingSpaces_ = rhs.trailingSpaces_;
      tokenType_ = rhs.tokenType_;
      isExcludedFromTranslation_ = rhs.isExcludedFromTranslation_;
      keepSourceExpression_ = rhs.keepSourceExpression_;
      isProperName_ = rhs.isProperName_;
      origSentPosition_ = rhs.origSentPosition_;
      modificationType_ = rhs.modificationType_;
   }
   return *this;
}

void LWord::persistOut(IWordMarkup * wrdMarkup)
{
	char * textPtr = reinterpret_cast<char *>(wrdMarkup+1);
	LTextualComponent::persistOut(textPtr);
	streamOutWordMarkup(wrdMarkup, v_markup, precedingSpaces_, trailingSpaces_,
                       isExcludedFromTranslation_, keepSourceExpression_,
                       isProperName_, origSentPosition_, modificationType_,
                       tokenType_);
	wrdMarkup->_sizeOfWord = length();
}

void LWord::persistIn(IWordMarkup * wrdMarkup)
{
	char * textPtr = reinterpret_cast<char *>(wrdMarkup+1);
	char temp[256];
	memcpy(temp, textPtr, wrdMarkup->_sizeOfWord);
	temp[wrdMarkup->_sizeOfWord] = '\0';
	LTextualComponent::persistIn(temp);
	streamInWordMarkup(wrdMarkup, v_markup, precedingSpaces_,  trailingSpaces_,
                      isExcludedFromTranslation_, keepSourceExpression_,
                      isProperName_, origSentPosition_, modificationType_,
                      tokenType_);
}

// returns true if unsigned char represents a word end
bool LWord::isWordEnd(unsigned char ch)
{
   return isOneOf(ch,wordEnds);
}

// returns true if unsigned char represents a protection char
bool LWord::isProtectionChar(unsigned char ch)
{
   return protectionMap[ch];
}

// tests membership of given integer in given integer array
bool LWord::isOneOf(unsigned int value, const unsigned char list[])
{
   if (value == 0 || list == 0)
   {
      return false;
   }
   for (int i = 0; list[i] != 0; i++)
   {
      if (value == list[i])
      {
         return true;
      }
   }
   return false;
}
