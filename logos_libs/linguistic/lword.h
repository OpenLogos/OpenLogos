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
#ifndef __lword_h__
#define __lword_h__

//-------------------------------------------------------------------
// File - lword.h
//
// Class - LWord
//
//-------------------------------------------------------------------

#include <logos_libs/linguistic/ltextualcomponent.h>
#include <logos_libs/linguistic/lwordmarkup.h>
#include <logos_libs/linguistic/lookuptokentype.h>

class ILgsWordMarkup;
class IWordMarkup;

//-------------------------------------------------------------------
class LWord: public LTextualComponent
{
public:
   static const short NO_REMOVE;
   static const short REMOVE_HEAD;
   static const short REMOVE_NON_HEAD;
   DummyLess(LWord);
   static LgsString tokenTypeDesc(LookupTokenType::Type tokenType);
   static bool isWordEnd(unsigned char ch);
   static bool isProtectionChar(unsigned char ch);
   static bool isOneOf(unsigned int value, const unsigned char list[]);

   //---------------------------------------------------------------
   LWord();
   LWord(ILgsWordMarkup& wordInfo, LgsString& newWord, const LLanguage* aLanguage, bool parseWord = false);
   LWord(const LgsString aText, const LLanguage* aLanguage, int precedingSpaces = 1);
   LWord(const LWord&);
   virtual ~LWord();

   //---------------------------------------------------------------
   const LWordMarkup& markup() const;
   void markup(const LWordMarkup&);
   void markupId(int);
   int markupId() const;
   void orMarkupMask(const LWord&);

   //---------------------------------------------------------------
   void fillWordInfo(ILgsWordMarkup& wordInfo) const;
   const LWord& operator=(const LWord&);
   int precedingSpaces() const;
   void setPrecedingSpaces(int);
   int trailingSpaces() const;
   void setTrailingSpaces(int);

   LookupTokenType::Type getTokenType() const;
   void setTokenType(LookupTokenType::Type tokenType);

   virtual void persistOut(IWordMarkup * iWrdMarkup);
   virtual void persistIn(IWordMarkup * iWrdMarkup);

   bool isProtected() const;
   bool isMixed() const;
   bool isAlphaNumeric() const;
   bool isNeverEos() const;
   bool isInitiallyCap() const;
   bool isBold() const;
   bool isItalic() const;
   bool isUnderlined() const;
   bool isSingleQuoted() const;
   bool isDoubleQuoted() const;
   bool isFrenchSingleQuoted() const;
   bool isFrenchDoubleQuoted() const;
   bool isExcludedFromTranslation() const;
   void setExcludedFromTranslation(bool value = true);
   bool keepSourceExpression() const;
   void setKeepSourceExpression(bool value = true);
   bool isProperName() const;
   void setIsProperName(bool value = true);
   void setOriginalSentencePosition(int position);
   void setModificationType(short modificationType);
   short getModificationType() const;
   int originalSentencePosition();

   bool isHyphenated() const;
   void setHyphenated(bool value = true);

   bool isProtectedWord() const;
   void setProtectedWord(bool value = true);
   void setText(const LgsString& text);

private:
   LookupTokenType::Type tokenType_;
   LWordMarkup v_markup;

   bool isHyphenated_;
   bool isProtectedWord_;
   int precedingSpaces_;
   int trailingSpaces_;
   bool isExcludedFromTranslation_;
   bool keepSourceExpression_;
   bool isProperName_;
   int origSentPosition_;
   short modificationType_;
};

typedef LgsVector(LWord) LWordVector;
typedef LgsVector(LWord)::iterator LWordIterator;
typedef LgsVector(LWord*) LWordPVector;
typedef LgsVector(LWord*)::iterator LWordPIterator;

//-------------------------------------------------------------------
inline const LWordMarkup& LWord::markup() const
{
   return v_markup;
}
//-------------------------------------------------------------------
inline void LWord::markup(const LWordMarkup& rhs)
{
   v_markup = rhs;
}
//-------------------------------------------------------------------
inline void LWord::orMarkupMask(const LWord& rhs)
{
   v_markup.orMask(rhs.markup());
}
//-------------------------------------------------------------------
inline void LWord::markupId(int l)
{
   v_markup.setId(l);
}
//-------------------------------------------------------------------
inline int LWord::markupId() const
{
   return v_markup.id();
}
//-------------------------------------------------------------------
inline bool LWord::isProtected() const
{
   return v_markup.isProtected();
}
//-------------------------------------------------------------------
inline bool LWord::isMixed() const
{
   return v_markup.isMixed();
}
//-------------------------------------------------------------------
inline bool LWord::isAlphaNumeric() const
{
   return v_markup.isAlphaNumeric();
}
//-------------------------------------------------------------------
inline bool LWord::isNeverEos() const
{
   return v_markup.isNeverEos();
}
//-------------------------------------------------------------------
inline bool LWord::isInitiallyCap() const
{
   return v_markup.isInitiallyCap();
}
//-------------------------------------------------------------------
inline bool LWord::isBold() const
{
   return v_markup.isBold();
}
//-------------------------------------------------------------------
inline bool LWord::isItalic() const
{
   return v_markup.isItalic();
}
//-------------------------------------------------------------------
inline bool LWord::isUnderlined() const
{
   return v_markup.isUnderlined();
}
//-------------------------------------------------------------------
inline bool LWord::isSingleQuoted() const
{
   return v_markup.isSingleQuoted();
}
//-------------------------------------------------------------------
inline bool LWord::isDoubleQuoted() const
{
   return v_markup.isDoubleQuoted();
}
//-------------------------------------------------------------------
inline bool LWord::isFrenchSingleQuoted() const
{
   return v_markup.isFrenchSingleQuoted();
}
//-------------------------------------------------------------------
inline bool LWord::isFrenchDoubleQuoted() const
{
   return v_markup.isFrenchDoubleQuoted();
}
//-------------------------------------------------------------------
inline bool LWord::isExcludedFromTranslation() const
{
   return isExcludedFromTranslation_;
}
//-------------------------------------------------------------------
inline void LWord::setExcludedFromTranslation(bool value)
{
   isExcludedFromTranslation_ = value;
}
//-------------------------------------------------------------------
inline bool LWord::keepSourceExpression() const
{
   return keepSourceExpression_;
}
//-------------------------------------------------------------------
inline void LWord::setKeepSourceExpression(bool value)
{
   keepSourceExpression_ = value;
}
//-------------------------------------------------------------------
inline bool LWord::isProperName() const
{
   return isProperName_;
}
//-------------------------------------------------------------------
inline void LWord::setIsProperName(bool value)
{
   isProperName_ = value;
}
//-------------------------------------------------------------------
inline bool LWord::isHyphenated() const
{
   return isHyphenated_;
}
//-------------------------------------------------------------------
inline void LWord::setHyphenated(bool value)
{
   isHyphenated_ = value;
}
//-------------------------------------------------------------------
inline bool LWord::isProtectedWord() const
{
   return isProtectedWord_;
}
//-------------------------------------------------------------------
inline void LWord::setProtectedWord(bool value)
{
   isProtectedWord_ = value;
}
//-------------------------------------------------------------------
inline void LWord::setText(const LgsString& text)
{
  LgsString::operator=(text);
}
//-------------------------------------------------------------------
inline int LWord::precedingSpaces() const
{
   return precedingSpaces_;
}
//-------------------------------------------------------------------
inline void LWord::setPrecedingSpaces(int n)
{
   precedingSpaces_ = n;
}
//-------------------------------------------------------------------
inline int LWord::trailingSpaces() const
{
   return trailingSpaces_;
}
//-------------------------------------------------------------------
inline void LWord::setTrailingSpaces(int n)
{
   trailingSpaces_ = n;
}
//-------------------------------------------------------------------
inline void LWord::setOriginalSentencePosition(int position)
{
   origSentPosition_ = position;
}
//-------------------------------------------------------------------
inline int LWord::originalSentencePosition()
{
   return origSentPosition_;
}
//-------------------------------------------------------------------
inline LookupTokenType::Type LWord::getTokenType() const
{
   return tokenType_;
}
//-------------------------------------------------------------------
inline void LWord::setTokenType(LookupTokenType::Type tokenType)
{
   tokenType_ = tokenType;
}
//-------------------------------------------------------------------
inline void LWord::setModificationType(short modificationType)
{
   modificationType_ = modificationType;
}
//-------------------------------------------------------------------
inline short LWord::getModificationType() const
{
   return modificationType_;
}

#endif // __lword_h__



