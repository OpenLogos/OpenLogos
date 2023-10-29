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
// File - LWordMarkup.cpp
//
// Class - LWordMarkup (implementation)
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/lwordmarkup.h>
#include <transl/interface.h>
#include <transl/lgstraninterface.h>

//-------------------------------------------------------------------
LWordMarkup::LWordMarkup()
            :v_id(-1),
             v_isProtected(false),
             v_isMixed(false),
             v_isAlphaNumeric(false),
             v_isNeverEos(false),
             v_isInitiallyCap(false),
             v_isBold(false),
             v_isItalic(false),
             v_isUnderlined(false),
             v_isSingleQuoted(false),
             v_isDoubleQuoted(false),
             v_isFrenchSingleQuoted(false),
             v_isFrenchDoubleQuoted(false)
{
}
//-------------------------------------------------------------------
LWordMarkup::LWordMarkup(const ILgsWordMarkup& aWord)
            :v_id(aWord._id),
             v_isProtected(aWord._isProtected),
             v_isMixed(false),
             v_isAlphaNumeric(false),
             v_isNeverEos(false),
             v_isInitiallyCap(false),
             v_isBold(aWord._isBold),
             v_isItalic(aWord._isItalic),
             v_isUnderlined(aWord._isUnderlined),
             v_isSingleQuoted(aWord._isSingleQuoted),
             v_isDoubleQuoted(aWord._isDoubleQuoted),
             v_isFrenchSingleQuoted(false),
             v_isFrenchDoubleQuoted(false)
{
}
//-------------------------------------------------------------------
void LWordMarkup::fillWordInfo(ILgsWordMarkup& wordInfo) const
{
   wordInfo._id = v_id;
   wordInfo._isProtected = v_isProtected;
   wordInfo._isBold = v_isBold;
   wordInfo._isItalic = v_isItalic;
   wordInfo._isUnderlined = v_isUnderlined;
   wordInfo._isSingleQuoted = v_isSingleQuoted;
   wordInfo._isDoubleQuoted = v_isDoubleQuoted;
}
//-------------------------------------------------------------------
LWordMarkup::LWordMarkup(const LWordMarkup& aWord)
            :v_id(aWord.v_id),
             v_isProtected(aWord.v_isProtected),
             v_isMixed(aWord.v_isMixed),
             v_isAlphaNumeric(aWord.v_isAlphaNumeric),
             v_isNeverEos(aWord.v_isNeverEos),
             v_isInitiallyCap(aWord.v_isInitiallyCap),
             v_isBold(aWord.v_isBold),
             v_isItalic(aWord.v_isItalic),
             v_isUnderlined(aWord.v_isUnderlined),
             v_isSingleQuoted(aWord.v_isSingleQuoted),
             v_isDoubleQuoted(aWord.v_isDoubleQuoted),
             v_isFrenchSingleQuoted(aWord.v_isFrenchSingleQuoted),
             v_isFrenchDoubleQuoted(aWord.v_isFrenchDoubleQuoted)
{
   // This is the copy constructor.
}
//-------------------------------------------------------------------
LWordMarkup::~LWordMarkup()
{
}
//-------------------------------------------------------------------
void LWordMarkup::orMask(const LWordMarkup& rhs)
{
   v_isProtected = orMask(v_isProtected, rhs.v_isProtected);
   v_isMixed = orMask(v_isMixed, rhs.v_isMixed);
   v_isAlphaNumeric = orMask(v_isAlphaNumeric, rhs.v_isAlphaNumeric);
   v_isNeverEos = orMask(v_isNeverEos, rhs.v_isNeverEos);
   v_isInitiallyCap = orMask(v_isInitiallyCap, rhs.v_isInitiallyCap);
   v_isBold = orMask(v_isBold, rhs.v_isBold);
   v_isItalic = orMask(v_isItalic, rhs.v_isItalic);
   v_isUnderlined = orMask(v_isUnderlined, rhs.v_isUnderlined);
   v_isSingleQuoted = orMask(v_isSingleQuoted, rhs.v_isSingleQuoted);
   v_isDoubleQuoted = orMask(v_isDoubleQuoted, rhs.v_isDoubleQuoted);
   v_isFrenchSingleQuoted = orMask(v_isFrenchSingleQuoted, rhs.v_isFrenchSingleQuoted);
   v_isFrenchDoubleQuoted = orMask(v_isFrenchDoubleQuoted, rhs.v_isFrenchDoubleQuoted);
}
//-------------------------------------------------------------------
const LWordMarkup& LWordMarkup::operator=(const LWordMarkup& rhs)
{
   if (&rhs != this)
   {
      v_id = rhs.v_id;
      v_isProtected = rhs.v_isProtected;
      v_isMixed = rhs.v_isMixed;
      v_isAlphaNumeric = rhs.v_isAlphaNumeric;
      v_isNeverEos = rhs.v_isNeverEos;
      v_isInitiallyCap = rhs.v_isInitiallyCap;
      v_isBold = rhs.v_isBold;
      v_isItalic = rhs.v_isItalic;
      v_isUnderlined = rhs.v_isUnderlined;
      v_isSingleQuoted = rhs.v_isSingleQuoted;
      v_isDoubleQuoted = rhs.v_isDoubleQuoted;
      v_isFrenchSingleQuoted = rhs.v_isFrenchSingleQuoted;
      v_isFrenchDoubleQuoted = rhs.v_isFrenchDoubleQuoted;
   }
   return *this;
}
//---------------------------------------------------------------------
bool LWordMarkup::hasMarkup(void)
{
   if (v_isBold || v_isItalic || v_isUnderlined || v_isSingleQuoted ||
       v_isDoubleQuoted || v_isFrenchSingleQuoted || v_isFrenchDoubleQuoted)
   {
      return true;
   }
   else
   {
      return false;
   }
}
//---------------------------------------------------------------------
void streamOutWordMarkup(IWordMarkup * dest, const LWordMarkup& object, int precedingSpaces,
                         int trailingSpaces, bool excludedFromTranslation, bool keepSourceExpression,
                         bool isProperName, int origPosition, short modificationType,
                         LookupTokenType::Type tokenType)
{
   dest->_id = object.v_id;
   dest->_isProtected = object.v_isProtected;
   dest->_isMixed = object.v_isMixed;
   dest->_isAlphaNumeric = object.v_isAlphaNumeric;
   dest->_isNeverEos = object.v_isNeverEos;
   dest->_isInitiallyCap = object.v_isInitiallyCap;
   dest->_isBold = object.v_isBold;
   dest->_isItalic = object.v_isItalic;
   dest->_isUnderlined = object.v_isUnderlined;
   dest->_isSingleQuoted = object.v_isSingleQuoted;
   dest->_isDoubleQuoted = object.v_isDoubleQuoted;
   dest->_isFrenchSingleQuoted = object.v_isFrenchSingleQuoted;
   dest->_isFrenchDoubleQuoted = object.v_isFrenchDoubleQuoted;
   dest->_isExcludedFromTranslation = excludedFromTranslation;
   dest->_precedingSpaces = precedingSpaces;
   dest->_trailingSpaces = trailingSpaces;
   dest->_keepSourceExpression = keepSourceExpression;
   dest->_isProperName = isProperName;
   dest->_origSentPosition = origPosition;
   dest->_modificationType = modificationType;
   dest->_tokenType = tokenType;

}
//-------------------------------------------------------------------
void streamInWordMarkup(IWordMarkup * source, LWordMarkup& object, int& precedingSpaces,
                        int& trailingSpaces, bool& excludedFromTranslation, bool& keepSourceExpression,
                        bool& isProperName, int& origPosition, short & modificationType,
                        LookupTokenType::Type& tokenType)
{
   object.v_id = source->_id;
   object.v_isProtected  = source->_isProtected;
   object.v_isMixed = source->_isMixed;
   object.v_isAlphaNumeric = source->_isAlphaNumeric;
   object.v_isNeverEos = source->_isNeverEos;
   object.v_isInitiallyCap = source->_isInitiallyCap;
   object.v_isBold = source->_isBold;
   object.v_isItalic  = source->_isItalic;
   object.v_isUnderlined = source->_isUnderlined;
   object.v_isSingleQuoted = source->_isSingleQuoted;
   object.v_isDoubleQuoted = source->_isDoubleQuoted;
   object.v_isFrenchSingleQuoted = source->_isFrenchSingleQuoted;
   object.v_isFrenchDoubleQuoted = source->_isFrenchDoubleQuoted;
   excludedFromTranslation = source->_isExcludedFromTranslation;
   precedingSpaces = source->_precedingSpaces;
   trailingSpaces = source->_trailingSpaces;
   keepSourceExpression = source->_keepSourceExpression;
   isProperName = source->_isProperName;
   origPosition = source->_origSentPosition;
   modificationType = source->_modificationType;
   tokenType = source->_tokenType;
}
