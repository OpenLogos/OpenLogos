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
program. If not, write to Globalware AG, Hospitalstraße 6, D-99817 Eisenach.

Linux port modifications and additions by Bernd Kiefer, Walter Kasper,
Deutsches Forschungszentrum fuer kuenstliche Intelligenz (DFKI)
Stuhlsatzenhausweg 3, D-66123 Saarbruecken
*/
#ifndef _INTERFACE_H_
#define _INTERFACE_H_

#include <logos_libs/SubjectMatterCodes/SubjectMatterCode.h>
#include <logos_libs/linguistic/lookuptokentype.h>

struct ISWorkInfoHeader
{
   int _blackHoleLocation;
   int _capitalizationState;
   int _precedingSpaces;
   int _headWordLocation;
   int _hashLocation;
   int _henum1;
   int _henum2;
   int _rootHenum1;
   int _rootHenum2;
   bool _isEof;
   int _isFound;
   int _isProtected;
   int _primarySsu;
   int _sourcePosition;
   int _position;
   int _ssuCount;
   int _wordCount;
   int _wordSize;
   bool _isBold;
   bool _isItalic;
   bool _isUnderlined;
   bool _isSingleQuoted;
   bool _isDoubleQuoted;
   short _compoundInfo;
};

struct ISWorkInfo
{
   char _companyCode[3];
   int _formCode;
   int _overflow2b;
   int _overflow3b;
   int _gender;
   int _hashCode1;
   int _hashCode2;
   int _rootHashCode1;
   int _rootHashCode2;
   int _isTransferred;
   int _matchedOnFullyCapped;
   int _meaningID;
   int _patNumber;
   int _protectionCode;
   int _setID;
   int _sourceStemNumber;
   int _subsetID;
   int _supersetID;
   int _usageID;
   int _wordClass;
   int _wordID;
   int _wordTypeCode;
   int _canonicalWordSize;
   int _targetWordSize;
   char _subjectMatterCode[15];
   int _auxiliaryCode;
   //int _atomicCode;							// TO BE REMOVED WHEN NEW SMC IS TESTED
   //int _genericCode;						// TO BE REMOVED WHEN NEW SMC IS TESTED
};

struct IWordMarkup
{
   // Following fields are WordMarkup information
   int _id;
   bool _isProtected;
   bool _isMixed;
   bool _isAlphaNumeric;
   bool _isNeverEos;
   bool _isInitiallyCap;
   bool _isBold;
   bool _isItalic;
   bool _isUnderlined;
   bool _isSingleQuoted;
   bool _isDoubleQuoted;
   bool _isFrenchSingleQuoted;
   bool _isFrenchDoubleQuoted;
   bool _isExcludedFromTranslation;
   int _precedingSpaces;
   int _trailingSpaces;
   bool _keepSourceExpression;
   bool _isProperName;
   int _origSentPosition;
   int _sizeOfWord;
   short _modificationType;
   LookupTokenType::Type _tokenType;
};

struct ISentenceInfo
{
   bool _isObject;
   int _position;
   int _ssuListSize;
   int _partOf;
   int _numberOfParts;
   int _translationState;
   int _caseState;
   long _sentMarkup;
   bool _bold;
   bool _italic;
   bool _underlined;
   bool _singleQuoted;
   bool _doubleQuoted;
   bool _bDifferentSentence;
};

#endif
