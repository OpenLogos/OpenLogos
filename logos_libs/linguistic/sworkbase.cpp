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
// --------------------------------------------------------------------------
// File - SWorkBase.cpp
//
// Class - SWorkBase (implementation)
//
// --------------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/sworkbase.h>
#include <logos_libs/utility/stringutil.h>
#include <stdio.h>
#include <transl/interface.h>

int SWorkBase::st_lowSetID       = 17;
int SWorkBase::st_highSetID      = 99;
int SWorkBase::st_lowSubsetID    = 100;
int SWorkBase::st_highSubsetID   = 998;
int SWorkBase::st_lowSupersetID  = 1;
int SWorkBase::st_highSupersetID = 16;

// --------------------------------------------------------------------------
// Default constructor
// --------------------------------------------------------------------------
SWorkBase::SWorkBase()
	// set default general information of this SWORK
          :v_isFound(false),
           v_isProtected(false),
           v_primarySsu(0),
           v_sourcePosition(1),
           v_ssuCount(1),
           v_wordCount(1),
           v_blackHoleLocation(0),
           v_headWordLocation(1),
           v_hashLocation(1),
           v_isHeadWordBuilt(false),
           v_position(1),
           v_capitalizationState(0),
           v_isEof(false),
           v_isBold(false),
           v_isItalic(false),
           v_isUnderlined(false),
           v_isSingleQuoted(false),
           v_isDoubleQuoted(false),
           v_precedingSpaces(1),
		   v_compoundInfo(0)
{
	// set default information for each of the semanto-syntactic units of this SWORK
	SubjectMatterCode aSMC;			// a default SMC
	for (int i = 0; i < 3; i++)
	{
      AuxiliaryCode(i, 0);
		FormCode(i, 0);
		Overflow2b(i, 0);
		Overflow3b(i, 0);
		CompanyCode(i, "   ");
      CanonicalWord(i, "");
      TargetWord(i, "");
		Gender(i, 0);
		HashCode1(i, 0);
		HashCode2(i, 0);
		rootHashCode1(i, 0);
		rootHashCode2(i, 0);
		IsTransferred(i, 0);
      MatchedOnFullyCapped(i, 0);
		MeaningID(i, 0);
		PatNumber(i, 0);
		ProtectionCode(i, 0);
		SetID(i, 0);
		SourceStemNumber(i, 0);
		SubsetID(i, 0);
		SupersetID(i, 0);
		UsageID(i, 0);
		WordClass(i, 0);
		WordID(i, 0);
		WordTypeCode(i, 0);
		subjectMatterCode_[i]=aSMC;
		AtomicCode(i, 0);					// TO BE REMOVED WHEN NEW SMC IS TESTED
		GenericCode(i, 0);					// TO BE REMOVED WHEN NEW SMC IS TESTED
   }
}


// --------------------------------------------------------------------------
// Destructor
// --------------------------------------------------------------------------
SWorkBase::~SWorkBase()
{
}


// --------------------------------------------------------------------------
const LgsString& SWorkBase::HeadWord()
{
   if (!v_isHeadWordBuilt)
   {
      InitializeHeadWord();
      v_isHeadWordBuilt = true;
   }
   return v_headWord;
}


// --------------------------------------------------------------------------
const LgsString SWorkBase::HeadWordInUpperCase()
{
   if (StringUtil::containsUpperCase(HeadWord()))
      return StringUtil::upper(HeadWord());
   else
      return HeadWord();
}


// --------------------------------------------------------------------------
void SWorkBase::HeadWord(const LgsString& rhs)
{
   v_isHeadWordBuilt = true;
   v_headWord = rhs;
}


// --------------------------------------------------------------------------
void SWorkBase::InitializeHeadWord()
{
   if (WordCount() < 2)
   {
      HeadWord(Word());
   }
   else if (HeadWordLocation() > WordCount())
   {
      HeadWord(Word());
   }
   else
   {
      int pos = 0;
      for (int i = 1; i < HeadWordLocation(); i++)
      {
         pos = Word().find(' ', pos);
         if (pos == LgsString::npos)
         {
            break;
         }
         pos++;
      }
      if (pos == LgsString::npos)
      {
         HeadWord(Word());
      }
      else
      {
         int endPos = Word().find(' ', pos);
         if (endPos == LgsString::npos)
         {
            endPos = Word().length();
         }
         HeadWord(Word().substr(pos, endPos));
      }
   }
}


// --------------------------------------------------------------------------
bool SWorkBase::IsValidSetID(int id)
{
   bool isValid = true;

   if ((id < st_lowSetID) || (id > st_highSetID))
   {
      isValid = false;
   }
   return isValid;
}


// --------------------------------------------------------------------------
bool SWorkBase::IsValidSubsetID(int id)
{
   bool isValid = true;

   if ((id < st_lowSubsetID) || (id > st_highSubsetID))
   {
      isValid = false;
   }
   return isValid;
}


// --------------------------------------------------------------------------
bool SWorkBase::IsValidSupersetID(int id)
{
   bool isValid = true;

   if ((id < st_lowSupersetID) || (id > st_highSupersetID))
   {
      isValid = false;
   }
   return isValid;
}


// --------------------------------------------------------------------------
int SWorkBase::TypeID(size_t i) const
{
   return SetID(i);
}


// --------------------------------------------------------------------------
const LgsString SWorkBase::WordInUpperCase() const
{
   if (StringUtil::containsUpperCase(Word()))
      return StringUtil::upper(Word());
   else
      return Word();
}


// --------------------------------------------------------------------------
// Write this object as a LgsString of characters for message passing between modules
// --------------------------------------------------------------------------
char* streamOutSWorkInfo(char* dest, SWorkBase& object)
{
   ISWorkInfoHeader* swHeader = reinterpret_cast<ISWorkInfoHeader*>(dest);
   if (object.v_ssuCount > 3)
   {
      object.v_ssuCount = 3;	// maximum number of semanto-syntactic units allowed per sentence unit
   }
   swHeader->_blackHoleLocation = object.v_blackHoleLocation;
   swHeader->_capitalizationState = object.v_capitalizationState;
   swHeader->_precedingSpaces = object.v_precedingSpaces;
   swHeader->_headWordLocation = object.v_headWordLocation;
   swHeader->_hashLocation = object.v_hashLocation;
   swHeader->_henum1 = object.v_henum1;
   swHeader->_henum2 = object.v_henum2;
   swHeader->_rootHenum1 = object.v_rootHenum1;
   swHeader->_rootHenum2 = object.v_rootHenum2;
   swHeader->_isEof = object.v_isEof;
   swHeader->_isFound = object.v_isFound;
   swHeader->_isProtected = object.v_isProtected;
   swHeader->_primarySsu = object.v_primarySsu;
   swHeader->_sourcePosition = object.v_sourcePosition;
   swHeader->_position = object.v_position;
   swHeader->_ssuCount = object.v_ssuCount;
   swHeader->_wordCount = object.v_wordCount;
   swHeader->_wordSize = object.v_word.length();
   swHeader->_isBold = object.v_isBold;
   swHeader->_isItalic = object.v_isItalic;
   swHeader->_isUnderlined = object.v_isUnderlined;
   swHeader->_isSingleQuoted = object.v_isSingleQuoted;
   swHeader->_isDoubleQuoted = object.v_isDoubleQuoted;
   swHeader->_compoundInfo = object.v_compoundInfo;

   dest += sizeof(ISWorkInfoHeader);
   memcpy(dest, object.v_word.c_str(), object.v_word.length());

   dest += object.v_word.length();
   ISWorkInfo* swInfo = reinterpret_cast<ISWorkInfo*>(dest);

   for (int i = 0; i < object.v_ssuCount; i++)
   {
      memcpy(swInfo->_companyCode, object.v_companyCode[i].c_str(), object.v_companyCode[i].length()) ;
      if (object.v_companyCode[i].length() < 3)
      {
         for (int j = object.v_companyCode[i].length(); j < 3 ; j++)
         {
            swInfo->_companyCode[j] = 0;
         }
      }
      swInfo->_auxiliaryCode = object.v_auxiliaryCode[i];
      swInfo->_formCode = object.v_formCode[i];
      swInfo->_overflow2b = object.v_overflow2b[i];
      swInfo->_overflow3b = object.v_overflow3b[i];
      swInfo->_gender = object.v_gender[i];
      swInfo->_hashCode1 = object.v_hashCode1[i];
      swInfo->_hashCode2 = object.v_hashCode2[i];
	  swInfo->_rootHashCode1 = object.v_rootHashCode1[i];
	  swInfo->_rootHashCode2 = object.v_rootHashCode2[i];
      swInfo->_isTransferred = object.v_isTransferred[i];
      swInfo->_matchedOnFullyCapped = object.v_matchedOnFullyCapped[i];
      swInfo->_meaningID = object.v_meaningID[i];
      swInfo->_patNumber = object.v_patNumber[i];
      swInfo->_protectionCode = object.v_protectionCode[i];
      swInfo->_setID = object.v_setID[i];
      swInfo->_sourceStemNumber = object.v_sourceStemNumber[i];
      swInfo->_subsetID = object.v_subsetID[i];
      swInfo->_supersetID = object.v_supersetID[i];
      swInfo->_usageID = object.v_usageID[i];
      swInfo->_wordClass = object.v_wordClass[i];
      swInfo->_wordID = object.v_wordID[i];
      swInfo->_wordTypeCode = object.v_wordTypeCode[i];
      swInfo->_canonicalWordSize = object.v_canonicalWord[i].length();
      swInfo->_targetWordSize = object.v_targetWord[i].length();
      strcpy(swInfo->_subjectMatterCode,object.subjectMatterCode_[i].formattedContent().c_str());	// ... but it has to be done that way!

      dest += sizeof(ISWorkInfo);
      memcpy(dest, object.v_canonicalWord[i].c_str(), object.v_canonicalWord[i].length());
      dest += object.v_canonicalWord[i].length();
      memcpy(dest, object.v_targetWord[i].c_str(), object.v_targetWord[i].length());
      dest += object.v_targetWord[i].length();
      swInfo = reinterpret_cast<ISWorkInfo*>(dest);
   }

//   return reinterpret_cast<char *>(swInfo);
   return dest;
}


// --------------------------------------------------------------------------
// Set this object from the given sequence of characters
// --------------------------------------------------------------------------
char* streamInSWorkInfo(char* source, SWorkBase& object)
{
   ISWorkInfoHeader* swHeader = reinterpret_cast<ISWorkInfoHeader*>(source);

   object.v_blackHoleLocation = swHeader->_blackHoleLocation;
   object.v_capitalizationState = swHeader->_capitalizationState;
   object.v_precedingSpaces = swHeader->_precedingSpaces;
   object.v_headWordLocation = swHeader->_headWordLocation;
   object.v_hashLocation = swHeader->_hashLocation;
   object.v_henum1 = swHeader->_henum1;
   object.v_henum2 = swHeader->_henum2;
   object.v_rootHenum1 = swHeader->_rootHenum1;
   object.v_rootHenum2 = swHeader->_rootHenum2;
   object.v_isEof = swHeader->_isEof;
   object.v_isFound = swHeader->_isFound;
   object.v_isProtected = swHeader->_isProtected;
   object.v_primarySsu = swHeader->_primarySsu;
   object.v_sourcePosition = swHeader->_sourcePosition;
   object.v_position = swHeader->_position;
   object.v_ssuCount = swHeader->_ssuCount;
   object.v_wordCount = swHeader->_wordCount;
   object.v_isBold = swHeader->_isBold;
   object.v_isItalic = swHeader->_isItalic;
   object.v_isUnderlined = swHeader->_isUnderlined;
   object.v_isSingleQuoted = swHeader->_isSingleQuoted;
   object.v_isDoubleQuoted = swHeader->_isDoubleQuoted;
   object.v_compoundInfo = swHeader->_compoundInfo;

   source += sizeof(ISWorkInfoHeader);
   int wordSize;
   wordSize =  swHeader->_wordSize;
   char buff[256];
   memcpy(buff, source, wordSize);
   buff[wordSize] = '\0';
   object.v_word = buff ;
   
   source += wordSize;
   ISWorkInfo* swInfo = reinterpret_cast<ISWorkInfo*>(source);

   for (int i = 0; i < 3; i++)
   {
      if (i < object.v_ssuCount)
      {
         char temp[4];
         temp[3] = '\0';
         memcpy(temp, swInfo->_companyCode, 3);
         object.v_companyCode[i] = temp;
         object.v_auxiliaryCode[i] = swInfo->_auxiliaryCode;
         object.v_formCode[i] = swInfo->_formCode;
         object.v_overflow2b[i] = swInfo->_overflow2b;
         object.v_overflow3b[i] = swInfo->_overflow3b;
         object.v_gender[i] = swInfo->_gender;
         object.v_hashCode1[i] = swInfo->_hashCode1;
         object.v_hashCode2[i] = swInfo->_hashCode2;
		 object.v_rootHashCode1[i] = swInfo->_rootHashCode1;
		 object.v_rootHashCode2[i] = swInfo->_rootHashCode2;
         object.v_isTransferred[i] = swInfo->_isTransferred;
         object.v_matchedOnFullyCapped[i] = swInfo->_matchedOnFullyCapped;
         object.v_meaningID[i] = swInfo->_meaningID;
         object.v_patNumber[i] = swInfo->_patNumber;
         object.v_protectionCode[i] = swInfo->_protectionCode;
         object.v_setID[i] = swInfo->_setID;
         object.v_sourceStemNumber[i] = swInfo->_sourceStemNumber;
         object.v_subsetID[i] = swInfo->_subsetID;
         object.v_supersetID[i] = swInfo->_supersetID;
         object.v_usageID[i] = swInfo->_usageID;
         object.v_wordClass[i] = swInfo->_wordClass;
         object.v_wordID[i] = swInfo->_wordID;
         object.v_wordTypeCode[i] = swInfo->_wordTypeCode;
         object.subjectMatterCode_[i].set(swInfo->_subjectMatterCode);

         source += sizeof(ISWorkInfo);
         wordSize =  swInfo->_canonicalWordSize;
         memcpy(buff, source, wordSize);
         buff[wordSize] = '\0';
         object.v_canonicalWord[i] = buff ;
         source += wordSize;

         wordSize =  swInfo->_targetWordSize;
         memcpy(buff, source, wordSize);
         buff[wordSize] = '\0';
         object.v_targetWord[i] = buff ;
         source += wordSize;
         swInfo = reinterpret_cast<ISWorkInfo*>(source);
      }
      else
      {
         object.v_companyCode[i] = "   ";
         object.v_canonicalWord[i] = "";
         object.v_targetWord[i] = "";
         object.v_auxiliaryCode[i] = 0;
         object.v_formCode[i] = 0;
         object.v_overflow2b[i] = 0;
         object.v_overflow3b[i] = 0;
         object.v_gender[i] = 0;
         object.v_hashCode1[i] = 0;
         object.v_hashCode2[i] = 0;
         object.v_isTransferred[i] = 0;
         object.v_matchedOnFullyCapped[i] = 0;
         object.v_meaningID[i] = 0;
         object.v_patNumber[i] = 0;
         object.v_protectionCode[i] = 0;
         object.v_setID[i] = 0;
         object.v_sourceStemNumber[i] = 0;
         object.v_subsetID[i] = 0;
         object.v_supersetID[i] = 0;
         object.v_usageID[i] = 0;
         object.v_wordClass[i] = 0;
         object.v_wordID[i] = 0;
         object.v_wordTypeCode[i] = 0;
         object.subjectMatterCode_[i].clear();
	  }
   }
//   return reinterpret_cast<char *>(swInfo);
   return source;
}


// --------------------------------------------------------------------------
void SWorkBase::display(ostream& stream)
{
   LgsString countString;
   char s[250];

   sprintf(s, "%2d %3d", position(), SourcePosition());

   for (int i = 0; i < 3; i++)
   {
      char asterisk = ' ';
      if (!IsTransferred(i) && (MeaningID(i) != 0))
      {
         asterisk = '*';
      }

      if (CompanyCode(i).empty())
      {
         v_companyCode[i] = "   ";
      }
      sprintf(s, 
			  //"%s %2d %3d %2d %3d %3d   0  0    0 %s %2d %3d %3d %3d %7d%c|",		// TO BE REMOVED WHEN NEW SMC IS TESTED
			  "%s %2d %3d %2d %3d %3d   0  0    0 %s %s %3d %3d %7d%c|",	// testing (kept AC+GC as well as new SMC) - TO BE REMOVED WHEN NEW SMC IS TESTED
			  //"%s %2d %3d %2d %3d %3d   0  0    0 %s %s %3d %3d %7d%c|",			// this is the line to keep when testing is done
              s, 
			  WordClass(i), 
			  TypeID(i), 
			  FormCode(i), 
			  SubsetID(i), 
			  SupersetID(i),
           CompanyCode(i).data(), 
			  subjectMatterCode_[i].formattedContent().c_str(),
			  //GenericCode(i),					
			  //AtomicCode (i),					
			  Overflow2b (i),
           Overflow3b (i), 
			  MeaningID (i), 
			  asterisk);
   }
   sprintf(s, "%s %2d", s, WordCount());
   stream << s << " ";
   stream << Word() << endl;
}


// --------------------------------------------------------------------------
// Display the SMCs (testing purposes)
// --------------------------------------------------------------------------
void SWorkBase::displaySMCs()
{
	cout << "SMCs for (" << v_word << "):";
	for (int i = 0; i < 3; i++)
      cout << " " << subjectMatterCode_[i].content();
	cout << endl;
}


