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
#include <logos_include/logoscommon.h>
#include <logos_libs/entity/dwordphrase.h>

#define HEN_NUM_DEFAULT_VALUE -1

//-------------------------------------------------------------------
DWordPhrase::DWordPhrase()
            :v_companyCode("LOG"),
             v_wordID(0),
             v_word(""),
             v_wordCount(1),
             v_languageCode(2),
             v_wordTypeCode(1),
             v_protectionCode(0),
             v_hashCode1(0),
             v_hashCode2(0),
			 v_rootHashCode1(0),
			 v_rootHashCode2(0),
			 v_henNum1(HEN_NUM_DEFAULT_VALUE),
			 v_henNum2(HEN_NUM_DEFAULT_VALUE),
			 v_rootHenNum1(HEN_NUM_DEFAULT_VALUE),
			 v_rootHenNum2(HEN_NUM_DEFAULT_VALUE),
             v_headWord(0),
             v_hashLocation(0),
             v_blackHoleLocation(0),
             v_wildcardPosition(0),
             v_isAspire(false),
             v_isMonth(false),
             v_isPrefix(false),
             v_isNeverEos(false),
             v_isOrdinal(false),
             v_isPatException(false),
			 v_endingLength(0)
{
}
//-------------------------------------------------------------------
DWordPhrase::DWordPhrase(const LgsString& word, int language_code)
            :v_companyCode("LOG"),
             v_wordID(0),
             v_word(word),
             v_wordCount(1),
             v_languageCode(language_code),
             v_wordTypeCode(1),
             v_protectionCode(0),
             v_hashCode1(0),
             v_hashCode2(0),
			 v_rootHashCode1(0),
			 v_rootHashCode2(0),
			 v_henNum1(HEN_NUM_DEFAULT_VALUE),
			 v_henNum2(HEN_NUM_DEFAULT_VALUE),
			 v_rootHenNum1(HEN_NUM_DEFAULT_VALUE),
			 v_rootHenNum2(HEN_NUM_DEFAULT_VALUE),
             v_headWord(0),
             v_hashLocation(0),
             v_blackHoleLocation(0),
             v_wildcardPosition(0),
             v_isAspire(false),
             v_isMonth(false),
             v_isPrefix(false),
             v_isNeverEos(false),
             v_isOrdinal(false),
             v_isPatException(false),
			 v_endingLength(0)
{
}
//-------------------------------------------------------------------
DWordPhrase::DWordPhrase(const DWordPhrase& rhs)
            :DObject(rhs),
             v_companyCode(rhs.CompanyCode()),
             v_wordID(rhs.WordID()),
             v_word(rhs.Word()),
             v_wordCount(rhs.WordCount()),
             v_languageCode(rhs.LanguageCode()),
             v_wordTypeCode(rhs.WordTypeCode()),
             v_protectionCode(rhs.ProtectionCode()),
             v_hashCode1(rhs.HashCode1()),
             v_hashCode2(rhs.HashCode2()),
			 v_rootHashCode1(rhs.rootHashCode1()),
			 v_rootHashCode2(rhs.rootHashCode2()),
			 v_henNum1(rhs.henNum1()),
			 v_henNum2(rhs.henNum2()),
			 v_rootHenNum1(rhs.rootHenNum1()),
			 v_rootHenNum2(rhs.rootHenNum2()),
             v_headWord(rhs.HeadWord()),
             v_hashLocation(rhs.HashLocation()),
             v_blackHoleLocation(rhs.BlackHoleLocation()),
             v_wildcardPosition(rhs.WildcardPosition()),
             v_isAspire(rhs.IsAspire()),
             v_isMonth(rhs.IsMonth()),
             v_isPrefix(rhs.IsPrefix()),
             v_isNeverEos(rhs.IsNeverEos()),
             v_isOrdinal(rhs.IsOrdinal()),
             v_isPatException(rhs.IsPatException()),
			 v_endingLength(rhs.EndingLength())
{
}
//-------------------------------------------------------------------
DWordPhrase::DWordPhrase(int blackHoleLocation, 
						 int hashCode1, int hashCode2, int rootHashCode1, int rootHashCode2,
						 int henNum1, int henNum2, int rootHenNum1, int rootHenNum2,
                         int hashLocation, int headWord, bool isAspire, bool isMonth,
                         bool isNeverEos, bool isOrdinal, bool isPatException,
                         bool isPrefix, int languageCode, int protectionCode,
                         int wildcardPosition, int wordCount, int wordID,
                         int wordTypeCode, int endingLength)
            :v_blackHoleLocation(blackHoleLocation),
             v_hashCode1(hashCode1),
             v_hashCode2(hashCode2),
			 v_rootHashCode1(rootHashCode1),
			 v_rootHashCode2(rootHashCode2),
			 v_henNum1(henNum1),
			 v_henNum2(henNum2),
			 v_rootHenNum1(rootHenNum1),
			 v_rootHenNum2(rootHenNum2),
             v_hashLocation(hashLocation),
             v_headWord(headWord),
             v_isAspire(isAspire),
             v_isMonth(isMonth),
             v_isNeverEos(isNeverEos),
             v_isOrdinal(isOrdinal),
             v_isPatException(isPatException),
             v_isPrefix(isPrefix),
             v_languageCode(languageCode),
             v_protectionCode(protectionCode),
             v_wildcardPosition(wildcardPosition),
             v_wordCount(wordCount),
             v_wordID(wordID),
             v_wordTypeCode(wordTypeCode),
             v_companyCode(""),
             v_word(""),
			 v_endingLength(endingLength)
{
}
//-------------------------------------------------------------------
DWordPhrase::~DWordPhrase()
{
}
//-------------------------------------------------------------------
void DWordPhrase::Initialize(int blackHoleLocation, 
							 int hashCode1, int hashCode2, int rootHashCode1, int rootHashCode2,
							 int henNum1, int henNum2, int rootHenNum1, int rootHenNum2,
                             int hashLocation, int headWord, bool isAspire,
                             bool isMonth, bool isNeverEos, bool isOrdinal,
                             bool isPatException, bool isPrefix, int languageCode,
                             int protectionCode, int wildcardPosition, int wordCount,
                             int wordID, int wordTypeCode, int endingLength)
{
   v_blackHoleLocation = blackHoleLocation;
   v_hashCode1 = hashCode1;
   v_hashCode2 = hashCode2;
   v_rootHashCode1 = rootHashCode1;
   v_rootHashCode2 = rootHashCode2;
   v_henNum1 = henNum1;
   v_henNum2 = henNum2;
   v_rootHenNum1 = rootHenNum1;
   v_rootHenNum2 = rootHenNum2;
   v_hashLocation = hashLocation;
   v_headWord = headWord;
   v_isAspire = isAspire;
   v_isMonth = isMonth;
   v_isNeverEos = isNeverEos;
   v_isOrdinal = isOrdinal;
   v_isPatException = isPatException;
   v_isPrefix = isPrefix;
   v_languageCode = languageCode;
   v_protectionCode = protectionCode;
   v_wildcardPosition = wildcardPosition;
   v_wordCount = wordCount;
   v_wordID = wordID;
   v_wordTypeCode = wordTypeCode;
   v_endingLength = endingLength;
}
//-------------------------------------------------------------------
int DWordPhrase::Compare(const DObject& anObject) const
{
    // Compare overload.
   DWordPhrase& rhs = dynamic_cast<DWordPhrase&>(const_cast<DObject&>(anObject));

   if (WordID() > rhs.WordID())
   {
      return 1;
   }
   if (WordID() < rhs.WordID())
   {
      return -1;
   }
   if (CompanyCode() > rhs.CompanyCode())
   {
      return 1;
   }
   if (CompanyCode() < rhs.CompanyCode())
   {
      return -1;
   }
   return 0;
}
//-------------------------------------------------------------------
const DWordPhrase& DWordPhrase::operator=(const DWordPhrase& rhs)
{
   DObject::operator= (rhs);
   CompanyCode(rhs.CompanyCode());
   WordID(rhs.WordID());
   Word(rhs.Word());
   WordCount(rhs.WordCount());
   LanguageCode(rhs.LanguageCode());
   WordTypeCode(rhs.WordTypeCode());
   ProtectionCode(rhs.ProtectionCode());
   HashCode1(rhs.HashCode1());
   HashCode2(rhs.HashCode2());
   rootHashCode1(rhs.rootHashCode1());
   rootHashCode2(rhs.rootHashCode2());
   henNum1(rhs.henNum1());
   henNum2(rhs.henNum2());
   rootHenNum1(rhs.rootHenNum1());
   rootHenNum2(rhs.rootHenNum2());
   HeadWord(rhs.HeadWord());
   HashLocation(rhs.HashLocation());
   BlackHoleLocation(rhs.BlackHoleLocation());
   WildcardPosition(rhs.WildcardPosition());
   IsAspire(rhs.IsAspire());
   IsMonth(rhs.IsMonth());
   IsPrefix(rhs.IsPrefix());
   IsNeverEos(rhs.IsNeverEos());
   IsOrdinal(rhs.IsOrdinal());
   IsPatException(rhs.IsPatException());
   EndingLength(rhs.EndingLength());
   return *this;
}
//-------------------------------------------------------------------
void DWordPhrase::CompanyCode(const LgsString& newValue)
{
   if (newValue.size() != 3)
   {
      throw ("Invalid Company Code.  Must be 3 characters.");
   }
   v_companyCode = newValue;
}
//-------------------------------------------------------------------
void DWordPhrase::Word(const LgsString& newValue)
{
   v_word = newValue;
}
//-------------------------------------------------------------------
void DWordPhrase::WordID(int newValue)
{
   v_wordID = newValue;
}
//-------------------------------------------------------------------
void DWordPhrase::WordCount(int newValue)
{
   v_wordCount = newValue;
}
//-------------------------------------------------------------------
void DWordPhrase::LanguageCode(int newValue)
{
   v_languageCode = newValue;
}
//-------------------------------------------------------------------
void DWordPhrase::WordTypeCode(int newValue)
{
   v_wordTypeCode = newValue;
}
//-------------------------------------------------------------------
void DWordPhrase::ProtectionCode(int newValue)
{
   v_protectionCode = newValue;
}
//-------------------------------------------------------------------
void DWordPhrase::HashCode1(int newValue)
{
   v_hashCode1 = newValue;
}
//-------------------------------------------------------------------
void DWordPhrase::HashCode2(int newValue)
{
   v_hashCode2 = newValue;
}
//-------------------------------------------------------------------
void DWordPhrase::rootHashCode1(int newValue)
{
   v_rootHashCode1 = newValue;
}
//-------------------------------------------------------------------
void DWordPhrase::rootHashCode2(int newValue)
{
   v_rootHashCode2 = newValue;
}
//-------------------------------------------------------------------
void DWordPhrase::henNum1(int newValue)
{
   v_henNum1 = newValue;
}
//-------------------------------------------------------------------
void DWordPhrase::henNum2(int newValue)
{
   v_henNum2 = newValue;
}
//-------------------------------------------------------------------
void DWordPhrase::rootHenNum1(int newValue)
{
   v_rootHenNum1 = newValue;
}
//-------------------------------------------------------------------
void DWordPhrase::rootHenNum2(int newValue)
{
   v_rootHenNum2 = newValue;
}
//-------------------------------------------------------------------
void DWordPhrase::HeadWord(int newValue)
{
   v_headWord = newValue;
}
//-------------------------------------------------------------------
void DWordPhrase::HashLocation(int newValue)
{
   v_hashLocation = newValue;
}
//-------------------------------------------------------------------
void DWordPhrase::BlackHoleLocation(int newValue)
{
   v_blackHoleLocation = newValue;
}
//-------------------------------------------------------------------
void DWordPhrase::WildcardPosition(int newValue)
{
   v_wildcardPosition = newValue;
}
//-------------------------------------------------------------------
void DWordPhrase::EndingLength(int newValue)
{
   v_endingLength = newValue;
}
//-------------------------------------------------------------------
void DWordPhrase::IsAspire(bool newValue)
{
   v_isAspire = newValue;
}
//-------------------------------------------------------------------
void DWordPhrase::IsMonth(bool newValue)
{
   v_isMonth = newValue;
}
//-------------------------------------------------------------------
void DWordPhrase::IsPrefix(bool newValue)
{
   v_isPrefix = newValue;
}
//-------------------------------------------------------------------
void DWordPhrase::IsNeverEos(bool newValue)
{
   v_isNeverEos = newValue;
}
//-------------------------------------------------------------------
void DWordPhrase::IsOrdinal(bool newValue)
{
   v_isOrdinal = newValue;
}
//-------------------------------------------------------------------
void DWordPhrase::IsPatException(bool newValue)
{
   v_isPatException = newValue;
}
//-------------------------------------------------------------------
void DWordPhrase::LanguageCode(const LgsString& s)
{
   int working = 0;

   if (s.size() == 2)
   {
      if (s == "GE")
         working = 1;
      if (s == "EN")
         working = 2;
      if (s == "FR")
         working = 3;
      if (s == "SP")
         working = 4;
      if (s == "IT")
         working = 5;
   }

   LanguageCode(working);        // call the old constructor
}
//-------------------------------------------------------------------
