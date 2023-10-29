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
// file: Phrase.cpp

#include <logos_include/logoscommon.h>
#include <logos_libs/utility/phrase.h>
#include <logos_libs/utility/stringutil.h>

Phrase::Phrase(Phrase::Key key, const LgsString &text, int trailingSpaces, char separator)
       : key_(key),
         dirty_(false),
         separator_(separator)
{
   assert(trailingSpaces >= 0);
   text_ = text + LgsString(trailingSpaces, separator);
}
//-------------------------------------------------------------------
LgsString Phrase::getText(int *trailingSpaces)
{
   dirty_ = false;
   LgsString ret = text_;
   StringUtil::rightTrim(ret, separator_);
   if (trailingSpaces != 0)
      *trailingSpaces = text_.length() - ret.length();
   return ret;
}
//-------------------------------------------------------------------
void Phrase::setText(const CharacterVector& buffer, int trailingSpaces)
{
   text_ = buffer.asString() + LgsString(trailingSpaces, separator_);
   dirty_ = true;
}
//-------------------------------------------------------------------
void Phrase::replace(int pos, int length, const LgsString& replacement)
{
   text_.replace(pos, length, replacement);
   dirty_ = true;
}
//-------------------------------------------------------------------
int Phrase::wordCount(int startPos, int endPos) const
{
   if (endPos == startPos)
      return 0;

   LgsString temp = text_;
   StringUtil::rightTrim(temp, separator_);
   endPos = max(endPos, int(temp.length()));

#if _MSC_VER >= 1100
   int words = 1 + count(temp.begin() + startPos, temp.begin() + endPos, separator_);
#else
   int words = 1;
   // changed (bk Jul 19 2005), no idea where this count is supposed to come
   // from 
   LgsString::iterator curr = temp.begin() + startPos;
   LgsString::iterator end = temp.begin() + endPos;
   while (curr != end) {
     if (*curr != separator_) curr++;
     else {
       words++;
       // skip multiple spaces
       while (*curr == separator_ && curr != end) curr++;
     }
   }
   //count(temp.begin() + startPos, temp.begin() + endPos, separator_, words);
#endif
   return words;
}
//-------------------------------------------------------------------
int Phrase::eraseAndCountWords(int startPos, int endPos)
{
   int words = wordCount(startPos, endPos);
   insertPos_ = startPos;
   text_.erase(text_.begin() + startPos, text_.begin() + endPos);
   dirty_ = true;
   return words;
}
//-------------------------------------------------------------------
void Phrase::insert(const CharacterVector &buffer)
{	
   //ObjectSpace STL does not support member template interfaces
   //text_.insert(text_.begin() + insertPos_, buffer.begin(), buffer.end());
   int index = insertPos_;
   for (LgsVector(char)::const_iterator beginIter = buffer.begin(); beginIter != buffer.end(); beginIter++)
   {
      text_.insert(text_.begin() + index, *beginIter);
      index++;
   }
}
//-------------------------------------------------------------------
void Phrase::moveLeadingSpaces(Phrase* nextPhrase)
{
   LgsString::size_type spaces = nextPhrase->removeLeadingSpaces();
   if (spaces != 0)
      text_.insert(LgsString::size_type(length()), spaces, separator_);
   dirty_ = true;
}
//-------------------------------------------------------------------
LgsString::size_type Phrase::removeLeadingSpaces()
{
   LgsString::size_type spaces = text_.find_first_not_of(separator_);
   if (spaces == NPOS)
   {
      // text_ is all spaces
      spaces = text_.length();
   }

   if (spaces != 0)
   {
      text_.erase(0, spaces);
      dirty_ = true;
   }

   return spaces;
}

