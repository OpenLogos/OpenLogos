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
// File - compositeword.cpp
//
// Class - CompositeWord (implementation)
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/compositeword.h>
#include <logos_libs/linguistic/llanguage.h>

//-------------------------------------------------------------------
CompositeWord::CompositeWord()
              :delimiter_(' ')
{
}
//-------------------------------------------------------------------
CompositeWord::CompositeWord(const LWord& w)
              :p_language(w.language()),
               delimiter_(' ')
{
   operator+=(w);
}
//-------------------------------------------------------------------
CompositeWord::CompositeWord(LWordIterator begin, LWordIterator end)
              :p_language(begin->language()),
               delimiter_(' ')
{
   setWords(begin, end);
}
//-------------------------------------------------------------------
CompositeWord::CompositeWord(const LgsString& s, const LLanguage* aLanguage)
              :delimiter_(' ')
{
   fromString(s, aLanguage);
}
//-------------------------------------------------------------------
CompositeWord::CompositeWord(const CompositeWord& rhs)
              :buffer_(rhs.buffer_),
               words_(rhs.words_),
               delimiter_(rhs.delimiter()),
               p_language(rhs.p_language)
{
}
//-------------------------------------------------------------------
CompositeWord::~CompositeWord()
{
}
//-------------------------------------------------------------------
bool CompositeWord::beginsUpperCase() const
{
   for (LWordVector::const_iterator i = words_.begin(); i != words_.end(); ++i)
   {
      if (!StringUtil::beginsUpperCase(*i))
      {
         return false;
      }
   }
   return true;
}
//-------------------------------------------------------------------
bool CompositeWord::isAllUpperCase() const
{
   // If every word is All Upper Case this object is All Upper.
   for (LWordVector::const_iterator i =  words_.begin(); i != words_.end(); ++i)
   {
      if (!StringUtil::isAllUpperCase(*i))
      {
         return false;
      }
   }
   return true;
}
//-------------------------------------------------------------------
const CompositeWord& CompositeWord::operator=(const CompositeWord& rhs)
{
   if (&rhs != this)
   {
      buffer_ = rhs.buffer_;
      words_ = rhs.words_;
      delimiter_ = rhs.delimiter();
      p_language = rhs.p_language;
   }
   return *this;
}
//-------------------------------------------------------------------
char CompositeWord::delimiter() const
{
   return delimiter_;
}
//-------------------------------------------------------------------
void CompositeWord::setDelimiter(char c)
{
   delimiter_ = c;
   synchronizeString();
}
//-------------------------------------------------------------------
const CompositeWord& CompositeWord::operator=(const LWord& rhs)
{
   words_.erase(words_.begin(), words_.end());
   operator+=(rhs);
   synchronizeString();

   return *this;
}
//-------------------------------------------------------------------
bool CompositeWord::isEmpty() const
{
   return words_.empty();
}
//-------------------------------------------------------------------
void CompositeWord::clear()
{
   words_.erase(words_.begin(), words_.end());
   buffer_ = "";
}
//-------------------------------------------------------------------
void CompositeWord::insertString(int position, const LgsString& s)
{
   if (buffer_.length() <= position)
   {
      position = s.length() - 1;
   }
   buffer_.insert(position, s);
   buffer_.insert(position + s.size(), LgsString(" "));
   synchronizeWords();
}
//-------------------------------------------------------------------
void CompositeWord::insertWord(int position, const LWord& w)
{
   if (words_.size() <= position)
   {
      words_.push_back(w);
   }
   else
   {
      words_.insert (words_.begin() + position, w);
   }
   synchronizeString();
}
//-------------------------------------------------------------------
LgsString::size_type CompositeWord::length() const
{
   // Simply returns the length of the buffer LgsString.
   return buffer_.length();
}
//-------------------------------------------------------------------
const LWordVector& CompositeWord::words() const
{
   // Simply returns the word vector.
   return words_;
}
//-------------------------------------------------------------------
void CompositeWord::setWords(LWordIterator begin, LWordIterator end)
{
   words_.erase(words_.begin(), words_.end());
   if (begin != end)
   {
      for (LWordIterator i = begin; i != end; i++)
      {
         operator+=(*i);
      }
   }
   synchronizeString();
}
//-------------------------------------------------------------------
const LgsString& CompositeWord::toString() const
{
   // Just returns the buffer.
   (const_cast <CompositeWord *>(this))->synchronizeString();
   return buffer_;
}
//-------------------------------------------------------------------
void CompositeWord::fromString(const LgsString& s, const LLanguage* l)
{
   buffer_.replace(0, buffer_.length(), s);
   p_language = l;
   synchronizeWords();
}
//-------------------------------------------------------------------
const CompositeWord& CompositeWord::operator+=(const LWord& w)
{
   words_.push_back(w);
   synchronizeString();
   return *this;
}
//-------------------------------------------------------------------
const CompositeWord& CompositeWord::operator+=(const CompositeWord& w)
{
   for (LWordIterator i = w.beginWords(); i != w.endWords(); i++)
   {
      words_.push_back(*i);
   }
   synchronizeString();
   return *this;
}
//-------------------------------------------------------------------
void CompositeWord::synchronizeString()
{
   char s[1024] = "";
   char delimiterString[2];

   delimiterString[0] = delimiter();
   delimiterString[1] = 0;

   for (LWordVector::const_iterator i =  words_.begin(); i != words_.end(); i++)
   {
      if (i != (words_.begin()))
      {
         for (int j = 0; j < i->precedingSpaces(); j++)
            strncat(s, delimiterString, 1023);
      }
      strncat(s, i->c_str(), 1023);
   }
   buffer_ = (char*) s;
}
//-------------------------------------------------------------------
//void CompositeWord::synchronizeWords()
//{
//}
//-------------------------------------------------------------------
void CompositeWord::synchronizeWords()
{
   LgsVector(LgsString) temp1Words;
   LgsVector(LgsString) temp2Words;
   LgsVector(LgsString) tokens;
   StringUtil::parseInto(buffer_, temp1Words, delimiter(), true);
   LgsVector(LgsString)::iterator i;
   LgsVector(LgsString)::iterator j;
   for (i =  temp1Words.begin(); i != temp1Words.end(); i++)
   {
      LgsVector(LgsString) t1;
      StringUtil::parseInto(*i, t1, '-', true);
      if (1 < t1.size())
      {
         for (j = t1.begin(); j != t1.end(); j++)
         {
            temp2Words.push_back(*j);
         }
      }
      else
      {
         temp2Words.push_back(*i);
      }
   }
   for (i = temp2Words.begin(); i != temp2Words.end(); i++)
   {
      LgsVector(LgsString) t2;
      StringUtil::parseInto(*i, t2, '/', true);
      if (1 < t2.size())
      {
         for (j = t2.begin(); j != t2.end(); j++)
         {
            tokens.push_back(*j);
         }
      }
      else
      {
         tokens.push_back(*i);
      }
   }

   words_.erase(words_.begin(), words_.end());
   LgsString prevStr = "";
   int prevSpace = 1;
   for (LgsVector(LgsString)::const_iterator iter = tokens.begin(); iter != tokens.end(); iter++)
   {
      if (iter == tokens.begin())
      {
         if (((*iter)[0] == '-') || ((*iter)[0] == '/'))
            prevSpace = 0;
         else
            prevSpace = 1;
      }
      else
      {
         if (((*iter)[0] == '-') && (prevStr[0] == '-'))
            prevSpace = 0;
      }

      if ((*iter)[0] == delimiter())
      {
         prevSpace = 1;
      }
      else
      {
         words_.push_back(LWord(*iter, p_language, prevSpace));
         prevSpace = 0;
         prevStr = *iter;
      }
   }
}
