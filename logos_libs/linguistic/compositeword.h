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
#ifndef __compositeword_h__
#define __compositeword_h__

//-------------------------------------------------------------------
// File - compositeword.h
//
// Class - CompositeWord
//
// Description - this class simulates the behavior of a LgsString.
//      However, it maintains its data to two forms, as a simple
//      LgsString object (the buffer) and as a vector of individual
//      words.
//
//      The duplicated data involved with the buffer and the vector
//      of words have to be in sync whenever the data is updated.
//
//-------------------------------------------------------------------

#include <logos_libs/linguistic/lword.h>

class LLanguage;

class CompositeWord
{
public:
   CompositeWord();
   CompositeWord(const LWord&);
   CompositeWord(LWordIterator begin, LWordIterator end);
   CompositeWord(const LgsString&, const LLanguage*);
   CompositeWord(const CompositeWord&);
   virtual ~CompositeWord();

   LgsString::size_type length() const;
   int totalWords() const;
   bool isEmpty() const;
   void clear();
   const LWordVector& words() const;
   void setWords(LWordIterator begin, LWordIterator end);
   char delimiter() const;
   void setDelimiter(char);
   const LgsString& toString() const;
   void fromString(const LgsString&, const LLanguage*);

   // beginsUpperCase() - returns true if the object begins in upper case.
   bool beginsUpperCase() const;
   bool isAllUpperCase() const;

   int precedingSpaces() const;
   int trailingSpaces() const;
   const CompositeWord& operator+=(const CompositeWord&);
   const CompositeWord& operator+=(const LWord&);
   const CompositeWord& operator=(const CompositeWord&);
   const CompositeWord& operator=(const LWord&);
   void insertString(int position, const LgsString&);
   void insertWord(int position, const LWord&);

protected:
   void synchronizeString();
   void synchronizeWords();

   LWordIterator beginWords() const;
   LWordIterator endWords() const;

private:
   LWordVector words_;
   LgsString buffer_;
   char delimiter_;
   const LLanguage* p_language;
};

//-------------------------------------------------------------------
inline LWordIterator CompositeWord::beginWords() const
{
   return const_cast<LWordVector&>(words_).begin();
}
//-------------------------------------------------------------------
inline LWordIterator CompositeWord::endWords() const
{
   return const_cast<LWordVector&>(words_).end();
}
//-------------------------------------------------------------------
inline int CompositeWord::precedingSpaces() const
{
   if (words_.empty())
   {
      return 0;
   }
   return words_.front().precedingSpaces();
//   return beginWords()->precedingSpaces();
}
//-------------------------------------------------------------------
inline int CompositeWord::trailingSpaces() const
{
   if (words_.empty())
   {
      return 0;
   }
   return words_.back().trailingSpaces();
}

//-------------------------------------------------------------------
inline int CompositeWord::totalWords() const
{
   return words_.size();
}

#endif // __CompositeWord_h__

