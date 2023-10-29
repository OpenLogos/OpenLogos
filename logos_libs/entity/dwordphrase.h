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
#ifndef __DWordPhrase_h__
#define __DWordPhrase_h__

//-------------------------------------------------------------------
// File - DWordPhrase.h
//
// Class - DWordPhrase
//
//-------------------------------------------------------------------

#include <logos_libs/entity/dobject.h>

class DWordPhrase: public DObject
{
public:
   //---------------------------------------------------------------
   // The public constructors and destructors. The default
   // constructor is typically made public unless class construction
   // is being limited (in which case it is made protected. The
   // destructor is declared virtual to allow subclassing.
   //---------------------------------------------------------------
   DWordPhrase();
   DWordPhrase(const LgsString& word, int language_code);
   DWordPhrase(const DWordPhrase&);
   DWordPhrase(int blackHoleLocation, 
	           int,int,int,int,		// hash code info
			   int,int,int,int,		// hen num info
			   int hashLocation,
               int headWord, bool isAspire, bool isMonth, bool isNeverEos,
               bool isOrdinal, bool isPatException, bool isPrefix, int languageCode,
               int protectionCode, int wildcardPosition, int wordCount, int wordID,
               int wordTypeCode, int endingLength );
   virtual ~DWordPhrase();

   //---------------------------------------------------------------
   const LgsString& AsString() const;
   int Length() const;

   //---------------------------------------------------------------
   // These are "getter" member functions. They allow read access
   // to the internal data of the class. There is no real behavior
   // in these functions but they are important for use in more
   // complex member functions (these member functions can be made
   // "protected" if we do not want to export this data).
   //
   // These functions are typically defined inline because they are
   // usually only one statement in length. If the compiler leaves
   // them inline they should have absolutely not overhead.
   //---------------------------------------------------------------
   const LgsString& CompanyCode() const;
   int WordID() const;
   const LgsString& Word() const;
   int WordCount() const;
   int LanguageCode() const;
   int WordTypeCode() const;
   int HeadWord() const;
   int ProtectionCode() const;
   int HashCode1() const;
   int HashCode2() const;
   int rootHashCode1() const;
   int rootHashCode2() const;
   int henNum1() const;
   int henNum2() const;
   int rootHenNum1() const;
   int rootHenNum2() const;	
   int HashLocation() const;
   int BlackHoleLocation() const;
   int WildcardPosition() const;
   int EndingLength() const;
   bool IsAspire() const;
   bool IsMonth() const;
   bool IsPrefix() const;
   bool IsNeverEos() const;
   bool IsOrdinal() const;
   bool IsPatException() const;

   //---------------------------------------------------------------
   // These are "setter" member functions. They allow write access
   // to the internal data of the class. Again, there is no real
   // behavior here. These functions do perform more abstract
   // behavior in that the not only update a member variable but
   // they set the object to a "dirty" state, indicating that it
   // needs to be saved to the database, etc.,.
   //
   // Since they perform non-simple operations they are not defined
   // as inline.
   //---------------------------------------------------------------
   virtual void CompanyCode(const LgsString&);
   virtual void WordID(int);
   virtual void Word(const LgsString&);
   virtual void WordCount(int);
   virtual void LanguageCode(int);
   virtual void LanguageCode(const LgsString& );
   virtual void WordTypeCode(int);
   virtual void ProtectionCode(int);
   virtual void HashCode1(int);
   virtual void HashCode2(int);
   virtual void rootHashCode1(int);
   virtual void rootHashCode2(int);
   virtual void henNum1(int);
   virtual void henNum2(int);
   virtual void rootHenNum1(int);
   virtual void rootHenNum2(int);
   virtual void HeadWord(int);
   virtual void HashLocation(int);
   virtual void BlackHoleLocation(int);
   virtual void WildcardPosition(int);
   virtual void EndingLength(int);
   virtual void IsAspire(bool);
   virtual void IsMonth(bool);
   virtual void IsPrefix(bool);
   virtual void IsNeverEos(bool);
   virtual void IsOrdinal(bool);
   virtual void IsPatException(bool);

   //---------------------------------------------------------------
   void Initialize(int blackHoleLocation, 
					int,int,int,int,	// hash code info
					int,int,int,int,	// hen num info
                   int hashLocation, int headWord, bool isAspire, bool isMonth,
                   bool isNeverEos, bool isOrdinal, bool isPatException,
                   bool isPrefix, int languageCode, int protectionCode,
                   int wildcardPosition, int wordCount, int wordID, int wordTypeCode,
				   int endingLength);

   //---------------------------------------------------------------
   // Each subclass of DObject should overload the Compare()
   // member function. All the relational operators are written in
   // terms of this member and are inherited from DObject.
   //
   // This member returns a 0 if the object is equal to the
   // the arguement, a -1 if the object is less thatn the arguement,
   // and a +1 if it is greater than the arguement.If a class does
   // not overload this member the one defined in DObject will
   // be called and will return a 0 (meaning the objects will always
   // appear to be equal to each other).
   //---------------------------------------------------------------
   virtual int Compare(const DObject&) const;

   //---------------------------------------------------------------
   // The assignment operator. This needs to be overloaded in each
   // class. The compiler supplies a default one that performs a
   // shallow copy.
   //---------------------------------------------------------------
   const DWordPhrase& operator=(const DWordPhrase&);

private:
   //---------------------------------------------------------------
   // The member variables. These are private because all access to
   // them, including access by member functions of this class or
   // derived class should be through the "getter" and "setter"
   // member functions.
   //---------------------------------------------------------------
   LgsString v_companyCode;
   int v_wordID;
   LgsString v_word;
   int v_wordCount;
   int v_languageCode;
   int v_wordTypeCode;
   int v_protectionCode;
   int v_hashCode1;
   int v_hashCode2;
   int v_rootHashCode1;
   int v_rootHashCode2;
   int v_henNum1;
   int v_henNum2;
   int v_rootHenNum1;
   int v_rootHenNum2;	
   int v_headWord;
   int v_hashLocation;
   int v_blackHoleLocation;
   int v_wildcardPosition;
   bool v_isAspire;
   bool v_isMonth;
   bool v_isPrefix;
   bool v_isNeverEos;
   bool v_isOrdinal;
   bool v_isPatException;
   int v_endingLength;
};

typedef LgsVector(DWordPhrase) DWordPhraseVector;
typedef DWordPhraseVector::iterator DWordPhraseIterator;

//-------------------------------------------------------------------
inline const LgsString& DWordPhrase::AsString() const
{
   return v_word;
}
//-------------------------------------------------------------------
inline int DWordPhrase::Length() const
{
   return v_word.size();
}
//-------------------------------------------------------------------
inline const LgsString& DWordPhrase::CompanyCode() const
{
   return v_companyCode;
}
//-------------------------------------------------------------------
inline int DWordPhrase::WordID() const
{
   return v_wordID;
}
//-------------------------------------------------------------------
inline const LgsString& DWordPhrase::Word() const
{
   return v_word;
}
//-------------------------------------------------------------------
inline int DWordPhrase::WordCount() const
{
   return v_wordCount;
}
//-------------------------------------------------------------------
inline int DWordPhrase::LanguageCode() const
{
   return v_languageCode;
}
//-------------------------------------------------------------------
inline int DWordPhrase::WordTypeCode() const
{
   return v_wordTypeCode;
}
//-------------------------------------------------------------------
inline int DWordPhrase::ProtectionCode() const
{
   return v_protectionCode;
}
//-------------------------------------------------------------------
inline int DWordPhrase::HashCode1() const
{
   return v_hashCode1;
}
//-------------------------------------------------------------------
inline int DWordPhrase::HashCode2() const
{
   return v_hashCode2;
}
//-------------------------------------------------------------------
inline int DWordPhrase::rootHashCode1() const
{
   return v_rootHashCode1;
}
//-------------------------------------------------------------------
inline int DWordPhrase::rootHashCode2() const
{
   return v_rootHashCode2;
}
//-------------------------------------------------------------------
inline int DWordPhrase::henNum1() const
{
   return v_henNum1;
}
//-------------------------------------------------------------------
inline int DWordPhrase::henNum2() const
{
   return v_henNum2;
}
//-------------------------------------------------------------------
inline int DWordPhrase::rootHenNum1() const
{
   return v_rootHenNum1;
}
//-------------------------------------------------------------------
inline int DWordPhrase::rootHenNum2() const
{
   return v_rootHenNum2;
}
//-------------------------------------------------------------------
inline int DWordPhrase::HeadWord() const
{
   return v_headWord;
}
//-------------------------------------------------------------------
inline int DWordPhrase::HashLocation() const
{
   return v_hashLocation;
}
//-------------------------------------------------------------------
inline int DWordPhrase::BlackHoleLocation() const
{
   return v_blackHoleLocation;
}
//-------------------------------------------------------------------
inline int DWordPhrase::WildcardPosition() const
{
   return v_wildcardPosition;
}
//-------------------------------------------------------------------
inline int DWordPhrase::EndingLength() const
{
   return v_endingLength;
}
//-------------------------------------------------------------------
inline bool DWordPhrase::IsAspire() const
{
   return v_isAspire;
}
//-------------------------------------------------------------------
inline bool DWordPhrase::IsMonth() const
{
   return v_isMonth;
}
//-------------------------------------------------------------------
inline bool DWordPhrase::IsPrefix() const
{
   return v_isPrefix;
}
//-------------------------------------------------------------------
inline bool DWordPhrase::IsNeverEos() const
{
   return v_isNeverEos;
}
//-------------------------------------------------------------------
inline bool DWordPhrase::IsOrdinal() const
{
   return v_isOrdinal;
}
//-------------------------------------------------------------------
inline bool DWordPhrase::IsPatException() const
{
   return v_isPatException;
}

#endif // __DWordPhrase_H__

