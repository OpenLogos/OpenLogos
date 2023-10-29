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
#ifndef __punctuationconstantsentenceunit_h__
#define __punctuationconstantsentenceunit_h__

//-------------------------------------------------------------------
// File - punctuationconstantsentenceunit.h
//
// Class - PunctuationConstantSentenceUnit
//
// Description - This is the abstract "root" class for Sentence Units
//      that exist on either the source or target side.
//
//-------------------------------------------------------------------

#include <logos_libs/linguistic/linkedtargetunit.h>

class LDictionary;

class PunctuationConstantSentenceUnit: public LinkedTargetUnit
{
public:
   //---------------------------------------------------------------
   // Public constructors and destructors.
   //---------------------------------------------------------------

   PunctuationConstantSentenceUnit();
   PunctuationConstantSentenceUnit(int);
   PunctuationConstantSentenceUnit(LDictionary&);
   PunctuationConstantSentenceUnit(const PunctuationConstantSentenceUnit&);
   virtual ~PunctuationConstantSentenceUnit();

   //---------------------------------------------------------------
   virtual void reconcileMarkup();
   virtual void stemGenerate();
   virtual bool isPunctuation() const;
   virtual bool isPunctuationConstant() const;
   virtual bool allowsPrecedingSpaces() const;
   virtual bool allowsTrailingSpaces() const;
   virtual void capitalize(int offset);
   virtual void setSurfaceExpressionFromDictionary();
   virtual int precedingSpaces()const;
   virtual int trailingSpaces() const;
   virtual void persistOut(ostream&);
   virtual void persistIn(istream&);
   virtual const LgsString& word()const;
   virtual void findAssociatedSourceUnit(SourceUnitVector&);
   static bool isPunctuationConstantAddress(int opadr);

   //---------------------------------------------------------------
   typedef enum
   {
      Period             = -121,
      GmComma            = -122,
      Colon              = -123,
      Semicolon          = -124,
      RightParen         = -125,
      LeftParen          = -126,
      GmHyphen           = -127,
      EqualSign          = -128,
      Exclamation        = -130,
      Hyphen2            = -133,
      Hyphen3            = -575,
      HyphenRemove       = -579,
      LeftParen2         = -866,
      RightParen2        = -877,
      Period2            = -879,
      MinusMinus         = -939
   } PunctuationCONSTANT;

private:
    LgsString v_word;
};
//---------------------------------------------------------------------
inline bool PunctuationConstantSentenceUnit::isPunctuationConstant() const
{
   return true;
}
//---------------------------------------------------------------------
inline void PunctuationConstantSentenceUnit::stemGenerate()
{
}

#endif // __punctuationconstantsentenceunit_h__

