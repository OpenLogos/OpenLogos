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
#ifndef __targetdictionaryunit_h__
#define __targetdictionaryunit_h__

//-------------------------------------------------------------------
// File - targetdictionaryunit.h
//
// Class - TargetDictionaryUnit
//
// Description - .
//
//-------------------------------------------------------------------

#include <logos_libs/linguistic/linkedtargetunit.h>
//#include <logos_libs/linguistic/sourcesentenceunit.h>
//#include <logos_libs/linguistic/ldictionaryentry.h>
#include  <logos_libs/linguistic/lsemantosyntacticunit.h>
#include <logos_libs/linguistic/transfer.h>

class LDictionary;
class LDictionaryEntry;

class TargetDictionaryUnit: public LinkedTargetUnit
{
public:
   enum wordTypeCODE
   {
      wordTYPE = 1,
      acronymTYPE = 2,
      abbrevTYPE = 3
   };

   TargetDictionaryUnit();
   TargetDictionaryUnit(class LDictionary&);
   TargetDictionaryUnit(const TargetDictionaryUnit&);
   virtual ~TargetDictionaryUnit();

   virtual void reconcileMarkup();
   virtual void assignMarkupToWords();
   virtual void makeAspiration(TargetSentenceUnit* prev);

   LDictionaryEntry& dictionaryEntry() const;
   void dictionaryEntry(LDictionaryEntry*);
   void deleteEntry();
   const LSemantoSyntacticUnit& primarySsu() const;
   int verbPrefixInseparableLength() const;
   int verbPrefixInseparablePosition() const;
   int verbPrefixSeparableLength() const;
   int verbPrefixSeparablePosition() const;
   int positionAfterVerbPrefix() const;
   int sourceMeaningID();
   virtual int headWord() const;

   virtual void capitalize(int offset);
   virtual int wordTypeCode() const;
   virtual int wordClassCode() const;
   virtual const LgsString& word() const;
   virtual int wordID() const;
   virtual int wordCount() const;
   virtual int patNumber() const;
   virtual int genderCode() const;
   virtual int usageID() const;
   virtual int declension() const;
   virtual int degree() const;

   void addTransfer(const Transfer&);
   int transferID();
   int combiningFormCode();
   int blackHoleLocation() const;
   const LgsString& stem () const;
   const LgsString& separableVerbPrefix() const;
   const LgsString& inseparableVerbPrefix() const;
   virtual void persistOut(ostream&);
   virtual void persistIn (istream&);
   int primarySsuPosition() const;
   virtual void checkSconValues();
   virtual bool isTargetDictionaryUnit();

protected:
   enum {
      MainTransfer = 1,
      FirstAlternateTransfer = 2,
      SecondAlternateMainTransfer = 3,
      SecondAlternateFirstTransfer = 4
   };

   Transfer* transfer();
   int blackHoleLength () const;
   void incrementBlackHoleLength(int);
   bool isDictionaryMatched() const;
   void decomposeWord() const;
   virtual bool isAspired() const;

private:
   TransferVector v_transfers;
   LDictionaryEntry* p_dictionaryEntry;
   int v_blackHoleLength;
   bool v_areTransfersSorted;
   bool v_useCombiningForm;
   bool v_isDecomposed;
   LgsString v_stem;
   LgsString v_separableVerbPrefix;
   LgsString v_inseparableVerbPrefix;
};

//---------------------------------------------------------------------
inline void TargetDictionaryUnit::addTransfer(const Transfer& t)
{
   v_transfers.push_back (t);
}
//---------------------------------------------------------------------
inline bool TargetDictionaryUnit::isAspired() const
{
   return primarySsu().isAspire();
}
//---------------------------------------------------------------------
inline int TargetDictionaryUnit::blackHoleLocation() const
{
   return primarySsu().blackHoleLocation() + v_blackHoleLength;
}
//---------------------------------------------------------------------
inline int TargetDictionaryUnit::headWord() const
{
   return primarySsu().headWord();
}
//-------------------------------------------------------------------
inline int TargetDictionaryUnit::blackHoleLength() const
{
   return v_blackHoleLength;
}
//-------------------------------------------------------------------
inline void TargetDictionaryUnit::incrementBlackHoleLength(int n)
{
   v_blackHoleLength += n;
}
//---------------------------------------------------------------------
inline bool TargetDictionaryUnit::isDictionaryMatched() const
{
   return (wordID() != LSemantoSyntacticUnit::unfoundWordID());
}
//-------------------------------------------------------------------
inline LDictionaryEntry& TargetDictionaryUnit::dictionaryEntry() const
{
   return *p_dictionaryEntry;
}
//-------------------------------------------------------------------
inline void TargetDictionaryUnit::dictionaryEntry(LDictionaryEntry* entry)
{
   assert(0 == p_dictionaryEntry);
   p_dictionaryEntry = entry;
}
//-------------------------------------------------------------------
inline int TargetDictionaryUnit::primarySsuPosition() const
{
   return 0;
}
//-------------------------------------------------------------------
inline int TargetDictionaryUnit::verbPrefixInseparableLength() const
{
   return primarySsu().verbPrefixInsepLength();
}
//-------------------------------------------------------------------
inline int TargetDictionaryUnit::verbPrefixInseparablePosition() const
{
   return primarySsu().verbPrefixInseparable();
}
//-------------------------------------------------------------------
inline int TargetDictionaryUnit::verbPrefixSeparableLength() const
{
   return primarySsu().verbPrefixSepLength();
}
//-------------------------------------------------------------------
inline int TargetDictionaryUnit::verbPrefixSeparablePosition() const
{
   return primarySsu().verbPrefixSeparable();
}
//-------------------------------------------------------------------
inline int TargetDictionaryUnit::wordTypeCode() const
{
   return primarySsu().wordTypeCode();
}
//---------------------------------------------------------------------
inline int TargetDictionaryUnit::wordID() const
{
   return primarySsu().wordID();
}
//---------------------------------------------------------------------
inline int TargetDictionaryUnit::genderCode() const
{
   return primarySsu().genderCode();
}
//---------------------------------------------------------------------
inline int TargetDictionaryUnit::wordCount() const
{
   return primarySsu().wordCount();
}
//-------------------------------------------------------------------
inline bool TargetDictionaryUnit::isTargetDictionaryUnit()
{
   return true;
}

#endif // __targetdictionaryunit_h__

