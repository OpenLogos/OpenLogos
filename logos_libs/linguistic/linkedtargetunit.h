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
#ifndef __LinkedTargetUnit_h__
#define __LinkedTargetUnit_h__

//-------------------------------------------------------------------
// File - constantsentenceunit.h
//
// Class - LinkedTargetUnit
//
// Description - This is the abstract "root" class for Sentence Units
//      that exist on either the source or target side.
//
//-------------------------------------------------------------------

#include <logos_libs/linguistic/targetsentenceunit.h>
//#include <logos_libs/linguistic/sourcesentenceunit.h>

class LDictionary;
class SourceSentenceUnit;

class LinkedTargetUnit: public TargetSentenceUnit
{
public:
   LinkedTargetUnit();
   LinkedTargetUnit(LDictionary&);
   LinkedTargetUnit(const LinkedTargetUnit&);
   virtual ~LinkedTargetUnit();

   //---------------------------------------------------------------
   virtual void stemGenerate();
   virtual void capitalize(int offset);
   virtual const LgsString& word() const;

   virtual void findAssociatedSourceUnit(SourceUnitVector&);
   virtual void setSurfaceExpressionFromDictionary();
   virtual bool allowsPrecedingSpaces() const;
   virtual bool allowsTrailingSpaces() const;
   virtual int precedingSpaces() const;

   virtual void persistOut(ostream&);
   virtual void persistIn(istream&);

   SourceSentenceUnit& sourceSentenceUnit() const;
   virtual SourceSentenceUnit* sourceSentenceUnit();
   void setSourceSentenceUnit(SourceSentenceUnit* unit);
   void checkForPhraseModification(SourceSentenceUnit* unit);
   virtual bool isLinkedTargetUnit();

protected:
   SourceSentenceUnit* p_sourceSentenceUnit;
};

//-------------------------------------------------------------------
inline SourceSentenceUnit& LinkedTargetUnit::sourceSentenceUnit()const
{
   assert(p_sourceSentenceUnit);
   return *p_sourceSentenceUnit;
}
//-------------------------------------------------------------------
inline SourceSentenceUnit* LinkedTargetUnit::sourceSentenceUnit()
{
//   assert(p_sourceSentenceUnit);
   return p_sourceSentenceUnit;
}
//-------------------------------------------------------------------
inline void LinkedTargetUnit::setSourceSentenceUnit(SourceSentenceUnit* unit)
{
   assert(!p_sourceSentenceUnit);
   p_sourceSentenceUnit = unit;
}
//-------------------------------------------------------------------
inline bool LinkedTargetUnit::isLinkedTargetUnit()
{
   return true;
}

#endif // __LinkedTargetUnit_h__

