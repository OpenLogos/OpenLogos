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
#ifndef __constantsentenceunit_h__
#define __constantsentenceunit_h__

//-------------------------------------------------------------------
// File - constantsentenceunit.h
//
// Class - ConstantSentenceUnit
//
// Description - This is the abstract "root" class for Sentence Units
//      that exist on either the source or target side.
//
//-------------------------------------------------------------------

#include <logos_libs/linguistic/targetdictionaryunit.h>

class LDictionary;
//class TermSummary;

class ConstantSentenceUnit: public TargetDictionaryUnit
{
public:
   ConstantSentenceUnit();
   ConstantSentenceUnit(LDictionary&);
   virtual ~ConstantSentenceUnit();

   bool isConstantSentenceUnit();
   virtual int constantID() const;
   virtual void generateDictionaryEntry();
   virtual void setSurfaceExpressionFromDictionary();
   virtual void generateStem();
   virtual int precedingSpaces() const;
   virtual int  blackHoleSentenceAddress() const;
   virtual void insertBlackHole(const BlackHole*);

   virtual void persistOut(ostream&);
   virtual void persistIn(istream&);

   bool isHigh() const;
   void setHigh(bool);

private:
   LWordMarkup v_wordMarkup;
   bool v_isHigh;
};

//-------------------------------------------------------------------
inline bool ConstantSentenceUnit::isConstantSentenceUnit()
{
   return true;
}
//-------------------------------------------------------------------
inline bool ConstantSentenceUnit::isHigh() const
{
   return v_isHigh;
}
//-------------------------------------------------------------------
inline void ConstantSentenceUnit::setHigh(bool b)
{
   v_isHigh = b;
}
//-------------------------------------------------------------------
inline int ConstantSentenceUnit::constantID() const
{
   int constantID = opadr();
   return (0 > constantID) ? -constantID : constantID;
}

#endif // __constantsentenceunit_h__

