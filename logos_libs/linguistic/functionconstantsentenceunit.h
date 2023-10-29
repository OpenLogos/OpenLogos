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
#ifndef __functionconstantsentenceunit_h__
#define __functionconstantsentenceunit_h__

//-------------------------------------------------------------------
// File - functionconstantsentenceunit.h
//
// Class - FunctionConstantSentenceUnit
//
// Description - This is the abstract "root" class for Sentence Units
//      that exist on either the source or target side.
//
//-------------------------------------------------------------------

#include <logos_libs/linguistic/targetsentenceunit.h>

class FunctionConstantSentenceUnit: public TargetSentenceUnit
{
public:
   FunctionConstantSentenceUnit();
   FunctionConstantSentenceUnit(int);
   FunctionConstantSentenceUnit(LDictionary&);
   FunctionConstantSentenceUnit(const FunctionConstantSentenceUnit&);
   virtual ~FunctionConstantSentenceUnit();

   //---------------------------------------------------------------
   virtual void reconcileMarkup();
   virtual void setSurfaceExpressionFromDictionary();
   virtual void generateStem();
   virtual const LgsString& word() const;
   virtual bool isBlackHoleStart() const;
   virtual bool isBlackHoleEnd() const;
   virtual bool isFunctionalConstant() const;
   virtual bool isCloseSpaceFunctionalConstant(TargetUnitIterator prev) const;
   virtual bool isInhibitCapConstant() const;
   virtual void persistOut(ostream&);
   virtual void persistIn (istream&);
   virtual int wordClassCode() const;
   virtual int patNumber() const;
   virtual void generateDictionaryEntry(LgsString& prevWord);
   static bool isFunctionalConstantAddress(int opadr);
   virtual bool allowsPrecedingSpaces() const;
   virtual bool allowsTrailingSpaces() const;


   //---------------------------------------------------------------
   typedef enum
   {
      UnresolvedTarget = -99,
      NullAddress      = -140,
      InitialCapFlag   = -297,
      Qd               = -366,
      Qz               = -453,
      InitialCapFlag2  = -500,
      CloseSpaceLeft   = -607,
      GerAdjInflection = -800,
      CloseSpaceRight  = -864,
      LowercaseRight   = -865,
      BlackHoleQ1      = -867,
      BlackHoleQ2      = -868,
      BlackHoleQ3      = -869,
      BlackHoleQ4      = -870,
      BlackHoleZ1      = -871,
      BlackHoleZ2      = -872,
      BlackHoleZ3      = -873,
      BlackHoleZ4      = -874,
      GermanApostrophe = -876,
      InhibitFullCap   = -878,
      InvertedQuestion = -881
   } FunctionID;

protected:
   LgsString v_word;
};

#endif // __functionconstantsentenceunit_h__

