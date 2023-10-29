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
// File - functionconstantsentenceunit.cpp
//
// Class - FunctionConstantSentenceUnit (implementation)
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/functionconstantsentenceunit.h>
#include <logos_libs/linguistic/ldictionary.h>
#include <logos_libs/linguistic/llanguage.h>

//-------------------------------------------------------------------
FunctionConstantSentenceUnit::FunctionConstantSentenceUnit()
{
}
//-------------------------------------------------------------------
FunctionConstantSentenceUnit::FunctionConstantSentenceUnit(int opadr)
{
 switch (opadr)
   {
   case UnresolvedTarget:
      v_word = " @?";
      break;
   case NullAddress:
      v_word = " @NULL_ADR";
      break;
   case InitialCapFlag:
      v_word = " @INIT_CAP";
      break;
   case Qd:
      v_word = " @QD";
      break;
   case Qz:
      v_word = " @QZ";
      break;
   case InitialCapFlag2:
      v_word = " @INIT_CAP";
      break;
   case CloseSpaceLeft:
      v_word = "d";
      break;
   case GerAdjInflection:
      v_word = "";
      setTrailingSpaces(1);
      break;
   case CloseSpaceRight:
      v_word = " @CLS_SP";
      break;
   case LowercaseRight:
      v_word = " @LC_RGHT";
      break;
   case BlackHoleQ1:
      v_word = " @BH_Q1";
      break;
   case BlackHoleQ2:
      v_word = " @BH_Q2";
      break;
   case BlackHoleQ3:
      v_word = " @BH_Q3";
      break;
   case BlackHoleQ4:
      v_word = " @BH_Q4";
      break;
   case BlackHoleZ1:
      v_word = " @BH_Z1";
      break;
   case BlackHoleZ2:
      v_word = " @BH_Z2";
      break;
   case BlackHoleZ3:
      v_word = " @BH_Z3";
      break;
   case BlackHoleZ4:
      v_word = " @BH_Z4";
      break;
   case GermanApostrophe:
      v_word = "'";
      break;
   case InhibitFullCap:
      v_word = "";
      setTrailingSpaces(0);
      break;
   case InvertedQuestion:
      v_word = "ø";
      setTrailingSpaces(0);
      break;
   }
}
//-------------------------------------------------------------------
FunctionConstantSentenceUnit::FunctionConstantSentenceUnit(LDictionary& dictionary)
                             :TargetSentenceUnit(dictionary)
{
}
//-------------------------------------------------------------------
FunctionConstantSentenceUnit::FunctionConstantSentenceUnit(const FunctionConstantSentenceUnit& rhs)
                             :TargetSentenceUnit(rhs)
{
}
//-------------------------------------------------------------------
FunctionConstantSentenceUnit::~FunctionConstantSentenceUnit()
{
}
//-------------------------------------------------------------------
void FunctionConstantSentenceUnit::generateDictionaryEntry(LgsString& prevWord)
{
   if (opadr() == GermanApostrophe)
   {
      if (prevWord.length() > 0)
      {
         switch (prevWord.at(prevWord.length() - 1))
         {
         case 's':
         case 'x':
         case 'z':
         case 'S':
         case 'X':
         case 'Z':
         case 'ﬂ':
            v_word = '\'';
            break;
         default:
            v_word = 's';
            break;
         }
      }
   }
   // Word class comes in as a 21, must be set to 4.
   if ((opadr() == CloseSpaceLeft) || (opadr() == GerAdjInflection))
      setWordClassCode(4);
   TargetSentenceUnit::generateDictionaryEntry();
}
//-------------------------------------------------------------------
bool FunctionConstantSentenceUnit::isBlackHoleStart() const
{
   if (opadr() == BlackHoleQ2)
      return true;
   else
      return false;
}
//-------------------------------------------------------------------
bool FunctionConstantSentenceUnit::isBlackHoleEnd() const
{
   if (opadr() == BlackHoleQ3)
      return true;
   else
      return false;
}
//-------------------------------------------------------------------
bool FunctionConstantSentenceUnit::isFunctionalConstant() const
{
    return true;
}
//-------------------------------------------------------------------
bool FunctionConstantSentenceUnit::isCloseSpaceFunctionalConstant(TargetUnitIterator prev) const
{
   if ((v_word == " @CLS_SP") ||
       ((v_word == " @QD") && language().isRomanceLanguage() && ((*prev)->word() == " @QZ") &&
	    ((*prev)->surfaceExpressionAsString() != "~QZQD~")))
      return true;
   else
      return false;
}
//---------------------------------------------------------------------
void FunctionConstantSentenceUnit::reconcileMarkup()
{
}
//-------------------------------------------------------------------
void FunctionConstantSentenceUnit::setSurfaceExpressionFromDictionary()
{
    surfaceExpressionFromString ( word() );
}
//---------------------------------------------------------------------
void FunctionConstantSentenceUnit::generateStem()
{
   if ((opadr() == GerAdjInflection) || (opadr() == CloseSpaceLeft))
      dictionary().generateStem(this);
}
//---------------------------------------------------------------------
void FunctionConstantSentenceUnit::persistOut(ostream& stream)
{
   TargetSentenceUnit::persistOut(stream);
}
//-------------------------------------------------------------------
void FunctionConstantSentenceUnit::persistIn(istream& stream)
{
   TargetSentenceUnit::persistIn(stream);
}
//---------------------------------------------------------------------
const LgsString& FunctionConstantSentenceUnit::word() const
{
   return v_word;
}
//---------------------------------------------------------------------
bool FunctionConstantSentenceUnit::isInhibitCapConstant() const
{
   if (opadr() == InhibitFullCap)
      return true;
   else
      return TargetSentenceUnit::isInhibitCapConstant();
}
//---------------------------------------------------------------------
int FunctionConstantSentenceUnit::wordClassCode() const
{
   return TargetSentenceUnit::wordClassCode();
}
//---------------------------------------------------------------------
int FunctionConstantSentenceUnit::patNumber() const
{
   if ((opadr() == GerAdjInflection) || (opadr() == CloseSpaceLeft))
      return 90;
   else
      return TargetSentenceUnit::patNumber();
}
//---------------------------------------------------------------------
bool FunctionConstantSentenceUnit::isFunctionalConstantAddress(int opadr)
{
   switch (opadr)
   {
   case NullAddress:
   case InitialCapFlag:
   case Qd:
   case Qz:
   case InitialCapFlag2:
   case CloseSpaceLeft:
   case GerAdjInflection:
   case CloseSpaceRight:
   case LowercaseRight:
   case BlackHoleQ1:
   case BlackHoleQ2:
   case BlackHoleQ3:
   case BlackHoleQ4:
   case BlackHoleZ1:
   case BlackHoleZ2:
   case BlackHoleZ3:
   case BlackHoleZ4:
   case GermanApostrophe:
   case InhibitFullCap:
   case InvertedQuestion:
      return true;
      break;
   default:
      return false;
      break;
   }
}
//---------------------------------------------------------------------
bool FunctionConstantSentenceUnit::allowsPrecedingSpaces() const
{
   if (opadr() == GermanApostrophe)
   {
      return false;
   }
   else
   {
      return TargetSentenceUnit::allowsPrecedingSpaces();
   }
}
//---------------------------------------------------------------------
bool FunctionConstantSentenceUnit::allowsTrailingSpaces() const
{
   if (opadr() == InvertedQuestion)
   {
      return false;
   }
   else
   {
      return TargetSentenceUnit::allowsTrailingSpaces();
   }
}
