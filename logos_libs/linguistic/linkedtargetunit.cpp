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
// File - transfersentenceuit.cpp
//
// Class - LinkedTargetUnit (implementation)
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/linkedtargetunit.h>
#include <logos_libs/linguistic/linkexception.h>
#include <logos_libs/linguistic/sourcesentenceunit.h>
#include <logos_libs/translutility/translcommonobjects.h>

//-------------------------------------------------------------------
LinkedTargetUnit::LinkedTargetUnit()
                 :p_sourceSentenceUnit(0)
{
}
//-------------------------------------------------------------------
LinkedTargetUnit::LinkedTargetUnit(LDictionary& d)
                 :TargetSentenceUnit(d),
                  p_sourceSentenceUnit(0)
{
}
//-------------------------------------------------------------------
LinkedTargetUnit::LinkedTargetUnit(const LinkedTargetUnit& rhs)
                 :TargetSentenceUnit(rhs),
                  p_sourceSentenceUnit(rhs.p_sourceSentenceUnit)
{
}
//-------------------------------------------------------------------
LinkedTargetUnit::~LinkedTargetUnit()
{
}
//---------------------------------------------------------------------
void LinkedTargetUnit::capitalize(int offset)
{
   // This is an overloaded method. Besides doing the normal
   // capitalization, the objects of this class inquire their
   // respective source objects for their case state.
   if (SentenceUnit::AllUpperCase == sourceSentenceUnit()->caseState())
   {
      setCaseState(SentenceUnit::AllUpperCase);
   }
   TargetSentenceUnit::capitalize(offset);
}
//---------------------------------------------------------------------
void LinkedTargetUnit::stemGenerate()
{
   // This method provides default behavior for target units that
   // are linked to source units. The default surface expression is
   // based upon the source side. This is overloaded only in the
   // case where the linked unit has a real target definition.
   // That situation is handled in subclasses of this class.

   setSurfaceExpression( sourceSentenceUnit()->surfaceExpression() );
}
//---------------------------------------------------------------------
void LinkedTargetUnit::findAssociatedSourceUnit(SourceUnitVector& sourceUnits)
{
   // This is the cornerstone of this subclass of target sentence unit.
   // Unlike the parent class, this class is made up of objects that
   // must have a connection back to the source side. The source unit
   // is found by an agreement between this unit's source position and
   // the relative position of the unit on the source side. If a match
   // fails and exception is thrown (there should always be a source
   // unit.

   bool matched = false;

   for (SourceUnitIterator i = sourceUnits.begin(); i != sourceUnits.end(); i++)
   {
      if ((*i)->position() == sourceUnitPosition())
      {
         setSourceSentenceUnit((*i));
         setIsProperName((*i)->isProperName());
         setTrailingSpaces((*i)->trailingSpaces());
         setIsProtected((*i)->isProtected());
         checkForPhraseModification((*i));
         matched = true;
         break;
      }
   }
   if (!matched)
   {
      throw LinkException("No Source Unit Found");
   }
}
//---------------------------------------------------------------------
void LinkedTargetUnit::persistOut(ostream& stream)
{
   SentenceUnit::persistOut(stream);
}
//-------------------------------------------------------------------
void LinkedTargetUnit::persistIn(istream& stream)
{
   SentenceUnit::persistIn(stream);
}
//---------------------------------------------------------------------
void LinkedTargetUnit::setSurfaceExpressionFromDictionary()
{
   surfaceExpressionFromString(word());
}
//-------------------------------------------------------------------
const LgsString& LinkedTargetUnit::word() const
{
   // Again this supplies the default behavior for target units
   // that are linked to source units. The default is that they
   // use the word text from the source side. Only the subclasses
   // that have a dictionary match use their own version ov the
   // word.

   assert(p_sourceSentenceUnit);
   return p_sourceSentenceUnit->word();
}
//---------------------------------------------------------------------
int LinkedTargetUnit::precedingSpaces() const
{
   if (p_sourceSentenceUnit)
   {
      return p_sourceSentenceUnit->precedingSpaces();
   }
   return TargetSentenceUnit::precedingSpaces();
}
//-------------------------------------------------------------------
bool LinkedTargetUnit::allowsPrecedingSpaces() const
{
   LgsString& str = (LgsString&) surfaceExpressionAsString();
   if (TargetSentenceUnit::allowsPrecedingSpaces())
   {
      if ((precedingSpaces() == 0) && (LLanguage::PUNCTUATION == wordClassCode()))
      {
         return false;
      }
      else
      {
         return true;
      }

   }
   else
   {
      if ((precedingSpaces() > 0) &&
          ((LLanguage::PUNCTUATION == wordClassCode()) ||
           ((str.length() >= 1) && (str.find_first_of(".,;:?!%]})>/'\"", 0) == 0))))
      {
         return true;
      }
      else
      {
         return false;
      }
   }
}
//---------------------------------------------------------------------
bool LinkedTargetUnit::allowsTrailingSpaces() const
{
   if (!TargetSentenceUnit::allowsTrailingSpaces())
   {
      if (trailingSpaces() > 0)
      {
         return true;
      }
      else
      {
         return false;
      }
   }
   return true;
}
//---------------------------------------------------------------------
void LinkedTargetUnit::checkForPhraseModification(SourceSentenceUnit* unit)
{
   if (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::GermanID)
   {
      LWordVector& words = unit->surfaceWords();

      for (LWordIterator word = words.begin(); word != words.end(); word++)
      {
         short modifier = (*word).getModificationType();
         if ((modifier == LWord::REMOVE_NON_HEAD) || (modifier == LWord::REMOVE_HEAD))
         {
            setPhraseModifier(modifier);
            break;
         }
      }
   }
}
