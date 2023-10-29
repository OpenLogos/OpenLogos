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
// File - InsertedSentenceUnit.cpp
//
// Class - InsertedSentenceUnit (implementation)
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/translutility/translcommonobjects.h>
#include <logos_libs/linguistic/insertedsentenceunit.h>

//-------------------------------------------------------------------
InsertedSentenceUnit::InsertedSentenceUnit()
{
}
//-------------------------------------------------------------------
InsertedSentenceUnit::InsertedSentenceUnit(LDictionary& dictionary)
                     :ConstantSentenceUnit(dictionary)
{
}
//-------------------------------------------------------------------
InsertedSentenceUnit::~InsertedSentenceUnit()
{
}
//---------------------------------------------------------------------
void InsertedSentenceUnit::capitalize(int offset)
{
   TargetSentenceUnit::capitalize(offset);
}
//---------------------------------------------------------------------
void
InsertedSentenceUnit::findAssociatedSourceUnit (SourceUnitVector&)
{
}
//---------------------------------------------------------------------
void InsertedSentenceUnit::persistOut(ostream& stream)
{
   ConstantSentenceUnit::persistOut(stream);
}
//-------------------------------------------------------------------
void InsertedSentenceUnit::persistIn(istream& stream)
{
   ConstantSentenceUnit::persistIn(stream);
}
//---------------------------------------------------------------------
void InsertedSentenceUnit::reconcileMarkup()
{
}
//---------------------------------------------------------------------
void InsertedSentenceUnit::completeGenerate()
{
   if (((TranslCommonObjects::GetTargetLanguage()->id() == LLanguage::SpanishID) && (opadr() == SpaOrdinalAbbrev)) ||
       ((TranslCommonObjects::GetTargetLanguage()->id() == LLanguage::ItalianID) && (opadr() == ItaOrdinalAbbrev)) ||
       ((TranslCommonObjects::GetTargetLanguage()->id() == LLanguage::FrenchID) && (opadr() == FreOrdinalAbbrev1)) ||
       ((TranslCommonObjects::GetTargetLanguage()->id() == LLanguage::FrenchID) && (opadr() == FreOrdinalAbbrev2)))
   {
      LgsString prefix;
      LgsString suffix;

      if (opadr() == SpaOrdinalAbbrev)
      {
         prefix = "<SUP><U>";
         suffix = "</U></SUP>";
      }
      else
      {
         prefix = "<SUP>";
         suffix = "</SUP>";
      }
      LgsString newString = prefix + surfaceExpressionAsString() + suffix;
      surfaceExpressionFromString(newString);
   }
}
