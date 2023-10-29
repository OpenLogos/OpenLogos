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
#ifndef __punctuationprototypebuilder_h__
#define __punctuationprototypebuilder_h__

//-------------------------------------------------------------------
// File - punctuationprototypebuilder.h
//
// Class - PunctuationPrototypeBuilder (Abstract)
//
// Description - This is a part of an application of the "Builder"
//      pattern.
//
// Patterns
//      Builder
//
//-------------------------------------------------------------------

#include <logos_libs/linguistic/ldictionaryentry.h>
#include <logos_libs/linguistic/lsemantosyntacticunit.h>
#include <logos_libs/gerdem/patternprototypemap.h>

class PunctuationPrototypeBuilder
{
public:
   //---------------------------------------------------------------
   // The destructor will perhaps do nothing for this class but it
   // needs to be declared virtual so that the delete of a pointer
   // to this abstract class will cause the subclass's destructor to
   // be executed.
   //---------------------------------------------------------------
   PunctuationPrototypeBuilder();
   virtual ~PunctuationPrototypeBuilder();

   //--------------------------------------------------------------
   // Builder - This is the primary member of class. It
   //    returns a new SubjectMatter object.
   //-----------------------------------------------------------------
   void Build(PatternPrototypeMap&) const;

protected:
   LDictionaryEntry* createPeriodEntry() const;
   LDictionaryEntry* createColonEntry() const;
//   LDictionaryEntry* createCommaEntry() const;
   LDictionaryEntry* createExclamationEntry() const;
   LDictionaryEntry* createQuestionEntry() const;
   LDictionaryEntry* createLeftParenEntry() const;
   LDictionaryEntry* createRightParenEntry() const;
   LDictionaryEntry* createLeftSquareEntry() const;
   LDictionaryEntry* createRightSquareEntry() const;
   LDictionaryEntry* createLeftBraceEntry() const;
   LDictionaryEntry* createRightBraceEntry() const;
   inline LDictionaryEntry* createCombinedEntry (const char * wordVal) const;
   LDictionaryEntry* createQuestionParenEntry() const;
   LDictionaryEntry* createQuestionSquareEntry() const;
   LDictionaryEntry* createQuestionCurlyEntry() const;
   LDictionaryEntry* createExclamationParenEntry() const;
   LDictionaryEntry* createExclamationSquareEntry() const;
   LDictionaryEntry* createExclamationCurlyEntry() const;
   LDictionaryEntry* createPeriodParenEntry() const;
   LDictionaryEntry* createPeriodSquareEntry() const;
   LDictionaryEntry* createPeriodCurlyEntry() const;

   LDictionaryEntry*  createDefaultEntry() const;

   void setDefaultWordPhrase(LSemantoSyntacticUnit*) const;
   void setDefaultMorphology(LSemantoSyntacticUnit*) const;
   void setDefaultMeaning(LSemantoSyntacticUnit*) const;
   void setLeftMeaning(LSemantoSyntacticUnit*) const;
   void setRightMeaning(LSemantoSyntacticUnit*) const;
};

#endif // __punctuationprototypebuilder_h__

