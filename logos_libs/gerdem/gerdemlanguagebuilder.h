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
program. If not, write to Globalware AG, Hospitalstraße 6, D-99817 Eisenach.

Linux port modifications and additions by Bernd Kiefer, Walter Kasper,
Deutsches Forschungszentrum fuer kuenstliche Intelligenz (DFKI)
Stuhlsatzenhausweg 3, D-66123 Saarbruecken
*/
#ifndef __gerdemlanguagebuilder_h__
#define __gerdemlanguagebuilder_h__

//-------------------------------------------------------------------
// File - gerdemlanguagebuilder.h
//
// Class - GerdemLanguageBuilder
//
// Description - This is a part of an application of the "Builder"
//      pattern.
//
// Patterns
//      Builder
//
//-------------------------------------------------------------------

#include <logos_libs/linguistic/languagebuilder.h>
#include <logos_libs/linguistic/linflection.h>
#include <logos_libs/entity/dderivedform.h>
#include <logos_libs/linguistic/linguisticfactory.h>

class GerdemLanguageBuilder: public LanguageBuilder
{
public:
   //---------------------------------------------------------------
   // The destructor will perhaps do nothing for this class but it
   // needs to be declared virtual so that the delete of a pointer
   // to this abstract class will cause the subclass's destructor to
   // be executed.
   //---------------------------------------------------------------
   GerdemLanguageBuilder();
   virtual ~GerdemLanguageBuilder();

protected:
   virtual void BuildSourceSpecifics(LLanguage*, LinguisticFactory&) const;
   virtual void BuildTargetSpecifics(LLanguage*, LinguisticFactory&,
                                     const LgsString& elisionFileName) const;

//   DDerivedFormVector* BuildDerivedForms(LLanguage*, LinguisticFactory&) const;
   DDerivedFormMap * BuildDerivedFormsMap(LLanguage*, LinguisticFactory&) const;
   LInflectionVector*  BuildInflections(LLanguage*, LinguisticFactory&) const;
};

#endif // __gerdemlanguagebuilder_h__

