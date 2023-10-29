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
#ifndef __languagebuilder_h__
#define __languagebuilder_h__

//-------------------------------------------------------------------
// File - languagebuilder.h
//
// Class - LanguageBuilder
//
// Description - This is a part of an application of the "Builder"
//      pattern.
//
// Patterns
//      Builder
//
//-------------------------------------------------------------------

class LLanguage;
class LinguisticFactory;

class LanguageBuilder
{
public:
   LanguageBuilder();
   virtual ~LanguageBuilder();

   LLanguage* CreateSource (int langCode, LinguisticFactory&) const;
   LLanguage* CreateTarget (int langCode, LinguisticFactory&,
                            const LgsString& elisionFileName) const;

protected:
   LLanguage* CreateLanguage(int langCode) const;
   virtual void BuildSourceSpecifics(LLanguage*, LinguisticFactory&) const = 0;
   virtual void BuildTargetSpecifics(LLanguage*, LinguisticFactory&,
                                     const LgsString& elisionFileName) const = 0;
};

//---------------------------------------------------------------------
inline LLanguage* LanguageBuilder::CreateSource(int langCode, LinguisticFactory& f) const
{
   LLanguage* l = CreateLanguage(langCode);
   if (l)
   {
      BuildSourceSpecifics(l, f);
   }
   return l;
}
//---------------------------------------------------------------------
inline LLanguage* LanguageBuilder::CreateTarget(int langCode, LinguisticFactory& f,
                                                const LgsString& elisionFileName) const
{
   LLanguage* l = CreateLanguage(langCode);
   if (l)
   {
      BuildTargetSpecifics(l, f, elisionFileName);
   }
   return l;
}

#endif // __LanguageBuilder_h__

