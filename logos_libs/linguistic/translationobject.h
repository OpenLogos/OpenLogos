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
#ifndef __translationobject_h__
#define __translationobject_h__

//-------------------------------------------------------------------
// File - translationobject.h
//
// Class - TranslationObject
//
// Description - This is the "root" class for many of the classes
//      in the Gerdem subsystem.
//
//-------------------------------------------------------------------

#include <logos_libs/linguistic/lsyntaxunit.h>

class LLanguage;
class LDictionary;

class TranslationObject: public LSyntaxUnit
{
public:
   //---------------------------------------------------------------
   // Public constructors and destructors. There are no public constructors
   // since this is an abstract class. The constructors are "protected" because
   // only they are only called by constructors in subclasses. The following
   // destructor is virtual to support polymorphism.
   //---------------------------------------------------------------
   virtual ~TranslationObject();

   //---------------------------------------------------------------
   virtual void lookup() = 0;

   //---------------------------------------------------------------
   const LLanguage& sourceLanguage() const;
   const LLanguage& targetLanguage() const;

protected:
   //-----------------------------------------------------------------
   // The default constructor. It has protected access status in order to
   // prevent users from attempting to create an object of this class
   // (this class is intended to be abstract). Copy constructor is included
   // so that subclasses that support a copy constructor can use the copy
   // constructor of this class.
   //-----------------------------------------------------------------
   TranslationObject();
   TranslationObject(LDictionary& dictionary);
   TranslationObject(const TranslationObject&);

   //-----------------------------------------------------------------
   void persistOut(int * outData);
   void persistIn (int * inData);
};

//---------------------------------------------------------------------
inline void TranslationObject::persistOut(int * outData)
{
   LSyntaxUnit::persistOut(outData);
}
//-------------------------------------------------------------------
inline void TranslationObject::persistIn(int * inData)
{
   LSyntaxUnit::persistIn(inData);
}
#endif // __translationobject_h__
