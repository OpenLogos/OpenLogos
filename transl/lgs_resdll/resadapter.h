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
#ifndef resadapter_h
#define resadapter_h

//-----------------------------------------------------------------
// File - resadapter.cpp
//
// Class - none.
//
//-------------------------------------------------------------------

#include <logos_libs/linguistic/sworkbase.h>
#include <logos_libs/multithreadlib/comminterface.h>

class ResAdapter
{
public:
   static ResAdapter* Create(LgsMessage* msg);

   ~ResAdapter(void);
   SWorkBaseVector* NextSentence();

   enum TranslationState
   {
      DoTranslate = 0,
      DoNotTranslate = 1
   };
   enum CaseState
   {
      UndeterminedCaseState = -1,
      LowerCase             =  0,
      BeginsUpperCase       =  1,
      AllUpperCase          =  2
   };

   int isTranslateable();
   int caseState();
   int sentencePosition();
   bool isBold() const;
   bool isItalic() const;
   bool isUnderlined() const;
   bool isSingleQuoted() const;
   bool isDoubleQuoted() const;
   void TestPersistence();

protected:
   ResAdapter(void);

private:
   int isTranslateable_;
   int caseState_;
   int sentencePosition_;
   bool bold_;
   bool italic_;
   bool underlined_;
   bool singleQuoted_;
   bool doubleQuoted_;
};
//-------------------------------------------------------------------
inline int ResAdapter::isTranslateable()
{
   return isTranslateable_;
}
//-------------------------------------------------------------------
inline int ResAdapter::caseState()
{
   return caseState_;
}
//-------------------------------------------------------------------
inline int ResAdapter::sentencePosition()
{
   return sentencePosition_;
}
//---------------------------------------------------------------------
inline bool ResAdapter::isBold() const
{
   return bold_;
}
//---------------------------------------------------------------------
inline bool ResAdapter::isItalic() const
{
   return italic_;
}
//---------------------------------------------------------------------
inline bool ResAdapter::isUnderlined() const
{
   return underlined_;
}
//---------------------------------------------------------------------
inline bool ResAdapter::isSingleQuoted() const
{
   return singleQuoted_;
}
//---------------------------------------------------------------------
inline bool ResAdapter::isDoubleQuoted() const
{
   return doubleQuoted_;
}

#endif // resadapter_h


