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
#ifndef __LUntranslatedSentence_h__
#define __LUntranslatedSentence_h__

//-------------------------------------------------------------------
// File - LSentence.h
//
// Class - LSentence (interface)
//
// Description - the primary object of the Gerdem subsystem. An object
// of this class is created using the "Create()" static member
// function. The result of the "Create()" function is a LSentence
// object that is complete with all its GDictionaryEntry objects and
// their respective GSemantoSyntacticUnits. In fact, the LSentence
// object is ready for the RES subsystem.
//
//-------------------------------------------------------------------

#include <logos_libs/linguistic/lsentence.h>

class LUntranslatedSentence: public LSentence
{
public:

    DummyLess(LUntranslatedSentence);
    DummyEqual(LUntranslatedSentence);

    //---------------------------------------------------------------
    // The public constructors and destructors. The destructor is
    // declared virtual to allow subclassing.
    //---------------------------------------------------------------

//             LUntranslatedSentence();
//           LUntranslatedSentence( LDictionary& dictionary );
             LUntranslatedSentence( LSentence& );
//             LUntranslatedSentence(const LUntranslatedSentence&);
    virtual ~LUntranslatedSentence();

    //---------------------------------------------------------------
    // The number of words or phrases, either matched or unmatched
    // within this sentence.
    //---------------------------------------------------------------

    virtual int translatedWordCount() const;
    virtual bool isTranslated() const;

    //---------------------------------------------------------------
    //---------------------------------------------------------------

    virtual void makeTargetSentenceUnits();
    virtual void capitalize             ();

    //---------------------------------------------------------------
    // elide() - performs the finishing rules on this sentence.
    //---------------------------------------------------------------

    virtual void elide   ();

    //---------------------------------------------------------------
    // generateStems() - has each target unit look itself up in the
    //      database's pat tables and convert its text to the
    //      appropriate form based on gender, peson, etc.,.
    // inhibitCapConstant() -
    //---------------------------------------------------------------

    virtual void generateStems          ();
    virtual void inhibitCapConstant     ();

    //---------------------------------------------------------------
    //---------------------------------------------------------------

    virtual void makeAspirations  ();
    virtual void processBlackHoles();

    //---------------------------------------------------------------
    // removeExtraneousTargetUnits() -
    // setSurfaceExpressionFromDictionary() -
    //---------------------------------------------------------------

    virtual void removeExtraneousTargetUnits       ();

    //---------------------------------------------------------------
    //---------------------------------------------------------------

protected:
private:
};

#endif // __LUntranslatedSentence_h__

