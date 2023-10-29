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
#ifndef __DDerivedForm_h__
#define __DDerivedForm_h__

//-------------------------------------------------------------------
// File - DDerivedForm.h
//
// Class - DDerivedForm
//
//-------------------------------------------------------------------

#include <logos_libs/entity/dobject.h>

class DDerivedForm: public DObject
{
public:
    //---------------------------------------------------------------
    // The public constructors and destructors. The default
    // constructor is typically made public unless class construction
    // is being limited (in which case it is made protected. The
    // destructor is declared virtual to allow subclassing.
    //---------------------------------------------------------------
                 DDerivedForm();
                 DDerivedForm( const DDerivedForm& );
        virtual ~DDerivedForm();

    //---------------------------------------------------------------
    // These are "getter" member functions. They allow read access
        // to the internal data of the class. There is no real behavior
        // in these functions but they are important for use in more
        // complex member functions (these member functions can be made
        // "protected" if we do not want to export this data).
        //
        // These functions are typically defined inline because they are
        // usually only one statement in length. If the compiler leaves
        // them inline they should have absolutely not overhead.
    //---------------------------------------------------------------
    const LgsString& DerivedFormID  () const;
    int           LanguageCode   () const;
    const LgsString& PatType        () const;
    int           PatNumber      () const;
    int           StemNumber     () const;
    const LgsString& Ending         () const;
    int           WordClassCode  () const;
    int           FormCode       () const;
//---------------------------------------------------------------
    // These are "setter" member functions. They allow write access
        // to the internal data of the class. Again, there is no real
        // behavior here. These functions do perform more abstract
        // behavior in that the not only update a member variable but
        // they set the object to a "dirty" state, indicating that it
        // needs to be saved to the database, etc.,.
    //
        // Since they perform non-simple operations they are not defined
        // as inline.
    //---------------------------------------------------------------
    void SetDerivedFormID    ( const LgsString& );
    void SetLanguageCode     ( int );
    void SetPatType          ( const LgsString& );
    void SetPatNumber        ( const int      );
    void SetStemNumber       ( const int      );
    void SetEnding           ( const LgsString&      );
    void SetWordClassCode    ( int );
    void SetFormCode         ( int );
 //---------------------------------------------------------------
    // Each subclass of DObject should overload the Compare()
        // member function. All the relational operators are written in
        // terms of this member and are inherited from DObject.
        //
        // This member returns a 0 if the object is equal to the
        // the arguement, a -1 if the object is less thatn the arguement,
        // and a +1 if it is greater than the arguement.If a class does
        // not overload this member the one defined in DObject will
        // be called and will return a 0 (meaning the objects will always
        // appear to be equal to each other).
        //---------------------------------------------------------------

        virtual int Compare( const DObject& ) const;

        //---------------------------------------------------------------
        // The assignment operator. This needs to be overloaded in each
        // class. The compiler supplies a default one that performs a
        // shallow copy.
        //---------------------------------------------------------------

        //---------------------------------------------------------------

        const DDerivedForm& operator=( const DDerivedForm& );

private:
    //---------------------------------------------------------------
    // The member variables. These are private because all access to
        // them, including access by member functions of this class or
        // derived class should be through the "getter" and "setter"
        // member functions.
    //---------------------------------------------------------------

        LgsString v_derivedFormID     ;
        int    v_languageCode;
        LgsString v_patType     ;
        int    v_patNumber    ;
        int    v_stemNumber    ;
        LgsString v_ending;
        int    v_wordClassCode;
        int    v_formCode;
};

// typedef LgsVector(DDerivedForm) DDerivedFormVector;

class DerivedFormKey {
public:
	int v_patNumber;
	int v_stemNumber;
	char v_ending[6];
	DerivedFormKey(int pn, int vn, const char *e);
};

extern bool operator<(DerivedFormKey,DerivedFormKey);

typedef LgsMap(DerivedFormKey,DDerivedForm) DDerivedFormMap;

//-------------------------------------------------------------------
inline const LgsString& DDerivedForm::DerivedFormID() const
{
        return v_derivedFormID;
}
//-------------------------------------------------------------------
inline int DDerivedForm::LanguageCode() const
{
        return v_languageCode;
}
//-------------------------------------------------------------------
inline const LgsString& DDerivedForm::PatType() const
{
        return v_patType;
}
//-------------------------------------------------------------------
inline int DDerivedForm::PatNumber() const
{
        return v_patNumber;
}
//-------------------------------------------------------------------
inline int DDerivedForm::StemNumber() const
{
        return v_stemNumber;
}
//-------------------------------------------------------------------
inline const LgsString& DDerivedForm::Ending() const
{
        return v_ending;
}
//-------------------------------------------------------------------
inline int DDerivedForm::WordClassCode() const
{
        return v_wordClassCode;
}
//-------------------------------------------------------------------
inline int
DDerivedForm::FormCode() const
{
        return v_formCode;
}

#endif // DDerivedForm

