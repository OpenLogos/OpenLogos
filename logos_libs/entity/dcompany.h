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
#ifndef __DCompany_h__
#define __DCompany_h__

//-------------------------------------------------------------------
// File - DCompany.h
//
// Class - DCompany
//
//-------------------------------------------------------------------

#include <logos_libs/entity/dobject.h>

class DCompany: public DObject
{
public:
    //---------------------------------------------------------------
    // The public constructors and destructors. The default
    // constructor is typically made public unless class construction
    // is being limited (in which case it is made protected. The
    // destructor is declared virtual to allow subclassing.
    //---------------------------------------------------------------
    DCompany();
    DCompany( const DCompany& );
    virtual ~DCompany();

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
    const LgsString& CompanyCode      () const;
    const LgsString& Description      () const;
    bool          RestrictSwitch   () const;
    
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
    void SetCompanyCode      ( const LgsString& );
    void SetDescription      ( const LgsString&      );
    void SetRestrictSwitch   ( const bool      );

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

    const DCompany& operator=( const DCompany& );
	bool operator==(const DCompany& rhs) const;

private:
    //---------------------------------------------------------------
    // The member variables. These are private because all access to
    // them, including access by member functions of this class or
    // derived class should be through the "getter" and "setter"
    // member functions.
    //---------------------------------------------------------------

    LgsString v_companyCode;
    LgsString v_description     ;
    bool   v_restrictSwitch     ;
};

typedef LgsVector(DCompany) DCompanyVector;

//-------------------------------------------------------------------
inline const LgsString& DCompany::CompanyCode() const
{
    return v_companyCode;
}
//-------------------------------------------------------------------
inline const LgsString& DCompany::Description() const
{
    return v_description;
}
//-------------------------------------------------------------------
inline bool DCompany::RestrictSwitch() const
{
    return v_restrictSwitch;
}

#endif

