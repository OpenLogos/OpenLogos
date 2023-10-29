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
#ifndef __DObject_h__
#define __DObject_h__

//-------------------------------------------------------------------
// File - DObject.h
//
// Class - DObject
//
// Description - This is the "root" class for many of the classes
//      in the Gerdata subsystem.
//
//-------------------------------------------------------------------

#include <logos_libs/utility/argumentparser.h>
#include <logos_libs/entity/domaininteger.h>

typedef LgsVector(Argument) ArgumentVector;

class DObject
{
public:
        //---------------------------------------------------------------
    // Public constructors and destructors. There are no public
        // constructors since this is an abstract class. The constructors
        // are "protected" because only they are only called by
        // constructors in subclasses. The following destructor is
        // virtual to support polymorphism.
        //---------------------------------------------------------------

        virtual ~DObject();

        const DObject& operator=( const DObject& );

        //---------------------------------------------------------------
    // The following are a full gamut of relational operators. These
        // member functions are all based upon the "Compare()" member
        // function that is virtual and should be overloaded in any
        // subclass where ordering is important (the "Compare()" member
        // function defaults to always returning a result of equality.
        //---------------------------------------------------------------

    virtual bool operator==( const DObject& ) const;
    virtual bool operator!=( const DObject& ) const;
    virtual bool operator< ( const DObject& ) const;
    virtual bool operator<=( const DObject& ) const;
    virtual bool operator> ( const DObject& ) const;
    virtual bool operator>=( const DObject& ) const;

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

        virtual int Compare ( const DObject& ) const;

        Argument*       Findarg( ArgumentVector& v_arguments, LgsString& s);

protected:
        //---------------------------------------------------------------
    // The default constructor. It has protected access status in
        // order to prevent users from attempting to create an object of
        // this class (this class is intended to be abstract). Copy
        // constructor is included so that subclasses that support a
        // copy constructor can use the copy constructor of this class.
        //---------------------------------------------------------------

    DObject();
    DObject( const DObject& );

    static const DomainInteger& IdDomain                ();
    static const DomainInteger& GenderCodeDomain        ();
    static const DomainInteger& SourceAnalysisCodeDomain();

private:
    //---------------------------------------------------------------
    // The member variables. These are private because all access to
        // them, including access by member functions of this class or
        // derived class should be through the "getter" and "setter"
        // member functions.
    //---------------------------------------------------------------

    static DomainInteger st_idDomain        ;
    static DomainInteger st_languageCodeDomain;
    static DomainInteger st_genderCodeDomain;
    static DomainInteger st_wordCountDomain;
    static DomainInteger st_sourceAnalysisCodeDomain;
};

//-------------------------------------------------------------------
inline const DomainInteger&
DObject::IdDomain()
{
    return st_idDomain;
}
//-------------------------------------------------------------------
inline const DomainInteger&
DObject::GenderCodeDomain()
{
    return st_genderCodeDomain;
}
//-------------------------------------------------------------------
inline const DomainInteger&
DObject::SourceAnalysisCodeDomain()
{
    return st_sourceAnalysisCodeDomain;
}

#endif // __DObject_h__

