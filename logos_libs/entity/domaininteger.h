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
#ifndef __domaininteger_h__
#define __domaininteger_h__

//-------------------------------------------------------------------
// File - domaininteger.h
//
// Class - DomainInteger
//
//-------------------------------------------------------------------

class DomainInteger
{
public:
    //---------------------------------------------------------------
    // The public constructors and destructors. The default
    // constructor is typically made public unless class construction
    // is being limited (in which case it is made protected. The
    // destructor is declared virtual to allow subclassing.
    //---------------------------------------------------------------
                 DomainInteger( int minimum, int maximum, int defaultValue );
    virtual ~DomainInteger();

    int Default() const;
    int Minimum() const;
    int Maximum() const;

    bool IsValid  ( int ) const;
    bool IsDefault( int ) const;

private:
    //---------------------------------------------------------------
    // The member variables. These are private because all access to
        // them, including access by member functions of this class or
        // derived class should be through the "getter" and "setter"
        // member functions.
    //---------------------------------------------------------------

    int v_default;
    int v_minimum;
    int v_maximum;

};

//-------------------------------------------------------------------
inline int DomainInteger::Default() const
{
        return v_default;
}
//-------------------------------------------------------------------
inline int DomainInteger::Minimum() const
{
        return v_minimum;
}
//-------------------------------------------------------------------
inline int DomainInteger::Maximum() const
{
        return v_maximum;
}
//-------------------------------------------------------------------
inline bool DomainInteger::IsValid  ( int n ) const
{
    return ( (n >= Minimum()) && (n <= Maximum()) );
}
//-------------------------------------------------------------------
inline bool DomainInteger::IsDefault( int n ) const
{
    return (n == Default());
}

#endif // __domaininteger_h__

