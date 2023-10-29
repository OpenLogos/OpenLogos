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
// property.h: interface for the Property class.
// This represents a Property with a name and associated value. Property is used to 
// represent a parameter for Translation like SourceLanguage. It is also used to represent 
// a return value from the Translation Server like the full path for the target file 
// (Diagnostics or Output Text).
//
// Copyright (C) 2000 Logos Corporation
////////////////////////////////////////////////////////////////////////////////////////////

#ifndef _PROPERTY_H_
#define _PROPERTY_H_

#include <engine_api/xlationinterface/xlationinterfacedefs.h>

class Property
{
public:
    // Constructor
    Property(const UString& name, const UString& value);
    Property(const Property& copyValue);
    // Function to return the name of the Property. 
    const wchar_t* name(void) const;
    // Function to return the value for the Property. 
    const wchar_t* value(void) const;
    // Function to set the value for the Property. 
    void setValue(const UString& value);
    // Comparison operators
    bool operator!=(const Property& rhs) const;
    bool operator==(const Property& rhs) const;
    // Assignment operator
    const Property& operator=(const Property& rhs);
private:
    UString m_name; // Name for the property
    UString m_value; // Value for the property
};

inline Property::Property(const UString& name, const UString& value) :
        m_name(name), m_value(value)
{

}

inline Property::Property(const Property& copyValue) : 
            m_name(copyValue.m_name), m_value(copyValue.m_value)
{

}

inline const wchar_t* Property::name(void) const
{
    return m_name.c_str();
}

inline const wchar_t* Property::value(void) const
{
    return m_value.c_str();
}

inline void Property::setValue(const UString& value)
{
    m_value = value;
}

inline bool Property::operator!=(const Property& rhs) const
{
    if (!wcscmp(rhs.name(), name()))
    {
        return false;
    }
    return true;
}

inline bool Property::operator==(const Property& rhs) const
{
    return !(rhs != *this);
}

inline const Property& Property::operator=(const Property& rhs)
{
    m_name = rhs.m_name;
    m_value = rhs.m_value;
    return *this;
}

#endif
