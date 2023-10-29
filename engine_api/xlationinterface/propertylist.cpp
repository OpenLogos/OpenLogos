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
// propertylist.cpp: implementation of the PropertyList class.
//
//////////////////////////////////////////////////////////////////////

#include <engine_api/xlationinterface/propertylist.h>

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

PropertyList::PropertyList()
{

}

PropertyList::~PropertyList()
{
    m_properties.clear();
}

void PropertyList::setPropertyValue(const UString& name, const UString& value)
{
    Property searchValue(name, value);
    PropertyIterator foundValue = m_properties.begin();
    PropertyIterator endIter = m_properties.end();
    for (;foundValue != endIter && (*foundValue) != searchValue ; foundValue++);
    if (foundValue != endIter)
    {
        (*foundValue).setValue(value);
    }
    else
    {
        m_properties.push_back(searchValue);
    }
}

const wchar_t* PropertyList::getPropertyValue(const UString& name) const
{
    static UString emptyString;
    Property searchValue(name, L"");
    ConstPropertyIterator foundValue = m_properties.begin();
    ConstPropertyIterator endIter = m_properties.end();
    for (;foundValue != endIter && (*foundValue) != searchValue ; foundValue++);
    if (foundValue != endIter)
    {
        return (*foundValue).value();
    }
    return emptyString.c_str();
}

void PropertyList::removeProperty(const UString& name)
{
    Property searchValue(name, L"");
    PropertyIterator foundValue = m_properties.begin();
    PropertyIterator endIter = m_properties.end();
    for (;foundValue != endIter && (*foundValue) != searchValue ; foundValue++);
    if (foundValue != endIter)
    {
        m_properties.erase(foundValue);
    }
}

void PropertyList::clearAll(void)
{
    m_properties.clear();    
}

ParameterIterator PropertyList::getIterator(void) const
{
    ParameterIterator paramIterator(m_properties);
    return paramIterator;
}

ParameterIterator::ParameterIterator(const PropertyVector & propertyList) :
                     m_properties(propertyList), m_propertyIterator(m_properties.begin())
{

}

const Property& ParameterIterator::operator++(int)
{
    ConstPropertyIterator retValue = m_propertyIterator;
    m_propertyIterator++;
    return (*retValue);    
}

void ParameterIterator::reset(void)
{
    m_propertyIterator = m_properties.begin();
}

bool ParameterIterator::hasMoreElements(void)
{
    return (m_propertyIterator != m_properties.end());
}
