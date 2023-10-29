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
// propertylist.h: interface for the PropertyList class.
// This contains a list of properties. This class is used by the XlationSession to store 
// the properties required to perform an operation and also to return the values returned
// by the Translation Server.
//
// Copyright (C) 2000 Logos Corporation
//////////////////////////////////////////////////////////////////////

#ifndef _PROPERTYLIST_H_
#define _PROPERTYLIST_H_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <engine_api/xlationinterface/xlationinterfacedefs.h>
#include <engine_api/xlationinterface/property.h>
#include <vector>
using namespace std;

typedef vector<Property> PropertyVector;
typedef PropertyVector::iterator PropertyIterator;
typedef PropertyVector::const_iterator ConstPropertyIterator;

// An iterator class to iterate through the list of properties
class ParameterIterator
{
public:
   // Constructor
   ParameterIterator(const PropertyVector &propertyList);
   // Post increment operator; returns the current element and then increments 
   // to point to the next element
   const Property& operator++(int);
   // Set the iteration to start from the beginning of the list
   void reset(void);
   // Returns true if there are more elements to be iterated through
   bool hasMoreElements(void);

private:
   const PropertyVector &m_properties; // List of properties
   ConstPropertyIterator m_propertyIterator; // STL Iterator
};

class PropertyList  
{
public:
   // Constructor
   PropertyList();
   // Destructor
   virtual ~PropertyList();
   // Searches for a particular Property object using the name passed as a parameter 
   // and sets the value for the Property. If the Property object does not exist, a 
   // new Property object is created and inserted into the list.
   virtual void setPropertyValue(const UString& name, const UString& value);
   // Searches for a particular Property object using the name passed as a parameter 
   // and returns the value for the Property.
   virtual const wchar_t* getPropertyValue(const UString& name) const;
   // Searches for a particular Property object using the name passed as a parameter 
   // and removes it from the list.
   virtual void removeProperty(const UString& name);
   // Remove all properties from the list
   virtual void clearAll(void);
   // Returns an iterator object (to iterate through the list of properties)
   virtual ParameterIterator getIterator(void) const; 

private:
   PropertyVector m_properties; // List of properties
};
#endif // _PROPERTYLIST_H_
