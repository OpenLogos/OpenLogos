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
// --------------------------------------------------------------------------
// File: ProperName.cpp (implementation)
// --------------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/PatternRecognition/ProperName.h>

// --------------------------------------------------------------------------
// Construct a default object - necessary for STL
// --------------------------------------------------------------------------
ProperName::ProperName() 
{
	position_=0;
}


// --------------------------------------------------------------------------
// Construct the object based on the given information
// --------------------------------------------------------------------------
ProperName::ProperName(ProperNamePattern* thePattern,		// set with this general pattern
					   LgsString element,						// set the first element of the match pattern with this element
					   int ssuNumber,						// position of this element in the SSU vector
					   LgsString surfaceForm)					// the surface form of the SSU itself
{
	// set the general pattern
	pattern_=*thePattern;								// set the pattern to use to form the proper name
	position_=1;										// start with the first element of that pattern

	// set the matched pattern
	properNameConstituents_.push_back(element);
	sourceSentenceUnitNumbers_.push_back(ssuNumber);
	sourceSurfaceExpressions_.push_back(surfaceForm);
}


// --------------------------------------------------------------------------
// Destructor
// --------------------------------------------------------------------------
ProperName::~ProperName() 
{
}


// --------------------------------------------------------------------------
// Display this object
// --------------------------------------------------------------------------
void ProperName::display() 
{
	cout << prettyPrint() << endl;
}
// --------------------------------------------------------------------------
LgsString ProperName::prettyPrint() 
{
	LgsString info="";

	// construct info
	LgsString surfaceForm="";
	LgsString detailedInfo="";
	LgsStringIterator ssu=sourceSurfaceExpressions_.begin();
	LgsStringIterator constituent=properNameConstituents_.begin();
	while (constituent!=properNameConstituents_.end() && ssu!=sourceSurfaceExpressions_.end())
	{
		surfaceForm+= " " + (*ssu);
		//detailedInfo+= " " + form + " (" + (*constituent) + "/" + (*ssuNumber) + ")";
		detailedInfo+= " " + (*ssu) + " (" + (*constituent) + ")";
		ssu++;
		constituent++;
	}

	// display the proper name in its source surface form
	info+= "Recognized Proper Name:" + surfaceForm + "\n";

	// display the pattern (rule) that has been used to match this proper name
	info+= "\tpattern used: " + pattern_.description() + "\n";

	// display the actual proper name with detailed info
	info+= "\tmatched pattern:" + detailedInfo + "\n";

	return info;
}


// --------------------------------------------------------------------------
// Return the current element of the pattern.
// --------------------------------------------------------------------------
LgsString ProperName::currentElement() 
{
	return pattern_.element(position_);
}


// --------------------------------------------------------------------------
// Return the next to current element of the pattern.
// --------------------------------------------------------------------------
LgsString ProperName::nextElement() 
{
	return pattern_.element(position_+1);
}


// --------------------------------------------------------------------------
// Update the matched pattern with this element and its corresponding position
// in the SSU vector.
// If the current constituent in the pattern has a + that corresponds to this element
// then add another instance of that constituent.
// --------------------------------------------------------------------------
void ProperName::update(LgsString element,						// set the first element of the match pattern with this element
						int ssuNumber,						// position of this element in the SSU vector
						LgsString surfaceForm)					// the surface form of the SSU itself
{
	properNameConstituents_.push_back(element);
	sourceSentenceUnitNumbers_.push_back(ssuNumber);
	sourceSurfaceExpressions_.push_back(surfaceForm);
}


// --------------------------------------------------------------------------
// Current element to be matched in the pattern.
// --------------------------------------------------------------------------
void ProperName::moveToNextElement() 
{
	position_++;
}


// --------------------------------------------------------------------------
// Return whether the current element to match in the pattern accepts one or
// more instances of the constituent.
// --------------------------------------------------------------------------
bool ProperName::currentElementIsOneOrMore() 
{
	return pattern_.oneOrMore(position_);
}


// --------------------------------------------------------------------------
// Return whether the pattern has been completed (matched completely).
// --------------------------------------------------------------------------
bool ProperName::completed()
{
	return (pattern_.numberOfElements()==position_);
}


// --------------------------------------------------------------------------
// Return the number of elements in the pattern
// --------------------------------------------------------------------------
int ProperName::numberOfElements() 
{
	return pattern_.numberOfElements();
}


// --------------------------------------------------------------------------
// Reset this object.
// --------------------------------------------------------------------------
void ProperName::clear()
{
	pattern_.clear();
	position_=0;
	sourceSentenceUnitNumbers_.clear();
	properNameConstituents_.clear();
	sourceSurfaceExpressions_.clear();
}


// --------------------------------------------------------------------------
// Set this object from the content of the given object
// --------------------------------------------------------------------------
void ProperName::setTo(ProperName& anotherProperName)
{
	pattern_=anotherProperName.pattern_;
	position_=anotherProperName.position_;
	sourceSentenceUnitNumbers_=anotherProperName.sourceSentenceUnitNumbers_;
	properNameConstituents_=anotherProperName.properNameConstituents_;
	sourceSurfaceExpressions_=anotherProperName.sourceSurfaceExpressions_;
}


// --------------------------------------------------------------------------
// Return whether this object has been set
// --------------------------------------------------------------------------
bool ProperName::empty()
{
	return pattern_.empty();
}


// --------------------------------------------------------------------------
// Destructor
// --------------------------------------------------------------------------
ProperNameVector::~ProperNameVector() 
{
}


// --------------------------------------------------------------------------
// Display the list of proper names
// --------------------------------------------------------------------------
void ProperNameVector::display() 
{
	ProperNameVector::iterator i;

	cout << "*** PROPER NAME VECTOR ***" << endl;
	for (i=begin(); i!=end(); i++) 
	{
		(*i).display();
	}
}


