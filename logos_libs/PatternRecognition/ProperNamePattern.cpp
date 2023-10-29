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
// File: ProperNamePattern.cpp (implementation)
// --------------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/PatternRecognition/ProperNamePattern.h>

// --------------------------------------------------------------------------
// Construct a default object
// --------------------------------------------------------------------------
ProperNamePattern::ProperNamePattern() 
{
	clear();
}


// --------------------------------------------------------------------------
// Reset this object
// --------------------------------------------------------------------------
void ProperNamePattern::clear() 
{
	pattern_.clear();
	pat_.clear();
	patternNumber_="";
}


// --------------------------------------------------------------------------
// Return whether this object has been set
// --------------------------------------------------------------------------
bool ProperNamePattern::empty() 
{
	return pattern_.empty();
}


// --------------------------------------------------------------------------
// Destructor
// --------------------------------------------------------------------------
ProperNamePattern::~ProperNamePattern() 
{
}


// --------------------------------------------------------------------------
// Append a constituent to the pattern
// --------------------------------------------------------------------------
void ProperNamePattern::append(LgsString token) 
{
	pattern_.push_back(token);
	LgsString s = token;
	if(token[token.size()-1]=='+') {
			s = token.substr(0 ,token.size()-1);
	}
	pat_.push_back(s);
}


// --------------------------------------------------------------------------
// Set the number of this pattern
// --------------------------------------------------------------------------
void ProperNamePattern::setPatternNumber(int n) 
{
	char s[5];
	sprintf(s,"%d",n);
	patternNumber_=s;
}


// --------------------------------------------------------------------------
// Return the description of the pattern
// --------------------------------------------------------------------------
LgsString ProperNamePattern::description() 
{
	LgsString description="";
	description+="("+patternNumber_+") ";
	description+="properName <-";
	LgsStringIterator i;
	for (i=pattern_.begin(); i!=pattern_.end(); i++) 
	{
		description+=" "+(*i);
	}
	return description;
}


// --------------------------------------------------------------------------
// Return the nth element of the pattern.
// Remove the + if it exists
// --------------------------------------------------------------------------
LgsString ProperNamePattern::element(int n) {
/*
	LgsString theElement="";

	if (n<=pattern_.size()) 		// if n is not out of range
	{
		LgsStringIterator str=pattern_.begin();		// start with first element in sequence
		for (int i=0; i<n-1; i++) str++;				// go to element n
		theElement=*str;
		// remove the + sign if it exists (this sign means one or more of this symbol)
		if (theElement[theElement.size()-1]=='+') 
		{
			theElement=theElement.substr(0,theElement.size()-1);
		}
	}

	return theElement;
*/
	LgsString theElement;

	int sz = pattern_.size();
	if (n<=sz) { 		// if n is not out of range
		theElement = pat_.at(n-1);
	} else
		theElement = "";

	return theElement;

}


// --------------------------------------------------------------------------
// Return whether the current constituent has a + for one or more instances.
// --------------------------------------------------------------------------
bool ProperNamePattern::oneOrMore(int n)
{
	LgsString theElement="";
	bool hasFlag=false;

	if (n<=pattern_.size()) 		// if n is not out of range
	{
		LgsStringIterator str=pattern_.begin();		// start with first element in sequence
		for (int i=0; i<n-1; i++) str++;					// go to element n
		theElement=*str;
		if (theElement[theElement.size()-1]=='+')
		{
			hasFlag=true;
		}
	}

	return hasFlag;
}


// --------------------------------------------------------------------------
// Return the number of elements in the pattern
// --------------------------------------------------------------------------
int ProperNamePattern::numberOfElements() 
{
	return pattern_.size();
}


// --------------------------------------------------------------------------
// Desctructor
// --------------------------------------------------------------------------
ProperNamePatternVector::~ProperNamePatternVector() 
{
}


// --------------------------------------------------------------------------
// Display the list of patterns
// --------------------------------------------------------------------------
void ProperNamePatternVector::display() 
{
	cout << endl << "LIST OF PROPER NAME PATTERNS" << endl;

	ProperNamePatternVector::iterator i;

	if (empty()) 
	{
		cout << "no patterns" << endl;
	}
	else 
	{
		for (i=begin(); i!=end(); i++) 
		{
			cout << (*i).description() << endl;
		}
	}
}
