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
// File: ProperName.h (interface)
// Purpose: define a proper name.
// --------------------------------------------------------------------------

#ifndef _ProperName_h
#define _ProperName_h

#include <logos_libs/PatternRecognition/ProperNamePattern.h>

// --------------------------------------------------------------------------
// Interface for class ProperName
// --------------------------------------------------------------------------
class ProperName 
{
public:
	ProperName();
	ProperName(ProperNamePattern*,LgsString,int,LgsString);
	virtual ~ProperName();
	void display();
	LgsString prettyPrint();
	bool completed();									// whether the pattern has been completely matched (fired)
	LgsString currentElement();
	LgsString nextElement();
	void update(LgsString,int,LgsString);
	void moveToNextElement();
	bool currentElementIsOneOrMore();
	void clear();
	int numberOfElements();
	void setTo(ProperName&);
	bool empty();
	LgsVector(int) sourceSentenceUnitNumbers() {return sourceSentenceUnitNumbers_;}
	LgsStringVector properNameConstituents() {return properNameConstituents_;}

private:
	ProperNamePattern pattern_;						// the pattern (rule) used to match this proper name
	int position_;									// current position in reading the pattern
	LgsVector(int) sourceSentenceUnitNumbers_;			// corresponding SSU numbers from the SSU vector
	LgsStringVector properNameConstituents_;			// sequence of constituents forming this proper name
	LgsStringVector sourceSurfaceExpressions_;
};


// --------------------------------------------------------------------------
// Interface for class ProperNameVector
// --------------------------------------------------------------------------
class ProperNameVector : public LgsVector(ProperName)
{
public:
	ProperNameVector() {}
	virtual ~ProperNameVector();
	void append(ProperName properName) {push_back(properName);}
	void display();
};

typedef ProperNameVector::iterator ProperNameVectorIterator;

#endif
