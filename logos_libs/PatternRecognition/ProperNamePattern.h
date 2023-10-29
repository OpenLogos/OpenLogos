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
// File: ProperNamePattern.h (interface)
// Purpose: define a particular pattern to recognize one set of proper names.
// --------------------------------------------------------------------------

#ifndef _ProperNamePattern_h
#define _ProperNamePattern_h

// --------------------------------------------------------------------------
// Interface for class ProperNamePattern
// --------------------------------------------------------------------------
class ProperNamePattern
{
public:
	ProperNamePattern();				// default constructor
	virtual ~ProperNamePattern();		// destructor
	void clear();						// reset this object
	LgsString description();				// return the description of this pattern (rule format)
	void append(LgsString);				// append a constituent to this pattern
	void setPatternNumber(int);
	LgsString element(int);
	bool oneOrMore(int);
	int numberOfElements();
	bool empty();

private:
	LgsStringVector pattern_;	// the constituents of this pattern
	LgsStringVector pat_;		// the constituents of this pattern w/o "+" sign
	LgsString patternNumber_;		// the pattern number (mainly for testing purposes)
	int position_;				// position of the current element in the pattern being matched against constituents
};


// --------------------------------------------------------------------------
// Interface for class ProperNamePatternVector
// --------------------------------------------------------------------------
class ProperNamePatternVector : public LgsVector(ProperNamePattern)
{
public:
	ProperNamePatternVector() {}
	virtual ~ProperNamePatternVector();
	void append(ProperNamePattern pattern) {push_back(pattern);}
	void display();
};

typedef ProperNamePatternVector::iterator ProperNamePatternVectorIterator;


#endif
