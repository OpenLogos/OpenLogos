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
// File: SubjectMatterCodeNode.h (interface)
// Purpose: defines a node in the tree of subject matter codes.
// --------------------------------------------------------------------------

#ifndef _SubjectMatterCodeNode_h
#define _SubjectMatterCodeNode_h

#include <logos_libs/SubjectMatterCodes/SubjectMatterCode.h>

class SubjectMatterCodeNodeVector;

// --------------------------------------------------------------------------
// Interface for class SubjectMatterCodeNode
// --------------------------------------------------------------------------
class SubjectMatterCodeNode : public Object
{
public:
	SubjectMatterCodeNode();								// default constructor
	SubjectMatterCodeNode(SubjectMatterCode,int);	// constructor (SMC value and sequence order)
	virtual ~SubjectMatterCodeNode();					// destructor

	// getter methods
	SubjectMatterCode& subjectMatterCode() {return smc_;}
	int sequenceOrder() {return sequenceOrder_;}
	SubjectMatterCodeNodeVector* children() {return children_;}

	// add a child at the appropriate place among the children
	void addChild(SubjectMatterCodeNode*);

	bool hasChildren() {return children_!=0;}

	void display(LgsString);
	void displayChildren(LgsString);
    void resetIterator(void);
    SubjectMatterCode& operator++(int);
    bool isEnd(void) const;
    void appendSmcInfo(LgsString& smcTree);

private:
	SubjectMatterCode smc_;							// SMC value at this node
	int sequenceOrder_;								// its sequence order
	SubjectMatterCodeNodeVector* children_;	// all SMC children of this node (no children by default)
    bool iterationComplete_;
};


// --------------------------------------------------------------------------
// Interface for class SubjectMatterCodeNodeVectorVector, a vector of SMC node objects.
// --------------------------------------------------------------------------
class SubjectMatterCodeNodeVector : public LgsVector(SubjectMatterCodeNode*) 
{
public:
	SubjectMatterCodeNodeVector();				// default constructor
	virtual ~SubjectMatterCodeNodeVector();	// default destructor

	void append(SubjectMatterCodeNode*);		// add a child at the appropriate place among the children
	bool member(SubjectMatterCode&,int&);
	bool removeNode(SubjectMatterCode*);
	bool member(SubjectMatterCodeNode*);		// return whether the given node is present in this vector

	void display(LgsString);
    void resetIterator(void);
    SubjectMatterCode& operator++(int);
    bool isEnd(void) const;
    void appendSmcInfo(LgsString& smcTree);

private:
    iterator current_;
};

typedef SubjectMatterCodeNodeVector::iterator SubjectMatterCodeNodeVectorIterator;
typedef SubjectMatterCodeNodeVector::iterator SubjectMatterCodeNodeIterator;

#endif
