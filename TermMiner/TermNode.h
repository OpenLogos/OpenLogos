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
// TermNode.h: interface for the TermNode class.
//
//////////////////////////////////////////////////////////////////////

#ifndef _TermNode_h_
#define _TermNode_h_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <TermMiner/Term.h>

class TermSearchStatistic;

// --------------------------------------------------------------------------
// Definition of a node containing related Term objects to be stored as a single node
// in a binary tree data structure. In a node, all terms are the same, meaning that
// they have the same source canonical form only.
// --------------------------------------------------------------------------
class TermNode : public Object
{
public:
	TermNode(LgsString);											// create this node given the key (label)
	virtual ~TermNode();
	LgsString key();												// return the node label (key)
	void append(Term*,TermSearchStatistic&);
	TermNode* leftNode();											// return pointer to the left descendant
	TermNode* rightNode();											// return pointer to the right descendant
	void createLeftChild(Term*,LgsString,TermSearchStatistic&);		// create a left descendant given a Term and a key
	void createRightChild(Term*,LgsString,TermSearchStatistic&);	// create a right descendant given a Term and a key
	void report(ostream&);											// report the node in specified output stream
	int numberOccurrences();										// return number of occurrences at this node
	void incrementTotalNumberOccurrencesInDocument();
	int totalNumberOccurrencesInDocument();

private:
	LgsString key_;								// key that defines the relationship among Terms stored at this node
	TermVector terms_;							// information stored at this node (a set of related Term objects)
	TermNode* leftNode_;						// points to the left descendant (if any, else to null)
	TermNode* rightNode_;						// points to the right descendant (if any, else to null)
	int totalNumberOccurrencesInDocument_;
};

// --------------------------------------------------------------------------
// Return the node label (key).
// --------------------------------------------------------------------------
inline LgsString TermNode::key()
{
	return key_;
}

// --------------------------------------------------------------------------
// Return pointer to the left descendant.
// --------------------------------------------------------------------------
inline TermNode* TermNode::leftNode()
{
	return leftNode_;
}

// --------------------------------------------------------------------------
// Return pointer to the right descendant.
// --------------------------------------------------------------------------
inline TermNode* TermNode::rightNode()
{
	return rightNode_;
}

// --------------------------------------------------------------------------
// Return number of occurrences at this node.
// --------------------------------------------------------------------------
inline int TermNode::numberOccurrences()
{
	return terms_.size();
}

// --------------------------------------------------------------------------
inline void TermNode::incrementTotalNumberOccurrencesInDocument()
{
	totalNumberOccurrencesInDocument_++;
}

// --------------------------------------------------------------------------
inline int TermNode::totalNumberOccurrencesInDocument()
{
	return totalNumberOccurrencesInDocument_;
}

#endif // _TermNode_h_
