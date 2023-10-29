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
// BinaryTree.h: interface for the BinaryTree class.
//
//////////////////////////////////////////////////////////////////////

#ifndef _BinaryTree_h_
#define _BinaryTree_h_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

class Term;
class TermNode;
class TermSearchStatistic;

// --------------------------------------------------------------------------
// Definition of a binary tree data structure
// --------------------------------------------------------------------------
class BinaryTree : public Object
{
public:
	BinaryTree();
	virtual ~BinaryTree();

	// append the given Term object at the appropriate node in the tree, given a key
	void append(Term*,LgsString,TermSearchStatistic&);		

	// 
	void setMaxOccurrences(int);		

	// traverse the tree in order and send to stream each node
	void report(ostream&,int);		

private:
	TermNode* root_;			// root node
	int maxOccurrences_;		// maximum number of occurrences at each node

	// append the term starting at the given node, given the key
	void append(TermNode*,Term*,LgsString,TermSearchStatistic&);

	// report starting at given node
	void reportAtNode(TermNode*,ostream&,int);	

	// sort starting at given node
	void sortAtNode(TermNode*);						
};

#endif // _BinaryTree_h_
