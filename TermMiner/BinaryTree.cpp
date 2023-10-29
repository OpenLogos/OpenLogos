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
// BinaryTree.cpp: implementation of the BinaryTree class.
//
//////////////////////////////////////////////////////////////////////

#include <logos_include/logoscommon.h>
#include <TermMiner/BinaryTree.h>
#include <TermMiner/Term.h>
#include <TermMiner/TermNode.h>
#include <TermMiner/TermSearchStatistic.h>
//#include <lgs_db_io/lgsdbcommonobjects.h>

// --------------------------------------------------------------------------
// Default constructor
// --------------------------------------------------------------------------
BinaryTree::BinaryTree() :
root_(0)			// empty binary tree
{
	// set the maximum number of occurrences at each node of the tree
//	JobControlArguments& jobCntrlArgs = LgsDBCommonObjects::GetJobControlArguments();
//	maxOccurrences_ = jobCntrlArgs.WordSearchFoundLimit();
	maxOccurrences_ = 1;
}

// --------------------------------------------------------------------------
// Destructor
// --------------------------------------------------------------------------
BinaryTree::~BinaryTree()
{
	delete root_;
}

void BinaryTree::setMaxOccurrences(int max)
{
	maxOccurrences_ = max;
}


// ---------------------------------------------------------------------------
// Append the given Term object at the appropriate node in the tree, given a key.
// Search for the key in the tree (eg, search for a node with the same key). If
// no match, then create a new node.
// Pass to the search the maximum nuber of objects to be stored at the node. If the
// selected node cannot hold another object, then it will not be saved.
// ---------------------------------------------------------------------------
void BinaryTree::append(Term* aTerm, LgsString key, TermSearchStatistic& stat)
{
#ifdef __TERM_SEARCH_STATS__
	stat.incrementNumberInsertionsInTree();
	stat.incrementNumberAppendOperationsForInsertions();
#endif

	if (root_ == 0)  		// tree is empty, this Term object is then the root
	{
		if (maxOccurrences_ > 0)   // Create root only if number of objects is > 0
		{
			root_ = new TermNode(key);
			root_->append(aTerm,stat);
			root_->incrementTotalNumberOccurrencesInDocument();
		}
	}
	else 		// tree is not empty, search for the key, starting from root node
	{
		append(root_,aTerm,key,stat);
	}
}

// ---------------------------------------------------------------------------
// Append the given object in the appropriate node based on the given key.
// If the given node is not the appropriate node (key not matching), then try
// to append in either left of right node depending on the key value. If the
// child is empty, then create a new node with that event and that key.
// ---------------------------------------------------------------------------
void BinaryTree::append(TermNode* aNode, Term* aTerm, LgsString key, TermSearchStatistic& stat)
{
#ifdef __TERM_SEARCH_STATS__
	stat.incrementNumberAppendOperationsForInsertions();
#endif

	if (aNode->key() == key)	// this term belongs to this node
	{
		// save this term at this node only if it does not exceeds the user-specified maximum
		// number of objects to be stored at the node
		if (aNode->numberOccurrences() < maxOccurrences_)
		{
			// add it to the list of terms at this node
			aNode->append(aTerm,stat);							
		}

		// maintain the total number of occurences of this term seen in the document (independently
		// of being saved or not in this node)
		aNode->incrementTotalNumberOccurrencesInDocument();
	}
	else	// this term belongs to some (grand)child of this node
	{
		if (key < aNode->key())   		// it belongs to some left grand(child)
		{
			if (aNode->leftNode() == 0)		// there is no left child to this node
			{
				// then create a new left node with the key and the event
				aNode->createLeftChild(aTerm,key,stat);		
			}
			else // there is a left child
			{
				// then try to append the term in the left branch
				append(aNode->leftNode(),aTerm,key,stat);	
			}
		}
		else 	// it belongs to some right grand(child)
		{
			if (aNode->rightNode() == 0)	// there is no right child to this node
			{
				// then create a new right node with the key and the event
				aNode->createRightChild(aTerm,key,stat);		
			}
			else   			// there is a right child
			{
				// then try to append the term in the left branch
				append(aNode->rightNode(),aTerm,key,stat);	
			}
		}
	}
}

// ---------------------------------------------------------------------------
// Traverse the tree in order.
// Report each node only if it has at least min occurrences.
// ---------------------------------------------------------------------------
void BinaryTree::report(ostream& out, int min)
{
	reportAtNode(root_,out,min);		// start from top of binary tree
}

// ---------------------------------------------------------------------------
// Display the information at the given node in the following order: display
// the left child first, display the node, then display the right child. This
// is to keep the alphabetical order.
// Report each node only if it has at least min occurrences.
// ---------------------------------------------------------------------------
void BinaryTree::reportAtNode(TermNode* aNode, ostream& out, int min)
{
	if (aNode != 0)
	{
		// report all nodes at left of this node
		reportAtNode(aNode->leftNode(),out,min);

		// report this node only if it contains a user specified minimum number of term objects
		if (aNode->totalNumberOccurrencesInDocument() >= min)
		{
			aNode->report(out);
		}

		// report all nodes at right of this node
		reportAtNode(aNode->rightNode(),out,min);
   }
}
