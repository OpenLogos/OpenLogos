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
// File: SubjectMatterCodeTree.cpp (implementation)
// --------------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/SubjectMatterCodes/SubjectMatterCodeTree.h>
#include <logos_libs/SubjectMatterCodes/SubjectMatterCode.h>
#include <logos_libs/SubjectMatterCodes/SubjectMatterCodeNode.h>
#include <logos_libs/SubjectMatterCodes/SMCTreeQuery.h>
#include <logos_libs/odbcsql/globalsqlconnection.h>
#include <lgs_db_io/lgsdbcommonobjects.h>
#include <logos_libs/utility/stringutil.h>
#include <logos_libs/sql/sqlexception.h>

// --------------------------------------------------------------------------
// Default constructor. Set the tree reflecting the whole SMC tree structure
// and sequence order found in the database.
// --------------------------------------------------------------------------
SubjectMatterCodeTree::SubjectMatterCodeTree()
{
	// no SMC nodes by default (tree is empty)
	rootChildren_ = new SubjectMatterCodeNodeVector();

//printf("SMCTree %x ctor w/ SMCNodeVector %x\n", this, rootChildren_); 
//fflush(stdout);
}

// --------------------------------------------------------------------------
// Default destructor
// --------------------------------------------------------------------------
SubjectMatterCodeTree::~SubjectMatterCodeTree()
{
//printf("SMCTREE %x dtor\n", this); fflush(stdout);
	delete rootChildren_;
//printf("SMCTREE %x dtor out\n", this); fflush(stdout);
}

// --------------------------------------------------------------------------
// Display content of the complete SMC tree
// --------------------------------------------------------------------------
void SubjectMatterCodeTree::display()
{
	cout << endl;
	cout << "       +----------+" << endl;
	cout << "-------| SMC tree |-------" << endl;
	cout << "       +----------+" << endl;
	rootChildren_->display("");
	cout << "-----------------------------------" << endl;
}

// --------------------------------------------------------------------------
// Set this tree as the whole tree of SMCs as ordered by the user and contained
// in the database. The approach is to read the database and get all the nodes and
// their sequence numbers to reconstruct the tree.
// --------------------------------------------------------------------------
void SubjectMatterCodeTree::setAsCompleteTree(const LgsString& selectedTreeName)
{
	// get each pair <SMC value,sequence order> for the selected tree
	SqlConnection* databaseConnection = getSqlConnection();
	SMCTreeQuery databaseQuery;
	try 
	{
		// establish database connection
		databaseQuery.open(databaseConnection);
	}
	catch (SqlException& x) 
	{
		// catch any connection problems
		throw(x);
	}
	// query the database
	databaseQuery.execute(selectedTreeName);
	// extract info from query result
	LgsString smcValue = "";
	int sequenceOrder = 0;
	while (databaseQuery.fetch(smcValue, sequenceOrder))
	{
		// create a new SMC object from this value
		SubjectMatterCode aSMC(smcValue);
		// create a node to add in the SMC tree
		SubjectMatterCodeNode* aSMCNode = new SubjectMatterCodeNode(aSMC, sequenceOrder);
		// add this SMC node into the appropriate place in this SMC tree
		insertInto(aSMCNode, rootChildren_);
	}
	// clean up
	databaseQuery.close();
	freeSqlConnection(databaseConnection);
}

SubjectMatterCodeNode * 
SubjectMatterCodeTree::copySubtree(SubjectMatterCodeNode *src) {
	SubjectMatterCodeNodeVectorIterator i = src->children()->begin();
	SubjectMatterCodeNode *dst = new SubjectMatterCodeNode(
		src->subjectMatterCode(), src->sequenceOrder());
	while(i!=src->children()->end()) {
		SubjectMatterCodeNode *srckid = (*i);
		SubjectMatterCodeNode *dstkid = copySubtree(srckid);
		dst->children()->push_back(dstkid);
		i++;
	}
	return dst;
}

// --------------------------------------------------------------------------
// Set this tree as the sequence of user-selected SMC subtrees. The approach is
// to get the user-selection top nodes from the job arguments, to get the whole
// SMC tree (argument) from which the whole subtree for each top node is extracted.
// The order of the subject matters is assumed to be in descending priority.
// --------------------------------------------------------------------------
void SubjectMatterCodeTree::setAsUserSelectedSubtrees(SubjectMatterCodeTree* wholeTree)
{
	// get the list of subtree SMC roots selected by the user for this job
	LgsString SMClist = LgsDBCommonObjects::GetJobControlArguments().SubjectMatterCodes();
	LgsStringVector tokens;
	StringUtil::parseInto(SMClist, tokens);		// transform list of SMCs into separate tokens
	LgsStringIterator i;

	// construct the selected subtrees of SMCs
 	for (i = tokens.begin(); i != tokens.end(); i++)		// for each subtree root node, get the children nodes
	{
		SubjectMatterCode* smc = new SubjectMatterCode(*i);

		// find the subtree root node in the whole tree based on the given selected SMC.
		// If found, extract the whole subtree corresponding to this root from the whole tree
		// and put it into the set of user-selected subtrees
		SubjectMatterCodeNode* aSelectedSubtree;
		if (findInSiblings(smc, wholeTree->tree(), aSelectedSubtree))
		{
			SubjectMatterCodeNode* newSubtree = copySubtree(aSelectedSubtree);
			rootChildren_->push_back(newSubtree);
		}
		delete smc;
	}
}

// --------------------------------------------------------------------------
// Set this tree as the difference between the two given trees.
// Approach: for each node of tree1, if this is a top node of tree2, then do not
// include this node and any of its descendants to this tree, else include this
// node. Next, check its descendants, and so on.
// --------------------------------------------------------------------------
void SubjectMatterCodeTree::setAsDifference(SubjectMatterCodeTree* tree1, SubjectMatterCodeTree* tree2)
{
	selectNodes(tree1->tree(), tree2->tree());
}

// --------------------------------------------------------------------------
// Select the nodes from vector1 that are not in vector2. For each, store in vector,
// then select the children nodes in the same fashion.
// --------------------------------------------------------------------------
void SubjectMatterCodeTree::selectNodes(SubjectMatterCodeNodeVector* vector1,SubjectMatterCodeNodeVector* vector2)
{
	for (SubjectMatterCodeNodeIterator node = vector1->begin(); node != vector1->end(); node++)
	{
		//cout << "NODE: " << (*node)->subjectMatterCode().content() << endl;
		if (!vector2->member(*node))
		{
			//cout << "\tNOT IN VECTOR2" << endl;
			insertInto(*node, rootChildren_);
			selectNodes((*node)->children(), vector2);
		}
	}
}

// --------------------------------------------------------------------------
// Append the given SMC node in the correct place in the SMC tree. This is based on the
// SMC value (to determine its depth level xxx goes at level 1, xxxxxx goes at level 2, etc.)
// and on its order sequence value to put the SMC at the right order at the selected level.
// --------------------------------------------------------------------------
void SubjectMatterCodeTree::insertInto(SubjectMatterCodeNode* node, SubjectMatterCodeNodeVector* siblings) 
{
	// if there is no siblings then add this node to the siblings
	if (siblings->empty())
	{
		siblings->append(node);
	}
	// there are sibling(s)
	else 
	{
		// is this node to be included in the siblings? (only if both are at same level in the tree)
		// check first element of the siblings for its level and compare to node to be inserted
		if (node->subjectMatterCode().level() == (*(siblings->begin()))->subjectMatterCode().level())
		{
			// then store this node as one of the root node children
			siblings->append(node);
		}
		else
		{
			// else find the siblings to which this node belongs. That is, find its parent node in this
			// set of siblings, then try to insert the node into this parent's siblings.
			SubjectMatterCodeNodeVectorIterator i = siblings->begin();
			bool foundParent=false;
			while (!foundParent && (i != siblings->end()))	// search for parent node (if any)
			{
				if (node->subjectMatterCode().contentAtLevel((*i)->subjectMatterCode().level()) ==
                (*i)->subjectMatterCode().content())
				{
					foundParent = true;
				}
				else
				{
					i++;
				}
			}
			if (foundParent)		// found its parent node
			{
				insertInto(node, (*i)->children());
			}
		}
	}
}

// --------------------------------------------------------------------------
// Extract the whole subtree from the given tree, where the root of the subtree
// is the node containing the given SMC value.
// Recursively find the top node among the siblings corresponding to the given SMC.
// Recursively go to the siblings with the same level as a the node to find, then
// find the node among the siblings.
// Arguments:
// Find the top node (node) among the siblings (siblings) corresponding to the SMC (smc).
// Return whether a subtree has been found corresponding to this SMC.
// --------------------------------------------------------------------------
bool SubjectMatterCodeTree::findInSiblings(SubjectMatterCode* smc,
                                           SubjectMatterCodeNodeVector* siblings,
                                           SubjectMatterCodeNode*& node)
{
	bool foundNode = false;

	// if there is no siblings then cannot find the wanted top node
	if (siblings->empty())
	{
		foundNode = false;
	}
	// there are sibling(s)
	else 
	{
		// is this smc is at the same level as the current set of siblings? (check with the first sibling)
		if (smc->level() == (*(siblings->begin()))->subjectMatterCode().level())
		{
			// then, find the smc among these siblings
			SubjectMatterCodeNodeVectorIterator i = siblings->begin();
			while (!foundNode && (i != siblings->end()))
			{
				if (smc->content() == (*i)->subjectMatterCode().content())
				{
					node = *i;
					foundNode = true;
				}
				else 
				{
					i++;
				}
			}
		}
		else
		{
			// else find the siblings to which this smc belongs. That is, find its parent node in this
			// set of siblings, then try to find the top node into this parent's siblings.
			SubjectMatterCodeNodeVectorIterator i = siblings->begin();
			bool foundParent = false;
			while (!foundParent && (i != siblings->end()))	// search for parent node (if any)
			{
				if (smc->contentAtLevel((*i)->subjectMatterCode().level()) ==
                (*i)->subjectMatterCode().content())
				{
					foundParent = true;
				}
				else
				{
					i++;
				}
			}
			if (foundParent)		// found its parent node
			{
				foundNode = findInSiblings(smc, (*i)->children(), node);
			}
		}
	}

	return foundNode;
}

// --------------------------------------------------------------------------
// Add the Logos default SMC from the selected subtrees. This is needed when the user
// has selected a regular dictionary search with Logos default.
// --------------------------------------------------------------------------
/*
void SubjectMatterCodeTree::addLogosDefaultToSelectedSubtrees(SubjectMatterCode* smcLogosDefault)
{
	SubjectMatterCodeNode* aSMCNode = new SubjectMatterCodeNode(*smcLogosDefault, 0);
	selectedSubTrees_->push_back(aSMCNode);
}
*/

// --------------------------------------------------------------------------
// Remove the given SMC from the given tree. Find the node that contains this SMC.
// Return whether it has been removed.
// --------------------------------------------------------------------------
bool SubjectMatterCodeTree::removeNode(SubjectMatterCode* aNode, SubjectMatterCodeNodeVector* theTree)
{
	bool foundNode=false;

	if (!theTree->empty())
	{
		// try to remove the node at this level
		foundNode = theTree->removeNode(aNode);

		// if not found, search each child in turn to remove the node
		SubjectMatterCodeNodeVectorIterator node = theTree->begin();

		while (!foundNode && (node != theTree->end()))
		{
			foundNode = removeNode(aNode, (*node)->children());
			node++;
		}
	}

	return foundNode;
}

// --------------------------------------------------------------------------
// Search the ordered set of SMC in the complete tree for the given SMC. Return the node
// number of the match (return 0 if no match). The search is depth-first.
// --------------------------------------------------------------------------
int SubjectMatterCodeTree::nodeNumber(const SubjectMatterCode& theSMC)
{
	int nodeNumber = 0;
	bool foundNode = search(theSMC, rootChildren_, nodeNumber);

	if (!foundNode)
	{
		nodeNumber = 0;
	}

	return nodeNumber;
}

// --------------------------------------------------------------------------
// Return the node number of the given SMC in the given tree.
// The search is depth-first.
// --------------------------------------------------------------------------
bool SubjectMatterCodeTree::search(const SubjectMatterCode& theSMC,
                                   SubjectMatterCodeNodeVector* theTree,
                                   int& nodeNumber)
{
	bool foundNode = false;

	if (!theTree->empty())
	{
          // if not found, search each child in turn
          nodeNumber++;
          SubjectMatterCodeNodeVectorIterator node;
            for (node = theTree->begin();
                 node != theTree->end()
                   && !(theSMC == (*node)->subjectMatterCode()
                        || search(theSMC,(*node)->children(),nodeNumber));
                 node++, nodeNumber++);
          foundNode = node != theTree->end();
	}

	return foundNode;
}

void SubjectMatterCodeTree::resetIterator(void)
{
    if (rootChildren_)
    {
        rootChildren_->resetIterator();
    }
}

SubjectMatterCode& SubjectMatterCodeTree::operator++(int)
{
    SubjectMatterCode *smc = 0;
    if (rootChildren_)
    {
        return (*rootChildren_)++;
    }
    return (*smc);
}

bool SubjectMatterCodeTree::isEnd(void) const
{
    if (rootChildren_)
    {
        return rootChildren_->isEnd();
    }
    return true;
}

void SubjectMatterCodeTree::getTreeAsString(LgsString& smcTree)
{
    if (rootChildren_)
    {
        rootChildren_->appendSmcInfo(smcTree);
    }
}
