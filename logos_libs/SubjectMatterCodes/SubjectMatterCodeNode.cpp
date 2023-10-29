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
// File: SubjectMatterCodeNode.cpp (implementation)
// --------------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/SubjectMatterCodes/SubjectMatterCodeNode.h>

// --------------------------------------------------------------------------
// Default constructor (for STL purposes to have a vector of SMC nodes)
// NOT TO BE USED
// --------------------------------------------------------------------------
SubjectMatterCodeNode::SubjectMatterCodeNode()
{
	sequenceOrder_ = 0;										// no particular position among the nodes at the level of this node
	children_ = new SubjectMatterCodeNodeVector;	// no children (next level)
    iterationComplete_ = false;
}

// --------------------------------------------------------------------------
// Construct this node with the given SMC and sequence order
// --------------------------------------------------------------------------
SubjectMatterCodeNode::SubjectMatterCodeNode(SubjectMatterCode aSMC, int aSequenceOrder)
{
	smc_ = aSMC;									// SMC value at this node
	sequenceOrder_ = aSequenceOrder;				// set the position among the nodes at the level of this node
	children_ = new SubjectMatterCodeNodeVector;	// no children (next level)
}

// --------------------------------------------------------------------------
// Default destructor
// --------------------------------------------------------------------------
SubjectMatterCodeNode::~SubjectMatterCodeNode()
{
//	printf("SMCNode \"%s\" deletion\n", subjectMatterCode().content().c_str()); 
//	printf("SMCNode %x deletion\n", this);
	fflush(stdout);
	delete children_;
}

// --------------------------------------------------------------------------
// Add a child SMC to this node.
// --------------------------------------------------------------------------
void SubjectMatterCodeNode::addChild(SubjectMatterCodeNode* newChild)
{
	children_->append(newChild);
}

// --------------------------------------------------------------------------
// Display all children SMC nodes of this node
// --------------------------------------------------------------------------
void SubjectMatterCodeNode::displayChildren(LgsString iterator)
{
	children_->display(iterator);
}

// --------------------------------------------------------------------------
// Display content of this node
// --------------------------------------------------------------------------
void SubjectMatterCodeNode::display(LgsString iterator)
{
	cout << iterator << smc_.content() << endl;
	displayChildren(iterator + "  ");
}

void SubjectMatterCodeNode::resetIterator(void)
{
    iterationComplete_ = false;
    if (children_)
    {
        children_->resetIterator();
    }
}

SubjectMatterCode& SubjectMatterCodeNode::operator++(int)
{
    if (children_ && !children_->isEnd())
    {
        SubjectMatterCode& smc = (*children_)++;
        return smc;
    }
    iterationComplete_ = true;
    return smc_;
}

bool SubjectMatterCodeNode::isEnd(void) const
{
    return iterationComplete_;
}

void SubjectMatterCodeNode::appendSmcInfo(LgsString& smcTree)
{
    smc_.appendSmcInfo(smcTree);
    if (children_)
    {
        children_->appendSmcInfo(smcTree);
    }
}

// --------------------------------------------------------------------------
// Default constructor
// --------------------------------------------------------------------------
SubjectMatterCodeNodeVector::SubjectMatterCodeNodeVector()
{
}

// --------------------------------------------------------------------------
// Default destructor
// --------------------------------------------------------------------------
SubjectMatterCodeNodeVector::~SubjectMatterCodeNodeVector()
{
//	printf("SMCNodeVector %x deletion\n", this); fflush(stdout);
//	printf("SMCNodeVector %x has %d elements\n", this, size()); fflush(stdout);

	SubjectMatterCodeNodeVectorIterator i;
	for(i=begin();i!=end();i++) {
//		printf("SMCNode %x is about to be deleted\n", *i); fflush(stdout);
//		printf("SMCNode %x has smc %x\n", *i, (*i)->subjectMatterCode());
//		printf("SMCNode %x has smc.content \"%s\"\n", *i,
//			(*i)->subjectMatterCode().content().c_str());
		delete (*i);
//		printf("SMCNode %x has been deleted\n", *i); fflush(stdout);
	}

}

// --------------------------------------------------------------------------
// Add a child SMC to this node at this appropriate location among the children.
// Children SMC are ordered, and this is based on the node orderSequence value.
// --------------------------------------------------------------------------
void SubjectMatterCodeNodeVector::append(SubjectMatterCodeNode* aSMCNode)
{
	// find where to insert this node
	SubjectMatterCodeNodeVectorIterator i = begin();
	int pos = 0;
	while (i != end())
	{
		if ((*i)->sequenceOrder() <= aSMCNode->sequenceOrder())
		{
			i++;
			pos++;
		}
      else
      {
         break;
      }
	}

	// insert the node at the appropriate location
	if (i != end())
	{
		insert(begin() + pos, aSMCNode);	// insert as the i-th element in the vector
	}
	else
	{
		push_back(aSMCNode);			// insert at end of vector
	}
}

// --------------------------------------------------------------------------
// Display content of vector of SMC nodes
// --------------------------------------------------------------------------
void SubjectMatterCodeNodeVector::display(LgsString iterator)
{
	SubjectMatterCodeNodeVectorIterator i;
	for (i = begin(); i != end(); i++) 
	{
		(*i)->display(iterator);
	}
}

// --------------------------------------------------------------------------
// Search the ordered set of SMC for the given SMC in this vector of SMC nodes (a
// level in the tree). Return the node number of the match (return 0 if no match).
// --------------------------------------------------------------------------
bool SubjectMatterCodeNodeVector::member(SubjectMatterCode& theSMC, int& nodeNumber)
{
	bool foundNode = false;
	SubjectMatterCodeNodeVectorIterator i = begin();

	while (i != end() && !foundNode)
	{
		nodeNumber++;
		if ((*i)->subjectMatterCode() == theSMC)
		{
			foundNode = true;		// this is the wanted node
		}
		else
		{
			i++;				// consider next node
		}
	}

	return foundNode;
}

// --------------------------------------------------------------------------
// Return whether the given node is present in this vector of nodes.
// --------------------------------------------------------------------------
bool SubjectMatterCodeNodeVector::member(SubjectMatterCodeNode* aNode)
{
	bool foundNode = false;
	SubjectMatterCodeNodeVectorIterator i=begin();

	while ((i != end()) && !foundNode)
	{
		if ((*i)->subjectMatterCode() == aNode->subjectMatterCode())
		{
			foundNode = true;		// this is the wanted node
		}
		else
		{
			i++;				// consider next node
		}
	}

	return foundNode;
}

// --------------------------------------------------------------------------
// Remove from this vector the node corresponding to the given SMC. Return
// whether the node has been removed.
// --------------------------------------------------------------------------
bool SubjectMatterCodeNodeVector::removeNode(SubjectMatterCode* theSMC)
{
	bool foundNode = false;
	SubjectMatterCodeNodeVectorIterator i = begin();

	while ((i != end()) && !foundNode)
	{
		if ((*i)->subjectMatterCode() == *theSMC)
		{
			foundNode = true;
			erase(i);
		}
		else
		{
			i++;
		}
	}

	return foundNode;
}

void SubjectMatterCodeNodeVector::resetIterator(void)
{
    for (iterator nodeIter = begin(); nodeIter != end(); nodeIter++)
    {
        SubjectMatterCodeNode * smcNode = (*nodeIter);
        smcNode->resetIterator();
    }

    current_ = begin();
}

// returns the current element and points to the next element
// isEnd() SHOULD be called before this function is called to make sure 
// elements are there
SubjectMatterCode& SubjectMatterCodeNodeVector::operator++(int)
{
    SubjectMatterCodeNode* smcNode = *current_;
    SubjectMatterCode& smc = (*smcNode)++;
    if (smcNode->isEnd())
    {
        current_++;
    }

    return smc;
}

bool SubjectMatterCodeNodeVector::isEnd(void) const
{
    return (end() == current_);
}

void SubjectMatterCodeNodeVector::appendSmcInfo(LgsString& smcTree)
{
    for (iterator nodeIter = begin(); nodeIter != end(); nodeIter++)
    {
        SubjectMatterCodeNode* smcNode = *nodeIter;
        smcNode->appendSmcInfo(smcTree);
    }
}
