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
// File: SubjectMatterCodeTree.h (interface)
// Purpose: defines a tree of subject matter codes. The tree has a single root node (top_)
// from which are all subtrees for several levels (depending on nodes attachements). The subtrees
// at each level are in a sequence order.
// --------------------------------------------------------------------------

#ifndef _SubjectMatterCodeTree_h
#define _SubjectMatterCodeTree_h

class SubjectMatterCode;
class SubjectMatterCodeNode;
class SubjectMatterCodeNodeVector;

// --------------------------------------------------------------------------
// Interface for class SubjectMatterCodeTree
// --------------------------------------------------------------------------
class SubjectMatterCodeTree : public Object
{
public:
	SubjectMatterCodeTree();						// default constructor
	virtual ~SubjectMatterCodeTree();			// default destructor

	// - setter methods
	// set this tree as the whole SMC tree (user ordered)
	void setAsCompleteTree(const LgsString& selectedTreeName = LgsString("LOGOS1"));
    // Create SMC tree from a wide character string in a certain format
    void setAsCompleteTree(const wchar_t* smcTreeContents);
	// set this tree as the set of user-selected SMC subtrees
	void setAsUserSelectedSubtrees(SubjectMatterCodeTree*);
	// set this tree as the difference between the given two trees
	void setAsDifference(SubjectMatterCodeTree*,SubjectMatterCodeTree*);

	// getter methods
	SubjectMatterCodeNodeVector* tree() {return rootChildren_;}

	// display content of this SMC tree
	void display();

	// membership methods
	int nodeNumber(const SubjectMatterCode &);

	// remove the given node from the tree structure
	bool removeNode(SubjectMatterCode*,SubjectMatterCodeNodeVector*);
    void resetIterator(void);
    SubjectMatterCode& operator++(int);
    bool isEnd(void) const;
    void getTreeAsString(LgsString& smcTree);

	SubjectMatterCodeNode * copySubtree(SubjectMatterCodeNode *src);


private:
	// top of tree (level 1) - an ordered vector of SMC nodes
	SubjectMatterCodeNodeVector* rootChildren_;

	// append a SMC in the appropriate place in the SMC tree
	void insertInto(SubjectMatterCodeNode*,SubjectMatterCodeNodeVector*);

	bool findInSiblings(SubjectMatterCode*,SubjectMatterCodeNodeVector*,SubjectMatterCodeNode*&);
	bool search(const SubjectMatterCode&,SubjectMatterCodeNodeVector*,int&);

	// select the nodes of vector1 that are not in vector2 - recursively do the same for each selected node
	void selectNodes(SubjectMatterCodeNodeVector*,SubjectMatterCodeNodeVector*);
};


#endif
