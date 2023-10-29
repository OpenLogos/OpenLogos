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
// TermNode.cpp: implementation of the TermNode class.
//
//////////////////////////////////////////////////////////////////////

#include <logos_include/logoscommon.h>
#include <TermMiner/TermNode.h>
#include <TermMiner/TermSearchStatistic.h>

// --------------------------------------------------------------------------
// Default constructor
// --------------------------------------------------------------------------
TermNode::TermNode(LgsString theKey) :
key_(theKey),									// set the node label
leftNode_(0),									// initially, no left descendant to this node
rightNode_(0),									// initially, no right descendant to this node
totalNumberOccurrencesInDocument_(0)
{
}

// --------------------------------------------------------------------------
// Destructor
// --------------------------------------------------------------------------
TermNode::~TermNode()
{
	delete leftNode_;
	delete rightNode_;
}

// --------------------------------------------------------------------------
// Append a Term object which has the same key.
// --------------------------------------------------------------------------
void TermNode::append(Term* aTerm, TermSearchStatistic& stat)
{
#ifdef __TERM_SEARCH_STATS__
	stat.incrementNumberInsertionsInTreeNodes();
	stat.incrementNumberAppendOperationsForInsertions();
#endif

	terms_.push_back(*aTerm);
}

// --------------------------------------------------------------------------
// Create a left/right descendant given a Term to store in it and a node label (key)
// --------------------------------------------------------------------------
void TermNode::createLeftChild(Term* aTerm, LgsString key, TermSearchStatistic& stat)
{
	leftNode_ = new TermNode(key);			// create a new node object
	leftNode_->append(aTerm,stat);					// store its first Term object
	leftNode_->incrementTotalNumberOccurrencesInDocument();
}

// --------------------------------------------------------------------------
void TermNode::createRightChild(Term* aTerm, LgsString key, TermSearchStatistic& stat)
{
	rightNode_ = new TermNode(key);			// create a new node object
	rightNode_->append(aTerm,stat);					// store its first Term object
	rightNode_->incrementTotalNumberOccurrencesInDocument();
}

// --------------------------------------------------------------------------
// Report this node in the specified output stream.
// --------------------------------------------------------------------------
void TermNode::report(ostream& out)
{
	LgsString separator = "\t";

	for (TermIterator term=terms_.begin(); term!=terms_.end(); term++)
	{
		out << term->sourceLanguage() << separator;
		out << term->targetLanguage() << separator;
		out << term->formattedSearchStatus() << separator;
		out << term->sourceSurfaceForm() << separator;
		out << term->sourceCanonicalForm() << separator;
		out << term->sourceHeadInCanonicalForm() << separator;
		out << term->formattedSourcePOS() << separator;
		out << term->formattedSourceGender() << separator;
		out << term->targetSurfaceForm() << separator;
		out << term->formattedTargetStatus() << separator;
		out << term->formattedTargetGender() << separator;
		out << term->companyCode() << separator;
		out << term->subjectMatterCode() << separator;
		out << term->sourceLocation() << separator;
		out << totalNumberOccurrencesInDocument_ << separator;
		out << term->sentenceNumber() << separator;
		out << term->context() << separator;
		out << endl;
	}
}
