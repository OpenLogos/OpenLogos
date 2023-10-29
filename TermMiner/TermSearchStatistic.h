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
// TermSearchStatistic.h: interface for the TermSearchStatistic class.
//
//////////////////////////////////////////////////////////////////////

#ifndef _TermSearchStatistic_h_
#define _TermSearchStatistic_h_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

// --------------------------------------------------------------------------
class TermSearchStatistic : public Object
{
public:
	TermSearchStatistic();
	virtual ~TermSearchStatistic();
	void report(ostream&);
	void incrementNumberDictionaryUnfoundTermObjectsCreated();
	void incrementNumberDictionaryFoundTermObjectsCreated();
	void incrementNumberGermanCompoundTermObjectsCreated();
	void incrementNumberGermanNounPhraseTermObjectsCreated();
	void incrementNumberEnglishNounPhraseTermObjectsCreated();
	void incrementNumberInsertionsInTree();
	void incrementNumberInsertionsInTreeNodes();
	void incrementNumberAppendOperationsForInsertions();

private:
	int totalDictionaryUnfoundTermObjectsCreated_;
	int totalDictionaryFoundTermObjectsCreated_;
	int totalGermanCompoundTermObjectsCreated_;
	int totalGermanNounPhraseTermObjectsCreated_;
	int totalEnglishNounPhraseTermObjectsCreated_;
	int totalInsertionsInTree_;
	int totalInsertionsInTreeNodes_;
	int totalAppendOperationsForInsertions_;
};


// --------------------------------------------------------------------------
inline void TermSearchStatistic::incrementNumberDictionaryUnfoundTermObjectsCreated()
{
	totalDictionaryUnfoundTermObjectsCreated_++;
}

// --------------------------------------------------------------------------
inline void TermSearchStatistic::incrementNumberDictionaryFoundTermObjectsCreated()
{
	totalDictionaryFoundTermObjectsCreated_++;
}

// --------------------------------------------------------------------------
inline void TermSearchStatistic::incrementNumberGermanCompoundTermObjectsCreated()
{
	totalGermanCompoundTermObjectsCreated_++;
}

// --------------------------------------------------------------------------
inline void TermSearchStatistic::incrementNumberGermanNounPhraseTermObjectsCreated()
{
	totalGermanNounPhraseTermObjectsCreated_++;
}

// --------------------------------------------------------------------------
inline void TermSearchStatistic::incrementNumberEnglishNounPhraseTermObjectsCreated()
{
	totalEnglishNounPhraseTermObjectsCreated_++;
}

// --------------------------------------------------------------------------
inline void TermSearchStatistic::incrementNumberInsertionsInTree()
{
	totalInsertionsInTree_++;
}

// --------------------------------------------------------------------------
inline void TermSearchStatistic::incrementNumberInsertionsInTreeNodes()
{
	totalInsertionsInTreeNodes_++;
}

// --------------------------------------------------------------------------
inline void TermSearchStatistic::incrementNumberAppendOperationsForInsertions()
{
	totalAppendOperationsForInsertions_++;
}

#endif // _TermSearchStatistic_h_
