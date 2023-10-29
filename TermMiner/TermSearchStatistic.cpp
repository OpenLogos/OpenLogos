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
// TermSearchStatistic.cpp: implementation of the TermSearchStatistic class.
//
//////////////////////////////////////////////////////////////////////

#include <logos_include/logoscommon.h>
#include <TermMiner/TermSearchStatistic.h>

// --------------------------------------------------------------------------
TermSearchStatistic::TermSearchStatistic() :
totalDictionaryUnfoundTermObjectsCreated_(0),
totalDictionaryFoundTermObjectsCreated_(0),
totalGermanCompoundTermObjectsCreated_(0),
totalGermanNounPhraseTermObjectsCreated_(0),
totalEnglishNounPhraseTermObjectsCreated_(0),
totalInsertionsInTree_(0),
totalInsertionsInTreeNodes_(0),
totalAppendOperationsForInsertions_(0)
{
}

// --------------------------------------------------------------------------
TermSearchStatistic::~TermSearchStatistic()
{
}

// --------------------------------------------------------------------------
void TermSearchStatistic::report(ostream& out)
{
	int totalObjectsCreated =   totalDictionaryUnfoundTermObjectsCreated_
		                      + totalDictionaryFoundTermObjectsCreated_
							  + totalGermanCompoundTermObjectsCreated_
							  + totalGermanNounPhraseTermObjectsCreated_
							  + totalEnglishNounPhraseTermObjectsCreated_;
	int totalInsertions = totalInsertionsInTree_ + totalInsertionsInTreeNodes_;
	double averageInsertionsPerTermObject = (double) totalInsertions / totalObjectsCreated;
	double averageAppendOperationsPerTermObject = (double) totalAppendOperationsForInsertions_ / totalObjectsCreated;
	double averageAppendOperationsPerInsertion = (double) totalAppendOperationsForInsertions_ / totalInsertions;
	double averageAppendOperationsPerNodeInsertion = (double) totalAppendOperationsForInsertions_ / totalInsertionsInTreeNodes_;

	out << setprecision(2);
	out << "total Term objects created ......................................: " << totalObjectsCreated << endl;
	out << "   total Dictionary Undfound Term objects created .....: " << totalDictionaryUnfoundTermObjectsCreated_ << endl;
	out << "   total Dictionary Found Term objects created ........: " << totalDictionaryFoundTermObjectsCreated_ << endl;
	out << "   total German Compound Term objects created .........: " << totalGermanCompoundTermObjectsCreated_ << endl;
	out << "   total German Noun Phrase Term objects created ......: " << totalGermanNounPhraseTermObjectsCreated_ << endl;
	out << "   total English Noun Phrase Term objects created .....: " << totalEnglishNounPhraseTermObjectsCreated_ << endl;
	out << "total insertions in Term list ...................................: " << totalInsertions << endl;
	out << "   total insertions in tree ...........................: " << totalInsertionsInTree_ << endl;
	out << "   total insertions in tree nodes .....................: " << totalInsertionsInTreeNodes_ << endl;
	out << "   average insertions per Term object created .........: " << averageInsertionsPerTermObject << endl;
	out << "total append operations used for all insertions in Term list ....: " << totalAppendOperationsForInsertions_ << endl;
	out << "   average append operations per Term object created ..: " << averageAppendOperationsPerTermObject << endl;
	out << "   average append operations per insertion ............: " << averageAppendOperationsPerInsertion << endl;
	out << "   average append operations per node insertion .......: " << averageAppendOperationsPerNodeInsertion << endl;
}

