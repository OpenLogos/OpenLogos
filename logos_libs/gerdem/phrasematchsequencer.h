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
#ifndef _PHRASEMATCHSEQUENCER_H_
#define _PHRASEMATCHSEQUENCER_H_

#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/lword.h>
#include <logos_libs/linguistic/lroot.h>

class GRootedWord;

class PhraseMatchSequencer
{
public:
	enum RootAnalysisCode {NO_ANALYSIS, EXACT_MATCH, FW_ROOT, SW_ROOT, ADJ_NP, RAC_MAX_VALUE};
	PhraseMatchSequencer(LWordIterator startWord);
	~PhraseMatchSequencer(void);
	const LgsString & firstWord(void) const;
	const LgsString & secondWord(void) const;
	short noSpaces(void) const;
	operator bool(void) const;
	void operator++(int);
	void operator++(void);
	RootAnalysisCode analysisCode(void) const;
	static bool isValidNPending(const LgsString & ending);
private:
	RootAnalysisCode m_racOrder[RAC_MAX_VALUE];
	LWordIterator m_startWord;
	LgsString m_firstWord;
	LgsString m_secondWord;
	short m_noSpaces;
	GRootedWord *m_fwRoots;
	GRootedWord *m_swRoots;
	LRootVector::const_iterator m_fwRootIterator;
	LRootVector::const_iterator m_fwRootEndIterator;
	LRootVector::const_iterator m_swRootIterator;
	LRootVector::const_iterator m_swRootEndIterator;
	short m_racIndex;
	static LgsString m_endingList[];
	short currentEndIndex;
};

#endif
