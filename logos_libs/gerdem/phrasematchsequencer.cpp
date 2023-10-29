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
#include <logos_libs/gerdem/phrasematchsequencer.h>
#include <logos_libs/gerdem/grootedword.h>
#include <logos_libs/translutility/translcommonobjects.h>

LgsString PhraseMatchSequencer::m_endingList[] = { "e", "es", "er", "en", "em"};

PhraseMatchSequencer::PhraseMatchSequencer(LWordIterator startWord)
{
	int i = 0;
	m_racOrder[i++] = EXACT_MATCH;
	if (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::GermanID)
	{
		m_racOrder[i++] = ADJ_NP;
	}
	m_racOrder[i++] = FW_ROOT;
	m_racOrder[i++] = SW_ROOT;
	m_racOrder[i++] = NO_ANALYSIS;
	m_racIndex = 0;
	m_startWord = startWord;
	m_fwRoots = 0;
	m_swRoots = 0;
	currentEndIndex = sizeof(m_endingList)/sizeof(m_endingList[0])+1;
	m_firstWord = *startWord;
	if ((*startWord).isHyphenated())
	{
		m_secondWord = "-";
	}
	else
	{
		m_secondWord = *(startWord+1);
	}
	m_noSpaces = (*startWord).trailingSpaces();
}

PhraseMatchSequencer::~PhraseMatchSequencer(void)
{
	if (m_fwRoots)
	{
		delete m_fwRoots;
		m_fwRoots = 0;
	}
	if (m_swRoots)
	{
		delete m_swRoots;
		m_swRoots = 0;
	}
}

const LgsString & PhraseMatchSequencer::firstWord(void) const
{
	return m_firstWord;
}

const LgsString & PhraseMatchSequencer::secondWord(void) const
{
	return m_secondWord;
}

short PhraseMatchSequencer::noSpaces(void) const
{
	return m_noSpaces;
}

PhraseMatchSequencer::operator bool(void) const
{
	if (NO_ANALYSIS == m_racOrder[m_racIndex])
	{
		return false;
	}
	return true;
}

void PhraseMatchSequencer::operator++(int)
{
	bool bContinue = true;
	while (bContinue)
	{
		switch (m_racOrder[m_racIndex])
		{
		case EXACT_MATCH:
			m_racIndex++;
			break;
		case ADJ_NP:
			for (currentEndIndex--; currentEndIndex; currentEndIndex--)
			{
				if ((*m_startWord).length()-1 < m_endingList[currentEndIndex-1].length()+3)
				{
					continue;
				}
				LgsString::iterator sourceIter = (*m_startWord).end()-m_endingList[currentEndIndex-1].length();
				LgsString::iterator targetIter = m_endingList[currentEndIndex-1].begin();
				for (; sourceIter != (*m_startWord).end() && (*sourceIter) == (*targetIter); sourceIter++, targetIter++);
				if (sourceIter == (*m_startWord).end())
				{
					break;
				}
			}
			if ((*(m_startWord+1) != "-") && currentEndIndex)
			{
				m_firstWord = (*m_startWord).substr(0, (*m_startWord).length()-m_endingList[currentEndIndex-1].length());
				m_secondWord = "";
				bContinue = false;
			}
			else
			{
				m_racIndex++;
			}
			break;
		case NO_ANALYSIS:
			bContinue = false;
			break;
		case FW_ROOT:
			if (!m_fwRoots)
			{
				m_fwRoots = new GRootedWord(&(*m_startWord));
				m_fwRootIterator = m_fwRoots->roots()->begin();
			}
			else
			{
				m_fwRootIterator++;
			}
			if (m_fwRootIterator == m_fwRoots->roots()->end())
			{
				m_racIndex++;
				break;
			}
			m_firstWord = *m_fwRootIterator;
			m_secondWord = *(m_startWord+1);
			bContinue = false;
			break;
		case SW_ROOT:
			if (!m_swRoots)
			{
				m_swRoots = new GRootedWord(&(*(m_startWord+1)));
				m_swRootIterator = m_swRoots->roots()->begin();
			}
			else
			{
				m_swRootIterator++;
			}
			if (m_swRootIterator == m_swRoots->roots()->end())
			{
				m_racIndex++;
				break;
			}
			m_firstWord = *m_startWord;
			m_secondWord = *m_swRootIterator;
			bContinue = false;
			break;
		default:
			bContinue = false;
			break;
		}
	}
}

void PhraseMatchSequencer::operator++(void)
{
	operator++(1);
}

PhraseMatchSequencer::RootAnalysisCode PhraseMatchSequencer::analysisCode(void) const
{
	return m_racOrder[m_racIndex];
}

bool PhraseMatchSequencer::isValidNPending(const LgsString & ending)
{
	int lstSize = sizeof(m_endingList)/sizeof(m_endingList[0]);
        int i;
	for (i = 0; i < lstSize && ending.compare(m_endingList[i]) ; i++);
	return (i != lstSize);
}
