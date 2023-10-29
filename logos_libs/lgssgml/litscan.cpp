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
//////////////////////////////////////////////////////////////////////
//
// LitScan.cpp: implementation of the CLitScan class.
//
//////////////////////////////////////////////////////////////////////
#include <logos_include/logoscommon.h>
#include <logos_libs/lgssgml/litscan.h>
#include <logos_libs/lgssgml/lgsutil.h>

// -------------------------------------------------------------------
CLitScan::CLitScan()
{
	m_attached = false;
	m_maxNumStates = 1000;
	m_startIx = -1;
	m_numPos = 0;
	m_numStates = 0;
   m_pos2sym = 0;
	for (int i = 0; i < 256; i++)
		m_term[i] = false;
}
// -------------------------------------------------------------------
CLitScan::~CLitScan()
{
	for (int i = 0; i < m_numStates; i++)
   {
		delete m_states[i];
   }
   m_states.erase(m_states.begin(), m_states.end());
   m_trans.erase(m_trans.begin(), m_trans.end());
   m_expanded.erase(m_expanded.begin(), m_expanded.end());
   if (m_pos2sym)
   {
      delete m_pos2sym;
   }
}
// -------------------------------------------------------------------
bool CLitScan::Init(int numStates, LgsVector(LgsString)& lits)
{
	if (numStates > 0)
		m_maxNumStates = numStates;

	for (int ii = 0; ii < m_maxNumStates*256; ii++)
			m_trans.push_back(0); // err state
	for (int j = 0; j < m_maxNumStates; j++)
	{
		m_states.push_back(NULL);
		m_expanded.push_back(false);
	}

	// init err state entry
	m_states[m_numStates] = new StateRec();
   CLgsUtil::Assert((bool)(m_states[m_numStates]), "out of memory");
	m_expanded[m_numStates] = true;
	m_numStates++;

	// build pos2sym array, and firstPos set which we will need
	// in build DFA step
	int pos = 0;
	State firstPos = new StateRec();
	CLgsUtil::Assert((bool)(firstPos), "out of memory");
	int uIx = -1;

	m_numPos = 0;
        LgsVector(LgsString)::iterator i;
	for (i = lits.begin(); i != lits.end(); i++)
	{
		
		if ((1 == (*i).length()) && (!isalpha((*i)[0])) )
			m_term[((unsigned char)(*i)[0])] = true;
		else if ((1 == (*i).length()) && ((*i) == "\x97")) //emdash  Added by Anand Bosco
							  //on 12/10/98 to break the words seperated by emdash. This condition 
							   //is  added because emdash is treated is alpha i.e isalpha returns true.
		{
			m_term[((unsigned char)(*i)[0])] = true;
		}
		else
			m_numPos += (*i).length() + 1;
	}

   m_pos2sym = new unsigned char[m_numPos];
	CLgsUtil::Assert((bool)(m_pos2sym), "out of memory");
   memset(m_pos2sym, '\0', m_numPos);

	for (i = lits.begin(); i != lits.end(); i++)
	{
		if (
			((*i).length() == 0) || (
										(1 == (*i).length()) && (
																	(!isalpha((*i)[0])) ||((*i) == "\x97")
																) 
									 ) 
		  ) continue;

		firstPos -> push_back(pos);

		// enum sym in this LgsString
		for (LgsString::iterator j = (*i).begin(); j != (*i).end(); j++)
		{
			m_pos2sym[pos++] = (*j);
		}

#if defined(DFA1)
		cout << " state containing pos " << pos << " is accepting state for literal \"" << (*i).c_str() << "\"\n";
#endif // DFA1

		// add $-entry
		m_pos2sym[pos++] = 0;
	}

	//assert(pos == m_numPos);

	// build DFA
	m_startIx = m_numStates;
	m_states[m_numStates] = firstPos;
	m_expanded[m_numStates++] = false;
	if (m_numStates >= m_maxNumStates)
	{
      AugmentStateTable();
	}

	while (-1 != (uIx = NextUState()))
		Expand(uIx);

#if defined(DFA2)
	cout << "========== states table ============\n";
	for (int k = 0; k < m_numStates; k++)
	{
		cout << "i" << k << ": ";
		for (StateRec::iterator h = m_states[k]->begin();
				h != m_states[k]->end(); h++)
		{
			cout << (*h);
			cout << (0 == m_pos2sym[*h] ? "+" : "-");
		}
		cout << "\n";
	}
#endif // DFA2

#if defined(DFA3)
	cout << "\n";
	cout << "========== transiiton table ============\n";
	for (k = 0; k < m_numStates; k++)
	{
		for (int m = 0; m < 256; m++)
			cout << m_trans[k*256+m] << ",";
		cout << "\n";
	}
#endif // DFA3

	return true;
}
// -------------------------------------------------------------------
int CLitScan::NextUState()
{
	for (int i = 0; i < m_numStates; i++)
	{
		if (!m_expanded[i])
			return i;
	}

	return -1;
}
// -------------------------------------------------------------------
void CLitScan::Expand(int stateIx)
{
	State state = m_states[stateIx];

	for (int i = 0; i < 256; i++)
	{
		State u = NextPos(state, (unsigned char)i);

		if (u)
		{
			int k = IsNewState(u);
			if (-1 == k)
			{
				k = m_numStates;
				m_states[m_numStates] = u;
				m_expanded[m_numStates++] = false;
            if (m_numStates >= m_maxNumStates)
               AugmentStateTable();
			}
			else
			{
				delete u;
			}
			m_trans[stateIx*256 +i] = k;
		}
	}
	m_expanded[stateIx] = true;
}
// -------------------------------------------------------------------
int CLitScan::IsNewState(State state)
{
	for (int i = 0; i < m_numStates; i++)
	{
		if (state->size() != m_states[i]->size())
			continue;

                StateRec::iterator j ;
		for (j = state->begin(); j != state->end(); j++)
		{
			int pos = *j;
                        StateRec::iterator k;
			for (k = m_states[i]->begin();
					k != m_states[i]->end(); k++)
				if (pos == *k)
					break;

			if (k == m_states[i]->end())
				break;
		}

		if (j == state->end())
			return i;
	}

	return -1;
}
// -------------------------------------------------------------------
LgsVector(int)* CLitScan::NextPos(State state, unsigned char sym)
{
	// valid for literals only
	State result = NULL;
	for (StateRec::iterator j = state->begin(); j != state->end(); j++)
	{
		if (sym == m_pos2sym[*j] && sym > 0)
		{
			if (!result)
				result = new StateRec();
			result -> push_back(*j + 1);
		}
	}
	return result;
}
// -------------------------------------------------------------------
static inline bool IsErrorState(int ix)
{
   return 0 == ix;
}
// -------------------------------------------------------------------
void CLitScan::Scan(const LgsString& input, LgsVector(LgsString)& lexems, LgsVector(unsigned char)& flags)
{
	LgsString& inp = (LgsString&)input;
	int ix = m_startIx;
	LgsString s;
	int prevIx = -1;
	bool skippingOverSpaces = false;

	for (LgsString::iterator i = inp.begin(); i != inp.end(); i++)
	{
		prevIx = ix;

		if (strchr(" \t\xd\xa", *i))
		{
			if (!skippingOverSpaces)
			{
				skippingOverSpaces = true;
				int slen = s.length();
				if (slen > 0)
				{
					lexems.push_back(s);
					flags.push_back(
						IsAcceptingState(prevIx)?
							((slen > 1 || !m_term[ (unsigned char)(s[slen-1]) ])?
								SCF_LITERAL : SCF_PUNCTUATION) : SCF_GENERIC_WORD);
					s.erase();
				}
			}
			s += *i;
			continue;
		}
		else if (skippingOverSpaces)
		{
			skippingOverSpaces = false;
         if (s.length() > 0)
         {
			   lexems.push_back(s);
			   flags.push_back(SCF_SPACES);
			   s.erase();
         }
			ix = m_startIx;
		}

		ix = m_trans[ix*256 + (unsigned char)(*i)]; // state to go

		if (m_term[(unsigned char)*i] && IsAcceptingState(prevIx) && IsErrorState(ix))
		{
			// add pun/lit to output
         if (s.length() > 0)
         {
			   lexems.push_back(s);
			   flags.push_back(s.length() > 1 ? SCF_LITERAL : SCF_PUNCTUATION);
			   s.erase();
         }
			ix = m_startIx;
			i--;
		}
		else if (prevIx == m_startIx && m_term[(unsigned char)*i])
		{
			// add pun/lit to output
			s += *i;
			lexems.push_back(s);
			flags.push_back(s.length() > 1 ? SCF_LITERAL : SCF_PUNCTUATION);
			s.erase();
			ix = m_startIx;
		}
		else if (IsErrorState(ix) && m_term[(unsigned char)*i])
		{
			// add lit, or junk to output
			if (s.length() > 0)
			{
				lexems.push_back(s);
				s.erase();
				flags.push_back(
					IsAcceptingState(prevIx)? SCF_LITERAL : SCF_GENERIC_WORD);
			}

			ix = m_startIx;
			i--;
		}
		else
		{
			// continue to acc
			s += *i;
		}
	}

	if (s.length() > 0)
	{
		lexems.push_back(s);
		flags.push_back(
			 skippingOverSpaces? SCF_SPACES :
				IsAcceptingState(ix)? SCF_LITERAL : SCF_GENERIC_WORD);
	}
}
// -------------------------------------------------------------------
bool CLitScan::IsAcceptingState(int ix)
{
	State state = m_states[ix];
	for (StateRec::iterator j = state->begin(); j != state->end(); j++)
	{
		 if (0 == m_pos2sym[*j]) 
			return true;
	}

	return false;
}
// -------------------------------------------------------------------
bool CLitScan::AttatchToFile(const char* filename)
{
	return false;
	// ...
	//m_attached = true;
	//return true;
}
// -------------------------------------------------------------------
bool CLitScan::NextLexem(LgsString& lexem, unsigned char& flag)
{
	if (!m_attached) return false;
	// ...
	return true;
}
// -------------------------------------------------------------------
void CLitScan::AugmentStateTable()
{
   int Increment = 100; //Increment in terms of 100
   for (int ii = m_maxNumStates*256; ii < (Increment + m_maxNumStates)*256; ii++)
		m_trans.push_back(0); // err state
	for (int j = m_maxNumStates; j < m_maxNumStates+Increment; j++)
	{
		m_states.push_back(NULL);
		m_expanded.push_back(false);
	}
   m_maxNumStates += Increment;
}
