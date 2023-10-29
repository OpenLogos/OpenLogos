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
// LitScan.h: interface for the CLitScan class.
//
//////////////////////////////////////////////////////////////////////

#ifndef __lgsscan_h__
#define __lgsscan_h__

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#define SCF_LITERAL      1
#define SCF_PUNCTUATION  2
#define SCF_GENERIC_WORD 3
#define SCF_SPACES       4

class CLitScan  
{
public:
	CLitScan();
	virtual ~CLitScan();

   bool Init(int numStates, LgsVector(LgsString)& lits);
	void Scan(const LgsString& input, LgsVector(LgsString)& lexems, LgsVector(unsigned char)& flags);

	// these two aren't implemented yet, don't use them
	bool AttatchToFile(const char* filename);
	bool NextLexem(LgsString& lexem, unsigned char& flag);

	typedef LgsVector(int) StateRec, *State;

private:
	bool IsAcceptingState(int ix);
	LgsVector(int)* NextPos(State state, unsigned char sym);
	int IsNewState(State state);
	void Expand(int stateIx);
	int NextUState();
   void AugmentStateTable();
	LgsVector(bool) m_expanded;  //bool* m_expanded;
	LgsVector(int) m_trans;      //int* m_trans;
	LgsVector(State) m_states;   //State* m_states;

   bool m_attached;
	bool m_term[256];
	int m_maxNumStates;
	int m_numStates;
	int m_startIx;
	unsigned char* m_pos2sym;	// array which maps position to symbol value
	int m_numPos;	// #of entries in m_pos2sym array
};

#endif

