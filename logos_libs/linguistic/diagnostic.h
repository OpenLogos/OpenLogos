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
#ifndef __diagnostic_h__
#define __diagnostic_h__

//---------------------------------------------------------------------
// File - diagnostic.h
//
// Class - Diagnostic (interface)
//
//---------------------------------------------------------------------

#include <logos_libs/linguistic/lword.h>

class LSentence;
class SourceUnitVector;

class Diagnostic
{
public:
   Diagnostic();
	Diagnostic(int diagType, const LgsString& streamName,
				  int startingLine = 0, int endingLine = 0);
	virtual ~Diagnostic();

	bool isDiagnosticsOn();
	bool isOutputBlocked() const { return v_blockOutput; }

	virtual void dictionaryCompare(const SourceUnitVector& vec);
	virtual void write(const LgsString& str);
   virtual void writeAlways(const LgsString& str);
	virtual void writeLine(const LgsString& str);
	virtual void writeBrokenLines(const LgsString& str);
	virtual void wordsAsString(const LWordVector& words, bool withTypes = false);
	virtual void dictionaryAsString(const SourceUnitVector& vec);
	virtual void lookupSentence(LSentence& sent);
	virtual void generateSentence(LSentence& sent);
	virtual void sconCompare(LSentence& sent);
	virtual void lookupSentenceUnit(const SourceUnitVector& vec);
	virtual void lookupSummary(const SourceUnitVector& vec);
	virtual void flush();
	void setCurrLineNumber(int lineNo);
	ostream& stream();

protected:
	enum { LineSize = 100 };

private:
	bool v_blockOutput;
	bool v_outputRestricted;
	int v_startLine;
	int v_endLine;
	int v_currLine;
	ostream* p_stream;
   bool v_diagnosticsOn;
};

//---------------------------------------------------------------------
inline ostream& Diagnostic::stream()
{
	return *p_stream;
}
//---------------------------------------------------------------------
inline bool Diagnostic::isDiagnosticsOn()
{
   return v_diagnosticsOn;
}
#endif // __diagnostic_h__

