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
// LgsTable.cpp: implementation of the CLgsTable class.
//
//////////////////////////////////////////////////////////////////////

#ifdef _MSC_VER
#include <logos_libs/GrammarRules/stdafx.h>
#endif
#include <logos_libs/GrammarRules/LgsLanguage.h>
#include <logos_libs/GrammarRules/LgsVtrStatement.h>
#include <logos_libs/GrammarRules/ExtRec.h>
#include <logos_libs/GrammarRules/LgsTable.h>

IMPLEMENT_SERIAL(CLgsTable,CObject,1);

#define FREE_VTRS() \
if (m_vtrStatement) \
{ \
	for (int i = 0; i < m_numVtrs; i++) \
		delete m_vtrStatement[i]; \
	delete[] m_vtrStatement; \
	m_vtrStatement = NULL; \
	m_numVtrs = 0; \
}

#define FREE_COMMENTS_AND_HINTS() \
if (m_comments) \
{ \
	for (int i = 0; m_hints[i]; i++) \
	{ \
		char ixix = 0xff & m_hints[i]; \
		if (-1 != ixix) \
		{ \
			delete m_comments[ixix - 1]; m_comments[ixix - 1] = NULL; \
		} \
	} \
	delete[] m_comments; m_comments = NULL; \
	delete[] m_hints; m_hints = NULL; \
}

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CLgsTable::CLgsTable()
{
	m_hints = NULL;
	m_comments = NULL;
	m_bStripped = false; // load all stuff by default
    m_id = -1;
    m_level = 0;
	m_tableNo = 0;
    m_description = "";
    m_vtrStatement = NULL;
	m_numVtrs = 0;
	m_ext = NULL;
}

CLgsTable::CLgsTable(int id)
{
	m_hints = NULL;
	m_comments = NULL;
	m_bStripped = false; // load all stuff by default
    m_id = id;
    m_level = 0;
	m_tableNo = 0;
    m_description = "";
    m_vtrStatement = NULL;
	m_numVtrs = 0;
	m_ext = NULL;
}

CLgsTable::CLgsTable(CLgsTable* table)
{
	m_hints = NULL;
	m_comments = NULL;
	m_bStripped = false; // load all stuff by default
    m_id = -1;
    m_level = 0;
	m_tableNo = 0;
    m_description = "";
    m_vtrStatement = NULL;
	m_numVtrs = 0;
	m_ext = NULL;

	// serialize source table to mem buffer
	CMemFile f;
	CArchive arToBuf(&f, CArchive::store);
	table->Serialize(arToBuf);
	arToBuf.Close();

	// serialize from the buffer
	f.Seek(0, CFile::begin);
	UINT fileSize = f.GetLength();

	CArchive arFromBuf(&f, CArchive::load);
	Serialize(arFromBuf);
	arFromBuf.Close();
}

CLgsTable::~CLgsTable()
{
	FREE_VTRS();
	if (m_ext)
      delete m_ext;
	FREE_COMMENTS_AND_HINTS();
}

void CLgsTable::Serialize(CArchive& ar)
{
	short ii;
	unsigned int _ll;
	int hasExtRec;
	int hasHints;

	if (ar.IsStoring())
	{
		ar << m_id;
		ar << m_level;
		ar << m_tableNo;

		hasExtRec = m_ext ? 1 : 0;
		ar << hasExtRec;
		if (m_ext)
			m_ext->Serialize(ar);

		ar << m_description.length();
		if (m_description.length() > 0)
			ar.Write(m_description.c_str(), m_description.length());

		ar << m_numVtrs;
		if (m_numVtrs > 0)
		{
			for (int i = 0; i < m_numVtrs; i++)
				m_vtrStatement[i]->Serialize(ar);
		}

		// vtr hints, and comments
		hasHints = m_comments ? 1 : 0;
		ar << hasHints;
		if (hasHints)
		{
                        int numHints;
			for (numHints = 0; m_hints[numHints]; numHints++);

			int numComments = 0;

			ar << numHints;
                        int i;
			for (i = 0; i < numHints; i++)
			{
				ar << m_hints[i];
				if (0xff != (m_hints[i] & 0xff))
					numComments++;
			}

			ar << numComments;
			for (i = 0; i < numComments; i++)
			{
				ar << m_comments[i]->length();
				if (m_comments[i]->length() > 0)
					ar.Write(m_comments[i]->c_str(), m_comments[i]->length());
			}
		}
	}
	else
	{
		ar >> m_id;
		ar >> m_level;
		ar >> m_tableNo;

		ar >> hasExtRec;
		if (hasExtRec)
		{
			if (m_ext)
            delete m_ext;
			m_ext = new CExtensionRec();
			m_ext->Serialize(ar);
			if (m_bStripped)
			{
				// save memory by discarding extension record
				delete m_ext, m_ext = NULL;
			}
		}

		m_description.erase();
		ar >> _ll;
		if (_ll > 0)
		{
			char* b = new char[_ll+1];
			if (_ll != ar.Read(b, _ll))
				throw("");
			b[_ll] = 0;
			m_description = b;
			delete[] b;
		}

		// vtrs
		ar >> ii;
		FREE_VTRS();
		if (ii > 0)
		{
			m_numVtrs = ii;
			m_vtrStatement = new CLgsVtrStatement* [m_numVtrs];

			for (int i = 0; i < m_numVtrs; i++)
			{
				m_vtrStatement[i] = new CLgsVtrStatement();
				m_vtrStatement[i]->Serialize(ar);
			}
		}

		// vtr hints, and comments
		FREE_COMMENTS_AND_HINTS();
		ar >> hasHints;
		if (hasHints)
		{
			int num;
			ar >> num; // #of hints
			m_hints = new unsigned short [1 + num];
                        int i;
			for (i = 0; i < num; i++)
				ar >> m_hints[i];
			m_hints[i] = 0; // 0-terminate the array

			ar >> num; // #of comments
			m_comments = new LgsString* [num];
			for (i = 0; i < num; i++)
			{
				m_comments[i] = new LgsString();
				m_comments[i]->operator=("");
				ar >> _ll;
				if (_ll > 0)
				{
					char* b = new char[_ll+1];
					if (_ll != ar.Read(b, _ll))
						throw("");
					b[_ll] = 0;
					m_comments[i]->operator=(b);
					delete[] b;
				}
			}

			if (m_bStripped)
			{
				// save space by discarding vtr hints, and comments
				FREE_COMMENTS_AND_HINTS();
			}
		}
	}
}

int CLgsTable::id() { return m_id; }
void CLgsTable::id(int i) { m_id = i; }

int CLgsTable::Level() { return m_level; }
void CLgsTable::Level(int l) { m_level = l; }

LPCTSTR CLgsTable::User()
{
	return m_ext ? m_ext->m_user.c_str() : NULL;
}

void CLgsTable::User(LPCTSTR user)
{
	if (!m_ext)
		m_ext = new CExtensionRec();
	m_ext->m_user = user;
}

CTime* CLgsTable::Created()
{
	return m_ext ? &m_ext->m_created : NULL;
}

void CLgsTable::Created(CTime& date)
{
	if (!m_ext)
		m_ext = new CExtensionRec();
	m_ext->m_created = date;
}

CTime* CLgsTable::LastModified()
{
	return m_ext ? &m_ext->m_lastModified : NULL;
}

void CLgsTable::LastModified(CTime& date)
{
	if (!m_ext)
		m_ext = new CExtensionRec();
	m_ext->m_lastModified = date;
}

LPCTSTR CLgsTable::Notes()
{
	return m_ext ? m_ext->m_notes.c_str() : NULL;
}

void CLgsTable::Notes(LPCTSTR notes)
{
	if (!m_ext)
		m_ext = new CExtensionRec();
	m_ext->m_notes = notes;
}

LPCTSTR CLgsTable::Description()
{
	return m_description.c_str();
}

void CLgsTable::Description(LPCTSTR desc)
{
	m_description = desc;
}

// discarding vtr lines comments
bool CLgsTable::parse(LPCTSTR str)
{
	LgsString strstr = str;

	if (strstr.length() < 12)
		return false;

    int lineNo = 0;
    int start = 0;
    int numLines = 0;
    int i;
    for (i = 0; i < strstr.length(); i++)
        if (0xa == strstr[i]) numLines++;

    if (numLines < 1 || '*' != strstr[0] || ' ' != strstr[1])
		return false;
        
    // split strstr into separate lines, discarding line endings
    LgsString** line = new LgsString* [numLines];

    for (i = 0, lineNo = 0, start = 0; i < strstr.length(); i++)
    {
        if (0xa == strstr[i])
        {
            bool dosEndings = i > 0 && 0xd == strstr[i-1];
            line[lineNo] = new LgsString(
                strstr.substr(start, i - (dosEndings ? 1 : 0) - start));
            start = i + 1;
            lineNo++;
        }
    }

	if (line[0]->length() < 11)
	{
		for (i = 0; i < numLines; i++)
      {
			if (line[i])
            delete line[i];
      }
		delete[] line;
		return false;
	}

	// level
	m_level = atoi(line[0]->substr(2, 2).c_str());

	// table#
	m_tableNo = atoi(line[0]->substr(5, 6).c_str());

    // use table's comment line as description, cut off the level, and table#
    m_description = line[0]->substr(11, line[0]->length() - 11);

	// vtr statements
	FREE_VTRS();

	// count #of vtr numbers, #of comments, and #of vtr lines
    int numVtrNumbers = 0;
    int numComments = 0;
    int numVtrLines = 0;
	int prevFlag = -1; // -1 not set, 0 comment, 1 vtrline
    for (i = 1; i < numLines; i++)
    {
		if ('*' == line[i]->c_str()[0])
		{
			if (prevFlag != 0)
				numComments++;
			prevFlag = 0; // comment
		}
		else
		{
			numVtrNumbers += line[i]->length() / 3;
			LgsString tmp = line[i]->substr(line[i]->length()-3, 3);
			if (line[i]->length() >= 3 &&
				 0 == tmp.compare("999"))
				numVtrNumbers--;

			if (line[i]->length() >= 6)
			{
				tmp = line[i]->substr(line[i]->length()-6, 6);
				if (0 == tmp.compare("888888"))
					numVtrNumbers -= 2;
			}

			//if (prevFlag != 1)
				numVtrLines++;
			prevFlag = 1; // vtr line
		}
    }

	// allocate memory for comments, and hints
	// (only if there are some comments)
	if (numComments > 0 && numVtrNumbers > 0)
	{
		m_comments = new LgsString* [numComments];
		for (int i = 0; i < numComments; i++)
		{
			m_comments[i] = new LgsString();
			m_comments[i]->operator=("");
		}

		m_hints = new unsigned short [1 + numVtrLines];
	}

	// build binary data including vtrs, comments, and hints
	prevFlag = -1; // -1 not set, 0 comment, 1 vtrline
	int cIx = 0;
	int hIx = 0;
    if (numVtrNumbers > 0)
    {
        short* vtrNumbers = new short[numVtrNumbers];
        int ix = 0;
        for (i = 1; i < numLines; i++)
        {
			if ('*' == line[i]->c_str()[0])
			{
				if (0 == prevFlag)
					m_comments[cIx]->append("\n");
				m_comments[cIx] ->append(line[i]->c_str());
				prevFlag = 0; // comment
			}
			else
			{
				// vtr line
                                int j, k;
				for (j = 0, k = 0; j < line[i]->length() / 3; j++)
				{
					if ('A' == line[i]->substr(3*j, 3).c_str()[0])
					{
						vtrNumbers[ix++] =
							-1000 - atoi(line[i]->substr(3*j, 3).c_str() + 1);
						k++;
					}
					else
					{
						int vtrnum = atoi(line[i]->substr(3*j, 3).c_str());
						if (999 == vtrnum && j == line[i]->length()/3 - 1)
							break;
						if (888 == vtrnum &&
							  j == line[i]->length()/3 - 2 &&
							  888 == atoi(line[i]->substr(3*(j+1), 3).c_str()))
							break;
						vtrNumbers[ix++] = vtrnum;
						k++;
					}
				}

				if (m_hints)
				{
					unsigned short x =
						(k << 8) | (0 == prevFlag ? (cIx+1) : 0xff);
					m_hints[hIx++] = x;
				}
				if (0 == prevFlag)
					cIx++;
				prevFlag = 1; // vtr line
			}
        }

		if (m_hints)
		{
			m_hints[hIx] = 0; // 0-terminate it
		}

		CLgsVtrStatement::parseVtrNumbers(
			vtrNumbers, numVtrNumbers, &m_vtrStatement, &m_numVtrs);
    }

	if (line)
	{
		for (i = 0; i < numLines; i++)
			if (line[i])
            delete line[i];
		delete[] line;
	}

	return true;
}

LPTSTR CLgsTable::toString()
{
	LgsString resultStr;
	char bbb[128];

    // build comment line
	LgsString levStr = _itoa(m_level, bbb, 10);
	LgsString tabNoStr = _itoa(m_tableNo, bbb, 10);
    LgsString commentLine = "* " + levStr + " " + tabNoStr + " " + m_description;
	LgsString restOfIt;
    
	if (m_vtrStatement)
	{
		for (int i = 0; i < m_numVtrs; i++)
		{
			restOfIt += m_vtrStatement[i]->Description();
			short* nums;
			int numNums = m_vtrStatement[i]->GetNumbers(&nums);
			for (int j = 0; j < numNums; j++)
			{
				restOfIt += _itoa(nums[j], bbb, 10);
				restOfIt += ",";
			}
			if (m_numVtrs - 1 == i) restOfIt += "999";
			restOfIt += "\n";
		}
	}

	resultStr = commentLine;
	resultStr += "\n";
	resultStr += restOfIt;

	LPTSTR result = new char[1 + resultStr.length()];
	strcpy(result, resultStr.c_str());
	return result;
}

int CLgsTable::VtrStatements(CLgsVtrStatement*** vtrs)
{
	*vtrs = m_vtrStatement;
	return m_numVtrs;
}

void CLgsTable::VtrStatements(CLgsVtrStatement** vtrs, int num)
{
	FREE_VTRS();
	m_vtrStatement = vtrs;
	m_numVtrs = num;
}

int CLgsTable::TableNo()
{
	return m_tableNo;
}

void CLgsTable::TableNo(int no)
{
	m_tableNo = no;
}

void CLgsTable::SetStripped(bool stripped)
{
	m_bStripped = stripped;
}

void CLgsTable::GetHints(unsigned short** hints, LgsString*** comments)
{
	*hints = m_hints;
	*comments = m_comments;
}

void CLgsTable::SetHints(unsigned short* hints, LgsString** comments)
{
   m_hints = hints;
   m_comments = comments;
}
