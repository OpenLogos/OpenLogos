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
// LgsRule.cpp: implementation of the CLgsRule class.
//
//////////////////////////////////////////////////////////////////////

#ifdef _MSC_VER
#include <logos_libs/GrammarRules/stdafx.h>
#endif
#include <logos_libs/GrammarRules/LgsLanguage.h>
#include <logos_libs/GrammarRules/LgsTypeStatement.h>
#include <logos_libs/GrammarRules/LgsSpElement.h>
#include <logos_libs/GrammarRules/LgsVtrStatement.h>
#include <logos_libs/GrammarRules/LgsSortKeyTemplate.h>
#include <logos_libs/GrammarRules/ExtRec.h>
#include <logos_libs/GrammarRules/LgsRule.h>

// calling rule level for TRAN rules / specificity for RES rules
#define m_aliasSpecAdj m_callingRuleLevel

// CLgsRule stuff
IMPLEMENT_SERIAL(CLgsRule,CObject,1);

#define FREE_SPS()						\
if (m_spElement)						\
{										\
	for (i = 0; i < m_numSps; i++)	\
		delete m_spElement[i];			\
	delete[] m_spElement;				\
	m_spElement = NULL;					\
	m_numSps = 0;						\
}

#define FREE_VTRS()						\
if (m_vtrStatement)						\
{										\
	for (i = 0; i < m_numVtrs; i++)	\
		delete m_vtrStatement[i];		\
	delete[] m_vtrStatement;				\
	m_vtrStatement = NULL;				\
	m_numVtrs = 0;						\
}

CLgsRule::CLgsRule()
{
	// need c_str() be NULL for uninitialized var
   //m_description = "";
   m_id = -1;
   m_level = -1;
   m_callingRuleLevel = -1;
   m_spElement = NULL;
   m_numSps = 0;
   m_vtrStatement = NULL;
   m_numVtrs = 0;
   m_ext = NULL;
}

CLgsRule::CLgsRule(int id)
{
   //m_description = "";
   m_id = id;
   m_level = -1;
   m_callingRuleLevel = -1;
   m_spElement = NULL;
   m_numSps = 0;
   m_vtrStatement = NULL;
   m_numVtrs = 0;
   m_ext = NULL;
}

CLgsRule::CLgsRule(CLgsRule* rule)
{
   //m_description = "";
   m_id = -1;
   m_level = -1;
   m_callingRuleLevel = -1;
   m_spElement = NULL;
   m_numSps = 0;
   m_vtrStatement = NULL;
   m_numVtrs = 0;
   m_ext = NULL;

	// serialize source rule to mem buffer
	CMemFile f;
	CArchive arToBuf(&f, CArchive::store);
	rule->Serialize(arToBuf);
	arToBuf.Close();

	// serialize from the buffer
	f.Seek(0, CFile::begin);
	UINT fileSize = f.GetLength();

	CArchive arFromBuf(&f, CArchive::load);
	Serialize(arFromBuf);
	arFromBuf.Close();
}

CLgsRule::~CLgsRule()
{
	int i;
	FREE_SPS();
	FREE_VTRS();
	if (m_ext)
      delete m_ext;
}

void CLgsRule::Serialize(CArchive& ar)
{
	short ii;
	int i;
	unsigned int _ll;
	int hasExtRec;

	if (ar.IsStoring())
	{
		ar << m_id;
		ar << m_level;

//// TEMP !!! now storing specAdj instead of spec
//m_aliasSpecAdj =
//	m_description.length() > 1 && '+' == m_description.c_str()[0] ?
//		atoi(m_description.c_str()+1) : 0;
//// TEMP !!!

		ar << m_callingRuleLevel;

		hasExtRec = m_ext ? 1 : 0;
		ar << hasExtRec;
		if (m_ext)
			m_ext->Serialize(ar);

		// sps
		ar << m_numSps;
		if (m_numSps > 0)
		{
			for (i = 0; i < m_numSps; i++)
				m_spElement[i]->Serialize(ar);
		}

		// vtrs
		ar << m_numVtrs;
		if (m_numVtrs > 0)
		{
			for (i = 0; i < m_numVtrs; i++)
				m_vtrStatement[i]->Serialize(ar);
		}

		// ==> temporarily here
		// make all multiple sequences of spaces just one space
		//
		// this code should be removed, after all rule/table files
		// are converted to new format
		int j;
		LPTSTR des = new char[1+m_description.length()];
		for (i = 0, j = 0; i < m_description.length(); i++)
		{
			if (0 == i || ' ' != m_description[i-1] || ' ' != m_description[i])
				des[j++] = m_description[i];
		}
		des[j] = 0;
		ar << j;
		if (j > 0)
			ar.Write(des, j);
		delete[] des;
		// <== temporarily here, replace with the following line by uncommenting it
		//ar << m_description.length();
		//if (m_description.length() > 0)
		//	ar.Write(m_description.c_str(), m_description.length());
	}
	else
	{
		ar >> m_id;
		ar >> m_level;
		ar >> m_callingRuleLevel;

		ar >> hasExtRec;
		if (hasExtRec)
		{
			if (m_ext)
            delete m_ext;
			m_ext = new CExtensionRec();
			m_ext->Serialize(ar);
		}

		// sps
		ar >> ii;
		FREE_SPS();
		if (ii > 0)
		{
			m_numSps = ii;
			m_spElement = new CLgsSpElement* [m_numSps];
			for (i = 0; i < m_numSps; i++)
			{
				m_spElement[i] = new CLgsSpElement();
				m_spElement[i]->Serialize(ar);
			}
		}

		// vtrs
		ar >> ii;
		FREE_VTRS();
		if (ii > 0)
		{
			m_numVtrs = ii;
			m_vtrStatement = new CLgsVtrStatement* [m_numVtrs];

			for (i = 0; i < m_numVtrs; i++)
			{
				m_vtrStatement[i] = new CLgsVtrStatement();
				m_vtrStatement[i]->Serialize(ar);
			}
		}

		// description
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
	}
}

int CLgsRule::id()
{
   return m_id;
}

void CLgsRule::id(int i)
{
   m_id = i;
}

int CLgsRule::Level()
{
   return m_level;
}

void CLgsRule::Level(int l)
{
   m_level = l;
}

int CLgsRule::SpecificityAdjustment()
{
   return m_aliasSpecAdj;
}

void CLgsRule::SpecificityAdjustment(int specadj)
{
   m_aliasSpecAdj = specadj;
}

int CLgsRule::CallingRuleLevel()
{
   return m_callingRuleLevel;
}

void CLgsRule::CallingRuleLevel(int l)
{
   m_callingRuleLevel = l;
}

int CLgsRule::Specificity()
{
	// make sense for res rules only
	return CalcSpec(m_aliasSpecAdj);
}

LPCTSTR CLgsRule::User()
{
	return m_ext ? m_ext->m_user.c_str() : NULL;
}

void CLgsRule::User(LPCTSTR user)
{
	if (!m_ext)
		m_ext = new CExtensionRec();
	m_ext->m_user = user;
}

CTime* CLgsRule::Created()
{
	return m_ext ? &m_ext->m_created : NULL;
}

void CLgsRule::Created(CTime& date)
{
	if (!m_ext)
		m_ext = new CExtensionRec();
	m_ext->m_created = date;
}

CTime* CLgsRule::LastModified()
{
	return m_ext ? &m_ext->m_lastModified : NULL;
}

void CLgsRule::LastModified(CTime& date)
{
	if (!m_ext)
		m_ext = new CExtensionRec();
	m_ext->m_lastModified = date;
}

LPCTSTR CLgsRule::Notes()
{
	return m_ext ? m_ext->m_notes.c_str() : NULL;
}

void CLgsRule::Notes(LPCTSTR notes)
{
	if (!m_ext)
		m_ext = new CExtensionRec();
	m_ext->m_notes = notes;
}

LPCTSTR CLgsRule::Description()
{
	return m_description.c_str();
}

void CLgsRule::Description(LPCTSTR desc)
{
	m_description = desc;
}

bool CLgsRule::parse(LPCTSTR str, bool isTranRule)
{
	LgsString strstr = str;
   int lineNo = 0;
   int start = 0;
   int numLines = 0;
   LgsString** line = NULL;

   int i;
   for (i = 0; i < strstr.length(); i++)
   {
      if (0xa == strstr[i])
         numLines++;
   }

   if (numLines < 3 || ' ' != strstr[2])
      return false;

   // split strstr into separate lines, discarding line endings
   line = new LgsString* [numLines];

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

   // sp elements array
   int numSps = line[1]->length() / 7;
   m_spElement = new CLgsSpElement* [numSps];
   m_numSps = numSps;

   int tline = 0;
   bool wc9or10 = false;
   for (i = 0; i < numSps; i++)
   {
      short* tss = NULL;
      int wc = atoi(line[1]->substr(7*i+0, 2).c_str());
      int ty = atoi(line[1]->substr(7*i+2, 3).c_str());
      int fc = atoi(line[1]->substr(7*i+5, 2).c_str());

      if (9 == wc || 10 == wc)
         wc9or10 = true;

      int numTags = 0;
      if (-2 == ty)
      {
         // reference to a tagset
         LgsString tmpStr;
         int ixix;
         char hints[256];
         hints[0] = 0;

         // count #of tags
         for (numTags = 0, ixix = 0,
              tmpStr = line[2+tline + ixix]->substr(isTranRule? line[2+tline + ixix]->length() - 4 : 0, 4);
              0 == tmpStr.compare("8888");
              ixix++, tmpStr = line[2+tline + ixix]->substr(isTranRule? line[2+tline + ixix]->length() - 4 : 0, 4))
            numTags += line[2+tline + ixix]->length() / 4; // - 1;
         numTags += line[2+tline + ixix]->length() / 4;

         // alloc and populate tags array
         tss = new short[numTags];

         int k = 0;
         int numTagsInThisLine;
         for (ixix = 0,
              tmpStr = line[2+tline + ixix]->substr(isTranRule? line[2+tline + ixix]->length() - 4 : 0, 4);
              0 == tmpStr.compare("8888"); ixix++,
              tmpStr = line[2+tline + ixix]->substr(isTranRule? line[2+tline + ixix]->length() - 4 : 0, 4))
         {
            numTagsInThisLine = line[2+tline + ixix]->length() / 4; // - 1;
            for (int j = 0; j < numTagsInThisLine; j++)
               tss[k++] = atoi(line[2+tline + ixix]->substr(4*j, 4).c_str());
            hints[ixix] = numTagsInThisLine;
         }
         numTagsInThisLine = line[2+tline + ixix]->length() / 4;
         for (int j = 0; j < numTagsInThisLine; j++)
            tss[k++] = atoi(line[2+tline + ixix]->substr(4*j, 4).c_str());
         hints[ixix] = numTagsInThisLine;
         hints[ixix + 1] = 0;

         tline += ixix + 1;
         m_spElement[i] = new CLgsSpElement(wc, fc, tss, numTags, hints);
      }
      else
      {
         // single number type value
         numTags = 1;
         tss = new short[1];
         tss[0] = ty;
         m_spElement[i] = new CLgsSpElement(wc, fc, tss, numTags, NULL);
      }
   }

   // m_level
   m_level = atoi(line[0]->substr(0, 2).c_str());

   // calling rule level
   if (isTranRule && wc9or10)
      m_callingRuleLevel = atoi(line[0]->substr(3, 2).c_str());

   // use comment line as description, with level cut
   m_description = line[0]->substr(3, line[0]->length() - 3);

   // count # of vtr numbers, and
   // alloc and populate array of all vtr numbers,
   int firstVtrLineIx = 2 + tline;
   int numVtrNumbers = 0;
   for (i = firstVtrLineIx; i < numLines; i++)
   {
      numVtrNumbers += line[i]->length() / 3;
      LgsString tmp = line[i]->substr(line[i]->length()-3, 3);
      if (line[i]->length() >= 3 && 0 == tmp.compare("999"))
         numVtrNumbers--;
      if (line[i]->length() >= 6)
      {
         tmp = line[i]->substr(line[i]->length()-6, 6);
         if (line[i]->length() >= 6 && 0 == tmp.compare("888888"))
            numVtrNumbers -= 2;
      }
   }

   if (numVtrNumbers > 0)
   {
      short* vtrNumbers = new short[numVtrNumbers];
      int ix = 0;
      for (i = firstVtrLineIx; i < numLines; i++)
      {
         for (int j = 0; j < line[i]->length() / 3; j++)
         {
            int vtrnum = atoi(line[i]->substr(3*j, 3).c_str());
            if (999 == vtrnum && j == line[i]->length()/3 - 1)
               continue;
            if (888 == vtrnum && j == line[i]->length()/3 - 2)
               break;
            vtrNumbers[ix++] = vtrnum;
         }
      }

      CLgsVtrStatement::parseVtrNumbers(vtrNumbers, numVtrNumbers, &m_vtrStatement, &m_numVtrs);
   }

   if (line)
   {
      for (i = 0; i < numLines; i++)
      {
         if (line[i])
            delete line[i];
      }
      delete[] line;
   }

   // extract specificity adjustment from comment line for res rules
   if (!isTranRule)
   {
      m_aliasSpecAdj =
      m_description.length() > 1 && '+' == m_description.c_str()[0] ?
      atoi(m_description.c_str()+1) : 0;
   }

   return true;
}

LPTSTR CLgsRule::toString()
{
  LgsString ruStr;
  char bbb[128];
  
  // build comment line
  LgsString levStr = _itoa(m_level, bbb, 10);
  LgsString commentLine = levStr + " " + m_description;
    
  // build sp line and tagsets line, if any
  LgsString spLine;
  LgsString tagsetLines;
    
  if (m_spElement)
    {
      for (int i = 0; i < m_numSps; i++)
        {
          LgsString type;
          LgsString wc = _itoa(m_spElement[i]->WordClass(), bbb, 10);
          spLine += "(" + wc + ",";
            
          CLgsTypeStatement* ts = m_spElement[i]->TypeStatement();
          short* nums;
          int numNums = ts->GetNumbers(&nums);
          if (1 == numNums && !ts->GetHints())
            type = _itoa(nums[0], bbb, 10);
          else
            type = "-2";
            
          spLine += type + ",";
            
          if (1 != numNums)
            {
              for (int j = 0; j < numNums; j++)
                {
                  tagsetLines += _itoa(nums[j], bbb, 10);
                  if (j < numNums -1)
                    tagsetLines += ",";
                }
              tagsetLines += "\xd\xa";
            }
            
          LgsString fc = _itoa(m_spElement[i]->FormCode(), bbb, 10);
          spLine += fc + ")";
        }
    }

  // build vtr line
  LgsString vtrLine;
  if (m_vtrStatement)
    {
      char tmp[256];
      short* vtrNums;
      int numVtrNums = m_vtrStatement[0]->GetNumbers(&vtrNums);
      for (int i = 0; i < numVtrNums; i++)
        {
          if (i > 0 && vtrNums[i] >= CLgsVtrStatement::MinSwVal && vtrNums[i] <= CLgsVtrStatement::MaxSwVal)
            {
              // start a new line on a switch boundery
              vtrLine += "\xd\xa";
            }
          sprintf(tmp, "%03d", vtrNums[i]);
          vtrLine += tmp;
        }
    }
  //vtrLine += "999";

  ruStr += commentLine;
  ruStr += "\xd\xa";
  ruStr += spLine;
  ruStr += "\xd\xa";
  ruStr += tagsetLines;
  ruStr += vtrLine;
  ruStr += "\xd\xa";
  LPTSTR result = new char[1 + ruStr.length()];
  strcpy(result, ruStr.c_str());
  return result;
}

LPTSTR CLgsRule::CommentDisplayString(bool)
{
	LPTSTR result = new char[1 + m_description.length()];
	strcpy(result, m_description.c_str());
	return result;
/*
	LgsString coStr;
	char bbb[128];
	LgsString levStr = _itoa(m_level, bbb, 10);
    coStr += levStr + " " + m_description;
	LPTSTR result = new char[1 + coStr.length()];
	strcpy(result, coStr.c_str());
	return result;
*/
}

LPTSTR CLgsRule::SpsDisplayString(bool)
{
   LgsString spStr;
	char bbb[128];
    
   if (m_spElement)
   {
      for (int i = 0; i < m_numSps; i++)
      {
         LgsString type;
         LgsString wc = _itoa(m_spElement[i]->WordClass(), bbb, 10);
         spStr += "(" + wc + " ";
            
         CLgsTypeStatement* ts = m_spElement[i]->TypeStatement();
         short* nums;
			int numNums = ts->GetNumbers(&nums);
         if (1 == numNums && !ts->GetHints())
            type = _itoa(nums[0], bbb, 10);
         else
            type = "-2";

         spStr += type + " ";

         LgsString fc = _itoa(m_spElement[i]->FormCode(), bbb, 10);
         spStr += fc + ")";
			if (i != m_numSps-1)
				spStr += "  ";
      }
   }

	LPTSTR result = new char[1 + spStr.length()];
	strcpy(result, spStr.c_str());
	return result;
}

LPTSTR CLgsRule::TagsetsDisplayString(bool rtf, bool cr)
{
   LgsString tsStr;
	char bbb[128];
	const char* del = rtf ? " \\par " : cr ? "\xd\xa" : "\n";

   if (m_spElement)
   {
      for (int i = 0, numTagsets = 0; i < m_numSps; i++)
      {
         CLgsTypeStatement* ts = m_spElement[i]->TypeStatement();
         short* nums;
			int numNums = ts->GetNumbers(&nums);

         if (!(1 == numNums && !ts->GetHints()))
         {
      		if (numTagsets > 0)
					tsStr += del;
				tsStr += rtf ? "\\{ " : "{ ";
            for (int j = 0; j < numNums; j++)
            {
		      	if (8888 != nums[j])
					{
						tsStr += _itoa(nums[j], bbb, 10);
						if (j < numNums -1)
                     tsStr += " ";
					}
            }
				tsStr += rtf ? " \\}" : " }";
				numTagsets++;
         }
      }
   }

	LPTSTR result = new char[1 + tsStr.length()];
	strcpy(result, tsStr.c_str());
	return result;
}

LPTSTR CLgsRule::VtrsDisplayString(bool rtf, bool cr)
{
   LgsString vtStr;
   const char* del = rtf ? " \\par " : cr ? "\xd\xa" : "\n";
   if (m_vtrStatement)
   {
		char tmp[256];
		short* vtrNums;
      int numVtrNums = m_vtrStatement[0]->GetNumbers(&vtrNums);
		int t_norm = 16;
		int t_table = 3;
		if (numVtrNums > 0)
		{
			int t = -63 == vtrNums[0] || -64 == vtrNums[0] || -65 == vtrNums[0] ? t_table : t_norm;
			for (int i = 0, numNumsInThisLine = 0; i < numVtrNums; i++)
			{
				bool x;
				if (i > 0 && ((x = -63 == vtrNums[i] || -64 == vtrNums[i] || -65 == vtrNums[i]) ||
					           (t == t_table && numNumsInThisLine > t) ||
					           (vtrNums[i] >= CLgsVtrStatement::MinSwVal &&
						         vtrNums[i] <= CLgsVtrStatement::MaxSwVal && numNumsInThisLine > t)))
				{
					t = x ? t_table : t_norm;

					vtStr += del;
					numNumsInThisLine = 0;
				}
				sprintf(tmp, "%03d ", vtrNums[i]);
				vtStr += tmp;
				numNumsInThisLine++;
			}
		}
   }

	LPTSTR result = new char[1 + vtStr.length()];
	strcpy(result, vtStr.c_str());
	return result;
}

int CLgsRule::SpStatements(CLgsSpElement*** sps)
{
	*sps = m_spElement;
	return m_numSps;
}

void CLgsRule::SpStatements(CLgsSpElement** sps, int num)
{
	int i;
	FREE_SPS();
	m_spElement = sps;
	m_numSps = num;
}

int CLgsRule::VtrStatements(CLgsVtrStatement*** vtrs)
{
	*vtrs = m_vtrStatement;
	return m_numVtrs;
}

void CLgsRule::VtrStatements(CLgsVtrStatement** vtrs, int num)
{
	int i;

	FREE_VTRS();
	m_vtrStatement = vtrs;
	m_numVtrs = num;
}

int CLgsRule::Compare(CLgsRule * r, CLgsSortKeyTemplate * t)
{
	int numItems = t->m_matchItems.size();

	for (int i = 0; i < numItems; i++)
	{
		CLgsSortKeyTemplate::MatchItem* mit = t->m_matchItems[i];
		if (MIT_SPELEMEMT == mit->type)
		{
			if (mit->no >= m_numSps && mit->no < r->m_numSps)
            return -1;
			else if (mit->no < m_numSps && mit->no >= r->m_numSps)
            return 1;
			else if (mit->no < m_numSps && mit->no < r->m_numSps)
			{
				CLgsSpElement* sp = m_spElement[mit->no];
				CLgsSpElement* r_sp = r->m_spElement[mit->no];
				if (mit->flags & SPF_WC)
				{
					int wc = sp->WordClass();
					int r_wc = r_sp->WordClass();

					if (wc < 0 && r_wc < 0)
					{
						wc = -wc;
						r_wc = -r_wc;
					}

					if (wc < r_wc)
                  return -1;
					else if (wc > r_wc)
                  return 1;
				}
				if (mit->flags & SPF_TY)
				{
					short* nums;
					int numNums = sp->TypeStatement()->GetNumbers(&nums);
					int ty = !(numNums == 1 && !sp->TypeStatement()->GetHints()) ? -2 : nums[0];
					short* r_nums;
					int r_numNums = r_sp->TypeStatement()->GetNumbers(&r_nums);
					int r_ty = !(r_numNums == 1 && !r_sp->TypeStatement()->GetHints()) ? -2 : r_nums[0];

					if (ty < 0 && r_ty < 0)
					{
						ty = -ty;
						r_ty = -r_ty;
					}

					if (ty < r_ty)
                  return -1;
					else if (ty > r_ty)
                  return 1;
				}
				if (mit->flags & SPF_FC)
				{
					int fc = sp->FormCode();
					int r_fc = r_sp->FormCode();

					if (fc < 0 && r_fc < 0)
					{
						fc = -fc;
						r_fc = -r_fc;
					}

					if (fc < r_fc)
                  return -1;
					else if (fc > r_fc)
                  return 1;
				}
			}
		}
		else if (MIT_LEVEL == mit->type)
		{
			if (m_level < r->m_level)
            return -1;
			else if (m_level > r->m_level)
            return 1;
		}
		else // MIT_TAGSET
		{
			short* nums = NULL;
			int numNums = 0;
			short* r_nums = NULL;
			int r_numNums = 0;

			if (mit->no < m_numSps)
			{
				for (int i = 0, k = 0; i < m_numSps; i++)
				{
					numNums = m_spElement[i] ->
								TypeStatement()->GetNumbers(&nums);
					if (!(numNums == 1 && !m_spElement[i]->TypeStatement()->GetHints()))
					{
						if (mit->no == k)
							break;
						k++;
					}
					nums = NULL;
					numNums = 0;
				}
			}

			if (mit->no < r->m_numSps)
			{
				for (int i = 0, k = 0; i < r->m_numSps; i++)
				{
					r_numNums = r->m_spElement[i] ->
									TypeStatement()->GetNumbers(&r_nums);
					if (!(r_numNums == 1 && !r->m_spElement[i]->TypeStatement()->GetHints()))
					{
						if (mit->no == k)
							break;
						k++;
					}
					r_nums = NULL;
					r_numNums = 0;
				}
			}

			if (!numNums && r_numNums)
            return -1;
			else if (numNums && !r_numNums)
            return 1;
			else if (numNums && r_numNums)
			{
				for (int j = 0; j < numNums && j < r_numNums; j++)
				{
					if (nums[j] < r_nums[j])
                  return -1;
					else if (nums[j] > r_nums[j])
                  return 1;
				}
				if (numNums < r_numNums)
               return -1;
				else if (numNums > r_numNums)
               return 1;
			}
		}
	}

	return 0;
}

int CLgsRule::CalcSpec(int specad)
{
	int q = Level();
	if (q < 10)
      q -= ((q-1)/10)*10;
	int j = 2;
	int k = 3;
	int l = 4;
	int m = 1;
	int n = 2;
	int o = 3;
	int sv = specad;
	int ruleSpec = 0;

	CLgsSpElement** sps;
	int numSps = SpStatements(&sps);

	for (int i = 0; i < numSps; i++)
	{
		short* nums;
		int numNums = sps[i]->TypeStatement()->GetNumbers(&nums);

		if (!(numNums == 1 && !sps[i]->TypeStatement()->GetHints()))
			ruleSpec++;

		int wc = sps[i]->WordClass();
		int ty = numNums > 1 ? -2 : nums[0];
		int fc = sps[i]->FormCode();

		// piece of old FORTRAN code from res2gen/resspec.f
		// is still here (in C notation)
		if (wc == 1)
         goto lab101;
		if (wc == 2)
         goto lab102;
		if (wc == 3)
         goto lab102;
		if (wc == -9)
         goto lab1021;
		if (wc > 2)
         goto lab103;
		goto lab104;
lab101:   sv++;
		if (ty == 1)
         sv++;
		if (ty == 1)
         goto lab1011;
		if (ty == 13)
         sv++;
		if (ty == 13)
         goto lab1011;
		if (ty == 101)
         sv++;
		if (ty == 101)
         goto lab1011;
		if (ty == 102)
         sv++;
		if (ty == 102)
         goto lab1011;
		if (ty == 103)
         sv++;
		if (ty == 103)
         goto lab1011;
		if (ty  ==  997)
         sv++;
		if (ty  ==  997)
         goto lab1011;
		if (ty > 1)
         sv += 2;
lab1011:
		if (fc > 0)
         sv++;
		continue;
lab102:
		sv++;
lab1021:
		if (ty  >  0)
         sv += 2;
		if (ty  ==  31 || ty  ==  845)
         sv--;
		if (fc > 0)
         sv++;
		continue;
lab103:
		sv++;
lab104:
		if (ty > 0)
         sv += 2;
		if (wc  ==  -5 && ty  ==  997)
         sv--;
		if (wc  ==  -3 && ty  ==  998)
         sv--;
		if (fc > 0)
         sv++;
	}

	return sv + 2 * ruleSpec;
}

bool CLgsRule::IsEqualTo(CLgsRule* r)
{
	// dummy block comment rule equals to any rule
	// to prevent it's merge to main
	if (!m_description.c_str()[0] || !r->m_description.c_str()[0])
		return true;

	if (m_level != r->m_level)
		return false;
	if (m_callingRuleLevel != r->m_callingRuleLevel)
		return false;
	if (m_numSps != r->m_numSps)
		return false;
	if (m_numVtrs != r->m_numVtrs)
		return false;

	for (int i = 0; i < m_numSps; i++)
	{
		if (!m_spElement[i]->IsEqualTo(r->m_spElement[i]))
			return false;
	}

	return true;
}
