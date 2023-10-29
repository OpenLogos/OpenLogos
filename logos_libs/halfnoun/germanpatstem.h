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
#ifndef __GermanPatStem_h__
#define __GermanPatStem_h__

#include <logos_libs/halfnoun/germanflag.h>

//---------------------------------------------------------------------
// File - GermanPatStem.h
//
// class - GermanWordPatStem
// class - GermanPatStem
// class - GermanPatStemLetter
//
// Description - classes used to hold German Pat and Stem (and last letter type)
//             - key for various tables
//
//---------------------------------------------------------------------

const int maxStem = 100;
const int maxLetter = /* 256 */ 1000;

//---------------------------------------------------------------------
class GermanWordPatStem
{
public:
    DefaultConstructor(GermanWordPatStem)
    GermanWordPatStem(const LgsString& company, const LgsString& word, int pat, int stem);

    bool operator<(const GermanWordPatStem& rhs) const;

private:
    LgsString companyWord_;
    int patStem_;
};

inline GermanWordPatStem::GermanWordPatStem(const LgsString& company, const LgsString& word, int pat, int stem)
    : companyWord_(company + word)
    , patStem_(pat * maxStem + stem)
{
}

inline bool GermanWordPatStem::operator<(const GermanWordPatStem& rhs) const
{
    int comp = companyWord_.compare(rhs.companyWord_);
    if (comp < 0)
        return true;
    else if (comp > 0)
        return false;
    else
        return patStem_ < rhs.patStem_;
}

//---------------------------------------------------------------------
class GermanPatStem
{
public:
    DefaultConstructor(GermanPatStem)
    GermanPatStem(int pat, int stem);

    bool operator<(const GermanPatStem& rhs) const;

private:
    int patStem_;
};

inline GermanPatStem::GermanPatStem(int pat, int stem)
    : patStem_(pat * maxStem + stem)
{
}

inline bool GermanPatStem::operator<(const GermanPatStem& rhs) const
{
    return patStem_ < rhs.patStem_;
}

//---------------------------------------------------------------------
class GermanPatStemLetter
{
public:
    DefaultConstructor(GermanPatStemLetter)
    // lastLetter corresponds to the last letter of the search word in lower case
    // lastLetter = 0 is a special value that means all other letters - ie default case
    GermanPatStemLetter(int pat, int stem, char lastLetter);

    bool operator<(const GermanPatStemLetter& rhs) const;

private:
    int patStemLetter_;
};

inline GermanPatStemLetter::GermanPatStemLetter(int pat, int stem, char lastLetter)
    : patStemLetter_((pat * maxStem + stem) * maxLetter + lastLetter)
{
}

inline bool GermanPatStemLetter::operator<(const GermanPatStemLetter& rhs) const
{
    return patStemLetter_ < rhs.patStemLetter_;
}

class GermanSuffixPatStemConn
{
public:
   GermanSuffixPatStemConn(const LgsString & suffix, int pat, int stem, const GermanConnectorFlag & connectorFlag);
   GermanSuffixPatStemConn(const LgsString & suffix, int pat, int stem);
   int pat() const;
   int stem() const;
   const LgsString & suffix() const;
   const GermanConnectorFlag & connectorFlag(void) const;
   bool operator <(const GermanSuffixPatStemConn & rhs) const;
   bool setCommonSuffix(const LgsString & suffixVal);
private:
   LgsString m_suffix;
   int m_pat;
   int m_stem;
   GermanConnectorFlag m_connectorFlag;
};

inline GermanSuffixPatStemConn::GermanSuffixPatStemConn(const LgsString & suffix, int pat, int stem,
                                                        const GermanConnectorFlag & connectorFlag):
   m_suffix(suffix), m_pat(pat), m_stem(stem), m_connectorFlag(connectorFlag)
{

}
inline GermanSuffixPatStemConn::GermanSuffixPatStemConn(const LgsString & suffix, int pat, int stem):
   m_suffix(suffix), m_pat(pat), m_stem(stem)
{

}

inline const LgsString & GermanSuffixPatStemConn::suffix(void) const
{
   return m_suffix;
}

inline bool GermanSuffixPatStemConn::operator <(const GermanSuffixPatStemConn & rhs) const
{
   bool retFlag = true;
   if ((retFlag = m_pat < rhs.m_pat) || m_pat > rhs.m_pat)
   {
      return retFlag;
   }
   if ((retFlag = m_stem < rhs.m_stem) || m_stem > rhs.m_stem)
   {
      return retFlag;
   }

   bool bSmallerString = m_suffix.length() < rhs.m_suffix.length();

   int compareLength = bSmallerString? m_suffix.length(): rhs.m_suffix.length();
   LgsString::const_iterator thisSuffix = m_suffix.end()-1;
   LgsString::const_iterator rhsSuffix = rhs.m_suffix.end()-1;
   for (; compareLength && *thisSuffix == *rhsSuffix; compareLength--, thisSuffix--, rhsSuffix--);

   if ((compareLength && *thisSuffix < *rhsSuffix) || 
	   (!compareLength && bSmallerString))
   {
      return true;
   }

   return false;
}

inline bool GermanSuffixPatStemConn::setCommonSuffix(const LgsString & suffixVal)
{
   int compareLength = (m_suffix.length() < suffixVal.length())? m_suffix.length(): suffixVal.length();
   LgsString::const_iterator thisSuffix = m_suffix.end()-1;
   LgsString::const_iterator rhsSuffix = suffixVal.end()-1;
   int i;
   for (i = 0; i != compareLength && *thisSuffix == *rhsSuffix; i++, thisSuffix--, rhsSuffix--);
   
   if (!i)
   {
      return false;
   }
   m_suffix.erase(0, m_suffix.length()-i);   
   return true;
}

inline int GermanSuffixPatStemConn::pat(void) const
{
   return m_pat;
}

inline int GermanSuffixPatStemConn::stem(void) const
{
   return m_stem;
}

inline const GermanConnectorFlag & GermanSuffixPatStemConn::connectorFlag(void) const
{
   return m_connectorFlag;
}

#endif



