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
#ifndef __GermanFlag_h__
#define __GermanFlag_h__

//---------------------------------------------------------------------
// File - GermanFlag.h
//
// class - GermanFlag
// class - GermanConnectorFlag
// class - GermanDictionaryEntryFlag
//
// Description - classes used to hold flags, can set but not unset
//
//---------------------------------------------------------------------

#include <logos_libs/halfnoun/germanwordclass.h>
#include <logos_libs/utility/bitfields.h>

//---------------------------------------------------------------------
template <int flagCount>
class GermanFlag: public Object
{
public:
    enum { byteCount = (flagCount + 7) / 8 };

    GermanFlag();
    bool blank() const;

    // read/write data on an output stream in binary format
    void printOn(ofstream& output) const;
    bool readFrom(istream& input);
    bool get_(int index) const;
    void set_(int index);
    void unset_(int index);
    void operator|=(const GermanFlag & rhs);

protected:
    char buf_[byteCount];

};

template <int flagCount>
inline GermanFlag<flagCount>::GermanFlag()
{
    memset(buf_, 0, byteCount);
}

template <int flagCount>
inline bool GermanFlag<flagCount>::get_(int index) const
{
    assert(index < flagCount);
    return (buf_[index / 8] & (1 << (index % 8))) != 0;
}

template <int flagCount>
inline void GermanFlag<flagCount>::set_(int index)
{
    assert(index < flagCount);
    buf_[index / 8] |= (1 << (index % 8));
}

template <int flagCount>
inline void GermanFlag<flagCount>::unset_(int index)
{
    assert(index < flagCount);
    buf_[index / 8] &= ~(1 << (index % 8));
}

template <int flagCount>
inline void GermanFlag<flagCount>::operator|=(const GermanFlag & rhs)
{
    for (int i = 0; i < byteCount; i++)
        buf_[i] |= rhs.buf_[i];
}


template <int flagCount>
void GermanFlag<flagCount>::printOn(ofstream& output) const
{
    GermanUtil::printByteArrayOn(buf_, byteCount, output);
}

template <int flagCount>
bool GermanFlag<flagCount>::readFrom(istream& input)
{
    return GermanUtil::readByteArrayFrom(buf_, byteCount, input);
}

//---------------------------------------------------------------------
// connector values
enum ConnectorFlagValue // note: these are in same order without gaps as below values
{
    conn_none, conn_e, conn_es, conn_er, conn_en, conn_n, conn_nen, conn_s
};
const int conn_count = conn_s + 1;

// returns the longest allowed connector
inline int longestConnector()
{
    return 3;
}

class GermanConnectorFlag: public LgsBitFields
{
public:
   GermanConnectorFlag(void);
   GermanConnectorFlag(const GermanConnectorFlag & rhs);
   short get(ConnectorFlagValue index) const;
   void unset(ConnectorFlagValue index);
   void set(ConnectorFlagValue index, short connectorWeight);
   
   // set the index if the word does not end in the connector
   void setWord(const LgsString& word, ConnectorFlagValue index, short connectorWeight);
	ConnectorFlagValue indexFromConnector(const LgsString& connector) const;

   // unset a connector if the word ends in the connector
   void removeSuffix(const LgsString& word);

   void operator|=(const GermanConnectorFlag& rhs);
   const GermanConnectorFlag & operator=(const GermanConnectorFlag & rhs);
};

inline GermanConnectorFlag::GermanConnectorFlag(void):
         LgsBitFields(conn_count, 4)
{

}

inline GermanConnectorFlag::GermanConnectorFlag(const GermanConnectorFlag & rhs):
         LgsBitFields(rhs)
{

}

inline const GermanConnectorFlag & GermanConnectorFlag::operator=(const GermanConnectorFlag & rhs)
{
   copyFlag(rhs);
   return (*this);
}

inline short GermanConnectorFlag::get(ConnectorFlagValue index) const
{
   FLD_VALUE_TYPE fldValue = 0;
   getField((unsigned short)index, fldValue);
   return fldValue;
}

inline void GermanConnectorFlag::set(ConnectorFlagValue index, short connectorWeight)
{
    setField((unsigned short)index, connectorWeight);
}

inline void GermanConnectorFlag::unset(ConnectorFlagValue index)
{
    setField((unsigned short)index, 0);
}

//---------------------------------------------------------------------
// suffix values
enum SuffixFlagValue
{
    suf_none, suf_e, suf_em, suf_en, suf_end, suf_ens, suf_er, suf_ere, suf_erem, suf_eren,
    suf_erer, suf_eres, suf_ern, suf_es, suf_et, suf_ete, suf_eten, suf_ien,
    suf_le, suf_n, suf_nen, suf_ns, suf_s, suf_se, suf_sen, suf_ses, suf_ste,
    suf_stem, suf_sten, suf_ster, suf_stes, suf_t, suf_te, suf_este, suf_esten, suf_ester, suf_estem, 
	suf_estes, suf_in, suf_innen, suf_Innen, suf_ten
};

const int suf_count = suf_ten + 1;

// returns the longest allowed suffix
inline int longestSuffix()
{
    return 4;
}

class GermanSuffixFlag: public GermanFlag<suf_count>
{
public:
    bool get(SuffixFlagValue index) const;
    void set(SuffixFlagValue index);
    void unset(SuffixFlagValue index);

    // convert suffix to enum value - throw an error if invalid
    static SuffixFlagValue indexFromSuffix(const LgsString& suffix);
    void operator |=(const GermanSuffixFlag & rhs);
};

inline void GermanSuffixFlag::operator|=(const GermanSuffixFlag & rhs)
{
   GermanFlag<suf_count>::operator|=(rhs);
}

inline bool GermanSuffixFlag::get(SuffixFlagValue index) const
{
    return get_(int(index));
}

inline void GermanSuffixFlag::set(SuffixFlagValue index)
{
    set_(int(index));
}

inline void GermanSuffixFlag::unset(SuffixFlagValue index)
{
    unset_(int(index));
}

//---------------------------------------------------------------------
enum DictEntryFlagValue
{
    // un-normalized word attributes ------------------------------

    // begins with a capital letter
    de_initialCapital,

    // if a data-base entry ends in a double letter we add an extra
    // dictionary entry with the last letter removed
    // eg Ballett - normalized form of ballet
    de_doubleEndingRemoved,

    // if the normalized and un-normalized form are different apart
    // from the above differences, we store the un-normalized form
    // and set this flag
    de_nonNormalizedStored

    // connector information - from above
    // suffix information - from above
};

const int de_count = de_nonNormalizedStored + 1;
typedef GermanFlag<de_count> DictionaryEntryFlag;

class GermanDictionaryEntryFlag
{
public:
    DefaultConstructor(GermanDictionaryEntryFlag)

    bool get(DictEntryFlagValue index) const;
    short get(ConnectorFlagValue index) const;
    bool get(SuffixFlagValue index) const;

    void set(DictEntryFlagValue index);
    void set(ConnectorFlagValue index, short connectorWeight);
    void set(SuffixFlagValue index);

    void unset(DictEntryFlagValue index);
    void unset(ConnectorFlagValue index);
    void unset(SuffixFlagValue index);

    void printOnAsText(ofstream& output) const;

    void operator|=(const GermanConnectorFlag& rhs);
    void operator|=(const GermanSuffixFlag& rhs);
    void operator|=(const GermanDictionaryEntryFlag& rhs);

    GermanConnectorFlag asConnectorFlag() const;
    GermanSuffixFlag asSuffixFlag() const;
    // read/write data on an output stream in binary format
    void printOn(ofstream& output) const;
    bool readFrom(istream& input);
private:
   DictionaryEntryFlag m_dictEntryFlag;
   GermanConnectorFlag m_connectorFlag;
   GermanSuffixFlag m_suffixFlag;
};

inline bool GermanDictionaryEntryFlag::get(DictEntryFlagValue index) const
{
    return m_dictEntryFlag.get_(int(index));
}
inline short GermanDictionaryEntryFlag::get(ConnectorFlagValue index) const
{
   FLD_VALUE_TYPE connectorValue;
   m_connectorFlag.getField((unsigned short)(index), connectorValue);
   return connectorValue;
}
inline bool GermanDictionaryEntryFlag::get(SuffixFlagValue index) const
{
    return m_suffixFlag.get(index);
}

inline void GermanDictionaryEntryFlag::set(DictEntryFlagValue index)
{
    m_dictEntryFlag.set_(int(index));
}
inline void GermanDictionaryEntryFlag::set(ConnectorFlagValue index, short connectorWeight)
{
    m_connectorFlag.setField((unsigned short)index, connectorWeight);
}
inline void GermanDictionaryEntryFlag::set(SuffixFlagValue index)
{
    m_suffixFlag.set(index);
}

inline void GermanDictionaryEntryFlag::unset(DictEntryFlagValue index)
{
    m_dictEntryFlag.unset_(int(index));
}
inline void GermanDictionaryEntryFlag::unset(ConnectorFlagValue index)
{
    m_connectorFlag.setField(int(index), 0);
}
inline void GermanDictionaryEntryFlag::unset(SuffixFlagValue index)
{
    m_suffixFlag.unset(index);
}

inline void GermanDictionaryEntryFlag::printOn(ofstream& output) const
{
   m_dictEntryFlag.printOn(output);
   m_suffixFlag.printOn(output);
   m_connectorFlag.printOn(output);
}

inline bool GermanDictionaryEntryFlag::readFrom(istream& input)
{
    return m_dictEntryFlag.readFrom(input) && m_suffixFlag.readFrom(input) &&
           m_connectorFlag.readFrom(input);
}


#endif



