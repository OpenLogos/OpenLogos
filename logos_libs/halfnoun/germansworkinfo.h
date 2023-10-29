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
#ifndef __GermanSWorkInfo_h__
#define __GermanSWorkInfo_h__

//---------------------------------------------------------------------
// File - GermanSWorkInfo.h
//
// class - GermanSWorkInfo
//
// Description - German class used to hold store SWork information
//               Tries to minimize space overhead
//               Can be used later to create an SWork record for RES
//
//---------------------------------------------------------------------

class GermanSWorkInfo
{
public:
    DefaultConstructor(GermanSWorkInfo)
    GermanSWorkInfo(const LgsString& s0, const LgsString& s1, const LgsString& s2, const LgsString& form,
        const LgsString& of2a, const LgsString& of2b, const LgsString& of3a, const LgsString& of3b,
        const LgsString& generic, const LgsString& atomic, int priority);

    int s0() const;
    int s1() const;
    int s2() const;
    int form() const;
    int of2a() const;
    int of2b() const;
    int of3a() const;
    int of3b() const;
    int generic() const;
    int atomic() const;
    int priority() const;

private:
    unsigned char data_[11]; // can reduce to fewer - but 11 is easier

    void setDigits(int& pos, const LgsString& s, int length);
    void setNibble(int byte, int nibble, char c);

    int get(int pos, int length) const;
    unsigned char getNibble(int byte, int nibble) const;

    void incNibble(int& byte, int& nibble) const;             // increment byte/nibble
};

inline int GermanSWorkInfo::s0() const
{
    return get(0, 2);
}

inline int GermanSWorkInfo::s1() const
{
    return get(2, 3);
}

inline int GermanSWorkInfo::s2() const
{
    return get(5, 3);
}

inline int GermanSWorkInfo::form() const
{
    return get(8, 2);
}

inline int GermanSWorkInfo::of2a() const
{
    return get(10, 1);
}

inline int GermanSWorkInfo::of2b() const
{
    return get(11, 1);
}

inline int GermanSWorkInfo::of3a() const
{
    return get(12, 1);
}

inline int GermanSWorkInfo::of3b() const
{
    return get(13, 1);
}

inline int GermanSWorkInfo::generic() const
{
    return get(14, 2);
}

inline int GermanSWorkInfo::atomic() const
{
    return get(16, 3);
}

inline int GermanSWorkInfo::priority() const
{
    return get(19, 2);
}

#endif



