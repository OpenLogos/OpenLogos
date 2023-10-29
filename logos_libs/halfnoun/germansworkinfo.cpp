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
//---------------------------------------------------------------------
// File - GermanSWorkInfo.cpp
//
// class - GermanSWorkInfo
//
//---------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/halfnoun/germansworkinfo.h>
#include <logos_libs/utility/stringutil.h>

// byte indexes and nibbles go from left to right ie high to low
// byte:          0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 ...
// nibble:        0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 ...
// store each character as one nibble, incrementing from left to right

GermanSWorkInfo::GermanSWorkInfo(
    const string& s0, const string& s1, const string& s2, const string& form,
    const string& of2a, const string& of2b, const string& of3a, const string& of3b,
    const string& generic, const string& atomic, int priority)
{
    // use a bcd approach - store 14 digits in 7 bytes = 14 nibbles
    memset(data_, 0, sizeof(data_));

    // convert priority to a 2 digit string                     
    string priorityString = "00";
    assert(0 <= priority && priority < 100);
    priorityString[0] = priority / 10;
    priorityString[1] = priority % 10;
    
    int pos = 0;      // current position in data_ field
    setDigits(pos, s0, 2);
    setDigits(pos, s1, 3);
    setDigits(pos, s2, 3);
    setDigits(pos, form, 2);
    setDigits(pos, of2a, 1);
    setDigits(pos, of2b, 1);
    setDigits(pos, of3a, 1);
    setDigits(pos, of3b, 1);
    setDigits(pos, generic, 2);
    setDigits(pos, atomic, 3);
    setDigits(pos, priorityString, 2);

    assert(pos <= sizeof(data_) * 2);
}

void GermanSWorkInfo::setDigits(int& pos, const string& s, int length)
{
    assert(StringUtil::isAllNumeric(s) && s.length() <= length);
    assert(0 <= pos && pos + length <= 2 * sizeof(data_));

    int start = pos + length - s.length();
    int byte = start / 2;
    int nibble = start % 2;

    for (string::const_iterator iter = s.begin(); iter != s.end(); iter++)
    {
        setNibble(byte, nibble, *iter);
        incNibble(byte, nibble);
    }

    pos += length;
}

void GermanSWorkInfo::setNibble(int byte, int nibble, char c)
{
    assert(0 <= byte && byte < sizeof(data_));
    assert(0 <= nibble && nibble < 2);
    assert(CharUtil::isNumeric(c));

    unsigned char data = c - '0';
    if (nibble == 0)
    {
        // store as high order nibble
        data <<= 4;
    }
    data_[byte] |= data;
}

int GermanSWorkInfo::get(int pos, int length) const
{
    int byte = pos / 2;
    int nibble = pos % 2;
    int ret = 0;

    for (int i = 0; i < length; i++)
    {
        unsigned char data = getNibble(byte, nibble);
        ret *= 10;
        ret += data;
        incNibble(byte, nibble);
    }

    return ret;
}

unsigned char GermanSWorkInfo::getNibble(int byte, int nibble) const
{
    assert(0 <= byte && byte < sizeof(data_));
    assert(0 <= nibble && nibble < 2);

    if (nibble == 0)
        return data_[byte] >> 4;
    else
        return data_[byte] & 15;
}

void GermanSWorkInfo::incNibble(int& byte, int& nibble) const
{
    nibble = 1 - nibble;
    if (nibble == 0)
        byte++;
}

