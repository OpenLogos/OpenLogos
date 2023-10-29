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
// note: note tested char hc = CharUtil::henumChar(char(c));

#include <iostream>
#include <iomanip>
#include "charutil.h"

char* type_of(unsigned char uc)
{
    static char buf[80];
    *buf = 0;
    char c = char(uc);

    if (CharUtil::isAscii(c))
        strcat(buf, "asc ");
    else
        strcat(buf, "... ");

    if (CharUtil::isSpace(c))
        strcat(buf, "sp ");
    else
        strcat(buf, ".. ");

    if (CharUtil::isPrint(c))
        strcat(buf, "pr ");
    else
        strcat(buf, ".. ");
    if (CharUtil::isGraph(c))
        strcat(buf, "g ");
    else
        strcat(buf, ". ");

    if (CharUtil::isControl(c))
        strcat(buf, "c ");
    else
        strcat(buf, ". ");

    if (CharUtil::isHexDigit(c))
        strcat(buf, "h ");
    else
        strcat(buf, ". ");

    if (CharUtil::isBlank(c))
        strcat(buf, "b ");
    else
        strcat(buf, ". ");

    if (CharUtil::isPunctuation(c))
        strcat(buf, "p ");
    else
        strcat(buf, ". ");

    if (CharUtil::isLower(c))
        strcat(buf, "l ");
    else
        strcat(buf, ". ");

    if (CharUtil::isUpper(c))
        strcat(buf, "u ");
    else
        strcat(buf, ". ");

    if (CharUtil::isAlphabetic(c))
        strcat(buf, "a ");
    else
        strcat(buf, ". ");

    if (CharUtil::isNumeric(c))
        strcat(buf, "n ");
    else
        strcat(buf, ". ");

    if (CharUtil::isAlphaNumeric(c))
        strcat(buf, "an ");
    else
        strcat(buf, ".. ");

    if (CharUtil::isSymbol(c))
        strcat(buf, "s ");
    else
        strcat(buf, ". ");

    return buf;
}

void dump(unsigned char c)
{
    cout << "Char '\\x" << hex << setw(2) << setfill('0') << int(c) << "' ";
    if (c >= 0x20)
    {
        char lc = CharUtil::lower(char(c));
        char uc = CharUtil::upper(char(c));

        cout << c;
        if (c != (unsigned char)lc || c != (unsigned char)uc)
            cout << lc << uc;
        else
            cout << "  ";
        cout << " ";
    }
    else
        cout << "    ";

    cout << "      " << type_of(c) << endl;

}

void dumpValues()
{
    cout << "space = '" << CharUtil::space() << "'" << endl;
    cout << "unsigned_space = '" << CharUtil::unsigned_space() << "'" << endl;
    cout << "null = " << (int) CharUtil::null() << endl;
    cout << endl;
}

void dumpVowels()
{
    cout << "English vowels = \"" << CharUtil::vowels() << "\" = ";
    for (int c = 0; c <= 0xff; c++)
        if (CharUtil::isVowel(c))
            cout << char(c) << ",";
    cout << endl << endl;
}

void dumpList()
{
    cout << "asc=ascii, sp=space, pr=printable, g=graphics, c=control, h=hex-digit" << endl;
    cout << "p=punctuation, b=blank, s=symbol" << endl;
    cout << "l=lower, u=upper, a=alpha, n=numeric, an=alpha-numeric" << endl;
    cout << "lower and upper case conversions are printed if applicable" << endl;
    cout << endl;

    for (int c = 0; c <= 0xff; c++)
        dump((unsigned char)c);
    cout << endl;
}

int main()
{
    dumpValues();
    dumpVowels();
    dumpList();

    return 0;
}

