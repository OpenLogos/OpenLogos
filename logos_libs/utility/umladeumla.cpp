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
// file: umladeumla.cpp

#include <logos_include/logoscommon.h>
#include <logos_libs/utility/umladeumla.h>

class strless: public less<LgsString>{};

typedef LgsMap(LgsString, unsigned char) MapStrChar;
typedef LgsMap(unsigned char, LgsString) MapCharStr;

//Sun Compiler has a problem with the commented defn.
//typedef MapStrChar::value_type MStrCharValueType;
//typedef MapCharStr::value_type MCharStrValueType;
typedef LgsMap(LgsString, unsigned char)::value_type MStrCharValueType;
typedef LgsMap(unsigned char, LgsString)::value_type MCharStrValueType;


class CPrivateUmlaDeumla {
public:
        static LgsString umla(const LgsString &word);
        static LgsString deumla(const LgsString &word);

        static void   _populateMaps();
        static MapCharStr  m_iso2LogosTeleCodes;
        static MapStrChar  m_logosTeleCodes2iso;
};

MapCharStr CPrivateUmlaDeumla::m_iso2LogosTeleCodes;
MapStrChar CPrivateUmlaDeumla::m_logosTeleCodes2iso;


void CPrivateUmlaDeumla::_populateMaps()
{
        if( !m_iso2LogosTeleCodes.empty()  )
                return;

        static LgsString teleCodes[] =
        {
                "A1", "¡",
                "A2", "¿",
                "A3", "¬",
                "A4", "ƒ",

                "a1", "·",
                "a2", "‡",
                "a3", "‚",
                "a4", "‰",

                "E1", "…",
                "E2", "»",
                "E3", " ",
                "E4", "À",

                "e1", "È",
                "e2", "Ë",
                "e3", "Í",
                "e4", "Î",

                "I1", "Õ",
                "I2", "Ã",
                "I3", "Œ",
                "I4", "œ",

                "i1", "Ì",
                "i2", "Ï",
                "i3", "Ó",
                "i4", "Ô",

                "O1", "”",
                "O2", "“",
                "O3", "‘",
                "O4", "÷",

                "o1", "Û",
                "o2", "Ú",
                "o3", "Ù",
                "o4", "ˆ",

                "U1", "⁄",
                "U2", "Ÿ",
                "U3", "€",
                "U4", "‹",

                "u1", "˙",
                "u2", "˘",
                "u3", "˚",
                "u4", "¸",

                "C5", "«",
                "c5", "Á",

                "N6", "—",
                "n6", "Ò",

				"AE7", "∆",
				"ae7", "Ê",

                "ss2", "ﬂ",
                "SS2", "ﬂ", //xdf can be either SS2 or SS4
                "Ss2", "ﬂ", //Initial Caps might result into this
                "SS4", "ﬂ",
                "Ss4", "ﬂ", //Initial Caps might result into this
                "ss4", "ﬂ",

                "" , ""  //Terminating Record

        };

        for(int i=0; teleCodes[i].size() > 0; i+=2 )
        {
                LgsString logosRep = teleCodes[i];
                unsigned char isoRep = teleCodes[i+1][0];

                m_iso2LogosTeleCodes.insert(MCharStrValueType(isoRep, logosRep));
                m_logosTeleCodes2iso.insert(MStrCharValueType(logosRep, isoRep));
        }
}

//Converts iso extended characters to logos tele codes
LgsString CPrivateUmlaDeumla::deumla(const LgsString &word)
{
        _populateMaps();

        LgsString output;

        for(LgsString::const_iterator i=word.begin(); i < word.end(); i++)
        {
                MapCharStr::iterator k = m_iso2LogosTeleCodes.find(*i);

                //If the character is an extended ascii character and
                //If the character is a iso equivalent of logos telecode
                if( (*k).first > 127 && k != m_iso2LogosTeleCodes.end() )
                {
                        output += (*k).second;
                }
                else
                {
                        output += *i;
                }
        }

        return output;
}

//Converts logos telecodes to iso characters
LgsString CPrivateUmlaDeumla::umla(const LgsString &word)
{
        _populateMaps();

        LgsString output;//(100, 0); //Preallocate for efficiency

        char buf2[3];   buf2[2] = '\0';
        char buf3[4];   buf3[3] = '\0';

        for(int cp = 0; cp < (int)word.size(); cp++)
        {
                //If it is 2nd or later character
                if( isdigit(word[cp]) && cp > 0)
                {
                        buf2[0] = word[cp-1];
                        buf2[1] = word[cp];

                        MapStrChar::iterator k = m_logosTeleCodes2iso.find(buf2);

                        //If the tmp1 is a logos telecode
                        if( k != m_logosTeleCodes2iso.end() )
                        {
                                //Change the last character
                                output[output.size()-1] = (*k).second;
                        }
                        //If it is third or later character
                        else if ( cp > 1)
                        {
                                buf3[0] = word[cp-2];
                                buf3[1] = word[cp-1];
                                buf3[2] = word[cp];

                                k = m_logosTeleCodes2iso.find(buf3);

                                if( k != m_logosTeleCodes2iso.end() )
                                {
                                        //Remove the last two characters
                                        output = output.substr(0, output.size()-2);

                                        output = output + (char)(*k).second;
                                }
                                else
                                {
                                        output += word.substr(cp,1);
                                }
                        }
                        else
                        {
                                output += word.substr(cp,1);
                        }
                }
                else
                {
                        output +=  word.substr(cp, 1);
                }
        }

        return output;
}

LgsString CUmlaDeumla::deumla(const LgsString &word)
{
        return CPrivateUmlaDeumla::deumla(word);
}

LgsString CUmlaDeumla::umla(const LgsString &word)
{
        return CPrivateUmlaDeumla::umla(word);
}

bool CUmlaDeumla::isUpper(const char a)
{
	if ( a == 'ﬂ' )
		return false;

    LgsString b;

    b += a;

    b = deumla(b);

    return isupper(b[0]);
}

bool CUmlaDeumla::isLower(const char a)
{
	if ( a == 'ﬂ' )
		return true;

    LgsString b;

    b += a;

    b = deumla(b);

    return islower(b[0]);
}

unsigned char CUmlaDeumla::toUpper(const char a)
{
        LgsString b;

        b += a;

        b = deumla(b);

        for(LgsString::iterator i = b.begin(); i < b.end(); i++)
        {
                *i = toupper(*i);
        }

        b = umla(b);

        return b[0];
}

unsigned char CUmlaDeumla::toLower(const char a)
{
        LgsString b;

        b += a;

        b = deumla(b);

        for(LgsString::iterator i = b.begin(); i < b.end(); i++)
        {
                *i = tolower(*i);
        }

        b = umla(b);

        return b[0];
}

