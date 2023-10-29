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
// File - GermanFlag.cpp
//
// class - GermanDictionaryEntryFlag
//
//---------------------------------------------------------------------

#include <logos_include/logoscommon.h>

#include <logos_libs/halfnoun/germanflag.h>
#include <logos_libs/halfnoun/germanutil.h>
#include <logos_libs/halfnoun/logfile.h>

//---------------------------------------------------------------------
// connector values

void GermanConnectorFlag::setWord(const LgsString& word, ConnectorFlagValue index, short connectorWeight)
{
    switch (index)
    {
    case conn_none:
        set(index, connectorWeight);
        break;

    case conn_e:
        if (!GermanUtil::isEnding("e", word))
            set(index, connectorWeight);
        break;

    case conn_es:
        if (!GermanUtil::isEnding("es", word))
            set(index, connectorWeight);
        break;

    case conn_er:
        if (!GermanUtil::isEnding("er", word))
            set(index, connectorWeight);
        break;

    case conn_en:
        if (!GermanUtil::isEnding("en", word))
            set(index, connectorWeight);
        break;

    case conn_n:
        if (!GermanUtil::isEnding("n", word))
            set(index, connectorWeight);
        break;

    case conn_nen:
        if (!GermanUtil::isEnding("nen", word))
            set(index, connectorWeight);
        break;

    case conn_s:
        {
            char last = GermanUtil::lastChar(word);
            if (last != 's' && last != 'x' && last != 'z' && !GermanUtil::isEnding("sch", word))
                set(index, connectorWeight);
        }
        break;

    default:
        assert(("invalid value", 0));
        break;
    }
}

//---------------------------------------------------------------------
// returns ConnectorFlagValue (index) for the connector LgsString

ConnectorFlagValue GermanConnectorFlag::indexFromConnector(const LgsString& connector) const
{
	if (connector == "e")
	{
		return conn_e;
	} 
	else if (connector == "es")
	{
		return conn_es;
	} 
	else if (connector == "er")
	{
		return conn_er;
	} 
	else if (connector == "en")
	{
		return conn_en;
	}
	else if (connector == "n")
	{
		return conn_n;
	}
	else if (connector == "nen")
	{
		return conn_nen;
	}
	else if (connector == "s")
	{
		return conn_s;
	} 
	else 
	{
		return conn_none;
	}
}

void GermanConnectorFlag::removeSuffix(const LgsString& word)
{
    if (GermanUtil::isEnding("e", word))
    {
#ifdef DEBUG_LOG_ON
if (get(conn_e)) LogFile::singleton().printDateTime() << "\t\t Removing connector e" << endl;
#endif
        unset(conn_e);
    }

    if (GermanUtil::isEnding("es", word))
    {
#ifdef DEBUG_LOG_ON
if (get(conn_es)) LogFile::singleton().printDateTime() << "\t\t Removing connector es" << endl;
#endif
        unset(conn_es);
    }

    if (GermanUtil::isEnding("er", word))
    {
#ifdef DEBUG_LOG_ON
if (get(conn_er)) LogFile::singleton().printDateTime() << "\t\t Removing connector er" << endl;
#endif
        unset(conn_er);
    }

    if (GermanUtil::isEnding("en", word))
    {
#ifdef DEBUG_LOG_ON
if (get(conn_en)) LogFile::singleton().printDateTime() << "\t\t Removing connector en" << endl;
#endif
        unset(conn_en);
    }

    if (GermanUtil::isEnding("n", word))
    {
#ifdef DEBUG_LOG_ON
if (get(conn_n)) LogFile::singleton().printDateTime() << "\t\t Removing connector n" << endl;
#endif
        unset(conn_n);
    }

    if (GermanUtil::isEnding("nen", word))
    {
#ifdef DEBUG_LOG_ON
if (get(conn_nen)) LogFile::singleton().printDateTime() << "\t\t Removing connector nen" << endl;
#endif
        unset(conn_nen);
    }

    char last = GermanUtil::lastChar(word);
    if (last == 's' || last == 'x' || last == 'z')
    {
#ifdef DEBUG_LOG_ON
if (get(conn_s)) LogFile::singleton().printDateTime() << "\t\t Removing connector s" << endl;
#endif
        unset(conn_s);
    }
}

//---------------------------------------------------------------------
SuffixFlagValue GermanSuffixFlag::indexFromSuffix(const LgsString& suffix)
{
    if (suffix == LgsString())
        return suf_none;
    else if (suffix == LgsString("e"))
        return suf_e;
    else if (suffix == LgsString("em"))
        return suf_em;
    else if (suffix == LgsString("en"))
        return suf_en;
    else if (suffix == LgsString("end"))
        return suf_end;
    else if (suffix == LgsString("ens"))
        return suf_ens;
    else if (suffix == LgsString("er"))
        return suf_er;
    else if (suffix == LgsString("ere"))
        return suf_ere;
    else if (suffix == LgsString("erem"))
        return suf_erem;
    else if (suffix == LgsString("eren"))
        return suf_eren;
    else if (suffix == LgsString("erer"))
        return suf_erer;
    else if (suffix == LgsString("eres"))
        return suf_eres;
    else if (suffix == LgsString("ern"))
        return suf_ern;
    else if (suffix == LgsString("es"))
        return suf_es;
    else if (suffix == LgsString("et"))
        return suf_et;
    else if (suffix == LgsString("ete"))
        return suf_ete;
    else if (suffix == LgsString("eten"))
        return suf_eten;
    else if (suffix == LgsString("ien"))
        return suf_ien;
    else if (suffix == LgsString("le"))
        return suf_le;
    else if (suffix == LgsString("n"))
        return suf_n;
    else if (suffix == LgsString("nen"))
        return suf_nen;
    else if (suffix == LgsString("ns"))
        return suf_ns;
    else if (suffix == LgsString("s"))
        return suf_s;
    else if (suffix == LgsString("se"))
        return suf_se;
    else if (suffix == LgsString("sen"))
        return suf_sen;
    else if (suffix == LgsString("ses"))
        return suf_ses;
    else if (suffix == LgsString("ste"))
        return suf_ste;
    else if (suffix == LgsString("stem"))
        return suf_stem;
    else if (suffix == LgsString("sten"))
        return suf_sten;
    else if (suffix == LgsString("ster"))
        return suf_ster;
    else if (suffix == LgsString("stes"))
        return suf_stes;
    else if (suffix == LgsString("t"))
        return suf_t;
    else if (suffix == LgsString("te"))
        return suf_te;
    else if (suffix == LgsString("ten"))
        return suf_ten;
	else if (suffix == LgsString("este"))
		return suf_este;
	else if (suffix == LgsString("estem"))
		return suf_estem;
	else if (suffix == LgsString("esten"))
		return suf_esten;
	else if (suffix == LgsString("ester"))
		return suf_ester;
	else if (suffix == LgsString("estes"))
		return suf_estes;
	else if (suffix == LgsString("in"))
		return suf_in;
	else if (suffix == LgsString("innen"))
		return suf_innen;
	else if (suffix == LgsString("Innen"))
		return suf_Innen;
    else
    {
        throw LgsString("Invalid suffix: ") + suffix;
        return suf_ten;        // keep compiler happy
    }
}

void GermanConnectorFlag::operator|=(const GermanConnectorFlag& rhs)
{
   LgsBitFields::operator |=(rhs);
}


ostream& operator<<(ostream& os, SuffixFlagValue suffix)
{
    switch (suffix)
    {
    case suf_none:
        os << "suf_none";
        break;
    case suf_e:
        os << "suf_e";
        break;
    case suf_em:
        os << "suf_em";
        break;
    case suf_en:
        os << "suf_en";
        break;
    case suf_end:
        os << "suf_end";
        break;
    case suf_ens:
        os << "suf_ens";
        break;
    case suf_er:
        os << "suf_er";
        break;
    case suf_ere:
        os << "suf_ere";
        break;
    case suf_erem:
        os << "suf_erem";
        break;
    case suf_eren:
        os << "suf_eren";
        break;
    case suf_erer:
        os << "suf_erer";
        break;
    case suf_eres:
        os << "suf_eres";
        break;
    case suf_ern:
        os << "suf_ern";
        break;
    case suf_es:
        os << "suf_es";
        break;
    case suf_et:
        os << "suf_et";
        break;
    case suf_ete:
        os << "suf_ete";
        break;
    case suf_eten:
        os << "suf_eten";
        break;
    case suf_ien:
        os << "suf_ien";
        break;
    case suf_le:
        os << "suf_le";
        break;
    case suf_n:
        os << "suf_n";
        break;
    case suf_nen:
        os << "suf_nen";
        break;
    case suf_ns:
        os << "suf_ns";
        break;
    case suf_s:
        os << "suf_s";
        break;
    case suf_se:
        os << "suf_se";
        break;
    case suf_sen:
        os << "suf_sen";
        break;
    case suf_ses:
        os << "suf_ses";
        break;
    case suf_ste:
        os << "suf_ste";
        break;
    case suf_stem:
        os << "suf_stem";
        break;
    case suf_sten:
        os << "suf_sten";
        break;
    case suf_ster:
        os << "suf_ster";
        break;
    case suf_stes:
        os << "suf_stes";
        break;
    case suf_t:
        os << "suf_t";
        break;
    case suf_te:
        os << "suf_te";
        break;
    case suf_ten:
        os << "suf_ten";
        break;
	case suf_este:
		os << "suf_este";
		break;
	case suf_esten:
		os << "suf_esten";
		break;
	case suf_estes:
		os << "suf_estes";
		break;
	case suf_estem:
		os << "suf_estem";
		break;
	case suf_ester:
		os << "suf_ester";
		break;
	case suf_innen:
		os << "suf_innen";
		break;
	case suf_Innen:
		os << "suf_Innen";
		break;
	case suf_in:
		os << "suf_in";
		break;
    default:
        assert(("invalid value", 0));
        break;
    }

    return os;
}

//---------------------------------------------------------------------
void GermanDictionaryEntryFlag::printOnAsText(ofstream& output) const
{
    if (get(de_initialCapital))
        output << "cap ";
    if (get(de_nonNormalizedStored))
        output << "stored ";
    if (get(de_doubleEndingRemoved))
        output << "------- duplicate-special ";

    output << endl << "\tconn: ";
    short connectorWeight;
    if (connectorWeight = get(conn_none))
        output << "<none> " << connectorWeight << " ";
    if (connectorWeight = get(conn_e))
        output << "e " << connectorWeight << " ";
    if (connectorWeight = get(conn_es))
        output << "es " << connectorWeight << " ";
    if (connectorWeight = get(conn_er))
        output << "er " << connectorWeight << " ";
    if (connectorWeight = get(conn_en))
        output << "en " << connectorWeight << " ";
    if (connectorWeight = get(conn_n))
        output << "n " << connectorWeight << " ";
    if (connectorWeight = get(conn_nen))
        output << "nen " << connectorWeight << " ";
    if (connectorWeight = get(conn_s))
        output << "s " << connectorWeight << " ";

    output << endl << "\tsuffix: ";

    if (get(suf_none))
        output << "<none> ";
    if (get(suf_e))
        output << "e ";
    if (get(suf_em))
        output << "em ";
    if (get(suf_en))
        output << "en ";
    if (get(suf_end))
        output << "end ";
    if (get(suf_ens))
        output << "ens ";
    if (get(suf_er))
        output << "er ";
    if (get(suf_ere))
        output << "ere ";
    if (get(suf_erem))
        output << "erem ";
    if (get(suf_eren))
        output << "eren ";
    if (get(suf_erer))
        output << "erer ";
    if (get(suf_eres))
        output << "eres ";
    if (get(suf_ern))
        output << "ern ";
    if (get(suf_es))
        output << "es ";
    if (get(suf_et))
        output << "et ";
    if (get(suf_ete))
        output << "ete ";
    if (get(suf_eten))
        output << "eten ";
    if (get(suf_ien))
        output << "ien ";
    if (get(suf_le))
        output << "le ";
    if (get(suf_n))
        output << "n ";
    if (get(suf_nen))
        output << "nen ";
    if (get(suf_ns))
        output << "ns ";
    if (get(suf_s))
        output << "s ";
    if (get(suf_se))
        output << "se ";
    if (get(suf_sen))
        output << "sen ";
    if (get(suf_ses))
        output << "ses ";
    if (get(suf_ste))
        output << "ste ";
    if (get(suf_stem))
        output << "stem ";
    if (get(suf_sten))
        output << "sten ";
    if (get(suf_ster))
        output << "ster ";
    if (get(suf_stes))
        output << "stes ";
    if (get(suf_t))
        output << "t ";
    if (get(suf_te))
        output << "te ";
	if (get(suf_este))
		output << "este";
    if (get(suf_ten))
        output << "ten ";
    if (get(suf_esten))
        output << "esten ";
    if (get(suf_estem))
        output << "estem ";
    if (get(suf_ester))
        output << "ester ";
    if (get(suf_estes))
        output << "estes ";
    if (get(suf_in))
        output << "in ";
    if (get(suf_innen))
        output << "innen ";
	if (get(suf_Innen))
		output << "Innen";

    output << endl;
}

void GermanDictionaryEntryFlag::operator|=(const GermanConnectorFlag& rhs)
{
   m_connectorFlag |= rhs;
}

void GermanDictionaryEntryFlag::operator|=(const GermanSuffixFlag& rhs)
{
   m_suffixFlag |= rhs;
}

void GermanDictionaryEntryFlag::operator|=(const GermanDictionaryEntryFlag& rhs)
{
   m_connectorFlag |= rhs.m_connectorFlag;
   m_suffixFlag |= rhs.m_suffixFlag;
   m_dictEntryFlag |= rhs.m_dictEntryFlag;
}

GermanConnectorFlag GermanDictionaryEntryFlag::asConnectorFlag() const
{
    return m_connectorFlag;
}

GermanSuffixFlag GermanDictionaryEntryFlag::asSuffixFlag() const
{
    return m_suffixFlag;
}

