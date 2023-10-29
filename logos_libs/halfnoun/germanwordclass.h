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
#ifndef __GermanWordClass_h__
#define __GermanWordClass_h__

#include <logos_libs/linguistic/llanguage.h>
//---------------------------------------------------------------------
// File - GermanWordClass.h
//
// class GermanWordClass
//
// Description - internal wordclass for german half noun decomposition
//             - holds dictionary wordclass - and whether the word can
//               appear in a head or non-head position
//
//---------------------------------------------------------------------

class GermanWordClass
{
public:
    // word classes from the dictionary, used in decomposition
    enum PartOfSpeech {noun, adj, verb, halfnoun, arith14, arith16, invalid };

    DefaultConstructor(GermanWordClass) 
    
    // construct word class information directly
    GermanWordClass(PartOfSpeech partOfSpeech, bool head, bool nonHead);

    // construct word class from a previously encoded form
    GermanWordClass(unsigned char code);

    // overwrite with new values    
    void reset(PartOfSpeech partOfSpeech, bool head, bool nonHead);

    // get methods
    PartOfSpeech getPartOfSpeech() const;
    bool inHead() const;
    bool inNonHead() const;
	static PartOfSpeech convertToPartOfSpeech(int wcCode, int patValue);
    
    // encode the object - fits in 5 bits
    unsigned char encode() const;
    
    // return a human readable form of the word-class only, in a single char
    char toChar() const;

private:
    enum { headBit = 8, nonHeadBit = 16, wordClassMask = 7 };
    
    PartOfSpeech partOfSpeech_;    // bits 0, 1, 2
    bool head_;                    // bit 3
    bool nonHead_;                 // bit 4
};

inline GermanWordClass::GermanWordClass(PartOfSpeech partOfSpeech, bool head, bool nonHead)
{
    reset(partOfSpeech, head, nonHead);
}

inline void GermanWordClass::reset(PartOfSpeech partOfSpeech, bool head, bool nonHead)
{
    partOfSpeech_ = partOfSpeech;
    head_ = head;
    nonHead_ = nonHead;
}

inline GermanWordClass::PartOfSpeech GermanWordClass::getPartOfSpeech() const
{
    return partOfSpeech_;
}

inline bool GermanWordClass::inHead() const
{
    return head_;
}

inline bool GermanWordClass::inNonHead() const
{
    return nonHead_;
}

inline GermanWordClass::GermanWordClass(unsigned char code)
{
    reset(PartOfSpeech(code & wordClassMask), (code & headBit) != 0, (code & nonHeadBit) != 0);
}

inline unsigned char GermanWordClass::encode() const
{
    return (unsigned char)partOfSpeech_ +
           (head_ ? (unsigned char)headBit : 0) +
           (nonHead_ ? (unsigned char)nonHeadBit : 0);
}

inline char GermanWordClass::toChar() const
{
    switch (partOfSpeech_)
    {
    case noun:
        return 'n';
    case adj:
        return 'a';
    case verb:
        return 'v';
    case halfnoun:
        return 'h';
    case arith14:
    case arith16:
        return '9';
    default:
        assert(("invalid value", 0));
        return 'h';
    }
}

inline GermanWordClass::PartOfSpeech GermanWordClass::convertToPartOfSpeech(int wcCode, int patValue)
{
    PartOfSpeech retValue;
	switch (wcCode)
	{
		case LLanguage::NOUN:
			retValue = noun;
			break;
		case LLanguage::VERB:
			retValue = verb;
			break;
		case LLanguage::ADJECTIVE:
			if (485 == patValue)
				retValue = halfnoun;
			else
				retValue = adj;
			break;
		case LLanguage::ARTICLE_DEFINITE:
			retValue = arith14;
			break;
		case LLanguage::ARITHMATE:
			retValue = arith16;
			break;
		default:
			retValue = invalid;
			break;
	}
	return retValue;
}

inline ostream& operator<<(ostream& os, GermanWordClass wc)
{
    switch (wc.getPartOfSpeech())
    {
    case GermanWordClass::noun:
        os << "noun";
        break;
    case GermanWordClass::adj:
        os << "adj";
        break;
    case GermanWordClass::verb:
        os << "verb";
        break;
    case GermanWordClass::halfnoun:
        os << "halfnoun";
        break;
    case GermanWordClass::arith14:
        os << "arith14";
        break;
    case GermanWordClass::arith16:
        os << "arith16";
        break;
    default:
        assert(("invalid value", 0));
        break;
    }
    
    if (wc.inHead())
        os << " head";
    if (wc.inNonHead())
        os << " non-head";

    return os;
}

#endif



