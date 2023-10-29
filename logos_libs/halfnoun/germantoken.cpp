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
// File - GermanToken.cpp
//
// class GermanTokenAbstract
//     class GermanToken
//         class GermanTokenWord
//         class GermanTokenConnector
//         class GermanTokenSuffix
//     class GermanTokenTrial
//         class GermanTokenTrialWord
//         class GermanTokenTrialConnector
//         class GermanTokenTrialSuffix
// class GermanTokenList
//
//---------------------------------------------------------------------

#include <logos_include/logoscommon.h>

#include <logos_libs/halfnoun/germantoken.h>
#include <logos_libs/halfnoun/germanwordclass.h>

//---------------------------------------------------------------------
GermanTokenWord::GermanTokenWord(const LgsString& text,
    const LgsList(const GermanDictionaryEntry*)& entries)
    : GermanToken(text)
{
	for (LgsList(const GermanDictionaryEntry*)::const_iterator iter = entries.begin(); 
		 iter != entries.end(); iter++)
	{
		DictionaryEntryLink linkObj((*iter), 0);
		entries_.push_back(linkObj);
	}
}

GermanTokenWord::GermanTokenWord(const LgsString& text,
    const EntryLinkList & entries)
    : GermanToken(text), entries_(entries)
{

}

void GermanTokenWord::decLinkCount(const GermanDictionaryEntry* dictEntry)
{
        EntryLinkIterator iter;
	for (iter = entries_.begin(); 
		 iter != entries_.end() && (*iter).first != dictEntry ; iter++);

	if (iter == entries_.end())
	{
		return;
	}
	(*iter).second--;
	
	if (!(*iter).second)
	{
		entries_.erase(iter);
	}
}

void GermanTokenWord::removeNoConnMatchEntries(void)
{	
	for (EntryLinkIterator iter = entries_.begin(); 
		 iter != entries_.end() ;)
	{
		if (!(*iter).second)
		{
			iter = entries_.erase(iter);
		}
		else
		{
			iter++;
		}
	}
}

void GermanTokenWord::incLinkCount(const GermanDictionaryEntry* dictEntry)
{
        EntryLinkIterator iter;
	for (iter = entries_.begin(); 
		 iter != entries_.end() && (*iter).first != dictEntry ; iter++);

	if (iter == entries_.end())
	{
		return;
	}
	(*iter).second++;
}

bool GermanTokenWord::isDoubleEnded(void) const
{
	const GermanDictionaryEntry * dictEntry = (*entries_.begin()).first;
	if (dictEntry)
	{
		return dictEntry->isDoubleEnded();
	}
	return false;
}
bool GermanTokenWord::allowedAsNonHead(int wcCode, int patValue) const
{
	GermanWordClass::PartOfSpeech poSpeech = GermanWordClass::convertToPartOfSpeech(wcCode, patValue);
	if (poSpeech == GermanWordClass::invalid)
	{
		return false;
	}
    for (EntryLinkConstIterator iter = entries_.begin(); iter != entries_.end(); iter++)
    {
		GermanWordClass gwc = (*iter).first->getWordClass();
		if (gwc.getPartOfSpeech() == poSpeech && gwc.inNonHead())
		{
			return true;
		}
    }
	return false;
}

bool GermanTokenWord::allowedWordClass(int wcCode, int patValue) const
{
	GermanWordClass::PartOfSpeech poSpeech = GermanWordClass::convertToPartOfSpeech(wcCode, patValue);
	if (poSpeech == GermanWordClass::invalid)
	{
		return false;
	}
    for (EntryLinkConstIterator iter = entries_.begin(); iter != entries_.end(); iter++)
    {
		GermanWordClass gwc = (*iter).first->getWordClass();
		if ((gwc.getPartOfSpeech() == poSpeech) &&
          (patValue == (*iter).first->getPatNumber()))
		{
			return true;
		}
    }
	return false;

}

bool GermanTokenWord::matchWithLowerCase(void) const
{
    for (EntryLinkConstIterator iter = entries_.begin(); iter != entries_.end(); iter++)
    {
		GermanWordClass gwc = (*iter).first->getWordClass();
		GermanWordClass::PartOfSpeech partOfSpeech = gwc.getPartOfSpeech();
		if ( GermanWordClass::arith14 == partOfSpeech ||
			 GermanWordClass::arith16 == partOfSpeech ||
			 GermanWordClass::verb == partOfSpeech ||
			 GermanWordClass::adj == partOfSpeech)
		{
			return true;
		}
    }
	return false;
}

bool GermanTokenWord::matchWithUpperCase(void) const
{
    for (EntryLinkConstIterator iter = entries_.begin(); iter != entries_.end(); iter++)
    {
		GermanWordClass gwc = (*iter).first->getWordClass();
		GermanWordClass::PartOfSpeech partOfSpeech = gwc.getPartOfSpeech();
		if ( GermanWordClass::noun == partOfSpeech ||
			 GermanWordClass::halfnoun == partOfSpeech)
		{
			return true;
		}
    }
	return false;
}

void GermanTokenWord::printOn(ostream& os) const
{
    for (EntryLinkConstIterator iter = entries_.begin(); iter != entries_.end(); iter++)
    {
        if (iter  != entries_.begin())
            os << "/";
        os << *((*iter).first);
    }
}

GermanConnectorFlag GermanTokenWord::getConnectorRequirement()
{
    GermanConnectorFlag flag;
    for (EntryLinkConstIterator iter = entries_.begin(); iter != entries_.end(); iter++)
    {
        flag |= (*iter).first->getFlags().asConnectorFlag();
    }

    return flag;
}

//---------------------------------------------------------------------
void GermanTokenList::operator=(const GermanTokenList& rhs)
{
    if (this == &rhs)
        return;

    erase();
    append(rhs);
}

GermanTokenList::GermanTokenList(const GermanTokenList& rhs) : ownsElements_(true)
{
    append(rhs);
}

void GermanTokenList::erase(GermanTokenList::iterator element)
{
	delete *element;
	LgsVector(GermanTokenAbstract*)::erase(element);
}

GermanTokenList::~GermanTokenList()
{
    erase();
}

void GermanTokenList::append(const GermanTokenList& rhs)
{
    for (GermanTokenList::const_iterator iter = rhs.begin(); iter != rhs.end(); iter++)
        push_back((*iter)->clone());
}

void GermanTokenList::erase()
{
	if (ownsElements_ && begin() != end())
	{
                GermanTokenList::iterator iter;
		for (iter = end()-1; iter != begin(); iter--)
			delete *iter;
		delete *iter;
	}
	clear();
}

// streaming operator
ostream& operator<<(ostream& os, const GermanTokenList& container)
{
    for (GermanTokenList::const_iterator iter = container.begin(); iter != container.end(); iter++)
    {
        (*iter)->printOn(os);
        if (!(*iter)->isTrialToken() && iter != container.end() - 1)
            os << ", ";
    }
    return os;
}

GermanTokenWord* GermanTokenList::getHeadWord() const
{
    for (GermanTokenList::const_iterator iter = end() - 1; iter >= begin(); iter--)
    {
        GermanTokenWord* word = dynamic_cast<GermanTokenWord*>(*iter);
        if (word != 0)
            return word;
    }

    return 0;
}

GermanTokenSuffix* GermanTokenList::getSuffix() const
{
    return dynamic_cast<GermanTokenSuffix*>(*(end() - 1));
}

GermanTokenWord* GermanTokenList::getNextWord(const_iterator& current)
{
    if (current == end())
	{
		return 0;
	}

    assert((*current)->isTrialToken());

    current++;
    assert(current != end());

    GermanTokenWord* word = dynamic_cast<GermanTokenWord*>(*current);
    assert(word != 0);
    current++;
    
    return word;
}

GermanTokenConnector* GermanTokenList::getNextConnector(const_iterator& current)
{
    if (current == end())
	{
		return 0;
	}
    assert((*current)->isTrialToken());
    
    if (0 == dynamic_cast<GermanTokenTrialConnector*>(*current))
        return 0;
        
    current++;
    assert(current != end());

    GermanTokenConnector* connector = dynamic_cast<GermanTokenConnector*>(*current);
    assert(connector != 0);
    current++;
    
    return connector;
}

GermanTokenTrialConnector* GermanTokenList::getNextTrialConnector(const_iterator& current)
{
	for (; current != end() && 0 == dynamic_cast<GermanTokenTrialConnector*>(*current); 
		 current++ );
	if (current == end())
	{
		return 0;
	}
	return dynamic_cast<GermanTokenTrialConnector*>(*current);
}

//---------------------------------------------------------------------
GermanTokenTrialConnector::GermanTokenTrialConnector(int maxLength, GermanTokenWord* word)
    : currentConnector_(validConnectors_.begin()), maxLength_(maxLength), word_(word), initialized_(false)
{
}

