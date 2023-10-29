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
#ifndef __GermanToken_h__
#define __GermanToken_h__

//---------------------------------------------------------------------
// File - GermanToken.h
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
// Description - Tokens returned when parsing German compound words
//
//---------------------------------------------------------------------

#include <logos_libs/halfnoun/germandictionaryentry.h>

//---------------------------------------------------------------------
// abstract base class of all tokens
// a decomposition of a compound word will have the following list of tokens
// (TrialWord Word (TrialConnector Connector)?)+ (TrialWord Word (TrialSuffix Suffix)?)
//       where ? means 0 or 1, and + means 1 or more

class GermanTokenAbstract
{
public:
	virtual ~GermanTokenAbstract(void) { }
    virtual GermanTokenAbstract* clone() const = 0;
    virtual void printOn(ostream& os) const = 0;
    virtual bool isTrialToken();
};

inline bool GermanTokenAbstract::isTrialToken()
{
    return false;
}

//---------------------------------------------------------------------
// abstract base class of all tokens returned from the backtrack algorithm
class GermanToken: public GermanTokenAbstract
{
public:
	virtual ~GermanToken(void) { }
    GermanToken(const LgsString& text);
    GermanToken(const char* text, int length);

    LgsString text_;
};

inline GermanToken::GermanToken(const LgsString& text)
    : text_(text)
{
}

inline GermanToken::GermanToken(const char* text, int length)
    : text_(text, length)
{
}

//---------------------------------------------------------------------
// token representing a word in the dictionary
class GermanTokenWord: public GermanToken
{
public:
	typedef pair<const GermanDictionaryEntry*, short> DictionaryEntryLink;
	typedef LgsList(DictionaryEntryLink) EntryLinkList;
	typedef EntryLinkList::iterator EntryLinkIterator;
	typedef EntryLinkList::const_iterator EntryLinkConstIterator;

	~GermanTokenWord(void) { };
    GermanTokenWord(const LgsString& text, const LgsList(const GermanDictionaryEntry*)& entries);
	GermanTokenWord(const LgsString& text, const EntryLinkList & entries);

    void printOn(ostream& os) const;
    virtual GermanTokenAbstract* clone() const;
    GermanConnectorFlag getConnectorRequirement();
	bool isDoubleEnded(void) const;
	bool allowedAsNonHead(int wcCode, int patValue) const;
	void decLinkCount(const GermanDictionaryEntry *dictEntry);
	void incLinkCount(const GermanDictionaryEntry *dictEntry);
	void removeNoConnMatchEntries(void);
	bool allowedWordClass(int wcCode, int patValue) const;
	bool matchWithUpperCase(void) const;
	bool matchWithLowerCase(void) const;

    EntryLinkList entries_;
};

inline GermanTokenAbstract* GermanTokenWord::clone() const
{
    return new GermanTokenWord(text_, entries_);
}

//---------------------------------------------------------------------
// token representing a connector between words - not at the ends
class GermanTokenConnector: public GermanToken
{
public:
    GermanTokenConnector(const LgsString& text);
    GermanTokenConnector(const char* text, int length);
    void printOn(ostream& os) const;
    virtual GermanTokenAbstract* clone() const;
};

inline GermanTokenConnector::GermanTokenConnector(const LgsString& text)
    : GermanToken(text)
{
}

inline GermanTokenConnector::GermanTokenConnector(const char* text, int length)
   : GermanToken(text, length)
{
}     

inline void GermanTokenConnector::printOn(ostream& os) const
{
    os << text_;
}

inline GermanTokenAbstract* GermanTokenConnector::clone() const
{
    return new GermanTokenConnector(text_);
}

//---------------------------------------------------------------------
// token representing a suffix of the compound word - at the end
class GermanTokenSuffix: public GermanToken
{
public:
    GermanTokenSuffix(const LgsString& text);
    virtual void printOn(ostream& os) const;
    virtual GermanTokenAbstract* clone() const;
};

inline GermanTokenSuffix::GermanTokenSuffix(const LgsString& text)
    : GermanToken(text)
{
}

inline void GermanTokenSuffix::printOn(ostream& os) const
{
    os << text_;
}

inline GermanTokenAbstract* GermanTokenSuffix::clone() const
{
    return new GermanTokenSuffix(text_);
}

//---------------------------------------------------------------------
// abstract base class of all tokens not returned from the backtrack algorithm
// represent trial tokens
// the backtrack algorithm will then try to append a token of the corresponding type
class GermanTokenTrial: public GermanTokenAbstract
{
    virtual bool isTrialToken();
public:
    virtual ~GermanTokenTrial();
};
inline GermanTokenTrial::~GermanTokenTrial(void)
{

}

inline bool GermanTokenTrial::isTrialToken()
{
    return true;
}

//---------------------------------------------------------------------
// token representing a trial word
// the following token will be a word token of length <= length_
// length_ will be adjusted to the word length
class GermanTokenTrialWord: public GermanTokenTrial
{
public:
    GermanTokenTrialWord(int length);
    void printOn(ostream& os) const;
    virtual GermanTokenAbstract* clone() const;

    int length_;
};

inline GermanTokenTrialWord::GermanTokenTrialWord(int length)
    : length_(length)
{
}

inline void GermanTokenTrialWord::printOn(ostream& os) const
{
    os << 'w' << length_ << ":";
}

inline GermanTokenAbstract* GermanTokenTrialWord::clone() const
{
    return new GermanTokenTrialWord(length_);
}

//---------------------------------------------------------------------
// token representing a trial connector
// the following token will be a connector of length <= length_
// length_ will be adjusted to the connector length
struct ConnectorInfo
{
   ConnectorInfo(short connLength, short connWeight, const GermanDictionaryEntry * dictEntry);
   virtual ~ConnectorInfo(void);
   const ConnectorInfo & operator=(const ConnectorInfo & rhs);
   bool operator <(const ConnectorInfo & rhs) const;
   const GermanDictionaryEntry *dictEntry(void) const;
   short m_length;
   short m_weight;
   const GermanDictionaryEntry *m_dictEntry;
};

inline ConnectorInfo::ConnectorInfo(short connLength, short connWeight, const GermanDictionaryEntry * dictEntry):
   m_length(connLength), m_weight(connWeight), m_dictEntry(dictEntry)
{

}

inline ConnectorInfo::~ConnectorInfo(void)
{
}

inline const GermanDictionaryEntry* ConnectorInfo::dictEntry() const
{
	return m_dictEntry;
}

inline const ConnectorInfo & ConnectorInfo::operator=(const ConnectorInfo & rhs) 
{
   m_length = rhs.m_length;
   m_weight = rhs.m_weight;
   m_dictEntry = rhs.m_dictEntry;
}

inline bool ConnectorInfo::operator <(const ConnectorInfo & rhs) const
{
	if (m_weight > rhs.m_weight)
	{
		return true;
	}
	bool retValue = false;

	if (m_weight == rhs.m_weight)
	{
	   retValue = (m_length > rhs.m_length)? true: (m_length == rhs.m_length && m_dictEntry > rhs.m_dictEntry);
	}
	return retValue;
}

class GermanTokenTrialConnector: public GermanTokenTrial
{
public:
    GermanTokenTrialConnector(int maxLength, GermanTokenWord* word);
    ~GermanTokenTrialConnector(void);
    void insert(short connWeight, short connLength, const GermanDictionaryEntry * dictEntry);
    int maxLength(void) const;
    int length(void) const;
    bool backtrack(void);
    void reset(void);
    void printOn(ostream& os) const;
    virtual GermanTokenAbstract* clone() const;
    bool initialized(void) const;
    void setAsInitialized(void);
	void unlinkInvalidEntries(void);
    typedef LgsSet(ConnectorInfo) ConnectorInfoSet;
    ConnectorInfoSet validConnectors_;
    ConnectorInfoSet::const_iterator currentConnector_;
    int maxLength_;
	// the pointer to GermanTokenWord is NOT owned by this class,
	// DO NOT delete this pointer
    GermanTokenWord* word_;
    bool initialized_;
};

inline void GermanTokenTrialConnector::insert(short connWeight, short connLength, const GermanDictionaryEntry *dictEntry)
{
	validConnectors_.insert(ConnectorInfo(connLength, connWeight, dictEntry));
	if (dictEntry)
		word_->incLinkCount(dictEntry);
}

inline GermanTokenTrialConnector::~GermanTokenTrialConnector(void)
{
   for (currentConnector_ = validConnectors_.begin(); currentConnector_ != validConnectors_.end();
		currentConnector_++)
   {
		if ((*currentConnector_).dictEntry())
			word_->decLinkCount((*currentConnector_).dictEntry());
   }
   validConnectors_.erase(validConnectors_.begin(), validConnectors_.end());
}

inline void GermanTokenTrialConnector::reset(void)
{
   currentConnector_ = validConnectors_.begin();
}

inline bool GermanTokenTrialConnector::initialized(void) const
{
   return initialized_;
}

inline void GermanTokenTrialConnector::setAsInitialized(void)
{
   initialized_ = true;
}

inline int GermanTokenTrialConnector::maxLength(void) const
{
   return maxLength_;
}

inline int GermanTokenTrialConnector::length(void) const
{
   return (validConnectors_.empty() || currentConnector_ == validConnectors_.end()) ? -1 : (*currentConnector_).m_length;
}

inline bool GermanTokenTrialConnector::backtrack(void)
{
	if (validConnectors_.empty() || currentConnector_ == validConnectors_.end())
	{
	  return false;
	}
	LgsSet(ConnectorInfo)::const_iterator savedIter = currentConnector_;
	int currLength = (*currentConnector_).m_length;
	for (;currentConnector_ != validConnectors_.end(); currentConnector_++)
	{
		if (currLength == (*currentConnector_).m_length)
		{
			word_->decLinkCount((*currentConnector_).dictEntry());
			ConnectorInfo & connInfo = const_cast<ConnectorInfo&>(*currentConnector_);
			connInfo.m_dictEntry = 0;
		}
	}

	// point to the next valid connector 
	for (currentConnector_= savedIter; 
		 currentConnector_ != validConnectors_.end() && (*currentConnector_).dictEntry() == 0; 
		 currentConnector_++);
	
	return (currentConnector_ != validConnectors_.end());
}

inline void GermanTokenTrialConnector::unlinkInvalidEntries(void)
{
	LgsSet(ConnectorInfo)::const_iterator savedIter = currentConnector_;
	int validLength = (*currentConnector_).m_length;
	for (currentConnector_ = validConnectors_.begin();
		 currentConnector_ != validConnectors_.end(); currentConnector_++)
	{
		if (validLength != (*currentConnector_).m_length)
		{
			word_->decLinkCount((*currentConnector_).dictEntry());
			ConnectorInfo & connInfo = const_cast<ConnectorInfo&>(*currentConnector_);
			connInfo.m_dictEntry = 0;
		}
	}
	currentConnector_ = savedIter;
}

inline void GermanTokenTrialConnector::printOn(ostream& os) const
{
    if (!validConnectors_.empty() && currentConnector_ != validConnectors_.end())
    {
      os << 'c' << (*currentConnector_).m_length;
      if ((*currentConnector_).m_length > 0)
         os << ":";
    }
}

inline GermanTokenAbstract* GermanTokenTrialConnector::clone() const
{
    GermanTokenTrialConnector *trialConnector = new GermanTokenTrialConnector(maxLength_, word_);
    LgsSet(ConnectorInfo)::const_iterator endIter = validConnectors_.end();
    for (LgsSet(ConnectorInfo)::const_iterator currInfo = validConnectors_.begin(); 
         currInfo != endIter; currInfo++)
    {
      trialConnector->insert((*currInfo).m_weight, (*currInfo).m_length, (*currInfo).m_dictEntry);
    }
    trialConnector->currentConnector_ = trialConnector->validConnectors_.find(*currentConnector_);
    trialConnector->initialized_ = initialized_;
    return trialConnector;
}

//---------------------------------------------------------------------
// token representing a trial suffix
// the following token will be a suffix
class GermanTokenTrialSuffix: public GermanTokenTrial
{
public:
    GermanTokenTrialSuffix(const GermanTokenWord* word);
    void printOn(ostream& os) const;
    virtual GermanTokenAbstract* clone() const;

    const GermanTokenWord* word_;
};

inline GermanTokenTrialSuffix::GermanTokenTrialSuffix(const GermanTokenWord* word)
    : word_(word)
{
}

inline void GermanTokenTrialSuffix::printOn(ostream& os) const
{
    os << 's' << ":";
}

inline GermanTokenAbstract* GermanTokenTrialSuffix::clone() const
{
    return new GermanTokenTrialSuffix(word_);
}

//---------------------------------------------------------------------
class GermanTokenList: public LgsVector(GermanTokenAbstract*)
{
public:
    GermanTokenList(void);
    ~GermanTokenList();
    GermanTokenList(const GermanTokenList& rhs);
    void operator=(const GermanTokenList& rhs);
    void append(const GermanTokenList& rhs);

	void ownsElements(bool ownership);
    // get the last word in the token list, 0 if none
    GermanTokenWord* getHeadWord() const;
    
    // get the suffix in the token list, 0 if none
    GermanTokenSuffix* getSuffix() const;

    // get the next word token
    // current is the current postion in the list - it is incremented past the word token   
    // pre-condition: current is pointing at the trial word token
    // see comment at top of this file regarding sequence of tokens
    GermanTokenWord* getNextWord(const_iterator& current);

    // get the next connector token - at the current position
    // note: if the next token is a word token - 0 is returned
    // current is the current postion in the list - it is incremented past the word token   
    // pre-condition: current is pointing at the trial connector or word token
    // see comment at top of this file regarding sequence of tokens
    GermanTokenConnector* getNextConnector(const_iterator& current);
	GermanTokenTrialConnector* getNextTrialConnector(const_iterator& current);
	void erase(iterator element);
    // erase all items
    void erase();

private:
	bool ownsElements_;
};

ostream& operator<<(ostream& os, const GermanTokenList& container);
inline GermanTokenList::GermanTokenList(void) : ownsElements_(true)
{

}

inline void GermanTokenList::ownsElements(bool ownership)
{
	ownsElements_ = ownership;
}
#endif



