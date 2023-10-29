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
#ifndef __LDictionaryEntry_h__
#define __LDictionaryEntry_h__

//-------------------------------------------------------------------
// File - LDictionaryEntry.h
//
// Class - LDictionaryEntry (interface)
// Class - LCompoundDictionaryEntry (interface)
//
// Description - each object of this class represents one segment of
//      a sentence object that has a match in the database.
//
//-------------------------------------------------------------------

#include <logos_libs/linguistic/lsemantosyntacticunit.h>
#include <logos_libs/linguistic/llanguage.h>
#include <logos_libs/linguistic/ldictionarytoken.h>
#include <logos_libs/linguistic/lword.h>

class SsuList: public LgsVector(LSemantoSyntacticUnit)
{
};

//-------------------------------------------------------------------
class LDictionaryEntry: public Object
{
public:
	static const short NON_COMPOUND_UNIT;
	static const short HEAD_UNIT;
	static const short NON_HEAD_UNIT;

    DummyLess(LDictionaryEntry);

    LDictionaryEntry();
    LDictionaryEntry(const LDictionaryEntry&);
    LDictionaryEntry(const LLanguage*);
	virtual ~LDictionaryEntry(void);

    virtual bool isCompound();

    const SsuList& semantoSyntacticUnits() const;
    SsuList& semantoSyntacticUnits();
    const LSemantoSyntacticUnit& ssuAt(int) const;
    void orderSemantoSyntacticUnits();
    int countOfSemantoSyntacticUnits() const;         // no. ssu's in this entry.
    LSemantoSyntacticUnit& addSsu(LSemantoSyntacticUnit&);

    const LDictionaryToken& dictionaryToken() const;
    void dictionaryToken(LDictionaryToken&);

    const LgsString& ending();

    void eliminateUnitsForBadContexts();

    int derivedFormCodeOf(int) const;
    void derivedFormCode(bool germanNonHead = false);
    void eliminateUnitsByDerivedForms();

    int headWord() const;
	int hashLocation() const;

    int wordsUsedInMatch() const;
	void wordsUsedInMatch(int iNoWords); 

    const LLanguage& language() const;
    void language(const LLanguage*);

    bool isUnfoundWord() const;
    void setToUnfoundWord();

    bool isCached() const;
    void setCached(bool b = true);

    void removeAllUnitsWithPriorityOrder( int );
    void removeAllUnitsByIntransitivity();
	void removeNonHeadUnits(void);
	void removeTitleUnits(void);

    void makeBeginningOfQuestion();

    void deepCopy(const LDictionaryEntry&);

    const LDictionaryEntry& operator=( const LDictionaryEntry& );

    void limitUnitsWithinPriorityOrder       ();
    void limitUnitsByConflictingPriorityOrder();
    void limitUnitsByTransfer                ();
    void limitUnitsByTransitivity            ();
	void setOverflow3b(int ovrflow);

    void deleteSsuComponents();
	bool headUnit(void) const;
	void setAsHead(void);
	bool nonHeadUnit(void) const;
	void setAsNonHead(void);
	bool compoundUnit(void) const;
	short compoundInfo(void) const;
	void compoundInfo(short sCompoundInfo);

protected:
    void removeUnitsByLesserOids( int bestOid, int priorityOrder );
    int  bestOidOfPriorityGroup ( int priorityOrder );

private:
    LDictionaryToken	v_dictionaryToken;
    SsuList				v_semantoSyntacticUnits;
    const LLanguage*	p_language;
	LWordIterator		v_nextIterator;
    bool				v_isUnfoundWord;
    bool				v_isCached;
	short				v_compoundInfo;
};

typedef LgsVector(LDictionaryEntry) LDictionaryEntryVector;
typedef LDictionaryEntryVector::iterator LDictionaryEntryIterator;
typedef LgsVector(LDictionaryEntry*) LDictionaryEntryPVector;
typedef LDictionaryEntryPVector::iterator LDictionaryEntryPIterator;

inline bool LDictionaryEntry::isCompound()
{
    return false;
}

inline LDictionaryEntry::~LDictionaryEntry(void)
{

}

inline int LDictionaryEntry::countOfSemantoSyntacticUnits() const
{
    return semantoSyntacticUnits().size();
}

inline LSemantoSyntacticUnit& LDictionaryEntry::addSsu(LSemantoSyntacticUnit& unit)
{
    semantoSyntacticUnits().push_back( unit );

    LSemantoSyntacticUnit& rUnit = semantoSyntacticUnits().back();

    rUnit.setLanguage( &language() );

    return rUnit;
}

inline const LDictionaryToken& LDictionaryEntry::dictionaryToken() const
{
    return v_dictionaryToken;
}

inline void LDictionaryEntry::dictionaryToken( LDictionaryToken& token )
{
    v_dictionaryToken = token;
}

inline int LDictionaryEntry::derivedFormCodeOf( int i ) const
{
    LSemantoSyntacticUnit& ssu = const_cast<LSemantoSyntacticUnit&>
                                    ( ssuAt( i ) );
    return ssu.derivedFormCode ();
}

inline void LDictionaryEntry::derivedFormCode(bool germanNonHead)
{

   SsuList & ssuLst = semantoSyntacticUnits();
   SsuList::iterator endIter = ssuLst.end();
   for (SsuList::iterator i = ssuLst.begin(); i != endIter; i++)
   {
      i->derivedFormCode (dictionaryToken().inflectionUsed(), germanNonHead);
   }
}

inline void LDictionaryEntry::setOverflow3b(int ovrflow)
{
   SsuList & ssuLst = semantoSyntacticUnits();
   SsuList::iterator endIter = ssuLst.end();
   for (SsuList::iterator i = ssuLst.begin(); i != endIter; i++)
   {
      i->setOverflow3b(ovrflow);
   }
}


inline void LDictionaryEntry::language( const LLanguage* aLanguage )
{
    p_language = aLanguage;
}

inline const SsuList& LDictionaryEntry::semantoSyntacticUnits() const
{
    return v_semantoSyntacticUnits;
}

inline SsuList& LDictionaryEntry::semantoSyntacticUnits()
{
    return v_semantoSyntacticUnits;
}

inline const LLanguage& LDictionaryEntry::language() const
{
    return *p_language;
}

inline const LSemantoSyntacticUnit& LDictionaryEntry::ssuAt( int i ) const
{
    assert( semantoSyntacticUnits().size() > i );
    return semantoSyntacticUnits()[i];
}

inline int LDictionaryEntry::headWord() const
{
    return ssuAt( 0 ).headWord();
}

inline int LDictionaryEntry::hashLocation() const
{
	return ssuAt(0).hashLocation();
}

inline bool LDictionaryEntry::isUnfoundWord() const
{
    return v_isUnfoundWord;
}

inline void LDictionaryEntry::setToUnfoundWord()
{
    v_isUnfoundWord = true;
}

inline bool LDictionaryEntry::isCached () const
{
    return v_isCached;
}

inline void LDictionaryEntry::setCached( bool b )
{
    v_isCached = b;
}

inline int LDictionaryEntry::wordsUsedInMatch() const
{
    return dictionaryToken().wordsUsedInMatch();
}

inline void LDictionaryEntry::wordsUsedInMatch(int iNoWords) 
{
	v_dictionaryToken.setWordsUsedInMatch(iNoWords);
}

inline void LDictionaryEntry::setAsHead(void) 
{
	v_compoundInfo = HEAD_UNIT;
}

inline bool LDictionaryEntry::headUnit(void) const 
{
	return (HEAD_UNIT == v_compoundInfo);
}

inline void LDictionaryEntry::setAsNonHead(void) 
{
	v_compoundInfo = NON_HEAD_UNIT;
}


inline bool LDictionaryEntry::nonHeadUnit(void) const 
{
	return (NON_HEAD_UNIT == v_compoundInfo);
}

inline bool LDictionaryEntry::compoundUnit(void) const
{
	return (NON_COMPOUND_UNIT != v_compoundInfo);
}

inline short LDictionaryEntry::compoundInfo(void) const
{
	return v_compoundInfo;
}

inline void LDictionaryEntry::compoundInfo(short sCompoundInfo)
{
	v_compoundInfo = sCompoundInfo;
}

//---------------------------------------------------------------------
// holds a list of dictionary entries - used by german decomposition
// a compound word splits into a number of dictionary entries
// note that the instance variables of the parent class are not used
// the only instance variables that will be used are the language and the
// vector of entries.

class LCompoundDictionaryEntry: public LDictionaryEntry
{
    DisableCopyAssign(LCompoundDictionaryEntry);

public:
    LCompoundDictionaryEntry(const LLanguage* language);
    virtual bool isCompound();
    void addDictionaryEntry(LDictionaryEntry* entry);
    int numberOfEntries();
    LDictionaryEntry* getEntry(int i);

protected:
    LDictionaryEntryPVector entries_;
};

inline LCompoundDictionaryEntry::LCompoundDictionaryEntry(const LLanguage* language)
    : LDictionaryEntry(language)
{
}

inline bool LCompoundDictionaryEntry::isCompound()
{
    return true;
}

inline void LCompoundDictionaryEntry::addDictionaryEntry(LDictionaryEntry* entry)
{
    entries_.push_back(entry);
}

inline int LCompoundDictionaryEntry::numberOfEntries()
{
    return entries_.size();
}

inline LDictionaryEntry* LCompoundDictionaryEntry::getEntry(int i)
{
    return entries_[i];
}

#endif // __LDictionaryEntry_h__


