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
//-------------------------------------------------------------------
// File - LDictionaryEntry.cpp
//
// Class - LDictionaryEntry (implementation)
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/ldictionaryentry.h>
#include <logos_libs/linguistic/prissufilter.h>

const short LDictionaryEntry::NON_COMPOUND_UNIT = 0;
const short LDictionaryEntry::HEAD_UNIT = 1;
const short LDictionaryEntry::NON_HEAD_UNIT = 2;

//-------------------------------------------------------------------
LDictionaryEntry::LDictionaryEntry()
    : p_language       ( 0 )
    , v_isUnfoundWord  ( false )
    , v_isCached       ( false )
	, v_compoundInfo(NON_COMPOUND_UNIT)
{
    semantoSyntacticUnits().reserve (6);
}
//-------------------------------------------------------------------
LDictionaryEntry::LDictionaryEntry( const LDictionaryEntry& rhs )
    : p_language       ( rhs.p_language )
    , v_isUnfoundWord  ( rhs.v_isUnfoundWord )
    , v_isCached       ( false )
	, v_compoundInfo(rhs.v_compoundInfo)
    , v_dictionaryToken( rhs.v_dictionaryToken )
{
    semantoSyntacticUnits().reserve (rhs.semantoSyntacticUnits().size());
    for( SsuList::const_iterator i =
              rhs.semantoSyntacticUnits().begin();
         i != rhs.semantoSyntacticUnits().end(); i++ )
    {
        LSemantoSyntacticUnit* unit 
          = const_cast<LSemantoSyntacticUnit*>(&(*i));
        addSsu( *unit );
    }
}
//-------------------------------------------------------------------
LDictionaryEntry::LDictionaryEntry( const LLanguage* language )
    : p_language       ( language )
    , v_isUnfoundWord  ( false )
    , v_isCached       ( false )
	, v_compoundInfo(NON_COMPOUND_UNIT)
{
    semantoSyntacticUnits().reserve (6);
}
//-------------------------------------------------------------------
void LDictionaryEntry::deleteSsuComponents()
{
    semantoSyntacticUnits().erase(semantoSyntacticUnits().begin(),
                                  semantoSyntacticUnits().end());
}
//-------------------------------------------------------------------
void
LDictionaryEntry::makeBeginningOfQuestion()
{
    // If the dictionary entry is considered the beginning of a
    // question, each of its SSU's must reflect that.

    for (SsuList::iterator i = semantoSyntacticUnits().begin();
                           i != semantoSyntacticUnits().end(); i++ )
    {
        i->makeBeginningOfQuestion();
    }
}
//-------------------------------------------------------------------
void
LDictionaryEntry::eliminateUnitsByDerivedForms()
{
    // this member removes any SSU that does not use the inflection
    // that was used in finding the DictionaryEntry. Notice that the
    // iterator operates in reverse. This is to keep the iterator
    // valid after an SSU has been deleted. an "erase" from a vector
    // causes the elements of the vector to be left-shifted, making
    // the iterator invalid for everything beyond the point of the
    // deletion.

    if (!dictionaryToken().inflectionUsed())
    {
        return;
    }
    //if (LLanguage::nullInflection() != dictionaryToken().inflectionUsed())
    //{
    SsuList::iterator i =  semantoSyntacticUnits().end() - 1;
    while( (semantoSyntacticUnits().size      ()) &&
           (i >= semantoSyntacticUnits().begin()) )
    {
        if( i->isFormDeriveable (dictionaryToken().inflectionUsed()) == false )
        {
            semantoSyntacticUnits().erase( i );
        }
        --i;
    }
    //}
}
//-------------------------------------------------------------------
void
LDictionaryEntry::eliminateUnitsForBadContexts()
{
    // Erases all ssu's that have absolutely no contextual match.

    SsuList::iterator i =  semantoSyntacticUnits().end() - 1;
    while ((semantoSyntacticUnits().size      ()) &&
           (i >= semantoSyntacticUnits().begin()))
    {
        if (Context::UnmatchedValue == i->contextValue( language() ))
        {
            semantoSyntacticUnits().erase( i );
        }
        --i;
    }
	sort(semantoSyntacticUnits().begin(), semantoSyntacticUnits().end(), ContextCompare());
}
//-------------------------------------------------------------------
void
LDictionaryEntry::limitUnitsWithinPriorityOrder()
{
    // Whenever SSU's have the same priority order, all but one of
    // then have to be removed. The conflicting units are ordered
    // by preference in the method "bestOidOfPriorityGroup()".

    typedef LgsSet(int) PrioritySet;
    typedef LgsSet(int) BestOidSet;

    PrioritySet prioritySet;
    PrioritySet pluralSet;
    BestOidSet  bestOidSet;

    for (SsuList::iterator i =  semantoSyntacticUnits().begin();
                           i != semantoSyntacticUnits().end  (); i++)
    {
        if( prioritySet.find( i->priorityOrder() ) ==
            prioritySet.end() )
        {
            prioritySet.insert( i->priorityOrder() );
        }
        else
        {
            pluralSet.insert( i->priorityOrder() );
        }
    }
    for( PrioritySet::iterator w =  pluralSet.begin();
                               w != pluralSet.end  (); w++ )
    {
        removeUnitsByLesserOids( bestOidOfPriorityGroup( *w ), *w );
    }
}
//-------------------------------------------------------------------
void
LDictionaryEntry::removeAllUnitsWithPriorityOrder( int priorityOrder )
{
    // Given a certain priority number, this method erases all ssu's
    // that have that priority number.

    SsuList::iterator i =  semantoSyntacticUnits().end() - 1;
    while ((semantoSyntacticUnits().size      ()) &&
           (i >= semantoSyntacticUnits().begin()))
    {
        if( priorityOrder == i->priorityOrder() )
        {
            semantoSyntacticUnits().erase( i );
        }
        --i;
    }
}

//-------------------------------------------------------------------
void
LDictionaryEntry::removeNonHeadUnits(void)
{
    // Given a certain priority number, this method erases all ssu's
    // that have that priority number.

    SsuList::iterator i =  semantoSyntacticUnits().end() - 1;
    while ((semantoSyntacticUnits().size      ()) &&
           (i >= semantoSyntacticUnits().begin()))
    {
        if( 485 == i->patNumber() )
        {
            semantoSyntacticUnits().erase( i );
        }
        --i;
    }
}
//-------------------------------------------------------------------
void
LDictionaryEntry::removeTitleUnits(void)
{
    // Given a certain priority number, this method erases all ssu's
    // that have that priority number.

    SsuList::iterator i =  semantoSyntacticUnits().end() - 1;
    while ((semantoSyntacticUnits().size      ()) &&
           (i >= semantoSyntacticUnits().begin()))
    {
        if( 1 == i->wordClassCode() && 5 == i->superSetID() && 91 == i->setID() && 807 == i->subSetID())
        {
            semantoSyntacticUnits().erase( i );
        }
        --i;
    }
}


//-------------------------------------------------------------------
void
LDictionaryEntry::removeAllUnitsByIntransitivity()
{
    // Remove an SSU if it is intransitive and it is of priority
    // value of 18. This method should only be used if there
    // exists an SSU of priority 18 that is transitive.

    SsuList::iterator i =  semantoSyntacticUnits().end() - 1;
    while ((semantoSyntacticUnits().size      ()) &&
           (i >= semantoSyntacticUnits().begin()))
    {
        if( (18 == i->priorityOrder()) && (!i->transitivity()) )
        {
            semantoSyntacticUnits().erase( i );
        }
        --i;
    }
}
//-------------------------------------------------------------------
void
LDictionaryEntry::limitUnitsByConflictingPriorityOrder()
{
    // According to Liz, some priority order values obviate the
    // other. The method eliminates all units with a priority
    // value that is forbidden by other priority values.

    //LSsuIterator adjective = semantoSyntacticUnits().end();
    //LSsuIterator noun      = semantoSyntacticUnits().end();

    //for ( LSsuIterator i  = semantoSyntacticUnits().begin();
    //                   i != semantoSyntacticUnits().end  (); i++ )
    //{
    //    if ((i->priorityOrder() == 18) ||
    //        (i->priorityOrder() == 19))
    //    {
    //        removeAllUnitsWithPriorityOrder( 13 );
    //        break;
    //    }
    //}
    orderSemantoSyntacticUnits();
    PriorityBasedSSUFilter priorityFilter;
    priorityFilter.filter(&semantoSyntacticUnits());
}
//-------------------------------------------------------------------
void
LDictionaryEntry::limitUnitsByTransitivity()
{
    // Removes all SSU's that are intransitive -- providing there
    // is one unit that exists that is transitive. The only units
    // that are counted here are the ones with priority value of
    // 18.

    LSsuIterator adjective = semantoSyntacticUnits().end();
    LSsuIterator noun      = semantoSyntacticUnits().end();

    for( LSsuIterator i  = semantoSyntacticUnits().begin();
                      i != semantoSyntacticUnits().end  (); i++ )
    {
        if( (i->priorityOrder() == 18) && i->transitivity() )
        {
            removeAllUnitsByIntransitivity();
            break;
        }
    }
}
//-------------------------------------------------------------------
void
LDictionaryEntry::limitUnitsByTransfer()
{
    // All SSUs that do not have a transfer to the target language
    // are eliminated here.

    SsuList::iterator i =  semantoSyntacticUnits().end() - 1;
    while( (semantoSyntacticUnits().size      ()) &&
           (i >= semantoSyntacticUnits().begin()) )
    {
        if( !(i->isSourceTransferred()) )
        {
            semantoSyntacticUnits().erase( i );
        }
        --i;
    }
}
//-------------------------------------------------------------------
void
LDictionaryEntry::removeUnitsByLesserOids( int bestOid, int priority )
{
    // This method removes all the SSU's with a give priority
    // vallue except the SSU with a given OID (object ID).

    SsuList::iterator i =  semantoSyntacticUnits().end() - 1;
    while( (semantoSyntacticUnits().size      ()) &&
           (i >= semantoSyntacticUnits().begin()) )
    {
        if( (i->priorityOrder() == priority) &&
            (i->oid() != bestOid) )
        {
            semantoSyntacticUnits().erase( i );
        }
        --i;
    }
}
//-------------------------------------------------------------------
int
LDictionaryEntry::bestOidOfPriorityGroup( int priorityOrder )
{
    // Among those SSU's with a given priority value only the one
    // with the highest context value is chosen here. Note that
    // another criteria is whether the SSU is transferred.

    SsuList::iterator best = semantoSyntacticUnits().end();

    for( SsuList::iterator i =  semantoSyntacticUnits().begin();
                           i != semantoSyntacticUnits().end  (); i++ )
    {
        if( i->priorityOrder() == priorityOrder )
        {
            if( best == semantoSyntacticUnits().end() )
            {
                best = i;
            }
            else
            {
                if( (i->isSourceTransferred()) &&
                    (   i->contextValue( language() ) >
                     best->contextValue( language() )) )
                {
                    best = i;
                }
            }
        }
    }
    assert( best != semantoSyntacticUnits().end() );
    return best->oid();
}
//-------------------------------------------------------------------
class SsuOrder
{
    // Simple functor that prioritizes two SSU's based upon
    // descending order of priority value. Simply returns a boolean
    // designating if the left-most argument is preferred.
public:
    bool operator()( const LSemantoSyntacticUnit& firstSsu,
                     const LSemantoSyntacticUnit& secondSsu )
    {
        bool isFirstLesser = false;

        if( firstSsu.priorityOrder() != secondSsu.priorityOrder() )
        {
            if( firstSsu.priorityOrder() < secondSsu.priorityOrder() )
            {
                isFirstLesser = true;
            }
        }
        return isFirstLesser;
    }
};
//-------------------------------------------------------------------
void
LDictionaryEntry::orderSemantoSyntacticUnits()
{
    // Orders all the SSU's according to criteria in the functor
    // SsuOrder -- this functor is defined above, but it selects
    // one of two Ssu's according to priority value.

    sort( semantoSyntacticUnits().begin(),
          semantoSyntacticUnits().end  (),
          SsuOrder() );
}
//-------------------------------------------------------------------
void
LDictionaryEntry::deepCopy( const LDictionaryEntry& rhs )
{
    for( SsuList::const_iterator i =
              rhs.semantoSyntacticUnits().begin();
         i != rhs.semantoSyntacticUnits().end(); i++ )
    {
        LSemantoSyntacticUnit* unit
          = const_cast<LSemantoSyntacticUnit*>(&(*i));
        addSsu( *unit );
    }
    language( rhs.p_language );
    v_dictionaryToken = rhs.v_dictionaryToken;
    v_isUnfoundWord   = rhs.v_isUnfoundWord;
    v_isCached        = rhs.v_isCached;
}
//-------------------------------------------------------------------
const LDictionaryEntry&
LDictionaryEntry::operator=( const LDictionaryEntry& rhs )
{
    if( this != &rhs )
    {
        deepCopy( rhs );
    }
    return *this;
}
