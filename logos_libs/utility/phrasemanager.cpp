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
// file: PhraseManager.cpp

#include <logos_include/logoscommon.h>
#include <logos_libs/utility/phrasemanager.h>
#include <logos_libs/utility/phrasemanagerstrategy.h>
#include <logos_libs/utility/phrasemanagerinterface.h>
#include <logos_libs/utility/stringutil.h>

//for debug code
//#include <logos_libs/utility/PhraseDebug.h>

//----------------------------------------------------------------------
// template <class Key>
PhraseManager::PhraseManager(PhraseManagerStrategy* strategy, char separator)
    : strategy_(strategy), separator_(separator)
{
   interface_ = new PhraseManagerInterface(this);
   
   if (strategy_ == 0)
      strategy_ = new DefaultPhraseManagerStrategy();
   
   strategy_->SetManager(GetPhraseManagerInterface());
}

// template <class Key>
PhraseManager::~PhraseManager()
{
   delete strategy_;
   delete interface_;

   // delete phrases in phraseList_
   PhraseIterator current;
   
   for (current = phraseList_.begin(); current != phraseList_.end(); current++)
   {
      delete (*current);
   }
}

// template <class Key>
void PhraseManager::changeStrategy(PhraseManagerStrategy* strategy)
{
    delete strategy_;
    strategy_ = strategy;
    strategy->SetManager(GetPhraseManagerInterface());
}

// template <class Key>
void PhraseManager::appendPhrase(const LgsString& phrase, 
                                 PhraseManager::Key key, 
                                 int trailingSpaces)
{
   Phrase* newPhrase = new Phrase(key, phrase, trailingSpaces, separator_);
   lookup_[key] = newPhrase;
   phraseList_.push_back(newPhrase);
   appendPhraseToBuffer(phrase, trailingSpaces);
}

// template <class Key>
Phrase* PhraseManager::lookup(PhraseManager::Key key) const
{
    LgsMap(Key, Phrase*)::const_iterator iter = lookup_.find(key);
    assert(iter != lookup_.end());
    return (*iter).second;
}

// template <class Key>
// could add caching so that this is not invoked more than once, when it has not changed
// but with a small number of phrases this should not be an issue
bool PhraseManager::phraseExists(PhraseManager::Key key) const
{
    LgsMap(Key, Phrase*)::const_iterator iter = lookup_.find(key);
    return iter != lookup_.end();
}

// template <class Key>
LgsString PhraseManager::getPhrase(PhraseManager::Key key, 
                                int* trailingSpaces) const
{
    return lookup(key)->getText(trailingSpaces);
}

// template <class Key>
LgsString PhraseManager::getFullPhrase(PhraseManager::Key key) const
{
    return lookup(key)->getFullText();
}

// template <class Key>
bool PhraseManager::phraseChanged(PhraseManager::Key key) const
{
    return lookup(key)->isDirty();
}

// template <class Key>
int PhraseManager::getPhrasePos(PhraseManager::Key key) const
{
    return getPhrasePos(lookup(key));
}

// template <class Key>
int PhraseManager::getPhrasePos(Phrase* phrase) const
{
    int offset;
    PhraseConstIterator current;
    for (current = phraseList_.begin(), offset = 0;
         *current != phrase;
         offset += (*current)->length(), current++) // order is important
        ;
    return offset;
}

// template <class Key>
PhraseManager::Key PhraseManager::getPhraseAtPos(int pos)
{
    PhraseIterator current;
    int offset;
    int phraseLength;

    for (current = phraseList_.begin(), offset = 0;
         current != phraseList_.end();
         current++, offset += phraseLength)
    {
        phraseLength = (*current)->length();
        
        if (offset +  phraseLength > pos)
        {
            // found phrase
            return (*current)->getKey();
        }
    }

    assert(("Invalid position", 0));
    return Key();
}

// template <class Key>
LgsString PhraseManager::getText() const
{
    return LgsString(buffer_.begin(), buffer_.begin() + buffer_.size());
}

// template <class Key>
int PhraseManager::getTextLength() const
{
    return buffer_.size();
}

// template <class Key>
void PhraseManager::replace(int position, int length, const LgsString& replacement)
{
   assert(position >= 0);
   assert(length >= 0);
   assert(position + length <= buffer_.size());
   assert(phraseList_.size() != 0);

   //debug code
   //dumpString(cout, "LgsString replace", getText().replace(pos, length, replacement));

   int pos = position;
   int len = length;
   LgsString repText = replacement;

   // ---- step 1: adjust position and length for minimum replacement
   adjForMinReplace(pos, len, repText);

   // ---- step 2: get starting and ending position details
   getBounds(pos, len);

   //debug code
   //cout << "  startPhrase " << matchBounds_.startPhrase - phraseList_.begin() << endl;
   //cout << "  endPhrase " << matchBounds_.endPhrase - phraseList_.begin() << endl;
   //cout << "  startOffsetInPhrase " << matchBounds_.startOffsetInPhrase << endl;
   //cout << "  endOffsetInPhrase " << matchBounds_.endOffsetInPhrase << endl;

   // ---- step 3: replace text in buffer
   replaceText(pos, len, repText);

   //debug code
   //dumpCharVector(cout, "buffer_ replace", buffer_);

   // --- if there is only one phrase do a simple replace
   if (matchBounds_.endPhrase == matchBounds_.startPhrase + 1)
   {
      (*matchBounds_.startPhrase)->replace(matchBounds_.startOffsetInPhrase, len, repText);
   }
   else
   {
      // ---- step 4: count words in each relevant sub-phrase, and remove affected sub-phrases
      countAndDeleteWords();

      //debug code
      //dumpVector(cout, "wordCount", wordCount_);

      // ---- step 5: split replacement text into words
      replacementWords_.resize(0);
      LgsIntVector oSpaces;
      StringUtil::parseInto(repText, replacementWords_, oSpaces);
      LgsIntIterator oSpaceIter = oSpaces.begin();
      LgsStringIterator oWordIter = replacementWords_.begin();
      if (*oSpaceIter != 0)
      {
         (*oWordIter) = " " + (*oWordIter);
      }
/*      if (*oSpaceIter == 0)
      {
         replacementWords_.insert(replacementWords_.begin(), "");
      }
      else
      {
         replacementWords_.insert(replacementWords_.begin(), " ");
      }
      LgsStringIterator oWordIter = replacementWords_.begin() + 1;*/
      for (oSpaceIter = oSpaces.begin() + 1; oWordIter != replacementWords_.end(); oWordIter++, oSpaceIter++)
      {
         if (*oSpaceIter > 0)
         {
            (*oWordIter) += ' ';
         }
         else
         {
            if ((oWordIter + 1) != replacementWords_.end())
            {
               (*oWordIter) += (*(oWordIter + 1));
               replacementWords_.erase(oWordIter + 1);
               oWordIter--;
            }
         }
      }
//      LgsStringIterator oWordIterator=replacementWords_.begin();
//      LgsStringIterator oEndIterator=replacementWords_.end();
//      for (LgsIntIterator oSpaceIterator = oSpaces.begin()+1;
//           oWordIterator != oEndIterator; oWordIterator++, oSpaceIterator++)
//      {
//         for (int spaceCount = (*oSpaceIterator); spaceCount; (*oWordIterator) += ' ', spaceCount--);
//      }
      //debug code
      //dumpVector(cout, "words", replacementWords_);

      // ---- step 6: call strategy to distribute buffer-words into phrases
      // if no replace words - nothing to do
      if (replacementWords_.size() != 0)
      {
         strategy_->distribute(replacementWords_.size(), (const LgsIntVector&) wordCount_, totalWordCount_);
      }
   }

   // ---- step 7: remove any blank phrases
   removeBlankPhrases();
}

void PhraseManager::adjForMinReplace(int& pos, int& len, LgsString &replacement)
{
   bool done = false;
   int adj = 0;
   LgsString::iterator currReplaceChar = replacement.begin();
   CharacterVector::iterator currChar = buffer_.begin() + pos;

   while (!done && (adj < replacement.length()))
   {
      if (*currReplaceChar == *currChar)
      {
         adj++;
         currReplaceChar++;
         currChar++;
      }
      else
         done = true;
   }

   if (adj > 0)
   {
      replacement = replacement.substr(adj, replacement.length() - adj);
      pos += adj;
      len -= adj;
      len = max(0, len);
   }
}

// template <class Key>
void PhraseManager::appendString(const LgsString& s, CharacterVector& buffer)
{
    for (LgsString::const_iterator current = s.begin(); current != s.end(); current++)
        buffer.push_back(*current);
}

// template <class Key>
void PhraseManager::appendPhraseToBuffer(const LgsString& phrase, int trailingSpaces)
{
    appendString(phrase, buffer_);
    for (int i = 0; i < trailingSpaces; i++)
        buffer_.push_back(separator_);
}

// template <class Key>
void PhraseManager::getBounds(int pos, int length)
{
    // example - replacing the following substring (s) in the text below
    // text:      aaaaa bbbbb ccccc ddddd eeeee
    // substring s:      xxxxxxxxxxxxxx      - pos = 7, len = 10
    //
    // startPhrase_.phrase = 1 = starting phrase ("bbbbb")
    // startPhrase_.offset = 6 = offset of starting phrase
    //      (a separator is regarded as part of the preceding phrase)
    //      (the end of the buffer is regarded as part of the last phrase)
    // startOffsetInPhrase_ = 1 = start of s
    //
    // endPhrase_.phrase = 4 = one beyond ending phrase   ("eeeee")
    // endPhrase_.offset = 23 = offset just beyond ending phrase
    //          (at separator or end of buffer)
    // endOffsetInPhrase_ = 3 = one beyond end of s

    PhraseIterator current;
    int offset;
    int phraseLength;
    for (current = phraseList_.begin(), offset = 0;
         current != phraseList_.end();
         current++, offset += phraseLength)
    {
        phraseLength = (*current)->length();
        if (offset +  phraseLength > pos)
        {
            // found start phrase
            matchBounds_.startPhrase = current;
            matchBounds_.startOffsetInPhrase = pos - offset;

            // now search for ending position details
            pos += length;
            int prevOffset;
            do
            {
                prevOffset = offset;
                offset += (*current++)->length();
            }
            while (current != phraseList_.end() && offset < pos);

            // found end phrase
            matchBounds_.endPhrase = current;
            matchBounds_.endOffsetInPhrase = pos - prevOffset;

            return;
        }
    }

    // reached here - so must be at end of the sentance
    assert(pos == buffer_.size());
    assert(length == 0);

    // setup to replace beyond the last phrase
    matchBounds_.startPhrase = current - 1;
    matchBounds_.endPhrase = current;
    matchBounds_.startOffsetInPhrase = matchBounds_.endOffsetInPhrase = (*matchBounds_.startPhrase)->length();
}

// template <class Key>
void PhraseManager::replaceText(int pos, int length, const LgsString& replacement)
{
    // insertion point
    CharacterVector::iterator current = buffer_.begin() + pos;

    // erase or insert elements into the buffer to get to the correct size
    int deltaLength = replacement.length() - length;  // increase in size
    if (deltaLength > 0)
        buffer_.insert(current, deltaLength, 0);
    else if (deltaLength < 0)
        buffer_.erase(current, current - deltaLength);

    // now replace the replacement LgsString into the buffer
    // trap - don't use current as last parameter as it could have changed in last step
    copy(replacement.begin(), replacement.end(), buffer_.begin() + pos);
}

// template <class Key>
int PhraseManager::startPos(const PhraseConstIterator& current) const
{
    return (current == matchBounds_.startPhrase) ? matchBounds_.startOffsetInPhrase : 0;
}

// template <class Key>
int PhraseManager::endPos(const PhraseConstIterator& current) const
{
    return (current + 1 == matchBounds_.endPhrase) ? matchBounds_.endOffsetInPhrase : (*current)->length();
}

// template <class Key>
void PhraseManager::countAndDeleteWords()
{
    wordCount_.resize(0);
    totalWordCount_ = 0;
    for (PhraseIterator current = matchBounds_.startPhrase;
         current != matchBounds_.endPhrase;
         current++)
    {
        // remove sub-phrase, store insertion point, and count words
        int words = (*current)->eraseAndCountWords(startPos(current), endPos(current));
        totalWordCount_ += words;
        wordCount_.push_back(words);
    }
}

void PhraseManager::removePhrase(PhraseIterator& current)
{
    // delete phrase
    delete (*current);

    // remove phrase from collection - note that erase invalidates iterators after point of erasure
    PhraseIterator prev = current - 1;
    phraseList_.erase(current);
    current = prev + 1;
}

void PhraseManager::removeBlankPhrases()
{
    PhraseIterator current = matchBounds_.startPhrase;
    while (current != matchBounds_.endPhrase)
    {
        if ((*current)->isBlank())
        {
            // found a blank phrase

            // remove the phrase from the character buffer
            int length = (*current)->length();
            if (length != 0)
            {
                CharacterVector::iterator start = buffer_.begin() + getPhrasePos(*current);
                buffer_.erase(start, start + length);
            }

            // save the key
            Key key = (*current)->getKey();

            // remove the current phrase from the collection and delete the phrase
            removePhrase(current);

            // update matchBounds_.endPhrase - since the vector has moved the elements
            // revisit this code if the vector of phrases becomes another type of container
            matchBounds_.endPhrase--;

            // remove phrase from lookup_ map
            int erased = lookup_.erase(key);
            assert(erased == 1);
        }
        else
            current++;
    }
}

// template <class Key>
void PhraseManager::distribute(int startWord, int words, int phrase)
{
    //debug code
    //cout << "distribute(startWord = " << startWord
    //     << ", words = " << words
    //     << ", phrase = " << phrase << ")"
    //     << endl;

    if (words == 0)
        return;

    assert(startWord >= 0 && startWord < replacementWords_.size());
    assert(phrase >= 0 && phrase < matchBounds_.endPhrase - matchBounds_.startPhrase);

    // build up buffer to insert into phrase
    CharacterVector buffer;
    LgsStringConstIterator currentWord = replacementWords_.begin() + startWord;
    while (words-- > 0)
    {
        appendString(*currentWord++, buffer);
        //if (words != 0)
        //    buffer.push_back(separator_);      //+++ append a single separator only
    }

    // do the insertion into the phrase
    PhraseIterator currentPhrase = matchBounds_.startPhrase + phrase;
    (*currentPhrase)->insert(buffer);
}

PhraseIterator PhraseManager::getIterator(Phrase* phrase)
{
    // search sequentially for the phrase
    PhraseIterator current;
    for (current = phraseList_.begin(); *current != phrase; current++)
        ;
    assert(current != phraseList_.end());
    return current;
}

PhraseConstIterator PhraseManager::getIterator(Phrase* phrase) const
{
    // search sequentially for the phrase
    PhraseConstIterator current;
    for (current = phraseList_.begin(); *current != phrase; current++)
        ;
    assert(current != phraseList_.end());
    return current;
}

bool PhraseManager::getNextKey(PhraseManager::Key& key) const
{
    PhraseConstIterator current = getIterator(lookup(key));

    // increment current and test for end of list
    if (++current == phraseList_.end())
        return false;

    // update key and return found
    key = (*current)->getKey();
    return true;
}

bool PhraseManager::getPrevKey(PhraseManager::Key& key) const
{
    PhraseConstIterator current = getIterator(lookup(key));

    // test for start of list
    if (current == phraseList_.begin())
        return false;

    // decrement current, update key and return found
    current--;
    key = (*current)->getKey();
    return true;
}

void PhraseManager::merge(PhraseManager::Key key, int noOfPhrases)
{
    assert(noOfPhrases > 1);

    PhraseIterator first = getIterator(lookup(key));
    PhraseIterator current = first + 1;

    CharacterVector buffer;                         // buffer for merged phrases
    appendString((*first)->getFullText(), buffer);  // include trailing spaces

    noOfPhrases--;
    while (noOfPhrases-- > 0)
    {
        assert(current != phraseList_.end());

        // collect the phrases in the buffer
        if (noOfPhrases != 0)
        {
            // not on the last phrase
            // append the trailing spaces in the buffer as well
            appendString((*current)->getFullText(), buffer); // include trailing spaces
        }
        else
        {
            // on the last phrase
            // don't append trailing spaces in the buffer
            int trailingBlanks;
            appendString((*current)->getText(&trailingBlanks), buffer);

            // replace the text and trailing spaces of the first phrase
            (*first)->setText(buffer, trailingBlanks);
        }

        // remove the current phrase
        removePhrase(current);
    }
}

void PhraseManager::moveLeadingSpaces()
{
    if (phraseList_.size() < 2)
        return;

    PhraseIterator current;
    PhraseIterator next;
    for (current = phraseList_.begin(), next = current + 1;
         next != phraseList_.end();
         current = next++)
    {
        (*current)->moveLeadingSpaces(*next);
    }
}

void PhraseManager::erase()
{
   // delete phrases in phraseList_
   PhraseIterator current;

   for (current = phraseList_.begin(); current != phraseList_.end(); current++)
   {
	  delete (*current);
   }
   phraseList_.erase(phraseList_.begin(), phraseList_.end());
   lookup_.erase(lookup_.begin(), lookup_.end());
   buffer_.erase(buffer_.begin(), buffer_.end());
}

