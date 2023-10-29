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
// File - Phrase.h

#ifndef __Phrase_h__
#define __Phrase_h__

#include <logos_libs/utility/charactervector.h>

// final class
// individual phrase details
// template <class Key>
class Phrase
{
public:
   typedef int Key;

    DefaultConstructor(Phrase)
    Phrase(Key key, const LgsString& text, int trailingSpaces, char separator);

    // returns the key for the phrase
    Key getKey() const;

    // replace text, set dirty flag
    void replace(int pos, int length, const LgsString& replacement);

    // erase text, set dirty flag, count words, store insertion pt
    int eraseAndCountWords(int startPos, int endPos);

    // insert text at stored insertion point
    void insert(const CharacterVector& buffer);

    // Get text - including trailing spaces. not const - changes the dirty flag.
    LgsString getFullText();

    // Get text - without trailing spaces. not const - changes the dirty flag.
    // If trailingSpaces is not null, the number of trailing spaces is returned in this parameter.
    LgsString getText(int* trailingSpaces);

    // Set the text, together with the trailing spaces
    void setText(const CharacterVector& buffer, int trailingSpaces);

    // has phrase already been read with getText()
    bool isDirty() const;

    // length of phrase with trailing spaces
    int length() const;

    // return true iff the phrase consists of only blanks
    bool isBlank() const;

    // remove leading spaces from the next phrase
    // increment the trailing spaces of this phrase by spaces removed
    void moveLeadingSpaces(Phrase* nextPhrase);

private:
    Key key_;                // key for phrase
    LgsString text_;            // text of the phrase
    bool dirty_;             // phrase has been modified
    char separator_;    // separator between words in the phrase
    int insertPos_;          // position to insert - set after an erase operation

    // no of words in the phrase between startPos and endPos - 1
    int wordCount(int startPos, int endPos) const;

    // remove leading spaces
    // return number of spaces removed
    LgsString::size_type removeLeadingSpaces();
};

// template <class Key>
inline Phrase::Key Phrase::getKey() const
{
    return key_;
}

// template <class Key>
inline bool Phrase::isDirty() const
{
    return dirty_;
}

// template <class Key>
inline bool Phrase::isBlank() const
{
    /*
    for (int i = 0; i < text_.length(); i++)
        if (text_[i] != separator_)
            return false;
    return true;
    */
    return text_.length() == 0;
}

// template <class Key>
inline int Phrase::length() const
{
    return text_.length();
}

// template <class Key>
inline LgsString Phrase::getFullText()
{
    dirty_ = false;
    return text_;
}

#endif



