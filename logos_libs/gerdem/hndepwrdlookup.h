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
#ifndef _HNDEPWRDLOOKUP_H_
#define _HNDEPWRDLOOKUP_H_

#include <logos_libs/linguistic/ldictionaryentry.h>
#include <logos_libs/gerdem/dictionaryentrybuilder.h>
#include <logos_libs/startrules/variable.h>
#include <logos_libs/regex/regularexpression.h>
#include <logos_libs/gerdem/wordgrouplookup.h>


class HNounDepWrdLookup : public WordGroupLookup
{

public:
   // Type Definition
	enum PatternType
	{
		HNounBeginPattern,
		HNounEndPattern
	};

   // Constructor
	HNounDepWrdLookup(DictionaryEntryBuilder & iEntryBuilder, PatternType iPattType);

   // Virtual Functions (Overrides)
	virtual ~HNounDepWrdLookup(void);
   // lookup will call dictionary().createSentenceUnit(word+iMergeWord)
	virtual void _lookup(bool BOS, bool bAllCapitalWords);
   void SetWordCount(LDictionaryEntry * dictEntry, short dictIndex);

private:
	// example pattern:
	// (und|oder|bis|bzw|noch|prep|statt|wie auch)
	static LgsString _conjPattern;
	static LgsVector(LgsString) _prefixv;
	static bool _initialized;

	short _compoundGroupNo;
	short _wordGroupNo;
	short _hyphenGroupNo;
	short _conjGroupNo;
	short _wordPos;
	short _conjPos;
	short _compoundPos;
	LWordIterator _wordStart;
	LWordIterator _wordEnd;
	LWordIterator _compoundStart;
	LWordIterator _compoundEnd;
	LWordIterator _conjStart;
	LWordIterator _conjEnd;
	short _compNoWords;
	short _noWordsInWord;
	short _conjNoWords;
	DictionaryEntryBuilder & _builder;
	PatternType _pattType;
        static const int MaxEntries;
};

#endif

