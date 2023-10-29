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
// --------------------------------------------------------------------------
// File - SourceSentenceUnit.cpp (implementation)
// --------------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/sourcesentenceunit.h>
#include <logos_libs/linguistic/ldictionaryentry.h>
#include <logos_libs/linguistic/swork.h>
#include <logos_libs/utility/henum.h>
#include <logos_libs/SubjectMatterCodes/SubjectMatterCode.h>
#include <logos_libs/translutility/translcommonobjects.h>

// --------------------------------------------------------------------------
// Constructor.
// This is a default constructor required for STL vectors and should never be used.
// --------------------------------------------------------------------------
SourceSentenceUnit::SourceSentenceUnit()
                   :p_henum(0),
                    v_primarySsuPosition(0),
                    p_dictionaryEntry(0),
                    isHyphenated_(false),
                    isHyphenatedHyphen_(false),
                    isProtectedWord_(false),
                    isProtected_(false),
                    unitFormCode_(0),
                    precedingSpaces_(0),
                    trailingSpaces_(0),
                    origSentPosition_(0)
{
}


// --------------------------------------------------------------------------
// Constructor
// --------------------------------------------------------------------------
SourceSentenceUnit::SourceSentenceUnit(LDictionary& dictionary)
                   :SentenceUnit(dictionary),
                    p_henum(0),
                    v_primarySsuPosition(0),
                    p_dictionaryEntry(0),
                    isHyphenated_(false),
                    isHyphenatedHyphen_(false),
                    isProtectedWord_(false),
                    isProtected_(false),
                    unitFormCode_(0),
                    precedingSpaces_(0),
                    trailingSpaces_(0),
                    origSentPosition_(0)
{
}


// --------------------------------------------------------------------------
// Constructor
// --------------------------------------------------------------------------
SourceSentenceUnit::SourceSentenceUnit(LDictionary& dictionary, LDictionaryEntry* entry)
                   :SentenceUnit(dictionary),
                    p_henum(0),
                    v_primarySsuPosition(0),
                    p_dictionaryEntry(entry),
                    isHyphenated_(false),
                    isHyphenatedHyphen_(false),
                    isProtectedWord_(false),
                    isProtected_(false),
                    unitFormCode_(0),
                    precedingSpaces_(0),
                    trailingSpaces_(0),
                    origSentPosition_(0)
{
}


// --------------------------------------------------------------------------
// Constructor
// --------------------------------------------------------------------------
SourceSentenceUnit::SourceSentenceUnit(LDictionary& dictionary, const SWork& swork)
                   :SentenceUnit(dictionary),
                    p_henum(0),
                    p_dictionaryEntry(0),
                    isHyphenated_(false),
                    isHyphenatedHyphen_(false),
                    isProtectedWord_(false),
                    isProtected_(false),
                    unitFormCode_(0),
                    precedingSpaces_(swork.precedingSpaces()),
                    trailingSpaces_(0),
                    origSentPosition_(0)
{
	dictionaryEntry(new LDictionaryEntry());
	position(swork.position());
	setSentenceAddress(swork.SourcePosition());

	wordMarkup()->setBold(swork.isBold());
	wordMarkup()->setItalic(swork.isItalic());
	wordMarkup()->setUnderlined(swork.isUnderlined());
	wordMarkup()->setSingleQuoted(swork.isSingleQuoted());
	wordMarkup()->setDoubleQuoted(swork.isDoubleQuoted());
	wordMarkup()->setProtected(swork.IsProtected());

	if (swork.IsFound()==0)
	{
		dictionaryEntry().setToUnfoundWord();
	}

	switch (swork.capitalizationState())
	{
      case 0:
         setCaseState(LowerCase);
         break;
      case 1:
         setCaseState(BeginsUpperCase);
         break;
      case 2:
         setCaseState(AllUpperCase);
         break;
      default:
         setCaseState(LowerCase);
         break;
	}

	setEndOfSentence(swork.isEndOfSentence ());

	// get all semanto-syntactic units (up to 3)
	int ssuCount = swork.SsuCount();
	for (int i = 0; i < ssuCount; i++)
	{
		LSemantoSyntacticUnit ssu;
		ssu.setBlackHoleLocation(swork.BlackHoleLocation());
		ssu.setHashCode1(swork.HashCode1(i));
		ssu.setHashCode2(swork.HashCode2(i));
		ssu.setRootHashCode1(swork.rootHashCode1(i));
		ssu.setRootHashCode2(swork.rootHashCode2(i));
		ssu.setHashLocation(swork.HashLocation());
		ssu.setHeadWord(swork.HeadWordLocation());
		ssu.setProtectionCode(swork.ProtectionCode(i));
		ssu.setWordCount(swork.WordCount());
		ssu.setWordID(swork.WordID(i));
		ssu.setWordTypeCode(swork.WordTypeCode(i));
		ssu.setCompanyCode(swork.CompanyCode(i));
      if (swork.CanonicalWord(i) == "")
      {
         ssu.setWord(swork.Word());
         ssu.setTargetWord(swork.Word());
      }
      else
      {
         ssu.setWord(swork.CanonicalWord(i));
         ssu.setTargetWord(swork.TargetWord(i));
      }
		ssu.setPatNumber(swork.PatNumber (i));
		ssu.setSourceStemNumber(swork.SourceStemNumber(i));
		ssu.setUsageID(swork.UsageID(i));
		ssu.setWordClassCode(swork.WordClass(i));
		ssu.setGenderCode(swork.Gender(i));
		ssu.setMeaningID(swork.MeaningID(i));
		ssu.setSetID(swork.SetID(i));
		ssu.setSuperSetID(swork.SupersetID(i));
		ssu.setSubSetID(swork.SubsetID(i));
		ssu.setFormCode(swork.FormCode(i));
		ssu.setOverflow2b(swork.Overflow2b(i));
		ssu.setOverflow3b(swork.Overflow3b(i));
//		ssu.setGenericCode(swork.GenericCode(i));		// TO BE REMOVED WHEN NEW SMC IS TESTED
//		ssu.setAtomicCode(swork.AtomicCode(i));			// TO BE REMOVED WHEN NEW SMC IS TESTED
		ssu.setSubjectMatterCode(swork.subjectMatterCode(i));
		ssu.setMatchedOnFullyCapped(swork.MatchedOnFullyCapped(i));

		dictionaryEntry().addSsu(ssu);
	}

	setPrimarySsuPosition(swork.PrimarySsu());
	dictionaryEntry().compoundInfo(swork.CompoundInfo());
}


// --------------------------------------------------------------------------
// Set this object from the given source sentence unit
// --------------------------------------------------------------------------
SourceSentenceUnit::SourceSentenceUnit(const SourceSentenceUnit& rhs)
                   :SentenceUnit(rhs),
                    p_dictionaryEntry(rhs.p_dictionaryEntry),
                    p_henum(0),
                    v_primarySsuPosition(0),
                    isHyphenated_(rhs.isHyphenated_),
                    isHyphenatedHyphen_(rhs.isHyphenatedHyphen_),
                    isProtectedWord_(rhs.isProtectedWord_),
                    isProtected_(rhs.isProtected_),
                    unitFormCode_(rhs.unitFormCode_),
                    precedingSpaces_(rhs.precedingSpaces_),
                    trailingSpaces_(rhs.trailingSpaces_),
                    origSentPosition_(rhs.origSentPosition_)
{
}


// --------------------------------------------------------------------------
void SourceSentenceUnit::reconcilePrecedingSpaces()
{
   precedingSpaces_ = surfaceExpression().precedingSpaces();
}


// --------------------------------------------------------------------------
// Destructor
// --------------------------------------------------------------------------
SourceSentenceUnit::~SourceSentenceUnit()
{
   if (p_henum)
   {
      delete p_henum;
   }
}


// --------------------------------------------------------------------------
void SourceSentenceUnit::deleteEntry()
{
   // Performs memory cleanup. However, care must be taken not to
   // delete objects that are cached.
   if (p_dictionaryEntry)
   {
      if (!(dictionaryEntry().isCached()))
      {
         p_dictionaryEntry->deleteSsuComponents();
         delete p_dictionaryEntry;
      }
   }
}


// --------------------------------------------------------------------------
void SourceSentenceUnit::getMarkupFromWords()
{
   LWordVector& words = surfaceWords();

   if (words.size())
   {
      wordMarkup()->setId (words.begin()->markupId());
      for (LWordIterator i = words.begin(); i != words.end(); i++)
      {
         wordMarkup()->orMask (i->markup());
      }
   }
}


// --------------------------------------------------------------------------
int SourceSentenceUnit::henum1() const
{
	if (isUnfoundWord())
	{
		return -1;
	}
	if (primarySsu().useHashForHenum() &&
		p_dictionaryEntry->wordsUsedInMatch() == 1)
	{
		return primarySsu().hashCode1();
	} else if (!p_henum)
	{
		const_cast<SourceSentenceUnit*>(this)->p_henum = new Henum(word());
	}
	return p_henum->first();
}


// --------------------------------------------------------------------------
int SourceSentenceUnit::henum2() const
{
	if (isUnfoundWord())
	{
		return -1;
	}
	if (primarySsu().useHashForHenum() && 
		p_dictionaryEntry->wordsUsedInMatch() == 1)
	{
		return primarySsu().hashCode2();
	} 
	else if (!p_henum)
	{
		const_cast<SourceSentenceUnit*>(this)->p_henum = new Henum(word());
	}
	return p_henum->second();
}


// --------------------------------------------------------------------------
bool SourceSentenceUnit::isTranslateable() const
{
   if (isProtectedFromTranslation())
   {
      return false;
   }
   if (isEndOfSentence())
   {
      return false;
   }
   if (isUnfoundWord())
   {
      return false;
   }
   return true;
}


// --------------------------------------------------------------------------
void SourceSentenceUnit::persistOut(ostream& stream)
{
   SentenceUnit::persistOut(stream);
   stream << isHyphenatedHyphen_ << endl;
}


// --------------------------------------------------------------------------
void SourceSentenceUnit::persistIn(istream& stream)
{
   SentenceUnit::persistIn(stream);
   stream >> isHyphenatedHyphen_;
}


// --------------------------------------------------------------------------
int SourceSentenceUnit::primarySsuPosition() const
{
   return v_primarySsuPosition;
}


// --------------------------------------------------------------------------
const LSemantoSyntacticUnit& SourceSentenceUnit::ssuAt(int n) const
{
   return dictionaryEntry().ssuAt(n);
}


// --------------------------------------------------------------------------
int SourceSentenceUnit::translatedWordCount() const
{
   return wordCount();
}


// --------------------------------------------------------------------------
const LgsString& SourceSentenceUnit::word() const
{
   return primarySsu().word();
}


// --------------------------------------------------------------------------
int SourceSentenceUnit::wordCount() const
{
   return primarySsu().wordCount();
}


// --------------------------------------------------------------------------
int SourceSentenceUnit::wordID() const
{
   return primarySsu().wordID();
}


// --------------------------------------------------------------------------
int SourceSentenceUnit::wordLength() const
{
   return word().length();
}


// --------------------------------------------------------------------------
SourceUnitVector::~SourceUnitVector()
{
}


// --------------------------------------------------------------------------
void SourceUnitVector::removeAndDelete(SourceUnitIterator start, SourceUnitIterator end)
{
   for (iterator i = start; i != end; i++)
   {
      SourceSentenceUnit* p = *i;
      delete p;
   }
   erase(start, end);
}


// --------------------------------------------------------------------------
void SourceUnitVector::removeAndDeleteAll()
{
   removeAndDelete(begin(), end());
}


// --------------------------------------------------------------------------
// Return the SSU at the given position in the sentence (ie, the i-th element
// in the vector of SourceSentenceUnit objects).
// Return the SSU matched or NULL if no match.
// --------------------------------------------------------------------------
SourceSentenceUnit* SourceUnitVector::at(int positionInSentence) 
{
	if (positionInSentence>0) 
	{
		SourceUnitIterator i;
		int n=1;
		for (i=begin(); i!=end(); i++) 
		{
			if (n==positionInSentence) return (*i);
			n++;
		}
	}
	return NULL;		// no match
}


// --------------------------------------------------------------------------
// Display on console content of this vector (for testing)
// --------------------------------------------------------------------------
void SourceUnitVector::display(LgsString msg) 
{
	cout << endl << "*** SOURCE SENTENCE UNITS ***" << endl;
	SourceUnitIterator ssu;
	int n=0;
	for (ssu=begin();ssu!=end();ssu++) 
	{
		cout << msg << ++n;
		(*ssu)->display();
	}
}
// --------------------------------------------------------------------------
void SourceSentenceUnit::display()
{
	//cout << "\tsuperset=" << superSetID();
	//cout << "\tset=" << setID();
	//cout << "\tsubset=" << subSetID();
	//cout << "\tformcode=" << formCode();
	//cout << "\tisProperName: " << isProperName();
	//cout << "\tkeepSourceExpression: " << keepSourceExpression();
	cout << "\t" << surfaceExpressionAsString();
	cout << endl;
}


// -------------------------------------------------------------------
// Reconstruct the surface form of the source sentence based on the source units.
// For the case of german compound, each decomposed unit contains the whole compound
// in surface form in method surfaceExpressionAsString(). Therefore, only the one of
// the head needs to be displayed.
// -------------------------------------------------------------------
LgsString SourceUnitVector::generateSentenceSurfaceForm() 
{
	LgsString wholeSentence = "";
	SourceUnitIterator ssu;
   
   bool bSingleQuoted = false;
   bool bDoubleQuoted = false;
   bool bFrenchSingleQuoted = false;
   bool bFrenchDoubleQuoted = false;
   bool bFrenchTarget = false;
   
   LgsString SingleQuote("'");
   LgsString DoubleQuote("\"");
   LgsString FrenchSingleQuote("'");
   LgsString FrenchDoubleQuote("\"");
   LgsString Hyphen("-");
	LgsString Period(".");
   bool bHyphen=false;
   bool bWasHyphen=false;
   bool bCompoundWithoutHead = false;
   bool bFirstElementToOutput = true;
   
   if(TranslCommonObjects::GetTargetLanguage()->id()==LLanguage::FrenchID)
   {
	   bFrenchTarget = true;
   }

	for (ssu=begin();ssu!=end();++ssu) 
	{
		
        if ( bCompoundWithoutHead && (*ssu)->compoundUnit() && !(*ssu)->headUnit())
		{
            continue;
        }
        bCompoundWithoutHead = false;

        LgsString sourceWord = (*ssu)->surfaceExpressionAsString();
		if (sourceWord!="bos") 
		{
			int nSpace=(*ssu)->precedingSpaces();
			if( (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::EnglishID) ||
             (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::GermanID) ) // do it for both languages (inc#3226)
			{
				if(bHyphen && nSpace >0)
				{
					nSpace--;  //Adjust the preceding space previous word ends with an hyphen
				}
			}

			bWasHyphen = bHyphen; // in case we have German compound non-head unit
			bHyphen = false;
			LgsString preceedingSpaces=LgsString(nSpace,' ');

			 //--------------------------------------------------------------
			 // Get the markup information. Determine weather word is quoted 
			 //--------------------------------------------------------------
			 LWordMarkup* pWordMarkup = (*ssu)->wordMarkup();

			 if (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::EnglishID)
			 {
				 if(( (*ssu)->isHyphenated() ) ||( (*ssu)->formCode() == 6 ) )//add a hyphen.
				 {
					 sourceWord += Hyphen;
					 bHyphen = true;
				 }
			 }
          // check if have to add a period or a hyphen (inc#3226,3275)
          if (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::GermanID)
          {
				 if( ((*ssu)->formCode() == 9) && ((*ssu)->superSetID() == 13) )
				 {
               int iSubSet = (*ssu)->subSetID();
               if( iSubSet == 455 )
               {
	               sourceWord += Period; // add a period right now
               }
               else if( (iSubSet == 123) || (iSubSet == 222) ||
			               (iSubSet == 234) || (iSubSet == 567) || (iSubSet == 789) )
               {
	               sourceWord += Hyphen; // add a hyphen right now
	               bHyphen = true; // and remove a space later
               }
				 }
          }
             
                
			 if( (!bSingleQuoted) && pWordMarkup->isSingleQuoted() ) //add a opening  single quote
			 {
				sourceWord= SingleQuote + sourceWord;
				bSingleQuoted = true;
			 }

			 if(bSingleQuoted && (!pWordMarkup->isSingleQuoted()) ) //add a closing  single quote
			 {
				wholeSentence+= SingleQuote;
				bSingleQuoted = false;
			 }
         
			 if( (!bDoubleQuoted) && pWordMarkup->isDoubleQuoted() ) //add a opening  Double quote
			 {
				sourceWord= DoubleQuote + sourceWord;
				bDoubleQuoted = true;
			 }

			 if(bDoubleQuoted && (!pWordMarkup->isDoubleQuoted()) ) //add a closing  Double quote
			 {
				wholeSentence+= DoubleQuote;
				bDoubleQuoted = false;
			 }
         
			 if(bFrenchTarget)
			  {
				 if( (!bFrenchSingleQuoted) && pWordMarkup->isFrenchSingleQuoted() ) //add a opening  single quote
				{
				   sourceWord= FrenchSingleQuote + sourceWord;
				   bFrenchSingleQuoted = true;
				}

				if(bFrenchSingleQuoted && (!pWordMarkup->isFrenchSingleQuoted()) ) //add a closing  single quote
				{
				   wholeSentence+= FrenchSingleQuote;
				   bFrenchSingleQuoted = false;
				}
         
				if( (!bFrenchDoubleQuoted) && pWordMarkup->isFrenchDoubleQuoted() ) //add a opening  Double quote
				{
				   sourceWord= FrenchDoubleQuote + sourceWord;
				   bFrenchDoubleQuoted = true;
				}

				if(bFrenchDoubleQuoted && (!pWordMarkup->isFrenchDoubleQuoted()) ) //add a closing  Double quote
				{
				   wholeSentence+= FrenchDoubleQuote;
				   bFrenchDoubleQuoted = false;
				}

			 }

			if (   TranslCommonObjects::GetSourceLanguage()->id()==LLanguage::GermanID 
				&& (*ssu)->compoundUnit()
				&& !(*ssu)->headUnit())
			{
				int nSize = sourceWord.size();
				if( ( nSize >0) && (sourceWord[nSize-1] == '-') )
				{
					bCompoundWithoutHead = true;
					if(bFirstElementToOutput )
					{   //Supress the leading space for the  first element to be outputed
						wholeSentence +=  sourceWord;
						bFirstElementToOutput = false;
					}
					else wholeSentence +=  preceedingSpaces + sourceWord;
				}
				else
				{
					// German compound non-head unit is not outputed => restore bHyphen
					if( bWasHyphen )
					{
						bHyphen = true;
					}
				}
			}
			else
			{
				if(bFirstElementToOutput )
                {   //Supress the leading space for the  first element to be outputed
                      wholeSentence +=  sourceWord;
                      bFirstElementToOutput = false;
                }
                else wholeSentence +=  preceedingSpaces + sourceWord;
             }
      } //End  if (sourceWord!="bos")  block.
	}//End  for (ssu=begin();ssu!=end();++ssu)   block.

   //----------------------------------------------------------------
   //We have reached the end of sentence. Check the Quoted flags.
   // Add the closing quote if required.
   //----------------------------------------------------------------
   if(bSingleQuoted)  //add a closing  single quote
   {
	   wholeSentence+= SingleQuote;
   }

   if(bDoubleQuoted) //add a closing  Double quote
   {
	   wholeSentence+= DoubleQuote;
   }

   if(bFrenchSingleQuoted)  //add a closing  single quote
   {
	   wholeSentence+= FrenchSingleQuote;
   }

   if(bFrenchDoubleQuoted) //add a closing  Double quote
   {
	   wholeSentence+= FrenchDoubleQuote;
   }

	return wholeSentence;
}


// --------------------------------------------------------------------------
void SourceSentenceUnit::setWordClassCode(int x)
{
	LSemantoSyntacticUnit& ssu = const_cast<LSemantoSyntacticUnit&>(primarySsu());
	ssu.setWordClassCode(x);
}


// --------------------------------------------------------------------------
void SourceSentenceUnit::setSuperSetID(int x) 
{
	LSemantoSyntacticUnit& ssu = const_cast<LSemantoSyntacticUnit&>(primarySsu());
	ssu.setSuperSetID(x);
}


// --------------------------------------------------------------------------
void SourceSentenceUnit::setSetID(int x) 
{
	LSemantoSyntacticUnit& ssu = const_cast<LSemantoSyntacticUnit&>(primarySsu());
	ssu.setSetID(x);
}


// --------------------------------------------------------------------------
void SourceSentenceUnit::setSubSetID(int x) 
{
	LSemantoSyntacticUnit& ssu = const_cast<LSemantoSyntacticUnit&>(primarySsu());
	ssu.setSubSetID(x);
}


// --------------------------------------------------------------------------
void SourceSentenceUnit::setFormCode(int x) 
{
	LSemantoSyntacticUnit& ssu = const_cast<LSemantoSyntacticUnit&>(primarySsu());
	ssu.setFormCode(x);
}


// --------------------------------------------------------------------------
bool SourceSentenceUnit::isUnfoundWord() const
{
	return dictionaryEntry().isUnfoundWord();
}


// --------------------------------------------------------------------------
// Set each word that compose this source sentence unit with the information
// whether to keep the source form expression (value is true) or the actual
// transfer (value is false).
// --------------------------------------------------------------------------
void SourceSentenceUnit::setKeepSourceExpression(bool value) 
{
   LWordVector& words = surfaceWords();
   for (LWordIterator word = words.begin(); word != words.end(); word++)
   {
      word->setKeepSourceExpression(value);
   }
}


// --------------------------------------------------------------------------
// To decide whether to use the transfer or the source expression during generate
// for this source sentence unit, access the words that compose this unit and get 
// the value of the flag keepSourceExpression. Since all constituents are set to
// the same value (see SourceSentenceUnit::setKeepSourceExpression), one need
// only to access the first word to decide for the whole source sentence unit.
// --------------------------------------------------------------------------
bool SourceSentenceUnit::keepSourceExpression() 
{
   LWordVector& words = surfaceWords();
   bool keepIt = false;

   if (words.size() > 0)
   {
      LWordIterator word = words.begin();
      if (word->size() > 0) 
      {
         keepIt = word->keepSourceExpression();
      }
   }

   return keepIt;
}


// --------------------------------------------------------------------------
// Set each word that composes this source sentence unit with the information
// about whether to is a proper name.
// --------------------------------------------------------------------------
void SourceSentenceUnit::setIsProperName(bool value) 
{
   LWordVector& words = surfaceWords();
   for (LWordIterator word = words.begin(); word != words.end(); word++)
   {
      word->setIsProperName(value);
   }
}


// --------------------------------------------------------------------------
// If one of the words in the list of surface words is set to true (a proper name),
// then the sentence unit is a proper name. In reality, all the units should be set
// to the same value.
// --------------------------------------------------------------------------
bool SourceSentenceUnit::isProperName() 
{
	LWordVector& words = surfaceWords();
   for (LWordIterator word = words.begin(); word != words.end(); word++)
   {
      if (word->isProperName())
      {
         return true;
      }
   }
   return false;
}
// --------------------------------------------------------------------------
void SourceSentenceUnit::propagateOrigSentPosition() 
{
   LWordVector& words = surfaceWords();
   for (LWordIterator word = words.begin(); word != words.end(); word++)
   {
      word->setOriginalSentencePosition(origSentPosition_);
   }
}
