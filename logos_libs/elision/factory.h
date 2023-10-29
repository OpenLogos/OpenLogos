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
#ifndef __ElisionFactory_h__
#define __ElisionFactory_h__

//----------------------------------------------------------------------------
// File - Factory.h
//
// Class - EL_Factory - abstract
//
// Description - A factory class to create an elision rule-engine.
//               Will be used as a base class by each language elision factory
//
//----------------------------------------------------------------------------

#include <logos_libs/ruleengine/streamfactory.h>
#include <logos_libs/ruleengine/sequentialrulebase.h>
#include <logos_libs/ruleengine/serialandantecedent.h>
#include <logos_libs/ruleengine/serialconsequent.h>
#include <logos_libs/ruleengine/document.h>
#include <logos_libs/elision/searchantecedent.h>
#include <logos_libs/elision/replaceconsequent.h>
#include <logos_libs/elision/initcapconsequent.h>
#include <logos_libs/elision/lowercaseconsequent.h>
#include <logos_libs/elision/variable.h>

#define EXIT_ELISION throw "Fatal error in elision"

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace Elision{

template <class Engine_>
class EL_Factory: public
    RE_StreamFactory<EL_Variable, Engine_, RE_SequentialRuleBase<EL_Variable> >
{
    DisableCopyAssign(EL_Factory);

    typedef RE_StreamFactory<EL_Variable, Engine_, RE_SequentialRuleBase<EL_Variable> > Parent;

public:
    EL_Factory(const LgsString& language, const LgsString& description, istream* input);
	virtual ~EL_Factory();

protected:
    // finds the section associated with the language - found in square brackets in the stream
    // set atEof_ accordingly
    virtual void initialize();

    // return true if not at eof, and not at the next section
    virtual bool moreData() const;

    // reads a word list from a stream, into a given set of words
    // the list starts with the startList line, ends with with the endList line
    void readWordList(LgsString startList, LgsString endList, LgsSet(LgsString)& wordList);

    // read a group at current position - skip white space and handle errors
    // validate group is in range
    int readGroup();

    // parse the antecedent - return 0 if none
    // the implementation for this class is to parse for a search antecedent
    virtual RE_Antecedent<EL_Variable>* parseAntecedent();

    // parse the consequent - return 0 if none
    virtual RE_Consequent<EL_Variable>* parseConsequent();

    // parse the pre-scan strings
    LgsStringVector* parsePreScanStrings();

private:
    enum { leftSectionMarker = '[', rightSectionMarker = ']' };

    LgsString language_;   // language enclosed in square brackets
    int ruleNo_; // current rule number
    int groups_; // no of groups for current rule
	LgsString ruleid_;

    // get the next rule from the stream
    virtual RE_Rule<EL_Variable>* getNextRule();

    // get the next antecedent - which must be a search antecedent
    EL_SearchAntecedent* parseSearchAntecedent(int antecedents, int ruleNumber);

    // check that a given group is in range - handle errors
    void validateGroup(int group);

    // read pre-process section in the stream
    // this consists of rules for all languages which occur at the start of the rule engine
    void ReadPreprocessSection();

    // search for start of the pre-proces section in the stream
    void FindPreprocessSection();
};

//----------------------------------------------------------------------------
template <class Engine_>
EL_Factory<Engine_>::EL_Factory(
    const LgsString& language, const LgsString& description, istream* input)
    : RE_StreamFactory<EL_Variable, Engine_, RE_SequentialRuleBase<EL_Variable> >(
        description, input)
    , ruleNo_(0)
    , language_(LgsString(1, leftSectionMarker) + language +  LgsString(1, rightSectionMarker))
{
}

template <class Engine_>
EL_Factory<Engine_>::~EL_Factory() {}

template <class Engine_>
void EL_Factory<Engine_>::initialize()
{
    assert(language_.length() != 0);

    ReadPreprocessSection();

    if (language_ == LgsString("[none]"))
        return;

    while (Parent::moreData())
    {
        if (Parent::search(language_))
        {
            Parent::readLine();
            return;
        }
        Parent::readLine();
    }

    Parent::displayError("missing language section in elision file", false);
    EXIT_ELISION;
}

template <class Engine_>
void EL_Factory<Engine_>::FindPreprocessSection()
{
    static LgsString preProcessKey = "[pre-process]";

    // find pre-process section in file
    while (Parent::moreData())
    {
        if (Parent::search(preProcessKey))
        {
            Parent::readLine();
            return;
        }
        Parent::readLine();
    }

    Parent::displayError("missing pre-process section in elision file", false);
    EXIT_ELISION;
}

template <class Engine_>
void EL_Factory<Engine_>::ReadPreprocessSection()
{
    FindPreprocessSection();

    // read rules until start of next section or end of file
    RE_SequentialRuleBase<EL_Variable>* ruleBase = Parent::getRuleBase();
    RE_Rule<EL_Variable>* rule;
    while ((rule = getNextRule()) != 0)
        ruleBase->insertRule(rule);
}

template <class Engine_>
bool EL_Factory<Engine_>::moreData() const
{
    if (!Parent::moreData())
        return false;

    const LgsString& line = Parent::getLine();
    if (line.length() != 0 && line[0] == leftSectionMarker)
        return false;

    return true;
}

template <class Engine_>
void EL_Factory<Engine_>::readWordList(LgsString startList, LgsString endList, LgsSet(LgsString)& wordList)
{
    bool inList = false;
    while (moreData())
    {
        // skip blank lines and comments
        if (Parent::isBlankLine() || Parent::isCommentLine())
        {
            Parent::readLine();
        }
        else if (inList)
        {
            if (Parent::search(endList))
            {
                Parent::readLine();
                // inList = false; - not needed since we are exiting
                break;
            }
            else
            {
                wordList.insert(Parent::getLine());
                Parent::readLine();
            }
        }
        else
        {
            if (Parent::search(startList))
            {
                inList = true;
                Parent::readLine();
            }
            else
            {
                Parent::displayError("missing list of words in elision file");
                EXIT_ELISION;
            }
        }
    }
}

template <class Engine_>
RE_Rule<EL_Variable>* EL_Factory<Engine_>::getNextRule()
{
   static LgsString ruleKey = "rule>";
   static LgsString antecedentKey = "ant";
   static LgsString consequentKey = "con";

   RE_Document* document = 0;
   bool inRule = false;
   EL_SearchAntecedent* searchAntecedent = 0;
   RE_SerialConsequent<EL_Variable>* serialConsequent = 0;
   RE_Consequent<EL_Variable>* childConsequent = 0;
   int antecedents = 0;
   int consequents = 0;
   int ruleNumber = 0;
   LgsStringVector* scanStrings = 0;

   groups_ = 0;

   while (moreData())
   {
      // skip blank lines and comments
      if (Parent::isBlankLine() || Parent::isCommentLine())
      {
         Parent::readLine();
      }

      // process rule parts
      else if (inRule)
      {
         if (searchAntecedent == 0)
         {
            searchAntecedent = parseSearchAntecedent(antecedents - 1, ruleNumber);
            assert(searchAntecedent != 0);
            // no readLine() sinc parseSearchAntecedent has already consumed the line
         }
         else if (consequents > 0)
         {
            // read the next childConsequent - and add to serialConsequent if present
            RE_Consequent<EL_Variable>* simpleConsequent = parseConsequent();
            if (simpleConsequent == 0)
            {
               Parent::displayError("missing consequent in elision file");
               EXIT_ELISION;
            }
            if (serialConsequent != 0)
               serialConsequent->insert(simpleConsequent);
            else
               childConsequent = simpleConsequent;
            consequents--;
            Parent::readLine();
         }
         else if (scanStrings == 0)
         {
            scanStrings = parsePreScanStrings();
            searchAntecedent->setPreScanStrings(scanStrings);
         }
         else
            break;
      }
      // look for the next rule
      else
      {
         if (Parent::search(ruleKey))
         {
            // found a rule line
            ruleid_ = Parent::line_.substr(Parent::position_);
            inRule = true;

            if (!Parent::readRuleNumber(ruleNumber))
            {
               Parent::displayError("rule number expected");
               EXIT_ELISION;
            }

            if (!Parent::readInteger(antecedents) || antecedents <= 0)
            {
               Parent::displayError("antecedent count expected");
               EXIT_ELISION;
            }
            Parent::skipWhiteSpace();

            if (!Parent::search(antecedentKey))
            {
               Parent::displayError("'ant' expected");
               EXIT_ELISION;
            }
            Parent::skipWhiteSpace();

            if (!Parent::readInteger(consequents) || consequents <= 0)
            {
               Parent::displayError("consequent count expected");
               EXIT_ELISION;
            }
            Parent::skipWhiteSpace();
            if (consequents > 1)
               childConsequent = serialConsequent = new RE_SerialConsequent<EL_Variable>;

            if (!Parent::search(consequentKey))
            {
               Parent::displayError("'con' expected");
               EXIT_ELISION;
            }
            Parent::skipWhiteSpace();

            // parse for a pattern - the documentation in angled braces
            if (!Parent::parsePattern())
            {
               Parent::displayError("rule documentation expected");
               EXIT_ELISION;
            }

            document = new RE_Document(++ruleNo_, Parent::getPattern());
            Parent::readLine();
         }
         else
         {
            Parent::displayError("'rule>' expected");
            EXIT_ELISION;
         }
      }
   }

   // check we don't have an incomplete rule
   if (consequents != 0)
   {
      Parent::displayError("end of file - incomplete rule", false);
      EXIT_ELISION;
   }

   return searchAntecedent ?
      new RE_Rule<EL_Variable>(searchAntecedent, childConsequent, document, ruleid_) : 0;
}

template <class Engine_>
int EL_Factory<Engine_>::readGroup()
{
    Parent::skipWhiteSpace();

    int group;
    if (!Parent::readInteger(group) || group < 0)
    {
        Parent::displayError("group expected");
        EXIT_ELISION;
    }

    validateGroup(group);
    return group;
}

template <class Engine_>
void EL_Factory<Engine_>::validateGroup(int group)
{
    if (!inrange(group, 0, groups_))
    {
        Parent::displayError("group out of range");
        EXIT_ELISION;
    }
}

template <class Engine_>
EL_SearchAntecedent* EL_Factory<Engine_>::parseSearchAntecedent(int antecedents, int ruleNumber)
{
   assert(antecedents >= 0);

   static LgsString searchKey = "search ";
   static LgsString noBorderKey = "no_border ";
   static LgsString leftBorderKey = "left_border ";
   static LgsString rightBorderKey = "right_border ";

   EL_SearchAntecedent* searchAntecedent = 0;
   RE_SerialAndAntecedent<EL_Variable>* serialAntecedent = (antecedents > 1) ?
     new RE_SerialAndAntecedent<EL_Variable> : 0;
   RE_Antecedent<EL_Variable>* childAntecedent = serialAntecedent;

   while (moreData())
   {
      // skip blank lines and comments
      if (Parent::isBlankLine() || Parent::isCommentLine())
      {
         Parent::readLine();
      }
      else if (searchAntecedent == 0)
      {
         if (!Parent::search(searchKey))
         {
            Parent::displayError("missing search antecedent in elision file");
            EXIT_ELISION;
         }
         Parent::skipWhiteSpace();

         bool leftBorder = false;
         bool rightBorder = false;
         if (Parent::search(noBorderKey))
         {
         }
         else if (Parent::search(leftBorderKey))
         {
            leftBorder = true;
         }
         else if (Parent::search(rightBorderKey))
         {
            rightBorder = true;
         }
         else
         {
            leftBorder = true;
            rightBorder = true;
         }

         RegularExpression::Borders borders;
         if (leftBorder && rightBorder)
            borders = RegularExpression::ElideBoth;
         else if (leftBorder)
            borders = RegularExpression::ElideLeft;
         else if (rightBorder)
            borders = RegularExpression::ElideRight;
         else
            borders = RegularExpression::None;

         // parse for a pattern
         if (Parent::parsePattern())
         {
            searchAntecedent = new EL_SearchAntecedent(Parent::getPattern(), borders, ruleNumber);
            groups_ = searchAntecedent->groups();
         }
         else
         {
            Parent::displayError("<pattern> expected");
            EXIT_ELISION;
         }
         Parent::readLine();
      }
      else if (antecedents > 0)
      {
         // read the next antecedent - and add to serialAntecedent if present
         RE_Antecedent<EL_Variable>* simpleAntecedent = parseAntecedent();
         if (simpleAntecedent == 0)
         {
            Parent::displayError("missing antecedent in elision file");
            EXIT_ELISION;
         }
         if (serialAntecedent != 0)
            serialAntecedent->insert(simpleAntecedent);
         else
            childAntecedent = simpleAntecedent;
         antecedents--;
         Parent::readLine();
      }
      else
         break;
   }

   if (antecedents != 0)
   {
      Parent::displayError("end of file - incomplete rule", false);
      EXIT_ELISION;
   }

   searchAntecedent->setAntecedent(childAntecedent);
   return searchAntecedent;
}

template <class Engine_>
LgsStringVector* EL_Factory<Engine_>::parsePreScanStrings()
{
   static LgsString preScanKey = "prescan>";

   LgsStringVector* scanStrings = 0;
   int numScanStrings = 0;

   while (moreData())
   {
      // skip blank lines and comments
      if (Parent::isBlankLine() || Parent::isCommentLine())
      {
         Parent::readLine();
      }
      else if (scanStrings == 0)
      {
         scanStrings = new LgsStringVector;
         if (Parent::search(preScanKey))
         {
            if (!Parent::readRuleNumber(numScanStrings))
            {
               Parent::displayError("number of pre-scan strings expected");
               EXIT_ELISION;
            }
            Parent::readLine();
         }
      }
      else if (numScanStrings > 0)
      {
         LgsString scanString;

         if (!Parent::parseNextPattern(scanString))
         {
            EXIT_ELISION;
         }
         scanStrings->push_back(scanString);
         numScanStrings--;
         Parent::readLine();
      }
      else
         break;
   }
   return scanStrings;
}

template <class Engine_>
RE_Antecedent<EL_Variable>* EL_Factory<Engine_>::parseAntecedent()
{
    return 0;
}

template <class Engine_>
RE_Consequent<EL_Variable>* EL_Factory<Engine_>::parseConsequent()
{
    static LgsString replaceKey = "replace group ";
    static LgsString with = "with ";
    static LgsString initCapKey = "init_cap ";
    static LgsString lowerCaseKey = "lower_case ";

    RE_Consequent<EL_Variable>* consequent = 0;

    while (moreData())
    {
        // skip blank lines and comments
        if (Parent::isBlankLine() || Parent::isCommentLine())
        {
            Parent::readLine();
        }
        else if (Parent::search(replaceKey))
        {
            Parent::skipWhiteSpace();

            int group = readGroup();
            Parent::skipWhiteSpace();

            if (!Parent::search(with))
            {
                Parent::displayError("'with' expected");
                EXIT_ELISION;
            }
            Parent::skipWhiteSpace();

            // parse for a pattern
            if (Parent::parsePattern(false))
                consequent = new EL_ReplaceConsequent(group, Parent::getPattern());
            else
            {
                Parent::displayError("<pattern> expected");
                EXIT_ELISION;
            }
            break;
        }
        else if (Parent::search(initCapKey))
        {
            consequent = new EL_InitCapConsequent(readGroup());
            break;
        }
        else if (Parent::search(lowerCaseKey))
        {
            consequent = new EL_LowerCaseConsequent(readGroup());
            break;
        }
        else
            break;
    }

    return consequent;
}

//}

#endif



