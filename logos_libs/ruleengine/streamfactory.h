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
#ifndef __RuleEngineStreamFactory_h__
#define __RuleEngineStreamFactory_h__

//----------------------------------------------------------------------------
// File - StreamFactory.h
//
// Class - RE_StreamFactory - abstract
//
// Description - A factory class to create a rule-engine from a stream
//
//----------------------------------------------------------------------------

#include <logos_libs/ruleengine/rule.h>
#include <logos_libs/ruleengine/factory.h>
#include <logos_libs/utility/stringutil.h>
#include <logos_libs/regex/charutil.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
//namespace RuleEngine {

template <class RuleVariable_, class Engine_, class RuleBase_>
class RE_StreamFactory: public RE_Factory<RuleVariable_>
{
   DisableCopyAssign(RE_StreamFactory);

public:
   //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   RE_StreamFactory(const LgsString& description, istream* input);

   virtual ~RE_StreamFactory();

   // implementation provided for parent class hook - not to be overridden
   virtual RE_Engine<RuleVariable_>* createRuleEngine();

protected:
   enum
   {
      MaximumBufferSize = 1024,           // buffer size
      PatternSize = 128                   // pattern size - LgsString will grow if bigger
   };

   Engine_* engine_;                        // the engine being built by the factory

   // hook methods - must be overridden
   // return next rule from the stream, 0 if at the end
   virtual RE_Rule<RuleVariable_>* getNextRule() = 0;

   // hook methods - may be overridden, but default implementations may be sufficient
   // hook to advance the stream to the correct place, etc
   // the default implementation does nothing
   virtual void initialize();

   // is there more data in the stream
   // the default implementation is to return true iff not at the end of file, and no error
   virtual bool moreData() const;

   // display error
   // the default implementation is to display to cerr
   virtual void displayError(const char* error, bool displayRule = true) const;

   // is the current line a blank line - skips leading white space as a side effect
   virtual bool isBlankLine();

   // is the current line a comment line - skips leading white space as a side effect
   // the default implementation is to return true if the first non-whitespace char is a semicolon
   virtual bool isCommentLine();

   // parse the input for a pattern - set pattern_
   // return true if no syntax error while parsing
   // default implementation:
   //   a pattern is enclosed in angled brackets as in "<abcd>"
   //   the pattern can span multiple lines with the following restrictions:
   //       the first line must include at least the '<' character
   //       each continuation line must end in a continuation '\' character
   //   the first '>' will terminate the pattern. If a '>' appears in the pattern,
   //       it must be doubled eg <abcd>>efg> represents the pattern "abcd>efg"
   //       a doubled '>' character cannot be split over two lines
   virtual bool parsePattern(bool checkForBlank = true, bool optional = false);

   virtual bool parseNextPattern(LgsString& pattern);

   // utility methods to be used by child classes
   // read the next line into the buffer
   void readLine();

   // returns a copy of the pattern that was parsed
   LgsString getPattern() const;

   // search for the key from the current position and bump the position if found
   bool search(const LgsString& keyword);

   // parse an integer from the current position and bump the position if found
   // currently only read integers with values from 0 to 9
   bool readInteger(int& value);
   bool readRuleNumber(int& value);

   // skip white space
   void skipWhiteSpace();

   // check that the current position is at the end of line, allowing for trailing spaces
   // return true if at end of line
   bool checkEOL();

   // return the last line read - from the current position to the end of the line
   LgsString getLine() const;

   // return the input stream
   const istream& getInput() const;

   RuleBase_* getRuleBase();

protected:
   LgsString line_;                       // last line read from the stream
   LgsString::size_type position_;        // current position in the buffer

private:
   istream* input_;                    // input stream
   LgsString description_;                // description of rule factory - for error reporting
   char buffer_[MaximumBufferSize];    // buffer to hold the line
   RuleBase_* ruleBase_;               // rule-base for the engine to be created
   bool atEof_;                        // at end of file
   LgsString pattern_;                    // pattern in the buffer
};

//----------------------------------------------------------------------------
template <class RuleVariable_, class Engine_, class RuleBase_>
RE_StreamFactory<RuleVariable_, Engine_, RuleBase_>::RE_StreamFactory(const LgsString& description,
                                                                      istream* input)
                                                    :atEof_(false),
                                                     description_(description),
                                                     input_(input)
{
   assert(input_ != 0);
   readLine();
   pattern_.reserve(PatternSize);
}
template <class RuleVariable_, class Engine_, class RuleBase_>
RE_StreamFactory<RuleVariable_, Engine_, RuleBase_>::~RE_StreamFactory() {
}
//----------------------------------------------------------------------------
template <class RuleVariable_, class Engine_, class RuleBase_>
RE_Engine<RuleVariable_>* RE_StreamFactory<RuleVariable_, Engine_, RuleBase_>::createRuleEngine()
{
   ruleBase_ = new RuleBase_;
   engine_ = new Engine_(ruleBase_);

   RE_Rule<RuleVariable_>* rule;
   initialize();
   while ((rule = getNextRule()) != 0)
      ruleBase_->insertRule(rule);

   return engine_;
}
//----------------------------------------------------------------------------
template <class RuleVariable_, class Engine_, class RuleBase_>
void RE_StreamFactory<RuleVariable_, Engine_, RuleBase_>::initialize()
{
}
//----------------------------------------------------------------------------
template <class RuleVariable_, class Engine_, class RuleBase_>
bool RE_StreamFactory<RuleVariable_, Engine_, RuleBase_>::moreData() const
{
   return !atEof_;
}
//----------------------------------------------------------------------------
template <class RuleVariable_, class Engine_, class RuleBase_>
void RE_StreamFactory<RuleVariable_, Engine_, RuleBase_>::displayError(const char* error,
                                                                       bool displayRule) const
{
   cerr << "Error in " << description_ << " rule: " << error << '.' << endl;
   if (displayRule)
   {
      cerr << "\tRule is: " << line_ << endl;
      cerr << "\t         ";
      for (int i = 0; i < position_; i++)
         cerr << ' ';
      cerr << '^' << endl;
   }
}
//----------------------------------------------------------------------------
template <class RuleVariable_, class Engine_, class RuleBase_>
bool RE_StreamFactory<RuleVariable_, Engine_, RuleBase_>::isBlankLine()
{
   skipWhiteSpace();
   return position_ == line_.length();
}
//----------------------------------------------------------------------------
template <class RuleVariable_, class Engine_, class RuleBase_>
bool RE_StreamFactory<RuleVariable_, Engine_, RuleBase_>::isCommentLine()
{
   static LgsString comment = ";";
   return search(comment);
}
//----------------------------------------------------------------------------
template <class RuleVariable_, class Engine_, class RuleBase_>
bool RE_StreamFactory<RuleVariable_, Engine_, RuleBase_>::parsePattern(bool checkForBlank, bool optional)
{
   static LgsString left_s = "<";
   static char right_s = '>';
   static char bs_s = '\\';

   // clear the pattern
   pattern_.erase();

   if (!search(left_s))
   {
      if (!optional)
         displayError("'<' expected");
      return false;
   }

   for (;;)
   {
      // check for end of line
      if (position_ == line_.length())
      {
         // should have a continuation line - ending in a backslash
         if ((line_[position_ - 1] == bs_s)
             || ((line_[position_ - 1] == '\r')
                 && (line_[position_ - 2] == bs_s)))
         {
            readLine();                                            // read next line
            if (!atEof_)
               continue;

            displayError("end of file: expecting '>'");
            return false;
         }
         else
         {
            displayError("'>' not found");
            return false;
         }
      }

      // read next character
      char ch = line_[position_++];

      // add to pattern if we are not at the end of a continuation line, and not a closing '>'
      if ((ch != bs_s || position_ != line_.length()) && ch != right_s)
      {
         pattern_ += ch;
      }

      // check for closing angle bracket
      if (ch == right_s)
      {
         // check for doubled angle-bracket character - this is an escape sequence
         if (position_ < line_.length() && line_[position_] == right_s)
         {
            // add '>' to pattern and skip over 2nd angle-bracket character
            pattern_ += ch;
            position_++;
         }
         // other cases - this character closes the pattern
         else if (checkForBlank && pattern_.length() == 0)
         {
            displayError("empty pattern");
            return false;
         }
         else
         {
            return true;
         }
      }
   }
}
//----------------------------------------------------------------------------
template <class RuleVariable_, class Engine_, class RuleBase_>
bool RE_StreamFactory<RuleVariable_, Engine_, RuleBase_>::parseNextPattern(LgsString& pattern)
{
   static LgsString left_s = "<";
   static char right_s = '>';

   // clear the pattern
   pattern.erase();

   if (!search(left_s))
   {
      displayError("'<' expected");
      return false;
   }

   for (;;)
   {
      // check for end of line
      if (position_ == line_.length())
      {
         displayError("'>' not found");
         return false;
      }

      // read next character
      char ch = line_[position_++];

      // add to pattern if we are not at the end of a continuation line, and not a closing '>'
      if ((position_ != line_.length()) && ch != right_s)
      {
         pattern += ch;
      }

      // check for closing angle bracket
      if (ch == right_s)
      {
         // check for doubled angle-bracket character - this is an escape sequence
         if (position_ < line_.length() && line_[position_] == right_s)
         {
            // add '>' to pattern and skip over 2nd angle-bracket character
            pattern += ch;
            position_++;
         }
         // other cases - this character closes the pattern
         else if (pattern.length() == 0)
         {
            displayError("empty pattern");
            return false;
         }
         else
         {
            return true;
         }
      }
   }
}

//----------------------------------------------------------------------------
template <class RuleVariable_, class Engine_, class RuleBase_>
void RE_StreamFactory<RuleVariable_, Engine_, RuleBase_>::readLine()
{
   assert(input_ != 0);

   position_ = 0;
   if (input_->bad() || input_->eof())
   {
      atEof_ = true;
      *buffer_ = 0;
   }
   else
   {
      input_->getline(buffer_, MaximumBufferSize);
   }
   line_ = buffer_;
   skipWhiteSpace();
}
//----------------------------------------------------------------------------
template <class RuleVariable_, class Engine_, class RuleBase_>
LgsString RE_StreamFactory<RuleVariable_, Engine_, RuleBase_>::getPattern() const
{
    return pattern_;
}
//----------------------------------------------------------------------------
template <class RuleVariable_, class Engine_, class RuleBase_>
bool RE_StreamFactory<RuleVariable_, Engine_, RuleBase_>::search(const LgsString& keyword)
{
   LgsString::size_type foundPos = StringUtil::findIgnoreCase(line_, keyword, position_);
   if (foundPos != position_)
      return false;
   position_ += keyword.length();
   return true;
}
//----------------------------------------------------------------------------
template <class RuleVariable_, class Engine_, class RuleBase_>
bool RE_StreamFactory<RuleVariable_, Engine_, RuleBase_>::readInteger(int& value)
{
   if (isBlankLine())
      return false;
   value = 0;
   char next = line_[position_];
   bool found = false;
   while (CharUtil::isNumeric(next))
   {
      found = true;
      position_++;
      value = (value * 10) + int(next - '0');
      next = line_[position_];
   }
   return found;
}
//----------------------------------------------------------------------------
template <class RuleVariable_, class Engine_, class RuleBase_>
bool RE_StreamFactory<RuleVariable_, Engine_, RuleBase_>::readRuleNumber(int& value)
{
   if (isBlankLine())
      return false;
   skipWhiteSpace();
   char next = line_[position_++];
   if (next != '<')
      return false;
   if (!readInteger(value))
      return false;
   next = line_[position_++];
   if (next != '>')
      return false;
   return true;
}
//----------------------------------------------------------------------------
template <class RuleVariable_, class Engine_, class RuleBase_>
void RE_StreamFactory<RuleVariable_, Engine_, RuleBase_>::skipWhiteSpace()
{
   static LgsString whiteSpace = " \t\r";

   while (line_.find_first_of(whiteSpace, position_) == position_)
      position_++;
}
//----------------------------------------------------------------------------
template <class RuleVariable_, class Engine_, class RuleBase_>
bool RE_StreamFactory<RuleVariable_, Engine_, RuleBase_>::checkEOL()
{
   skipWhiteSpace();
   if (position_ != line_.length())
   {
      displayError("End of line expected");
      return false;
   }

   return true;
}
//----------------------------------------------------------------------------
template <class RuleVariable_, class Engine_, class RuleBase_>
LgsString RE_StreamFactory<RuleVariable_, Engine_, RuleBase_>::getLine() const
{
   return LgsString(line_.begin() + position_, line_.end());
}
//----------------------------------------------------------------------------
template <class RuleVariable_, class Engine_, class RuleBase_>
const istream& RE_StreamFactory<RuleVariable_, Engine_, RuleBase_>::getInput() const
{
   assert(input_ != 0);
   return *input_;
}
//----------------------------------------------------------------------------
template <class RuleVariable_, class Engine_, class RuleBase_>
RuleBase_* RE_StreamFactory<RuleVariable_, Engine_, RuleBase_>::getRuleBase()
{
   return ruleBase_;
}

//}

#endif



