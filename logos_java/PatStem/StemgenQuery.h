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
/*******************************************************************
 *
 *    DESCRIPTION:   Contains all information specific to Stem 
 *                   generation process.
 *
 *    AUTHOR:        Vlad Yakimetz
 *
 *    HISTORY:       06/02/98
 *
 *******************************************************************/

#ifndef _STEMGENQUERY_H_ 
#define _STEMGENQUERY_H_

#include "statement.h"

class SqlColumn;
struct StemGeneratorRules;


class StemgenQuery
{
public:
   StemgenQuery()
      : _wordClassCd(0),
        _patNo(0),     
        _auxiliaryCode(0),   
        _sepPrefixPos(0),
        _sepPrefixLen(0),
        _insepPrefixPos(0),
        _insepPrefixLen(0),
        _rules(0),
        _dropEnding(0),
        _replaceRule(0),
        _addPrefix(0),
        _addEnding(0) 
   {}
   StemgenQuery(const LgsString& word_,
                const LgsString& langCd_,
                int wordClassCd_,
                int patNo_,
                char auxiliaryCode_,
                int sepPrefixPos_,
                int sepPrefixLen_,
                int insepPrefixPos_,
                int insepPrefixLen_);
   StemgenQuery(const StemgenQuery&  sq_);
   ~StemgenQuery();

   StemgenQuery& operator=(const StemgenQuery& sq_);
         
   void bind(Statement& st_);
   LgsString& query() { return _qryString; }
   
   const LgsString& word() { return _word; }
   const LgsString& languageCode() { return _langCd; }
   int wordClassCode() { return _wordClassCd; }
   StemGeneratorRules& rules() const { return *_rules; }   
   bool updateRules();
   
private:
   LgsString _qryString;
   void _makeQryString();

   LgsString _word;
   LgsString _langCd;
   int _wordClassCd;
   int _patNo;
   char _auxiliaryCode;
   int _sepPrefixPos;
   int _sepPrefixLen;
   int _insepPrefixPos; 
   int _insepPrefixLen;

   StemGeneratorRules* _rules;
   
   SqlColumn* _dropEnding;
   SqlColumn* _replaceRule;
   SqlColumn* _addPrefix;
   SqlColumn* _addEnding;
   
};     


#endif //_STEMGENQUERY_H_
