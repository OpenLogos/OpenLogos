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
program. If not, write to Globalware AG, Hospitalstraße 6, D-99817 Eisenach.

Linux port modifications and additions by Bernd Kiefer, Walter Kasper,
Deutsches Forschungszentrum fuer kuenstliche Intelligenz (DFKI)
Stuhlsatzenhausweg 3, D-66123 Saarbruecken
*/
#ifndef __prissufilter_h__
#define __prissufilter_h__

//-------------------------------------------------------------------
// File - prissufilter.h
//
// Class - PriorityBasedSSUFilter (interface)
// Class - PriorityElement 
//
// Description - This class is responsible to reduce the SSU (
// SemantoSyntacticUnit) list by using the following rules:
//
//    1. Exclusion
//       Some priorities are not allowed to co-exist. The priority
//       to be removed is specified in the logos_system.ini file.
//       An example to show the format in the logos_system.ini file is:
//       [$LANGUAGE$_exclusion] ($LANGUAGE$ can take the following values:
//                               "english", "french", "german", "italian", 
//                               "spanish")
//       1=5,6
//       13=18,19
//       12=6,7
//       Interpretation of the statements are:
//       Statement 1: SSU corresponding to priority 1 will be removed if any 
//       SSU with priority 5 or 6 exists.
//       Statement 2: SSU corresponding to priority 13 will be removed if any
//       SSU with priority 18 or 19 exist
//       Statement 3: SSU corresponding to priority 12 will be removed if any
//       SSU with priority 6 or 7 exists.
//
//    2. Replacement
//       Some higher higher priority valued SSUs can replace lower priority 
//       valued SSUs if number of SSUs > 3.
//       An example is:
//       [$LANGUAGE$_replacement]
//       32=22,23,24,25,26,27,28,29,30
//       33=22,23,24,25,26,27,28,29,30
//       Interpretation of the statements are:
//       Statement 1: SSU with priority 32 can replace SSUs with 
//       priorities 22,23,24,25,26,27,28,29,30 if no of SSUs > 3.
//       Statement 2: similar to statement 1
//-------------------------------------------------------------------
#include <bitset>

#define MaxPriority  128

class SsuList;
class IniParser;

class PriorityElement
{
   int _priorityOrder;
   LgsVector(int) _priorityMap;

public:
   PriorityElement(void);
   PriorityElement(int priority);
   PriorityElement(const PriorityElement & srcPE);
   void addElement(int priority); 
   bool operator <(const PriorityElement & rhs) const;
   bool operator ==(const PriorityElement & rhs) const;
   const PriorityElement & operator=(const PriorityElement & rhs);
   const LgsVector(int) & getPriorityMap(void) const;
};


class PriorityBasedSSUFilter
{
   static bool _initialized;
   static LgsVector(PriorityElement) *_replacementMap;
   static LgsVector(PriorityElement) *_exclusionMap;
   bitset<MaxPriority> _priorityBitSet;
   SsuList *_ssuList;
   static void loadPriorityMap(IniParser & argumentParser, LgsVector(PriorityElement) *exclusionMap);

public:
   static void initialize(void);
   static void cleanup(void);
   PriorityBasedSSUFilter(void);
   void filter(SsuList * ssuList);
   void applyExclusionFilter(void);
   void applyReplacementFilter(void);
};

#endif

