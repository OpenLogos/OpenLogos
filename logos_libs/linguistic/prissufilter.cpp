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
// File - prissufilter.cpp
//
// Class - PriorityBasedSSUFilter (implementation)
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/prissufilter.h>
#include <logos_libs/linguistic/ldictionaryentry.h>
#include <logos_libs/utility/iniparser.h>
#include <configdatafileinterface/configdatainterfacemain.h>

bool PriorityBasedSSUFilter::_initialized = false;
LgsVector(PriorityElement) *PriorityBasedSSUFilter::_exclusionMap = 0;
LgsVector(PriorityElement) *PriorityBasedSSUFilter::_replacementMap = 0;


PriorityElement::PriorityElement(void)
{
}

PriorityElement::PriorityElement(int priority)
                :_priorityOrder(priority)
{
}

PriorityElement::PriorityElement(const PriorityElement & srcPE)
                :_priorityOrder(srcPE._priorityOrder)
{
   const LgsVector(int) & tempPM = srcPE.getPriorityMap();
   for (LgsVector(int)::const_iterator i = tempPM.begin(); i != tempPM.end(); i++)
   {
      _priorityMap.push_back(*i);
   }
}

void PriorityElement::addElement(int priority)
{
   _priorityMap.push_back(priority);   
}

bool PriorityElement::operator <(const PriorityElement & rhs) const
{
   return (_priorityOrder < rhs._priorityOrder);
}

bool PriorityElement::operator ==(const PriorityElement & rhs) const
{
   return (_priorityOrder == rhs._priorityOrder);

}

const PriorityElement & PriorityElement::operator=(const PriorityElement & srcPE)
{
   _priorityOrder = srcPE._priorityOrder;
   const LgsVector(int) & tempPM = srcPE.getPriorityMap();
   for (LgsVector(int)::const_iterator  i = tempPM.begin(); i != tempPM.end(); i++)
   {
      _priorityMap.push_back(*i);
   }
   return (*this);
}

const LgsVector(int) & PriorityElement::getPriorityMap(void) const
{
   return _priorityMap;
}

void PriorityBasedSSUFilter::initialize(void)
{
   IniParser argumentParser;
   char generalTableFileName[MAX_FILEPATH_LEN];

   _exclusionMap = new LgsVector(PriorityElement);
   _replacementMap = new LgsVector(PriorityElement);

   GetConfigData("sourcedata", "generaltbl", generalTableFileName, MAX_FILEPATH_LEN);
	argumentParser.open(generalTableFileName, "boris_exclusion");
   loadPriorityMap(argumentParser, _exclusionMap);
   argumentParser.section("boris_replacement");
   loadPriorityMap(argumentParser, _replacementMap);

   _initialized = true;
   
}

void PriorityBasedSSUFilter::cleanup(void)
{
   if (_exclusionMap)
   {
      _exclusionMap->clear();
      delete _exclusionMap;
      _exclusionMap = 0;
   }
   if (_replacementMap)
   {
      _replacementMap->clear();
      delete _replacementMap;
      _replacementMap = 0;
   }
   _initialized = false;
}

inline void PriorityBasedSSUFilter::loadPriorityMap(IniParser & argParser, 
                                             LgsVector(PriorityElement) *priorityMap)
{
   for (int i = 0; i <= MaxPriority; i++)
   {
      char buff[10];
      sprintf(buff, "%d", i);
      const Argument & arg = argParser.GetArgument(buff);
      if (!arg.IsNull())
      {
         PriorityElement tempPE(i);
         LgsVector(PriorityElement)::iterator currPE = 
                                   priorityMap->insert(priorityMap->end(), tempPE);
         LgsVector(LgsString) arrayValues;
         StringUtil::parseInto(arg.Value(), arrayValues, ',');
         for (LgsVector(LgsString)::iterator j = arrayValues.begin(); j != arrayValues.end(); j++)
         {
            int priorityNo = atoi(j->c_str());
            (*currPE).addElement(priorityNo);
         }
      }
   }
}

PriorityBasedSSUFilter::PriorityBasedSSUFilter(void)
{
   if (!_initialized)
   {
      initialize();
   }
}
void PriorityBasedSSUFilter::filter(SsuList * ssuList)
{
   _ssuList = ssuList;
   _priorityBitSet.reset();
   for (SsuList::iterator i = _ssuList->end()-1; i >= _ssuList->begin(); i--)
   {
      _priorityBitSet.set(i->priorityOrder());
   }

   applyExclusionFilter();
   applyReplacementFilter();
}

void PriorityBasedSSUFilter::applyExclusionFilter(void)
{
   for (SsuList::iterator i = _ssuList->end()-1; i >= _ssuList->begin(); i--)
   {
      PriorityElement tempPE(i->priorityOrder());
      LgsVector(PriorityElement)::const_iterator foundPE = 
                  find(_exclusionMap->begin(), _exclusionMap->end(), tempPE);   
      if (foundPE != _exclusionMap->end())
      {
         const LgsVector(int) & tempPM = foundPE->getPriorityMap();
         LgsVector(int)::const_iterator j;
         for (j = tempPM.begin(); (j != tempPM.end()) && (!_priorityBitSet.test(*j)); j++);
         if (j != tempPM.end())
         {
            _priorityBitSet.reset(i->priorityOrder());
            _ssuList->erase(i);
         }
      }
   }
}

void PriorityBasedSSUFilter::applyReplacementFilter(void)
{
   for (SsuList::iterator i = _ssuList->end()-1; (i >= _ssuList->begin()) && (_ssuList->size() > 3); i--)
   {
      if (_priorityBitSet.test(i->priorityOrder()))
      {
         PriorityElement tempPE(i->priorityOrder());
         LgsVector(PriorityElement)::const_iterator foundPE = 
                     find(_replacementMap->begin(), _replacementMap->end(), tempPE);   
         if (foundPE != _replacementMap->end())
         {
            const LgsVector(int) & tempPM = foundPE->getPriorityMap();
            for (LgsVector(int)::const_iterator j = tempPM.begin(); (j != tempPM.end()); j++)
            {
               // reset the bit corresponding to the priority which will be replaced by 
               // current SSU with i->priorityOrder(). The replacement is done later by 
               // testing for the bit we reset here and removing the SSU if the test fails.
               _priorityBitSet.reset(*j);
            }
         }
         else
         {
            // remove SSU which does not have any SSU to replace
            _ssuList->erase(i);
         }
      }
      else
      {
         // remove SSU, since this SSU was marked for deletion previously when an SSU
         // was found eligible to replace this SSU
         _ssuList->erase(i);
      }
   }
}
