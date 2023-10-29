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
// File - LSemantoSyntacticUnit.cpp (implementation)
// --------------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/lsemantosyntacticunit.h>
#include <logos_libs/linguistic/context.h>
#include <logos_libs/translutility/translcommonobjects.h>

int LSemantoSyntacticUnit::st_nextOid = 1;
int LSemantoSyntacticUnit::st_unfoundWordID = -10;


// --------------------------------------------------------------------------
LSemantoSyntacticUnit* LSemantoSyntacticUnit::create()
{
   LSemantoSyntacticUnit* newUnit = new LSemantoSyntacticUnit;
   return newUnit;
}

// --------------------------------------------------------------------------
// Default constructor - Sets defaults for all the data that eventually should come
// out of the database for this unit.
// --------------------------------------------------------------------------
LSemantoSyntacticUnit::LSemantoSyntacticUnit()
                      :v_contextValue(Context::InvalidValue),
                       v_isSourceTransferred(false),
                       p_language(0),
                       companyCode_("LOG"),
                       wordID_(0),
                       wordCount_(1),
                       wordTypeCode_(1),
                       protectionCode_(0),
                       hashCode1_(0),
					   rootHashCode1_(0),
                       hashCode2_(0),
					   rootHashCode2_(0),
                       hashLocation_(0),
                       headWord_(1),
                       blackHoleLocation_(0),
                       wildcardPosition_(0),
                       isAspire_(false),
                       isMonth_(false),
                       isPrefix_(false),
                       isNeverEos_(false),
                       isOrdinal_(false),
                       wordClassCode_(LLanguage::NOUN),
                       genderCode_(0),
                       numericConstraint_(0),
                       auxiliaryCode_(0),
                       inflectionPosition_(0),
                       verbPrefixInseparable_(0),
                       verbPrefixSeparable_(0),
                       verbPrefixInsepLength_(0),
                       verbPrefixSepLength_(0),
                       patNumber_(0),
                       sourceStemNumber_(0),
                       rootUsageID_(0),
                       sourceAnalysisCode_(1),
                       meaningID_(0),
                       usageID_(0),
                       priorityOrder_(99),
                       atomicCode_(0),      // TO BE REMOVED WHEN NEW SMC IS TESTED,
                       genericCode_(0),     // TO BE REMOVED WHEN NEW SMC IS TESTED,
                       superSetID_(1),
                       setWordClassCode_(0),
                       setID_(1),
                       superWordClassCode_(0),
                       subSetID_(1),
                       formCode_(0),
                       overflow2b_(0),
                       overflow3b_(0),
                       transitivity_(true),
                       isHomograph_(false),
                       isPrimary_(false),
                       isDeactivated_(false),
                       isMiscoded_(false),
                       matchedOnFullyCapped_(false)
{
   setOid();
   smc_.clear();
}

// --------------------------------------------------------------------------
// Set this object from the given object
// --------------------------------------------------------------------------
LSemantoSyntacticUnit::LSemantoSyntacticUnit(const LSemantoSyntacticUnit& rhs)
                      :v_oid(rhs.v_oid)
{
   p_language = rhs.p_language;
   copyComponents(rhs);
   v_oid = rhs.oid();
}

// --------------------------------------------------------------------------
// Destructor
// --------------------------------------------------------------------------
LSemantoSyntacticUnit::~LSemantoSyntacticUnit()
{
}

// --------------------------------------------------------------------------
bool LSemantoSyntacticUnit::useHashForHenum() const
{
   return ((TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::EnglishID) &&
           (((wordClassCode_ == 1) && ((patNumber_ == 18) || (patNumber_ == 177))) ||
            (((wordClassCode_ == 3) || (wordClassCode_ == 6)) &&
             ((patNumber_ == 55) || (patNumber_ == 195))) ||
            ((wordClassCode_ == 4) && ((patNumber_ == 33) || (patNumber_ == 39) || (patNumber_ == 285)))));
}

// --------------------------------------------------------------------------
// Copy the given object to this object
// --------------------------------------------------------------------------
void LSemantoSyntacticUnit::copyComponents(const LSemantoSyntacticUnit& rhs)
{
   companyCode_ = rhs.companyCode_;
   wordID_ = rhs.wordID_;
   wordCount_ = rhs.wordCount_;
   word_ = rhs.word_;
   targetWord_ = rhs.targetWord_;
   wordTypeCode_ = rhs.wordTypeCode_;
   protectionCode_ = rhs.protectionCode_;
   hashCode1_ = rhs.hashCode1_;
   rootHashCode1_ = rhs.rootHashCode1_;
   hashCode2_ = rhs.hashCode2_;
   rootHashCode2_ = rhs.rootHashCode2_;
   hashLocation_ = rhs.hashLocation_;
   headWord_ = rhs.headWord_;
   blackHoleLocation_ = rhs.blackHoleLocation_;
   wildcardPosition_ = rhs.wildcardPosition_;
   isAspire_ = rhs.isAspire_;
   isMonth_ = rhs.isMonth_;
   isPrefix_ = rhs.isPrefix_;
   isNeverEos_ = rhs.isNeverEos_;
   isOrdinal_ = rhs.isOrdinal_;
   wordClassCode_ = rhs.wordClassCode_;
   genderCode_ = rhs.genderCode_;
   numericConstraint_ = rhs.numericConstraint_;
   auxiliaryCode_ = rhs.auxiliaryCode_;
   inflectionPosition_ = rhs.inflectionPosition_;
   verbPrefixInseparable_ = rhs.verbPrefixInseparable_;
   verbPrefixSeparable_ = rhs.verbPrefixSeparable_;
   verbPrefixInsepLength_ = rhs.verbPrefixInsepLength_;
   verbPrefixSepLength_ = rhs.verbPrefixSepLength_;
   patNumber_ = rhs.patNumber_;
   sourceStemNumber_ = rhs.sourceStemNumber_;
   rootUsageID_ = rhs.rootUsageID_;
   sourceAnalysisCode_ = rhs.sourceAnalysisCode_;
   meaningID_ = rhs.meaningID_;
   usageID_ = rhs.usageID_;
   priorityOrder_ = rhs.priorityOrder_;
   atomicCode_ = rhs.atomicCode_;         // TO BE REMOVED WHEN NEW SMC IS TESTED
   genericCode_ = rhs.genericCode_;       // TO BE REMOVED WHEN NEW SMC IS TESTED
   smc_ = rhs.smc_;
   superSetID_ = rhs.superSetID_;
   setWordClassCode_ = rhs.setWordClassCode_;
   setID_ = rhs.setID_;
   superWordClassCode_ = rhs.superWordClassCode_;
   subSetID_ = rhs.subSetID_;
   formCode_ = rhs.formCode_;
   overflow2b_ = rhs.overflow2b_;
   overflow3b_ = rhs.overflow3b_;
   transitivity_ = rhs.transitivity_;
   isHomograph_ = rhs.isHomograph_;
   isPrimary_ = rhs.isPrimary_;
   isDeactivated_ = rhs.isDeactivated_;
   isMiscoded_ = rhs.isMiscoded_;
   setIsSourceTransferred(rhs.isSourceTransferred());
   v_contextValue = rhs.v_contextValue;
   matchedOnFullyCapped_ = rhs.matchedOnFullyCapped_;
}


// --------------------------------------------------------------------------
// The inflection could well be an empty LgsString. Even if it is it is relevant for
// determining a derived form. The actual algorithm for determining a derived form
// is language dependent and is within the appropriate language object. If the language
// object fails to return a derived form or returns a 0 the unit resorts to its default
// form code from the database.
// --------------------------------------------------------------------------
void LSemantoSyntacticUnit::derivedFormCode(const LInflection* inflection, bool germanNonHead)
{
   if (germanNonHead)
   {
      formCode_ = 9;
      return;
   }

   if (inflection)
   {
      if (patNumber())
      {
         const DDerivedForm* pForm =
         language().findDerivedForm(patNumber(), sourceStemNumber(), inflection);
         if (pForm && (!pForm->FormCode() == 0))
         {
            formCode_ = pForm->FormCode();
         }
      }
   }
}

// --------------------------------------------------------------------------
int LSemantoSyntacticUnit::derivedFormCode() const
{
   return formCode_;
}

// --------------------------------------------------------------------------
// True only if the language does has a matching derived form (or if the patNumber
// is 0 and the inflection is empty. Otherwise returns false.
// --------------------------------------------------------------------------
bool LSemantoSyntacticUnit::isFormDeriveable(const LInflection* inflection) const
{
   bool result = false;

   if (patNumber())
   {
      if (language().findDerivedForm(patNumber(), sourceStemNumber(), inflection))
      {
         result = true;
      }
   }
   else if (inflection == LLanguage::nullInflection())
   {
      result = true;
   }
   return result;
}

// --------------------------------------------------------------------------
const LSemantoSyntacticUnit& LSemantoSyntacticUnit::operator=(const LSemantoSyntacticUnit& rhs)
{
   if (this != &rhs)
   {
      copyComponents(rhs);
      v_oid = rhs.oid();
      p_language = rhs.p_language;
   }
   return *this;
}

// --------------------------------------------------------------------------
bool LSemantoSyntacticUnit::operator==(const LSemantoSyntacticUnit& rhs) const
{
   bool equivalent = false;

   if (oid() == rhs.oid())
   {
      equivalent = true;
   }
   else 
   {
      equivalent = false;
   }

   return equivalent;
}


// --------------------------------------------------------------------------
// Calculate the contextual value of the proposed company code and subject matter code
// depending on the selected option for the dictionary search (ie, value of where the
// proposed pair <CC,SMC> stands in the ordered strutures of selected CCs and SMCs.
// --------------------------------------------------------------------------
void LSemantoSyntacticUnit::findContextValue(const LLanguage& language)
{
	//cout << endl << "*** WORD: " << word() << " ***" << endl;		// debug
   ContextualUnit cu(companyCode_, smc_);
	v_contextValue = TranslCommonObjects::GetContext()->match(cu);
}
