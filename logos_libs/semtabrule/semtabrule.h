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
#ifndef _semtabrule_h_
#define _semtabrule_h_
#include  <math.h>
#include <logos_libs/utility/timestamp.h>

//#if defined(_MSC_VER)
//        #pragma comment(lib, "semtabrule.lib")
//#endif

//************************************************************
// Description: CSemtabRule class is used to encapsulate
//                              a semtab (also known as SP) rule.
//                              It initially has a constructor to constuctor
//                              the object from a blob (a bunch of bytes).
//                              We can add more constructors as needed.
//                              The blob is currently stored in relationa;
//                              database and CSemtabRuleBuilder is used to
//                              query the database and return a vector of
//                              CSemtabRule also known as CSemtabRuleVector.
//
// Author:              Manoj Agarwala
// History:             09/23/96 - Originally Conceived
//************************************************************
class CSemtabRule;

class CSPElement{
public:
        DummyLess(CSPElement)
        DummyEqual(CSPElement)

        CSPElement();
        ~CSPElement();

        const int& getWordClass() const { return m_wordClass; }
        void setWordClassCode(int num ) { m_wordClass = num;}

        const int& getType() const { return m_type; }
        void setType(int num ) { m_type = num;}

        const int& getForm() const { return m_form; }
        void setForm(int num ) { m_form = num;}

        const LgsVector(int)& getTags() const { return m_Tags; }
        LgsVector(int)& getTags() { return m_Tags; }

private:
        int  m_wordClass;
        int  m_type;
        int  m_form;
        LgsVector(int) m_Tags;
};

class CSemtabRule
{
public:
        DummyEqual(CSemtabRule)

        CSemtabRule(); //Default constructor for the CSemtabRuleVector
        ~CSemtabRule();

        ostream& OutputToOstream(ostream &s) const;
        istream& InputFromIstream(istream &s);


        friend bool operator<(const CSemtabRule& x, const CSemtabRule& y);
        friend ostream& operator<<(ostream&, const CSemtabRule&);
        friend istream& operator>>(istream&, CSemtabRule&);

        const int& getSemtabId() const { return m_semtab_id;}
        void setSemtabId(int semtabId) { m_semtab_id = semtabId;}

        const LgsString& getCompanyCode() const { return m_company_code;}
        void setCompanyCode(const LgsString& str ) { m_company_code = str;}

        const LgsString& getSourceLanguageCode() const { return m_source_language_code;}
        void setSourceLanguageCode(const LgsString& str ) { m_source_language_code = str;}

        const LgsString& getTargetLanguageCode() const { return m_target_language_code;}
        void setTargetLanguageCode(const LgsString& str ) { m_target_language_code = str;}

        const LgsString& getSubjectMatterCode() const { return m_subject_matter_code;}
        void setSubjectMatterCode(const LgsString& str ) { m_subject_matter_code = str;}


        const LgsString& getCommentLine() const { return m_comment_line;}
        void setCommentLine(const LgsString& str ) { m_comment_line = str;}

        const LgsString& getCreatedBy() const { return m_createdBy;}
        void setCreatedBy(const LgsString& str ) { m_createdBy = str;}

        const LgsString& getModifiedBy() const { return m_modifiedBy;}
        void setModifiedBy(const LgsString& str ) { m_modifiedBy = str;}

        const TimeStamp& getDateCreated() const { return m_dateCreated;}
        void setDateCreated(const TimeStamp& obj ) { m_dateCreated = obj;}

        const TimeStamp& getDateModified() const { return m_dateModified;}
        void setDateModified(const TimeStamp& obj ) { m_dateModified = obj;}


        const int& getWordClassCode() const { return m_word_class_code;}
        void setWordClassCode(int num ) { m_word_class_code = num;}

        const int& getSubsetId() const { return m_subset_id;}
        void setSubsetId(int num ) { m_subset_id = num;}

        const int& getSetId() const { return m_set_id;}
        void setSetId(int num ) { m_set_id = num;}

        const int& getRuleLevel() const { return m_rule_level;}
        void setRuleLevel(int num ) { m_rule_level = num;}

        const int& getSpecificity() const { return m_specificity;}
        void setSpecificity(int num ) { m_specificity = num;}


        char getDeactivationSwitch() const { return m_deactivation_switch;}
        void setDeactivationSwitch(char c ) { m_deactivation_switch = c;}

        const LgsVector(int)&  getVtrs() const { return m_Vtrs; }
        const LgsVector(CSPElement)& getSPElements()  const { return m_SPElements; }

        LgsVector(int)&  getVtrs(){ return m_Vtrs; }
        LgsVector(CSPElement)& getSPElements()  { return m_SPElements; }

private:

        int                 m_semtab_id;
        LgsString              m_company_code;
        LgsString              m_source_language_code;
        LgsString              m_target_language_code;
		LgsString              m_subject_matter_code;
        LgsString              m_comment_line;
        int                 m_word_class_code;
        int                 m_subset_id;
        int                 m_set_id;
        char                m_deactivation_switch;
        LgsVector(int)         m_Vtrs;
        LgsVector(CSPElement)  m_SPElements; //Number of CSPElement is m_rule_level-1
        int                 m_rule_level;
        int                 m_specificity;
                TimeStamp           m_dateCreated;
                LgsString              m_createdBy;
                TimeStamp           m_dateModified;
                LgsString              m_modifiedBy;
};

/*
	Special compare utility used in sorting the semtab rule. This compare makes sure
	that pieces of the rule are sort in the right order.
	For example some of the fields of a semtab rule would be the following:
			6
			9
			0
			-4
			-8
	These vales should end up sorting the following:
			9
			6
			-8
			-4
			0
	so return -1 if x < y
			   0 if x = y
			   1 if x > y
*/
inline int logosCompare(int x, int y)
{
		
	if ( x == y ) return 0;

	if ( x >= 0 && y >= 0)
	{
		 if (x < y) return -1;
		 return 1;
	}
	else if ( x < 0 && y < 0 )
	{
		 if (x > y) return -1;			// tricky one!
		 return 1;
	}
	else if (x == 0)
	{
		return -1;
	}
	else if (y == 0)
	{
		return 1;
	}
	else if (x < 0)
	{
		return -1;
	}
	else if (y < 0)
	{
		return 1;
	}
	else 
	{
		return 0;
	}
}
//We are not using typedef becuase this allows us to
//add member functions as needed
class CSemtabRuleVector : public LgsVector(CSemtabRule)
{
};


#endif

