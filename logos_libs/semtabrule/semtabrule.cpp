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
#include <logos_include/logoscommon.h>
#include <logos_libs/semtabrule/semtabrule.h>
#include <logos_libs/utility/umladeumla.h>
#include <logos_libs/utility/stringutil.h>
#include /**/ <memory.h>

CSPElement::CSPElement()
{
}
 
CSPElement::~CSPElement()
{
   m_Tags.erase(m_Tags.begin(), m_Tags.end());
}

CSemtabRule::CSemtabRule()
: m_semtab_id(-1)
{
        m_dateCreated.year = 0;
        m_dateCreated.month = 0;
        m_dateCreated.day = 0;
        m_dateCreated.hour = 0;
        m_dateCreated.minute = 0;
        m_dateCreated.second = 0;
        m_dateCreated.fraction = 0;

        m_dateModified.year = 0;
        m_dateModified.month = 0;
        m_dateModified.day = 0;
        m_dateModified.hour = 0;
        m_dateModified.minute = 0;
        m_dateModified.second = 0;
        m_dateModified.fraction = 0;
}

CSemtabRule::~CSemtabRule()
{
   m_SPElements.erase(m_SPElements.begin(), m_SPElements.end());
   m_Vtrs.erase(m_Vtrs.begin(), m_Vtrs.end());
}

//Returns true if x < y, else returns false
bool operator<(const CSemtabRule& x, const CSemtabRule& y)
{
	int ret;
        //We are assuming that source and target language are same
        //when comparison is made. It does not make sense to compare
        //rules from different source and target language pairs.
        //
        //We might want to throw an exception if this is not true;


        //Compare wordclass
		ret = logosCompare(x.getWordClassCode(), y.getWordClassCode() );
		if (ret < 0 ) return true;
		if (ret > 0 ) return false;
        //Compare subset
        if( x.getSubsetId() < y.getSubsetId() )
                return true;
        else if( x.getSubsetId() > y.getSubsetId() )
                return false;
        //Compare set
        else if( x.getSetId() < y.getSetId() )
                return true;
        else if( x.getSetId() > y.getSetId() )
                return false;
        //Compare wordclass[0]
        else if( x.getSPElements()[0].getWordClass() < y.getSPElements()[0].getWordClass() )
                return true;
        else if( x.getSPElements()[0].getWordClass() > y.getSPElements()[0].getWordClass() )
                return false;
        //Compare type[0]
        else if( x.getSPElements()[0].getType() < y.getSPElements()[0].getType() )
                return true;
        else if( x.getSPElements()[0].getType() > y.getSPElements()[0].getType() )
                return false;
        //Compare form[0]
		ret = logosCompare(x.getSPElements()[0].getForm(), y.getSPElements()[0].getForm() );
		if (ret < 0 ) return true;
		if (ret > 0 ) return false;
        //Compare rulelevel
        if( x.getRuleLevel() < y.getRuleLevel() )
                return true;
        else if( x.getRuleLevel() > y.getRuleLevel() )
                return false;

    //Control here means rule levels are equal
    //i starts at 1 because 0th element has already been compared
    int i;
    for(i=1; i < x.getRuleLevel()-1; i++)
    {
        //Compare wordclass[i]
		ret = logosCompare(x.getSPElements()[i].getWordClass(), y.getSPElements()[i].getWordClass() );
		if (ret < 0 ) return true;
		if (ret > 0 ) return false;
        //Compare type[i]
        else if( x.getSPElements()[i].getType() < y.getSPElements()[i].getType() )
                return true;
        else if( x.getSPElements()[i].getType() > y.getSPElements()[i].getType() )
                return false;
        //Compare form[i]
		ret = logosCompare(x.getSPElements()[i].getForm(), y.getSPElements()[i].getForm() );
		if (ret < 0 ) return true;
		if (ret > 0 ) return false;
    }

    //Control here means rule levels are equal
    for(i=0; i < x.getRuleLevel()-1; i++)
    {
        //Compare tag[i][0]
        if( x.getSPElements()[i].getTags()[0] < y.getSPElements()[i].getTags()[0] )
                return true;
        else if( x.getSPElements()[i].getTags()[0] > y.getSPElements()[i].getTags()[0] )
                return false;

        //Compare tag[i][1]
        if( x.getSPElements()[i].getTags()[1] < y.getSPElements()[i].getTags()[1] )
                return true;
        else if( x.getSPElements()[i].getTags()[1] > y.getSPElements()[i].getTags()[1] )
                return false;
    }

    //Compare companycode
    if( x.getCompanyCode() < y.getCompanyCode() )
            return true;
    else if( x.getCompanyCode() > y.getCompanyCode() )
            return false;
    //Compare specificity
    else if( x.getSpecificity() < y.getSpecificity() )
            return true;
    else if( x.getSpecificity() > y.getSpecificity() )
            return false;

    //Control here means all the items are equal
    return false;
}

istream& operator>>(istream& s, CSemtabRule& sr)
{
        return sr.InputFromIstream(s);
}

istream& CSemtabRule::InputFromIstream(istream &s)
{
                //Make sure that the input stream is set to base 10
                s.setf(ios::dec, ios::basefield );

        unsigned char  seperator;
        char  buffer[260];

        m_semtab_id = -1;

        s   >> m_dateCreated.month ;
		s   >> seperator ;
        s   >> m_dateCreated.day ;
		s   >> seperator;
        s   >> m_dateCreated.year ;
		s   >> seperator;

        //Extract the company code (1 extra is needed for null
        s.get(buffer, 4, ',');
        m_company_code = buffer;
        s >> seperator; //Extract the seperator

        //Extract the source language code and seperator
        s.get(buffer, 3, '\n');
        m_source_language_code = buffer;
        s >> seperator; //Extract the seperator

        //Extract the target language code and seperator
        s.get(buffer, 3, '\n');
        m_target_language_code = buffer;
        s >> seperator; //Extract the seperator

        s >> m_deactivation_switch >> seperator;


        //Extract the 2 character generic code code and seperator
        s.get(buffer, 3, '\n');
        LgsString generic_code_as_str = buffer;
        s >> seperator; //Extract the seperator

        //Extract the 3 character atmic code code and seperator
        s.get(buffer, 4, '\n');
        LgsString atomic_code_as_str = buffer;
        int atomic_code = atoi(buffer);
        s >> seperator;

		LgsString tmp = itoa(atomic_code, buffer, 32);
		if ( strlen(buffer) == 1 )
			tmp = "0" + tmp;

		if ( m_company_code == "LOG" )
			m_subject_matter_code = '0' + generic_code_as_str + atomic_code_as_str ;
		else
			m_subject_matter_code = m_company_code + 'U' + generic_code_as_str + 'U' + StringUtil::upper(tmp) ;

        s   >> m_word_class_code >> seperator
            >> m_subset_id >> seperator
            >> m_set_id >> seperator
            >> m_specificity >> seperator
            >> m_rule_level >> seperator;

        getSPElements().erase(getSPElements().begin(), getSPElements().end());

        int i;
        for(i=0; i<9; i++)
        {
                short wci, typei, formi;

                s       >> wci >> seperator
                        >> typei >> seperator
                        >> formi >> seperator;

                if( i < m_rule_level-1 )
                {
                        CSPElement sPElement;

                        sPElement.setWordClassCode(wci);
                        sPElement.setType(typei);
                        sPElement.setForm(formi);

                        getSPElements().push_back(sPElement);
                }
        }

        for(i=0; i<9; i++)
        {
                short tagi1, tagi2;

                s       >> tagi1 >> seperator
                        >> tagi2 >> seperator;

                if( i < m_rule_level-1 )
                {
                        getSPElements()[i].getTags().push_back(tagi1);
                        getSPElements()[i].getTags().push_back(tagi2);
                }
        }

        short commentLength;
        s >> commentLength >> seperator;

        if(  s.eof() )
        {
                int  tmp = 0;
                throw 0;
        }

        if( !s.good() )
        {
                LgsString msg = "Stream is not good any more";
                throw msg;
        }

        if( commentLength > 256 )
        {
                LgsString msg = "Comment cannot be more than 256 characters, comment length =" + commentLength;
                throw msg;
        }

        //Extract the comment and seperator +1 for NULL
        s.get(buffer, commentLength+1, '\n');
        m_comment_line = buffer;
        s >> seperator;

                //Remove Extraneous information such as date created,
                //creator's initial etc. from the comment line.
                //We use heuristic for this.
                //
                //Remove any characters which follows 3 or more spaces.
                int pos = m_comment_line.find(LgsString("   "));
                if( pos >=0 )
                        m_comment_line = m_comment_line.substr(0, pos);

                //Converts logos telecodes to iso characters
                m_comment_line = CUmlaDeumla::umla(m_comment_line);

        for(i=0; i < m_rule_level-1; i++)
        {
                short tagSetILength;

                s >> tagSetILength >> seperator;

                for(int j=0; j< tagSetILength ; j++)
                {
                        short tag;

                        s >> tag >> seperator;

                        getSPElements()[i].getTags().push_back(tag);
                }
        }

        getVtrs().erase(getVtrs().begin(), getVtrs().end());

        short numOfVtrs;

                //We don't extract the separator here as the
                //numOfVtrs may be 0
        s >> numOfVtrs ;

        for(i=0; i< numOfVtrs; i++)
        {
                short vtr;

                s  >> seperator >> vtr ;

                getVtrs().push_back(vtr);
        }

//#if _MSC_VER >= 1100
                //Read the rest of the line upto newline character.
                //getline will automatically discard the newline character
                //Rest of the line will be either a comma followed by a newline
                //or just a newline.
        s.getline(buffer, 3);
//#else
        //I was not able to read the newline character into seperator
        //s.get(buffer, 2, ',');
//#endif

        return s;
}

ostream& operator<<(ostream& s, const CSemtabRule& sr)
{
        return sr.OutputToOstream(s);
}

ostream& CSemtabRule::OutputToOstream(ostream &s) const
{
        char seperator=',';


        s
                << m_dateCreated.month << seperator
                << m_dateCreated.day << seperator
                << m_dateCreated.year << seperator
                << m_company_code << seperator
                << m_source_language_code << seperator
                << m_target_language_code << seperator
                << m_deactivation_switch << seperator
                << m_word_class_code << seperator
                << m_subset_id << seperator
                << m_set_id << seperator
                << m_specificity << seperator
                << m_rule_level << seperator;

        int i;
        for(i=0; i<9; i++)
        {
                if( i < m_rule_level-1 )
                {
                        s       << getSPElements()[i].getWordClass() << seperator
                                << getSPElements()[i].getType() << seperator
                                << getSPElements()[i].getForm() << seperator;
                }
                else
                {
                        s       << 0 << seperator
                                << 0 << seperator
                                << 0 << seperator;
                }
        }

        for(i=0; i<9; i++)
        {
                if( i < m_rule_level-1 )
                {
                        s << getSPElements()[i].getTags()[0] << seperator;
                        s << getSPElements()[i].getTags()[1] << seperator;
                }
                else
                {
                        s << 0 << seperator;
                        s << 0 << seperator;
                }
        }

        s << m_comment_line.length() << seperator;

        s << m_comment_line << seperator;

        for(i=0; i < m_rule_level-1; i++)
        {
                //First 2 tags are outputed separately, hence -2
                short tagSetILength = m_SPElements[i].getTags().size() -2;

                s << tagSetILength << seperator;

                for(int j=0; j< tagSetILength ; j++)
                {
                        s << getSPElements()[i].getTags()[j+2] << seperator;
                }
        }

        short numOfVtrs = getVtrs().size();

        s << numOfVtrs << seperator;

        for(i=0; i< numOfVtrs; i++)
        {
                //Treat the last item slightly differently
                if( i == numOfVtrs-1)
                        s << getVtrs()[i] << '\n';
                else
                        s << getVtrs()[i] << seperator;
        }

        return s;
}

