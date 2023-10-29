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
//////////////////////////////////////////////////////////////////////
//
// LgsEntity.cpp: implementation of the CLgsEntity class.
//
//////////////////////////////////////////////////////////////////////

#include <logos_include/logoscommon.h>
#include <logos_libs/lgssgml/lgsutil.h>
#include <logos_libs/lgssgml/lgsentity.h>

// -------------------------------------------------------------------
CLgsEntity::CLgsEntity()
{
   Init();
}
// -------------------------------------------------------------------
CLgsEntity::~CLgsEntity()
{
   m_CharToEntityMap.clear();
   m_EntityToCharMap.clear();
}
// -------------------------------------------------------------------
//if entity found in table, return Logos text for it
char* CLgsEntity::toTranText(char* str, bool Text)
{
	if (Text)
	{
      EntityToChar::iterator cteIt = m_EntityToCharMap.find(LgsString(str));
      return (cteIt != m_EntityToCharMap.end())? &((*cteIt).second): 0 ;
	}
	else
	{
      CharToEntity::iterator etcIt = m_CharToEntityMap.find(*str);
      return (etcIt != m_CharToEntityMap.end())? const_cast<char*>((*etcIt).second.c_str()): 0;
	}
}
// -------------------------------------------------------------------
void CLgsEntity::replaceExtChars(LgsString & str)
{
   LgsString internalString = "";
   for (LgsString::iterator sIter = str.begin(); sIter != str.end(); sIter++)
   {
      char ch = *sIter;
      //Check if the '&' found belongs to a entity
      if (ch == '\x26')
      {
         //MS IE allows entities of this format &#169 also
         //From Nitish
         if (sIter + 1 != str.end() && *(sIter+1) == '#')
         {
            internalString += *sIter;
            sIter++;
         }
         else
         {
	         char tch = 0;
	         LgsString::iterator tmpIt = sIter;
	         while (++tmpIt != str.end())
	         {
		         tch = *tmpIt;
		         if (tch == ';' || tmpIt - sIter > 7)
			         break;
	         }
	         if (tch == ';')
            {
               internalString += *sIter;
		         sIter++;
            }
         }
      }
      if (sIter == str.end())
      {
         internalString += *sIter;
         break;
      }
      else
         ch = *sIter;
      //If the char is greater than 127 then lookup in the table
      if (ch < 0  || ch == '\x26' || ch == '\x3c' || ch == '\x3e'
         || ch == '\\' || ch == '{' || ch == '}') 
      {
         char* pBuffer = toTranText(&ch, false);
         if (pBuffer)
            internalString += LgsString("&") + LgsString(pBuffer) + LgsString(";");
         else
            internalString += *sIter;
      }
      else
         internalString += *sIter;
   }
   
   str = internalString;
}
// -------------------------------------------------------------------
void CLgsEntity::replaceEntity(LgsString & str)
{
	LgsString internalString = "";
	for (LgsString::iterator sIter = str.begin(); sIter != str.end(); sIter++)
	{
		char ch = *sIter;
      unsigned char uch =  0;
      bool bIEntity = false;
		//Entity begins with an ampersand('&') and ends with a semi-colon(';')
		//the characters between them uniquely represent an accented or extended character
		if (ch == '&')
		{
			char tmpBuffer[10] = ""; 
			int nIndex = 0;
			LgsString::iterator tmpIter = sIter;
			while (++sIter != str.end())
			{
				ch = *sIter;
				if (ch == ';' || nIndex > 7)
					break;
				else
					tmpBuffer[nIndex++] = ch;
			}
			//Terminate the LgsString accumulated
			tmpBuffer[nIndex++] = '\0';
			// The following is added to fix bug 1583
			int nTempNum = atoi(tmpBuffer+1);
         if ((tmpBuffer[0] == '#') && (nTempNum  < 256) ) //old(tmpBuffer[0] == '#')
         {
            uch = atoi(tmpBuffer+1);
            internalString += uch;
         }
         else
         {
			   //Search in the Entity table
			   char* pBuffer = toTranText(tmpBuffer, true);   
			   if (pBuffer)  //entity found
			   {
				   pBuffer[1] ='\0';
				   internalString += LgsString(pBuffer);
			   }
			   else
			   {
				   sIter = tmpIter;
				   internalString += *sIter;
			   }
         }
		}
		else
			internalString += *sIter;
		//Bounds check - b'cause when the iterator is incremented beyond 'end()' the 
		//condition str.end() != iterator is still true and causes access violation
		if (sIter == str.end())
			break;
	}
	//return the resulting LgsString
	str = internalString; 
}
// -------------------------------------------------------------------
void CLgsEntity::AddEntityPair(char* ch, char * entity)
{
   if (ch != 0 && entity != 0)
      m_CharToEntityMap.insert(CharToEntity::value_type(*ch, LgsString(entity)));
}
// -------------------------------------------------------------------
void CLgsEntity::Init()
{
   LgsVector(EntityPair) entityTable;
   LgsVector(EntityPair)::iterator entityTableIt, entityTableEnd;

   //accents are the critical ones; some others added
   entityTable.push_back(EntityPair("\xc1", "Aacute"));  // Aacute 
   entityTable.push_back(EntityPair("\xc0", "Agrave"));  // Agrave 
   entityTable.push_back(EntityPair("\xc2", "Acirc"));   // Acircumflex 
   entityTable.push_back(EntityPair("\xc3", "Atilde"));  // Atilde 
   entityTable.push_back(EntityPair("\xc4", "Auml"));    // Adieresis 
   entityTable.push_back(EntityPair("\xc5", "Aring"));   // Aring 
   entityTable.push_back(EntityPair("\xc6", "AElig"));   // AElig 
   entityTable.push_back(EntityPair("\xc7", "Ccedil"));  // Ccedilla 
   entityTable.push_back(EntityPair("\xc9", "Eacute"));  // Eacute 
   entityTable.push_back(EntityPair("\xc8", "Egrave"));  // Egrave 
   entityTable.push_back(EntityPair("\xca", "Ecirc"));   // Ecircumflex 
   entityTable.push_back(EntityPair("\xcb", "Euml"));    // Edieresis 
   entityTable.push_back(EntityPair("\xcd", "Iacute"));  // Iacute 
   entityTable.push_back(EntityPair("\xcc", "Igrave"));  // Igrave 
   entityTable.push_back(EntityPair("\xce", "Icirc"));   // Icircumflex 
   entityTable.push_back(EntityPair("\xcf", "Iuml"));    // Idieresis 
   entityTable.push_back(EntityPair("\xd0", "ETH"));     // ETH 
   entityTable.push_back(EntityPair("\xd1", "Ntilde"));  // Ntilde 
   entityTable.push_back(EntityPair("\xd3", "Oacute"));  // Oacute 
   entityTable.push_back(EntityPair("\xd2", "Ograve"));  // Ograve 
   entityTable.push_back(EntityPair("\xd4", "Ocirc"));   // Ocircumflex 
   entityTable.push_back(EntityPair("\xd5", "Otilde"));  // Otilde 
   entityTable.push_back(EntityPair("\xd6", "Ouml"));    // Odieresis 
   entityTable.push_back(EntityPair("\xd8", "Oslash"));  // Oslash
   entityTable.push_back(EntityPair("\xda", "Uacute"));  // Uacute 
   entityTable.push_back(EntityPair("\xd9", "Ugrave"));  // Ugrave 
   entityTable.push_back(EntityPair("\xdb", "Ucirc"));   // Ucircumflex 
   entityTable.push_back(EntityPair("\xdc", "Uuml"));    // Udieresis 
   entityTable.push_back(EntityPair("\xdd", "Yacute"));  // Yacute 
   entityTable.push_back(EntityPair("\xde", "THORN"));   // THORN
   entityTable.push_back(EntityPair("\xe1", "aacute"));  // aacute 
   entityTable.push_back(EntityPair("\xe0", "agrave"));  // agrave 
   entityTable.push_back(EntityPair("\xe2", "acirc"));   // acircumflex 
   entityTable.push_back(EntityPair("\xe3", "atilde"));  // atilde 
   entityTable.push_back(EntityPair("\xe4", "auml"));    // adieresis 
   entityTable.push_back(EntityPair("\xe5", "aring"));   // aring 
   entityTable.push_back(EntityPair("\xe6", "aelig"));   // aelig 
   entityTable.push_back(EntityPair("\xe7", "ccedil"));  // ccedilla 
   entityTable.push_back(EntityPair("\xe9", "eacute"));  // eacute 
   entityTable.push_back(EntityPair("\xe8", "egrave"));  // egrave 
   entityTable.push_back(EntityPair("\xea", "ecirc"));   // ecircumflex 
   entityTable.push_back(EntityPair("\xeb", "euml"));    // edieresis 
   entityTable.push_back(EntityPair("\xed", "iacute"));  // iacute 
   entityTable.push_back(EntityPair("\xec", "igrave"));  // igrave 
   entityTable.push_back(EntityPair("\xee", "icirc"));   // icircumflex 
   entityTable.push_back(EntityPair("\xef", "iuml"));    // idieresis 
   entityTable.push_back(EntityPair("\xf0", "eth"));     // eth 
   entityTable.push_back(EntityPair("\xf1", "ntilde"));  // ntilde 
   entityTable.push_back(EntityPair("\xf3", "oacute"));  // oacute 
   entityTable.push_back(EntityPair("\xf2", "ograve"));  // ograve 
   entityTable.push_back(EntityPair("\xf4", "ocirc"));   // ocircumflex 
   entityTable.push_back(EntityPair("\xf5", "otilde"));  // otilde 
   entityTable.push_back(EntityPair("\xf6", "ouml"));    // odieresis 
   entityTable.push_back(EntityPair("\xf8", "oslash"));  // oslash
   entityTable.push_back(EntityPair("\xdf", "szlig"));   // sharpS 
   entityTable.push_back(EntityPair("\xfa", "uacute"));  // uacute 
   entityTable.push_back(EntityPair("\xf9", "ugrave"));  // ugrave 
   entityTable.push_back(EntityPair("\xfb", "ucirc"));   // ucircumflex 
   entityTable.push_back(EntityPair("\xfc", "uuml"));    // udieresis 
   entityTable.push_back(EntityPair("\xfd", "yacute"));  // yacute 
   entityTable.push_back(EntityPair("\xfe", "thorn"));   // thorn 
   entityTable.push_back(EntityPair("\xff", "yuml"));    // yuml 

   entityTable.push_back(EntityPair("\xa1", "iexcl"));
   entityTable.push_back(EntityPair("\xa2", "cent"));
   entityTable.push_back(EntityPair("\xa3", "pound"));
   entityTable.push_back(EntityPair("\xa4", "curren"));

   entityTable.push_back(EntityPair("\xa5", "yen"));
   entityTable.push_back(EntityPair("\xa6", "brvbar"));
   entityTable.push_back(EntityPair("\xa7", "sect"));
   entityTable.push_back(EntityPair("\xa8", "uml"));

   entityTable.push_back(EntityPair("\xaa", "ordf"));
   entityTable.push_back(EntityPair("\xab", "laquo"));
   entityTable.push_back(EntityPair("\xac", "not"));
   entityTable.push_back(EntityPair("\xad", "shy"));
   entityTable.push_back(EntityPair("\xaf", "macr"));

   entityTable.push_back(EntityPair("\xb0", "deg"));
   entityTable.push_back(EntityPair("\xb1", "plusmn"));
   entityTable.push_back(EntityPair("\xb2", "sup2"));
   entityTable.push_back(EntityPair("\xb3", "sup3"));

   entityTable.push_back(EntityPair("\xb4", "acute"));
   entityTable.push_back(EntityPair("\xb5", "micro"));
   entityTable.push_back(EntityPair("\xb6", "para"));
   entityTable.push_back(EntityPair("\xb7", "middot"));

   entityTable.push_back(EntityPair("\xb8", "cedil"));
   entityTable.push_back(EntityPair("\xb9", "sup1"));
   entityTable.push_back(EntityPair("\xba", "ordm"));
   entityTable.push_back(EntityPair("\xbb", "raquo"));

   entityTable.push_back(EntityPair("\xbc", "frac14"));
   entityTable.push_back(EntityPair("\xbd", "frac12"));
   entityTable.push_back(EntityPair("\xbe", "frac34"));
   entityTable.push_back(EntityPair("\xbf", "iquest"));

   entityTable.push_back(EntityPair("\xd7", "times"));
   entityTable.push_back(EntityPair("\xf7", "divide"));

   //ASCII punct chars 
   entityTable.push_back(EntityPair("\x26", "amp"));     // --=ampersand--  
   entityTable.push_back(EntityPair("\x3e", "gt"));      // --=greater-than sign R:--  
   entityTable.push_back(EntityPair("\x3c", "lt"));      // --=less-than sign R:--  
   entityTable.push_back(EntityPair("\xA9", "copy"));    //Copyright(c)
   entityTable.push_back(EntityPair("\xAE", "reg"));     //Registered (R)
   entityTable.push_back(EntityPair("\x99", "trade"));   //TradeMark (TM)
   entityTable.push_back(EntityPair("\\", "slash"));     //Backslash
   entityTable.push_back(EntityPair("{", "obrace"));     //Opening brace
   entityTable.push_back(EntityPair("}", "cbrace"));     //Closing brace
   entityTable.push_back(EntityPair("'", "quot"));       //quote
   entityTable.push_back(EntityPair("\"", "ldquo"));     //left double quote
   entityTable.push_back(EntityPair("\"", "rdquo"));     //right double quote
   entityTable.push_back(EntityPair("\'", "lsquo"));     //left single quote
   entityTable.push_back(EntityPair("\'", "rsquo"));     //right single quote


   // start  fix bug 1583
   // Some html entities are appearing in the form &#82xx in HTML4.0 documents
   // They are not handled by the forward filter. 
   // The following mapping my give rise to other bugs.
   entityTable.push_back(EntityPair("\'", "#8216"));     //left single quote
   entityTable.push_back(EntityPair("\'", "#8217"));     //right single quote
   entityTable.push_back(EntityPair("\"", "#8220"));     //left double quote
   entityTable.push_back(EntityPair("\"", "#8221"));     //right double quote
   entityTable.push_back(EntityPair("\x96", "#8212"));   //emdash
   entityTable.push_back(EntityPair("\x97", "#8211"));   //endash
   //end fix bug 1583

   //End of Table

   for (entityTableIt = entityTable.begin(), entityTableEnd = entityTable.end();
        entityTableIt != entityTableEnd; entityTableIt++)
   {
      m_CharToEntityMap.insert(CharToEntity::value_type(*(entityTableIt->first), 
                                                        LgsString(entityTableIt->second)));
      m_EntityToCharMap.insert(EntityToChar::value_type(LgsString(entityTableIt->second),
                                                        *(entityTableIt->first)));
   }

   entityTable.clear();
}
