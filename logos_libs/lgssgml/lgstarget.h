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
//////////////////////////////////////////////////////////////////////
//
// Class Declaration - Used for buffering the output i.e Merged file.
// Purpose - Will be useful to format the output in memory before 
// writing it to file.
// LgsTarget.h: interface for the CLgsTarget class.
//
//////////////////////////////////////////////////////////////////////

#ifndef __lgstarget_h__
#define __lgstarget_h__

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

class CLgsTarget  
{
public:
	CLgsTarget& operator<<(const char* buf);
	CLgsTarget(const LgsString& filename, bool text = false);
	virtual ~CLgsTarget();

   CLgsTarget& operator<<(char ch);
	CLgsTarget& operator<<(int num);
	CLgsTarget& operator<<(const LgsString& str);
	void ClearAdjustFlag();
	void SetAdjustFlag();
	void ClearFlush();
	void SetFlush();
	void write(const char* buf, int buflength);

private:
	bool m_bText;
	ofstream *m_fsOutfile;
	bool m_bFlush;
	LgsString m_strBuffer;
	bool m_bAdjustLastSpace;
};

//-------------------------------------------------------------------
inline void CLgsTarget::ClearAdjustFlag()
{
   m_bAdjustLastSpace = false;
}
//-------------------------------------------------------------------
inline void CLgsTarget::SetAdjustFlag()
{
   m_bAdjustLastSpace = true;
}
//-------------------------------------------------------------------
inline void CLgsTarget::ClearFlush()
{
   m_bFlush = false;
}
//-------------------------------------------------------------------
inline void CLgsTarget::SetFlush()
{
   m_bFlush = true;
}

#endif
