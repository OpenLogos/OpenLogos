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
// TranData.h: interface for the CTranData class.
//
//////////////////////////////////////////////////////////////////////

#ifndef __TranData_h__
#define __TranData_h__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
#include <configdatafileinterface/sourcedata.h>

class CTranData : public CSourceData
{
public:
   virtual bool GetData(LgsString& key, LgsString& dataBuf);
   CTranData(const LgsString& srcLang, const LgsString& trgLang, bool isTwoPass, CConfigDataFile* pConfigDataFile,
             const LgsString& tran1Main, const LgsString& tran2Main, const LgsString& tran3Main,
             const LgsString& tran4Main, const LgsString& parse1Main, const LgsString& parse2Main,
             const LgsString& parse3Main, const LgsString& parse4Main, const LgsString& tran1Mini,
             const LgsString& tran2Mini, const LgsString& tran3Mini, const LgsString& tran4Mini,
             const LgsString& parse1Mini, const LgsString& parse2Mini, const LgsString& parse3Mini,
             const LgsString& parse4Mini);
   virtual ~CTranData();

private:
   bool GetMainFiles(LgsString& key, LgsString& DataBuf);
   bool GetMiniFiles(LgsString& key, LgsString& DataBuf);
   LgsString m_strLangPairSecName;
   LgsString tran1MainFileName;
   LgsString tran2MainFileName;
   LgsString tran3MainFileName;
   LgsString tran4MainFileName;
   LgsString parse1MainFileName;
   LgsString parse2MainFileName;
   LgsString parse3MainFileName;
   LgsString parse4MainFileName;
   LgsString tran1MiniFileName;
   LgsString tran2MiniFileName;
   LgsString tran3MiniFileName;
   LgsString tran4MiniFileName;
   LgsString parse1MiniFileName;
   LgsString parse2MiniFileName;
   LgsString parse3MiniFileName;
   LgsString parse4MiniFileName;
};

#endif
