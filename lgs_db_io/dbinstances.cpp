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
#include <logos_libs/odbcsql/globalsqlconnection.h>
#include <logos_libs/sql/sqlexception.h>
#include <logos_libs/dbms2restran/transfercodebuilder.h>
#include <logos_libs/dbms2restran/constantcodebuilder.h>
#include <logos_libs/dbms2restran/constantpointerretriever.h>
#include <logos_libs/dbms2restran/targetretriever.h>
#include <logos_libs/dbms2restran/transferretriever.h>
#include <lgs_db_io/dbinstances.h>

GlobalDBInstances::GlobalDBInstances(void)
                  :_refCount(0),
                   _dbConnection(0)
{
}

short GlobalDBInstances::refCount(void) const
{
	return _refCount;
}

void GlobalDBInstances::incRefCount(void)
{
	_refCount++;
}

void GlobalDBInstances::decRefCount(void)
{
	_refCount--;
}

void GlobalDBInstances::initialize(void)
{
	_dbConnection = getSqlConnection();
	_xferCodeBuilder.initialize();
	_constCodeBuilder.initialize();
	_xferRetriever.initialize();
	_constPtrRetriever.initialize();
	_srbStaticData.initialize();
	_targetRetriever.initialize();
}

void GlobalDBInstances::setThreadLocals(void)
{
	_xferCodeBuilder.setValue(new CTransferCodeBuilder(_dbConnection));
	_constCodeBuilder.setValue(new CConstantCodeBuilder(_dbConnection));
	_xferRetriever.setValue(new CTransferRetriever(_dbConnection));
	_constPtrRetriever.setValue(new CConstantPointerRetriever(_dbConnection));
	LSPStaticData * srbStatic = new LSPStaticData;
	srbStatic->p_srb = new CSemtabRuleBuilder(_dbConnection);
	srbStatic->numItemsFetched = 0;
	srbStatic->numOfItemsUsed = 0;
	_srbStaticData.setValue(srbStatic);
	_targetRetriever.setValue(new CTargetRetriever(_dbConnection));
}

GlobalDBInstances::~GlobalDBInstances(void)
{
	freeSqlConnection(_dbConnection);
	_xferCodeBuilder.cleanup();
	_constCodeBuilder.cleanup();
	_xferRetriever.cleanup();
	_constPtrRetriever.cleanup();
	_srbStaticData.cleanup();
	_targetRetriever.cleanup();
}

void GlobalDBInstances::freeThreadLocals(void)
{
	CTransferCodeBuilder * tcb = _xferCodeBuilder.getValue();
	if (tcb)
	{
		delete tcb;	
	}
	CConstantCodeBuilder * ccb = _constCodeBuilder.getValue();
	if (ccb)
	{
		delete ccb;
	}
	CTransferRetriever *tr = _xferRetriever.getValue();
	if (tr)
	{
		delete tr;
	}
	CConstantPointerRetriever * cpr = _constPtrRetriever.getValue();
	if (cpr)
	{
		delete cpr;
	}

	LSPStaticData *staticData = _srbStaticData.getValue();
	if (staticData)
	{
		CSemtabRuleBuilder * srb = staticData->p_srb;
		if (srb)
		{
			delete srb;
		}
		delete staticData;
	}

	CTargetRetriever * targetR = _targetRetriever.getValue();
	if (targetR)
	{
		delete targetR;
	}
}

CTransferCodeBuilder & GlobalDBInstances::xferCodeBuilder(void)
{
   return *_xferCodeBuilder.getValue();
}

CConstantCodeBuilder & GlobalDBInstances::constCodeBuilder(void)
{
   return *_constCodeBuilder.getValue();
}

CTransferRetriever & GlobalDBInstances::xferRetriever(void)
{
   return *_xferRetriever.getValue();
}

CConstantPointerRetriever & GlobalDBInstances::constPtrRetriever(void)
{
   return *_constPtrRetriever.getValue();
}

LSPStaticData & GlobalDBInstances::srbStaticData(void)
{
   return *_srbStaticData.getValue();
}

CTargetRetriever  & GlobalDBInstances::targetRetriever(void)
{
   return *_targetRetriever.getValue();
}
