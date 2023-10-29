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
#include <lgs_db_io/lgsdbcommonobjects.h>

extern "C" {

int sourceLanguage()
{
   return LgsDBCommonObjects::GetJobControlArguments().SourceLanguage();
}

int targetLanguage()
{
   return LgsDBCommonObjects::GetJobControlArguments().TargetLanguage();
}
char *companyCodes(char *buf)
{
   sprintf(buf, "%s", LgsDBCommonObjects::GetJobControlArguments().CompanyCodes().c_str());
   return buf;
}
 
int extendedSearch()
{
   if (LgsDBCommonObjects::GetJobControlArguments().ExtendedSearch())
      return 1;
   else
      return 0;
}

int targetForm()
{
   return LgsDBCommonObjects::GetJobControlArguments().TargetForm();
}

int diagnosticStartLine()
{
   return LgsDBCommonObjects::GetJobControlArguments().DiagnosticStartLine();
}

int diagnosticEndLine()
{
   return LgsDBCommonObjects::GetJobControlArguments().DiagnosticEndLine();
}

int diagnosticLevel()
{
   return LgsDBCommonObjects::GetJobControlArguments().DiagnosticLevel();
}

int jobType()
{
   return LgsDBCommonObjects::GetJobControlArguments().JobType();
}

int wordSearchOptions()
{
   return LgsDBCommonObjects::GetJobControlArguments().WordSearchOptions();
}

char *miniRes2(char *buf)
{
   sprintf(buf, "%s", LgsDBCommonObjects::GetJobControlArguments().MiniRes2().c_str());
   return buf;
}

char *miniTran1(char *buf)
{
   sprintf(buf, "%s", LgsDBCommonObjects::GetJobControlArguments().MiniTran1().c_str());
   return buf;
}

char *miniTran2(char *buf)
{
   sprintf(buf, "%s", LgsDBCommonObjects::GetJobControlArguments().MiniTran2().c_str());
   return buf;
}

char *miniTran3(char *buf)
{
   sprintf(buf, "%s", LgsDBCommonObjects::GetJobControlArguments().MiniTran3().c_str());
   return buf;
}

char *miniTran4(char *buf)
{
   sprintf(buf, "%s", LgsDBCommonObjects::GetJobControlArguments().MiniTran4().c_str());
   return buf;
}

char *miniParse1(char *buf)
{
   sprintf(buf, "%s", LgsDBCommonObjects::GetJobControlArguments().MiniParse1().c_str());
   return buf;
}

char *miniParse2(char *buf)
{
   sprintf(buf, "%s", LgsDBCommonObjects::GetJobControlArguments().MiniParse2().c_str());
   return buf;
}

char *miniParse3(char *buf)
{
   sprintf(buf, "%s", LgsDBCommonObjects::GetJobControlArguments().MiniParse3().c_str());
   return buf;
}

char *miniParse4(char *buf)
{
   sprintf(buf, "%s", LgsDBCommonObjects::GetJobControlArguments().MiniParse4().c_str());
   return buf;
}

char *inputFile(char *buf)
{
   sprintf(buf, "%s", LgsDBCommonObjects::GetJobControlArguments().InputFile().c_str());
   return buf;
}

int jobID()
{
   return LgsDBCommonObjects::GetJobControlArguments().JobID();
}

} // extern "C"
