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
#ifndef __SqlRow_Inc__
#define __SqlRow_Inc__
/*
file:
  sqlrow.h

description:
  declares an sqlrow

modification stack:
  $Log: sqlrow.h,v $
  Revision 1.3  2005/11/02 16:14:43  kiefer
  Changed the functionality of lexit to simply return a value because the
  pthread cleanup functions did not get called when pthread_exit was called
  directly (a problem with the dynamic libraries? It remains unclear).

  Removed all CR characters.

  Revision 1.2  2005/10/25 09:10:41  kiefer
  Added disclaimer comment in source files.
  Made ready for distribution.

  Revision 1.1  2005/08/09 08:56:52  kiefer


  Initial check in.

  04-26-96,dwa,creation date
*/

#include <logos_libs/sql/sqlcolumn.h>
//#include <rw/tpvector.h>

//class SqlRow: public RWTPtrVector< SqlColumn >
class SqlRow: public LgsVector(SqlColumn*)
{
public:
                      SqlRow();
  virtual            ~SqlRow();
  virtual void        dump();
};

#endif


