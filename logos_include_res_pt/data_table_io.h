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

#define NOT_USED -1		// indicate table/file is not used.
						// set the records field to NOT_USED


						// structure to hold definition of a table in memory
						// read in from an external file. For example a rul or vtr file.
 struct data_table_info {
	char *buf;			// address of buffer holding records
	int	 buf_size;		// number of bytes in buf
	int  record_size;	// number of bytes in a record
	int  records;		// count of records in buffer
	int	 requested_record;	// record requested out of table
	char *filename_env; // environment variable holding filename 
	char *filename;		// filename of file
	char bswap_read;	// flag indicate if bytes are to be swaped on reading (y = swap bytes i2)
	char exist_optional;// flag indicated if file is optional to read.
						// for example a mini 30 table may not be supplied
}; 

