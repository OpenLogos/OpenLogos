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
#include <stdio.h> 
//#include <fcrt.h>
#define byte char
typedef char* STRING;


short streval(char *LgsString);
void diag_check(long);
void diag_get_targs(long,long,long,byte[],short[]);
void errlog(STRING,long,long,long);
int labort(long);
char* readjc(char *section, char *keyword, char *buf, int buflen);

int data_load();
char *filename_from_env(char *env_string);
int mem_compress(char *input_buffer, int input_size,
                 char *output_buffer, int output_buffer_size);
int mem_uncompress(char *input_buffer, int input_size,
                   char *output_buffer, int output_buffer_size);


FILE *env_open(STRING);
void ccompx(STRING,long,STRING,long,long,short*);
//void exit(int);
void flush(long);
void frmtst(short*,STRING,short*);
int jcloadc(STRING);
int lexit(int);
//void lmove(short*,long,short*,long,long);
#define lmove(t,toff,s,soff,len) ((void)memcpy(((char *)t+toff-1),((char *)s+soff-1),len))
void match2(short*,short [],long,short*);
void match4(short*,short*,long,short*);
void valhun(short*,short*,short);
void valths(short*,short*,short);
void zapit(short*,long,char);
void swap_byte(char *buffer, int buf_size);
void findla (char *LgsString, int *retval, int count);

void diag_write_shorts(char *header,
					   short values[], int num_of_values, int values_per_line,
					   int format_size,
					   char *footer);
void diag_write_sworko();
void diag_write_swork();
void diag_write_parsetran_outarrays();
void diag_write_rule_comment(char *comment);
void diag_write_rule_sp(short level, short sp[], short spov[]);
void diag_write_rule_vtr(short vtrs[]);
void diag_write_rule_tags(short tag_sets, short tags[][21]);
