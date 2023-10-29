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
#ifndef _COMPARAMETERS_H_
#define _COMPARAMETERS_H_

// Language definitions
const short GERMAN = 1;
const short ENGLISH = 2;
const short FRENCH = 3;
const short SPANISH = 4;
const short ITALIAN = 5;
const short PORTUGUESE = 6;
const short INVALID_LANGUAGE = 7;

// File Formats
const short RTF = 1;
const short SGML = 2;
const short HTML = 3;
const short XML = 4;
const short INTERLEAF_ASCII = 5;
const short FRAME_MIF = 6;
const short LGS_TXT_EXCHANGE = 7;
const short MS_WORD8 = 8;
const short RTF_WINHELP = 9;
const short TMX = 10;

// Flag values
const unsigned char FLAG_ON = 1;
const unsigned char FLAG_OFF = 0;

// Diag values
const unsigned char NO_DIAG = 0;
const unsigned char SHORT_DIAG = 2;
const unsigned char LONG_DIAG = 3;
const unsigned char DEEP_DIAG = 4;
const unsigned char STAT_DIAG = 5;

// Word Search option values
const unsigned short UNFOUND = 1;
const unsigned short FOUND_NOUN = 2;
const unsigned short FOUND_VERB = 4;
const unsigned short FOUND_ADJ = 8;
const unsigned short FOUND_ADV = 16;

// Lookup parameter value
const unsigned short FLAG_UNFOUND = 0x0100;

#endif

