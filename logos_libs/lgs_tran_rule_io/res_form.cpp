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
// res/tran form tables stuff
//
// the idea was to allow res and trans to read ascii res/tran form data,
// and create bitmaps on the fly instead of using special generating
// fortran tools to create bitmaps offline
//
// Res form tables are loaded by function res_form_load(),
// see prototype below. The function reads the filenames from filesnames.ini file
//
// All three must be set (except when source language is german,
// LGS_RESCFS may be unset in this case), and all the files should exist,
// and contain valid resform data (in ASCII), otherwise the function fails
// Hardcoded in the source files is the width, which is 26 for resform
//
// Tran form tables are loaded by function tran_form_load(),
// see prototype below. The function reads the filenames from the filenames.ini file
//
// All three must be set, and all the files should exist,
// and contain valid formtran data (in ASCII), otherwise the function fails
// Hardcoded in the source files is the width, which is 45 for tran form data
//
#ifdef _MSC_VER
#include "stdafx.h"
#endif

#define _RES_RULE_IO_IMPL_
#include "res_rule_io.h"

#define _TRAN_RULE_IO_IMPL_
#include "tran_rule_io.h"

#include "util.h"
extern "C"
{
#include <configdatafileinterface/configdatainterfacemain.h>
}



// size of rsfrm array is always 80*width shorts
//  width should be 26 for RES form data, and 45 for TRAN form data
static bool formdata_from_file( int width, const char* file_name, short* rsfrm_buf )
{
	char buf[1024];

	if ( !file_name || !rsfrm_buf )
		return false;

	ifstream in( file_name, ios::in );
	if ( !in.is_open() )
		return false;

	ZeroMemory(rsfrm_buf, sizeof(short)*80*width);

	for ( int lineNo = 0; lineNo < 80 && !in.eof(); lineNo++ )
	{
		in.getline(buf, sizeof(buf));
		int numNo = 0;

		for ( char* p = buf; *p; p++ )
		{
			if ( '/' == *p || ' ' == *p )
				break;
			if ( ',' == *p )
				continue;
			*(rsfrm_buf + width*lineNo + numNo++) = atoi(p);
			while ( isdigit(*p) ) p++;
		}
	}

	in.close();
	return true;
}

static void pack_it( int width, short* in, char* out )
{
	// the following code has been copied as is from the old
	// $logos_dev/inhouse_tools/rule_dev_tools/formgen fortran project
	// with neccessary changes for the Intel platform (byte order issue)
	// (the project in fact has never been ported to NT)
	for ( int i = 0; i < 80; i++ )
	{
		short* t = in + width*i;
		for ( int j = 0; j < width; j++ )
		{
			if ( 0 == t[j] ) break;
			int temp4 = t[j] - 1;
			int temp4d = temp4 / 8;
			int temp4r = temp4 % 8;
			temp4 = 1 << (8 - temp4r - 1);
			unsigned char* temprc = (unsigned char*)&temp4r;
			unsigned char* temp4c = (unsigned char*)&temp4;
			temprc[3] = temp4c[0];
			int iptr = temp4d;
			temp4 = 0;
			temp4c[3] = out[i*13 + iptr];
			temp4d = temp4r;
			temp4r = temp4 | temp4d;
			out[i*13 + iptr] = temprc[3];
		}
	}
}

// The lengths of the arrays are not tested by the following function
// It is the caller's resposibility for the lengths to be
//
//     vn_formres:	2080 bytes (2*80*13 bytes)
//		rsfrm_cfs:	4160 bytes (80*width*sizeof(short) bytes)
//
// where width is 26
//
extern "C" int res_form_load( short* rsfrm_csf, char* vn_formres )
{
	// locations for temporary storage of verb/noun rsfrm data
	// before it is compressed to formres representation
	const int width = 26;
	short rsfrm_verb[80*width];
	short rsfrm_noun[80*width];

	// rsfrm_csf may be NULL (german source doesn't use one)
	// while vn_formres cannot
	if (!vn_formres)
		return 0;

	ZeroMemory(vn_formres, sizeof(char)*2*80*13);
   char fileName[MAX_FILEPATH_LEN];

	if (rsfrm_csf)
	{

      if (GetConfigData("resdata", "csf", fileName, MAX_FILEPATH_LEN) == CONFDATA_SUCCESS)
      {
         if (!formdata_from_file(width, fileName, rsfrm_csf))
         {
            return 0;
         }
      }
      else
      {
         return 0;
      }
	}

   if (GetConfigData("resdata", "noun", fileName, MAX_FILEPATH_LEN) == CONFDATA_SUCCESS)
   {
      if (!formdata_from_file(width, fileName, rsfrm_noun))
      {
         return 0;
      }
   }
   else
   {
      return 0;
   }

   if (GetConfigData("resdata", "verb", fileName, MAX_FILEPATH_LEN) == CONFDATA_SUCCESS)
   {
      if (!formdata_from_file(width, fileName, rsfrm_verb))
      {
         return 0;
      }
   }
   else
   {
      return 0;
   }

	// convert verb/noun rsfrm data to formres format
	// cfs rsfrm data will be used as is by the caller
	pack_it(width, rsfrm_noun, vn_formres);
	pack_it(width, rsfrm_verb, vn_formres + 80*13);
	return 1;
}

// The length of the array are never tested by the following function
// It is the caller's resposibility for the lengths to be
//
//     vno_formres:	3120 bytes (3*80*13 bytes)
//
extern "C" int tran_form_load( char* vno_formres )
{
	// locations for temporary storage of verb/noun/other frm data
	// before it is compressed to formtran representation
	const int width = 45;
	short rsfrm_verb[80*width];
	short rsfrm_noun[80*width];
	short rsfrm_other[80*width];

	if (!vno_formres)
		return 0;

	ZeroMemory(vno_formres, sizeof(char)*3*80*13);
   char fileName[MAX_FILEPATH_LEN];

   if (GetConfigData("sourcedata", "nounform", fileName, MAX_FILEPATH_LEN) == CONFDATA_SUCCESS)
   {
      if (!formdata_from_file(width, fileName, rsfrm_noun))
      {
         return 0;
      }
   }
   else
   {
      return 0;
   }
	
   if (GetConfigData("sourcedata", "verbform", fileName, MAX_FILEPATH_LEN) == CONFDATA_SUCCESS)
   {
      if (!formdata_from_file(width, fileName, rsfrm_verb))
      {
         return 0;
      }
   }
   else
	{
		return 0;
	}

   if (GetConfigData("sourcedata", "otherform", fileName, MAX_FILEPATH_LEN) == CONFDATA_SUCCESS)
   {
      if (!formdata_from_file(width, fileName, rsfrm_other))
      {
         return 0;
      }
   }
   else
   {
      return 0;
   }

	// convert tran form data to formtran format
	pack_it(width, rsfrm_verb, vno_formres);
	pack_it(width, rsfrm_noun, vno_formres + 80*13);
	pack_it(width, rsfrm_other, vno_formres + 2*80*13);
	return 1;
}


// The length of the output buffer is not tested by the following function
// It is the caller's resposibility for the length to be at least 520 shorts
extern "C" int tran_o3b_load( short* buf )
{
	int result = 0; // failure
	char line[2048];

	// dimentions of the o3b array
	const int colNo = 11;
	const int rowNo = 47;

	// #of extra shorts above colNo*rowNo
	const int extra = 3;

	// position of # of significant rows value in the array
	const int lenIx = colNo*rowNo + 3 - 1;

	if (!buf)
		return result;

	ZeroMemory(buf, (colNo * rowNo + extra) * sizeof(short));

   char fileName[MAX_FILEPATH_LEN];
   if (GetConfigData("sourcedata", "o3btab", fileName, MAX_FILEPATH_LEN) != CONFDATA_SUCCESS)
   {
      return result;
   }

   ifstream in(fileName, ios::in);
   if (!in.is_open())
      return result;

	// read the array
	// there should be exactly rowNo rows in the array
   int rows;
	for ( rows = 0; rows < rowNo && !in.eof(); )
	{
		in.getline(line, sizeof(line));

		if ( '#' == line[0] )
			continue; // comment line
		char* p = line;
		atoi(p); // discard row#, which is there only for readability
		for ( int j = 0; j < colNo; j++ )
		{
			// step over the number already read in, and read the next number
			while ( ' ' == *p || '\t' == *p ) p++;
			while ( isdigit(*p) ) p++;
			buf[rows*colNo + j] = atoi(p);
		}

		// row has been read in, advance the counter
		rows++;
	}

	if ( rowNo == rows )
	{
		// step over comment lines if any
		do in.getline(line, sizeof(line));
		while ( '#' == line[0] && !in.eof() );

		// read the #of significant lines in the array
		if ( !in.eof() )
		{
			buf[lenIx] = atoi(line);
			result = buf[lenIx] > 0; // success only if #of sig lines > 0
		}
	}
	// else failure: #of rows should be exactly rowNo

	in.close();
	return result;
}

