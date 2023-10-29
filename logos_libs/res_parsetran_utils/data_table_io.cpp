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
//   function to read write, or close the io data passed between
//   the RES program
//		arguments:
//				operation = open_input, open_output read, write, close_input, close_output
//
#include <iostream>
#include <fstream>
#include <stdio.h>
#include <errno.h>
#include <logos_include/lgsstring.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <memory>
#include <logos_include_res_pt/data_table_io.h>

#include <configdatafileinterface/configdatainterfacemain.h>


#include <unistd.h>
#define _swab swab

using namespace std;

////////////////////////////////////////////////////////////////
//   return the filename pointed to by the past environment
//   variable.
//
//  		return  
//					0 errors
//					number of records loaded in table

char *filename_from_env(char *envptr) {

  char *ptr,*fname;

  try
    {

      // get the name of the output file from env variable
      if( (fname = (char *)getenv(envptr)) == NULL )
        {  
          //	throw ("Unable to getenv for file.");
          return NULL;
        }					
#if (DIR_SEP_CHAR == '\\')
      // file names are comming in as unix with forward slash 
      for (ptr=(fname+strlen(fname)-1); ptr>fname; ptr--)
        {
          if (*ptr == '/') *ptr = '\\'; 
        }
#endif
      return (fname);

    }		//try
  catch (char *s)
    {
      cerr << " filename_from_env: " << s << *envptr  << endl;
      return (0);
    }
  catch (...)
    {
      return (0);
    }	

}		//function

////////////////////////////////////////////////////////////////
//   Read in a table from disk and load it into memory. The memory will
//   be allocated here and the pointer returned
//
//  		return  
//					0  errors
//					+n number of records loaded in table
//					-1 could not open file but file was optional

int data_table_build(struct data_table_info *datatable)
{

ifstream inp_stream;

try
{
	datatable->buf_size = 0;		//clear return size of buffer
	datatable->records = 0;			//clear return coutn of records loaded

						// get the name of the output file from env variable
	datatable->filename = filename_from_env(datatable->filename_env);
						// Open the file.
	inp_stream.open (datatable->filename,  ios::binary);    // open stream in binary mode
    if(!inp_stream.good()) 
	{
								// if the file is optional then set 
								// structure and just return
		if(datatable->exist_optional == 'y' )
		{
			datatable->exist_optional = NOT_USED;
			return(-1);
		}
		throw ("Unable to open file.");
	}

								// find out size of file in bytes
	struct stat buf;

	stat(datatable->filename, &buf);
								// allocate mem for all records in file 
	datatable->buf_size = buf.st_size;
	datatable->buf = new char[datatable->buf_size];
    if (datatable->buf_size == 0)
	{
	  throw("Unable to allocating memory. ");
	}
		
    inp_stream.read( datatable->buf, datatable->buf_size); // read in the records
	if( !inp_stream.good() )
	{
       throw ("Unable to read file.");
	}

	inp_stream.close();			// close file

	if ( datatable->bswap_read == 'y' )
	{
		_swab(datatable->buf,datatable->buf,datatable->buf_size);
	}
								// return count of records
	datatable->records = datatable->buf_size / datatable->record_size;

	return (datatable->records);

}		//try
catch (char *s)
{
   cerr << " data_table_build: " << s << datatable->filename << " errno message = " << strerror(errno) << endl;
   inp_stream.close();			// close file
   return (0);
}
catch (...)
{
	return (0);
}	

}		//function
////////////////////////////////////////////////////////////////
//   Init a file table structure. Done before a table is loaded.
//
//  		return  
//

void data_table_init(struct data_table_info *datatable)
{

			datatable->record_size = 0;
			datatable->records = 0;
			datatable->requested_record = 0;
			datatable->buf = 0;
			datatable->buf_size = 0;
			datatable->filename_env = "";
			datatable->filename = "";
			datatable->bswap_read = 'n';

}		//function

////////////////////////////////////////////////////////////////
/*   Read in a table from disk and load it into memory. The address of the memory
   will be passed to the function. The number of bytes to read will also be passed
   Function will return the number of bytes read.
   This is called from c and fortran programs.

  INPUT:
		arg1=	environment variable containing name of file.
		arg2=	address of buffer to hold read data.
		arg3=   number of bytes to skip in the file before transfer
				starts. Used to skip records in file. Most of the time
				this will be set to 0.
		arg4=	number of bytes to read. Calling routine must make
				sure buffer is large enough to hold data.

  		return  
					0 errors
					number of bytes read
*/
			
extern "C"
{
int data_load(char *lpszSecKey , char *lpszNameKey, char *buffer,
              int byte_skip, int buffer_size)
{
   ifstream inp_stream;
   char fileName[MAX_FILEPATH_LEN];

   GetConfigData(lpszSecKey, lpszNameKey, fileName, MAX_FILEPATH_LEN);

   try
   {
      // get the name of the output file from env variable
      // Open the file.
      inp_stream.open (fileName, ios::binary);    // open stream in binary mode
      if (!inp_stream.good()) 
      {
         throw("Unable to open file. ");
      }

      // find out size of file in bytes
      struct stat buf;
      stat(fileName, &buf);

      inp_stream.seekg(byte_skip);// skip past bytes. most cases this will be 0

      inp_stream.read(buffer, buffer_size); // read in the records
      if (!inp_stream.good())
      {
         throw("Unable to read read file.");
      }

      inp_stream.close();			// close file

      return (buffer_size);
   }		//try
   catch (char *s)
   {
      cerr << " data_load: " << s << fileName << "errno message = " << strerror(errno) << endl;
      return (0);
   }
   catch (...)
   {
      return (0);
   }	
}		//function
}		//extern c

////////////////////////////////////////////////////////////////
//	Get a record from a table that was built from an external file.
//
//   return
//			int   =  0 good
//				  = -1 record is not in table
//			arg 2 = record bytes copied to this address

int data_table_getrecord(struct data_table_info *datatable, char *ret_buffer)
{

int rec_offset;
try
{

	// make sure record requested is in the memory table
	if ( (rec_offset = ((datatable->requested_record-1) * datatable->record_size)) > datatable->buf_size)
	{
		throw ("Record requested is not valid.");
	}

	memmove (ret_buffer, (datatable->buf + rec_offset), datatable->record_size);

	return(0);


}		//try
catch (char *s)
{
/*
   cerr << " data_table_getrecord: " << s << endl
	    << " Record requested: " << datatable->requested_record 
		<< "  Total Records: " << datatable->records << endl
		<< " File Name: " << datatable->filename << endl;
*/
   return (-1);
}
catch (...)
{
	return (-2);
}	

}		//function
////////////////////////////////////////////////////////////////
//   Read in a table from disk and load it into memory. The memory will
//   be allocated here and the pointer returned
//   this is done for the 30 40 50 tables which have the index and
//   data all in one disk file.
//		input:
//				arg1 = firsts data structure to hold the tables
//				arg2 = second data structure to hold the indexes
//
//  		return  
//					0  errors
//					-1 could not open file but file was optional
//					+n number of records loaded in table

int data_table_build_304050(struct data_table_info *datatable1,
							struct data_table_info *datatable2)
{

ifstream inp_stream;
short index_record;

try
{
	datatable1->buf_size = 0;	  	    //clear return size of buffer
	datatable1->records = 0;			//clear return coutn of records loaded
	datatable2->buf_size = 0;	  	    //clear return size of buffer
	datatable2->records = 0;			//clear return coutn of records loaded

								// get the name of the output file from env variable
	datatable1->filename = filename_from_env(datatable1->filename_env);
//	datatable2->filename = datatable1->filename;
								// Open the file.
	inp_stream.open (datatable1->filename, ios::binary);    // open stream in binary mode
    if(!inp_stream.good()) 
	{
								// if the file is optional then set 
								// structure and just return
		if(datatable1->exist_optional == 'y' )
		{
			datatable1->exist_optional = NOT_USED;
			return(-1);
		}
		throw ("Unable to open file.");
	}
								// find out size of file in bytes
	struct stat buf;
	stat(datatable1->filename, &buf);

								// first 2 bytes indicate the starting record
								// of index
    inp_stream.read( (char *) &index_record, sizeof(index_record) ); // read in the records
	if( !inp_stream.good() )
	{
       throw ("Unable to read file.");
	}
								// swap the bytes if required
	if ( datatable1->bswap_read == 'y' )
	{
		_swab((char *) &index_record,(char *) &index_record, sizeof(index_record));
	}

					// skip first record which only had index pointer
	inp_stream.seekg( 0 );
	inp_stream.seekg( datatable1->record_size );

								// allocate mem for all table records in file 
	datatable1->buf_size = (index_record-2) * datatable1->record_size;
	datatable1->buf = new char[datatable1->buf_size];
    if (datatable1->buf_size == 0)
	{
	  throw("Unable to allocating memory. ");
	}
    inp_stream.read( datatable1->buf, datatable1->buf_size); // read in the records
	if( !inp_stream.good() )
	{
       throw ("Unable to read file.");
	}
								// swap the bytes if required
	if ( datatable1->bswap_read == 'y' )
	{
		_swab(datatable1->buf,datatable1->buf, datatable1->buf_size);
	}
								// allocate mem for all index records in file 
	datatable2->buf_size = buf.st_size - datatable2->record_size - datatable1->buf_size;
	datatable2->buf = new char[datatable2->buf_size];
    if (datatable2->buf_size == 0)
	{
	  throw("Unable to allocating memory. ");
	}
    inp_stream.read( datatable2->buf, datatable2->buf_size); // read in the records
	if( !inp_stream.good() )
	{
       throw ("Unable to read file.");
	}
								// swap the bytes if required
	if ( datatable1->bswap_read == 'y' )
	{
		_swab(datatable2->buf,datatable2->buf, datatable2->buf_size);
	}

	inp_stream.close();			// close file

								// return count of records
	datatable1->records = datatable1->buf_size / datatable1->record_size;
	datatable2->records = datatable2->buf_size / datatable2->record_size;

	return (datatable1->records);

}		//try
catch (char *s)
{
	cerr << " data_table_build_304050: " << s << datatable1->filename << " errno message = " << strerror(errno) << endl;
   inp_stream.close();			// close file
   return (0);
}
catch (...)
{
	return (0);
}	

}		//function
