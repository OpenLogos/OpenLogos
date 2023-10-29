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
/* -*- Mode: C++ -*- */

/***************************************************************************
 PORTABLE ROUTINES FOR WRITING PRIVATE PROFILE STRINGS --  by Joseph J. Graf
 Header file containing prototypes and compile-time configuration.
***************************************************************************/

#define MAX_LINE_LENGTH    512

/***** Routines to read profile strings --  by Joseph J. Graf ******/
#include <cstdio>
#include <cstring>
#include <cstdlib>
#include <cctype>
#include "unistd.h"

/*****************************************************************
* Function:     read_line()
* Arguments:    <FILE *> fp - a pointer to the file to be read from
*               <char *> bp - a pointer to the copy buffer
* Returns:      TRUE if successful FALSE otherwise
******************************************************************/
int read_line(FILE *fp, char *bp) {
  char c = '\0';
  int i = 0;
  /* Read one line from the source file */
  while( (c = getc(fp)) != '\n' ) {
    if( c == EOF )         /* return FALSE on unexpected EOF */
      return(0);
    if (c != '\r')  /* Again, MS sucks, ignore CR */
      bp[i++] = c;
  }
  bp[i] = '\0';
  return(1);
}


/************************************************************************
* Function:     get_private_profile_int()
* Arguments:    <char *> section - the name of the section to search for
*               <char *> entry - the name of the entry to find the value of
*               <int> def - the default value in the event of a failed read
*               <char *> file_name - the name of the .ini file to read from
* Returns:      the value located at entry
*************************************************************************/
int get_private_profile_int(const char *section,
    const char *entry, int def, const char *file_name)
{   FILE *fp = fopen(file_name,"r");
    char buff[MAX_LINE_LENGTH];
    char *ep;
    char t_section[MAX_LINE_LENGTH];
    char value[6];
    int len = strlen(entry);
    int i;
    if( !fp ) return(0);
    sprintf(t_section,"[%s]",section); /* Format the section name */
    /*  Move through file 1 line at a time until a section is matched or EOF */
    do
    {   if( !read_line(fp,buff) )
        {   fclose(fp);
            return(def);
        }
    } while( strcmp(buff,t_section) );
    /* Now that the section has been found, find the entry.
     * Stop searching upon leaving the section's area. */
    do
    {   if( !read_line(fp,buff) || buff[0] == '[' )
        {   fclose(fp);
            return(def);
        }
    }  while( strncmp(buff,entry,len) );
    ep = strrchr(buff,'=');    /* Parse out the equal sign */
    ep++;
    if( !strlen(ep) )          /* No setting? */
        return(def);
    /* Copy only numbers fail on characters */

    for(i = 0; isdigit(ep[i]); i++ )
        value[i] = ep[i];
    value[i] = '\0';
    fclose(fp);                /* Clean up and return the value */
    return(atoi(value));
}


/**************************************************************************
* Function:     get_private_profile_string()
* Arguments:    <char *> section - the name of the section to search for
*               <char *> entry - the name of the entry to find the value of
*               <char *> def - default string in the event of a failed read
*               <char *> buffer - a pointer to the buffer to copy into
*               <int> buffer_len - the max number of characters to copy
*               <char *> file_name - the name of the .ini file to read from
* Returns:      the number of characters copied into the supplied buffer
***************************************************************************/
int get_private_profile_string(const char *section, const char *entry,
                               const char *def,
                               char *buffer, int buffer_len, 
                               const char *file_name)
{
    FILE *fp = fopen(file_name,"r");
    char buff[MAX_LINE_LENGTH];
    char *ep;
    char t_section[MAX_LINE_LENGTH];
    // entry == NULL means copy whole section
    int len = (entry != NULL) ? strlen(entry) : 0;
    if( !fp ) return(0);
    sprintf(t_section,"[%s]",section);    /* Format the section name */
    /*  Move through file 1 line at a time until a section is matched or EOF */
    do
    {   if( !read_line(fp,buff) )
        {   fclose(fp);
            strncpy(buffer,def,buffer_len);
            return(strlen(buffer));
        }
    }
    while( strcmp(buff,t_section) );
    /* Now that the section has been found, find the entry.
     * Stop searching upon leaving the section's area. */
    do
    {   if( !read_line(fp,buff) || buff[0] == '[' )
        {   fclose(fp);
            strncpy(buffer,def,buffer_len);
            return(strlen(buffer));
        }
    }  while( (len == 0) || strncmp(buff,entry,len) );
    if (len > 0) {
      ep = strrchr(buff,'=');    /* Parse out the equal sign */
      ep++;
      /* Copy up to buffer_len chars to buffer */
      strncpy(buffer,ep,buffer_len - 1);
    }

    buffer[buffer_len - 1] = '\0';
    fclose(fp);               /* Clean up and return the amount copied */
    return(strlen(buffer));
}


/***** Routine for writing private profile strings --- by Joseph J. Graf *****/


/*************************************************************************
 * Function:    write_private_profile_string()
 * Arguments:   <char *> section - the name of the section to search for
 *              <char *> entry - the name of the entry to find the value of
 *              <char *> buffer - pointer to the buffer that holds the string
 *              <char *> file_name - the name of the .ini file to read from
 * Returns:     TRUE if successful, otherwise FALSE
 *************************************************************************/
int write_private_profile_string(const char *section, const char *entry,
                                 const char *buffer, const char *file_name)

{   FILE *rfp, *wfp;
    char tmp_name[15] = "PRIVPROFXXXXXX";
    char buff[MAX_LINE_LENGTH];
    char t_section[MAX_LINE_LENGTH];
    int len = strlen(entry);
    int tmpfd = mkstemp(tmp_name); /* Get a temporary file name to copy to */
    if (tmpfd == -1) return 0;
    sprintf(t_section,"[%s]",section);/* Format the section name */
    if( !(rfp = fopen(file_name,"r")) )  /* If the .ini file doesn't exist */
    {   if( !(wfp = fopen(file_name,"w")) ) /*  then make one */
        {   return(0);   }
        fprintf(wfp,"%s\n",t_section);
        fprintf(wfp,"%s=%s\n",entry,buffer);
        fclose(wfp);
        return(1);
    }
    if( !(wfp = fdopen(tmpfd,"w")) )
    {   fclose(rfp);
        return(0);
    }

    /* Move through the file one line at a time until a section is
     * matched or until EOF. Copy to temp file as it is read. */

    do
    {   if( !read_line(rfp,buff) )
        {   /* Failed to find section, so add one to the end */
            fprintf(wfp,"\n%s\n",t_section);
            fprintf(wfp,"%s=%s\n",entry,buffer);
            /* Clean up and rename */
            fclose(rfp);
            fclose(wfp);
            unlink(file_name);
            rename(tmp_name,file_name);
            return(1);
        }
        fprintf(wfp,"%s\n",buff);
    } while( strcmp(buff,t_section) );

    /* Now that the section has been found, find the entry. Stop searching
     * upon leaving the section's area. Copy the file as it is read
     * and create an entry if one is not found.  */
    while( 1 )
    {   if( !read_line(rfp,buff) )
        {   /* EOF without an entry so make one */
            fprintf(wfp,"%s=%s\n",entry,buffer);
            /* Clean up and rename */
            fclose(rfp);
            fclose(wfp);
            unlink(file_name);
            rename(tmp_name,file_name);
            return(1);

        }

        if( !strncmp(buff,entry,len) || buff[0] == '\0' )
            break;
        fprintf(wfp,"%s\n",buff);
    }

    if( buff[0] == '\0' )
    {   fprintf(wfp,"%s=%s\n",entry,buffer);
        do
        {
            fprintf(wfp,"%s\n",buff);
        } while( read_line(rfp,buff) );
    }
    else
    {   fprintf(wfp,"%s=%s\n",entry,buffer);
        while( read_line(rfp,buff) )
        {
             fprintf(wfp,"%s\n",buff);
        }
    }

    /* Clean up and rename */
    fclose(wfp);
    fclose(rfp);
    unlink(file_name);
    rename(tmp_name,file_name);
    return(1);
}

#undef MAX_LINE_LENGTH

#define GetPrivateProfileString get_private_profile_string
#define WritePrivateProfileString write_private_profile_string

