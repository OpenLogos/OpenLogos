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

//#include <fcrt.h>

#ifndef JBEXTERN
#define JBEXTERN extern
#else
#undef JBEXTERN
#define JBEXTERN
#endif

#define MAX_ELEMENTS_RES 70
#define ELMMAX 100
#define One 1
#define byte char
/* typedef char* STRING; */

#ifndef LOGCC
#define LOGCC "LOG"
#endif

		// file opened for diagnostics
JBEXTERN  FILE * _spec_fp;



JBEXTERN  struct t_jbctrlX_ {
	short int jclgus, jcusus, jcellv, jcimpe, jcmnrs[2], jctmnp;
	int jcccls_count;
	char jcccls[10][3];
	int jcextendedsearch;
	}	jbctrlX_;

/*     /JCADD/  = Additional Jobctrl Parameters
 *
 *           Word search type flags: 1=on, 0=off
 *        WRDSRC = Any Word Search Flag
 *        UFWORD = Unfound Words
 *        UFNPHR = Unfound Noun Phrases
 *        FNDWRD = Found Word flag - Any Part of Speech
 *        FNDNOU = Found Nouns
 *        FNDADJ = Found Adjectives
 *        FNDADV = Found Adverbs
 *        FNDVRB = Found Verbs
 *           Count limits:
 *        UFWRMN = Min occurences of Unfound word before reporting
 *        UFWRMX = Max occurences of Unfound word to report
 *        UFNPMN = Min occurences of Unfound Noun Phrases before reporting
 *        UFNPMN = Max occurences of Unfound Noun Phrases to report
 *        FNDMIN = Min occurences of found words before reporting 
 *        FNDMAX = Max occurences of found words to report
 *
 *        WSOUTF = Output Form 
 *         (default) w = WYSIWYG  (original source inflection & final 
 *                                 translated output inflection )
 *                   c = Canonical (uninflected - dictionary form )
 *
 *        TAGGER = Tagging flag
 *        TAGSRC = Source tagger flag
 *
 *        TWOPSS = two pass flag strategy is active or inactive 
 *	      Y = yes , 2 pass is active
 *	      N = no,   2 pass is NOT active
 *
 *	 PRSTRN = two pass flag indicates whether to run as PARSE or TRAN step
 *             0 = two pass off, process as TRAN in old method
 *             1 = two pass on, process as PARSE, the 1st pass
 *             2 = two pass on, process as TRAN, the 2nd pass
 *
 *  	 LSTPRS = Last PARSE program to execute, if 2 pass is on
 *
 */
JBEXTERN  struct t_jcaddX_ {
	short int wrdsrc, ufword, ufwrmn, ufwrmx, ufnphr, ufnpmn, ufnpmx, 
	  fndwrd, fndmin, fndmax, fndnou, fndadj, fndvrb, fndadv, tagger, 
	  tagsrc;
	byte wsoutf, prstrn, twopss, lstprs;
	}	jcaddX_;

JBEXTERN  struct t_srcflgX_ {
	short int srcflg;
	}	srcflgX_;

JBEXTERN  struct t_trgflgX_ {
	short int trgflg;
	}	trgflgX_;


                                                                                
JBEXTERN  struct t_errvrsX_ {
	long int untcnt;
	char mod[9], pgm[9];
	long int linenm, err, logerr, errlvl;
	byte errdat[216];
	}	errvrsX_;
                                         
/*
 *             DIAGNOSTIC FLAGS                                                  
 *             0 = OFF, 1 = ON                                                   
 *
 *   SHRTDI -  SHORT DIAGNOSTICS FLAG                                            
 *   DEEPDI -  DEEP DIAGNOSTICS FLAG                                             
 *   LONGDI -  LONG DIAGNOSTICS FLAG                                             
 *   ANYDI -   indicates whether any of the above diagnostics are on,            
 *             i.e. anydi = shrtdi or deepdi or longdi                           
 *   STATS  -  STATISTICS FLAG                                             
 *
 */
JBEXTERN  struct t_diagsX_ {
	short int longdi, deepdi, shrtdi, anydi, stats;
	}	diagsX_;

JBEXTERN 	struct  {
		short int sw[30];
		short int savsw[30];
		int   diag_line_start;
		int   diag_line_end;
	}	opswX_;
/*
 *winnt
 *		common to hold the source sentence 
 *		for debug writes to unit 6
 */
JBEXTERN  struct t_source_sentX_ {
	long int sentid, sentlng;
	byte sentbuf[500];
	}	source_sentX_;
		//following replaces glX
		// holds word of the sentece. Used mostly for diags.
JBEXTERN struct {
	char source_word[ELMMAX][20];
	int  source_word_size[ELMMAX];
	int  source_word_count[ELMMAX];
}   sent_wordsX_;

JBEXTERN  struct t_bypasX_ {
	short int bypas;
	}	bypasX_;

/* trncom.h */
/*
 ****************************************************************************    
 *
 * This include file contains common declarations found throughout all 4 trans.  
 *
 ****************************************************************************    
 *
 *     Data Descriptions:                                                        
 *------------------                                                             
 *     VTRF -  Internal work array containing a copy of the current vtr.  Copied 
 *             from VTR  and loaded by VTRFWR.                                   
 *
 *   MINIP1 -  Linguistic mini flag, for passes 1 and 2, respectively.           
 *   MINIP2    If 2 pass strategy is inactive (passfl=0), then only              
 *             MINIP1 is significant.                                            
 *                0 = No mini files active for this tran                         
 *                1 = Minis are active                                           
 *             Set in TXOPEN().  If the mini data files can be opened and        
 *             read as filedefed then it is assumed the minis are active.        
 *             As of 7/91, this applies to both product and development          
 *             modes (distinction is made when filedefing at EXEC level).        
 *
 *   ORIGCT -  Number of elements for any sentence at the beginning of TRAN1     
 *             (including any EOS or BOS added). Set to ECOUNT in T1INIT and     
 *             never updated thereafter.                                         
 *
 *   ELEMCT -  total number of elements currently in the system, both            
 *             original and added.   Must be incremented for each new            
 *             full element added ( by t2clmove or txsw68).                      
 *
 *   PARSCT -  total number of original source elements = ORIGCT + 
 *	      any alements added in PARSE by a 68 switch.  All PARSCT
 * 	      elements will comprise the starting sentence for TRAN1.
 *	      Differences from ELEMCT- does not include: 
 *	        * new -67sw elements
 * 	        * new -68switch elements that could not be located in SWORKI.
 *
 *   SNTBRK  - Flag whether or not the sentence exceeded the maximum
 *             number of dictionary matches and was broken up by Gerdem.
 *
 *                 0 = Sentence is complete, not broken up.
 *                52 = Final piece of a broken sentence (i.e. the right
 *                     hand side), or the last sentence in a list.
 *
 *             The following numbers indicate that the sentence is the
 *             left hand side of a broken sentence and the character at
 *             which the break was made (i.e. the EOS element):
 *               186 = colon
 *               185 = semi-colon
 *               188 = comma
 *               189 = hyphen or space
 *
 *
 *
 *   PASSFL -  flag to indicate whether or not the new strategy for              
 *             INDEPENDENT SOURCE and TARGET ANALYSIS and GENERATION             
 *             is active for a particular tran. If active, then there are        
 *             2 passes per tran, otherwise only 1.                              
 *                  0 = off, only 1 pass per tran per sentence.                  
 *                  1 = on , 2 passes per tran per sentence.                     
 *
 *   PASSCT -  count of which pass through the current TRAN a sentence is in.    
 *
 *   PRSLST =  flag if this is the last parse program 
 *
 *   PASSKP -  indicates whether or not to do only one pass per tran and         
 *             which pass to execute.  PASSFL MUST BE ON (ie. = 1).              
 *                 = 0,  do both passes in the current tran                      
 *                 = 1,  do only pass1 in the current tran.                      
 *                 = 2,  do only pass2.                                          
 *             Set by 2 pass indicator value in control file.                    
 *             flow changes: 1) dont open and load unneeded data files.          
 *              2) txdriver flow depends upon which passes will execute.         
 *              3) txmopup and txwrite flow dependent,too.                       
 *   PSSNAM -  pass name:  PARSEx or TRANx where x = module number (1-4)
 *   TRANID -  TRAN stage identifier.  (1,2,3,or 4)                              
 *
 *   SENTCT -  sentence counter .                                                
 *
 *   TABLCT =  count of tables called per rule.  Used to support a
 *             limit that catches infinite loops 
 *   TMNPFL = Translation Memory Noun Phrase facility ON/OFF flag.
 *             0 = OFF.
 *             1 = ON.
 ***************************************************************************     
 *
 */
	/*COMMON translations*/
JBEXTERN  struct t_vtrfX_ {
	short int vtrf[460];
	}	vtrfX_;
JBEXTERN  struct {
	char vtrfm[460];
	}	vtrfmX_;
JBEXTERN  struct t_miniflX_ {
	short int minip1, minip2, minifl;
	}	miniflX_;
JBEXTERN  struct t_elemctX_ {
	short int origct, elemct, sntbrk, parsct;
	}	elemctX_;
JBEXTERN  struct t_passesX_ {
	short int passfl, passct, passkp;
	byte prslst;
	char pssnam[7];
	}	passesX_;
JBEXTERN  struct t_tranidX_ {
	long int tranid;
	}	tranidX_;
JBEXTERN  struct t_sentctX_ {
	short int sentct;
	}	sentctX_;
JBEXTERN  struct t_tablctX_ {
	short int tablct;
	}	tablctX_;
JBEXTERN  struct t_tmnphrX_ {
	short int tmnpfl;
	}	tmnphrX_;


/* trgcds.h */
/*
 *         TARGET CODES RECORD                                                   
 *  PLEASE UPDATE THE FOLLOWING DOCUMENTATION WHEN ADDING A FIELD.               
 *              COUNT  START                      PACKED PACKED                  
 * TYP  FIELD   BYTES  BYTE       DESCRIPTION     COUNT  START                   
 * I    TCWDAD     4    1  TARGET WORDS ADDRESS      4    1                      
 * I    TCOV2A     2    5  OVERFLOW 2A               1    5                      
 * I    TCOV2B     2    7  OVERFLOW 2B               1    6                      
 * I    TCOV3A     2    9  OVERFLOW 3A               1    7                      
 * I    TCOV3B     2   11  OVERFLOW 3B               1    8                      
 * I    TCPATM(5)  2   13  MAIN PAT(S)               1    9                      
 * I    TCPATA(5)  2   23  ALTERNATE PAT(S)          1   14                      
 * I    TCGENM     2   33  MAIN GENDER               1   19                      
 * I    TCGENA     2   35  ALTERNATE GENDER          1   20                      
 * I    TCFIL8(4)  2   37  FILLER                    1   21                      
 *
JBEXTERN  struct {
      int   TCWDAD;              // target words address
      short TCOV2A;      // overflow 2a
      short TCOV2B;      // overflow 2b
      short TCOV3A;      // overflow 3a
      short TCOV3B;      // overflow 3b
      short TCPATM[5];   // main pats
      short TCPATA[5];   // alternate pats
      short TCGENM;      // main gender
      short TCGENA;      // alternate gender
      short TCFIL8[4];   // filler not used?
	  int word_class;	//target wordclass need in tran1 for german source 
} TRGCDS;
 */


JBEXTERN  struct t_trgcdsX_ {
	long int tcwdad;
	short int tcov2a, tcov2b, tcov3a, tcov3b, tcpatm[5], tcpata[5], 
	          tcgenm, tcgena, tcfil8[4];
	short word_class;						// 5-19-98 added word class
	}	trgcdsX_;

JBEXTERN  struct t_hfreqX_ {
	short int hfdo[22];
	short word_class;						// 5-19-98 added word class
	}	hfreqX_;



JBEXTERN  struct {
        char cmpcod[MAX_ELEMENTS_RES][3][3];
        char smccod[MAX_ELEMENTS_RES][3][3];
        char gencod[MAX_ELEMENTS_RES][3][2];
} cmpsmcX_;


	// structure used to list the commons which should be written out. We write the entire common area
	// in block mode so all we need is the address and number of bytes. To write additional commons just 
	// add to the list in this structure.
struct fort_commons {
		char *address;
		int size;
}; 
