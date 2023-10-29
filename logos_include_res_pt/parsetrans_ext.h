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
/*
 ********************************************************************            
 *
 *    PARAMETER DECLARATIONS - for constants which appear in more than           
 *
 ****************                                                                
 *
 *   OPADRX  -  The size of OPADR, SCONPO, HFDOPO arrays.                        
 *   CCMAX   -  is the maximum number of Company codes a user can now            
 *              specify to control SEMTAB searching plus 1 for the               
 *              possible inclusion of "***" to indicate extended search.         
 *              Used in T1SEM2/SEMSRC. - 3/22/89 JAL.                            
 *   ORGMAX _   maximum number of original sentence elements allowed             
 *              for a given  sentence.  - 8/28/89  JAL                           
 *   ELMMAX -   Maximum size of arrays which hold complete sentence              
 *              elements, i.e. not constants added during trans.                 
 *              Includes ORGMAX plus any elements added by TRAN processing when  
 *              executing -68 switch or moving a clause(-67switch).              
 * NOT USED   ADDMAX -   Maximum number of elements which may be added throughout         
 *              all 4 trans.                                                     
 *                   ADDMAX <= ELMMAX - ORGMAX .                                 
 *   AD1MAX -   Maximum number of elements which may be added in TRAN1 (via      
 *              68 switch).  Actually, AD1MAX SCON rows are reserved after the   
 *              original elements (ORIGCT) are loaded in TRAN1.  This temporary  
 *              restriction allows us to add elements in without having to shift 
 *              the whole SCON array. In TRANs 2,3,4 SWORK(4,n) is               
 *              an linking pointer from the swork to its SCON position.  Thus,   
 *              the swork array can be shifted without necessitating a SCON      
 *              shift.  In TRAN 1 the SWORK position is itself the pointer       
 *              to the scons; there is no link ptr to change.                    
 *              Note: the first constant or vc loaded in TRAN1 gets SCON at      
 *              ORIGCT + AD1MAX + 1.  The first swork created via -68 switch     
 *              gets SCON at ORIGCT + 1.                                         
 *
 *  HFCNLO
 *  HFCNHI -    Low and high values for the range of valid high frequency
 *              Constant addresses.
 *
 *  TBLOOP -    Max number of tables that can be called by any single
 *              rule in any of the 4 TRANS.  Set high enough to avoid 
 *              infinite loops.
 *
 *  VCADLO -    range of VC addresses in the OPADR. 
 *  VCADHI      Expressed as positive values. As addresses in the
 *              OPADRI and OPADRO they are negative values. 
 *
 *        SWITCH PARAMETERS
 *
 *  SW20NX -    add this value to the number of sworks in the send in order
 *              to flag "no execution of the semtab vtr" upon matching.
 *
 *
 ***********************************************************************         
 *
 */
		/* PARAMETER translations */
#define	AD1MAX	20
#define	ADDMAX	30
#define	CCMAX	6
#define	ELMMAX	100
#define	HFCNHI	(-121)
#define	HFCNLO	(-999)
#define	OPADRX	450
#define	ORGMAX	70
#define	SCONX	150
#define	SCONX1	20
#define	SCONY	450
#define	SW20NX	50
#define	TBLOOP	100
#define	VCADHI	121
#define	VCADLO	100/* tfbhparm.h */

#define MAX_ELEMENTS ELMMAX
/*
 * TXBHOL -  BLACK HOLE Declarations
 *               constant for left bracket of Black hole filler LgsString 
 */
#define	BFBHFL	20
#define	BFBHPO	19
#define	BFLFBR	868
#define	BFMAX	20
#define	BFMVLF	800
#define	BFMVRT	900
#define	BFRTBR	869
#define	BHMAX	10
#define	FPBHCT	15
#define	TXBHCT	100



 /*
 *
 *    /SWORKO/ = THE SWORK ARRAYS FOR THE NEXT TRAN.                             
 *       SWORKO -  CONCATENATED ELEMENTS (PHRASES) FOR THE NEXT TRAN             
 *
 *             CREATED BY the following routines:                                
 *                   MAIN(),  SW24(),  SW34(), ELEMLD, MOPUP                     
 *             LOADED (i.e.set) BY the following routines:                       
 *                   MAIN(),  SW24(),  SW34(), ELEMLD, MOPUP,                    
 *                   SW18(), SW25(), SW26(), SW34(), SW38(), SW43()              
 *
 *       PHRBGO -  LINKS EACH NSWORK ELEMENT TO ALL OPADRO (TRANSFER             
 *       PHRNDO    ELEMENTS) WHICH IT REPRESENTS.                                
 *       PHRHDO -  SCON POINTER FOR THE HEAD ELEMENT OF THE CORRESPONDING        
 *                 PHRASE IN SWORKO.                                             
 *       PHCTO  -  NUMBER OF ELEMENTS IN SWORKO.                                 
 *
 *   PHCTP1 -  saved value of PHCTO at the end of pass1.  This is                
 *             used in the new 2 pass strategy to ensure that pass 2             
 *             ends up with the same number of sworks.                           
 *
 */
	/*COMMON translations*/
EXTERN  struct t_sworkoX_ {
	short int sworko[ELMMAX][4], phrbgo[ELMMAX], phrndo[ELMMAX], phrhdo[ELMMAX], 
	  phcto;
	}	sworkoX_;
EXTERN  struct t_phctp1X_ {
	short int phctp1;
	}	phctp1X_;

// very much like SWORKO
EXTERN  struct t_sworkX_ {
	short int swork[ELMMAX][4], phrbeg[ELMMAX], phrend[ELMMAX], phrhed[ELMMAX], phct;
	}	sworkX_;
/*
 *
 *
 *  /SWKLNK/  =  Link between the output SWORKO array and the
 *               input sworks (SWORKI) that are concatenated with it.
 *
 *    SWKLNK(x) =  pointer to the last SWORKI element that is concatenated
 *                 into SWORKO(x).  The first SWORKI element concatenated
 *                 into SWORKO(x) is (SWKLNK(x-1) + 1).  It is assumed 
 *                 that all concatenated SWORKs are contiguous.  While this
 *                 is generally true, it is not always true ( V N Particle,
 *                 for example).
 *
 *     Note:  SWKLNK() is set every time PHCTO is incremented.
 *
 *
 */
EXTERN  struct t_swklnkX_ {
	short int swklnk[ELMMAX];
	}	swklnkX_;
	 


EXTERN struct t_spinX_ {
	short int nswork[ELMMAX][4], phrase[ELMMAX], phrlim;
	}	spinX_;

#define	SWK1SZ	15
EXTERN struct {
	short int swork1[ELMMAX][SWK1SZ], 
		      phct,
		      scont1[ELMMAX],
			  scont2[ELMMAX], 
			  scont3[ELMMAX],
			  swrk1i[ELMMAX][SWK1SZ],
			  ecnti;
} swork1X_;

/*
 *
 *     Data used in PARSE1-4 to maintain a representation of the SWORK array
 *     that will be input to TRAN1 and that can be selectively
 *     modified in PARSE.
 *          SWORKI  -  The source SWORKs input to PARSE1 that are
 *        	      selectively modified by
 *		      PARSE for input to TRAN1.
 *	   SC2SWI  -  link/indirect ptr from SCON to SWORKI
 *			Used to linke current SWORK to its original position.
 *
 *     SW3146 =  flags -31 046 switch immediately before a -46 switch; indicating
 *               that the -46 switch affects the input swork (SWORKI) not SWORK. 
 */
EXTERN  struct t_sworkiX_ {
	short int sworki[ELMMAX][4], sc2swi[SCONY];
	}	sworkiX_;
EXTERN  struct t_sw3146X_ {
	short int sw3146;
	}	sw3146X_;

EXTERN struct t_sav36sX_ {
	short int sav36s[11];
	}	sav36sX_;
EXTERN struct t_sgnX_ {
	short int gn[21];
	}	sgnX_;
EXTERN struct t_typtrX_ {
	short int typtr;
	}	typtrX_;
EXTERN struct t_srch46X_ {
	short int srch46;
	}	srch46X_;
EXTERN struct t_im81X_ {
	short int im81;
	}	im81X_;
EXTERN struct t_frmarrX_ {
	short int frmarr[2];
	}	frmarrX_;
EXTERN struct t_vtrnX_ {
	short int vtrn;
	}	vtrnX_;
EXTERN struct t_flag32X_ {
	short int prm32, ad32;
	}	flag32X_;
EXTERN struct t_ofl2X_ {
	short int ofl2i[ELMMAX];
	}	ofl2X_;
EXTERN struct t_case_X_ {
	short int semcas, semc14, case_;
	}	case_X_;
EXTERN struct t_xpatnfX_ {
	short int xpatfm[ELMMAX][3];
	}	xpatnfX_;

EXTERN struct t_cnX_ {
	short int cn[30], vtrdon, slot[30][160];
	}	cnX_;
EXTERN struct t_sw36cX_ {
	short int sw36no, sw36pa;
	}	sw36cX_;
EXTERN struct t_nptpsvX_ {
	short int nptpsv;
	}	nptpsvX_;

EXTERN struct t_subswX_ {
	short int subsw;
	}	subswX_;
EXTERN struct t_nwcsemX_ {
	short int nwcsem[10];
	}	nwcsemX_;
EXTERN struct t_swcsemX_ {
	short int wcsem[10][5];
	}	swcsemX_;
EXTERN struct t_prevbX_ {
	short int wc7, load;
	}	prevbX_;
EXTERN struct t_phr26X_ {
	short int phr26;
	}	phr26X_;
EXTERN struct t_backspX_ {
	short int bsdflt, sw41p1;
	}	backspX_;
EXTERN struct t_vtrptrX_ {
	long int vtrunt;
	short int nptp;
	}	vtrptrX_;
EXTERN struct t_spcompX_ {
	short int idensp, iden42, nptpm, nptp42;
	}	spcompX_;
EXTERN struct t_spcbX_ {
	short int k7x, lvl, oflax;
	}	spcbX_;
EXTERN struct t_spyxX_ {
	short int ovrfx[22];
	}	spyxX_;
EXTERN struct t_spxxX_ {
	short int spx[12];
	}	spxxX_;
EXTERN struct t_cbsp3X_ {
	short int k7save[10];
	}	cbsp3X_;
EXTERN struct t_strw10X_ {
	short int wc50sv[3], ch50sv[3], i3stsv;
	}	strw10X_;
EXTERN struct t_cbsp2X_ {
//	short int mwc50m, mlok50[3], mcng50[3], mc50el[3], i3x, lix;
	short int wc5xm, look5x[3], chng5x[3], wc5xel[3], i3x, lix;
//	short int wc50m, look50[3], chng50[3], wc50el[3], i3x, lix;
	} cbsp2X_;
EXTERN struct t_s11frmX_ {
	short int s11frm[20][2];
	}	s11frmX_;

					//  vtr records load in these buffers
struct vtrbuf {
	short vtr[460];
};
EXTERN struct vtrbuf vtrX_;		//  main rules vtr
EXTERN struct vtrbuf spzxX_;		//  mini rules vtr

EXTERN struct {
	short gernum[4][100];
} nounsX_;
EXTERN struct {
	int rmorst;
} rmorstX_;	

EXTERN struct t_scommsX_ {
	short int sav14, switch_, setck;
	}	scommsX_;
EXTERN struct t_scommkX_ {
	short int k3, k3n;
	}	scommkX_;
EXTERN struct t_semflgX_ {
	short int semtch, vtrinh;
	}	semflgX_;
							// sw29 data

EXTERN struct t_data29X_ {
	long int targ[ORGMAX][3];
	short int ofl4r[ORGMAX][3], ofl1r[ORGMAX][3], rswork[ORGMAX][9];
	}	data29X_;

						// semtab rule structure
struct  t_f_sp5_rule {
	short int level, spdummy, spkey[100], spdata[1024], sp5vtr[1024];
	char company_code[3];
	};
EXTERN struct t_f_sp5_rule sp5rulX_;
EXTERN struct t_f_sp5_rule save_sp5rulX_;

EXTERN struct t_f_sp5_rule *curr_sp5rule_ptr;
EXTERN struct t_f_sp5_rule *save_sp5rule_ptr;
EXTERN short int save_sp5rule_level;
EXTERN short int curr_sp5rule_level;
EXTERN char curr_sp5rule_company_code[3];
EXTERN char save_sp5rule_company_code[3];

EXTERN struct t_svtrfX_ {
	short int svtrf[100];
	}	svtrfX_;

EXTERN struct t_scommpX_ {
	short int prm1, pntrsv;
	}	scommpX_;

struct  {
	short int orig[22], i3sem, sem46[5][4];
	} semtrX_;
EXTERN struct t_semfrmX_ {
	short int semwrk[21][4];
	}	semfrmX_;


EXTERN struct  {
		short int i, wc42m, wc50m, iz4, swx, s11prt, wc50ms;
		}	vwarg2X_;

EXTERN struct t_spX_ {
	short int *sp;
}	spX_;
EXTERN struct t_spX_ *spX_ptr;

EXTERN struct t_vbdataX_ {
	short int vbrkpt[99], vbcell[100], vbkend, vbkcon, k3n, k3, k3p1;
	}	vbdataX_;

EXTERN struct t_prtscoX_ {
	short int sct, scterr;
	}	prtscoX_;




/* clsnfo.h */
/*
 *CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          
 *   Contains all data structures used for                                       
 *   storing information about clauses.                                          
 *
 *   PARAMETER                                                                   
 *     CLSMAX - THE MAXIMUM NUMBER OF CLAUSES THAT CAN BE HANDLED PER            
 *              SENTENCE. THIS TOTAL INCLUDES THE MAIN CLAUSE.                   
 *              ** NOTE **: For every extra clause allowed                       
 *              ELMMAX must be increased by 3 to account for the new             
 *              elements created when a clause is moved.  Arrays relating        
 *              to the new elements must also be increased.                      
 *     CLSCEL - NUMBER OF CELLS ASSIGNED UNIQUELY (WRITE ACCESS) TO A            
 *              CLAUSE.  Must be a contiguous block in VBCELL.                   
 *     CLSCL1 - the first byte in VBCELL which corresponds to the block          
 *              defined by clscel above, i.e. first byte of clause               
 *              specific VBCELL.  Used for moving.                               
 *     CLSPCL - size of the array used to save the parent cells for any          
 *              child clause.                                                    
 *
 *  COMMON DATA                                                                  
 *   /CLSNFO/ - PARSING INFO. FOR EACH NEW CLAUSE IN A SENTENC                   
 *     CLCRNT - Id number of clause currently being parsed.                      
 *     CLTOTL - TOTAL NUMBER OF CLAUSES CURRENTLY FOUND AND RELOCATED            
 *              PER SENTENCE. Incremented by -67001 when beginning of            
 *              a clause is recognized.                                          
 *     CLPRNT(CLSMAX)  -  THE IDENTIFYING NUMBER OF THE PARENT CLAUSE.           
 *                        REALLY A POINTER INTO /CLSNFO/ ARRAYS.                 
 *     CLMRKR(CLSMAX)  -  POINTS TO THE SCON OF THE ELEMENT WHICH HOLDS          
 *                        THE ORIGINAL LOCATION OF THIS CLAUSE.                  
 *     CLANSC(CLSMAX)  -  POINTS TO SCON OF CLAUSE'S ANTECEDENT.                 
 *     CLBGNS(CLSMAX)  -  POINTER TO FIRST INPUT SWORK OF EACH CLASUE.           
 *     CLNDNS(CLSMAX)  -  POINTER TO LAST  INPUT SWORK OF EACH CLAUSE.           
 *     CLRELP(CLSMAX)  -  POINTER TO RELATIVE PRONOUN OF A CLAUSE. set by        
 *            -67005-8x switch. Used in SEMTAB to determine which element        
 *            is related to the antecedent.                                      
 *     CLANSW(CLSMAX)  -  used to locate all elements and constants that         
 *     CLANBG(CLSMAX)     are concatenated with the the antecedent so that       
 *     CLANND(CLSMAX)     semtab work on the relative pronoun in a child         
 *                        clause can be applied to the antecedent.               
 *                        Set in t2/3/4DRIVER whenever parsing of a clause       
 *                        is completed.  If the antecedent is unconcatenated     
 *                        in the output swork (i.e. it is still a head), then    
 *                        clansw() = ptr to antecedent swork, else = 0.          
 *                        clanbg() and clannd() = the sconpo values              
 *                        of the 1st and last opadr address concatenated         
 *                        under the antecedent as head.                          
 *
 *     CLPCEL(CLSCEL,CLSMAX) -  saved cells from the parent clause.  Set by      
 *            switch 67006...                                                    
 *     CLCELL(CLSCEL,CLSMAX) -  saved cells for a clause.                        
 *              As of 8/16/91 CLCELL is unused and small. It is initialezed      
 *              in T2SW67 by -67001 and would be set in TxDRIVER().              
 *
 *  OUTPUT CLAUSE ARRAYS:                                                        
 *
 *     CLBGNO(CLSMAX)  -  POINTER TO FIRST  OUTPUT  SWORK OF EACH CLASUE.        
 *     CLNDNO(CLSMAX)  -  POINTER TO LAST   OUTPUT  SWORK OF EACH CLAUSE.        
 *
 *
 *
 * NOT PASSED FROM ONE TRAN TO THE NEXT:                                         
 *
 *   SW676R, SW676L =  search boundaries for switch 67006 right and left         
 *                     search, respectively.  points to the SWORK which          
 *                     stops the search. the swork is not searched.              
 *                     see txsw67 for details.  initialized in txdriver.         
 *
 *CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          
 *
 */
#define	CLSCEL	1
#define	CLSCL1	21
#define	CLSMAX	10
#define	CLSPCL	100

EXTERN  struct t_clsnfoX_ {
	short int clbgns[CLSMAX], clndns[CLSMAX], clprnt[CLSMAX], clmrkr[CLSMAX], 
	  clrelp[CLSMAX], clansc[CLSMAX], clansw[CLSMAX], clanbg[CLSMAX], 
	  clannd[CLSMAX], cltotl, clcrnt, clpcel[CLSMAX][CLSPCL], clcell[CLSMAX][CLSCEL];
	}	clsnfoX_;
/*
 *****          COMMON  /CLSNFO/                                                 
 *              NOTE:  CLBGNS,CLNDNS are first to facilitate writing out         
 *                     clndno,clbgno and reading in clbgns,clndns.               
 *
 *
 ******          COMMON /CLSOUT/ -  output clause information                    
 *
 *
 ******           COMMON /CLSNOP/   - clause info not passed to the next tran    
 *
*/
EXTERN  struct t_clsoutX_ {
	short int clbgno[CLSMAX], clndno[CLSMAX];
	}	clsoutX_;
EXTERN  struct t_clsnopX_ {
	short int cl676r[CLSMAX], cl676l[CLSMAX];
	}	clsnopX_;
/*CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          
 *   Contains all data structures used to reorder clauses when they are          
 *   moved.  Parent clauses must be inserted before nested, child clause         
 *   even though moved after child clauses.  Insures that dependent              
 *   nested clauses are parsed after parent.                                     
 *
 *   PARAMETER:                                                                  
 *     CLsMAX - see clsnfo member.                                               
 *
 *   COMMON:                                                                     
 *   /CLSORD/ - NEEDED TO KEEP MOVED CLAUSES PROPERLY ORDERED.                   
 *
 *         FOLLOWING USED TO ASSURE PROPER LEFT-RIGHT ORDERING OF NESTED         
 *         CLAUSES AFTER THEY HAVE BEEN MOVED.  THE IDEA: ALL PARENT             
 *         CLAUSES SHOULD BE LOCATED TO THE LEFT OF THEIR RESPECTIVE             
 *         CHILD CLAUSES EVEN THOUGH THEY HAVE BEEN MOVED AND APPENDED           
 *         LATER THAN THE CHILD CLAUSES.                                         
 *
 *     CLOPT1(CLOMAX) -  THE NSWORK POSITION FOR THE FIRST ELEMENT OF            
 *                       A LgsString BEFORE THAT LgsString was moved.                  
 *     CLOPT2(CLOMAX) -  THE current nswork position for the beginning           
 *                       of the moved clause.                                    
 *     CLOCNT         -  STACK COUNTER FOR ENTRIES IN CLRPT1/2.                  
 */
EXTERN  struct t_clsordX_ {
	short int clopt1[CLSMAX], clopt2[CLSMAX], clocnt;
	}	clsordX_;





/* clssco.h */
/*
 *CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          
 *   Contains clause related data structures which are associated with           
 *   scons.  Identifies elements as belonging to a particular clause             
 *   and relates parent elements to the child clause.                            
 *
 *  NOTE:   FOLLOWING IS ASSOCIATED WITH tHE SCONs and assumes that              
 *          the parameter ELMMAX has been declared and defined.                  
 *
 *
 *   COMMON /SCON/ .... ACHILD,HCHILD                                            
 *
 *      ACHILD(ELMMAX) -  FOR EACH ELEMENT IN THE SCONS, IF THE ELEMENT  S       
 *        is tHE ANTECEDENT OF A CLAUSE THEN CONTAINS THE ID NUMBER OF  H        
 *        the CHILD CLAUSE. IF ZERO THEN it is NOt an ANTECEDENT and             
 *        there is no child clause.                                              
 *      CMCHLD(ELMMAX) -  CHILD CLAUSE PTR FOR EACH ELEMENT WHICH IS             
 *           A PLACE HOLDER FOR A MOVED CLAUSE - CLAUSE MARKER. THE              
 *           POINTER value is the id of the child clause.                        
 *      CLSID(ELMMAX)  -  THE CLAUSE TO WHICH AN ELEMENT IN THE SCON             
 *           TABLE BELONGS.                                                      
 *CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          
 *
 ****        SCON ADD-ONS                                                        
 *
 */
EXTERN  struct t_clsconX_ {
	short int achild[ELMMAX], cmchld[ELMMAX], clsid[ELMMAX];
	}	clsconX_;


/* clsmov.h */
/*
 *CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          
 *   Contains all data structures used for                                       
 *    moving the clauses in TRAN2 as they are defined.                           
 *    used only in TRAN2                                                         
 *
 *   PARAMETER                                                                   
 *     CLMSTK - NUMBER OF IMBEDDED CLAUSES WHICH CAN BE MOVED AT ONE             
 *              TIME.                                                            
 ********************************                                                
 *
 *  COMMON /CLSMOV/ - INFO REGARDING A CLAUSE TO BE MOVED.                       
 *
 *     CLMCNT - number of clauses currently being moved (i.e. on CLM             
 *              stack). Only imbedded clauses stack up past one.                 
 *                        ** NOTE **                                             
 *              PARSE IS IN A "CLAUSE MOVE STATE" IF:                            
 *                  (CLMCNT .GT. 0) .OR.  (XIST67 .EQ. 1)                        
 *                        **********                                             
 *
 *     CLMWTF(3,CLMSTK) - WORD CLASS , TYPE , AND FORM RESPECTIVELY              
 *            FOR THE ELEMENT WHICH REPLACES THE CHILD CLAUSE IN PARENT.         
 *     CLMBOS(3,CLMSTK) - WC,TYPE,FORM OF NEW BOS ELEMENT FOR CLAUSE.            
 *     CLMPOS(CLMSTK) -   POSITION OF PARSE BEFORE ENTERING CLAUSE MOVE          
 *                        STATE.  SHOULD BE AN UNLOADED SWORK.                   
 *     CLMID(clmstk)  -   clause id assigned when clause beg first               
 *                        defined (-67001).  If assigned later the paren         
 *                        clause becomes difficult to identify.                  
 *     CLMORD(clmstk) -   id of this clause's first child clause.  Used by t2clmo
 *                        to maintain left to right clause ordering even when a c
 *                        child clause is moved before the parent.               
 *     CLMSCB(CLMSTK) -   saved scon pointer to the child clause BOS.            
 *     CLMSCE(CLMSTK) -   saved scon pointer to the child clause EOS.            
 *     CLMOPC(CLMSTK) -   saved opadri pointer to address of new clause          
 *                        marker element. BOS, EOS address pointers              
 *                        are +1 and +2, respectively.                           
 *     CLMELC(CLMSTK) -   saved scono pointer for the clause marker              
 *                        element, i.e. saved ELEMCT value.  BOS, EOS            
 *                        pointers are +1 and +2,respectively.                   
 *
 *     XIST67         -   Flags the existence of a -67001... switch in           
 *                        the current VTR. (0 = no, 1 = yes).  Set in            
 *                        VTRFWR.  -41sw backspacing is decided in VTRFW         
 *                        before VTR executed. Since CLMCNT may  = 0,            
 *                        even with -67001 switch in VTR, must precheck          
 *                        for -67001 to allow backspacing over stretches         
 *
 *
 *CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          
 *
 */
#define	CLMSTK	5

EXTERN  struct t_clsmovX_ {
	short int clmwtf[CLMSTK][3], clmbos[CLMSTK][3], clmpos[CLMSTK], 
	  clmid[CLMSTK], clmord[CLMSTK], clmscb[CLMSTK], clmsce[CLMSTK], 
	  clmelc[CLMSTK], clmopc[CLMSTK], clmcnt;
	}	clsmovX_;






EXTERN  struct {
	short hashcd[MAX_ELEMENTS][2];
	short root_hashcd[MAX_ELEMENTS][2];
} hashX_;
EXTERN  struct {
	short henum2[MAX_ELEMENTS][2];
	short root_henum2[MAX_ELEMENTS][2];
} hensavX_;

/*
 *        SW68CT = # OF SWORKS CREATED via a 68 switch per rule                  
 *
 */
EXTERN  struct t_swtc68X_ {
	short int sw68ct;
	}	swtc68X_;


/*
 *     TRANSLATION MEMORY FOR NOUNPRHASES :   COMMONS
 *
 *       TRANS/FPRINT - internal work data, passed to FPRINT.
 *
 *  PARAMETER  NPMAX   =  Max number of noun phrases handled per sentence
 *
 *
 *
 *  INTEGER  NPCNT  =  Count of noun phrases identified in current sentence.
 *                     Corresponds to y index below.
 *
 *  INTEGER  NPCNTX(x) =  Count of NP identified by end of TRAN x.
 *                     Used for parent-child processing. NP's identified
 *                     at the end of TRAN1, can be a part of an NP
 *                     at end of TRAN2 and thus a child to the TRAN2 parent.
 *                       ex:  NP1 PREP NP2 (TRAN1) = NP3 (TRAN2)
 *
 *  INTEGER  NPHDSC(y)  =  SCON ptr of the linguistic head of the yth NP.
 *                      =  ptr to SCON row for head, not SCON(10,head_row). 
 *              **** if = 0, then this NP has been voided due to
 *                           an error, just skip it from then on.
 *                           Set =0, by NPOPAD of OPXREF() is bad.
 *  INTEGER  NPTYPE(y)  =  Type of NP (see NPTYPE in /NPTNFO/ below).
 *                         NP type. Set at end of TRAN2. 
 *                  1= simple NP (e.g. NN)
 *                  2= complex NP (e.g. NP PREP NP )
 *                  3= Any NP concatenated in TRAN2 or TRAN3 from more than
 *                     one SWORK in the previous TRAN, that is not of type 3.
 *                  4= NP made up of a phrasal dict entry. Fails all other
 *                     inclusionary criteria.  Identified after TRAN1.
 *
 *  INTEGER NPPRNT(y) = x, where x is a ptr to the parent of the yth NP.
 *             0 = no parent exists
 *             x = parent's position in the TRTMNP arrays (ex. NPHDSC(x)
 *                 pertains to the parent).
 *             NP from TRAN1, can be concatenated into a larger NP by the
 *             end of TRAN2. NPPRNT() is a method for linking the 2.  
 *             This TREE is never more than 2 levels deep: TRAN1 NPs as
 *             child and TRAN2 NPs as parent.
 *
 *      ========     NP  list    arrays   =============
 *
 *   PARAMETER   NPLSMX  =  size of list arrays (n index below)
 *
 *      NPLSBG(n)  =  pointer to head of nth noun phrase list.
 *      NPLSND(n)  =  pointer to end of nth noun phrase list.
 *      NPLSSZ     =  total number of entries that have been added to
 *                    all lists in the list array.  Next entry added to
 *                    a list is added at NPLSSZ+1.
 *      NPLSNX(n)  =  ptr to next entry for a list entry.
 *      NPLSLK(n)  =  Flag to indicate if the entry is locked in this
 *                    Noun phrase.
 *      NPLSSC(n)  =  SCON ptr for this entry.
 *
 *      **********   IMPORTANT     ****************  
 *              When changing the sizeof /TRTMNP/ make sure the
 *              new size is consistent with RECL declaration in
 *              the tranx.sh shell.     
 *      **********   IMPORTANT     ****************  
 */
/*
 *
 *    NP INFO  ARRAYS  used for tran internal processing
 *
 */
#define	NPLSMX	(NPMAX*8)
#define	NPMAX	15

EXTERN  struct t_trtmnpX_ {
	short int npcnt, npcntx[2], nphdsc[NPMAX], nptype[NPMAX], npprnt[NPMAX], 
	  nplsbg[NPMAX], nplsnd[NPMAX], nplssz, nplsnx[NPLSMX], nplslk[NPLSMX], 
	  nplssc[NPLSMX];
	}	trtmnpX_;
	 

/* trvc.h */
/*
 ***************************                                                     
 *     Contains all COMMON  declarations that are functionally                   
 *     related to the internal structure and processing of the VC's.             
 *     Parameters applying only to structures in this file are also              
 *     included; otherwise they appear in TRPARM.                                
 *         HFDOPO, HFDOPI -  Contain VC elements if only one element in          
 *                           a VC.  1-1 correspondence with OPADRX for           
 *                           indexing.                                           
 *         HFPOAD         -  Overflow VC elements.  Used when more than          
 *                           VC element and HFDOPO/I is the pointer.             
 *         SCONHF         -  SCON position for each element in HFPOAD.           
 *
 *    Changes:
 *     11/24/93 AVK Changed HFPOLO from 65 to 71 to avoid
 *                  conflict with element numbers 65 through 70.
 *
 ***************************                                                     
 ****          OPADRX = 450Parameterized in TRPARM include member.               
 */
		/* PARAMETER translations */
#define	HFPADX	50
#define	HFPADY	10
#define	HFPOHI	(HFPOLO + HFPADY - 1)
#define	HFPOL1	(HFPOLO - 1)
#define	HFPOLO	71
#define	SIZ	(HFPADX*2)
#define	VTRDIM	460
		/* end of PARAMETER translations */

EXTERN struct t_ctltblX_ {
	short int cursw, tblvln, tblvtr[VTRDIM];
	}	ctltblX_;

	/*COMMON translations*/
EXTERN  struct t_hfdoaX_ {
	short int hfpoad[HFPADY][HFPADX], sconhf[HFPADY][HFPADX], adct;
	}	hfdoaX_;
EXTERN  struct t_hfdoaiX_ {
	short int hfpoai[HFPADY][HFPADX], sconhi[HFPADY][HFPADX], adcti;
	}	hfdoaiX_;
EXTERN  struct t_hpdopiX_ {
	short int hfdopi[450];
	}	hpdopiX_;
EXTERN  struct t_hpdopoX_ {
	short int hfdopo[450];
	}	hpdopoX_;
/*
 *
 *  OPADRO =  OUTPUT TRANSFER ELEMENTS FOR THE CURRENT TRAN                      
 *  OPO    =  NUMBER OF ELEMENTS IN OPADRO                                       
 *  SCONPO =  SCON POINTER FOR EACH TRANSFER ELEMENT IN OPADRO.                  
 *
 */
EXTERN  struct t_opadroX_ {
	short int opadro[OPADRX], opo, sconpo[OPADRX];
	char company_code[OPADRX][3];
	}	opadroX_;

/*
 *
 *  OPADRI =  INPUT TRANSFER ELEMENTS FOR THE CURRENT TRAN                       
 *  OPI    =  NUMBER OF ELEMENTS IN OPADRI                                       
 *  SCONPI =  SCON POINTER FOR EACH TRANSFER ELEMENT IN OPADRI.                  
 *
 */
EXTERN  struct t_opadriX_ {
	short int opadri[OPADRX], opi, sconpi[OPADRX];
	char company_code[OPADRX][3];
	}	opadriX_;

/* elmnfo.h */
/*
 *
 *   ELMNFO -  arrays related to complete sentence elements and which            
 *             have no better home.                                              
 *
 *
 *  FORMSV (i)  -  Ooriginal form value for ith element.                         
 *  PRSFRM (i)  -  Original form selectively modified by PARSE for passing 
 *                 to TRAN1.  Part of paralled data in PARSE. 
 *  JS (i)      -  Wwhich of 3 potential dictionary entries (POS) is currently   
 *                 being used for the ith element.                               
 *  TARG25 (i)  -  target info.  tcgenm from the target codes file.              
 *  TARGPN (i)  -  target info                                                   
 *
 */
EXTERN  struct t_formsaX_ {
	short int formsv[ELMMAX], prsfrm[ELMMAX];
	}	formsaX_;
EXTERN  struct t_prctX_ {
	short int js[ELMMAX];
	}	prctX_;
EXTERN struct t_prctjX_ {
	short int j;
	}	prctjX_;
EXTERN  struct t_targ25X_ {
	short int targ25[ELMMAX];
	}	targ25X_;
EXTERN  struct t_targpnX_ {
	short int targpn[ELMMAX];
	}	targpnX_;
/*
 *               constant for right bracket of Black hole filler LgsString 
 *               SCON where black hole count flag is stored in the TRANS.
 *                  change references if changes from SCONO to SCON 
 *                  position (see txbhset.f,t4mopup.f).
 *               SCON where black hole count flag is stored in FPRINT.
 *                  moved from TRBHCT to FPBHCT at end of TRAN4 in MOPUP.
 *                  change references if changes from SCON to SCONO position
 *                  (see fpbhprep.f,t4mopup.f).
 *
 *                 SCON(BFBHFL) = indicates direction of search if a 
 *                    blind directional search, or the SCON of the 
 *                    desired Black Hole if a direct pointer.
 *                 SCON(BFBHPO) = If a directional search indicates the
 *                    nth black hole encountered in search.  If a direct
 *                    pointer, indicates the nth BH marker in a transfer.
 *                 NOTE: constants only have SCONX1 SCONS(dont get SCONO). 
 *
 *                Destination flags: 
 *                  SCON(BFBHSC) indicates the direction to search 
 *                  for the destination Black Hole if equal to one of
 *                  the following values.
 *                  SCON(BFBHCT) indicates the nth black hole
 *
 *                move BF to the nth Bhole to the right in the OPADR
 *                move BF to the nth Bhole to the left in the OPADR
 *          ***** IMPORTANT: The move flag values must be positive 
 *          *****       numbers, greater than SCONY (450, now). This
 *          *****       avoids conflicts in the VTR grammar and 
 *          *****       overlaps with direct scon pointer values 
 *          *****       (i.e. 1-SCONY). So must be > SCONY.
 *               Maximum number of filler and black holes per sentence
 */


EXTERN  struct t_negty2X_ {
	short int lstng2[20], numng2[20];
	}	negty2X_;
EXTERN  struct t_negtx2X_ {
	short int lstnx2[20], numnx2[20];
	}	negtx2X_;
	 

/* trscni.h */
/*
 *************************************************************                   
 *  TRSCNI - Input SCON   DECLARATIONS.  Used in 2 pass strategy
 *           to save the incoming SCON(2,11,13) values.
 *	SCONIN is indexed from the SCON array via SCOLNK; just
 * 	like SCONO, FORMSV, etal.
 *
 *    MUST FOLLOW:  INCLUDE (TRPARM)                                             
 *
 *  CREATED: 4/30/93  JAL                                                        
 *
 *  CHANGES:                                                                     
 *************************************************************                   
 *
 *  ELMMAX IS THE max NUMBER OF SENTENCE ELEMENTS allowed at any time. May       
 *         include original elements and those addedED when moving a clause in   
 *         tran2 or executing the -68 switch.ELMMAX IS DEFINED AS A PARAMETER    
 *         IN (TRPARM).                                                          
 *
 *  It is only necessary to save the SCON values for source sentence elements,
 *  not created target entities, which also get scons.                 
 *
 *
 *  SCINSZ = number of scon values to save.
 *  SCONIN(x,y) - save the input SCON 2,11,13 values for 2 pass strategy.
 *
 */
		/* PARAMETER translations */
#define	SCINSZ	3
		/* end of PARAMETER translations */

	/*COMMON translations*/
EXTERN  struct t_sconinX_ {
	short int sconin[ELMMAX][SCINSZ];
	}	sconinX_;
	 
/* trscon.h */
/*
 *************************************************************                   
 *  TRSCON - SCON   DECLARATIONS.                                                
 *
 *    MUST FOLLOW:  INCLUDE (TRPARM)                                             
 *
 *  CREATED: 8/15/89  JAL                                                        
 *
 *  CHANGES:                                                                     
 *   9/03/91 JAL-  replaced sconz parameter with elmmax                          
 *   8/15/89 JAL-  SCOLNK HAS BEEN ADDED AS A LINKLIST BETWEEN                   
 *           SCON AND SCONO, AND SCONO HAS BEEN EXPANEDED TO                     
 *           ACCOMODATE ELEMENTS CREATED DYNAMICALLY BY THE NEW                  
 *           REL CLAUSE FACILITY.                                                
 *************************************************************                   
 *
 *  SCONX  IS THE max NUMBER OF SCONS PER ORIGINAL SENTENCE ELEMENT              
 *  SCONX1 IS THE max NUMBER OF SCONS IN THE PRIMARY SCON TABLE, SCON.           
 *  SCONY  IS THE max NUMBER OF ELEMENTS (ORIGINAL AND ADDED) and constants.     
 *  ELMMAX IS THE max NUMBER OF SENTENCE ELEMENTS allowed at any time. May       
 *         include original elements and those addedED when moving a clause in   
 *         tran2 or executing the -68 switch.ELMMAX IS DEFINED AS A PARAMETER    
 *         IN (TRPARM).                                                          
 *
 *   NOTE: ORIGINAL SENTENCE ELEMENTS MUST STILL BE LOADED INTO FIRST            
 *         SCON POSITIONS.                                                       
 *
 *
 *  SCON(X,Y) - THE PRIMARY SCON TABLE CONTAINING 1ST 20 SCONS PER               
 *              ORIGINAL AND ADDED ELEMENT.                                      
 *  SCONO(A,B) - THE SCON OVERFLOW ARRAY FOR ORIGINAL ELEMENTS AND               
 *              ELEMENTS CREATED BY THE REL CLAUSE FACILITY IN TRAN2             
 *              or the 68 switch in any of the 4 trans.                          
 *  SCOLNK(Y) - Link list ptr to scono for every scon entry. Init to             
 *              zero(T1READ). If scolnk(y) = 0 then this is not a                
 *              complete element but a constant or vc, and there is              
 *              no corresponding row in the scono array.                         
 *
 *         N.B. SCOLNK(Y) is really a pointer in to element related              
 *              arrays (ex: SCONO,FORMSV,HASHCD,HENUM, etc.) for any SWORK.      
 *              Using SWORK(4,x) or PHRHED(x) ,the scon pointer for any swork,   
 *              SCOLNK(swork(4,x)) = the element number for the head of the xth  
 *              phrase. This indirection has become necessary with ability       
 *              to create new sworks or elements.  The nth new element will      
 *              probably not be the nth scon row, and making all element related 
 *              arrays as big as SCON() would be costly.                         
 *
 *  ELEMCT    - total number of elements currently in the system, both           
 *              original and added.   Must be incremented for each new           
 *              full element added ( by t2clmove or txsw68).  Declared           
 *              in TRNCOM member of TRANS MACLIB.                                
 *
 *   SCTSAV     VALUE OF SCT AS INPUT TO THE CURRENT TRAN. SAVED BEFORE          
 *              PASS 1 OF THE 2 PASS STRATEGY TO BE RESTORED BEFORE SECOND       
 *              PASS, TO ASSURE NO SOURCE ADDED SCONS REMAIN.                    
 *
 */
	/*COMMON translations*/
EXTERN  struct t_sconX_ {
	short int scon[SCONY][SCONX1], scolnk[SCONY], scono[ELMMAX][SCONX - SCONX1];
	}	sconX_;
EXTERN  struct t_sconctX_ {
	short int sctsav;
	}	sconctX_;
	 
/* trsz01.h */
/*
 *
 *    SIZE PARAMETERS FOR DATA ARRAYS IN TRAN1 PASS1                             
 *
 *    Setting Instructions for 'DEV' Development Staff and                       
 *                             'MIG' for Product Migrators.                      
 *
 *  Usage notes:                                                                 
 *              - Migrators should set all mini parameters = 1.                  
 *
 *              - The entire indexes must be read into memory‹                   
 *
 *                If you need to save on space, first try to tune internal       
 *                array sizes to coincide with the largest files read in         
 *                + 5% to accomodate any patches.  Indices and other main rule   
 *                information are contained in separate data files.  See TxSPINIT
 *                for details.  On the other hand, generated table files contain 
 *                both tables and indices in one file.                           
 *                The header record contains a pointer to the first index record.
 *                The first index record follows the last table entry.  See      
 *                TBINIT for details.                                            
 *
 *                If you need to save more space, cut down on the number         
 *                of 30, 40 and 50 tables IN THAT ORDER‹  Note, as of 11/7/90    
 *                German source uses 50 tables in TRAN4 only (consult a          
 *                linguist before taking action).  Otherwise, there is no        
 *                clear reason to prefer 40 tables over 50 tables.               
 *
 *                If absolutely necessary, you may set the VTR size below        
 *                the actual number of vtrs.  Any VTR not loaded in              
 *                memory will be read from file when needed.                     
 *
 *              - You should study the number of tables for all the give         
 *                targets (GE,GF & GS for German) to see the differences         
 *                in sizes.  If you can, use the largest number.                 
 *
 *
 *                                                                       T1S00370
 * Unix:Changes to facilitate the sizing of buffers at migration 10/87 +         
 *  Declare the parameters for the 30, 40 and 50 Tables of both                  
 *  the Main and Mini rules.                                                     
 */
		/* PARAMETER translations */
#define	M3SIZ1	3000
#define	M4SIZ1	3000
#define	M5SIZ1	3000
#define	MOVSZ1	2200
#define	MSPSZ1	2400
#define	MVTSZ1	1200
#define	MX3SZ1	TX3SZ1
#define	MX4SZ1	TX4SZ1
#define	MX5SZ1	TX5SZ1
#define	OVRSZ1	6500
#define	SPSZ1	6500
#define	T3SIZ1	8000
#define	T4SIZ1	3000
#define	T5SIZ1	3000
#define	TMSZ11	1024
#define	TMSZ21	1
#define	TX3SZ1	8000
#define	TX4SZ1	3000
#define	TX5SZ1	3000
#define	TYSZ11	1024
#define	TYSZ21	1
#define	VTRSZ1	8000
		/* end of PARAMETER translations */

/*
 *                                                                       TBO00300
 *
 *   Tran 1-4 30 Tables, Where 'x' represents the Tran level:                    
 *   T3xSIZ = # Records in 30 table in TRANx.      MIG: exact count              
 *   TX3xSZ = Index size for TRANx  30 Table       MIG: DO NOT TOUCH‹            
 *   M3xSIZ = # Records in mini 30 table in TRANx. MIG = 1                       
 *   MX3xSZ = Index size for Mini TRANx  30 Table  MIG = 1                       
 *                                                 DEV = TX3xSZ                  
 *
 *
 *   Tran 40 Tables (All 4 Trans share the the same 40 & 50 tables)              
 *   T4xSIZ = # Records in 40 table                MIG: exact count              
 *   TX40SZ = Index size for 40 Table              MIG: DO NOT TOUCH‹            
 *   M4xSIZ = # Records in mini 40 table           MIG = 1                       
 *   MX40SZ = Index size for Mini 40 Table         MIG = 1                       
 *                                                 DEV = TX40SZ                  
 *
 *
 *   Tran 50 Tables (All 4 Trans share the the same 40 & 50 tables)              
 *   T5xSIZ = # Records in 50 table                MIG: exact count              
 *   TX50SZ = Index size for 50 Table              MIG: DO NOT TOUCH‹            
 *   M5xSIZ = # Records in mini 50 table           MIG = 1                       
 *   MX50SZ = Index size for Mini 50 Table         MIG = 1                       
 *                                                 DEV = TX40SZ                  
 *
 *
 *  Declare the parameters for the SP, OVR, VTR arrays of both                   
 *  the Main and Mini rules.                                                     
 *  Migrators should set all of the Mini Parameters = 1, and                     
 *  tune all of the other parameters equal to the number of records              
 *  for each file + 2%. (To accomodate any patches to the rules which            
 *  may be slightly larger.)                                                     
 *
 *
 *
 *
 *       SIZE ARRAYS TO HOLD MAIN SPXINDX1 AND SPXINDX2  FILES                   
 *         typos1(x,y) = ptr to last rule with wc=y and type = x                 
 *         nftyp1(x,y) = number of rules   "    "    "    "                      
 *       SUBSCRIPT SIZE PARAMETER = TYSZab (main) and TMSZab (mini),             
 *          where a = subsript position (1,2) & b = pass (1,2)                   
 *
 *   **** IMPORTANT *****                                                        
 *     if there are mini rules in any of the 4 trans then TMSZ11 must = 1024     
 *     in order to avoid memory addressing errors.  If no minis are active then  
 *     make tmsz11 = 2 to minimize memory use.                                   
 *                            main rules                                         
 *                            mini rules                                         
 */
/* trsz02.h */
/*
 *
 *    SIZE PARAMETERS FOR DATA ARRAYS IN TRAN1 PASS2                             
 *
 *    Setting Instructions for 'DEV' Development Staff and                       
 *                             'MIG' for Product Migrators.                      
 *
 *  Usage notes:                                                                 
 *              - Migrators should set all mini parameters = 1.                  
 *
 *              - The entire indexes must be read into memory‹                   
 *
 *                If you need to save on space, first try to tune internal       
 *                array sizes to coincide with the largest files read in         
 *                + 5% to accomodate any patches.  Indices and other main rule   
 *                information are contained in separate data files.  See TxSPINIT
 *                for details.  On the other hand, generated table files contain 
 *                both tables and indices in one file.                           
 *                The header record contains a pointer to the first index record.
 *                The first index record follows the last table entry.  See      
 *                TBINIT for details.                                            
 *
 *                If you need to save more space, cut down on the number         
 *                of 30, 40 and 50 tables IN THAT ORDER‹  Note, as of 11/7/90    
 *                German source uses 50 tables in TRAN4 only (consult a          
 *                linguist before taking action).  Otherwise, there is no        
 *                clear reason to prefer 40 tables over 50 tables.               
 *
 *                If absolutely necessary, you may set the VTR size below        
 *                the actual number of vtrs.  Any VTR not loaded in              
 *                memory will be read from file when needed.                     
 *
 *              - You should study the number of tables for all the give         
 *                targets (GE,GF & GS for German) to see the differences         
 *                in sizes.  If you can, use the largest number.                 
 *
 *
 *                                                                       T1S00370
 * Unix:Changes to facilitate the sizing of buffers at migration 10/87 +         
 *  Declare the parameters for the 30, 40 and 50 Tables of both                  
 *  the Main and Mini rules.                                                     
 */
		/* PARAMETER translations */
#define	M3SIZ2	2
#define	M4SIZ2	2
#define	M5SIZ2	2
#define	MOVSZ2	2
#define	MSPSZ2	2
#define	MVTSZ2	2
#define	MX3SZ2	2
#define	MX4SZ2	TX4SZ2
#define	MX5SZ2	TX5SZ2
#define	OVRSZ2	2
#define	SPSZ2	2
#define	T3SIZ2	2
#define	T4SIZ2	2
#define	T5SIZ2	2
#define	TMSZ12	1024
#define	TMSZ22	2
#define	TX3SZ2	2
#define	TX4SZ2	2
#define	TX5SZ2	2
#define	TYSZ12	1024
#define	TYSZ22	2
#define	VTRSZ2	2
		/* end of PARAMETER translations */

/*
 *                                                                       TBO00300
 *
 *   Tran 1-4 30 Tables, Where 'x' represents the Tran level:                    
 *   T3xSIZ = # Records in 30 table in TRANx.      MIG: exact count              
 *   TX3xSZ = Index size for TRANx  30 Table       MIG: DO NOT TOUCH‹            
 *   M3xSIZ = # Records in mini 30 table in TRANx. MIG = 1                       
 *   MX3xSZ = Index size for Mini TRANx  30 Table  MIG = 1                       
 *                                                 DEV = TX3xSZ                  
 *
 *
 *   Tran 40 Tables (All 4 Trans share the the same 40 & 50 tables)              
 *   T4xSIZ = # Records in 40 table                MIG: exact count              
 *   TX40SZ = Index size for 40 Table              MIG: DO NOT TOUCH‹            
 *   M4xSIZ = # Records in mini 40 table           MIG = 1                       
 *   MX40SZ = Index size for Mini 40 Table         MIG = 1                       
 *                                                 DEV = TX40SZ                  
 *
 *
 *   Tran 50 Tables (All 4 Trans share the the same 40 & 50 tables)              
 *   T5xSIZ = # Records in 50 table                MIG: exact count              
 *   TX50SZ = Index size for 50 Table              MIG: DO NOT TOUCH‹            
 *   M5xSIZ = # Records in mini 50 table           MIG = 1                       
 *   MX50SZ = Index size for Mini 50 Table         MIG = 1                       
 *                                                 DEV = TX40SZ                  
 *
 *
 *  Declare the parameters for the SP, OVR, VTR arrays of both                   
 *  the Main and Mini rules.                                                     
 *  Migrators should set all of the Mini Parameters = 1, and                     
 *  tune all of the other parameters equal to the number of records              
 *  for each file + 2%. (To accomodate any patches to the rules which            
 *  may be slightly larger.)                                                     
 *
 *
 *
 *
 *       SIZE ARRAYS TO HOLD MAIN SPXINDX1 AND SPXINDX2  FILES                   
 *         typos1(x,y) = ptr to last rule with wc=y and type = x                 
 *         nftyp1(x,y) = number of rules   "    "    "    "                      
 *       SUBSCRIPT SIZE PARAMETER = TYSZab (main) and TMSZab (mini),             
 *          where a = subsript position (1,2) & b = pass (1,2)                   
 *
 *   **** IMPORTANT *****                                                        
 *     if pass2 is active for any of the 4 trans then TYSZ12 must = 1024         
 *     in order to avoid memory addressing errors.  If pass2 is inactive in all  
 *     trans then set TYSZ12 = 2 to save memory.  Likewise, if pass2 minis are   
 *     used in any TRAN then set TMSZ12 = 1024, else = 2.                        
 *
 *                            main rules                                         
 *                            mini rules                                         
 */

struct  {
	short int tagptr[5], nsptr[5], scnptr[5];
}	tagblkX_;

EXTERN struct t_phrhdiX_ {
	short int phrhed[ELMMAX];
	}	phrhdiX_;

EXTERN struct  {
	short int k7, oflad, n3, dummy[26];
	} diacbX_;
EXTERN struct t_sw34bkX_ {
	short int sw34n;
	}	sw34bkX_;
EXTERN struct t_elscnpX_ {
	short int elscnp[ELMMAX];
	}	elscnpX_;
EXTERN struct t_supresX_ {
	short int n3sv, n3sw25, n3up, phcts, phsup, phsups;
	}	supresX_;
EXTERN struct t_sw16bkX_ {
	short int sw16n;
	}	sw16bkX_;
EXTERN struct t_sw21bkX_ {
	short int sw21n, case_;
	}	sw21bkX_;
EXTERN struct  {
//from tran2 short int hedcas, hedgen, hednum, hedper, sw25n, tw25, adlock;
	short int hedcas, hedgen, hednum, hedper, hedsc3, ngen, nnum, relngn, relnnm, relnpr, sw25n, tw25, adlock;
	}	sw25bkX_;
EXTERN struct t_sw19bkX_ {
	short int tense;
	}	sw19bkX_;
EXTERN 	struct  {
		short int ad44vc[5][2], conspo, prevc, pre44, slot44;
		short int buckct, nicht;
} sw44bkX_;
EXTERN struct t_inhbX_ {
	short int inhb[1000];
	}	inhbX_;

EXTERN struct t_stradjX_ {
	short int stradj, spptr;
	}	stradjX_;

EXTERN struct t_prttagX_ {
	short int prtidx, prttag[4][21];
	}	prttagX_;
EXTERN struct t_prmtagX_ {
	short int prmidx, prmtag[4][21];
	}	prmtagX_;
EXTERN struct t_tgpairX_ {
	short int tgpair;
	}	tgpairX_;

EXTERN struct t_neg2X_ {
	short int negwc2[9][8];
	}	neg2X_;
EXTERN struct t_gneg2X_ {
	short int gngwc2[9][8];
	}	gneg2X_;
EXTERN struct t_w50valX_ {
	short int gusn[3], cnt50[3], gustov[3], i3, step50, wc50ti, wc50st;
	}	w50valX_;
EXTERN struct t_sw26bkX_ {
	short int phr26;
	}	sw26bkX_;

EXTERN struct t_flagX_ {
	short int relon;
	}	flagX_;
EXTERN struct t_dct2X_ {
	short int dct2[3];
	}	dct2X_;
EXTERN struct t_dctX_ {
	short int dct[3];
	}	dctX_;
EXTERN struct t_dct3X_ {
	short int dct3[3];
	}	dct3X_;
EXTERN struct t_dct4X_ {
	short int dct4[3];
	}	dct4X_;
EXTERN struct t_dct5X_ {
	short int dct5[3];
	}	dct5X_;
EXTERN	struct  {
		short int phsup, n3sv, phcts;
} phsupX_;
	

/* structure to save Tran1 output units (source analysis) for term search use */
typedef struct {
	int headWordPOS;						// word class
	int headWordSSSet;					// superset or set or subset
	int headWordForm;						// form code
	int headWordSconPointer;			// element number in source sentence
	int elements[ELMMAX];				// sequence of scon pointers of the elements forming this unit
	int opadro[ELMMAX];		// sequence of OPADR in target sequence order
} tran1Unit_;

struct {
	int totalSworkElements;
	tran1Unit_ units[ELMMAX];
} tran1Units_;
