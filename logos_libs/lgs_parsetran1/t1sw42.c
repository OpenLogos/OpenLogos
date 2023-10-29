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
#include <stdlib.h>
#include <math.h>
#define MS_F77
#include <logos_include_res_pt/fcrt.h>
#include "project.h"
#include "logoslib.h"
#include "trans.h"
#include "projexts.h"
#include "parsetrans.h"
#include <string.h>
#include <logos_include_res_pt/jbctrl.h>

EXTERN struct t_getvtrX_ {
	short int getvtr;
	}	getvtrX_;
EXTERN struct t_loopckX_ {
	short int imatch[ELMMAX], noloop[21], nptpsv, nptpx, call36[2];
	}	loopckX_;
EXTERN struct t_sploopX_ {
	short int st16, li;
	}	sploopX_;
EXTERN struct t_sw42bkX_ {
	short int im1sav, i3keep, likeep;
	}	sw42bkX_;
EXTERN struct t_vtrs42X_ {
	short int sw42n;
	}	vtrs42X_;
EXTERN struct t_vwarg1X_ {
	short int look50[3], chng50[3], wc50el[3], isave, omfrm;
	}	vwarg1X_;
EXTERN struct t_ex42nX_ {
	short int swc, tyc;
	}	ex42nX_;
EXTERN struct t_sw24bkX_ {
	short int sw24n;
	}	sw24bkX_;

void /*FUNCTION*/ t1sw42()
{
	static long int _n;
		/*pointer COMMON transls*/
	struct  {
		short int sw1, sw2, sw3, sw4, sw5, sw6, sw7, sw8, sw9, sw10, 
		  sw11, sw12, sw13, sw14, sw15, sw16, sw17, sw18, sw19, sw20, 
		  sw21, sw22, sw23, sw24, sw25, sw26, sw27, sw28, sw29, sw30;
		}	*_opswX_ = (void*)&opswX_;
	struct  {
		long int untcnt;
		char mod[9], pgm[9];
		long int linenm, err, logerr, errlvl;
		byte errdat[216];
		}	*_errvrsX_ = (void*)&errvrsX_;
	struct  {
		short int k7, oflad, n3;
		}	*_diacbX_ = (void*)&diacbX_;
	struct  {
		short int i, wc42m, wc50m, iz4, swx, s11prt;
		}	*_vwarg2X_ = (void*)&vwarg2X_;
	struct  {
		short int vtr[26];
		}	*_vtrX_ = (void*)&vtrX_;
	struct  {
		short int im1, i3save, i3str, n6, n6jim, n6jims, phrstr;
		}	*_flowckX_ = (void*)&flowckX_;
	struct  {
		short int k7m, minifg;
		}	*_minickX_ = (void*)&minickX_;
	struct  {
		short int nwrks, index, k3p3, pntr9;
		}	*_semargX_ = (void*)&semargX_;
		/*end of pointer COMMON transls*/
	static short zero = 0;
	static char pgmnam[9] = "T1SW42  ";
	static short ms = 0;
	static short k3p2 = 0;
	static short k3p4 = 0;
	static short disam = 0;
	static short k7sav = 0;
	static short im81sv = 0;
	static short i3sv42 = 0;
	static short lisv42 = 0;
	static short negpas = 0;

	/* LAST CHG 04/08/87            Loop Logic Improvements  OGM */
	/*+ - - - - - - - - - - - - - - - - - - - -    PR 30,40,50 PROJECT12/86
	 *==== COMMON /DIACB/  K7,OFLAD,N3,VTR */
	/*- - - - - - - - - - - - - - - - - - - - -    PR 30,40,50 PROJECT 12/86 */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *
	 *SW42 */
	/*   ***** BEGINNING OF -42 SWITCH ***** */
	/*          THIS SWITCH INITIATES A SPECIAL SEARCH OF WC 10 FOR MATCH
	 *          ON SPECIAL SEMANTIC RULES.  HAS FOUR (4) PARAMETERS:
	 *          K3P1 IS WORDCLASS OF RULES TO BE SEARCHED, USUALLY WC 10
	 *          K3P2 IS BLOCK OR SEGMENT TYPE WITHIN K3P1 WHERE SESRCH BEG
	 *          K3P3 IS ELEMENT OF SWORK WHERE SEARCH IS TO BEGIN
	 *          K3P4 IS ELEMENT OF SWORK WHERE SEARCH IS TO END */
	vbdataX_.k3n = vbdataX_.k3 + 5;
	if( vtrs42X_.sw42n != 1 )
		vwarg1X_.isave = _vwarg2X_->i;
	vtrs42X_.sw42n = 1;

	if( loopckX_.call36[1-One] == 0 ){
		loopckX_.call36[1-One] = loopckX_.nptpsv;
		if( _minickX_->minifg == 1 )
			loopckX_.call36[2-One] = 1;
		}


	/*   IDENTIFY PARAMETERS */
	k3p2 = vtrfX_.vtrf[vbdataX_.k3+2-One];
	_semargX_->k3p3 = vtrfX_.vtrf[vbdataX_.k3+3-One];
	k3p4 = vtrfX_.vtrf[vbdataX_.k3+4-One];

	if( _vwarg2X_->wc50m == 1 )
		_vwarg2X_->wc42m = 1;
	if( _vwarg2X_->wc42m != 0 ){
		lmove(strw10X_.wc50sv,1,vwarg1X_.wc50el,1,6);
		lmove(strw10X_.ch50sv,1,vwarg1X_.chng50,1,6);
		}

	ex42nX_.swc = vbdataX_.k3p1;
	ex42nX_.tyc = k3p2;
	_flowckX_->i3str = _semargX_->k3p3 + _flowckX_->im1;

	/*   SET TO 4 WILL CAUSE SKIP OF NEGATIVE SEGMENT IN T1GETSP */
	negpas = 4;

	/*   SAVE OLD END POSITIONS */
	lisv42 = sploopX_.li;
	i3sv42 = _flowckX_->i3save;
	_flowckX_->i3save = 0;

	/*   SET UP VALUES FOR NEW SEARCH */
	sw42bkX_.im1sav = _flowckX_->im1;
	im81sv = im81X_.im81;
	k7sav = _diacbX_->k7;

	/*---------------------------------------------------------------------- */

	/*   SEARCH FOR A WC 10 RULE
	 *      IF K7  = 0, THEN NO WC10 RULES MATCHED ON IN MAIN FILE
	 *      IF K7M = 0, THEN NO WC10 RULES MATCHED ON IN MINI FILE */

	getsp_x(&diacbX_.k7, &diacbX_.oflad, sp1yX_.ovrflw);
	if( _errvrsX_->errlvl == 0 ){

		if( _diacbX_->k7 == 0 && _opswX_->sw10 == 1 )
			{
			fprintf( _spec_fp, " SW42 %5d%5d%5d%5d%5d%5d%5d%5d%5d\n", 
			  sw24bkX_.sw24n, ex42nX_.swc, ex42nX_.tyc, _flowckX_->i3str, 
			  negpas, disam, _diacbX_->k7, k7sav, _minickX_->k7m );
			}

		/*                 WAS WC10 MATCH FOUND IN EXPERIMENTAL RULES?? */
		if( _minickX_->k7m != 0 && _opswX_->sw3 == 1 )
			{
			fprintf( _spec_fp, " EXPERIMENTAL WINS VIA SW42 %5d%5d%5d", 
			  _minickX_->k7m, cbsp2X_.wc5xm, cbsp2X_.i3x );
			fprintf( _spec_fp, "       " );
			for(_n=0L; _n < sizeof(cbsp2X_.look5x)/sizeof(short); _n++)
				fprintf( _spec_fp, "%5d", cbsp2X_.look5x[_n] );
			for(_n=0L; _n < sizeof(cbsp2X_.chng5x)/sizeof(short); _n++)
				fprintf( _spec_fp, "%5d", cbsp2X_.chng5x[_n] );
			for(_n=0L; _n < sizeof(cbsp2X_.wc5xel)/sizeof(short); _n++)
				fprintf( _spec_fp, "%5d", cbsp2X_.wc5xel[_n] );
			fprintf( _spec_fp, "   \n" );
			}

		if( _diacbX_->k7 != 0 || _minickX_->k7m != 0 ){
			/*      WE FOUND A WC10 RULE. LETS GO GET ITS VTR AND PROCESS IT. */
			getvtrX_.getvtr = 1;
			}
		else{

			/*------------------------------------------------------------------
			 *        NO MATCH IN EITHER THE MAIN FILE OR THE MINI.
			 *        RESTORE PARAMETERS AND FINISH THE REST OF THE CURRENT VTR. */

			vtrs42X_.sw42n = 0;

			/*      Loop logic improvements                 OM FIX 4/8/87 (next line)
			 *     CALL ZAPIT (CALL36,4,ZERO) */

			if( _vwarg2X_->wc42m != 0 ){
				_vwarg2X_->wc50m = _vwarg2X_->wc42m;
				_vwarg2X_->wc42m = 0;
				lmove(vwarg1X_.wc50el,1,strw10X_.wc50sv,1,6);
				lmove(vwarg1X_.chng50,1,strw10X_.ch50sv,1,6);
				for( ms=1; ms <= 3; ms++ ){
					vwarg1X_.look50[ms-One] = vwarg1X_.wc50el[ms-One] - 
					  80;
					}
				}

			sploopX_.li = lisv42;
			_flowckX_->i3save = i3sv42;
			_flowckX_->im1 = sw42bkX_.im1sav;
			im81X_.im81 = im81sv;
			_vwarg2X_->i = vwarg1X_.isave;
			_diacbX_->k7 = k7sav;

			/*   ***** END OF -42 SWITCH ***** */

			/*SW42 */
			}
		}
	return;
} /*end of function*/

