SUBDIRS	= \
	dbcache \
	dbms2restran \
	elision \
	entity \
	gerdem \
	GrammarRules \
	lgs_parsetran1 \
	lgs_parsetran2 \
	lgs_parsetran3 \
	lgs_parsetran4 \
	lgssgml \
	linguistic \
	multithreadlib \
	newdebug \
	sql \
	odbcsql \
	parsetrans \
	patternmatcher \
	PatternRecognition \
	regex \
	res_parsetran_utils \
        ruleengine \
	semtabrule \
	startrules \
	SubjectMatterCodes \
	translutility \
	utility \
	halfnoun \
	halfnoundll \
	lgs_tran_rule_io


#DIST_SUBDIRS = $(SUBDIRS) common borland
#EXTRA_DIST   = BUGS CHANGELOG LICENSE README TODO 
#	       doxyconfig.cheap doxyconfig.flop

# For ebrowse (Emacs)
BROWSE: 
	echo `find $(topsrc_dir) -name '*.h' -print ` \
	     `find $(topsrc_dir) \( -name '*.c' -o -name '*.cpp' \) -print` | \
	tr ' ' '\n'| ebrowse

sign: dist
	gpg --detach-sign --armor $(distdir).tar.gz
	md5sum $(distdir).tar.gz | gpg --clearsign > $(distdir).tar.gz.md5sum

# Dependencies: all source files of the distribution
# Should be done when the distribution is made
#doc: flopdoc cheapdoc

#flopdoc:
#	doxygen doxyconfig.flop

#cheapdoc:
#	doxygen doxyconfig.cheap

