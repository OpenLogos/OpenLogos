SUBDIRS      = logos_libs lgs_stemgen TermMiner lgs_base\
	       configdatafileinterface lgs_db_io engine_api transl CacheMgr\
	       ConvertMini logos_include logos_include_res_pt doc\
	       logos_java shells

nobase_dist_pkgdata_DATA = bin/finish.rules bin/pgm_constants.ini \
	bin/proper_names.patterns bin/server.properties \
	bin/start.rules

EXTRA_DIST = bin_data openlogos.in testapi \
	INSTALL LICENSE \
	LogOSMaTran_Commercial_License.pdf \
	LogOSMaTran_Licensing_Policy.pdf \
	LogOSMaTran_Open_Source_License.pdf

install-data-hook:
	test -d "$(DESTDIR)$(pkgdatadir)/bin_data" || cp -r $(top_srcdir)/bin_data "$(DESTDIR)$(pkgdatadir)"

install-exec-local:
	test -d "$(DESTDIR)$(bindir)" || mkdir -p "$(DESTDIR)$(bindir)"
	sed "s|@@lgs_root@@|$(DESTDIR)$(pkgdatadir)|" < $(top_srcdir)/openlogos.in \
	| sed "s|@@bin_root@@|$(DESTDIR)$(bindir)|" \
	| sed "s|@@lib_root@@|$(DESTDIR)$(libdir)|" > "$(DESTDIR)$(bindir)/openlogos"
	chmod 755 "$(DESTDIR)$(bindir)/openlogos"

uninstall-local:
	rm -rf "$(DESTDIR)$(pkgdatadir)/bin_data"
	rm "$(DESTDIR)$(bindir)/openlogos"

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

