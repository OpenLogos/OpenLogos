This directory contains the files CharUtil.h and CharUtil.cpp
which contain various character utility functions that are
used by regular expressions, and other parts of the system.
In particular there are character classification functions
modelled on the <ctype.h> header, but customized for the
Latin1 character set (with the addition of the oe and OE
ligatures) and usable for translation. For example there
is a different definition of what constitutes an alphabetic
character and what constitutes punctuation.

This directory contains a library to do regular expression
parsing of a string. It is a C++ class wrapper around a
public-domain C library for regular expression parsing -
the GNU implementation.

The GNU C source file has been modified in order to compile -
the original source file is the 'GnuSource' subdirectory.

The directory also contains a test application to excercise
this library.
