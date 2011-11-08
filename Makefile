# $Id$
include Makefile.head

PROJECT=Frontc
VERSION=3.4
RELEASE=2
SUBDIRS = frontc ctoxml printc calipso mergec
DIST+=AUTHORS ChangeLog COPYING INSTALL NEWS README

include Makefile.tail

doc:
	test -d autodoc || mkdir autodoc
	cd frontc; $(MAKE) doc
