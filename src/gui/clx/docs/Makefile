# Simple Makefile to add the document structuring comments to the
# original postscript files, and to build a Lectern document from
# these.  You can find out about Lectern if you search the WWW for
# `Virtual Paper', and find your way to the DEC SRC Virtual Paper
# site.

# Before you begin, all of the original postscript files should be
# renamed like this: `for f in *.ps; do mv $f ${f%%ps}orig.ps; done'

# Then, do `make number', and if you like, `make Lectern'.

# Here's the order to build them in.
PS= contents.ps \
	s01.ps s02.ps s03.ps s04.ps s05.ps s06.ps s07.ps s08.ps s09.ps s10.ps \
	s11.ps s12.ps s13.ps s14.ps s15.ps s16.ps \
	appa.ps glossary.ps general.ps condition.ps functions.ps types.ps

all:
	echo Please look at the Makefile first.

# just assume they're all renamed if one exists.
number: s01.orig.ps
	xemacs -q -no-site-file -batch -l reverse-pages.elc \
		-f clx-man-add-page-comments-to-ps *.orig.ps

# edit as you please, of course.
Lectern:
	BuildLectern -PSnoOriginal -only 3 -PSfakeOCR ${PS} CLX-prog-ref.lect
	BuildLectern -include CLX-prog-ref.lect 1 9999 -page1 5 -contents 3 \
		-index 215 CLX-prog-ref.lect
