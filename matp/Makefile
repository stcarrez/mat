NAME=mat

-include Makefile.conf

include Makefile.defaults

INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL} $(AM_INSTALL_PROGRAM_FLAGS)
INSTALL_DATA = ${INSTALL} -m 644
INSTALL_SCRIPT = ${INSTALL}

MAT_GTKADA=no
MKDIR=mkdir
CP=cp

# Build executables for all mains defined by the project.
all build::

build::		Makefile.conf $(GPRPATH)
	$(GNATMAKE) $(GPRFLAGS) -p -P$(NAME) $(MAKE_ARGS)

build-test::
	$(GNATMAKE) $(GPRFLAGS) -p -P$(NAME)_tests $(MAKE_ARGS)

ifeq ($(HAVE_GTKADA),yes)
build:: gtk

gtk:
	$(GNATMAKE) $(GPRFLAGS) -p -P$(NAME)_gtk $(MAKE_ARGS)

endif

test:	build-test
	bin/mat_harness

install::
	mkdir -p $(DESTDIR)${bindir}
	mkdir -p $(DESTDIR)${mandir}/man1
	$(INSTALL_PROGRAM) bin/mat $(DESTDIR)$(bindir)/
	$(INSTALL) man/mat.1 $(DESTDIR)$(mandir)/man1/mat.1


# Development targets that requires ayacc and aflex to be installed.
# Do not call these unless you modify the lex/yacc grammar.
parser:	
	cd src/parser && \
	   ayacc -n 256 -v -e .ada mat-expressions-parser.y && \
	   gnatchop -w mat-expressions-parser.ada && \
	   rm -f mat-expressions-parser.ada
	-rm -f src/parser/mat-expressions-lexer_io.ads
	-rm -f src/parser/mat-expressions-lexer_io.adb
	-rm -f src/parser/mat-expressions-parser.verbose

lexer:
	cd src/parser; \
	aflex -ms mat-expressions-lexer.l
	cd src/parser && \
	  gnatchop -w mat-expressions-lexer.ada && \
	  rm -f mat-expressions-lexer.ada
