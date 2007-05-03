# choose your lisp and appropriate lisp_opts
CLISP=clisp
SBCL=sbcl

LISP=$(SBCL)

CLISP_OPTS=-K full -on-error exit ./make-image.lisp
SBCL_OPTS=--load ./make-image.lisp

LISP_OPTS= $(SBCL_OPTS)

# You shouldn't have to edit past this

# This is copied from the .asd file. It'd be nice to have the list in
# one place, but oh well.
FILES=package.lisp primitives.lisp keysyms.lisp keytrans.lisp kmap.lisp input.lisp core.lisp user.lisp mode-line.lisp stumpwm.lisp version.lisp make-image.lisp

all: stumpwm.info stumpwm

stumpwm.info: stumpwm.texi
	makeinfo stumpwm.texi

stumpwm: $(FILES)
	$(LISP) $(LISP_OPTS)

