#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(Stump Window Manager, esyscmd(grep :version stumpwm.asd | cut -d\" -f2 | tr -d \\n), dbjergaard@gmail.com)

AC_SUBST(MODULE_DIR)
AC_SUBST(LISP_PROGRAM)
AC_SUBST(LISP)
AC_SUBST(COMPRESSION)
AC_SUBST(STUMPWM_ASDF_DIR)

# Checks for programs.
AC_ARG_WITH(lisp,    [  --with-lisp=IMPL        use the specified lisp], LISP=$withval, LISP="sbcl")

AC_ARG_WITH(module-dir,
                     [  --with-module-dir=PATH specify location of contrib modules],
                     MODULE_DIR=$withval, MODULE_DIR="${HOME}/.stumpwm.d/modules")

AC_ARG_ENABLE(compression,
              [  --enable-compression    use SBCL's core compression feature if available],
              COMPRESSION="t",
              COMPRESSION="nil")

STUMPWM_ASDF_DIR="`pwd`"

if test -x "$SBCL_PATH"; then
   SBCL=$SBCL_PATH
   AC_MSG_CHECKING([for sbcl])
   AC_MSG_RESULT($SBCL)
else
   AC_PATH_PROG([SBCL], sbcl,"")
fi

if test "x$LISP" = "xsbcl"; then
   LISP_PROGRAM=$SBCL
   if test -z "$SBCL_HOME"; then
      AC_MSG_WARN(SBCL_HOME must be defined to use asdf/quicklisp (it should be where your sbcl.core resides))
   fi
fi

if test "x$LISP_PROGRAM" = "x"; then
   AC_MSG_ERROR([*** No lisp is available.])
fi

AC_MSG_NOTICE([Using $LISP at $LISP_PROGRAM])

# check for makeinfo

AC_CHECK_PROG(MAKEINFO,makeinfo,yes,no)

if test "$MAKEINFO" = "no"; then
   AC_MSG_WARN([Please install makeinfo for the manual.])
fi

AC_OUTPUT(Makefile)
AC_OUTPUT(make-image.lisp)
AC_OUTPUT(load-stumpwm.lisp)
