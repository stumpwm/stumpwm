#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(Stump Window Manager, 0.9.6-git, sabetts@vcn.bc.ca)

AC_SUBST(LISP_PROGRAM)
AC_SUBST(LISP)
AC_SUBST(PPCRE_PATH)

# Checks for programs.
AC_ARG_WITH(lisp, [ --with-lisp=IMPL      use the specified lisp (either sbcl or clisp)], LISP=$withval, LISP="sbcl")
AC_ARG_WITH(sbcl, [ --with-sbcl=PATH      specify location of sbcl], SBCL_PATH=$withval, SBCL_PATH="")
AC_ARG_WITH(clisp, [ --with-clisp=PATH     specify location of clisp], CLISP_PATH=$withval, CLISP_PATH="")
AC_ARG_WITH(openmcl, [ --with-openmcl=PATH     specify location of openmcl], OPENMCL_PATH=$withval, OPENMCL_PATH="")
AC_ARG_WITH(ppcre, [ --with-ppcre=PATH     specify location of cl-ppcre], PPCRE_PATH=$withval, PPCRE_PATH="`pwd`/cl-ppcre")

if test -x "$SBCL_PATH"; then
   SBCL=$SBCL_PATH
   AC_MSG_CHECKING([for sbcl])
   AC_MSG_RESULT($SBCL)
else
   AC_PATH_PROG([SBCL], sbcl,"")
fi

if test -x "$CLISP_PATH"; then
   CLISP=$CLISP_PATH
   AC_MSG_CHECKING([for clisp])
   AC_MSG_RESULT($CLISP)
else
   AC_PATH_PROG([CLISP],clisp,"")
fi

if test -x "$OPENMCL_PATH"; then
   OPENMCL=$OPENMCL_PATH
   AC_MSG_CHECKING([for openmcl])
   AC_MSG_RESULT($OPENMCL)
else
   AC_PATH_PROG([OPENMCL],openmcl,"")
fi

if test "x$LISP" = "xclisp"; then
   if test "x$CLISP" = "x"; then
      LISP=sbcl
      LISP_PROGRAM=$SBCL
   else
      LISP_PROGRAM=$CLISP
   fi
elif test "x$LISP" = "xsbcl"; then
   if test "x$SBCL" = "x"; then
      LISP=clisp
      LISP_PROGRAM=$CLISP
   else
      LISP_PROGRAM=$SBCL
   fi
elif test "x$LISP" = "xopenmcl"; then
   if test "x$OPENMCL" = "x"; then
      LISP=sbcl
      LISP_PROGRAM=$SBCL
   else
      LISP_PROGRAM=$OPENMCL
   fi

fi

if test "x$LISP_PROGRAM" = "x"; then
   AC_MSG_ERROR([*** No lisp is available.])
fi

AC_MSG_NOTICE([Using $LISP at $LISP_PROGRAM])

# check for makeinfo

AC_CHECK_PROG(MAKEINFO,makeinfo,yes,no)

if test "$MAKEINFO" = "no"; then
   AC_MSG_ERROR([Please install makeinfo for the manual.])
fi

AC_CHECK_PROG(XDPYINFO,xdpyinfo,yes,no)
if test "$XDPINFO" = "no"; then
   AC_MSG_WARN([xdpyinfo is needed for xinerama support.])
fi

if test "$LISP" = "clisp"; then
   AC_CHECK_FILE([$PPCRE_PATH/cl-ppcre.asd],,AC_MSG_ERROR([Cannot find ppcre. When using clisp you must specify its location using --with-ppcre]))
fi

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.
AC_OUTPUT(Makefile)
AC_OUTPUT(make-image.lisp)
AC_OUTPUT(version.lisp)
