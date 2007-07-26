#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(Stump Window Manager, 0.0.6, sabetts@vcn.bc.ca)

AC_SUBST(LISP_PROGRAM)
AC_SUBST(LISP)

# Checks for programs.
AC_ARG_WITH(lisp, [ --with-lisp=IMPL      use the specified lisp (either sbcl or clisp)], LISP=$withval, LISP="sbcl")
AC_ARG_WITH(sbcl, [ --with-sbcl=PATH      specify full path to sbcl], SBCL_PATH=$withval, SBCL_PATH="")
AC_ARG_WITH(clisp, [ --with-clisp=PATH     specify full path to clisp], CLISP_PATH=$withval, CLISP_PATH="")

AC_PATH_PROG([CLISP], clisp,"",[$CLISP_PATH:$PATH])
AC_PATH_PROG([SBCL], sbcl,"",[$SBCL_PATH:$PATH])
AC_PATH_PROG([SBCL], sbcl,"",[$SBCL_PATH:$PATH])

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

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.
AC_OUTPUT(Makefile)
