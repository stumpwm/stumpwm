#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(Stump Window Manager, esyscmd(grep :version stumpwm.asd | cut -d\" -f2 | tr -d \\n), sabetts@gmail.com)

AC_SUBST(CONTRIB_DIR)
AC_SUBST(LISP_PROGRAM)
AC_SUBST(LISP)

# Checks for programs.
AC_ARG_WITH(lisp,    [  --with-lisp=IMPL        use the specified lisp (sbcl, clisp, ccl or ecl)], LISP=$withval, LISP="any")
AC_ARG_WITH(sbcl,    [  --with-sbcl=PATH        specify location of sbcl], SBCL_PATH=$withval, SBCL_PATH="")
AC_ARG_WITH(clisp,   [  --with-clisp=PATH       specify location of clisp], CLISP_PATH=$withval, CLISP_PATH="")
AC_ARG_WITH(ccl,     [  --with-ccl=PATH         specify location of ccl], CCL_PATH=$withval, CCL_PATH="")
AC_ARG_WITH(ecl,     [  --with-ecl=PATH         specify location of ecl], ECL_PATH=$withval, ECL_PATH="")
AC_ARG_WITH(lw,      [  --with-lw=PATH          specify location of lispworks], LW_PATH=$withval, LW_PATH="")

AC_ARG_WITH(contrib-dir,
                     [  --with-contrib-dir=PATH specify location of contrib modules],
                     CONTRIB_DIR=$withval, CONTRIB_DIR="`pwd`/contrib")

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

if test -x "$CCL_PATH"; then
   CCL=$CCL_PATH
   AC_MSG_CHECKING([for ccl])
   AC_MSG_RESULT($CCL)
else
   AC_PATH_PROG([CCL],ccl,"")
fi

if test -x "$ECL_PATH"; then
   ECL=$ECL_PATH
   AC_MSG_CHECKING([for ecl])
   AC_MSG_RESULT($ECL)
else
   AC_PATH_PROG([ECL], ecl,"")
fi

if test -x "$LW_PATH"; then
   LW=$LW_PATH
   AC_MSG_CHECKING([for lispworks])
   AC_MSG_RESULT($LW)
else
   AC_PATH_PROG([LW], lw,"")
fi

if test "x$LISP" = "xany"; then
   if test "$SBCL"; then
      LISP=sbcl
   elif test "$CLISP"; then
      LISP=clisp
   elif test "$CCL"; then
      LISP=ccl
   elif test "$ECL"; then
      LISP=ecl
   elif test "$LW"; then
      LISP=lw
   fi
fi

if test "x$LISP" = "xsbcl"; then
   LISP_PROGRAM=$SBCL
elif test "x$LISP" = "xclisp"; then
   LISP_PROGRAM=$CLISP
elif test "x$LISP" = "xccl"; then
   LISP_PROGRAM=$CCL
elif test "x$LISP" = "xecl"; then
   LISP_PROGRAM=$ECL
elif test "x$LISP" = "xlw"; then
   LISP_PROGRAM=$LW
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

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.
AC_OUTPUT(Makefile)
AC_OUTPUT(make-image.lisp)
