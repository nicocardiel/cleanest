# Process this file with autoconf to produce a configure script.
#

# Prelude
AC_INIT(cleanest,5.0,[cardiel@ucm.es])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([config.h.in])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])

# Checks for programs
AC_PROG_F77

# Safety check
AC_CONFIG_SRCDIR([src/cleanest.f])

# Definitions
DEFAULT_NXMAX="4300"
DEFAULT_NYMAX="4300"
DEFAULT_NAUXMAX="3"
NXMAX=${NXMAX="$DEFAULT_NXMAX"}
AC_ARG_VAR(NXMAX,[Maximum image dimension (pixels) in the X direction])
NYMAX=${NYMAX="$DEFAULT_NYMAX"}
AC_ARG_VAR(NYMAX,[Maximum image dimension (pixels) in the Y direction])
NAUXMAX=${NAUXMAX="$DEFAULT_NAUXMAX"}
AC_ARG_VAR(NAUXMAX,[Maximum number of auxiliary images])

# Define directories to search for libraries
AC_MSG_CHECKING([uname -s for detecting host operating system])
OSNAME=`uname -s`
case "$OSNAME" in
  Darwin*)
    AC_MSG_RESULT(Darwin)
    LDFLAGS="$LDFLAGS -L/opt/local/lib"
  ;;
  Linux*)
    AC_MSG_RESULT(Linux)
    LDFLAGS="$LDFLAGS -L/usr/local/pgplot"
  ;;
esac

# Checks for libraries
AC_MSG_RESULT(LDFLAGS set to... $LDFLAGS)
AC_CHECK_LIB([pgplot], main,,AC_MSG_ERROR(pgplot library not found))
AC_CHECK_LIB([X11], main,,AC_MSG_ERROR(X11 library not found))
AC_CHECK_LIB([cfitsio], main,,AC_MSG_ERROR(cfitsio library not found))
# Define in LIBS all the previous libraries
LIBS="$LIBS"
AC_MSG_RESULT(LIBS set to... $LIBS)

# Output files
AC_CONFIG_FILES([Makefile 
                 src/Makefile
                 src/button.inc
                 src/dimensions.inc
                 src/version.inc])
AC_OUTPUT

