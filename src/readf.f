C------------------------------------------------------------------------------
C Version 21-August-2004                                          File: readf.f
C------------------------------------------------------------------------------
C Copyright N. Cardiel & J. Gorgas, Departamento de Astrofisica
C Universidad Complutense de Madrid, 28040-Madrid, Spain
C E-mail: cardiel@ucm.es
C------------------------------------------------------------------------------
C This routine is free software; you can redistribute it and/or modify it
C under the terms of the GNU General Public License as published by the Free
C Software Foundation; either version 3 of the License, or (at your option) any
C later version.
C------------------------------------------------------------------------------
Comment
C
C REAL FUNCTION READF(CDEF)
C
C Input: CDEF
C Output: READF (function)
C
C Return a float number entered by the user through the keyboard.
C
C CHARACTER*(*) CDEF -> character string with default value for READF
C               ('@' if there is no default)
C
Comment
C------------------------------------------------------------------------------
        REAL FUNCTION READF(CDEF)
        IMPLICIT NONE
        CHARACTER*(*) CDEF
C
        INTEGER I,L1,L2
        INTEGER NERR
        REAL F
        INTEGER TRUEBEG,TRUELEN
        CHARACTER*1 C
        CHARACTER*255 CDUMMY
        CHARACTER*255 CADENA
        LOGICAL LRUN,LMANUAL,LHTML
C------------------------------------------------------------------------------
        NERR=0
10      IF(CDEF.NE.'@')THEN
          L1=TRUEBEG(CDEF)
          IF(L1.NE.0)THEN
            L2=TRUELEN(CDEF)
            WRITE(*,100)'['
            WRITE(*,100)CDEF(L1:L2)
            WRITE(*,100)'] ? '
          END IF
        ELSE
          WRITE(*,100)'? '
        END IF
        READ(*,'(A)',ERR=20)CADENA
        IF(TRUELEN(CADENA).GE.10) CALL ENDPROGRAM(CADENA)
        IF(TRUELEN(CADENA).EQ.0)THEN
          IF(CDEF.EQ.'@')THEN
            GOTO 10
          END IF
          CADENA=CDEF
        END IF
        DO I=1,TRUELEN(CADENA)
          C=CADENA(I:I)
          IF((INDEX('abcfghijklmnoprstuvwxyz',C).NE.0).OR.
     +     (INDEX('ABCFGHIJKLMNOPRSTUVWXYZ/',C).NE.0))THEN
            GOTO 20
          END IF
        END DO
        READ(CADENA,*,ERR=20) F
        READF=F
        CALL LRUNX(LRUN,LMANUAL,LHTML)
        IF(LRUN)THEN
          WRITE(CDUMMY,*) F
          CALL RMBLANK(CDUMMY,CDUMMY,L2)
          !WRITE(*,101) CDUMMY(1:L2)
          WRITE(*,101) ACHAR(27)//'[1;46m'//CDUMMY(1:L2)//
     +     ACHAR(27)//'[1;m'
        END IF
        IF(LMANUAL)THEN
          WRITE(CDUMMY,*) F
          CALL RMBLANK(CDUMMY,CDUMMY,L2)
          WRITE(*,101) '\\ttshade{'//CDUMMY(1:L2)//'}'
        END IF
        IF(LHTML)THEN
          WRITE(CDUMMY,*) F
          CALL RMBLANK(CDUMMY,CDUMMY,L2)
          WRITE(*,101) '<FONT COLOR="#FF0000">'//CDUMMY(1:L2)//'</FONT>'
        END IF
        RETURN
20      WRITE(*,101)'ERROR: invalid character(s) found in '//
     +   'number. Try again.'
        IF(CDEF.EQ.'@') WRITE(*,100)'? '
        NERR=NERR+1
        IF(NERR.GT.10) STOP 'FATAL ERROR: too many errors.'
        GOTO 10
100     FORMAT(A,$)
101     FORMAT(A)
        END
