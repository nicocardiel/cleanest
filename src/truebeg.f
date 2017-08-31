C------------------------------------------------------------------------------
C Version 25-November-1996                                      File: truebeg.f
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
C INTEGER FUNCTION TRUEBEG(CADENA)
C
C Input: CADENA
C Output: TRUEBEG (function)
C
C Return the position of the first non-blank character in CADENA (ignoring
C also control characters with ASCII value < 32)
C
C CHARACTER*(*) CADENA -> input character string
C
Comment
C------------------------------------------------------------------------------
        INTEGER FUNCTION TRUEBEG(CADENA)
        IMPLICIT NONE
        CHARACTER*(*) CADENA
C
        INTEGER I,L
C------------------------------------------------------------------------------
        L=LEN(CADENA)
C
        DO I=1,L
          IF(ICHAR(CADENA(I:I)).GT.32)THEN
            TRUEBEG=I
            RETURN
          END IF
        END DO
        TRUEBEG=0
        END
