C------------------------------------------------------------------------------
C Version 26-November-1996                                      File: buttqcf.f
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
C SUBROUTINE BUTTQCF(FONT)
C
C Output: FONT
C
C Return the current character font type in buttons.
C
C INTEGER FONT -> the current font number (in range 1-4)
C
Comment
C------------------------------------------------------------------------------
C
        SUBROUTINE BUTTQCF(FONT)
        IMPLICIT NONE
        INTEGER FONT
        INCLUDE 'button.inc'
C------------------------------------------------------------------------------
        FONT=PGSCF_BUTT
        END
