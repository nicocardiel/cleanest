C------------------------------------------------------------------------------
C Version 26-November-1996                                   File: buttqytext.f
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
C SUBROUTINE BUTTQYTEXT(YTEXT)
C
C Output: YTEXT
C
C Return the current relative y-position of the text baseline in buttons
C (from 0 to 1)
C
C REAL YTEXT -> = YTEXT_BUTT
C
Comment
C------------------------------------------------------------------------------
C
        SUBROUTINE BUTTQYTEXT(YTEXT)
        IMPLICIT NONE
        REAL YTEXT
        INCLUDE 'button.inc'
C------------------------------------------------------------------------------
        YTEXT=YTEXT_BUTT
        END
