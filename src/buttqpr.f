C------------------------------------------------------------------------------
C Version 26-November-1996                                      File: buttqpr.f
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
C SUBROUTINE BUTTQPR(X1,X2,Y1,Y2)
C
C Output: X1,X2,Y1,Y2
C
C Return the plot region limits.
C
C REAL X1 -> x-coordinate of the left hand edge of the plot region viewport,
C      in normalized device coordinates
C REAL X2 -> x-coordinate of the right hand edge of the plot region viewport,
C      in normalized device coordinates
C REAL Y1 -> y-coordinate of the bottom edge of the plot region viewport,
C      in normalized device coordinates
C REAL Y2 -> y-coordinate of the top edge of the plot region viewport,
C      in normalized device coordinates
C
Comment
C------------------------------------------------------------------------------
        SUBROUTINE BUTTQPR(X1,X2,Y1,Y2)
        IMPLICIT NONE
        REAL X1,X2,Y1,Y2
        INCLUDE 'button.inc'
C------------------------------------------------------------------------------
        X1=X1VPORT
        X2=X2VPORT
        Y1=Y1VPORT
        Y2=Y2VPORT
        END
