C Version 31-August-2017                                 File: systemfunction.f
C------------------------------------------------------------------------------
C Copyright N. Cardiel, Departamento de Astrofisica
C Universidad Complutense de Madrid, 28040-Madrid, Spain
C E-mail: cardiel@ucm.es
C------------------------------------------------------------------------------
C This routine is free software; you can redistribute it and/or modify it
C under the terms of the GNU General Public License as published by the Free
C Software Foundation; either version 2 of the License, or (at your option) any
C later version. See the file gnu-public-license.txt for details.
C------------------------------------------------------------------------------
        INTEGER FUNCTION SYSTEMFUNCTION(COMANDO)
        IMPLICIT NONE
        CHARACTER*(*) COMANDO
C------------------------------------------------------------------------------
        CALL SYSTEM(COMANDO,SYSTEMFUNCTION)
        END
