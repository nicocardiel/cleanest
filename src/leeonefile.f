C------------------------------------------------------------------------------
C Version 31-August-2017                                     File: leeonefile.f
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
        SUBROUTINE LEEONEFILE(INFILE_,LOK)
        IMPLICIT NONE
        CHARACTER*255 INFILE_
        LOGICAL LOK
C functions
        INTEGER TRUEBEG
        INTEGER TRUELEN
        INTEGER SYSTEMFUNCTION
C variables
        INTEGER L1,L2
        INTEGER NLINES
        INTEGER ISYSTEM
        CHARACTER*255 CLINEA
C------------------------------------------------------------------------------
        L1=TRUEBEG(INFILE_)
        L2=TRUELEN(INFILE_)
        ISYSTEM=SYSTEMFUNCTION('ls -l *'//INFILE_(L1:L2)//
     +   '* > .tmp_input_file_xnirspec')
        IF(ISYSTEM.NE.0)THEN
          LOK=.FALSE.
          RETURN
        END IF
C------------------------------------------------------------------------------
        NLINES=0
10      OPEN(10,FILE='.tmp_input_file_xnirspec',STATUS='OLD',
     +   FORM='FORMATTED',ERR=98)
        READ(10,101,END=98) CLINEA
        NLINES=NLINES+1
        GOTO 10
98      CLOSE(10)
        IF(NLINES.NE.1)THEN
          ISYSTEM=SYSTEMFUNCTION('cat .tmp_input_file_xnirspec')
        END IF
        ISYSTEM=SYSTEMFUNCTION('rm -f .tmp_input_file_xnirspec')
C
        IF(NLINES.NE.1)THEN
          LOK=.FALSE.
          RETURN
        END IF
C
        ISYSTEM=SYSTEMFUNCTION('ls *'//INFILE_(L1:L2)//
     +   '* > .tmp_input_file_xnirspec_single')
        OPEN(20,FILE='.tmp_input_file_xnirspec_single',STATUS='OLD',
     +   FORM='FORMATTED',ERR=99)
        READ(20,101,END=99) CLINEA
        CLOSE(20)
        ISYSTEM=SYSTEMFUNCTION('rm -f .tmp_input_file_xnirspec_single')
        INFILE_=CLINEA
        LOK=.TRUE.
        RETURN
C
99      LOK=.FALSE.
        RETURN
C------------------------------------------------------------------------------
101     FORMAT(A)
        END
