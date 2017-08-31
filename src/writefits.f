C Version 31-August-2017                                      File: writefits.f
C------------------------------------------------------------------------------
C Copyright N. Cardiel, Departamento de Astrofisica
C Universidad Complutense de Madrid, 28040-Madrid, Spain
C E-mail: cardiel@ucm.es
C------------------------------------------------------------------------------
C This routine is free software; you can redistribute it and/or modify it
C under the terms of the GNU General Public License as published by the Free
C Software Foundation; either version 3 of the License, or (at your option) any
C later version.
C------------------------------------------------------------------------------
C
        SUBROUTINE WRITEFITS(ORIGINAL_NAMFIL,EXTNUM,BITPIX,A,
     +   NSCAN,NCHAN,OUTFILE)
        IMPLICIT NONE
        INCLUDE 'dimensions.inc'
        CHARACTER*255 ORIGINAL_NAMFIL
        INTEGER EXTNUM
        INTEGER BITPIX
        REAL A(NCMAX,NSMAX)
        INTEGER NSCAN,NCHAN
        CHARACTER*255 OUTFILE
C
        INTEGER TRUEBEG,TRUELEN
        INTEGER SYSTEMFUNCTION
        CHARACTER*255 READC
C
        INTEGER ISYSTEM
        INTEGER I,J
        INTEGER L1,L2
        INTEGER L1_,L2_
        INTEGER IUNIT
        INTEGER ISTATUS
        INTEGER IREADWRITE
        INTEGER BLOCKSIZE
        INTEGER HDUTYPE
        INTEGER GROUP
        REAL*8 AA(NCMAX,NSMAX)
        LOGICAL LOGFILE
C------------------------------------------------------------------------------
        LOGFILE=.TRUE.
        DO WHILE(LOGFILE)
          L1=TRUEBEG(ORIGINAL_NAMFIL)
          L2=TRUELEN(ORIGINAL_NAMFIL)
          WRITE(*,100) '* Previous name: '
          WRITE(*,101) ORIGINAL_NAMFIL(L1:L2)
          WRITE(*,100) 'Output file name '
          OUTFILE=READC('*.*fit*','@')
          L1_=TRUEBEG(OUTFILE)
          L2_=TRUELEN(OUTFILE)
          IF((INDEX(OUTFILE,'*').NE.0).OR.
     +     (INDEX(OUTFILE,'?').NE.0))THEN
            ISYSTEM=SYSTEMFUNCTION('ls '//OUTFILE(L1_:L2_))
          ELSE
            INQUIRE(FILE=OUTFILE(L1_:L2_),EXIST=LOGFILE)
            IF(LOGFILE)THEN
              WRITE(*,101)'ERROR: this file already exist. Try again.'
            END IF
          END IF
        END DO
C------------------------------------------------------------------------------
C copy original file to avoid overwriting
        ISYSTEM=SYSTEMFUNCTION('cp '//ORIGINAL_NAMFIL(L1:L2)//' '//
     +   OUTFILE(L1_:L2_))
C------------------------------------------------------------------------------
        ISTATUS=0
        IUNIT=80
C open the file in read/write mode
        IREADWRITE=1
        CALL FTOPEN(IUNIT,OUTFILE(L1_:L2_),IREADWRITE,BLOCKSIZE,ISTATUS)
        IF(ISTATUS.GT.0) CALL PRINTERROR(ISTATUS)
C move the specified extension
        CALL FTMAHD(IUNIT,EXTNUM,HDUTYPE,ISTATUS)
        IF(ISTATUS.GT.0) CALL PRINTERROR(ISTATUS)
C write the array
        GROUP=1
        IF((BITPIX.EQ.16).OR.(BITPIX.EQ.32).OR.(BITPIX.EQ.-32))THEN
          CALL FTP2DE(IUNIT,GROUP,NCMAX,NCHAN,NSCAN,A,ISTATUS)
          IF(ISTATUS.GT.0) CALL PRINTERROR(ISTATUS)
        ELSEIF(BITPIX.EQ.-64)THEN
          DO I=1,NSCAN
            DO J=1,NCHAN
              AA(J,I)=A(J,I)
            END DO
          END DO
          CALL FTP2DD(IUNIT,GROUP,NCMAX,NCHAN,NSCAN,AA,ISTATUS)
          IF(ISTATUS.GT.0) CALL PRINTERROR(ISTATUS)
        ELSE
          WRITE(*,100)'FATAL ERROR: BITPIX='
          WRITE(*,*)BITPIX
          WRITE(*,101)'Unsupported BITPIX value!'
          CALL FTCLOS(IUNIT,ISTATUS)
          STOP
        END IF
C close the file and free the unit number
        CALL FTCLOS(IUNIT,ISTATUS)
        IF(ISTATUS.GT.0) CALL PRINTERROR(ISTATUS)
C
100     FORMAT(A,$)
101     FORMAT(A)
        END
