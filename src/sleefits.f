C Version 31-August-2017                                       File: sleefits.f
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
C Subrutina para leer una imagen
        SUBROUTINE SLEEFITS(A,NSCAN,NCHAN,INFILE,BITPIX,OBJECT,EXTNUM)
        IMPLICIT NONE
        INCLUDE 'dimensions.inc'
        REAL A(NCMAX,NSMAX)
        INTEGER NSCAN,NCHAN
        CHARACTER*255 INFILE
        INTEGER BITPIX
        CHARACTER*255 OBJECT
        INTEGER EXTNUM
C funciones auxiliares
        INTEGER READI
        INTEGER TRUEBEG
        INTEGER TRUELEN
        INTEGER SYSTEMFUNCTION
        CHARACTER*255 READC
C variables locales
        INTEGER ISYSTEM
        INTEGER HDUTYPE
        INTEGER JROW(NCMAX)
        INTEGER I,J
        INTEGER L1,L2
        INTEGER FIRSTPIX
        INTEGER ISTATUS,IREADWRITE,IUNIT
        INTEGER BLOCKSIZE,NULLVAL
        INTEGER NFOUND
        INTEGER NAXIS_(0:2)                                !OJO: el limite es 2
        REAL FROW(NCMAX)
        CHARACTER*50 MYCOMMENT
        CHARACTER*80 CLINEA
        LOGICAL LOOP
        LOGICAL EXTEND,ANYNULL
        LOGICAL LOGFILE,LOK
        LOGICAL LNULL_(NCMAX,NSMAX)
        LOGICAL LANYNULL
        LOGICAL LROW(NCMAX)
C------------------------------------------------------------------------------
C inicializamos variables
        ISTATUS=0               !controla posibles errores durante la ejecucion
        IREADWRITE=0                      !la imagen se abrira en modo READONLY
        NULLVAL=-999
        LANYNULL=.FALSE.
C------------------------------------------------------------------------------
        LOGFILE=.FALSE.
        DO WHILE(.NOT.LOGFILE)
          WRITE(*,100) 'Input FITS file '
          INFILE=READC('*.*fit*','@')
          IF((INDEX(INFILE,'*').NE.0).OR.
     +     (INDEX(INFILE,'?').NE.0))THEN
            L1=TRUEBEG(INFILE)
            L2=TRUELEN(INFILE)
            ISYSTEM=SYSTEMFUNCTION('ls '//INFILE(L1:L2))
          ELSE
            L2=TRUELEN(INFILE)
            INQUIRE(FILE=INFILE,EXIST=LOGFILE)
            IF(.NOT.LOGFILE)THEN
              L1=TRUEBEG(INFILE)
              L2=TRUELEN(INFILE)
              INQUIRE(FILE=INFILE(L1:L2)//'.fits',EXIST=LOGFILE)
              IF(LOGFILE)THEN
                INFILE=INFILE(L1:L2)//'.fits'
                WRITE(*,100) '>>> Assuming fits extension: '
                WRITE(*,101) INFILE(L1:L2)//'.fits'
              ELSE
                CALL LEEONEFILE(INFILE,LOK)
                IF(LOK)THEN
                  L1=TRUEBEG(INFILE)
                  L2=TRUELEN(INFILE)
                  WRITE(*,100) '>>> Using unambiguous file name: '
                  WRITE(*,101) INFILE(L1:L2)
                  LOGFILE=.TRUE.
                ELSE
                  WRITE(*,101) '***ERROR***'
                  WRITE(*,100) '=> This file does not exist.'
                  WRITE(*,100) ' Try again. (press <CR>...)'
                  READ(*,*)
                END IF
              END IF
            END IF
          END IF
        END DO
C------------------------------------------------------------------------------
        IUNIT=80 !ojo, IUNIT=99 entra en conflicto con el Postcript y es
                 !precisamente este numero el que toma esta funcion
C abrimos el fichero
        CALL FTOPEN(IUNIT,INFILE,IREADWRITE,BLOCKSIZE,ISTATUS)
        IF(ISTATUS.GT.0) CALL PRINTERROR(ISTATUS)
C miramos si la imagen tiene extensiones y, en caso afirmativo, preguntamos
C cual queremos leer
        CALL FTGKYL(IUNIT,'EXTEND',EXTEND,MYCOMMENT,ISTATUS)
        IF(ISTATUS.EQ.202)THEN
          EXTEND=.FALSE.
          ISTATUS=0
        END IF
        IF(ISTATUS.GT.0) CALL PRINTERROR(ISTATUS)
        IF(EXTEND)THEN
          WRITE(*,101) '***WARNING***'
          WRITE(*,101) '=> this file contains extensions'
          LOOP=.TRUE.
          DO WHILE(LOOP)
            WRITE(*,100) 'Extension number to be read (1=primary) '
            EXTNUM=READI('1')
            CALL FTMAHD(IUNIT,EXTNUM,HDUTYPE,ISTATUS)
            IF(ISTATUS.GT.0)THEN
              CALL PRINTERROR(ISTATUS)
              ISTATUS=0
            ELSE
              LOOP=.FALSE.
            END IF
          END DO
        ELSE
          EXTNUM=1
          CALL FTMAHD(IUNIT,EXTNUM,HDUTYPE,ISTATUS)
          IF(ISTATUS.GT.0) CALL PRINTERROR(ISTATUS)
        END IF
C leemos BITPIX
        CALL FTGKYJ(IUNIT,'BITPIX',BITPIX,MYCOMMENT,ISTATUS)
        IF(ISTATUS.GT.0) CALL PRINTERROR(ISTATUS)
        WRITE(*,100) 'CFITSIO> BITPIX: '
        WRITE(*,*) BITPIX
C comprobamos que NAXIS=2
        CALL FTGKYJ(IUNIT,'NAXIS',NAXIS_(0),MYCOMMENT,ISTATUS)
        IF(ISTATUS.GT.0) CALL PRINTERROR(ISTATUS)
        IF(NAXIS_(0).GT.2)THEN
          WRITE(*,101) '***FATAL ERROR***'
          WRITE(*,100) '=> NAXIS='
          WRITE(*,*) NAXIS_(0)
          WRITE(*,101) '=> NAXIS > 2'
          CALL FTCLOS(IUNIT,ISTATUS)
          STOP
        ELSEIF(NAXIS_(0).EQ.1)THEN
          NAXIS_(2)=1
        END IF
C leemos NAXIS1 y NAXIS2 [notar que el quinto parametro es NAXIS(1) en lugar
C de NAXIS para asi recuperar NAXIS(1) y NAXIS(2)]
        CALL FTGKNJ(IUNIT,'NAXIS',1,2,NAXIS_(1),NFOUND,ISTATUS)
        IF(ISTATUS.GT.0) CALL PRINTERROR(ISTATUS)
        WRITE(*,100) 'CFITSIO> NAXIS1: '
        WRITE(*,*) NAXIS_(1)
        WRITE(*,100) 'CFITSIO> NAXIS2: '
        WRITE(*,*) NAXIS_(2)
        IF(NAXIS_(1).GT.NCMAX)THEN
          WRITE(*,100) 'NAXIS(1), NCMAX: '
          WRITE(*,*) NAXIS_(1),NCMAX
          WRITE(*,101) '* FATAL ERROR in subroutine LEEFITS:'
          WRITE(*,101) 'NAXIS(1) > NCMAX'
          CALL FTCLOS(IUNIT,ISTATUS)
          STOP
        END IF
        IF(NAXIS_(2).GT.NSMAX)THEN
          WRITE(*,100) 'NAXIS(2), NSMAX: '
          WRITE(*,*) NAXIS_(2),NSMAX
          WRITE(*,101) '* FATAL ERROR in subroutine LEEFITS:'
          WRITE(*,101) 'NAXIS(2) > NSMAX'
          CALL FTCLOS(IUNIT,ISTATUS)
          STOP
        END IF
C leemos OBJECT
        CALL FTGKYS(IUNIT,'OBJECT',OBJECT,MYCOMMENT,ISTATUS)
        IF(ISTATUS.EQ.0)THEN
          WRITE(*,100) 'CFITSIO> OBJECT: '
          WRITE(*,101) OBJECT(1:TRUELEN(OBJECT))
        ELSE
          OBJECT='OBJECT undefined'
          ISTATUS=0
        END IF
C leemos la imagen
        IF(BITPIX.EQ.16)THEN
          DO I=1,NAXIS_(2)
            FIRSTPIX=(I-1)*NAXIS_(1)+1
            CALL FTGPFJ(IUNIT,1,FIRSTPIX,NAXIS_(1),JROW(1),LROW(1),
     +       ANYNULL,ISTATUS)
            DO J=1,NAXIS_(1)
              A(J,I)=REAL(JROW(J))
            END DO
            IF(ANYNULL)THEN
              DO J=1,NAXIS_(1)
                LNULL_(J,I)=LROW(J)
                if(lnull_(j,i)) print*,j,i,jrow(j)
              END DO
              LANYNULL=.TRUE.
            END IF
          END DO
        ELSEIF((BITPIX.EQ.32).OR.(BITPIX.EQ.-32).OR.(BITPIX.EQ.-64))THEN
          DO I=1,NAXIS_(2)
            FIRSTPIX=(I-1)*NAXIS_(1)+1
            CALL FTGPFE(IUNIT,1,FIRSTPIX,NAXIS_(1),FROW(1),LROW(1),
     +       ANYNULL,ISTATUS)
            DO J=1,NAXIS_(1)
              A(J,I)=FROW(J)
            END DO
            IF(ANYNULL)THEN
              DO J=1,NAXIS_(1)
                LNULL_(J,I)=LROW(J)
                if(lnull_(j,i)) print*,j,i,frow(j)
              END DO
              LANYNULL=.TRUE.
            END IF
          END DO
        ELSE
          WRITE(*,100) 'FATAL ERROR in subroutine LEEFITS: BITPIX ='
          WRITE(*,*) BITPIX
          CALL FTCLOS(IUNIT,ISTATUS)
          STOP
        END IF
C cerramos el fichero
        CALL FTCLOS(IUNIT,ISTATUS)
C chequeamos si se ha producido algun error
        IF(ISTATUS.GT.0)THEN
          CALL PRINTERROR(ISTATUS)
        END IF
C------------------------------------------------------------------------------
777     CONTINUE
        NCHAN=NAXIS_(1)
        NSCAN=NAXIS_(2)
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
        END
C
C******************************************************************************
C
        SUBROUTINE PRINTERROR(ISTATUS)
C Print out the FITSIO error messages to the user
        INTEGER ISTATUS
        CHARACTER ERRTEXT*30,ERRMESSAGE*80
C Check if status is OK (no error); if so, simply return
        IF(ISTATUS.LE.0) RETURN
C Get the text string which describes the error
        CALL FTGERR(ISTATUS,ERRTEXT)
        WRITE(*,'(A,$)') 'FITSIO Error Status = '
        WRITE(*,*) ISTATUS
        WRITE(*,'(A)') ERRTEXT
C Read and print out all the error messages on the FITSIO stack
        CALL FTGMSG(ERRMESSAGE)
        DO WHILE(ERRMESSAGE.NE.' ')
          WRITE(*,'(A)') ERRMESSAGE
          CALL FTGMSG(ERRMESSAGE)
        END DO
        END
