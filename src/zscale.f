C Version 31-August-2017                                         File: zscale.f
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
C Calcula los cortes z1 y z2 empleando una escala ZSCALE al estilo Iraf. Para
C que el cálculo sea rápido, sólo se emplean, como máximo, NZMAX píxels de la 
C imagen. También se calcula la media y la desviación típica.
C------------------------------------------------------------------------------
!From the DISPLAY help page:
!
!
!ZSCALE ALGORITHM
!    The zscale algorithm is designed to display the  image  values  near
!    the  median  image  value  without  the  time  consuming  process of
!    computing a full image histogram.  This is particularly  useful  for
!    astronomical  images  which  generally  have a very peaked histogram
!    corresponding to  the  background  sky  in  direct  imaging  or  the
!    continuum in a two dimensional spectrum.
!
!
!    The  sample  of pixels, specified by values greater than zero in the
!    sample mask zmask or by an  image  section,  is  selected  up  to  a
!    maximum  of nsample pixels.  If a bad pixel mask is specified by the
!    bpmask parameter then any pixels with mask values which are  greater
!    than  zero  are not counted in the sample.  Only the first pixels up
!    to the limit are selected where the order is by line beginning  from
!    the  first line.  If no mask is specified then a grid of pixels with
!    even spacing along lines and columns that  make  up  a  number  less
!    than or equal to the maximum sample size is used.
!
!
!    If  a  contrast of zero is specified (or the zrange flag is used and
!    the image does not have a  valid  minimum/maximum  value)  then  the
!    minimum  and maximum of the sample is used for the intensity mapping
!    range.
!
!
!    If the contrast  is  not  zero  the  sample  pixels  are  ranked  in
!    brightness  to  form  the  function  I(i) where i is the rank of the
!    pixel and I is its value.  Generally the midpoint of  this  function
!    (the  median) is very near the peak of the image histogram and there
!    is a well defined slope about the midpoint which is related  to  the
!    width  of the histogram.  At the ends of the I(i) function there are
!    a few very bright and dark pixels due to objects and defects in  the
!    field.   To  determine  the  slope  a  linear  function  is fit with
!    iterative rejection;
!
!
!            I(i) = intercept + slope * (i - midpoint)
!
!
!    If more than half of the points are rejected then there is  no  well
!    defined  slope  and  the full range of the sample defines z1 and z2.
!    Otherwise the endpoints of the linear function  are  used  (provided
!    they are within the original range of the sample):
!
!
!            z1 = I(midpoint) + (slope / contrast) * (1 - midpoint)
!            z2 = I(midpoint) + (slope / contrast) * (npoints - midpoint)
!
!
!    As  can  be  seen,  the parameter contrast may be used to adjust the
!    contrast produced by this algorithm.
!
C------------------------------------------------------------------------------
        SUBROUTINE ZSCALE(IMAGEN,NX1,NX2,NY1,NY2,Z1,Z2)
        IMPLICIT NONE
        INCLUDE 'dimensions.inc'
C
        REAL IMAGEN(NCMAX,NSMAX)
        INTEGER NX1,NX2,NY1,NY2
        REAL Z1,Z2
C
        INTEGER NZMAX
        PARAMETER (NZMAX=10000)
C
        REAL FMEAN0
        REAL FMEDIAN1
C
        INTEGER I,J
        INTEGER K,KK,KTOT
        INTEGER NPIXTOT
        REAL PIXEL(NCMAX*NSMAX)  !imagen auxiliar
        REAL FMEAN,FSIGMA
        REAL FSTEP
        REAL FMEDIAN
        REAL ZSLOPE
        LOGICAL LOOP
C------------------------------------------------------------------------------
C generamos un array unidimensional con los valores de la imagen
        K=0
        DO I=NY1,NY2
          DO J=NX1,NX2
            K=K+1
            PIXEL(K)=IMAGEN(J,I)
          END DO
        END DO
        NPIXTOT=K
C
        FMEAN=FMEAN0(NPIXTOT,PIXEL,FSIGMA)
        IF(NPIXTOT.EQ.1)THEN
          Z1=IMAGEN(NX1,NY1)-1.0
          Z2=IMAGEN(NX1,NY1)+1.0
          RETURN
        END IF
C si la imagen es grande, usamos como mucho NZMAX pixels
        IF(NPIXTOT.GT.NZMAX)THEN
          FSTEP=REAL(NPIXTOT)/REAL(NZMAX)         !este número es mayor que uno
          LOOP=.TRUE.
          K=0
          DO WHILE(LOOP)
            K=K+1
            KK=NINT(FSTEP*REAL(K))
            IF(KK.LT.NPIXTOT)THEN
              PIXEL(K)=PIXEL(KK)
            ELSE
              LOOP=.FALSE.
            END IF
          END DO
          KTOT=K-1
        ELSE
          KTOT=NPIXTOT
        END IF
C ordenamos por señal y calculamos mediana
        FMEDIAN=FMEDIAN1(KTOT,PIXEL)
C estimamos la pendiente usando solo dos pixels que delimitan el 25% de los
C pixels centrales (en lugar de ajustar una recta iterativamente, como hace
C Iraf)
        ZSLOPE=(PIXEL(5*KTOT/8)-PIXEL(3*KTOT/8))/(0.25*REAL(KTOT))
        Z1=FMEDIAN-(ZSLOPE*REAL(KTOT)/2.0)/0.25
        Z1=AMAX1(Z1,PIXEL(1))
        Z2=FMEDIAN+(ZSLOPE*REAL(KTOT)/2.0)/0.25
        Z2=AMIN1(Z2,PIXEL(KTOT))
        WRITE(*,100) '>>> z1= '
        WRITE(*,*) Z1
        WRITE(*,100) '>>> z2= '
        WRITE(*,*) Z2
C
100     FORMAT(A,$)
        END
