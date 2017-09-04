Using the program
=================

General description
-------------------

**cleanest** was inspired by ``CLEAN`` and ``BCLEAN``, the interactive and
automatic, respectively, cosmic ray removal commands belonging to the Starlink
general-purpose data reduction package `FIGARO <http://ascl.net/1411.022>`_.
The need for a program like **cleanest** arose after realizing that cosmic ray
cleaning could be improved by merging the capabilities of interactive and
automatic cosmic ray detection and removal in a single application.

The search of cosmic rays is based on the idea of looking for pixels with a
unexpectedly high signal. For this purpose, the program searches for selected
pixels of the frame, comparing its value with the mean of the neighbour
pixels (those within what is called the *searching square*). In this
statistic, pixels in the same row and column of the pixel under study are
not taken into account (if a pixel has been affected a cosmic ray, it is more
likely that its closest pixels have also been affected by the same cosmic ray).
 
Three initial searching criteria are available:

- ``SIGCR``: the signal in the pixel must exceed the mean value by ``SIGCR``
  times the r.m.s. in the searching square.

- ``MINVCR``: the signal in the pixel must exceed the mean value by ``MINVCR``.

- ``FACTCR``: the signal in the pixel must exceed the mean value by FACTCR 
  times the mean value.
 
The user can establish any combination of the three searching criteria (note
that if more than one criterium is currently in use, a cosmic ray will be found
if the considered pixel verifies any of the activated criteria).
 
Once a pixel has been detected as suspicious of being affected by a cosmic ray,
the program proceeds with a more sophisticated analysis. In this additional
step, a new size of the searching square is defined. The pixels in the new
searching square are sorted by increasing signal, computing new mean and r.m.s.
values using exclusively those pixels with signal comprised in the range
``[PERCENTILE1,PERCENTILE2]``. Both percentile limits can be modified by the
user.  In this way, pixels with a very high signal (cosmic rays, sky lines), as
well as pixels with too low signal (cool and dead pixels), can be removed from
the statistic. After this point, cosmic rays are searched by using a new
``SIGCR`` criterium called ``SIGTHRESHOLD`` (no matter what the initial
criteria were), starting by the suspicious pixel and then looking over the
eight pixels which surround it. If any of the neighbour pixels is found to be a
cosmic ray, the search continues until no more cosmic rays are found in the
neighbourhood of an already found cosmic ray. With this strategy, all the
pixels affected by a single cosmic ray hit are connected among themselves
(i.e., each one is connected to at least another pixel also affected by cosmic
ray).

Once all the suspiciuos pixels have been detected, their signal is replaced by
interpolated values, using for that purpose neighbour pixels not affected by
cosmic ray.

The presence of spectral features complicates the search for cosmic rays. For
that reason **cleanest** incorporates additional options which help to remove
cosmic rays in long-slit spectroscopic images:
 
- Before computing the mean and r.m.s. in the searching square, the averaged
  spatial and wavelength profiles can be subtracted, which highlights cosmic
  ray pixels with respect to the background signal.
 
- Similar images (to that being cleaned) can be loaded at running time,
  allowing for a dynamical comparison of image regions, which helps to
  discriminate doubtful pixels (e.g. hot pixels versus cosmic rays).


Execution example
-----------------

After installing the program, you can run **cleanest** by executing:

::

   $ cleanest

::

   *****************************************
       Welcome to cleanest (version 5.0)
   -----------------------------------------
   > See documentation in:
   http://cleanest.readthedocs.io/
   *****************************************
   
   >>> INTRODUCE INPUT DATA FRAME:
   Input FITS file [*.*fit*] ? wht070.fits

The first step is to indicate the name of the FITS file containing the image to
be cleaned. Since the program accepts wildcars, hitting ``RETURN`` provides a
list with the existing files at the current directory matching the specified
input.

::

   CFITSIO> BITPIX:           16
   CFITSIO> NAXIS1:         1124
   CFITSIO> NAXIS2:          226
   CFITSIO> OBJECT: DARK
   
   Work with error images (y/n) [n] ? n

An associated error image (containing the expected r.m.s. at each pixel) can
also be interpolated (applying the same interpolation options adopted in the
original image). Note that using this option leads to error correlation! In
this simple example we are not using it.

::

   * DEFAULT OPTIONS:
   SIGCR = 5.0
   SIGTHRESHOLD = 6.0
   FACTCR (not employed)
   MINVCR (not employed)
   SIGCRAUX = 10.0
   Do remove mean x/y direction before looking for C.R.
   Edge size of the Searching Square: 15 pixels
   Plot type 1
   FG and BG do not fixed
   
   Accept default options (y/n) [y] ? y

Default options are shown. The user here can decide whether to modify them or
continue.

::

   Are you using graphic buttons.....(y/n) [y] ? 
   Graphic device #1 (? to see list) [/XServe] ? 
   Graphic device #2 (NONE=EXIT) (? to see list) [NONE] ? 
   >>> palette: heat
   
   ---------------------  MAIN MENU   --------------------------
   start.....- begin automatic detection of C.R. (clean by HAND)
   region....- examination of some pixel region
   window....- change display window edge size
   automatic.- clean automatically
   look......- have a look to the image
   options...- change searching options
   save......- save current image
   histogram1- create DATA histogram
   histogram2- create SIGMA histogram
   top1000...- search the top 1000 in SIGMA
   plotsp3d  - emulate plotsp3d program
   QUIT......- end of program
   -------------------------------------------------------------
    
   NOTE: remember that # reverses data!!!
 
After specifying the PGPLOT graphic device, the graphic window opens at the
main menu, which provides the options briefly explained in the terminal.  Each
option is accesible through a graphic button that can be activated either by
clicking it with the mouse cursor or by pressing the key that appears between
brackets in the button label.

.. image:: images/main_menu.png
   :width: 100%
   :align: center


