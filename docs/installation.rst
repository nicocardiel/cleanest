Installation
============

Requirements
------------

**cleanest** is written in Fortran 77 and it has been developed in a linux
machine. The installation procedure should be trivial for most users. The only
requirement is to have a relatively recent linux distribution or Mac OS X
version, including a fortran compiler (e.g. ``g77`` or ``gfortran``) and the
GNU ``autotools``.

Before you install **cleanest**, make sure that ``CFITSIO``, as well as the
graphical library ``PGPLOT``, are already installed in your computer. Some
details about how I do typically install these libraries are given here for
`CFITSIO <https://guaix.fis.ucm.es/~ncl/howto/howto-cfitsio>`_ and `PGPLOT
<https://guaix.fis.ucm.es/~ncl/howto/howto-pgplot>`_.

Installation of **cleanest**
----------------------------

To install **cleanest** you need to perform the following steps:

1.- Download the latest version of the code from github:

::

   $ git clone https://github.com/nicocardiel/cleanest

2.- Install the program by executing:

::

   $ cd cleanest 
   $ ./autogen.sh
   $ ./configure

.. note:: Mac users can easily indicate a different Fortran compiler using, for
   example: 

   ``./configure F77=gfortran-mp-13 CC=gcc-mp-13``.

   Note that the associated C compiler is also provided (this helps configure
   to find PGPLOT).

.. note:: If you find problems detecting PGPLOT, you can help
   ``configure`` by setting the expected location. For example:

   ::

      $ ./configure LDFLAGS="-L/opt/local/lib"

.. warning:: Since Fortran 77 statically declares the dimensions of the arrays 
   at compilation time, you may need to declare the maximum size of the
   expected arrays (``NXMAX`` in the horizontal direction and ``NYMAX`` in the
   vertical direction) while running ``configure``:
   
   ::

      $ ./configure NXMAX=4096 NYMAX=4096

3.- After successfully executing configure, the system is ready to proceed with
the actual compilation of the code:

::

   $ make

If you get an error installing the software, check that the fortran compiler
you are using to install ``PGPLOT`` and to compile **cleanest** is the same. If
this is not the case, force the compiler to be the same by indicating it when
executing configure with the help of the paramter ``F77``. For example, if
``PGPLOT`` was installed using the g95 compiler, execute:

::

   $ make clean
   $ ./configure F77=g95
   $ make

4.- Finally, you must finish the installation procedure by placing the
executable file in its corresponding directory. If you are
installing the software in the by default directory (``/usr/local/...``), you
need root privileges:

::

   $ sudo make install

or

::

   $ su
   # make install

5.- You can optionally clean the intermediate object files generated during the
compilation procedure:

::

   $ make clean
