# cleanest

This program was initially developed as part of the `Reduceme` package
(https://github.com/nicocardiel/reduceme). Due to historical reasons, `Reduceme`
images are not stored as FITS files. For that reason, and in order to
facilitate the use of cleanest without the need to convert the images to/from
`Reduceme` to FITS format, cleanest has been detached from the `Reduceme` package
and converted in a stand-alone program.


This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3 of the License, or (at your option) any
later version.

- Maintainer: Nicolás Cardiel, cardiel@ucm.es
- Webpage (documentation): http://cleanest.readthedocs.io/
- Webpage (source): https://github.com/nicocardiel/cleanest

Installation example (Mac OSX):
```
$ ./autogen.sh
$ ./configure F77=gfortran-mp-6
$ make
$ sudo make install
```
