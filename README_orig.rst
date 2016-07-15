Pynini is a Python extension module which allows the user to compile, optimize, and apply grammar rules. Rules can be compiled into weighted finite state transducers (FSTs), pushdown transducers, or multi-pushdown transducers. For general information and a detailed tutorial, see `pynini.opengrm.org <http://pynini.opengrm.org>`__.

Pynini is developed by Kyle Gorman (kbg@google.com).

Dependencies
------------

Pynini depends on:

-  A standards-compliant C++ 11 compiler (GCC >= 4.8 or Clang >= 700)
-  [OpenFst 1.5.1](http://openfst.org) built with the ``far``, ``pdt``,
   and ``mpdt`` extensions (i.e.,
   ``./configure --enable-far --enable-pdt --enable-mpdt``)
-  `re2 <http:://github.com/google/re2>`__
-  `Python 2.7 <https://www.python.org>`__

It is regularly tested in two environments:

-  Ubuntu Linux 14.04 LTS on x86\_64, GCC 4.8, Python 2.7.6
-  Mac OS X 10.11 ("El Capitan"), XCode 7.1, Python 2.7.10

Installation instructions
-------------------------

Execute ``python setup.py install``. Depending on your environment, you may need to be superuser while running this command for installation to complete.

To confirm successful installation, execute ``python setup.py test``.

Python 3 support
----------------

Pynini is not regularly tested using Python 3 but it should work with little to no modification, assuming you have Cython (a Python-to-C transpiler). Minimally, you will want to regenerate ``pywrapfst.cc`` and ``pynini.cc`` (in the ``src`` directory) like so:

::

    cython -3 --cplus -o pywrapfst.cc pywrapfst.pyx
    cython -3 --cplus -o pynini.cc pynini.pyx

and then (re)compile as described above. There are still some warts related to the switch from byte to Unicode strings.

License
-------

Pynini is released under the Apache license. See ``LICENSE`` for more information.

Interested in contributing?
---------------------------

See ``CONTRIBUTING`` for more information.

Mandatory disclaimer
--------------------

This is not an official Google product.
