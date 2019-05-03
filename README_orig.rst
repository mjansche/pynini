Pynini is a Python extension module which allows the user to compile, optimize, and apply grammar rules. Rules can be compiled into weighted finite state transducers, pushdown transducers, or multi-pushdown transducers. For general information and a detailed tutorial, see `pynini.opengrm.org <http://pynini.opengrm.org>`__.

Pynini is developed by `Kyle Gorman <mailto:kbg@google.com>`__. If you use Pynini in your research, we would appreciate if you cite the following paper:

    K. Gorman. 2016. `Pynini: A Python library for weighted finite-state grammar compilation <http://openfst.cs.nyu.edu/twiki/pub/GRM/Pynini/pynini-paper.pdf >`__. In *Proc. ACL Workshop on Statistical NLP and Weighted Automata*, 75-80.

Dependencies
------------

Pynini depends on:

-  A standards-compliant C++ 11 compiler (GCC >= 4.8 or Clang >= 700)
-  The most recent version of `OpenFst <http://openfst.org>`__ (at the time of
   writing, 1.6.2) built with the ``far``, ``pdt``, ``mpdt``, and ``script``
   extensions (i.e., built with ``./configure --enable-grm``) and headers
-  A recent version of `re2 <http:://github.com/google/re2>`__ (at the time of
   writing, ``ae9cb49a2e2ba95de4f0c6ec5a8afd039996d2c7``) and headers
-  `Python 2.7 <https://www.python.org>`__ and headers

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
