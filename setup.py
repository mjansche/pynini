# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# Copyright 2016 and onwards Google, Inc.
#
# For general information on the Pynini grammar compilation library, see
# pynini.opengrm.org.


from setuptools import Extension
from setuptools import setup


# These extensions are expected to work anywhere with a:
#
# * Standards-compliant C++ 11 compiler (GCC >= 4.8 or Clang >= 700).
# * OpenFst 1.5.1 (http://openfst.org) built with the "far", "pdt", and "mpdt"
#   extensions.
# * re2 (http:://github.com/google/ee2).
# * Python 2.7 or better with "development" headers (though it should work with
#   Python 3.5 with minimal modifications).


COMPILE_ARGS = ["-std=c++11", "-Wno-unused-function", "-funsigned-char"]

pywrapfst = Extension(name="pywrapfst", language="c++",
                      extra_compile_args=COMPILE_ARGS,
                      libraries=["fstfarscript", "fstfar", "fstscript",
                                 "fst", "m", "dl"],
                      sources=["src/pywrapfst.cc"])

pynini = Extension(name="pynini", language="c++",
                   extra_compile_args=COMPILE_ARGS,
                   libraries=["re2",
                              "fstfarscript",
                              "fstpdtscript",
                              "fstmpdtscript",
                              "fstscript",
                              "fstfar",
                              "fst",
                              "m",
                              "dl"],
                   sources=["src/stringmapscript.cc",
                            "src/stringfile.cc",
                            "src/stringcompilescript.cc",
                            "src/stringcompile.cc",
                            "src/repeatscript.cc",
                            "src/pynini_stringify.cc",
                            "src/pynini_replace.cc",
                            "src/pynini_cdrewrite.cc",
                            "src/pynini.cc",
                            "src/pathsscript.cc",
                            "src/optimizescript.cc",
                            "src/mergescript.cc",
                            "src/merge.cc",
                            "src/lenientlycomposescript.cc",
                            "src/gtl.cc",
                            "src/crossproductscript.cc",
                            "src/containmentscript.cc"])

setup(
    name="pynini", version="1.1",
    description="Finite-state grammar compilation library",
    author="Kyle Gorman", author_email="kbg@google.com",
    url="http://pynini.opengrm.org/",
    keywords=["natural language processing", "speech recognition",
              "machine learning"],
    classifiers=["Programming Language :: Python :: 2.7",
                 "Development Status :: 4 - Beta",
                 "Environment :: Other Environment",
                 "Environment :: Console",
                 "Intended Audience :: Developers",
                 "License :: OSI Approved :: Apache Software License",
                 "Operating System :: OS Independent",
                 "Topic :: Software Development :: Libraries :: Python Modules",
                 "Topic :: Text Processing :: Linguistic",
                 "Topic :: Scientific/Engineering :: Artificial Intelligence",
                 "Topic :: Scientific/Engineering :: Mathematics"],
    ext_modules=[pywrapfst, pynini],
    test_suite="pynini_test")
