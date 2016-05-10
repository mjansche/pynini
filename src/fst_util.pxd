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


from libcpp cimport bool

from fst cimport FstClass
from fst cimport MutableFstClass
from fst cimport SymbolTable
from fst cimport WeightClass
from libcpp.string cimport string


cdef extern from "crossproductscript.h" \
    namespace "fst::script" nogil:

  void CrossProduct(const FstClass &, const FstClass &, MutableFstClass *)

  void OptimizeStringCrossProduct(MutableFstClass *)


cdef extern from "optimizescript.h" \
    namespace "fst::script" nogil:

  void Optimize(MutableFstClass *, bool)

  void OptimizeAcceptor(MutableFstClass *, bool)

  void OptimizeTransducer(MutableFstClass *, bool)


cdef extern from "pathsscript.h" \
    namespace "fst::script" nogil:

  enum TokenType:
    SYMBOL
    BYTE
    UTF8

  cdef cppclass StringPathsClass:

    StringPathsClass(const FstClass &, TokenType, const SymbolTable *,
                     const SymbolTable *)

    bool Done()

    bool Error()

    string IString()

    void Next()

    string OString()

    void Reset()

    WeightClass Weight()


cdef extern from "repeatscript.h" \
    namespace "fst::script" nogil:

  void Repeat(MutableFstClass *, int, int)


cdef extern from "stringcompilescript.h" \
    namespace "fst::script" nogil:

  bool CompileByteString(const string &, const WeightClass &,
                         MutableFstClass *)

  bool CompileUTF8String(const string &, const WeightClass &,
                         MutableFstClass *)

  bool CompileBracketedByteString(const string &, const WeightClass &,
                                  MutableFstClass *)

  bool CompileBracketedUTF8String(const string &, const WeightClass &,
                                  MutableFstClass *)


cdef extern from "merge.h" namespace "fst":

  enum MergeSymbolsType:
    MERGE_INPUT_SYMBOLS
    MERGE_OUTPUT_SYMBOLS
    MERGE_INPUT_AND_OUTPUT_SYMBOLS
    MERGE_LEFT_OUTPUT_AND_RIGHT_INPUT_SYMBOLS


cdef extern from "mergescript.h" \
    namespace "fst::script" nogil:

  bool MergeSymbols(MutableFstClass *, MutableFstClass *, MergeSymbolsType)

