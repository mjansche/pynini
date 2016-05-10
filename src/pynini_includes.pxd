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
from libcpp.utility cimport pair
from libcpp.vector cimport vector

from basictypes cimport int32
from basictypes cimport int64

from fst cimport FstClass
from fst cimport MutableFstClass
from fst cimport QueueType
from fst cimport ReplaceOptions
from fst cimport SymbolTable
from fst cimport WeightClass

from libcpp.string cimport string

from fst_util cimport TokenType


ctypedef pair[int64, int64] LabelPair
ctypedef pair[string, const FstClass *] StringFstClassPair


cdef extern from "<fst/extensions/mpdt/mpdtlib.h>" namespace "fst" nogil:


  cdef cppclass MPdtComposeOptions:

    MPdtComposeOptions(bool, PdtComposeFilter)

  cdef cppclass MPdtExpandOptions:

    MPdtExpandOptions(bool, bool)


cdef extern from "<fst/extensions/mpdt/mpdtscript.h>" \
    namespace "fst::script" nogil:

    void MPdtCompose(const FstClass &, const FstClass &,
                     const vector[LabelPair] &, const vector[int64] &,
                     MutableFstClass *, const MPdtComposeOptions &, bool)

    void MPdtExpand(const FstClass &, const vector[LabelPair] &,
                    const vector[int64] &, MutableFstClass *,
                    const MPdtExpandOptions &)

    void MPdtReverse(const FstClass &, const vector[LabelPair] &,
                     vector[int64] *, MutableFstClass *)


cdef extern from "<fst/extensions/mpdt/read_write_utils.h>" \
    namespace "fst" nogil:

  bool ReadLabelTriples[L](const string &, vector[pair[L, L]] *, vector[L] *,
                           bool)

  bool WriteLabelTriples[L](const string &, const vector[pair[L, L]] &,
                            const vector[L] &)


cdef extern from "<fst/extensions/pdt/pdtlib.h>" namespace "fst" nogil:

  cdef cppclass PdtComposeOptions:

    PdtComposeOptions(bool, PdtComposeFilter)

  enum PdtComposeFilter:
    PAREN_FILTER
    EXPAND_FILTER
    EXPAND_PAREN_FILTER

  enum PdtParserType:
    PDT_LEFT_PARSER
    PDT_LEFT_SR_PARSER

cdef extern from "<fst/extensions/pdt/pdtscript.h>" \
    namespace "fst::script" nogil:

  void PdtCompose(const FstClass &, const FstClass &,
                  const vector[LabelPair] &, MutableFstClass *,
                  const PdtComposeOptions &, bool)

  cdef cppclass PdtExpandOptions:

    PdtExpandOptions(bool, bool, const WeightClass &)

  void PdtExpand(const FstClass &, const vector[LabelPair] &,
                 MutableFstClass *, const PdtExpandOptions &)

  void PdtReverse(const FstClass &, const vector[LabelPair] &,
                  MutableFstClass *)

  cdef cppclass PdtShortestPathOptions:

    PdtShortestPathOptions(QueueType, bool, bool)

  void PdtShortestPath(const FstClass &, const vector[LabelPair] &,
                       MutableFstClass *, const PdtShortestPathOptions &)


cdef extern from "<fst/fstlib.h>" namespace "fst" nogil:

  bool ReadLabelPairs[L](const string &, vector[pair[L, L]] *, bool)

  bool WriteLabelPairs[L](const string &, const vector[pair[L, L]] &)


cdef extern from "cdrewrite.h" \
    namespace "fst" nogil:

  enum CDRewriteDirection:
    LEFT_TO_RIGHT
    RIGHT_TO_LEFT
    SIMULTANEOUS

  enum CDRewriteMode:
    OBLIGATORY
    OPTIONAL


cdef extern from "pynini_cdrewrite.h" \
    namespace "fst::script" nogil:

  void PyniniCDRewrite(const FstClass &, const FstClass &,
                       const FstClass &, const FstClass &,
                       MutableFstClass *, CDRewriteDirection, CDRewriteMode)


cdef extern from "pynini_replace.h" \
    namespace "fst::script" nogil:

  void PyniniReplace(const FstClass &, const vector[StringFstClassPair],
                     MutableFstClass *, const ReplaceOptions &)

  void PyniniPdtReplace(const FstClass &, const vector[StringFstClassPair] &,
                        MutableFstClass *, const vector[LabelPair] *,
                        PdtParserType)


cdef extern from "pynini_stringify.h" \
    namespace "fst::script" nogil:

  bool PyniniStringify(const FstClass &, TokenType,
                       const SymbolTable *, string *)

