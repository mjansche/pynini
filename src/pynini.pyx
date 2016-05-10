#cython: nonecheck=True
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


"""Pynini: finite-state grammar compilation for Python.

Pynini is an experimental Python module which implements compilation of
grammars as finite-state transducers (FSTs).

This module is designed to be wildcard-import-safe.
"""


## IMPLEMENTATION.

## Cython imports. Sorry. There are a lot.

from cython.operator cimport address as addr       # &foo
from cython.operator cimport dereference as deref  # *foo
from cython.operator cimport preincrement as inc   # ++foo

from libcpp cimport bool

from libcpp.vector cimport vector

from basictypes cimport int32
from basictypes cimport int64
from basictypes cimport uint64

from fst cimport CLOSURE_PLUS
from fst cimport CLOSURE_STAR

from fst cimport Closure
from fst cimport Equal
from fst cimport FstClass
from fst cimport LabelFstClassPair
from fst cimport MutableFstClass
from fst cimport ReplaceLabelType
from fst cimport ReplaceOptions
from fst cimport SymbolTable
from fst cimport VectorFstClass
from fst cimport WeightClass

from fst cimport kAcceptor
from fst cimport kDelta
from fst cimport kError
from fst cimport kString

from libcpp.string cimport string

from pywrapfst cimport _Fst
from pywrapfst cimport _MutableFst
from pywrapfst cimport _SymbolTable
from pywrapfst cimport FarReader
from pywrapfst cimport FarWriter
from pywrapfst cimport Weight


from pywrapfst cimport _get_WeightClass_or_One
from pywrapfst cimport \
    _get_WeightClass_or_Zero
from pywrapfst cimport _get_queue_type
from pywrapfst cimport _get_replace_label_type
from pywrapfst cimport _read_Fst

from pywrapfst cimport tostring

# C++ code from fst_util.

from fst_util cimport CompileBracketedByteString
from fst_util cimport CompileBracketedUTF8String
from fst_util cimport CompileSymbolString
from fst_util cimport CrossProduct
from fst_util cimport MergeSymbols
from fst_util cimport Optimize
from fst_util cimport OptimizeStringCrossProduct
from fst_util cimport Repeat
from fst_util cimport StringPathsClass

from fst_util cimport MergeSymbolsType
from fst_util cimport MERGE_INPUT_AND_OUTPUT_SYMBOLS
from fst_util cimport \
    MERGE_LEFT_OUTPUT_AND_RIGHT_INPUT_SYMBOLS

from fst_util cimport TokenType
from fst_util cimport BYTE
from fst_util cimport SYMBOL
from fst_util cimport UTF8

# C++ code for Pynini not from fst_util.

from pynini_includes cimport LabelPair
from pynini_includes cimport StringFstClassPair

from pynini_includes cimport MPdtCompose
from pynini_includes cimport MPdtComposeOptions
from pynini_includes cimport MPdtExpand
from pynini_includes cimport MPdtExpandOptions
from pynini_includes cimport MPdtReverse
from pynini_includes cimport PdtCompose
from pynini_includes cimport PdtComposeOptions
from pynini_includes cimport PdtExpand
from pynini_includes cimport PdtExpandOptions
from pynini_includes cimport PdtReverse
from pynini_includes cimport PdtShortestPath
from pynini_includes cimport \
    PdtShortestPathOptions
from pynini_includes cimport PyniniCDRewrite
from pynini_includes cimport PyniniPdtReplace
from pynini_includes cimport PyniniReplace
from pynini_includes cimport PyniniStringify
from pynini_includes cimport ReadLabelPairs
from pynini_includes cimport ReadLabelTriples
from pynini_includes cimport WriteLabelPairs
from pynini_includes cimport WriteLabelTriples

from pynini_includes cimport CDRewriteDirection
from pynini_includes cimport LEFT_TO_RIGHT
from pynini_includes cimport RIGHT_TO_LEFT
from pynini_includes cimport SIMULTANEOUS

from pynini_includes cimport CDRewriteMode
from pynini_includes cimport OBLIGATORY
from pynini_includes cimport OPTIONAL

from pynini_includes cimport PdtComposeFilter
from pynini_includes cimport PAREN_FILTER
from pynini_includes cimport EXPAND_FILTER
from pynini_includes cimport EXPAND_PAREN_FILTER

from pynini_includes cimport PdtParserType
from pynini_includes cimport PDT_LEFT_PARSER
from pynini_includes cimport PDT_LEFT_SR_PARSER


# Python imports needed for implementation.


import functools

from pywrapfst import FstArgError
from pywrapfst import FstIOError
from pywrapfst import FstOpError

import pywrapfst

import logging


# Constants


cdef uint64 kAcceptorAndString = kAcceptor | kString


# Custom exceptions.


class FstStringCompilationError(FstArgError, ValueError):

  pass


class FstSymbolTableMergeError(FstOpError, ValueError):

  pass


# Helper functions.


cdef TokenType _get_token_type(string token_type) except *:
  """Matches string with the appropriate TokenType enum value.

  This function takes a string argument and returns the matching TokenType
  enum value used by StringPathsClass and PyniniStringify.

  Args:
    token_type: A string matching a known token type.

  Returns:
    A TokenType enum value.

  Raises:
    FstArgError: Unknown token type.
  """
  cdef TokenType tt
  if token_type == b"byte":
    tt = BYTE
  elif token_type == b"symbol":
    tt = SYMBOL
  elif token_type == b"utf8":
    tt = UTF8
  else:
    raise FstArgError("Unknown token type: {!r}".format(token_type))
  return tt


cdef PdtComposeFilter _get_pdt_compose_filter(string cf) except *:
  """Matches string with the appropriate PdtComposeFilter enum value.

  Raises:
    FstArgError: Unknown PDT compose filter type.

  This function is not visible to Python users.
  """
  if cf == b"paren":
    return PAREN_FILTER
  if cf == b"expand":
    return EXPAND_FILTER
  if cf == b"expand_paren":
    return EXPAND_PAREN_FILTER
  raise FstArgError("Unknown PDT compose filter type: {!r}".format(cf))


cdef PdtParserType _get_pdt_parser_type(string parser_type) except *:
  """Matches string with the appropriate PdtParserType enum value.

  This function takes a string argument and returns the matching PdtParserType
  enum value used by PyniniPdtReplace.

  Args:
    parser_type: A string matching a known parser type.

  Returns:
    A PdtParserType enum value.

  Raises:
    FstArgError: Unknown PDT parser type.

  This function is not visible to Python users.
  """
  if parser_type == b"left":
    return PDT_LEFT_PARSER
  if parser_type == b"left_sr":
    return PDT_LEFT_SR_PARSER
  raise FstArgError("Unknown PDT parser type: {!r}".format(parser_type))


cdef void _add_parentheses_symbols(MutableFstClass *fst,
                                   const vector[LabelPair] &parens,
                                   bool left) except *:
  """Adds missing parentheses symbols to (M)PDTs.

  Args:
    fst: A pointer to the MutableFstClass to be modified.
    parens: A reference to the underlying parentheses vector.
    left: Was the input FST the left side of a MPDT or PDT composition?

  Raises:
    FstSymbolTableMergeError: Unable to resolve parentheses symbol table
        conflict.
    KeyError.

  This function is not visible to Python users.
  """
  cdef SymbolTable *source_syms
  cdef SymbolTable *sink_syms
  cdef size_t i = 0
  cdef int64 label
  cdef string symbol
  if left:
    source_syms = fst.MutableInputSymbols()
    if source_syms == NULL:
      return
    sink_syms = fst.MutableOutputSymbols()
    if sink_syms == NULL:
      return
  else:
    source_syms = fst.MutableOutputSymbols()
    if source_syms == NULL:
      return
    sink_syms = fst.MutableInputSymbols()
    if sink_syms == NULL:
      return
  for i in xrange(parens.size()):
    label = parens[i].first
    symbol = source_syms.FindSymbol(label)
    if symbol == b"":
      raise KeyError(label)
    if label != sink_syms.AddSymbol(symbol, label):
      raise FstSymbolTableMergeError(
          "Unable to resolve parentheses symbol table conflict")
    label = parens[i].second
    symbol = source_syms.FindSymbol(label)
    if symbol == b"":
      raise KeyError(label)
    if label != sink_syms.AddSymbol(symbol, label):
      raise FstSymbolTableMergeError(
          "Unable to resolve parentheses symbol table conflict")


# Class for FSTs created from within Pynini. It overrides instance methods of
# the superclass which take an FST argument so that it can string-compile said
# argument if it is not yet an FST. It also overloads binary == (equals),
# * (composition), # + (concatenation), and | (union).


cdef class Fst(_MutableFst):

  """
  Fst(arc_type="standard")

  Pynini finite-state transducer class.

  This class wraps a mutable FST and exposes all destructive methods.

  Attributes:
    arc_type: A string indicating the arc type.
    fst_type: A string indicating the FST (container) type.
    input_symbols: The input symbol table, or None if none is set.
    num_states: The number of states.
    output_symbols: The output symbol table, or None if none is set.
    start: The integer state ID for the start state.
    weight_type: A string indicating the weight type.
  """

  def __init__(self, arc_type=b"standard"):
    cdef VectorFstClass *tfst = new VectorFstClass(tostring(arc_type))
    if tfst.Properties(kError, True) == kError:
       raise FstArgError("Unknown arc type: {!r}".format(arc_type))
    self._mfst = self._fst = tfst

  @classmethod
  def read(cls, filename):
    """
    Fst.read(filename)

    Constructs an FST from a file.

    Args:
      filename: The string location of the input file.

     Returns:
       An FST.

     Raises:
       FstIOError: Read failed.
       FstOpError: Read-time conversion failed.
    """
    return _init_Fst_from_MutableFst(_read_Fst(filename, fst_type=b"vector"))

  # This hidden method is called immediately after FST operations to check for
  # the error bit.

  cdef void _check_pynini_op_error(self) except *:
    if self._fst.Properties(kError, True) == kError:
      raise FstOpError("Operation failed")

  cpdef StringPaths paths(self, token_type=b"byte", _SymbolTable isymbols=None,
                          _SymbolTable osymbols=None):
    """
    paths(self, token_type="byte", isymbols=None, osymbols=None)

    Creates iterator over all string paths in an acyclic FST.

    This method returns an iterator over all paths (represented as pairs of
    strings and an associated path weight) through an acyclic FST. This
    operation is only feasible when the FST is acyclic. Depending on the
    requested token type, the arc labels along the input and output sides of a
    path are interpreted as UTF-8-encoded Unicode strings, raw bytes, or a
    concatenation of string labels from a symbol table.

    Note that this method creates an iterator over all paths *at the time
    of creation* and the iterator will not be affected by any mutations to
    the FST after that point.

    Args:
      token_type: A string indicating how arc labels are to be interpreted as
          strings; (interprets arc labels as UTF-8 encoded Unicode strings),
          "byte" (interprets arc labels as byte strings), "symbol" (interprets
          arc labels according to the provided symbol tables).
      isymbols: Input symbol table (ignored unless token_type is "symbol").
      osymbols: Output symbol table (ignored unless token_Type is "symbol").

    Raises:
      FstArgError: Unknown token type.
      FstArgError: FST is not acyclic.

    See also: `StringPaths`. `StringPaths`. `StringPaths`. `StringPaths`.
    """
    return StringPaths(self, token_type, isymbols, osymbols)

  cpdef string stringify(self, token_type=b"byte") except *:
    """
    stringify(self, token_type="byte")

    Creates a Python string from a string FST.

    This method returns the string recognized by the FST as a Python byte or
    Unicode string. This is only well-defined when the FST is an acceptor and a
    "string" FST (meaning that the start state is numbered 0, and there is
    exactly one transition from each state i to each state i + 1, there are no
    other transitions, and the last state is final). Depending on the requested
    token type, the arc labels are interpreted as a UTF-8-encoded Unicode
    string, raw bytes, or as a concatenation of string labels from the output
    symbol table.

    The underlying routine reads only the output labels, so if the FST is
    not an acceptor, it will be treated as the output projection of the FST.

    If "symbol" mode is requested, the separator between symbols is determined
    by the command-line flag --fst_field_separator (by default, space).

    If "symbol" mode is requested but no symbol table is present, the integer
    arc labels will be used instead.

    Args:
      token_type: A string indicating how arc labels are to be interpreted as
          strings; (interprets arc labels as UTF-8 encoded Unicode strings),
          "byte" (interprets arc labels as byte strings), "symbol" (interprets
          arc labels according to FST's symbol tables).

    Returns:
      The Python string accepted by the FST.

    Raises:
      FstArgError: FST is not a string.
      FstArgError: Unknown token type.
    """
    cdef TokenType tt
    token_type = tostring(token_type)
    if token_type == b"byte":
      tt = BYTE
    elif token_type == b"symbol":
      tt = SYMBOL
    elif token_type == b"utf8":
      tt = UTF8
    else:
     raise FstArgError("Unknown token type: {!r}".format(token_type))
    cdef string result
    if not PyniniStringify(deref(self._fst), tt, self._fst.OutputSymbols(),
                           addr(result)):
      raise FstArgError("FST is not a string")
    return result

  # The following all override their definition in _MutableFst.

  cpdef Fst copy(self):
    """
    copy(self)

    Makes a copy of the FST.
    """
    return _init_Fst_from_MutableFst(super(_MutableFst, self).copy())

  def closure(self, int32 lower=0, int32 upper=0):
    """
    closure(self, lower)
    closure(self, lower, upper)

    Computes concatenative closure.

    This operation destructively converts the FST to its concatenative closure.
    If A transduces string x to y with weight w, then the zero-argument form
    `A.closure()` mutates A so that it transduces between empty strings with
    weight 1, transduces string x to y with weight w, transduces xx to yy with
    weight w \otimes w, string xxx to yyy with weight w \otimes w \otimes w
    (and so on).

    When called with two optional positive integer arguments, these act as
    lower and upper bounds, respectively, for the number of cycles through the
    original FST that the mutated FST permits. Therefore, `A.closure(0, 1)`
    mutates A so that it permits 0 or 1 cycles; i.e., the mutated A transduces
    between empty strings or transduces x to y.

    When called with one optional positive integer argument, this argument
    acts as the lower bound, with the upper bound implicitly set to infinity.
    Therefore, `A.closure(1)` performs a mutation roughly equivalent to
    `A.closure()` except that the former does not transduce between empty
    strings.

    The following are the equivalents for the closure-style syntax used in
    Perl-style regular expressions:

    Regexp:\t\tThis method:\t\tCopy shortcuts:

    /x?/\t\tx.closure(0, 1)\t\tx.ques()
    /x*/\t\tx.closure()\t\tx.star()
    /x+/\t\tx.closure(1)\t\tx.plus()
    /x{N}/\t\tx.closure(N, N)
    /x{M,N}/\t\tx.closure(M, N)
    /x{N,}/\t\tx.closure(N)
    /x{,N}/\t\tx.closure(0, N)

    See also `ques`, `star`, `plus`.
    """
    Repeat(self._mfst, lower, upper)
    self._check_pynini_op_error()

  # Closure aliases, which make copies.

  @property
  def plus(self):
    """
    Constructively computes +-closure.

    Returns:
      A +-closure FST.

    See also: `closure`.
    """
    cdef Fst result = self.copy()
    Closure(result._mfst, CLOSURE_PLUS)
    result._check_pynini_op_error()
    return result

  @property
  def ques(self):
    """
    Constructively computes ?-closure.

    Returns:
      A ?-closure FST.

    See also: `closure`.
    """
    cdef Fst result = self.copy()
    Repeat(result._mfst, 0, 1)
    result._check_pynini_op_error()
    return result

  @property
  def star(self):
    """
    Constructively computes *-closure.

    Returns:
      A *-closure FST.

    See also: `closure`.
    """
    cdef Fst result = self.copy()
    Closure(result._mfst, CLOSURE_STAR)
    result._check_pynini_op_error()
    return result


  def concat(self, ifst):
    """
    concat(self, ifst)

    Computes the concatenation (product) of two FSTs.

    This operation destructively concatenates the FST with a second FST. If A
    transduces string x to y with weight a and B transduces string w to v with
    weight b, then their concatenation transduces string xw to yv with weight
    a \otimes b.

    Args:
      ifst: The second input Fst.

    Raises:
      FstSymbolTableMergeError: Unable to resolve symbol table conflict
          without relabeling.
    """
    cdef Fst rhs = _compile_Fst(ifst, arc_type=self.arc_type)
    if not MergeSymbols(self._mfst, rhs._mfst, MERGE_INPUT_AND_OUTPUT_SYMBOLS):
      raise FstSymbolTableMergeError(
          "Unable to resolve symbol table conflict without relabeling")
    self._concat(rhs)

  cpdef void optimize(self, bool compute_props=False):
    """
    optimize(self, compute_props=False)

    Performs a generic optimization of the FST.

    This operation destructively optimizes the FST using epsilon-removal,
    arc-sum mapping, determinization, and minimization (where possible). The
    algorithm is as follows:

    * If the FST is not (known to be) epsilon-free, perform epsilon-removal.
    * Combine identically labeled multi-arcs and sum their weights.
    * If the FST does not have idempotent weights, halt.
    * If the FST is not (known to be) deterministic:
      - If the FST is a (known) acceptor:
        * If the FST is not (known to be) unweighted and/or acyclic, encode
          weights.
      - Otherwise, encode labels and, if the FST is not (known to be)
        unweighted, encode weights.
      - Determinize the FST.
    * Minimize the FST.
    * Decode the FST and combine identically-labeled multi-arcs and sum their
      weights, if the FST was previously encoded.

    By default, FST properties are not computed if they are not already set.

    This optimization may result in a reduction of size (due to epsilon-removal,
    arc-sum mapping, and minimization) and possibly faster composition, but
    determinization (a prerequisite of minimization) may result in an
    exponential blowup in size in the worst case. Judicious use of optimization
    is a bit of a black art.

    Args:
      compute_props: Should unknown FST properties be computed to help choose
        appropriate optimizations?
    """
    Optimize(self._mfst, compute_props)
    self._check_pynini_op_error()

  def union(self, ifst):
    """
    union(self, ifst)

    Computes the union (sum) of two FSTs.

    This operation destructively computes the union (sum) of two FSTs. If A
    transduces string x to y with weight a and B transduces string w to v with
    weight b, then their union transduces x to y with weight a and w to v with
    weight b.

    Args:
      ifst: The second input Fst.

    Raises:
      FstSymbolTableMergeError: Unable to resolve symbol table conflict
          without relabeling.
    """
    cdef Fst rhs = _compile_Fst(ifst, arc_type=self.arc_type)
    if not MergeSymbols(self._mfst, rhs._mfst, MERGE_INPUT_AND_OUTPUT_SYMBOLS):
      raise FstSymbolTableMergeError(
          "Unable to resolve symbol table conflict without relabeling")
    self._union(rhs)

  # Operator overloads.

  def __richcmp__(self, other, int op):
    cdef string arc_type = (self.arc_type if hasattr(self, "arc_type") else
                            other.arc_type)
    cdef Fst lhs = _compile_Fst(self, arc_type=arc_type)
    cdef Fst rhs = _compile_Fst(other, arc_type=arc_type)
    if op == 2:    # ==
      return Equal(deref(lhs._fst), deref(rhs._fst), kDelta)
    elif op == 3:  # !=
      return not Equal(deref(lhs._fst), deref(rhs._fst), kDelta)
    raise NotImplementedError("Operator {} not implemented".format(op))

  # x + y

  def __add__(self, other):
    cdef string arc_type = (self.arc_type if hasattr(self, "arc_type") else
                            other.arc_type)
    cdef Fst lhs = _compile_Fst(self, arc_type=arc_type)
    lhs.concat(other)
    return lhs

  # x - y

  def __sub__(self, other):
    cdef string arc_type = (self.arc_type if hasattr(self, "arc_type") else
                            other.arc_type)
    cdef Fst lhs = _compile_Fst(self, arc_type=arc_type)
    cdef Fst rhs = _compile_Fst(other, arc_type=arc_type)
    if not MergeSymbols(lhs._mfst, rhs._mfst, MERGE_INPUT_AND_OUTPUT_SYMBOLS):
      raise FstSymbolTableMergeError(
          "Unable to resolve symbol table conflict without relabeling")
    return _init_Fst_from_MutableFst(pywrapfst.difference(lhs, rhs))

  # x * y

  def __mul__(self, other):
    cdef string arc_type = (self.arc_type if hasattr(self, "arc_type") else
                            other.arc_type)
    cdef Fst lhs = _compile_Fst(self, arc_type=arc_type)
    cdef Fst rhs = _compile_Fst(other, arc_type=arc_type)
    lhs.arcsort(sort_type="olabel")
    rhs.arcsort(sort_type="ilabel")
    if not MergeSymbols(lhs._mfst, rhs._mfst,
                        MERGE_LEFT_OUTPUT_AND_RIGHT_INPUT_SYMBOLS):
      raise FstSymbolTableMergeError(
          "Unable to resolve symbol table conflict without relabeling")
    lhs = _init_Fst_from_MutableFst(pywrapfst.compose(lhs, rhs))
    if not lhs.num_states:
      logging.warning("Composed FST has no connected states")
    return lhs

  # x | y

  def __or__(self, other):
    cdef string arc_type = (self.arc_type if hasattr(self, "arc_type") else
                            other.arc_type)
    cdef Fst lhs = _compile_Fst(self, arc_type=arc_type)
    lhs.union(other)
    return lhs


# Makes a reference-counted copy, if it's already an FST; otherwise, compiles
# it into an acceptor.


cpdef Fst _compile_Fst(arg, arc_type=b"standard"):
  if not isinstance(arg, Fst):
    return acceptor(arg, arc_type=arc_type)
  else:
    return arg.copy()


# Down-casts a _MutableFst to a Pynini Fst by taking ownership of the underlying
# pointers of the former.


cdef Fst _init_Fst_from_MutableFst(_MutableFst rhs):
  cdef Fst result = Fst.__new__(Fst)
  result._mfst = rhs._mfst
  result._fst = rhs._fst
  rhs._mfst = rhs._fst = NULL
  del rhs
  return result


# Functions for FST compilation.


cpdef Fst acceptor(astring, weight=None, arc_type=b"standard",
                   token_type=b"byte"):
  """
  acceptor(astring, weight=None, arc_type="standard", token_type="byte")

  Creates an acceptor from a string.

  This function creates an FST which accepts its input with a fixed weight
  (defaulting to semiring One).

  Args:
    astring: The input string.
    weight: A Weight or weight string indicating the desired path weight. If
        omitted or null, the path weight is set to semiring One.
    arc_type: An optional string indicating the arc type for the compiled FST.
        This argument is silently ignored if istring and/or ostring is already
        compiled.
    token_type: Either a string indicating how the input string is to be
        encoded as arc labels---one of: utf8" (encodes the strings as UTF-8
        encoded Unicode string), "byte" (encodes the string as raw bytes)---or
        a SymbolTable to be used to encode the string.

    Returns:
      An FST acceptor.

    Raises:
      FstArgError: Unknown token type.
      FstStringCompilationError: String compilation failed.
  """
  cdef Fst result = Fst(tostring(arc_type))
  cdef WeightClass wc = _get_WeightClass_or_One(result.weight_type, weight)
  cdef string _astring = tostring(astring)
  cdef SymbolTable *syms
  try:
    token_type = tostring(token_type)
    if token_type == b"byte":
      if not CompileBracketedByteString(_astring, wc, result._mfst):
        raise FstStringCompilationError("Bytestring compilation failed")
    elif token_type == b"utf8":
      if not CompileBracketedUTF8String(_astring, wc, result._mfst):
        raise FstStringCompilationError("UTF8 string compilation failed")
    else:
      raise FstArgError("Unknown token type: {!r}".format(token_type))
  except (FstArgError, UnicodeDecodeError):
    if isinstance(token_type, pywrapfst._SymbolTable):
      syms = (<SymbolTable *> (<_SymbolTable> token_type)._table)
      if not CompileSymbolString(_astring, wc, deref(syms), result._mfst):
        raise FstStringCompilationError("Symbol string compilation failed")
    else:
      raise
  result._check_pynini_op_error()
  return result


cpdef Fst transducer(istring, ostring, weight=None, arc_type=b"standard",
                     input_token_type=b"byte", output_token_type=b"byte"):
  """
  transducer(istring, ostring, weight=None, arc_type="standard",
             input_token_type="byte", output_token_type=="byte")

  Creates a transducer from a pair of strings or acceptor FSTs.

  This function creates an FST which transduces from the first string to
  the second with a fixed weight (defaulting to semiring One). If one or both
  of the input arguments is already compiled as an FST, the resulting transducer
  is simply the cross-product between the language accepted by the upper and
  lower FSTs.

  Arg:
    istring: The input string, or an acceptor FST representing the upper
        language.
    ostring: The output string, or an acceptor FST representing the upper
        language.
    weight: A Weight or weight string indicating the desired path weight. If
        omitted or null, the path weight is set to semiring One. This argument
        is silently ignored if istring and/or ostring is already compiled.
    arc_type: An optional string indicating the arc type for the compiled FST.
        This argument is silently ignored if istring and/or ostring is already
        compiled.
    input_token_type: Either a string indicating how the input strings are to be
        encoded as arc labels---one of: utf8" (encodes strings as UTF-8 encoded
        Unicode strings), "byte" (encodes strings as raw bytes)---or a
        SymbolTable to be used to encode the string.
    output_token_type: Either a string indicating how the output strings are to be
        encoded as arc labels---one of: utf8" (encodes strings as UTF-8 encoded
        Unicode strings), "byte" (encodes strings as raw bytes)---or a
        SymbolTable to be used to encode the string.

  Returns:
    An FST transducer.

  Raises:
    FstArgError: Unknown token type.
    PyniniStringCompilationError: String compilation failed.
  """
  cdef Fst lower
  cdef Fst upper
  cdef Fst result = Fst(arc_type)
  # Sets up upper language.
  if not isinstance(istring, Fst):
    upper = acceptor(istring, arc_type=arc_type, token_type=input_token_type)
  else:
    upper = istring
  if upper._fst.Properties(kAcceptor, True) != kAcceptor:
    logging.warning("Expecting acceptor or string argument, got a transducer")
  # Sets up lower language, and passes weight.
  if not isinstance(ostring, Fst):
    lower = acceptor(ostring, weight=weight, arc_type=arc_type,
                     token_type=output_token_type)
  else:
    lower = ostring
    if lower._fst.Properties(kAcceptor, True) != kAcceptor:
      logging.warning("Expecting acceptor or string argument, got a transducer")
  # Actually computes cross-product.
  CrossProduct(deref(upper._fst), deref(lower._fst), result._mfst)
  # Optimizes output, if both inputs were strings or string FSTs.
  if (lower._fst.Properties(kAcceptorAndString, True) == kAcceptorAndString and
      upper._fst.Properties(kAcceptorAndString, True) == kAcceptorAndString):
    OptimizeStringCrossProduct(result._mfst)
  result._check_pynini_op_error()
  return result


cpdef Fst cdrewrite(tau, lambda_, rho, sigma_star, direction=b"ltr",
                    mode=b"obl"):
  """
  cdrewrite(tau, lambda, rho, sigma_star, direction="ltr", mode="obl")

  Generates a transducer expressing a context-dependent rewrite rule.

  This operation compiles a transducer representing a context-dependent
  rewrite rule of the form

      phi -> psi / lambda __ rho

  over a finite vocabulary. To apply the resulting transducer, simply compose
  it with an input string or lattice.

  Args:
    tau: A (weighted) transducer representing phi -> psi.
    lambda: An unweighted acceptor representing the left context.
    rho: An unweighted acceptor representing the right context.
    sigma_star: An unweighted acceptor representing the closure over the
        alphabet.
    direction: A string specifying the direction of rule application; one of:
        "ltr" (left-to-right application), "rtl" (right-to-left application),
        or "sim" (simultaneous application).
    mode: A string specifying the mode of rule application; one of: "obl"
        (obligatory application), "opt" (optional application).

  Returns:
    An FST representing the context-dependent rewrite rule.

  Raises:
    FstArgError: Unknown cdrewrite direction type.
    FstArgError: Unknown cdrewrite mode type.
    FstOpError: Operation failed.
  """
  cdef CDRewriteDirection cd
  direction = tostring(direction)
  if direction == b"ltr":
    cd = LEFT_TO_RIGHT
  elif direction == b"rtl":
    cd = RIGHT_TO_LEFT
  elif direction == b"sim":
    cd = SIMULTANEOUS
  else:
    raise FstArgError("Unknown rewrite direction type: "
                         "{!r}".format(direction))
  cdef CDRewriteMode cm
  mode = tostring(mode)
  if mode == b"obl":
    cm = OBLIGATORY
  elif mode == b"opt":
    cm = OPTIONAL
  else:
    raise FstArgError("Unknown rewrite mode type: {!r}".format(mode))
  cdef Fst tau_compiled = _compile_Fst(tau)
  cdef string arc_type = tau_compiled.arc_type
  cdef Fst lambda_compiled = _compile_Fst(lambda_, arc_type)
  cdef Fst rho_compiled = _compile_Fst(rho, arc_type)
  cdef Fst sigma_star_compiled = _compile_Fst(sigma_star, arc_type)
  cdef Fst result = Fst(arc_type)
  PyniniCDRewrite(deref(tau_compiled._fst), deref(lambda_compiled._fst),
                  deref(rho_compiled._fst), deref(sigma_star_compiled._fst),
                  result._mfst, cd, cm)
  result._check_pynini_op_error()
  if not result.num_states:
    logging.warning("Compiled rewrite rule has no connected states")
  return result


def string_map(*pairs, arc_type=b"standard", input_token_type=b"byte",
               output_token_type=b"byte"):
  """
  string_map(*pairs, arc_type="standard", input_token_type="byte",
             output_token_type="byte")

  Creates a transducer that maps between elements of mappings.

  Args:
    *pairs: an iterable of of input-output pairs of strings. If an element is
        a singleton, the identity mapping is used.
    arc_type: A string indicating the arc type.
    input_token_type: A string indicating how the input strings are to be
        encoded as arc labels---one of: utf8" (encodes strings as a UTF-8
        encoded Unicode strings), "byte" (encodes strings as raw bytes)---or a
        SymbolTable.
    output_token_type: A string indicating how the output strings are to be
        encoded as arc labels---one of: utf8" (encodes strings as a UTF-8
        encoded Unicode strings), "byte" (encodes strings as raw bytes)---or a
        SymbolTable.

  Returns:
    An FST.

  Raises:
    FstArgError: Mappings must be of length 1 or 2.
  """
  cdef Fst result = Fst(arc_type)
  cdef Fst tfst
  for pair in pairs:
    if (not 0 < len(pair) <= 2):
      raise FstArgError("Mappings must be of length 1 or 2")
    try:
      (p0, p1) = pair
    except ValueError:
      p1 = p0 = pair[0]
    tfst = transducer(p0, p1, arc_type=result.arc_type,
                      input_token_type=input_token_type,
                      output_token_type=output_token_type)
    result.union(tfst)
  return result


# Decorator for one-argument constructive FST operations.


def _1arg(fnc):
  @functools.wraps(fnc)
  def patch(arg, *args, **kwargs):
    return _init_Fst_from_MutableFst(fnc(_compile_Fst(arg), *args, **kwargs))
  return patch


arcmap = _1arg(pywrapfst.arcmap)
determinize = _1arg(pywrapfst.determinize)
disambiguate = _1arg(pywrapfst.disambiguate)
epsnormalize = _1arg(pywrapfst.epsnormalize)
prune = _1arg(pywrapfst.prune)
push = _1arg(pywrapfst.push)
randgen = _1arg(pywrapfst.randgen)
reverse = _1arg(pywrapfst.reverse)
rmepsilon = _1arg(pywrapfst.rmepsilon)
shortestpath = _1arg(pywrapfst.shortestpath)
statemap = _1arg(pywrapfst.statemap)
synchronize = _1arg(pywrapfst.synchronize)



def _shortestdistance(fnc):
  @functools.wraps(fnc)
  def patch(arg, *args, **kwargs):
    return fnc(_compile_Fst(arg), *args, **kwargs)
  return patch


shortestdistance = _shortestdistance(pywrapfst.shortestdistance)


# Two-argument constructive FST operations. If just one of the two FST
# arguments has been compiled, the arc type of the compiled argument is used to
# determine the arc type of the not-yet-compiled argument.


def _difference(fnc):
  @functools.wraps(fnc)
  def patch(arg1, arg2, *args, **kwargs):
    cdef Fst lhs = _compile_Fst(arg1)
    cdef Fst rhs = _compile_Fst(arg2)
    if not MergeSymbols(lhs._mfst, rhs._mfst, MERGE_INPUT_AND_OUTPUT_SYMBOLS):
      raise FstSymbolTableMergeError(
          "Unable to resolve symbol table conflict without relabeling")
    return _init_Fst_from_MutableFst(fnc(lhs, rhs, *args, **kwargs))
  return patch


difference = _difference(pywrapfst.difference)


def _intersect(fnc):
  @functools.wraps(fnc)
  def patch(arg1, arg2, *args, **kwargs):
    cdef Fst lhs = _compile_Fst(arg1)
    cdef Fst rhs = _compile_Fst(arg2)
    if not MergeSymbols(lhs._mfst, rhs._mfst,
                        MERGE_LEFT_OUTPUT_AND_RIGHT_INPUT_SYMBOLS):
      raise FstSymbolTableMergeError(
          "Unable to resolve symbol table conflict without relabeling")
    return _init_Fst_from_MutableFst(fnc(lhs, rhs, *args, **kwargs))
  return patch


intersect = _intersect(pywrapfst.intersect)


# Simple comparison operations.


def _comp(fnc):
  @functools.wraps(fnc)
  def patch(arg1, arg2, *args, **kwargs):
    cdef Fst lhs = _compile_Fst(arg1)
    cdef Fst rhs = _compile_Fst(arg2)
    return fnc(lhs, rhs, *args, **kwargs)
  return patch


equal = _comp(pywrapfst.equal)
isomorphic = _comp(pywrapfst.isomorphic)


# Comparison operations that require compatible symbol tables.


def _comp_merge(fnc):
  @functools.wraps(fnc)
  def patch(arg1, arg2, *args, **kwargs):
    cdef Fst lhs = _compile_Fst(arg1)
    cdef Fst rhs = _compile_Fst(arg2)
    if not MergeSymbols(lhs._mfst, rhs._mfst, MERGE_INPUT_AND_OUTPUT_SYMBOLS):
      raise FstSymbolTableMergeError(
          "Unable to resolve symbol table conflict without relabeling")
    return fnc(lhs, rhs, *args, **kwargs)
  return patch


equivalent = _comp_merge(pywrapfst.equivalent)
randequivalent = _comp_merge(pywrapfst.randequivalent)


# Speciality constructive varargs FST operations.


def compose(*args, **kwargs):
  """
  compose(*args, cf="auto", connect=True)

  Constructively composes two or more FSTs.

  This operation computes the composition of two or more FSTs. If A transduces
  string x to y with weight a and B transduces y to z with weight b, then their
  composition transduces string x to z with weight a \otimes b. The output
  labels of the first transducer or the input labels of the second transducer
  must be sorted (or otherwise support appropriate matchers).

  Args:
    *args: Two or more input FSTs.
    cf: A string matching a known composition filter; one of: "alt_sequence",
        "auto", "match", "null", "sequence", "trivial".
    connect: Should output be trimmed?

  Returns:
    A composed FST.

  Raises:
    FstArgError: Input FSTs must have the same arc type.
    FstArgError: Expected at least 2 positional arguments.
    FstSymbolTableMergeError: Unable to resolve symbol table conflict
         without relabeling.
  """
  # TODO(kbg): Heuristically optimize composition order somehow; it currently
  # just does left-associative composition.
  # TODO(kbg): Heuristically optimize arcsorting somehow; it currently sorts
  # the output labels of the LHS and the input labels of the RHS.
  (first, *rest) = args
  if len(rest) < 1:
    raise FstArgError("Expected at least 2 positional arguments "
                         "({} given)".format(len(rest) + 1))
  cdef Fst lhs = _compile_Fst(first)
  cdef Fst rhs
  for arg in rest:
    rhs = _compile_Fst(arg, arc_type=lhs.arc_type)
    lhs.arcsort(sort_type="olabel")
    rhs.arcsort(sort_type="ilabel")
    if not MergeSymbols(lhs._mfst, rhs._mfst,
                        MERGE_LEFT_OUTPUT_AND_RIGHT_INPUT_SYMBOLS):
      raise FstSymbolTableMergeError(
          "Unable to resolve symbol table conflict without relabeling")
    lhs = _init_Fst_from_MutableFst(pywrapfst.compose(lhs, rhs, **kwargs))
    if kwargs.get("connect", True) and not lhs.num_states:
      logging.warning("Composed FST has no connected states")
  return lhs


def replace(root, *, call_arc_labeling=b"neither",
            return_arc_labeling=b"neither", bool epsilon_on_replace=False,
            int64 return_label=0, **replacements):
  """
  replace(root, **replacements, call_arc_labeling="neither",
          return_arc_labeling="neither", epsilon_on_replace=False,
          return_label=0)

  Constructively replaces arcs in an FST with other FST(s).

  This operation performs the dynamic replacement of arcs in one FST with
  other FSTs, allowing for the definition of FSTs analogous to RTNs. The output
  FST is the result of recursively replacing each arc in all input FSTs that
  matches some "non-terminal" with a corresponding FST. More precisely, an arc
  from state s to state d with nonterminal output label n in an input FST is
  replaced by redirecting this "call" arc to the initial state of a copy of the
  the replacement FST and then adding "return" arcs from each final state of
  the replacement FST to d in the input FST. If there are cyclic dependencies
  among the replacement rules, the resulting FST does not have a finite
  expansion and an exception will be raised.

  Args:
    root: The root FST.
    **replacements: Keyword argument label/FST pairs to be replaced in
       `root_fst`. For example, they keyword argument `DT=union("the", "a")`
       indicates that all instances of label "DT" should be replaced by the
       union of "the" and "a".
    call_arc_labeling: A string indicating which call arc labels should be
        non-epsilon. One of: "input" (default), "output", "both", "neither".
        This value is set to "neither" if epsilon_on_replace is True.
    return_arc_labeling: A string indicating which return arc labels should be
        non-epsilon. One of: "input", "output", "both", "neither" (default).
        This value is set to "neither" if epsilon_on_replace is True.
    epsilon_on_replace: Should call and return arcs be epsilon arcs? If True,
        this effectively overrides call_arc_labeling and return_arc_labeling,
        setting both to "neither".
    return_label: The integer label for return arcs.

  Returns:
    A replaced FST.

  Raises:
    KeyError: Nonterminal symbol not found.
    FstOpError: Operation failed.

  See also: `pdt_replace`.
  """
  cdef Fst root_fst = _compile_Fst(root)
  cdef string arc_type = root_fst.arc_type
  cdef vector[StringFstClassPair] pairs
  # This has the pleasant effect of preventing Python from garbage-collecting
  # these FSTs until we're ready.
  # TODO(kbg): Is there a better way?
  replacement_set = [(tostring(nt), _compile_Fst(rep, arc_type)) for (nt, rep)
                     in replacements.iteritems()]
  cdef string nonterm
  cdef Fst replacement
  for (nonterm, replacement) in replacement_set:
    pairs.push_back(StringFstClassPair(nonterm, replacement._fst))
  cdef ReplaceLabelType cal = _get_replace_label_type(
      tostring(call_arc_labeling), epsilon_on_replace)
  cdef ReplaceLabelType ral = _get_replace_label_type(
      tostring(return_arc_labeling), epsilon_on_replace)
  cdef ReplaceOptions *opts = new ReplaceOptions(-1, cal, ral, return_label)
  cdef Fst result = Fst(arc_type)
  PyniniReplace(deref(root_fst._fst), pairs, result._mfst, deref(opts))
  del replacement_set
  del opts
  result._check_pynini_op_error()
  return result


def union(*args):
  """
  union(*args)

  Computes the union (sum) of two or more FSTs.

  This operation computes the union (sum) of two FSTs. If A transduces string
  x to y with weight a and B transduces string w to v with weight b, then their
  union transduces x to y with weight a and w to v with weight b.

  Args:
   *args: Two or more input FSTs.

  Returns:
    An Fst union.
  """
  (first, *rest) = args
  if len(args) < 1:
    raise FstArgError("Expected at least 2 positional arguments "
                         "({} given)".format(len(rest) + 1))
  cdef Fst lhs = _compile_Fst(first)
  for rhs in rest:
    lhs.union(rhs)
  return lhs


# Pushdown transducer classes and operations.


cdef class PdtParentheses(object):

  """
  PdtParentheses()

  Pushdown transducer parentheses class.

  This class wraps a vector of pairs of FST arc labels in which the first
  label is interpreted as a "push" stack operation and the second represents
  the corresponding "pop" operation. When efficiency is desired, the push and
  pop indices should be contiguous.

  A PDT is expressed as an (Fst, PdtParentheses) pair for the purposes of all
  supported PDT operations.
  """

  cdef vector[LabelPair] _parens

  def __repr__(self):
    return "<PdtParentheses at 0x{:x}>".format(id(self))

  def __len__(self):
    return self._parens.size()

  def __iter__(self):
    cdef size_t i = 0
    for i in xrange(self._parens.size()):
      yield (self._parens[i].first, self._parens[i].second)

  cpdef PdtParentheses copy(self):
    """
    copy(self)

    Makes a copy of this PdtParentheses object.

    Returns:
      A deep copy of the PdtParentheses object.
    """
    cpdef PdtParentheses result = PdtParentheses.__new__(PdtParentheses)
    result._parens = self._parens
    return result

  cpdef void add_pair(self, int64 push, int64 pop):
    """
    add_pair(push, pop)

    Adds a pair of parentheses to the set.

    Args:
      push: An FST arc label to be interpreted as a "push" operation.
      pop: An FST arc label to be interpreted as a "pop" operation.
    """
    self._parens.push_back(LabelPair(push, pop))

  @classmethod
  def read(cls, filename):
    """
    PdtParentheses.read(filename)

    Reads parentheses pairs from a text file.

    This class method creates a new PdtParentheses object from a pairs of
    integer labels in a text file.

    Args:
      filename: The string location of the input file.

    Returns:
      A new PdtParentheses instance.

    Raises:
      FstIOError: Read failed.
    """
    cdef PdtParentheses result = PdtParentheses.__new__(PdtParentheses)
    if not ReadLabelPairs[int64](tostring(filename), addr(result._parens),
                                 False):
      raise FstIOError("Read failed: {!r}".format(filename))
    return result

  cpdef void write(self, filename) except *:
    """
    write(filename)

    Writes parentheses pairs to text file.

    This method writes the PdtParentheses object to a text file.

    Args:
      filename: The string location of the output file.

    Raises:
      FstIOError: Write failed.
    """
    if not WriteLabelPairs[int64](tostring(filename), self._parens):
      raise FstIOError("Write failed: {!r}".format(filename))


def pdt_compose(ifst1, ifst2, PdtParentheses parens, cf=b"paren",
                bool left_pdt=True):
  """
  pdt_compose(ifst1, ifst2, parens, cf="paren", left_pdt=True)

  Composes a PDT with an FST.

  This operation composes a PDT with an FST. The input PDT is defined by the
  combination of an FST and a PdtParentheses object specifying the stack
  symbols. The caller should also specify whether the left-hand or the
  right-hand FST argument is to be interpreted as a PDT.

  Args:
    ifst1: The left-hand-side input FST or PDT.
    ifst2: The right-hand-side input FST or PDT.
    parens: A PdtParentheses object specifying the input PDT's stack symbols.
    cf: A string indicating the desired PDT composition filter; one of: "paren"
        (keeps parentheses), "expand" (expands and removes parentheses),
        "expand_paren" (expands and keeps parentheses).
    left_pdt: If true, the first argument is interpreted as a PDT and the
        second argument is interpreted as an FST; if false, the second
        argument is interpreted as a PDT and the first argument is interpreted
        as an FST.

  Returns:
    The FST component of an PDT produced by composition.

  Raises:
    FstOpError: Operation failed.
    FstSymbolTableMergeError: Unable to resolve symbol table conflict
        without relabeling.
  """
  cdef Fst lhs = _compile_Fst(ifst1)
  cdef Fst rhs = _compile_Fst(ifst2, lhs.arc_type)
  lhs.arcsort(sort_type="olabel")
  rhs.arcsort(sort_type="ilabel")
  if not MergeSymbols(lhs._mfst, rhs._mfst,
                      MERGE_LEFT_OUTPUT_AND_RIGHT_INPUT_SYMBOLS):
    raise FstSymbolTableMergeError(
        "Unable to resolve symbol table conflict without relabeling")
  cdef Fst result = Fst(lhs.arc_type)
  cdef PdtComposeFilter typed_cf = _get_pdt_compose_filter(tostring(cf))
  cdef PdtComposeOptions *opts = new PdtComposeOptions(True, typed_cf)
  PdtCompose(deref(lhs._fst), deref(rhs._fst), parens._parens, result._mfst,
             deref(opts), left_pdt)
  del opts
  if not result.num_states:
    logging.warning("Composed PDT has no connected states")
  result._check_pynini_op_error()
  # If the "expand" filter is selected, all parentheses have been mapped to
  # epsilon. This conveniently removes the arcs that result.
  if typed_cf == EXPAND_FILTER:
    result.rmepsilon()
  # Otherwise, we need to add the parentheses to the result.
  else:
    _add_parentheses_symbols(result._mfst, parens._parens, left_pdt)
  return result


def pdt_expand(ipdt, PdtParentheses parens, bool connect=True,
               bool keep_parentheses=False, weight=None):
  """
  pdt_expand(ipdt, parens, connect=True, keep_parentheses=False, weight=None)

  Expands a bounded-stack PDT to an FST.

  This operation converts a bounded-stack PDT into the equivalent FST. The
  input PDT is defined by the combination of an FST and a PdtParentheses object
  specifying the PDT stack symbols.

  If the input PDT does not have a bounded stack, then it is impossible to
  expand the PDT into an FST and this operation will not terminate.

  Args:
    ipdt: The FST component of the input PDT.
    parens: A PdtParentheses object specifying the input PDT's stack symbols.
    connect: Should the output FST be trimmed?
    keep_parentheses: Should the output FST preserve parentheses arcs?
    weight: A Weight or weight string indicating the desired weight threshold;
        paths with weights below this threshold will be pruned. If omitted or
        null, no paths are pruned.

  Returns:
    An FST produced by expanding the bounded-stack PDT.

  Raises:
    FstOpError: Operation failed.
  """
  cdef Fst pdt = _compile_Fst(ipdt)
  cdef Fst result = Fst(pdt.arc_type)
  cdef WeightClass wc = _get_WeightClass_or_Zero(result.weight_type, weight)
  cdef PdtExpandOptions *opts = new PdtExpandOptions(connect,
                                                     keep_parentheses, wc)
  PdtExpand(deref(pdt._fst), parens._parens, result._mfst, deref(opts))
  del opts
  result._check_pynini_op_error()
  return result


def pdt_replace(root, pdt_parser_type=b"left", **replacements):
  """
  pdt_replace(root, pdt_parser_type="left", **replacements)

  Constructive replaces arcs in an FST with other FST(s), producing a PDT.

  This operation performs the dynamic replacement of arcs in one FST with
  another FST, allowing the definition of a PDT analogues to RTNs. The output
  PDT, defined by the combination of an FST and a PdtParentheses object
  specifying the PDT stack symbols, is the result of recursively replacing each
  arc in an input FST that matches some "non-terminal" with a corresponding
  FST, inserting parentheses where necessary. More precisely, an arc from
  state s to state d with nonterminal output label n in an input FST is
  replaced by redirecting this "call" arc to the initial state of a copy of the
  replacement FST and then adding "return" arcs from each final state of the
  replacement FST to d in the input FST. Unlike `replace`, this operation is
  capable of handling cyclic dependencies among replacement rules, which is
  accomplished by adding "push" stack symbols to "call" arcs and "pop" stack
  symbols to "return" arcs.

  Args:
    root: The root FST.
    pdt_parser_type: A string matching a known PdtParserType. One of: "left"
        (default), "left_sr".
    **replacements: Keyword argument label/FST pairs to be replaced in
       `root_fst`. For example, the keyword argument `DT=union("the", "a")`
       indicates that all instances of label "DT" should be replaced by the
       union of "the" and "a".

  Returns:
   An (Fst, PdtParentheses) pair defining a PDT resulting from PDT replacement.

  Raises:
    FstOpError: Operation failed.

  See also: `replace`.
  """
  cdef Fst root_fst = _compile_Fst(root)
  cdef string arc_type = root_fst.arc_type
  cdef vector[StringFstClassPair] pairs
  # This has the pleasant effect of preventing Python from garbage-collecting
  # these FSTs until we're ready.
  # TODO(kbg): Is there a better way?
  replacement_set = [(tostring(nt), _compile_Fst(rep, arc_type)) for (nt, rep)
                     in replacements.iteritems()]
  cdef string nonterm
  cdef Fst replacement
  for (nonterm, replacement) in replacement_set:
    pairs.push_back(StringFstClassPair(nonterm, replacement._fst))
  cdef Fst result = Fst(arc_type)
  cdef PdtParentheses parens = PdtParentheses()
  PyniniPdtReplace(deref(root_fst._fst), pairs, result._mfst,
                   addr(parens._parens),
                   _get_pdt_parser_type(tostring(pdt_parser_type)))
  del replacement_set
  result._check_pynini_op_error()
  return (result, parens)


def pdt_reverse(ipdt, PdtParentheses parens):
  """
  pdt_reverse(ipdt, parens)

  Reverses a PDT.

  This operation reverses an PDT. The input PDT is defined by the combination
  of an FST and a PdtParentheses object specifying the PDT stack symbols.

  Args:
    ipdt: The FST component of the input PDT.
    parens: A PdtParentheses object specifying the input PDT's stack symbols.

  Returns:
    The FST component of a PDT resulting from reversing the input PDT.
  """
  cdef Fst pdt = _compile_Fst(ipdt)
  cdef Fst result = Fst(pdt.arc_type)
  PdtReverse(deref(pdt._fst), parens._parens, result._mfst)
  result._check_pynini_op_error()
  return result


def pdt_shortestpath(ipdt, PdtParentheses parens, qt=b"fifo",
                     bool keep_parentheses=False, bool path_gc=True):
  """
  pdt_shortestpath(ipdt, parens, qt="fifo", keep_parentheses=False,
                   path_gc=True)

  Computes the shortest path through a bounded-stack PDT.

  This operation computes the shortest path through a PDT. The input PDT is
  defined by the combination of an FST and a PdtParentheses object specifying
  the PDT stack symbols.

  Args:
    ipdt: The FST component of an input PDT.
    parens: A PdtParentheses object specifying the input PDT's stack symbols.
    qt: A string matching a known queue type; one of: "fifo" (default), "lifo",
        "state".
    keep_parentheses: Should the output FST preserve parentheses arcs?
    path_gc: Should shortest path data be garbage-collected?

  Returns:
    A string FST representing the shortest path.

  Raises:
    FstOpError: Operation failed.
  """
  cdef Fst pdt = _compile_Fst(ipdt)
  cdef Fst result = Fst(pdt.arc_type)
  cdef PdtShortestPathOptions *opts = new PdtShortestPathOptions(
        _get_queue_type(tostring(qt)), keep_parentheses, path_gc)
  PdtShortestPath(deref(pdt._fst), parens._parens, result._mfst, deref(opts))
  del opts
  result._check_pynini_op_error()
  return result



# Multi-pushdown transducer classes and operations.


cdef class MPdtParentheses(object):

  """
  MPdtParentheses()

  Multi-pushdown transducer parentheses class.

  This class wraps a vector of pairs of FST arc labels in which the first
  label is interpreted as a "push" stack operation and the second represents
  the corresponding "pop" operation, and an equally sized vector which assigns
  each pair to a stack. The library currently only permits two stacks (numbered
  1 and 2) to be used.

  A MPDT is expressed as an (Fst, MPdtParentheses) pair for the purposes of all
  supported MPDT operations.
  """

  cdef vector[LabelPair] _parens
  cdef vector[int64] _assign

  def __repr__(self):
    return "<MPdtParentheses at 0x{:x}>".format(id(self))

  def __len__(self):
    return self._parens.size()

  def __iter__(self):
    cdef size_t i = 0
    for i in xrange(self._parens.size()):
      yield (self._parens[i].first, self._parens[i].second, self._assign[i])

  cpdef MPdtParentheses copy(self):
    """
    copy(self)

    Makes a copy of this MPdtParentheses object.

    Returns:
      A deep copy of the MPdtParentheses object.
    """
    cpdef MPdtParentheses result = MPdtParentheses.__new__(MPdtParentheses)
    result._parens = self._parens
    result._assign = self._assign
    return result

  cpdef void add_triple(self, int64 push, int64 pop, int64 assignment):
    """
    add_triple(push, pop, assignment)

    Adds a triple of (left parenthesis, right parenthesis, stack assignment)
    triples to the object.

    Args:
      push: An FST arc label to be interpreted as a "push" operation.
      pop: An FST arc label to be interpreted as a "pop" operation.
      assignment: An FST arc label indicating what stack the parentheses pair
          is assigned to.
    """
    self._parens.push_back(LabelPair(push, pop))
    self._assign.push_back(assignment)

  @classmethod
  def read(cls, filename):
    """
    MPdtParentheses.read(filename)

    Reads parentheses/assignment triples from a text file.

    This class method creates a new PdtParentheses object from a pairs of
    integer labels in a text file.

    Args:
      filename: The string location of the input file.

    Returns:
      A new MPdtParentheses instance.

    Raises:
      FstIOError: Read failed.
    """
    cdef MPdtParentheses result = MPdtParentheses.__new__(MPdtParentheses)
    if not ReadLabelTriples[int64](tostring(filename), addr(result._parens),
                                   addr(result._assign), False):
      raise FstIOError("Read failed: {!r}".format(filename))
    return result

  cpdef void write(self, filename) except *:
    """
    write(filename)

    Writes parentheses triples to text file.

    This method writes the MPdtParentheses object to a text file.

    Args:
      filename: The string location of the output file.

    Raises:
      FstIOError: Write failed.
    """
    if not WriteLabelTriples[int64](tostring(filename), self._parens,
                                    self._assign):
      raise FstIOError("Write failed: {!r}".format(filename))


cpdef Fst mpdt_compose(ifst1, ifst2, MPdtParentheses parens, cf=b"paren",
                       bool left_mpdt=True):
  """
  mpdt_compose(ifst1, ifst2, parens, cf="paren", left_mpdt=True)

  Composes a MPDT with an FST.

  This operation composes a MPDT with an FST. The input MPDT is defined by the
  combination of an FST and a MPdtParentheses object specifying the stack
  symbols and assignments. The caller should also specify whether the left-hand
  or the right-hand FST argument is to be interpreted as a MPDT.

  Args:
    ifst1: The left-hand-side input FST or MPDT.
    ifst2: The right-hand-side input FST or MPDT.
    parens: A MPdtParentheses object specifying the input MPDT's stack
        operations and assignments.
    cf: A string indicating the desired MPDT composition filter; one of: "paren"
        (keeps parentheses), "expand" (expands and removes parentheses),
        "expand_paren" (expands and keeps parentheses).
    left_mpdt: If true, the first argument is interpreted as a MPDT and the
        second argument is interpreted as an FST; if false, the second
        argument is interpreted as a MPDT and the first argument is interpreted
        as an FST.

  Returns:
    The FST component of an MPDT produced by composition.

  Raises:
    FstOpError: Operation failed.
    FstSymbolTableMergeError: Unable to resolve symbol table conflict
        without relabeling.

  See also: `compose`.
  """
  cdef Fst lhs = _compile_Fst(ifst1)
  cdef Fst rhs = _compile_Fst(ifst2, lhs.arc_type)
  lhs.arcsort(sort_type="olabel")
  rhs.arcsort(sort_type="ilabel")
  if not MergeSymbols(lhs._mfst, rhs._mfst,
                      MERGE_LEFT_OUTPUT_AND_RIGHT_INPUT_SYMBOLS):
    raise FstSymbolTableMergeError(
        "Unable to resolve symbol table conflict without relabeling")
  cdef Fst result = Fst(lhs.arc_type)
  cdef PdtComposeFilter typed_cf = _get_pdt_compose_filter(tostring(cf))
  cdef MPdtComposeOptions *opts = new MPdtComposeOptions(True, typed_cf)
  MPdtCompose(deref(lhs._fst), deref(rhs._fst), parens._parens,
              parens._assign, result._mfst, deref(opts), left_mpdt)
  del opts
  if not result.num_states:
    logging.warning("Composed MPDT has no connected states")
  if result._fst.Properties(kError, True) == kError:
    raise FstOpError("Operation failed")
  # If the "expand" filter is selected, all parentheses have been mapped to
  # epsilon. This conveniently removes the arcs that result.
  if typed_cf == EXPAND_FILTER:
    result.rmepsilon()
  # Otherwise, we need to add the parentheses to the result.
  else:
    _add_parentheses_symbols(result._mfst, parens._parens, left_mpdt)
  return result


def mpdt_expand(impdt, MPdtParentheses parens, bool connect=True,
                bool keep_parentheses=False):
  """
  mpdt_expand(impdt, parens, connect=True, keep_parentheses=False):

  Expands a bounded-stack MPDT to an FST.

  This operation converts a bounded-stack MPDT into the equivalent FST. The
  input MPDT is defined by the combination of an FST and a MPdtParentheses
  object specifying the MPDT stack symbols and assignments.

  If the input MPDT does not have a bounded stack, then it is impossible to
  expand the MPDT into an FST and this operation will not terminate.

  Args:
    impdt: The FST component of the input MPDT.
    parens: A MPdtParentheses object specifying the input PDT's stack
        symbols and assignments.
    connect: Should the output FST be trimmed?
    keep_parentheses: Should the output FST preserve parentheses arcs?

  Returns:
    An FST produced by expanding the bounded-stack MPDT.

  Raises:
    FstOpError: Operation failed.
  """
  cdef Fst mpdt = _compile_Fst(impdt)
  cdef Fst result = Fst(mpdt.arc_type)
  cdef MPdtExpandOptions *opts = new MPdtExpandOptions(connect,
                                                       keep_parentheses)
  MPdtExpand(deref(mpdt._fst), parens._parens, parens._assign, result._mfst,
             deref(opts))
  del opts
  result._check_pynini_op_error()
  return result


def mpdt_reverse(impdt, MPdtParentheses parens):
  """
  mpdt_reverse(impdt, parens)

  Reverses a MPDT.

  This operation reverses an MPDT. The input MPDT is defined by the combination
  of an FST and a MPdtParentheses object specifying the MPDT stack symbols
  and assignments. Unlike PDT reversal, which only modifies the FST component,
  this operation also reverses the stack assignments. assignments.

  Args:
    impdt: The FST component of the input MPDT.
    parens: A MPdtParentheses object specifying the input MPDT's stack symbols
        and assignments.

  Returns:
    A (Fst, MPdtParentheses) pair specifying the reversed MPDT.
  """
  cdef Fst mpdt = _compile_Fst(impdt)
  cdef Fst result_fst = Fst(mpdt.arc_type)
  cdef MPdtParentheses result_parens = parens.copy()
  MPdtReverse(deref(mpdt._fst), result_parens._parens,
              addr(result_parens._assign), result_fst._mfst)
  result_fst._check_pynini_op_error()
  return (result_fst, result_parens)


# Class for printing paths.


cdef class StringPaths(object):

  """
  StringPaths(fst, token_type="byte", isymbols=None, osymbols=None)

  Iterator for string paths in acyclic FST.

  This class provides an iterator over all paths (represented as pairs of
  strings and an associated path weight) through an acyclic FST. This
  operation is only feasible when the FST is acyclic. Depending on the
  requested token type, the arc labels along the input and output sides of a
  path are interpreted as UTF-8-encoded Unicode strings, raw bytes, or a
  concatenation of string labels from a symbol table. This class is normally
  created by invoking the `paths` method of `Fst`.

  Note that this class is an iterator over all paths *at the time of creation*
  and the iterator will not be affected by any mutations to the argument
  FST or input symbol tables.

  Args:
    token_type: A string indicating how arcs labels are to be interpreted as strings;
        (interprets arc labels as UTF-8 encoded Unicode strings), "byte" (interprets
        arc labels as byte strings), "symbol" (interprets arc labels according to
        the provided symbol tables).
    isymbols: Input symbol table (ignored unless token_type is "symbol")
    osymbols: Output symbol table (ignored unless token_type is "symbol")

  Raises:
    FstArgError: Unknown token type.
    FstArgError: FST is not acyclic.
  """

  cdef FstClass *_fst
  cdef StringPathsClass *_paths

  def __repr__(self):
    return "<StringPaths at 0x{:x}>".format(id(self))

  def __init__(self, _Fst fst, token_type=b"byte", _SymbolTable isymbols=None,
               _SymbolTable osymbols=None):
    # Makes a reference-counted copy of the FST.
    self._fst = new FstClass(deref(fst._fst))
    cdef TokenType tt = _get_token_type(tostring(token_type))
    if tt == SYMBOL:
      # Makes reference-counted copies of the symbol tables.
      self._paths = new StringPathsClass(deref(self._fst), tt,
          (<SymbolTable *> NULL) if isymbols is None else
              isymbols._table.Copy(),
          (<SymbolTable *> NULL) if osymbols is None else
              osymbols._table.Copy())
    else:
      self._paths = new StringPathsClass(deref(self._fst), tt, NULL, NULL)
    if self._paths.Error():
      raise FstArgError("FST is not acyclic")

  def __dealloc__(self):
    del self._fst
    del self._paths

  # This just registers this class as a possible iterator.
  def __iter__(self):
    return self

  # Magic method used to get a Pythonic API out of the C++ API.
  def __next__(self):
    if self.done():
      raise StopIteration
    cdef string istring = self.istring()
    cdef string ostring = self.ostring()
    cdef Weight weight = self.weight()
    self.next()
    return (istring, ostring, weight)

  cpdef bool done(self):
    """"
    done(self)

    Indicates whether the iterator is exhausted or not.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.

    Returns:
      True if the iterator is exhausted, False otherwise.
    """
    return self._paths.Done()

  cpdef bool error(self):
    """
    error(self)

    Indicates whether the StringPaths has encountered an error.

    Returns:
      True if the StringPaths is in an errorful state, False otherwise.
    """
    return self._paths.Error()

  cpdef void reset(self):
    """
    reset(self)

    Resets the iterator to the initial position.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.
    """
    self._paths.Reset()

  cpdef void next(self):
    """
    next(self)

    Advances the iterator.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.
    """
    self._paths.Next()

  cpdef string istring(self):
    """
    istring(self)

    Returns the current path's input string.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.

    Returns:
      The path's input string.
    """
    return self._paths.IString()

  cpdef string ostring(self):
    """
    ostring(self)

    Returns the current path's output string.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.

    Returns:
      The path's output string.
    """
    return self._paths.OString()

  cpdef Weight weight(self):
    """
    weight(self)

    Returns the current path's total weight.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.

    Returns:
      The path's Weight.
    """
    cdef Weight weight = Weight.__new__(Weight)
    weight._weight = new WeightClass(self._paths.Weight())
    return weight


# Class for FAR reading and/or writing.


cdef class Far(object):

  """
  Far(filename, mode="r", arc_type="standard", far_type="default")

  Pynini FAR ("Fst ARchive") object.

  This class is used to either read FSTs from or write FSTs to a FAR. When
  opening a FAR for writing, the user may also specify the desired arc type
  and FAR type.

  Args:
    filename: A string indicating the filename.
    mode: FAR IO mode; one of: "r" (open for reading), "w" (open for writing).
    arc_type: Desired arc type; this is ignored if the FAR is opened for
        reading.
    far_type: Desired FAR type; this is ignored if the FAR is opened for
        reading.

  Attributes:
    arc_type: A string indicating the arc type.
    fst_type: A string indicating the FST (container) type.
    mode: A string indicating whether the FAR is open for reading ("r") or
        writing ("w").
    name: A string indicating the filename.
  """

  cdef char _mode
  cdef string _name
  cdef FarReader _reader
  cdef FarWriter _writer

  # Instances holds either a FarReader or a FarWriter, a boolean indicating

  def __init__(self, filename, mode=b"r", arc_type=b"standard",
               far_type=b"default"):
    self._name = tostring(filename)
    self._mode = tostring(mode)[0]
    if self._mode == b"r":
      self._reader = FarReader.open(self._name)
    elif self._mode == b"w":
      self._writer = FarWriter.create(self._name, arc_type=arc_type,
                                      far_type=far_type)
    else:
      raise FstArgError("Unknown mode: {!r}".format(mode))

  def __repr__(self):
    return "<{} Far {!r}, mode '{:c}' at 0x{:x}>".format(self.far_type,
                                                         self._name,
                                                         self._mode,
                                                         id(self))

  cdef void _check_mode(self, char mode) except *:
    if not self._mode == mode:
      raise FstOpError("Cannot invoke method in current mode: '{:c}'".format(
                       self._mode))

  cdef void _check_not_mode(self, char mode) except *:
    if self._mode == mode:
      raise FstOpError("Cannot invoke method in current mode: '{:c}'".format(
                       self._mode))

  # API shared between FarReader and FarWriter.

  cpdef bool error(self) except *:
    """
    error(self)

    Indicates whether the FAR has encountered an error.

    Returns:
      True if the FAR is in an errorful state, False otherwise.
    """
    self._check_not_mode(b"c")
    return self._reader.error() if self._mode == b"r" else self._writer.error()

  @property
  def arc_type(self):
    self._check_not_mode(b"c")
    return (self._reader._arc_type() if self._mode == b"r" else
            self._writer._arc_type())

  @property
  def closed(self):
    return self._mode == b"c"

  @property
  def far_type(self):
    if self._mode == b"r":
      return self._reader._far_type()
    elif self._mode == b"w":
      return self._writer._far_type()
    else:
      return "closed"

  @property
  def mode(self):
    return "{:c}".format(self._mode)

  @property
  def name(self):
    return self._name

  # FarReader API.

  # This just registers this class as a possible iterator.
  def __iter__(self):
    return self

  def __next__(self):
    self._check_mode(b"r")
    (key, mfst) = next(self._reader)
    return (key, _init_Fst_from_MutableFst(mfst))

  cpdef bool find(self, key) except *:
    """
    find(self, key)

    Sets the current position to the first entry greater than or equal to the
    key (a string) and indicates whether or not a match was found.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.

    Args:
      key: A string key.

    Returns:
      True if the key was found, False otherwise.

    Raises:
      FstOpError: Cannot invoke method in current mode.
    """
    self._check_mode(b"r")
    return self._reader.find(key)

  def get_fst(self):
    """
    get_fst(self)

    Returns the FST at the current position.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.

    Returns:
      A copy of the FST at the current position.

    Raises:
      FstOpError: Cannot invoke method in current mode.
    """
    self._check_mode(b"r")
    return _init_Fst_from_MutableFst(self._reader.get_fst())

  cpdef string get_key(self) except *:
    """
    get_key(self)

    Returns the string key at the current position.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.

    Returns:
      The string key at the current position.

    Raises:
      FstOpError: Cannot invoke method in current mode.
    """
    self._check_mode(b"r")
    return self._reader.get_key()

  cpdef void next(self) except *:
    """
    next(self)

    Advances the iterator.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.

    Raises:
      FstOpError: Cannot invoke method in current mode.
    """
    self._check_mode(b"r")
    self._reader.next()

  cpdef void reset(self) except *:
    """
    reset(self)

    Resets the iterator to the initial position.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.

    Raises:
      FstOpError: Cannot invoke method in current mode.
    """
    self._check_mode(b"r")
    self._reader.reset()

  def __getitem__(self, key):
    if not self.find(key):
      raise KeyError(key)
    return self.get_fst()

  # FarWriter API.

  cpdef void add(self, key, Fst fst) except *:
    """
    add(self, key, fst)

    Adds an FST to the FAR (when open for writing)

    This methods adds an FST to the FAR which can be retrieved with the
    specified string key.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.

    Args:
      key: The string used to key the input FST.
      fst: The FST to write to the FAR.

    Raises:
      FstOpError: Cannot invoke method in current mode.
      FstOpError: Incompatible or invalid arc type.
    """
    self._check_mode(b"w")
    self._writer.add(key, fst)

  def __setitem__(self, key, Fst fst):
    self._check_mode(b"w")
    self._writer[key] = fst

  cpdef void close(self) except *:
    """
    close(sel)

    Closes the FAR and flushes to disk (when open for writing).

    Raises:
      FstOpError: Cannot invoke method in current mode.
    """
    self._check_mode(b"w")
    self._writer._close()
    self._mode = b"c"

  # Adds support for use as a PEP-343 context manager.

  def __enter__(self):
    return self

  def __exit__(self, exc, value, tb):
    if self._mode == b"w":
      self._writer._close()
      self._mode = b"c"


## PYTHON IMPORTS.


# Classes from pywrapfst.


from pywrapfst import Arc
from pywrapfst import ArcIterator
from pywrapfst import EncodeMapper
from pywrapfst import MutableArcIterator
from pywrapfst import StateIterator
from pywrapfst import SymbolTableIterator
from pywrapfst import Weight


# Exceptions not yet imported.


from pywrapfst import FstBadWeightError
from pywrapfst import \
    FstDeletedConstructorError
from pywrapfst import FstIndexError
from pywrapfst import \
    FstUnknownWeightTypeError


# FST properties.


from pywrapfst import EXPANDED
from pywrapfst import MUTABLE
from pywrapfst import ERROR
from pywrapfst import ACCEPTOR
from pywrapfst import NOT_ACCEPTOR
from pywrapfst import I_DETERMINISTIC
from pywrapfst import NON_I_DETERMINISTIC
from pywrapfst import O_DETERMINISTIC
from pywrapfst import NON_O_DETERMINISTIC
from pywrapfst import EPSILONS
from pywrapfst import NO_EPSILONS
from pywrapfst import I_EPSILONS
from pywrapfst import NO_I_EPSILONS
from pywrapfst import O_EPSILONS
from pywrapfst import NO_O_EPSILONS
from pywrapfst import I_LABEL_SORTED
from pywrapfst import NOT_I_LABEL_SORTED
from pywrapfst import O_LABEL_SORTED
from pywrapfst import NOT_O_LABEL_SORTED
from pywrapfst import WEIGHTED
from pywrapfst import UNWEIGHTED
from pywrapfst import CYCLIC
from pywrapfst import ACYCLIC
from pywrapfst import INITIAL_CYCLIC
from pywrapfst import INITIAL_ACYCLIC
from pywrapfst import TOP_SORTED
from pywrapfst import NOT_TOP_SORTED
from pywrapfst import ACCESSIBLE
from pywrapfst import NOT_ACCESSIBLE
from pywrapfst import COACCESSIBLE
from pywrapfst import NOT_COACCESSIBLE
from pywrapfst import STRING
from pywrapfst import NOT_STRING
from pywrapfst import WEIGHTED_CYCLES
from pywrapfst import UNWEIGHTED_CYCLES
from pywrapfst import NULL_PROPERTIES
from pywrapfst import COPY_PROPERTIES
from pywrapfst import INTRINSIC_PROPERTIES
from pywrapfst import EXTRINSIC_PROPERTIES
from pywrapfst import SET_START_PROPERTIES
from pywrapfst import SET_FINAL_PROPERTIES
from pywrapfst import ADD_STATE_PROPERTIES
from pywrapfst import ADD_ARC_PROPERTIES
from pywrapfst import SET_ARC_PROPERTIES
from pywrapfst import DELETE_STATE_PROPERTIES
from pywrapfst import DELETE_ARC_PROPERTIES
from pywrapfst import STATE_SORT_PROPERTIES
from pywrapfst import ARC_SORT_PROPERTIES
from pywrapfst import \
    I_LABEL_INVARIANT_PROPERTIES
from pywrapfst import \
    O_LABEL_INVARIANT_PROPERTIES
from pywrapfst import \
    WEIGHT_INVARIANT_PROPERTIES
from pywrapfst import \
    ADD_SUPERFINAL_PROPERTIES
from pywrapfst import \
    RM_SUPERFINAL_PROPERTIES
from pywrapfst import BINARY_PROPERTIES
from pywrapfst import TRINARY_PROPERTIES
from pywrapfst import POS_TRINARY_PROPERTIES
from pywrapfst import NEG_TRINARY_PROPERTIES
from pywrapfst import FST_PROPERTIES


# The following block executes code that, due to syntactic limitations of
# Cython, can't be run at compile time. Instead, it is run at module import
# time.


exec("""
# Makes module-level variants of destructive operations which work
# constructively (copying their input and returning the mutated result).


def _copy(fnc):
  # The junk in the `functools.wraps` decorator is due to a long-standing bug
  # in Python 2.7 (https://bugs.python.org/issue3445).
  @functools.wraps(fnc, ("__name__", "__doc__"))
  def patch(arg1, *args, **kwargs):
    result = _compile_Fst(arg1)
    fnc(result, *args, **kwargs)
    return result
  return patch


arcsort = _copy(Fst.arcsort)
closure = _copy(Fst.closure)
concat = _copy(Fst.concat)
connect = _copy(Fst.connect)
decode = _copy(Fst.decode)
encode = _copy(Fst.encode)
invert = _copy(Fst.invert)
minimize = _copy(Fst.minimize)
optimize = _copy(Fst.optimize)
project = _copy(Fst.project)
relabel_pairs = _copy(Fst.relabel_pairs)
relabel_tables = _copy(Fst.relabel_tables)
reweight = _copy(Fst.reweight)
topsort = _copy(Fst.topsort)


# Very late imports that conflict with cimports.

SymbolTable = pywrapfst.SymbolTable
Weight = pywrapfst.Weight
""")

