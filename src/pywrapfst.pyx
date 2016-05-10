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
# See www.openfst.org for extensive documentation on this weighted
# finite-state transducer library.


"""Python interface to the FST scripting API.

Operations which construct new FSTs are implemented as traditional functions, as
are two-argument boolean functions like `equal` and `equivalent`. Destructive
operations---those that mutate an FST, in place---are instance methods, as is
`write`. Operator overloading is not used. The following example, based on
Mohri et al. 2002, shows the construction of an ASR system given a pronunciation
lexicon L, grammar G, a transducer from context-dependent phones to
context-independent phones C, and an HMM set H:

  L = fst.Fst.read("L.fst")
  G = fst.Fst.read("G.fst")
  C = fst.Fst.read("C.fst")
  H = fst.Fst.read("H.fst")
  LG = fst.determinize(fst.compose(L, G))
  CLG = fst.determinize(fst.compose(C, LG))
  HCLG = fst.determinize(fst.compose(H, CLG))
  HCLG.minimize()                                      # NB: works in-place.

Python variables here use snake_case and constants are in all caps, minus the
normal `k` prefix.
"""


# A few of the more idiosyncratic choices made here are due to "impedance
# mismatches" between C++ and Python, as follows.
#
# Another issue is that due to differences in C++ and Python scope rules, most
# C++ class instances have to be heap-allocated. Since all are packed into
# Python class instances, Python destructors are used to semi-automatically
# free C++ instances. The one exception are the various `...Options` structs.
# All that is included here are the constructors; there is no need to include
# the names of the struct members. Cython does not draw any meaningful
# distinction between structs and C++ classes, so these look just like class
# definitions.
#
# Cython's type annotations (e.g., `string`) are used when the variables will
# be sent as arguments to C++ functions, but are not used for variables used
# within the module.
#
# Internal functions which may raise a Python error do not have a C++ return
# type simply because this leads the C++ compiler to think that the resulting
# value could be used before it is populated.


# C imports.
from libc.limits cimport INT_MAX

# C++ imports.
from libcpp cimport bool
from libcpp.cast cimport const_cast
from libcpp.cast cimport static_cast
from libcpp.vector cimport vector

# Cython operator workarounds.
from cython.operator cimport address as addr       # &foo
from cython.operator cimport dereference as deref  # *foo
from cython.operator cimport preincrement as inc   # ++foo

# Python imports.
import atexit
import collections
import numbers
import subprocess
import logging


cdef size_t SIZE_MAX = <size_t> -1


# Custom exceptions.


class FstError(Exception):

  pass


class FstArgError(FstError, ValueError):

  pass


class FstBadWeightError(FstError, ValueError):

  pass


class FstDeletedConstructorError(FstError, RuntimeError):

  pass


class FstIndexError(FstError, IndexError):

  pass


class FstIOError(FstError, IOError):

  pass


class FstOpError(FstError, RuntimeError):

  pass


class FstUnknownWeightTypeError(FstError, ValueError):

  pass


# Various helpers used throughout.


cdef string tostring(data, string encoding=b"UTF-8") except *:
  """Converts strings to bytestrings.

  This function converts Python bytestrings and Unicode strings to bytestrings
  encoded in UTF-8. It is used to process most Python string arguments before
  passing them to the lower-level library.

  Args:
    data: A Unicode string or bytestring.
    encoding: The desired encoding, defaulting to UTF-8.

  Returns:
    A bytestring.

  Raises:
    FstArgError: Cannot encode string.
    UnicodeEncodeError.

  This function is not visible to Python users.
  """
  # A Python bytestring can be implicitly cast to a C++ string.
  if isinstance(data, bytes):
    return data
  elif isinstance(data, unicode):
    return data.encode(encoding)
  raise FstArgError("Cannot encode as string: {!r}".format(data))


cdef string weighttostring(data, string encoding=b"UTF-8") except *:
  """Converts strings or numerics to bytestrings.

  This function converts Python bytestrings, Unicode strings, and numerics
  which can be cast to floats to bytestrings encoded in UTF-8. It is used to
  process Python string arguments so they can be used to construct Weight
  objects. In most cases, weights are underlyingly floating-point, but since
  not all weights are, they can only be constructed using a string.

  Args:
    data: A Unicode string, bytestring, or type which can be converted to a
      Python float.

  Returns:
    A bytestring.

  Raise:
    FstArgError: Cannot encode string.
    ValueError: Invalid literal for float.
    UnicodeEncodeError.

  This function is not visible to Python users.
  """
  # A Python bytestring can be implicitly cast to a C++ string.
  if isinstance(data, bytes):
    return data
  elif isinstance(data, unicode):
    return data.encode(encoding)
  elif isinstance(data, numbers.Number):
    return bytes(float(data))
  raise FstArgError("Cannot encode as string: {!r}".format(data))


cdef fst.ComposeFilter _get_compose_filter(string cf) except *:
  """Matches string with the appropriate ComposeFilter enum value.

  This function takes a string argument and returns the matching ComposeFilter
  enum value used to initialize ComposeOptions instances. ComposeOptions is used
  by difference and intersection in addition to composition.

  Args:
    cf: A string matching a known composition filter; one of: "alt_sequence",
        "auto", "match", "null", "sequence", "trivial".

  Returns:
    A ComposeFilter enum value.

  Raises:
    FstArgError: Unknown compose filter type.

  This function is not visible to Python users.
  """
  if cf == b"alt_sequence":
    return fst.ALT_SEQUENCE_FILTER
  if cf == b"auto":
    return fst.AUTO_FILTER
  if cf == b"match":
    return fst.MATCH_FILTER
  if cf == b"null":
    return fst.NULL_FILTER
  if cf == b"sequence":
    return fst.SEQUENCE_FILTER
  if cf == b"trivial":
    return fst.TRIVIAL_FILTER
  raise FstArgError("Unknown compose filter type: {!r}".format(cf))


cdef uint32 _get_encode_mapper_flags(bool encode_labels, bool encode_weights):
  """Converts encoder booleans to an encoder flag bitmask.

  This function takes as arguments two booleans indicating whether arc labels
  and/or arc weights are to be encoded.

  Args:
    encode_labels: Should labels be encoded?
    encode_weights: Should weights be encoded?

  Returns:
    An unsigned integer that can be passed as the "flags" argument to create an
    encoder.

  This function is not visible to Python users.
  """
  cdef uint32 flags = 0
  if encode_labels:
    flags |= fst.kEncodeLabels
  if encode_weights:
    flags |= fst.kEncodeWeights
  if not flags:
    logging.warning("Specified encoder will perform an identity-mapping; did "
                    "you mean to request label and/or weight encoding?")
  return flags


cdef fst.QueueType _get_queue_type(string qt) except *:
  """Matches string with the appropriate QueueType enum value.

  This function takes a string argument and returns the matching QueueType enum
  value passed to the RmEpsilonOptions constructor.

  Args:
    qt: A string matching a known queue type; one of: "auto", "fifo", "lifo",
        "shortest", "state", "top".

  Returns:
    A QueueType enum value.

  Raises:
    FstArgError: Unknown queue type.

  This function is not visible to Python users.
  """
  if qt == b"auto":
    return fst.AUTO_QUEUE
  if qt == b"fifo":
    return fst.FIFO_QUEUE
  if qt == b"lifo":
    return fst.LIFO_QUEUE
  if qt == b"shortest":
    return fst.SHORTEST_FIRST_QUEUE
  if qt == b"state":
    return fst.STATE_ORDER_QUEUE
  if qt == b"top":
    return fst.TOP_ORDER_QUEUE
  raise FstArgError("Unknown queue type: {!r}".format(qt))


cdef fst.RandArcSelection _get_rand_arc_selection(string ras) except *:
  """Matches string with the appropriate RandArcSelection enum value.

  This function takes a string argument and returns the matching
  RandArcSelection enum value passed to the RandGenOptions constructor.

  Args:
    ras: A string matching a known random arc selection type; one of: "uniform",
        "log_prob", "fast_log_prob".

  Returns:
    A RandArcSelection enum value.

  Raises:
    FstArgError: Unknown random arc selection type.

  This function is not visible to Python users.
  """
  if ras == b"uniform":
    return fst.UNIFORM_ARC_SELECTOR
  if ras == b"log_prob":
    return fst.LOG_PROB_ARC_SELECTOR
  if ras == b"fast_log_prob":
    return fst.FAST_LOG_PROB_ARC_SELECTOR
  raise FstArgError("Unknown random arc selection type: {!r}".format(ras))


cdef fst.ReplaceLabelType _get_replace_label_type(string rlt,
    bool epsilon_on_replace) except *:
  """Matches string with the appropriate ReplaceLabelType enum value.

  This function takes a string argument and returns the matching
  ReplaceLabelType enum value passed to the ReplaceOptions constructor.

  Args:
    rlt: A string matching a known replace label type; one of: "neither",
        "input", "output", "both".
    epsilon_on_replace: Should call/return arcs be epsilon arcs?

  Returns:
    A ReplaceLabelType enum value.

  Raises:
    FstArgError: Unknown replace label type.

  This function is not visible to Python users.
  """
  if rlt == b"neither" or epsilon_on_replace:
    return fst.REPLACE_LABEL_NEITHER
  if rlt == b"input":
    return fst.REPLACE_LABEL_INPUT
  if rlt == b"output":
    return fst.REPLACE_LABEL_OUTPUT
  if rlt == b"both":
    return fst.REPLACE_LABEL_BOTH
  raise FstArgError("Unknown replace label type: {!r}".format(rlt))


cdef class Weight(object):

  """
  Weight(weight_type, weight_string)

  FST weight class.

  This class represents an FST weight. When passed as an argument to an FST
  operation, it should have the weight type of the input FST(s) to said
  operation.

  Args:
    weight_type: A string indicating the weight type.
    weight_string: A string indicating the underlying weight.

  Raises:
    FstBadWeightError: invalid weight.
    FstUnknownWeightTypeError: weight type not found.

  Attributes:
    type: A string indicating the weight type.
    string: A string indicating the underlying weight.
  """

  def __repr__(self):
    return "<{} Weight {} at 0x{:x}>".format(self.type, self.string, id(self))

  def __str__(self):
    return self._weight.ToString()

  def __init__(self, weight_type, weight):
    self._weight = new fst.WeightClass(tostring(weight_type),
                                       weighttostring(weight))
    if self._string() == b"BadNumber":
      raise FstBadWeightError(weight)
    if self._type() == b"none":
      raise FstUnknownWeightTypeError(weight_type)

  # To get around the inability to declare cdef class methods, we define
  # the C part out-of-class and then call it from within.

  @classmethod
  def Zero(cls, weight_type):
    """
    Weight.Zero(weight_type)
    """
    return _Weight_Zero(weight_type)

  @classmethod
  def One(cls, weight_type):
    """
    Weight.One(weight_type)
    """
    return _Weight_One(weight_type)

  @classmethod
  def NoWeight(cls, weight_type):
    """
    Weight.NoWeight(weight_type)
    """
    return _Weight_NoWeight(weight_type)

  def __dealloc__(self):
    del self._weight

  def __richcmp__(Weight x, Weight y, int op):
    # This is useful for unit tests.
    if op == 2:  # `==`
      return (x._weight.Type() == y._weight.Type() and
              x._weight.ToString() == y._weight.ToString())
    elif op == 3:  # `!=`
      return not (x == y)
    else:
      raise NotImplementedError("Invalid operator {!r}".format(op))

  cdef string _string(self):
    return self._weight.ToString()

  @property
  def string(self):
    return self._string()

  cdef string _type(self):
    return self._weight.Type()

  @property
  def type(self):
    return self._type()


cdef fst.WeightClass _get_WeightClass_or_Zero(const string &weight_type,
                                              weight) except *:
  """Converts weight string to WeightClass instance.

  This function constructs a WeightClass instance of the desired weight type.
  If the first argument is null, the weight is set to semiring Zero.

  Args:
    weight_type: A string denoting the desired weight type.
    weight: A object indicating the desired weight; if omitted, the weight is
        set to semiring Zero.

  Returns:
    A WeightClass instance.

  This function is not visible to Python users.
  """
  cdef fst.WeightClass result
  if weight is None:
    result = fst.WeightClass.Zero(weight_type)
  elif isinstance(weight, Weight):
    result = deref(<WeightClass_ptr> (<Weight> weight)._weight)
  else:
    result = fst.WeightClass(weight_type, weighttostring(weight))
    if result.ToString() == b"BadNumber":
      raise FstBadWeightError(weight)
  return result


cdef fst.WeightClass _get_WeightClass_or_One(const string &weight_type,
                                             weight) except *:
  """Converts weight string to WeightClass instance.

  This function constructs a WeightClass instance of the desired weight type.
  If the first argument is null, the weight is set to semiring One.

  Args:
    weight_type: A string denoting the desired weight type.
    weight: A object indicating the desired weight; if omitted, the weight is
        set to semiring One.

  Returns:
    A WeightClass instance.

  This function is not visible to Python users.
  """
  cdef fst.WeightClass result
  if weight is None:
    result = fst.WeightClass.One(weight_type)
  elif isinstance(weight, Weight):
    result = deref(<WeightClass_ptr> (<Weight> weight)._weight)
  else:
    result = fst.WeightClass(weight_type, weighttostring(weight))
    if result.ToString() == b"BadNumber":
      raise FstBadWeightError(weight)
  return result


cdef Weight _Weight_Zero(weight_type):
  cdef Weight result = Weight.__new__(Weight)
  result._weight = new fst.WeightClass(fst.WeightClass.Zero(
      tostring(weight_type)))
  if result._weight.Type() == b"none":
    raise FstUnknownWeightTypeError(weight_type)
  return result


cdef Weight _Weight_One(weight_type):
  cdef Weight result = Weight.__new__(Weight)
  result._weight = new fst.WeightClass(
        fst.WeightClass.One(tostring(weight_type)))
  if result._weight.Type() == b"none":
    raise FstUnknownWeightTypeError(weight_type)
  return result


cdef Weight _Weight_NoWeight(weight_type):
  cdef Weight result = Weight.__new__(Weight)
  result._weight = new fst.WeightClass(
        fst.WeightClass.NoWeight(tostring(weight_type)))
  return result


# SymbolTable hierarchy:
#
# _SymbolTable: abstract base class; has-a SymbolTable*.
# _ConstSymbolTable(_SymbolTable): adds repr method.
# SymbolTable(_SymbolTable): adds mutable methods and Py-accessible constructor.
#
# NB: The underlying pointer is "owned" by this class (meaning that it is
# deallocated upon garbage collection) only when the SymbolTable constructor
# is invoked, or when it is constructed via a copy operation.
#
# SymbolTableIterator is also exposed.


cdef class _SymbolTable(object):

  """
  (No constructor.)

  Base class for the symbol table hierarchy.

  This class is the base class for SymbolTable. It has a "deleted" constructor
  and implementations for the const methods of the wrapped SymbolTable.
  """

  # NB: Do not expose any non-const methods of the wrapped SymbolTable here.
  # Doing so will allow undefined behavior.

  def __cinit__(self):
    self._owner = False

  def __init__(self):
    raise FstDeletedConstructorError(
        "Cannot construct {}".format(self.__class__.__name__))

  def __dealloc__(self):
    if self._owner:
      del self._table

  def __iter__(self):
    return SymbolTableIterator(self)

  cdef int64 _available_key(self):
    return self._table.AvailableKey()

  @property
  def available_key(self):
    return self._available_key()

  cdef string _checksum(self):
    return self._table.CheckSum()

  @property
  def checksum(self):
    return self._checksum()

  cpdef SymbolTable copy(self):
    """
    copy(self)

    Returns a mutable copy of the SymbolTable.
    """
    return _init_SymbolTable(self._table.Copy(), True)

  def find(self, key):
    """
    find(self, key)

    Given a symbol or index, finds the other one.

    This method returns the index associated with a symbol key, or the symbol
    associated with a index key.

    Args:
      key: Either a string or an index.

    Returns:
      If key is a string, the associated index; if key is an integer, the
          associated symbol.

    Raises:
      KeyError: Key not found.
    """
    try:
      result = self._table.FindLabel(tostring(key))
      if result == -1:
        raise KeyError(key)
    except FstArgError:
      result = self._table.FindSymbol(key)
      if result == b"":
        raise KeyError(key)
    return result

  cpdef int64 get_nth_key(self, ssize_t pos) except *:
    """
    get_nth_key(self, pos)

    Retrieves the integer index of the n-th key in the table.

    Args:
      pos: The n-th key to retrieve.

    Returns:
      The integer index of the n-th key.

    Raises:
      KeyError: index not found.
    """
    cdef int64 result = self._table.GetNthKey(pos)
    if result == -1:
      raise KeyError(pos)
    return result

  cdef string _labeled_checksum(self):
    return self._table.LabeledCheckSum()

  @property
  def labeled_checksum(self):
    return self._labeled_checksum()

  cdef string _name(self):
    return self._table.Name()

  @property
  def name(self):
    return self._name()

  cdef size_t _num_symbols(self):
    return self._table.NumSymbols()

  @property
  def num_symbols(self):
    return self._num_symbols()

  cpdef void write(self, filename) except *:
    """
    write(self, filename)

    Serializes symbol table to a file.

    This methods writes the SymbolTable to a file in binary format.

    Args:
      filename: The string location of the output file.

    Raises:
      FstIOError: Write failed.
    """
    if not self._table.Write(tostring(filename)):
      raise FstIOError("Write failed: {!r}".format(filename))

  cpdef void write_text(self, filename) except *:
    """
    write_text(self, filename)

    Writes symbol table to text file.

    This method writes the SymbolTable to a file in human-readable format.

    Args:
      filename: The string location of the output file.

    Raises:
      FstIOError: Write failed.
    """
    if not self._table.WriteText(tostring(filename)):
      raise FstIOError("Write failed: {!r}".format(filename))


cdef class _ConstSymbolTable(_SymbolTable):

  """
  (No constructor.)

  Immutable SymbolTable class.

  This class wraps a library const SymbolTable and exposes const methods of the
  wrapped object. It is only to be returned by method, never constructed
  directly.

  Attributes:
    available_key: An integer indicating the next available key index in the
        table.
    checksum: A string indicating the label-agnostic MD5 checksum for the table.
    labeled_checksum: A string indicating the label-dependent MD5 checksum for
        the table.
    name: A string indicating the table's name.
    num_symbols: An integer indicating the number of symbols in the table.
  """

  # NB: Do not expose any non-const methods of the wrapped SymbolTable here.
  # Doing so will allow undefined behavior.

  def __repr__(self):
    return "<const SymbolTable {!r} at 0x{:x}>".format(self.name, id(self))


cdef class SymbolTable(_SymbolTable):

  """
  SymbolTable(name="<unspecified>")

  Mutable SymbolTable class.

  This class wraps the library SymbolTable and exposes both const (i.e.,
  access) and non-const (i.e., mutation) methods of wrapped object. Unlike
  other classes in the hierarchy, it has a working constructor and can be used
  to programmatically construct a SymbolTable in memory.

  Args:
    name: A string indicating the table's name

  Attributes:
    available_key: An integer indicating the next available key index in the
        table.
    checksum: A string indicating the label-agnostic MD5 checksum for the table.
    labeled_checksum: A string indicating the label-dependent MD5 checksum for
        the table.
    name: A string indicating the table's name.
    num_symbols: An integer indicating the number of symbols in the table.
  """

  def __repr__(self):
    return "<SymbolTable {!r} at 0x{:x}>".format(self.name, id(self))

  def __cinit__(self):
    self._owner = True

  def __init__(self, name=b"<unspecified>"):
    self._table = new fst.SymbolTable(tostring(name))

  @classmethod
  def read(cls, filename):
    """
    SymbolTable.read(filename)

    Reads symbol table from binary file.

    This class method creates a new SymbolTable from a symbol table binary file.

    Args:
      filename: The string location of the input binary file.

    Returns:
      A new SymbolTable instance.

    See also: `SymbolTable.read_fst`, `SymbolTable.read_text`.
    """
    return _init_SymbolTable(fst.SymbolTable.Read(tostring(filename)), True)

  @classmethod
  def read_text(cls, filename):
    """
    SymbolTable.read_text(filename)

    Reads symbol table from text file.

    This class method creates a new SymbolTable from a symbol table text file.

    Args:
      filename: The string location of the input text file.

    Returns:
      A new SymbolTable instance.

    See also: `SymbolTable.read`, `SymbolTable.read_fst`.
    """
    return _init_SymbolTable(fst.SymbolTable.ReadText(tostring(filename)), True)

  @classmethod
  def read_fst(cls, filename, bool input_table):
    """
    SymbolTable.read_fst(filename, input_table)

    Reads symbol table from an FST file without loading the corresponding FST.

    This class method creates a new SymbolTable by reading either the input or
    output symbol table from an FST file, without loading the corresponding FST.

    Args:
      filename: The string location of the input FST file.
      input_table: Should the input table be read (True) or the output table
          (False)?

    Returns:
      A new SymbolTable instance, or None if none can be read.

    Raises:
      FstIOError: Read failed.

    See also: `SymbolTable.read`, `SymbolTable.read_text`.
    """
    cdef SymbolTable_ptr tsyms = fst.FstReadSymbols(filename, input_table)
    if tsyms == NULL:
      raise FstIOError("Read failed: {!r}".format(filename))
    return _init_SymbolTable(tsyms)

  cpdef int64 add_symbol(self, symbol, int64 key=-1):
    """
    add_symbol(self, symbol, key=-1)

    Adds a symbol to the table and returns the index.

    This method adds a symbol to the table. The caller can optionally
    specify a non-negative integer index for the key.

    Args:
      symbol: A symbol string.
      key: An index for the symbol (-1 is reserved for "no symbol requested").

    Returns:
      The integer key of the new symbol.
    """
    cdef symbol_string = tostring(symbol)
    if key != -1:
      return self._table.AddSymbol(symbol_string, key)
    else:
      return self._table.AddSymbol(symbol_string)

  cpdef void add_table(self, _SymbolTable syms):
    """
    add_table(self, syms)

    Adds another SymbolTable to this table.

    This method merges another symbol table into the current table. All key
    values will be offset by the current available key.

    Args:
      syms: A SymbolTable to be merged with the current table.
    """
    self._table.AddTable(deref(syms._table))

  cpdef void set_name(self, new_name) except *:
    self._table.SetName(tostring(new_name))


cdef SymbolTable _init_SymbolTable(SymbolTable_ptr table, bool owner=True):
  cdef SymbolTable result = SymbolTable.__new__(SymbolTable)
  result._table = table
  result._owner = owner
  return result



# The caller must cast the pointer themselves, because reasons.
cdef _ConstSymbolTable _init_ConstSymbolTable(SymbolTable_ptr table):
  cdef _ConstSymbolTable result = _ConstSymbolTable.__new__(_ConstSymbolTable)
  result._table = table
  return result



# Constructive SymbolTable operations.


cpdef SymbolTable compact_symbol_table(_SymbolTable syms):
  """
  compact_symbol_table(syms)

  Constructively relabels a SymbolTable to make it a contiguous mapping.

  Args:
    syms: Input SymbolTable.

  Returns:
    A new compacted SymbolTable.
  """
  return _init_SymbolTable(fst.CompactSymbolTable(deref(syms._table)))


cpdef SymbolTable merge_symbol_table(_SymbolTable lhs, _SymbolTable rhs):
  """
  merge_symbol_table(lhs, rhs)

  Merges all symbols from the left table into the right.

  This function creates a new SymbolTable which is the merger of the two input
  symbol Tables. Symbols in the right-hand table that conflict with those in the
  left-hand table will be assigned values from the left-hand table. Thus the
  returned table will never modify symbol assignments from the left-hand side,
  but may do so on the right.

  If the left-hand table is associated with an FST, it may be necessary to
  relabel it using the output table.

  Args:
    lhs: Left-hand side SymbolTable.
    rhs: Left-hand side SymbolTable.

  Returns:
    A new merged SymbolTable.

  See also: `relabel_symbols`.
  """
  return _init_SymbolTable(fst.MergeSymbolTable(deref(lhs._table),
                                                deref(rhs._table), NULL))


# SymbolTable iteration.


cdef class SymbolTableIterator(object):

  """
  SymbolTableIterator(syms)

  This class is used for iterating over a symbol table using a Pythonic API. It
  also supports the C++ API methods, but most users should simply place a
  SymbolTable in an iteration context and take advantage of the Pythonic API
  that provides.
  """

  def __repr__(self):
    return "<SymbolTableIterator at 0x{:x}>".format(id(self))

  def __init__(self, _SymbolTable syms):
    self._siter = new fst.SymbolTableIterator(deref(syms._table))

  def __dealloc__(self):
    del self._siter

  # This just registers this class as a possible iterator.
  def __iter__(self):
    return self

  # Magic method used to get a Pythonic API out of the C++ API.
  def __next__(self):
    if self.done():
      raise StopIteration
    cdef int64 value = self.value()
    cdef string symbol = self.symbol()
    self.next()
    return (value, symbol)

  cpdef bool done(self):
    """
    done(self)

    Indicates whether the iterator is exhausted or not.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.

    Returns:
      True if the iterator is exhausted, False otherwise.
    """
    return self._siter.Done()

  cpdef void next(self):
    """
    next(self)

    Advances the iterator.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.
    """
    self._siter.Next()

  cpdef void reset(self):
    """
    reset(self)

    Resets the iterator to the initial position.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.
    """
    self._siter.Reset()

  cpdef string symbol(self):
    """
    symbol(self)

    Returns the current symbol string.

    This method returns the current symbol string at this point in the table.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.

    Returns:
      A symbol string.
    """
    return self._siter.Symbol()

  cpdef int64 value(self):
    """
    value(self)

    Returns the current integer index of the symbol.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.

    Returns:
      An integer index.
    """
    return self._siter.Value()


# FST-based symbol table functions are defined after the Fst class hierarchy
# and operations.



cdef class EncodeMapper(object):

  """
  EncodeMapper(arc_type="standard", encode_labels=False, encode_weights=False)

  Arc encoder class, wrapping EncodeMapperClass.

  This class provides an object which can be used to encode or decode FST arcs.
  This is most useful to convert an FST to an unweighted acceptor, on which
  some FST operations are more efficient, and then decoding the FST afterwards.

  To use an instance of this class to encode or decode a mutable FST, pass it
  as the first argument to the FST instance methods `encode` and `decode`.

  For implementational reasons, it is not currently possible to use an encoder
  on disk to construct this class.

  Args:
    arc_type: A string indicating the arc type.
    encode_labels: Should labels be encoded?
    encode_weights: Should weights be encoded?

  Attributes:
    arc_type: A string indicating the arc type.
    encode_type: A string indicating the encoder type.
    flags: An unsigned integer representing the encoder flags
    input_symbols: The input symbol table, or None if none is set.
    output_symbols: The output symbol table, or None if none is set.
    weight_type: A string indicating the weight type.
  """

  def __init__(self, arc_type=b"standard", bool encode_labels=False,
               bool encode_weights=False):
    cdef uint32 flags = _get_encode_mapper_flags(encode_labels, encode_weights)
    self._encoder = new fst.EncodeMapperClass(tostring(arc_type), flags,
                                              fst.ENCODE)
    if self._encoder == NULL:
      raise FstOpError("Unknown arc type: {!r}".format(arc_type))

  cdef string _arc_type(self):
    return self._encoder.ArcType()

  @property
  def arc_type(self):
    return self._arc_type()

  # Python's equivalent to operator().

  def __call__(self, int64 ilabel, int64 olabel, weight, int64 nextstate):
    """
    self(state, ilabel, olabel, weight, nextstate)

    Uses the encoder to encode an arc.

    Args:
      ilabel: The integer index of the input label.
      olabel: The integer index of the output label.
      weight: A Weight or weight string indicating the desired final weight; if
        null, it is set to semiring One.
      nextstate: The integer index of the destination state.

    Raises:
      FstOpError: Incompatible or invalid weight.
    """
    cdef fst.WeightClass wc = _get_WeightClass_or_One(self._weight_type(),
                                                      weight)
    return _init_Arc(self._encoder.__call__(fst.ArcClass(ilabel, olabel, wc,
                                                         nextstate)))

  cdef uint32 _flags(self):
    return self._encoder.Flags()

  @property
  def flags(self):
    return self._flags()

  cdef _ConstSymbolTable _input_symbols(self):
    if self._encoder.InputSymbols() == NULL:
      return
    return _init_ConstSymbolTable(const_cast[SymbolTable_ptr](
        self._encoder.InputSymbols()))

  @property
  def input_symbols(self):
    return self._input_symbols()

  cdef _ConstSymbolTable _output_symbols(self):
    if self._encoder.OutputSymbols() == NULL:
      return
    return _init_ConstSymbolTable(const_cast[SymbolTable_ptr](
        self._encoder.OutputSymbols()))

  @property
  def output_symbols(self):
    return self._output_symbols()

  cpdef uint64 properties(self, uint64 mask):
    """
    properties(self, mask)

    Provides property bits.

    This method provides user access to the properties attributes for the
    encoder. The resulting value is a long integer, but when it is cast to a
    boolean, it represents whether or not the FST has the `mask` property.

    Args:
      mask: The property mask to be compared to the encoder's properties.

    Returns:
      A 64-bit bitmask representing the requested properties.
    """
    return self._encoder.Properties(mask)

  cpdef void set_input_symbols(self, _SymbolTable syms) except *:
    """
    set_input_symbols(self, syms)

    Sets the input symbol table.

    Args:
      syms: A SymbolTable.

    See also: `set_output_symbols`.
    """
    self._encoder.SetInputSymbols(syms._table)

  cpdef void set_output_symbols(self, _SymbolTable syms) except *:
    """
    set_output_symbols(self, syms)

    Sets the output symbol table.

    Args:
      syms: A SymbolTable.

    See also: `set_input_symbols`.
    """
    self._encoder.SetOutputSymbols(syms._table)

  cdef string _weight_type(self):
    return self._encoder.WeightType()

  @property
  def weight_type(self):
    return self._weight_type()


# Fst hierarchy:
#
# _Fst: base class; has-a FstClass*.
# _MutableFst(_Fst): adds mutable methods.
# Fst(filename): pseudo-constructor.


cdef class _Fst(object):

  """
  (No constructor.)

  Immutable FST class, wrapping FstClass.

  This class is the basic user-facing FST object. It does not itself support any
  mutation operations.

  Attributes:
    arc_type: A string indicating the arc type.
    input_symbols: The input symbol table, or None if none is set.
    fst_type: A string indicating the FST (container) type.
    output_symbols: The output symbol table, or None if none is set.
    start: The integer state ID for the start state.
    weight_type: A string indicating the weight type.
  """

  # IPython notebook magic to produce an SVG of the FST.

  def _repr_svg_(self):
    """IPython notebook magic to produce an SVG of the FST using GraphViz.

    This method produces an SVG of the internal graph. Users wishing to create
    publication-quality graphs should instead use the method `draw`, which
    exposes additional parameters.

    Raises:
      OSError: Cannot locate the `dot` executable.
      subprocess.CalledProcessError: `dot` returned non-zero exit code.

    See also: `draw`, `text`.
    """
    # Throws OSError if the dot executable is not found.
    proc = subprocess.Popen(["dot", "-Tsvg"], stdin=subprocess.PIPE,
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    cdef stringstream sstrm
    fst.DrawFst(deref(self._fst), self._fst.InputSymbols(),
                self._fst.OutputSymbols(), NULL, self.properties(ACCEPTOR),
                b"", 8.5, 11, True, False, 0.4, 0.25, 14, 5, False,
                addr(sstrm), b"_repr_svg")
    (sout, serr) = proc.communicate(sstrm.str())
    if proc.returncode != 0:  # Just to be explicit.
      raise subprocess.CalledProcessError(proc.returncode, self._DOT_TSVG)
    return sout

  def __repr__(self):
    return "<{} Fst at 0x{:x}>".format(self.fst_type, id(self))

  def __init__(self):
    raise FstDeletedConstructorError(
        "Cannot construct {}".format(self.__class__.__name__))

  def __dealloc__(self):
    del self._fst

   # Other magic methods.

  def __str__(self):
    return self.text()

  cdef string _arc_type(self):
    return self._fst.ArcType()

  @property
  def arc_type(self):
    return self._arc_type()

  cpdef ArcIterator arcs(self, int64 state):
    """
    arcs(self, state)

    Returns an iterator over arcs leaving some state=.

    Args:
      s: The source state ID.

    Returns:
      An ArcIterator over arcs leaving state `state`.

    See also: `mutable_arcs`, `states`.
    """
    return ArcIterator(self, state)

  cpdef _Fst copy(self):
    """
    copy(self)

    Makes a copy of the FST.
    """
    cdef fst.FstClass *tfst = new fst.FstClass(deref(self._fst))
    return _init_XFst(tfst)

  cpdef void draw(self, filename, _SymbolTable isymbols=None,
                  _SymbolTable osymbols=None, SymbolTable ssymbols=None,
                  bool acceptor=False, title=b"", double width=8.5,
                  double height=11, bool portrait=False, bool vertical=False,
                  double ranksep=0.4, double nodesep=0.25, int32 fontsize=14,
                  int32 precision=5, bool show_weight_one=False):
    """
    draw(self, filename, isymbols=None, osymbols=None, ssymbols=None,
         acceptor=False, title="", width=8.5, height=11, portrait=False,
         vertical=False, ranksep=0.4, nodesep=0.25, fontsize=14,
         precision=5, show_weight_one=False):

    Writes out the FST in Graphviz text format.

    This method writes out the FST in the dot graph description language. The
    graph can be rendered using the `dot` executable provided by Graphviz.

    Args:
      filename: The string location of the output dot/Graphviz file.
      isymbols: An optional symbol table used to label input symbols.
      osymbols: An optional symbol table used to label output symbols.
      ssymbols: An optional symbol table used to label states.
      acceptor: Should the figure be rendered in acceptor format if possible?
      title: An optional string indicating the figure title.
      width: The figure width, in inches.
      height: The figure height, in inches.
      portrait: Should the figure be rendered in portrait rather than landscape
?
      vertical: Should the figure be rendered bottom-to-top rather than
          left-to-right?
      ranksep: The minimum separation separation between ranks, in inches.
      nodesep: The minimum separation between nodes, in inches.
      fontsize: Font size, in points.
      precision: Numeric precision for floats, in number of chars.
      show_weight_one: Should weights equivalent to semiring One be printed?

    For more information about the rendering options, see `man dot`.

    See also: `text`.
    """
    cdef string filename_string = tostring(filename)
    cdef ofstream *ostrm = new ofstream(filename_string.c_str())
    cdef SymbolTable_ptr ssymbols_ptr = NULL
    if ssymbols is not None:
      ssymbols_ptr = ssymbols._table
    fst.DrawFst(deref(self._fst),
        self._fst.InputSymbols() if isymbols is None else isymbols._table,
        self._fst.OutputSymbols() if osymbols is None else osymbols._table,
        ssymbols_ptr, acceptor, tostring(title), width, height, portrait,
        vertical, ranksep, nodesep, fontsize, precision, show_weight_one, ostrm,
        filename_string)
    del ostrm

  cpdef Weight final(self, int64 state):
    """
    final(self, state)

    Returns the final weight of a state.

    Args:
      state: The integer index of a state.

    Returns:
      The final Weight of that state.

    Raises:
      FstIndexError: State index out of range.
    """
    cdef Weight weight = Weight.__new__(Weight)
    weight._weight = new fst.WeightClass(self._fst.Final(state))
    return weight

  cdef string _fst_type(self):
    return self._fst.FstType()

  @property
  def fst_type(self):
    return self._fst_type()

  cdef _ConstSymbolTable _input_symbols(self):
    if self._fst.InputSymbols() == NULL:
      return
    return _init_ConstSymbolTable(const_cast[SymbolTable_ptr](
        self._fst.InputSymbols()))

  @property
  def input_symbols(self):
    return self._input_symbols()

  cdef _ConstSymbolTable _output_symbols(self):
    if self._fst.OutputSymbols() == NULL:
      return
    return _init_ConstSymbolTable(const_cast[SymbolTable_ptr](
        self._fst.OutputSymbols()))

  @property
  def output_symbols(self):
    return self._output_symbols()

  cpdef size_t num_arcs(self, int64 state) except *:
    """
    num_arcs(self, state)

    Returns the number of arcs leaving a state.

    Args:
      state: The integer index of a state.

    Returns:
      The number of arcs leaving that state.

    Raises:
      FstIndexError: State index out of range.

    See also: `num_states`.
    """
    cdef size_t result = self._fst.NumArcs(state)
    if result == SIZE_MAX:
      raise FstIndexError("State index out of range")
    return result

  cpdef size_t num_input_epsilons(self, int64 state) except *:
    """
    num_input_epsilsons(self, state)

    Returns the number of arcs with epsilon input labels leaving a state.

    Args:
      state: The integer index of a state.

    Returns:
      The number of epsilon-input-labeled arcs leaving that state.

    Raises:
      FstIndexError: State index out of range.

    See also: `num_output_epsilons`.
    """
    cdef size_t result = self._fst.NumInputEpsilons(state)
    if result == SIZE_MAX:
      raise FstIndexError("State index out of range")
    return result

  cpdef size_t num_output_epsilons(self, int64 state) except *:
    """
    num_output_epsilsons(self, state)

    Returns the number of arcs with epsilon output labels leaving a state.

    Args:
      state: The integer index of a state.

    Returns:
      The number of epsilon-output-labeled arcs leaving that state.

    Raises:
      FstIndexError: State index out of range.

    See also: `num_input_epsilons`.
    """
    cdef size_t result = self._fst.NumOutputEpsilons(state)
    if result == SIZE_MAX:
      raise FstIndexError("State index out of range")
    return result

  cpdef uint64 properties(self, uint64 mask, bool test=True):
    """
    properties(self, mask, test=True)

    Provides property bits.

    This method provides user access to the properties attributes for the FST.
    The resulting value is a long integer, but when it is cast to a boolean,
    it represents whether or not the FST has the `mask` property.

    Args:
      mask: The property mask to be compared to the FST's properties.
      test: Should any unknown values be computed before comparing against
          the mask?

    Returns:
      A 64-bit bitmask representing the requested properties.
    """
    return self._fst.Properties(mask, test)

  cdef int64 _start(self):
    return self._fst.Start()

  @property
  def start(self):
    return self._start()

  cpdef StateIterator states(self):
    """
    states(self)

    Returns an iterator over all states in the FST.

    Returns:
      A StateIterator object for the FST.

    See also: `arcs`, `mutable_arcs`.
    """
    return StateIterator(self)

  cpdef string text(self, _SymbolTable isymbols=None,
      _SymbolTable osymbols=None, _SymbolTable ssymbols=None,
      bool acceptor=False, bool show_weight_one=False, missing_sym=b""):
    """
    text(self, isymbols=None, osymbols=None, ssymbols=None, acceptor=False,
         show_weight_one=False, missing_sym="")

    Produces a human-readable string representation of the FST.

    This method generates a human-readable string representation of the FST.
    The caller may optionally specify SymbolTables used to label input labels,
    output labels, or state labels, respectively.

    Args:
      isyms: An optional symbol table used to label input symbols.
      osyms: An optional symbol table used to label output symbols.
      ssyms: An optional symbol table used to label states.
      acceptor: Should the FST be rendered in acceptor format if possible?
      show_weight_one: Should weights equivalent to semiring One be printed?
      missing_symbol: The string to be printed when symbol table lookup fails.

    Returns:
      A formatted string representing the machine.
    """
    # Prints FST to stringstream, then returns resulting string.
    cdef stringstream sstrm
    cdef SymbolTable_ptr ssymbols_ptr = NULL
    if ssymbols is not None:
      ssymbols_ptr = ssymbols._table
    fst.PrintFst(deref(self._fst), sstrm, __file__,
        self._fst.InputSymbols() if isymbols is None else isymbols._table,
        self._fst.OutputSymbols() if osymbols is None else osymbols._table,
        ssymbols_ptr, acceptor, show_weight_one, tostring(missing_sym))
    return sstrm.str()

  cpdef bool verify(self):
    """
    verify(self)

    Verifies that an FST's contents are sane.

    Returns:
      True if the contents are sane, False otherwise.
    """
    return fst.Verify(deref(self._fst))

  cdef string _weight_type(self):
    return self._fst.WeightType()

  @property
  def weight_type(self):
    return self._weight_type()

  cpdef void write(self, filename) except *:
    """
    write(self, filename)

    Serializes FST to a file.

    This method writes the FST to a file in a binary format.

    Args:
      filename: The string location of the output file.

    Raises:
      FstIOError: Write failed.
    """
    if not self._fst.Write(tostring(filename)):
      raise FstIOError("Write failed: {!r}".format(filename))


cdef class _MutableFst(_Fst):

  """
  (No constructor.)

  Mutable FST class, wrapping MutableFstClass.

  This class extends _Fst by adding mutation operations.

  Attributes:
    arc_type: A string indicating the arc type.
    fst_type: A string indicating the FST (container) type.
    input_symbols: The input symbol table, or None if none is set.
    num_states: The number of states.
    output_symbols: The output symbol table, or None if none is set.
    start: The integer state ID for the start state.
    states: An iterator over FST states.
    weight_type: A string indicating the weight type.
  """

  cdef void _check_mutating_imethod(self) except *:
    """Checks whether an operation mutating the FST has produced an error.

    This function is not visible to Python users.
    """
    if self.properties(ERROR):
      raise FstOpError("Operation failed")

  cpdef void add_arc(self, int64 state, int64 ilabel,
      int64 olabel, weight, int64 nextstate) except *:
    """
    add_arc(self, state, ilabel, olabel, weight, nextstate)

    Adds a new arc to the FST.

    Args:
      state: The integer index of the source state.
      ilabel: The integer index of the input label.
      olabel: The integer index of the output label.
      weight: A Weight or weight string indicating the desired final weight; if
        null, it is set to semiring One.
      nextstate: The integer index of the destination state.

    Raises:
      FstIndexError: State index out of range.
      FstOpdexError: Incompatible or invalid weight type.

    See also: `add_state`.
    """
    if not self._fst.ValidStateId(state):
      raise FstIndexError("State index out of range")
    if not self._mfst.AddArc(state, fst.ArcClass(ilabel, olabel,
        _get_WeightClass_or_One(self.weight_type, weight), nextstate)):
      raise FstOpError("Incompatible or invalid weight type")
    self._check_mutating_imethod()

  cpdef int64 add_state(self) except *:
    """
    add_state(self)

    Adds a new state to the FST.

    Returns:
      The integer index of the new state.

    See also: `add_arc`, `set_start`, `set_final`.
    """
    cdef int64 result = self._mfst.AddState()
    self._check_mutating_imethod()
    return result

  cpdef void arcsort(self, sort_type=b"ilabel") except *:
    """
    arcsort(self, sort_type="ilabel")

    Sorts arcs leaving each state of the FST.

    This operation destructively sorts arcs leaving each state using either
    input or output labels.

    Args:
      sort_type: Either "ilabel" (sort arcs according to input labels) or
          "olabel" (sort arcs according to output labels).

    Raises:
      FstArgError: Unknown sort type.

    See also: `topsort`.
    """
    cdef fst.ArcSortType ast
    sort_type = tostring(sort_type)
    if sort_type == b"ilabel":
      ast = fst.ILABEL_COMPARE
    elif sort_type == b"olabel":
      ast = fst.OLABEL_COMPARE
    else:
      raise FstArgError("Unknown sort type {!r}".format(sort_type))
    fst.ArcSort(self._mfst, ast)
    self._check_mutating_imethod()

  cpdef void closure(self, bool closure_plus=False) except *:
    """
    closure(self, closure_plus=False)

    Computes concatenative closure.

    This operation destructively converts the FST to its concatenative closure.
    If A transduces string x to y with weight a, then the closure transduces x
    to y with weight a, xx to yy with weight a \otimes a, xxx to yyy with weight
    a \otimes a \otimes a, and so on. The empty string is also transduced to
    itself with semiring One if `closure_plus` is False.

    Args:
      closure_plus: If False, do not accept the empty string.
    """
    fst.Closure(self._mfst, fst.CLOSURE_PLUS if closure_plus else
                            fst.CLOSURE_STAR)
    self._check_mutating_imethod()

  cpdef void concat(self, _Fst ifst) except *:
    """
    concat(self, ifst)

    Computes the concatenation (product) of two FSTs.

    This operation destructively concatenates the FST with a second FST. If A
    transduces string x to y with weight a and B transduces string w to v with
    weight b, then their concatenation transduces string xw to yv with weight a
    \otimes b.

    Args:
      ifst: The second input FST.
    """
    fst.Concat(self._mfst, deref(ifst._fst))
    self._check_mutating_imethod()

  cpdef void connect(self) except *:
    """
    connect(self)

    Removes unsuccessful paths.

    This operation destructively trims the FST, removing states and arcs that
    are not part of any successful path.
    """
    fst.Connect(self._mfst)
    self._check_mutating_imethod()

  cpdef void decode(self, EncodeMapper encoder) except *:
    """
    decode(self, encoder)

    Decodes encoded labels and/or weights.

    This operation reverses the encoding performed by `encode`.

    Args:
      encoder: An EncodeMapper object used to encode the FST.

    See also: `encode`.
    """
    fst.Decode(self._mfst, deref(encoder._encoder))
    self._check_mutating_imethod()

  cpdef void delete_arcs(self, int64 state, size_t n=0) except *:
    """
    delete_arcs(self, state, n=0)

    Deletes arcs leaving a particular state.

    Args:
      state: The integer index of a state.
      n: An optional argument indicating how many arcs to be deleted. If this
          argument is omitted or passed as zero, all arcs from this state are
          deleted.

    Raises:
      FstIndexError: State index out of range.

    See also: `delete_states`.
    """
    if not (self._mfst.DeleteArcs(state, n) if n else
            self._mfst.DeleteArcs(state)):
      raise FstIndexError("State index out of range")
    self._check_mutating_imethod()

  cpdef void delete_states(self, states=None) except *:
    """
    delete_states(self, states=None)

    Deletes states.

    Args:
      states: An optional iterable of integer indices of the states to be
          deleted. If this argument is omitted, all states are deleted.

    Raises:
      FstIndexError: State index out of range.

    See also: `delete_arcs`.
    """
    # Only the former signature has a possible indexing failure.
    if states:
      if not self._mfst.DeleteStates(<const vector[int64]> states):
        raise FstIndexError("State index out of range")
    else:
      self._mfst.DeleteStates()
    self._check_mutating_imethod()

  cpdef void encode(self, EncodeMapper encoder) except *:
    """
    encode(self, encoder)

    Encodes labels and/or weights.

    This operation allows for the representation of a weighted transducer as a
    weighted acceptor, an unweighted transducer, or an unweighted acceptor by
    considering the pair (input label, output label), the pair (input label,
    weight), or the triple (input label, output label, weight) as a single
    label. Applying this operation mutates the EncodeMapper argument, which
    can then be used to decode.

    Args:
      encoder: An EncodeMapper object to be used as the encoder.

    See also: `decode`.
    """
    fst.Encode(self._mfst, encoder._encoder)
    self._check_mutating_imethod()

  cpdef void invert(self) except *:
    """
    invert(self)

    Inverts the FST's transduction.

    This operation destructively inverts the FST's transduction by exchanging
    input and output labels.
    """
    fst.Invert(self._mfst)
    self._check_mutating_imethod()

  cpdef void minimize(self, float delta=fst.kDelta) except *:
    """
    minimize(self, delta=0.0009765625)

    Minimizes the FST.

    This operation destructively performs the minimization of deterministic
    weighted automata and transducers. If the input FST A is an acceptor, this
    operation produces the minimal acceptor B equivalent to A, i.e. the
    acceptor with a minimal number of states that is equivalent to A. If the
    input FST A is a transducer, this operation internally builds an equivalent
    transducer with a minimal number of states. However, this minimality is
    obtained by allowing transition having strings of symbols as output labels,
    this known in the litterature as a real-time transducer. Such transducers
    are not directly supported by the library. This function will convert such
    transducer by expanding each string-labeled transition into a sequence of
    transitions. This will results in the creation of new states, hence losing
    the minimality property.

    Args:
      delta: Comparison/quantization delta.
    """
    # This runs in-place when the second argument is null.
    fst.Minimize(self._mfst, NULL, delta)
    self._check_mutating_imethod()

  cpdef MutableArcIterator mutable_arcs(self, int64 state):
    """
    mutable_arcs(self, state)

    Returns a iterator over arcs leaving some state which supports
    arc mutation.

    Args:
      s: The source state ID.

    Returns:
      A MutableArcIterator over arcs leaving state `s`.

    See also: `arcs`, `states`.
    """
    return MutableArcIterator(self, state)

  cdef SymbolTable _mutable_input_symbols(self):
    cdef SymbolTable_ptr tst = self._mfst.MutableInputSymbols()
    if tst == NULL:
      return
    return _init_SymbolTable(tst, False)

  @property
  def mutable_input_symbols(self):
    return self._mutable_input_symbols()

  cdef SymbolTable _mutable_output_symbols(self):
    cdef SymbolTable_ptr tst = self._mfst.MutableOutputSymbols()
    if tst == NULL:
      return
    return _init_SymbolTable(tst, False)

  @property
  def mutable_output_symbols(self):
    return self._mutable_output_symbols()

  cdef int64 _num_states(self):
    return self._mfst.NumStates()

  @property
  def num_states(self):
    return self._num_states()

  cpdef void project(self, bool project_output=False) except *:
    """
    project(self, project_output=False)

    Converts the FST to an acceptor using input or output labels.

    This operation destructively projects an FST onto its domain or range by
    either copying each arc's input label to its output label (the default) or
    vice versa.

    Args:
      project_output: Should the output labels be projected?

    See also: `decode`, `encode`, `relabel_pairs`, `relabel_symbols`.
    """
    fst.Project(self._mfst, fst.PROJECT_OUTPUT if project_output else
                            fst.PROJECT_INPUT)
    self._check_mutating_imethod()

  cpdef void prune(self, float delta=fst.kDelta, int64 nstate=fst.kNoStateId,
                   weight=None) except *:
    """
    prune(self, delta=0.0009765625, nstate=-1, weight=None)

    Removes paths with weights below a certain threshold.

    This operation deletes states and arcs in the input FST that do not belong
    to a successful path whose weight is no more (w.r.t the natural semiring
    order) than the threshold t \otimes-times the weight of the shortest path in
    the input FST. Weights must be commutative and have the path property.

    Args:
      delta: Comparison/quantization delta.
      nstate: State number threshold.
      weight: A Weight or weight string indicating the desired weight threshold
          below which paths are pruned; if omitted, no paths are pruned.

    See also: The constructive variant.
    """
    # Threshold is set to semiring Zero (no pruning) if no weight is specified.
    cdef fst.WeightClass wc = _get_WeightClass_or_Zero(self.weight_type, weight)
    fst.Prune(self._mfst, wc, nstate, delta)
    self._check_mutating_imethod()

  cpdef void push(self, float delta=fst.kDelta, bool remove_total_weight=False,
                  bool to_final=False) except *:
    """
    push(self, delta=0.0009765625, remove_total_weight=False, to_final=False)

    Pushes weights towards the initial or final states.

    This operation destructively produces an equivalent transducer by pushing
    the weights towards the initial state or toward the final states. When
    pushing weights towards the initial state, the sum of the weight of the
    outgoing transitions and final weight at any non-initial state is equal to
    one in the resulting machine. When pushing weights towards the final states,
    the sum of the weight of the incoming transitions at any state is equal to
    one. Weights need to be left distributive when pushing towards the initial
    state and right distributive when pushing towards the final states.

    Args:
      delta: Comparison/quantization delta.
      remove_total_weight: If pushing weights, should the total weight be
          removed?
      to_final: Push towards final states?

    See also: The constructive variant, which also supports label pushing.
    """
    cdef fst.ReweightType rt = (fst.REWEIGHT_TO_FINAL if to_final else
                                fst.REWEIGHT_TO_INITIAL)
    fst.Push(self._mfst, rt, delta, remove_total_weight)
    self._check_mutating_imethod()

  cdef void _relabel_pairs(self, vector[fst.LabelPair] *ipairs,
                                 vector[fst.LabelPair] *opairs) except *:
    if ipairs.empty() and opairs.empty():
      raise FstArgError("No relabeling pairs specified.")
    fst.Relabel(self._mfst, deref(ipairs), deref(opairs))
    self._check_mutating_imethod()

  cpdef void relabel_pairs(self, ipairs=None, opairs=None) except *:
    """
    relabel_pairs(self, ipairs=None, opairs=None)

    Replaces input and/or output labels using pairs of labels.

    This operation destructively relabels the input and/or output labels of the
    FST using pairs of the form (old_ID, new_ID); omitted indices are
    identity-mapped.

    Args:
      ipairs: An iterable containing (older index, newer index) integer pairs.
      opairs: An iterable containing (older index, newer index) integer pairs.

    Raises:
      FstArgError: No relabeling pairs specified.

    See also: `decode`, `encode`, `project`, `relabel_tables`.
    """
    cdef vector[fst.LabelPair] *_ipairs = new vector[fst.LabelPair]()
    cdef vector[fst.LabelPair] *_opairs = new vector[fst.LabelPair]()
    cdef int64 before
    cdef int64 after
    if ipairs:
      for (before, after) in ipairs:
        _ipairs.push_back(fst.LabelPair(before, after))
    if opairs:
      for (before, after) in opairs:
        _opairs.push_back(fst.LabelPair(before, after))
    try:
      self._relabel_pairs(_ipairs, _opairs)
    finally:
      del _ipairs
      del _opairs

  cpdef void relabel_tables(self, _SymbolTable old_isymbols=None,
      _SymbolTable new_isymbols=None, bool attach_new_isymbols=True,
      _SymbolTable old_osymbols=None, _SymbolTable new_osymbols=None,
      bool attach_new_osymbols=True) except *:
    """
    relabel_tables(self, old_isymbols=None, new_isymbols=None,
                   attach_new_isymbols=True, old_osymbols=None,
                   new_osymbols=None, attach_new_osymbols=True)

    Replaces input and/or output labels using SymbolTables.

    This operation destructively relabels the input and/or output labels of the
    FST using user-specified symbol tables; omitted symbols are identity-mapped.

    Args:
       old_isymbols: The old SymbolTable for input labels, defaulting to the
          FST's input symbol table.
       new_isymbols: A SymbolTable used to relabel the input labels
       attach_new_isymbols: Should new_isymbols be made the FST's input symbol
          table?
       old_osymbols: The old SymbolTable for output labels, defaulting to the
          FST's output symbol table.
       new_osymbols: A SymbolTable used to relabel the output labels.
       attach_new_isymbols: Should new_osymbols be made the FST's output symbol
          table?
s
    Raises:
      FstArgError: No SymbolTable specified.

    See also: `decode`, `encode`, `project`, `relabel_pairs`.
    """
    if new_isymbols is None and new_osymbols is None:
      raise FstArgError("No new SymbolTables specified")
    cdef SymbolTable_ptr new_isymbols_ptr = NULL
    if new_isymbols is not None:
      new_isymbols_ptr = new_isymbols._table
    cdef SymbolTable_ptr new_osymbols_ptr = NULL
    if new_osymbols is not None:
      new_osymbols_ptr = new_osymbols._table
    fst.Relabel(self._mfst,
        self._fst.InputSymbols() if old_isymbols is None else
        old_isymbols._table, new_isymbols_ptr, attach_new_isymbols,
        self._fst.OutputSymbols() if old_osymbols is None else
        old_osymbols._table, new_osymbols_ptr, attach_new_osymbols)
    self._check_mutating_imethod()

  cpdef void reserve_arcs(self, int64 state, size_t n) except *:
    """
    reserve_arcs(self, state, n)

    Reserve n arcs at a particular state (best effort).

    Args:
      state: The integer index of a state.
      n: The number of arcs to reserve.

    Raises:
      FstIndexError: State index out of range.

    See also: `reserve_states`.
    """
    if not self._mfst.ReserveArcs(state, n):
      raise FstIndexError("State index out of range")
    self._check_mutating_imethod()

  cpdef void reserve_states(self, int64 n) except *:
    """
    reserve_states(self, n)

    Reserve n states (best effort).

    Args:
      n: The number of states to reserve.

    See also: `reserve_arcs`.
    """
    self._mfst.ReserveStates(n)
    self._check_mutating_imethod()

  cdef void _reweight(self, vector[fst.WeightClass] *potentials,
                      bool to_final=False) except *:
    cdef fst.ReweightType rt = (fst.REWEIGHT_TO_FINAL if to_final else
                                fst.REWEIGHT_TO_INITIAL)
    fst.Reweight(self._mfst, deref(potentials), rt)
    self._check_mutating_imethod()

  cpdef void reweight(self, potentials, bool to_final=False) except *:
    """
    reweight(self, potentials, to_final=False)

    Reweights an FST using an iterable of potentials.

    This operation destructively reweights an FST according to the potentials
    and in the direction specified by the user. An arc of weight w, with an
    origin state of potential p and destination state of potential q, is
    reweighted by p^{-1} \otimes (w \otimes q) when reweighting towards the
    initial state, and by (p \otimes w) \otimes q^{-1} when reweighting towards
    the final states. The weights must be left distributive when reweighting
    towards the initial state and right distributive when reweighting towards
    the final states (e.g., TropicalWeight and LogWeight).

    Args:
      potentials: An iterable of Weight or weight strings.
      to_final: Push towards final states?
    """
    cdef vector[fst.WeightClass] *_potentials = new vector[fst.WeightClass]()
    cdef string weight_type = self._weight_type()
    for weight in potentials:
        _potentials.push_back(_get_WeightClass_or_One(self.weight_type, weight))
    try:
      self._reweight(_potentials, to_final)
    finally:
      del _potentials
    self._check_mutating_imethod()

  cpdef void rmepsilon(self, bool connect=True, float delta=fst.kDelta,
                       int64 nstate=fst.kNoStateId, weight=None) except *:
    """
    rmepsilon(self, connect=True, delta=0.0009765625, nstate=-1, weight=None)

    Removes epsilon transitions.

    This operation destructively removes epsilon transitions, i.e., those where
    both input and output labels are epsilon) from an FST.

    Args:
      connect: Should output be trimmed?
      delta: Comparison/quantization delta.
      nstate: State number threshold.
      weight: A Weight or weight string indicating the desired weight threshold
          below which paths are pruned; if omitted, no paths are pruned.

    See also: The constructive variant, which also supports epsilon removal in
        reverse (and which may be more efficient).
    """
    # The threshold is set to semiring Zero (no pruning) if weight unspecified.
    cdef fst.WeightClass wc = _get_WeightClass_or_Zero(self.weight_type,
                                                        weight)
    fst.RmEpsilon(self._mfst, connect, wc, nstate, delta)
    self._check_mutating_imethod()

  cpdef void set_final(self, int64 state, weight=None) except *:
    """
    set_final(self, state, weight)

    Sets a state to be final with a fixed cost.

    Args:
      state: The integer index of a state.
      weight: A Weight or weight string indicating the desired final weight; if
          omitted, it is set to semiring One.

    Raises:
      FstIndexError: State index out of range.
      FstOpError: incompatible or invalid weight.

    See also: `set_start`.
    """
    if not self._mfst.ValidStateId(state):
      raise FstIndexError("State index out of range")
    cdef fst.WeightClass wc = _get_WeightClass_or_One(self.weight_type, weight)
    if not self._mfst.SetFinal(state, wc):
      raise FstOpError("incompatible or invalid weight")
    self._check_mutating_imethod()

  cpdef void set_properties(self, uint64 props, uint64 mask) except *:
    """
    set_properties(self, props, mask)

    Sets the properties bits.

    Args:
      props: The properties to be set.
      mask: A mask to be applied to the `props` argument before
        setting the FST's properties.
    """
    self._mfst.SetProperties(props, mask)

  cpdef void set_start(self, int64 state) except *:
    """
    set_start(self, state)

    Sets the initial state.

    Args:
      state: The integer index of a state.

    Raises:
      FstIndexError: State index out of range.

    See also: `set_final`.
    """
    if not self._mfst.SetStart(state):
      raise FstIndexError("State index out of range")
    self._check_mutating_imethod()

  cpdef void set_input_symbols(self, _SymbolTable syms) except *:
    """
    set_input_symbols(self, syms)

    Sets the input symbol table.

    Passing None as a value will delete the input symbol table.

    Args:
      syms: A SymbolTable.

    See also: `set_output_symbols`.
    """
    if syms is None:
      self._mfst.SetInputSymbols(NULL)
      return
    self._mfst.SetInputSymbols(syms._table)
    self._check_mutating_imethod()

  cpdef void set_output_symbols(self, _SymbolTable syms) except *:
    """
    set_output_symbols(self, syms)

    Sets the output symbol table.

    Passing None as a value will delete the output symbol table.

    Args:
      syms: A SymbolTable.

    See also: `set_input_symbols`.
    """
    if syms is None:
      self._mfst.SetOutputSymbols(NULL)
      return
    self._mfst.SetOutputSymbols(syms._table)
    self._check_mutating_imethod()

  cpdef void topsort(self) except *:
    """
    topsort(self)

    Sorts transitions by state IDs.

    This operation destructively topologically sorts the FST, if it is acyclic;
    otherwise it remains unchanged. Once sorted, all transitions are from lower
    state IDs to higher state IDs

    See also: `arcsort`.
    """
    # TopSort returns False if the FST is cyclic, and thus can't be TopSorted.
    if not fst.TopSort(self._mfst):
      logging.warning("Cannot topsort cyclic FST.")
    self._check_mutating_imethod()

  # Unfortunate clash with the C keyword.

  cdef void _union(self, _Fst ifst) except *:
    fst.Union(self._mfst, deref(ifst._fst))
    self._check_mutating_imethod()

  def union(self, _Fst ifst):
    """
    union(self, ifst)

    Computes the union (sum) of two FSTs.

    This operation computes the union (sum) of two FSTs. If A transduces string
    x to y with weight a and B transduces string w to v with weight b, then
    their union transduces x to y with weight a and w to v with weight b.

    Args:
      ifst: The second input FST.
    """
    self._union(ifst)
    self._check_mutating_imethod()


# Pseudo-constructors for _Fst and _MutableFst.
#
# _init_Fst and _init_MutableFst use an FstClass pointer to instantiate _Fst
# and _MutableFst objects, respectively. The latter function is only safe to
# call when the FST being wrapped is known to be kMutable. The caller can
# safely use it when they have either checked this bit (e.g., by using
# _init_XFst) or have themselves constructed a mutable container for the
# FstClass pointer they're passing (e.g., most of the constructive operations,
# storing their results in a VectorFstClass, a derivative of MutableFstClass).
#
# _create_Fst constructs an empty VectorFstClass of a user-specified arc type,
# and passes this pointer to _init_MutableFst.
#
# _read_Fst reads an FST from disk, performing FST conversion if requested, and
# then passes this pointer to _init_XFst.
#
# The Python class Fst provides a wrapper for these two operations. The former
# can be accessed by calling Fst(...), which acts like a class method, and the
# latter via Fst.read(...), which acts like a static method. This is a bit
# nasty, but totally hidden from the Python user.


cdef _Fst _init_Fst(FstClass_ptr tfst):
  if tfst.Properties(fst.kError, True):
    raise FstOpError("Operation failed")
  cdef _Fst ofst = _Fst.__new__(_Fst)
  ofst._fst = tfst
  return ofst


cdef _MutableFst _init_MutableFst(MutableFstClass_ptr tfst):
  if tfst.Properties(fst.kError, True):
    raise FstOpError("Operation failed")
  cdef _MutableFst ofst = _MutableFst.__new__(_MutableFst)
  ofst._mfst = ofst._fst = tfst
  return ofst


cdef _Fst _init_XFst(FstClass_ptr tfst):
  if tfst.Properties(fst.kMutable, True):
    return _init_MutableFst(static_cast[MutableFstClass_ptr](tfst))
  else:
    return _init_Fst(tfst)


cdef _MutableFst _create_Fst(arc_type=b"standard"):
  cdef fst.VectorFstClass *tfst = new fst.VectorFstClass(tostring(arc_type))
  if tfst == NULL:
    raise FstOpError("Unknown arc type: {!r}".format(arc_type))
  return _init_MutableFst(tfst)


cdef _Fst _read_Fst(filename, fst_type=None):
  cdef fst.FstClass *tfst = fst.FstClass.Read(tostring(filename))
  if tfst == NULL:
    raise FstIOError("Read failed: {!r}".format(filename))
  # Converts if requested.
  cdef string fst_type_string
  if fst_type:
    fst_type_string = tostring(fst_type)
    if fst_type_string != tfst.FstType():
      tfst = fst.Convert(deref(tfst), fst_type_string)
      if tfst == NULL:
        raise FstOpError("Conversion to {!r} failed.".format(fst_type))
  return _init_XFst(tfst)


class Fst(object):

   """
   Fst(arc_type="standard")

   Constructs an empty FST.

   Args:
     arc_type: A string indicating the arc type.

   Raises:
     FstError: Unknown arc type.

   Raises:
     FstOpError: operation failed.
   """

   def __new__(cls, arc_type=b"standard"):
    return _create_Fst(arc_type)

   @staticmethod
   def read(filename, fst_type=None):
     """
     read(filename, fst_type=None)

     Reads an FST from a file.

     Args:
       filename: The string location of the input file.
       fst_type: A string indicating the FST type to convert to; no conversion
         is performed if omitted or if the FST is already of the desired type.

     Returns:
       An FST object.

     Raises:
       FstIOError: Read failed.
       FstOpError: Read-time conversion failed.
     """
     return _read_Fst(filename, fst_type)


# Fst properties, exposed to Python users.


EXPANDED = fst.kExpanded
MUTABLE = fst.kMutable
ERROR = fst.kError
ACCEPTOR = fst.kAcceptor
NOT_ACCEPTOR = fst.kNotAcceptor
I_DETERMINISTIC = fst.kIDeterministic
NON_I_DETERMINISTIC = fst.kNonIDeterministic
O_DETERMINISTIC = fst.kODeterministic
NON_O_DETERMINISTIC = fst.kNonODeterministic
EPSILONS = fst.kEpsilons
NO_EPSILONS = fst.kNoEpsilons
I_EPSILONS = fst.kIEpsilons
NO_I_EPSILONS = fst.kNoIEpsilons
O_EPSILONS = fst.kOEpsilons
NO_O_EPSILONS = fst.kNoOEpsilons
I_LABEL_SORTED = fst.kILabelSorted
NOT_I_LABEL_SORTED = fst.kNotILabelSorted
O_LABEL_SORTED = fst.kOLabelSorted
NOT_O_LABEL_SORTED = fst.kNotOLabelSorted
WEIGHTED = fst.kWeighted
UNWEIGHTED = fst.kUnweighted
CYCLIC = fst.kCyclic
ACYCLIC = fst.kAcyclic
INITIAL_CYCLIC = fst.kInitialCyclic
INITIAL_ACYCLIC = fst.kInitialAcyclic
TOP_SORTED = fst.kTopSorted
NOT_TOP_SORTED = fst.kNotTopSorted
ACCESSIBLE = fst.kAccessible
NOT_ACCESSIBLE = fst.kNotAccessible
COACCESSIBLE = fst.kCoAccessible
NOT_COACCESSIBLE = fst.kNotCoAccessible
STRING = fst.kString
NOT_STRING = fst.kNotString
WEIGHTED_CYCLES = fst.kWeightedCycles
UNWEIGHTED_CYCLES = fst.kUnweightedCycles
NULL_PROPERTIES = fst.kNullProperties
COPY_PROPERTIES = fst.kCopyProperties
INTRINSIC_PROPERTIES = fst.kIntrinsicProperties
EXTRINSIC_PROPERTIES = fst.kExtrinsicProperties
SET_START_PROPERTIES = fst.kSetStartProperties
SET_FINAL_PROPERTIES = fst.kSetFinalProperties
ADD_STATE_PROPERTIES = fst.kAddStateProperties
ADD_ARC_PROPERTIES = fst.kAddArcProperties
SET_ARC_PROPERTIES = fst.kSetArcProperties
DELETE_STATE_PROPERTIES = fst.kDeleteStatesProperties
DELETE_ARC_PROPERTIES = fst.kDeleteArcsProperties
STATE_SORT_PROPERTIES = fst.kStateSortProperties
ARC_SORT_PROPERTIES = fst.kArcSortProperties
I_LABEL_INVARIANT_PROPERTIES = fst.kILabelInvariantProperties
O_LABEL_INVARIANT_PROPERTIES = fst.kOLabelInvariantProperties
WEIGHT_INVARIANT_PROPERTIES = fst.kWeightInvariantProperties
ADD_SUPERFINAL_PROPERTIES = fst.kAddSuperFinalProperties
RM_SUPERFINAL_PROPERTIES = fst.kRmSuperFinalProperties
BINARY_PROPERTIES = fst.kBinaryProperties
TRINARY_PROPERTIES = fst.kTrinaryProperties
POS_TRINARY_PROPERTIES = fst.kPosTrinaryProperties
NEG_TRINARY_PROPERTIES = fst.kNegTrinaryProperties
FST_PROPERTIES = fst.kFstProperties


# Definition and construction of an arc.


Arc = collections.namedtuple("Arc", ("ilabel", "olabel", "weight", "nextstate"))


cdef object _init_Arc(const fst.ArcClass &arc):
  cdef Weight weight = Weight.__new__(Weight)
  weight._weight = new fst.WeightClass(arc.weight)
  return Arc(<int64> arc.ilabel, <int64> arc.olabel, weight,
             <int64> arc.nextstate)


# Arc iterators.


cdef class ArcIterator(object):

  """
  ArcIterator(ifst, state)

  This class is used for iterating over the arcs leaving some state of an FST.
  It supports the full C++ API, but most users should just call the `arcs`
  method of an FST object and take advantage of the Pythonic API.
  """

  def __repr__(self):
    return "<ArcIterator at 0x{:x}>".format(id(self))

  def __init__(self, _Fst ifst, int64 state):
    if not ifst._fst.ValidStateId(state):
      raise FstIndexError("State index out of range")
    self._aiter = new fst.ArcIteratorClass(deref(ifst._fst), state)

  def __dealloc__(self):
    del self._aiter

  # This just registers this class as a possible iterator.
  def __iter__(self):
    return self

  # Magic method used to get a Pythonic API out of the C++ API.
  def __next__(self):
    if self.done():
      raise StopIteration
    result = self.value()
    self.next()
    return result

  cpdef bool done(self):
    """
    done(self)

    Indicates whether the iterator is exhausted or not.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.

    Returns:
      True if the iterator is exhausted, False otherwise.
    """
    return self._aiter.Done()

  cpdef uint32 flags(self):
    """
    flags(self)

    Returns the current iterator behavioral flags.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.

    Returns:
      The current iterator behavioral flags as an integer.
    """
    return self._aiter.Flags()

  cpdef void next(self):
    """
    next(self)

    Advances the iterator.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.
    """
    self._aiter.Next()

  cpdef size_t position(self):
    """
    next(self)

    Returns the position of the iterator.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.

    Returns:
      The iterator's position, expressed as an integer.
    """
    return self._aiter.Position()

  cpdef void reset(self):
    """
    reset(self)

    Resets the iterator to the initial position.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.
    """
    self._aiter.Reset()

  cpdef void seek(self, size_t a):
    """
    seek(self, a)

    Advance the iterator to a new position.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.

    Args:
      a: The position to seek to.
    """
    self._aiter.Seek(a)

  cpdef void set_flags(self, uint32 flags, uint32 mask):
    """
    set_flags(self, flags, mask)

    Sets the current iterator behavioral flags.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.

    Args:
      flags: The properties to be set.
      mask: A mask to be applied to the `flags` argument before setting them.
    """
    self._aiter.SetFlags(flags, mask)

  cpdef object value(self):
    """
    value(self)

    Returns the current arc, represented as a tuple.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.
    """
    return _init_Arc(self._aiter.Value())



cdef class MutableArcIterator(object):

  """
  MutableArcIterator(ifst, state)

  This class is used for iterating over the arcs leaving some state of an FST,
  also permitting mutation of the current arc. It supports the full C++ API,
  but most users should just call the `mutable_arcs` method of an FST object
  and take advantage of the Pythonic API.
  """

  def __repr__(self):
    return "<MutableArcIterator at 0x{:x}>".format(id(self))

  def __init__(self, _MutableFst ifst, int64 state):
    if not ifst._fst.ValidStateId(state):
      raise FstIndexError("State index out of range")
    self._maiter = new fst.MutableArcIteratorClass(ifst._mfst, state)
    # We store the underlying FST's weight type so we can forward the proper
    # weight to `set_value`.
    self._weight_type = ifst._mfst.WeightType()

  def __dealloc__(self):
    del self._maiter

  # This just registers this class as a possible iterator.
  def __iter__(self):
    return self

  # Magic method used to get a Pythonic API out of the C++ API.
  def __next__(self):
    if self.done():
      raise StopIteration
    result = self.value()
    self.next()
    return result

  cpdef bool done(self):
    """
    done(self)

    Indicates whether the iterator is exhausted or not.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.

    Returns:
      True if the iterator is exhausted, False otherwise.
    """
    return self._maiter.Done()

  cpdef uint32 flags(self):
    """
    flags(self)

    Returns the current iterator behavioral flags.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.

    Returns:
      The current iterator behavioral flags as an integer.
    """
    return self._maiter.Flags()

  cpdef void next(self):
    """
    next(self)

    Advances the iterator.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.
    """
    self._maiter.Next()

  cpdef size_t position(self):
    """
    next(self)

    Returns the position of the iterator.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.

    Returns:
      The iterator's position, expressed as an integer.
    """
    return self._maiter.Position()

  cpdef void reset(self):
    """
    reset(self)

    Resets the iterator to the initial position.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.
    """
    self._maiter.Reset()

  cpdef void seek(self, size_t a):
    """
    seek(self, a)

    Advance the iterator to a new position.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.

    Args:
      a: The position to seek to.
    """
    self._maiter.Seek(a)

  cpdef void set_flags(self, uint32 flags, uint32 mask):
    """
    set_flags(self, flags, mask)

    Sets the current iterator behavioral flags.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.

    Args:
      flags: The properties to be set.
      mask: A mask to be applied to the `flags` argument before setting them.
    """
    self._maiter.SetFlags(flags, mask)

  cpdef void set_value(self, int64 ilabel, int64 olabel, weight,
                       int64 nextstate) except *:
    """
    set_value(self, ilabel, olabel, weight, nextstate)

    Replace the current arc with a new arc.

    Args:
      ilabel: The integer index of the input label.
      olabel: The integer index of the output label.
      weight: A Weight or weight string indicating the desired final weight; if
        null, it is set to semiring One.
      nextstate: The integer index of the destination state.

    Raises:
      FstBadWeightError: invalid weight.
    """
    self._maiter.SetValue(fst.ArcClass(ilabel, olabel,
        _get_WeightClass_or_One(self._weight_type, weight), nextstate))

  cpdef object value(self):
    """
    value(self)

    Returns the current arc, represented as a tuple.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.
    """
    return _init_Arc(self._maiter.Value())


cdef class StateIterator(object):

  """
  StateIterator(ifst)

  This class is used for iterating over the states in an FST. It supports the
  full C++ API, but most users should just place an FST argument in an
  iteration context and take advantage of the Pythonic API.
  """

  def __init__(self, _Fst ifst):
    self._siter = new fst.StateIteratorClass(deref(ifst._fst))

  def __dealloc__(self):
    del self._siter

  # This just registers this class as a possible iterator.
  def __iter__(self):
    return self

  # Magic method used to get a Pythonic API out of the C++ API.
  def __next__(self):
    if self.done():
      raise StopIteration
    cdef int64 result = self.value()
    self.next()
    return result

  cpdef bool done(self):
    """
    done(self)

    Indicates whether the iterator is exhausted or not.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.

    Returns:
      True if the iterator is exhausted, False otherwise.
    """
    return self._siter.Done()

  cpdef void next(self):
    """
    next(self)

    Advances the iterator.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.
    """
    self._siter.Next()

  cpdef void reset(self):
    """
    reset(self)

    Resets the iterator to the initial position.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.
    """
    self._siter.Reset()

  cpdef int64 value(self):
    """
    value(self)

    Returns the current state index.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.
    """
    return self._siter.Value()


# Constructive and boolean operations.


# This function can handle both state (`arc_sum`) and arc mapping operations.


cdef _Fst _map(_Fst ifst, float delta=fst.kDelta, map_type=b"identity",
               weight=None):
  cdef fst.MapType mt
  cdef map_type_string = tostring(map_type)
  if map_type_string == b"arc_sum":
    mt = fst.ARC_SUM_MAPPER
  elif map_type_string == b"identity":
    mt = fst.IDENTITY_MAPPER
  elif map_type_string == b"input_epsilon":
    mt = fst.INPUT_EPSILON_MAPPER
  elif map_type_string == b"invert":
    mt = fst.INVERT_MAPPER
  elif map_type_string == b"output_epsilon":
    mt = fst.OUTPUT_EPSILON_MAPPER
  elif map_type_string == b"plus":
    mt = fst.PLUS_MAPPER
  elif map_type_string == b"quantize":
    mt = fst.QUANTIZE_MAPPER
  elif map_type_string == b"rmweight":
    mt = fst.RMWEIGHT_MAPPER
  elif map_type_string == b"superfinal":
    mt = fst.SUPERFINAL_MAPPER
  elif map_type_string == b"times":
    mt = fst.TIMES_MAPPER
  elif map_type_string == b"to_log":
    mt = fst.TO_LOG_MAPPER
  elif map_type_string == b"to_log64":
    mt = fst.TO_LOG64_MAPPER
  elif map_type_string == b"to_standard":
    mt = fst.TO_STD_MAPPER
  else:
    raise FstArgError("Unknown map type: {!r}".format(map_type))
  cdef fst.WeightClass wc = (_get_WeightClass_or_One(ifst.weight_type,
      weight) if mt == fst.TIMES_MAPPER else
      _get_WeightClass_or_Zero(ifst.weight_type, weight))
  return _init_XFst(fst.Map(deref(ifst._fst), mt, delta, wc))


cpdef _Fst arcmap(_Fst ifst, float delta=fst.kDelta, map_type=b"identity",
                  weight=None):
  """
  arcmap(ifst, delta=0.0009765625, map_type="identity", weight=None)

  Constructively applies a transform to all arcs and final states.

  This operation transforms each arc and final state in the input FST using
  one of the following:

    * identity: maps to self.
    * input_epsilon: replaces all input labels with epsilon.
    * invert: reciprocates all non-Zero weights.
    * output_epsilon: replaces all output labels with epsilon.
    * plus: adds a constant to all weights.
    * quantize: quantizes weights.
    * rmweight: replaces all non-Zero weights with 1.
    * superfinal: redirects final states to a new superfinal state.
    * times: right-multiplies a constant to all weights.
    * to_log: converts weights to the log semiring.
    * to_log64: converts weights to the log64 semiring.
    * to_standard: converts weights to the tropical ("standard") semiring.

  Args:
    ifst: The input FST.
    delta: Comparison/quantization delta (ignored unless `map_type` is
        `quantize`).
    map_type: A string matching a known mapping operation (see above).
    weight: A Weight or weight string passed to the arc-mapper; this is ignored
      unless `map_type` is `plus` (in which case it defaults to semiring Zero)
      or `times` (in which case it defaults to semiring One).

  Returns:
    An FST with arcs and final states remapped.

  Raises:
    FstArgError: Unknown map type.

  See also: `statemap`.
  """
  return _map(ifst, delta, map_type, weight)


cpdef _MutableFst compose(_Fst ifst1, _Fst ifst2, cf=b"auto",
                          bool connect=True):
  """
  compose(ifst1, ifst2, cf="auto", connect=True)

  Constructively composes two FSTs.

  This operation computes the composition of two FSTs. If A transduces string
  x to y with weight a and B transduces y to z with weight b, then their
  composition transduces string x to z with weight a \otimes b. The output
  labels of the first transducer or the input labels of the second transducer
  must be sorted (or otherwise support appropriate matchers).

  Args:
    ifst1: The first input FST.
    ifst2: The second input FST.
    cf: A string matching a known composition filter; one of: "alt_sequence",
        "auto", "match", "null", "sequence", "trivial".
    connect: Should output be trimmed?

  Returns:
    A composed FST.

  See also: `arcsort`.
  """
  cdef VectorFstClass_ptr tfst = new fst.VectorFstClass(ifst1.arc_type)
  cdef const fst.ComposeOptions *opts = new fst.ComposeOptions(connect,
      _get_compose_filter(tostring(cf)))
  fst.Compose(deref(ifst1._fst), deref(ifst2._fst), tfst, deref(opts))
  del opts
  return _init_MutableFst(tfst)


cpdef _Fst convert(_Fst ifst, fst_type=b""):
  """
  convert(ifst, fst_type="")

  Constructively converts an FST to a new internal representation.

  Args:
    ifst: The input FST.
    fst_type: A string indicating the FST type to convert to, or None if
      no conversion is desired.

  Returns:
    An equivalent Fst converted to the desired FST type.

  Raises:
    FstOpError: Conversion failed.
  """
  cdef FstClass_ptr tfst = new fst.FstClass(deref(ifst._fst))
  tfst = fst.Convert(deref(ifst._fst), tostring(fst_type))
  # Script-land Convert returns the null pointer to signal failure.
  if tfst == NULL:
    raise FstOpError("Conversion to {!r} failed".format(fst_type))
  return _init_XFst(tfst)


cpdef _MutableFst determinize(_Fst ifst, float delta=fst.kDelta,
    det_type=b"functional", int64 nstate=fst.kNoStateId,
    int64 subsequential_label=0, weight=None,
    bool increment_subsequential_label=False):
  """
  determinize(ifst, delta=0.0009765625, det_type="functional", nstate=-1,
              subsequential_label=0, weight=None,
              incremental_subsequential_label=False)

  Constructively determinizes a weighted FST.

  This operations creates an equivalent FST that has the property that no
  state has two transitions with the same input label. For this algorithm,
  epsilon transitions are treated as regular symbols (cf. `rmepsilon`).

  Args:
    ifst: The input FST.
    delta: Comparison/quantization delta.
    det_type: Type of determinization; one of: "functional" (input transducer
        is functional), "nonfunctional" (input transducer is not functional)
        and "disambiguate" (input transducer is not functional but only keep
        the min of ambiguous outputs).
    nstate: State number threshold.
    subsequential_label: Input label of arc corresponding to residual final
        output when producing a subsequential transducer.
    weight: A Weight or weight string indicating the desired weight threshold
        below which paths are pruned; if omitted, no paths are pruned.
    increment_subsequential_label: Increment subsequential when creating
        several arcs for the residual final output at a given state.

  Returns:
    An equivalent deterministic FST.

  Raises:
    FstArgError: Unknown determinization type.

  See also: `disambiguate`, `rmepsilon`.
  """
  cdef VectorFstClass_ptr tfst = new fst.VectorFstClass(ifst.arc_type)
  # The threshold is set to semiring Zero (no pruning) if weight unspecified.
  cdef fst.WeightClass wc = _get_WeightClass_or_Zero(ifst.weight_type, weight)
  det_type = tostring(det_type)
  cdef fst.DeterminizeType dt = fst.DETERMINIZE_FUNCTIONAL
  if det_type == b"functional":
    dt = fst.DETERMINIZE_FUNCTIONAL
  elif det_type == b"nonfunctional":
    dt = fst.DETERMINIZE_NONFUNCTIONAL
  elif det_type == b"disambiguate":
    dt = fst.DETERMINIZE_DISAMBIGUATE
  else:
    raise FstArgError("Unknown determinization type: {!r}".format(type))
  cdef const fst.DeterminizeOptions *opts = new fst.DeterminizeOptions(
      delta, wc, nstate, subsequential_label, dt, increment_subsequential_label)
  fst.Determinize(deref(ifst._fst), tfst, deref(opts))
  del opts
  return _init_MutableFst(tfst)


cpdef _MutableFst difference(_Fst ifst1, _Fst ifst2, cf=b"auto",
                              bool connect=True):
  """
  difference(ifst1, ifst2, cf="auto", connect=True)

  Constructively computes the difference of two FSTs.

  This operation computes the difference between two FSAs. Only strings that are
  in the first automaton but not in second are retained in the result. The first
  argument must be an acceptor; the second argument must be an unweighted,
  epsilon-free, deterministic acceptor. The output labels of the first
  transducer or the input labels of the second transducer must be sorted (or
  otherwise support appropriate matchers).

  Args:
    ifst1: The first input FST.
    ifst2: The second input FST.
    cf: A string matching a known composition filter; one of: "alt_sequence",
        "auto", "match", "null", "sequence", "trivial".
    connect: Should the output FST be trimmed?

  Returns:
    An FST representing the difference of the two input FSTs.
  """
  cdef VectorFstClass_ptr tfst = new fst.VectorFstClass(ifst1.arc_type)
  cdef const fst.ComposeOptions *opts = new fst.ComposeOptions(connect,
      _get_compose_filter(tostring(cf)))
  fst.Difference(deref(ifst1._fst), deref(ifst2._fst), tfst, deref(opts))
  del opts
  return _init_MutableFst(tfst)


cpdef _MutableFst disambiguate(_Fst ifst, float delta=fst.kDelta,
                               int64 nstate=fst.kNoStateId,
                               int64 subsequential_label=0, weight=None):
  """
  disambiguate(ifst, delta=0.0009765625, nstate=-1, subsequential_label=0,
               weight=None):

  Constructively disambiguates a weighted transducer.

  This operation disambiguates a weighted transducer. The result will be an
  equivalent FST that has the property that no two successful paths have the
  same input labeling. For this algorithm, epsilon transitions are treated as
  regular symbols (cf. `rmepsilon`).

  Args:
    ifst: The input FST.
    delta: Comparison/quantization delta.
    nstate: State number threshold.
    subsequential_label: Input label of arc corresponding to residual final
        output when producing a subsequential transducer.
    weight: A Weight or weight string indicating the desired weight threshold
        below which paths are pruned; if omitted, no paths are pruned.

  Returns:
    An equivalent disambiguated FST.

  See also: `determinize`, `rmepsilon`.
  """
  cdef VectorFstClass_ptr tfst = new fst.VectorFstClass(ifst.arc_type)
  # The threshold is set to semiring Zero (no pruning) if no weight is specified.
  cdef fst.WeightClass wc = _get_WeightClass_or_Zero(ifst.weight_type, weight)
  cdef const fst.DisambiguateOptions *opts = new fst.DisambiguateOptions(delta,
      wc, nstate, subsequential_label)
  fst.Disambiguate(deref(ifst._fst), tfst, deref(opts))
  del opts
  return _init_MutableFst(tfst)


cpdef _MutableFst epsnormalize(_Fst ifst, bool eps_norm_output=False):
  """
  epsnormalize(ifst, eps_norm_output=False)

  Constructively epsilon-normalizes an FST.

  This operation creates an equivalent FST that is epsilon-normalized. An
  acceptor is epsilon-normalized if it it is epsilon-removed (cf. `rmepsilon`).
  A transducer is input epsilon-normalized if, in addition, along any path, all
  arcs with epsilon input labels follow all arcs with non-epsilon input labels.
  Output epsilon-normalized is defined similarly. The input FST must be
  functional.

  Args:
    ifst: The input FST.
    eps_norm_output: Should the FST be output epsilon-normalized?

  Returns:
    An equivalent epsilon-normalized FST.

  See also: `rmepsilon`.
  """
  cdef VectorFstClass_ptr tfst = new fst.VectorFstClass(ifst.arc_type)
  fst.EpsNormalize(deref(ifst._fst), tfst, fst.EPS_NORM_OUTPUT if
                                           eps_norm_output else
                                           fst.EPS_NORM_INPUT)
  return _init_MutableFst(tfst)


cpdef bool equal(_Fst ifst1, _Fst ifst2, float delta=fst.kDelta):
  """
  equal(ifst1, ifst2, delta=0.0009765625)

  Are two FSTs equal?

  This function tests whether two FSTs have the same states with the same
  numbering and the same transitions with the same labels and weights in the
  same order.

  Args:
    ifst1: The first input FST.
    ifst2: The second input FST.
    delta: Comparison/quantization delta.

  Returns:
    True if the two transducers satisfy the above condition, else False.

  See also: `equivalent`, `isomorphic`, `randequivalent`.
  """
  return fst.Equal(deref(ifst1._fst), deref(ifst2._fst), delta)


cpdef bool equivalent(_Fst ifst1, _Fst ifst2, float delta=fst.kDelta) except *:
  """
  equivalent(ifst1, ifst2, delta=0.0009765625)

  Are the two acceptors equivalent?

  This operation tests whether two epsilon-free deterministic weighted
  acceptors are equivalent, that is if they accept the same strings with the
  same weights.

  Args:
    ifst1: The first input FST.
    ifst2: The second input FST.
    delta: Comparison/quantization delta.

  Returns:
    True if the two transducers satisfy the above condition, else False.

  Raises:
    FstOpError: Equivalence test encountered error.

  See also: `equal`, `isomorphic`, `randequivalent`.
  """
  cdef bool error
  cdef bool result
  result = fst.Equivalent(deref(ifst1._fst), deref(ifst2._fst), delta, &error)
  if error:
    raise FstOpError("Equivalence test encountered error")
  return result


cpdef _MutableFst intersect(_Fst ifst1, _Fst ifst2, cf=b"auto",
                            bool connect=True):
  """
  intersect(ifst1, ifst2, cf="auto", connect=True)

  Constructively intersects two FSTs.

  This operation computes the intersection (Hadamard product) of two FSTs.
  Only strings that are in both automata are retained in the result. The two
  arguments must be acceptors. One of the arguments must be label-sorted (or
  otherwise support appropriate matchers).

  Args:
    ifst1: The first input FST.
    ifst2: The second input FST.
    cf: A string matching a known composition filter; one of: "alt_sequence",
        "auto", "match", "null", "sequence", "trivial".
    connect: Should output be trimmed?

  Returns:
    An equivalent epsilon-normalized FST.
  """
  cdef VectorFstClass_ptr tfst = new fst.VectorFstClass(ifst1.arc_type)
  cdef const fst.ComposeOptions *opts = new fst.ComposeOptions(connect,
      _get_compose_filter(tostring(cf)))
  fst.Intersect(deref(ifst1._fst), deref(ifst2._fst), tfst, deref(opts))
  del opts
  return _init_MutableFst(tfst)


cpdef bool isomorphic(_Fst ifst1, _Fst ifst2, float delta=fst.kDelta) except *:
  """
  isomorphic(ifst1, ifst2, delta=0.0009765625)

  Are the two acceptors isomorphic?

  This operation determines if two transducers with a certain required
  determinism have the same states, irrespective of numbering, and the same
  transitions with the same labels and weights, irrespective of ordering. In
  other words, FSTs A, B are isomorphic if and only if the states of A can be
  renumbered and the transitions leaving each state reordered so the two are
  equal (according to the definition given in `equal`).

  Args:
    ifst1: The first input FST.
    ifst2: The second input FST.
    delta: Comparison/quantization delta.

  Returns:
    True if the two transducers satisfy the above condition, else False.

  Raises:
    FstOpError: Isomorphism test encountered error.

  See also: `equal`, `equivalent`, `randequivalent`.
  """
  cdef bool error
  cdef bool result
  result = fst.Isomorphic(deref(ifst1._fst), deref(ifst2._fst), delta, &error)
  if error:
    raise FstOpError("Isomorphism test encountered error")
  return result


cpdef _MutableFst prune(_Fst ifst, float delta=fst.kDelta,
                        int64 nstate=fst.kNoStateId, weight=None):
  """
  prune(ifst, delta=0.0009765625, nstate=-1, weight=None)

  Constructively removes paths with weights below a certain threshold.

  This operation deletes states and arcs in the input FST that do not belong
  to a successful path whose weight is no more (w.r.t the natural semiring
  order) than the threshold t \otimes-times the weight of the shortest path in
  the input FST. Weights must be commutative and have the path property.

  Args:
    ifst: The input FST.
    delta: Comparison/quantization delta.
    nstate: State number threshold.
    weight: A Weight or weight string indicating the desired weight threshold
        below which paths are pruned; if omitted, no paths are pruned.

  Returns:
    A pruned FST.

  See also: The destructive variant.
  """
  cdef VectorFstClass_ptr tfst = new fst.VectorFstClass(ifst.arc_type)
  cdef fst.WeightClass wc = _get_WeightClass_or_Zero(ifst.weight_type, weight)
  fst.Prune(deref(ifst._fst), tfst, wc, nstate, delta)
  return _init_MutableFst(tfst)


cpdef _MutableFst push(_Fst ifst, float delta=fst.kDelta,
                       bool push_weights=False, bool push_labels=False,
                       bool remove_common_affix=False,
                       bool remove_total_weight=False, bool to_final=False):
  """
  push(ifst, delta=0.0009765625, push_weights=False, push_labels=False,
       remove_common_affix=False, remove_total_weight=False, to_final=False)

  Constructively pushes weights/labels towards initial or final states.

  This operation produces an equivalent transducer by pushing the weights
  and/or the labels towards the initial state or toward the final states.

  When pushing weights towards the initial state, the sum of the weight of the
  outgoing transitions and final weight at any non-initial state is equal to 1
  in the resulting machine. When pushing weights towards the final states, the
  sum of the weight of the incoming transitions at any state is equal to 1.
  Weights need to be left distributive when pushing towards the initial state
  and right distributive when pushing towards the final states.

  Pushing labels towards the initial state consists in minimizing at every
  state the length of the longest common prefix of the output labels of the
  outgoing paths. Pushing labels towards the final states consists in
  minimizing at every state the length of the longest common suffix of the
  output labels of the incoming paths.

  Args:
    ifst: The input FST.
    delta: Comparison/quantization delta.
    push_weights: Should weights be pushed?
    push_labels: Should labels be pushed?
    remove_common_affix: If pushing labels, should common prefix/suffix be
        removed?
    remove_total_weight: If pushing weights, should total weight be removed?
    to_final: Push towards final states?

  Returns:
    An equivalent pushed FST.

  See also: The destructive variant.
  """
  # This is copied, almost verbatim, from nlp/fst/bin/fstpush.cc.
  cdef VectorFstClass_ptr tfst = new fst.VectorFstClass(ifst.arc_type)
  cdef uint32 flags = 0
  if push_weights:
    flags |= fst.kPushWeights
  if push_labels:
    flags |= fst.kPushLabels
  if remove_common_affix:
    flags |= fst.kPushRemoveCommonAffix
  if remove_total_weight:
    flags |= fst.kPushRemoveTotalWeight
  cdef fst.ReweightType rt = (fst.REWEIGHT_TO_FINAL if to_final else
                              fst.REWEIGHT_TO_INITIAL)
  fst.Push(deref(ifst._fst), tfst, flags, rt, delta)
  return _init_MutableFst(tfst)


cpdef bool randequivalent(_Fst ifst1, _Fst ifst2, float delta=fst.kDelta,
                          int32 max_length=INT_MAX, int32 npath=1,
                          time_t seed=time(NULL),
                          select=b"uniform") except *:
  """
  randequivalent(ifst1, ifst2, delta=0.0009765625, max_length=INT_MAX, npath=1,
                 seed=time(0), select="uniform")

  Are two acceptors stochastically equivalent?

  This operation tests whether two FSTs are equivalent by randomly generating
  paths alternatively in each of the two FSTs. For each randomly generated path,
  the algorithm computes for each of the two FSTs the sum of the weights of all
  the successful paths sharing the same input and output labels as the randomly
  generated path and checks that these two values are within `delta`.

  Args:
    ifst1: The first input FST.
    ifst2: The second input FST.
    delta: Comparison/quantization delta.
    max_length: The maximum length of each random path.
    npath: The number of random paths to generate.
    seed: An optional seed value for random path generation.
    select: A string matching a known random arc selection type; one of:
        "uniform", "log_prob", "fast_log_prob".

  Returns:
    True if the two transducers satisfy the above condition, else False.

  Raise:
    FstOpeError: Random equivalence test encountered error.

  See also: `equal`, `equivalent`, `isomorphic`, `randgen`.
  """
  cdef fst.RandArcSelection ras = _get_rand_arc_selection(tostring(select))
  cdef const fst.RandGenOptions[fst.RandArcSelection] *opts = \
      new fst.RandGenOptions[fst.RandArcSelection](ras, max_length)
  cdef bool error
  cdef bool result
  result = fst.RandEquivalent(deref(ifst1._fst), deref(ifst2._fst), seed, npath,
      delta, deref(opts), &error)
  del opts
  if error:
    raise FstOpError("Random equivalence test encountered error")
  return result


cpdef _MutableFst randgen(_Fst ifst, int32 max_length=INT_MAX, int32 npath=1,
                          bool remove_total_weight=False,
                          time_t seed=time(NULL), select=b"uniform",
                          bool weighted=False):
  """
  randgen(ifst, max_length=INT_MAX, npath=1, remove_total_weight=False,
          seed=time(0), select="uniform", weighted=False)

  Randomly generate successful paths in an FST.

  This operation randomly generates a set of successful paths in the input FST.
  This relies on a mechanism for selecting arcs, specified using the `select`
  argument. The default selector, "uniform", randomly selects a transition
  using a uniform distribution. The "log_prob" selector randomly selects a
  transition w.r.t. the weights treated as negative log probabilities after
  normalizing for the total weight leaving the state. In all cases, finality is
  treated as a transition to a super-final state.

  Args:
    ifst: The input FST.
    max_length: The maximum length of each random path.
    npath: The number of random paths to generate.
    remove_total_weight: Should the total weight be removed (ignored when
        `weighted` is False)?
    seed: An optional seed value for random path generation.
    select: A string matching a known random arc selection type; one of:
        "uniform", "log_prob", "fast_log_prob".
    weighted: Should the output be weighted by path count?

  Returns:
    An Fst containing one or more random paths.

  See also: `randequivalent`.
  """
  cdef fst.RandArcSelection ras = _get_rand_arc_selection(tostring(select))
  cdef const fst.RandGenOptions[fst.RandArcSelection] *opts = \
      new fst.RandGenOptions[fst.RandArcSelection](ras, max_length)
  cdef VectorFstClass_ptr tfst = new fst.VectorFstClass(ifst.arc_type)
  fst.RandGen(deref(ifst._fst), tfst, seed, deref(opts))
  del opts
  return _init_MutableFst(tfst)


cpdef _MutableFst replace(pairs, call_arc_labeling=b"input",
                          return_arc_labeling=b"neither",
                          bool epsilon_on_replace=False,
                          int64 return_label=0):
  """
  replace(pairs, call_arc_labeling="input", return_arc_labeling="neither",
          epsilon_on_replace=False, return_label=0)

  Recursively replaces arcs in the FST with other FST(s).

  This operation performs the dynamic replacement of arcs in one FST with
  another FST, allowing the definition of FSTs analogous to RTNs. It takes as
  input a set of pairs of a set of pairs formed by a non-terminal label and
  its corresponding FST, and a label identifying the root FST in that set.
  The resulting FST is obtained by taking the root FST and recursively replacing
  each arc having a nonterminal as output label by its corresponding FST. More
  precisely, an arc from state s to state d with (nonterminal) output label n in
  this FST is replaced by redirecting this "call" arc to the initial state of a
  copy F of the FST for n, and adding "return" arcs from each final state of F
  to d. Optional arguments control how the call and return arcs are labeled; by
  default, the only non-epsilon label is placed on the call arc.

  Args:

    pairs: An iterable of (nonterminal label, FST) pairs, where the former is an
        unsigned integer and the latter is an Fst instance.
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
    An FST resulting from expanding the input RTN.
  """
  cdef vector[fst.LabelFstClassPair] _pairs
  cdef int64 root_label
  cdef int64 label
  cdef _Fst ifst
  it = iter(pairs)
  (root_label, ifst) = next(it)
  _pairs.push_back(fst.LabelFstClassPair(root_label, ifst._fst))
  cdef VectorFstClass_ptr tfst = new fst.VectorFstClass(ifst.arc_type)
  for (label, ifst) in it:
    _pairs.push_back(fst.LabelFstClassPair(label, ifst._fst))
  cdef string call_arc_labeling_ = tostring(call_arc_labeling)
  cdef string return_arc_labeling_ = tostring(return_arc_labeling)
  cdef fst.ReplaceLabelType cal = _get_replace_label_type(call_arc_labeling_,
                                                          epsilon_on_replace)
  cdef fst.ReplaceLabelType ral = _get_replace_label_type(return_arc_labeling_,
                                                          epsilon_on_replace)
  cdef const fst.ReplaceOptions *opts = new fst.ReplaceOptions(root_label, cal,
      ral, return_label)
  fst.Replace(_pairs, tfst, deref(opts))
  del opts
  return _init_MutableFst(tfst)


cpdef _MutableFst reverse(_Fst ifst, bool require_superinitial=True):
  """
  reverse(ifst, require_superinitial=True)

  Constructively reverses an FST's transduction.

  This operation reverses an FST. If A transduces string x to y with weight a,
  then the reverse of A transduces the reverse of x to the reverse of y with
  weight a.Reverse(). (Typically, a = a.Reverse() and Arc = RevArc, e.g.,
  TropicalWeight and LogWeight.) In general, e.g., when the weights only form a
  left or right semiring, the output arc type must match the input arc type.

  Args:
    ifst: The input FST.
    require_superinitial: Should a superinitial state be created?

  Returns:
    A reversed FST.
  """
  cdef VectorFstClass_ptr tfst = new fst.VectorFstClass(ifst.arc_type)
  fst.Reverse(deref(ifst._fst), tfst, require_superinitial)
  return _init_MutableFst(tfst)


cpdef _MutableFst rmepsilon(_Fst ifst, bool connect=True,
                            float delta=fst.kDelta,
                            int64 nstate=fst.kNoStateId, qt=b"auto",
                            bool reverse=False, weight=None):
  """
  rmepsilon(ifst, connect=True, delta=0.0009765625, nstate=-1, qt="auto",
            reverse=False, weight=None)

  Constructively removes epsilon transitions from an FST.

  This operation removes epsilon transitions (those where both input and output
  labels are epsilon) from an FST.

  Args:
    ifst: The input FST.
    connect: Should output be trimmed?
    delta: Comparison/quantization delta.
    nstate: State number threshold.
    qt: A string matching a known queue type; one of: "auto", "fifo", "lifo",
        "shortest", "state", "top".
    reverse: Should epsilon transitions be removed in reverse order?
    weight: A string indicating the desired weight threshold; paths with
        weights below this threshold will be pruned.

  Returns:
    An equivalent FST with no epsilon transitions.
  """
  cdef fst.WeightClass wc = _get_WeightClass_or_Zero(ifst.weight_type, weight)
  cdef const fst.RmEpsilonOptions *opts = new fst.RmEpsilonOptions(
      _get_queue_type(tostring(qt)), delta, connect, wc, nstate)
  cdef VectorFstClass_ptr tfst = new fst.VectorFstClass(ifst.arc_type)
  fst.RmEpsilon(deref(ifst._fst), tfst, reverse, deref(opts))
  del opts
  return _init_MutableFst(tfst)


# Pure C++ helper for shortestdistance.


cdef vector[fst.WeightClass] *_shortestdistance(_Fst ifst,
    float delta=fst.kDelta, int64 nstate=fst.kNoStateId, qt=b"auto",
    bool reverse=False):
  cdef vector[fst.WeightClass] *distance = new vector[fst.WeightClass]()
  # For scoping reasons, these have to be declared here even though they may
  # not be used in all cases.
  cdef fst.ShortestDistanceOptions *opts
  if reverse:
    # Only the simpler signature supports shortest distance to final states;
    # `nstate` and `qt` arguments are ignored.
    fst.ShortestDistance(deref(ifst._fst), distance, True, delta)
  else:
    opts = new fst.ShortestDistanceOptions(
        _get_queue_type(tostring(qt)), fst.ANY_ARC_FILTER, nstate,
        delta)
    fst.ShortestDistance(deref(ifst._fst), distance, deref(opts))
    del opts
  return distance


def shortestdistance(_Fst ifst, float delta=fst.kDelta,
                     int64 nstate=fst.kNoStateId, qt=b"auto",
                     bool reverse=False):
  """
  shortestdistance(ifst, delta=0.0009765625, nstate=-1, qt="auto",
                   reverse=False)

  Compute the shortest distance from the initial or final state.

  This operation computes the shortest distance from the initial state (when
  `reverse` is False) or from every state to the final state (when `reverse` is
  True). The shortest distance from p to q is the \otimes-sum of the weights of
  all the paths between p and q. The weights must be right (if `reverse` is
  False) or left (if `reverse` is True) distributive, and k-closed (i.e., 1
  \otimes x \otimes x^2 \otimes ... \otimes x^{k + 1} = 1 \otimes x \otimes x^2
  \otimes ... \otimes x^k; e.g., TropicalWeight).

  Args:
    ifst: The input FST.
    delta: Comparison/quantization delta.
    nstate: State number threshold (this is ignored if `reverse` is True).
    qt: A string matching a known queue type; one of: "auto", "fifo", "lifo",
        "shortest", "state", "top" (this is ignored if `reverse` is True).
    reverse: Should the reverse distance (from each state to the final state)
        be computed?

  Returns:
    A list of Weight objects representing the shortest distance for each state.
  """
  cdef vector[fst.WeightClass] *distance = _shortestdistance(ifst, delta,
                                                             nstate, qt,
                                                             reverse)
  # Packs the distances, as strings, into a Python list.
  cdef string weight_type = ifst.weight_type
  result = []
  # This is just the Cython version of the normal vector iteration idiom.
  cdef vector[fst.WeightClass].iterator it = distance.begin()
  while it != distance.end():
    result.append(Weight(weight_type, deref(it).ToString()))
    inc(it)
  del distance
  return result


cpdef _MutableFst shortestpath(_Fst ifst, float delta=fst.kDelta,
                               int32 nshortest=1, int64 nstate=fst.kNoStateId,
                               qt=b"auto", bool unique=False, weight=None):
  """
  shortestpath(ifst, delta=0.0009765625, nshortest=1, nstate=-1, qt="auto",
               unique=False, weight=None)

  Construct an FST containing the shortest path(s) in the input FST.

  This operation produces an FST containing the n-shortest paths in the input
  FST. The n-shortest paths are the n-lowest weight paths w.r.t. the natural
  semiring order. The single path that can be read from the ith of at most n
  transitions leaving the initial state of the resulting FST is the ith
  shortest path. The weights need to be right distributive and have the path
  property. They also need to be left distributive as well for n-shortest with
  n > 1 (e.g., TropicalWeight).

  Args:
    ifst: The input FST.
    delta: Comparison/quantization delta.
    nshortest: The number of paths to return.
    nstate: State number threshold.
    qt: A string matching a known queue type; one of: "auto", "fifo", "lifo",
        "shortest", "state", "top".
    unique: Should the resulting FST only contain distinct paths? (Requires
        the input FST to be an acceptor; epsilons are treated as if they are
        regular symbols.)
    weight: A Weight or weight string indicating the desired weight threshold
        below which paths are pruned; if omitted, no paths are pruned.

  Returns:
    An FST containing the n-shortest paths.
  """
  cdef VectorFstClass_ptr tfst = new fst.VectorFstClass(ifst.arc_type)
  cdef vector[fst.WeightClass] *distance = new vector[fst.WeightClass]()
  # Threshold is set to semiring Zero (no pruning) if no weight is specified.
  cdef fst.WeightClass wc = _get_WeightClass_or_Zero(ifst.weight_type, weight)
  cdef const fst.ShortestPathOptions *opts = new fst.ShortestPathOptions(
      _get_queue_type(tostring(qt)), nshortest, unique, False, delta,
      False, wc, nstate)
  fst.ShortestPath(deref(ifst._fst), tfst, distance, deref(opts))
  del distance, opts
  return _init_MutableFst(tfst)



cpdef _Fst statemap(_Fst ifst, map_type=b"arc_sum"):
  """
  state_map(ifst, map_type="arc_sum")

  Constructively applies a transform to all states.

  This operation transforms each state according to the requested map type.
  Note that currently, only one state-mapping operation is supported.

  Args:
    ifst: The input FST.
    map_type: A string matching a known mapping operation; one of: "arc_sum"
       (sum weights of identically-labeled multi-arcs).

  Returns:
    An FST with states remapped.

  Raises:
    FstArgError: Unknown map type.

  See also: `arcmap`.
  """
  return _map(ifst, fst.kDelta, map_type, weight=None)


cpdef _MutableFst synchronize(_Fst ifst):
  """
  synchronize(ifst)

  Constructively synchronizes an FST.

  This operation synchronizes a transducer. The result will be an equivalent
  FST that has the property that during the traversal of a path, the delay is
  either zero or strictly increasing, where the delay is the difference between
  the number of non-epsilon output labels and input labels along the path. For
  the algorithm to terminate, the input transducer must have bounded delay,
  i.e., the delay of every cycle must be zero.

  Args:
    ifst: The input FST.

  Returns:
    An equivalent synchronized FST.
  """
  cdef VectorFstClass_ptr tfst = new fst.VectorFstClass(ifst.arc_type)
  fst.Synchronize(deref(ifst._fst), tfst)
  return _init_MutableFst(tfst)



# FST compiler class.


cdef class Compiler(object):

  """
  Compiler(fst_type="vector", arc_type="standard", isymbols=None,
           osymbols=None, ssymbols=None, acceptor=False, keep_isymbols=False,
           keep_osymbols=False, keep_state_numbering=False,
           allow_negative_labels=False)

  Class used to compile FSTs from strings.

  This class is used to compile FSTs specified using the AT&T FSM library
  format described here:

  http://web.eecs.umich.edu/~radev/NLP-fall2015/resources/fsm_archive/fsm.5.html

  This is the same format used by the `fstcompile` executable.

  Compiler options (symbol tables, etc.) are set at construction time.

      compiler = fst.Compiler(isymbols=ascii_syms, osymbols=ascii_syms)

  Once constructed, Compiler instances behave like a file handle opened for
  writing:

      # /ba+/
      print >> compiler, "0 1 50 50"
      print >> compiler, "1 2 49 49"
      print >> compiler, "2 2 49 49"
      print >> compiler, "2"

  The `compile` method returns an actual FST instance:

      sheep_machine = compiler.compile()

  Compilation flushes the internal buffer, so the compiler instance can be
  reused to compile new machines with the same symbol tables (etc.)

  Args:
    fst_type: A string indicating the container type for the compiled FST.
    arc_type: A string indicating the arc type for the compiled FST.
    isymbols: An optional SymbolTable used to label input symbols.
    osymbols: An optional SymbolTable used to label output symbols.
    ssymbols: An optional SymbolTable used to label states.
    acceptor: Should the FST be rendered in acceptor format if possible?
    keep_isymbols: Should the input symbol table be stored in the FST?
    keep_osymbols: Should the output symbol table be stored in the FST?
    keep_state_numbering: Should the state numbering be preserved?
    allow_negative_labels: Should negative labels be allowed? (Not
        recommended; may cause conflicts).
  """

  def __cinit__(self, string fst_type=b"vector", string arc_type=b"standard",
                SymbolTable isymbols=None, SymbolTable osymbols=None,
                SymbolTable ssymbols=None, bool acceptor=False,
                bool keep_isymbols=False, bool keep_osymbols=False,
                bool keep_state_numbering=False, allow_negative_labels=False):
    self._sstrm = new stringstream()
    self._fst_type = tostring(fst_type)
    self._arc_type = tostring(arc_type)
    self._isymbols = NULL
    if isymbols is not None:
      self._isymbols = isymbols._table
    self._osymbols = NULL
    if osymbols is not None:
      self._osymbols = osymbols._table
    self._ssymbols = NULL
    if ssymbols is not None:
      self._ssymbols = ssymbols._table
    self._acceptor = acceptor
    self._keep_isymbols = keep_isymbols
    self._keep_osymbols = keep_osymbols
    self._keep_state_numbering = keep_state_numbering
    self._allow_negative_labels = allow_negative_labels

  def __dealloc__(self):
    del self._sstrm

  cpdef _Fst compile(self):
    """
    compile()

    Compiles the FST in the compiler string buffer.

    This method compiles the FST and returns the resulting machine.

    Returns:
      The FST described by the compiler string buffer.

    Raises:
      FstOpError: Compilation failed.
    """
    cdef fst.FstClass *tfst = fst.CompileFstInternal(deref(self._sstrm),
        __file__, self._fst_type, self._arc_type, self._isymbols,
        self._osymbols, self._ssymbols, self._acceptor, self._keep_isymbols,
        self._keep_osymbols, self._keep_state_numbering,
        self._allow_negative_labels)
    # Resets stream.
    del self._sstrm
    self._sstrm = new stringstream()
    if tfst == NULL:
      raise FstOpError("Compilation failed")
    return _init_XFst(tfst)

  cpdef void write(self, expression):
    """
    write(expression)

    Writes a string into the compiler string buffer.

    This method adds a line to the compiler string buffer. It is normally
    invoked using the right shift operator, like so:

        compiler = fst.Compiler()
        print >> compiler, "0 0 49 49"
        print >> compiler, "0"

    Args:
      expression: A string expression to add to compiler string buffer.
    """
    deref(self._sstrm) << tostring(expression)


cdef class FarReader(object):

  """
  (No constructor.)

  FAR ("Fst ARchive") reader object.

  This class is used to read a FAR from disk. FARs contain one or more FSTs (of
  the same arc type) indexed by a unique string key. To construct a FarReader
  object, use the `open` class method. FSTs can be accessed from a FAR using the
  familiar C++ API methods, but a user who wishes to access all FSTs in random
  order should simply place a FarReader in an iteration context and take
  advantage of the Pythonic API that provides.

  Attributes:
    arc_type: A string indicating the arc type.
    far_type: A string indicating the FAR type.
  """

  def __init__(self):
    raise FstDeletedConstructorError(
        "Cannot construct {}".format(self.__class__.__name__))

  def __repr__(self):
    return "<{} FarReader at 0x{:x}>".format(self.far_type, id(self))

  @classmethod
  def open(cls, *filenames):
    """
    FarReader.open(*filenames)

    Creates a FarReader object.

    This class method creates a FarReader given the string location of one or
    more FAR files on disk.

    Args:
      *filenames: The string location of one or more input FAR files.

    Returns:
      A new FarReader instance.

    Raises:
      FstIOError: Read failed.
    """
    filenames = [tostring(filename) for filename in filenames]
    cdef fst.FarReaderClass *tfar = fst.FarReaderClass.Open(filenames)
    if tfar == NULL:
      raise FstIOError("Read failed: {!r}".format(filename))
    cdef FarReader result = FarReader.__new__(FarReader)
    result._reader = tfar
    return result

  def __dealloc__(self):
    del self._reader

  # This just registers this class as a possible iterator.
  def __iter__(self):
    return self

  # Magic method used to get a Pythonic API out of the C++ API.
  def __next__(self):
    if self.done():
      raise StopIteration
    cdef string k = self.get_key()
    cdef _MutableFst f = self.get_fst()
    self.next()
    return (k, f)

  cdef string _arc_type(self):
    return self._reader.ArcType()

  @property
  def arc_type(self):
    return self._arc_type()

  cpdef bool done(self):
    """
    done(self)

    Indicates whether the iterator is exhausted or not.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.

    Returns:
      True if the iterator is exhausted, False otherwise.
    """
    return self._reader.Done()

  cpdef bool error(self):
    """
    error(self)

    Indicates whether the FarReader has encountered an error.

    Returns:
      True if the FarReader is in an errorful state, False otherwise.
    """
    return self._reader.Error()

  cdef string _far_type(self):
    return fst.FarTypeToString(self._reader.Type())

  @property
  def far_type(self):
    return self._far_type()

  cpdef bool find(self, key):
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
    """
    return self._reader.Find(tostring(key))

  cpdef _Fst get_fst(self):
    """
    get_fst(self)

    Returns the FST at the current position.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.

    Returns:
      A copy of the FST at the current position.
    """
    # The reference returned by the underlying library is invalided if the
    # archive's current position moves. So we make a copy.
    cdef fst.FstClass *tfst = new fst.FstClass(self._reader.GetFstClass())
    return _init_XFst(tfst)

  cpdef string get_key(self):
    """
    get_key(self)

    Returns the string key at the current position.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.

    Returns:
      The string key at the current position.
    """
    return self._reader.GetKey()

  cpdef void next(self):
    """
    next(self)

    Advances the iterator.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.
    """
    self._reader.Next()

  cpdef void reset(self):
    """
    reset(self)

    Resets the iterator to the initial position.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.
    """
    self._reader.Reset()

  # Dictionary-like access by combining `find` and `get_fst`.
  def __getitem__(self, key):
    if not self.find(key):
      raise KeyError(key)
    return self.get_fst()


cdef class FarWriter(object):

  """
  (No constructor.)

  FAR ("Fst ARchive") writer object.

  This class is used to write FSTs (of the same arc type) to a FAR on disk. To
  construct a FarWriter, use the `create` class method.

  Note that the data is not guaranteed to flush to disk until the FarWriter
  is garbage-collected. If a FarWriter has been assigned to only one variable,
  then calling `del` on that variable should decrement the object's reference
  count from 1 to 0, triggering a flush to disk on the next GC cycle.

  Attributes:
    arc_type: A string indicating the arc type.
    far_type: A string indicating the FAR type.
  """

  def __init__(self):
    raise FstDeletedConstructorError(
        "Cannot construct {}".format(self.__class__.__name__))

  def __repr__(self):
    return "<{} FarWriter at 0x{:x}>".format(self.far_type, id(self))

  @classmethod
  def create(cls, filename, arc_type=b"standard", far_type=b"default"):
    """
    FarWriter.

    Creates a FarWriter object.

    This class method creates a FarWriter given the desired output location,
    arc type, and FAR type.

    Args:
      filename: The string location for the output FAR files.
      arc_type: A string indicating the arc type.
      far_type: A string indicating the FAR type; one of: "fst", "stlist",
          "sttable", "sstable", "default".

    Returns:
      A new FarWriter instance.

    Raises:
      FstIOError: Read failed.
    """
    cdef fst.FarType ft = fst.FarTypeFromString(tostring(far_type))
    cdef fst.FarWriterClass *tfar = fst.FarWriterClass.Create(
        tostring(filename), tostring(arc_type), ft)
    if tfar == NULL:
      raise FstIOError("Open failed: {!r}".format(filename))
    cdef FarWriter result = FarWriter.__new__(FarWriter)
    result._is_open = True
    result._writer = tfar
    return result

  # NB: Invoking this method is DANGEROUS: calling any other method on the
  # instance after this is invoked may result in a null dereference.
  cdef void _close(self):
    del self._writer
    self._is_open = False

  def __dealloc__(self):
    if self._is_open:
      del self._writer

  cpdef void add(self, key, _Fst fst) except *:
    """
    add(self, key, fst)

    Adds an FST to the FAR.

    This method adds an FST to the FAR which can be retrieved with the
    specified string key.

    This method is provided for compatibility with the C++ API only; most users
    should use the Pythonic API.

    Args:
      key: The string used to key the input FST.
      fst: The FST to write to the FAR.

    Raises:
      FstArgError: Key out of order.
      FstOpError: Incompatible or invalid arc type.
    """
    # Failure here results from passing an FST with a different arc type than
    # used by the FAR was initialized to use.
    if not self._writer.Add(tostring(key), deref(fst._fst)):
      raise FstOpError("Incompatible or invalid arc type")
    # An error here usually indicates a key out of order.
    if self._writer.Error():
      raise FstArgError("Key out of order")

  cdef string _arc_type(self):
    return self._writer.ArcType()

  @property
  def arc_type(self):
    return self._arc_type()

  cpdef bool error(self):
    """
    error(self)

    Indicates whether the FarWriter has encountered an error.

    Returns:
      True if the FarWriter is in an errorful state, False otherwise.
    """
    return self._writer.Error()

  cdef string _far_type(self):
    return fst.FarTypeToString(self._writer.Type())

  @property
  def far_type(self):
    return self._far_type()

  # Dictionary-like assignment.
  def __setitem__(self, key, _Fst fst):
    self.add(key, fst)



# Masks fst_error_fatal flags while this module is running, returning to the
# previous state upon module exit.


_fst_error_fatal_old = fst.FLAGS_fst_error_fatal
fst.FLAGS_fst_error_fatal = False


@atexit.register
def _reset_fst_error_fatal():
  fst.FLAGS_fst_error_fatal = _fst_error_fatal_old

