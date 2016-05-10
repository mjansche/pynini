# Encoding: UTF-8
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


"""Unit tests for the Pynini grammar compilation module."""

import os
import string
import tempfile
import unittest

from pynini import *

# Python 2/3 compatibility shims.
from six import iteritems
from six.moves import xrange
from six.moves import zip


class PyniniTest(unittest.TestCase):

  def testStringCompilation(self):
    """Tests string compilation operations."""
    # Compiled from bytestrings.
    cheese = "Red Leicester"
    reply = "I'm afraid we're fresh out of Red Leicester sir"
    cheese_compiled = acceptor(cheese)
    self.assertEqual(cheese_compiled, cheese)
    exchange = transducer(cheese, reply)
    exchange.project()
    exchange.rmepsilon()
    self.assertEqual(exchange, cheese)
    self.assertEqual(exchange, cheese_compiled)
    # Compiled from strings with Weight argument.
    bar_one = Weight.One("tropical")
    cheese_compiled = acceptor(cheese, weight=bar_one)
    self.assertEqual(cheese_compiled, cheese)
    exchange = transducer(cheese, reply, weight=bar_one)
    exchange.project()
    exchange.rmepsilon()
    self.assertEqual(exchange, cheese)
    self.assertEqual(exchange, cheese_compiled)
    # Compiled from strings with Python argument cast to Weight.
    bar_one = 0
    cheese_compiled = acceptor(cheese, weight=bar_one)
    self.assertEqual(cheese_compiled, cheese)
    exchange = transducer(cheese, reply, weight=bar_one)
    exchange.project()
    exchange.rmepsilon()
    self.assertEqual(exchange, cheese)
    self.assertEqual(exchange, cheese_compiled)
    # Transducers compiled using FST arguments.
    a_s = transducer(cheese_compiled, reply)
    reply_compiled = acceptor(reply)
    s_a = transducer(cheese, reply_compiled)
    a_a = transducer(cheese_compiled, reply_compiled)
    self.assertEqual(a_s, s_a)
    self.assertEqual(a_s, a_a)
    self.assertEqual(s_a, a_a)
    # Compiled using "tokenization".
    cheese_bracketed = "".join("[{}]".format(t) for t in cheese.split())
    reply_bracketed = "".join("[{}]".format(t) for t in reply.split())
    cheese_compiled = acceptor(cheese_bracketed)
    exchange = transducer(cheese_bracketed, reply_bracketed)
    self.assertEqual(cheese_compiled.num_states, 3)
    self.assertEqual(exchange.num_states, 10)
    cheese_token = cheese.split()[1]  # "Leicester"
    i = cheese_compiled.input_symbols.find(cheese_token)
    self.assertTrue(i > 255)
    self.assertEqual(i, cheese_compiled.output_symbols.find(cheese_token))
    self.assertEqual(i, exchange.input_symbols.find(cheese_token))
    # Compiled using bytes in brackets.
    cheese_bytes = "".join("[{:d}]".format(ord(ch)) for ch in cheese)
    reply_bytes = "".join("[{:d}]".format(ord(ch)) for ch in reply)
    cheese_compiled = acceptor(cheese_bytes)
    exchange = transducer(cheese_bytes, reply_bytes)
    self.assertEqual(cheese_compiled, cheese)
    exchange.project()
    exchange.rmepsilon()
    self.assertEqual(exchange, cheese)
    self.assertEqual(exchange, cheese_compiled)
    # String compilation exceptions.
    with self.assertRaises(FstBadWeightError):
      unused_a = acceptor(cheese, weight="nonexistent")
    with self.assertRaises(FstBadWeightError):
      unused_t = transducer(cheese, reply, weight="nonexistent")
    with self.assertRaises(FstArgError):
      unused_a = acceptor(cheese, arc_type="nonexistent")
    with self.assertRaises(FstArgError):
      unused_t = transducer(cheese, reply, arc_type="nonexistent")
    with self.assertRaises(FstStringCompilationError):
      unused_a = acceptor(cheese + "[0xfffffffffffffffffffff]")
    with self.assertRaises(FstStringCompilationError):
      unused_t = transducer(cheese, reply + "[0xfffffffffffffffffffff]")
    with self.assertRaises(FstStringCompilationError):
      unused_a = acceptor(cheese + "]")
    with self.assertRaises(FstStringCompilationError):
      unused_t = transducer(cheese, reply + "]")
    # Compiled from UTF-8-encoded Unicode strings.
    cheese = u"Pont l'Evêque"
    reply = "No"
    cheese_compiled = acceptor(cheese)
    self.assertEqual(cheese_compiled, cheese)
    exchange = transducer(cheese, reply)
    exchange.project()
    exchange.rmepsilon()
    self.assertEqual(exchange, cheese)
    self.assertEqual(exchange, cheese_compiled)
    # Compiled using escaped brackets.
    a = acceptor("[\[Camembert\] is a]\[cheese\]")
    # Should have 3 states accepting generated symbols, 8 accepting a byte,
    # and 1 final state.
    self.assertEqual(a.num_states, 12)

  def testStringify(self):
    ## Arc labels as bytes or Unicode codepoints.
    # ASCII bytestring.
    cheese = b"Fynbo"
    self.assertEqual(acceptor(cheese, token_type="byte").stringify("byte"),
                     cheese)
    self.assertEqual(acceptor(cheese, token_type="utf8").stringify("utf8"),
                     cheese)
    # UTF-8 bytestring.
    cheese = u"Pont l'Evêque".encode("utf8")
    self.assertEqual(acceptor(cheese, token_type="byte").stringify("byte"),
                     cheese)
    self.assertEqual(acceptor(cheese, token_type="utf8").stringify("utf8"),
                     cheese)
    # Unicode string; note that `acceptor` encodes Unicode string args as
    # UTF-8 as a preprocessing step before string compilation.
    cheese = u"Pont l'Evêque"
    self.assertEqual(acceptor(cheese, token_type="byte").stringify("byte"),
                     cheese.encode("utf8"))
    a = acceptor(cheese, token_type="utf8")
    self.assertEqual(a.stringify("utf8"), cheese.encode("utf8"))
    # Should still work after its symbol table is deleted.
    a.set_output_symbols(None)
    self.assertEqual(a.stringify("utf8"), cheese.encode("utf8"))
    ## Arc labels interpreted using the symbol table.
    a = acceptor(cheese, token_type="utf8")
    self.assertEqual(a.stringify("symbol"),
                     b"P o n t <space> l ' E v <0xea> q u e")
    # After its symbol table is deleted, it returns a string of
    # --fst_field_separator -delimited Unicode code points.
    a.set_output_symbols(None)
    codepoints = [int(cp) for cp in a.stringify("symbol").split()]
    self.assertEqual(codepoints, [ord(cp) for cp in cheese])
    ## Degenerate behaviors.
    # With a transducer (where the upper language string argument is longer),
    # it returns the lower language string padded with null bytes.
    self.assertEqual(transducer("Gouda", "Edam").stringify(), b"Edam\0")
    # With a non-string FST, it doesn't work.
    with self.assertRaises(FstArgError):
      unused_s = union("Fynbo", "Pont l'Evêque").stringify()

  def testExceptions(self):
    nonex = "nonexistent"
    # FstArgError.
    f = Fst()
    with self.assertRaises(FstArgError):
      unused_fst = rmepsilon(f, qt=nonex)
    with self.assertRaises(FstArgError):
      unused_fst = compose(f, f, cf=nonex)
    with self.assertRaises(FstArgError):
      unused_fst = randgen(f, select=nonex)
    with self.assertRaises(FstArgError):
      pairs = enumerate((f, f, f), 1)
      unused_fst = replace(pairs, call_arc_labeling=nonex)
    # FstIndexError.
    with self.assertRaises(FstIndexError):
      f.num_arcs(-1)
    with self.assertRaises(FstIndexError):
      f.num_input_epsilons(-1)
    with self.assertRaises(FstIndexError):
      f.num_output_epsilons(-1)
    with self.assertRaises(FstIndexError):
      f.add_arc(-1, 0, 0, Weight("tropical", 0), 0)
    with self.assertRaises(FstIndexError):
      f.delete_arcs(-1)
    with self.assertRaises(FstIndexError):
      f.delete_states([-1])
    with self.assertRaises(FstIndexError):
      f.reserve_arcs(-1, 10)
    with self.assertRaises(FstIndexError):
      f.set_final(-1, Weight("tropical", 0))
    # FstOpError.
    # Attempts mutation operations using weights of the wrong type.
    f = Fst()
    i = f.add_state()
    o = f.add_state()
    wrong_weight = Weight.One("log64")
    with self.assertRaises(FstOpError):
      f.copy().add_arc(i, 0, 0, wrong_weight, o)
    with self.assertRaises(FstOpError):
      unused_fst = determinize(f.copy(), weight=wrong_weight)
    with self.assertRaises(FstOpError):
      unused_fst = disambiguate(f.copy(), weight=wrong_weight)
    with self.assertRaises(FstOpError):
      f.copy().prune(weight=wrong_weight)
    with self.assertRaises(FstOpError):
      f.copy().rmepsilon(weight=wrong_weight)
    with self.assertRaises(FstOpError):
      f.copy().set_final(i, wrong_weight)
    # FstUnknownWeightError.
    with self.assertRaises(FstUnknownWeightTypeError):
      unused_weight = Weight(nonex, 1)
    # Queries for non-existent keys and indices raise KeyError.
    s = SymbolTable()
    with self.assertRaises(KeyError):
      s.find(nonex)
    with self.assertRaises(KeyError):
      s.find(1024)

  # Non-static helper.
  def TestRule(self, rule, istring, ostring):
    self.assertEqual(optimize(project(istring * rule, True)), ostring)

  def testContextDependentRewriteRuleCompilation(self):
    # Creates sigma* from ASCII letters.
    sigma = union(*string.ascii_letters)
    sigma.closure()
    sigma.optimize()
    # A -> B / C __ D.
    a_to_b = cdrewrite(transducer("A", "B"), "C", "D", sigma)
    self.TestRule(a_to_b, "CADCAD", "CBDCBD")
    # Pre-Latin rhotacism: s > r / V __ V
    vowel = union("A", "E", "I", "O", "V")
    rhotacism = cdrewrite(transducer("S", "R"), vowel, vowel, sigma)
    self.TestRule(rhotacism, "LASES", "LARES")
    # "Pre-s" deletion in Classical Latin:
    # [+cor] -> 0 / __ [+str]                 (condition: LTR)
    coronal = union("L", "N", "R", "T", "D")
    strident = "S"
    pre_s_deletion = cdrewrite(transducer(coronal, ""), "", strident + "[EOS]",
                               sigma)
    pre_s_deletion.optimize()
    self.TestRule(pre_s_deletion, "CONCORDS", "CONCORS")
    self.TestRule(pre_s_deletion, "PVLTS", "PVLS")        # cf. gen.sg. PVLTIS
    self.TestRule(pre_s_deletion, "HONORS", "HONOS")      # cf. gen.sg. HONORIS
    # cf. gen.sg. SANGVINIS
    self.TestRule(pre_s_deletion, "SANGVINS", "SANGVIS")
    # But, applied RTL, the rule would incorrectly iterate.
    pre_s_deletion_wrong = cdrewrite(transducer(coronal, ""), "",
                                     strident + "[EOS]", sigma, direction="rtl")
    # should be CONCORS
    self.TestRule(pre_s_deletion_wrong, "CONCORDS", "CONCOS")
    # Prothesis in loanwords in Hindi (informally):
    # 0 -> i / # __ [+str] [-cor, +con]
    non_coronal_consonant = union("M", "P", "B", "K", "G")
    prothesis = cdrewrite(transducer("", "I"), "[BOS]",
                          strident + non_coronal_consonant, sigma)
    self.TestRule(prothesis, "SKUUL", "ISKUUL")  # "school"
    # Optional TD-deletion in English:
    # [+cor, +obst, -cont] -> 0 / [+cons] __ #       (condition: LTR, optional)
    cons = union("M", "P", "B", "F", "V", "N", "S", "Z", "T", "D", "L", "K",
                 "G")  # etc.
    td_deletion = cdrewrite(transducer(union("T", "D"), ""), cons, "[EOS]",
                            sigma, direction="ltr", mode="opt")
    # Asserts that both are possible.
    self.TestRule(td_deletion, "FIST", optimize(union("FIS", "FIST")))

  def testBadContextDependentRewriteRules(self):
    # Creates sigma* from ASCII letters.
    sigma = union(*string.ascii_letters)
    sigma.closure()
    sigma.optimize()
    with self.assertRaises(FstOpError):
      unused_f = cdrewrite(acceptor("[tau]"), "[lambda]", "[rho]", sigma)
    tau = transducer("[phi]", "[psi]")
    with self.assertRaises(FstOpError):
      unused_f = cdrewrite(tau, transducer("[lambda]", "[lambda_prime]"),
                           "[rho]", sigma)
    with self.assertRaises(FstOpError):
      unused_f = cdrewrite(tau, acceptor("[lambda]", 2), "[rho]", sigma)
    with self.assertRaises(FstOpError):
      unused_f = cdrewrite(tau, "[lambda]", transducer("[rho]", "[rho_prime]"),
                           sigma)
    with self.assertRaises(FstOpError):
      unused_f = cdrewrite(tau, "[lambda]", acceptor("[rho]", 2), sigma)
    with self.assertRaises(FstOpError):
      unused_f = cdrewrite(tau, "[lambda]", "[rho]", "[sigma_prime]")

  def testFar(self):
    pairs = {b"1": acceptor("Camembert"), b"2": acceptor("Gruyere"),
             b"3": acceptor("Cheddar")}
    farfile = os.path.join(tempfile.mkdtemp(), "test.far")
    # STTable.
    with Far(farfile, "w", far_type="sttable") as sink:
      for (k, f) in sorted(iteritems(pairs)):
        sink[k] = f
    del sink
    with Far(farfile, "r") as source:
      self.assertEqual(source.far_type, b"sttable")
      for (k, f) in source:
         self.assertEqual(pairs[k], f)
    # STList.
    with Far(farfile, "w", far_type="stlist") as sink:
      for (k, f) in sorted(iteritems(pairs)):
        sink[k] = f
    del sink
    with Far(farfile, "r") as source:
      self.assertEqual(source.far_type, b"stlist")
      for (k, f) in source:
         self.assertEqual(pairs[k], f)

  def testPdt(self):
    s_rhs = union("a[S]b", "ab")  # a^n b^n.
    (f, parens) = pdt_replace("[S]", S=s_rhs)
    for n in xrange(1, 100):
      anbn = n * "a" + n * "b"
      self.assertEqual(pdt_compose(f, anbn, parens, cf="expand"), anbn)

  def testWorkedExample(self):
    pairs = zip(string.ascii_lowercase, string.ascii_uppercase)
    self.upcaser = union(*(transducer(*pair) for pair in pairs))
    self.upcaser.closure()
    self.upcaser.optimize()
    self.downcaser = invert(self.upcaser)
    awords = "You do have some cheese do you".lower().split()
    for aword in awords:
      result = compose(aword, self.upcaser)
      result.rmepsilon()
      result.project(True)
      self.assertEqual(result, aword.upper())
    cheese = "Parmesan".lower()
    cascade = compose(cheese, self.upcaser, self.downcaser, self.upcaser,
                      self.downcaser)
    self.assertEqual(cascade, cheese)


if __name__ == "__main__":
  unittest.main()
