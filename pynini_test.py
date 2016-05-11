# Encoding: UTF-8
# For general information on the Pynini grammar compilation library, see
# pynini.opengrm.org.


"""Tests for the Pynini grammar compilation module."""


import itertools
import string
import unittest

# This module is designed to be import-safe.
# pylint: disable=wildcard-import
# pylint: disable=undefined-variable
from pynini import *


SEED = 212


class PyniniCDRewriteTest(unittest.TestCase):

  @classmethod
  def setUpClass(cls):
    cls.sigstar = union(*string.letters)
    cls.sigstar.closure()
    cls.sigstar.optimize()
    cls.coronal = union("L", "N", "R", "T", "D")

  # Non-static helper.
  def TestRule(self, rule, istring, ostring):
    self.assertEqual((istring * rule).stringify(), ostring)

  # A -> B / C __ D.
  def testAGoesToBInTheContextOfCAndD(self):
    a_to_b = cdrewrite(transducer("A", "B"), "C", "D", self.sigstar)
    self.TestRule(a_to_b, "CADCAD", "CBDCBD")

  # Pre-Latin rhotacism:
  # s > r / V __ V.
  def testRhotacism(self):
    vowel = union("A", "E", "I", "O", "V")
    rhotacism = cdrewrite(transducer("S", "R"), vowel, vowel, self.sigstar)
    self.TestRule(rhotacism, "LASES", "LARES")

  # Classical-Latin "Pre-s deletion":
  # [+cor] -> 0 / __ [+str] (condition: LTR)
  def testPreSDeletion(self):
    pre_s_deletion = cdrewrite(transducer(self.coronal, ""), "", "S[EOS]",
                               self.sigstar)
    pre_s_deletion.optimize()
    self.TestRule(pre_s_deletion, "CONCORDS", "CONCORS")
    self.TestRule(pre_s_deletion, "PVLTS", "PVLS")        # cf. gen.sg. PVLTIS
    self.TestRule(pre_s_deletion, "HONORS", "HONOS")      # cf. gen.sg. HONORIS
    # cf. gen.sg. SANGVINIS
    self.TestRule(pre_s_deletion, "SANGVINS", "SANGVIS")

  # The same, but incorrectly applied RTL.
  def testPreSDeletionRTL(self):
    pre_s_deletion_wrong = cdrewrite(transducer(self.coronal, ""), "",
                                     "S[EOS]", self.sigstar, direction="rtl")
    # Should be CONCORS.
    self.TestRule(pre_s_deletion_wrong, "CONCORDS", "CONCOS")

  # Prothesis in loanwords in Hindi (informally):
  # 0 -> i / # __ [+str] [-cor, +con]
  def testProthesis(self):
    non_coronal_consonant = union("M", "P", "B", "K", "G")
    prothesis = cdrewrite(transducer("", "I"), "[BOS]",
                          "S" + non_coronal_consonant, self.sigstar)
    self.TestRule(prothesis, "SKUUL", "ISKUUL")  # "school"

  # TD-deletion in English:
  # [+cor, +obst, -cont] -> 0 / [+cons] __ # (conditions: LTR, optional)
  def testTDDeletion(self):
    cons = union("M", "P", "B", "F", "V", "N", "S", "Z", "T", "D", "L", "K",
                 "G")  # etc.
    td_deletion = cdrewrite(transducer(union("T", "D"), ""), cons, "[EOS]",
                            self.sigstar, direction="ltr", mode="opt")
    # Asserts that both are possible.
    self.assertEqual(optimize(project("FIST" * td_deletion, True)),
                     optimize(union("FIS", "FIST")))

  def testTauAcceptorRaisesFstOpError(self):
    with self.assertRaises(FstOpError):
      unused_f = cdrewrite(acceptor("[tau]"), "[lambda]", "[rho]", self.sigstar)

  def testLambdaTransducerRaisesFstOpError(self):
    with self.assertRaises(FstOpError):
      unused_f = cdrewrite(transducer("[phi]", "[psi]"),
                           transducer("[lambda]", "[lambda_prime]"),
                           "[rho]", self.sigstar)

  def testRhoTransducerRaisesFstOpError(self):
    with self.assertRaises(FstOpError):
      unused_f = cdrewrite(transducer("[phi]", "[psi]"), "[lambda]",
                           transducer("[rho]", "[rho_prime]"), self.sigstar)

  def testWeightedLambdaRaisesFstOpError(self):
    with self.assertRaises(FstOpError):
      unused_f = (
          cdrewrite(
              transducer("[phi]", "[psi]"), acceptor("[lambda]", 2),
              "[rho]", self.sigstar))

  def testWeightedRhoRaisesFstOpError(self):
    with self.assertRaises(FstOpError):
      unused_f = cdrewrite(transducer("[phi]", "[psi]"), "[lambda]",
                           acceptor("[rho]", 2), self.sigstar)


class PyniniClosureTest(unittest.TestCase):

  def testRangeClosure(self):
    m = 3
    n = 7
    cheese = "Red Windsor"
    a = acceptor(cheese)
    a.closure(m, n)
    # Doesn't accept <3 copies.
    for i in xrange(m):
      self.assertEqual(compose(a, cheese * i).num_states, 0)
    # Accepts between 3-7 copies.
    for i in xrange(m, n + 1):
      self.assertTrue(compose(a, cheese * i).num_states != 0)
    # Doesn't accept more than 7 copies.
    self.assertEqual(compose(a, cheese * (n + 1)).num_states, 0)


class PyniniEqualTest(unittest.TestCase):

  @classmethod
  def setUpClass(cls):
    cls.f = acceptor("Danish Blue")

  def testEqual(self):
    self.assertTrue(equal(self.f, self.f.copy()))

  def testEqualOperator(self):
    self.assertTrue(self.f == self.f.copy())

  def testNotEqualOperator(self):
    self.assertFalse(self.f != self.f.copy())


class PyniniExceptionsTest(unittest.TestCase):

  @classmethod
  def setUpClass(cls):
    cls.exchange = transducer("Liptauer", "No")
    cls.f = Fst()
    cls.s = SymbolTable()

  def testBadDestinationIndexAddArcDoesNotRaiseFstIndexError(self):
    f = self.f.copy()
    s = f.add_state()
    f.set_start(s)
    f.set_final(s)
    f.add_arc(s, Arc(0, 0, 0, -1))
    self.assertFalse(f.verify())

  def testBadIndexNumArcsRaisesFstIndexError(self):
    with self.assertRaises(FstIndexError):
      unused_n = self.f.num_arcs(-1)

  def testBadIndexNumInputEpsilonsRaisesFstIndexError(self):
    with self.assertRaises(FstIndexError):
      unused_n = self.f.num_input_epsilons(-1)

  def testBadIndexNumOutputEpsilonsRaisesFstIndexError(self):
    with self.assertRaises(FstIndexError):
      unused_n = self.f.num_output_epsilons(-1)

  def testBadIndexDeleteArcsRaisesFstIndexError(self):
    f = self.f.copy()
    with self.assertRaises(FstIndexError):
      f.delete_arcs(-1)

  def testBadIndicesDeleteStatesRaisesFstIndexError(self):
    f = self.f.copy()
    with self.assertRaises(FstIndexError):
      f.delete_states((-1, -2))

  def testBadSourceIndexAddArcRaisesFstIndexError(self):
    f = self.f.copy()
    with self.assertRaises(FstIndexError):
      f.add_arc(-1, Arc(0, 0, 0, 0))

  def testGarbageComposeFilterComposeRaisesFstArgError(self):
    with self.assertRaises(FstArgError):
      unused_f = compose(self.f, self.f, cf="nonexistent")

  def testGarbageComposeFilterDifferenceRaisesFstArgError(self):
    with self.assertRaises(FstArgError):
      unused_f = difference(self.f, self.f, cf="nonexistent")

  def testGarbageQueueTypeRmepsilonRaisesFstArgError(self):
    with self.assertRaises(FstArgError):
      unused_f = rmepsilon(self.f, qt="nonexistent")

  def testGarbageQueueTypeShortestDistanceRaisesFstArgError(self):
    with self.assertRaises(FstArgError):
      unused_sd = shortestdistance(self.f, qt="nonexistent")

  def testGarbageQueueTypeShortestPathRaisesFstArgError(self):
    with self.assertRaises(FstArgError):
      unused_f = shortestpath(self.f, qt="nonexistent")

  def testGarbageSelectTypeRandgenRaisesFstArgError(self):
    with self.assertRaises(FstArgError):
      unused_f = randgen(self.f, select="nonexistent")

  def testGarbageCallArcLabelingReplaceRaisesFstArgError(self):
    with self.assertRaises(FstArgError):
      unused_f = replace(self.f, f=self.f, call_arc_labeling="nonexistent")

  def testGarbageReturnArcLabelingReplaceRaisesFstArgError(self):
    with self.assertRaises(FstArgError):
      unused_f = replace(self.f, f=self.f, return_arc_labeling="nonexistent")

  def testTransducerDifferenceRaisesFstOpError(self):
    with self.assertRaises(FstOpError):
      unused_f = difference(self.exchange, self.exchange)

  def testTransducerEquivalentRaisesFstOpError(self):
    with self.assertRaises(FstOpError):
      unused_f = equivalent(self.exchange, self.exchange)

  def testWrongWeightTypeAddArcRaisesFstOpError(self):
    f = self.f.copy()
    s = f.add_state()
    f.set_start(s)
    f.set_final(s)
    with self.assertRaises(FstOpError):
      f.add_arc(s, Arc(0, 0, Weight.One("log"), 0))

  def testWrongWeightTypeDeterminizeRaisesFstOpError(self):
    with self.assertRaises(FstOpError):
      unused_f = determinize(self.f, weight=Weight.One("log"))

  def testWrongWeightTypeDisambiguateRaisesFstOpError(self):
    with self.assertRaises(FstOpError):
      unused_f = disambiguate(self.f, weight=Weight.One("log"))

  def testWrongWeightTypePruneRaisesFstOpError(self):
    with self.assertRaises(FstOpError):
      unused_f = prune(self.f, weight=Weight.One("log"))

  def testWrongWeightTypeRmepsilonRaisesFstOpError(self):
    with self.assertRaises(FstOpError):
      unused_f = rmepsilon(self.f, weight=Weight.One("log"))

  def testWrongWeightTypeSetFinalRaisesFstOpError(self):
    f = self.f.copy()
    s = f.add_state()
    f.set_start(s)
    with self.assertRaises(FstOpError):
      f.set_final(s, Weight.One("log"))

  def testGarbageWeightTypeRaisesFstUnknownWeightTypeError(self):
    with self.assertRaises(FstUnknownWeightTypeError):
      unused_w = Weight("nonexistent", 1)

  def testNonexistentKeyFindSymbolTableRaisesKeyError(self):
    with self.assertRaises(KeyError):
      self.s.find("nonexistent")

  def testNonexistentIndexFindSymbolTableRaisesKeyError(self):
    with self.assertRaises(KeyError):
      self.s.find(1024)


class PyniniPdtReplaceTest(unittest.TestCase):

  def testPdtReplace(self):
    s_rhs = union("a[S]b", "ab")  # a^n b^n.
    (f, parens) = pdt_replace("[S]", S=s_rhs)
    for n in xrange(1, 100):
      anbn = n * "a" + n * "b"
      self.assertEqual(pdt_compose(f, anbn, parens, cf="expand"), anbn)


class PyniniReplaceTest(unittest.TestCase):

  # Based loosely on an example from Thrax.

  def testReplace(self):
    root = acceptor("[Number] [Measure]")
    singular_numbers = transducer("1", "one")
    singular_measurements = string_map((("ft", "foot"), ("in", "inch"),
                                        ("cm", "centimeter"), ("m", "meter"),
                                        ("kg", "kilogram")))
    singular = replace(root, Number=singular_numbers,
                       Measure=singular_measurements,
                       call_arc_labeling="neither",
                       return_arc_labeling="neither")
    self.assertEqual(optimize(project("1 ft" * singular, True)), "one foot")
    plural_numbers = string_map((("2", "two"), ("3", "three"), ("4", "four"),
                                 ("5", "five"), ("6", "six"), ("7", "seven"),
                                 ("8", "eight"), ("9", "nine")))
    plural_measurements = string_map((("ft", "feet"), ("in", "inches"),
                                      ("cm", "centimeter"), ("m", "meters"),
                                      ("kg", "kilograms")))
    plural = replace(root, Number=plural_numbers, Measure=plural_measurements,
                     call_arc_labeling="neither", return_arc_labeling="neither")
    self.assertEqual(optimize(project("2 m" * plural, True)), "two meters")


class PyniniStringTest(unittest.TestCase):

  """Tests string compilation and stringification."""

  @classmethod
  def setUpClass(cls):
    cls.cheese = b"Red Leicester"
    cls.reply = b"I'm afraid we're fresh out of Red Leicester sir"
    cls.imported_cheese = u"Pont l'Evêque"
    cls.imported_cheese_encoded = cls.imported_cheese.encode("utf8")

  def testUnbracketedBytestringUnweightedAcceptorCompilation(self):
    cheese = acceptor(self.cheese)
    self.assertEqual(cheese, self.cheese)

  def testUnbracketedBytestringUnweightedTransducerCompilation(self):
    exchange = transducer(self.cheese, self.reply)
    exchange.project()
    exchange.rmepsilon()
    self.assertEqual(exchange, self.cheese)

  def testUnbracketedBytestringWeightedAcceptorCompilation(self):
    cheese = acceptor(self.cheese, weight=Weight.One("tropical"))
    self.assertEqual(cheese, self.cheese)

  def testUnbracketedBytestringWeightedTransducerCompilation(self):
    exchange = transducer(self.cheese, self.reply,
                          weight=Weight.One("tropical"))
    exchange.project()
    exchange.rmepsilon()
    self.assertEqual(exchange, self.cheese)

  def testUnbracketedBytestringCastingWeightedAcceptorCompilation(self):
    cheese = acceptor(self.cheese, weight=0)
    self.assertEqual(cheese, self.cheese)

  def testBracketedTokenizationAcceptorCompilation(self):
    cheese_tokens = self.cheese.split()
    cheese = acceptor("".join("[{}]".format(t) for t in cheese_tokens))
    i = cheese.input_symbols.find(cheese_tokens[1])  # "Leicester".
    self.assertGreater(i, 255)

  def testBracketedCharsBytestringAcceptorCompilation(self):
    cheese = acceptor("".join("[{:d}]".format(ord(ch)) for ch in self.cheese))
    self.assertEqual(cheese, self.cheese)

  def testUnicodeBytestringAcceptorCompilation(self):
    cheese = acceptor(self.imported_cheese)
    self.assertEqual(cheese, self.imported_cheese.encode("utf8"))

  def testAsciiUtf8AcceptorCompilation(self):
    cheese = acceptor(self.cheese, token_type="utf8")
    self.assertEqual(cheese, self.cheese)

  def testUnicodeUtf8AcceptorCompilation(self):
    cheese = acceptor(self.imported_cheese, token_type="utf8")
    for (i, state) in enumerate(cheese.states()):
      for arc in cheese.arcs(state):
        self.assertEqual(unichr(arc.olabel), self.imported_cheese[i])

  def testEscapedBracketsBytestringAcceptorCompilation(self):
    a = acceptor("[\[Camembert\] is a]\[cheese\]")
    self.assertEqual(a.num_states, 12)
    # Should have 3 states accepting generated symbols, 8 accepting a byte,
    # and 1 final state.

  def testGarbageWeightAcceptorRaisesFstBadWeightError(self):
    with self.assertRaises(FstBadWeightError):
      unused_a = acceptor(self.cheese, weight="nonexistent")

  def testGarbageWeightTransducerRaisesFstBadWeightError(self):
    with self.assertRaises(FstBadWeightError):
      unused_t = transducer(self.cheese, self.reply, weight="nonexistent")

  def testGarbageArcTypeAcceptorRaisesFstArgError(self):
    with self.assertRaises(FstArgError):
      unused_a = acceptor(self.cheese, arc_type="nonexistent")

  def testGarbageArcTypeTransducerRaisesFstArgError(self):
    with self.assertRaises(FstArgError):
      unused_t = transducer(self.cheese, self.reply, arc_type="nonexistent")

  def testHugeBracketedNumberAcceptorRaisesFstStringCompilationError(self):
    with self.assertRaises(FstStringCompilationError):
      unused_a = acceptor(self.cheese + "[0xfffffffffffffffffffff]")

  def testHugeBracketedNumberTransducerRaisesFstStringCompilationError(self):
    with self.assertRaises(FstStringCompilationError):
      unused_t = transducer(self.cheese,
                            self.reply + "[0xfffffffffffffffffffff]")

  def testUnbalancedBracketsAcceptorRaisesFstStringCompilationError(self):
    with self.assertRaises(FstStringCompilationError):
      unused_a = acceptor(self.cheese + "]")

  def testUnbalancedBracketsTransducerRaisesFstStringCompilationError(self):
    with self.assertRaises(FstStringCompilationError):
      unused_t = transducer(self.cheese, "[" + self.reply)

  def testCrossProductTransducerCompilation(self):
    cheese = acceptor(self.cheese)
    reply = acceptor(self.reply)
    exchange = transducer(cheese, reply)
    exchange.project()
    exchange.rmepsilon()
    self.assertEqual(exchange, self.cheese)

  def testAsciiByteStringify(self):
    self.assertEqual(acceptor(self.cheese).stringify(), self.cheese)

  def testAsciiUtf8Stringify(self):
    self.assertEqual(acceptor(self.cheese, token_type="utf8").stringify("utf8"),
                     self.cheese)

  def testUtf8ByteStringify(self):
    self.assertEqual(acceptor(self.imported_cheese_encoded).stringify(),
                     self.imported_cheese_encoded)

  def testByteStringifyAfterSymbolTableDeletion(self):
    a = acceptor(self.cheese)
    a.set_output_symbols(None)
    self.assertEqual(a.stringify("utf8"), self.cheese.encode("utf8"))

  def testUtf8Utf8Stringify(self):
    self.assertEqual(acceptor(self.imported_cheese_encoded,
                              token_type="utf8").stringify("utf8"),
                     self.imported_cheese_encoded)

  def testUnicodeByteStringify(self):
    self.assertEqual(acceptor(self.imported_cheese).stringify(),
                     self.imported_cheese_encoded)

  def testUnicodeUtf8Stringify(self):
    self.assertEqual(acceptor(self.imported_cheese,
                              token_type="utf8").stringify("utf8"),
                     self.imported_cheese_encoded)

  def testUtf8StringifyAfterSymbolTableDeletion(self):
    a = acceptor(self.imported_cheese, token_type="utf8")
    a.set_output_symbols(None)
    self.assertEqual(a.stringify("utf8"), self.imported_cheese_encoded)

  def testUnicodeSymbolStringify(self):
    a = acceptor(self.imported_cheese, token_type="utf8")
    self.assertEqual(a.stringify("symbol"),
                     b"P o n t <space> l ' E v <0xea> q u e")

  def testUnicodeSymbolStringifyWithNoSymbolTable(self):
    a = acceptor(self.imported_cheese, token_type="utf8")
    a.set_output_symbols(None)
    codepoints = [int(cp) for cp in a.stringify("symbol").split()]
    self.assertEqual(codepoints, [ord(cp) for cp in self.imported_cheese])

  def testStringifyOnNonkStringFstRaisesFstArgError(self):
    with self.assertRaises(FstArgError):
      unused_a = union(self.cheese, self.imported_cheese).stringify()


class PyniniStringMapTest(unittest.TestCase):

  @classmethod
  def setUpClass(cls):
    cls.pairs = (("[Bel Paese]", "Sorry"), ("Cheddar",),
                 ("Caithness", "Pont-l'Évêque"),
                 ("Pont-l'Évêque", "Camembert"))

  def testHeterogeneousStringMap(self):
    mapper = string_map([self.pairs[0],   # Tuple.
                         self.pairs[1]])  # String.
    self.assertEqual(optimize(project("[Bel Paese]" * mapper, True)), "Sorry")
    self.assertEqual(optimize(project("Cheddar" * mapper, True)), "Cheddar")

  def testByteToByteStringMap(self):
    mapper = string_map(self.pairs)
    self.assertEqual(optimize(project("[Bel Paese]" * mapper, True)), "Sorry")
    self.assertEqual(optimize(project("Cheddar" * mapper, True)), "Cheddar")
    self.assertEqual(optimize(project("Caithness" * mapper, True)),
                     "Pont-l'Évêque")
    self.assertEqual(optimize(project("Pont-l'Évêque" * mapper, True)),
                     "Camembert")

  def testDictionaryStringMap(self):
    mydict = {self.pairs[0][0]: self.pairs[0][1],
              self.pairs[1][0]: self.pairs[1][0]}
    mapper = string_map(mydict)
    self.assertEqual(optimize(project("[Bel Paese]" * mapper, True)), "Sorry")
    self.assertEqual(optimize(project("Cheddar" * mapper, True)), "Cheddar")

  def testByteToUtf8StringMap(self):
    mapper = string_map(self.pairs, output_token_type="utf8")
    self.assertEqual(optimize(project("[Bel Paese]" * mapper, True)), "Sorry")
    self.assertEqual(optimize(project("Cheddar" * mapper, True)), "Cheddar")
    self.assertEqual(optimize(project("Caithness" * mapper, True)),
                     acceptor("Pont-l'Évêque", token_type="utf8"))
    self.assertEqual(optimize(project("Pont-l'Évêque" * mapper, True)),
                     "Camembert")

  def testUtf8ToUtf8StringMap(self):
    mapper = string_map(self.pairs, input_token_type="utf8",
                        output_token_type="utf8")
    self.assertEqual(optimize(project("[Bel Paese]" * mapper, True)), "Sorry")
    self.assertEqual(optimize(project("Cheddar" * mapper, True)), "Cheddar")
    self.assertEqual(optimize(project("Caithness" * mapper, True)),
                     acceptor("Pont-l'Évêque", token_type="utf8"))
    self.assertEqual(optimize(project(acceptor("Pont-l'Évêque",
                                               token_type="utf8") * mapper,
                                      True)),
                     "Camembert")

  def testByteToSymbolStringMap(self):
    syms = SymbolTable()
    syms.add_symbol("<epsilon>")
    syms.add_symbol("Sorry")
    syms.add_symbol("Cheddar")
    syms.add_symbol("Pont-l'Évêque")
    syms.add_symbol("Camembert")
    mapper = string_map(self.pairs, output_token_type=syms)
    sorry = acceptor("Sorry", token_type=syms)
    self.assertEqual(optimize(project("[Bel Paese]" * mapper, True)), sorry)
    cheddar = acceptor("Cheddar", token_type=syms)
    self.assertEqual(optimize(project("Cheddar" * mapper, True)), cheddar)
    pont_levesque = acceptor("Pont-l'Évêque", token_type=syms)
    self.assertEqual(optimize(project("Caithness" * mapper, True)),
                     pont_levesque)


class PyniniStringPathsTest(unittest.TestCase):

  @classmethod
  def setUpClass(cls):
    cls.triples = (("Bel Paese", "Sorry", Weight("tropical", 4.)),
                   ("Red Windsor",
                    "Normally, sir, yes, but today the van broke down.",
                    Weight("tropical", 3.)),
                   ("Stilton", "Sorry", Weight("tropical", 2.)))
    cls.f = union(*(transducer(*triple) for triple in cls.triples))

  def testStringPaths(self):
    for (triple, triple_res) in itertools.izip(self.f.paths(token_type="byte"),
                                               self.triples):
      self.assertEqual(triple_res, triple)


class PyniniWorkedExampleTest(unittest.TestCase):

  def testWorkedExample(self):
    pairs = itertools.izip(string.ascii_lowercase, string.ascii_uppercase)
    self.upcaser = string_map(pairs).closure()
    self.downcaser = invert(self.upcaser)
    awords = "You do have some cheese do you".lower().split()
    for aword in awords:
      result = (aword * self.upcaser).project(True).optimize()
      self.assertEqual(result, aword.upper())
    cheese = "Parmesan".lower()
    cascade = (cheese * self.upcaser * self.downcaser * self.upcaser *
               self.downcaser)
    self.assertEqual(cascade.stringify(), cheese)


if __name__ == "__main__":
  unittest.main()
