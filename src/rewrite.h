// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// Copyright 2017 and onwards Google, Inc.
//
// For general information on the Pynini grammar compilation library, see
// pynini.opengrm.org.

#ifndef PYNINI_REWRITE_H_
#define PYNINI_REWRITE_H_

#include <string>
using std::string;
#include <type_traits>
#include <utility>
#include <vector>

#include <fst/types.h>
#include <fst/log.h>
#include <fst/extensions/mpdt/compose.h>
#include <fst/extensions/pdt/compose.h>
#include <fst/fstlib.h>
#include "paths.h"
#include "stringcompile.h"

// Generic rewrite utilities for string inputs.

namespace fst {
namespace internal {

// Post-composition check and cleanup.
template <class Arc>
inline bool CheckNonEmptyAndCleanup(MutableFst<Arc> *lattice) {
  if (lattice->Start() == kNoStateId) return false;
  Project(lattice, PROJECT_OUTPUT);
  RmEpsilon(lattice);
  return true;
}

}  // namespace internal

// Constructs a weighted, epsilon-free lattice of output strings given a
// input FST and a rule FST.
//
// Callers may wish to arc-sort the input side of the rule ahead of time.
template <class Arc>
bool RewriteLattice(const Fst<Arc> &input, const Fst<Arc> &rule,
                    MutableFst<Arc> *lattice) {
  static const ComposeOptions opts(true, ALT_SEQUENCE_FILTER);
  Compose(input, rule, lattice, opts);
  return internal::CheckNonEmptyAndCleanup(lattice);
}

// Same, but supports PDT composition.
template <class Arc>
bool RewriteLattice(
    const Fst<Arc> &input, const Fst<Arc> &rule, MutableFst<Arc> *lattice,
    const std::vector<std::pair<typename Arc::Label, typename Arc::Label>>
        &pdt_parens) {
  static const PdtComposeOptions opts(true, EXPAND_FILTER);
  Compose(input, rule, pdt_parens, lattice, opts);
  return internal::CheckNonEmptyAndCleanup(lattice);
}

// Same, but supports MPDT composition.
template <class Arc>
bool RewriteLattice(
    const Fst<Arc> &input, const Fst<Arc> &rule, MutableFst<Arc> *lattice,
    const std::vector<std::pair<typename Arc::Label, typename Arc::Label>>
        &pdt_parens,
    const std::vector<typename Arc::Label> &mpdt_assignments) {
  static const MPdtComposeOptions opts(true, EXPAND_FILTER);
  Compose(input, rule, pdt_parens, mpdt_assignments, lattice, opts);
  return internal::CheckNonEmptyAndCleanup(lattice);
}

// Given an epsilon-free lattice of output strings (such as produced by
// RewriteLattice), attempts to determinize it, pruning non-optimal paths if
// `optimal_only` is true. This is valid only in a semiring with the path
// property.
//
// To prevent unexpected blowup during determinization, a state threshold is
// also used and a warning is logged if this exact threshold is reached. The
// threshold is a multiplier of the size of the input lattice (by default, 4),
// plus a small constant factor. This is intended to be a sensible default
// and is not an inherently meaningful value in and of itself.
template <class Arc>
void LatticeToDfa(MutableFst<Arc> *lattice, bool optimal_only,
                  typename Arc::StateId state_multiplier = 4) {
  using StateId = typename Arc::StateId;
  using Weight = typename Arc::Weight;
  const auto &weight_threshold = optimal_only ? Weight::One() : Weight::Zero();
  const StateId state_threshold = 256 + state_multiplier * lattice->NumStates();
  const DeterminizeOptions<Arc> opts(kDelta, weight_threshold, state_threshold);
  Determinize(*lattice, lattice, opts);
  // Warns if we actually hit the state threshold; if so, we do not have the
  // full set of (optimal) rewrites; there may be cycles of unweighted
  // insertions, or the state threshold may just be too low.
  if (lattice->NumStates() == state_threshold) {
    LOG(WARNING) << "Unexpectedly hit state threshold; consider a higher value "
                    "for state_multiplier";
  }
}

// Given an epsilon-free lattice of output strings (such as produced by
// RewriteLattice), extracts n-shortest unique strings. This is valid only in
// a semiring with the path property.
template <class Arc>
void LatticeToShortest(MutableFst<Arc> *lattice, int32 nshortest) {
  VectorFst<Arc> shortest;
  // By requesting unique solutions we request on-the-fly determinization.
  ShortestPath(*lattice, &shortest, nshortest, /*unique=*/true);
  *lattice = shortest;
}

// Given an epsilon-free lattice of output strings (such as produced by
// RewriteLattice), extracts a single top string. This is only valid in a path
// semiring.
template <class Arc>
bool LatticeToTopString(const Fst<Arc> &lattice, string *output,
                        StringTokenType ttype = BYTE,
                        const SymbolTable *syms = nullptr) {
  VectorFst<Arc> ofst;
  ShortestPath(lattice, &ofst);
  return PrintString(ofst, output, ttype, syms);
}

// Attempts to extract a single top rewrite from a optimized DFA, logging a
// warning and returning false if there's a tie. This is valid only
template <class Arc>
bool LatticeToOneTopString(const Fst<Arc> &dfa_lattice, string *output,
                           StringTokenType ttype = BYTE,
                           const SymbolTable *syms = nullptr) {
  StringPathIterator<Arc> paths(dfa_lattice, ttype, syms,
                                /*check_acyclic=*/false);
  DCHECK(!paths.Error());
  DCHECK(!paths.Done());
  *output = paths.OString();
  paths.Next();
  if (!paths.Done()) {
    LOG(ERROR) << "Multiple top rewrites found: '" << *output << "' and '"
               << paths.OString() << "' (weight: " << paths.Weight() << ")";
    return false;
  }
  return !paths.Error();
}

// Clears vector and writes lattice strings to it.
template <class Arc>
bool LatticeToStrings(const Fst<Arc> &lattice, std::vector<string> *outputs,
                      StringTokenType ttype = BYTE,
                      const SymbolTable *syms = nullptr) {
  outputs->clear();
  if (lattice.Properties(kAcyclic, true) != kAcyclic) {
    LOG(ERROR) << "Lattice is unexpectedly cyclic";
    return false;
  }
  // Input token type and symbol table will be ignored; the lattice is
  // assumed to be acyclic.
  StringPathIterator<Arc> paths(lattice, ttype, syms, /*check_acyclic=*/false);
  DCHECK(!paths.Error());
  for (; !paths.Done(); paths.Next()) outputs->emplace_back(paths.OString());
  return !paths.Error();
}

// Top rewrite.
template <class Arc>
bool TopRewrite(const Fst<Arc> &input, const Fst<Arc> &rule, string *output,
                StringTokenType ttype = BYTE,
                const SymbolTable *syms = nullptr) {
  VectorFst<Arc> lattice;
  return RewriteLattice(input, rule, &lattice) &&
         LatticeToTopString(lattice, output, ttype, syms);
}

// Top rewrite, returning false and logging if there's a tie.
template <class Arc>
bool OneTopRewrite(const Fst<Arc> &input, const Fst<Arc> &rule, string *output,
                   StringTokenType ttype = BYTE,
                   const SymbolTable *syms = nullptr,
                   typename Arc::StateId state_multiplier = 4) {
  VectorFst<Arc> lattice;
  if (!RewriteLattice(input, rule, &lattice)) return false;
  LatticeToDfa(&lattice, /*optimal_only=*/true, state_multiplier);
  return LatticeToOneTopString(lattice, output, ttype, syms);
}

// All rewrites.
template <class Arc>
bool Rewrites(const Fst<Arc> &input, const Fst<Arc> &rule,
              std::vector<string> *outputs, StringTokenType ttype = BYTE,
              const SymbolTable *syms = nullptr,
              typename Arc::StateId state_multiplier = 4) {
  VectorFst<Arc> lattice;
  if (!RewriteLattice(input, rule, &lattice)) return false;
  LatticeToDfa(&lattice, /*optimal_only=*/false, state_multiplier);
  return LatticeToStrings(lattice, outputs, ttype, syms);
}

// The same, but with repeated string fields.
// All optimal rewrites.
template <class Arc>
bool TopRewrites(const Fst<Arc> &input, const Fst<Arc> &rule,
                 std::vector<string> *outputs, StringTokenType ttype = BYTE,
                 const SymbolTable *syms = nullptr,
                 typename Arc::StateId state_multiplier = 4) {
  VectorFst<Arc> lattice;
  if (!RewriteLattice(input, rule, &lattice)) return false;
  LatticeToDfa(&lattice, /*optimal_only=*/true, state_multiplier);
  return LatticeToStrings(lattice, outputs, ttype, syms);
}

// The same, but with repeated string fields.
// The top n rewrites.
template <class Arc>
bool TopRewrites(const Fst<Arc> &input, const Fst<Arc> &rule, int32 nshortest,
                 std::vector<string> *outputs, StringTokenType ttype = BYTE,
                 const SymbolTable *syms = nullptr) {
  VectorFst<Arc> lattice;
  if (!RewriteLattice(input, rule, &lattice)) return false;
  LatticeToShortest(&lattice, nshortest);
  return LatticeToStrings(lattice, outputs, ttype, syms);
}

// The same, but with repeated string fields.
// Determines whether a lattice contains an output.
template <class Arc>
bool Matches(const Fst<Arc> &lattice, const Fst<Arc> &output) {
  VectorFst<Arc> intersection;
  static const IntersectOptions opts(true, ALT_SEQUENCE_FILTER);
  Intersect(lattice, output, &intersection, opts);
  return intersection.Start() != kNoStateId;
}

// Determines whether a rule allows an input/output pair.
template <class Arc>
bool Matches(const Fst<Arc> &input, const Fst<Arc> &output,
             const Fst<Arc> &rule) {
  VectorFst<Arc> lattice;
  if (!RewriteLattice(input, rule, &lattice)) return false;
  ArcSort(&lattice, OLabelCompare<Arc>());
  return Matches(lattice, output);
}

}  // namespace fst

#endif  // PYNINI_REWRITE_H_

