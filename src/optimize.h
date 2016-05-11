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
// Copyright 2016 and onwards Google, Inc.
//
// For general information on the Pynini grammar compilation library, see
// pynini.opengrm.org.

#ifndef PYNINI_OPTIMIZE_H_
#define PYNINI_OPTIMIZE_H_

#include <fst/fstlib.h>

// These functions are generic optimization methods for mutable FSTs, inspired
// by those used in Thrax.

namespace fst {

// We don't need to encode weights if the FST has no weighted cycles. This is
// true if the kUnweightedCycles bit is true, and implied by kAcyclic and
// kUnweighted.
const uint64 kDoNotEncodeWeights = kAcyclic | kUnweighted | kUnweightedCycles;

// Generic FST optimization function; use the more-specialized forms below if
// the FST is known to be an acceptor or a transducer.

// Destructive signature.
template <class Arc>
void Optimize(MutableFst<Arc> *fst, bool compute_props = false) {
  if (fst->Properties(kAcceptor, compute_props) != kAcceptor) {
    // The FST is (may be) a transducer.
    OptimizeTransducer(fst, compute_props);
  } else {
    // The FST is (known to be) an acceptor.
    OptimizeAcceptor(fst, compute_props);
  }
}

// Generic FST optimization function to be used when the FST is known to be an
// acceptor.

template <class Arc>
void OptimizeAcceptor(MutableFst<Arc> *fst, bool compute_props = false) {
  // If the FST is not (known to be) epsilon-free, perform epsilon-removal.
  if (fst->Properties(kNoEpsilons, compute_props) != kNoEpsilons)
    RmEpsilon(fst);
  // Combines identically labeled arcs with the same source and destination,
  // and sums their weights.
  StateMap(fst, ArcSumMapper<Arc>(*fst));
  // The FST has non-idempotent weights; limiting optimization possibilities.
  if (!(Arc::Weight::Properties() & kIdempotent)) {
    if (fst->Properties(kIDeterministic, compute_props) != kIDeterministic) {
      // But "any acyclic weighted automaton over a zero-sum-free semiring has
      // the twins property and is determinizable" (Mohri 2006).
      if (fst->Properties(kAcyclic, compute_props) == kAcyclic) {
        std::unique_ptr<MutableFst<Arc>> tfst(fst->Copy());
        Determinize(*tfst, fst);
        Minimize(fst);
      }
    } else {
      Minimize(fst);
    }
  } else {
    // If the FST is not (known to be) deterministic, determinize it.
    if (fst->Properties(kIDeterministic, compute_props) != kIDeterministic) {
      // If the FST is not known to have no weighted cycles, it is encoded
      // before determinization and minimization.
      if (!fst->Properties(kDoNotEncodeWeights, compute_props)) {
        EncodeMapper<Arc> encoder(kEncodeWeights, ENCODE);
        Encode(fst, &encoder);
        std::unique_ptr<MutableFst<Arc>> tfst(fst->Copy());
        Determinize(*tfst, fst);
        Minimize(fst);
        Decode(fst, encoder);
        StateMap(fst, ArcSumMapper<Arc>(*fst));
      } else {
        std::unique_ptr<MutableFst<Arc>> tfst(fst->Copy());
        Determinize(*tfst, fst);
        Minimize(fst);
      }
    } else {
      Minimize(fst);
    }
  }
}

// Generic FST optimization function to be used when the FST is (may be) a
// transducer; this can handle acceptors too, but OptimizeAcceptor may be
// quicker.

template <class Arc>
void OptimizeTransducer(MutableFst<Arc> *fst, bool compute_props = false) {
  // If the FST is not (known to be) epsilon-free, perform epsilon-removal.
  if (fst->Properties(kNoEpsilons, compute_props) != kNoEpsilons)
    RmEpsilon(fst);
  // Combines identically labeled arcs with the same source and destination,
  // and sums their weights.
  StateMap(fst, ArcSumMapper<Arc>(*fst));
  // The FST has non-idempotent weights; limiting optimization possibilities.
  if (!(Arc::Weight::Properties() & kIdempotent)) {
    if (fst->Properties(kIDeterministic, compute_props) != kIDeterministic) {
      // But "any acyclic weighted automaton over a zero-sum-free semiring has
      // the twins property and is determinizable" (Mohri 2006). We just have to
      // encode labels.
      if (fst->Properties(kAcyclic, compute_props)) {
        EncodeMapper<Arc> encoder(kEncodeLabels, ENCODE);
        Encode(fst, &encoder);
        std::unique_ptr<MutableFst<Arc>> tfst(fst->Copy());
        Determinize(*tfst, fst);
        Minimize(fst);
        Decode(fst, encoder);
      }
    } else {
      Minimize(fst);
    }
  } else {
    // If the FST is not (known to be) deterministic, determinize it.
    if (fst->Properties(kIDeterministic, compute_props) != kIDeterministic) {
    // FST labels are always encoded before determinization and minimization.
    // If the FST is not known to have no weighted cycles, its weights are also
    // encoded before determinization and minimization.
      if (!fst->Properties(kDoNotEncodeWeights, compute_props)) {
        EncodeMapper<Arc> encoder(kEncodeLabels | kEncodeWeights, ENCODE);
        Encode(fst, &encoder);
        std::unique_ptr<MutableFst<Arc>> tfst(fst->Copy());
        Determinize(*tfst, fst);
        Minimize(fst);
        Decode(fst, encoder);
        StateMap(fst, ArcSumMapper<Arc>(*fst));
      } else {
        EncodeMapper<Arc> encoder(kEncodeLabels, ENCODE);
        Encode(fst, &encoder);
        std::unique_ptr<MutableFst<Arc>> tfst(fst->Copy());
        Determinize(*tfst, fst);
        Minimize(fst);
        Decode(fst, encoder);
      }
    } else {
      Minimize(fst);
    }
  }
}

// This function performs a simple space optimization on FSTs that are
// (unions of) pairs of strings. It first pushes labels towards the initial
// state, then performs epsilon-removal. This will reduce the number of arcs
// and states by the length of the shorter of the two strings in the
// cross-product; label-pushing may also speed up downstream composition.
template <class Arc>
void OptimizeStringCrossProducts(MutableFst<Arc> *fst) {
  std::unique_ptr<MutableFst<Arc>> tfst(fst->Copy());
  // Pushes labels towards the initial state.
  Push<Arc, REWEIGHT_TO_INITIAL>(*tfst, fst, kPushLabels);
  // Removes any trailing epsilon-to-epsilon arcs this produces.
  RmEpsilon(fst);
}

}  // namespace fst

#endif  // PYNINI_OPTIMIZE_H_

