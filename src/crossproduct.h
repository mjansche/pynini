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

#ifndef CROSSPRODUCT_H_
#define CROSSPRODUCT_H_

#include <memory>

#include <fst/fstlib.h>

namespace fst {

// This function combines two acceptors into a cross-product transducer; that
// if U accepts V_U and L accepts V_L, then their cross-product U x L
// accepts \forall v_u \in V_U, v_l \in V_L: v_u \rightarrow v_r. If called
// with a transducer for the first argument (the upper language), it will
// act as if it had already been projected onto its input, and if called with
// a transducer for the second argument (the lower language), it will act as if
// it had already been projected onto its output.
template <class Arc>
void CrossProduct(const Fst<Arc> &ifst1, const Fst<Arc> &ifst2,
                  MutableFst<Arc> *ofst) {
  // Initialize output FST using upper language.
  *ofst = ifst1;
  // Replaces output arcs on the upper language with epsilon, using the output
  // FST for temporary storage.
  OutputEpsilonMapper<Arc> oe_mapper;
  ArcMap(ofst, oe_mapper);
  // Modifies output FST's input symbol table to include
  // Replaces input arcs on the lower language with epsilon, using a temporary
  // mutable FST to store the mapped lower language.
  VectorFst<Arc> tfst(ifst2);
  InputEpsilonMapper<Arc> ie_mapper;
  ArcMap(&tfst, ie_mapper);
  // Concatenates the mapped lower language into the output FST.
  Concat(ofst, tfst);
  // Assigns symbol tables.
  ofst->SetInputSymbols(ifst1.InputSymbols());
  ofst->SetOutputSymbols(ifst2.OutputSymbols());
}

// This function performs a simple space optimization on cross-product FSTs
// constructed from two strings (i.e., FSTs with the properties kAcceptor &
// kString). It first pushes labels towards the initial state, then performs
// epsilon-removal. This will reduce the number of arcs and states by the
// length of the shorter of the two strings in the cross-product; label-pushing
// may also speed up downstream composition.
template <class Arc>
void OptimizeStringCrossProduct(MutableFst<Arc> *fst) {
  std::unique_ptr<MutableFst<Arc>> tfst(fst->Copy());
  // Pushes labels towards the initial state.
  Push<Arc, REWEIGHT_TO_INITIAL>(*tfst, fst, kPushLabels);
  // Removes any trailing epsilon-to-epsilon arcs this produces.
  RmEpsilon(fst);
}

}  // namespace fst

#endif  // CROSSPRODUCT_H_

