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

#ifndef PATHS_H_
#define PATHS_H_

// An iterative definition of all paths of an acyclic automaton.
//
// For a given path, one can ask for the input label sequence, output label
// sequence, and total weight.
//
// Uses a symbol-table-agnostic iterator for the inner algorithm and a wrapper
// that knows about symbol tables and string token types, and also checks the
// FST's properties (e.g. to make sure that it is acyclic).

#include <string>
using std::string;
#include <vector>

#include <fst/compat.h>
#include <fst/fstlib.h>
#include "join.h"

namespace fst {

namespace internal {
// An iterator to provide a succession of paths from an acyclic automaton.
// Calling Next() gets the next path. Done() returns true if all the paths are
// done. Accessible path values are IValue() --- the sequence of input labels,
// OValue() --- for output labels, and Weight(). Note that PathIterator is
// agnostic about symbol tables.
template <typename Arc>
class PathIterator {
  using Label = typename Arc::Label;
  using StateId = typename Arc::StateId;
  using ArcWeight = typename Arc::Weight;

 public:
  explicit PathIterator(const Fst<Arc> &fst);

  const std::vector<Label> &IValue() const { return path_ilabels_; }

  const std::vector<Label> &OValue() const { return path_olabels_; }

  ArcWeight Weight() const {
    ArcWeight weight = ArcWeight::One();
    for (const auto w : path_weights_)
      weight = Times(weight, w);
    return weight;
  }

  void Reset();

  void Next();

  bool Done() const { return path_states_.empty(); }

 private:
  const Fst<Arc> &fst_;
  // Vector of states visited on this path.
  std::vector<StateId> path_states_;
  // Vector of input labels.
  std::vector<Label> path_ilabels_;
  // Vector of output labels.
  std::vector<Label> path_olabels_;
  // Vector of weights.
  std::vector<ArcWeight> path_weights_;
  // Vector of offsets for each arc iterator for each state, so that we can
  // remember where we left off. Note that -2 and -1 here have special
  // meanings, on which see below.
  std::vector<int32> arc_iterator_offsets_;
  DISALLOW_COPY_AND_ASSIGN(PathIterator);
};

template <typename Arc>
PathIterator<Arc>::PathIterator(const Fst<Arc> &fst) :  fst_(fst) {
  Reset();
}

template <typename Arc>
void PathIterator<Arc>::Reset() {
  StateId start = fst_.Start();
  if (start == kNoStateId || CountStates(fst_) < 2) {
    return;
  }
  // Seed the search with the start state.
  path_states_.push_back(start);
  path_ilabels_.push_back(0);
  path_olabels_.push_back(0);
  ArcWeight weight = fst_.Final(start);
  path_weights_.push_back(weight);
  // If the initial state is also a final state, then Next() has immediate work
  // to do, so we indicate that with -2. Otherwise we set it to -1, which
  // means "I haven't started the arc iterator at this state yet."
  arc_iterator_offsets_.push_back(weight == ArcWeight::Zero() ? -1 : -2);
  Next();
}

template <typename Arc>
void PathIterator<Arc>::Next() {
  if (Done()) return;
  // -2 indicates a final initial state.
  if (arc_iterator_offsets_.back() == -2) {
    arc_iterator_offsets_.pop_back();
    arc_iterator_offsets_.push_back(-1);
    return;
  }
  // At the current state (the back of path_states_) we increment the
  // arc_iterator offset (meaning that if it's -1, then we set it to 0 and
  // therefore start reading).
  typename Arc::StateId nextstate;
  while (!Done()) {
    int32 offset = arc_iterator_offsets_.back() + 1;
    arc_iterator_offsets_.pop_back();
    arc_iterator_offsets_.push_back(offset);
    ArcIterator<Fst<Arc>> aiter(fst_, path_states_.back());
    aiter.Seek(offset);
    // If the arc iterator is done, then we are done at this state, and we move
    // back.
    if (aiter.Done()) {
      path_states_.pop_back();
      path_ilabels_.pop_back();
      path_olabels_.pop_back();
      path_weights_.pop_back();
      arc_iterator_offsets_.pop_back();
      // Otherwise we proceed moving to the current arc's next state, then break
      // out of this loop and attempt to move forward.
    } else {
      const Arc &arc = aiter.Value();
      path_ilabels_.pop_back();
      path_ilabels_.push_back(arc.ilabel);
      path_olabels_.pop_back();
      path_olabels_.push_back(arc.olabel);
      path_weights_.pop_back();
      path_weights_.push_back(arc.weight);
      nextstate = arc.nextstate;
      break;
    }
  }
  if (Done()) return;
  // Now we proceed forward until we hit a final state.
  while (nextstate != kNoStateId) {
    path_states_.push_back(nextstate);
    ArcWeight weight = fst_.Final(nextstate);
    if (weight == ArcWeight::Zero()) {
      ArcIterator<Fst<Arc>> aiter(fst_, nextstate);
      if (aiter.Done()) {
        // We reached a non-final state with no exiting arcs. Pop it. This
        // shouldn't happen unless someone passes an unconnected machine.
        path_states_.pop_back();
        return;
      } else {
        const Arc &arc = aiter.Value();
        path_ilabels_.push_back(arc.ilabel);
        path_olabels_.push_back(arc.olabel);
        path_weights_.push_back(arc.weight);
        arc_iterator_offsets_.push_back(0);
        nextstate = arc.nextstate;
      }
    } else {
      // If we are at a final state, then we mimic an epsilon arc to a
      // "superfinal" state.
      path_ilabels_.push_back(0);
      path_olabels_.push_back(0);
      path_weights_.push_back(weight);
      arc_iterator_offsets_.push_back(-1);
      return;
    }
  }
}

}  // namespace internal

// StringPaths is a wrapper for PathIterator that handles symbol tables, and
// the conversion of the label sequences to strings.
template <typename Arc>
class StringPaths {
  using Label = typename Arc::Label;
  using StateId = typename Arc::StateId;
  using TokenType = typename StringPrinter<Arc>::TokenType;
  using ArcWeight = typename Arc::Weight;

 public:
  StringPaths(const Fst<Arc> &fst, TokenType token_type,
              const SymbolTable *isyms, const SymbolTable *osyms);

  bool Done() { return iter_->Done(); }

  void Reset() { iter_->Reset(); }

  void Next() { iter_->Next(); }

  ArcWeight Weight() { return iter_->Weight(); }

  string IString() { return String(false); }

  string OString() { return String(true); }

  bool Error() const { return error_; }

 private:
  string String(bool output);
  std::unique_ptr<internal::PathIterator<Arc>> iter_;
  const SymbolTable *isyms_;
  const SymbolTable *osyms_;
  TokenType token_type_;
  bool acceptor_;
  bool error_;
};

template <typename Arc>
StringPaths<Arc>::StringPaths(const Fst<Arc> &fst, TokenType token_type,
                              const SymbolTable *isyms,
                              const SymbolTable *osyms) :
    isyms_(isyms), osyms_(osyms), token_type_(token_type), error_(false) {
  if (!fst.Properties(kAcyclic, true)) {
    error_ = true;
    FSTERROR() << "Fst passed to StringPaths must be acyclic";
    return;
  }
  if (token_type_ == TokenType::SYMBOL) {
    // If the FST has its own symbol tables, we use those unless isyms or osyms
    // are specified.
    if (!isyms_ && fst.InputSymbols()) isyms_ = fst.InputSymbols();
    if (!osyms_ && fst.OutputSymbols()) isyms_ = fst.OutputSymbols();
  }
  if (fst.Properties(kAcceptor, true)) {
    acceptor_ = true;
  } else {
    acceptor_ = false;
  }
  iter_.reset(new internal::PathIterator<Arc>(fst));
}

template <typename Arc>
string StringPaths<Arc>::String(bool output) {
  // The labels of interest are the output labels if this is an acceptor, or if
  // one specifies "output", otherwise the input labels.
  std::vector<Label> labels =
      (output || acceptor_) ? iter_->OValue() : iter_->IValue();
  // The symbol table of interest is the output symbol table if this is an
  // acceptor, or if one specifies "output", otherwise the input symbol table.
  const SymbolTable *syms = (output || acceptor_) ? osyms_ : isyms_;
  // Removes epsilons from the label sequence.
  // TODO(rws): Revisit if there are some circumstances in which we actually
  // want these.
  std::vector<Label> epsilon_free_labels;
  string result;
  for (auto label : labels) {
    if (label)
      epsilon_free_labels.push_back(label);
  }
  if (token_type_ == TokenType::BYTE) {
    for (auto label : epsilon_free_labels) {
      result += static_cast<char>(label);
    }
  } else if (token_type_ == TokenType::UTF8) {
    LabelsToUTF8String(epsilon_free_labels, &result);
  } else if (syms) {
    std::vector<string> label_vector;
    for (auto label : epsilon_free_labels) {
      label_vector.push_back(syms->Find(label));
    }
    result = strings::Join(label_vector, " ");
  } else {
    error_ = true;
    FSTERROR() << "StringPaths::String(): "
               << "Token type is neither UTF8 nor BYTE "
               << "but no symbol table was provided";
    result = "";
  }
  return result;
}

}  // namespace fst

#endif  // PATHS_H_

