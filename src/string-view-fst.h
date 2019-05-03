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

#ifndef PYNINI_STRING_VIEW_FST_H_
#define PYNINI_STRING_VIEW_FST_H_

#include "base/integral_types.h"
#include <fst/fst.h>
#include <fst/string.h>
#include <re2/stringpiece.h>
#include "third_party/icu/include/unicode/utf8.h"

// An Arc "iterator" that contains a single byte or Unicode code point.
namespace fst {

// A viewer returns a single arc given the byte offset. If the byte offset
// is "invalid" (i.e., a non-initial byte in a multibyte code point) then the
// arc labels returned are negative and the destination state ID is simply
// the next byte. Otherwise, the arc label returned is non-negative and the
// destination state ID is the next "valid" state.
template <class Arc>
struct ByteViewer {
  using Weight = typename Arc::Weight;

  using size_type = string::size_type;

  Arc operator()(string view, size_type byte_offset) const {
    const char ch = view[byte_offset];
    return Arc(ch, ch, Weight::One(), byte_offset + 1);
  }

  static constexpr StringTokenType TokenType() { return StringTokenType::BYTE; }
};

template <class Arc>
struct UTF8Viewer {
  using Label = typename Arc::Label;
  using Weight = typename Arc::Weight;

  using size_type = string::size_type;

  static_assert(sizeof(Label) >= 4,
                "Unicode codepoints require at least 24 bits");

  Arc operator()(string view, size_type byte_offset) const {
    UChar32 ch;
    int32 offset = byte_offset;
    U8_NEXT(view.begin(), offset, view.size(), ch);
    return Arc(ch, ch, Weight::One(), offset);
  }

  static constexpr StringTokenType TokenType() { return StringTokenType::UTF8; }
};

// Forward declaration.
template <class Arc, class View>
class StringViewFst;

template <class Arc, class Viewer>
class ArcIterator<StringViewFst<Arc, Viewer>> : public ArcIteratorBase<Arc> {
 public:
  using StateId = typename Arc::StateId;

  explicit ArcIterator(const StringViewFst<Arc, Viewer> &fst, StateId state) :
      arc_(viewer_(fst.GetImpl()->view(), state)),
      done_(!fst.NumArcs(state) || arc_.ilabel < 0) {}

  bool Done() const final { return done_; }

  const Arc &Value() const final { return arc_; }

  void Next() final { done_ = true; }

  void Seek(size_t s) final { done_ = (s == 0); }

  void Reset() final { Seek(0); }

  void SetFlags(uint32, uint32) final {}

  constexpr uint32 Flags() const final { return kArcValueFlags; }

  size_t Position() const final { return done_ ? 0 : 1; }

 private:
  Viewer viewer_;  // Stateless.
  const Arc arc_;
  bool done_;
};

namespace internal {

template <class A, class Viewer>
class StringViewFstImpl : public FstImpl<A> {
 public:
  using Arc = A;
  using StateId = typename Arc::StateId;
  using Weight = typename Arc::Weight;

  using FstImpl<Arc>::SetInputSymbols;
  using FstImpl<Arc>::SetOutputSymbols;
  using FstImpl<Arc>::SetType;
  using FstImpl<Arc>::SetProperties;
  using FstImpl<Arc>::Properties;

  explicit StringViewFstImpl(string view) : view_(view) {
    SetType("StringViewFst");
    SetProperties(kStaticProperties);
  }

  constexpr StateId Start() const { return 0; }

  Weight Final(StateId s) const {
    return IsFinal(s) ? Weight::One() : Weight::Zero();
  }

  StateId NumStates() const { return view_.size() + 1; }

  size_t NumArcs(StateId s) const { return !IsFinal(s); }

  constexpr size_t NumInputEpsilons(StateId) const { return 0; }

  constexpr size_t NumOutputEpsilons(StateId) const { return 0; }

  void InitStateIterator(StateIteratorData<Arc> *data) const {
    data->base = nullptr;
    data->nstates = NumStates();
  }

  // Returns the string view itself; used by pseudo-friend classes.
  string view() const { return view_; }

 private:
  static constexpr uint64 kStaticProperties = kAcceptor | kExpanded |
      kIDeterministic | kODeterministic | kILabelSorted | kOLabelSorted |
      kUnweighted | kUnweightedCycles | kAcyclic | kInitialAcyclic |
      kTopSorted | (Viewer::TokenType() == StringTokenType::BYTE ? kString : 0);

  bool IsFinal(StateId s) const { return s == view_.size(); }

  Viewer viewer_;  // Stateless.
  string view_;
};

template <class A, class Viewer>
constexpr uint64 StringViewFstImpl<A, Viewer>::kStaticProperties;

}  // namespace internal

// A stringview left-to-right FSA that creates an on-the-fly acceptor for a
// byte buffer passed as a string_view. The FSA does not allocate, own, copy
// or store the document that it processes. The FSA is not an ExpandedFst, as
// that may require a pass over the entire string to count the number of
// codepoints.
//
// The state number is the byte offset into the string, and this means that
// the states are not guaranteed to be fully connected when multibyte sequences
// are present. Byte offsets that point into multibyte sequences are simply
// unreachable states with no arcs.
//
// The string viewed is expected not to mutate during the lifetime, but the
// StringViewFst is essentially stateless except for the arc currently being
// viewed.
//
// UTF8View provides a UTF-32 codepoint per arc, and ByteView provides a byte
// per arc.
template <class A, class Viewer>
class StringViewFst
    : public ImplToExpandedFst<internal::StringViewFstImpl<A, Viewer>> {
 public:
  using Arc = A;
  using StateId = typename Arc::StateId;
  using Weight = typename Arc::Weight;
  using Impl = internal::StringViewFstImpl<Arc, Viewer>;

  friend class ArcIterator<StringViewFst<Arc, Viewer>>;

  template <class F, class G>
  friend void Cast(const F &, G *);

  explicit StringViewFst(string view)
      : ImplToExpandedFst<Impl>(std::make_shared<Impl>(view)) {}

  StringViewFst(const StringViewFst<Arc, Viewer> &fst, bool safe = false)
      : ImplToExpandedFst<Impl>(std::make_shared<Impl>(
            fst.GetImpl()->view())) {}

  // Gets a copy of this StringViewFst. See Fst<>::Copy() for further doc.
  StringViewFst *Copy(bool safe = false) const override {
    return new StringViewFst<Arc, Viewer>(*this, safe);
  }

  void InitStateIterator(StateIteratorData<Arc> *data) const override {
    GetImpl()->InitStateIterator(data);
  }

  void InitArcIterator(StateId s, ArcIteratorData<Arc> *data) const override {
    data->base = new ArcIterator<StringViewFst<Arc, Viewer>>(*this, s);
  }

 private:
  using ImplToFst<Impl, ExpandedFst<Arc>>::GetImpl;

  StringViewFst &operator=(const StringViewFst &) = delete;
};

// Useful aliases when using StdArc.
using StdByteStringViewFst = StringViewFst<StdArc, ByteViewer<StdArc>>;
using StdUTF8StringViewFst = StringViewFst<StdArc, UTF8Viewer<StdArc>>;

}  // namespace fst

#endif  // PYNINI_STRING_VIEW_FST_H_

