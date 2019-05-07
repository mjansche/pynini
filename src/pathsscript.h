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

#ifndef PYNINI_PATHSSCRIPT_H_
#define PYNINI_PATHSSCRIPT_H_

#include <algorithm>
#include <vector>

#include <fst/string.h>
#include <fst/script/arg-packs.h>
#include <fst/script/fstscript.h>
#include "paths.h"

namespace fst {
namespace script {

// Helpers.

template <class Arc>
inline StringTokenType GetStringPrinterTokenType(StringTokenType type) {
  return static_cast<StringTokenType>(type);
}

// Virtual interface implemented by each concrete StatesImpl<F>.
class StringPathIteratorImplBase {
 public:
  virtual bool Error() const = 0;
  virtual void IString(string *result) const = 0;
  virtual string IString() const = 0;
  virtual void OString(string *result) const = 0;
  virtual string OString() const = 0;
  virtual WeightClass Weight() const = 0;
  virtual bool Done() const = 0;
  virtual void Reset() = 0;
  virtual void Next() = 0;
  virtual ~StringPathIteratorImplBase() {}
};

// Templated implementation.
template <class Arc>
class StringPathIteratorImpl : public StringPathIteratorImplBase {
 public:
  using Label = typename Arc::Label;

  explicit StringPathIteratorImpl(const Fst<Arc> &fst,
                                  StringTokenType itype = BYTE,
                                  StringTokenType otype = BYTE,
                                  const SymbolTable *isyms = nullptr,
                                  const SymbolTable *osyms = nullptr)
      : impl_(new StringPathIterator<Arc>(fst, itype, otype, isyms, osyms)) {}

  bool Error() const override { return impl_->Error(); }

  void IString(string *result) const override { impl_->IString(result); }

  string IString() const override { return impl_->IString(); }

  void OString(string *result) const override { impl_->OString(result); }

  string OString() const override { return impl_->OString(); }

  WeightClass Weight() const override { return WeightClass(impl_->Weight()); }

  void Reset() override { impl_->Reset(); }

  void Next() override { impl_->Next(); }

  bool Done() const override { return impl_->Done(); }

 private:
  std::unique_ptr<StringPathIterator<Arc>> impl_;
};

class StringPathIteratorClass;

using InitStringPathIteratorClassArgs =
    std::tuple<const FstClass &, StringTokenType, StringTokenType,
               const SymbolTable *, const SymbolTable *,
               StringPathIteratorClass *>;

// Untemplated user-facing class holding templated pimpl.
class StringPathIteratorClass {
 public:
  explicit StringPathIteratorClass(const FstClass &fst,
                                   StringTokenType itype = BYTE,
                                   StringTokenType otype = BYTE,
                                   const SymbolTable *isyms = nullptr,
                                   const SymbolTable *osyms = nullptr);

  // Same as above, but applies the same string token type and symbol table
  // to both tapes.
  StringPathIteratorClass(const FstClass &fst, StringTokenType type,
                          const SymbolTable *syms = nullptr)
      : StringPathIteratorClass(fst, type, type, syms, syms) {}

  bool Error() const { return impl_->Error(); }

  void IString(string *result) const { impl_->IString(result); }

  string IString() const { return impl_->IString(); }

  void OString(string *result) const { impl_->OString(result); }

  string OString() const { return impl_->OString(); }

  WeightClass Weight() const { return WeightClass(impl_->Weight()); }

  void Reset() { impl_->Reset(); }

  void Next() { impl_->Next(); }

  bool Done() const { return impl_->Done(); }

  template <class Arc>
  friend void InitStringPathIteratorClass(
      InitStringPathIteratorClassArgs *args);

 private:
  std::unique_ptr<StringPathIteratorImplBase> impl_;
};

template <class Arc>
void InitStringPathIteratorClass(InitStringPathIteratorClassArgs *args) {
  const Fst<Arc> &fst = *(std::get<0>(*args).GetFst<Arc>());
  std::get<5>(*args)->impl_.reset(new StringPathIteratorImpl<Arc>(
      fst, std::get<1>(*args), std::get<2>(*args), std::get<3>(*args),
      std::get<4>(*args)));
}

}  // namespace script
}  // namespace fst

#endif  // PYNINI_PATHSSCRIPT_H_

