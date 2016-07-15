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
inline typename StringPrinter<Arc>::TokenType GetStringPrinterTokenType(
    StringTokenType type) {
  return static_cast<typename StringPrinter<Arc>::TokenType>(type);
}

// Virtual interface implemented by each concrete StatesImpl<F>.
class StringPathsImplBase {
 public:
  virtual bool Error() const = 0;
  virtual string IString() const = 0;
  virtual string OString() const = 0;
  virtual WeightClass Weight() const = 0;
  virtual std::vector<int64> ILabels(bool rm_epsilon = true) const = 0;
  virtual std::vector<int64> OLabels(bool rm_epsilon = true) const = 0;
  virtual bool Done() const = 0;
  virtual void Reset() = 0;
  virtual void Next() = 0;
  virtual ~StringPathsImplBase() {}
};

// Templated implementation.
template <class Arc>
class StringPathsImpl : public StringPathsImplBase {
 public:
  StringPathsImpl(const Fst<Arc> &fst,
                  typename StringPrinter<Arc>::TokenType token_type,
                  const SymbolTable *isyms, const SymbolTable *osyms)
      : impl_(new StringPaths<Arc>(fst, token_type, isyms, osyms)) {}

  bool Error() const override { return impl_->Error(); }

  string IString() const override { return impl_->IString(); }

  string OString() const override { return impl_->OString(); }

  WeightClass Weight() const override { return WeightClass(impl_->Weight()); }

  std::vector<int64> ILabels(bool rm_epsilon = true) const override {
    auto labels = impl_->ILabels(rm_epsilon);
    std::vector<int64> untyped_labels(labels.size());
    std::copy(labels.begin(), labels.end(), untyped_labels.begin());
    return untyped_labels;
  }

  std::vector<int64> OLabels(bool rm_epsilon = true) const override {
    auto labels = impl_->OLabels(rm_epsilon);
    std::vector<int64> untyped_labels(labels.begin(), labels.end());
    return untyped_labels;
  }

  void Reset() override { impl_->Reset(); }

  void Next() override { impl_->Next(); }

  bool Done() const override { return impl_->Done(); }

 private:
  std::unique_ptr<StringPaths<Arc>> impl_;
};

class StringPathsClass;

typedef args::Package<const FstClass &, StringTokenType, const SymbolTable *,
                      const SymbolTable *, StringPathsClass *>
    InitStringPathsClassArgs;

// Untemplated user-facing class holding templated pimpl.
class StringPathsClass {
 public:
  StringPathsClass(const FstClass &fst, StringTokenType token_type,
                   const SymbolTable *isyms, const SymbolTable *osyms);

  bool Error() const { return impl_->Error(); }

  string IString() const { return impl_->IString(); }

  string OString() const { return impl_->OString(); }

  WeightClass Weight() const { return WeightClass(impl_->Weight()); }

  std::vector<int64> ILabels(bool rm_epsilon = true) const {
    return impl_->ILabels(rm_epsilon);
  }

  std::vector<int64> OLabels(bool rm_epsilon = true) const {
    return impl_->OLabels(rm_epsilon);
  }

  void Reset() { impl_->Reset(); }

  void Next() { impl_->Next(); }

  bool Done() const { return impl_->Done(); }

  template <class Arc>
  friend void InitStringPathsClass(InitStringPathsClassArgs *args);

 private:
  std::unique_ptr<StringPathsImplBase> impl_;
};

template <class Arc>
void InitStringPathsClass(InitStringPathsClassArgs *args) {
  const Fst<Arc> &fst = *(args->arg1.GetFst<Arc>());
  args->arg5->impl_.reset(new StringPathsImpl<Arc>(
      fst, GetStringPrinterTokenType<Arc>(args->arg2), args->arg3, args->arg4));
}

}  // namespace script
}  // namespace fst

#endif  // PYNINI_PATHSSCRIPT_H_

