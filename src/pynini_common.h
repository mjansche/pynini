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

#ifndef PYNINI_COMMON_H_
#define PYNINI_COMMON_H_

// This header defines internal namespace utility functions.

#include <fst/fstlib.h>
#include "merge.h"

namespace fst {
namespace internal {

// Helpers for preparing FST symbol tables for context-dependent rewrite
// rule application and replacement.

// The caller owns the returned symbol table and should delete it (or capture
// it into a unique_ptr) to prevent leaks.
template <class Arc>
SymbolTable *PrepareInputSymbols(SymbolTable *syms, MutableFst<Arc> *fst) {
  bool relabel;
  SymbolTable *new_syms = MergeSymbols(syms, fst->InputSymbols(), &relabel);
  if (!new_syms) return syms ? syms->Copy() : nullptr;
  if (relabel) {  // Relabeling necessary.
    if (!FLAGS_fst_relabel_symbol_conflicts) {
      LOG(WARNING) << "PrepareInputSymbols: Unable to resolve symbol table "
                   << "conflict without relabeling";
    } else {
      Relabel(fst, new_syms, nullptr);
    }
  }
  fst->SetInputSymbols(nullptr);
  return new_syms;
}

// The caller owns the returned symbol table and should delete it (or capture
// it into a unique_ptr) to prevent leaks.
template <class Arc>
SymbolTable *PrepareOutputSymbols(SymbolTable *syms, MutableFst<Arc> *fst) {
  bool relabel;
  SymbolTable *new_syms = MergeSymbols(syms, fst->OutputSymbols(), &relabel);
  if (!new_syms) return syms ? syms->Copy() : nullptr;
  if (relabel) {  // Relabeling necessary.
    if (!FLAGS_fst_relabel_symbol_conflicts) {
      LOG(WARNING) << "PrepareOutputSymbols: Unable to resolve symbol table "
                   << "conflict without relabeling";
    } else {
      Relabel(fst, nullptr, new_syms);
    }
  }
  fst->SetOutputSymbols(nullptr);
  return new_syms;
}

}  // namespace internal
}  // namespace fst

#endif  // PYNINI_COMMON_H_

