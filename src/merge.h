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

#ifndef PYNINI_MERGE_H_
#define PYNINI_MERGE_H_

#include <memory>

#include <fst/fstlib.h>

// This header contains operations for merging FST symbol tables and resolving
// labeling conflicts (i.e. two tables assigning different indices to the
// same symbol). The lowest-level function is MergeSymbols, which creates a
// merged table and signals when relabeling is necessary due to labeling
// conflicts. The remaining functions target pairs of FSTs rather than tables,
// assigning the merged tables back to the argument FSTs when needed. The flag
// --fst_relabel_symbol_conflicts is used to determine whether merger will
// happen when it is required, or whether the operation will simply fail.

DECLARE_bool(fst_relabel_symbol_conflicts);

namespace fst {
namespace internal {

// Returns a symbol table merging the two argument symbol tables. Symbol/key
// pairs from the first table are never modified, but pairs from the second
// may be in the case of conflicts; if so, the boolean argument is set to
// true. As symbol tables are nullable, they are passed as const pointers
// rather than via const references.
SymbolTable *MergeSymbols(const SymbolTable *syms1, const SymbolTable *syms2,
                          bool *relabel);

// Specific implementations, not intended for client use.

template <class Arc>
bool MergeInputSymbols(MutableFst<Arc> *fst1, MutableFst<Arc> *fst2) {
  bool relabel = false;
  std::unique_ptr<SymbolTable> new_syms(
      MergeSymbols(fst1->InputSymbols(), fst2->InputSymbols(), &relabel));
  if (!new_syms) return true;  // No mutation necessary.
  if (relabel) {
    if (FLAGS_fst_relabel_symbol_conflicts) {
      Relabel(fst2, new_syms.get(), nullptr);
    } else {
      LOG(WARNING) << "MergeInputSymbols: Unable to resolve "
                   << "symbol table conflict without relabeling";
      return false;
    }
  }
  fst1->SetInputSymbols(new_syms.get());
  fst2->SetInputSymbols(new_syms.get());
  return true;
}

template <class Arc>
bool MergeOutputSymbols(MutableFst<Arc> *fst1, MutableFst<Arc> *fst2) {
  bool relabel = false;
  std::unique_ptr<SymbolTable> new_syms(
      MergeSymbols(fst1->OutputSymbols(), fst2->OutputSymbols(), &relabel));
  if (!new_syms) return true;  // No mutation necessary.
  if (relabel) {
    if (FLAGS_fst_relabel_symbol_conflicts) {
      Relabel(fst2, nullptr, new_syms.get());
    } else {
      LOG(WARNING) << "MergeOutputSymbols: Unable to resolve "
                   << "symbol table conflict without relabeling";
      return false;
    }
  }
  fst1->SetOutputSymbols(new_syms.get());
  fst2->SetOutputSymbols(new_syms.get());
  return true;
}

template <class Arc>
bool MergeLeftOutputAndRightInputSymbols(MutableFst<Arc> *fst1,
                                         MutableFst<Arc> *fst2) {
  bool relabel = false;
  std::unique_ptr<SymbolTable> new_syms(
      MergeSymbols(fst1->OutputSymbols(), fst2->InputSymbols(), &relabel));
  if (!new_syms) return true;  // No mutation necessary.
  if (relabel) {
    if (FLAGS_fst_relabel_symbol_conflicts) {
      Relabel(fst2, new_syms.get(), nullptr);
    } else {
      LOG(WARNING) << "MergeLeftOutputAndRightInputSymbols: "
                   << "Unable to resolve symbol table conflict without "
                   << "relabeling";
      return false;
    }
  }
  fst1->SetOutputSymbols(new_syms.get());
  fst2->SetInputSymbols(new_syms.get());
  return true;
}

}  // namespace internal

// These are encoded so that they be ORed together.
enum MergeSymbolsType {
  // 0 not defined, but would mean "do nothing".
  MERGE_INPUT_SYMBOLS = 1 << 0,
  MERGE_OUTPUT_SYMBOLS = 1 << 1,
  // = MERGE_INPUT_SYMBOLS | MERGE_OUTPUT_SYMBOLS; used for concat and union.
  MERGE_INPUT_AND_OUTPUT_SYMBOLS = (1 << 0) | (1 << 1),
  // Used for composition and intersection.
  MERGE_LEFT_OUTPUT_AND_RIGHT_INPUT_SYMBOLS = (1 << 2),
};

// This is the most generic merging function, and it is the one most clients
// should use. If the tables have symbol conflicts, the left FST is relabeled
// so long as the command-line flag --fst_relabel_symbol_table_conflicts is
// set to true.

template <class Arc>
bool MergeSymbols(MutableFst<Arc> *fst1, MutableFst<Arc> *fst2,
                  MergeSymbolsType mst) {
  bool success = true;
  if ((mst & MERGE_INPUT_SYMBOLS) == MERGE_INPUT_SYMBOLS) {
    success &= internal::MergeInputSymbols(fst1, fst2);
  }
  if ((mst & MERGE_OUTPUT_SYMBOLS) == MERGE_OUTPUT_SYMBOLS) {
    success &= internal::MergeOutputSymbols(fst1, fst2);
  }
  if ((mst & MERGE_LEFT_OUTPUT_AND_RIGHT_INPUT_SYMBOLS) ==
      MERGE_LEFT_OUTPUT_AND_RIGHT_INPUT_SYMBOLS) {
    success &= internal::MergeLeftOutputAndRightInputSymbols(fst1, fst2);
  }
  return success;
}

}  // namespace fst

#endif  // PYNINI_MERGE_H_

