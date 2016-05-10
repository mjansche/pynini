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

#include <vector>

#include "merge.h"

DEFINE_bool(fst_relabel_symbol_conflicts, true,
            "Resolve conflicts in FST symbol "
            "tables by relabeling; if "
            "false, operations requiring "
            "relabeling will fail");

namespace fst {

// FIXME(kbg) Consider adding code for the special case where one table is a
// superset of another.
SymbolTable *MergeSymbols(const SymbolTable *syms1, const SymbolTable *syms2,
                          bool *relabel) {
  if (!FLAGS_fst_compat_symbols) {  // Overrides any work here.
    return nullptr;
  } else if (!syms1) {  // If both are null, return nullptr.
    return syms2 ? syms2->Copy() : nullptr;
  } else if (!syms2) {
    return syms1->Copy();
  }
  // If their checksums match, returns the null pointer.
  if (syms1->LabeledCheckSum() == syms2->LabeledCheckSum()) {
    *relabel = false;
    return nullptr;
  }
  // Since we're more faithful to the first table, we'll reuse its name.
  SymbolTable *merged = new SymbolTable(syms1->Name());
  *relabel = false;
  // Adds all symbols from the left table to the merged table.
  for (SymbolTableIterator siter(*syms1); !siter.Done(); siter.Next()) {
    merged->AddSymbol(siter.Symbol(), siter.Value());
    if (!*relabel && internal::conflict(siter, *syms2)) *relabel = true;
  }
  // Adds all non-conflicting symbols from the right table to the merged table,
  // and saves conflicting ones for later.
  std::vector<string> conflicts;
  for (SymbolTableIterator siter(*syms2); !siter.Done(); siter.Next()) {
    if (internal::conflict(siter, *syms1)) {
      conflicts.push_back(siter.Symbol());
      *relabel = true;
    } else {
      merged->AddSymbol(siter.Symbol(), siter.Value());
    }
  }
  // Adds the conflicts to the end of the table.
  for (const string &symbol : conflicts)
    merged->AddSymbol(symbol);
  return merged;
}

}  // namespace fst

