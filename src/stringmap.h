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

#ifndef PYNINI_STRINGMAP_H_
#define PYNINI_STRINGMAP_H_

// This file contains functions for compiling FSTs from pairs of strings
// using a prefix tree.

#include <string>
using std::string;
#include <vector>
#include <utility>

#include <fst/fstlib.h>
#include "optimize.h"
#include "stringcompile.h"
#include "tokentype.h"
#include "prefix_tree.h"

namespace fst {

namespace internal {

// Helper for converting a string into a list of labels the three ways we know
// how.
template <class A>
bool StringToLabels(const string &str, TokenType ttype,
                    std::vector<typename A::Label> *labels,
                    SymbolTable *syms) {
  switch (ttype) {
    case SYMBOL:
      return SymbolStringToLabels<A>(str, *syms, labels);
    case BYTE:
      return BracketedByteStringToLabels<A>(str, labels, syms);
    case UTF8:
      return BracketedUTF8StringToLabels<A>(str, labels, syms);
    break;
  }
  // Should be unreachable.
  FSTERROR() << "StringToLabels: Unknown TokenType";
  return false;
}

}  // namespace internal

template <class A>
bool StringMap(const std::vector<std::pair<string, string>> &pairs,
               TokenType itype, TokenType otype,
               MutableFst<A> *fst,
               const SymbolTable *isyms = nullptr,
               const SymbolTable *osyms = nullptr) {
  using Arc = A;
  using Label = typename Arc::Label;
  using Weight = typename Arc::Weight;
  PrefixTree<A> ptree;
  std::unique_ptr<SymbolTable> new_isyms(internal::GetSymbolTable(itype,
                                                                  isyms));
  std::unique_ptr<SymbolTable> new_osyms(internal::GetSymbolTable(otype,
                                                                  osyms));
  // Converts the string pairs to vectors of arc labels.
  for (const auto &pair : pairs) {
    std::vector<Label> ilabels;
    if (!internal::StringToLabels<A>(pair.first, itype, &ilabels,
                                     new_isyms.get())) {
      return false;
    }
    std::vector<Label> olabels;
    if (!internal::StringToLabels<A>(pair.second, otype, &olabels,
                                     new_osyms.get())) {
      return false;
    }
    ptree.Add(ilabels.begin(), ilabels.end(), olabels.begin(), olabels.end(),
              Weight::One());
  }
  // Compiles the prefix tree into an FST.
  ptree.ToFst(fst);
  OptimizeStringCrossProducts(fst);
  // Sets symbol tables if requested.
  if (new_isyms) fst->SetInputSymbols(new_isyms.get());
  if (new_osyms) fst->SetOutputSymbols(new_osyms.get());
  return true;
}

}  // namespace fst

#endif  // PYNINI_STRINGMAP_H_

