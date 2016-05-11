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

#ifndef PYNINI_STRINGFILE_H_
#define PYNINI_STRINGFILE_H_

// This file contains functions for compiling FSTs from pairs of strings
// similar to StringMap, except the pairs are read from a TSV file on disk.

#include <string>
using std::string;
#include <vector>


#include <fst/fstlib.h>
#include "optimize.h"
#include "stringcompile.h"
#include "stringmap.h"
#include "tokentype.h"
#include "prefix_tree.h"
#include "gtl.h"

namespace fst {

template <class A>
bool StringFile(const string &fname, TokenType itype, TokenType otype,
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
  File *fp;
  auto status = file::Open(fname, "r", &fp, file::Defaults());
  if (!status.ok()) {
    LOG(ERROR) << "Can't open file " << fname;
    return false;
  }
  string line;
  size_t linenum = 0;
  for (InputBuffer ibuf(fp); ibuf.ReadLine(&line); ) {
    ++linenum;
    vector<string> words = strings::Split(line, "\t");
    size_t size = words.size();
    if (!size) {
      continue;
    } else if (size > 2) {
      LOG(WARNING) << "StringFile: Skipping ill-formed line " << linenum
                   << "in file " << fname;
      continue;
    }
    std::vector<Label> ilabels;
    if (!internal::StringToLabels<A>(words[0], itype, &ilabels,
                                     new_isyms.get())) {
      return false;
    }
    if (size == 1) {
      ptree.Add(ilabels.begin(), ilabels.end(), ilabels.begin(), ilabels.end(),
                Weight::One());
    } else {  // size == 2
      std::vector<Label> olabels;
      if (!internal::StringToLabels<A>(words[1], otype, &olabels,
                                       new_osyms.get())) {
        return false;
      }
      ptree.Add(ilabels.begin(), ilabels.end(), olabels.begin(), olabels.end(),
                Weight::One());
    }
  }
  // Compiles the prefix tree into an FST>
  ptree.ToFst(fst);
  OptimizeStringCrossProducts(fst);
  // Sets symbol tables if requested.
  if (new_isyms) fst->SetInputSymbols(new_isyms.get());
  if (new_osyms) fst->SetOutputSymbols(new_osyms.get());
  return true;
}

}  // namespace fst

#endif  // PYNINI_STRINGFILE_H_

