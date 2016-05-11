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

#ifndef PYNINI_STRINGFILESCRIPT_H_
#define PYNINI_STRINGFILESCRIPT_H_

#include <string>
using std::string;
#include <vector>
#include <utility>

#include <fst/fstlib.h>
#include <fst/script/arg-packs.h>
#include <fst/script/fstscript.h>
#include "stringfile.h"

namespace fst {
namespace script {

typedef args::Package<const string &, TokenType, TokenType, MutableFstClass *,
                      const SymbolTable *, const SymbolTable *>
                      StringFileInnerArgs;

typedef args::WithReturnValue<bool, StringFileInnerArgs> StringFileArgs;

template <class A>
void StringFile(StringFileArgs *args) {
  using Arc = A;
  MutableFst<Arc> *fst = args->args.arg4->GetMutableFst<Arc>();
  args->retval = StringFile(args->args.arg1, args->args.arg2, args->args.arg3,
                            fst, args->args.arg5, args->args.arg6);
}

bool StringFile(const string &fname, TokenType itype, TokenType otype,
                MutableFstClass *fst,
                const SymbolTable *isyms = nullptr,
                const SymbolTable *osyms = nullptr);

}  // namespace script
}  // namespace fst

#endif  // PYNINI_STRINGFILESCRIPT_H_

