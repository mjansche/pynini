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

#ifndef PYNINI_STRINGCOMPILESCRIPT_H_
#define PYNINI_STRINGCOMPILESCRIPT_H_

#include <fst/script/arg-packs.h>
#include <fst/script/fst-class.h>
#include "stringcompile.h"

namespace fst {
namespace script {

typedef args::Package<const string &, const WeightClass &, StringTokenType,
                      MutableFstClass *, const SymbolTable *>
    CompileStringInnerArgs;

typedef args::WithReturnValue<bool, CompileStringInnerArgs> CompileStringArgs;

template <class Arc>
void CompileString(CompileStringArgs *args) {
  typename Arc::Weight weight =
      *(args->args.arg2.GetWeight<typename Arc::Weight>());
  MutableFst<Arc> *fst = args->args.arg4->GetMutableFst<Arc>();
  args->retval = CompileString(args->args.arg1, weight, args->args.arg3, fst,
                               args->args.arg5);
}

bool CompileString(const string &str, const WeightClass &wc,
                   StringTokenType ttype, MutableFstClass *fst,
                   const SymbolTable *syms = nullptr);

}  // namespace script
}  // namespace fst

#endif  // PYNINI_STRINGCOMPILESCRIPT_H_

