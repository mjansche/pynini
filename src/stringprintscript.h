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

#ifndef PYNINI_STRINGPRINTSCRIPT_H_
#define PYNINI_STRINGPRINTSCRIPT_H_

#include <fst/fst-decl.h>
#include <fst/script/arg-packs.h>
#include <fst/script/fstscript.h>
#include "stringprint.h"

namespace fst {
namespace script {

typedef args::Package<const FstClass &, StringTokenType, string *,
                      const SymbolTable *, bool>
    PrintStringInnerArgs;

typedef args::WithReturnValue<bool, PrintStringInnerArgs> PrintStringArgs;

template <class Arc>
void PrintString(PrintStringArgs *args) {
  const Fst<Arc> &fst = *(args->args.arg1.GetFst<Arc>());
  args->retval = PrintString(fst, args->args.arg2, args->args.arg3,
                             args->args.arg4, args->args.arg5);
}

bool PrintString(const FstClass &fst, StringTokenType ttype, string *str,
                 const SymbolTable *syms = nullptr, bool rm_epsilon = true);

}  // namespace script
}  // namespace fst

#endif  // PYNINI_STRINGPRINTSCRIPT_H_

