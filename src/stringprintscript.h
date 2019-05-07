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

#ifndef PYNINI_STRINGPRINTSCRIPT_H_
#define PYNINI_STRINGPRINTSCRIPT_H_

#include <fst/fst-decl.h>
#include <fst/script/arg-packs.h>
#include <fst/script/fstscript.h>
#include "stringprint.h"

namespace fst {
namespace script {

using PrintStringInnerArgs = std::tuple<const FstClass &, string *,
                                        StringTokenType, const SymbolTable *>;

using PrintStringArgs = WithReturnValue<bool, PrintStringInnerArgs>;

template <class Arc>
void PrintString(PrintStringArgs *args) {
  const Fst<Arc> &fst = *(std::get<0>(args->args).GetFst<Arc>());
  args->retval = PrintString(fst, std::get<1>(args->args),
                             std::get<2>(args->args), std::get<3>(args->args));
}

bool PrintString(const FstClass &fst, string *str, StringTokenType ttype = BYTE,
                 const SymbolTable *syms = nullptr);

}  // namespace script
}  // namespace fst

#endif  // PYNINI_STRINGPRINTSCRIPT_H_

