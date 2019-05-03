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

#ifndef PYNINI_STRINGMAPSCRIPT_H_
#define PYNINI_STRINGMAPSCRIPT_H_

#include <string>
using std::string;
#include <utility>
#include <vector>

#include <fst/fstlib.h>
#include <fst/script/arg-packs.h>
#include <fst/script/fstscript.h>
#include "stringmap.h"

namespace fst {
namespace script {

using StringFileInnerArgs =
    args::Package<const string &, StringTokenType, StringTokenType,
                  MutableFstClass *, const SymbolTable *, const SymbolTable *>;

using StringFileArgs = args::WithReturnValue<bool, StringFileInnerArgs>;

template <class Arc>
void StringFile(StringFileArgs *args) {
  MutableFst<Arc> *fst = args->args.arg4->GetMutableFst<Arc>();
  args->retval =
      CompileStringFile(args->args.arg1, args->args.arg2, args->args.arg3, fst,
                        args->args.arg5, args->args.arg6);
}

bool StringFile(const string &fname, StringTokenType itype,
                StringTokenType otype, MutableFstClass *fst,
                const SymbolTable *isyms = nullptr,
                const SymbolTable *osyms = nullptr);

using StringMapInnerArgs =
    args::Package<const std::vector<std::pair<string, string>> &,
                  StringTokenType, StringTokenType, MutableFstClass *,
                  const SymbolTable *, const SymbolTable *>;

using StringMapArgs = args::WithReturnValue<bool, StringMapInnerArgs>;

template <class Arc>
void StringMap(StringMapArgs *args) {
  MutableFst<Arc> *fst = args->args.arg4->GetMutableFst<Arc>();
  args->retval =
      CompileStringMap(args->args.arg1, args->args.arg2, args->args.arg3, fst,
                       args->args.arg5, args->args.arg6);
}

bool StringMap(const std::vector<std::pair<string, string>> &pairs,
               StringTokenType itype, StringTokenType otype,
               MutableFstClass *fst, const SymbolTable *isyms = nullptr,
               const SymbolTable *osyms = nullptr);

}  // namespace script
}  // namespace fst

#endif  // PYNINI_STRINGMAPSCRIPT_H_

