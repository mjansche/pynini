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

#include "pynini_stringify.h"

namespace fst {
namespace script {

bool PyniniStringify(const FstClass &fst, StringTokenType token_type,
                     const SymbolTable *symbols, string *str) {
  PyniniStringifyInnerArgs iargs(fst, token_type, symbols, str);
  PyniniStringifyArgs args(iargs);
  Apply<Operation<PyniniStringifyArgs>>("PyniniStringify", fst.ArcType(),
                                        &args);
  return args.retval;
}

REGISTER_FST_OPERATION(PyniniStringify, StdArc, PyniniStringifyArgs);
REGISTER_FST_OPERATION(PyniniStringify, LogArc, PyniniStringifyArgs);
REGISTER_FST_OPERATION(PyniniStringify, Log64Arc, PyniniStringifyArgs);

}  // namespace script
}  // namespace fst

