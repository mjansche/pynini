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

#include <fst/script/script-impl.h>
#include "stringfilescript.h"

namespace fst {
namespace script {

bool StringFile(const string &fname, TokenType itype, TokenType otype,
                MutableFstClass *fst, const SymbolTable *isyms,
                const SymbolTable *osyms) {
  StringFileInnerArgs iargs(fname, itype, otype, fst, isyms, osyms);
  StringFileArgs args(iargs);
  Apply<Operation<StringFileArgs>>("StringFile", fst->ArcType(), &args);
  return args.retval;
}

REGISTER_FST_OPERATION(StringFile, StdArc, StringFileArgs);
REGISTER_FST_OPERATION(StringFile, LogArc, StringFileArgs);
REGISTER_FST_OPERATION(StringFile, Log64Arc, StringFileArgs);

}  // namespace script
}  // namespace fst

