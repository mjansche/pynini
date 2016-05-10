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
#include "stringcompilescript.h"

namespace fst {
namespace script {

bool CompileByteString(const string &str, const WeightClass &wc,
                       MutableFstClass *fst) {
  StringCompileInnerArgs iargs(str, wc, fst);
  StringCompileArgs args(iargs);
  Apply<Operation<StringCompileArgs>>("CompileByteString", fst->ArcType(),
                                      &args);
  return args.retval;
}

bool CompileUTF8String(const string &str, const WeightClass &wc,
                       MutableFstClass *fst) {
  StringCompileInnerArgs iargs(str, wc, fst);
  StringCompileArgs args(iargs);
  Apply<Operation<StringCompileArgs>>("CompileUTF8String", fst->ArcType(),
                                      &args);
  return args.retval;
}

bool CompileBracketedByteString(const string &str, const WeightClass &wc,
                                MutableFstClass *fst) {
  StringCompileInnerArgs iargs(str, wc, fst);
  StringCompileArgs args(iargs);
  Apply<Operation<StringCompileArgs>>("CompileBracketedByteString",
                                      fst->ArcType(), &args);
  return args.retval;
}

bool CompileBracketedUTF8String(const string &str, const WeightClass &wc,
                                MutableFstClass *fst) {
  StringCompileInnerArgs iargs(str, wc, fst);
  StringCompileArgs args(iargs);
  Apply<Operation<StringCompileArgs>>("CompileBracketedUTF8String",
                                      fst->ArcType(), &args);
  return args.retval;
}

REGISTER_FST_OPERATION(CompileByteString, StdArc, StringCompileArgs);
REGISTER_FST_OPERATION(CompileByteString, LogArc, StringCompileArgs);
REGISTER_FST_OPERATION(CompileByteString, Log64Arc, StringCompileArgs);

REGISTER_FST_OPERATION(CompileUTF8String, StdArc, StringCompileArgs);
REGISTER_FST_OPERATION(CompileUTF8String, LogArc, StringCompileArgs);
REGISTER_FST_OPERATION(CompileUTF8String, Log64Arc, StringCompileArgs);

REGISTER_FST_OPERATION(CompileBracketedByteString, StdArc, StringCompileArgs);
REGISTER_FST_OPERATION(CompileBracketedByteString, LogArc, StringCompileArgs);
REGISTER_FST_OPERATION(CompileBracketedByteString, Log64Arc, StringCompileArgs);

REGISTER_FST_OPERATION(CompileBracketedUTF8String, StdArc, StringCompileArgs);
REGISTER_FST_OPERATION(CompileBracketedUTF8String, LogArc, StringCompileArgs);
REGISTER_FST_OPERATION(CompileBracketedUTF8String, Log64Arc, StringCompileArgs);

}  // namespace script
}  // namespace fst

