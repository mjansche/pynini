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

#include "stringcompilescript.h"
#include <fst/script/script-impl.h>

namespace fst {
namespace script {

bool CompileByteString(const string &str, const WeightClass &wc,
                       MutableFstClass *fst) {
  if (!fst->WeightTypesMatch(wc, "CompileByteString")) {
    fst->SetProperties(kError, kError);
    return false;
  }
  CompileByteStringInnerArgs iargs(str, wc, fst);
  CompileByteStringArgs args(iargs);
  Apply<Operation<CompileByteStringArgs>>("CompileByteString", fst->ArcType(),
                                          &args);
  return args.retval;
}

bool CompileUTF8String(const string &str, const WeightClass &wc,
                       MutableFstClass *fst) {
  if (!fst->WeightTypesMatch(wc, "CompileUTF8String")) {
    fst->SetProperties(kError, kError);
    return false;
  }
  CompileUTF8StringInnerArgs iargs(str, wc, fst);
  CompileUTF8StringArgs args(iargs);
  Apply<Operation<CompileUTF8StringArgs>>("CompileUTF8String", fst->ArcType(),
                                          &args);
  return args.retval;
}

bool CompileSymbolString(const string &str, const WeightClass &wc,
                         const SymbolTable &syms, MutableFstClass *fst) {
  if (!fst->WeightTypesMatch(wc, "CompileSymbolString")) {
    fst->SetProperties(kError, kError);
    return false;
  }
  CompileSymbolStringInnerArgs iargs(str, wc, syms, fst);
  CompileSymbolStringArgs args(iargs);
  Apply<Operation<CompileSymbolStringArgs>>("CompileSymbolString",
                                            fst->ArcType(), &args);
  return args.retval;
}

bool CompileBracketedByteString(const string &str, const WeightClass &wc,
                                MutableFstClass *fst) {
  if (!fst->WeightTypesMatch(wc, "CompileBracketedByteString")) {
    fst->SetProperties(kError, kError);
    return false;
  }
  CompileBracketedByteStringInnerArgs iargs(str, wc, fst);
  CompileBracketedByteStringArgs args(iargs);
  Apply<Operation<CompileBracketedByteStringArgs>>("CompileBracketedByteString",
                                                   fst->ArcType(), &args);
  return args.retval;
}

bool CompileBracketedUTF8String(const string &str, const WeightClass &wc,
                                MutableFstClass *fst) {
  if (!fst->WeightTypesMatch(wc, "CompileBracketedUTF8String")) {
    fst->SetProperties(kError, kError);
    return false;
  }
  CompileBracketedUTF8StringInnerArgs iargs(str, wc, fst);
  CompileBracketedUTF8StringArgs args(iargs);
  Apply<Operation<CompileBracketedUTF8StringArgs>>("CompileBracketedUTF8String",
                                                   fst->ArcType(), &args);
  return args.retval;
}

REGISTER_FST_OPERATION(CompileByteString, StdArc, CompileByteStringArgs);
REGISTER_FST_OPERATION(CompileByteString, LogArc, CompileByteStringArgs);
REGISTER_FST_OPERATION(CompileByteString, Log64Arc, CompileByteStringArgs);

REGISTER_FST_OPERATION(CompileUTF8String, StdArc, CompileUTF8StringArgs);
REGISTER_FST_OPERATION(CompileUTF8String, LogArc, CompileUTF8StringArgs);
REGISTER_FST_OPERATION(CompileUTF8String, Log64Arc, CompileUTF8StringArgs);

REGISTER_FST_OPERATION(CompileSymbolString, StdArc, CompileSymbolStringArgs);
REGISTER_FST_OPERATION(CompileSymbolString, LogArc, CompileSymbolStringArgs);
REGISTER_FST_OPERATION(CompileSymbolString, Log64Arc, CompileSymbolStringArgs);

REGISTER_FST_OPERATION(CompileBracketedByteString, StdArc,
                       CompileBracketedByteStringArgs);
REGISTER_FST_OPERATION(CompileBracketedByteString, LogArc,
                       CompileBracketedByteStringArgs);
REGISTER_FST_OPERATION(CompileBracketedByteString, Log64Arc,
                       CompileBracketedByteStringArgs);

REGISTER_FST_OPERATION(CompileBracketedUTF8String, StdArc,
                       CompileBracketedUTF8StringArgs);
REGISTER_FST_OPERATION(CompileBracketedUTF8String, LogArc,
                       CompileBracketedUTF8StringArgs);
REGISTER_FST_OPERATION(CompileBracketedUTF8String, Log64Arc,
                       CompileBracketedUTF8StringArgs);

}  // namespace script
}  // namespace fst

