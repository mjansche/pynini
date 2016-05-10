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

#ifndef STRINGCOMPILESCRIPT_H_
#define STRINGCOMPILESCRIPT_H_

#include <fst/script/arg-packs.h>
#include <fst/script/fst-class.h>
#include "stringcompile.h"

namespace fst {
namespace script {

typedef args::Package<const string &, const WeightClass &, MutableFstClass *>
    CompileByteStringInnerArgs;

typedef args::WithReturnValue<bool, CompileByteStringInnerArgs>
    CompileByteStringArgs;

template <class Arc>
void CompileByteString(CompileByteStringArgs *args) {
  typename Arc::Weight weight =
      *(args->args.arg2.GetWeight<typename Arc::Weight>());
  MutableFst<Arc> *fst = args->args.arg3->GetMutableFst<Arc>();
  args->retval = CompileByteString(args->args.arg1, weight, fst);
}

bool CompileByteString(const string &str, const WeightClass &wc,
                       MutableFstClass *fst);

typedef CompileByteStringInnerArgs CompileUTF8StringInnerArgs;
typedef CompileByteStringArgs CompileUTF8StringArgs;

template <class Arc>
void CompileUTF8String(CompileUTF8StringArgs *args) {
  typename Arc::Weight weight =
      *(args->args.arg2.GetWeight<typename Arc::Weight>());
  MutableFst<Arc> *fst = args->args.arg3->GetMutableFst<Arc>();
  args->retval = CompileUTF8String(args->args.arg1, weight, fst);
}

typedef args::Package<const string &, const WeightClass &, const SymbolTable &,
                      MutableFstClass *>
    CompileSymbolStringInnerArgs;

typedef args::WithReturnValue<bool, CompileSymbolStringInnerArgs>
    CompileSymbolStringArgs;

template <class Arc>
void CompileSymbolString(CompileSymbolStringArgs *args) {
  typename Arc::Weight weight =
      *(args->args.arg2.GetWeight<typename Arc::Weight>());
  const SymbolTable &syms = args->args.arg3;
  MutableFst<Arc> *fst = args->args.arg4->GetMutableFst<Arc>();
  args->retval = CompileSymbolString(args->args.arg1, weight, syms, fst);
}

bool CompileSymbolString(const string &str, const WeightClass &wc,
                         const SymbolTable &syms, MutableFstClass *fst);

typedef CompileByteStringInnerArgs CompileBracketedByteStringInnerArgs;
typedef CompileByteStringArgs CompileBracketedByteStringArgs;

template <class Arc>
void CompileBracketedByteString(CompileBracketedByteStringArgs *args) {
  typename Arc::Weight weight =
      *(args->args.arg2.GetWeight<typename Arc::Weight>());
  MutableFst<Arc> *fst = args->args.arg3->GetMutableFst<Arc>();
  args->retval = CompileBracketedByteString(args->args.arg1, weight, fst);
}

bool CompileBracketedByteString(const string &str, const WeightClass &wc,
                                MutableFstClass *fst);

typedef CompileUTF8StringInnerArgs CompileBracketedUTF8StringInnerArgs;
typedef CompileUTF8StringArgs CompileBracketedUTF8StringArgs;

template <class Arc>
void CompileBracketedUTF8String(CompileBracketedUTF8StringArgs *args) {
  typename Arc::Weight weight =
      *(args->args.arg2.GetWeight<typename Arc::Weight>());
  MutableFst<Arc> *fst = args->args.arg3->GetMutableFst<Arc>();
  args->retval = CompileBracketedUTF8String(args->args.arg1, weight, fst);
}

bool CompileBracketedUTF8String(const string &str, const WeightClass &wc,
                                MutableFstClass *fst);

}  // namespace script
}  // namespace fst

#endif  // STRINGCOMPILESCRIPT_H_

