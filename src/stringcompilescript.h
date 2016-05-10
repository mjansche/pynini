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

// Inner argpack for all functions.
typedef args::Package<const string &, const WeightClass &, MutableFstClass *>
    StringCompileInnerArgs;

typedef args::WithReturnValue<bool, StringCompileInnerArgs> StringCompileArgs;

template <class Arc>
void CompileByteString(StringCompileArgs *args) {
  typename Arc::Weight weight =
      *(args->args.arg2.GetWeight<typename Arc::Weight>());
  MutableFst<Arc> *fst = args->args.arg3->GetMutableFst<Arc>();
  args->retval = CompileByteString(args->args.arg1, weight, fst);
}

bool CompileByteString(const string &str, const WeightClass &wc,
                       MutableFstClass *fst);

template <class Arc>
void CompileUTF8String(StringCompileArgs *args) {
  typename Arc::Weight weight =
      *(args->args.arg2.GetWeight<typename Arc::Weight>());
  MutableFst<Arc> *fst = args->args.arg3->GetMutableFst<Arc>();
  args->retval = CompileUTF8String(args->args.arg1, weight, fst);
}

bool CompileUTF8String(const string &str, const WeightClass &wc,
                       MutableFstClass *fst);

template <class Arc>
void CompileBracketedByteString(StringCompileArgs *args) {
  typename Arc::Weight weight =
      *(args->args.arg2.GetWeight<typename Arc::Weight>());
  MutableFst<Arc> *fst = args->args.arg3->GetMutableFst<Arc>();
  args->retval = CompileBracketedByteString(args->args.arg1, weight, fst);
}

bool CompileBracketedByteString(const string &str, const WeightClass &wc,
                                MutableFstClass *fst);

template <class Arc>
void CompileBracketedUTF8String(StringCompileArgs *args) {
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

