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

#ifndef PYNINI_PYNINI_STRINGIFY_H_
#define PYNINI_PYNINI_STRINGIFY_H_

// This file contains utility functions which allow conversion of kString
// FSTs to C++ strings; attempting to call these on a non-kString FST may
// produce jibberish.

#include <fst/fstlib.h>
#include <fst/script/arg-packs.h>
#include <fst/script/fstscript.h>
#include "pathsscript.h"  // For TokenType, etc.

namespace fst {
namespace script {

typedef args::Package<const FstClass &, TokenType,
                      const SymbolTable *, string *> PyniniStringifyInnerArgs;

typedef args::WithReturnValue<bool, PyniniStringifyInnerArgs>
    PyniniStringifyArgs;

template <class Arc>
void PyniniStringify(PyniniStringifyArgs *args) {
  const Fst<Arc> &fst = *(args->args.arg1.GetFst<Arc>());
  StringPrinter<Arc> sprinter(GetStringPrinterTokenType<Arc>(args->args.arg2),
                              args->args.arg3);
  // We destroy output epsilons if present.
  if (fst.Properties(kNoOEpsilons, false) != kNoOEpsilons) {
    VectorFst<Arc> fst_copy(fst);
    // Allows for deletion of epsilons on output side only.
    if (fst_copy.Properties(kAcceptor, false) != kAcceptor)
      Project(&fst_copy, PROJECT_OUTPUT);
    // Optimizes for RmEpsilon.
    if (fst_copy.Properties(kUnweighted, false) != kUnweighted)
      ArcMap(&fst_copy, RmWeightMapper<Arc>());
    RmEpsilon(&fst_copy);
    args->retval = sprinter(fst_copy, args->args.arg4);
  } else {
    args->retval = sprinter(fst, args->args.arg4);
  }
}

bool PyniniStringify(const FstClass &fst, TokenType token_type,
                     const SymbolTable *, string *);

}  // namespace script
}  // namespace fst

#endif  // PYNINI_PYNINI_STRINGIFY_H_

