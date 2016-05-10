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

#ifndef MERGESCRIPT_H_
#define MERGESCRIPT_H_

#include <fst/script/arg-packs.h>
#include <fst/script/fst-class.h>
#include "merge.h"

namespace fst {
namespace script {

// These operations merge FST-attached symbol tables so that they are
// compatible for other FST operations.

typedef args::Package<MutableFstClass *, MutableFstClass *,
                      MergeSymbolsType> MergeSymbolsInnerArgs;
typedef args::WithReturnValue<bool, MergeSymbolsInnerArgs> MergeSymbolsArgs;

template <class Arc>
void MergeSymbols(MergeSymbolsArgs *args) {
  MutableFst<Arc> *fst1 = args->args.arg1->GetMutableFst<Arc>();
  MutableFst<Arc> *fst2 = args->args.arg2->GetMutableFst<Arc>();
  args->retval = MergeSymbols(fst1, fst2, args->args.arg3);
}

bool MergeSymbols(MutableFstClass *fst1, MutableFstClass *fst2,
                  MergeSymbolsType mst);

}  // namespace script
}  // namespace fst

#endif  // MERGESCRIPT_H_

