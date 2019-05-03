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

#ifndef PYNINI_OPTIMIZESCRIPT_H_
#define PYNINI_OPTIMIZESCRIPT_H_

#include <fst/script/arg-packs.h>
#include <fst/script/fst-class.h>
#include "optimize.h"

namespace fst {
namespace script {

typedef args::Package<MutableFstClass *, bool> OptimizeArgs;

template <class Arc>
void Optimize(OptimizeArgs *args) {
  MutableFst<Arc> *fst = args->arg1->GetMutableFst<Arc>();
  Optimize(fst, args->arg2);
}

void Optimize(MutableFstClass *fst, bool compute_props = false);

template <class Arc>
void OptimizeStringCrossProducts(OptimizeArgs *args) {
  MutableFst<Arc> *fst = args->arg1->GetMutableFst<Arc>();
  OptimizeStringCrossProducts(fst, args->arg2);
}

void OptimizeStringCrossProducts(MutableFstClass *fst,
                                bool compute_props = false);

template <class Arc>
void OptimizeDifferenceRhs(OptimizeArgs *args) {
  MutableFst<Arc> *fst = args->arg1->GetMutableFst<Arc>();
  OptimizeDifferenceRhs(fst, args->arg2);
}

void OptimizeDifferenceRhs(MutableFstClass *fst, bool compute_props = false);

}  // namespace script
}  // namespace fst

#endif  // PYNINI_OPTIMIZESCRIPT_H_

