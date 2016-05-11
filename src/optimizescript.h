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

// One argpack, three script functions.
typedef args::Package<MutableFstClass *, bool> OptimizeArgs;

template <class Arc>
void Optimize(OptimizeArgs *args) {
  MutableFst<Arc> *fst = args->arg1->GetMutableFst<Arc>();
  Optimize(fst, args->arg2);
}

void Optimize(MutableFstClass *fst, bool compute_props);

template <class Arc>
void OptimizeAcceptor(OptimizeArgs *args) {
  MutableFst<Arc> *fst = args->arg1->GetMutableFst<Arc>();
  OptimizeAcceptor(fst, args->arg2);
}

void OptimizeAcceptor(MutableFstClass *fst, bool compute_props);

template <class Arc>
void OptimizeTransducer(OptimizeArgs *args) {
  MutableFst<Arc> *fst = args->arg1->GetMutableFst<Arc>();
  OptimizeTransducer(fst, args->arg2);
}

void OptimizeTransducer(MutableFstClass *fst, bool compute_props);

template <class Arc>
void OptimizeStringCrossProducts(MutableFstClass *fst) {
  MutableFst<Arc> *typed_fst = fst->GetMutableFst<Arc>();
  OptimizeStringCrossProducts(typed_fst);
}

void OptimizeStringCrossProducts(MutableFstClass *fst);

}  // namespace script
}  // namespace fst

#endif  // PYNINI_OPTIMIZESCRIPT_H_

