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

#ifndef REPEATSCRIPT_H_
#define REPEATSCRIPT_H_

#include <fst/script/arg-packs.h>
#include <fst/script/fst-class.h>
#include "repeat.h"

namespace fst {
namespace script {

typedef args::Package<MutableFstClass *, int, int> RepeatArgs;

template <class Arc>
void Repeat(RepeatArgs *args) {
  MutableFst<Arc> *fst = args->arg1->GetMutableFst<Arc>();
  Repeat(fst, args->arg2, args->arg3);
}

void Repeat(MutableFstClass *fst, int lower, int upper);

}  // namespace script
}  // namespace fst

#endif  // REPEATSCRIPT_H_

