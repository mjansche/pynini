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

#include <fst/script/fst-class.h>
#include <fst/script/script-impl.h>
#include "optimizescript.h"

namespace fst {
namespace script {

void Optimize(MutableFstClass *fst, bool compute_props) {
  OptimizeArgs args(fst, compute_props);
  Apply<Operation<OptimizeArgs>>("Optimize", fst->ArcType(), &args);
}

REGISTER_FST_OPERATION(Optimize, StdArc, OptimizeArgs);
REGISTER_FST_OPERATION(Optimize, LogArc, OptimizeArgs);
REGISTER_FST_OPERATION(Optimize, Log64Arc, OptimizeArgs);

void OptimizeAcceptor(MutableFstClass *fst, bool compute_props) {
  OptimizeArgs args(fst, compute_props);
  Apply<Operation<OptimizeArgs>>("OptimizeAcceptor", fst->ArcType(), &args);
}

REGISTER_FST_OPERATION(OptimizeAcceptor, StdArc, OptimizeArgs);
REGISTER_FST_OPERATION(OptimizeAcceptor, LogArc, OptimizeArgs);
REGISTER_FST_OPERATION(OptimizeAcceptor, Log64Arc, OptimizeArgs);

void OptimizeTransducer(MutableFstClass *fst, bool compute_props) {
  OptimizeArgs args(fst, compute_props);
  Apply<Operation<OptimizeArgs>>("OptimizeTransducer", fst->ArcType(), &args);
}

REGISTER_FST_OPERATION(OptimizeTransducer, StdArc, OptimizeArgs);
REGISTER_FST_OPERATION(OptimizeTransducer, LogArc, OptimizeArgs);
REGISTER_FST_OPERATION(OptimizeTransducer, Log64Arc, OptimizeArgs);

void OptimizeStringCrossProducts(MutableFstClass *fst) {
  Apply<Operation<MutableFstClass>>("OptimizeStringCrossProducts",
                                    fst->ArcType(), fst);
}

REGISTER_FST_OPERATION(OptimizeStringCrossProducts, StdArc, MutableFstClass);
REGISTER_FST_OPERATION(OptimizeStringCrossProducts, LogArc, MutableFstClass);
REGISTER_FST_OPERATION(OptimizeStringCrossProducts, Log64Arc, MutableFstClass);

}  // namespace script
}  // namespace fst

