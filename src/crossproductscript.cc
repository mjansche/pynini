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
#include "crossproductscript.h"

namespace fst {
namespace script {

void CrossProduct(const FstClass &ifst1, const FstClass &ifst2,
                  MutableFstClass *ofst) {
  if (!ArcTypesMatch(ifst1, ifst2, "CrossProduct") ||
      !ArcTypesMatch(ifst2, *ofst, "CrossProduct")) {
    ofst->SetProperties(kError, kError);
    return;
  }
  CrossProductArgs args(ifst1, ifst2, ofst);
  Apply<Operation<CrossProductArgs>>("CrossProduct", ofst->ArcType(), &args);
}

REGISTER_FST_OPERATION(CrossProduct, StdArc, CrossProductArgs);
REGISTER_FST_OPERATION(CrossProduct, LogArc, CrossProductArgs);
REGISTER_FST_OPERATION(CrossProduct, Log64Arc, CrossProductArgs);

void OptimizeStringCrossProduct(MutableFstClass *fst) {
  Apply<Operation<MutableFstClass>>("OptimizeStringCrossProduct",
                                    fst->ArcType(), fst);
}

REGISTER_FST_OPERATION(OptimizeStringCrossProduct, StdArc, MutableFstClass);
REGISTER_FST_OPERATION(OptimizeStringCrossProduct, LogArc, MutableFstClass);
REGISTER_FST_OPERATION(OptimizeStringCrossProduct, Log64Arc, MutableFstClass);

}  // namespace script
}  // namespace fst

