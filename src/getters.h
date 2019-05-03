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

#ifndef PYNINI_GETTERS_H_
#define PYNINI_GETTERS_H_

#include <string>
using std::string;

#include "cdrewrite.h"

namespace fst {
namespace script {

bool GetCDRewriteDirection(const string &str, CDRewriteDirection *rd);

bool GetCDRewriteMode(const string &str, CDRewriteMode *rm);

}  // namespace script
}  // namespace fst

#endif  // PYNINI_GETTERS_H_

