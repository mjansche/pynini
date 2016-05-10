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

#ifndef JOIN_H_
#define JOIN_H_

#include <string>
#include <vector>

using std::string;

// This provides a drop-in replacement for the most common prototype of
// strings::Join.

namespace strings {

string Join(const std::vector<string> &elements, const string &separator);

}  // namespace strings

#endif  // JOIN_H_
