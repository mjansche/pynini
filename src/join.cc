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

#include "join.h"

namespace strings {

namespace internal {

size_t GetResultSize(const std::vector<string> &elements, size_t s_size) {
  size_t length = 0;
  for (const string &element : elements)
    length += element.size();
  return length + s_size * (elements.size() - 1);
}

}  // namespace internal

string Join(const std::vector<string> &elements, const string &separator) {
  string result;
  if (elements.empty()) return result;
  size_t s_size = separator.size();
  result.reserve(internal::GetResultSize(elements, s_size));
  auto it = elements.begin();
  result.append(it->data(), it->size());
  for (++it; it != elements.end(); ++it) {
    result.append(separator.data(), s_size);
    result.append(it->data(), it->size());
  }
  return result;
}

}  // namespace strings
