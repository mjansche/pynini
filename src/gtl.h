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

#ifndef PYNINI_GTL_H_
#define PYNINI_GTL_H_

#include <fstream>
#include <memory>
#include <string>
#include <utility>
#include <vector>

using std::string;

// File stuff.

class File {
 public:
  File() : stream_(nullptr) {}

  explicit File(std::fstream *stream) : stream_(stream) {}

  void SetStream(std::fstream *stream) { stream_.reset(stream); }

  std::fstream *stream() { return stream_.get(); }

  void Close() {
    stream_->close();
    stream_.reset();
  }

 private:
  std::unique_ptr<std::fstream> stream_;
};

// 2^14 --- should be enough for 1 line for the intended use.
constexpr auto kMaxline = 16384;

class InputBuffer {
 public:
  explicit InputBuffer(File *fp) : fp_(fp) {}

  bool ReadLine(string *line) {
    line->clear();
    fp_->stream()->getline(buf_, kMaxline);
    if (!fp_->stream()->gcount()) {
      fp_.reset();
      return false;
    }
    line->append(buf_);
    return true;
  }

 private:
  std::unique_ptr<File> fp_;
  char buf_[kMaxline];
};

namespace file {

// Neither of these classes are really good for anything. They exist solely
// for compatibility reasons.

class Options {
 public:
  Options() { }
};

Options Defaults();

struct Status {

  explicit Status(bool stat) : status(stat) {}

  bool ok() const { return status; }

  bool status;
};

Status Open(const string &filename, const string &mode,
            File **f, const Options &opts);

}  // namespace file

// Prefix tree stuff.

template <class T>
void MapUtilAssignNewDefaultInstance(T **location) {
  *location = new T();
}

template <class T>
typename T::value_type::second_type LookupOrInsertNew(
    T *const collection, const typename T::value_type::first_type &key) {
  std::pair<typename T::iterator, bool> ret =
      collection->insert(typename T::value_type(
          key, static_cast<typename T::value_type::second_type>(nullptr)));
  if (ret.second) {
    MapUtilAssignNewDefaultInstance(&(ret.first->second));
  }
  return ret.first->second;
}

// Strings stuff.

namespace strings {

string Join(const std::vector<string> &elements, const string &delim);

std::vector<string> Split(const string &full, const char *delim);

}  // namespace strings

#endif  // PYNINI_GTL_H_
