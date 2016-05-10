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

#ifndef STRINGCOMPILE_H_
#define STRINGCOMPILE_H_

#include <limits>
#include <memory>
#include <string>

#include <fst/types.h>
#include <cstdlib>
#include <fst/fstlib.h>
#include <re2/stringpiece.h>
#include <re2/re2.h>

#include "split.h"

using re2::RE2;
using re2::StringPiece;
using std::string;

// The user-facing functions in this class behave similarly to the
// StringCompiler class, but add several additional functionalities.
//
// First, the compiler functions create a symbol table from the labels and
// assign this table to the resulting FST. The input strings are either treated
// as raw bytes (CompileByteString) or UTF-8 characters (CompileUTF8String).
//
// Secondly, the functions CompileBracketedByteString and
// CompileBracketedUTF8string treats strings enclosed within square brackets
// as generated symbols with labels beyond the range of the Unicode Basic
// Multilingual Plane.
//
// If a bracketed string contains multiple instances of the
// FLAGS_token_separator byte (by default, space), the bracketed span is
// assumed to contain multiple generated symbols.
//
// Each such symbol is first input is passed to the C library function strol,
// which attempts to parse it as a signed long integer---it can handle octal,
// decimal, and hexadecimal strings, and will ignore any leading whitespace---
// it is treated as a raw integer (usually a byte). If this fails, however,
// each token in the bracketed span is treated as a "generated" symbol with a
// unique integer label and a string form given by its input bytes.
//
// Nesting of square brackets is disallowed.
//
// To pass a bracket literal when using the CompileBracket* functions, simply
// escape it with a preceding backslash.

DECLARE_string(token_separator);
DECLARE_int32(generated_label_index_start);

namespace fst {

// Helper functions are enclosed within the `internal` namespace and are not
// intended for use by external callers; they may change their interfaces any
// time.

namespace internal {

// "Special" byte-range symbols.

constexpr char kByteSymbolTableName[] = "**Byte symbols";
constexpr char kUTF8SymbolTableName[] = "**UTF8 symbols";

constexpr char kPieces[] = R"(((?:(?:\\[\[\]])|(?:[^\[\]]))+)|)"
                           R"((?:\[((?:(?:\\[\[\]])|(?:[^\[\]]))+)\])|)"
                           R"((.+))";
constexpr char kEscapedLeftBracket[] = R"(\\\[)";
constexpr char kEscapedRightBracket[] = R"(\\\])";

// Helpers for creating a symbol table, initialized with just epsilon.

class SymbolTableFactory {
 public:
  explicit SymbolTableFactory(const string &name)
      : syms_(name) {
    // Label zero is reserved for epsilon.
    syms_.AddSymbol("<epsilon>", 0);
    // ASCII control characters.
    syms_.AddSymbol("<SOH>", 0x01);
    syms_.AddSymbol("<STX>", 0x02);
    syms_.AddSymbol("<ETX>", 0x03);
    syms_.AddSymbol("<EOT>", 0x04);
    syms_.AddSymbol("<ENQ>", 0x05);
    syms_.AddSymbol("<ACK>", 0x06);
    syms_.AddSymbol("<BEL>", 0x07);
    syms_.AddSymbol("<BS>", 0x08);
    syms_.AddSymbol("<HT>", 0x09);
    syms_.AddSymbol("<LF>", 0x0a);
    syms_.AddSymbol("<VT>", 0x0b);
    syms_.AddSymbol("<FF>", 0x0c);
    syms_.AddSymbol("<CR>", 0x0d);
    syms_.AddSymbol("<SO>", 0x0e);
    syms_.AddSymbol("<SI>", 0x0f);
    syms_.AddSymbol("<DLE>", 0x10);
    syms_.AddSymbol("<DC1>", 0x11);
    syms_.AddSymbol("<DC2>", 0x12);
    syms_.AddSymbol("<DC3>", 0x13);
    syms_.AddSymbol("<DC4>", 0x14);
    syms_.AddSymbol("<NAK>", 0x15);
    syms_.AddSymbol("<SYN>", 0x16);
    syms_.AddSymbol("<ETB>", 0x17);
    syms_.AddSymbol("<CAN>", 0x18);
    syms_.AddSymbol("<EM>", 0x19);
    syms_.AddSymbol("<SUB>", 0x1a);
    syms_.AddSymbol("<ESC>", 0x1b);
    syms_.AddSymbol("<FS>", 0x1c);
    syms_.AddSymbol("<GS>", 0x1d);
    syms_.AddSymbol("<RS>", 0x1e);
    syms_.AddSymbol("<US>", 0x1f);
    // Space doesn't print very nice.
    syms_.AddSymbol("<space>", 32);
    // Printable ASCII.
    for (char ch = 33; ch < 127; ++ch)
      syms_.AddSymbol(string(1, ch), ch);
    // One last control character.
    syms_.AddSymbol("<DEL>", 0x7f);
    // Adds supra-ASCII characters as hexidecimal strings.
    for (auto ch = 128; ch < 256; ++ch) {
      std::stringstream sstrm;
      sstrm << "<0x" << std::hex << ch << ">";
      syms_.AddSymbol(sstrm.str(), ch);
    }
    // This advances the next label for the one-argument form of AddSymbols
    // (used for user-generated symbols) to beyond the code points for the
    // Basic Multilingual Plane.
    syms_.AddSymbol("", FLAGS_generated_label_index_start);
  }

  // The SymbolTable's p-impl is reference-counted, so no deep copy is made
  // unless the strings used contain control characters or user-generated
  // symbols.
  SymbolTable *GetTable() const { return syms_.Copy(); }

 private:
  SymbolTable syms_;
};

const SymbolTableFactory byte_table_factory(kByteSymbolTableName);
const SymbolTableFactory utf8_table_factory(kUTF8SymbolTableName);

// Adds an integer to the symbol table.
int64 AddIntegerToSymbolTable(int64 ch, SymbolTable *syms);

// Adds a Unicode codepoint to the symbol table. Returns -1 to indicate that
// the input cannot be parsed as a Unicode codepoint.
int32 AddUnicodeCodepointToSymbolTable(int32 ch, SymbolTable *syms);

// Adds a generated label to the table.
inline int64 AddGeneratedToSymbolTable(const string &str, SymbolTable *syms) {
  return syms->AddSymbol(str);
}

// Populates a string FST using a vector of labels.
template <class A>
void CompileStringFromLabels(const std::vector<typename A::Label> &labels,
                             const typename A::Weight &weight,
                             MutableFst<A> *fst) {
  using Arc = A;
  using Weight = typename A::Weight;
  fst->DeleteStates();
  while (fst->NumStates() <= labels.size()) fst->AddState();
  for (auto i = 0; i < labels.size(); ++i)
    fst->AddArc(i, Arc(labels[i], labels[i], Weight::One(), i + 1));
  fst->SetStart(0);
  fst->SetFinal(labels.size(), weight);
}

// Removes backslashes acting as bracket escape characters (e.g. "\[").
void RemoveBracketEscapes(string *str);

// Processes the labels within a bracketed span.
template <class A>
bool ProcessBracketedSpan(string *str,
                          std::vector<typename A::Label> *labels,
                          SymbolTable *syms) {
  RemoveBracketEscapes(str);
  // Splits string on the token separator.
  std::vector<string> tokens = strings::Split(*str,
      FLAGS_token_separator[0]);
  // The span may not be empty, so that is not considered here.
  if (tokens.size() == 1) {
    // A bracketed span that does not contain the token-separator is processed
    // either as a numeric label (e.g., [32]), or if that fails, as a single
    // generated label.
    char *remainder = nullptr;
    string token_str = tokens[0];
    int64 label = strtol(token_str.c_str(), &remainder, 0);
    if (label == 0 && *remainder != '\0') {
      // If remainder doesn't point to a null byte, strtol halted before
      // consuming the whole string, so the string is to be treated as a
      //  generated label.
      labels->push_back(AddGeneratedToSymbolTable(token_str, syms));
    } else if (std::numeric_limits<typename A::Label>::min() < label &&
       label < std::numeric_limits<typename A::Label>::max()) {
       // Number is within numeric tolerances.
       labels->push_back(AddIntegerToSymbolTable(label, syms));
    } else {
      LOG(ERROR) << "ProcessBracketedSpan: Numeric label \" "
                 << token_str << "\" not in valid range";
      return false;
    }
  } else {  // tokens.size() > 1.
    // A bracketed span with multiple token-separator-delimited intervals is
    // processed as a sequence of generated labels.
    for (const auto &token : tokens)
      labels->push_back(AddGeneratedToSymbolTable(token, syms));
  }
  return true;
}

// Creates a vector of labels from a bracketed bytestring, updating the symbol
// table as it goes....
template <class A>
bool BracketedByteStringToLabels(const string &str,
                                 std::vector<typename A::Label> *labels,
                                 SymbolTable *syms) {
  StringPiece strp(str);
  static const RE2 pieces(kPieces);
  string unbracketed;
  string bracketed;
  string error;
  while (RE2::Consume(&strp, pieces, &unbracketed, &bracketed, &error)) {
    if (!error.empty()) {
      LOG(ERROR) << "BracketedByteStringToLabels: Unbalanced brackets";
      return false;
    } else if (!unbracketed.empty()) {
      RemoveBracketEscapes(&unbracketed);
      for (auto label : unbracketed)
        labels->push_back(label);
    } else {  // A non-empty bracketed span.
      if (!ProcessBracketedSpan<A>(&bracketed, labels, syms)) {
        return false;
      }
    }
  }
  return true;
}

// Creates a vector of labels from a bracketed UTF-8 string, updating the
// symbol table as it goes.
template <class A>
bool BracketedUTF8StringToLabels(const string &str,
                                 std::vector<typename A::Label> *labels,
                                 SymbolTable *syms) {
  StringPiece strp(str);
  static const RE2 pieces(kPieces);
  string unbracketed;
  string bracketed;
  string error;
  while (RE2::Consume(&strp, pieces, &unbracketed, &bracketed, &error)) {
    if (!error.empty()) {
      LOG(ERROR) << "BracketedUTF8StringToLabels: Unbalanced brackets";
      return false;
    } else if (!unbracketed.empty()) {
      RemoveBracketEscapes(&unbracketed);
      // Adds only the new unbracketed labels.
      auto i = labels->size();
      if (!UTF8StringToLabels(unbracketed, labels)) return false;
      for (; i < labels->size(); ++i)
        AddUnicodeCodepointToSymbolTable((*labels)[i], syms);
    } else {  // A non-empty bracketed span.
      if (!ProcessBracketedSpan<A>(&bracketed, labels, syms))
        return false;
    }
  }
  return true;
}

// Assigns symbol table to FST.
template <class A>
inline void AssignSymbolsToFst(MutableFst<A> *fst, SymbolTable *syms) {
  fst->SetInputSymbols(syms);
  fst->SetOutputSymbols(syms);
  delete syms;
}

// Compiles string from label, assigns symbols to FST, then deletes the symbol
// table pointer.
template <class A>
inline void FinalizeBracketedString(std::vector<typename A::Label> *labels,
                                    const typename A::Weight &weight,
                                    MutableFst<A> *fst,
                                    SymbolTable *syms) {
  CompileStringFromLabels<A>(*labels, weight, fst);
  AssignSymbolsToFst<A>(fst, syms);
}

}  // namespace internal

// Compiles bytestring into string FST.
template <class A>
bool CompileByteString(const string &str,
                       const typename A::Weight &weight,
                       MutableFst<A> *fst) {
  using Label = typename A::Label;
  std::vector<Label> labels;
  for (auto ch : str) labels.push_back(ch);
  SymbolTable *syms = internal::byte_table_factory.GetTable();
  internal::CompileStringFromLabels<A>(labels, weight, fst);
  internal::AssignSymbolsToFst(fst, syms);
  return true;
}

// Compiles UTF-8 string into string FST.
template <class A>
bool CompileUTF8String(const string &str,
                       const typename A::Weight &weight,
                       MutableFst<A> *fst) {
  using Label = typename A::Label;
  std::vector<Label> labels;
  if (!UTF8StringToLabels(str, &labels)) return false;
  SymbolTable *syms = internal::utf8_table_factory.GetTable();
  internal::CompileStringFromLabels<A>(labels, weight, fst);
  for (auto label : labels)
    internal::AddUnicodeCodepointToSymbolTable(label, syms);
  internal::AssignSymbolsToFst(fst, syms);
  return true;
}

// Compiles (possibly) bracketed bytestring into string FST.
template <class A>
bool CompileBracketedByteString(const string &str,
                                const typename A::Weight &weight,
                                MutableFst<A> *fst) {
  using Label = typename A::Label;
  std::vector<Label> labels;
  SymbolTable *syms = internal::byte_table_factory.GetTable();
  if (!internal::BracketedByteStringToLabels<A>(str, &labels, syms)) {
    delete syms;
    return false;
  }
  internal::FinalizeBracketedString<A>(&labels, weight, fst, syms);
  return true;
}

// Compiles (possibly) bracketed UTF-8 string into string FST.
template <class A>
bool CompileBracketedUTF8String(const string &str,
                                const typename A::Weight &weight,
                                MutableFst<A> *fst) {
  using Label = typename A::Label;
  std::vector<Label> labels;
  SymbolTable *syms = internal::utf8_table_factory.GetTable();
  if (!internal::BracketedUTF8StringToLabels<A>(str, &labels, syms)) {
    delete syms;
    return false;
  }
  internal::FinalizeBracketedString<A>(&labels, weight, fst, syms);
  return true;
}

}  // namespace fst

#endif  // STRINGCOMPILE_H_

