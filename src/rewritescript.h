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
// Copyright 2017 and onwards Google, Inc.
//
// For general information on the Pynini grammar compilation library, see
// pynini.opengrm.org.

#ifndef PYNINI_REWRITESCRIPT_H_
#define PYNINI_REWRITESCRIPT_H_

#include "base/integral_types.h"
#include <fst/script/arg-packs.h>
#include <fst/script/fst-class.h>
#include "rewrite.h"

namespace fst {
namespace script {

using TopRewriteInnerArgs =
    std::tuple<const FstClass &, const FstClass &, string *, StringTokenType,
               const SymbolTable *>;

using TopRewriteArgs = WithReturnValue<bool, TopRewriteInnerArgs>;

template <class Arc>
void TopRewrite(TopRewriteArgs *args) {
  const Fst<Arc> &input = *(std::get<0>(args->args).GetFst<Arc>());
  const Fst<Arc> &rule = *(std::get<1>(args->args).GetFst<Arc>());
  args->retval = TopRewrite(input, rule, std::get<2>(args->args),
                            std::get<3>(args->args), std::get<4>(args->args));
}

using OneTopRewriteInnerArgs = TopRewriteInnerArgs;

using OneTopRewriteArgs = WithReturnValue<bool, OneTopRewriteInnerArgs>;

template <class Arc>
void OneTopRewrite(OneTopRewriteArgs *args) {
  const Fst<Arc> &input = *(std::get<0>(args->args).GetFst<Arc>());
  const Fst<Arc> &rule = *(std::get<1>(args->args).GetFst<Arc>());
  args->retval = TopRewrite(input, rule, std::get<2>(args->args),
                            std::get<3>(args->args), std::get<4>(args->args));
}

using RewritesInnerArgs =
    std::tuple<const FstClass &, const FstClass &, std::vector<string> *,
               StringTokenType, const SymbolTable *, int32>;

using RewritesArgs = WithReturnValue<bool, RewritesInnerArgs>;

template <class Arc>
void Rewrites(RewritesArgs *args) {
  const Fst<Arc> &input = *(std::get<0>(args->args).GetFst<Arc>());
  const Fst<Arc> &rule = *(std::get<1>(args->args).GetFst<Arc>());
  args->retval =
      Rewrites(input, rule, std::get<2>(args->args), std::get<3>(args->args),
               std::get<4>(args->args), std::get<5>(args->args));
}

// TopRewrites is overloaded.

using TopRewritesInnerArgs1 =
    std::tuple<const FstClass &, const FstClass &, std::vector<string> *,
               StringTokenType, const SymbolTable *, int32>;

using TopRewritesArgs1 = WithReturnValue<bool, TopRewritesInnerArgs1>;

template <class Arc>
void TopRewrites(TopRewritesArgs1 *args) {
  const Fst<Arc> &input = *(std::get<0>(args->args).GetFst<Arc>());
  const Fst<Arc> &rule = *(std::get<1>(args->args).GetFst<Arc>());
  args->retval =
      TopRewrites(input, rule, std::get<2>(args->args), std::get<3>(args->args),
                  std::get<4>(args->args), std::get<5>(args->args));
}

using TopRewritesInnerArgs2 =
    std::tuple<const FstClass &, const FstClass &, int32, std::vector<string> *,
               StringTokenType, const SymbolTable *>;

using TopRewritesArgs2 = WithReturnValue<bool, TopRewritesInnerArgs2>;

template <class Arc>
void TopRewrites(TopRewritesArgs2 *args) {
  const Fst<Arc> &input = *(std::get<0>(args->args).GetFst<Arc>());
  const Fst<Arc> &rule = *(std::get<1>(args->args).GetFst<Arc>());
  args->retval =
      TopRewrites(input, rule, std::get<2>(args->args), std::get<3>(args->args),
                  std::get<4>(args->args), std::get<5>(args->args));
}

bool TopRewrite(const FstClass &input, const FstClass &rule, string *output,
                StringTokenType ttype = BYTE,
                const SymbolTable *syms = nullptr);

bool OneTopRewrite(const FstClass &input, const FstClass &rule, string *output,
                   StringTokenType ttype = BYTE,
                   const SymbolTable *syms = nullptr);

bool Rewrites(const FstClass &input, const FstClass &rule,
              std::vector<string> *outputs, StringTokenType ttype = BYTE,
              const SymbolTable *syms = nullptr, int32 state_multiplier = 4);

bool TopRewrites(const FstClass &input, const FstClass &rule,
                 std::vector<string> *outputs, StringTokenType ttype = BYTE,
                 const SymbolTable *syms = nullptr, int32 state_multiplier = 4);

bool TopRewrites(const FstClass &input, const FstClass &rule, int32 nshortest,
                 std::vector<string> *outputs, StringTokenType ttype = BYTE,
                 const SymbolTable *syms = nullptr);

}  // namespace script
}  // namespace fst

#endif  // PYNINI_REWRITESCRIPT_H_

